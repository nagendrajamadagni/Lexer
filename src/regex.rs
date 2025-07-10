/* Good resource for parsing regex at
 * https://matt.might.net/articles/parsing-regex-with-recursive-descent/ */

use color_eyre::eyre::{Report, Result};
use std::collections::HashSet;
use std::collections::VecDeque;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::PathBuf;

#[derive(Debug)]
pub enum Quantifier {
    Star,
    Question,
    Plus,
    Exact(u32),
    Range(u32, u32),
    Atleast(u32),
    Atmost(u32),
}

#[derive(Debug)]
pub enum Base {
    Character(char),
    EscapeCharacter(char),
    Exp(Box<RegEx>),
    CharSet(HashSet<char>),
}

#[derive(Debug)]
pub enum Factor {
    SimpleFactor(Base, Option<Quantifier>),
}

#[derive(Debug)]
pub enum Term {
    SimpleTerm(Factor),
    ConcatTerm(Factor, Box<Term>),
}

#[derive(Debug)]
pub enum RegEx {
    SimpleRegex(Term),
    AlterRegex(Term, Box<RegEx>),
}

#[derive(Debug)]
pub enum RegExError {
    MalformedMicrosyntaxError(String),
    InvalidRegexError(String),
    UnbalancedParenthesisError(String),
    FileOpenError(String),
    FileReadError(String),
    InvalidCharacterRange(char, char),
    InvalidEscapeCharacter(char),
    InvalidQuantifier(char),
}

impl std::fmt::Display for RegExError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RegExError::UnbalancedParenthesisError(regex) => {
                write!(f, "Error: {} has unbalanced parenthesis!", regex)
            }
            RegExError::InvalidRegexError(regex) => {
                write!(f, "Error: Invalid regex provided: {}", regex)
            }
            RegExError::MalformedMicrosyntaxError(regex) => {
                write!(f, "Error: Malformed microsyntax entry detected: {}", regex)
            }
            RegExError::FileOpenError(err_line) => write!(f, "{}", err_line),
            RegExError::FileReadError(err_line) => write!(f, "{}", err_line),
            RegExError::InvalidCharacterRange(start, end) => write!(
                f,
                "Error: Invalid character range provided: {} - {}",
                start, end
            ),
            RegExError::InvalidEscapeCharacter(ch) => {
                write!(f, "Error: Invalid escape character {}  provided!", ch)
            }
            RegExError::InvalidQuantifier(ch) => {
                write!(f, "Error: Invalid quantifier {} found!", ch)
            }
        }
    }
}

impl std::error::Error for RegExError {}

fn balanced_brackets(regex: &str) -> bool {
    let mut stack = Vec::new();
    let mut chars = regex.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch == '\\' {
            chars.next();
            continue;
        }
        match ch {
            '(' => {
                stack.push(ch);
            }
            ')' => {
                if stack.is_empty() || stack.pop() != Some('(') {
                    return false;
                }
            }
            '[' => {
                stack.push(ch);
            }
            ']' => {
                if stack.is_empty() || stack.pop() != Some('[') {
                    return false;
                }
            }
            '{' => {
                stack.push(ch);
            }
            '}' => {
                if stack.is_empty() || stack.pop() != Some('{') {
                    return false;
                }
            }
            _ => {}
        }
    }
    stack.is_empty()
}

// If these characters are found in a base term then it is invalid
fn nchar_is_valid(nchar: char) -> bool {
    !matches!(nchar, '*' | '|' | '?' | ')' | ']' | '{' | '}')
}

fn is_escape_char(escape_ch: char) -> bool {
    matches!(
        escape_ch,
        'n' | 't' | 'r' | '\\' | '(' | ')' | '[' | ']' | '{' | '}' | '|' | '*' | '+' | '?' | '.'
    )
}

fn parse_char_class(regex: &str, start: usize) -> Result<(HashSet<char>, usize), RegExError> {
    let mut new_start = start;
    let mut char_set: HashSet<char> = HashSet::new();
    let mut negation: bool = false;

    if new_start > regex.len() {
        let err = RegExError::InvalidRegexError(regex.to_string());
        return Err(err);
    }

    if regex.chars().nth(new_start).unwrap() == '^' {
        negation = true;
        new_start += 1;
    }

    while new_start < regex.len() && regex.chars().nth(new_start).unwrap() != ']' {
        if regex.chars().nth(new_start + 1).unwrap() == '-' {
            let char_start = regex.chars().nth(new_start).unwrap();
            let char_end = regex.chars().nth(new_start + 2).unwrap();
            if char_end < char_start {
                return Err(RegExError::InvalidCharacterRange(char_start, char_end));
            }
            for char in char_start..=char_end {
                char_set.insert(char);
            }
            new_start += 3;
        } else if regex.chars().nth(new_start).unwrap() == '\\' {
            if !is_escape_char(regex.chars().nth(new_start + 1).unwrap()) {
                return Err(RegExError::InvalidEscapeCharacter(
                    regex.chars().nth(new_start + 1).unwrap(),
                ));
            }
            match regex.chars().nth(new_start + 1).unwrap() {
                'n' => char_set.insert('\n'),
                't' => char_set.insert('\t'),
                'r' => char_set.insert('\r'),
                '\\' => char_set.insert('\\'),
                '(' => char_set.insert('('),
                ')' => char_set.insert(')'),
                '[' => char_set.insert('['),
                ']' => char_set.insert(']'),
                '{' => char_set.insert('{'),
                '}' => char_set.insert('}'),
                '|' => char_set.insert('|'),
                '*' => char_set.insert('*'),
                '+' => char_set.insert('+'),
                '?' => char_set.insert('?'),
                '.' => char_set.insert('.'),
                _ => {
                    return Err(RegExError::InvalidEscapeCharacter(
                        regex.chars().nth(new_start + 1).unwrap(),
                    ))
                }
            };
            new_start += 2;
        } else {
            char_set.insert(regex.chars().nth(new_start).unwrap());
            new_start += 1;
        }
    }

    if negation {
        let mut ascii_set: HashSet<char> = HashSet::new();

        let start_char: u8 = 32;
        let end_char: u8 = 126;

        for ch in start_char..=end_char {
            ascii_set.insert(ch as char);
        }
        ascii_set.insert('\t');

        let difference_set = ascii_set.difference(&char_set).cloned().collect();
        Ok((difference_set, new_start))
        // Calculate the difference and return
    } else {
        Ok((char_set, new_start))
    }
}

fn parse_base(regex: &str, start: usize) -> Result<(Base, usize)> {
    let nchar = regex.chars().nth(start);
    let nchar = match nchar {
        None => {
            let err = Report::new(RegExError::InvalidRegexError(regex.to_string()));
            return Err(err);
        }
        Some(nchar) => nchar,
    };
    if nchar == '(' {
        let (inner_regex, new_start) = parse_regex(regex, start + 1)?; // Consume the lparen
        let new_base = Base::Exp(Box::new(inner_regex));
        let new_start = new_start + 1; // Consume the rparen
        Ok((new_base, new_start))
    } else if nchar == '[' {
        let (char_set, new_start) = match parse_char_class(regex, start + 1) {
            Ok((char_set, new_start)) => (char_set, new_start),
            Err(err) => {
                let err = Report::new(err);
                return Err(err);
            }
        };
        let new_start = new_start + 1; // Consume the rparen
        let new_base = Base::CharSet(char_set);
        Ok((new_base, new_start))
    } else if nchar == '.' {
        let new_start = start + 1;
        let mut char_set = HashSet::new();

        let start_char: u8 = 32;
        let end_char: u8 = 126;

        for ch in start_char..=end_char {
            char_set.insert(ch as char);
        }

        char_set.insert('\t'); // Insert tab separately

        let new_base = Base::CharSet(char_set);

        Ok((new_base, new_start))
    } else if nchar == '\\' {
        if !is_escape_char(regex.chars().nth(start + 1).unwrap()) {
            let err = Report::new(RegExError::InvalidEscapeCharacter(
                regex.chars().nth(start + 1).unwrap(),
            ));
            return Err(err);
        }
        let new_base = Base::EscapeCharacter(regex.chars().nth(start + 1).unwrap());
        let new_start = start + 2;
        Ok((new_base, new_start))
    } else if nchar_is_valid(nchar) {
        let new_base = Base::Character(nchar);
        let new_start = start + 1;
        Ok((new_base, new_start))
    } else {
        let err = Report::new(RegExError::InvalidRegexError(regex.to_string()));
        return Err(err);
    }
}

fn get_numeric_quantifier(regex: &str, start: usize) -> Result<(Quantifier, usize)> {
    let mut pos = start;
    let mut lower_number = 0;
    let mut higher_number = 0;

    let mut atmost = false;

    while regex.chars().nth(pos).unwrap() == ' ' {
        // Skip any spaces after the LBrace
        pos += 1;
    }

    if regex.chars().nth(pos).unwrap() == '-' {
        atmost = true;
        pos += 1;
    }

    while regex.chars().nth(pos).unwrap() != '}' && regex.chars().nth(pos).unwrap() != '-' {
        // As long as this is a number
        match regex.chars().nth(pos) {
            Some(_) => {}
            None => {
                let err = Report::new(RegExError::InvalidRegexError(regex.to_string()));
                return Err(err);
            }
        };
        let digit = match regex.chars().nth(pos).unwrap().to_digit(10) {
            Some(digit) => digit,
            None => {
                let err = Report::new(RegExError::InvalidQuantifier(
                    regex.chars().nth(pos).unwrap(),
                ));
                return Err(err);
            }
        };

        lower_number = (lower_number * 10) + digit;
        pos += 1;
    }

    if regex.chars().nth(pos).unwrap() == '}' {
        if atmost {
            return Ok((Quantifier::Atmost(lower_number), pos + 1));
        } else {
            return Ok((Quantifier::Exact(lower_number), pos + 1)); // Consume the rbrace
        }
    }

    while regex.chars().nth(pos).unwrap() == ' ' {
        // Skip any spaces after the number
        pos += 1;
    }

    if regex.chars().nth(pos).unwrap() == '-' {
        pos += 1;
    } else {
        let err = Report::new(RegExError::InvalidQuantifier(
            regex.chars().nth(pos).unwrap(),
        ));
        return Err(err);
    }

    // We have either a range or an atleast quantifier

    if regex.chars().nth(pos).unwrap() == '}' {
        return Ok((Quantifier::Atleast(lower_number), pos + 1));
    }

    while regex.chars().nth(pos).unwrap() != '}' {
        // As long as this is a number

        match regex.chars().nth(pos) {
            Some(_) => {}
            None => {
                let err = Report::new(RegExError::InvalidRegexError(regex.to_string()));
                return Err(err);
            }
        };

        let digit = match regex.chars().nth(pos).unwrap().to_digit(10) {
            Some(digit) => digit,
            None => {
                let err = Report::new(RegExError::InvalidQuantifier(
                    regex.chars().nth(pos).unwrap(),
                ));
                return Err(err);
            }
        };
        higher_number = (higher_number * 10) + digit;
        pos += 1;
    }

    if lower_number >= higher_number {
        let err = Report::new(RegExError::InvalidRegexError(regex.to_string()));
        return Err(err);
    }

    Ok((Quantifier::Range(lower_number, higher_number), pos + 1))
}

fn parse_factor(regex: &str, start: usize) -> Result<(Factor, usize)> {
    let (base, new_start) = parse_base(regex, start)?;

    let mut new_start = new_start;
    let quantifier = {
        if new_start >= regex.len() {
            None
        } else if regex.chars().nth(new_start).unwrap() == '*' {
            new_start += 1;
            Some(Quantifier::Star)
        } else if regex.chars().nth(new_start).unwrap() == '?' {
            new_start += 1;
            Some(Quantifier::Question)
        } else if regex.chars().nth(new_start).unwrap() == '+' {
            new_start += 1;
            Some(Quantifier::Plus)
        } else if regex.chars().nth(new_start).unwrap() == '{' {
            let (quantifier, nstart) = get_numeric_quantifier(regex, new_start + 1)?;
            new_start = nstart;
            Some(quantifier)
        } else {
            None
        }
    };
    let term = Factor::SimpleFactor(base, quantifier);
    Ok((term, new_start))
}

fn parse_term(regex: &str, start: usize) -> Result<(Term, usize)> {
    let (factor, mut new_start) = parse_factor(regex, start)?;

    let mut prev_term = Term::SimpleTerm(factor);

    while new_start < regex.len() {
        let nchar = regex.chars().nth(new_start).unwrap();
        if nchar == '|' || nchar == ')' {
            break;
        } else {
            let (next_factor, tmp_start) = parse_factor(regex, new_start)?;
            let next_term = Term::ConcatTerm(next_factor, Box::new(prev_term));
            prev_term = next_term;
            new_start = tmp_start;
        }
    }
    Ok((prev_term, new_start))
}

fn parse_regex(regex: &str, start: usize) -> Result<(RegEx, usize)> {
    if !balanced_brackets(regex) {
        let err = Report::new(RegExError::UnbalancedParenthesisError(regex.to_string()));
        return Err(err);
    }

    if regex.is_empty() {
        let err = Report::new(RegExError::InvalidRegexError(regex.to_string()));
        return Err(err);
    }

    let (term, new_start) = parse_term(regex, start)?;
    if new_start >= regex.len() {
        Ok((RegEx::SimpleRegex(term), new_start))
    } else if regex.chars().nth(new_start).unwrap() == '|' {
        let (next_regex, new_start) = parse_regex(regex, new_start + 1)?;
        Ok((RegEx::AlterRegex(term, Box::new(next_regex)), new_start))
    } else {
        Ok((RegEx::SimpleRegex(term), new_start))
    }
}

fn build_syntax_tree(regex: &str) -> Result<RegEx> {
    let (syntax_tree, _) = parse_regex(regex, 0)?;
    Ok(syntax_tree)
}
/// Parse a list of microsyntaxes provided and return the parse trees
pub fn parse_microsyntax_list(
    regex_list: Vec<(String, String)>,
) -> Result<VecDeque<(String, RegEx, String)>> {
    let mut syntax_tree_list = VecDeque::new();

    for regex_entry in regex_list {
        let (regex, category) = regex_entry;

        let syntax_tree = build_syntax_tree(&regex)?;

        syntax_tree_list.push_back((regex, syntax_tree, category));
    }
    Ok(syntax_tree_list)
}
/// Parse a file containing microsyntaxes and return the parse trees
pub fn read_microsyntax_file(file_path: &str) -> Result<Vec<(String, String)>, RegExError> {
    let file_path = PathBuf::from(file_path);

    let file = File::open(file_path);
    let file = match file {
        Ok(file) => file,
        Err(error) => {
            let err_line = format!("Error: Failed to open the microsyntax file {}", error);
            return Err(RegExError::FileOpenError(err_line));
        }
    };
    let reader = BufReader::new(file);

    let mut regex_list: Vec<(String, String)> = Vec::new();

    for (line_number, line) in reader.lines().enumerate() {
        let line = match line {
            Ok(line) => line,
            Err(error) => {
                let err_line = format!(
                    "Error: Failed to read line number {} in microsyntaxes file {}",
                    line_number, error
                );
                return Err(RegExError::FileReadError(err_line));
            }
        };

        let content: Vec<&str> = line.split("::").collect();

        if content.len() != 2 {
            return Err(RegExError::MalformedMicrosyntaxError(
                content[0].to_string(),
            ));
        }

        let lhs = content[0];
        let lhs = lhs.replace("\\:\\:", "::"); // Escape the double colons itself
        let rhs = content[1];

        let pair = (lhs.to_string(), rhs.to_string());
        regex_list.push(pair);
    }

    Ok(regex_list)
}

#[cfg(test)]
mod regex_tests {
    use crate::regex::{parse_regex, Base, Factor, Quantifier, RegEx, RegExError, Term};

    use super::get_numeric_quantifier;

    // Helper function to simplify match assertions
    fn assert_simple_char(regex: &RegEx, expected_char: char) {
        match regex {
            RegEx::SimpleRegex(Term::SimpleTerm(Factor::SimpleFactor(
                Base::Character(c),
                None,
            ))) if *c == expected_char => {}
            _ => panic!("Expected simple char '{}', got {:?}", expected_char, regex),
        }
    }

    fn assert_grouped_char(regex: &RegEx, expected_char: char) {
        match regex {
            RegEx::SimpleRegex(Term::SimpleTerm(Factor::SimpleFactor(
                Base::Exp(inner_regex),
                None,
            ))) => assert_simple_char(inner_regex, expected_char),
            _ => panic!("Expected grouped char '{}', got {:?}", expected_char, regex),
        }
    }

    fn assert_quantified_char(regex: &RegEx, expected_char: char, expected_quantifier: Quantifier) {
        match regex {
            RegEx::SimpleRegex(Term::SimpleTerm(Factor::SimpleFactor(
                Base::Character(c),
                Some(q),
            ))) if *c == expected_char => match (q, &expected_quantifier) {
                (Quantifier::Star, Quantifier::Star) => {}
                (Quantifier::Plus, Quantifier::Plus) => {}
                (Quantifier::Question, Quantifier::Question) => {}
                _ => panic!("Expected quantifier {:?}, got {:?}", expected_quantifier, q),
            },
            _ => panic!(
                "Expected quantified char '{}', got {:?}",
                expected_char, regex
            ),
        }
    }

    fn assert_concatenation(regex: &RegEx, first_char: char, second_char: char) {
        match regex {
            RegEx::SimpleRegex(Term::ConcatTerm(
                Factor::SimpleFactor(Base::Character(c2), None),
                box_term,
            )) if *c2 == second_char => match **box_term {
                Term::SimpleTerm(Factor::SimpleFactor(Base::Character(c1), None))
                    if c1 == first_char => {}
                _ => panic!("Expected first char '{}', got {:?}", first_char, box_term),
            },
            RegEx::SimpleRegex(Term::ConcatTerm(
                Factor::SimpleFactor(Base::Character(c2), None),
                box_term,
            )) if *c2 == second_char => match **box_term {
                Term::SimpleTerm(Factor::SimpleFactor(Base::EscapeCharacter(c1), None))
                    if c1 == first_char => {}
                _ => panic!("Expected first char '{}', got {:?}", first_char, box_term),
            },
            RegEx::SimpleRegex(Term::ConcatTerm(
                Factor::SimpleFactor(Base::EscapeCharacter(c2), None),
                box_term,
            )) if *c2 == second_char => match **box_term {
                Term::SimpleTerm(Factor::SimpleFactor(Base::Character(c1), None))
                    if c1 == first_char => {}
                _ => panic!("Expected first char '{}', got {:?}", first_char, box_term),
            },
            RegEx::SimpleRegex(Term::ConcatTerm(
                Factor::SimpleFactor(Base::EscapeCharacter(c2), None),
                box_term,
            )) if *c2 == second_char => match **box_term {
                Term::SimpleTerm(Factor::SimpleFactor(Base::EscapeCharacter(c1), None))
                    if c1 == first_char => {}
                _ => panic!("Expected first char '{}', got {:?}", first_char, box_term),
            },
            _ => panic!(
                "Expected concatenation of '{}' and '{}', got {:?}",
                first_char, second_char, regex
            ),
        }
    }

    fn assert_alternation(regex: &RegEx, first_char: char, second_char: char) {
        match regex {
            RegEx::AlterRegex(
                Term::SimpleTerm(Factor::SimpleFactor(Base::Character(c1), None)),
                box_regex,
            ) if *c1 == first_char => match **box_regex {
                RegEx::SimpleRegex(Term::SimpleTerm(Factor::SimpleFactor(
                    Base::Character(c2),
                    None,
                ))) if c2 == second_char => {}
                _ => panic!(
                    "Expected second alternative '{}', got {:?}",
                    second_char, box_regex
                ),
            },
            _ => panic!(
                "Expected alternation of '{}' and '{}', got {:?}",
                first_char, second_char, regex
            ),
        }
    }

    // Basic tests
    #[test]
    fn test_regex_simple_base() {
        let regex = "a";
        let result = parse_regex(regex, 0);
        assert!(result.is_ok());
        let (base, _) = result.unwrap();
        assert_simple_char(&base, 'a');
    }

    #[test]
    fn test_regex_group_base() {
        let regex = "(a)";
        let result = parse_regex(regex, 0);
        assert!(result.is_ok());
        let (base, _) = result.unwrap();
        assert_grouped_char(&base, 'a');
    }

    // Additional tests for more complex patterns
    #[test]
    fn test_regex_quantifiers() {
        // Test star quantifier
        let regex = "a*";
        let result = parse_regex(regex, 0);
        assert!(result.is_ok());
        let (base, _) = result.unwrap();
        assert_quantified_char(&base, 'a', Quantifier::Star);

        // Test plus quantifier
        let regex = "a+";
        let result = parse_regex(regex, 0);
        assert!(result.is_ok());
        let (base, _) = result.unwrap();
        assert_quantified_char(&base, 'a', Quantifier::Plus);

        // Test question mark quantifier
        let regex = "a?";
        let result = parse_regex(regex, 0);
        assert!(result.is_ok());
        let (base, _) = result.unwrap();
        assert_quantified_char(&base, 'a', Quantifier::Question);
    }

    #[test]
    fn test_regex_concatenation() {
        let regex = "ab";
        let result = parse_regex(regex, 0);
        assert!(result.is_ok());
        let (base, _) = result.unwrap();
        assert_concatenation(&base, 'a', 'b');
    }

    #[test]
    fn test_hyphen_cocatenation() {
        let regex = "a-";
        let result = parse_regex(regex, 0);
        assert!(result.is_ok());
        let (base, _) = result.unwrap();
        assert_concatenation(&base, 'a', '-');
    }

    #[test]
    fn test_escape_cocatenation() {
        let regex = "a\\?";
        let result = parse_regex(regex, 0);
        assert!(result.is_ok());
        let (base, _) = result.unwrap();
        assert_concatenation(&base, 'a', '?');
    }

    #[test]
    fn test_regex_alternation() {
        let regex = "a|b";
        let result = parse_regex(regex, 0);
        assert!(result.is_ok());
        let (base, _) = result.unwrap();
        assert_alternation(&base, 'a', 'b');
    }

    // Error case tests
    #[test]
    fn test_unbalanced_parenthesis() {
        let regex = "(a";
        let result = parse_regex(regex, 0);
        assert!(result.is_err());
        match result.unwrap_err().downcast_ref().unwrap() {
            RegExError::UnbalancedParenthesisError(_) => {}
            err => panic!("Expected UnbalancedParenthesisError, got {:?}", err),
        }
    }

    #[test]
    fn test_invalid_escape() {
        let regex = "\\y"; // Assuming \y is not a valid escape
        let result = parse_regex(regex, 0);
        assert!(result.is_err(), "Expected Error got {:?}", result);
        match result.unwrap_err().downcast_ref().unwrap() {
            RegExError::InvalidEscapeCharacter(_) => {}
            err => panic!("Expected InvalidEscapeCharacter, got {:?}", err),
        }
    }

    // Test for character sets
    #[test]
    fn test_character_set() {
        let regex = "[abc]";
        let result = parse_regex(regex, 0);
        assert!(result.is_ok());
        let (base, _) = result.unwrap();

        match base {
            RegEx::SimpleRegex(Term::SimpleTerm(Factor::SimpleFactor(
                Base::CharSet(set),
                None,
            ))) => {
                assert_eq!(set.len(), 3);
                assert!(set.contains(&'a'));
                assert!(set.contains(&'b'));
                assert!(set.contains(&'c'));
            }
            _ => panic!("Expected character set, got {:?}", base),
        }
    }

    // Test for character range
    #[test]
    fn test_character_range() {
        let regex = "[a-c]";
        let result = parse_regex(regex, 0);
        assert!(result.is_ok());
        let (base, _) = result.unwrap();

        match base {
            RegEx::SimpleRegex(Term::SimpleTerm(Factor::SimpleFactor(
                Base::CharSet(set),
                None,
            ))) => {
                assert_eq!(set.len(), 3);
                assert!(set.contains(&'a'));
                assert!(set.contains(&'b'));
                assert!(set.contains(&'c'));
            }
            _ => panic!("Expected character set, got {:?}", base),
        }
    }

    // Test for character set with escape character
    #[test]
    fn test_character_set_escape_char() {
        let regex = "[ab\\?]";
        let result = parse_regex(regex, 0);
        assert!(result.is_ok());
        let (base, _) = result.unwrap();

        match base {
            RegEx::SimpleRegex(Term::SimpleTerm(Factor::SimpleFactor(
                Base::CharSet(set),
                None,
            ))) => {
                assert_eq!(set.len(), 3);
                assert!(set.contains(&'a'));
                assert!(set.contains(&'b'));
                assert!(set.contains(&'?'));
            }
            _ => panic!("Expected character set, got {:?}", base),
        }
    }

    // Test for invalid character range
    #[test]
    fn test_character_range_fail() {
        let regex = "[a-9]";
        let result = parse_regex(regex, 0);
        assert!(result.is_err());

        match result.unwrap_err().downcast_ref().unwrap() {
            RegExError::InvalidCharacterRange(_, _) => {}
            result => panic!("Expected invalid character range error. Got {:?}", result),
        }
    }

    // Test for complex nested patterns
    #[test]
    fn test_nested_pattern() {
        let regex = "(a|b)*c";
        let result = parse_regex(regex, 0);
        assert!(result.is_ok());
        let result = result.unwrap().0;

        match result {
            RegEx::SimpleRegex(Term::ConcatTerm(
                Factor::SimpleFactor(Base::Character('c'), None),
                box_term,
            )) => match *box_term {
                Term::SimpleTerm(Factor::SimpleFactor(
                    Base::Exp(inner_regex),
                    Some(Quantifier::Star),
                )) => match *inner_regex {
                    RegEx::AlterRegex(
                        Term::SimpleTerm(Factor::SimpleFactor(Base::Character('a'), None)),
                        right,
                    ) => match *right {
                        RegEx::SimpleRegex(Term::SimpleTerm(Factor::SimpleFactor(
                            Base::Character('b'),
                            None,
                        ))) => {}
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            },

            _ => unreachable!(),
        }
    }

    #[test]
    fn test_exact_quantifier() {
        let regex = "a{5}";

        let result = get_numeric_quantifier(regex, 2);

        assert!(result.is_ok());

        let result = result.unwrap().0;

        match result {
            Quantifier::Exact(5) => {}
            _ => unreachable!(),
        }

        let result = parse_regex(regex, 0);

        assert!(result.is_ok());

        let result = result.unwrap().0;

        match result {
            RegEx::SimpleRegex(Term::SimpleTerm(Factor::SimpleFactor(
                Base::Character('a'),
                Some(Quantifier::Exact(5)),
            ))) => {}
            _ => unreachable!(),
        }
    }

    #[test]
    fn test_exact_quantifier_multi_digit() {
        let regex = "a{456}";

        let result = get_numeric_quantifier(regex, 2);

        assert!(result.is_ok());

        let result = result.unwrap().0;

        match result {
            Quantifier::Exact(456) => {}
            _ => unreachable!(),
        }

        let result = parse_regex(regex, 0);

        assert!(result.is_ok());

        let result = result.unwrap().0;

        match result {
            RegEx::SimpleRegex(Term::SimpleTerm(Factor::SimpleFactor(
                Base::Character('a'),
                Some(Quantifier::Exact(456)),
            ))) => {}
            _ => unreachable!(),
        }
    }

    #[test]
    fn test_exact_quantifier_invalid() {
        let regex = "a{4f6}";

        let result = get_numeric_quantifier(regex, 2);

        assert!(result.is_err());

        match result.unwrap_err().downcast_ref().unwrap() {
            RegExError::InvalidQuantifier(_) => {}
            _ => unreachable!(),
        }

        let result = parse_regex(regex, 0);

        assert!(result.is_err());

        match result.unwrap_err().downcast_ref().unwrap() {
            RegExError::InvalidQuantifier(_) => {}
            _ => unreachable!(),
        }
    }

    #[test]
    fn test_range_quantifier() {
        let regex = "a{5-7}";

        let result = get_numeric_quantifier(regex, 2);

        assert!(result.is_ok());

        let result = result.unwrap().0;

        match result {
            Quantifier::Range(5, 7) => {}
            _ => unreachable!(),
        }

        let result = parse_regex(regex, 0);

        assert!(result.is_ok());

        let result = result.unwrap().0;

        match result {
            RegEx::SimpleRegex(Term::SimpleTerm(Factor::SimpleFactor(
                Base::Character('a'),
                Some(Quantifier::Range(5, 7)),
            ))) => {}
            _ => unreachable!(),
        }
    }

    #[test]
    fn test_range_quantifier_multi_digit() {
        let regex = "a{45-64}";

        let result = get_numeric_quantifier(regex, 2);

        assert!(result.is_ok());

        let result = result.unwrap().0;

        match result {
            Quantifier::Range(45, 64) => {}
            _ => unreachable!(),
        }

        let result = parse_regex(regex, 0);

        assert!(result.is_ok());

        let result = result.unwrap().0;

        match result {
            RegEx::SimpleRegex(Term::SimpleTerm(Factor::SimpleFactor(
                Base::Character('a'),
                Some(Quantifier::Range(45, 64)),
            ))) => {}
            _ => unreachable!(),
        }
    }

    #[test]
    fn test_range_quantifier_invalid1() {
        let regex = "a{4-f}";

        let result = get_numeric_quantifier(regex, 2);

        assert!(result.is_err());

        match result.unwrap_err().downcast_ref().unwrap() {
            RegExError::InvalidQuantifier(_) => {}
            _ => unreachable!(),
        }

        let result = parse_regex(regex, 0);

        assert!(result.is_err());

        match result.unwrap_err().downcast_ref().unwrap() {
            RegExError::InvalidQuantifier(_) => {}
            _ => unreachable!(),
        }
    }

    #[test]
    fn test_range_quantifier_invalid2() {
        let regex = "a{4-1}";

        let result = get_numeric_quantifier(regex, 2);

        assert!(result.is_err());

        match result.unwrap_err().downcast_ref().unwrap() {
            RegExError::InvalidRegexError(_) => {}
            _ => unreachable!(),
        }

        let result = parse_regex(regex, 0);

        assert!(result.is_err());

        match result.unwrap_err().downcast_ref().unwrap() {
            RegExError::InvalidRegexError(_) => {}
            _ => unreachable!(),
        }
    }

    #[test]
    fn test_atleast_quantifier() {
        let regex = "a{5-}";

        let result = get_numeric_quantifier(regex, 2);

        assert!(result.is_ok());

        let result = result.unwrap().0;

        match result {
            Quantifier::Atleast(5) => {}
            _ => unreachable!(),
        }

        let result = parse_regex(regex, 0);

        assert!(result.is_ok());

        let result = result.unwrap().0;

        match result {
            RegEx::SimpleRegex(Term::SimpleTerm(Factor::SimpleFactor(
                Base::Character('a'),
                Some(Quantifier::Atleast(5)),
            ))) => {}
            _ => unreachable!(),
        }
    }

    #[test]
    fn test_atleast_quantifier_multi_digit() {
        let regex = "a{45-}";

        let result = get_numeric_quantifier(regex, 2);

        assert!(result.is_ok());

        let result = result.unwrap().0;

        match result {
            Quantifier::Atleast(45) => {}
            _ => unreachable!(),
        }

        let result = parse_regex(regex, 0);

        assert!(result.is_ok());

        let result = result.unwrap().0;

        match result {
            RegEx::SimpleRegex(Term::SimpleTerm(Factor::SimpleFactor(
                Base::Character('a'),
                Some(Quantifier::Atleast(45)),
            ))) => {}
            _ => unreachable!(),
        }
    }

    #[test]
    fn test_atleast_quantifier_invalid1() {
        let regex = "a{4f-}";

        let result = get_numeric_quantifier(regex, 2);

        assert!(result.is_err());

        match result.unwrap_err().downcast_ref().unwrap() {
            RegExError::InvalidQuantifier(_) => {}
            _ => unreachable!(),
        }

        let result = parse_regex(regex, 0);

        assert!(result.is_err());

        match result.unwrap_err().downcast_ref().unwrap() {
            RegExError::InvalidQuantifier(_) => {}
            _ => unreachable!(),
        }
    }

    #[test]
    fn test_atmost_quantifier() {
        let regex = "a{-5}";

        let result = get_numeric_quantifier(regex, 2);

        assert!(result.is_ok());

        let result = result.unwrap().0;

        match result {
            Quantifier::Atmost(5) => {}
            _ => unreachable!(),
        }

        let result = parse_regex(regex, 0);

        assert!(result.is_ok());

        let result = result.unwrap().0;

        match result {
            RegEx::SimpleRegex(Term::SimpleTerm(Factor::SimpleFactor(
                Base::Character('a'),
                Some(Quantifier::Atmost(5)),
            ))) => {}
            _ => unreachable!(),
        }
    }

    #[test]
    fn test_atmost_quantifier_multi_digit() {
        let regex = "a{-45}";

        let result = get_numeric_quantifier(regex, 2);

        assert!(result.is_ok());

        let result = result.unwrap().0;

        match result {
            Quantifier::Atmost(45) => {}
            _ => unreachable!(),
        }

        let result = parse_regex(regex, 0);

        assert!(result.is_ok());

        let result = result.unwrap().0;

        match result {
            RegEx::SimpleRegex(Term::SimpleTerm(Factor::SimpleFactor(
                Base::Character('a'),
                Some(Quantifier::Atmost(45)),
            ))) => {}
            _ => unreachable!(),
        }
    }

    #[test]
    fn test_atmost_quantifier_invalid1() {
        let regex = "a{-4f}";

        let result = get_numeric_quantifier(regex, 2);

        assert!(result.is_err());

        match result.unwrap_err().downcast_ref().unwrap() {
            RegExError::InvalidQuantifier(_) => {}
            _ => unreachable!(),
        }

        let result = parse_regex(regex, 0);

        assert!(result.is_err());

        match result.unwrap_err().downcast_ref().unwrap() {
            RegExError::InvalidQuantifier(_) => {}
            _ => unreachable!(),
        }
    }

    #[test]
    fn test_lbrace() {
        let regex = "{";

        let result = parse_regex(regex, 0);

        assert!(result.is_err());

        match result.unwrap_err().downcast_ref().unwrap() {
            RegExError::UnbalancedParenthesisError(_) => {}
            _ => unreachable!(),
        }
    }

    #[test]
    fn test_lbrace_escaped() {
        let regex = "\\{";

        let result = parse_regex(regex, 0);

        assert!(result.is_ok());

        let result = result.unwrap().0;

        match result {
            RegEx::SimpleRegex(Term::SimpleTerm(Factor::SimpleFactor(
                Base::EscapeCharacter('{'),
                None,
            ))) => {}
            _ => panic!("Expected {{, got {:?}", result),
        }
    }

    #[test]
    fn test_lbrace_rbrace() {
        let regex = "{}";

        let result = parse_regex(regex, 0);

        assert!(result.is_err());

        match result.unwrap_err().downcast_ref().unwrap() {
            RegExError::InvalidRegexError(_) => {}
            _ => unreachable!(),
        }
    }

    #[test]
    fn test_lbrace_rbrace_escaped() {
        let regex = "\\{\\}";

        let result = parse_regex(regex, 0);

        assert!(result.is_ok());

        let result = result.unwrap().0;

        match result {
            RegEx::SimpleRegex(Term::ConcatTerm(
                Factor::SimpleFactor(Base::EscapeCharacter('}'), None),
                lterm,
            )) => match *lterm {
                Term::SimpleTerm(Factor::SimpleFactor(Base::EscapeCharacter('{'), None)) => {}
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    #[test]
    fn test_dot_escaped() {
        let regex = "\\.";

        let result = parse_regex(regex, 0);

        assert!(result.is_ok());

        let result = result.unwrap().0;

        match result {
            RegEx::SimpleRegex(Term::SimpleTerm(Factor::SimpleFactor(
                Base::EscapeCharacter('.'),
                None,
            ))) => {}
            _ => panic!("Expected {{, got {:?}", result),
        }
    }

    #[test]
    fn test_dot() {
        let regex = ".";

        let result = parse_regex(regex, 0);

        assert!(result.is_ok());

        let result = result.unwrap().0;

        match result {
            RegEx::SimpleRegex(Term::SimpleTerm(Factor::SimpleFactor(
                Base::CharSet(set),
                None,
            ))) => {
                let start_char: u8 = 32;
                let end_char: u8 = 126;

                for ch in start_char..=end_char {
                    assert!(set.contains(&(ch as char)));
                }
                assert!(set.contains(&'\t'));
            }
            _ => unreachable!(),
        }
    }

    #[test]
    fn test_dot_set() {
        let regex = "[.]";

        let result = parse_regex(regex, 0);

        assert!(result.is_ok());

        let result = result.unwrap().0;

        match result {
            RegEx::SimpleRegex(Term::SimpleTerm(Factor::SimpleFactor(
                Base::CharSet(set),
                None,
            ))) => {
                assert!(set.contains(&'.'));
            }
            _ => unreachable!(),
        }
    }

    #[test]
    fn test_dot_set_escaped() {
        let regex = "[\\.]";

        let result = parse_regex(regex, 0);

        assert!(result.is_ok());

        let result = result.unwrap().0;

        match result {
            RegEx::SimpleRegex(Term::SimpleTerm(Factor::SimpleFactor(
                Base::CharSet(set),
                None,
            ))) => {
                assert!(set.contains(&'.'));
            }
            _ => unreachable!(),
        }
    }

    #[test]
    fn test_negation() {
        let regex = "[^a-z]";

        let result = parse_regex(regex, 0);

        assert!(result.is_ok());

        let result = result.unwrap().0;

        match result {
            RegEx::SimpleRegex(Term::SimpleTerm(Factor::SimpleFactor(
                Base::CharSet(set),
                None,
            ))) => {
                for ch in 'a'..='z' {
                    assert!(!set.contains(&ch));
                }
                assert!(set.contains(&'A'));
            }
            _ => unreachable!(),
        }
    }

    #[test]
    fn test_not_negation() {
        let regex = "[a-z^]";

        let result = parse_regex(regex, 0);

        assert!(result.is_ok());

        let result = result.unwrap().0;

        match result {
            RegEx::SimpleRegex(Term::SimpleTerm(Factor::SimpleFactor(
                Base::CharSet(set),
                None,
            ))) => {
                for ch in 'a'..='z' {
                    assert!(set.contains(&ch));
                }
                assert!(set.contains(&'^'));
                assert!(!set.contains(&'A'));
            }
            _ => unreachable!(),
        }
    }
}
