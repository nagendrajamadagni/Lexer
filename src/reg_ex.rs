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
            _ => {}
        }
    }
    stack.is_empty()
}

fn nchar_is_valid(nchar: char) -> bool {
    match nchar {
        '*' | '|' | '?' | ')' | ']' => false,
        _ => true,
    }
}

fn parse_char_class(regex: &str, start: usize) -> Result<(HashSet<char>, usize), RegExError> {
    let mut new_start = start;
    let mut char_set: HashSet<char> = HashSet::new();

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
            new_start = new_start + 3;
        } else {
            if regex.chars().nth(new_start).unwrap() == '\\' {
                match regex.chars().nth(new_start + 1).unwrap() {
                    'n' => char_set.insert('\n'),
                    't' => char_set.insert('\t'),
                    'r' => char_set.insert('\r'),
                    '\\' => char_set.insert('\\'),
                    'C' => char_set.insert('('),
                    ')' => char_set.insert(')'),
                    '[' => char_set.insert('['),
                    ']' => char_set.insert(']'),
                    '|' => char_set.insert('|'),
                    '*' => char_set.insert('*'),
                    '+' => char_set.insert('+'),
                    '?' => char_set.insert('?'),
                    _ => {
                        return Err(RegExError::InvalidEscapeCharacter(
                            regex.chars().nth(new_start + 1).unwrap(),
                        ))
                    }
                };
                new_start = new_start + 2;
            } else {
                char_set.insert(regex.chars().nth(new_start).unwrap());
                new_start = new_start + 1;
            }
        }
    }

    return Ok((char_set, new_start));
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
    } else if nchar == '\\' {
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
    let (term, new_start) = parse_term(regex, start)?;
    if new_start >= regex.len() {
        return Ok((RegEx::SimpleRegex(term), new_start));
    } else if regex.chars().nth(new_start).unwrap() == '|' {
        let (next_regex, new_start) = parse_regex(regex, new_start + 1)?;
        return Ok((RegEx::AlterRegex(term, Box::new(next_regex)), new_start));
    } else {
        return Ok((RegEx::SimpleRegex(term), new_start));
    }
}

fn build_syntax_tree(regex: &str) -> Result<RegEx> {
    if !balanced_brackets(regex) {
        let err = Report::new(RegExError::UnbalancedParenthesisError(regex.to_string()));
        return Err(err);
    }

    if regex.len() == 0 {
        let err = Report::new(RegExError::InvalidRegexError(regex.to_string()));
        return Err(err);
    }

    let (syntax_tree, _) = parse_regex(regex, 0)?;
    return Ok(syntax_tree);
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
    return Ok(syntax_tree_list);
}
/// Parse a file containing microsyntaxes and return the parse trees
pub fn read_microsyntax_file(file_path: String) -> Result<Vec<(String, String)>, RegExError> {
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
