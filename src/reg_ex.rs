/* Good resource for parsing regex at
 * https://matt.might.net/articles/parsing-regex-with-recursive-descent/ */

use std::collections::HashSet;

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

fn balanced_brackets(reg_ex: &str) -> bool {
    let mut stack = Vec::new();

    for ch in reg_ex.chars() {
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

fn parse_char_class(regex: &str, start: usize) -> (HashSet<char>, usize) {
    let mut new_start = start;
    let mut char_set: HashSet<char> = HashSet::new();

    while new_start < regex.len() && regex.chars().nth(new_start).unwrap() != ']' {
        if regex.chars().nth(new_start + 1).unwrap() == '-' {
            let char_start = regex.chars().nth(new_start).unwrap();
            let char_end = regex.chars().nth(new_start + 2).unwrap();
            if char_end < char_start {
                panic!("Invalid character range provided");
            }
            for char in char_start..=char_end {
                char_set.insert(char);
            }
            new_start = new_start + 3;
        } else {
            char_set.insert(regex.chars().nth(new_start).unwrap());
            new_start = new_start + 1;
        }
    }

    return (char_set, new_start);
}

fn parse_base(regex: &str, start: usize) -> (Base, usize) {
    let nchar = regex.chars().nth(start);
    let nchar = match nchar {
        None => panic!("Invalid regex provided"),
        Some(nchar) => nchar,
    };
    if nchar == '(' {
        let (inner_regex, new_start) = parse_regex(regex, start + 1); // Consume the lparen
        let new_base = Base::Exp(Box::new(inner_regex));
        let new_start = new_start + 1; // Consume the rparen
        (new_base, new_start)
    } else if nchar == '[' {
        let (char_set, new_start) = parse_char_class(regex, start + 1);
        let new_start = new_start + 1; // Consume the rparen
        let new_base = Base::CharSet(char_set);
        (new_base, new_start)
    } else if nchar == '\\' {
        let new_base = Base::EscapeCharacter(regex.chars().nth(start + 1).unwrap());
        let new_start = start + 2;
        (new_base, new_start)
    } else if nchar_is_valid(nchar) {
        let new_base = Base::Character(nchar);
        let new_start = start + 1;
        (new_base, new_start)
    } else {
        panic!("Invalid regex provided!");
    }
}

fn parse_factor(regex: &str, start: usize) -> (Factor, usize) {
    let (base, new_start) = parse_base(regex, start);

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
    (term, new_start)
}

fn parse_term(regex: &str, start: usize) -> (Term, usize) {
    let (factor, mut new_start) = parse_factor(regex, start);

    let mut prev_term = Term::SimpleTerm(factor);

    while new_start < regex.len() {
        let nchar = regex.chars().nth(new_start).unwrap();
        if nchar == '|' || nchar == ')' {
            break;
        } else {
            let (next_factor, tmp_start) = parse_factor(regex, new_start);
            let next_term = Term::ConcatTerm(next_factor, Box::new(prev_term));
            prev_term = next_term;
            new_start = tmp_start;
        }
    }
    (prev_term, new_start)
}

fn parse_regex(regex: &str, start: usize) -> (RegEx, usize) {
    let (term, new_start) = parse_term(regex, start);
    if new_start >= regex.len() {
        return (RegEx::SimpleRegex(term), new_start);
    } else if regex.chars().nth(new_start).unwrap() == '|' {
        let (next_regex, new_start) = parse_regex(regex, new_start + 1);
        return (RegEx::AlterRegex(term, Box::new(next_regex)), new_start);
    } else {
        return (RegEx::SimpleRegex(term), new_start);
    }
}

pub fn build_syntax_tree(regex: &str) -> RegEx {
    if !balanced_brackets(regex) {
        panic!("The provided regular expression has unbalanced parentheses");
    }

    if regex.len() == 0 {
        panic!("Empty regex provided");
    }

    let (syntax_tree, _) = parse_regex(regex, 0);
    return syntax_tree;
}
