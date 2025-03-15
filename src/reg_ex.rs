/* Good resource for parsing regex at
 * https://matt.might.net/articles/parsing-regex-with-recursive-descent/ */

#[derive(Debug)]
pub enum Quantifier {
    Star,
    Question,
    Plus
}

#[derive(Debug)]
pub enum Base {
    Character(char),
    EscapeCharacter(char),
    Exp(Box<RegEx>)
}

#[derive(Debug)]
pub enum Factor {
    SimpleFactor(Base, Option<Quantifier>),
}

#[derive(Debug)]
pub enum Term {
    SimpleTerm(Factor),
    ConcatTerm(Factor, Box<Term>)
}

#[derive(Debug)]
pub enum RegEx {
    SimpleRegex(Term),
    AlterRegex(Term, Box<RegEx>)
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
            _ => {}
        }
    }
    stack.is_empty()
}

fn parse_base(regex: &str, start:usize) -> (Base, usize) {
    let mut chars = regex.chars(); // (a(b|c))* start = 0
    let nchar = chars.nth(start).unwrap();
    if nchar == '(' {
        let (inner_regex, new_start) = parse_regex(regex, start + 1);
        let new_base = Base::Exp(Box::new(inner_regex));
        let new_start = new_start + 1; // Consume the rparen
        (new_base, new_start)
    }
    else if nchar == '\\' {
        let new_base = Base::EscapeCharacter(chars.nth(start + 1).unwrap());
        let new_start = start + 2;
        (new_base, new_start)
    }
    else {
        let new_base = Base::Character(nchar);
        let new_start = start + 1; // Parsed a, b, c
        (new_base, new_start)
    }

}

fn parse_factor(regex: &str, start: usize) -> (Factor, usize) {
    let (base, new_start) = parse_base(regex, start); // (a(b|c))* start = 0

    let mut new_start = new_start; // new_start = 7
    let quantifier = {
        if new_start >= regex.len() {
            None
        }
        else if regex.chars().nth(new_start).unwrap() == '*' {
            new_start += 1;
            Some(Quantifier::Star)
        }
        else if regex.chars().nth(new_start).unwrap() == '?' {
            new_start += 1;
            Some (Quantifier::Question)
        }
        else if regex.chars().nth(new_start).unwrap() == '+' {
            new_start += 1;
            Some (Quantifier::Plus)
        }
        else {
            None
        }
    };
    let term = Factor::SimpleFactor(base, quantifier);
    (term, new_start)
}

fn parse_term(regex: &str, start: usize) -> (Term, usize) {
    let (factor, mut new_start) = parse_factor(regex, start); // (a(b|c))* start = 0
    
    let mut prev_term = Term::SimpleTerm(factor);


    while new_start < regex.len() {
        let nchar = regex.chars().nth(new_start).unwrap();
        if nchar == '|' || nchar == ')' {
            break;
        }
        else {
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
    }
    else if regex.chars().nth(new_start).unwrap() == '|' {
        let (next_regex, new_start) = parse_regex(regex, new_start + 1);
        return (RegEx::AlterRegex(term, Box::new(next_regex)), new_start);
    }
    else {
        //panic!("Invalid regex provided!");
        return (RegEx::SimpleRegex(term), new_start);
    }

}

pub fn build_syntax_tree(regex: &str) -> RegEx {
    println!("Attempting to build syntax tree for {regex}");

    if !balanced_brackets(regex) {
        panic!("The provided regular expression has unbalanced parentheses");
    }

    if regex.len() == 0 {
        panic!("Empty regex provided");
    }

    let (syntax_tree, _) = parse_regex(regex, 0);
    return syntax_tree;
}
