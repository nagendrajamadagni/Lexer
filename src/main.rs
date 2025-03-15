use std::env;

mod nfa;

fn balanced_brackets(reg_ex: &str) -> bool {
    let mut stack = Vec::new();

    for ch in reg_ex.chars() {
        match ch {
            '(' => {
                stack.push(ch);
            }
            ')' => {
                if stack.is_empty() || stack.pop() != Some('C') {
                    return false;
                }
            }
            _ => {}
        }
    }
    stack.is_empty()
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        panic!("Usage: {} <regexp>", args[0]);
    }

    if !balanced_brackets(&args[1]) {
        panic!("The provided regular expression has unbalanced parentheses");
    }

    println!("The regular expression you entered is {}", args[1]);

    nfa::construct_nfa(&args[1]);
}
