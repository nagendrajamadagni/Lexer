use std::env;

mod dfa;
mod fa;
mod nfa;
mod reg_ex;
mod scanner;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        panic!("Usage: {} <regexp>", args[0]);
    }
    let syntax_tree = reg_ex::build_syntax_tree(&args[1]);

    let nfa = nfa::construct_nfa(&args[1], syntax_tree);

    let dfa = dfa::construct_dfa(nfa);
    let dfa = dfa::construct_minimal_dfa(dfa);

    scanner::construct_scanner(&dfa);
}
