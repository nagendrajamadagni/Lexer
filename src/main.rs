use std::env;

mod dfa;
mod fa;
mod nfa;
mod reg_ex;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        panic!("Usage: {} <regexp>", args[0]);
    }
    let syntax_tree = reg_ex::build_syntax_tree(&args[1]);

    let nfa = nfa::construct_nfa(&args[1], syntax_tree);

    let _dfa = dfa::construct_dfa(nfa);
}
