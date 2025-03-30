use clap::{Arg, Command};

mod dfa;
mod fa;
mod nfa;
mod reg_ex;
mod scanner;

fn main() {
    let args = Command::new("lexer").version("1.0").author("Nagendra Kumar Jamadagni")
                        .about("A sample lexer built from following Engineering a Compiler by Keith Cooper and Linda Torczan")
                        .arg(Arg::new("regex").short('r').value_name("REGEX")
                        .help("The regex input to build the lexer for").required(true))
                        .arg(Arg::new("save-nfa").short('n')
                        .help("Save the NFA after Thompson Construction of the regex")
                        .action(clap::ArgAction::SetTrue))
                        .arg(Arg::new("save-minimal-dfa").short('m')
                        .help("Save the minimal DFA after applying Hopcroft's Minimalization Algorithm")
                        .action(clap::ArgAction::SetTrue))
                        .arg(Arg::new("save-dfa").short('d')
                        .help("Save the un-optimized DFA obtained after Subset Construction of NFA")
                        .action(clap::ArgAction::SetTrue)).get_matches();

    let regex = args.get_one::<String>("regex").unwrap();

    let save_nfa = args.get_flag("save-nfa");

    let save_dfa = args.get_flag("save-dfa");

    let save_minimal_dfa = args.get_flag("save-minimal-dfa");

    println!(
        "The flags are {:?} {:?} {:?}",
        save_nfa, save_dfa, save_minimal_dfa
    );

    let syntax_tree = reg_ex::build_syntax_tree(regex);

    let nfa = nfa::construct_nfa(regex, syntax_tree, save_nfa);

    let dfa = dfa::construct_dfa(nfa, save_dfa);
    let dfa = dfa::construct_minimal_dfa(dfa, save_minimal_dfa);

    scanner::construct_scanner(&dfa);
}
