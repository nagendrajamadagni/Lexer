use crate::reg_ex::RegEx;
use clap::{Arg, Command};
use std::collections::VecDeque;

mod dfa;
mod fa;
mod nfa;
mod reg_ex;
mod scanner;

fn main() {
    let args = Command::new("lexer").version("1.0").author("Nagendra Kumar Jamadagni")
                        .about("A sample lexer built from following Engineering a Compiler by Keith Cooper and Linda Torczan")
                        .arg(Arg::new("microsyntax").short('r').value_name("[REGEX, SYNTACTIC CATEGORY]")
                        .num_args(2).action(clap::ArgAction::Append).required(true).value_parser(clap::value_parser!(String))
                        .help("Pair of regular expression and syntactic category specified by the regex. Both must be provided"))
                        .arg(Arg::new("save-nfa").short('n')
                        .help("Save the NFA after Thompson Construction of the regex")
                        .action(clap::ArgAction::SetTrue))
                        .arg(Arg::new("save-minimal-dfa").short('m')
                        .help("Save the minimal DFA after applying Hopcroft's Minimalization Algorithm")
                        .action(clap::ArgAction::SetTrue))
                        .arg(Arg::new("save-dfa").short('d')
                        .help("Save the un-optimized DFA obtained after Subset Construction of NFA")
                        .action(clap::ArgAction::SetTrue)).get_matches();

    let mut regex_list: VecDeque<(String, String)> = VecDeque::new();

    let mut token_type_priority_list: VecDeque<String> = VecDeque::new();

    if let Some(values) = args.get_occurrences::<String>("microsyntax") {
        for value_group in values {
            let value_vec: Vec<_> = value_group.collect();

            if value_vec.len() == 2 {
                regex_list.push_back((value_vec[0].clone(), value_vec[1].clone()));
            } else {
                panic!("Both regex and syntactic category should be provided");
            }

            token_type_priority_list.push_back(value_vec[1].clone());
        }
    }

    let save_nfa = args.get_flag("save-nfa");

    let save_dfa = args.get_flag("save-dfa");

    let save_minimal_dfa = args.get_flag("save-minimal-dfa");

    let mut syntax_tree_list: VecDeque<(String, RegEx, String)> = VecDeque::new();

    while !regex_list.is_empty() {
        let (regex, category) = regex_list.pop_front().unwrap();

        let syntax_tree = reg_ex::build_syntax_tree(&regex);

        syntax_tree_list.push_back((regex, syntax_tree, category));
    }

    let nfa = nfa::construct_nfa(syntax_tree_list, save_nfa);

    let dfa = dfa::construct_dfa(nfa, save_dfa);
    let dfa = dfa::construct_minimal_dfa(dfa, save_minimal_dfa);

    scanner::construct_scanner(&dfa, token_type_priority_list);
}
