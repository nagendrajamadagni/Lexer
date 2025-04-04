use crate::reg_ex::RegEx;
use clap::{Arg, Command};
use std::collections::VecDeque;
use std::fs::File;
use std::io::{self, BufRead, BufReader};
use std::path::PathBuf;

mod dfa;
mod fa;
mod nfa;
mod reg_ex;
mod scanner;
mod visualizer;

fn read_microsyntax_file(
    file_path: PathBuf,
) -> io::Result<(VecDeque<(String, String)>, VecDeque<String>)> {
    let file = File::open(file_path);
    let file = match file {
        Ok(file) => file,
        Err(error) => panic!("Error: Failed to open the microsyntax file {:?}", error),
    };
    let reader = BufReader::new(file);

    let mut regex_list: VecDeque<(String, String)> = VecDeque::new();

    let mut token_type_priority_list: VecDeque<String> = VecDeque::new();

    for (line_number, line) in reader.lines().enumerate() {
        let line = match line {
            Ok(line) => line,
            Err(error) => panic!(
                "Error: Failed to read line number {:?} in microsyntaxes file! {:?}",
                line_number, error
            ),
        };

        let content: Vec<&str> = line.split("::").collect();

        if content.len() != 2 {
            panic!("Error: Malformed microsyntax file! Each file should contain only 2 :: separated values, the regex and the syntactic category described by the regex")
        }

        let pair = (content[0].to_string(), content[1].to_string());
        regex_list.push_back(pair);

        token_type_priority_list.push_back(content[1].to_string());
    }

    Ok((regex_list, token_type_priority_list))
}

fn main() {
    let args = Command::new("lexer")
                        .version("1.0")
                        .author("Nagendra Kumar Jamadagni")
                        .about("A sample lexer built from following Engineering a Compiler by Keith Cooper and Linda Torczan")
                        .arg(
                            Arg::new("microsyntax")
                                .short('r')
                                .long("microsyntax")
                                .value_name("[REGEX, SYNTACTIC CATEGORY]")
                                .num_args(2)
                                .action(clap::ArgAction::Append)
                                .value_parser(clap::value_parser!(String))
                                .help("Pair of regular expression and syntactic category specified by the regex. Both must be provided")
                        )
                        .arg(
                            Arg::new("save-nfa")
                                .short('n')
                                .long("save-nfa")
                                .help("Save the NFA after Thompson Construction of the regex")
                                .action(clap::ArgAction::SetTrue))
                                .arg(Arg::new("save-minimal-dfa")
                                .short('m')
                                .help("Save the minimal DFA after applying Hopcroft's Minimalization Algorithm")
                                .action(clap::ArgAction::SetTrue)
                        )
                        .arg(
                            Arg::new("save-dfa")
                                .short('d')
                                .long("save-dfa")
                                .help("Save the un-optimized DFA obtained after Subset Construction of NFA")
                                .action(clap::ArgAction::SetTrue)
                        )
                        .arg(
                            Arg::new("microsyntax-file")
                                .short('f')
                                .long("microsyntax-file")
                                .help("Provide a file with a list of regular expressions and the corresponsing syntactic category name. The order of the list determines the priority of the regular expressions during token scanning")
                                .value_name("MICROSYNTAX FILE")
                                .value_parser(clap::value_parser!(PathBuf))
                        )
                        .arg(
                            Arg::new("input")
                            .short('i')
                            .long("input")
                            .help("The program source file which should be scanned and tokenized")
                            .value_name("INPUT SOURCE FILE")
                            .value_parser(clap::value_parser!(PathBuf))
                            .required(true)
                        )
                        .arg(
                            Arg::new("output")
                            .short('o')
                            .long("output")
                            .help("The output file to store the lexer's output")
                            .value_name("OUTPUT RESULT FILE")
                            .value_parser(clap::value_parser!(PathBuf))
                        )
                        .arg(
                            Arg::new("skip-whitespace")
                            .short('w')
                            .long("skip-whitespace")
                            .help("Instruct scanner to skip whitespaces if they are semantically meaningless in your language. On by default")
                            .default_value("true")
                            .value_parser(clap::value_parser!(bool))
                            .num_args(1)
                        )
                        .arg(
                            Arg::new("visualize")
                            .short('v')
                            .long("visualize")
                            .help("Visualize the finite automata graphs inside an interactive window that allows for zooming, panning and clicking of elements")
                            .value_name("DFA, NFA, MINIMAL")
                            .value_parser(clap::value_parser!(String))
                            .num_args(1)
                        )
                        .get_matches();

    let mut regex_list: VecDeque<(String, String)> = VecDeque::new();

    let mut token_type_priority_list: VecDeque<String> = VecDeque::new();

    if let Some(mst_file_path) = args.get_one::<PathBuf>("microsyntax-file") {
        if mst_file_path.exists() {
            let (rlist, _) = match read_microsyntax_file(mst_file_path.to_path_buf()) {
                Ok((rlist, plist)) => (rlist, plist),
                Err(error) => panic!("Error reading the microsyntax file {:?}", error),
            };
            regex_list = rlist;
        } else {
            panic!("Error: Provided file does not exist!");
        }
    } else if let Some(values) = args.get_occurrences::<String>("microsyntax") {
        for value_group in values {
            let value_vec: Vec<_> = value_group.collect();

            if value_vec.len() == 2 {
                regex_list.push_back((value_vec[0].clone(), value_vec[1].clone()));
            } else {
                panic!("Error: Both regex and syntactic category should be provided");
            }

            token_type_priority_list.push_back(value_vec[1].clone());
        }
    } else {
        panic!("Error: Either a microsyntax file or a list of microsyntaxes should be provided!");
    }

    let src_file_path = match args.get_one::<PathBuf>("input") {
        Some(file_path) => file_path,
        None => panic!("Error: Input source file not provided!"),
    };

    let out_file_path = match args.get_one::<PathBuf>("output") {
        Some(file_path) => file_path,
        None => {
            let in_file_stem = src_file_path.file_stem().unwrap().to_str().unwrap();
            let out_file_name = format!("{in_file_stem}.lex");
            &PathBuf::from(out_file_name)
        }
    };

    let save_nfa = args.get_flag("save-nfa");

    let save_dfa = args.get_flag("save-dfa");

    let save_minimal_dfa = args.get_flag("save-minimal-dfa");

    let skip_whitespace = args
        .get_one::<bool>("skip-whitespace")
        .copied()
        .unwrap_or(true);

    let visualize = args.get_one::<String>("visualize");

    let visualize = match visualize {
        None => "none",
        Some(str) => {
            if str.eq_ignore_ascii_case("nfa") {
                "nfa"
            } else if str.eq_ignore_ascii_case("dfa") {
                "dfa"
            } else if str.eq_ignore_ascii_case("minimal") {
                "minimal"
            } else {
                panic!("visualize should be one of NFA | DFA | MINIMAL")
            }
        }
    }

    let mut syntax_tree_list: VecDeque<(String, RegEx, String)> = VecDeque::new();

    while !regex_list.is_empty() {
        let (regex, category) = regex_list.pop_front().unwrap();

        let syntax_tree = reg_ex::build_syntax_tree(&regex);

        syntax_tree_list.push_back((regex, syntax_tree, category));
    }

    let nfa = nfa::construct_nfa(syntax_tree_list, save_nfa);

    let dfa = dfa::construct_dfa(nfa, save_dfa);
    let minimal_dfa = dfa::construct_minimal_dfa(dfa, save_minimal_dfa);

    let scanner = scanner::construct_scanner(&minimal_dfa);

    scanner.scan(
        src_file_path.to_path_buf(),
        out_file_path.to_path_buf(),
        skip_whitespace,
    );

    if visualize == "nfa" {
        visualizer::visualize(&nfa);
    } else if visualize == "dfa" {
        visualizer::visualize(&dfa);
    } else if visualize == "minimal" {
        visualizer::visualize(&minimal_dfa);
    }
}
