use clap::{Arg, Command};

mod dfa;
mod fa;
mod nfa;
mod reg_ex;
mod scanner;
mod visualizer;

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
                                .value_parser(clap::value_parser!(String))
                        )
                        .arg(
                            Arg::new("input")
                            .short('i')
                            .long("input")
                            .help("The program source file which should be scanned and tokenized")
                            .value_name("INPUT SOURCE FILE")
                            .value_parser(clap::value_parser!(String))
                            .required(true)
                        )
                        .arg(
                            Arg::new("output")
                            .short('o')
                            .long("output")
                            .help("The output file to store the lexer's output")
                            .value_name("OUTPUT RESULT FILE")
                            .value_parser(clap::value_parser!(String))
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
                        .arg(
                            Arg::new("skip-categories")
                            .short('s')
                            .long("skip-categories")
                            .help("Provide list of syntactic categories which can be skipped in the final lex output which is to be fed to the parser")
                            .value_name("CATEGORY")
                            .num_args(1)
                            .value_parser(clap::value_parser!(String))
                            .action(clap::ArgAction::Append)

                        )
                        .get_matches();

    let mut regex_list: Vec<(String, String)> = Vec::new();

    if let Some(mst_file_path) = args.get_one::<String>("microsyntax-file") {
        let rlist = match reg_ex::read_microsyntax_file(mst_file_path.to_string()) {
            Ok(rlist) => rlist,
            Err(error) => panic!("Error reading the microsyntax file {:?}", error),
        };
        regex_list = rlist;
    } else if let Some(values) = args.get_occurrences::<String>("microsyntax") {
        for value_group in values {
            let value_vec: Vec<_> = value_group.cloned().collect();
            regex_list.push(reg_ex::read_microsyntax(value_vec));
        }
    } else {
        panic!("Error: Either a microsyntax file or a list of microsyntaxes should be provided!");
    }

    let src_file_path = match args.get_one::<String>("input") {
        Some(file_path) => file_path.to_string(),
        None => panic!("Error: Input source file not provided!"),
    };

    let out_file_path = args.get_one::<String>("output").cloned();

    let mut skip_list: Vec<String> = Vec::new();

    if let Some(values) = args.get_many::<String>("skip-categories") {
        skip_list = values.cloned().collect();
    }

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
    };

    let syntax_tree_list = reg_ex::parse_microsyntax_list(regex_list);

    let nfa = nfa::construct_nfa(syntax_tree_list, save_nfa);

    let dfa = dfa::construct_dfa(&nfa, save_dfa);
    let minimal_dfa = dfa::construct_minimal_dfa(&dfa, save_minimal_dfa);

    let scanner = scanner::construct_scanner(&minimal_dfa);

    scanner.scan(
        src_file_path,
        out_file_path,
        skip_whitespace,
        Some(skip_list),
    );

    if visualize == "nfa" {
        visualizer::visualize(&nfa);
    } else if visualize == "dfa" {
        visualizer::visualize(&dfa);
    } else if visualize == "minimal" {
        visualizer::visualize(&minimal_dfa);
    }
}
