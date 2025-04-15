use clap::{Arg, Command};
use lexer::{
    construct_dfa, construct_minimal_dfa, construct_nfa, construct_scanner, parse_microsyntax_list,
    read_microsyntax_file, visualize,
};

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
        let rlist = match read_microsyntax_file(mst_file_path.to_string()) {
            Ok(rlist) => rlist,
            Err(error) => panic!("Error reading the microsyntax file {:?}", error),
        };
        regex_list = rlist;
    } else if let Some(values) = args.get_occurrences::<String>("microsyntax") {
        for value_group in values {
            let value_vec: Vec<_> = value_group.collect();
            if value_vec.len() == 2 {
                regex_list.push((value_vec[0].to_string(), value_vec[1].to_string()));
            } else {
                panic!("Error: Both regex and syntactic category should be provided");
            }
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

    let visualize_opt = args.get_one::<String>("visualize");

    let visualize_opt = match visualize_opt {
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

    let syntax_tree_list = parse_microsyntax_list(regex_list);

    let nfa = construct_nfa(syntax_tree_list, save_nfa);

    let dfa = construct_dfa(&nfa, save_dfa);
    let minimal_dfa = construct_minimal_dfa(&dfa, save_minimal_dfa);

    let scanner = construct_scanner(&minimal_dfa);

    let token_list = scanner.scan(
        src_file_path,
        out_file_path,
        skip_whitespace,
        Some(skip_list),
    );

    for token in token_list {
        println!(
            "The token is {} and the category is {}",
            token.get_token(),
            token.get_category()
        );
    }

    if visualize_opt == "nfa" {
        visualize(&nfa);
    } else if visualize_opt == "dfa" {
        visualize(&dfa);
    } else if visualize_opt == "minimal" {
        visualize(&minimal_dfa);
    }
}
