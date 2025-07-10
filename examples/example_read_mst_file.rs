use lexviz::{
    construct_dfa, construct_minimal_dfa, construct_nfa, construct_scanner, parse_microsyntax_list,
    read_microsyntax_file,
};

fn main() {
    let regex_list = read_microsyntax_file("examples/abc.mst").unwrap();

    let syntax_tree = parse_microsyntax_list(regex_list).unwrap();

    let nfa = construct_nfa(syntax_tree, false).unwrap();

    let dfa = construct_dfa(&nfa, false);

    let minimal_dfa = construct_minimal_dfa(&dfa, false);

    let scanner = construct_scanner(&minimal_dfa);

    let token_list = scanner.scan("examples/abc", None, false, None).unwrap();

    for token in token_list {
        println!(
            "The token is {} and the category is {}",
            token.get_token(),
            token.get_category()
        );
    }
}
