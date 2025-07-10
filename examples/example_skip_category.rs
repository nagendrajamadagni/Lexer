use lexviz::{
    construct_dfa, construct_minimal_dfa, construct_nfa, construct_scanner, parse_microsyntax_list,
};

fn main() {
    let regex = "abc";
    let category = "ABC";

    let mut regex_list: Vec<(String, String)> = Vec::new();

    regex_list.push((regex.to_string(), category.to_string()));

    let regex = "def";
    let category = "DEF";

    regex_list.push((regex.to_string(), category.to_string()));

    let regex = "[ \t\r\n]+";
    let category = "WHITESPACE";

    regex_list.push((regex.to_string(), category.to_string()));

    let syntax_tree = parse_microsyntax_list(regex_list).unwrap();

    let nfa = construct_nfa(syntax_tree, false).unwrap();

    let dfa = construct_dfa(&nfa, false);

    let minimal_dfa = construct_minimal_dfa(&dfa, false);

    let scanner = construct_scanner(&minimal_dfa);

    let skip_list = vec!["ABC".to_string()];

    let token_list = scanner
        .scan("examples/abc_def", None, false, Some(skip_list))
        .unwrap();

    for token in token_list {
        println!(
            "The token is {} and the category is {}",
            token.get_token(),
            token.get_category()
        );
    }
}
