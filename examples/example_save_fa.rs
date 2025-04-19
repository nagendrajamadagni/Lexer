use lexviz::{
    construct_dfa, construct_minimal_dfa, construct_nfa, construct_scanner, parse_microsyntax_list,
};

use std::path::Path;

fn main() {
    let regex = "abc";
    let category = "ABC";

    let root = env!("CARGO_MANIFEST_DIR");
    let path = Path::new(root);

    let src_file_path = path.join("examples/abc").to_str().unwrap().to_string();

    let mut regex_list: Vec<(String, String)> = Vec::new();

    regex_list.push((regex.to_string(), category.to_string()));

    let syntax_tree = parse_microsyntax_list(regex_list).unwrap();

    let nfa = construct_nfa(syntax_tree, true).unwrap();

    let dfa = construct_dfa(&nfa, true);

    let minimal_dfa = construct_minimal_dfa(&dfa, true);

    let scanner = construct_scanner(&minimal_dfa);

    let token_list = scanner.scan(src_file_path, None, false, None).unwrap();

    for token in token_list {
        println!(
            "The token is {} and the category is {}",
            token.get_token(),
            token.get_category()
        );
    }
}
