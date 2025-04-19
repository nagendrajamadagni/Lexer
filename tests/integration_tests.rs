mod integration_tests_helper {

    use lexviz::{
        construct_dfa, construct_minimal_dfa, construct_nfa, construct_scanner,
        parse_microsyntax_list, read_microsyntax_file,
    };

    use lexviz::scanner::{Scanner, Token};

    pub fn get_token(token: &str, category: &str) -> Token {
        Token::new(token.to_string(), category.to_string())
    }

    pub fn get_scanner(mst_path: &str) -> Scanner {
        let regex_list = read_microsyntax_file(mst_path.to_string());

        // assert that reading the file was successful
        assert!(regex_list.is_ok());

        let regex_list = regex_list.unwrap();

        let syntax_tree_list = parse_microsyntax_list(regex_list);

        // assert parsing the regex was successful
        assert!(syntax_tree_list.is_ok());

        let syntax_tree_list = syntax_tree_list.unwrap();

        let nfa = construct_nfa(syntax_tree_list, false);

        // assert that nfa construction was successful
        assert!(nfa.is_ok());

        let nfa = nfa.unwrap();

        let dfa = construct_dfa(&nfa, false);

        let dfa = construct_minimal_dfa(&dfa, false);

        let scanner = construct_scanner(&dfa);
        scanner
    }
}

mod integration_tests {
    use crate::integration_tests_helper::{get_scanner, get_token};

    use lexviz::scanner::{ScannerError, Token};

    #[test]
    fn test_valid_invalid_lex() {
        let scanner = get_scanner("test_data/sample.mst");
        let mut expected_list: Vec<Token> = Vec::new();
        expected_list.push(get_token("main", "IDENTIFIER"));
        expected_list.push(get_token("(", "LPAREN"));
        expected_list.push(get_token("input", "IDENTIFIER"));
        expected_list.push(get_token(")", "RPAREN"));
        expected_list.push(get_token("{", "LBRACE"));
        expected_list.push(get_token("add1", "KEYWORD"));
        expected_list.push(get_token("(", "LPAREN"));
        expected_list.push(get_token("sub1", "KEYWORD"));
        expected_list.push(get_token("(", "LPAREN"));
        expected_list.push(get_token("negate", "KEYWORD"));
        expected_list.push(get_token("(", "LPAREN"));
        expected_list.push(get_token("5", "NUMBER"));
        expected_list.push(get_token(")", "RPAREN"));
        expected_list.push(get_token(")", "RPAREN"));
        expected_list.push(get_token(")", "RPAREN"));
        expected_list.push(get_token(";", "TERMINATOR"));
        expected_list.push(get_token("}", "RBRACE"));

        // assert reading file was successful

        let src_file_path = "test_data/valid.snek".to_string();
        let token_list = scanner.scan(src_file_path, None, true, None);

        // assert that scanning succeeded
        assert!(token_list.is_ok());

        let token_list: Vec<Token> = token_list.unwrap();

        assert_eq!(token_list, expected_list);

        let src_file_path = "test_data/invalid.snek".to_string();
        let token_list = scanner.scan(src_file_path, None, true, None);
        assert!(token_list.is_err());

        let err = token_list.unwrap_err();

        let err = err.downcast_ref().unwrap();

        match err {
            ScannerError::BadToken(_) => assert!(true),
            _ => assert!(false),
        }
    }

    #[test]
    fn test_comments() {
        let scanner = get_scanner("test_data/comments.mst");

        let src_file_path = "test_data/comments.snek".to_string();

        let mut skip_list: Vec<String> = Vec::new();
        skip_list.push("COMMENT".to_string());

        let token_list = scanner.scan(src_file_path, None, false, Some(skip_list));
        assert!(token_list.is_ok());

        let token_list = token_list.unwrap();

        let mut expected_list: Vec<Token> = Vec::new();
        expected_list.push(get_token("add1", "KEYWORD"));
        expected_list.push(get_token(" ", "WHITESPACE"));
        expected_list.push(get_token("4", "NUMBER"));
        expected_list.push(get_token(" ", "WHITESPACE"));
        expected_list.push(get_token("\n", "WHITESPACE"));
        expected_list.push(get_token(
            "\"This is a string constant\"",
            "STRING_CONSTANT",
        ));
        expected_list.push(get_token("\n", "WHITESPACE"));
        expected_list.push(get_token(
            "\"This is a # inside a string constant\"",
            "STRING_CONSTANT",
        ));
        expected_list.push(get_token("\n", "WHITESPACE"));

        assert_eq!(token_list, expected_list);
    }

    #[test]
    fn test_empty() {
        let scanner = get_scanner("test_data/empty.mst");

        let src_file_path = "test_data/empty.snek".to_string();

        let token_list = scanner.scan(src_file_path, None, true, None);
        assert!(token_list.is_ok(),);

        let token_list = token_list.unwrap();

        let mut expected_list: Vec<Token> = Vec::new();
        expected_list.push(get_token("()", "EMPTY"));

        assert_eq!(token_list, expected_list);
    }
}
