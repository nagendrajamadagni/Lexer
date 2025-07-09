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

        construct_scanner(&dfa)
    }
}

mod integration_tests {
    use crate::integration_tests_helper::{get_scanner, get_token};

    use lexviz::scanner::{load_scanner, ScannerError, Token};

    #[test]
    fn test_valid_invalid_lex() {
        let scanner = get_scanner("test_data/sample.mst");
        let expected_list: Vec<Token> = vec![
            get_token("main", "IDENTIFIER"),
            get_token("(", "LPAREN"),
            get_token("input", "IDENTIFIER"),
            get_token(")", "RPAREN"),
            get_token("{", "LBRACE"),
            get_token("add1", "KEYWORD"),
            get_token("(", "LPAREN"),
            get_token("sub1", "KEYWORD"),
            get_token("(", "LPAREN"),
            get_token("negate", "KEYWORD"),
            get_token("(", "LPAREN"),
            get_token("5", "NUMBER"),
            get_token(")", "RPAREN"),
            get_token(")", "RPAREN"),
            get_token(")", "RPAREN"),
            get_token(";", "TERMINATOR"),
            get_token("}", "RBRACE"),
        ];

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
            ScannerError::BadToken(_) => {}
            _ => panic!("Expeced bad token!"),
        }
    }

    #[test]
    fn test_comments() {
        let scanner = get_scanner("test_data/comments.mst");

        let src_file_path = "test_data/comments.snek".to_string();

        let skip_list: Vec<String> = vec!["COMMENT".to_string()];

        let token_list = scanner.scan(src_file_path, None, false, Some(skip_list));
        assert!(token_list.is_ok());

        let token_list = token_list.unwrap();

        let expected_list: Vec<Token> = vec![
            get_token("add1", "KEYWORD"),
            get_token(" ", "WHITESPACE"),
            get_token("4", "NUMBER"),
            get_token(" ", "WHITESPACE"),
            get_token("\n", "WHITESPACE"),
            get_token("\"This is a string constant\"", "STRING_CONSTANT"),
            get_token("\n", "WHITESPACE"),
            get_token(
                "\"This is a # inside a string constant\"",
                "STRING_CONSTANT",
            ),
            get_token("\n", "WHITESPACE"),
        ];

        assert_eq!(token_list, expected_list);
    }

    #[test]
    fn test_empty() {
        let scanner = get_scanner("test_data/empty.mst");

        let src_file_path = "test_data/empty.snek".to_string();

        let token_list = scanner.scan(src_file_path, None, true, None);
        assert!(token_list.is_ok(),);

        let token_list = token_list.unwrap();

        let expected_list: Vec<Token> = vec![get_token("()", "EMPTY")];

        assert_eq!(token_list, expected_list);
    }

    #[test]
    fn test_atleast_atmost() {
        let scanner = get_scanner("test_data/atleast_atmost.mst");

        let src_file_path = "test_data/atleast_atmost.snek".to_string();

        let skip_list = vec!["NEWLINE".to_string()];

        let token_list = scanner.scan(src_file_path, None, false, Some(skip_list));

        assert!(token_list.is_ok());

        let token_list = token_list.unwrap();

        let expected_list: Vec<Token> = vec![
            get_token("aaaaa", "ATLEAST_FIVE_A"),
            get_token("aaaa", "ATMOST_FIVE_A"),
            get_token("aaaaaa", "ATLEAST_FIVE_A"),
        ];

        assert_eq!(token_list, expected_list);
    }

    #[test]
    fn test_range() {
        let scanner = get_scanner("test_data/range.mst");

        let src_file_path = "test_data/range.snek".to_string();

        let skip_list = vec!["NEWLINE".to_string()];

        let token_list = scanner.scan(src_file_path, None, false, Some(skip_list));

        assert!(token_list.is_ok());

        let token_list = token_list.unwrap();

        let expected_list: Vec<Token> = vec![
            get_token("aaaaa", "THREE_FIVE_A"),
            get_token("aaa", "THREE_FIVE_A"),
            get_token("aaaa", "THREE_FIVE_A"),
        ];

        assert_eq!(token_list, expected_list);
    }

    #[test]
    fn test_range_invalid() {
        let scanner = get_scanner("test_data/range_invalid.mst");

        let src_file_path = "test_data/range_invalid.snek".to_string();

        let skip_list = vec!["NEWLINE".to_string()];

        let token_list = scanner.scan(src_file_path, None, false, Some(skip_list));

        assert!(token_list.is_err());

        match token_list.unwrap_err().downcast_ref().unwrap() {
            ScannerError::BadToken(_) => {}
            _ => panic!("Expected Bad Token Error!"),
        }
    }

    #[test]
    fn test_dot_operator() {
        let scanner = get_scanner("test_data/everything.mst");

        let src_file_path = "test_data/everything.snek".to_string();

        let token_list = scanner.scan(src_file_path, None, true, None);

        assert!(token_list.is_ok());

        let token_list = token_list.unwrap();

        let expected_list: Vec<Token> = vec![
            get_token("a", "EVERYTHING"),
            get_token("b", "EVERYTHING"),
            get_token("c", "EVERYTHING"),
        ];

        assert_eq!(token_list, expected_list);
    }

    #[test]
    fn test_save_load_scanner() {
        let scanner = get_scanner("test_data/everything.mst");

        let result = scanner.save_scanner("test_data/everything_scanner.scn");

        assert!(result.is_ok());

        let scanner = load_scanner("test_data/everything_scanner.scn");

        assert!(scanner.is_ok());

        let scanner = scanner.unwrap();

        let src_file_path = "test_data/everything.snek".to_string();

        let token_list = scanner.scan(src_file_path, None, true, None);

        assert!(token_list.is_ok());

        let token_list = token_list.unwrap();

        let expected_list: Vec<Token> = vec![
            get_token("a", "EVERYTHING"),
            get_token("b", "EVERYTHING"),
            get_token("c", "EVERYTHING"),
        ];

        assert_eq!(token_list, expected_list);
    }
}
