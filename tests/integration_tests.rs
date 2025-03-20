use std::collections::HashSet;
use std::fs::File;
use std::io::{self, BufRead, BufReader};
use std::path::{Path, PathBuf};
use std::process::Command;

fn get_test_regex(filename: &str) -> io::Result<Vec<String>> {
    let path = PathBuf::from("tests").join(filename);

    let file = File::open(path);

    let file = match file {
        Ok(file) => file,
        Err(error) => panic!("Failed to open the file {:?}", error),
    };

    let reader = BufReader::new(file);

    let mut lines = Vec::new();

    for line in reader.lines() {
        let line = line;
        let line = match line {
            Ok(line) => line,
            Err(error) => panic!("Failed to read line{:?}", error),
        };
        lines.push(line);
    }
    return Ok(lines);
}

#[test]
fn fa_success_test() {
    let success_regexes = get_test_regex("success_tests.txt");
    let success_regexes = match success_regexes {
        Ok(lines) => lines,
        _ => panic!("Failed to open the test file"),
    };
    for regex in success_regexes {
        let status = Command::new("cargo")
            .args(&["run", "--", &regex])
            .status()
            .expect("Failed to run the test");
        assert!(status.success(), "Test failed to run for {regex}");

        let nfa_file = format!("{regex}_nfa.dot");
        let dfa_file = format!("{regex}_dfa.dot");

        let output_nfa_file = Path::new(&nfa_file);
        let output_dfa_file = Path::new(&dfa_file);

        assert!(
            output_nfa_file.exists(),
            "Output NFA file was not created {:?}",
            output_nfa_file
        );
        assert!(
            output_dfa_file.exists(),
            "Output DFA file was not created {:?}",
            output_dfa_file
        );

        let expected_nfa_file = PathBuf::from("./tests")
            .join("expected_output")
            .join(&nfa_file);
        let expected_dfa_file = PathBuf::from("./tests")
            .join("expected_output")
            .join(&dfa_file);

        let expected_nfa = File::open(expected_nfa_file);
        let expected_dfa = File::open(expected_dfa_file);
        let output_nfa = File::open(output_nfa_file);
        let output_dfa = File::open(output_dfa_file);

        println!("{:?}", expected_nfa);

        let expected_nfa = match expected_nfa {
            Ok(file) => file,
            Err(error) => panic!("Failed to open the file {:?}", error),
        };

        let reader = BufReader::new(expected_nfa);
        let mut nfa = HashSet::new();

        for line in reader.lines() {
            let line = match line {
                Ok(line) => line,
                Err(_) => panic!("Failed to read line file"),
            };
            nfa.insert(line);
        }
        let output_nfa = match output_nfa {
            Ok(file) => file,
            Err(error) => panic!("Failed to open the file {:?}", error),
        };

        let reader = BufReader::new(output_nfa);

        for line in reader.lines() {
            let line = match line {
                Ok(line) => line,
                Err(_) => panic!("Failed to read line file"),
            };
            nfa.remove(&line);
        }

        assert!(nfa.is_empty(), "The NFA generated for {regex} is incorrect");

        let expected_dfa = match expected_dfa {
            Ok(file) => file,
            Err(error) => panic!("Failed to open the file {:?}", error),
        };

        let reader = BufReader::new(expected_dfa);
        let mut dfa = HashSet::new();

        for line in reader.lines() {
            let line = match line {
                Ok(line) => line,
                Err(_) => panic!("Failed to read line file"),
            };
            dfa.insert(line);
        }

        let output_dfa = match output_dfa {
            Ok(file) => file,
            Err(error) => panic!("Failed to open the file {:?}", error),
        };

        let reader = BufReader::new(output_dfa);

        for line in reader.lines() {
            let line = match line {
                Ok(line) => line,
                Err(_) => panic!("Failed to read line file"),
            };
            dfa.remove(&line);
        }

        //assert!(dfa.is_empty(), "The DFA generated for {regex} is incorrect");
    }
}
