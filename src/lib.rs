//! # lexviz
//!
//! A lexer library built following the text "Engineering a Compiler 2e"
//! by Keith Cooper and Linda Torczan.
//!
//! This library provides functionality to:
//! - Parse regular expressions into syntax trees
//! - Convert regular expressions to NFAs using Thompson Construction
//! - Convert NFAs to DFAs using Subset Construction
//! - Minimize DFAs using Hopcroft's Algorithm
//! - Scan and tokenize input based on the constructed automata
//! - Visualize the automata state machine

use std::{error, fmt};

// Re-export the modules
pub mod dfa;
pub mod fa;
pub mod nfa;
pub mod regex;
pub mod scanner;
pub mod visualizer;

// Re-export commonly used functions for convenience
pub use dfa::{construct_dfa, construct_minimal_dfa};
pub use nfa::construct_nfa;
pub use regex::{parse_microsyntax_list, read_microsyntax_file};
pub use scanner::{construct_scanner, load_scanner};
pub use visualizer::visualize;

// List of all possible Lexer Error Codes

#[derive(Debug)]
pub enum LexerError {
    /// Error when trying to read the microsyntax
    MicroSyntaxReadError,
    /// Error when trying to parse the regex and its category from the list of microsyntaxes
    RegexCategoryError,
    /// Error when trying read the input to scan into tokens
    InputMissingError,
    /// Error when trying to parse an option for the binary
    WrongOptionError,
    //// Error when trying to look for microsyntax file
    MissingMicrosyntaxError,
}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::MicroSyntaxReadError => {
                write!(f, "Error: Failed to read the microsyntax file provided!")
            }
            Self::RegexCategoryError => write!(
                f,
                "Error: The number of arguments provided for the microsyntax entry is incorrect!",
            ),
            Self::InputMissingError => write!(f, "Error: Input source file missing!"),
            Self::WrongOptionError => write!(f, "Error: Wrong option provided!"),
            Self::MissingMicrosyntaxError => write!(
                f,
                "Error: No microsyntax file or microsyntax list provided!"
            ),
        }
    }
}

impl error::Error for LexerError {}
