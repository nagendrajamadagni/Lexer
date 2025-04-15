//! # lexer
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

// Re-export the modules
pub mod dfa;
pub mod fa;
pub mod nfa;
pub mod reg_ex;
pub mod scanner;
pub mod visualizer;

// Re-export commonly used functions for convenience
pub use dfa::{construct_dfa, construct_minimal_dfa};
pub use nfa::construct_nfa;
pub use reg_ex::{parse_microsyntax_list, read_microsyntax_file};
pub use scanner::construct_scanner;
pub use visualizer::visualize;
