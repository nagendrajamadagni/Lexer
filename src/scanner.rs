/* Implementation of Maximal Munch Table Scanner. Given a sequence of text, this scanner should
 * detect the regex specified in the micro-syntax for a given syntactic grouping. Then we output
 * the position where the lexeme was found and classify it into a syntactic grouping. */

use crate::dfa::DFA;
use crate::fa::{Symbol, FA};
use std::collections::HashMap;

struct Scanner {
    transition_table: Vec<Vec<i32>>,
    classifier_table: HashMap<char, String>,
    token_type_table: HashMap<usize, String>,
}

impl Scanner {
    fn new() -> Self {
        Scanner {
            transition_table: vec![],
            classifier_table: HashMap::new(),
            token_type_table: HashMap::new(),
        }
    }

    fn init_transition_table(&mut self, dfa: &DFA) {
        // Add a column for every character in the alphabet and a row for every state in the DFA

        let mut alphabet: Vec<char> = dfa.get_alphabet().iter().cloned().collect();
        let num_states = dfa.get_num_states();

        alphabet.sort(); // Sort the alphabet so that the transition table is in order

        for _ in 0..num_states {
            let mut column_vec: Vec<i32> = Vec::new();
            for _ in 0..alphabet.len() {
                column_vec.push(-1);
            }
            self.transition_table.push(column_vec);
        }

        for state in 0..num_states {
            let dfa_state = dfa.get_state(state);
            let transitions = dfa_state.get_transitions();

            for transition in transitions {
                let symbol = transition.0;
                let target = transition.1;

                let target: i32 = *target as i32;

                let symbol = match symbol {
                    Symbol::Epsilon => panic!("Epsilon transition found in a DFA"),
                    Symbol::Char(ch) => ch,
                };

                let char_index = match alphabet.binary_search(symbol) {
                    Ok(index) => index,
                    Err(_) => panic!("Character {:?} not found in alphabet", symbol),
                };

                self.transition_table[state][char_index] = target;
            }
        }
    }

    fn print_transition_table(&self) {
        for column_vec in self.transition_table.iter() {
            for target in column_vec {
                print!("{target}   ");
            }
            println!("");
        }
    }
}

pub fn construct_scanner(dfa: &DFA) {
    let mut scanner = Scanner::new();

    scanner.init_transition_table(dfa);

    scanner.print_transition_table();
}
