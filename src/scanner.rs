/* Implementation of Maximal Munch Table Scanner. Given a sequence of text, this scanner should
 * detect the regex specified in the micro-syntax for a given syntactic grouping. Then we output
 * the position where the lexeme was found and classify it into a syntactic grouping. */

use crate::dfa::DFA;
use crate::fa::{Symbol, FA};
use std::collections::{HashMap, VecDeque};
use std::hash::{DefaultHasher, Hash, Hasher};

struct Scanner {
    transition_table: Vec<Vec<usize>>, // Matrix of input characters and dfa states
    classifier_table: HashMap<Option<char>, usize>, // Mapping from alphabet to its class id
    token_type_table: HashMap<usize, String>, // Mapping of accept state number and token type
    token_type_priority_list: VecDeque<String>, // Priority list for different token types
}

impl Scanner {
    fn new(token_type_priority_list: VecDeque<String>) -> Self {
        Scanner {
            transition_table: vec![],
            classifier_table: HashMap::new(),
            token_type_table: HashMap::new(),
            token_type_priority_list,
        }
    }

    fn compress_init_table(&mut self, init_table: &Vec<Vec<usize>>, alphabet: &Vec<char>) {
        // Generate a 64 bit hash for each column based on contents
        // Map each hash with a class id
        // If 2 columns get the same hash, they will have the same class id.
        // We can use this to map the alphabet with the class id in the classifier table
        // Fill in the compressed transition table using the init table and the grouping table

        let num_cols = init_table[0].len();
        let num_rows = init_table.len();

        let mut hash_to_class_map: HashMap<u64, usize> = HashMap::new();

        for col_id in 0..num_cols {
            let mut hasher = DefaultHasher::new();

            for row_id in 0..num_rows {
                init_table[row_id][col_id].hash(&mut hasher);
            }

            let hash = hasher.finish(); // Generate the 64 bit hash for the column
            let next_class = hash_to_class_map.len(); // Map the hash with a class id
            let class_id = hash_to_class_map.entry(hash).or_insert(next_class);
            let char_input = alphabet.get(col_id).copied(); // If we are checking for "all other
                                                            // characters not in the alphabet, we
                                                            // use the None enum
            self.classifier_table.insert(char_input, *class_id);
        }

        // Add nstates number of rows and nclasses number of columns for the transition table

        let num_classes = hash_to_class_map.len();

        for _ in 0..num_rows {
            let mut column_vec: Vec<usize> = Vec::new();
            for _ in 0..num_classes {
                column_vec.push(num_classes);
            }
            self.transition_table.push(column_vec);
        }

        // Compress the transition table columns by getting the class id for each character

        for row_id in 0..num_rows {
            for col_id in 0..num_cols {
                let destination = init_table[row_id][col_id];
                let char_input = alphabet.get(col_id).copied();
                let class_id = self.classifier_table.get(&char_input).unwrap();
                self.transition_table[row_id][*class_id] = destination;
            }
        }
    }

    fn init_transition_table(&mut self, dfa: &DFA) {
        // Add a column for every character in the alphabet and a row for every state in the DFA

        let mut alphabet: Vec<char> = dfa.get_alphabet().iter().cloned().collect();
        let num_states = dfa.get_num_states();

        let mut init_transition_table: Vec<Vec<usize>> = vec![];

        alphabet.sort(); // Sort the alphabet so that the transition table is in order

        let num_chars = alphabet.len();

        for _ in 0..=num_states {
            let mut column_vec: Vec<usize> = Vec::new();
            for _ in 0..=num_chars {
                column_vec.push(num_states);
            }
            init_transition_table.push(column_vec);
        }

        for state in 0..num_states {
            // For all states
            let dfa_state = dfa.get_state(state);
            let transitions = dfa_state.get_transitions();

            for transition in transitions {
                // And all transitions for that state
                let symbol = transition.0;
                let target = transition.1;

                let symbol = match symbol {
                    Symbol::Epsilon => panic!("Epsilon transition found in a DFA"),
                    Symbol::Char(ch) => ch,
                };

                let char_index = match alphabet.binary_search(symbol) {
                    Ok(index) => index,
                    Err(_) => panic!("Character {:?} not found in alphabet", symbol),
                }; // Get the index in the sorted alphabet set for the character

                init_transition_table[state][char_index] = *target; // For that state index and char
                                                                    // index mark the transition
                                                                    // state id.
            }
        }

        self.compress_init_table(&init_transition_table, &alphabet);
    }

    fn print_transition_table(&self) {
        for column_vec in self.transition_table.iter() {
            for target in column_vec {
                print!("{target}   ");
            }
            println!("");
        }
    }

    fn init_token_type_table(&mut self, dfa: &DFA) {
        let accept_states = dfa.get_acceptor_states();

        for accept_state in accept_states.iter_ones() {
            let category = dfa.get_state(accept_state).get_category();
            self.token_type_table
                .insert(accept_state, category.to_string());
        }
    }

    fn print_token_type_table(&self) {
        for (id, category) in self.token_type_table.iter() {
            println!("The category for accept state {:?} is {:?}", id, category);
        }
    }

    fn print_priority_list(&self) {
        for token_type in self.token_type_priority_list.iter() {
            println!("{:?}", token_type);
        }
    }
}

pub fn construct_scanner(dfa: &DFA, token_type_priority_list: VecDeque<String>) {
    let mut scanner = Scanner::new(token_type_priority_list);

    scanner.init_transition_table(dfa);

    scanner.init_token_type_table(dfa);

    scanner.print_transition_table();

    scanner.print_token_type_table();

    scanner.print_priority_list();
}
