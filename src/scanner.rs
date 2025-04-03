/* Implementation of Maximal Munch Table Scanner. Given a sequence of text, this scanner should
 * detect the regex specified in the micro-syntax for a given syntactic grouping. Then we output
 * the position where the lexeme was found and classify it into a syntactic grouping. */

use bitvec::vec::BitVec;

use crate::dfa::DFA;
use crate::fa::{Symbol, FA};
use std::collections::{HashMap, VecDeque};
use std::fs::File;
use std::hash::{DefaultHasher, Hash, Hasher};
use std::io::{self, BufReader, Read, Write};
use std::path::PathBuf;

struct Buffer {
    source_buffer: [u8; 1024],
    input_ptr: usize,
    fence: usize,
    buf_reader: BufReader<File>,
}

impl Buffer {
    fn new(file_path: PathBuf) -> Self {
        let file = match File::open(file_path) {
            Ok(file) => file,
            Err(error) => panic!("Error: Could not open the file {:?}", error),
        };

        let buf_reader = BufReader::new(file);
        let mut buffer = Buffer {
            input_ptr: 0,
            fence: 0,
            source_buffer: [0; 1024],
            buf_reader,
        };

        buffer.fill_buffer(0, buffer.source_buffer.len() / 2);
        return buffer;
    }

    fn fill_buffer(&mut self, start: usize, end: usize) {
        assert!(end > start);
        assert!(end - start == self.source_buffer.len() / 2);
        let _ = match self.buf_reader.read(&mut self.source_buffer[start..end]) {
            Ok(pos) => pos,
            Err(error) => panic!("Error: Failed to read bytes from file {:?}", error),
        };
    }

    fn rollback(&mut self, amount: usize) {
        if self.input_ptr - amount <= self.fence {
            panic!("Error: Rollback failed!");
        }

        self.input_ptr = (self.input_ptr - amount) % self.source_buffer.len();
    }

    fn next_char(&mut self) -> char {
        let ch = self.source_buffer[self.input_ptr];
        let two_n = self.source_buffer.len();
        let n = two_n / 2;

        self.input_ptr = (self.input_ptr + 1) % two_n;

        if self.input_ptr % n == 0 {
            self.fill_buffer(self.input_ptr, self.input_ptr + n);
            self.fence = (self.input_ptr + n) % two_n;
        }

        ch.try_into().unwrap()
    }

    fn is_eof(&self) -> bool {
        let ch = self.source_buffer[self.input_ptr];

        ch == 0
    }
}

pub struct Scanner {
    transition_table: Vec<Vec<usize>>, // Matrix of input characters and dfa states
    classifier_table: HashMap<Option<char>, usize>, // Mapping from alphabet to its class id
    token_type_table: HashMap<usize, String>, // Mapping of accept state number and token type
    error_state: usize,
    accept_states: BitVec<u8>,
}

impl Scanner {
    fn new() -> Self {
        Scanner {
            transition_table: vec![],
            classifier_table: HashMap::new(),
            token_type_table: HashMap::new(),
            error_state: 0,
            accept_states: BitVec::new(),
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
        let num_states = dfa.get_num_states(); // The number of rows in the transition table

        let mut init_transition_table: Vec<Vec<usize>> = vec![];

        alphabet.sort(); // Sort the alphabet so that the transition table is in order

        let num_chars = alphabet.len(); // The number of columns in the transition table

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

        self.error_state = num_states;

        self.accept_states = dfa.get_acceptor_states().clone();

        self.accept_states.push(false);
    }
    #[cfg(debug_assertions)]
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
    #[cfg(debug_assertions)]
    fn print_token_type_table(&self) {
        for (id, category) in self.token_type_table.iter() {
            println!("The category for accept state {:?} is {:?}", id, category);
        }
    }

    fn next_word(&self, buffer: &mut Buffer) -> Result<(String, String), String> {
        let mut state = 0; // Keeps track of the current state in the DFA
        let mut lexeme = String::new();
        let mut stack: VecDeque<(usize, usize)> = VecDeque::new(); // Stack to back track after
                                                                   // overshooting the lexeme's
                                                                   // accept state
        let mut cur_pos = 0; // Keeps track of current character position in the word
        let mut last_accept_pos: i64 = -1;
        let mut last_accept_state: i64 = -1;

        let mut failed_points: HashMap<(usize, usize), bool> = HashMap::new(); // Sparse matrix for memoization of failed states
                                                                               // for early exit during lexing.

        stack.push_front((state, cur_pos));

        while state != self.error_state {
            // While we still haven't reached the error state

            if failed_points.contains_key(&(state, cur_pos)) {
                // Check if this state and position
                // leads to failures and exit early
                break;
            }

            if buffer.is_eof() {
                break;
            }

            let ch = buffer.next_char();

            cur_pos = cur_pos + 1;
            lexeme.push(ch);

            let category = self.classifier_table.get(&Some(ch));

            let category = match category {
                None => self.classifier_table.get(&None).unwrap(),
                Some(category) => category,
            };

            let next_state = self.transition_table[state][*category];

            let is_accept = self.accept_states.get(next_state);

            let is_accept = match is_accept {
                None => panic!("Invalid state provided"),
                Some(is_accept) => is_accept,
            };

            if *is_accept {
                last_accept_pos = cur_pos.try_into().unwrap();
                last_accept_state = next_state.try_into().unwrap();
                stack.clear();
            }

            stack.push_front((next_state, cur_pos));
            state = next_state;
        }

        // At this point we could have either found a bad token or over shot from the accept state
        // of our lexeme

        if last_accept_pos == -1 && last_accept_state == -1 {
            // We never found a good token return bad token
            let error_string = format!("Bad token found! {} is not a valid token", lexeme);
            Err(error_string)
        } else {
            // Truncate lexeme to last_accept_pos size
            // Rollback buffer by same number of characters

            let last_accept: usize = last_accept_pos as usize;

            let rollback_amount = cur_pos - last_accept;

            buffer.rollback(rollback_amount);
            lexeme.truncate(lexeme.len() - rollback_amount);

            while !stack.is_empty() {
                let (state, pos) = match stack.pop_front() {
                    Some((state, pos)) => (state, pos),
                    None => panic!("Trying to pop from an empty stack"),
                };

                if last_accept_pos < pos.try_into().unwrap() {
                    failed_points.insert((state, pos), true);
                }
            }

            let final_accept_state: usize = last_accept_state.try_into().unwrap();

            let category = match self.token_type_table.get(&final_accept_state) {
                Some(category) => category,
                None => panic!("Error: Accept state does not belong to any known category"),
            };

            Ok((lexeme, category.to_string()))
        }
    }

    pub fn scan(&self, source_file: PathBuf, out_file: PathBuf) {
        let mut buffer = Buffer::new(source_file);

        let mut out_file = match File::create(out_file) {
            Ok(file) => file,
            Err(error) => panic!("Could not create output file! {}", error),
        };

        while !buffer.is_eof() {
            let next_word = match self.next_word(&mut buffer) {
                Err(error) => panic!("{}", error),
                Ok(next_word) => next_word,
            };

            let output_line = format!("({:?}, {})", next_word.0, next_word.1);
            let _ = match writeln!(out_file, "{}", output_line) {
                Err(_) => panic!("Failed to write to output file!"),
                Ok(_) => {}
            };
        }
    }

    #[cfg(debug_assertions)]
    fn print_classifier_table(&self) {
        println!("{:?}", self.classifier_table);
    }
}

pub fn construct_scanner(dfa: &DFA) -> Scanner {
    let mut scanner = Scanner::new();

    scanner.init_transition_table(dfa);

    scanner.init_token_type_table(dfa);

    #[cfg(debug_assertions)]
    {
        scanner.print_transition_table();

        scanner.print_token_type_table();

        scanner.print_classifier_table();
    }

    return scanner;
}
