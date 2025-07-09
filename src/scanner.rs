/* Implementation of Maximal Munch Table Scanner. Given a sequence of text, this scanne should
 * detect the regex specified in the micro-syntax for a given syntactic grouping. Then we output
 * the position where the lexeme was found and classify it into a syntactic grouping. */

use bitvec::vec::BitVec;

use serde::{Deserialize, Deserializer, Serialize, Serializer};

use serde_json;

use crate::dfa::DFA;
use crate::fa::{Symbol, FA};
use color_eyre::eyre::{Report, Result};
use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt;
use std::fs::File;
use std::hash::{DefaultHasher, Hash, Hasher};
use std::io::{BufReader, Read, Write};
use std::path::PathBuf;

#[derive(Debug, PartialEq, Eq)]
pub struct Token {
    token: String,
    category: String,
}

impl Token {
    pub fn new(token: String, category: String) -> Self {
        Token { token, category }
    }
    /// Get the token from the Token struct
    pub fn get_token(&self) -> &String {
        &self.token
    }
    /// Get the syntactic category to which the token belongs to
    pub fn get_category(&self) -> &String {
        &self.category
    }
}

#[derive(Debug)]
enum BufferError {
    RollbackError,
    FillError,
}

impl std::fmt::Display for BufferError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::RollbackError => write!(f, "Error: Buffer rollback failed!"),
            Self::FillError => write!(f, "Error: Buffer filling failed!"),
        }
    }
}

impl std::error::Error for BufferError {}

struct Buffer {
    source_buffer: [u8; 1024],
    input_ptr: usize,
    fence: usize,
    buf_reader: BufReader<File>,
    fill_end: usize,
}

impl Buffer {
    fn new(file_path: PathBuf) -> Result<Self> {
        let file = File::open(file_path)?;

        let buf_reader = BufReader::new(file);
        let mut buffer = Buffer {
            input_ptr: 0,
            fence: 0,
            source_buffer: [0; 1024],
            buf_reader,
            fill_end: 0,
        };

        buffer.fill_buffer(0, buffer.source_buffer.len() / 2)?;
        Ok(buffer)
    }

    fn fill_buffer(&mut self, start: usize, end: usize) -> Result<()> {
        assert!(end > start);
        assert!(end - start == self.source_buffer.len() / 2);
        let bytes_read = self.buf_reader.read(&mut self.source_buffer[start..end]);

        let bytes_read = match bytes_read {
            Ok(bytes_read) => bytes_read,
            Err(_) => {
                let err = Report::new(BufferError::FillError);
                return Err(err);
            }
        };

        if bytes_read < end - start {
            // We reached the EOF and cannot read anymore, mark the EOF
            // in the circular buffer as well.

            self.source_buffer[start + bytes_read] = 0;
        }

        self.fill_end = end % self.source_buffer.len();

        Ok(())
        //TODO create an error called fill error and propagate it
    }

    fn rollback(&mut self, amount: usize) -> Result<()> {
        if amount > self.source_buffer.len() {
            // If we try to rollback more than the buffer
            // length, throw an error
            let err = Report::new(BufferError::RollbackError);
            return Err(err);
        }

        let final_cursor_position = if amount <= self.input_ptr {
            self.input_ptr - amount
        } else {
            self.input_ptr + self.source_buffer.len() - amount
        }; // Calculate the final cursor position after rollback

        if amount > self.input_ptr && final_cursor_position <= self.fence {
            let err = Report::new(BufferError::RollbackError);
            return Err(err);
        }

        self.input_ptr = final_cursor_position;
        Ok(())
    }

    fn next_char(&mut self) -> char {
        let ch = self.source_buffer[self.input_ptr];
        let two_n = self.source_buffer.len();
        let n = two_n / 2;

        self.input_ptr = (self.input_ptr + 1) % two_n;

        if self.input_ptr == self.fill_end {
            self.fill_buffer(self.input_ptr, self.input_ptr + n)
                .unwrap();
            self.fence = (self.input_ptr + n) % two_n;
        }

        ch.into()
    }

    fn is_eof(&self) -> bool {
        let ch = self.source_buffer[self.input_ptr];

        ch == 0
    }
}

/// List of possible errors in the scanner
#[derive(Debug)]
pub enum ScannerError {
    /// Found an epsilon transition in a DFA
    EpsilonInDFA,
    /// Found a bad token which cannot be categorized in the list of syntactic categories provided
    BadToken(String),
}

impl std::fmt::Display for ScannerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ScannerError::EpsilonInDFA => write!(f, "Error: Found an epsilon transition in a DFA!"),
            ScannerError::BadToken(token) => {
                write!(f, "Error: Bad token found! {} is not a valid token!", token)
            }
        }
    }
}

impl std::error::Error for ScannerError {}

fn serialize_classifier_table<S>(
    classifier_table: &HashMap<Option<char>, usize>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    use serde::ser::SerializeMap;

    let mut ser_map = serializer.serialize_map(Some(classifier_table.len()))?;

    for (key, value) in classifier_table {
        let key_str = match key {
            Some(c) => c.to_string(),
            None => "null".to_string(),
        };
        ser_map.serialize_entry(&key_str, value)?;
    }
    ser_map.end()
}

fn deserialize_classifier_table<'de, D>(
    deserializer: D,
) -> Result<HashMap<Option<char>, usize>, D::Error>
where
    D: Deserializer<'de>,
{
    let classifier_table: HashMap<String, usize> = HashMap::deserialize(deserializer)?;

    let mut result = HashMap::new();

    for (key, value) in classifier_table {
        let key = if key == "null" {
            None
        } else if key.chars().count() == 1 {
            Some(key.chars().next().unwrap())
        } else {
            return Err(serde::de::Error::custom(format!(
                "Invalid key for Option<char>: {}",
                key
            )));
        };

        result.insert(key, value);
    }

    Ok(result)
}

#[derive(Serialize, Deserialize)]
pub struct Scanner {
    transition_table: Vec<Vec<usize>>, // Matrix of input characters and dfa states
    #[serde(
        serialize_with = "serialize_classifier_table",
        deserialize_with = "deserialize_classifier_table"
    )]
    classifier_table: HashMap<Option<char>, usize>, // Mapping from alphabet to its class id
    token_type_table: HashMap<usize, String>, // Mapping of accept state number and token type
    error_state: usize,
    accept_states: BitVec<u8>,
    start_state: usize,
}

impl Scanner {
    fn new() -> Self {
        Scanner {
            transition_table: vec![],
            classifier_table: HashMap::new(),
            token_type_table: HashMap::new(),
            error_state: 0,
            accept_states: BitVec::new(),
            start_state: 0,
        }
    }

    fn compress_init_table(&mut self, init_table: &[Vec<usize>], alphabet: &[char]) {
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

            for row in init_table.iter().take(num_rows) {
                row[col_id].hash(&mut hasher);
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

        for (row_id, row) in init_table.iter().take(num_rows).enumerate() {
            for (col_id, col) in row.iter().take(num_cols).enumerate() {
                let destination = *col;
                let char_input = alphabet.get(col_id).copied();
                let class_id = self.classifier_table.get(&char_input).unwrap();
                self.transition_table[row_id][*class_id] = destination;
            }
        }
    }

    fn init_transition_table(&mut self, dfa: &DFA) -> Result<(), ScannerError> {
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

        //for state in 0..num_states {
        for (state_id, state) in init_transition_table
            .iter_mut()
            .enumerate()
            .take(num_states)
        {
            // For all states
            let dfa_state = dfa.get_state(state_id);
            let transitions = dfa_state.get_transitions();

            for transition in transitions {
                // And all transitions for that state
                let symbol = transition.0;
                let target = transition.1;

                let symbol = match symbol {
                    Symbol::Epsilon => return Err(ScannerError::EpsilonInDFA),
                    Symbol::Char(ch) => ch,
                };

                let char_index = alphabet.binary_search(symbol).unwrap(); // Get the index in the sorted alphabet set for the character

                state[char_index] = *target; // For that state index and char
                                             // index mark the transition
                                             // state id.
            }
        }

        self.compress_init_table(&init_transition_table, &alphabet);

        self.error_state = num_states;

        self.start_state = dfa.get_start_state();

        self.accept_states = dfa.get_acceptor_states().clone();

        self.accept_states.push(false);

        Ok(())
    }

    #[allow(dead_code)]
    #[cfg(debug_assertions)]
    fn print_transition_table(&self) {
        for column_vec in self.transition_table.iter() {
            for target in column_vec {
                print!("{target}   ");
            }
            println!();
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

    fn next_word(
        &self,
        buffer: &mut Buffer,
        skip_whitespace: bool,
    ) -> Result<(String, String), ScannerError> {
        let mut state = self.start_state; // Keeps track of the current state in the DFA
        let mut lexeme = String::new();
        let mut stack: VecDeque<(usize, usize)> = VecDeque::new(); // Stack to back track after
                                                                   // overshooting the lexeme's
                                                                   // accept state
        let mut cur_pos = 0; // Keeps track of current character position in the word
        let mut last_accept_pos: i64 = -1;
        let mut last_accept_state: i64 = -1;

        let mut failed_points: HashMap<(usize, usize), bool> = HashMap::new(); // Sparse matrix for memoization of failed states
                                                                               // for early exit during lexing.
        let mut inside_string_constant = false;

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

            if ch == '"' {
                // If you encounter a double quote, toggle the fact that we are inside a
                // string constant
                inside_string_constant = !inside_string_constant;
            }

            if !inside_string_constant && skip_whitespace && ch.is_whitespace() {
                continue;
            }

            cur_pos += 1;
            lexeme.push(ch);

            let category = self.classifier_table.get(&Some(ch));

            let category = match category {
                None => self.classifier_table.get(&None).unwrap(),
                Some(category) => category,
            };

            let next_state = self.transition_table[state][*category];

            let is_accept = self.accept_states.get(next_state).unwrap();

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
            Err(ScannerError::BadToken(lexeme))
        } else {
            // Truncate lexeme to last_accept_pos size
            // Rollback buffer by same number of characters

            let last_accept: usize = last_accept_pos as usize;

            let rollback_amount = cur_pos - last_accept;

            buffer.rollback(rollback_amount).unwrap();
            lexeme.truncate(lexeme.len() - rollback_amount);

            while !stack.is_empty() {
                let (state, pos) = stack.pop_front().unwrap();

                if last_accept_pos < pos.try_into().unwrap() {
                    failed_points.insert((state, pos), true);
                }
            }

            let final_accept_state: usize = last_accept_state.try_into().unwrap();

            let category = self.token_type_table.get(&final_accept_state).unwrap();

            Ok((lexeme, category.to_string()))
        }
    }
    /// Scan the source file for tokens and accept valid tokens and categorize them. The accepted
    /// tokens are written out to the out_file. If skip_whitespace
    /// is true, whitespace tokens are skipped (except within double quotes) without throwing an error.
    /// The skip list is an optional list of syntactic categories that can be skipped without writing
    /// them out to the out_file. By default, any tokens belonging to the SKIP category are not written
    /// to the outfile.
    pub fn scan(
        &self,
        source_file: String,
        out_file: Option<String>,
        skip_whitespace: bool,
        skip_list: Option<Vec<String>>,
    ) -> Result<Vec<Token>> {
        let source_file = PathBuf::from(source_file);

        let write_to_file = out_file.is_some();

        let mut token_list: Vec<Token> = Vec::new();

        let mut buffer = Buffer::new(source_file).unwrap();

        let mut skip_set = HashSet::new();
        skip_set.insert("SKIP".to_string());

        if skip_list.is_some() {
            for elem in skip_list.unwrap() {
                skip_set.insert(elem);
            }
        }

        while !buffer.is_eof() {
            let next_word = match self.next_word(&mut buffer, skip_whitespace) {
                Ok(word) => word,
                Err(err) => {
                    let err = Report::new(err);
                    return Err(err);
                }
            };

            if skip_set.contains(&next_word.1) {
                continue;
            }

            token_list.push(Token::new(next_word.0, next_word.1));
        }
        if write_to_file {
            let mut out_file = File::create(out_file.unwrap()).unwrap();

            for token in token_list.iter() {
                let output_line = format!("({}, {})", token.token, token.category);

                writeln!(out_file, "{}", output_line).unwrap();
            }
        }
        Ok(token_list)
    }

    #[allow(dead_code)]
    #[cfg(debug_assertions)]
    fn print_classifier_table(&self) {
        println!("{:?}", self.classifier_table);
    }

    pub fn save_scanner(&self, file_name: &str) {
        let json_string = serde_json::to_string_pretty(self).unwrap();

        let mut file = File::create(file_name).unwrap();

        writeln!(file, "{}", json_string).unwrap();
    }
}
/// Construct a scanner for the provided DFA. For best performance, always provide the minized DFA.
pub fn construct_scanner(dfa: &DFA) -> Scanner {
    let mut scanner = Scanner::new();

    scanner.init_transition_table(dfa).unwrap();

    scanner.init_token_type_table(dfa);

    scanner
}

/// Load a scanner from a saved json file
pub fn load_scanner(file_name: &str) -> Result<Scanner> {
    let file = File::open(file_name)?;

    let buf_reader = BufReader::new(file);

    let scanner: Scanner = serde_json::from_reader(buf_reader).unwrap();
    Ok(scanner)
}

#[cfg(test)]
mod buffer_test_helpers {

    use super::Buffer;
    use std::path::PathBuf;

    pub fn setup_buffer() -> Buffer {
        let manifest_dir = env!("CARGO_MANIFEST_DIR");

        let mut test_file_path = PathBuf::from(manifest_dir);
        test_file_path.push("test_data/buffer_test_text.txt");

        Buffer::new(test_file_path).unwrap()
    }
}

#[cfg(test)]
mod buffer_tests {
    use crate::scanner::{buffer_test_helpers::setup_buffer, BufferError};

    #[test]
    fn test_buffer_fill() {
        let mut test_buffer = setup_buffer();
        let mut contents = String::new();

        for _ in 0..11 {
            let ch = test_buffer.next_char();
            contents.push(ch);
        }

        assert!(contents == "Lorem ipsum");
    }

    #[test]
    fn test_buffer_rollback() {
        let mut test_buffer = setup_buffer();

        let mut contents = String::new();

        for _ in 0..11 {
            test_buffer.next_char();
        }

        let rollback_result = test_buffer.rollback(5);

        assert!(rollback_result.is_ok());

        for _ in 0..5 {
            contents.push(test_buffer.next_char());
        }

        assert!(contents == "ipsum", "found, {contents}");
    }

    #[test]
    fn test_buffer_rollback_fail1() {
        let mut test_buffer = setup_buffer();

        while !test_buffer.is_eof() {
            test_buffer.next_char();
        }

        let rollback_result = test_buffer.rollback(600);

        assert!(rollback_result.is_err());

        let err = rollback_result.unwrap_err();

        match err.downcast_ref() {
            Some(BufferError::RollbackError) => {}
            _ => unreachable!(),
        }
    }

    #[test]
    fn test_buffer_rollback_fail2() {
        let mut test_buffer = setup_buffer();

        while !test_buffer.is_eof() {
            test_buffer.next_char();
        }

        let rollback_result = test_buffer.rollback(2000);

        assert!(rollback_result.is_err());

        let err = rollback_result.unwrap_err();

        match err.downcast_ref() {
            Some(BufferError::RollbackError) => {}
            _ => unreachable!(),
        }
    }

    #[test]
    fn test_buffer_rollback_buffer_bottom_edge() {
        let mut test_buffer = setup_buffer();

        let mut contents = String::new();

        let mut reread_contents = String::new();

        for _ in 0..512 {
            // Go to the edge of the buffer
            test_buffer.next_char();
        }

        for _ in 0..10 {
            contents.push(test_buffer.next_char()); // Read the 10 characters after the end of
                                                    // previous window.
        }

        let rollback_result = test_buffer.rollback(20); // Rollback by 20

        assert!(rollback_result.is_ok()); // Assert rollback succeeded

        for _ in 0..10 {
            // Come to the edge of the buffer again
            test_buffer.next_char();
        }

        for _ in 0..10 {
            // Read the 10 characters after the end of the window again.
            reread_contents.push(test_buffer.next_char());
        }

        assert!(contents == reread_contents);
    }

    #[test]
    fn test_buffer_rollback_buffer_top_edge() {
        let mut test_buffer = setup_buffer();

        let mut contents = String::new();

        let mut reread_contents = String::new();

        for _ in 0..1024 {
            // Go to the edge of the buffer
            test_buffer.next_char();
        }

        for _ in 0..10 {
            contents.push(test_buffer.next_char());
        }

        let rollback_result = test_buffer.rollback(20); // Rollback by 20

        assert!(rollback_result.is_ok()); // Assert rollback succeeded

        for _ in 0..10 {
            // Come to the edge of the buffer again
            test_buffer.next_char();
        }

        for _ in 0..10 {
            // Read the 10 characters after the end of the window again.
            reread_contents.push(test_buffer.next_char());
        }

        assert!(contents == reread_contents);
    }
}
