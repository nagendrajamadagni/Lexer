use bitvec::prelude::*;
use color_eyre::eyre::{Report, Result};
use petgraph::dot::Dot;
use petgraph::graph::{EdgeIndex, NodeIndex};
use petgraph::prelude::StableGraph;
use std::collections::{HashMap, HashSet, VecDeque};
use std::error;
use std::fmt;
use std::fs::File;
use std::hash::{Hash, Hasher};
use std::io::Write;
use std::process::Command;

use crate::fa::{Symbol, FA};
use crate::regex::{Base, Factor, Quantifier, RegEx, Term};

#[derive(Debug)]
pub enum NFAError {
    InvalidEscapeCharError(char),
    InvalidIndexError,
}

impl fmt::Display for NFAError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NFAError::InvalidEscapeCharError(ch) => {
                write!(f, "Error: Invalid escape character {} provided!", ch)
            }
            NFAError::InvalidIndexError => write!(f, "Error: Invalid index provided!"),
        }
    }
}

impl error::Error for NFAError {}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct NFAState {
    id: usize,
    transitions: HashMap<Symbol, HashSet<usize>>, // Store by reference is not a thing in Rust
    category: String,
}

#[derive(Debug, Clone)]
pub struct NFA {
    states: Vec<NFAState>,
    start_state: usize,
    accept_states: BitVec<u8>,
    alphabet: HashSet<char>,
    regex: String,
}

impl Hash for NFAState {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl FA for NFA {
    fn get_num_states(&self) -> usize {
        self.states.len()
    }

    fn get_start_state(&self) -> usize {
        self.start_state
    }

    fn get_alphabet(&self) -> &HashSet<char> {
        &self.alphabet
    }

    fn get_acceptor_states(&self) -> &BitVec<u8> {
        &self.accept_states
    }
    fn get_state_transitions(&self, state_id: usize) -> Vec<(&Symbol, &usize)> {
        let mut transition_list: Vec<(&Symbol, &usize)> = Vec::new();

        for transition in self.states[state_id].transitions.iter() {
            let symbol = transition.0;
            for target in transition.1.iter() {
                transition_list.push((symbol, target));
            }
        }

        transition_list
    }
}

impl NFAState {
    fn new(id: usize) -> Self {
        NFAState {
            id,
            transitions: HashMap::new(),
            category: String::new(),
        }
    }
    /// Get a list of all outgoing transitions for the given state
    pub fn get_transitions(&self) -> &HashMap<Symbol, HashSet<usize>> {
        &self.transitions
    }
    /// Get the id of the state
    pub fn get_id(&self) -> usize {
        self.id
    }
    /// Get the syntactic category that this state accepts, if it is an accept state. Otherwise, it
    /// returns an empty string.
    pub fn get_category(&self) -> &String {
        &self.category
    }
}

impl NFA {
    fn add_state(&mut self) -> usize {
        let state_id = self.states.len();
        let new_state: NFAState = NFAState::new(state_id);
        self.states.push(new_state);
        self.accept_states.push(false);
        state_id
    }

    fn show_fa(&self, filename: &str) {
        let mut stable_graph = StableGraph::new();

        let num_states = self.states.len();

        let mut edge_map: HashMap<(NodeIndex, NodeIndex), EdgeIndex> = HashMap::new();

        // Add all nodes

        for _state_idx in 0..num_states {
            stable_graph.add_node(String::new());
        }

        // Add all edges and store in map for adding labels later

        for state_idx in 0..num_states {
            let transition_list = &self.states[state_idx].transitions;

            for transition in transition_list {
                let edge_set = transition.1;

                for edge_target in edge_set {
                    if !stable_graph
                        .contains_edge(NodeIndex::new(state_idx), NodeIndex::new(*edge_target))
                    {
                        let edge_idx = stable_graph.add_edge(
                            NodeIndex::new(state_idx),
                            NodeIndex::new(*edge_target),
                            String::new(),
                        );
                        edge_map.insert(
                            (NodeIndex::new(state_idx), NodeIndex::new(*edge_target)),
                            edge_idx,
                        );
                    }
                }
            }
        }

        for state_idx in 0..num_states {
            let node_label = format!("State {}", state_idx);

            stable_graph[NodeIndex::new(state_idx)] = node_label;

            let transition_list = &self.states[state_idx].transitions;

            for transition in transition_list {
                let edge_label = match transition.0 {
                    Symbol::Char(ch) => format!("{}", ch),
                    Symbol::Epsilon => "𝛆".to_string(),
                };

                let edge_set = transition.1;

                for edge_target in edge_set {
                    let edge_idx = edge_map
                        .get(&(NodeIndex::new(state_idx), NodeIndex::new(*edge_target)))
                        .unwrap();

                    let old_label = &stable_graph[*edge_idx];
                    let new_label = if old_label.is_empty() {
                        edge_label.clone()
                    } else {
                        format!("{}, {}", old_label, edge_label)
                    };

                    stable_graph[*edge_idx] = new_label;
                }
            }
        }

        let dot = Dot::new(&stable_graph);

        // Write dot to file
        let dot_filename = format!("{}.dot", filename);
        let mut dot_file = File::create(&dot_filename).expect("Failed to create dot file");

        dot_file
            .write_all(dot.to_string().as_bytes())
            .expect("Failed to write dot file");

        Command::new("dot")
            .args(["-Tjpg", &dot_filename, "-o", &format!("{}.jpg", filename)])
            .output()
            .expect("Failed to execute Graphviz");

        println!("NFA vizualization saved as {filename}.jpg");
    }

    fn new() -> Self {
        NFA {
            states: Vec::new(),
            start_state: 0,
            accept_states: BitVec::new(),
            alphabet: HashSet::new(),
            regex: "".to_string(),
        }
    }

    fn alternation(nfa1: NFA, nfa2: NFA) -> NFA {
        let mut result = NFA::new();
        let new_start = result.add_state();

        // Copy states from NFA 1
        let offset1 = result.states.len();

        for mut state in nfa1.states {
            state.id += offset1;
            let mut new_transitions = HashMap::new();

            for (symbol, targets) in state.transitions {
                let mut new_targets = HashSet::new();

                for target in targets {
                    new_targets.insert(target + offset1);
                }
                new_transitions.insert(symbol, new_targets);
            }
            state.transitions = new_transitions;
            result.states.push(state);
            result.accept_states.push(false);
        }

        // Add epsilon transition from new start to start state of NFA1

        result.states[new_start]
            .transitions
            .entry(Symbol::Epsilon)
            .or_default()
            .insert(nfa1.start_state + offset1);

        let offset2 = result.states.len();

        for mut state in nfa2.states {
            // Copy states from NFA2
            state.id += offset2;
            let mut new_transitions = HashMap::new();

            for (symbol, targets) in state.transitions {
                let mut new_targets = HashSet::new();

                for target in targets {
                    new_targets.insert(target + offset2);
                }
                new_transitions.insert(symbol, new_targets);
            }
            state.transitions = new_transitions;
            result.states.push(state);
            result.accept_states.push(false);
        }

        // Add epsilon transition from new start to start state of NFA2

        result.states[new_start]
            .transitions
            .entry(Symbol::Epsilon)
            .or_default()
            .insert(nfa2.start_state + offset2);

        let new_accept = result.add_state();

        // Add epsilon transitions from NFA1s accept states to new accept
        let nfa1_accepts: Vec<usize> = nfa1.accept_states.iter_ones().collect();
        for accept_state in nfa1_accepts {
            result.states[accept_state + offset1]
                .transitions
                .entry(Symbol::Epsilon)
                .or_default()
                .insert(new_accept);
        }

        let nfa2_accepts: Vec<usize> = nfa2.accept_states.iter_ones().collect();

        // Add epsilon transitions from NFA2s accept states to new accept
        for accept_state in nfa2_accepts {
            result.states[accept_state + offset2]
                .transitions
                .entry(Symbol::Epsilon)
                .or_default()
                .insert(new_accept);
        }

        result.start_state = new_start;
        result.accept_states.set(new_accept, true);
        result.alphabet = nfa1.alphabet.union(&nfa2.alphabet).cloned().collect();

        result
    }

    fn closure(nfa: NFA, quantifier: Quantifier) -> NFA {
        let mut result = NFA::new();
        let new_start = result.add_state(); // Add a new start and accept state

        // Copy states from the original NFA

        let offset = result.states.len();

        for mut state in nfa.states {
            state.id += offset;
            let mut new_transitions = HashMap::new();

            for (symbol, targets) in state.transitions {
                let mut new_targets = HashSet::new();

                for target in targets {
                    new_targets.insert(target + offset);
                }
                new_transitions.insert(symbol, new_targets);
            }
            state.transitions = new_transitions;
            result.states.push(state);
            result.accept_states.push(false);
        }
        result.states[new_start]
            .transitions
            .entry(Symbol::Epsilon)
            .or_default()
            .insert(nfa.start_state + offset); // Add epsilon transitions from new start to old
                                               // start
        let new_accept = result.add_state();
        match quantifier {
            Quantifier::Star | Quantifier::Question => {
                // Add epsilon transitions from new start to new accept state

                result.states[new_start]
                    .transitions
                    .entry(Symbol::Epsilon)
                    .or_default()
                    .insert(new_accept);
            }
            _ => {}
        }

        let accept_states: Vec<usize> = nfa.accept_states.iter_ones().collect();

        for accept in accept_states {
            // Add epsilon transitions from old accept to new accept
            // and old accept and old start
            match quantifier {
                Quantifier::Star | Quantifier::Plus => {
                    result.states[accept + offset]
                        .transitions
                        .entry(Symbol::Epsilon)
                        .or_default()
                        .insert(nfa.start_state + offset);
                }
                _ => {}
            }

            result.states[accept + offset]
                .transitions
                .entry(Symbol::Epsilon)
                .or_default()
                .insert(new_accept);
        }

        result.start_state = new_start;
        result.accept_states.set(new_accept, true);
        result.alphabet = nfa.alphabet;
        result
    }

    fn concatenate(nfa1: NFA, nfa2: NFA) -> NFA {
        let mut result: NFA = NFA::new();
        let offset = nfa1.states.len();
        result.states = nfa1.states; // Clone all states from nfa1
        for _ in &nfa1.accept_states {
            result.accept_states.push(false);
        }

        // Add states and their transitions from nfa2 into the resultant nfa
        for mut state in nfa2.states {
            // For each state in NFA2
            state.id += offset; // Change their ID by offset
            let mut new_transitions = HashMap::new(); // Create new transitions

            for (symbol, targets) in state.transitions {
                let mut new_targets = HashSet::new();

                for target in targets {
                    new_targets.insert(target + offset);
                }
                new_transitions.insert(symbol, new_targets);
            }
            state.transitions = new_transitions; // Add the new transitions to new states.
            result.states.push(state); // Add the new states to the results
            result.accept_states.push(false);
        }

        // Add epsilon transitions from each acceptor state of NFA1 to start state of NFA2

        let nfa1_accepts: Vec<usize> = nfa1.accept_states.iter_ones().collect();

        for accept_id in nfa1_accepts {
            result.states[accept_id]
                .transitions
                .entry(Symbol::Epsilon)
                .or_default()
                .insert(nfa2.start_state + offset);
        }

        result.start_state = nfa1.start_state; // Make the start state of NFA1 the start state of
                                               // the result
        let nfa2_accepts: Vec<usize> = nfa2.accept_states.iter_ones().collect();

        let accept_states: Vec<usize> = nfa2_accepts.into_iter().map(|s| s + offset).collect(); // Make the accept states of NFA2 the accept
                                                                                                // states of the result
        for accept in accept_states {
            result.accept_states.set(accept, true);
        }

        result.alphabet = nfa1.alphabet.union(&nfa2.alphabet).cloned().collect();
        result
    }

    fn literal_construction(character: char) -> NFA {
        let mut result: NFA = NFA::new();
        let start_state = result.add_state();
        let end_state = result.add_state();

        result.alphabet.insert(character);
        result.states[start_state]
            .transitions
            .entry(Symbol::Char(character))
            .or_default()
            .insert(end_state);

        result.start_state = start_state;
        result.accept_states.set(end_state, true);
        result
    }

    fn escape_literal_construction(character: char) -> Result<NFA, NFAError> {
        let mut result: NFA = NFA::new();
        let start_state = result.add_state();
        let end_state = result.add_state();

        let escape_character = match character {
            'n' => '\n',
            't' => '\t',
            'r' => '\r',
            '\\' => '\\',
            '(' => '(',
            ')' => ')',
            '[' => '[',
            ']' => ']',
            '{' => '{',
            '}' => '}',
            '|' => '|',
            '*' => '*',
            '+' => '+',
            '?' => '?',
            '.' => '.',
            _ => return Err(NFAError::InvalidEscapeCharError(character)),
        };

        result.alphabet.insert(escape_character);
        result.states[start_state]
            .transitions
            .entry(Symbol::Char(escape_character))
            .or_default()
            .insert(end_state);

        result.start_state = start_state;
        result.accept_states.set(end_state, true);

        Ok(result)
    }
    /// Get the state for the provided id
    pub fn get_state(&self, id: usize) -> Result<&NFAState, NFAError> {
        let state = self.states.get(id);
        match state {
            Some(state) => Ok(state),
            None => Err(NFAError::InvalidIndexError),
        }
    }

    fn set_accept_category(&mut self, category: String) -> Result<(), NFAError> {
        let accept_states = self.accept_states.clone();

        for state in accept_states.iter_ones() {
            let state = match self.states.get_mut(state) {
                Some(state) => state,
                None => return Err(NFAError::InvalidIndexError),
            };
            let old_category = &state.category;
            if old_category.is_empty() {
                state.category = category.clone();
            }
        }
        Ok(())
    }
    /// Get the regular expression that the NFA models
    pub fn get_regex(&self) -> &String {
        &self.regex
    }
}

fn parse_base_tree(tree: Base) -> Result<NFA> {
    match tree {
        Base::Character(character) => Ok(NFA::literal_construction(character)),
        Base::EscapeCharacter(character) => match NFA::escape_literal_construction(character) {
            Ok(character) => Ok(character),
            Err(err) => {
                let err = Report::new(err);
                Err(err)
            }
        },
        Base::Exp(regex) => {
            let regex = *regex;
            parse_regex_tree(regex)
        }
        Base::CharSet(char_set) => {
            let next_char = char_set.iter().next().unwrap();
            let mut result = NFA::literal_construction(*next_char);
            for char in char_set {
                let char_nfa = NFA::literal_construction(char);
                result = NFA::alternation(char_nfa, result);
            }
            Ok(result)
        }
    }
}

fn parse_factor_tree(tree: Factor) -> Result<NFA> {
    match tree {
        Factor::SimpleFactor(base, quantifier) => {
            let nfa = parse_base_tree(base)?;
            match quantifier {
                None => Ok(nfa),
                Some(Quantifier::Star) | Some(Quantifier::Plus) | Some(Quantifier::Question) => {
                    Ok(NFA::closure(nfa, quantifier.unwrap()))
                }
                Some(Quantifier::Exact(num)) => {
                    let mut result = nfa.clone();
                    for _ in 1..num {
                        result = NFA::concatenate(result, nfa.clone());
                    }
                    Ok(result)
                }
                Some(Quantifier::Atleast(num)) => {
                    let mut result = nfa.clone();
                    for _ in 1..num {
                        // Concatenate atleast num times
                        result = NFA::concatenate(result, nfa.clone());
                    }

                    // Create a closure of 0 or more times
                    let nfa_star = NFA::closure(nfa.clone(), Quantifier::Star);

                    // After atleast num times concatenate the 0 or more times to the result
                    result = NFA::concatenate(result, nfa_star);
                    Ok(result)
                }
                Some(Quantifier::Atmost(num)) => {
                    // Create a 0 or 1 closure of the NFA
                    let question = NFA::closure(nfa, Quantifier::Question);

                    // Repeat the 0 or 1 closure num times
                    let mut result = question.clone();

                    for _ in 1..num {
                        result = NFA::concatenate(result, question.clone());
                    }

                    Ok(result)
                }
                Some(Quantifier::Range(start, end)) => {
                    let mut least = nfa.clone();

                    for _ in 1..start {
                        // Concatenate atleast start number of times
                        least = NFA::concatenate(least, nfa.clone());
                    }

                    // Repeat the 0 or 1 closure end - start times

                    let question = NFA::closure(nfa, Quantifier::Question);

                    let mut most = question.clone();

                    for _ in 1..(end - start) {
                        most = NFA::concatenate(most, question.clone());
                    }

                    // Concatenate the least and the optional number of times to get least and most
                    // range

                    let result = NFA::concatenate(least, most);

                    Ok(result)
                }
            }
        }
    }
}

fn parse_term_tree(tree: Term) -> Result<NFA> {
    match tree {
        Term::SimpleTerm(factor) => parse_factor_tree(factor),
        Term::ConcatTerm(rfactor, lterm) => {
            let lterm = *lterm;
            let nfa1 = parse_term_tree(lterm)?;
            let nfa2 = parse_factor_tree(rfactor)?;
            Ok(NFA::concatenate(nfa1, nfa2))
        }
    }
}

fn parse_regex_tree(tree: RegEx) -> Result<NFA> {
    match tree {
        RegEx::SimpleRegex(term) => parse_term_tree(term),
        RegEx::AlterRegex(lterm, rregex) => {
            let rregex = *rregex; // Unboxing the value
            let nfa1 = parse_term_tree(lterm)?;
            let nfa2 = parse_regex_tree(rregex)?;
            Ok(NFA::alternation(nfa1, nfa2))
        }
    }
}

/// Apply Thomson construction algorithm to build an NFA for a given regular expression syntax
/// tree. If save_nfa is set to true, the constructed NFA is saved as a jpg.
pub fn construct_nfa(
    mut syntax_tree_list: VecDeque<(String, RegEx, String)>,
    save_nfa: bool,
) -> Result<NFA> {
    let (regex, syntax_tree, category) = syntax_tree_list.pop_front().unwrap();

    let mut result = parse_regex_tree(syntax_tree)?;
    result.regex = regex;

    result.set_accept_category(category).unwrap();

    while !syntax_tree_list.is_empty() {
        let (regex, syntax_tree, category) = syntax_tree_list.pop_front().unwrap();
        let mut nfa = parse_regex_tree(syntax_tree)?;
        nfa.regex = regex.clone();
        nfa.set_accept_category(category).unwrap();
        let old_regex = result.regex.clone();
        result = NFA::alternation(result, nfa);
        let new_regex = format!("{old_regex}|{regex}");
        result.regex = new_regex;
    }
    if save_nfa {
        let filename = "constructed_nfa";
        result.show_fa(filename);
    }

    Ok(result)
}

#[cfg(test)]
mod nfa_tests {
    use super::*;

    #[test]
    fn test_nfa_state_creation() {
        let state = NFAState::new(1);
        assert_eq!(state.get_id(), 1);
        assert_eq!(state.get_transitions().len(), 0);
        assert_eq!(state.get_category(), "");
    }

    #[test]
    fn test_nfa_basic_construction() {
        let mut nfa = NFA::new();
        let start = nfa.add_state();
        let end = nfa.add_state();

        assert_eq!(nfa.get_num_states(), 2);
        assert_eq!(nfa.get_start_state(), 0);
        assert_eq!(nfa.get_acceptor_states().len(), 2);
        assert!(!nfa.get_acceptor_states()[end]);
        assert!(!nfa.get_acceptor_states()[start]);

        // Mark end as accept state
        nfa.accept_states.set(end, true);
        assert!(nfa.get_acceptor_states()[end]);
    }

    #[test]
    fn test_literal_construction() {
        let nfa = NFA::literal_construction('a');

        assert_eq!(nfa.get_num_states(), 2);
        assert_eq!(nfa.get_start_state(), 0);
        assert_eq!(nfa.get_alphabet().len(), 1);
        assert!(nfa.get_alphabet().contains(&'a'));

        // Check transitions
        let transitions = nfa.get_state_transitions(0);
        assert_eq!(transitions.len(), 1);
        assert_eq!(*transitions[0].0, Symbol::Char('a'));
        assert_eq!(*transitions[0].1, 1);

        // Check accept state
        assert!(nfa.get_acceptor_states()[1]);
        assert!(!nfa.get_acceptor_states()[0]);
    }

    #[test]
    fn test_escape_literal_construction() {
        let nfa = NFA::escape_literal_construction('n').unwrap();

        assert_eq!(nfa.get_num_states(), 2);
        assert!(nfa.get_alphabet().contains(&'\n'));

        // Test invalid escape character
        let result = NFA::escape_literal_construction('x');
        assert!(result.is_err());
        match result {
            Err(NFAError::InvalidEscapeCharError(c)) => assert_eq!(c, 'x'),
            _ => panic!("Expected InvalidEscapeCharError"),
        }
    }

    #[test]
    fn test_concatenation() {
        let nfa1 = NFA::literal_construction('a');
        let nfa2 = NFA::literal_construction('b');

        let concat_nfa = NFA::concatenate(nfa1, nfa2);

        assert_eq!(concat_nfa.get_num_states(), 4);
        assert_eq!(concat_nfa.get_start_state(), 0);
        assert_eq!(concat_nfa.get_alphabet().len(), 2);
        assert!(concat_nfa.get_alphabet().contains(&'a'));
        assert!(concat_nfa.get_alphabet().contains(&'b'));

        // Check that the original accept state of nfa1 has an epsilon transition to nfa2's start
        let transitions = concat_nfa.get_state_transitions(1);
        let has_epsilon_to_nfa2_start = transitions
            .iter()
            .any(|(sym, target)| **sym == Symbol::Epsilon && **target == 2);
        assert!(has_epsilon_to_nfa2_start);

        // Check that only nfa2's accept state is now an accept state
        assert!(!concat_nfa.get_acceptor_states()[0]);
        assert!(!concat_nfa.get_acceptor_states()[1]);
        assert!(!concat_nfa.get_acceptor_states()[2]);
        assert!(concat_nfa.get_acceptor_states()[3]);
    }

    #[test]
    fn test_alternation() {
        let nfa1 = NFA::literal_construction('a');
        let nfa2 = NFA::literal_construction('b');

        let alt_nfa = NFA::alternation(nfa1, nfa2);

        // Should have: new start + 2 from nfa1 + 2 from nfa2 + new accept = 6 states
        assert_eq!(alt_nfa.get_num_states(), 6);

        // Check that new start state has epsilon transitions to both nfa start states
        let start_transitions = alt_nfa.get_state_transitions(0);
        assert_eq!(start_transitions.len(), 2);

        // Check that both original accept states have epsilon transitions to new accept
        let nfa1_accept_transitions = alt_nfa.get_state_transitions(2);
        let nfa2_accept_transitions = alt_nfa.get_state_transitions(4);

        let nfa1_to_new_accept = nfa1_accept_transitions
            .iter()
            .any(|(sym, target)| **sym == Symbol::Epsilon && **target == 5);
        let nfa2_to_new_accept = nfa2_accept_transitions
            .iter()
            .any(|(sym, target)| **sym == Symbol::Epsilon && **target == 5);

        assert!(nfa1_to_new_accept);
        assert!(nfa2_to_new_accept);

        // Check that only the new accept state is marked as an accept state
        let accept_states: Vec<usize> = alt_nfa.get_acceptor_states().iter_ones().collect();
        assert_eq!(accept_states, vec![5]);
    }

    #[test]
    fn test_closure_star() {
        let nfa = NFA::literal_construction('a');
        let star_nfa = NFA::closure(nfa, Quantifier::Star);

        // Should have new start + 2 from original + new accept = 4 states
        assert_eq!(star_nfa.get_num_states(), 4);

        // Start state should have epsilon to original start and to new accept (for empty match)
        let start_transitions = star_nfa.get_state_transitions(0);
        let to_original_start = start_transitions
            .iter()
            .any(|(sym, target)| **sym == Symbol::Epsilon && **target == 1);
        let to_new_accept = start_transitions
            .iter()
            .any(|(sym, target)| **sym == Symbol::Epsilon && **target == 3);

        assert!(to_original_start);
        assert!(to_new_accept);

        // Original accept should have epsilon to original start (for repetition) and new accept
        let original_accept_transitions = star_nfa.get_state_transitions(2);
        let back_to_start = original_accept_transitions
            .iter()
            .any(|(sym, target)| **sym == Symbol::Epsilon && **target == 1);
        let to_new_accept_from_original = original_accept_transitions
            .iter()
            .any(|(sym, target)| **sym == Symbol::Epsilon && **target == 3);

        assert!(back_to_start);
        assert!(to_new_accept_from_original);
    }

    #[test]
    fn test_closure_plus() {
        let nfa = NFA::literal_construction('a');
        let plus_nfa = NFA::closure(nfa, Quantifier::Plus);

        // Should have new start + 2 from original + new accept = 4 states
        assert_eq!(plus_nfa.get_num_states(), 4);

        // Start state should have epsilon to original start but NOT to new accept (at least one match required)
        let start_transitions = plus_nfa.get_state_transitions(0);
        let to_original_start = start_transitions
            .iter()
            .any(|(sym, target)| **sym == Symbol::Epsilon && **target == 1);
        let to_new_accept = start_transitions
            .iter()
            .any(|(sym, target)| **sym == Symbol::Epsilon && **target == 3);

        assert!(to_original_start);
        assert!(!to_new_accept);

        // Original accept should have epsilon to original start (for repetition) and new accept
        let original_accept_transitions = plus_nfa.get_state_transitions(2);
        let back_to_start = original_accept_transitions
            .iter()
            .any(|(sym, target)| **sym == Symbol::Epsilon && **target == 1);
        let to_new_accept_from_original = original_accept_transitions
            .iter()
            .any(|(sym, target)| **sym == Symbol::Epsilon && **target == 3);

        assert!(back_to_start);
        assert!(to_new_accept_from_original);
    }

    #[test]
    fn test_closure_question() {
        let nfa = NFA::literal_construction('a');
        let question_nfa = NFA::closure(nfa, Quantifier::Question);

        // Should have new start + 2 from original + new accept = 4 states
        assert_eq!(question_nfa.get_num_states(), 4);

        // Start state should have epsilon to original start and to new accept (for optional match)
        let start_transitions = question_nfa.get_state_transitions(0);
        let to_original_start = start_transitions
            .iter()
            .any(|(sym, target)| **sym == Symbol::Epsilon && **target == 1);
        let to_new_accept = start_transitions
            .iter()
            .any(|(sym, target)| **sym == Symbol::Epsilon && **target == 3);

        assert!(to_original_start);
        assert!(to_new_accept);

        // Original accept should have epsilon to new accept but NOT back to original start
        let original_accept_transitions = question_nfa.get_state_transitions(2);
        let back_to_start = original_accept_transitions
            .iter()
            .any(|(sym, target)| **sym == Symbol::Epsilon && **target == 1);
        let to_new_accept_from_original = original_accept_transitions
            .iter()
            .any(|(sym, target)| **sym == Symbol::Epsilon && **target == 3);

        assert!(!back_to_start);
        assert!(to_new_accept_from_original);
    }

    #[test]
    fn test_get_state() {
        let mut nfa = NFA::new();
        let s0 = nfa.add_state();
        let s1 = nfa.add_state();

        let state0 = nfa.get_state(s0).unwrap();
        let state1 = nfa.get_state(s1).unwrap();

        assert_eq!(state0.get_id(), 0);
        assert_eq!(state1.get_id(), 1);

        let result = nfa.get_state(99);
        assert!(result.is_err());
        match result {
            Err(NFAError::InvalidIndexError) => {}
            _ => panic!("Expected InvalidIndexError"),
        }
    }

    #[test]
    fn test_set_accept_category() {
        let mut nfa = NFA::new();
        let s0 = nfa.add_state();
        let s1 = nfa.add_state();

        nfa.accept_states.set(s1, true);

        let result = nfa.set_accept_category("IDENTIFIER".to_string());
        assert!(result.is_ok());

        let state1 = nfa.get_state(s1).unwrap();
        assert_eq!(state1.get_category(), "IDENTIFIER");

        // Non-accept states shouldn't be affected
        let state0 = nfa.get_state(s0).unwrap();
        assert_eq!(state0.get_category(), "");
    }

    #[test]
    fn test_fa_trait_implementation() {
        let nfa = NFA::literal_construction('a');

        assert_eq!(nfa.get_num_states(), 2);
        assert_eq!(nfa.get_start_state(), 0);
        assert!(nfa.get_alphabet().contains(&'a'));

        let accept_states: Vec<usize> = nfa.get_acceptor_states().iter_ones().collect();
        assert_eq!(accept_states, vec![1]);

        let transitions = nfa.get_state_transitions(0);
        assert_eq!(transitions.len(), 1);
        assert_eq!(*transitions[0].0, Symbol::Char('a'));
        assert_eq!(*transitions[0].1, 1);
    }

    #[test]
    fn test_complex_regex_construction() {
        // Test parsing and constructing an NFA for a simple regex like "a(b|c)*d"
        // This would require integration with your regex parser, so let's simulate it

        // First create NFAs for the components
        let nfa_a = NFA::literal_construction('a');
        let nfa_b = NFA::literal_construction('b');
        let nfa_c = NFA::literal_construction('c');
        let nfa_d = NFA::literal_construction('d');

        // Create b|c
        let nfa_b_or_c = NFA::alternation(nfa_b, nfa_c);

        // Create (b|c)*
        let nfa_b_or_c_star = NFA::closure(nfa_b_or_c, Quantifier::Star);

        // Create a(b|c)*
        let nfa_a_bc_star = NFA::concatenate(nfa_a, nfa_b_or_c_star);

        // Create a(b|c)*d
        let nfa_final = NFA::concatenate(nfa_a_bc_star, nfa_d);

        // Verify the structure
        assert!(nfa_final.get_alphabet().contains(&'a'));
        assert!(nfa_final.get_alphabet().contains(&'b'));
        assert!(nfa_final.get_alphabet().contains(&'c'));
        assert!(nfa_final.get_alphabet().contains(&'d'));

        // The exact number would depend on how your alternation and closure work
        // but we can at least check it's of a reasonable size
        assert!(nfa_final.get_num_states() > 8); // Rough minimum estimate

        // Only one accept state at the end
        let accept_count = nfa_final.get_acceptor_states().count_ones();
        assert_eq!(accept_count, 1);
    }
}
