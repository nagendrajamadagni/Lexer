use bitvec::prelude::*;
use petgraph::dot::Dot;
use petgraph::graph::DiGraph;
use std::collections::{HashMap, HashSet, VecDeque};
use std::fs::File;
use std::hash::{Hash, Hasher};
use std::io::Write;
use std::process::Command;

use crate::fa::{Symbol, FA};
use crate::reg_ex::{Base, Factor, Quantifier, RegEx, Term};

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct NFAState {
    id: usize,
    transitions: HashMap<Symbol, HashSet<usize>>, // Store by reference is not a thing in Rust
    category: String,
}

#[derive(Debug)]
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
        return &self.alphabet;
    }

    fn get_acceptor_states(&self) -> &BitVec<u8> {
        return &self.accept_states;
    }
    fn get_state_transitions(&self, state_id: usize) -> Vec<(&Symbol, &usize)> {
        let mut transition_list: Vec<(&Symbol, &usize)> = Vec::new();

        for transition in self.states[state_id].transitions.iter() {
            let symbol = transition.0;
            for target in transition.1.iter() {
                transition_list.push((symbol, target));
            }
        }

        return transition_list;
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
        return self.id;
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
        return state_id;
    }

    fn show_fa(&self, filename: &str) {
        let mut graph = DiGraph::new();
        let mut node_map = std::collections::HashMap::new();

        // Add nodes
        for state in &self.states {
            let node = graph.add_node(format!("State {}", state.id));
            node_map.insert(state.id, node);
        }

        // Add edges
        for state in &self.states {
            for (symbol, targets) in &state.transitions {
                for target in targets {
                    let symbol_str = match symbol {
                        Symbol::Char(c) => c.to_string(),
                        Symbol::Epsilon => "𝛆".to_string(),
                    };
                    graph.add_edge(node_map[&state.id], node_map[&target], symbol_str);
                }
            }
        }

        // Mark Start and Accept States

        let start_node = node_map[&self.start_state];
        graph[start_node] = format!("Start\nState {}", self.start_state);

        let accept_states: Vec<usize> = self.accept_states.iter_ones().collect();

        for accept in accept_states {
            let accept_node = node_map[&accept];
            graph[accept_node] = format!("State {}\nAccept", accept);
        }

        let dot = Dot::new(&graph);

        // Write dot to file
        let dot_filename = format!("{}.dot", filename);
        let mut dot_file = File::create(&dot_filename).expect("Failed to create dot file");

        dot_file
            .write_all(dot.to_string().as_bytes())
            .expect("Failed to write dot file");

        Command::new("dot")
            .args(&["-Tjpg", &dot_filename, "-o", &format!("{}.jpg", filename)])
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

        return result;
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
            Quantifier::Plus => {}
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
        return result;
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
        return result;
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
        return result;
    }

    fn escape_literal_construction(character: char) -> NFA {
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
            '|' => '|',
            '*' => '*',
            '+' => '+',
            '?' => '?',
            _ => panic!("Invalid escape cahracter found {}!", character),
        };

        result.alphabet.insert(escape_character);
        result.states[start_state]
            .transitions
            .entry(Symbol::Char(escape_character))
            .or_default()
            .insert(end_state);

        result.start_state = start_state;
        result.accept_states.set(end_state, true);

        return result;
    }
    /// Get the state for the provided id
    pub fn get_state(&self, id: usize) -> &NFAState {
        let state = self.states.get(id);
        match state {
            Some(state) => state,
            None => panic!("Invalid state index provided"),
        }
    }

    fn set_accept_category(&mut self, category: String) {
        let accept_states = self.accept_states.clone();

        for state in accept_states.iter_ones() {
            let state = match self.states.get_mut(state) {
                Some(state) => state,
                None => panic!("Invalid state index provided"),
            };
            let old_category = &state.category;
            if old_category.is_empty() {
                state.category = category.clone();
            }
        }
    }
    /// Get the regular expression that the NFA models
    pub fn get_regex(&self) -> &String {
        return &self.regex;
    }
}

fn parse_base_tree(tree: Base) -> NFA {
    match tree {
        Base::Character(character) => NFA::literal_construction(character),
        Base::EscapeCharacter(character) => NFA::escape_literal_construction(character),
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
            return result;
        }
    }
}

fn parse_factor_tree(tree: Factor) -> NFA {
    match tree {
        Factor::SimpleFactor(base, quantifier) => {
            let nfa = parse_base_tree(base);
            match quantifier {
                None => nfa,
                Some(quantifier) => NFA::closure(nfa, quantifier),
            }
        }
    }
}

fn parse_term_tree(tree: Term) -> NFA {
    match tree {
        Term::SimpleTerm(factor) => parse_factor_tree(factor),
        Term::ConcatTerm(rfactor, lterm) => {
            let lterm = *lterm;
            let nfa1 = parse_term_tree(lterm);
            let nfa2 = parse_factor_tree(rfactor);
            NFA::concatenate(nfa1, nfa2)
        }
    }
}

fn parse_regex_tree(tree: RegEx) -> NFA {
    match tree {
        RegEx::SimpleRegex(term) => parse_term_tree(term),
        RegEx::AlterRegex(lterm, rregex) => {
            let rregex = *rregex; // Unboxing the value
            let nfa1 = parse_term_tree(lterm);
            let nfa2 = parse_regex_tree(rregex);
            NFA::alternation(nfa1, nfa2)
        }
    }
}

/// Apply Thomson construction algorithm to build an NFA for a given regular expression syntax
/// tree. If save_nfa is set to true, the constructed NFA is saved as a jpg.
pub fn construct_nfa(
    mut syntax_tree_list: VecDeque<(String, RegEx, String)>,
    save_nfa: bool,
) -> NFA {
    let (regex, syntax_tree, category) = syntax_tree_list.pop_front().unwrap();

    let mut result = parse_regex_tree(syntax_tree);
    result.regex = regex.to_string();

    result.set_accept_category(category);

    while !syntax_tree_list.is_empty() {
        let (regex, syntax_tree, category) = syntax_tree_list.pop_front().unwrap();
        let mut nfa = parse_regex_tree(syntax_tree);
        nfa.regex = regex.to_string();
        nfa.set_accept_category(category);
        let old_regex = result.regex.clone();
        result = NFA::alternation(result, nfa);
        let new_regex = format!("{old_regex}|{regex}");
        result.regex = new_regex;
    }
    if save_nfa {
        let filename = format!("constructed_nfa");
        result.show_fa(&filename);
    }

    result
}
