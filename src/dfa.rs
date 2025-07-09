/* Perform subset construction to convert NFA into DFA
* Apply Hopcroft's algorithm to generate minimal DFA */

use crate::fa::{Symbol, FA};
use crate::nfa::NFA;
use bitvec::prelude::*;
use petgraph::dot::Dot;
use petgraph::graph::{EdgeIndex, NodeIndex};
use petgraph::prelude::StableGraph;
use std::collections::hash_map::Values;
use std::collections::VecDeque;
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::hash::{DefaultHasher, Hash, Hasher};
use std::io::Write;
use std::process::Command;

/// A struct which is a bitvec and its hash stored together to ease fetching the hash of the bitvec
/// quickly instead of calculating it each time.

#[derive(Clone)]
struct HashedBitVec {
    bv: BitVec<u8>,
    hash: u64,
}

impl HashedBitVec {
    fn new(bv: BitVec<u8>) -> Self {
        let mut hasher = DefaultHasher::new();
        bv.hash(&mut hasher);
        let hash = hasher.finish();
        Self { bv, hash }
    }
}

impl Hash for HashedBitVec {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u64(self.hash);
    }
}

impl PartialEq for HashedBitVec {
    fn eq(&self, other: &Self) -> bool {
        self.hash == other.hash && self.bv == other.bv
    }
}

impl Eq for HashedBitVec {}

#[derive(Debug)]
pub struct DFA {
    states: Vec<DFAState>,
    start_state: usize,
    accept_states: BitVec<u8>,
    alphabet: HashSet<char>,
    regex: String,
}

#[derive(Debug, Clone)]
pub struct DFAState {
    transitions: HashMap<Symbol, usize>, // Store by reference is not a thing in Rust
    category: String,
}

struct LookupTable {
    state_to_set_map: HashMap<usize, usize>,
    set_to_states_map: HashMap<usize, HashSet<usize>>,
}

impl LookupTable {
    fn new() -> Self {
        LookupTable {
            state_to_set_map: HashMap::new(),
            set_to_states_map: HashMap::new(),
        }
    }

    fn insert_state_in_set(&mut self, state: usize, set: usize) {
        let prev_set = self.state_to_set_map.insert(state, set);
        match prev_set {
            None => {
                // If state was not in a previous set, insert it into the provided set
                self.set_to_states_map.entry(set).or_default().insert(state);
            }
            Some(prev_set_key) => {
                // If state was present in a previous set, remove it from previous set and insert
                // it into new set

                if let Some(prev_set) = self.set_to_states_map.get_mut(&prev_set_key) {
                    prev_set.remove(&state);
                    if prev_set.is_empty() {
                        self.set_to_states_map.remove(&prev_set_key);
                    }
                }

                self.set_to_states_map.entry(set).or_default().insert(state);
            }
        }
    }

    fn get_set_of_state(&self, state: &usize) -> Option<&usize> {
        self.state_to_set_map.get(state)
    }

    fn get_num_sets(&self) -> usize {
        self.set_to_states_map.len()
    }

    fn get_sets(&self) -> Values<usize, HashSet<usize>> {
        self.set_to_states_map.values()
    }
}

impl FA for DFA {
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
            transition_list.push(transition);
        }
        transition_list
    }
}

impl DFAState {
    fn new() -> Self {
        DFAState {
            transitions: HashMap::new(),
            category: String::new(),
        }
    }
    /// Get a list of all outgoing transitions for the given state
    pub fn get_transitions(&self) -> &HashMap<Symbol, usize> {
        &self.transitions
    }

    fn set_category(&mut self, category: String) {
        self.category = category;
    }
    /// Get the syntactic category which this DFA state accepts if it is an accept state. Otherwise it
    /// returns an empty string.
    pub fn get_category(&self) -> &String {
        &self.category
    }
}

impl DFA {
    fn add_state(&mut self) -> usize {
        let state_id = self.states.len();
        let new_state: DFAState = DFAState::new();
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
                let edge_target = transition.1;

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

        for state_idx in 0..num_states {
            let node_label = format!("State {}", state_idx);

            stable_graph[NodeIndex::new(state_idx)] = node_label;

            let transition_list = &self.states[state_idx].transitions;

            for transition in transition_list {
                let edge_label = match transition.0 {
                    Symbol::Char(ch) => format!("{}", ch),
                    Symbol::Epsilon => "𝛆".to_string(),
                };

                let edge_target = transition.1;
                let edge_idx = edge_map
                    .get(&(NodeIndex::new(state_idx), NodeIndex::new(*edge_target)))
                    .unwrap();

                let old_label = &stable_graph[*edge_idx];
                let new_label = if old_label.is_empty() {
                    edge_label
                } else {
                    format!("{}, {}", old_label, edge_label)
                };

                stable_graph[*edge_idx] = new_label;
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

        println!("DFA vizualization saved as {}.jpg", filename);
    }

    fn new() -> Self {
        DFA {
            states: Vec::new(),
            start_state: 0,
            accept_states: BitVec::new(),
            alphabet: HashSet::new(),
            regex: String::new(),
        }
    }

    /// Returns a reference to the DFA state whose id is provided
    pub fn get_state(&self, id: usize) -> &DFAState {
        let state = self.states.get(id).unwrap();
        state
    }
    /// Returns a list of all states present in the DFA
    pub fn get_states(&self) -> Vec<DFAState> {
        self.states.clone()
    }

    fn set_accept_category(&mut self, category: &str) {
        let accept_states = self.accept_states.clone();

        for accept_state in accept_states.iter_ones() {
            let state = self.states.get_mut(accept_state).unwrap();
            let old_category = &state.category;
            if old_category.is_empty() {
                state.set_category(category.to_string());
            }
        }
    }
}

fn get_epsilon_closure(nfa: &NFA, nfa_states: BitVec<u8>) -> HashedBitVec {
    let num_states: usize = nfa.get_num_states();

    let mut epsilon_closure: BitVec<u8, Lsb0> = BitVec::repeat(false, num_states);

    let mut visited: BitVec<u8, Lsb0> = BitVec::repeat(false, num_states);

    let mut nfa_states: VecDeque<_> = nfa_states.iter_ones().collect();

    while !nfa_states.is_empty() {
        let state = nfa_states.pop_front().unwrap();
        let state = nfa.get_state(state).unwrap();
        let transitions = state.get_transitions();

        let eps_transitions = transitions.get(&Symbol::Epsilon);
        if let Some(targets) = eps_transitions {
            for target in targets {
                let target = *target; // Unboxing the value
                if !visited[target] {
                    visited.set(target, true);
                    nfa_states.push_back(target);
                }
            }
        }

        epsilon_closure.set(state.get_id(), true); // Adding the state itself to the epsilon closure
    }

    HashedBitVec::new(epsilon_closure)
}

// This function returns the set of states accessible via char c within the set q

fn delta(nfa: &NFA, q: &HashedBitVec, c: char) -> BitVec<u8> {
    let mut result = BitVec::repeat(false, q.bv.len());
    let nodes: Vec<usize> = q.bv.iter_ones().collect();
    for node in nodes {
        let nfa_state = nfa.get_state(node).unwrap();
        let transitions = nfa_state.get_transitions();
        let target_state_ids = transitions.get(&Symbol::Char(c));
        let target_state_ids = match target_state_ids {
            None => continue,
            Some(state_ids) => state_ids,
        };
        for state_id in target_state_ids {
            let state_id = *state_id; // Unwrapping the box
            result.set(state_id, true);
        }
    }
    result
}

fn compare_transitions(
    state1: &DFAState,
    state2: &DFAState,
    alphabet: &HashSet<char>,
    lookup_table: &LookupTable,
) -> bool {
    let state1_transitions = state1.get_transitions();
    let state2_transitions = state2.get_transitions();

    let mut same_transitions = true;

    for c in alphabet {
        if !same_transitions {
            break;
        }
        let state1_dest = state1_transitions.get(&Symbol::Char(*c)); // Get destination
                                                                     // for the state for
                                                                     // this symbol and
                                                                     // member
        let state2_dest = state2_transitions.get(&Symbol::Char(*c));

        match (state1_dest, state2_dest) {
            (None, None) => same_transitions = true, // If both don't have a transition, no splitting
            (Some(_), None) | (None, Some(_)) => {
                same_transitions = false;
            } // If only one has a transition, split
            (Some(state_dest), Some(member_dest)) => {
                // If both have transitions,
                // make sure both transition to
                // same set
                let state_dest_set = lookup_table.get_set_of_state(state_dest).unwrap();
                let member_dest_set = lookup_table.get_set_of_state(member_dest).unwrap();

                same_transitions = state_dest_set == member_dest_set;
            }
        }
    }
    same_transitions
}

fn get_lookup_table(dfa: &DFA) -> LookupTable {
    let alphabet = dfa.get_alphabet();
    let mut lookup_table = LookupTable::new();
    let states = dfa.get_acceptor_states();
    let mut set_changes: VecDeque<(usize, usize)> = VecDeque::new();
    // 0 is non acceptors states, 1 is acceptor states
    // If all states are acceptor states, then 0 is the only set id

    let set_id = 0;

    for non_accept_state in states.iter_zeros() {
        lookup_table.insert_state_in_set(non_accept_state, set_id);
    }

    let set_id = if states.all() { 0 } else { 1 }; // Test if all bits are 1, i.e all states are
                                                   // acceptors and there are no non acceptor
                                                   // states

    let mut category_set_id: HashMap<String, usize> = HashMap::new(); // Mapping of category of accept state and set id

    for accept_state in states.iter_ones() {
        let category = dfa.get_state(accept_state).get_category();
        let offset_map_len = set_id + category_set_id.len();

        let insert_id = category_set_id
            .entry(category.to_string())
            .or_insert(offset_map_len);

        lookup_table.insert_state_in_set(accept_state, *insert_id);
    }

    loop {
        let number_of_sets = lookup_table.get_num_sets(); // Get number of sets at start of
                                                          // iteration
        let sets: Vec<_> = lookup_table.get_sets().cloned().collect(); // Get list of sets

        // Try to split the sets further

        for set in sets.iter() {
            if set.len() == 1 {
                // Cannot split a set with only 1 element
                continue;
            }
            let next_set = lookup_table.get_num_sets(); // The next set which will be inserted
            let member_state_id = set.iter().next().unwrap();

            let member_state = dfa.get_state(*member_state_id);

            for state_id in set {
                let state = dfa.get_state(*state_id);

                if !compare_transitions(state, member_state, alphabet, &lookup_table) {
                    set_changes.push_back((*state_id, next_set));
                }
            }

            while !set_changes.is_empty() {
                let change = set_changes.pop_front().unwrap();
                lookup_table.insert_state_in_set(change.0, change.1);
            }
        }
        let new_number_of_sets = lookup_table.get_num_sets();

        if number_of_sets == new_number_of_sets {
            break;
        }
    }
    lookup_table
}
fn reorder_minimal_dfa(dfa: &DFA) -> DFA {
    let mut result = DFA::new(); // Set up result DFA
    let mut reorder_map = HashMap::new(); // Set up a re-order table
    let mut stack: VecDeque<usize> = VecDeque::new(); // Set up a stack for DFS

    let mut visited: BitVec<u8, Lsb0> = BitVec::repeat(false, dfa.states.len());

    for _ in 0..dfa.states.len() {
        // Add as many states as in the initial DFA because
        // we are not adding or removing ant states, just
        // re-ordering them.
        result.add_state();
    }
    let dfa_start = dfa.start_state; // Get the starting dfa state

    let mut next_id = 0;

    stack.push_front(dfa_start); // Add the start state to the stack

    while !stack.is_empty() {
        // Start DFS
        let state_id = stack.pop_front().unwrap(); // Get the head of stack

        if *visited.get(state_id).unwrap() {
            // If node is already visited skip
            continue;
        }

        visited.set(state_id, true); // Mark the current node as visited

        let reorder_state_id = match reorder_map.get(&state_id) {
            Some(&id) => id,
            None => {
                reorder_map.insert(state_id, next_id);
                let reordered_id = next_id;
                next_id += 1;
                reordered_id
            }
        }; // Get the re-ordered equivalent state or add one

        let state = &dfa.states[state_id]; // Get the state from the dfa

        let transitions = &state.transitions; // Get the transitions from the original state

        let reorder_state: &mut DFAState = result.states.get_mut(reorder_state_id).unwrap(); // Get the state from the re-ordered DFA

        for transition in transitions {
            // For each transition, check if the target state is
            // present
            let symbol = transition.0.clone();
            let target = transition.1;

            let reorder_target_id = match reorder_map.get(target) {
                // If not present, take the next available state and map it to the current
                // un-ordered state
                Some(&id) => id,
                None => {
                    let state_id = next_id;
                    reorder_map.insert(*target, state_id);
                    next_id += 1; // Pick the next available id
                    state_id
                }
            };

            reorder_state.transitions.insert(symbol, reorder_target_id); // Add a transition from
                                                                         // the reordered state to
                                                                         // the reordered target
            stack.push_front(*target); // Add the target to the head of the stack now
        }
    }

    let start_state = dfa.start_state;

    let reordered_start_state = reorder_map.get(&start_state).unwrap();
    result.start_state = *reordered_start_state;

    // Mark the acceptor states

    for accept in dfa.accept_states.iter_ones() {
        let remapped_id = reorder_map.get(&accept).unwrap();
        let category = &dfa.states[accept].category;
        result.accept_states.set(*remapped_id, true);
        result.set_accept_category(category);
    }

    result
}
/// Apply Hopcroft's algorithm on a provided DFA to minimize it. If save_minimal_dfa is set to true,
/// the constructed minimal DFA is saved as a jpg.
pub fn construct_minimal_dfa(dfa: &DFA, save_minimal_dfa: bool) -> DFA {
    let lookup_table = get_lookup_table(dfa);
    let sets = lookup_table.set_to_states_map.values();

    // Create a new DFA

    let mut minimal_dfa = DFA::new();

    // Clone the alphabet for the new DFA

    minimal_dfa.alphabet = dfa.alphabet.clone();

    minimal_dfa.regex = dfa.regex.to_string();

    // For every set in the lookup table, add a state

    for _ in 0..(lookup_table.set_to_states_map.len()) {
        minimal_dfa.add_state();
    }

    // Get the set to which the current DFA's start state belongs to and mark the state
    // corresponsing to that set id as the starting state.

    let start_state = dfa.start_state;

    let start_set = lookup_table.state_to_set_map.get(&start_state).unwrap();

    minimal_dfa.start_state = *start_set;

    // Repeat the same process as above for the acceptor states

    let acceptor_states = &dfa.accept_states;

    for accept_state in acceptor_states.iter_ones() {
        let category = &dfa.states[accept_state].category;
        if let Some(accept_set) = lookup_table.state_to_set_map.get(&accept_state) {
            minimal_dfa.accept_states.set(*accept_set, true);
            minimal_dfa.set_accept_category(category);
        }
    }

    for set in sets {
        // For each set in which all elements have the same set of transitions
        let representative = set.iter().next().unwrap(); // Pick a representative member

        let transitions = &dfa.states[*representative].transitions; // Get the set of
                                                                    // transitiosn for the
                                                                    // representative
        let current_set = lookup_table.state_to_set_map.get(representative).unwrap(); // Get the
                                                                                      // representative
                                                                                      // state's
                                                                                      // set

        for transition in transitions {
            let destination_state = transition.1;
            let destination_set = lookup_table
                .state_to_set_map
                .get(destination_state)
                .unwrap();

            minimal_dfa.states[*current_set]
                .transitions
                .insert(transition.0.clone(), *destination_set);
        }
    }

    let regex = &minimal_dfa.regex;

    let mut result = reorder_minimal_dfa(&minimal_dfa);
    result.alphabet = minimal_dfa.alphabet.clone();
    result.regex = regex.to_string();

    if save_minimal_dfa {
        let filename = "constructed_minimal_dfa".to_string();
        result.show_fa(&filename);
    }

    result // We need to always reorder now as visualization is possible
}
///  Apply the subset construction algorithm on an NFA to build a DFA. If save_dfa is set to true,
///  the constructed DFA is saved as a jpg.
pub fn construct_dfa(nfa: &NFA, save_dfa: bool) -> DFA {
    let mut result = DFA::new(); // Create new DFA
    result.alphabet = nfa.get_alphabet().clone(); // DFA has same alphabet as NFA

    let nfa_accepts = nfa.get_acceptor_states();

    let di = result.add_state(); // Add an iniital state

    result.start_state = di;
    let n0: usize = nfa.get_start_state(); // Get n0
    let mut q_list = HashMap::new(); // Mapping from nfa state set to DFA state
    let mut work_list = VecDeque::new();

    let mut nfa_states = BitVec::repeat(false, nfa.get_num_states()); // Get the initial nfa states
    nfa_states.set(n0, true); // Add the start state to nfa states set

    let q0 = get_epsilon_closure(nfa, nfa_states); // Get its epsilon closure
    q_list.insert(q0.clone(), di); // Add it to the mapping
    work_list.push_back(q0.clone()); // Add the first nfa states set to the work list

    let has_common = (q0.clone().bv & nfa_accepts).any();

    if has_common {
        result.accept_states.set(di, true);

        for state in q0.bv.iter_ones() {
            let category = nfa.get_state(state).unwrap().get_category();
            if !category.is_empty() {
                result.set_accept_category(category);
            }
        }
    }

    while !work_list.is_empty() {
        let q = work_list.pop_front().unwrap();
        for c in nfa.get_alphabet().iter() {
            // Since NFAs and DFAs have the same alphabet, and we
            // cannot borrow the DFAs alphabet as immutable we
            // borrow the NFAs alphabet
            // Iterate over each alphabet in the dfa
            let end_states = delta(nfa, &q, *c);
            if end_states.not_any() {
                continue;
            }

            let t = get_epsilon_closure(nfa, end_states);

            let di = if let Some(&existing_di) = q_list.get(&t) {
                // This is an expensive operation
                existing_di
            } else {
                let di = result.add_state();

                q_list.insert(t.clone(), di); // This is an expensive operation

                work_list.push_back(t.clone());

                let has_common = (t.clone().bv & nfa_accepts).any();

                if has_common {
                    // check if di is as an acceptor state
                    result.accept_states.set(di, true);
                    for state in t.bv.iter_ones() {
                        let category = nfa.get_state(state).unwrap().get_category();
                        if !category.is_empty() {
                            result.set_accept_category(category);
                        }
                    }
                }
                di
            };

            // add a transition from diq to dit
            let dq = q_list.get(&q).unwrap(); // This is an expensive operation
            let dq = *dq; // Unwrapping the box
            result.states[dq].transitions.insert(Symbol::Char(*c), di);
        }
    }

    let regex = nfa.get_regex();
    result.regex = regex.to_string();
    if save_dfa {
        let filename = "constructed_dfa".to_string();
        result.show_fa(&filename);
    }

    result
}

#[cfg(test)]
mod dfa_tests {
    use super::*;
    use std::collections::HashSet;

    #[test]
    fn test_dfa_state_creation() {
        let state = DFAState::new();
        assert_eq!(state.get_transitions().len(), 0);
        assert_eq!(state.get_category(), "");
    }

    #[test]
    fn test_dfa_basic_construction() {
        let mut dfa = DFA::new();
        let start = dfa.add_state();
        let end = dfa.add_state();

        assert_eq!(dfa.get_num_states(), 2);
        assert_eq!(dfa.get_start_state(), 0);
        assert_eq!(dfa.get_acceptor_states().len(), 2);
        assert!(!dfa.get_acceptor_states()[end]);
        assert!(!dfa.get_acceptor_states()[start]);

        // Mark end as accept state
        dfa.accept_states.set(end, true);
        assert!(dfa.get_acceptor_states()[end]);

        // Add transition
        dfa.states[start].transitions.insert(Symbol::Char('a'), end);
        let transitions = dfa.get_state_transitions(start);
        assert_eq!(transitions.len(), 1);
        assert_eq!(*transitions[0].0, Symbol::Char('a'));
        assert_eq!(*transitions[0].1, end);
    }

    #[test]
    fn test_lookup_table() {
        let mut lookup_table = LookupTable::new();

        // Test inserting states into sets
        lookup_table.insert_state_in_set(0, 1);
        lookup_table.insert_state_in_set(1, 1);
        lookup_table.insert_state_in_set(2, 2);

        assert_eq!(lookup_table.get_set_of_state(&0), Some(&1));
        assert_eq!(lookup_table.get_set_of_state(&1), Some(&1));
        assert_eq!(lookup_table.get_set_of_state(&2), Some(&2));
        assert_eq!(lookup_table.get_num_sets(), 2);

        // Test moving a state to a different set
        lookup_table.insert_state_in_set(0, 2);
        assert_eq!(lookup_table.get_set_of_state(&0), Some(&2));

        // Get sets
        let sets: Vec<HashSet<usize>> = lookup_table.get_sets().cloned().collect();
        assert_eq!(sets.len(), 2);

        // Check if sets contain correct elements
        let set1 = sets.iter().find(|set| set.contains(&1)).unwrap();
        let set2 = sets
            .iter()
            .find(|set| set.contains(&0) && set.contains(&2))
            .unwrap();

        assert_eq!(set1.len(), 1);
        assert_eq!(set2.len(), 2);
    }

    #[test]
    fn test_construct_minimal_dfa() {
        // Create a DFA with redundant states
        let mut dfa = DFA::new();

        // Add states
        let s0 = dfa.add_state(); // start state
        let s1 = dfa.add_state(); // accepts 'a'
        let s2 = dfa.add_state(); // accepts 'b'
        let s3 = dfa.add_state(); // duplicate of s1, accepts 'a'

        // Set start state
        dfa.start_state = s0;

        // Add characters to alphabet
        dfa.alphabet.insert('a');
        dfa.alphabet.insert('b');

        // Add transitions
        dfa.states[s0].transitions.insert(Symbol::Char('a'), s1);
        dfa.states[s0].transitions.insert(Symbol::Char('b'), s2);
        dfa.states[s1].transitions.insert(Symbol::Char('a'), s1);
        dfa.states[s1].transitions.insert(Symbol::Char('b'), s2);
        dfa.states[s2].transitions.insert(Symbol::Char('a'), s3);
        dfa.states[s2].transitions.insert(Symbol::Char('b'), s2);
        dfa.states[s3].transitions.insert(Symbol::Char('a'), s3);
        dfa.states[s3].transitions.insert(Symbol::Char('b'), s2);

        // Set accept states
        dfa.accept_states.set(s1, true);
        dfa.accept_states.set(s3, true);

        // Set categories
        dfa.states[s1].category = "A".to_string();
        dfa.states[s3].category = "A".to_string();

        // Minimize the DFA
        let minimal_dfa = construct_minimal_dfa(&dfa, false);

        // The minimal DFA should have 2 states instead of 4
        // s1 and s3 should be merged
        assert_eq!(minimal_dfa.get_num_states(), 2);

        // Check accept states
        let accept_count = minimal_dfa.get_acceptor_states().count_ones();
        assert_eq!(accept_count, 1);

        // Verify the category is preserved
        let accept_states: Vec<usize> = minimal_dfa.get_acceptor_states().iter_ones().collect();
        for state_id in accept_states {
            assert_eq!(minimal_dfa.get_state(state_id).get_category(), "A");
        }
    }

    #[test]
    fn test_compare_transitions() {
        // Create two DFA states with same transitions
        let mut dfa = DFA::new();
        let s0 = dfa.add_state();
        let s1 = dfa.add_state();
        let s2 = dfa.add_state();
        let s3 = dfa.add_state();

        dfa.alphabet.insert('a');
        dfa.alphabet.insert('b');

        // States with same transitions pattern
        dfa.states[s0].transitions.insert(Symbol::Char('a'), s2);
        dfa.states[s0].transitions.insert(Symbol::Char('b'), s3);
        dfa.states[s1].transitions.insert(Symbol::Char('a'), s2);
        dfa.states[s1].transitions.insert(Symbol::Char('b'), s3);

        // Create lookup table
        let mut lookup_table = LookupTable::new();
        lookup_table.insert_state_in_set(s0, 0);
        lookup_table.insert_state_in_set(s1, 0);
        lookup_table.insert_state_in_set(s2, 1);
        lookup_table.insert_state_in_set(s3, 2);

        // States should have same transitions
        let result = compare_transitions(
            &dfa.states[s0],
            &dfa.states[s1],
            &dfa.alphabet,
            &lookup_table,
        );
        assert!(result);

        // Now modify one transition
        dfa.states[s1].transitions.insert(Symbol::Char('b'), s2);

        // States should have different transitions now
        let result = compare_transitions(
            &dfa.states[s0],
            &dfa.states[s1],
            &dfa.alphabet,
            &lookup_table,
        );
        assert!(!result);
    }

    #[test]
    fn test_reorder_minimal_dfa() {
        // Create a DFA with unordered states
        let mut dfa = DFA::new();

        // Add states in non-consecutive order
        let s0 = dfa.add_state();
        let s1 = dfa.add_state();
        let s2 = dfa.add_state();

        // Set up transitions
        dfa.states[s0].transitions.insert(Symbol::Char('a'), s2);
        dfa.states[s2].transitions.insert(Symbol::Char('b'), s1);

        // Set start and accept states
        dfa.start_state = s0;
        dfa.accept_states.set(s1, true);
        dfa.states[s1].category = "TEST".to_string();

        // Add to alphabet
        dfa.alphabet.insert('a');
        dfa.alphabet.insert('b');

        // Reorder
        let reordered = reorder_minimal_dfa(&dfa);

        // Should still have 3 states
        assert_eq!(reordered.get_num_states(), 3);

        // Accept state should still be marked
        let accept_states: Vec<usize> = reordered.get_acceptor_states().iter_ones().collect();
        assert_eq!(accept_states.len(), 1);

        // Category should be preserved
        for accept in accept_states {
            assert_eq!(reordered.get_state(accept).get_category(), "TEST");
        }

        // Check if transitions are preserved
        let start_transitions = reordered.get_state_transitions(reordered.get_start_state());
        assert_eq!(start_transitions.len(), 1);

        // Follow path: start --a--> middle --b--> accept
        let middle_state = match start_transitions[0].0 {
            Symbol::Char('a') => *start_transitions[0].1,
            _ => panic!("Expected 'a' transition"),
        };

        let middle_transitions = reordered.get_state_transitions(middle_state);
        assert_eq!(middle_transitions.len(), 1);

        let end_state = match middle_transitions[0].0 {
            Symbol::Char('b') => *middle_transitions[0].1,
            _ => panic!("Expected 'b' transition"),
        };

        assert!(reordered.get_acceptor_states()[end_state]);
    }

    #[test]
    fn test_fa_trait_implementation_for_dfa() {
        // Create a simple DFA
        let mut dfa = DFA::new();
        let s0 = dfa.add_state();
        let s1 = dfa.add_state();

        dfa.start_state = s0;
        dfa.accept_states.set(s1, true);
        dfa.alphabet.insert('a');

        dfa.states[s0].transitions.insert(Symbol::Char('a'), s1);

        // Test FA trait methods
        assert_eq!(dfa.get_num_states(), 2);
        assert_eq!(dfa.get_start_state(), s0);
        assert!(dfa.get_alphabet().contains(&'a'));

        let accept_states: Vec<usize> = dfa.get_acceptor_states().iter_ones().collect();
        assert_eq!(accept_states, vec![s1]);

        let transitions = dfa.get_state_transitions(s0);
        assert_eq!(transitions.len(), 1);
        assert_eq!(*transitions[0].0, Symbol::Char('a'));
        assert_eq!(*transitions[0].1, s1);
    }
}
