/* Perform subset construction to convert NFA into DFA
* Apply Hopcroft's algorithm to generate minimal DFA */

use crate::fa::{FAState, Symbol, FA};
use crate::nfa::NFA;
use bitvec::prelude::*;
use petgraph::dot::Dot;
use petgraph::graph::DiGraph;
use std::collections::hash_map::Values;
use std::collections::VecDeque;
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::Write;
use std::process::Command;

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
    id: usize,
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
                self.set_to_states_map
                    .entry(set)
                    .or_insert_with(HashSet::new)
                    .insert(state);
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

                self.set_to_states_map
                    .entry(set)
                    .or_insert_with(HashSet::new)
                    .insert(state);
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
    fn show_fa(&self, filename: &str) {
        let mut graph = DiGraph::new();
        let mut node_map = std::collections::HashMap::new();

        // Add nodes
        for state in self.get_states() {
            let node = graph.add_node(format!("State {}", state.id));
            node_map.insert(state.id, node);
        }

        // Add edges
        for state in self.get_states() {
            for (symbol, target) in &state.transitions {
                let symbol_str = match symbol {
                    Symbol::Char(c) => c.to_string(),
                    Symbol::Epsilon => "𝛆".to_string(),
                };
                graph.add_edge(node_map[&state.id], node_map[&target], symbol_str);
            }
        }

        // Mark Start and Accept States

        let start_node = node_map[&self.start_state];
        graph[start_node] = format!("Start\nState {}", self.start_state);

        let accept_states: Vec<usize> = self.accept_states.iter_ones().collect();

        for accept in accept_states {
            let accept_node = node_map[&accept];
            graph[accept_node] =
                graph[accept_node].clone() + &format!("\nAccept\nState {}", accept);
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

        println!("DFA vizualization saved as {}.jpg", filename);
    }

    fn add_transition(&mut self, from: usize, symbol: Symbol, to: usize) {
        self.states[from].add_transition(symbol, to);
    }

    fn set_accept_state(&mut self, state_id: usize) {
        self.accept_states.set(state_id, true);
    }

    fn set_start_state(&mut self, state_id: usize) {
        self.start_state = state_id;
    }

    fn add_state(&mut self) -> usize {
        let state_id = self.states.len();
        let new_state: DFAState = DFAState::new(state_id);
        self.states.push(new_state);
        self.accept_states.push(false);
        return state_id;
    }

    fn get_num_states(&self) -> usize {
        self.states.len()
    }

    fn get_start_state(&self) -> usize {
        self.start_state
    }

    fn get_alphabet(&self) -> &HashSet<char> {
        &self.alphabet
    }

    fn set_alphabet(&mut self, alphabet: HashSet<char>) {
        self.alphabet = alphabet;
    }

    fn get_acceptor_states(&self) -> &BitVec<u8> {
        &self.accept_states
    }

    fn get_regex(&self) -> &String {
        &self.regex
    }

    fn add_alphabet(&mut self, ch: char) {
        self.alphabet.insert(ch);
    }
}

impl FAState for DFAState {
    fn add_transition(&mut self, symbol: Symbol, to: usize) {
        self.transitions.insert(symbol, to);
    }
}

impl DFAState {
    fn new(id: usize) -> Self {
        DFAState {
            id,
            transitions: HashMap::new(),
            category: String::new(),
        }
    }

    pub fn get_transitions(&self) -> &HashMap<Symbol, usize> {
        &self.transitions
    }

    fn set_category(&mut self, category: String) {
        self.category = category;
    }

    pub fn get_category(&self) -> &String {
        &self.category
    }
}

impl DFA {
    fn new() -> Self {
        DFA {
            states: Vec::new(),
            start_state: 0,
            accept_states: BitVec::new(),
            alphabet: HashSet::new(),
            regex: String::new(),
        }
    }

    fn set_regex(&mut self, regex: String) {
        self.regex = regex;
    }

    pub fn get_state(&self, id: usize) -> &DFAState {
        let state = self.states.get(id);
        match state {
            Some(state) => state,
            None => panic!("Invalid state index provided"),
        }
    }

    fn get_state_mut(&mut self, id: usize) -> &mut DFAState {
        let state = self.states.get_mut(id);
        match state {
            Some(state) => state,
            None => panic!("Invalid state index provided"),
        }
    }

    pub fn get_states(&self) -> Vec<DFAState> {
        self.states.clone()
    }

    fn set_accept_category(&mut self, category: &String) {
        let accept_states = self.get_acceptor_states().clone();

        for accept_state in accept_states.iter_ones() {
            let state = self.get_state_mut(accept_state);
            let old_category = state.get_category();
            if old_category.is_empty() {
                state.set_category(category.clone());
            }
        }
    }
}

fn get_epsilon_closure(nfa: &NFA, nfa_states: BitVec<u8>) -> BitVec<u8> {
    let num_states: usize = nfa.get_num_states();

    let mut epsilon_closure: BitVec<u8, Lsb0> = BitVec::repeat(false, num_states);

    let mut visited: BitVec<u8, Lsb0> = BitVec::repeat(false, num_states);

    let mut nfa_states: VecDeque<_> = nfa_states.iter_ones().collect();

    while !nfa_states.is_empty() {
        let state = nfa_states.pop_front();
        let state = match state {
            Some(state) => state,
            None => panic!("Trying to remove element from empty queue"),
        };
        let state = nfa.get_state(state);
        let transitions = state.get_transitions();

        let eps_transitions = transitions.get(&Symbol::Epsilon);
        match eps_transitions {
            Some(targets) => {
                for target in targets {
                    let target = *target; // Unboxing the value
                    if !visited[target] {
                        visited.set(target, true);
                        nfa_states.push_back(target);
                    }
                }
            }
            None => {}
        }
        epsilon_closure.set(state.get_id(), true); // Adding the state itself to the epsilon closure
    }
    return epsilon_closure;
}

// This function returns the set of states accessible via char c within the set q

fn delta(nfa: &NFA, q: &BitVec<u8>, c: char) -> BitVec<u8> {
    let mut result = BitVec::repeat(false, q.len());
    let nodes: Vec<usize> = q.iter_ones().collect();
    for node in nodes {
        let nfa_state = nfa.get_state(node);
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
    return result;
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
    return same_transitions;
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
            let member_state_id = set.iter().next();
            let member_state_id = match member_state_id {
                Some(id) => id,
                None => panic!("Trying to remove element from empty set!"),
            };

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
    return lookup_table;
}
fn reorder_minimal_dfa(dfa: &DFA) -> DFA {
    let mut result = DFA::new(); // Set up result DFA
    let mut reorder_map = HashMap::new(); // Set up a re-order table
    let mut stack: VecDeque<usize> = VecDeque::new(); // Set up a stack for DFS

    let mut visited: BitVec<u8, Lsb0> = BitVec::repeat(false, dfa.get_num_states());

    for _ in 0..dfa.get_num_states() {
        // Add as many states as in the initial DFA because
        // we are not adding or removing ant states, just
        // re-ordering them.
        result.add_state();
    }
    let dfa_start = dfa.get_start_state(); // Get the starting dfa state

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
                next_id = next_id + 1;
                reordered_id
            }
        }; // Get the re-ordered equivalent state or add one

        let state = dfa.get_state(state_id); // Get the state from the dfa

        let transitions = state.get_transitions(); // Get the transitions from the original state

        let reorder_state: &mut DFAState = result.get_state_mut(reorder_state_id); // Get the state from the
                                                                                   // re-ordered DFA

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
                    next_id = next_id + 1; // Pick the next available id
                    state_id
                }
            };

            reorder_state.add_transition(symbol, reorder_target_id); // Add a transition from the
                                                                     // reordered state to the
                                                                     // reordered target
            stack.push_front(*target); // Add the target to the head of the stack now
        }
    }

    let start_state = dfa.get_start_state();

    let reordered_start_state = reorder_map.get(&start_state).unwrap();
    result.set_start_state(*reordered_start_state);

    // Mark the acceptor states

    for accept in dfa.get_acceptor_states().iter_ones() {
        let remapped_id = reorder_map.get(&accept).unwrap();
        let category = dfa.get_state(accept).get_category();
        result.set_accept_state(*remapped_id);
        result.set_accept_category(category);
    }

    return result;
}

pub fn construct_minimal_dfa(dfa: DFA, save_minimal_dfa: bool) -> DFA {
    let lookup_table = get_lookup_table(&dfa);
    let sets = lookup_table.get_sets();

    // Create a new DFA

    let mut minimal_dfa = DFA::new();

    // Clone the alphabet for the new DFA

    minimal_dfa.set_alphabet(dfa.alphabet.clone());

    minimal_dfa.set_regex(dfa.get_regex().to_string());

    // For every set in the lookup table, add a state

    for _ in 0..(lookup_table.get_num_sets()) {
        minimal_dfa.add_state();
    }

    // Get the set to which the current DFA's start state belongs to and mark the state
    // corresponsing to that set id as the starting state.

    let start_state = dfa.get_start_state();

    let start_set = lookup_table.get_set_of_state(&start_state);

    let start_set = match start_set {
        Some(set) => set,
        None => panic!("Invalid set number provided!"),
    };

    minimal_dfa.set_start_state(*start_set);

    // Repeat the same process as above for the acceptor states

    let acceptor_states = dfa.get_acceptor_states();

    for accept_state in acceptor_states.iter_ones() {
        let category = dfa.get_state(accept_state).get_category();
        if let Some(accept_set) = lookup_table.get_set_of_state(&accept_state) {
            minimal_dfa.set_accept_state(*accept_set);
            minimal_dfa.set_accept_category(category);
        }
    }

    for set in sets {
        // For each set in which all elements have the same set of transitions
        let representative = set.iter().next().unwrap(); // Pick a representative member

        let transitions = dfa.get_state(*representative).get_transitions(); // Get the set of
                                                                            // transitiosn for the
                                                                            // representative

        let current_set = lookup_table.get_set_of_state(representative).unwrap(); // Get the
                                                                                  // representative
                                                                                  // state's set
        for transition in transitions {
            let destination_state = transition.1;
            let destination_set = lookup_table.get_set_of_state(destination_state);
            let destination_set = match destination_set {
                Some(set) => set,
                None => panic!("Provided state does not exist in any set!"),
            };

            minimal_dfa.add_transition(*current_set, transition.0.clone(), *destination_set);
        }
    }

    let regex = minimal_dfa.get_regex();

    if save_minimal_dfa {
        let mut result = reorder_minimal_dfa(&minimal_dfa);
        result.set_alphabet(minimal_dfa.get_alphabet().clone());
        result.set_regex(regex.to_string());
        let filename = format!("{regex}_minimal_dfa");
        result.show_fa(&filename);
    }

    return minimal_dfa; // We only need to re-arrange if we are saving, the scanner does not need
                        // the DFA to be organized
}

pub fn construct_dfa(nfa: NFA, save_dfa: bool) -> DFA {
    let mut result = DFA::new(); // Create new DFA
    result.set_alphabet(nfa.get_alphabet().clone()); // DFA has same alphabet as NFA

    let nfa_accepts = nfa.get_acceptor_states();

    let di = result.add_state(); // Add an iniital state

    result.set_start_state(di);
    let n0: usize = nfa.get_start_state(); // Get n0
    let mut q_list = HashMap::new(); // Mapping from nfa state set to DFA state
    let mut work_list = VecDeque::new();

    let mut nfa_states = BitVec::repeat(false, nfa.get_num_states()); // Get the initial nfa states
    nfa_states.set(n0, true); // Add the start state to nfa states set

    let q0 = get_epsilon_closure(&nfa, nfa_states); // Get its epsilon closure
    q_list.insert(q0.clone(), di); // Add it to the mapping
    work_list.push_back(q0.clone()); // Add the first nfa states set to the work list

    let has_common = (q0.clone() & nfa_accepts).any();

    if has_common {
        result.set_accept_state(di);

        for state in q0.iter_ones() {
            let category = nfa.get_state(state).get_category();
            if !category.is_empty() {
                result.set_accept_category(category);
            }
        }
    }

    let dfa_alphabet = result.alphabet.clone();

    while !work_list.is_empty() {
        let q = work_list.pop_front();
        let q = match q {
            Some(q) => q,
            None => panic!("trying to pop empty list!"),
        };
        for c in dfa_alphabet.iter() {
            let end_states = delta(&nfa, &q, *c);
            if end_states.not_any() {
                continue;
            }
            let t = get_epsilon_closure(&nfa, end_states);

            if !q_list.contains_key(&t) {
                // check if di is as an acceptor state
                let di = result.add_state();
                q_list.insert(t.clone(), di);
                work_list.push_back(t.clone());
                let has_common = (t.clone() & nfa_accepts).any();
                if has_common {
                    result.set_accept_state(di);
                    for state in t.iter_ones() {
                        let category = nfa.get_state(state).get_category();
                        if !category.is_empty() {
                            result.set_accept_category(category);
                        }
                    }
                }
            }
            // add a transition from diq to dit
            let dq = q_list.get(&q);
            let dq = match dq {
                Some(dq) => dq,
                None => panic!("value not found in hash table"),
            };
            let di = q_list.get(&t);
            let di = match di {
                Some(di) => di,
                None => panic!("value not found in hash table"),
            };
            let di = *di;
            let dq = *dq; // Unwrapping the box
            result.add_transition(dq, Symbol::Char(*c), di);
        }
    }
    let regex = nfa.get_regex();
    result.set_regex(regex.to_string());
    if save_dfa {
        let filename = format!("{regex}_dfa");
        result.show_fa(&filename);
    }

    return result;
}
