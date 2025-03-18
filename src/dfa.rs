/* Perform subset construction to convert NFA into DFA
 * Apply Hopcroft's algorithm to generate minimal DFA */

use crate::fa::{FAState, Symbol, FA};
use crate::nfa::{NFAState, NFA};
use petgraph::dot::Dot;
use petgraph::graph::DiGraph;
use std::collections::VecDeque;
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::hash::{Hash, Hasher};
use std::io::Write;
use std::process::Command;

#[derive(Debug)]
pub struct DFA {
    states: Vec<DFAState>,
    start_state: usize,
    accept_states: HashSet<usize>,
    alphabet: HashSet<char>,
}

#[derive(Debug, Clone)]
struct DFAState {
    id: usize,
    transitions: HashMap<Symbol, usize>, // Store by reference is not a thing in Rust
}

impl FA for DFA {
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

        for accept in &self.accept_states {
            let accept_node = node_map[&accept];
            graph[accept_node] = format!("Accept\nState {}", accept);
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
        self.accept_states.insert(state_id);
    }

    fn add_state(&mut self) -> usize {
        let state_id = self.states.len();
        let new_state: DFAState = DFAState::new(state_id);
        self.states.push(new_state.clone());
        return state_id;
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
        }
    }
}

impl DFA {
    fn new() -> Self {
        DFA {
            states: Vec::new(),
            start_state: 0,
            accept_states: HashSet::new(),
            alphabet: HashSet::new(),
        }
    }
}

#[derive(Eq, PartialEq, Debug, Clone)]
struct EpsilonClosure(HashSet<usize>);

impl Hash for EpsilonClosure {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let mut sorted_values: Vec<_> = self.0.iter().collect();
        sorted_values.sort();
        for value in sorted_values {
            value.hash(state);
        }
    }
}

fn get_epsilon_closure(nfa: &NFA, nfa_states: HashSet<NFAState>) -> EpsilonClosure {
    let mut epsilon_closure = HashSet::new();

    let mut visited = HashSet::new();

    let mut nfa_states: VecDeque<_> = nfa_states.into_iter().collect();

    while !nfa_states.is_empty() {
        let state = nfa_states.pop_front();
        let state = match state {
            Some(state) => state,
            None => panic!("Trying to remove element from empty queue"),
        };
        let transitions = state.get_transitions();

        let eps_transitions = transitions.get(&Symbol::Epsilon);
        match eps_transitions {
            Some(targets) => {
                for target in targets {
                    let target = *target; // Unboxing the value
                    if !visited.contains(&target) {
                        visited.insert(target);
                        let next_state = nfa.get_state(target);
                        nfa_states.push_back(next_state);
                    }
                }
            }
            None => {}
        }
        epsilon_closure.insert(state.get_id()); // Adding the state itself to the epsilon closure
    }
    return EpsilonClosure(epsilon_closure);
}

fn delta(nfa: &NFA, q: &HashSet<usize>, c: char) -> Option<HashSet<NFAState>> {
    let mut result = HashSet::new();
    for nstate in q {
        let nstate = *nstate; // Unboxing
        let nfa_state = nfa.get_state(nstate);
        let transitions = nfa_state.get_transitions();
        let target_state_ids = transitions.get(&Symbol::Char(c));
        let target_state_ids = match target_state_ids {
            None => continue,
            Some(state_ids) => state_ids,
        };
        for state_id in target_state_ids {
            let state_id = *state_id; // Unwrapping the box
            let state = nfa.get_state(state_id);
            result.insert(state);
        }
    }
    if result.is_empty() {
        return None;
    } else {
        return Some(result);
    }
}

pub fn construct_dfa(nfa: NFA) -> DFA {
    let mut result = DFA::new(); // Create new DFA
    result.alphabet = nfa.get_alphabet(); // DFA has same alphabet as NFA

    let di = result.add_state(); // Add an iniital state
    result.start_state = di;
    let n0: NFAState = nfa.get_state(nfa.get_start_state()); // Get n0
    let mut q_list = HashMap::new(); // Mapping from nfa state set to DFA state
    let mut work_list = VecDeque::new();

    let mut nfa_states = HashSet::new(); // Get the initial nfa states
    nfa_states.insert(n0); // Add the start state to nfa states set

    let q0 = get_epsilon_closure(&nfa, nfa_states); // Get its epsilon closure
    q_list.insert(q0.clone(), di); // Add it to the mapping
    work_list.push_back(q0); // Add the first nfa states set to the work list

    while !work_list.is_empty() {
        let q = work_list.pop_front();
        let q = match q {
            Some(q) => q,
            None => panic!("Trying to pop empty list!"),
        };
        let dfa_alphabet = result.alphabet.clone();
        for c in dfa_alphabet {
            let end_states = delta(&nfa, &q.0, c);
            let end_states = match end_states {
                Some(end_states) => end_states,
                None => continue,
            };
            let t = get_epsilon_closure(&nfa, end_states);

            if !q_list.contains_key(&t) {
                // Check if di is as an acceptor state
                let di = result.add_state();
                q_list.insert(t.clone(), di);
                work_list.push_back(t.clone());
                if !nfa.get_acceptor_states().is_disjoint(&t.0) {
                    result.set_accept_state(di);
                }
            }
            // Add a transition from diq to dit
            let dq = q_list.get(&q);
            let dq = match dq {
                Some(dq) => dq,
                None => panic!("Value not found in Hash Table"),
            };
            let di = q_list.get(&t);
            let di = match di {
                Some(di) => di,
                None => panic!("Value not found in Hash Table"),
            };
            let di = *di;
            let dq = *dq; // Unwrapping the box
            result.add_transition(dq, Symbol::Char(c), di);
        }
    }
    let regex = nfa.get_regex();
    let filename = format!("{regex}_dfa");
    result.show_fa(&filename);
    return result;
}
