use std::collections::{HashMap, HashSet};
use std::process::Command;
use petgraph::graph::DiGraph;
use petgraph::dot::Dot;
use std::fs::File;
use std::io::Write;


#[derive(Debug, PartialEq, Eq, Hash, Clone)]
enum Symbol {
    Epsilon,
    Char(char),
}

#[derive(Debug, Clone)]
struct State {
    id:usize,
    transitions: HashMap<Symbol, HashSet<usize>>, // Store by reference is not a thing in Rust
}

#[derive(Debug)]
struct NFA {
    states: Vec<State>,
    start_state: usize,
    accept_states: HashSet<usize>,
    regex: String
}

impl State {
    fn new(id: usize) -> Self {
        State {
            id,
            transitions: HashMap::new()
        }
    }

    fn add_transition(&mut self, symbol:Symbol, to: usize) {
        self.transitions.entry(symbol).or_default().insert(to);
    }
}

impl NFA {
    fn new(reg_ex: &str) -> Self {
        NFA {
            states: Vec::new(),
            start_state: 0,
            accept_states: HashSet::new(),
            regex: reg_ex.to_string()
        }
    }

    fn add_state(&mut self) -> usize {
        let state_id = self.states.len();
        let new_state: State = State::new(state_id);
        self.states.push(new_state.clone());
        return state_id;
    }

    fn add_transition(&mut self, from: usize, symbol:Symbol, to: usize) {
        self.states[from].add_transition(symbol, to);
    }

    fn set_accept_state(&mut self, state_id: usize) {
        self.accept_states.insert(state_id);
    }


    fn alternation(nfa1: NFA, nfa2: NFA) -> NFA {
        let mut result = NFA::new("");
        let new_start = result.add_state();

        // Copy states from NFA 1
        let offset1 = result.states.len();

        for mut state in nfa1.states {
            state.id += offset1;
            let mut new_transitions = HashMap::new();

            for (symbol, targets) in state.transitions {
                let mut new_targets = HashSet::new();

                for target in targets {
                    new_targets.insert(target+offset1);
                }
                new_transitions.insert(symbol, new_targets);
            }
            state.transitions = new_transitions;
            result.states.push(state);
        }

        // Add epsilon transition from new start to start state of NFA1
        result.add_transition(new_start, Symbol::Epsilon, nfa1.start_state + offset1);

        let offset2 = result.states.len();

        for mut state in nfa2.states { // Copy states from NFA2
            state.id += offset2;
            let mut new_transitions = HashMap::new();

            for (symbol, targets) in state.transitions {
                let mut new_targets = HashSet::new();

                for target in targets {
                    new_targets.insert(target+offset2);
                }
                new_transitions.insert(symbol, new_targets);
            }
            state.transitions = new_transitions;
            result.states.push(state);
        }

        // Add epsilon transition from new start to start state of NFA2
        result.add_transition(new_start, Symbol::Epsilon, nfa2.start_state + offset2);
        
        let new_accept = result.add_state();

        // Add epsilon transitions from NFA1s accept states to new accept
        for accept_state in nfa1.accept_states {
            result.add_transition(accept_state + offset1, Symbol::Epsilon, new_accept);
        }
        
        // Add epsilon transitions from NFA2s accept states to new accept
        for accept_state in nfa2.accept_states.clone() {
            result.add_transition(accept_state + offset2, Symbol::Epsilon, new_accept);
        }

        result.start_state = new_start;
        result.set_accept_state(new_accept);

        return result;
    }

    fn kleene_closure(nfa: NFA) -> NFA {
        let mut result = NFA::new("");
        let new_start = result.add_state(); // Add a new start and accept state

        // Copy states from the original NFA

        let offset = result.states.len();

        for mut state in nfa.states {
            state.id += offset;
            let mut new_transitions = HashMap::new();

            for (symbol, targets) in state.transitions {
                let mut new_targets = HashSet::new();

                for target in targets {
                    new_targets.insert(target+offset);
                }
                new_transitions.insert(symbol, new_targets);
            }
            state.transitions = new_transitions;
            result.states.push(state);
        }

        result.add_transition(new_start, Symbol::Epsilon, nfa.start_state + offset);
        let new_accept = result.add_state();
        result.add_transition(new_start, Symbol::Epsilon, new_accept); // Add epsilon transitions
                                                                       // from new start to new
                                                                       // accept state

        for accept in nfa.accept_states { // Add epsilon transitions from old accept to new accept
                                          // and old accept and old start
            result.add_transition(accept + offset, Symbol::Epsilon, nfa.start_state + offset);
            result.add_transition(accept + offset, Symbol::Epsilon, new_accept);
        }

        result.start_state = new_start; // Set new start and new accepts
        result.set_accept_state(new_accept);
        return result;
    }

   fn concatenate(nfa1: NFA, nfa2: NFA) -> NFA {
       let mut result: NFA = NFA::new(&nfa1.regex);
       result.states = nfa1.states.clone(); // Clone all states from nfa1
       let offset = nfa1.states.len();
        
       // Add states and their transitions from nfa2 into the resultant nfa
       for mut state in nfa2.states { // For each state in NFA2
            state.id += offset; // Change their ID by offset
            let mut new_transitions = HashMap::new(); // Create new transitions

            for (symbol, targets) in state.transitions {
                let mut new_targets = HashSet::new();
            
                for target in targets {
                    new_targets.insert(target+offset);
                }
                new_transitions.insert(symbol, new_targets);
            }
            state.transitions = new_transitions; // Add the new transitions to new states
            result.states.push(state); // Add the new states to the results
       }

       // Add epsilon transitions from each acceptor state of NFA1 to start state of NFA2
        
       for accept_id in nfa1.accept_states {
           result.add_transition(accept_id, Symbol::Epsilon, nfa2.start_state + offset);
       }

       result.start_state = nfa1.start_state; // Make the start state of NFA1 the start state of
                                              // the result
       result.accept_states = nfa2.accept_states.into_iter()
                                                .map(|s| s + offset)
                                                .collect(); // Make the accept states of NFA2 the accept
                                                  // states of the result
       return result;
   }

   fn literal( character:char) -> NFA {
       let mut result: NFA = NFA::new(&character.to_string());
       let start_state = result.add_state();
       let end_state = result.add_state();
       result.add_transition(start_state, Symbol::Char(character), end_state);

       result.start_state = start_state;
       result.set_accept_state(end_state);
       return result;
   }


    fn show_nfa(&self, filename: &str) {
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
                       Symbol::Epsilon => "𝛆".to_string()
                    };
                    graph.add_edge(node_map[&state.id], node_map[&target], symbol_str);
                }
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

        dot_file.write_all(dot.to_string().as_bytes()).expect("Failed to write dot file");

        Command::new("dot").args(&["-Tjpg", &dot_filename, "-o", &format!("{}.jpg", filename)]).output().expect("Failed to execute Graphviz");

        println!("NFA vizualization saved as {}.jpg", filename);
    }
}

pub fn construct_nfa(reg_ex: &str) {

    let a_nfa = NFA::literal('a');
    let b_nfa = NFA::literal('b');

    //let result = NFA::concatenate(nfa, nnfa);
    //let result = NFA::kleene_closure(nfa);
    let result = NFA::alternation(a_nfa, b_nfa);
    result.show_nfa("regex_a|b");


    //println!("Created a non finite automata for the string {}", nfa.regex);

    //nfa.show_nfa("regex_a");

}
