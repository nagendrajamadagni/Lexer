use bitvec::prelude::BitVec;
use std::collections::HashSet;
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Symbol {
    Epsilon,
    Char(char),
}

pub trait FA {
    /// Get the number of states in the finite automata
    fn get_num_states(&self) -> usize;
    /// Get the start state of the finite automata
    fn get_start_state(&self) -> usize;
    /// Get the set of all characters in the finite automata
    fn get_alphabet(&self) -> &HashSet<char>;
    /// Get a bit-vec of acceptor states in the finite automata, if the bit is set to 1, then the
    /// state is an acceptor state, otherwise it is not.
    fn get_acceptor_states(&self) -> &BitVec<u8>;
    /// Get the list of all outgoing transitions for the given state
    fn get_state_transitions(&self, id: usize) -> Vec<(&Symbol, &usize)>;
}
