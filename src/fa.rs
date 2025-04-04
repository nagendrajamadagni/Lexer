use bitvec::prelude::BitVec;
use std::collections::HashSet;
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Symbol {
    Epsilon,
    Char(char),
}

pub trait FA {
    fn get_num_states(&self) -> usize;
    fn get_start_state(&self) -> usize;
    fn get_alphabet(&self) -> &HashSet<char>;
    fn get_acceptor_states(&self) -> &BitVec<u8>;
}
