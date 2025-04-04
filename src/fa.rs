use bitvec::prelude::BitVec;
use std::collections::HashSet;
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Symbol {
    Epsilon,
    Char(char),
}

pub trait FA {
    fn show_fa(&self, file_name: &str);
    //fn add_transition(&mut self, from: usize, symbol: Symbol, to: usize);
    fn add_state(&mut self) -> usize;
    fn get_num_states(&self) -> usize;
    fn get_start_state(&self) -> usize;
    fn get_alphabet(&self) -> &HashSet<char>;
    fn get_acceptor_states(&self) -> &BitVec<u8>;
}

//pub trait FAState {
//    fn add_transition(&mut self, symbol: Symbol, to: usize);
//}
