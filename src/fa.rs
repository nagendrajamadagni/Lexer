use bitvec::prelude::BitVec;
use std::collections::HashSet;
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Symbol {
    Epsilon,
    Char(char),
}

pub trait FA {
    fn show_fa(&self, file_name: &str);
    fn add_transition(&mut self, from: usize, symbol: Symbol, to: usize);
    fn set_accept_state(&mut self, state_id: usize);
    fn set_start_state(&mut self, state_id: usize);
    fn add_state(&mut self) -> usize;
    fn get_num_states(&self) -> usize;
    fn get_start_state(&self) -> usize;
    fn get_alphabet(&self) -> &HashSet<char>;
    fn add_alphabet(&mut self, ch: char);
    fn set_alphabet(&mut self, alphabet: HashSet<char>);
    fn get_acceptor_states(&self) -> &BitVec<u8>;
    fn get_regex(&self) -> &String;
}

pub trait FAState {
    fn add_transition(&mut self, symbol: Symbol, to: usize);
}
