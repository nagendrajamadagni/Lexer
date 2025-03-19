#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Symbol {
    Epsilon,
    Char(char),
}

pub trait FA {
    fn show_fa(&self, file_name: &str);
    fn add_transition(&mut self, from: usize, symbol: Symbol, to: usize);
    fn set_accept_state(&mut self, state_id: usize);
    fn add_state(&mut self) -> usize;
    fn get_num_states(&self) -> usize;
}

pub trait FAState {
    fn add_transition(&mut self, symbol: Symbol, to: usize);
}
