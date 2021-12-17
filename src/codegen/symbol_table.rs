pub(super) struct SymbolTable {}

impl SymbolTable {
    pub fn new() -> Self {
        Self {}
    }

    pub fn get_string_label(&mut self) -> String {
        "_str_0".to_owned()
    }
}
