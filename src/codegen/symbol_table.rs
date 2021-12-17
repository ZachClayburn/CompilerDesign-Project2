pub(super) struct SymbolTable {
    string_count: isize,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self { string_count: 0 }
    }

    pub fn get_string_label(&mut self) -> String {
        let label = format!("_str_{}", self.string_count);
        self.string_count += 1;
        label
    }
}
