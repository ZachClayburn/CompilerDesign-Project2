use super::{CodeGenError, Result};

pub(super) struct SymbolTable {
    string_count: isize,
    const_num_count: isize,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            string_count: 0,
            const_num_count: 0,
        }
    }

    pub fn get_string_label(&mut self) -> String {
        let label = format!("_str_{}", self.string_count);
        self.string_count += 1;
        label
    }

    pub fn new_const_num(&mut self, name: String) -> Result<String> {
        let label = format!("_num_const_{}_{}", self.const_num_count, name);
        self.const_num_count += 1;
        Ok(label)
    }
}
