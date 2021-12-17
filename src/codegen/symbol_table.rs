use super::{CodeGenError, Result};
use std::collections::HashMap;

pub(super) struct SymbolTable {
    string_count: isize,
    const_num_count: isize,
    num_map: HashMap<String, String>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            string_count: 0,
            const_num_count: 0,
            num_map: HashMap::new(),
        }
    }

    pub fn get_string_label(&mut self) -> String {
        let label = format!("_str_{}", self.string_count);
        self.string_count += 1;
        label
    }

    pub fn new_const_num(&mut self, name: String) -> Result<String> {
        let label = format!("_num_const_{}_{}", self.const_num_count, name);
        self.num_map.entry(name).or_insert(label.to_owned());
        self.const_num_count += 1;
        Ok(label)
    }

    pub fn get_num_label(&self, name: String) -> Result<&String> {
        if let Some(value) = self.num_map.get(&name) {
            Ok(value)
        } else {
            return Err(format!("Could not find {}", name).into());
        }
    }
}
