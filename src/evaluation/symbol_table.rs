use std::{collections::HashMap, convert::TryInto};

use crate::parser::ast::{Expression, TypedVar};

use super::Result;

pub struct SymbolTable {
    table: HashMap<String, TableItem>,
}

pub enum TableItem {
    NumVariable(i32),
    IshVariable(f32),
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            table: HashMap::new(),
        }
    }

    pub fn get_value(&self, name: &String) -> Result<&TableItem> {
        match self.table.get(name) {
            Some(val) => Ok(val),
            None => Err(format!("Unkown symbol {}", name).into()),
        }
    }

    pub fn add_value(&mut self, name: &TypedVar, value: &Expression) -> Result<()> {
        match (name, value) {
            (TypedVar::Num(name), Expression::NumberLiteral(num)) => {
                if self.table.contains_key(name) {
                    Err(format!("symbol {} is already in use", name).into())
                } else {
                    // TODO Fix this when I fix the types
                    let num: i32 = match (*num).try_into() {
                        Ok(num) => num,
                        Err(err) => return Err(format!("Error processing {}: {}", num, err).into()),
                    };
                    self.table
                        .entry(name.to_string())
                        .or_insert(TableItem::NumVariable(num));
                    Ok(())
                }
            }
            (TypedVar::Ish(name), Expression::FloatLiteral(num)) => {
                if self.table.contains_key(name) {
                    Err(format!("symbol {} is already in use", name).into())
                } else {
                    self.table
                        .entry(name.to_string())
                        .or_insert(TableItem::IshVariable(*num));
                    Ok(())
                }
            }
            (bad_name, bad_value) => Err(format!(
                "Adding {} = {} to the symbol table not allowed",
                bad_name, bad_value,
            )
            .into()),
        }
    }
}
