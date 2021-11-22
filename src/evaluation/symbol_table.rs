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
                    self.table // TODO Fix this when I fix the types
                        .entry(name.to_string())
                        .or_insert(TableItem::NumVariable((*num).try_into().unwrap()));
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
            _ => todo!(),
        }
    }
}
