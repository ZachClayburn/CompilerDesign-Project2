use super::ProcedureType;
use crate::parser::ast::{Expression, Statement, TypedVar};
use std::{collections::HashMap, convert::TryInto};

use super::Result;

struct Scope {
    pub table: HashMap<String, TableItem>,
    pub return_type: ProcedureType,
}

pub struct SymbolTable {
    table: HashMap<String, TableItem>,
    scope_tables: Vec<Scope>,
}

pub enum TableItem {
    NumVariable(i32),
    IshVariable(f32),
    NumProcedure {
        params: Vec<TypedVar>,
        statements: Vec<Statement>,
    },
    IshProcedure {
        params: Vec<TypedVar>,
        statements: Vec<Statement>,
    },
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            table: HashMap::new(),
            scope_tables: Vec::new(),
        }
    }

    pub fn get_value(&self, name: &String) -> Result<&TableItem> {
        match self.table.get(name) {
            Some(val) => Ok(val),
            None => Err(format!("Unkown symbol {}", name).into()),
        }
    }

    fn add_item(&mut self, name: &String, item: TableItem) -> Result<()> {
        if self.table.contains_key(name) {
            Err(format!("symbol {} is already in use", name).into())
        } else {
            self.table.entry(name.to_string()).or_insert(item);
            Ok(())
        }
    }

    pub fn add_expression(&mut self, name: &TypedVar, value: &Expression) -> Result<()> {
        match (name, value) {
            (TypedVar::Num(name), Expression::NumberLiteral(num)) => {
                // TODO Fix this when I fix the types
                let num: i32 = match (*num).try_into() {
                    Ok(num) => num,
                    Err(err) => return Err(format!("Error processing {}: {}", num, err).into()),
                };
                self.add_item(&name, TableItem::NumVariable(num))
            }
            (TypedVar::Ish(name), Expression::FloatLiteral(num)) => {
                self.add_item(&name, TableItem::IshVariable(*num))
            }
            (bad_name, bad_value) => Err(format!(
                "Adding {} = {} to the symbol table not allowed",
                bad_name, bad_value,
            )
            .into()),
        }
    }

    pub(super) fn push_scope(&mut self, return_type: ProcedureType) {
        self.scope_tables.push(Scope {
            table: HashMap::new(),
            return_type,
        })
    }

    pub fn pop_scope(&mut self) -> Result<()> {
        match self.scope_tables.pop() {
            Some(..) => Ok(()),
            None => Err("Attempted to pop scope when no scopes are active!"
                .to_string()
                .into()),
        }
    }

    pub fn add_procedure(
        &mut self,
        name: &TypedVar,
        params: Vec<TypedVar>,
        statements: Vec<Statement>,
    ) -> Result<()> {
        let (name, item) = match name {
            TypedVar::Num(name) => (name, TableItem::NumProcedure { params, statements }),
            TypedVar::Ish(name) => (name, TableItem::IshProcedure { params, statements }),
        };
        self.add_item(name, item)
    }
}
