use crate::parser::ast::Statement;

#[derive(Debug, PartialEq, Clone)]
pub struct CodeGenError {
    pub error_msg: String,
}

pub type Result<T> = std::result::Result<T, CodeGenError>;

pub fn generate_assembly(statements: Vec<Statement>) -> Result<Vec<String>> {
    todo!()
}
