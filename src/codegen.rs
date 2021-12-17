mod code_file;
mod symbol_table;

use crate::parser::ast::{Expression, PrintExpr, Statement, TypedVar};
use code_file::CodeFile;
use log::info;
use symbol_table::SymbolTable;

#[derive(Debug, PartialEq, Clone)]
pub struct CodeGenError {
    pub error_msg: String,
}

impl Into<CodeGenError> for String {
    fn into(self) -> CodeGenError {
        CodeGenError { error_msg: self }
    }
}

impl Into<CodeGenError> for &str {
    fn into(self) -> CodeGenError {
        CodeGenError {
            error_msg: self.to_owned(),
        }
    }
}

pub type Result<T> = std::result::Result<T, CodeGenError>;

pub fn generate_assembly(statements: Vec<Statement>) -> Result<String> {
    let mut code = CodeFile::new();
    let mut table = SymbolTable::new();

    for statement in statements {
        match statement {
            Statement::Declaration {
                name_and_type: TypedVar::Num(name),
                expression: Expression::NumberLiteral(value),
            } => {
                let label = table.new_const_num(name)?;
                code.data.push(format!("{} dd {}", label, value));
            }
            Statement::Declaration {
                name_and_type: _,
                expression: _,
            } => info!("Unsupported declaration"),
            Statement::ProcedureDeclaration {
                name_and_type: _,
                params: _,
                statements: _,
            } => info!("unsupported procedure declaration"),
            Statement::ReturnStatement(_) => info!("Unsupported return statement"),
            Statement::PrintStatement(PrintExpr::String(to_print)) => {
                let label = table.get_string_label();
                code.rodata
                    .push(format!(r#"{} db "{}",0"#, label, to_print));
                code.main.extend_from_slice(&[
                    format!("mov esi, {}", label),
                    "mov edi, stringPrinter".to_owned(),
                    "mov eax, 0".to_owned(),
                    "call printf".to_owned(),
                ])
            }
            Statement::PrintStatement(PrintExpr::Num(name)) => {
                let label = table.get_num_label(name)?;
                code.main.extend_from_slice(&[
                    "mov rdi, numPrinter".to_owned(),
                    format!("mov rsi, [{}]", label),
                    "mov rax, 0".to_owned(),
                    "call printf".to_owned(),
                ]);
            }
            Statement::PrintStatement(_) => info!("Unsupported print statement"),
            Statement::ReadStatement(variable) => {
                let label = match table.get_num_label(variable) {
                    Ok(label) => label,
                    Err(CodeGenError { error_msg }) => {
                        info!("Error in read statment: {}", error_msg);
                        continue;
                    }
                };
                code.main.extend_from_slice(&[
                    "lea rdi, [numReader]".to_owned(),
                    format!("lea rsi, [{}]", label),
                    "mov rax, 0".to_owned(),
                    "call scanf".to_owned(),
                ])
            }
        }
    }

    // Add return
    code.main.extend_from_slice(&[
        "mov eax, 0".to_owned(),
        "pop rbp".to_owned(),
        "ret".to_owned(),
    ]);

    Ok(format!("{}", code))
}

#[cfg(test)]
mod test {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    fn flatten_lines(input: &str) -> Vec<String> {
        input
            .lines()
            .map(|line| line.split_whitespace().collect::<Vec<&str>>().join(" "))
            .collect()
    }

    #[test]
    fn flatten_lines_works() {
        let output = flatten_lines(indoc! {"
            global main
                        section .bss
            _n_0_num1   resq 1
                        section .text
            main:
                        mov     rax, 60
                        xor     rdi, rdi
                        syscall
            "});
        let expected = vec![
            "global main".to_owned(),
            "section .bss".into(),
            "_n_0_num1 resq 1".into(),
            "section .text".into(),
            "main:".into(),
            "mov rax, 60".into(),
            "xor rdi, rdi".into(),
            "syscall".into(),
        ];
        assert_eq!(output, expected);
    }

    #[test]
    fn empty_program_generates_correctly() {
        let statements = vec![];
        let assembly = generate_assembly(statements)
            .unwrap()
            .lines()
            .map(|x| x.to_owned())
            .collect::<Vec<_>>();
        let expected = flatten_lines(indoc! {r#"
            global main
            extern printf
            extern scanf
                            section .rodata
            numPrinter      db "%d",0x0a,0
            ishPrinter      db "%f",0x0a,0
            stringPrinter   db "%s",0x0a,0
            numReader       db "%d",0
            ishReader       db "%f",0
                            section .data
                            section .bss
                            section .text
            main:
                            push    rbp
                            mov     rbp, rsp
                            mov     eax, 0
                            pop     rbp
                            ret
            "#});
        assert_eq!(assembly, expected);
    }

    #[test]
    fn program_can_print_string() {
        let statements = vec![
            Statement::PrintStatement(PrintExpr::String("Hello,".to_owned())),
            Statement::PrintStatement(PrintExpr::String("World!".to_owned())),
        ];
        let assembly = generate_assembly(statements)
            .unwrap()
            .lines()
            .map(|x| x.to_owned())
            .collect::<Vec<_>>();
        let expected = flatten_lines(indoc! {r#"
            global main
            extern printf
            extern scanf
                            section .rodata
            numPrinter      db "%d",0x0a,0
            ishPrinter      db "%f",0x0a,0
            stringPrinter   db "%s",0x0a,0
            numReader       db "%d",0
            ishReader       db "%f",0
            _str_0          db "Hello,",0
            _str_1          db "World!",0
                            section .data
                            section .bss
                            section .text
            main:
                            push    rbp
                            mov     rbp, rsp
                            mov     esi, _str_0
                            mov     edi, stringPrinter
                            mov     eax, 0
                            call    printf
                            mov     esi, _str_1
                            mov     edi, stringPrinter
                            mov     eax, 0
                            call    printf
                            mov     eax, 0
                            pop     rbp
                            ret
            "#});
        assert_eq!(assembly, expected);
    }

    #[test]
    fn programs_can_have_constant_integers() {
        let statements = vec![
            Statement::Declaration {
                name_and_type: TypedVar::Num("a".to_owned()),
                expression: Expression::NumberLiteral(42),
            },
            Statement::Declaration {
                name_and_type: TypedVar::Num("b".to_owned()),
                expression: Expression::NumberLiteral(3),
            },
        ];
        let assembly = generate_assembly(statements)
            .unwrap()
            .lines()
            .map(|x| x.to_owned())
            .collect::<Vec<_>>();
        let expected = flatten_lines(indoc! {r#"
            global main
            extern printf
            extern scanf
                            section .rodata
            numPrinter      db "%d",0x0a,0
            ishPrinter      db "%f",0x0a,0
            stringPrinter   db "%s",0x0a,0
            numReader       db "%d",0
            ishReader       db "%f",0
                            section .data
            _num_const_0_a  dd 42
            _num_const_1_b  dd 3
                            section .bss
                            section .text
            main:
                            push    rbp
                            mov     rbp, rsp
                            mov     eax, 0
                            pop     rbp
                            ret
            "#});
        assert_eq!(assembly, expected);
    }

    #[test]
    pub fn programs_can_print_nums() {
        let statements = vec![
            Statement::Declaration {
                name_and_type: TypedVar::Num("a".to_owned()),
                expression: Expression::NumberLiteral(42),
            },
            Statement::PrintStatement(PrintExpr::Num("a".to_owned())),
        ];
        let assembly = generate_assembly(statements)
            .unwrap()
            .lines()
            .map(|x| x.to_owned())
            .collect::<Vec<_>>();
        let expected = flatten_lines(indoc! {r#"
            global main
            extern printf
            extern scanf
                            section .rodata
            numPrinter      db "%d",0x0a,0
            ishPrinter      db "%f",0x0a,0
            stringPrinter   db "%s",0x0a,0
            numReader       db "%d",0
            ishReader       db "%f",0
                            section .data
            _num_const_0_a  dd 42
                            section .bss
                            section .text
            main:
                            push    rbp
                            mov     rbp, rsp
                            mov     rdi, numPrinter
                            mov     rsi, [_num_const_0_a]
                            mov     rax, 0
                            call    printf
                            mov     eax, 0
                            pop     rbp
                            ret
            "#});
        assert_eq!(assembly, expected);
    }

    #[test]
    fn programs_can_read_nums() {
        let statements = vec![
            Statement::Declaration {
                name_and_type: TypedVar::Num("a".to_owned()),
                expression: Expression::NumberLiteral(0),
            },
            Statement::ReadStatement("a".to_owned()),
            Statement::PrintStatement(PrintExpr::Num("a".to_owned())),
        ];
        let assembly = generate_assembly(statements)
            .unwrap()
            .lines()
            .map(|x| x.to_owned())
            .collect::<Vec<_>>();
        let expected = flatten_lines(indoc! {r#"
            global main
            extern printf
            extern scanf
                            section .rodata
            numPrinter      db "%d",0x0a,0
            ishPrinter      db "%f",0x0a,0
            stringPrinter   db "%s",0x0a,0
            numReader       db "%d",0
            ishReader       db "%f",0
                            section .data
            _num_const_0_a  dd 0
                            section .bss
                            section .text
            main:
                            push    rbp
                            mov     rbp, rsp
                            lea     rdi, [numReader]
                            lea     rsi, [_num_const_0_a]
                            mov     rax, 0
                            call    scanf
                            mov     rdi, numPrinter
                            mov     rsi, [_num_const_0_a]
                            mov     rax, 0
                            call    printf
                            mov     eax, 0
                            pop     rbp
                            ret
            "#});
        assert_eq!(assembly, expected);
    }
}
