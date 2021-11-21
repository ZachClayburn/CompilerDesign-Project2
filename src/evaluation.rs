use crate::parser::ast::Statement;
use either::{Either, Left, Right};

#[derive(Debug, PartialEq)]
pub struct EvaluationError {
    pub bad_statement: Statement,
    pub error_msg: String,
}

pub fn evaluate(statements: Vec<Statement>) -> Vec<Either<Statement, EvaluationError>> {
    // let mut evaluated = Vec::new();
    let evaluated = statements.into_iter().map(|x| Left(x)).collect();
    evaluated
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{
        parser::{
            ast::{CompilationUnit, Expression, TypedVar},
            parse,
        },
        scanner::Scanner,
    };
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    #[test]
    fn simple_int_math_statements_can_be_evaluated() {
        let scan = Scanner::from_text(indoc! {"
            program test; begin
            num a = 1 + 1;
            num b = 2 * 2;
            num c = 3 / 3;
            num d = 4 - 4;
            num e = 5 ^ 5;
            end.
        "});
        let CompilationUnit {
            statements: parsed, ..
        } = parse(scan).unwrap();

        let out = evaluate(parsed);

        let expected = vec![
            Left(Statement::Declaration {
                name_and_type: TypedVar::Num("a".into()),
                expression: Expression::NumberLiteral(2),
            }),
            Left(Statement::Declaration {
                name_and_type: TypedVar::Num("b".into()),
                expression: Expression::NumberLiteral(4),
            }),
            Left(Statement::Declaration {
                name_and_type: TypedVar::Num("c".into()),
                expression: Expression::NumberLiteral(1),
            }),
            Left(Statement::Declaration {
                name_and_type: TypedVar::Num("d".into()),
                expression: Expression::NumberLiteral(0),
            }),
            Left(Statement::Declaration {
                name_and_type: TypedVar::Num("e".into()),
                expression: Expression::NumberLiteral(3125),
            }),
        ];

        assert_eq!(out, expected);
    }
}
