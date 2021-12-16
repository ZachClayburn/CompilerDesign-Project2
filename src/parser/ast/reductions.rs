use super::*;
use crate::scanner::Token;
use crate::scanner::TokenInfo;
use either::{Left, Right};

pub fn get_reduction_name(function_pointer: &ReductionOp) -> &'static str {
    match *function_pointer {
        x if x == reduce_value => "reduce_value()",
        x if x == reduce_binary_op => "reduce_binary_op()",
        x if x == reduce_parenthetical => "reduce_parenthetical()",
        x if x == reduce_unary_operator => "reduce_unary_operator()",
        x if x == reduce_program => "reduce_program()",
        x if x == reduce_statement_list => "reduce_statement_list()",
        x if x == reduce_declaration => "reduce_declaration()",
        x if x == reduce_param_spec => "reduce_param_spec()",
        x if x == reduce_return_statement => "reduce_return_statement()",
        x if x == reduce_procedure_declaration => "reduce_procedure_declaration()",
        x if x == reduce_procedure_call => "reduce_procedure_call()",
        _ => panic!("unknown reduction"),
    }
}

pub fn reduce_value(mut stack: Vec<ValueItem>) -> Result<Vec<ValueItem>> {
    use Expression::*;
    use AST::*;
    match stack.pop() {
        Some(Right(Token::Number(info))) => {
            let number = info.content.parse()?;
            stack.push(Left(Expr(NumberLiteral(number))));
            Ok(stack)
        }
        Some(Right(Token::Identifier(info))) => {
            stack.push(Left(Expr(Variable(info.content))));
            Ok(stack)
        }
        Some(Right(Token::Float(info))) => {
            let number = info.content.parse()?;
            stack.push(Left(Expr(FloatLiteral(number))));
            Ok(stack)
        }
        Some(unexpected) => Err(format!(
            "Reducion error: Expectied a number or variable, but {} was found",
            unexpected
        )
        .into()),
        None => Err("Attempting to reduce a value, but the stack is empty!".into()),
    }
}

pub fn reduce_binary_op(mut stack: Vec<ValueItem>) -> Result<Vec<ValueItem>> {
    use Expression::*;
    use Token::*;
    use AST::*;
    let rhs = match stack.pop() {
        Some(Left(Expr(expr))) => expr,
        Some(bad) => {
            return Err(format!("Reducion error: Expected an expression, found {}", bad).into())
        }
        None => {
            return Err("Reducion error: Missing ) while trying to reduce parenthetical".into())
        }
    };
    let op = match stack.pop().unwrap().unwrap_right() {
        Plus(_) => Operator::Plus,
        Minus(_) => Operator::Minus,
        Star(_) => Operator::Multiply,
        Div(_) => Operator::Divide,
        Pow(_) => Operator::Power,
        bad => return Err(format!("Reducion error: {} not a valid binary operator", bad).into()),
    };
    let lhs = match stack.pop() {
        Some(Left(Expr(expr))) => expr,
        Some(bad) => {
            return Err(format!("Reducion error: Expected an expression, found {}", bad).into())
        }
        None => {
            return Err("Reducion error: Missing ) while trying to reduce parenthetical".into())
        }
    };

    stack.push(Left(Expr(BinaryOperation(
        Box::new(lhs),
        op,
        Box::new(rhs),
    ))));

    Ok(stack)
}

pub fn reduce_parenthetical(mut stack: Vec<ValueItem>) -> Result<Vec<ValueItem>> {
    match stack.pop() {
        Some(Right(Token::RParen(_))) => (),
        Some(bad) => return Err(format!("Reducion error: Expected ), found {}", bad).into()),
        None => {
            return Err("Reducion error: Missing ) while trying to reduce parenthetical".into())
        }
    };
    let expr = match stack.pop() {
        Some(Left(expr)) => expr,
        Some(bad) => {
            return Err(format!("Reducion error: Expected expression, found {}", bad).into())
        }
        None => {
            return Err(
                "Reducion error: Missing expression while trying to reduce parenthetical".into(),
            )
        }
    };
    match stack.pop() {
        Some(Right(Token::LParen(_))) => (),
        Some(bad) => return Err(format!("Reducion error: Expected (, found {}", bad).into()),
        None => {
            return Err("Reducion error: Missing ( while trying to reduce parenthetical".into())
        }
    };

    stack.push(Left(expr));
    Ok(stack)
}

pub fn reduce_unary_operator(mut stack: Vec<ValueItem>) -> Result<Vec<ValueItem>> {
    use AST::*;
    let expr = match stack.pop() {
        Some(Left(Expr(expr))) => expr,
        Some(bad) => {
            return Err(format!("Reducion error: Expected expression, found {}", bad).into())
        }
        None => {
            return Err(
                "Reducion error: Missing expression while trying to reduce unary operator".into(),
            )
        }
    };
    match stack.pop() {
        Some(Right(Token::Minus(_))) => (),
        Some(bad) => return Err(format!("Reducion error: Expected -, found {}", bad).into()),
        None => {
            return Err("Reducion error: Missing - while trying to reduce unary operator".into())
        }
    };

    stack.push(Left(Expr(Expression::UnaryOperation(
        Operator::Minus,
        Box::new(expr),
    ))));
    Ok(stack)
}

pub fn reduce_program(mut stack: Vec<ValueItem>) -> Result<Vec<ValueItem>> {
    use Token::*;

    match stack.pop() {
        Some(Right(Dot(_))) => (),
        Some(bad) => return Err(format!("Reducion error: Expected ., but found {}", bad).into()),
        None => {
            return Err(format!("Reducion error: Missing . while trying to reduce program").into())
        }
    };

    match stack.pop() {
        Some(Right(End(_))) => (),
        Some(bad) => return Err(format!("Reducion error: Expected end, but found {}", bad).into()),
        None => {
            return Err(
                format!("Reducion error: Missing end while trying to reduce program").into(),
            )
        }
    };

    let statents = match stack.pop() {
        Some(Left(AST::StmntList(list))) => list,
        Some(bad) => {
            return Err(
                format!("Reducion error: Expected Statement List, but found {}", bad).into(),
            )
        }
        None => {
            return Err(format!(
                "Reducion error: Missing Statement List while trying to reduce program"
            )
            .into())
        }
    };

    match stack.pop() {
        Some(Right(Begin(_))) => (),
        Some(bad) => {
            return Err(format!("Reducion error: Expected begin, but found {}", bad).into())
        }
        None => {
            return Err(
                format!("Reducion error: Missing begin while trying to reduce program").into(),
            )
        }
    };

    match stack.pop() {
        Some(Right(Semicolon(_))) => (),
        Some(bad) => return Err(format!("Reducion error: Expected ;, but found {}", bad).into()),
        None => {
            return Err(format!("Reducion error: Missing ; while trying to reduce program").into())
        }
    };

    let id = match stack.pop() {
        Some(Right(Identifier(TokenInfo { content, .. }))) => content,
        Some(bad) => {
            return Err(format!("Reducion error: Expected program name, but found {}", bad).into())
        }
        None => {
            return Err(format!(
                "Reducion error: Missing program name while trying to reduce program"
            )
            .into())
        }
    };

    match stack.pop() {
        Some(Right(Program(_))) => (),
        Some(bad) => {
            return Err(format!("Reducion error: Expected program, but found {}", bad).into())
        }
        None => {
            return Err(
                format!("Reducion error: Missing program while trying to reduce program").into(),
            )
        }
    };

    stack.push(Left(AST::Prog(CompilationUnit {
        name: id,
        statements: statents,
    })));

    Ok(stack)
}

pub fn reduce_statement_list(mut stack: Vec<ValueItem>) -> Result<Vec<ValueItem>> {
    let mut statements = vec![];

    loop {
        if let Some(top) = stack.pop() {
            if let Left(AST::Stmnt(stmnt)) = top {
                statements.push(stmnt);
            } else {
                stack.push(top);
                break;
            }
        } else {
            break;
        }
    }

    statements.reverse();

    stack.push(Left(AST::StmntList(statements)));
    Ok(stack)
}

pub fn reduce_declaration(mut stack: Vec<ValueItem>) -> Result<Vec<ValueItem>> {
    use Token::*;

    match stack.pop() {
        Some(Right(Semicolon(_))) => (),
        Some(bad) => return Err(format!("Reducion error: Expected ;, but found {}", bad).into()),
        None => {
            return Err(format!("Reducion error: Missing ; while trying to reduce program").into())
        }
    };

    let expression = match stack.pop() {
        Some(Left(AST::Expr(expression))) => expression,
        Some(bad) => {
            return Err(format!("Reducion error: Expected expression, but found {}", bad).into())
        }
        None => {
            return Err(format!(
                "Reducion error: Missing expression while trying to reduce program"
            )
            .into())
        }
    };

    match stack.pop() {
        Some(Right(Assign(_))) => (),
        Some(bad) => return Err(format!("Reducion error: Expected =, but found {}", bad).into()),
        None => {
            return Err(format!("Reducion error: Missing = while trying to reduce program").into())
        }
    };

    let name = match stack.pop() {
        Some(Right(Identifier(TokenInfo { content, .. }))) => content,
        Some(bad) => return Err(format!("Reducion error: Expected name, but found {}", bad).into()),
        None => {
            return Err(
                format!("Reducion error: Missing name while trying to reduce program").into(),
            )
        }
    };

    match stack.pop() {
        Some(Right(Num(_))) => stack.push(Left(AST::Stmnt(Statement::Declaration {
            name_and_type: TypedVar::Num(name),
            expression,
        }))),
        Some(Right(Ish(_))) => stack.push(Left(AST::Stmnt(Statement::Declaration {
            name_and_type: TypedVar::Ish(name),
            expression,
        }))),
        Some(bad) => return Err(format!("Reducion error: Expected Num, but found {}", bad).into()),
        None => {
            return Err(
                format!("Reducion error: Missing Num while trying to reduce program").into(),
            )
        }
    };

    Ok(stack)
}

pub fn reduce_param_spec(mut stack: Vec<ValueItem>) -> Result<Vec<ValueItem>> {
    use Token::*;

    match stack.pop() {
        Some(Right(RParen(_))) => (),
        Some(bad) => return Err(format!("Reducion error: Expected ), but found {}", bad).into()),
        None => {
            return Err(format!("Reducion error: Missing ) while trying to reduce program").into())
        }
    };

    let mut params = Vec::new();

    loop {
        let name = match stack.pop() {
            Some(Right(LParen(_))) => break,
            Some(Right(Identifier(TokenInfo { content, .. }))) => content,
            Some(bad) => {
                return Err(
                    format!("Reducion error: Expected Identifier, but found {}", bad).into(),
                )
            }
            None => {
                return Err(format!(
                    "Reducion error: Missing Identifier while trying to reduce program"
                )
                .into())
            }
        };
        let param = match stack.pop() {
            Some(Right(Num(_))) => TypedVar::Num(name),
            Some(Right(Ish(_))) => TypedVar::Ish(name),
            Some(bad) => {
                return Err(
                    format!("Reducion error: Expected num or ish, but found {}", bad).into(),
                )
            }
            None => {
                return Err(format!(
                    "Reducion error: Missing num or ish while trying to reduce program"
                )
                .into())
            }
        };
        params.push(param);
        match stack.pop() {
            Some(Right(LParen(_))) => break,
            Some(Right(Comma(_))) => (),
            Some(bad) => {
                return Err(format!("Reducion error: Expected ), but found {}", bad).into())
            }
            None => {
                return Err(
                    format!("Reducion error: Missing ) while trying to reduce program").into(),
                )
            }
        };
    }

    params.reverse();

    stack.push(Left(AST::ParamSpec(params)));

    Ok(stack)
}

pub fn reduce_return_statement(mut stack: Vec<ValueItem>) -> Result<Vec<ValueItem>> {
    use Token::*;

    match stack.pop() {
        Some(Right(Semicolon(_))) => (),
        Some(bad) => return Err(format!("Reducion error: Expected ;, but found {}", bad).into()),
        None => {
            return Err(format!("Reducion error: Missing ; while trying to reduce program").into())
        }
    };

    let expr = match stack.pop() {
        Some(Left(AST::Expr(expr))) => expr,
        Some(bad) => {
            return Err(format!("Reducion error: Expected Expression, but found {}", bad).into())
        }
        None => {
            return Err(format!(
                "Reducion error: Missing Expression while trying to reduce program"
            )
            .into())
        }
    };

    match stack.pop() {
        Some(Right(Return(_))) => (),
        Some(bad) => {
            return Err(format!("Reducion error: Expected Return, but found {}", bad).into())
        }
        None => {
            return Err(
                format!("Reducion error: Missing Return while trying to reduce program").into(),
            )
        }
    };

    stack.push(Left(AST::Stmnt(Statement::ReturnStatement(expr))));

    Ok(stack)
}

pub fn reduce_procedure_declaration(mut stack: Vec<ValueItem>) -> Result<Vec<ValueItem>> {
    use Token::*;

    match stack.pop() {
        Some(Right(RBrace(_))) => (),
        Some(bad) => return Err(format!("Reducion error: Expected }}, but found {}", bad).into()),
        None => {
            return Err(format!("Reducion error: Missing }} while trying to reduce program").into())
        }
    };

    let statements = match stack.pop() {
        Some(Left(AST::StmntList(list))) => list,
        Some(bad) => {
            return Err(
                format!("Reducion error: Expected Statement List, but found {}", bad).into(),
            )
        }
        None => {
            return Err(format!(
                "Reducion error: Missing Statement List while trying to reduce program"
            )
            .into())
        }
    };

    match stack.pop() {
        Some(Right(LBrace(_))) => (),
        Some(bad) => return Err(format!("Reducion error: Expected {{, but found {}", bad).into()),
        None => {
            return Err(format!("Reducion error: Missing {{ while trying to reduce program").into())
        }
    };

    let params = match stack.pop() {
        Some(Left(AST::ParamSpec(list))) => list,
        Some(bad) => {
            return Err(format!("Reducion error: Expected Param Spec, but found {}", bad).into())
        }
        None => {
            return Err(format!(
                "Reducion error: Missing Param Spec while trying to reduce program"
            )
            .into())
        }
    };

    let name = match stack.pop() {
        Some(Right(Identifier(TokenInfo { content, .. }))) => content,
        Some(bad) => {
            return Err(format!("Reducion error: Expected Param Spec, but found {}", bad).into())
        }
        None => {
            return Err(format!(
                "Reducion error: Missing Param Spec while trying to reduce program"
            )
            .into())
        }
    };

    match stack.pop() {
        Some(Right(Procedure(_))) => (),
        Some(bad) => {
            return Err(format!("Reducion error: Expected procedure, but found {}", bad).into())
        }
        None => {
            return Err(
                format!("Reducion error: Missing procedure while trying to reduce program").into(),
            )
        }
    };

    let name_and_type = match stack.pop() {
        Some(Right(Num(_))) => TypedVar::Num(name),
        Some(Right(Ish(_))) => TypedVar::Ish(name),
        Some(bad) => {
            return Err(format!("Reducion error: Expected procedure, but found {}", bad).into())
        }
        None => {
            return Err(
                format!("Reducion error: Missing procedure while trying to reduce program").into(),
            )
        }
    };

    stack.push(Left(AST::Stmnt(Statement::ProcedureDeclaration {
        name_and_type,
        params,
        statements,
    })));

    Ok(stack)
}

pub fn reduce_procedure_call(mut stack: Vec<ValueItem>) -> Result<Vec<ValueItem>> {
    use Token::*;

    match stack.pop() {
        Some(Right(RParen(_))) => (),
        Some(bad) => return Err(format!("Reducion error: Expected ), but found {}", bad).into()),
        None => {
            return Err(format!("Reducion error: Missing ) while trying to reduce program").into())
        }
    };

    let mut args = Vec::new();

    loop {
        let arg = match stack.pop() {
            Some(Right(LParen(_))) => break,
            Some(Left(AST::Expr(expr))) => expr,
            Some(bad) => {
                return Err(
                    format!("Reducion error: Expected Expression, but found {}", bad).into(),
                )
            }
            None => {
                return Err(format!(
                    "Reducion error: Missing Expression while trying to reduce program"
                )
                .into())
            }
        };
        args.push(arg);

        match stack.pop() {
            Some(Right(Comma(_))) => (),
            Some(Right(LParen(_))) => break,
            Some(bad) => {
                return Err(format!("Reducion error: Expected \",\", but found {}", bad).into())
            }
            None => {
                return Err(
                    format!("Reducion error: Missing ) while trying to reduce program").into(),
                )
            }
        };
    }

    args.reverse();

    let name = match stack.pop() {
        Some(Right(Identifier(TokenInfo { content, .. }))) => content,
        Some(bad) => return Err(format!("Reducion error: Expected (, but found {}", bad).into()),
        None => {
            return Err(format!("Reducion error: Missing ( while trying to reduce program").into())
        }
    };

    stack.push(Left(AST::Expr(Expression::ProcedureCall { name, args })));

    Ok(stack)
}
