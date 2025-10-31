use std::io::Write;

use program_structure::ast::*;

use crate::expression::*;

pub fn export_statement(writer: &mut Box<dyn Write>, stmt: &Statement) {
    use Statement::*;
    match stmt {
        IfThenElse { cond, if_case, else_case, .. } => {
            write!(writer, "{{\"$\": \"Stmt\", ").unwrap();
            write!(writer, "\"@\": \"IfThenElse\", ").unwrap();
            write!(writer, "\"cond\": ").unwrap();
            export_expression(writer, cond);
            write!(writer, ", \"if_case\": ").unwrap();
            export_statement(writer, if_case);
            write!(writer, ",").unwrap();
            write!(writer, "\"else_case\": ").unwrap();
            if let Some(else_stmt) = else_case {
                export_statement(writer, else_stmt);
            } else {
                write!(writer, "null").unwrap();
            }
            write!(writer, "}}").unwrap();
        }
        While { cond, stmt, .. } => {
            write!(writer, "{{\"$\": \"Stmt\", ").unwrap();
            write!(writer, "\"@\": \"While\", ").unwrap();
            write!(writer, "\"cond\": ").unwrap();
            export_expression(writer, cond);
            write!(writer, ", \"stmt\": ").unwrap();
            export_statement(writer, stmt);
            write!(writer, "}}").unwrap();
        }
        Return { value, .. } => {
            write!(writer, "{{\"$\": \"Stmt\", ").unwrap();
            write!(writer, "\"@\": \"Return\", ").unwrap();
            write!(writer, "\"value\": ").unwrap();
            export_expression(writer, value);
            write!(writer, "}}").unwrap();
        }
        InitializationBlock { xtype, initializations, .. } => {
            write!(writer, "{{\"$\": \"Stmt\",").unwrap();
            write!(writer, "\"@\": \"InitializationBlock\", ").unwrap();
            write!(writer, "\"xtype\": ").unwrap();
            export_variable_type(writer, xtype);
            write!(writer, ", \"initializations\": [").unwrap();
            for (i, stmt) in initializations.iter().enumerate() {
                if i > 0 {
                    writeln!(writer, ",").unwrap();
                } else {
                    writeln!(writer).unwrap();
                }
                export_statement(writer, stmt);
            }
            write!(writer, "]}}").unwrap();
        }
        Declaration { xtype, name, dimensions, is_constant, is_anonymous, .. } => {
            write!(writer, "{{\"$\": \"Stmt\", ").unwrap();
            write!(writer, "\"@\": \"Declaration\", ").unwrap();
            write!(writer, "\"xtype\": ").unwrap();
            export_variable_type(writer, xtype);
            write!(writer, ", \"name\": \"{}\", ", name).unwrap();
            write!(writer, "\"dimensions\": ").unwrap();
            export_expression_list(writer, dimensions);
            write!(writer, ", \"is_constant\": {}", is_constant).unwrap();
            write!(writer, ", \"is_anonymous\": {}", is_anonymous).unwrap();
            write!(writer, "}}").unwrap();
        }
        Substitution { var, access, op, rhe, .. } => {
            write!(writer, "{{\"$\": \"Stmt\", ").unwrap();
            write!(writer, "\"@\": \"Substitution\", ").unwrap();
            write!(writer, "\"var\": \"{}\", ", var).unwrap();
            write!(writer, "\"access\": ").unwrap();
            export_access_list(writer, access);
            write!(writer, ", \"op\": \"{:?}\", ", op).unwrap();
            write!(writer, "\"rhe\": ").unwrap();
            export_expression(writer, rhe);
            write!(writer, "}}").unwrap();
        }
        MultSubstitution { .. } => unreachable!(),
        UnderscoreSubstitution { op, rhe, .. } => {
            write!(writer, "{{\"$\": \"Stmt\", ").unwrap();
            write!(writer, "\"@\": \"UnderscoreSubstitution\", ").unwrap();
            write!(writer, "\"op\": \"{:?}\", ", op).unwrap();
            write!(writer, "\"rhe\": ").unwrap();
            export_expression(writer, rhe);
            write!(writer, "}}").unwrap();
        }
        ConstraintEquality { lhe, rhe, .. } => {
            write!(writer, "{{\"$\": \"Stmt\", ").unwrap();
            write!(writer, "\"@\": \"ConstraintEquality\", ").unwrap();
            write!(writer, "\"lhe\": ").unwrap();
            export_expression(writer, lhe);
            write!(writer, ", \"rhe\": ").unwrap();
            export_expression(writer, rhe);
            write!(writer, "}}").unwrap();
        }
        LogCall { args, .. } => {
            write!(writer, "{{\"$\": \"Stmt\", ").unwrap();
            write!(writer, "\"@\": \"LogCall\", ").unwrap();
            write!(writer, "\"args\": ").unwrap();
            export_log_argument_list(writer, args);
            write!(writer, "}}").unwrap();
        }
        Block { stmts, .. } => {
            write!(writer, "{{\"$\": \"Stmt\", ").unwrap();
            write!(writer, "\"@\": \"Block\", ").unwrap();
            write!(writer, "\"stmts\": [").unwrap();
            for (i, stmt) in stmts.iter().enumerate() {
                if i > 0 {
                    writeln!(writer, ",").unwrap();
                } else {
                    writeln!(writer).unwrap();
                }
                export_statement(writer, stmt);
            }
            write!(writer, "]}}").unwrap();
        }
        Assert { arg, .. } => {
            write!(writer, "{{\"$\": \"Stmt\", ").unwrap();
            write!(writer, "\"@\": \"Assert\", ").unwrap();
            write!(writer, "\"arg\": ").unwrap();
            export_expression(writer, arg);
            write!(writer, "}}").unwrap();
        }
    }
}

fn export_variable_type(writer: &mut Box<dyn Write>, var_type: &VariableType) {
    use VariableType::*;
    match var_type {
        Var => write!(writer, "{{\"$\": \"VarType\", \"@\": \"Var\"}}").unwrap(),
        Signal(signal_type, tag_list) => {
            write!(writer, "{{\"$\": \"VarType\",").unwrap();
            write!(writer, "\"@\": \"Signal\",").unwrap();
            write!(writer, "\"signal_type\": \"{:?}\"", signal_type).unwrap();
            write!(writer, ", \"tag_list\": ").unwrap();
            export_tag_list(writer, tag_list);
            write!(writer, "}}").unwrap();
        }
        Component => write!(writer, "{{\"$\": \"VarType\", \"@\": \"Component\"}}").unwrap(),
        AnonymousComponent => {
            write!(writer, "{{\"$\": \"VarType\", \"@\": \"AnonymousComponent\"}}").unwrap()
        }
        Bus(name, signal_type, tag_list) => {
            write!(writer, "{{\"$\": \"VarType\",").unwrap();
            write!(writer, "\"@\": \"Bus\",").unwrap();
            write!(writer, "\"id\": \"{}\",", name).unwrap();
            write!(writer, "\"signal_type\": \"{:?}\"", signal_type).unwrap();
            write!(writer, ", \"tag_list\": ").unwrap();
            export_tag_list(writer, tag_list);
            write!(writer, "}}").unwrap();
        }
    }
}

fn export_tag_list(writer: &mut Box<dyn Write>, tag_list: &TagList) {
    write!(writer, "[").unwrap();
    for (i, tag) in tag_list.iter().enumerate() {
        if i > 0 {
            write!(writer, ", ").unwrap();
        }
        write!(writer, "\"{}\"", tag).unwrap();
    }
    write!(writer, "]").unwrap();
}

fn export_log_argument(writer: &mut Box<dyn Write>, arg: &LogArgument) {
    use LogArgument::*;
    match arg {
        LogStr(s) => {
            write!(writer, "{{\"$\": \"LogArg\", \"@\": \"LogStr\", \"literal\": \"{}\"}}", s)
                .unwrap();
        }
        LogExp(expr) => {
            write!(writer, "{{\"$\": \"LogArg\", \"@\": \"LogExp\", \"value\": ").unwrap();
            export_expression(writer, expr);
            write!(writer, "}}").unwrap();
        }
    }
}

fn export_log_argument_list(writer: &mut Box<dyn Write>, args: &Vec<LogArgument>) {
    write!(writer, "[").unwrap();
    for (i, arg) in args.iter().enumerate() {
        if i > 0 {
            write!(writer, ", ").unwrap();
        }
        export_log_argument(writer, arg);
    }
    write!(writer, "]").unwrap();
}
