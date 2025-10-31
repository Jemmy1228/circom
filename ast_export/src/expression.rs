use std::io::Write;

use program_structure::ast::*;

pub fn export_expression(writer: &mut Box<dyn Write>, expr: &Expression) {
    use Expression::*;
    match expr {
        InfixOp { lhe, infix_op, rhe, .. } => {
            write!(writer, "{{\"$\": \"Expr\", ").unwrap();
            write!(writer, "\"@\": \"InfixOp\", ").unwrap();
            write!(writer, "\"infix_op\": \"{:?}\", ", infix_op).unwrap();
            write!(writer, "\"lhe\": ").unwrap();
            export_expression(writer, lhe);
            write!(writer, ", \"rhe\": ").unwrap();
            export_expression(writer, rhe);
            write!(writer, "}}").unwrap();
        }
        PrefixOp { prefix_op, rhe, .. } => {
            write!(writer, "{{\"$\": \"Expr\", ").unwrap();
            write!(writer, "\"@\": \"PrefixOp\", ").unwrap();
            write!(writer, "\"prefix_op\": \"{:?}\", ", prefix_op).unwrap();
            write!(writer, "\"rhe\": ").unwrap();
            export_expression(writer, rhe);
            write!(writer, "}}").unwrap();
        }
        InlineSwitchOp { cond, if_true, if_false, .. } => {
            write!(writer, "{{\"$\": \"Expr\", ").unwrap();
            write!(writer, "\"@\": \"InlineSwitchOp\", ").unwrap();
            write!(writer, "\"cond\": ").unwrap();
            export_expression(writer, cond);
            write!(writer, ", \"if_true\": ").unwrap();
            export_expression(writer, if_true);
            write!(writer, ", \"if_false\": ").unwrap();
            export_expression(writer, if_false);
            write!(writer, "}}").unwrap();
        }
        ParallelOp { rhe, .. } => {
            write!(writer, "{{\"$\": \"Expr\", ").unwrap();
            write!(writer, "\"@\": \"ParallelOp\", ").unwrap();
            write!(writer, "\"rhe\": ").unwrap();
            export_expression(writer, rhe);
            write!(writer, "}}").unwrap();
        }
        Variable { name, access, .. } => {
            write!(writer, "{{\"$\": \"Expr\", ").unwrap();
            write!(writer, "\"@\": \"Variable\", ").unwrap();
            write!(writer, "\"name\": \"{}\", ", name).unwrap();
            write!(writer, "\"access\": ").unwrap();
            export_access_list(writer, access);
            write!(writer, "}}").unwrap();
        }
        Number(_, value) => {
            write!(writer, "{{\"$\": \"Expr\", ").unwrap();
            write!(writer, "\"@\": \"Number\", ").unwrap();
            write!(writer, "\"value\": \"{}\"}}", value).unwrap();
        }
        Call { id, args, .. } => {
            write!(writer, "{{\"$\": \"Expr\", ").unwrap();
            write!(writer, "\"@\": \"Call\", ").unwrap();
            write!(writer, "\"id\": \"{}\", ", id).unwrap();
            write!(writer, "\"args\": ").unwrap();
            export_expression_list(writer, args);
            write!(writer, "}}").unwrap();
        }
        BusCall { id, args, .. } => {
            write!(writer, "{{\"$\": \"Expr\", ").unwrap();
            write!(writer, "\"@\": \"BusCall\", ").unwrap();
            write!(writer, "\"id\": \"{}\", ", id).unwrap();
            write!(writer, "\"args\": ").unwrap();
            export_expression_list(writer, args);
            write!(writer, "}}").unwrap();
        }
        AnonymousComp { .. } => unreachable!(),
        ArrayInLine { values, .. } => {
            write!(writer, "{{\"$\": \"Expr\", ").unwrap();
            write!(writer, "\"@\": \"ArrayInLine\", ").unwrap();
            write!(writer, "\"values\": ").unwrap();
            export_expression_list(writer, values);
            write!(writer, "}}").unwrap();
        }
        Tuple { .. } => unreachable!(),
        UniformArray { value, dimension, .. } => {
            write!(writer, "{{\"$\": \"Expr\", ").unwrap();
            write!(writer, "\"@\": \"UniformArray\", ").unwrap();
            write!(writer, "\"value\": ").unwrap();
            export_expression(writer, value);
            write!(writer, ", \"dimension\": ").unwrap();
            export_expression(writer, dimension);
            write!(writer, "}}").unwrap();
        }
    }
}

pub fn export_expression_list(writer: &mut Box<dyn Write>, expr_list: &Vec<Expression>) {
    write!(writer, "[").unwrap();
    for (i, expr) in expr_list.iter().enumerate() {
        if i > 0 {
            write!(writer, ", ").unwrap();
        }
        export_expression(writer, expr);
    }
    write!(writer, "]").unwrap();
}

pub fn export_access(writer: &mut Box<dyn Write>, access: &Access) {
    use Access::*;
    match access {
        ComponentAccess(name) => {
            write!(
                writer,
                "{{\"$\": \"Access\", \"@\": \"ComponentAccess\", \"name\": \"{}\"}}",
                name
            )
            .unwrap();
        }
        ArrayAccess(expr) => {
            write!(writer, "{{\"$\": \"Access\", \"@\": \"ArrayAccess\", \"index\": ").unwrap();
            export_expression(writer, expr);
            write!(writer, "}}").unwrap();
        }
    }
}

pub fn export_access_list(writer: &mut Box<dyn Write>, access_list: &Vec<Access>) {
    write!(writer, "[").unwrap();
    for (i, access) in access_list.iter().enumerate() {
        if i > 0 {
            write!(writer, ", ").unwrap();
        }
        export_access(writer, access);
    }
    write!(writer, "]").unwrap();
}
