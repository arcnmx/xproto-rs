use xcbgen::defs as xcbdefs;
use std::collections::hash_map::{HashMap, Entry as HashMapEntry};

/// Specifies how the value of a field can be calculated
/// from other fields.
#[derive(Debug)]
pub enum DeducibleField {
    /// The value is the length of a list.
    ///
    /// `(list name, operation)`
    LengthOf(String, DeducibleLengthFieldOp),
    /// The value is the discriminant of a case switch
    ///
    /// `(switch name)`
    CaseSwitchExpr(String, DeducibleFieldOp),
    /// The value is the discriminant of a bitcase switch
    ///
    /// `(switch name)`
    BitCaseSwitchExpr(String, DeducibleFieldOp),
    RequestOpcode,
    RequestLength,
}

#[derive(Clone, Debug)]
pub enum DeducibleLengthFieldOp {
    /// `deduced field = list length`
    None,
    /// `deduced field = list length * n`
    Mul(u32),
    /// `deduced field = list length / n`
    Div(u32),
}

#[derive(Debug, Clone)]
pub enum DeducibleFieldOp {
    /// `deduced field = value`.
    None,
    /// `deduced field = value | expr`.
    Or(Box<xcbdefs::Expression>),
}

/// Gathers deducible fields (fields whose value can be calculated
/// from other fields) from a list of fields.
pub fn gather_deducible_fields(fields: &[&xcbdefs::FieldDef]) -> HashMap<String, DeducibleField> {
    fn extract_length(expr: &xcbdefs::Expression) -> Option<(String, DeducibleLengthFieldOp)> {
        match expr {
            xcbdefs::Expression::FieldRef(field_ref_expr) => Some((
                field_ref_expr.field_name.clone(),
                DeducibleLengthFieldOp::None,
            )),
            xcbdefs::Expression::BinaryOp(bin_op_expr) => {
                if bin_op_expr.operator == xcbdefs::BinaryOperator::Mul {
                    match (&*bin_op_expr.lhs, &*bin_op_expr.rhs) {
                        (
                            xcbdefs::Expression::FieldRef(field_ref_expr),
                            xcbdefs::Expression::Value(value),
                        ) => Some((
                            field_ref_expr.field_name.clone(),
                            DeducibleLengthFieldOp::Div(*value),
                        )),
                        (
                            xcbdefs::Expression::Value(value),
                            xcbdefs::Expression::FieldRef(field_ref_expr),
                        ) => Some((
                            field_ref_expr.field_name.clone(),
                            DeducibleLengthFieldOp::Div(*value),
                        )),
                        _ => None,
                    }
                } else if bin_op_expr.operator == xcbdefs::BinaryOperator::Div {
                    match (&*bin_op_expr.lhs, &*bin_op_expr.rhs) {
                        (
                            xcbdefs::Expression::FieldRef(field_ref_expr),
                            xcbdefs::Expression::Value(value),
                        ) => Some((
                            field_ref_expr.field_name.clone(),
                            DeducibleLengthFieldOp::Mul(*value),
                        )),
                        _ => None,
                    }
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    let mut deducible_fields = HashMap::new();
    for field in fields.iter() {
        let deducible_field = match field {
            xcbdefs::FieldDef::List(list_field) => list_field
                .length_expr
                .as_ref()
                .and_then(extract_length)
                .map(|(len_field_name, op)| {
                    (
                        len_field_name,
                        DeducibleField::LengthOf(list_field.name.clone(), op),
                    )
                }),
            xcbdefs::FieldDef::FdList(fd_list_field) => extract_length(&fd_list_field.length_expr)
                .map(|(len_field_name, op)| {
                    (
                        len_field_name,
                        DeducibleField::LengthOf(fd_list_field.name.clone(), op),
                    )
                }),
            xcbdefs::FieldDef::Switch(switch_field) => {
                if switch_field.cases.iter().any(|case| case.exprs.len() != 1) {
                    None
                } else if switch_field.kind == xcbdefs::SwitchKind::Case {
                    if let xcbdefs::Expression::FieldRef(ref field_ref_expr) = switch_field.expr {
                        Some((
                            field_ref_expr.field_name.clone(),
                            DeducibleField::CaseSwitchExpr(
                                switch_field.name.clone(),
                                DeducibleFieldOp::None,
                            ),
                        ))
                    } else {
                        unreachable!("Can't figure out deducible field of {:#?}", switch_field);
                    }
                } else if switch_field.kind == xcbdefs::SwitchKind::BitCase {
                    if let xcbdefs::Expression::FieldRef(ref field_ref_expr) = switch_field.expr {
                        Some((
                            field_ref_expr.field_name.clone(),
                            DeducibleField::BitCaseSwitchExpr(
                                switch_field.name.clone(),
                                DeducibleFieldOp::None,
                            ),
                        ))
                    } else if let xcbdefs::Expression::BinaryOp(ref binary_op_expr) =
                        switch_field.expr
                    {
                        if let xcbdefs::Expression::FieldRef(ref field_ref_expr) =
                            *binary_op_expr.lhs
                        {
                            match binary_op_expr.operator {
                                xcbdefs::BinaryOperator::And => {
                                    // This appears in XKB:SelectEvents.
                                    // There we're provided the additional constraint that
                                    // the right hand side of this operation is a strict superset of
                                    // the left hand side. Therefore, we can negate the right
                                    // hand side and OR it with the switch field to undo the
                                    // AND and deduce affectWhich.
                                    // Because this is not true in general, we assert this is
                                    // the field we expect.
                                    assert_eq!(field_ref_expr.field_name, "affectWhich");
                                    let rhs = binary_op_expr.rhs.clone();
                                    Some((
                                        field_ref_expr.field_name.clone(),
                                        DeducibleField::BitCaseSwitchExpr(
                                            switch_field.name.clone(),
                                            DeducibleFieldOp::Or(rhs.negate()),
                                        ),
                                    ))
                                }
                                // No other operators are actually used.
                                _ => unreachable!(),
                            }
                        } else {
                            unreachable!("Can't figure out deducible field of {:#?}", switch_field);
                        }
                    } else {
                        unreachable!("Can't figure out deducible field of {:#?}", switch_field);
                    }
                } else {
                    None
                }
            }
            _ => None,
        };

        if let Some((field_name, deducible_field)) = deducible_field {
            let is_not_ext_param = fields
                .iter()
                .any(|field| field.name() == Some(field_name.as_str()));

            if is_not_ext_param {
                match deducible_fields.entry(field_name) {
                    HashMapEntry::Occupied(_) => {
                        // field used more than once,
                        // deduce it from the first use
                        // (do not replace entry)
                    }
                    HashMapEntry::Vacant(entry) => {
                        entry.insert(deducible_field);
                    }
                }
            }
        }
    }

    deducible_fields
}
