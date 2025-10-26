use super::modular_arithmetic;
pub use super::modular_arithmetic::ArithmeticError;
use num_bigint::BigInt;
use num_traits::{Num, ToPrimitive, Zero};
use program_structure::ast::{ExpressionInfixOpcode, ExpressionPrefixOpcode};
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::fmt::{Display, Formatter};
use std::hash::Hash;

type HExpr = HintExpression;

#[derive(Clone, Debug)]
pub enum HintAccess {
    ComponentAccess(String),
    ArrayAccess(HintExpression),
}

impl HintAccess {
    pub fn to_json(&self) -> String {
        use HintAccess::*;
        match self {
            ComponentAccess(name) => {
                format!("{{\"$\": \"Access\", \"@\": \"ComponentAccess\", \"name\": \"{}\"}}", name)
            }
            ArrayAccess(index) => {
                format!("{{\"$\": \"Access\", \"@\": \"ArrayAccess\", \"index\": {}}}", index.to_json())
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum HintExpression {
    Unknown,
    Number { value: BigInt },
    Symbol {
        symbol: String,
        access: Vec<HintAccess>,
    },
    Variable { symbol: String },
    InfixOp {
        lhe: Box<HintExpression>,
        rhe: Box<HintExpression>,
        infix_op: ExpressionInfixOpcode,
    },
    PrefixOp {
        rhe: Box<HintExpression>,
        prefix_op: ExpressionPrefixOpcode,
    },
    MemorySlice {
        route: Vec<usize>,
        values: Vec<HintExpression>,
    },
    InlineSwitch {
        condition: Box<HintExpression>,
        if_true: Box<HintExpression>,
        if_false: Box<HintExpression>,
    },
}

impl HintExpression {
    pub fn to_json(&self) -> String {
        use HintExpression::*;
        match self {
            Unknown => {
                format!("{{\"$\": \"Expr\", \"@\": \"Unknown\"}}")
            }
            Number { value } => {
                format!("{{\"$\": \"Expr\", \"@\": \"Number\", \"value\": \"{}\"}}", value.to_str_radix(10))
            }
            Symbol { symbol, access } => {
                format!("{{\"$\": \"Expr\", \"@\": \"Symbol\", \"symbol\": \"{}\", \"access\": [{}]}}", symbol,
                    access.iter()
                    .map(|acc| acc.to_json())
                    .collect::<Vec<String>>()
                    .join(", ")
                )
            }
            Variable { symbol } => {
                format!("{{\"$\": \"Expr\", \"@\": \"Variable\", \"symbol\": \"{}\"}}", symbol)
            }
            InfixOp { lhe, rhe, infix_op } => {
                format!(
                    "{{\"$\": \"Expr\", \"@\": \"{:?}\", \"lhe\": {}, \"rhe\": {}}}",
                    infix_op,
                    lhe.to_json(),
                    rhe.to_json(),
                )
            }
            PrefixOp { rhe, prefix_op } => {
                format!(
                    "{{\"$\": \"Expr\", \"@\": \"{:?}\", \"rhe\": {}}}",
                    prefix_op,
                    rhe.to_json(),
                )
            }
            MemorySlice { route, values } => {
                let route_json = route.iter()
                    .map(|dim| dim.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");
                let values_json = values.iter()
                    .map(|val| val.to_json())
                    .collect::<Vec<String>>()
                    .join(", ");
                format!(
                    "{{\"$\": \"Expr\", \"@\": \"MemorySlice\", \"route\": [{}], \"values\": [{}]}}",
                    route_json,
                    values_json
                )
            }
            InlineSwitch { condition, if_true, if_false } => {
                format!(
                    "{{\"$\": \"Expr\", \"@\": \"InlineSwitch\", \"condition\": {}, \"if_true\": {}, \"if_false\": {}}}",
                    condition.to_json(),
                    if_true.to_json(),
                    if_false.to_json(),
                )
            }
        }
    }
}

pub enum ArithmeticExpression<C>
where
    C: Hash + Eq,
{
    Number {
        value: BigInt,
        expr: HintExpression,
    },
    Signal {
        symbol: C,
        expr: HintExpression,
    },
    Linear {
        // Represents the expression: c1*s1 + .. + cn*sn + C
        // where c1..cn are integers modulo a prime and
        // s1..sn are signals. C is a constant value
        coefficients: HashMap<C, BigInt>,
        expr: HintExpression,
    },
    Quadratic {
        // Is a quadratic expression of the form:
        //              a*b + c
        // Where a,b and c are linear expression
        a: HashMap<C, BigInt>,
        b: HashMap<C, BigInt>,
        c: HashMap<C, BigInt>,
        expr: HintExpression,
    },
    NonQuadratic { expr: HintExpression }, // Represents an expression that is not quadratic
}

impl<C: Default + Clone + Display + Hash + Eq + Ord> ArithmeticExpression<C> {
    fn to_constrain_json(&self) -> String {
        fn hashmap_to_json_entries<C: Default + Clone + Display + Hash + Eq + Ord>(
            coefficients: &HashMap<C, BigInt>,
        ) -> String {
            let coefficients = coefficients
                .iter()
                .filter(|(_, value)| !value.is_zero())
                .collect::<BTreeMap<_, _>>();
            let mut coeffs_string: String = "".to_string();
            for (symbol, coefficient) in coefficients {
                if !coeffs_string.is_empty() {
                    coeffs_string.push_str(",");
                }
                coeffs_string.push_str(
                    format!(
                        "\"{}\":\"{}\"",
                        symbol,
                        coefficient.to_str_radix(10)
                    )
                    .as_str(),
                );
            }
            coeffs_string
        }
        use ArithmeticExpression::*;
        match self {
            Number { value, .. } => {
                format!("{{\"$\": \"QuadExpr\", \"@\": \"Number\", \"value\": \"{}\"}}", value.to_str_radix(10))
            }
            Signal { symbol, .. } => {
                format!("{{\"$\": \"QuadExpr\", \"@\": \"Signal\", \"symbol\": \"{}\"}}", symbol)
            }
            Linear { coefficients , ..} => {
                format!(
                    "{{\"$\": \"QuadExpr\", \"@\": \"Linear\", \"coefficients\": {{{}}}}}",
                    hashmap_to_json_entries(coefficients)
                )
            }
            Quadratic { a, b, c , ..} => {
                format!(
                    "{{\"$\": \"QuadExpr\", \"@\": \"Quadratic\", \"a\": {{{}}}, \"b\": {{{}}}, \"c\": {{{}}}}}",
                    hashmap_to_json_entries(a),
                    hashmap_to_json_entries(b),
                    hashmap_to_json_entries(c)
                )
            }
            NonQuadratic { .. }  => unreachable!(),
        }
    }

    fn to_expr_json(&self) -> String {
        use ArithmeticExpression::*;
        match self {
            Number { expr, .. } => expr.to_json(),
            Signal { expr, .. } => expr.to_json(),
            Linear { expr, .. } => expr.to_json(),
            Quadratic { expr, .. } => expr.to_json(),
            NonQuadratic { expr } => expr.to_json(),
        }
    }

    pub fn to_json(&self, expr: bool) -> String {
        if expr {
            self.to_expr_json()
        } else {
            self.to_constrain_json()
        }
    }

    pub fn to_hint_expr(&self) -> HintExpression {
        use ArithmeticExpression::*;
        match self {
            Number { expr, .. } => expr.clone(),
            Signal { expr, .. } => expr.clone(),
            Linear { expr, .. } => expr.clone(),
            Quadratic { expr, .. } => expr.clone(),
            NonQuadratic { expr } => expr.clone(),
        }
    }
}

impl<C: Default + Clone + Display + Hash + Eq + Ord> Display for ArithmeticExpression<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use ArithmeticExpression::*;
        match self {
            Number { value, .. } => {
                write!(f, "{}", value.to_str_radix(10))
            }
            Signal { symbol, .. } => {
                write!(f, "{}", symbol)
            }
            Linear { coefficients , ..} => {
                let string_coefficients = ArithmeticExpression::string_from_coefficients(coefficients);
                write!(f, "{}", string_coefficients)
            }
            Quadratic { a, b, c , ..} => {
                let string_a = ArithmeticExpression::string_from_coefficients(a);
                let string_b = ArithmeticExpression::string_from_coefficients(b);
                let string_c = ArithmeticExpression::string_from_coefficients(c);
                write!(f, "({})*({}) + ({})", string_a, string_b, string_c)
            }
            NonQuadratic { .. }  => {
                write!(f, "NQ")
            }
        }
    }
}

impl<C: Default + Clone + Display + Hash + Eq + Ord> Clone for ArithmeticExpression<C> {
    fn clone(&self) -> Self {
        use ArithmeticExpression::*;
        match self {
            Number { value , expr} => Number { value: value.clone() , expr: expr.clone() },
            Signal { symbol , expr} => Signal { symbol: symbol.clone() , expr: expr.clone() },
            Linear { coefficients , expr} => Linear { coefficients: coefficients.clone() , expr: expr.clone() },
            Quadratic { a, b, c , expr} => Quadratic { a: a.clone(), b: b.clone(), c: c.clone() , expr: expr.clone() },
            NonQuadratic { expr } => NonQuadratic { expr: expr.clone() },
        }
    }
}

impl<C: Default + Clone + Display + Hash + Eq + Ord> Eq for ArithmeticExpression<C> {}
impl<C: Default + Clone + Display + Hash + Eq + Ord> PartialEq for ArithmeticExpression<C> {
    fn eq(&self, other: &Self) -> bool {
        use ArithmeticExpression::*;
        match (self, other) {
            (Number { value: v_0, .. }, Number { value: v_1, .. }) => *v_0 == *v_1,
            (Signal { symbol: s_0, .. }, Signal { symbol: s_1, .. }) => *s_0 == *s_1,
            (Linear { coefficients: c_0, .. }, Linear { coefficients: c_1, .. }) => *c_0 == *c_1,
            (Quadratic { a: a_0, b: b_0, c: c_0, ..  }, Quadratic { a: a_1, b: b_1, c: c_1, .. }) => {
                *a_0 == *a_1 && *b_0 == *b_1 && *c_0 == *c_1
            }
            _ => false,
        }
    }
}

impl<C: Default + Clone + Display + Hash + Eq + Ord> Default for ArithmeticExpression<C> {
    fn default() -> Self {
        ArithmeticExpression::NonQuadratic { expr: HExpr::Unknown }
    }
}

impl<C: Default + Clone + Display + Hash + Eq + Ord> ArithmeticExpression<C> {
    pub fn new() -> ArithmeticExpression<C> {
        ArithmeticExpression::default()
    }

    // printing utils
    fn string_from_coefficients(coefficients: &HashMap<C, BigInt>) -> String {
        use num_bigint_dig::Sign::*;

        let coefficients = coefficients
            .iter()
            .filter(|(_, value)| !value.is_zero())
            .collect::<BTreeMap<_, _>>();

        let mut string_coefficients = "".to_string();
        for (signal, value) in coefficients {
            let (sign, abs_component_string) = if signal.eq(&ArithmeticExpression::constant_coefficient()) {
                (value.sign(), value.to_str_radix(10).trim_start_matches('-').to_string())
            } else {
                if value == &BigInt::from(1) || value == &BigInt::from(-1) {
                    (value.sign(), signal.to_string())
                } else {
                    (value.sign(), format!("{}*{}", value.to_str_radix(10).trim_start_matches('-'), signal))
                }
            };
            if sign != NoSign {
                if !string_coefficients.is_empty() {
                    if sign == Minus {
                        string_coefficients.push_str(format!(" - {}", abs_component_string).as_str());
                    } else {
                        string_coefficients.push_str(format!(" + {}", abs_component_string).as_str());
                    }
                } else {
                    if sign == Minus {
                        string_coefficients.push_str(format!("-{}", abs_component_string).as_str());
                    } else {
                        string_coefficients.push_str(format!("{}", abs_component_string).as_str());
                    }
                }
            }
        }
        string_coefficients
    }

    // constraint generation utils
    // transforms constraints into a constraint, None if the expression was non-quadratic
    pub fn transform_expression_to_constraint_form(
        arithmetic_expression: ArithmeticExpression<C>,
        field: &BigInt,
    ) -> Option<Constraint<C>> {
        use ArithmeticExpression::*;
        let mut a = HashMap::new();
        let mut b = HashMap::new();
        let mut c = HashMap::new();
        ArithmeticExpression::initialize_hashmap_for_expression(&mut a);
        ArithmeticExpression::initialize_hashmap_for_expression(&mut b);
        ArithmeticExpression::initialize_hashmap_for_expression(&mut c);
        match arithmetic_expression {
            NonQuadratic { .. } => {
                return Option::None;
            }
            Quadratic { a: old_a, b: old_b, c: old_c, .. } => {
                a = old_a;
                b = old_b;
                c = old_c;
            }
            Number { value, .. } => {
                c.insert(ArithmeticExpression::constant_coefficient(), value);
            }
            Signal { symbol, .. } => {
                c.insert(symbol, BigInt::from(1));
            }
            Linear { coefficients, .. } => {
                c = coefficients;
            }
        }
        ArithmeticExpression::multiply_coefficients_by_constant(&BigInt::from(-1), &mut c, field);
        Option::Some(Constraint::new(a, b, c))
    }

    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    // All the operations must ensure that the Hashmaps used
    // to construct Expressions contain the empty string
    // as key.
    // Therefore the function 'initialize_hashmap_for_expression'
    // is meant to be call each time a hashmap is going to be
    // part of a Expression
    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    fn constant_coefficient() -> C {
        C::default()
    }
    fn initialize_hashmap_for_expression(initial: &mut HashMap<C, BigInt>) {
        initial
            .entry(ArithmeticExpression::constant_coefficient())
            .or_insert_with(|| BigInt::from(0));
        debug_assert!(ArithmeticExpression::valid_hashmap_for_expression(initial));
    }
    pub fn modularize_hashmap(
        coefficients: &HashMap<C, BigInt>,
        field: &BigInt,
    ) -> HashMap<C, BigInt> {
        let mut modularized = HashMap::new();
        for (symbol, coefficient) in coefficients {
            modularized.insert(
                symbol.clone(),
                (coefficient % field + field) % field,
            );
        }
        modularized
    }
    fn valid_hashmap_for_expression(h: &HashMap<C, BigInt>) -> bool {
        let cc = ArithmeticExpression::constant_coefficient();
        h.contains_key(&cc)
    }
    fn initialize_symbol_in_coefficients(symbol: &C, coefficients: &mut HashMap<C, BigInt>) {
        debug_assert!(ArithmeticExpression::valid_hashmap_for_expression(coefficients));
        if !coefficients.contains_key(symbol) {
            coefficients.insert(symbol.clone(), BigInt::from(0));
        }
        debug_assert!(ArithmeticExpression::valid_hashmap_for_expression(coefficients));
    }
    fn add_constant_to_coefficients(
        value: &BigInt,
        coefficients: &mut HashMap<C, BigInt>,
        field: &BigInt,
    ) {
        debug_assert!(ArithmeticExpression::valid_hashmap_for_expression(coefficients));
        let cc: C = ArithmeticExpression::constant_coefficient();
        coefficients.insert(
            cc.clone(),
            modular_arithmetic::add(coefficients.get(&cc).unwrap(), value, field),
        );
        debug_assert!(ArithmeticExpression::valid_hashmap_for_expression(coefficients));
    }
    fn add_symbol_to_coefficients(
        symbol: &C,
        coefficient: &BigInt,
        coefficients: &mut HashMap<C, BigInt>,
        field: &BigInt,
    ) {
        debug_assert!(ArithmeticExpression::valid_hashmap_for_expression(coefficients));
        ArithmeticExpression::initialize_symbol_in_coefficients(symbol, coefficients);
        coefficients.insert(
            symbol.clone(),
            modular_arithmetic::add(coefficients.get(symbol).unwrap(), coefficient, field),
        );
        debug_assert!(ArithmeticExpression::valid_hashmap_for_expression(coefficients));
    }
    fn add_coefficients_to_coefficients(
        coefficients_0: &HashMap<C, BigInt>,
        coefficients_1: &mut HashMap<C, BigInt>,
        field: &BigInt,
    ) {
        debug_assert!(ArithmeticExpression::valid_hashmap_for_expression(coefficients_0));
        debug_assert!(ArithmeticExpression::valid_hashmap_for_expression(coefficients_1));
        for (symbol, coefficient) in coefficients_0 {
            ArithmeticExpression::add_symbol_to_coefficients(
                symbol,
                coefficient,
                coefficients_1,
                field,
            );
        }
        debug_assert!(ArithmeticExpression::valid_hashmap_for_expression(coefficients_0));
        debug_assert!(ArithmeticExpression::valid_hashmap_for_expression(coefficients_1));
    }
    fn multiply_coefficients_by_constant(
        constant: &BigInt,
        coefficients: &mut HashMap<C, BigInt>,
        field: &BigInt,
    ) {
        debug_assert!(ArithmeticExpression::valid_hashmap_for_expression(coefficients));
        for value in coefficients.values_mut() {
            *value = modular_arithmetic::mul(value, constant, field);
        }
        debug_assert!(ArithmeticExpression::valid_hashmap_for_expression(coefficients));
    }
    fn divide_coefficients_by_constant(
        constant: &BigInt,
        coefficients: &mut HashMap<C, BigInt>,
        field: &BigInt,
    ) -> Result<(), ArithmeticError> {
        debug_assert!(ArithmeticExpression::valid_hashmap_for_expression(coefficients));
        let inverse_constant = modular_arithmetic::div(
            &BigInt::from(1),
            constant,
            &field
        )?;
        ArithmeticExpression::multiply_coefficients_by_constant(&inverse_constant, coefficients, field);
        debug_assert!(ArithmeticExpression::valid_hashmap_for_expression(coefficients));
        Result::Ok(())
    }

    fn nonquadratic_prefix(
        rhe: &ArithmeticExpression<C>,
        prefix_op: &ExpressionPrefixOpcode,
    ) -> ArithmeticExpression<C> {
        use ArithmeticExpression::*;
        match rhe {
            NonQuadratic { expr: rhe_expr } | Quadratic { expr: rhe_expr, .. } | Linear { expr: rhe_expr, .. } | Signal { expr: rhe_expr, .. } | Number { expr: rhe_expr, .. } => {
                NonQuadratic { expr: HExpr::PrefixOp {
                    rhe: Box::new(rhe_expr.clone()),
                    prefix_op: prefix_op.clone(),
                } }
            }
        }
    }

    fn nonquadratic_infix(
        left: &ArithmeticExpression<C>,
        right: &ArithmeticExpression<C>,
        infix_op: &ExpressionInfixOpcode,
    ) -> ArithmeticExpression<C> {
        use ArithmeticExpression::*;
        match (left, right) {
            (NonQuadratic { expr: lhe }, NonQuadratic { expr: rhe })
            | (NonQuadratic { expr: lhe }, Quadratic { expr: rhe, .. })
            | (NonQuadratic { expr: lhe }, Linear { expr: rhe, .. })
            | (NonQuadratic { expr: lhe }, Signal { expr: rhe, .. })
            | (NonQuadratic { expr: lhe }, Number { expr: rhe, .. })
            | (Quadratic { expr: lhe, .. }, NonQuadratic { expr: rhe })
            | (Quadratic { expr: lhe, .. }, Quadratic { expr: rhe, .. })
            | (Quadratic { expr: lhe, .. }, Linear { expr: rhe, .. })
            | (Quadratic { expr: lhe, .. }, Signal { expr: rhe, .. })
            | (Quadratic { expr: lhe, .. }, Number { expr: rhe, .. })
            | (Linear { expr: lhe, .. }, NonQuadratic { expr: rhe })
            | (Linear { expr: lhe, .. }, Quadratic { expr: rhe, .. })
            | (Linear { expr: lhe, .. }, Linear { expr: rhe, .. })
            | (Linear { expr: lhe, .. }, Signal { expr: rhe, .. })
            | (Linear { expr: lhe, .. }, Number { expr: rhe, .. })
            | (Signal { expr: lhe, .. }, NonQuadratic { expr: rhe })
            | (Signal { expr: lhe, .. }, Quadratic { expr: rhe, .. })
            | (Signal { expr: lhe, .. }, Linear { expr: rhe, .. })
            | (Signal { expr: lhe, .. }, Signal { expr: rhe, .. })
            | (Signal { expr: lhe, .. }, Number { expr: rhe, .. })
            | (Number { expr: lhe, .. }, NonQuadratic { expr: rhe })
            | (Number { expr: lhe, .. }, Quadratic { expr: rhe, .. })
            | (Number { expr: lhe, .. }, Linear { expr: rhe, .. })
            | (Number { expr: lhe, .. }, Signal { expr: rhe, .. })
            | (Number { expr: lhe, .. }, Number { expr: rhe, .. }) => {
                NonQuadratic { expr: HExpr::InfixOp {
                    lhe: Box::new(lhe.clone()),
                    rhe: Box::new(rhe.clone()),
                    infix_op: infix_op.clone(),
                } }
            }
        }
    }

    pub fn add(
        left: &ArithmeticExpression<C>,
        right: &ArithmeticExpression<C>,
        field: &BigInt,
    ) -> ArithmeticExpression<C> {
        use ArithmeticExpression::*;
        match (left, right) {
            (NonQuadratic { .. }, _) | (_, NonQuadratic { .. }) | (Quadratic { .. }, Quadratic { .. }) => {
                ArithmeticExpression::nonquadratic_infix(left, right, &ExpressionInfixOpcode::Add)
            }
            (Number { value: v_0, expr: lhe }, Number { value: v_1, expr: rhe }) => {
                Number { expr: HExpr::InfixOp {
                    lhe: Box::new(lhe.clone()),
                    rhe: Box::new(rhe.clone()),
                    infix_op: ExpressionInfixOpcode::Add,
                }, value: modular_arithmetic::add(v_0, v_1, field) }
            }
            (Number { value, expr: lhe }, Signal { symbol, expr: rhe }) | (Signal { symbol, expr: lhe }, Number { value, expr: rhe }) => {
                let mut coefficients = HashMap::new();
                ArithmeticExpression::initialize_hashmap_for_expression(&mut coefficients);
                ArithmeticExpression::add_constant_to_coefficients(value, &mut coefficients, field);
                ArithmeticExpression::add_symbol_to_coefficients(
                    symbol,
                    &BigInt::from(1),
                    &mut coefficients,
                    field,
                );
                Linear { expr: HExpr::InfixOp {
                    lhe: Box::new(lhe.clone()),
                    rhe: Box::new(rhe.clone()),
                    infix_op: ExpressionInfixOpcode::Add,
                }, coefficients }
            }
            (Number { value, expr: lhe }, Linear { coefficients, expr: rhe }) | (Linear { coefficients, expr: lhe }, Number { value, expr: rhe }) => {
                let mut n_coefficients = coefficients.clone();
                ArithmeticExpression::add_constant_to_coefficients(
                    value,
                    &mut n_coefficients,
                    field,
                );
                Linear { expr: HExpr::InfixOp {
                    lhe: Box::new(lhe.clone()),
                    rhe: Box::new(rhe.clone()),
                    infix_op: ExpressionInfixOpcode::Add,
                }, coefficients: n_coefficients }
            }
            (Number { value, expr: lhe }, Quadratic { a, b, c, expr: rhe }) | (Quadratic { a, b, c, expr: lhe }, Number { value, expr: rhe }) => {
                let mut n_c = c.clone();
                ArithmeticExpression::add_constant_to_coefficients(value, &mut n_c, field);
                Quadratic { expr: HExpr::InfixOp {
                    lhe: Box::new(lhe.clone()),
                    rhe: Box::new(rhe.clone()),
                    infix_op: ExpressionInfixOpcode::Add,
                }, a: a.clone(), b: b.clone(), c: n_c }
            }
            (Signal { symbol, expr: lhe }, Signal { symbol: symbol_1, expr: rhe }) => {
                let mut coefficients = HashMap::new();
                ArithmeticExpression::initialize_hashmap_for_expression(&mut coefficients);
                ArithmeticExpression::add_symbol_to_coefficients(
                    symbol,
                    &BigInt::from(1),
                    &mut coefficients,
                    field,
                );
                ArithmeticExpression::add_symbol_to_coefficients(
                    symbol_1,
                    &BigInt::from(1),
                    &mut coefficients,
                    field,
                );
                Linear { expr: HExpr::InfixOp {
                    lhe: Box::new(lhe.clone()),
                    rhe: Box::new(rhe.clone()),
                    infix_op: ExpressionInfixOpcode::Add,
                }, coefficients }
            }
            (Signal { symbol, expr: lhe }, Linear { coefficients, expr: rhe }) | (Linear { coefficients, expr: lhe }, Signal { symbol, expr: rhe }) => {
                let mut n_coefficients = coefficients.clone();
                ArithmeticExpression::add_symbol_to_coefficients(
                    symbol,
                    &BigInt::from(1),
                    &mut n_coefficients,
                    field,
                );
                Linear { expr: HExpr::InfixOp {
                    lhe: Box::new(lhe.clone()),
                    rhe: Box::new(rhe.clone()),
                    infix_op: ExpressionInfixOpcode::Add,
                }, coefficients: n_coefficients }
            }
            (Signal { symbol, expr: lhe }, Quadratic { a, b, c, expr: rhe }) | (Quadratic { a, b, c, expr: lhe }, Signal { symbol, expr: rhe }) => {
                let mut coefficients = c.clone();
                ArithmeticExpression::add_symbol_to_coefficients(
                    symbol,
                    &BigInt::from(1),
                    &mut coefficients,
                    field,
                );
                Quadratic { expr: HExpr::InfixOp {
                    lhe: Box::new(lhe.clone()),
                    rhe: Box::new(rhe.clone()),
                    infix_op: ExpressionInfixOpcode::Add,
                }, a: a.clone(), b: b.clone(), c: coefficients }
            }
            (Linear { coefficients, expr: lhe }, Linear { coefficients: coefficients_1 , expr: rhe}) => {
                let mut n_coefficients = coefficients_1.clone();
                ArithmeticExpression::add_coefficients_to_coefficients(
                    coefficients,
                    &mut n_coefficients,
                    field,
                );
                Linear { expr: HExpr::InfixOp {
                    lhe: Box::new(lhe.clone()),
                    rhe: Box::new(rhe.clone()),
                    infix_op: ExpressionInfixOpcode::Add,
                }, coefficients: n_coefficients }
            }
            (Linear { coefficients, expr: lhe }, Quadratic { a, b, c, expr: rhe }) | (Quadratic { a, b, c, expr: lhe }, Linear { coefficients, expr: rhe }) => {
                let mut coefficients_1 = c.clone();
                ArithmeticExpression::add_coefficients_to_coefficients(
                    coefficients,
                    &mut coefficients_1,
                    field,
                );
                Quadratic { expr: HExpr::InfixOp {
                    lhe: Box::new(lhe.clone()),
                    rhe: Box::new(rhe.clone()),
                    infix_op: ExpressionInfixOpcode::Add,
                }, a: a.clone(), b: b.clone(), c: coefficients_1 }
            }
        }
    }

    pub fn mul(
        left: &ArithmeticExpression<C>,
        right: &ArithmeticExpression<C>,
        field: &BigInt,
    ) -> ArithmeticExpression<C> {
        use ArithmeticExpression::*;
        match (left, right) {
            (NonQuadratic { .. }, _)
            | (_, NonQuadratic { .. })
            | (Quadratic { .. }, Quadratic { .. })
            | (Quadratic { .. }, Linear { .. })
            | (Linear { .. }, Quadratic { .. })
            | (Quadratic { .. }, Signal { .. })
            | (Signal { .. }, Quadratic { .. }) => {
                ArithmeticExpression::nonquadratic_infix(left, right, &ExpressionInfixOpcode::Mul)
            }
            (Number { value: value_0, expr: lhe }, Number { value: value_1, expr: rhe }) => {
                Number { expr: HExpr::InfixOp {
                    lhe: Box::new(lhe.clone()),
                    rhe: Box::new(rhe.clone()),
                    infix_op: ExpressionInfixOpcode::Mul,
                }, value: modular_arithmetic::mul(value_0, value_1, field) }
            }
            (Number { value, expr: lhe }, Signal { symbol, expr: rhe }) | (Signal { symbol, expr: lhe }, Number { value, expr: rhe }) => {
                let mut coefficients = HashMap::new();
                ArithmeticExpression::initialize_hashmap_for_expression(&mut coefficients);
                ArithmeticExpression::add_symbol_to_coefficients(
                    symbol,
                    value,
                    &mut coefficients,
                    field,
                );
                Linear { expr: HExpr::InfixOp {
                    lhe: Box::new(lhe.clone()),
                    rhe: Box::new(rhe.clone()),
                    infix_op: ExpressionInfixOpcode::Mul,
                }, coefficients }
            }
            (Number { value, expr: lhe }, Linear { coefficients, expr: rhe }) | (Linear { coefficients, expr: lhe }, Number { value, expr: rhe }) => {
                let mut n_coefficients = coefficients.clone();
                ArithmeticExpression::multiply_coefficients_by_constant(
                    value,
                    &mut n_coefficients,
                    field,
                );
                Linear { expr: HExpr::InfixOp {
                    lhe: Box::new(lhe.clone()),
                    rhe: Box::new(rhe.clone()),
                    infix_op: ExpressionInfixOpcode::Mul,
                }, coefficients: n_coefficients }
            }
            (Number { value, expr: lhe }, Quadratic { a, b, c, expr: rhe }) | (Quadratic { a, b, c, expr: lhe }, Number { value, expr: rhe }) => {
                let mut n_a = a.clone();
                let n_b = b.clone();
                let mut n_c = c.clone();
                ArithmeticExpression::multiply_coefficients_by_constant(value, &mut n_a, field);
                ArithmeticExpression::multiply_coefficients_by_constant(value, &mut n_c, field);
                Quadratic { expr: HExpr::InfixOp {
                    lhe: Box::new(lhe.clone()),
                    rhe: Box::new(rhe.clone()),
                    infix_op: ExpressionInfixOpcode::Mul,
                }, a: n_a, b: n_b, c: n_c }
            }
            (Signal { symbol: symbol_0, expr: lhe }, Signal { symbol: symbol_1, expr: rhe }) => {
                let mut a = HashMap::new();
                let mut b = HashMap::new();
                let mut c = HashMap::new();
                ArithmeticExpression::initialize_hashmap_for_expression(&mut a);
                ArithmeticExpression::initialize_hashmap_for_expression(&mut b);
                ArithmeticExpression::initialize_hashmap_for_expression(&mut c);
                ArithmeticExpression::add_symbol_to_coefficients(
                    symbol_0,
                    &BigInt::from(1),
                    &mut a,
                    field,
                );
                ArithmeticExpression::add_symbol_to_coefficients(
                    symbol_1,
                    &BigInt::from(1),
                    &mut b,
                    field,
                );
                Quadratic { expr: HExpr::InfixOp {
                    lhe: Box::new(lhe.clone()),
                    rhe: Box::new(rhe.clone()),
                    infix_op: ExpressionInfixOpcode::Mul,
                }, a, b, c }
            }
            (Signal { symbol, expr: lhe }, Linear { coefficients, expr: rhe }) | (Linear { coefficients, expr: lhe }, Signal { symbol, expr: rhe }) => {
                let a = coefficients.clone();
                let mut b = HashMap::new();
                let mut c = HashMap::new();
                ArithmeticExpression::initialize_hashmap_for_expression(&mut b);
                ArithmeticExpression::initialize_hashmap_for_expression(&mut c);
                ArithmeticExpression::add_symbol_to_coefficients(
                    symbol,
                    &BigInt::from(1),
                    &mut b,
                    field,
                );
                Quadratic { expr: HExpr::InfixOp {
                    lhe: Box::new(lhe.clone()),
                    rhe: Box::new(rhe.clone()),
                    infix_op: ExpressionInfixOpcode::Mul,
                }, a, b, c }
            }
            (Linear { coefficients: coefficients_0 , expr: lhe}, Linear { coefficients: coefficients_1 , expr: rhe}) => {
                let a = coefficients_0.clone();
                let b = coefficients_1.clone();
                let mut c = HashMap::new();
                ArithmeticExpression::initialize_hashmap_for_expression(&mut c);
                Quadratic { expr: HExpr::InfixOp {
                    lhe: Box::new(lhe.clone()),
                    rhe: Box::new(rhe.clone()),
                    infix_op: ExpressionInfixOpcode::Mul,
                }, a, b, c }
            }
        }
    }
    pub fn sub(
        left: &ArithmeticExpression<C>,
        right: &ArithmeticExpression<C>,
        field: &BigInt,
    ) -> ArithmeticExpression<C> {
        use ArithmeticExpression::*;
        let minus_one = Number { value: BigInt::from(-1), expr: HExpr::Number { value: BigInt::from(-1) } };
        let step_one = ArithmeticExpression::mul(&minus_one, right, field);
        ArithmeticExpression::add(left, &step_one, field)
    }

    pub fn div(
        left: &ArithmeticExpression<C>,
        right: &ArithmeticExpression<C>,
        field: &BigInt,
    ) -> Result<ArithmeticExpression<C>, ArithmeticError> {
        use ArithmeticExpression::*;
        match (left, right) {
            (Number { value: value_0 , expr: lhe}, Number { value: value_1 , expr: rhe}) => {
                let value = modular_arithmetic::div(value_0, value_1, field)?;
                Result::Ok(Number { expr: HExpr::InfixOp {
                    lhe: Box::new(lhe.clone()),
                    rhe: Box::new(rhe.clone()),
                    infix_op: ExpressionInfixOpcode::Div,
                }, value })
            }
            (Signal { symbol , expr: lhe}, Number { value , expr: rhe}) => {
                let mut coefficients = HashMap::new();
                ArithmeticExpression::initialize_hashmap_for_expression(&mut coefficients);
                ArithmeticExpression::add_symbol_to_coefficients(
                    symbol,
                    &BigInt::from(1),
                    &mut coefficients,
                    field,
                );
                ArithmeticExpression::divide_coefficients_by_constant(
                    value,
                    &mut coefficients,
                    field,
                )?;
                Result::Ok(Linear { expr: HExpr::InfixOp {
                    lhe: Box::new(lhe.clone()),
                    rhe: Box::new(rhe.clone()),
                    infix_op: ExpressionInfixOpcode::Div,
                }, coefficients })
            }
            (Linear { coefficients , expr: lhe}, Number { value , expr: rhe}) => {
                let mut coefficients = coefficients.clone();
                ArithmeticExpression::divide_coefficients_by_constant(
                    value,
                    &mut coefficients,
                    field,
                )?;
                Result::Ok(Linear { expr: HExpr::InfixOp {
                    lhe: Box::new(lhe.clone()),
                    rhe: Box::new(rhe.clone()),
                    infix_op: ExpressionInfixOpcode::Div,
                }, coefficients })
            }
            (Quadratic { a, b, c , expr: lhe}, Number { value , expr: rhe}) => {
                let mut a = a.clone();
                let b = b.clone();
                let mut c = c.clone();
                ArithmeticExpression::divide_coefficients_by_constant(value, &mut a, field)?;
                ArithmeticExpression::divide_coefficients_by_constant(value, &mut c, field)?;
                Result::Ok(Quadratic { expr: HExpr::InfixOp {
                    lhe: Box::new(lhe.clone()),
                    rhe: Box::new(rhe.clone()),
                    infix_op: ExpressionInfixOpcode::Div,
                }, a, b, c })
            }
            _ => {
                Result::Ok(ArithmeticExpression::nonquadratic_infix(left, right, &ExpressionInfixOpcode::Div))
            }
        }
    }
    pub fn idiv(
        left: &ArithmeticExpression<C>,
        right: &ArithmeticExpression<C>,
        field: &BigInt,
    ) -> Result<ArithmeticExpression<C>, ArithmeticError> {
        use ArithmeticExpression::*;
        match (left, right) {
            (Number { value: value_0 , expr: lhe}, Number { value: value_1 , expr: rhe}) => {
                let value = modular_arithmetic::idiv(value_0, value_1, field)?;
                Result::Ok(Number { expr: HExpr::InfixOp {
                    lhe: Box::new(lhe.clone()),
                    rhe: Box::new(rhe.clone()),
                    infix_op: ExpressionInfixOpcode::IntDiv,
                }, value })
            }
            _ => {
                Result::Ok(ArithmeticExpression::nonquadratic_infix(left, right, &ExpressionInfixOpcode::IntDiv))
            }
        }
    }
    pub fn mod_op(
        left: &ArithmeticExpression<C>,
        right: &ArithmeticExpression<C>,
        field: &BigInt,
    ) -> Result<ArithmeticExpression<C>, ArithmeticError> {
        use ArithmeticExpression::*;
        if let (Number { value: value_0, expr: lhe }, Number { value: value_1, expr: rhe }) = (left, right) {
            let value = modular_arithmetic::mod_op(value_0, value_1, field)?;
            Result::Ok(Number { expr: HExpr::InfixOp {
                lhe: Box::new(lhe.clone()),
                rhe: Box::new(rhe.clone()),
                infix_op: ExpressionInfixOpcode::Mod,
            }, value })
        } else {
            Result::Ok(ArithmeticExpression::nonquadratic_infix(left, right, &ExpressionInfixOpcode::Mod))
        }
    }
    pub fn pow(
        left: &ArithmeticExpression<C>,
        right: &ArithmeticExpression<C>,
        field: &BigInt,
    ) -> ArithmeticExpression<C> {
        use ArithmeticExpression::*;
        match (left, right) {
            (Number { value: value_0 , expr: lhe}, Number { value: value_1 , expr: rhe}) => {
                Number { expr: HExpr::InfixOp {
                    lhe: Box::new(lhe.clone()),
                    rhe: Box::new(rhe.clone()),
                    infix_op: ExpressionInfixOpcode::Pow,
                }, value: modular_arithmetic::pow(value_0, value_1, field) }
            }
            (Signal { symbol , expr: lhe }, Number { value , .. }) if *value == BigInt::from(2) => {
                let left = Signal { symbol: symbol.clone(), expr: lhe.clone() };
                let right = Signal { symbol: symbol.clone(), expr: lhe.clone() };
                ArithmeticExpression::mul(&left, &right, field)
            }
            (Linear { coefficients , expr: lhe}, Number { value , .. }) if *value == BigInt::from(2) => {
                let left = Linear { coefficients: coefficients.clone(), expr: lhe.clone() };
                let right = Linear { coefficients: coefficients.clone(), expr: lhe.clone() };
                ArithmeticExpression::mul(&left, &right, field)
            }
            _ => {
                ArithmeticExpression::nonquadratic_infix(left, right, &ExpressionInfixOpcode::Pow)
            }
        }
    }
    pub fn prefix_sub(elem: &ArithmeticExpression<C>, field: &BigInt) -> ArithmeticExpression<C> {
        use ArithmeticExpression::*;
        let minus_one = Number { value: BigInt::from(-1), expr: HExpr::Number { value: BigInt::from(-1) } };
        ArithmeticExpression::mul(elem, &minus_one, field)
    }

    // Bit operations
    pub fn complement(
        elem: &ArithmeticExpression<C>,
        field: &BigInt,
    ) -> ArithmeticExpression<C> {
        use ArithmeticExpression::*;
        if let Number { value, expr: rhe } = elem {
            Number { expr: HExpr::PrefixOp {
                rhe: Box::new(rhe.clone()),
                prefix_op: ExpressionPrefixOpcode::Complement,
            }, value: modular_arithmetic::complement(value, field) }
        } else {
            ArithmeticExpression::nonquadratic_prefix(elem, &ExpressionPrefixOpcode::Complement)
        }
    }

    pub fn shift_l(
        left: &ArithmeticExpression<C>,
        right: &ArithmeticExpression<C>,
        field: &BigInt,
    ) -> Result<ArithmeticExpression<C>, ArithmeticError> {
        use ArithmeticExpression::*;
        if let (Number { value: value_0 , expr: lhe}, Number { value: value_1 , expr: rhe}) = (left, right) {
            let shifted_elem = modular_arithmetic::shift_l(value_0, value_1, field)?;
            Result::Ok(Number { expr: HExpr::InfixOp {
                lhe: Box::new(lhe.clone()),
                rhe: Box::new(rhe.clone()),
                infix_op: ExpressionInfixOpcode::ShiftL,
            }, value: shifted_elem })
        } else {
            Result::Ok(ArithmeticExpression::nonquadratic_infix(left, right, &ExpressionInfixOpcode::ShiftL))
        }
    }
    pub fn shift_r(
        left: &ArithmeticExpression<C>,
        right: &ArithmeticExpression<C>,
        field: &BigInt,
    ) -> Result<ArithmeticExpression<C>, ArithmeticError> {
        use ArithmeticExpression::*;
        if let (Number { value: value_0 , expr: lhe}, Number { value: value_1 , expr: rhe}) = (left, right) {
            let shifted_elem = modular_arithmetic::shift_r(value_0, value_1, field)?;
            Result::Ok(Number { expr: HExpr::InfixOp {
                lhe: Box::new(lhe.clone()),
                rhe: Box::new(rhe.clone()),
                infix_op: ExpressionInfixOpcode::ShiftR,
            }, value: shifted_elem })
        } else {
            Result::Ok(ArithmeticExpression::nonquadratic_infix(left, right, &ExpressionInfixOpcode::ShiftR))
        }
    }
    pub fn bit_or(
        left: &ArithmeticExpression<C>,
        right: &ArithmeticExpression<C>,
        field: &BigInt,
    ) -> ArithmeticExpression<C> {
        use ArithmeticExpression::*;
        if let (Number { value: value_0 , expr: lhe}, Number { value: value_1 , expr: rhe}) = (left, right) {
            Number { expr: HExpr::InfixOp {
                lhe: Box::new(lhe.clone()),
                rhe: Box::new(rhe.clone()),
                infix_op: ExpressionInfixOpcode::BitOr,
            }, value: modular_arithmetic::bit_or(value_0, value_1, field) }
        } else {
            ArithmeticExpression::nonquadratic_infix(left, right, &ExpressionInfixOpcode::BitOr)
        }
    }
    pub fn bit_and(
        left: &ArithmeticExpression<C>,
        right: &ArithmeticExpression<C>,
        field: &BigInt,
    ) -> ArithmeticExpression<C> {
        use ArithmeticExpression::*;
        if let (Number { value: value_0 , expr: lhe}, Number { value: value_1 , expr: rhe}) = (left, right) {
            Number { expr: HExpr::InfixOp {
                lhe: Box::new(lhe.clone()),
                rhe: Box::new(rhe.clone()),
                infix_op: ExpressionInfixOpcode::BitAnd,
            }, value: modular_arithmetic::bit_and(value_0, value_1, field) }
        } else {
            ArithmeticExpression::nonquadratic_infix(left, right, &ExpressionInfixOpcode::BitAnd)
        }
    }
    pub fn bit_xor(
        left: &ArithmeticExpression<C>,
        right: &ArithmeticExpression<C>,
        field: &BigInt,
    ) -> ArithmeticExpression<C> {
        use ArithmeticExpression::*;
        if let (Number { value: value_0 , expr: lhe}, Number { value: value_1 , expr: rhe}) = (left, right) {
            Number { expr: HExpr::InfixOp {
                lhe: Box::new(lhe.clone()),
                rhe: Box::new(rhe.clone()),
                infix_op: ExpressionInfixOpcode::BitXor,
            }, value: modular_arithmetic::bit_xor(value_0, value_1, field) }
        } else {
            ArithmeticExpression::nonquadratic_infix(left, right, &ExpressionInfixOpcode::BitXor)
        }
    }

    // Boolean operations
    pub fn get_boolean_equivalence(elem: &ArithmeticExpression<C>, field: &BigInt) -> Option<bool> {
        use ArithmeticExpression::*;
        if let Number { value , .. } = elem {
            Option::Some(modular_arithmetic::as_bool(value, field))
        } else {
            Option::None
        }
    }
    pub fn not(elem: &ArithmeticExpression<C>, field: &BigInt) -> ArithmeticExpression<C> {
        use ArithmeticExpression::*;
        if let Number { value, expr } = elem {
            let value = modular_arithmetic::not(value, field);
            Number { expr: HExpr::PrefixOp {
                rhe: Box::new(expr.clone()),
                prefix_op: ExpressionPrefixOpcode::BoolNot,
            }, value }
        } else {
            ArithmeticExpression::nonquadratic_prefix(elem, &ExpressionPrefixOpcode::BoolNot)
        }
    }
    pub fn bool_or(
        left: &ArithmeticExpression<C>,
        right: &ArithmeticExpression<C>,
        field: &BigInt,
    ) -> ArithmeticExpression<C> {
        use ArithmeticExpression::*;
        if let (Number { value: value_0 , expr: lhe}, Number { value: value_1 , expr: rhe}) = (left, right) {
            Number { expr: HExpr::InfixOp {
                lhe: Box::new(lhe.clone()),
                rhe: Box::new(rhe.clone()),
                infix_op: ExpressionInfixOpcode::BoolOr,
            }, value: modular_arithmetic::bool_or(value_0, value_1, field) }
        } else {
            ArithmeticExpression::nonquadratic_infix(left, right, &ExpressionInfixOpcode::BoolOr)
        }
    }
    pub fn bool_and(
        left: &ArithmeticExpression<C>,
        right: &ArithmeticExpression<C>,
        field: &BigInt,
    ) -> ArithmeticExpression<C> {
        use ArithmeticExpression::*;
        if let (Number { value: value_0 , expr: lhe}, Number { value: value_1 , expr: rhe}) = (left, right) {
            Number { expr: HExpr::InfixOp {
                lhe: Box::new(lhe.clone()),
                rhe: Box::new(rhe.clone()),
                infix_op: ExpressionInfixOpcode::BoolAnd,
            }, value: modular_arithmetic::bool_and(value_0, value_1, field) }
        } else {
            ArithmeticExpression::nonquadratic_infix(left, right, &ExpressionInfixOpcode::BoolAnd)
        }
    }
    pub fn eq(
        left: &ArithmeticExpression<C>,
        right: &ArithmeticExpression<C>,
        field: &BigInt,
    ) -> ArithmeticExpression<C> {
        use ArithmeticExpression::*;
        if let (Number { value: value_0 , expr: lhe}, Number { value: value_1 , expr: rhe}) = (left, right) {
            Number { expr: HExpr::InfixOp {
                lhe: Box::new(lhe.clone()),
                rhe: Box::new(rhe.clone()),
                infix_op: ExpressionInfixOpcode::Eq,
            }, value: modular_arithmetic::eq(value_0, value_1, field) }
        } else {
            ArithmeticExpression::nonquadratic_infix(left, right, &ExpressionInfixOpcode::Eq)
        }
    }
    pub fn not_eq(
        left: &ArithmeticExpression<C>,
        right: &ArithmeticExpression<C>,
        field: &BigInt,
    ) -> ArithmeticExpression<C> {
        use ArithmeticExpression::*;
        if let (Number { value: value_0 , expr: lhe}, Number { value: value_1 , expr: rhe}) = (left, right) {
            Number { expr: HExpr::InfixOp {
                lhe: Box::new(lhe.clone()),
                rhe: Box::new(rhe.clone()),
                infix_op: ExpressionInfixOpcode::NotEq,
            }, value: modular_arithmetic::not_eq(value_0, value_1, field) }
        } else {
            ArithmeticExpression::nonquadratic_infix(left, right, &ExpressionInfixOpcode::NotEq)
        }
    }
    pub fn lesser(
        left: &ArithmeticExpression<C>,
        right: &ArithmeticExpression<C>,
        field: &BigInt,
    ) -> ArithmeticExpression<C> {
        use ArithmeticExpression::*;
        if let (Number { value: value_0 , expr: lhe}, Number { value: value_1 , expr: rhe}) = (left, right) {
            Number { expr: HExpr::InfixOp {
                lhe: Box::new(lhe.clone()),
                rhe: Box::new(rhe.clone()),
                infix_op: ExpressionInfixOpcode::Lesser,
            }, value: modular_arithmetic::lesser(value_0, value_1, field) }
        } else {
            ArithmeticExpression::nonquadratic_infix(left, right, &ExpressionInfixOpcode::Lesser)
        }
    }
    pub fn lesser_eq(
        left: &ArithmeticExpression<C>,
        right: &ArithmeticExpression<C>,
        field: &BigInt,
    ) -> ArithmeticExpression<C> {
        use ArithmeticExpression::*;
        if let (Number { value: value_0 , expr: lhe}, Number { value: value_1 , expr: rhe}) = (left, right) {
            Number { expr: HExpr::InfixOp {
                lhe: Box::new(lhe.clone()),
                rhe: Box::new(rhe.clone()),
                infix_op: ExpressionInfixOpcode::LesserEq,
            }, value: modular_arithmetic::lesser_eq(value_0, value_1, field) }
        } else {
            ArithmeticExpression::nonquadratic_infix(left, right, &ExpressionInfixOpcode::LesserEq)
        }
    }
    pub fn greater(
        left: &ArithmeticExpression<C>,
        right: &ArithmeticExpression<C>,
        field: &BigInt,
    ) -> ArithmeticExpression<C> {
        use ArithmeticExpression::*;
        if let (Number { value: value_0 , expr: lhe}, Number { value: value_1 , expr: rhe}) = (left, right) {
            Number { expr: HExpr::InfixOp {
                lhe: Box::new(lhe.clone()),
                rhe: Box::new(rhe.clone()),
                infix_op: ExpressionInfixOpcode::Greater,
            }, value: modular_arithmetic::greater(value_0, value_1, field) }
        } else {
            ArithmeticExpression::nonquadratic_infix(left, right, &ExpressionInfixOpcode::Greater)
        }
    }
    pub fn greater_eq(
        left: &ArithmeticExpression<C>,
        right: &ArithmeticExpression<C>,
        field: &BigInt,
    ) -> ArithmeticExpression<C> {
        use ArithmeticExpression::*;
        if let (Number { value: value_0 , expr: lhe}, Number { value: value_1 , expr: rhe}) = (left, right) {
            Number { expr: HExpr::InfixOp {
                lhe: Box::new(lhe.clone()),
                rhe: Box::new(rhe.clone()),
                infix_op: ExpressionInfixOpcode::GreaterEq,
            }, value: modular_arithmetic::greater_eq(value_0, value_1, field) }
        } else {
            ArithmeticExpression::nonquadratic_infix(left, right, &ExpressionInfixOpcode::GreaterEq)
        }
    }

    // Utils
    pub fn apply_substitutions(
        expr: &mut ArithmeticExpression<C>,
        substitution: &Substitution<C>,
        field: &BigInt,
    ) {
        use ArithmeticExpression::*;
        match expr {
            Linear { coefficients, expr } => {
               raw_substitution(coefficients, substitution, field);
               *coefficients = remove_zero_value_coefficients(std::mem::take(coefficients));
               *expr = HExpr::Unknown;
            }
            Signal { symbol, .. } if *symbol == substitution.from => {
                *expr = Linear { coefficients: substitution.to.clone(), expr: HExpr::Unknown };
            }
            Quadratic { a, b, c, expr } => {
                raw_substitution(a, substitution, field);
                *a = remove_zero_value_coefficients(std::mem::take(a));
                raw_substitution(b, substitution, field);
                *b = remove_zero_value_coefficients(std::mem::take(b));
                raw_substitution(c, substitution, field);
                *c = remove_zero_value_coefficients(std::mem::take(c));
                *expr = HExpr::Unknown;
            }
            _ => {}
        }
    }
    pub fn get_usize(expr: &ArithmeticExpression<C>) -> Option<usize> {
        use ArithmeticExpression::*;
        if let Number { value, .. } = expr {
            value.to_usize()
        } else {
            Option::None
        }
    }
    pub fn is_number(&self) -> bool {
        matches!(self, ArithmeticExpression::Number { .. })
    }
    pub fn is_nonquadratic(&self) -> bool {
        matches!(self, ArithmeticExpression::NonQuadratic { .. })
    }
    pub fn is_quadratic(&self) -> bool {
        matches!(self, ArithmeticExpression::Quadratic { .. })
    }
    pub fn is_linear(&self) -> bool {
        matches!(self, ArithmeticExpression::Linear { .. })
    }

    pub fn hashmap_into_arith(mut map: HashMap<C, BigInt>) -> ArithmeticExpression<C> {
        let c: C = ArithmeticExpression::constant_coefficient();
        let expr = if HashMap::len(&map) == 1 && HashMap::contains_key(&map, &c) {
            let value = HashMap::remove(&mut map, &c).unwrap();
            ArithmeticExpression::Number { value, expr: HExpr::Unknown }
        } else if HashMap::len(&map) == 1 {
            let mut values: Vec<_> = map.values().cloned().collect();
            let mut symbols: Vec<_> = map.keys().cloned().collect();
            let symbol = symbols.pop().unwrap();
            let value = values.pop().unwrap();
            if value == BigInt::from(1) {
                ArithmeticExpression::Signal { symbol , expr: HExpr::Unknown }
            } else {
                ArithmeticExpression::initialize_hashmap_for_expression(&mut map);
                ArithmeticExpression::Linear { coefficients: map, expr: HExpr::Unknown }
            }
        } else {
            ArithmeticExpression::initialize_hashmap_for_expression(&mut map);
            ArithmeticExpression::Linear { coefficients: map, expr: HExpr::Unknown }
        };
        expr
    }
}

// ******************************** Constraint Definition ********************************

/*
    Wrapper for linear expression that will be used as a substitution
*/

#[derive(Clone)]
pub struct Substitution<C>
where
    C: Hash + Eq,
{
    pub(crate) from: C,
    pub(crate) to: HashMap<C, BigInt>,
}
impl<C: Default + Clone + Display + Hash + Eq + Ord> Substitution<C> {
    // Substitution public utils
    pub fn new(from: C, to: ArithmeticExpression<C>) -> Option<Substitution<C>> {
        use ArithmeticExpression::*;
        match to {
            Number { value, .. } => {
                let mut to = HashMap::new();
                to.insert(ArithmeticExpression::constant_coefficient(), value);
                Option::Some(Substitution { from, to })
            }
            Signal { symbol, .. } => {
                let mut to = HashMap::new();
                to.insert(symbol, BigInt::from(1));
                Option::Some(Substitution { from, to })
            }
            Linear { coefficients: to, .. } if !to.contains_key(&from) => {
                Option::Some(Substitution { from, to })
            }
            _ => Option::None,
        }
    }

    pub fn apply_correspondence_and_drop<K>(
        substitution: Substitution<C>,
        symbol_correspondence: &HashMap<C, K>,
    ) -> Substitution<K>
    where
        K: Default + Clone + Display + Hash + Eq + Ord,
    {
        Substitution::apply_correspondence(&substitution, symbol_correspondence)
    }

    pub fn constant_coefficient() -> C {
        ArithmeticExpression::constant_coefficient()
    }

    pub fn apply_correspondence<K>(
        substitution: &Substitution<C>,
        symbol_correspondence: &HashMap<C, K>,
    ) -> Substitution<K>
    where
        K: Default + Clone + Display + Hash + Eq + Ord,
    {
        let from = symbol_correspondence.get(&substitution.from).unwrap().clone();
        let to = apply_raw_correspondence(&substitution.to, symbol_correspondence);
        Substitution { to, from }
    }

    pub fn apply_substitution(src: &mut Substitution<C>, change: &Substitution<C>, field: &BigInt) {
        raw_substitution(&mut src.to, change, field);
    }

    pub fn substitution_into_constraint(
        substitution: Substitution<C>,
        field: &BigInt,
    ) -> Constraint<C> {
        let symbol = substitution.from;
        let mut coefficients = substitution.to;
        ArithmeticExpression::initialize_hashmap_for_expression(&mut coefficients);
        coefficients.insert(symbol, BigInt::from(-1 % field));
        let arith = ArithmeticExpression::Linear { coefficients, expr: HExpr::Unknown };
        ArithmeticExpression::transform_expression_to_constraint_form(arith, field).unwrap()
    }

    pub fn decompose(substitution: Substitution<C>) -> (C, ArithmeticExpression<C>) {
        let c: C = ArithmeticExpression::constant_coefficient();
        let mut to = substitution.to;
        let right = if HashMap::len(&to) == 1 && HashMap::contains_key(&to, &c) {
            let value = HashMap::remove(&mut to, &c).unwrap();
            ArithmeticExpression::Number { value, expr: HExpr::Unknown }
        } else if HashMap::len(&to) == 1 {
            let mut values: Vec<_> = to.values().cloned().collect();
            let mut symbols: Vec<_> = to.keys().cloned().collect();
            let symbol = symbols.pop().unwrap();
            let value = values.pop().unwrap();
            if value == BigInt::from(1) {
                ArithmeticExpression::Signal { symbol, expr: HExpr::Unknown }
            } else {
                ArithmeticExpression::initialize_hashmap_for_expression(&mut to);
                ArithmeticExpression::Linear { coefficients: to, expr: HExpr::Unknown }
            }
        } else {
            ArithmeticExpression::initialize_hashmap_for_expression(&mut to);
            ArithmeticExpression::Linear { coefficients: to, expr: HExpr::Unknown }
        };
        (substitution.from, right)
    }

    pub fn map_into_arith_expr(
        substitution: Substitution<C>,
        field: &BigInt,
    ) -> ArithmeticExpression<C> {
        let (left, right) = Substitution::decompose(substitution);
        let left = ArithmeticExpression::Signal { symbol: left, expr: HExpr::Unknown };
        ArithmeticExpression::sub(&right, &left, field)
    }

    pub fn from(&self) -> &C {
        &self.from
    }

    pub fn to(&self) -> &HashMap<C, BigInt> {
        &self.to
    }

    pub fn take_cloned_signals(&self) -> HashSet<C> {
        let cq: C = ArithmeticExpression::constant_coefficient();
        let mut signals = HashSet::new();
        for s in self.to.keys() {
            if cq != *s {
                signals.insert(s.clone());
            }
        }
        signals
    }

    pub fn take_signals(&self) -> HashSet<&C> {
        let cq: C = ArithmeticExpression::constant_coefficient();
        let mut signals = HashSet::new();
        for s in self.to.keys() {
            if cq != *s {
                signals.insert(s);
            }
        }
        signals
    }

    pub fn rmv_zero_coefficients(substitution: &mut Substitution<C>) {
        substitution.to = remove_zero_value_coefficients(std::mem::take(&mut substitution.to))
    }
}

impl<C: Default + Clone + Display + Hash + Eq + Ord + std::cmp::Ord> Substitution<C> {
    pub fn take_cloned_signals_ordered(&self) -> BTreeSet<C> {
        let cq: C = ArithmeticExpression::constant_coefficient();
        let mut signals = BTreeSet::new();
        for s in self.to.keys() {
            if cq != *s {
                signals.insert(s.clone());
            }
        }
        signals
    }
}

impl Substitution<usize> {
    pub fn apply_offset(&self, offset: usize) -> Substitution<usize> {
        let constant: usize = Substitution::constant_coefficient();
        debug_assert_ne!(self.from, constant);
        let from = self.from + offset;
        let to = apply_raw_offset(&self.to, offset);
        Substitution { from, to }
    }
}

/*
    Represents a constraint of the form: A*B - C = 0
    where A,B and C are linear expression.
*/
#[derive(Clone)]
pub struct Constraint<C>
where
    C: Hash + Eq,
{
    pub(crate) a: HashMap<C, BigInt>,
    pub(crate) b: HashMap<C, BigInt>,
    pub(crate) c: HashMap<C, BigInt>,
}

impl<C: Default + Clone + Display + Hash + Eq + Ord> Constraint<C> {
    fn new(a: HashMap<C, BigInt>, b: HashMap<C, BigInt>, c: HashMap<C, BigInt>) -> Constraint<C> {
        Constraint { a, b, c }
    }

    pub fn empty() -> Constraint<C> {
        Constraint::new(
            HashMap::with_capacity(0),
            HashMap::with_capacity(0),
            HashMap::with_capacity(0),
        )
    }

    pub fn constant_coefficient() -> C {
        ArithmeticExpression::constant_coefficient()
    }
    pub fn apply_correspondence_and_drop<K>(
        constraint: Constraint<C>,
        symbol_correspondence: &HashMap<C, K>,
    ) -> Constraint<K>
    where
        K: Default + Clone + Display + Hash + Eq + Ord,
    {
        Constraint::apply_correspondence(&constraint, symbol_correspondence)
    }

    pub fn apply_correspondence<K>(
        constraint: &Constraint<C>,
        symbol_correspondence: &HashMap<C, K>,
    ) -> Constraint<K>
    where
        K: Default + Clone + Display + Hash + Eq + Ord,
    {
        let a = apply_raw_correspondence(&constraint.a, symbol_correspondence);
        let b = apply_raw_correspondence(&constraint.b, symbol_correspondence);
        let c = apply_raw_correspondence(&constraint.c, symbol_correspondence);
        Constraint::new(a, b, c)
    }

    // Constraint simplifications

    pub fn is_linear(constraint: &Constraint<C>) -> bool {
        constraint.a.is_empty() && constraint.b.is_empty()
    }

    pub fn clear_signal_from_linear(
        constraint: Constraint<C>,
        signal: &C,
        field: &BigInt,
    ) -> Substitution<C> {
        debug_assert!(Constraint::is_linear(&constraint));
        debug_assert!(constraint.c.contains_key(signal));
        let raw_expression = Constraint::clear_signal(constraint.c, &signal, field);
        Substitution { from: signal.clone(), to: raw_expression }
    }

    pub fn clear_signal_from_linear_not_normalized(
        constraint: Constraint<C>,
        signal: &C,
        field: &BigInt,
    ) -> (BigInt, Substitution<C>) {
        debug_assert!(Constraint::is_linear(&constraint));
        debug_assert!(constraint.c.contains_key(signal));
        let (coefficient, raw_expression) = Constraint::clear_signal_not_normalized(constraint.c, &signal, field);
        (coefficient, Substitution {from: signal.clone(), to: raw_expression})
    }

    pub fn take_cloned_signals(&self) -> HashSet<C> {
        let mut signals = HashSet::new();
        for signal in self.a().keys() {
            signals.insert(signal.clone());
        }
        for signal in self.b().keys() {
            signals.insert(signal.clone());
        }
        for signal in self.c().keys() {
            signals.insert(signal.clone());
        }
        signals.remove(&Constraint::constant_coefficient());
        signals
    }
    pub fn take_signals(&self) -> HashSet<&C> {
        let cc: C = Constraint::constant_coefficient();
        let mut signals = HashSet::new();
        for signal in self.a().keys() {
            signals.insert(signal);
        }
        for signal in self.b().keys() {
            signals.insert(signal);
        }
        for signal in self.c().keys() {
            signals.insert(signal);
        }
        HashSet::remove(&mut signals, &cc);
        signals
    }

    fn clear_signal(
        mut symbols: HashMap<C, BigInt>,
        key: &C,
        field: &BigInt,
    ) -> HashMap<C, BigInt> {
        let key_value = symbols.remove(&key).unwrap();
        assert!(!key_value.is_zero());
        let value_to_the_right = modular_arithmetic::mul(&key_value, &BigInt::from(-1), field);
        ArithmeticExpression::initialize_hashmap_for_expression(&mut symbols);
        let arithmetic_result = ArithmeticExpression::divide_coefficients_by_constant(
            &value_to_the_right,
            &mut symbols,
            field,
        );
        assert!(arithmetic_result.is_ok());
        remove_zero_value_coefficients(symbols)
    }

    fn clear_signal_not_normalized(
        mut symbols: HashMap<C, BigInt>,
        key: &C,
        field: &BigInt,
    ) -> (BigInt, HashMap<C, BigInt>) {
        let key_value = symbols.remove(&key).unwrap();
        assert!(!key_value.is_zero());
        let value_to_the_right = modular_arithmetic::mul(&key_value, &BigInt::from(-1), field);
        ArithmeticExpression::initialize_hashmap_for_expression(&mut symbols);
        (value_to_the_right, symbols)
    }

    pub fn apply_substitution(
        constraint: &mut Constraint<C>,
        substitution: &Substitution<C>,
        field: &BigInt,
    ) {
        raw_substitution(&mut constraint.a, substitution, field);
        raw_substitution(&mut constraint.b, substitution, field);
        raw_substitution(&mut constraint.c, substitution, field);
        //Constraint::fix_constraint(constraint, field);
    }

    pub fn remove_zero_value_coefficients(constraint: &mut Constraint<C>) {
        constraint.a = remove_zero_value_coefficients(std::mem::take(&mut constraint.a));
        constraint.b = remove_zero_value_coefficients(std::mem::take(&mut constraint.b));
        constraint.c = remove_zero_value_coefficients(std::mem::take(&mut constraint.c));
    }

    pub fn fix_constraint(constraint: &mut Constraint<C>, field: &BigInt) {
        fix_raw_constraint(&mut constraint.a, &mut constraint.b, &mut constraint.c, field);
    }

    pub fn is_empty(&self) -> bool {
        self.a.is_empty() && self.b.is_empty() && self.c.is_empty()
    }

    pub fn has_constant_coefficient(&self) -> bool {
        self.a.contains_key(&Constraint::constant_coefficient())
            || self.b.contains_key(&Constraint::constant_coefficient())
            || self.a.contains_key(&Constraint::constant_coefficient())
    }

    pub fn a(&self) -> &HashMap<C, BigInt> {
        &self.a
    }
    pub fn b(&self) -> &HashMap<C, BigInt> {
        &self.b
    }

    pub fn c(&self) -> &HashMap<C, BigInt> {
        &self.c
    }

    pub fn is_equality(&self, field: &BigInt) -> bool {
        signal_equals_signal(&self.a, &self.b, &self.c, field)
    }

    pub fn is_constant_equality(&self) -> bool {
        signal_equals_constant(&self.a, &self.b, &self.c)
    }

    pub fn into_arithmetic_expressions(self) -> (ArithmeticExpression<C>, ArithmeticExpression<C>, ArithmeticExpression<C>) {
        (
            ArithmeticExpression::Linear { coefficients: self.a, expr: HExpr::Unknown },
            ArithmeticExpression::Linear { coefficients: self.b, expr: HExpr::Unknown },
            ArithmeticExpression::Linear { coefficients: self.c, expr: HExpr::Unknown },
        )
    }

}

impl<C: Default + Clone + Display + Hash + Eq + Ord + std::cmp::Ord> Constraint<C> {
    pub fn take_cloned_signals_ordered(&self) -> BTreeSet<C> {
        let mut signals = BTreeSet::new();
        for signal in self.a().keys() {
            signals.insert(signal.clone());
        }
        for signal in self.b().keys() {
            signals.insert(signal.clone());
        }
        for signal in self.c().keys() {
            signals.insert(signal.clone());
        }
        signals.remove(&Constraint::constant_coefficient());
        signals
    }

}

impl Constraint<usize> {
    pub fn apply_offset(&self, offset: usize) -> Constraint<usize> {
        let a = apply_raw_offset(&self.a, offset);
        let b = apply_raw_offset(&self.b, offset);
        let c = apply_raw_offset(&self.c, offset);
        Constraint::new(a, b, c)
    }
    pub fn apply_witness(&self, witness: &Vec<usize>) -> Constraint<usize> {
        let a = apply_vectored_correspondence(&self.a, witness);
        let b = apply_vectored_correspondence(&self.b, witness);
        let c = apply_vectored_correspondence(&self.c, witness);
        Constraint::new(a, b, c)
    }
}

// model utils
type RawExpr<C> = HashMap<C, BigInt>;

fn apply_vectored_correspondence(
    symbols: &HashMap<usize, BigInt>,
    map: &Vec<usize>,
) -> HashMap<usize, BigInt> {
    let mut mapped = HashMap::new();
    for (s, v) in symbols {
        mapped.insert(map[*s], v.clone());
    }
    mapped
}

fn apply_raw_correspondence<C, K>(
    symbols: &HashMap<C, BigInt>,
    map: &HashMap<C, K>,
) -> HashMap<K, BigInt>
where
    K: Default + Clone + Display + Hash + Eq + Ord,
    C: Default + Clone + Display + Hash + Eq + Ord,
{
    let constant_coefficient: C = ArithmeticExpression::constant_coefficient();
    let mut coefficients_as_correspondence = HashMap::new();
    for (key, value) in symbols {
        let id = if key.eq(&constant_coefficient) {
            ArithmeticExpression::constant_coefficient()
        } else {
            map.get(&key).expect(&format!("Unknown signal: {}", key)).clone()
        };
        coefficients_as_correspondence.insert(id, value.clone());
    }
    coefficients_as_correspondence
}

fn apply_raw_offset(h: &HashMap<usize, BigInt>, offset: usize) -> HashMap<usize, BigInt> {
    let mut new = HashMap::new();
    let constant: usize = Constraint::constant_coefficient();
    for (k, v) in h {
        if *k == constant {
            new.insert(*k, v.clone());
        } else {
            new.insert(*k + offset, v.clone());
        }
    }
    new
}

fn raw_substitution<C>(
    change: &mut HashMap<C, BigInt>,
    substitution: &Substitution<C>,
    field: &BigInt,
) where
    C: Default + Clone + Display + Hash + Eq + Ord,
{
    ArithmeticExpression::initialize_hashmap_for_expression(change);
    if let Option::Some(val) = change.remove(&substitution.from) {
        let mut coefficients = substitution.to.clone();
        ArithmeticExpression::initialize_hashmap_for_expression(&mut coefficients);
        ArithmeticExpression::multiply_coefficients_by_constant(&val, &mut coefficients, field);
        ArithmeticExpression::add_coefficients_to_coefficients(&coefficients, change, field);
    }
    //*change = remove_zero_value_coefficients(std::mem::take(change));
}

fn remove_zero_value_coefficients<C>(raw_expression: HashMap<C, BigInt>) -> HashMap<C, BigInt>
where
    C: Default + Clone + Display + Hash + Eq + Ord,
{
    let mut clean_raw = HashMap::new();
    for (key, val) in raw_expression {
        if !val.is_zero() {
            clean_raw.insert(key, val);
        }
    }
    clean_raw
}

fn fix_raw_constraint<C>(a: &mut RawExpr<C>, b: &mut RawExpr<C>, c: &mut RawExpr<C>, field: &BigInt)
where
    C: Default + Clone + Display + Hash + Eq + Ord,
{
    *a = remove_zero_value_coefficients(std::mem::take(a));
    *b = remove_zero_value_coefficients(std::mem::take(b));
    *c = remove_zero_value_coefficients(std::mem::take(c));
    if HashMap::is_empty(a) || HashMap::is_empty(b) {
        HashMap::clear(a);
        HashMap::clear(b);
    } else if is_constant_expression(a) {
        constant_linear_linear_reduction(a, b, c, field);
    } else if is_constant_expression(b) {
        constant_linear_linear_reduction(b, a, c, field);
    }
}

fn constant_linear_linear_reduction<C>(
    a: &mut RawExpr<C>,
    b: &mut RawExpr<C>,
    c: &mut RawExpr<C>,
    field: &BigInt,
) where
    C: Default + Clone + Display + Hash + Eq + Ord,
{
    let cq: C = ArithmeticExpression::constant_coefficient();
    ArithmeticExpression::initialize_hashmap_for_expression(c);
    ArithmeticExpression::initialize_hashmap_for_expression(b);
    let constant = HashMap::remove(a, &cq).unwrap();
    ArithmeticExpression::multiply_coefficients_by_constant(&constant, b, field);
    ArithmeticExpression::multiply_coefficients_by_constant(&BigInt::from(-1), b, field);
    ArithmeticExpression::add_coefficients_to_coefficients(b, c, field);
    *c = remove_zero_value_coefficients(std::mem::take(c));
    HashMap::clear(a);
    HashMap::clear(b);
}

fn signal_equals_signal<C>(a: &RawExpr<C>, b: &RawExpr<C>, c: &RawExpr<C>, field: &BigInt) -> bool
where
    C: Default + Clone + Display + Hash + Eq + Ord,
{
    let cq: C = ArithmeticExpression::constant_coefficient();
    if a.is_empty() && b.is_empty() && !HashMap::contains_key(c, &cq) && c.len() == 2 {
        let signals: Vec<_> = c.keys().cloned().collect();
        let c0 = HashMap::get(c, &signals[0]).unwrap();
        let c1 = HashMap::get(c, &signals[1]).unwrap();
        let c1_p = modular_arithmetic::mul(&BigInt::from(-1), c1, field);
        c1_p == *c0
    } else {
        false
    }
}

fn signal_equals_constant<C>(a: &RawExpr<C>, b: &RawExpr<C>, c: &RawExpr<C>) -> bool
where
    C: Default + Clone + Display + Hash + Eq + Ord,
{
    let cq: C = ArithmeticExpression::constant_coefficient();
    HashMap::is_empty(a)
        && HashMap::is_empty(b)
        && 
        	((HashMap::contains_key(c, &cq) && HashMap::len(c) == 2) ||
        	(!HashMap::contains_key(c, &cq) && HashMap::len(c) == 1))
}

fn is_constant_expression<C>(expr: &RawExpr<C>) -> bool
where
    C: Default + Clone + Display + Hash + Eq + Ord,
{
    let cq: C = ArithmeticExpression::constant_coefficient();
    HashMap::contains_key(expr, &cq) && HashMap::len(expr) == 1
}

pub fn normalize(c: Constraint<usize>, _field: &BigInt) -> Constraint<usize> {
    use std::collections::LinkedList;
    let _a: LinkedList<_> = c.a.iter().clone().collect();
    let _b: LinkedList<_> = c.b.iter().clone().collect();
    let _c: LinkedList<_> = c.c.iter().clone().collect();
    todo!()
}

#[cfg(test)]
mod test {
    use crate::algebra::{ArithmeticExpression, Constraint, HintExpression, Substitution};
    use crate::modular_arithmetic;
    use num_bigint::BigInt;
    use std::collections::HashMap;
    const FIELD: &str = "257";
    type C = Constraint<usize>;
    type S = Substitution<usize>;
    type A = ArithmeticExpression<usize>;

    #[test]
    fn algebra_constraint_offset() {
        let offset = 7;
        let x = 1;
        let a = HashMap::new();
        let b = HashMap::new();
        let mut c = HashMap::new();
        c.insert(C::constant_coefficient(), BigInt::from(12));
        c.insert(x, BigInt::from(3));
        let constraint = C::new(a, b, c);
        let constraint_with_offset = constraint.apply_offset(offset);
        assert!(constraint_with_offset.a.is_empty());
        assert!(constraint_with_offset.b.is_empty());
        assert!(constraint_with_offset.c.contains_key(&C::constant_coefficient()));
        assert!(!constraint_with_offset.c.contains_key(&x));
        let new_x = x + offset;
        assert!(constraint_with_offset.c.contains_key(&new_x));
        let value = constraint_with_offset.c.get(&new_x).unwrap();
        assert!(value.eq(&BigInt::from(3)));
    }

    #[test]
    fn algebra_constraint_clear_signal() {
        let field = BigInt::parse_bytes(FIELD.as_bytes(), 10)
            .expect("generating the big int was not possible");
        let constant = C::constant_coefficient();
        let c_coefficient = BigInt::from(3);
        let x = 1;
        let x_coefficient = BigInt::from(1);
        let y = 2;
        let y_coefficient = BigInt::from(1);
        let a = HashMap::new();
        let b = HashMap::new();
        let mut c = HashMap::new();
        c.insert(x, x_coefficient);
        c.insert(y, y_coefficient);
        c.insert(constant, c_coefficient);
        // constraint: x + y + 3 = 0
        let constraint = C::new(a, b, c);
        // sub: x = -y -3  ==> x = 256*y + 254
        let sub = C::clear_signal_from_linear(constraint, &x, &field);
        assert_eq!(sub.from, x);
        let y_new_coefficient = modular_arithmetic::sub(&field, &BigInt::from(1), &field);
        let sub_value = sub.to.get(&y).unwrap();
        assert_eq!(*sub_value, y_new_coefficient);

        let constant_new_coefficient = BigInt::from(254);
        let sub_value = sub.to.get(&constant).unwrap();
        assert_eq!(*sub_value, constant_new_coefficient);
    }

    #[test]
    fn algebra_constraint_apply_substitution() {
        let field = BigInt::parse_bytes(FIELD.as_bytes(), 10)
            .expect("generating the big int was not possible");
        // symbols
        let x = 1;
        let y = 2;
        let constant = C::constant_coefficient();

        // constraint: x + y + 4 = 0
        let x_c = BigInt::from(1);
        let y_c = BigInt::from(1);
        let constant_c = BigInt::from(4);
        let a = HashMap::new();
        let b = HashMap::new();
        let mut c = HashMap::new();
        c.insert(x, x_c);
        c.insert(y, y_c);
        c.insert(constant, constant_c);
        let mut constraint = C::new(a, b, c);

        // substitution: x = 2y + 3
        let y_c = BigInt::from(2);
        let constant_c = BigInt::from(3);
        let from = x;
        let mut to_raw = HashMap::new();
        to_raw.insert(y, y_c);
        to_raw.insert(constant, constant_c);
        let to = A::Linear { coefficients: to_raw, expr: HintExpression::Unknown };
        let substitution = S::new(from, to).unwrap();

        // result: 3y + 7 = 0
        let expected_y_c = BigInt::from(3);
        let expected_constant_c = BigInt::from(7);
        C::apply_substitution(&mut constraint, &substitution, &field);
        let y_c = constraint.c.get(&y).unwrap();
        let constant_c = constraint.c.get(&constant).unwrap();
        assert!(constraint.a.is_empty());
        assert!(constraint.b.is_empty());
        assert_eq!(*y_c, expected_y_c);
        assert_eq!(*constant_c, expected_constant_c);
    }
}
