use super::modular_arithmetic;
pub use super::modular_arithmetic::ArithmeticError;
use num_bigint::BigInt;
use num_traits::{Num, ToPrimitive, Zero};
use program_structure::ast::{ExpressionInfixOpcode, ExpressionPrefixOpcode};
use program_structure::memory_slice::{MemorySlice, Update};
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
    Useless,
    Number { value: BigInt },
    Signal {
        symbol: String,
        access: Vec<HintAccess>,
    },
    Intermediate {
        symbol: String,
        access: Vec<HintAccess>,
    },
    Variable {
        symbol: String,
        access: Vec<HintAccess>,
    },
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
    Call {
        function_name: String,
        arguments: Vec<HintExpression>,
    },
}

impl HintExpression {
    pub fn to_json(&self) -> String {
        use HintExpression::*;
        match self {
            Useless => unreachable!(),
            Unknown => {
                format!("{{\"$\": \"Expr\", \"@\": \"Unknown\"}}")
            }
            Number { value } => {
                format!("{{\"$\": \"Expr\", \"@\": \"Number\", \"value\": \"{}\"}}", value.to_str_radix(10))
            }
            Intermediate { symbol, access } => {
                format!("{{\"$\": \"Expr\", \"@\": \"Intermediate\", \"symbol\": \"{}\", \"access\": [{}]}}", symbol,
                    access.iter()
                    .map(|acc| acc.to_json())
                    .collect::<Vec<String>>()
                    .join(", ")
                )
            }
            Signal { symbol, access } => {
                format!("{{\"$\": \"Expr\", \"@\": \"Signal\", \"symbol\": \"{}\", \"access\": [{}]}}", symbol,
                    access.iter()
                    .map(|acc| acc.to_json())
                    .collect::<Vec<String>>()
                    .join(", ")
                )
            }
            Variable { symbol, access } => {
                format!("{{\"$\": \"Expr\", \"@\": \"Variable\", \"symbol\": \"{}\", \"access\": [{}]}}", symbol,
                    access.iter()
                    .map(|acc| acc.to_json())
                    .collect::<Vec<String>>()
                    .join(", ")
                )
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
                    "{{\"$\": \"Expr\", \"@\": \"InlineSwitch\", \"condition\": {}, \"true_case\": {}, \"false_case\": {}}}",
                    condition.to_json(),
                    if_true.to_json(),
                    if_false.to_json(),
                )
            }
            Call { function_name, arguments } => {
                let args_json = arguments.iter()
                    .map(|arg| arg.to_json())
                    .collect::<Vec<String>>()
                    .join(", ");
                format!(
                    "{{\"$\": \"Expr\", \"@\": \"Call\", \"function_name\": \"{}\", \"arguments\": [{}]}}",
                    function_name,
                    args_json
                )
            }
        }
    }
}

#[derive(Clone)]
pub struct ArithmeticExpression<C>
where
    C: Default + Clone + Display + Hash + Eq + Ord,
{
    pub hint: HintExpression,
    pub pure: PureArithmeticExpression<C>,
}

impl<C: Default + Clone + Display + Hash + Eq + Ord> ArithmeticExpression<C>
{
    pub fn add(
        left: &ArithmeticExpression<C>,
        right: &ArithmeticExpression<C>,
        field: &BigInt,
    ) -> ArithmeticExpression<C> {
        ArithmeticExpression {
            hint: HExpr::InfixOp {
                lhe: Box::new(left.hint.clone()),
                rhe: Box::new(right.hint.clone()),
                infix_op: ExpressionInfixOpcode::Add,
            },
            pure: PureArithmeticExpression::add(&left.pure, &right.pure, field),
        }
    }

    pub fn mul(
        left: &ArithmeticExpression<C>,
        right: &ArithmeticExpression<C>,
        field: &BigInt,
    ) -> ArithmeticExpression<C> {
        ArithmeticExpression {
            hint: HExpr::InfixOp {
                lhe: Box::new(left.hint.clone()),
                rhe: Box::new(right.hint.clone()),
                infix_op: ExpressionInfixOpcode::Mul,
            },
            pure: PureArithmeticExpression::mul(&left.pure, &right.pure, field),
        }
    }

    pub fn sub(
        left: &ArithmeticExpression<C>,
        right: &ArithmeticExpression<C>,
        field: &BigInt,
    ) -> ArithmeticExpression<C> {
        ArithmeticExpression {
            hint: HExpr::InfixOp {
                lhe: Box::new(left.hint.clone()),
                rhe: Box::new(right.hint.clone()),
                infix_op: ExpressionInfixOpcode::Sub,
            },
            pure: PureArithmeticExpression::sub(&left.pure, &right.pure, field),
        }
    }

    pub fn div(
        left: &ArithmeticExpression<C>,
        right: &ArithmeticExpression<C>,
        field: &BigInt,
    ) -> Result<ArithmeticExpression<C>, ArithmeticError> {
        Ok(ArithmeticExpression {
            hint: HExpr::InfixOp {
                lhe: Box::new(left.hint.clone()),
                rhe: Box::new(right.hint.clone()),
                infix_op: ExpressionInfixOpcode::Div,
            },
            pure: PureArithmeticExpression::div(&left.pure, &right.pure, field)?,
        })
    }

    pub fn idiv(
        left: &ArithmeticExpression<C>,
        right: &ArithmeticExpression<C>,
        field: &BigInt,
    ) -> Result<ArithmeticExpression<C>, ArithmeticError> {
        Ok(ArithmeticExpression {
            hint: HExpr::InfixOp {
                lhe: Box::new(left.hint.clone()),
                rhe: Box::new(right.hint.clone()),
                infix_op: ExpressionInfixOpcode::IntDiv,
            },
            pure: PureArithmeticExpression::idiv(&left.pure, &right.pure, field)?,
        })
    }

    pub fn mod_op(
        left: &ArithmeticExpression<C>,
        right: &ArithmeticExpression<C>,
        field: &BigInt,
    ) -> Result<ArithmeticExpression<C>, ArithmeticError> {
        Ok(ArithmeticExpression {
            hint: HExpr::InfixOp {
                lhe: Box::new(left.hint.clone()),
                rhe: Box::new(right.hint.clone()),
                infix_op: ExpressionInfixOpcode::Mod,
            },
            pure: PureArithmeticExpression::mod_op(&left.pure, &right.pure, field)?,
        })
    }

    pub fn pow(
        base: &ArithmeticExpression<C>,
        exponent: &ArithmeticExpression<C>,
        field: &BigInt,
    ) -> ArithmeticExpression<C> {
        ArithmeticExpression {
            hint: HExpr::InfixOp {
                lhe: Box::new(base.hint.clone()),
                rhe: Box::new(exponent.hint.clone()),
                infix_op: ExpressionInfixOpcode::Pow,
            },
            pure: PureArithmeticExpression::pow(&base.pure, &exponent.pure, field),
        }
    }

    pub fn prefix_sub(
        expr: &ArithmeticExpression<C>,
        field: &BigInt,
    ) -> ArithmeticExpression<C> {
        ArithmeticExpression {
            hint: HExpr::PrefixOp {
                rhe: Box::new(expr.hint.clone()),
                prefix_op: ExpressionPrefixOpcode::Neg,
            },
            pure: PureArithmeticExpression::prefix_sub(&expr.pure, field),
        }
    }

    pub fn complement(
        expr: &ArithmeticExpression<C>,
        field: &BigInt,
    ) -> ArithmeticExpression<C> {
        ArithmeticExpression {
            hint: HExpr::PrefixOp {
                rhe: Box::new(expr.hint.clone()),
                prefix_op: ExpressionPrefixOpcode::Complement,
            },
            pure: PureArithmeticExpression::complement(&expr.pure, field),
        }
    }

    pub fn shift_l(
        left: &ArithmeticExpression<C>,
        right: &ArithmeticExpression<C>,
        field: &BigInt,
    ) -> Result<ArithmeticExpression<C>, ArithmeticError> {
        Ok(ArithmeticExpression {
            hint: HExpr::InfixOp {
                lhe: Box::new(left.hint.clone()),
                rhe: Box::new(right.hint.clone()),
                infix_op: ExpressionInfixOpcode::ShiftL,
            },
            pure: PureArithmeticExpression::shift_l(&left.pure, &right.pure, field)?,
        })
    }

    pub fn shift_r(
        left: &ArithmeticExpression<C>,
        right: &ArithmeticExpression<C>,
        field: &BigInt,
    ) -> Result<ArithmeticExpression<C>, ArithmeticError> {
        Ok(ArithmeticExpression {
            hint: HExpr::InfixOp {
                lhe: Box::new(left.hint.clone()),
                rhe: Box::new(right.hint.clone()),
                infix_op: ExpressionInfixOpcode::ShiftR,
            },
            pure: PureArithmeticExpression::shift_r(&left.pure, &right.pure, field)?,
        })
    }

    pub fn bit_or(
        left: &ArithmeticExpression<C>,
        right: &ArithmeticExpression<C>,
        field: &BigInt,
    ) -> ArithmeticExpression<C> {
        ArithmeticExpression {
            hint: HExpr::InfixOp {
                lhe: Box::new(left.hint.clone()),
                rhe: Box::new(right.hint.clone()),
                infix_op: ExpressionInfixOpcode::BitOr,
            },
            pure: PureArithmeticExpression::bit_or(&left.pure, &right.pure, field),
        }
    }

    pub fn bit_and(
        left: &ArithmeticExpression<C>,
        right: &ArithmeticExpression<C>,
        field: &BigInt,
    ) -> ArithmeticExpression<C> {
        ArithmeticExpression {
            hint: HExpr::InfixOp {
                lhe: Box::new(left.hint.clone()),
                rhe: Box::new(right.hint.clone()),
                infix_op: ExpressionInfixOpcode::BitAnd,
            },
            pure: PureArithmeticExpression::bit_and(&left.pure, &right.pure, field),
        }
    }

    pub fn bit_xor(
        left: &ArithmeticExpression<C>,
        right: &ArithmeticExpression<C>,
        field: &BigInt,
    ) -> ArithmeticExpression<C> {
        ArithmeticExpression {
            hint: HExpr::InfixOp {
                lhe: Box::new(left.hint.clone()),
                rhe: Box::new(right.hint.clone()),
                infix_op: ExpressionInfixOpcode::BitXor,
            },
            pure: PureArithmeticExpression::bit_xor(&left.pure, &right.pure, field),
        }
    }

    pub fn not(
        expr: &ArithmeticExpression<C>,
        field: &BigInt,
    ) -> ArithmeticExpression<C> {
        ArithmeticExpression {
            hint: HExpr::PrefixOp {
                rhe: Box::new(expr.hint.clone()),
                prefix_op: ExpressionPrefixOpcode::BoolNot,
            },
            pure: PureArithmeticExpression::not(&expr.pure, field),
        }
    }

    pub fn bool_or(
        left: &ArithmeticExpression<C>,
        right: &ArithmeticExpression<C>,
        field: &BigInt,
    ) -> ArithmeticExpression<C> {
        ArithmeticExpression {
            hint: HExpr::InfixOp {
                lhe: Box::new(left.hint.clone()),
                rhe: Box::new(right.hint.clone()),
                infix_op: ExpressionInfixOpcode::BoolOr,
            },
            pure: PureArithmeticExpression::bool_or(&left.pure, &right.pure, field),
        }
    }

    pub fn bool_and(
        left: &ArithmeticExpression<C>,
        right: &ArithmeticExpression<C>,
        field: &BigInt,
    ) -> ArithmeticExpression<C> {
        ArithmeticExpression {
            hint: HExpr::InfixOp {
                lhe: Box::new(left.hint.clone()),
                rhe: Box::new(right.hint.clone()),
                infix_op: ExpressionInfixOpcode::BoolAnd,
            },
            pure: PureArithmeticExpression::bool_and(&left.pure, &right.pure, field),
        }
    }

    pub fn eq(
        left: &ArithmeticExpression<C>,
        right: &ArithmeticExpression<C>,
        field: &BigInt,
    ) -> ArithmeticExpression<C> {
        ArithmeticExpression {
            hint: HExpr::InfixOp {
                lhe: Box::new(left.hint.clone()),
                rhe: Box::new(right.hint.clone()),
                infix_op: ExpressionInfixOpcode::Eq,
            },
            pure: PureArithmeticExpression::eq(&left.pure, &right.pure, field),
        }
    }

    pub fn not_eq(
        left: &ArithmeticExpression<C>,
        right: &ArithmeticExpression<C>,
        field: &BigInt,
    ) -> ArithmeticExpression<C> {
        ArithmeticExpression {
            hint: HExpr::InfixOp {
                lhe: Box::new(left.hint.clone()),
                rhe: Box::new(right.hint.clone()),
                infix_op: ExpressionInfixOpcode::NotEq,
            },
            pure: PureArithmeticExpression::not_eq(&left.pure, &right.pure, field),
        }
    }

    pub fn lesser(
        left: &ArithmeticExpression<C>,
        right: &ArithmeticExpression<C>,
        field: &BigInt,
    ) -> ArithmeticExpression<C> {
        ArithmeticExpression {
            hint: HExpr::InfixOp {
                lhe: Box::new(left.hint.clone()),
                rhe: Box::new(right.hint.clone()),
                infix_op: ExpressionInfixOpcode::Lesser,
            },
            pure: PureArithmeticExpression::lesser(&left.pure, &right.pure, field),
        }
    }

    pub fn lesser_eq(
        left: &ArithmeticExpression<C>,
        right: &ArithmeticExpression<C>,
        field: &BigInt,
    ) -> ArithmeticExpression<C> {
        ArithmeticExpression {
            hint: HExpr::InfixOp {
                lhe: Box::new(left.hint.clone()),
                rhe: Box::new(right.hint.clone()),
                infix_op: ExpressionInfixOpcode::LesserEq,
            },
            pure: PureArithmeticExpression::lesser_eq(&left.pure, &right.pure, field),
        }
    }

    pub fn greater(
        left: &ArithmeticExpression<C>,
        right: &ArithmeticExpression<C>,
        field: &BigInt,
    ) -> ArithmeticExpression<C> {
        ArithmeticExpression {
            hint: HExpr::InfixOp {
                lhe: Box::new(left.hint.clone()),
                rhe: Box::new(right.hint.clone()),
                infix_op: ExpressionInfixOpcode::Greater,
            },
            pure: PureArithmeticExpression::greater(&left.pure, &right.pure, field),
        }
    }

    pub fn greater_eq(
        left: &ArithmeticExpression<C>,
        right: &ArithmeticExpression<C>,
        field: &BigInt,
    ) -> ArithmeticExpression<C> {
        ArithmeticExpression {
            hint: HExpr::InfixOp {
                lhe: Box::new(left.hint.clone()),
                rhe: Box::new(right.hint.clone()),
                infix_op: ExpressionInfixOpcode::GreaterEq,
            },
            pure: PureArithmeticExpression::greater_eq(&left.pure, &right.pure, field),
        }
    }
}

impl<C: Default + Clone + Display + Hash + Eq + Ord> Display for ArithmeticExpression<C>
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.pure)
    }
}

impl<C: Default + Clone + Display + Hash + Eq + Ord> Eq for ArithmeticExpression<C>
{
}

impl<C: Default + Clone + Display + Hash + Eq + Ord> PartialEq for ArithmeticExpression<C>
{
    fn eq(&self, other: &Self) -> bool {
        self.pure == other.pure
    }
}

impl<C: Default + Clone + Display + Hash + Eq + Ord> Default for ArithmeticExpression<C>
{
    fn default() -> Self {
        ArithmeticExpression {
            hint: HintExpression::Unknown,
            pure: PureArithmeticExpression::default(),
        }
    }
}

impl<C: Default + Clone + Display + Hash + Eq + Ord> Update for ArithmeticExpression<C> {
    fn update_pure(&mut self, other: &Self) {
        self.pure = other.pure.clone();
    }

    fn update_hint(&mut self, other: &Self) {
        self.hint = other.hint.clone();
    }
}

impl<C: Default + Clone + Display + Hash + Eq + Ord> ArithmeticExpression<C>
{
    pub fn new() -> Self {
        ArithmeticExpression::default()
    }

    pub fn slice_to_json(slice: MemorySlice<ArithmeticExpression<C>>) -> String {
        if slice.is_single() {
            MemorySlice::unwrap_to_single(slice.clone()).hint.to_json()
        } else {
            unimplemented!();
        }
    }
}

pub enum PureArithmeticExpression<C>
where
    C: Hash + Eq,
{
    Number {
        value: BigInt,
    },
    Signal {
        symbol: C,
    },
    Linear {
        // Represents the expression: c1*s1 + .. + cn*sn + C
        // where c1..cn are integers modulo a prime and
        // s1..sn are signals. C is a constant value
        coefficients: HashMap<C, BigInt>,
    },
    Quadratic {
        // Is a quadratic expression of the form:
        //              a*b + c
        // Where a,b and c are linear expression
        a: HashMap<C, BigInt>,
        b: HashMap<C, BigInt>,
        c: HashMap<C, BigInt>,
    },
    NonQuadratic, // Represents an expression that is not quadratic
}

impl<C: Default + Clone + Display + Hash + Eq + Ord> PureArithmeticExpression<C> {
    pub fn to_json(&self) -> String {
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
        use PureArithmeticExpression::*;
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
}

impl<C: Default + Clone + Display + Hash + Eq + Ord> Display for PureArithmeticExpression<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use PureArithmeticExpression::*;
        match self {
            Number { value, .. } => {
                write!(f, "{}", value.to_str_radix(10))
            }
            Signal { symbol, .. } => {
                write!(f, "{}", symbol)
            }
            Linear { coefficients , ..} => {
                let string_coefficients = PureArithmeticExpression::string_from_coefficients(coefficients);
                write!(f, "{}", string_coefficients)
            }
            Quadratic { a, b, c , ..} => {
                let string_a = PureArithmeticExpression::string_from_coefficients(a);
                let string_b = PureArithmeticExpression::string_from_coefficients(b);
                let string_c = PureArithmeticExpression::string_from_coefficients(c);
                write!(f, "({})*({}) + ({})", string_a, string_b, string_c)
            }
            NonQuadratic { .. }  => {
                write!(f, "NQ")
            }
        }
    }
}

impl<C: Default + Clone + Display + Hash + Eq + Ord> Clone for PureArithmeticExpression<C> {
    fn clone(&self) -> Self {
        use PureArithmeticExpression::*;
        match self {
            Number { value } => Number { value: value.clone() },
            Signal { symbol } => Signal { symbol: symbol.clone() },
            Linear { coefficients } => Linear { coefficients: coefficients.clone() },
            Quadratic { a, b, c } => Quadratic { a: a.clone(), b: b.clone(), c: c.clone() },
            NonQuadratic => NonQuadratic,
        }
    }
}

impl<C: Default + Clone + Display + Hash + Eq + Ord> Eq for PureArithmeticExpression<C> {}
impl<C: Default + Clone + Display + Hash + Eq + Ord> PartialEq for PureArithmeticExpression<C> {
    fn eq(&self, other: &Self) -> bool {
        use PureArithmeticExpression::*;
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

impl<C: Default + Clone + Display + Hash + Eq + Ord> Default for PureArithmeticExpression<C> {
    fn default() -> Self {
        PureArithmeticExpression::NonQuadratic
    }
}

impl<C: Default + Clone + Display + Hash + Eq + Ord> PureArithmeticExpression<C> {
    pub fn new() -> PureArithmeticExpression<C> {
        PureArithmeticExpression::default()
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
            let (sign, abs_component_string) = if signal.eq(&PureArithmeticExpression::constant_coefficient()) {
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
        arithmetic_expression: PureArithmeticExpression<C>,
        field: &BigInt,
    ) -> Option<Constraint<C>> {
        use PureArithmeticExpression::*;
        let mut a = HashMap::new();
        let mut b = HashMap::new();
        let mut c = HashMap::new();
        PureArithmeticExpression::initialize_hashmap_for_expression(&mut a);
        PureArithmeticExpression::initialize_hashmap_for_expression(&mut b);
        PureArithmeticExpression::initialize_hashmap_for_expression(&mut c);
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
                c.insert(PureArithmeticExpression::constant_coefficient(), value);
            }
            Signal { symbol, .. } => {
                c.insert(symbol, BigInt::from(1));
            }
            Linear { coefficients, .. } => {
                c = coefficients;
            }
        }
        PureArithmeticExpression::multiply_coefficients_by_constant(&BigInt::from(-1), &mut c, field);
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
            .entry(PureArithmeticExpression::constant_coefficient())
            .or_insert_with(|| BigInt::from(0));
        debug_assert!(PureArithmeticExpression::valid_hashmap_for_expression(initial));
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
        let cc = PureArithmeticExpression::constant_coefficient();
        h.contains_key(&cc)
    }
    fn initialize_symbol_in_coefficients(symbol: &C, coefficients: &mut HashMap<C, BigInt>) {
        debug_assert!(PureArithmeticExpression::valid_hashmap_for_expression(coefficients));
        if !coefficients.contains_key(symbol) {
            coefficients.insert(symbol.clone(), BigInt::from(0));
        }
        debug_assert!(PureArithmeticExpression::valid_hashmap_for_expression(coefficients));
    }
    fn add_constant_to_coefficients(
        value: &BigInt,
        coefficients: &mut HashMap<C, BigInt>,
        field: &BigInt,
    ) {
        debug_assert!(PureArithmeticExpression::valid_hashmap_for_expression(coefficients));
        let cc: C = PureArithmeticExpression::constant_coefficient();
        coefficients.insert(
            cc.clone(),
            modular_arithmetic::add(coefficients.get(&cc).unwrap(), value, field),
        );
        debug_assert!(PureArithmeticExpression::valid_hashmap_for_expression(coefficients));
    }
    fn add_symbol_to_coefficients(
        symbol: &C,
        coefficient: &BigInt,
        coefficients: &mut HashMap<C, BigInt>,
        field: &BigInt,
    ) {
        debug_assert!(PureArithmeticExpression::valid_hashmap_for_expression(coefficients));
        PureArithmeticExpression::initialize_symbol_in_coefficients(symbol, coefficients);
        coefficients.insert(
            symbol.clone(),
            modular_arithmetic::add(coefficients.get(symbol).unwrap(), coefficient, field),
        );
        debug_assert!(PureArithmeticExpression::valid_hashmap_for_expression(coefficients));
    }
    fn add_coefficients_to_coefficients(
        coefficients_0: &HashMap<C, BigInt>,
        coefficients_1: &mut HashMap<C, BigInt>,
        field: &BigInt,
    ) {
        debug_assert!(PureArithmeticExpression::valid_hashmap_for_expression(coefficients_0));
        debug_assert!(PureArithmeticExpression::valid_hashmap_for_expression(coefficients_1));
        for (symbol, coefficient) in coefficients_0 {
            PureArithmeticExpression::add_symbol_to_coefficients(
                symbol,
                coefficient,
                coefficients_1,
                field,
            );
        }
        debug_assert!(PureArithmeticExpression::valid_hashmap_for_expression(coefficients_0));
        debug_assert!(PureArithmeticExpression::valid_hashmap_for_expression(coefficients_1));
    }
    fn multiply_coefficients_by_constant(
        constant: &BigInt,
        coefficients: &mut HashMap<C, BigInt>,
        field: &BigInt,
    ) {
        debug_assert!(PureArithmeticExpression::valid_hashmap_for_expression(coefficients));
        for value in coefficients.values_mut() {
            *value = modular_arithmetic::mul(value, constant, field);
        }
        debug_assert!(PureArithmeticExpression::valid_hashmap_for_expression(coefficients));
    }
    fn divide_coefficients_by_constant(
        constant: &BigInt,
        coefficients: &mut HashMap<C, BigInt>,
        field: &BigInt,
    ) -> Result<(), ArithmeticError> {
        debug_assert!(PureArithmeticExpression::valid_hashmap_for_expression(coefficients));
        let inverse_constant = modular_arithmetic::div(
            &BigInt::from(1),
            constant,
            &field
        )?;
        PureArithmeticExpression::multiply_coefficients_by_constant(&inverse_constant, coefficients, field);
        debug_assert!(PureArithmeticExpression::valid_hashmap_for_expression(coefficients));
        Result::Ok(())
    }


    pub fn add(
        left: &PureArithmeticExpression<C>,
        right: &PureArithmeticExpression<C>,
        field: &BigInt,
    ) -> PureArithmeticExpression<C> {
        use PureArithmeticExpression::*;
        match (left, right) {
            (NonQuadratic, _) | (_, NonQuadratic) | (Quadratic { .. }, Quadratic { .. }) => {
                NonQuadratic
            }
            (Number { value: v_0 }, Number { value: v_1 }) => {
                Number { value: modular_arithmetic::add(v_0, v_1, field) }
            }
            (Number { value }, Signal { symbol }) | (Signal { symbol }, Number { value }) => {
                let mut coefficients = HashMap::new();
                PureArithmeticExpression::initialize_hashmap_for_expression(&mut coefficients);
                PureArithmeticExpression::add_constant_to_coefficients(value, &mut coefficients, field);
                PureArithmeticExpression::add_symbol_to_coefficients(
                    symbol,
                    &BigInt::from(1),
                    &mut coefficients,
                    field,
                );
                Linear { coefficients }
            }
            (Number { value }, Linear { coefficients })
            | (Linear { coefficients }, Number { value }) => {
                let mut n_coefficients = coefficients.clone();
                PureArithmeticExpression::add_constant_to_coefficients(
                    value,
                    &mut n_coefficients,
                    field,
                );
                Linear { coefficients: n_coefficients }
            }
            (Number { value }, Quadratic { a, b, c })
            | (Quadratic { a, b, c }, Number { value }) => {
                let mut n_c = c.clone();
                PureArithmeticExpression::add_constant_to_coefficients(value, &mut n_c, field);
                Quadratic { a: a.clone(), b: b.clone(), c: n_c }
            }
            (Signal { symbol: symbol_0 }, Signal { symbol: symbol_1 }) => {
                let mut coefficients = HashMap::new();
                PureArithmeticExpression::initialize_hashmap_for_expression(&mut coefficients);
                PureArithmeticExpression::add_symbol_to_coefficients(
                    symbol_0,
                    &BigInt::from(1),
                    &mut coefficients,
                    field,
                );
                PureArithmeticExpression::add_symbol_to_coefficients(
                    symbol_1,
                    &BigInt::from(1),
                    &mut coefficients,
                    field,
                );
                Linear { coefficients }
            }
            (Signal { symbol }, Linear { coefficients })
            | (Linear { coefficients }, Signal { symbol }) => {
                let mut n_coefficients = coefficients.clone();
                PureArithmeticExpression::add_symbol_to_coefficients(
                    symbol,
                    &BigInt::from(1),
                    &mut n_coefficients,
                    field,
                );
                Linear { coefficients: n_coefficients }
            }
            (Signal { symbol }, Quadratic { a, b, c })
            | (Quadratic { a, b, c }, Signal { symbol }) => {
                let mut coefficients = c.clone();
                PureArithmeticExpression::add_symbol_to_coefficients(
                    symbol,
                    &BigInt::from(1),
                    &mut coefficients,
                    field,
                );
                Quadratic { a: a.clone(), b: b.clone(), c: coefficients }
            }
            (Linear { coefficients: coefficients_0 }, Linear { coefficients: coefficients_1 }) => {
                let mut n_coefficients = coefficients_1.clone();
                PureArithmeticExpression::add_coefficients_to_coefficients(
                    coefficients_0,
                    &mut n_coefficients,
                    field,
                );
                Linear { coefficients: n_coefficients }
            }
            (Linear { coefficients }, Quadratic { a, b, c })
            | (Quadratic { a, b, c }, Linear { coefficients }) => {
                let mut coefficients_1 = c.clone();
                PureArithmeticExpression::add_coefficients_to_coefficients(
                    coefficients,
                    &mut coefficients_1,
                    field,
                );
                Quadratic { a: a.clone(), b: b.clone(), c: coefficients_1 }
            }
        }
    }

    pub fn mul(
        left: &PureArithmeticExpression<C>,
        right: &PureArithmeticExpression<C>,
        field: &BigInt,
    ) -> PureArithmeticExpression<C> {
        use PureArithmeticExpression::*;
        match (left, right) {
            (NonQuadratic, _)
            | (_, NonQuadratic)
            | (Quadratic { .. }, Quadratic { .. })
            | (Quadratic { .. }, Linear { .. })
            | (Linear { .. }, Quadratic { .. })
            | (Quadratic { .. }, Signal { .. })
            | (Signal { .. }, Quadratic { .. }) => NonQuadratic,
            (Number { value: value_0 }, Number { value: value_1 }) => {
                Number { value: modular_arithmetic::mul(value_0, value_1, field) }
            }
            (Number { value }, Signal { symbol }) | (Signal { symbol }, Number { value }) => {
                let mut coefficients = HashMap::new();
                PureArithmeticExpression::initialize_hashmap_for_expression(&mut coefficients);
                PureArithmeticExpression::add_symbol_to_coefficients(
                    symbol,
                    value,
                    &mut coefficients,
                    field,
                );
                Linear { coefficients }
            }
            (Number { value }, Linear { coefficients })
            | (Linear { coefficients }, Number { value }) => {
                let mut n_coefficients = coefficients.clone();
                PureArithmeticExpression::multiply_coefficients_by_constant(
                    value,
                    &mut n_coefficients,
                    field,
                );
                Linear { coefficients: n_coefficients }
            }
            (Number { value }, Quadratic { a, b, c })
            | (Quadratic { a, b, c }, Number { value }) => {
                let mut n_a = a.clone();
                let n_b = b.clone();
                let mut n_c = c.clone();
                PureArithmeticExpression::multiply_coefficients_by_constant(value, &mut n_a, field);
                PureArithmeticExpression::multiply_coefficients_by_constant(value, &mut n_c, field);
                Quadratic { a: n_a, b: n_b, c: n_c }
            }
            (Signal { symbol: symbol_0 }, Signal { symbol: symbol_1 }) => {
                let mut a = HashMap::new();
                let mut b = HashMap::new();
                let mut c = HashMap::new();
                PureArithmeticExpression::initialize_hashmap_for_expression(&mut a);
                PureArithmeticExpression::initialize_hashmap_for_expression(&mut b);
                PureArithmeticExpression::initialize_hashmap_for_expression(&mut c);
                PureArithmeticExpression::add_symbol_to_coefficients(
                    symbol_0,
                    &BigInt::from(1),
                    &mut a,
                    field,
                );
                PureArithmeticExpression::add_symbol_to_coefficients(
                    symbol_1,
                    &BigInt::from(1),
                    &mut b,
                    field,
                );
                Quadratic { a, b, c }
            }
            (Signal { symbol }, Linear { coefficients })
            | (Linear { coefficients }, Signal { symbol }) => {
                let a = coefficients.clone();
                let mut b = HashMap::new();
                let mut c = HashMap::new();
                PureArithmeticExpression::initialize_hashmap_for_expression(&mut b);
                PureArithmeticExpression::initialize_hashmap_for_expression(&mut c);
                PureArithmeticExpression::add_symbol_to_coefficients(
                    symbol,
                    &BigInt::from(1),
                    &mut b,
                    field,
                );
                Quadratic { a, b, c }
            }
            (Linear { coefficients: coefficients_0 }, Linear { coefficients: coefficients_1 }) => {
                let a = coefficients_0.clone();
                let b = coefficients_1.clone();
                let mut c = HashMap::new();
                PureArithmeticExpression::initialize_hashmap_for_expression(&mut c);
                Quadratic { a, b, c }
            }
        }
    }
    pub fn sub(
        left: &PureArithmeticExpression<C>,
        right: &PureArithmeticExpression<C>,
        field: &BigInt,
    ) -> PureArithmeticExpression<C> {
        use PureArithmeticExpression::*;
        let minus_one = Number { value: BigInt::from(-1) };
        let step_one = PureArithmeticExpression::mul(&minus_one, right, field);
        PureArithmeticExpression::add(left, &step_one, field)
    }

    pub fn div(
        left: &PureArithmeticExpression<C>,
        right: &PureArithmeticExpression<C>,
        field: &BigInt,
    ) -> Result<PureArithmeticExpression<C>, ArithmeticError> {
        use PureArithmeticExpression::*;
        match (left, right) {
            (Number { value: value_0 }, Number { value: value_1 }) => {
                let value = modular_arithmetic::div(value_0, value_1, field)?;
                Result::Ok(Number { value })
            }
            (Signal { symbol }, Number { value }) => {
                let mut coefficients = HashMap::new();
                PureArithmeticExpression::initialize_hashmap_for_expression(&mut coefficients);
                PureArithmeticExpression::add_symbol_to_coefficients(
                    symbol,
                    &BigInt::from(1),
                    &mut coefficients,
                    field,
                );
                PureArithmeticExpression::divide_coefficients_by_constant(
                    value,
                    &mut coefficients,
                    field,
                )?;
                Result::Ok(Linear { coefficients })
            }
            (Linear { coefficients }, Number { value }) => {
                let mut coefficients = coefficients.clone();
                PureArithmeticExpression::divide_coefficients_by_constant(
                    value,
                    &mut coefficients,
                    field,
                )?;
                Result::Ok(Linear { coefficients })
            }
            (Quadratic { a, b, c }, Number { value }) => {
                let mut a = a.clone();
                let b = b.clone();
                let mut c = c.clone();
                PureArithmeticExpression::divide_coefficients_by_constant(value, &mut a, field)?;
                PureArithmeticExpression::divide_coefficients_by_constant(value, &mut c, field)?;
                Result::Ok(Quadratic { a, b, c })
            }
            _ => Result::Ok(NonQuadratic),
        }
    }
    pub fn idiv(
        left: &PureArithmeticExpression<C>,
        right: &PureArithmeticExpression<C>,
        field: &BigInt,
    ) -> Result<PureArithmeticExpression<C>, ArithmeticError> {
        use PureArithmeticExpression::*;
        match (left, right) {
            (Number { value: value_0 }, Number { value: value_1 }) => {
                let value = modular_arithmetic::idiv(value_0, value_1, field)?;
                Result::Ok(Number { value })
            }
            _ => Result::Ok(NonQuadratic),
        }
    }
    pub fn mod_op(
        left: &PureArithmeticExpression<C>,
        right: &PureArithmeticExpression<C>,
        field: &BigInt,
    ) -> Result<PureArithmeticExpression<C>, ArithmeticError> {
        use PureArithmeticExpression::*;
        if let (Number { value: value_0 }, Number { value: value_1 }) = (left, right) {
            let value = modular_arithmetic::mod_op(value_0, value_1, field)?;
            Result::Ok(Number { value })
        } else {
            Result::Ok(NonQuadratic)
        }
    }
    pub fn pow(
        left: &PureArithmeticExpression<C>,
        right: &PureArithmeticExpression<C>,
        field: &BigInt,
    ) -> PureArithmeticExpression<C> {
        use PureArithmeticExpression::*;
        match (left, right) {
            (Number { value: value_0 }, Number { value: value_1 }) => {
                let value = modular_arithmetic::pow(value_0, value_1, field);
                Number { value }
            }
            (Signal { symbol }, Number { value }) if *value == BigInt::from(2) => {      
                let left = Signal { symbol: symbol.clone() };
                let right = Signal { symbol: symbol.clone() };
                PureArithmeticExpression::mul(&left, &right, field)
            }
            (Linear { coefficients }, Number {value}) if *value == BigInt::from(2) => {
                let left = Linear { coefficients: coefficients.clone() };
                let right = Linear { coefficients: coefficients.clone() };
                PureArithmeticExpression::mul(&left, &right, field)
            }
            _ => NonQuadratic,
        }
    }
    pub fn prefix_sub(elem: &PureArithmeticExpression<C>, field: &BigInt) -> PureArithmeticExpression<C> {
        use PureArithmeticExpression::*;
        let minus_one = Number { value: BigInt::from(-1) };
        PureArithmeticExpression::mul(elem, &minus_one, field)
    }

    // Bit operations
    pub fn complement(
        elem: &PureArithmeticExpression<C>,
        field: &BigInt,
    ) -> PureArithmeticExpression<C> {
        use PureArithmeticExpression::*;
        if let Number { value } = elem {
            Number { value: modular_arithmetic::complement(value, field) }
        } else {
            NonQuadratic
        }
    }
    pub fn shift_l(
        left: &PureArithmeticExpression<C>,
        right: &PureArithmeticExpression<C>,
        field: &BigInt,
    ) -> Result<PureArithmeticExpression<C>, ArithmeticError> {
        use PureArithmeticExpression::*;
        if let (Number { value: value_0 }, Number { value: value_1 }) = (left, right) {
            let shifted_elem = modular_arithmetic::shift_l(value_0, value_1, field)?;
            Result::Ok(Number { value: shifted_elem })
        } else {
            Result::Ok(NonQuadratic)
        }
    }
    pub fn shift_r(
        left: &PureArithmeticExpression<C>,
        right: &PureArithmeticExpression<C>,
        field: &BigInt,
    ) -> Result<PureArithmeticExpression<C>, ArithmeticError> {
        use PureArithmeticExpression::*;
        if let (Number { value: value_0 }, Number { value: value_1 }) = (left, right) {
            let shifted_elem = modular_arithmetic::shift_r(value_0, value_1, field)?;
            Result::Ok(Number { value: shifted_elem })
        } else {
            Result::Ok(NonQuadratic)
        }
    }
    pub fn bit_or(
        left: &PureArithmeticExpression<C>,
        right: &PureArithmeticExpression<C>,
        field: &BigInt,
    ) -> PureArithmeticExpression<C> {
        use PureArithmeticExpression::*;
        if let (Number { value: value_0 }, Number { value: value_1 }) = (left, right) {
            let value = modular_arithmetic::bit_or(value_0, value_1, field);
            Number { value }
        } else {
            NonQuadratic
        }
    }
    pub fn bit_and(
        left: &PureArithmeticExpression<C>,
        right: &PureArithmeticExpression<C>,
        field: &BigInt,
    ) -> PureArithmeticExpression<C> {
        use PureArithmeticExpression::*;
        if let (Number { value: value_0 }, Number { value: value_1 }) = (left, right) {
            let value = modular_arithmetic::bit_and(value_0, value_1, field);
            Number { value }
        } else {
            NonQuadratic
        }
    }
    pub fn bit_xor(
        left: &PureArithmeticExpression<C>,
        right: &PureArithmeticExpression<C>,
        field: &BigInt,
    ) -> PureArithmeticExpression<C> {
        use PureArithmeticExpression::*;
        if let (Number { value: value_0 }, Number { value: value_1 }) = (left, right) {
            let value = modular_arithmetic::bit_xor(value_0, value_1, field);
            Number { value }
        } else {
            NonQuadratic
        }
    }

    // Boolean operations
    pub fn get_boolean_equivalence(elem: &PureArithmeticExpression<C>, field: &BigInt) -> Option<bool> {
        use PureArithmeticExpression::*;
        if let Number { value } = elem {
            Option::Some(modular_arithmetic::as_bool(value, field))
        } else {
            Option::None
        }
    }
    pub fn not(elem: &PureArithmeticExpression<C>, field: &BigInt) -> PureArithmeticExpression<C> {
        use PureArithmeticExpression::*;
        if let Number { value } = elem {
            let value = modular_arithmetic::not(value, field);
            Number { value }
        } else {
            NonQuadratic
        }
    }
    pub fn bool_or(
        left: &PureArithmeticExpression<C>,
        right: &PureArithmeticExpression<C>,
        field: &BigInt,
    ) -> PureArithmeticExpression<C> {
        use PureArithmeticExpression::*;
        if let (Number { value: value_0 }, Number { value: value_1 }) = (left, right) {
            let value = modular_arithmetic::bool_or(value_0, value_1, field);
            Number { value }
        } else {
            NonQuadratic
        }
    }
    pub fn bool_and(
        left: &PureArithmeticExpression<C>,
        right: &PureArithmeticExpression<C>,
        field: &BigInt,
    ) -> PureArithmeticExpression<C> {
        use PureArithmeticExpression::*;
        if let (Number { value: value_0 }, Number { value: value_1 }) = (left, right) {
            let value = modular_arithmetic::bool_and(value_0, value_1, field);
            Number { value }
        } else {
            NonQuadratic
        }
    }
    pub fn eq(
        left: &PureArithmeticExpression<C>,
        right: &PureArithmeticExpression<C>,
        field: &BigInt,
    ) -> PureArithmeticExpression<C> {
        use PureArithmeticExpression::*;
        if let (Number { value: value_0 }, Number { value: value_1 }) = (left, right) {
            let value = modular_arithmetic::eq(value_0, value_1, field);
            Number { value }
        } else {
            NonQuadratic
        }
    }
    pub fn not_eq(
        left: &PureArithmeticExpression<C>,
        right: &PureArithmeticExpression<C>,
        field: &BigInt,
    ) -> PureArithmeticExpression<C> {
        use PureArithmeticExpression::*;
        if let (Number { value: value_0 }, Number { value: value_1 }) = (left, right) {
            let value = modular_arithmetic::not_eq(value_0, value_1, field);
            Number { value }
        } else {
            NonQuadratic
        }
    }
    pub fn lesser(
        left: &PureArithmeticExpression<C>,
        right: &PureArithmeticExpression<C>,
        field: &BigInt,
    ) -> PureArithmeticExpression<C> {
        use PureArithmeticExpression::*;
        if let (Number { value: value_0 }, Number { value: value_1 }) = (left, right) {
            let value = modular_arithmetic::lesser(value_0, value_1, field);
            Number { value }
        } else {
            NonQuadratic
        }
    }
    pub fn lesser_eq(
        left: &PureArithmeticExpression<C>,
        right: &PureArithmeticExpression<C>,
        field: &BigInt,
    ) -> PureArithmeticExpression<C> {
        use PureArithmeticExpression::*;
        if let (Number { value: value_0 }, Number { value: value_1 }) = (left, right) {
            let value = modular_arithmetic::lesser_eq(value_0, value_1, field);
            Number { value }
        } else {
            NonQuadratic
        }
    }
    pub fn greater(
        left: &PureArithmeticExpression<C>,
        right: &PureArithmeticExpression<C>,
        field: &BigInt,
    ) -> PureArithmeticExpression<C> {
        use PureArithmeticExpression::*;
        if let (Number { value: value_0 }, Number { value: value_1 }) = (left, right) {
            let value = modular_arithmetic::greater(value_0, value_1, field);
            Number { value }
        } else {
            NonQuadratic
        }
    }
    pub fn greater_eq(
        left: &PureArithmeticExpression<C>,
        right: &PureArithmeticExpression<C>,
        field: &BigInt,
    ) -> PureArithmeticExpression<C> {
        use PureArithmeticExpression::*;
        if let (Number { value: value_0 }, Number { value: value_1 }) = (left, right) {
            let value = modular_arithmetic::greater_eq(value_0, value_1, field);
            Number { value }
        } else {
            NonQuadratic
        }
    }

    // Utils
    pub fn apply_substitutions(
        expr: &mut PureArithmeticExpression<C>,
        substitution: &Substitution<C>,
        field: &BigInt,
    ) {
        use PureArithmeticExpression::*;
        match expr {
            Linear { coefficients } => {
               raw_substitution(coefficients, substitution, field);
               *coefficients = remove_zero_value_coefficients(std::mem::take(coefficients));
            }
            Signal { symbol } if *symbol == substitution.from => {
                *expr = Linear { coefficients: substitution.to.clone() };
            }
            Quadratic { a, b, c } => {
                raw_substitution(a, substitution, field);
                *a = remove_zero_value_coefficients(std::mem::take(a));
                raw_substitution(b, substitution, field);
                *b = remove_zero_value_coefficients(std::mem::take(b));
                raw_substitution(c, substitution, field);
                *c = remove_zero_value_coefficients(std::mem::take(c));
            }
            _ => {}
        }
    }
    pub fn get_usize(expr: &PureArithmeticExpression<C>) -> Option<usize> {
        use PureArithmeticExpression::*;
        if let Number { value, .. } = expr {
            value.to_usize()
        } else {
            Option::None
        }
    }
    pub fn is_number(&self) -> bool {
        matches!(self, PureArithmeticExpression::Number { .. })
    }
    pub fn is_nonquadratic(&self) -> bool {
        matches!(self, PureArithmeticExpression::NonQuadratic { .. })
    }
    pub fn is_quadratic(&self) -> bool {
        matches!(self, PureArithmeticExpression::Quadratic { .. })
    }
    pub fn is_linear(&self) -> bool {
        matches!(self, PureArithmeticExpression::Linear { .. })
    }

    pub fn hashmap_into_arith(mut map: HashMap<C, BigInt>) -> PureArithmeticExpression<C> {
        let c: C = PureArithmeticExpression::constant_coefficient();
        let expr = if HashMap::len(&map) == 1 && HashMap::contains_key(&map, &c) {
            let value = HashMap::remove(&mut map, &c).unwrap();
            PureArithmeticExpression::Number { value }
        } else if HashMap::len(&map) == 1 {
            let mut values: Vec<_> = map.values().cloned().collect();
            let mut symbols: Vec<_> = map.keys().cloned().collect();
            let symbol = symbols.pop().unwrap();
            let value = values.pop().unwrap();
            if value == BigInt::from(1) {
                PureArithmeticExpression::Signal { symbol }
            } else {
                PureArithmeticExpression::initialize_hashmap_for_expression(&mut map);
                PureArithmeticExpression::Linear { coefficients: map }
            }
        } else {
            PureArithmeticExpression::initialize_hashmap_for_expression(&mut map);
            PureArithmeticExpression::Linear { coefficients: map }
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
    pub fn new(from: C, to: PureArithmeticExpression<C>) -> Option<Substitution<C>> {
        use PureArithmeticExpression::*;
        match to {
            Number { value, .. } => {
                let mut to = HashMap::new();
                to.insert(PureArithmeticExpression::constant_coefficient(), value);
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
        PureArithmeticExpression::constant_coefficient()
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
        PureArithmeticExpression::initialize_hashmap_for_expression(&mut coefficients);
        coefficients.insert(symbol, BigInt::from(-1 % field));
        let arith = PureArithmeticExpression::Linear { coefficients };
        PureArithmeticExpression::transform_expression_to_constraint_form(arith, field).unwrap()
    }

    pub fn decompose(substitution: Substitution<C>) -> (C, PureArithmeticExpression<C>) {
        let c: C = PureArithmeticExpression::constant_coefficient();
        let mut to = substitution.to;
        let right = if HashMap::len(&to) == 1 && HashMap::contains_key(&to, &c) {
            let value = HashMap::remove(&mut to, &c).unwrap();
            PureArithmeticExpression::Number { value }
        } else if HashMap::len(&to) == 1 {
            let mut values: Vec<_> = to.values().cloned().collect();
            let mut symbols: Vec<_> = to.keys().cloned().collect();
            let symbol = symbols.pop().unwrap();
            let value = values.pop().unwrap();
            if value == BigInt::from(1) {
                PureArithmeticExpression::Signal { symbol }
            } else {
                PureArithmeticExpression::initialize_hashmap_for_expression(&mut to);
                PureArithmeticExpression::Linear { coefficients: to }
            }
        } else {
            PureArithmeticExpression::initialize_hashmap_for_expression(&mut to);
            PureArithmeticExpression::Linear { coefficients: to }
        };
        (substitution.from, right)
    }

    pub fn map_into_arith_expr(
        substitution: Substitution<C>,
        field: &BigInt,
    ) -> PureArithmeticExpression<C> {
        let (left, right) = Substitution::decompose(substitution);
        let left = PureArithmeticExpression::Signal { symbol: left };
        PureArithmeticExpression::sub(&right, &left, field)
    }

    pub fn from(&self) -> &C {
        &self.from
    }

    pub fn to(&self) -> &HashMap<C, BigInt> {
        &self.to
    }

    pub fn take_cloned_signals(&self) -> HashSet<C> {
        let cq: C = PureArithmeticExpression::constant_coefficient();
        let mut signals = HashSet::new();
        for s in self.to.keys() {
            if cq != *s {
                signals.insert(s.clone());
            }
        }
        signals
    }

    pub fn take_signals(&self) -> HashSet<&C> {
        let cq: C = PureArithmeticExpression::constant_coefficient();
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
        let cq: C = PureArithmeticExpression::constant_coefficient();
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
        PureArithmeticExpression::constant_coefficient()
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
        PureArithmeticExpression::initialize_hashmap_for_expression(&mut symbols);
        let arithmetic_result = PureArithmeticExpression::divide_coefficients_by_constant(
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
        PureArithmeticExpression::initialize_hashmap_for_expression(&mut symbols);
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

    pub fn into_arithmetic_expressions(self) -> (PureArithmeticExpression<C>, PureArithmeticExpression<C>, PureArithmeticExpression<C>) {
        (
            PureArithmeticExpression::Linear { coefficients: self.a },
            PureArithmeticExpression::Linear { coefficients: self.b },
            PureArithmeticExpression::Linear { coefficients: self.c }
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
    let constant_coefficient: C = PureArithmeticExpression::constant_coefficient();
    let mut coefficients_as_correspondence = HashMap::new();
    for (key, value) in symbols {
        let id = if key.eq(&constant_coefficient) {
            PureArithmeticExpression::constant_coefficient()
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
    PureArithmeticExpression::initialize_hashmap_for_expression(change);
    if let Option::Some(val) = change.remove(&substitution.from) {
        let mut coefficients = substitution.to.clone();
        PureArithmeticExpression::initialize_hashmap_for_expression(&mut coefficients);
        PureArithmeticExpression::multiply_coefficients_by_constant(&val, &mut coefficients, field);
        PureArithmeticExpression::add_coefficients_to_coefficients(&coefficients, change, field);
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
    let cq: C = PureArithmeticExpression::constant_coefficient();
    PureArithmeticExpression::initialize_hashmap_for_expression(c);
    PureArithmeticExpression::initialize_hashmap_for_expression(b);
    let constant = HashMap::remove(a, &cq).unwrap();
    PureArithmeticExpression::multiply_coefficients_by_constant(&constant, b, field);
    PureArithmeticExpression::multiply_coefficients_by_constant(&BigInt::from(-1), b, field);
    PureArithmeticExpression::add_coefficients_to_coefficients(b, c, field);
    *c = remove_zero_value_coefficients(std::mem::take(c));
    HashMap::clear(a);
    HashMap::clear(b);
}

fn signal_equals_signal<C>(a: &RawExpr<C>, b: &RawExpr<C>, c: &RawExpr<C>, field: &BigInt) -> bool
where
    C: Default + Clone + Display + Hash + Eq + Ord,
{
    let cq: C = PureArithmeticExpression::constant_coefficient();
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
    let cq: C = PureArithmeticExpression::constant_coefficient();
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
    let cq: C = PureArithmeticExpression::constant_coefficient();
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
    use crate::algebra::{PureArithmeticExpression, Constraint, HintExpression, Substitution};
    use crate::modular_arithmetic;
    use num_bigint::BigInt;
    use std::collections::HashMap;
    const FIELD: &str = "257";
    type C = Constraint<usize>;
    type S = Substitution<usize>;
    type A = PureArithmeticExpression<usize>;

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
        let to = A::Linear { coefficients: to_raw };
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
