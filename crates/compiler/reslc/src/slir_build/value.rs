#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Value {
    Value(slir::cfg::Value),
    FnAddr(slir::Function),
    Void,
}

impl Value {
    pub fn expect_value(&self) -> slir::cfg::Value {
        if let Value::Value(v) = self {
            *v
        } else {
            panic!("expected value")
        }
    }

    pub fn expect_fn_addr(&self) -> slir::Function {
        if let Value::FnAddr(f) = self {
            *f
        } else {
            panic!("expected function address")
        }
    }
}

impl From<slir::cfg::Value> for Value {
    fn from(value: slir::cfg::Value) -> Self {
        Value::Value(value)
    }
}

impl From<slir::cfg::LocalValue> for Value {
    fn from(value: slir::cfg::LocalValue) -> Self {
        Value::Value(slir::cfg::Value::Local(value))
    }
}

impl From<slir::cfg::InlineConst> for Value {
    fn from(value: slir::cfg::InlineConst) -> Self {
        Value::Value(slir::cfg::Value::InlineConst(value))
    }
}

impl From<slir::Function> for Value {
    fn from(value: slir::Function) -> Self {
        Value::FnAddr(value)
    }
}
