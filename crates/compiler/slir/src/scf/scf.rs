use std::hash::{Hash, Hasher};
use std::ops::{Deref, Index};

use delegate::delegate;
use indexmap::{IndexMap, IndexSet};
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use slotmap::SlotMap;

use crate::builtin_function::BuiltinFunction;
use crate::ty::{Type, TypeKind, TypeRegistry, TY_BOOL, TY_F32, TY_I32, TY_U32};
use crate::{
    ty, BinaryOperator, Constant, ConstantRegistry, Function, Module, StorageBinding,
    StorageBindingRegistry, UnaryOperator, UniformBinding, UniformBindingRegistry,
    WorkgroupBinding, WorkgroupBindingRegistry,
};

slotmap::new_key_type! {
    pub struct Statement;
    pub struct Block;
    pub struct LocalBinding;
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct LocalBindingData {
    ty: Type,
    kind: LocalBindingKind,
}

impl LocalBindingData {
    pub fn ty(&self) -> Type {
        self.ty
    }

    pub fn kind(&self) -> &LocalBindingKind {
        &self.kind
    }
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub enum LocalBindingKind {
    Argument(u32),
    ExprBinding(Statement),
    ControlFlowVar {
        statement: Statement,
        out_var: usize,
    },
}

impl LocalBindingKind {
    pub fn expect_argument(&self) -> u32 {
        if let LocalBindingKind::Argument(index) = self {
            *index
        } else {
            panic!("expected an argument");
        }
    }

    pub fn expect_expr_binding(&self) -> Statement {
        if let LocalBindingKind::ExprBinding(binding) = self {
            *binding
        } else {
            panic!("expected an expression-binding statement");
        }
    }

    pub fn expect_control_flow_var(&self) -> (Statement, usize) {
        if let LocalBindingKind::ControlFlowVar { statement, out_var } = self {
            (*statement, *out_var)
        } else {
            panic!("expected a control-flow variable");
        }
    }
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct Expression {
    ty: Type,
    kind: ExpressionKind,
}

impl Expression {
    pub fn ty(&self) -> Type {
        self.ty
    }

    pub fn kind(&self) -> &ExpressionKind {
        &self.kind
    }
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct OpUnary {
    operator: UnaryOperator,
    operand: LocalBinding,
}

impl OpUnary {
    pub fn operator(&self) -> UnaryOperator {
        self.operator
    }

    pub fn operand(&self) -> LocalBinding {
        self.operand
    }
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct OpBinary {
    operator: BinaryOperator,
    lhs: LocalBinding,
    rhs: LocalBinding,
}

impl OpBinary {
    pub fn operator(&self) -> BinaryOperator {
        self.operator
    }

    pub fn lhs(&self) -> LocalBinding {
        self.lhs
    }

    pub fn rhs(&self) -> LocalBinding {
        self.rhs
    }
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct OpVector {
    vector_ty: ty::Vector,
    elements: Vec<LocalBinding>,
}

impl OpVector {
    pub fn vector_ty(&self) -> &ty::Vector {
        &self.vector_ty
    }

    pub fn elements(&self) -> &[LocalBinding] {
        &self.elements
    }
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct OpMatrix {
    matrix_ty: ty::Matrix,
    columns: Vec<LocalBinding>,
}

impl OpMatrix {
    pub fn matrix_ty(&self) -> &ty::Matrix {
        &self.matrix_ty
    }

    pub fn columns(&self) -> &[LocalBinding] {
        &self.columns
    }
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct OpPtrElementPtr {
    pointer: LocalBinding,
    indices: Vec<LocalBinding>,
}

impl OpPtrElementPtr {
    pub fn pointer(&self) -> LocalBinding {
        self.pointer
    }

    pub fn indices(&self) -> &[LocalBinding] {
        &self.indices
    }
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct OpExtractElement {
    value: LocalBinding,
    indices: Vec<LocalBinding>,
}

impl OpExtractElement {
    pub fn value(&self) -> LocalBinding {
        self.value
    }

    pub fn indices(&self) -> &[LocalBinding] {
        &self.indices
    }
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct OpCallBuiltin {
    callee: BuiltinFunction,
    arguments: Vec<LocalBinding>,
}

impl OpCallBuiltin {
    pub fn callee(&self) -> &BuiltinFunction {
        &self.callee
    }

    pub fn arguments(&self) -> &[LocalBinding] {
        &self.arguments
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub enum GlobalPtr {
    Uniform(UniformBinding),
    Storage(StorageBinding),
    Workgroup(WorkgroupBinding),
    Constant(Constant),
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub enum ExpressionKind {
    FallbackValue,
    ConstU32(u32),
    ConstI32(i32),
    ConstF32(f32),
    ConstBool(bool),
    GlobalPtr(GlobalPtr),
    OpAlloca(Type),
    OpUnary(OpUnary),
    OpBinary(OpBinary),
    OpVector(OpVector),
    OpMatrix(OpMatrix),
    OpPtrElementPtr(OpPtrElementPtr),
    OpExtractElement(OpExtractElement),
    OpLoad(LocalBinding),
    OpCallBuiltin(OpCallBuiltin),
}

impl ExpressionKind {
    pub fn expect_fallback_value(&self) {
        if !matches!(self, ExpressionKind::FallbackValue) {
            panic!("expected a fallback-value expression");
        }
    }

    pub fn expect_const_u32(&self) -> u32 {
        if let ExpressionKind::ConstU32(value) = self {
            *value
        } else {
            panic!("expected a constant u32 expression");
        }
    }

    pub fn expect_const_i32(&self) -> i32 {
        if let ExpressionKind::ConstI32(value) = self {
            *value
        } else {
            panic!("expected a constant i32 expression");
        }
    }

    pub fn expect_const_f32(&self) -> f32 {
        if let ExpressionKind::ConstF32(value) = self {
            *value
        } else {
            panic!("expected a constant f32 expression");
        }
    }

    pub fn expect_const_bool(&self) -> bool {
        if let ExpressionKind::ConstBool(value) = self {
            *value
        } else {
            panic!("expected a constant bool expression");
        }
    }

    pub fn expect_global_ptr(&self) -> &GlobalPtr {
        if let ExpressionKind::GlobalPtr(expr) = self {
            expr
        } else {
            panic!("expected a global-pointer expression");
        }
    }

    pub fn expect_op_unary(&self) -> &OpUnary {
        if let ExpressionKind::OpUnary(op) = self {
            op
        } else {
            panic!("expected an unary operation expression");
        }
    }

    pub fn expect_op_binary(&self) -> &OpBinary {
        if let ExpressionKind::OpBinary(op) = self {
            op
        } else {
            panic!("expected a binary operation expression");
        }
    }

    pub fn expect_op_vector(&self) -> &OpVector {
        if let ExpressionKind::OpVector(op) = self {
            op
        } else {
            panic!("expected a vector operation expression");
        }
    }

    pub fn expect_op_matrix(&self) -> &OpMatrix {
        if let ExpressionKind::OpMatrix(op) = self {
            op
        } else {
            panic!("expected a matrix operation expression");
        }
    }

    pub fn expect_op_ptr_element_ptr(&self) -> &OpPtrElementPtr {
        if let ExpressionKind::OpPtrElementPtr(op) = self {
            op
        } else {
            panic!("expected a pointer-element-pointer operation expression");
        }
    }

    pub fn expect_op_extract_element(&self) -> &OpExtractElement {
        if let ExpressionKind::OpExtractElement(op) = self {
            op
        } else {
            panic!("expected an extract-element operation expression");
        }
    }

    pub fn expect_op_load(&self) -> LocalBinding {
        if let ExpressionKind::OpLoad(op) = self {
            *op
        } else {
            panic!("expected a load operation expression");
        }
    }

    pub fn expect_op_call_builtin(&self) -> &OpCallBuiltin {
        if let ExpressionKind::OpCallBuiltin(op) = self {
            op
        } else {
            panic!("expected a call-builtin operation expression");
        }
    }
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct BlockData {
    statements: IndexSet<Statement>,
    control_flow_vars: IndexMap<LocalBinding, LocalBinding>,
}

impl BlockData {
    fn new() -> Self {
        Self {
            statements: Default::default(),
            control_flow_vars: Default::default(),
        }
    }

    pub fn statements(&self) -> &IndexSet<Statement> {
        &self.statements
    }

    pub fn control_flow_var(&self, binding: LocalBinding) -> LocalBinding {
        *self
            .control_flow_vars
            .get(&binding)
            .expect("no control-flow variable associated with the local binding")
    }

    pub fn control_flow_var_iter(
        &self,
    ) -> impl Iterator<Item = (LocalBinding, LocalBinding)> + use<'_> {
        self.control_flow_vars
            .iter()
            .map(|(binding, expr)| (*binding, *expr))
    }

    pub fn is_empty(&self) -> bool {
        self.statements.is_empty() && self.control_flow_vars.is_empty()
    }

    fn add_statement(&mut self, position: BlockPosition, statement: Statement) {
        match position {
            BlockPosition::Append => self.append_statement(statement),
            BlockPosition::Prepend => self.prepend_statement(statement),
            BlockPosition::InsertBefore(before) => self.insert_before(before, statement),
            BlockPosition::InsertAfter(after) => self.insert_after(after, statement),
        }
    }

    fn append_statement(&mut self, statement: Statement) {
        self.statements.insert(statement);
    }

    fn prepend_statement(&mut self, statement: Statement) {
        // Note that `IndexSet::insert_before` accepts indices in the `0..=set.len()` range, so
        // this works even if the block is currently empty.
        self.statements.insert_before(0, statement);
    }

    fn insert_before(&mut self, before: Statement, statement: Statement) {
        let Some(index) = self.statements.get_index_of(&before) else {
            panic!("'before' statement not found in the block")
        };

        self.statements.insert_before(index, statement);
    }

    fn insert_after(&mut self, after: Statement, statement: Statement) {
        let Some(index) = self.statements.get_index_of(&after) else {
            panic!("'after' statement not found in the block")
        };

        // Note that `IndexSet::insert_before` accepts indices in the `0..=set.len()` range, so
        // this works even if the `after` statement is the last statement in the block.
        self.statements.insert_before(index + 1, statement);
    }

    fn remove_statement(&mut self, statement: Statement) -> bool {
        self.statements.shift_remove(&statement)
    }

    fn set_control_flow_var(&mut self, index: usize, value: LocalBinding) {
        if let Some(mut entry) = self.control_flow_vars.get_index_entry(index) {
            entry.insert(value);
        } else {
            panic!("no control-flow variable associated with the index");
        }
    }

    fn add_control_flow_var(&mut self, binding: LocalBinding, value: LocalBinding) {
        self.control_flow_vars.insert(binding, value);
    }

    fn remove_control_flow_var(&mut self, binding: LocalBinding) {
        self.control_flow_vars.shift_remove(&binding);
    }
}

#[derive(Clone, Copy, PartialEq, Serialize, Deserialize, Debug)]
pub enum LoopControl {
    Head(LocalBinding),
    Tail(LocalBinding),
    Infinite,
}

#[derive(Clone, Copy, Serialize, Deserialize, Debug)]
pub struct LoopVar {
    binding: LocalBinding,
    initial_value: LocalBinding,
}

impl LoopVar {
    pub fn binding(&self) -> LocalBinding {
        self.binding
    }

    pub fn initial_value(&self) -> LocalBinding {
        self.initial_value
    }
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct Loop {
    loop_block: Block,
    control: LoopControl,
    loop_vars: Vec<LoopVar>,
}

impl Loop {
    pub fn block(&self) -> Block {
        self.loop_block
    }

    pub fn control(&self) -> LoopControl {
        self.control
    }

    pub fn loop_vars(&self) -> &[LoopVar] {
        &self.loop_vars
    }
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct If {
    condition: LocalBinding,
    then_block: Block,
    else_block: Option<Block>,
    out_vars: Vec<LocalBinding>,
}

impl If {
    pub fn condition(&self) -> LocalBinding {
        self.condition
    }

    pub fn then_block(&self) -> Block {
        self.then_block
    }

    pub fn else_block(&self) -> Option<Block> {
        self.else_block
    }

    pub fn out_vars(&self) -> &[LocalBinding] {
        &self.out_vars
    }
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct SwitchCase {
    case: u32,
    block: Block,
}

impl SwitchCase {
    pub fn case(&self) -> u32 {
        self.case
    }

    pub fn block(&self) -> Block {
        self.block
    }
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct Switch {
    on: LocalBinding,
    cases: Vec<SwitchCase>,
    default: Block,
    out_vars: Vec<LocalBinding>,
}

impl Switch {
    pub fn on(&self) -> LocalBinding {
        self.on
    }

    pub fn cases(&self) -> &[SwitchCase] {
        &self.cases
    }

    pub fn default(&self) -> Block {
        self.default
    }

    pub fn out_vars(&self) -> &[LocalBinding] {
        &self.out_vars
    }
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct Return {
    value: Option<LocalBinding>,
}

impl Return {
    pub fn value(&self) -> Option<LocalBinding> {
        self.value
    }
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct ExprBinding {
    binding: LocalBinding,
    expression: Expression,
}

impl ExprBinding {
    pub fn binding(&self) -> LocalBinding {
        self.binding
    }

    pub fn expression(&self) -> &Expression {
        &self.expression
    }
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct Store {
    pointer: LocalBinding,
    value: LocalBinding,
}

impl Store {
    pub fn pointer(&self) -> LocalBinding {
        self.pointer
    }

    pub fn value(&self) -> LocalBinding {
        self.value
    }
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct StatementData {
    block: Block,
    kind: StatementKind,
}

impl StatementData {
    pub fn block(&self) -> Block {
        self.block
    }

    pub fn kind(&self) -> &StatementKind {
        &self.kind
    }

    delegate! {
        to self.kind {
            pub fn expect_if(&self) -> &If;
            pub fn expect_switch(&self) -> &Switch;
            pub fn expect_loop(&self) -> &Loop;
            pub fn expect_return(&self) -> &Return;
            pub fn expect_expr_binding(&self) -> &ExprBinding;
        }
    }
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub enum StatementKind {
    If(If),
    Switch(Switch),
    Loop(Loop),
    Return(Return),
    ExprBinding(ExprBinding),
    Store(Store),
    CallBuiltin(OpCallBuiltin),
}

impl StatementKind {
    pub fn expect_if(&self) -> &If {
        if let StatementKind::If(stmt) = self {
            stmt
        } else {
            panic!("expected an if statement");
        }
    }

    fn expect_if_mut(&mut self) -> &mut If {
        if let StatementKind::If(stmt) = self {
            stmt
        } else {
            panic!("expected an if statement");
        }
    }

    pub fn expect_switch(&self) -> &Switch {
        if let StatementKind::Switch(stmt) = self {
            stmt
        } else {
            panic!("expected a switch statement");
        }
    }

    fn expect_switch_mut(&mut self) -> &mut Switch {
        if let StatementKind::Switch(stmt) = self {
            stmt
        } else {
            panic!("expected a switch statement");
        }
    }

    pub fn expect_loop(&self) -> &Loop {
        if let StatementKind::Loop(stmt) = self {
            stmt
        } else {
            panic!("expected a loop statement");
        }
    }

    fn expect_loop_mut(&mut self) -> &mut Loop {
        if let StatementKind::Loop(stmt) = self {
            stmt
        } else {
            panic!("expected a loop statement");
        }
    }

    pub fn expect_return(&self) -> &Return {
        if let StatementKind::Return(stmt) = self {
            stmt
        } else {
            panic!("expected a return statement");
        }
    }

    pub fn expect_expr_binding(&self) -> &ExprBinding {
        if let StatementKind::ExprBinding(stmt) = self {
            stmt
        } else {
            panic!("expected an expression-binding statement");
        }
    }

    pub fn expect_store(&self) -> &Store {
        if let StatementKind::Store(stmt) = self {
            stmt
        } else {
            panic!("expected a store statement");
        }
    }

    pub fn expect_call_builtin(&self) -> &OpCallBuiltin {
        if let StatementKind::CallBuiltin(stmt) = self {
            stmt
        } else {
            panic!("expected a call-builtin statement");
        }
    }
}

pub enum BlockPosition {
    Append,
    Prepend,
    InsertBefore(Statement),
    InsertAfter(Statement),
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct FunctionBody {
    block: Block,
    argument_bindings: Vec<LocalBinding>,
}

impl FunctionBody {
    pub fn block(&self) -> Block {
        self.block
    }

    pub fn argument_bindings(&self) -> &[LocalBinding] {
        &self.argument_bindings
    }
}

#[derive(Clone, Deserialize, Debug)]
pub struct ScfData {
    statements: SlotMap<Statement, StatementData>,
    blocks: SlotMap<Block, BlockData>,
    function_bodies: FxHashMap<Function, FunctionBody>,
    local_bindings: SlotMap<LocalBinding, LocalBindingData>,
}

#[derive(Serialize, Debug)]
pub struct Scf {
    #[serde(skip_serializing)]
    ty: TypeRegistry,
    statements: SlotMap<Statement, StatementData>,
    blocks: SlotMap<Block, BlockData>,
    function_bodies: FxHashMap<Function, FunctionBody>,
    local_bindings: SlotMap<LocalBinding, LocalBindingData>,
}

impl Scf {
    pub fn new(type_registry: TypeRegistry) -> Self {
        Self {
            ty: type_registry,
            statements: Default::default(),
            blocks: Default::default(),
            function_bodies: Default::default(),
            local_bindings: Default::default(),
        }
    }

    pub fn from_ty_and_data(ty: TypeRegistry, data: ScfData) -> Self {
        let ScfData {
            statements,
            blocks,
            function_bodies,
            local_bindings,
        } = data;

        Scf {
            ty,
            statements,
            blocks,
            function_bodies,
            local_bindings,
        }
    }

    pub fn ty(&self) -> &TypeRegistry {
        &self.ty
    }

    pub fn register_function(&mut self, module: &Module, function: Function) -> &FunctionBody {
        let sig = &module.fn_sigs[function];
        let argument_bindings = sig
            .args
            .iter()
            .enumerate()
            .map(|(i, arg)| {
                self.local_bindings.insert(LocalBindingData {
                    ty: arg.ty,
                    kind: LocalBindingKind::Argument(i as u32),
                })
            })
            .collect();
        let block = self.blocks.insert(BlockData::new());

        self.function_bodies
            .entry(function)
            .or_insert(FunctionBody {
                block,
                argument_bindings,
            })
    }

    pub fn get_function_body(&self, function: Function) -> Option<&FunctionBody> {
        self.function_bodies.get(&function)
    }

    pub fn add_bind_fallback_value(
        &mut self,
        block: Block,
        position: BlockPosition,
        ty: Type,
    ) -> (Statement, LocalBinding) {
        let binding = self.local_bindings.insert(LocalBindingData {
            ty,
            // Initialize with a temporary value, remember to adjust after statement initialization.
            kind: LocalBindingKind::ExprBinding(Statement::default()),
        });

        let statement = self.statements.insert(StatementData {
            block,
            kind: StatementKind::ExprBinding(ExprBinding {
                binding,
                expression: Expression {
                    ty,
                    kind: ExpressionKind::FallbackValue,
                },
            }),
        });

        self.blocks[block].add_statement(position, statement);

        // Adjust the temporary value we set above to the actual statement.
        self.local_bindings[binding].kind = LocalBindingKind::ExprBinding(statement);

        (statement, binding)
    }

    pub fn add_bind_const_u32(
        &mut self,
        block: Block,
        position: BlockPosition,
        value: u32,
    ) -> (Statement, LocalBinding) {
        let binding = self.local_bindings.insert(LocalBindingData {
            ty: TY_U32,
            // Initialize with a temporary value, remember to adjust after statement initialization.
            kind: LocalBindingKind::ExprBinding(Statement::default()),
        });

        let statement = self.statements.insert(StatementData {
            block,
            kind: StatementKind::ExprBinding(ExprBinding {
                binding,
                expression: Expression {
                    ty: TY_U32,
                    kind: ExpressionKind::ConstU32(value),
                },
            }),
        });

        self.blocks[block].add_statement(position, statement);

        // Adjust the temporary value we set above to the actual statement.
        self.local_bindings[binding].kind = LocalBindingKind::ExprBinding(statement);

        (statement, binding)
    }

    pub fn add_bind_const_i32(
        &mut self,
        block: Block,
        position: BlockPosition,
        value: i32,
    ) -> (Statement, LocalBinding) {
        let binding = self.local_bindings.insert(LocalBindingData {
            ty: TY_I32,
            // Initialize with a temporary value, remember to adjust after statement initialization.
            kind: LocalBindingKind::ExprBinding(Statement::default()),
        });

        let statement = self.statements.insert(StatementData {
            block,
            kind: StatementKind::ExprBinding(ExprBinding {
                binding,
                expression: Expression {
                    ty: TY_I32,
                    kind: ExpressionKind::ConstI32(value),
                },
            }),
        });

        self.blocks[block].add_statement(position, statement);

        // Adjust the temporary value we set above to the actual statement.
        self.local_bindings[binding].kind = LocalBindingKind::ExprBinding(statement);

        (statement, binding)
    }

    pub fn add_bind_const_f32(
        &mut self,
        block: Block,
        position: BlockPosition,
        value: f32,
    ) -> (Statement, LocalBinding) {
        let binding = self.local_bindings.insert(LocalBindingData {
            ty: TY_F32,
            // Initialize with a temporary value, remember to adjust after statement initialization.
            kind: LocalBindingKind::ExprBinding(Statement::default()),
        });

        let statement = self.statements.insert(StatementData {
            block,
            kind: StatementKind::ExprBinding(ExprBinding {
                binding,
                expression: Expression {
                    ty: TY_F32,
                    kind: ExpressionKind::ConstF32(value),
                },
            }),
        });

        self.blocks[block].add_statement(position, statement);

        // Adjust the temporary value we set above to the actual statement.
        self.local_bindings[binding].kind = LocalBindingKind::ExprBinding(statement);

        (statement, binding)
    }

    pub fn add_bind_const_bool(
        &mut self,
        block: Block,
        position: BlockPosition,
        value: bool,
    ) -> (Statement, LocalBinding) {
        let binding = self.local_bindings.insert(LocalBindingData {
            ty: TY_BOOL,
            // Initialize with a temporary value, remember to adjust after statement initialization.
            kind: LocalBindingKind::ExprBinding(Statement::default()),
        });

        let statement = self.statements.insert(StatementData {
            block,
            kind: StatementKind::ExprBinding(ExprBinding {
                binding,
                expression: Expression {
                    ty: TY_BOOL,
                    kind: ExpressionKind::ConstBool(value),
                },
            }),
        });

        self.blocks[block].add_statement(position, statement);

        // Adjust the temporary value we set above to the actual statement.
        self.local_bindings[binding].kind = LocalBindingKind::ExprBinding(statement);

        (statement, binding)
    }

    pub fn add_bind_uniform_ptr(
        &mut self,
        block: Block,
        position: BlockPosition,
        registry: &UniformBindingRegistry,
        binding: UniformBinding,
    ) -> (Statement, LocalBinding) {
        let ty = registry[binding].ty;
        let ptr_ty = self.ty().register(TypeKind::Ptr(ty));
        let global_ptr = GlobalPtr::Uniform(binding);

        let binding = self.local_bindings.insert(LocalBindingData {
            ty: ptr_ty,
            // Initialize with a temporary value, remember to adjust after statement initialization.
            kind: LocalBindingKind::ExprBinding(Statement::default()),
        });

        let statement = self.statements.insert(StatementData {
            block,
            kind: StatementKind::ExprBinding(ExprBinding {
                binding,
                expression: Expression {
                    ty: ptr_ty,
                    kind: ExpressionKind::GlobalPtr(global_ptr),
                },
            }),
        });

        self.blocks[block].add_statement(position, statement);

        // Adjust the temporary value we set above to the actual statement.
        self.local_bindings[binding].kind = LocalBindingKind::ExprBinding(statement);

        (statement, binding)
    }

    pub fn add_bind_storage_ptr(
        &mut self,
        block: Block,
        position: BlockPosition,
        registry: &StorageBindingRegistry,
        binding: StorageBinding,
    ) -> (Statement, LocalBinding) {
        let ty = registry[binding].ty;
        let ptr_ty = self.ty().register(TypeKind::Ptr(ty));
        let global_ptr = GlobalPtr::Storage(binding);

        let binding = self.local_bindings.insert(LocalBindingData {
            ty: ptr_ty,
            // Initialize with a temporary value, remember to adjust after statement initialization.
            kind: LocalBindingKind::ExprBinding(Statement::default()),
        });

        let statement = self.statements.insert(StatementData {
            block,
            kind: StatementKind::ExprBinding(ExprBinding {
                binding,
                expression: Expression {
                    ty: ptr_ty,
                    kind: ExpressionKind::GlobalPtr(global_ptr),
                },
            }),
        });

        self.blocks[block].add_statement(position, statement);

        // Adjust the temporary value we set above to the actual statement.
        self.local_bindings[binding].kind = LocalBindingKind::ExprBinding(statement);

        (statement, binding)
    }

    pub fn add_bind_workgroup_ptr(
        &mut self,
        block: Block,
        position: BlockPosition,
        registry: &WorkgroupBindingRegistry,
        binding: WorkgroupBinding,
    ) -> (Statement, LocalBinding) {
        let ty = registry[binding].ty;
        let ptr_ty = self.ty().register(TypeKind::Ptr(ty));
        let global_ptr = GlobalPtr::Workgroup(binding);

        let binding = self.local_bindings.insert(LocalBindingData {
            ty: ptr_ty,
            // Initialize with a temporary value, remember to adjust after statement initialization.
            kind: LocalBindingKind::ExprBinding(Statement::default()),
        });

        let statement = self.statements.insert(StatementData {
            block,
            kind: StatementKind::ExprBinding(ExprBinding {
                binding,
                expression: Expression {
                    ty: ptr_ty,
                    kind: ExpressionKind::GlobalPtr(global_ptr),
                },
            }),
        });

        self.blocks[block].add_statement(position, statement);

        // Adjust the temporary value we set above to the actual statement.
        self.local_bindings[binding].kind = LocalBindingKind::ExprBinding(statement);

        (statement, binding)
    }

    pub fn add_bind_constant_ptr(
        &mut self,
        block: Block,
        position: BlockPosition,
        registry: &ConstantRegistry,
        constant: Constant,
    ) -> (Statement, LocalBinding) {
        let ty = registry[constant].ty();
        let ptr_ty = self.ty().register(TypeKind::Ptr(ty));
        let global_ptr = GlobalPtr::Constant(constant);

        let binding = self.local_bindings.insert(LocalBindingData {
            ty: ptr_ty,
            // Initialize with a temporary value, remember to adjust after statement initialization.
            kind: LocalBindingKind::ExprBinding(Statement::default()),
        });

        let statement = self.statements.insert(StatementData {
            block,
            kind: StatementKind::ExprBinding(ExprBinding {
                binding,
                expression: Expression {
                    ty: ptr_ty,
                    kind: ExpressionKind::GlobalPtr(global_ptr),
                },
            }),
        });

        self.blocks[block].add_statement(position, statement);

        // Adjust the temporary value we set above to the actual statement.
        self.local_bindings[binding].kind = LocalBindingKind::ExprBinding(statement);

        (statement, binding)
    }

    pub fn add_bind_op_alloca(
        &mut self,
        block: Block,
        position: BlockPosition,
        ty: Type,
    ) -> (Statement, LocalBinding) {
        let ptr_ty = self.ty().register(TypeKind::Ptr(ty));

        let binding = self.local_bindings.insert(LocalBindingData {
            ty: ptr_ty,
            // Initialize with a temporary value, remember to adjust after statement initialization.
            kind: LocalBindingKind::ExprBinding(Statement::default()),
        });

        let statement = self.statements.insert(StatementData {
            block,
            kind: StatementKind::ExprBinding(ExprBinding {
                binding,
                expression: Expression {
                    ty: ptr_ty,
                    kind: ExpressionKind::OpAlloca(ty),
                },
            }),
        });

        self.blocks[block].add_statement(position, statement);

        // Adjust the temporary value we set above to the actual statement.
        self.local_bindings[binding].kind = LocalBindingKind::ExprBinding(statement);

        (statement, binding)
    }

    pub fn add_bind_op_unary(
        &mut self,
        block: Block,
        position: BlockPosition,
        operator: UnaryOperator,
        operand: LocalBinding,
    ) -> (Statement, LocalBinding) {
        let ty = self.local_bindings[operand].ty();

        let binding = self.local_bindings.insert(LocalBindingData {
            ty,
            // Initialize with a temporary value, remember to adjust after statement initialization.
            kind: LocalBindingKind::ExprBinding(Statement::default()),
        });

        let statement = self.statements.insert(StatementData {
            block,
            kind: StatementKind::ExprBinding(ExprBinding {
                binding,
                expression: Expression {
                    ty,
                    kind: ExpressionKind::OpUnary(OpUnary { operator, operand }),
                },
            }),
        });

        self.blocks[block].add_statement(position, statement);

        // Adjust the temporary value we set above to the actual statement.
        self.local_bindings[binding].kind = LocalBindingKind::ExprBinding(statement);

        (statement, binding)
    }

    pub fn add_bind_op_binary(
        &mut self,
        block: Block,
        position: BlockPosition,
        operator: BinaryOperator,
        lhs: LocalBinding,
        rhs: LocalBinding,
    ) -> (Statement, LocalBinding) {
        let lhs_ty = self.local_bindings[lhs].ty();
        let rhs_ty = self.local_bindings[rhs].ty();
        let ty = self.ty().check_binary_op(operator, lhs_ty, rhs_ty).unwrap();

        let binding = self.local_bindings.insert(LocalBindingData {
            ty,
            // Initialize with a temporary value, remember to adjust after statement initialization.
            kind: LocalBindingKind::ExprBinding(Statement::default()),
        });

        let statement = self.statements.insert(StatementData {
            block,
            kind: StatementKind::ExprBinding(ExprBinding {
                binding,
                expression: Expression {
                    ty,
                    kind: ExpressionKind::OpBinary(OpBinary { operator, lhs, rhs }),
                },
            }),
        });

        self.blocks[block].add_statement(position, statement);

        // Adjust the temporary value we set above to the actual statement.
        self.local_bindings[binding].kind = LocalBindingKind::ExprBinding(statement);

        (statement, binding)
    }

    pub fn add_bind_op_vector(
        &mut self,
        block: Block,
        position: BlockPosition,
        vector_ty: ty::Vector,
        elements: impl IntoIterator<Item = LocalBinding>,
    ) -> (Statement, LocalBinding) {
        let size = vector_ty.size.to_usize();

        let mut collected_elements = Vec::with_capacity(size);
        let mut iter = elements.into_iter();

        for i in 0..size {
            let Some(binding) = iter.next() else {
                panic!(
                    "expected at least {} elements for a vector of type `{}` (found only {})",
                    size, vector_ty, i
                );
            };

            let ty = self.local_bindings[binding].ty();

            let TypeKind::Scalar(s) = *self.ty().kind(ty) else {
                panic!(
                    "expected all vector element inputs to be `{}` values (element `{}` was of \
                    type `{}`)",
                    vector_ty.scalar,
                    i,
                    ty.to_string(self.ty())
                );
            };

            if s != vector_ty.scalar {
                panic!(
                    "expected all vector element inputs to be `{}` values (element `{}` was of \
                    type `{}`)",
                    vector_ty.scalar,
                    i,
                    ty.to_string(self.ty())
                );
            }

            collected_elements.push(binding);
        }

        if let Some(_) = iter.next() {
            panic!(
                "expected only {} elements for a vector of type `{}`, but more were provided",
                size, vector_ty
            );
        }

        let ty = self.ty().register(TypeKind::Vector(vector_ty));

        let binding = self.local_bindings.insert(LocalBindingData {
            ty,
            // Initialize with a temporary value, remember to adjust after statement initialization.
            kind: LocalBindingKind::ExprBinding(Statement::default()),
        });

        let statement = self.statements.insert(StatementData {
            block,
            kind: StatementKind::ExprBinding(ExprBinding {
                binding,
                expression: Expression {
                    ty,
                    kind: ExpressionKind::OpVector(OpVector {
                        vector_ty,
                        elements: collected_elements,
                    }),
                },
            }),
        });

        self.blocks[block].add_statement(position, statement);

        // Adjust the temporary value we set above to the actual statement.
        self.local_bindings[binding].kind = LocalBindingKind::ExprBinding(statement);

        (statement, binding)
    }

    pub fn add_bind_op_matrix(
        &mut self,
        block: Block,
        position: BlockPosition,
        matrix_ty: ty::Matrix,
        columns: impl IntoIterator<Item = LocalBinding>,
    ) -> (Statement, LocalBinding) {
        let size = matrix_ty.columns.to_usize();

        let expected_vector_ty = ty::Vector {
            scalar: matrix_ty.scalar,
            size: matrix_ty.rows,
        };

        let mut collected_columns = Vec::with_capacity(size);
        let mut iter = columns.into_iter();

        for i in 0..size {
            let Some(binding) = iter.next() else {
                panic!(
                    "expected at least {} columns for a matrix of type `{}` (found only {})",
                    size, matrix_ty, i
                );
            };

            let ty = self.local_bindings[binding].ty();

            let TypeKind::Vector(v) = *self.ty().kind(ty) else {
                panic!(
                    "expected all column inputs to be `{}` values (element `{}` was of type `{}`)",
                    expected_vector_ty,
                    i,
                    ty.to_string(self.ty())
                );
            };

            if v != expected_vector_ty {
                panic!(
                    "expected all column inputs to be `{}` values (element `{}` was of type `{}`)",
                    expected_vector_ty,
                    i,
                    ty.to_string(self.ty())
                );
            }

            collected_columns.push(binding);
        }

        if let Some(_) = iter.next() {
            panic!(
                "expected only {} columns for a matrix of type `{}`, but more were provided",
                size, matrix_ty
            );
        }

        let ty = self.ty().register(TypeKind::Matrix(matrix_ty));

        let binding = self.local_bindings.insert(LocalBindingData {
            ty,
            // Initialize with a temporary value, remember to adjust after statement initialization.
            kind: LocalBindingKind::ExprBinding(Statement::default()),
        });

        let statement = self.statements.insert(StatementData {
            block,
            kind: StatementKind::ExprBinding(ExprBinding {
                binding,
                expression: Expression {
                    ty,
                    kind: ExpressionKind::OpMatrix(OpMatrix {
                        matrix_ty,
                        columns: collected_columns,
                    }),
                },
            }),
        });

        self.blocks[block].add_statement(position, statement);

        // Adjust the temporary value we set above to the actual statement.
        self.local_bindings[binding].kind = LocalBindingKind::ExprBinding(statement);

        (statement, binding)
    }

    pub fn add_bind_op_ptr_element_ptr(
        &mut self,
        block: Block,
        position: BlockPosition,
        pointer: LocalBinding,
        element_ty: Type,
        indices: impl IntoIterator<Item = LocalBinding>,
    ) -> (Statement, LocalBinding) {
        let ptr_ty = self.local_bindings[pointer].ty();

        let TypeKind::Ptr(_) = *self.ty.kind(ptr_ty) else {
            panic!("expected `pointer` expression to have a pointer type")
        };

        let element_ptr_ty = self.ty.register(TypeKind::Ptr(element_ty));

        let indices = indices.into_iter().collect::<Vec<_>>();

        let binding = self.local_bindings.insert(LocalBindingData {
            ty: element_ptr_ty,
            // Initialize with a temporary value, remember to adjust after statement initialization.
            kind: LocalBindingKind::ExprBinding(Statement::default()),
        });

        let statement = self.statements.insert(StatementData {
            block,
            kind: StatementKind::ExprBinding(ExprBinding {
                binding,
                expression: Expression {
                    ty: element_ptr_ty,
                    kind: ExpressionKind::OpPtrElementPtr(OpPtrElementPtr { pointer, indices }),
                },
            }),
        });

        self.blocks[block].add_statement(position, statement);

        // Adjust the temporary value we set above to the actual statement.
        self.local_bindings[binding].kind = LocalBindingKind::ExprBinding(statement);

        (statement, binding)
    }

    pub fn add_bind_op_extract_element(
        &mut self,
        block: Block,
        position: BlockPosition,
        value: LocalBinding,
        element_ty: Type,
        indices: impl IntoIterator<Item = LocalBinding>,
    ) -> (Statement, LocalBinding) {
        let indices = indices.into_iter().collect::<Vec<_>>();

        let binding = self.local_bindings.insert(LocalBindingData {
            ty: element_ty,
            // Initialize with a temporary value, remember to adjust after statement initialization.
            kind: LocalBindingKind::ExprBinding(Statement::default()),
        });

        let statement = self.statements.insert(StatementData {
            block,
            kind: StatementKind::ExprBinding(ExprBinding {
                binding,
                expression: Expression {
                    ty: element_ty,
                    kind: ExpressionKind::OpExtractElement(OpExtractElement { value, indices }),
                },
            }),
        });

        self.blocks[block].add_statement(position, statement);

        // Adjust the temporary value we set above to the actual statement.
        self.local_bindings[binding].kind = LocalBindingKind::ExprBinding(statement);

        (statement, binding)
    }

    pub fn add_bind_op_load(
        &mut self,
        block: Block,
        position: BlockPosition,
        pointer: LocalBinding,
    ) -> (Statement, LocalBinding) {
        let ptr_ty = self.local_bindings[pointer].ty();

        let TypeKind::Ptr(pointee_ty) = *self.ty.kind(ptr_ty) else {
            panic!("expected `pointer` expression to have a pointer type")
        };

        let binding = self.local_bindings.insert(LocalBindingData {
            ty: pointee_ty,
            // Initialize with a temporary value, remember to adjust after statement initialization.
            kind: LocalBindingKind::ExprBinding(Statement::default()),
        });

        let statement = self.statements.insert(StatementData {
            block,
            kind: StatementKind::ExprBinding(ExprBinding {
                binding,
                expression: Expression {
                    ty: pointee_ty,
                    kind: ExpressionKind::OpLoad(pointer),
                },
            }),
        });

        self.blocks[block].add_statement(position, statement);

        // Adjust the temporary value we set above to the actual statement.
        self.local_bindings[binding].kind = LocalBindingKind::ExprBinding(statement);

        (statement, binding)
    }

    pub fn add_bind_op_call_builtin(
        &mut self,
        block: Block,
        position: BlockPosition,
        callee: BuiltinFunction,
        arguments: impl IntoIterator<Item = LocalBinding>,
    ) -> (Statement, LocalBinding) {
        let Some(ret_ty) = callee.return_type() else {
            panic!(
                "only function calls that return a value can be bound as an expression; \
                to call a function without a return value, use `add_call_builtin` instead"
            )
        };

        let arguments = self.collect_call_builtin_args(&callee, arguments);

        let binding = self.local_bindings.insert(LocalBindingData {
            ty: ret_ty,
            // Initialize with a temporary value, remember to adjust after statement initialization.
            kind: LocalBindingKind::ExprBinding(Statement::default()),
        });

        let statement = self.statements.insert(StatementData {
            block,
            kind: StatementKind::ExprBinding(ExprBinding {
                binding,
                expression: Expression {
                    ty: ret_ty,
                    kind: ExpressionKind::OpCallBuiltin(OpCallBuiltin { callee, arguments }),
                },
            }),
        });

        self.blocks[block].add_statement(position, statement);

        // Adjust the temporary value we set above to the actual statement.
        self.local_bindings[binding].kind = LocalBindingKind::ExprBinding(statement);

        (statement, binding)
    }

    pub fn add_if(
        &mut self,
        block: Block,
        position: BlockPosition,
        condition: LocalBinding,
    ) -> (Statement, Block) {
        let then_block = self.blocks.insert(BlockData::new());

        let statement = self.statements.insert(StatementData {
            block,
            kind: StatementKind::If(If {
                condition,
                then_block,
                else_block: None,
                out_vars: vec![],
            }),
        });

        self.blocks[block].add_statement(position, statement);

        (statement, then_block)
    }

    pub fn add_if_out_var(&mut self, if_statement: Statement, ty: Type) -> LocalBinding {
        let stmt = self.statements[if_statement].kind.expect_if_mut();
        let index = stmt.out_vars.len();

        let binding = self.local_bindings.insert(LocalBindingData {
            ty,
            kind: LocalBindingKind::ControlFlowVar {
                statement: Default::default(),
                out_var: index,
            },
        });

        stmt.out_vars.push(binding);

        let then_block = stmt.then_block;
        let else_block = stmt.else_block;

        let (_, fallback) = self.add_bind_fallback_value(then_block, BlockPosition::Append, ty);

        self.blocks[then_block].add_control_flow_var(binding, fallback);

        if let Some(else_block) = else_block {
            let (_, fallback) = self.add_bind_fallback_value(else_block, BlockPosition::Append, ty);

            self.blocks[else_block].add_control_flow_var(binding, fallback);
        }

        binding
    }

    pub fn remove_if_out_var(&mut self, if_statement: Statement, binding: LocalBinding) -> bool {
        let stmt = self.statements[if_statement].kind.expect_if_mut();

        if let Some(index) = stmt.out_vars.iter().position(|b| *b == binding) {
            stmt.out_vars.remove(index);

            self.blocks[stmt.then_block].remove_control_flow_var(binding);

            if let Some(else_block) = stmt.else_block {
                self.blocks[else_block].remove_control_flow_var(binding);
            }

            self.local_bindings.remove(binding);

            true
        } else {
            false
        }
    }

    pub fn add_else_block(&mut self, if_statement: Statement) -> Block {
        let stmt = self.statements[if_statement].kind.expect_if_mut();

        let else_block = self.blocks.insert(BlockData::new());

        let mut case_block_data = BlockData::new();
        let out_var_count = stmt.out_vars.len();

        for i in 0..out_var_count {
            let binding = self.statements[if_statement].kind.expect_if().out_vars[i];
            let ty = self.local_bindings[binding].ty();
            let (_, fallback) = self.add_bind_fallback_value(else_block, BlockPosition::Append, ty);

            case_block_data.add_control_flow_var(binding, fallback);
        }

        let stmt = self.statements[if_statement].kind.expect_if_mut();

        stmt.else_block = Some(else_block);

        else_block
    }

    pub fn remove_else_block(&mut self, if_statement: Statement) -> bool {
        let stmt = self.statements[if_statement].kind.expect_if_mut();

        if let Some(else_block) = stmt.else_block.take() {
            self.remove_block(else_block);

            true
        } else {
            false
        }
    }

    pub fn add_switch(
        &mut self,
        block: Block,
        position: BlockPosition,
        on: LocalBinding,
    ) -> Statement {
        let default = self.blocks.insert(BlockData::new());

        let statement = self.statements.insert(StatementData {
            block,
            kind: StatementKind::Switch(Switch {
                on,
                cases: vec![],
                default,
                out_vars: vec![],
            }),
        });

        self.blocks[block].add_statement(position, statement);

        statement
    }

    pub fn add_switch_out_var(&mut self, switch_statement: Statement, ty: Type) -> LocalBinding {
        let stmt = self.statements[switch_statement].kind.expect_switch_mut();
        let index = stmt.out_vars.len();

        let binding = self.local_bindings.insert(LocalBindingData {
            ty,
            kind: LocalBindingKind::ControlFlowVar {
                statement: switch_statement,
                out_var: index,
            },
        });

        stmt.out_vars.push(binding);

        let case_count = stmt.cases.len();
        let default = stmt.default;

        for i in 0..case_count {
            let block = self.statements[switch_statement].kind.expect_switch().cases[i].block;
            let (_, fallback) = self.add_bind_fallback_value(block, BlockPosition::Append, ty);

            self.blocks[block].add_control_flow_var(binding, fallback);
        }

        let (_, fallback) = self.add_bind_fallback_value(default, BlockPosition::Append, ty);

        self.blocks[default].add_control_flow_var(binding, fallback);

        binding
    }

    pub fn remove_switch_out_var(
        &mut self,
        switch_statement: Statement,
        binding: LocalBinding,
    ) -> bool {
        let stmt = self.statements[switch_statement].kind.expect_switch_mut();

        if let Some(index) = stmt.out_vars.iter().position(|b| *b == binding) {
            stmt.out_vars.remove(index);

            for case in &stmt.cases {
                self.blocks[case.block].remove_control_flow_var(binding);
            }

            self.blocks[stmt.default].remove_control_flow_var(binding);
            self.local_bindings.remove(binding);

            true
        } else {
            false
        }
    }

    pub fn add_switch_case(&mut self, switch_statement: Statement, case: u32) -> Block {
        let stmt = self.statements[switch_statement].kind.expect_switch_mut();

        if stmt.cases.iter().any(|c| c.case == case) {
            panic!("switch already covers the given `case`")
        };

        let case_block = self.blocks.insert(BlockData::new());
        let out_var_count = stmt.out_vars.len();

        stmt.cases.push(SwitchCase {
            case,
            block: case_block,
        });

        for i in 0..out_var_count {
            let binding = self.statements[switch_statement]
                .kind
                .expect_switch()
                .out_vars[i];
            let ty = self.local_bindings[binding].ty();
            let (_, fallback) = self.add_bind_fallback_value(case_block, BlockPosition::Append, ty);

            self.blocks[case_block].add_control_flow_var(binding, fallback);
        }

        case_block
    }

    pub fn remove_switch_case(&mut self, switch_statement: Statement, case: u32) -> bool {
        let stmt = self.statements[switch_statement].kind.expect_switch_mut();

        if let Some(index) = stmt.cases.iter().position(|c| c.case == case) {
            let block = stmt.cases[index].block;

            stmt.cases.remove(index);
            self.remove_block(block);

            true
        } else {
            false
        }
    }

    pub fn add_loop(&mut self, block: Block, position: BlockPosition) -> (Statement, Block) {
        let loop_block = self.blocks.insert(BlockData::new());
        let loop_statement = self.statements.insert(StatementData {
            block,
            kind: StatementKind::Loop(Loop {
                loop_block,
                control: LoopControl::Infinite,
                loop_vars: vec![],
            }),
        });

        self.blocks[block].add_statement(position, loop_statement);

        (loop_statement, loop_block)
    }

    pub fn set_loop_control(&mut self, loop_statement: Statement, control: LoopControl) {
        let stmt = self.statements[loop_statement].kind.expect_loop_mut();

        stmt.control = control;
    }

    pub fn add_loop_var(
        &mut self,
        loop_statement: Statement,
        initial_value: LocalBinding,
    ) -> LocalBinding {
        let stmt = self.statements[loop_statement].kind.expect_loop_mut();
        let ty = self.local_bindings[initial_value].ty();
        let index = stmt.loop_vars.len();

        let binding = self.local_bindings.insert(LocalBindingData {
            ty,
            kind: LocalBindingKind::ControlFlowVar {
                statement: loop_statement,
                out_var: index,
            },
        });

        let loop_block = stmt.loop_block;

        stmt.loop_vars.push(LoopVar {
            binding,
            initial_value,
        });

        let (_, fallback) = self.add_bind_fallback_value(loop_block, BlockPosition::Append, ty);

        self.blocks[loop_block].add_control_flow_var(binding, fallback);

        binding
    }

    pub fn remove_loop_var(&mut self, loop_statement: Statement, binding: LocalBinding) -> bool {
        let stmt = self.statements[loop_statement].kind.expect_loop_mut();

        if let Some(index) = stmt.loop_vars.iter().position(|c| c.binding == binding) {
            stmt.loop_vars.remove(index);

            let loop_block = stmt.loop_block;

            self.blocks[loop_block].remove_control_flow_var(binding);
            self.local_bindings.remove(binding);

            true
        } else {
            false
        }
    }

    pub fn add_return(
        &mut self,
        block: Block,
        position: BlockPosition,
        value: Option<LocalBinding>,
    ) -> Statement {
        let statement = self.statements.insert(StatementData {
            block,
            kind: StatementKind::Return(Return { value }),
        });

        self.blocks[block].add_statement(position, statement);

        statement
    }

    pub fn add_store(
        &mut self,
        block: Block,
        position: BlockPosition,
        pointer: LocalBinding,
        value: LocalBinding,
    ) -> Statement {
        let pointer_ty = self.local_bindings[pointer].ty();
        let value_ty = self.local_bindings[value].ty();

        let TypeKind::Ptr(pointee_ty) = *self.ty.kind(pointer_ty) else {
            panic!("expected `pointer` to be a pointer type");
        };

        assert_eq!(
            pointee_ty, value_ty,
            "the `value`'s type must match the `pointer`'s pointee type"
        );

        let statement = self.statements.insert(StatementData {
            block,
            kind: StatementKind::Store(Store { pointer, value }),
        });

        self.blocks[block].add_statement(position, statement);

        statement
    }

    pub fn add_call_builtin(
        &mut self,
        block: Block,
        position: BlockPosition,
        callee: BuiltinFunction,
        arguments: impl IntoIterator<Item = LocalBinding>,
    ) -> Statement {
        let arguments = self.collect_call_builtin_args(&callee, arguments);

        let statement = self.statements.insert(StatementData {
            block,
            kind: StatementKind::CallBuiltin(OpCallBuiltin { callee, arguments }),
        });

        self.blocks[block].add_statement(position, statement);

        statement
    }

    pub fn remove_statement(&mut self, statement: Statement) {
        let block = self.statements[statement].block;

        self.blocks[block].remove_statement(statement);
        self.remove_statement_and_bindings(statement);
    }

    pub fn set_control_flow_var(&mut self, block: Block, index: usize, value: LocalBinding) {
        self.blocks[block].set_control_flow_var(index, value);
    }

    fn remove_statement_and_bindings(&mut self, statement: Statement) {
        match self.statements[statement].kind() {
            StatementKind::If(stmt) => {
                for out_var in &stmt.out_vars {
                    self.local_bindings.remove(*out_var);
                }
            }
            StatementKind::Switch(stmt) => {
                for out_var in &stmt.out_vars {
                    self.local_bindings.remove(*out_var);
                }
            }
            StatementKind::Loop(stmt) => {
                for loop_var in &stmt.loop_vars {
                    self.local_bindings.remove(loop_var.binding);
                }
            }
            StatementKind::ExprBinding(stmt) => {
                self.local_bindings.remove(stmt.binding());
            }
            _ => {}
        }

        self.statements.remove(statement);
    }

    fn remove_block(&mut self, block: Block) {
        let stmt_count = self.blocks[block].statements.len();

        for i in 0..stmt_count {
            let stmt = self.blocks[block].statements[i];

            self.remove_statement_and_bindings(stmt);
        }
    }

    fn collect_call_builtin_args(
        &self,
        callee: &BuiltinFunction,
        arguments: impl IntoIterator<Item = LocalBinding>,
    ) -> Vec<LocalBinding> {
        let mut collected_args = Vec::new();

        for (i, arg) in arguments.into_iter().enumerate() {
            let arg_ty = self.local_bindings[arg].ty();
            let expected_ty = callee.arguments()[i];

            assert_eq!(arg_ty, expected_ty, "argument {} has wrong type", i);

            collected_args.push(arg);
        }

        collected_args
    }
}

impl Index<LocalBinding> for Scf {
    type Output = LocalBindingData;

    fn index(&self, index: LocalBinding) -> &Self::Output {
        &self.local_bindings[index]
    }
}

impl Index<Statement> for Scf {
    type Output = StatementData;

    fn index(&self, index: Statement) -> &Self::Output {
        &self.statements[index]
    }
}

impl Index<Block> for Scf {
    type Output = BlockData;

    fn index(&self, index: Block) -> &Self::Output {
        &self.blocks[index]
    }
}
