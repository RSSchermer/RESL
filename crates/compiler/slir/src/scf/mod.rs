use std::hash::{Hash, Hasher};
use std::ops::{Deref, Index};

use indexmap::{IndexMap, IndexSet};
use serde::{Deserialize, Serialize};
use slotmap::SlotMap;

use crate::ty::{Type, TypeKind, TypeRegistry, TY_BOOL, TY_F32, TY_I32, TY_U32};
use crate::{
    BinaryOperator, StorageBinding, StorageBindingRegistry, UnaryOperator, UniformBinding,
    UniformBindingRegistry, WorkgroupBinding, WorkgroupBindingRegistry,
};

slotmap::new_key_type! {
    pub struct Expression;
    pub struct Statement;
    pub struct Block;
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub struct LocalBinding {
    id: u64,
    ty: Type,
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct ExpressionData {
    ty: Type,
    kind: ExpressionKind,
}

impl ExpressionData {
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
    operand: Expression,
}

impl OpUnary {
    pub fn operator(&self) -> UnaryOperator {
        self.operator
    }

    pub fn operand(&self) -> Expression {
        self.operand
    }
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct OpBinary {
    operator: BinaryOperator,
    lhs: Expression,
    rhs: Expression,
}

impl OpBinary {
    pub fn operator(&self) -> BinaryOperator {
        self.operator
    }

    pub fn lhs(&self) -> Expression {
        self.lhs
    }

    pub fn rhs(&self) -> Expression {
        self.rhs
    }
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct OpPtrElementPtr {
    pointer: Expression,
    indices: Vec<Expression>,
}

impl OpPtrElementPtr {
    pub fn pointer(&self) -> Expression {
        self.pointer
    }

    pub fn indices(&self) -> &[Expression] {
        &self.indices
    }
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct OpExtractElement {
    value: Expression,
    indices: Vec<Expression>,
}

impl OpExtractElement {
    pub fn value(&self) -> Expression {
        self.value
    }

    pub fn indices(&self) -> &[Expression] {
        &self.indices
    }
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub enum ExpressionKind {
    LocalValue(LocalBinding),
    UniformValue(UniformBinding),
    StorageValue(StorageBinding),
    WorkgroupValue(WorkgroupBinding),
    FallbackValue(Type),
    ConstU32(u32),
    ConstI32(i32),
    ConstF32(f32),
    ConstBool(bool),
    OpUnary(OpUnary),
    OpBinary(OpBinary),
    OpPtrElementPtr(OpPtrElementPtr),
    OpExtractElement(OpExtractElement),
    OpLoad(Expression),
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct BlockData {
    statements: IndexSet<Statement>,
    control_flow_vars: IndexMap<LocalBinding, Expression>,
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

    pub fn control_flow_var(&self, binding: LocalBinding) -> Expression {
        *self
            .control_flow_vars
            .get(&binding)
            .expect("no control-flow variable associated with the local binding")
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

    fn set_control_flow_var(&mut self, binding: LocalBinding, value: Expression) {
        if self.control_flow_vars.insert(binding, value).is_none() {
            panic!("no control-flow variable associated with the local binding");
        }
    }

    fn add_control_flow_var(&mut self, binding: LocalBinding, value: Expression) {
        self.control_flow_vars.insert(binding, value);
    }

    fn remove_control_flow_var(&mut self, binding: LocalBinding) {
        self.control_flow_vars.shift_remove(&binding);
    }
}

#[derive(Clone, Copy, Serialize, Deserialize, Debug)]
pub enum LoopControl {
    Head(Expression),
    Tail(Expression),
    Infinite,
}

#[derive(Clone, Copy, Serialize, Deserialize, Debug)]
pub struct LoopVar {
    binding: LocalBinding,
    initial_value: Expression,
}

impl LoopVar {
    pub fn binding(&self) -> LocalBinding {
        self.binding
    }

    pub fn initial_value(&self) -> Expression {
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

    // pub fn add_loop_var(&mut self, init: Expression) {
    //     todo!()
    // }
    //
    // pub fn remove_loop_var(&mut self, binding: LocalBinding) -> bool {
    //     if let Some(index) = self.loop_vars.iter().position(|x| x.binding == binding) {
    //         self.loop_vars.remove(index);
    //         self.block.remove_control_flow_var(binding);
    //
    //         true
    //     } else {
    //         false
    //     }
    // }
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct If {
    condition: Expression,
    then_block: Block,
    else_block: Option<Block>,
    out_vars: Vec<LocalBinding>,
}

impl If {
    pub fn condition(&self) -> Expression {
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
    case: i64,
    block: Block,
}

impl SwitchCase {
    pub fn case(&self) -> i64 {
        self.case
    }

    pub fn block(&self) -> Block {
        self.block
    }
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct Switch {
    on: Expression,
    cases: Vec<SwitchCase>,
    default: Block,
    out_vars: Vec<LocalBinding>,
}

impl Switch {
    pub fn on(&self) -> Expression {
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
    value: Option<Expression>,
}

impl Return {
    pub fn value(&self) -> Option<Expression> {
        self.value
    }
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct ExprBinding {
    binding: LocalBinding,
    expr: Expression,
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct Alloca {
    binding: LocalBinding,
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct Store {
    pointer: Expression,
    value: Expression,
}

impl Store {
    pub fn ptr(&self) -> Expression {
        self.pointer
    }

    pub fn value(&self) -> Expression {
        self.value
    }
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct StatementData {
    kind: StatementKind,
}

impl StatementData {
    pub fn kind(&self) -> &StatementKind {
        &self.kind
    }
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub enum StatementKind {
    If(If),
    Switch(Switch),
    Loop(Loop),
    Return(Return),
    ExprBinding(ExprBinding),
    Alloca(Alloca),
    Store(Store),
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
}

pub enum BlockPosition {
    Append,
    Prepend,
    InsertBefore(Statement),
    InsertAfter(Statement),
}

#[derive(Clone, Serialize, Deserialize, Debug)]
struct LocalBindingGenerator {
    id: u64,
}

impl LocalBindingGenerator {
    fn new() -> Self {
        Self { id: 0 }
    }

    fn generate(&mut self, ty: Type) -> LocalBinding {
        let id = self.id;

        self.id += 1;

        LocalBinding { id, ty }
    }
}

#[derive(Clone, Deserialize, Debug)]
pub struct ScfData {
    expressions: SlotMap<Expression, ExpressionData>,
    statements: SlotMap<Statement, StatementData>,
    blocks: SlotMap<Block, BlockData>,
    local_binding_generator: LocalBindingGenerator,
}

#[derive(Serialize, Debug)]
pub struct Scf {
    #[serde(skip_serializing)]
    ty: TypeRegistry,
    expressions: SlotMap<Expression, ExpressionData>,
    statements: SlotMap<Statement, StatementData>,
    blocks: SlotMap<Block, BlockData>,
    local_binding_generator: LocalBindingGenerator,
}

impl Scf {
    pub fn new(type_registry: TypeRegistry) -> Self {
        Self {
            ty: type_registry,
            expressions: Default::default(),
            statements: Default::default(),
            blocks: Default::default(),
            local_binding_generator: LocalBindingGenerator::new(),
        }
    }

    pub fn from_ty_and_data(ty: TypeRegistry, data: ScfData) -> Self {
        let ScfData {
            expressions,
            statements,
            blocks,
            local_binding_generator,
        } = data;

        Scf {
            ty,
            expressions,
            statements,
            blocks,
            local_binding_generator,
        }
    }

    pub fn ty(&self) -> &TypeRegistry {
        &self.ty
    }

    pub fn make_expr_local_value(&mut self, binding: LocalBinding) -> Expression {
        self.expressions.insert(ExpressionData {
            ty: binding.ty,
            kind: ExpressionKind::LocalValue(binding),
        })
    }

    pub fn make_expr_uniform_value(
        &mut self,
        registry: UniformBindingRegistry,
        binding: UniformBinding,
    ) -> Expression {
        self.expressions.insert(ExpressionData {
            ty: registry[binding].ty,
            kind: ExpressionKind::UniformValue(binding),
        })
    }

    pub fn make_expr_storage_value(
        &mut self,
        registry: StorageBindingRegistry,
        binding: StorageBinding,
    ) -> Expression {
        self.expressions.insert(ExpressionData {
            ty: registry[binding].ty,
            kind: ExpressionKind::StorageValue(binding),
        })
    }

    pub fn make_expr_workgroup_value(
        &mut self,
        registry: WorkgroupBindingRegistry,
        binding: WorkgroupBinding,
    ) -> Expression {
        self.expressions.insert(ExpressionData {
            ty: registry[binding].ty,
            kind: ExpressionKind::WorkgroupValue(binding),
        })
    }

    pub fn make_expr_fallback_value(&mut self, ty: Type) -> Expression {
        self.expressions.insert(ExpressionData {
            ty,
            kind: ExpressionKind::FallbackValue(ty),
        })
    }

    pub fn make_expr_const_u32(&mut self, value: u32) -> Expression {
        self.expressions.insert(ExpressionData {
            ty: TY_U32,
            kind: ExpressionKind::ConstU32(value),
        })
    }

    pub fn make_expr_const_i32(&mut self, value: i32) -> Expression {
        self.expressions.insert(ExpressionData {
            ty: TY_I32,
            kind: ExpressionKind::ConstI32(value),
        })
    }

    pub fn make_expr_const_f32(&mut self, value: f32) -> Expression {
        self.expressions.insert(ExpressionData {
            ty: TY_F32,
            kind: ExpressionKind::ConstF32(value),
        })
    }

    pub fn make_expr_const_bool(&mut self, value: bool) -> Expression {
        self.expressions.insert(ExpressionData {
            ty: TY_BOOL,
            kind: ExpressionKind::ConstBool(value),
        })
    }

    pub fn make_expr_op_unary(
        &mut self,
        operator: UnaryOperator,
        operand: Expression,
    ) -> Expression {
        let ty = self.expressions[operand].ty;

        self.expressions.insert(ExpressionData {
            ty,
            kind: ExpressionKind::OpUnary(OpUnary { operator, operand }),
        })
    }

    pub fn make_expr_op_binary(
        &mut self,
        operator: BinaryOperator,
        lhs: Expression,
        rhs: Expression,
    ) -> Expression {
        let lhs_ty = self.expressions[lhs].ty;
        let rhs_ty = self.expressions[rhs].ty;

        let Some(ty) = operator.output_ty(&self.ty, lhs_ty, rhs_ty) else {
            panic!("operand types not compatible with operator");
        };

        self.expressions.insert(ExpressionData {
            ty,
            kind: ExpressionKind::OpBinary(OpBinary { operator, lhs, rhs }),
        })
    }

    pub fn make_expr_op_ptr_element_ptr(
        &mut self,
        pointer: Expression,
        element_ty: Type,
        indices: impl IntoIterator<Item = Expression>,
    ) -> Expression {
        let ptr_ty = self.expressions[pointer].ty;

        let TypeKind::Ptr(_) = *self.ty.kind(ptr_ty) else {
            panic!("expected `pointer` expression to have a pointer type")
        };

        let element_ptr_ty = self.ty.register(TypeKind::Ptr(element_ty));

        let indices = indices.into_iter().collect::<Vec<_>>();

        self.expressions.insert(ExpressionData {
            ty: element_ptr_ty,
            kind: ExpressionKind::OpPtrElementPtr(OpPtrElementPtr { pointer, indices }),
        })
    }

    pub fn make_expr_op_extract_element(
        &mut self,
        value: Expression,
        element_ty: Type,
        indices: impl IntoIterator<Item = Expression>,
    ) -> Expression {
        let indices = indices.into_iter().collect::<Vec<_>>();

        self.expressions.insert(ExpressionData {
            ty: element_ty,
            kind: ExpressionKind::OpExtractElement(OpExtractElement { value, indices }),
        })
    }

    pub fn make_expr_op_load(&mut self, pointer: Expression) -> Expression {
        let ptr_ty = self.expressions[pointer].ty;

        let TypeKind::Ptr(pointee_ty) = *self.ty.kind(ptr_ty) else {
            panic!("expected `pointer` expression to have a pointer type")
        };

        self.expressions.insert(ExpressionData {
            ty: pointee_ty,
            kind: ExpressionKind::OpLoad(pointer),
        })
    }

    pub fn add_stmt_if(
        &mut self,
        block: Block,
        position: BlockPosition,
        condition: Expression,
    ) -> Statement {
        let then_block = self.blocks.insert(BlockData::new());

        let statement = self.statements.insert(StatementData {
            kind: StatementKind::If(If {
                condition,
                then_block,
                else_block: None,
                out_vars: vec![],
            }),
        });

        self.blocks[block].add_statement(position, statement);

        statement
    }

    pub fn add_if_out_var(&mut self, if_statement: Statement, ty: Type) -> LocalBinding {
        let binding = self.local_binding_generator.generate(ty);
        let fallback = self.make_expr_fallback_value(ty);
        let stmt = self.statements[if_statement].kind.expect_if_mut();

        stmt.out_vars.push(binding);

        self.blocks[stmt.then_block].add_control_flow_var(binding, fallback);

        if let Some(else_block) = stmt.else_block {
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

            true
        } else {
            false
        }
    }

    pub fn add_else_block(&mut self, if_statement: Statement) {
        let stmt = self.statements[if_statement].kind.expect_if_mut();

        let mut case_block_data = BlockData::new();
        let out_var_count = stmt.out_vars.len();

        for i in 0..out_var_count {
            let binding = self.statements[if_statement].kind.expect_if().out_vars[i];
            let fallback = self.make_expr_fallback_value(binding.ty);

            case_block_data.add_control_flow_var(binding, fallback);
        }

        let else_block = self.blocks.insert(BlockData::new());

        let stmt = self.statements[if_statement].kind.expect_if_mut();

        stmt.else_block = Some(else_block);
    }

    pub fn remove_else_block(&mut self, if_statement: Statement) -> bool {
        let stmt = self.statements[if_statement].kind.expect_if_mut();

        stmt.else_block.take().is_some()
    }

    pub fn add_stmt_switch(
        &mut self,
        block: Block,
        position: BlockPosition,
        on: Expression,
    ) -> Statement {
        let default = self.blocks.insert(BlockData::new());

        let statement = self.statements.insert(StatementData {
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
        let binding = self.local_binding_generator.generate(ty);
        let fallback = self.make_expr_fallback_value(ty);
        let stmt = self.statements[switch_statement].kind.expect_switch_mut();

        stmt.out_vars.push(binding);

        for case in stmt.cases() {
            self.blocks[case.block].add_control_flow_var(binding, fallback);
        }

        self.blocks[stmt.default].add_control_flow_var(binding, fallback);

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

            true
        } else {
            false
        }
    }

    pub fn add_switch_case(&mut self, switch_statement: Statement, case: i64) -> Block {
        let stmt = self.statements[switch_statement].kind.expect_switch_mut();

        if stmt.cases.iter().any(|c| c.case == case) {
            panic!("switch already covers the given `case`")
        };

        let mut case_block_data = BlockData::new();
        let out_var_count = stmt.out_vars.len();

        for i in 0..out_var_count {
            let binding = self.statements[switch_statement]
                .kind
                .expect_switch()
                .out_vars[i];
            let fallback = self.make_expr_fallback_value(binding.ty);

            case_block_data.add_control_flow_var(binding, fallback);
        }

        let case_block = self.blocks.insert(BlockData::new());
        let stmt = self.statements[switch_statement].kind.expect_switch_mut();

        stmt.cases.push(SwitchCase {
            case,
            block: case_block,
        });

        case_block
    }

    pub fn remove_switch_case(&mut self, switch_statement: Statement, case: i64) -> bool {
        let stmt = self.statements[switch_statement].kind.expect_switch_mut();

        if let Some(index) = stmt.cases.iter().position(|c| c.case == case) {
            stmt.cases.remove(index);

            true
        } else {
            false
        }
    }

    pub fn add_stmt_loop(&mut self, block: Block, position: BlockPosition) -> Statement {
        let loop_block = self.blocks.insert(BlockData::new());
        let loop_statement = self.statements.insert(StatementData {
            kind: StatementKind::Loop(Loop {
                loop_block,
                control: LoopControl::Infinite,
                loop_vars: vec![],
            }),
        });

        self.blocks[block].add_statement(position, loop_statement);

        loop_statement
    }

    pub fn set_loop_control(&mut self, loop_statement: Statement, control: LoopControl) {
        let stmt = self.statements[loop_statement].kind.expect_loop_mut();

        stmt.control = control;
    }

    pub fn add_loop_var(
        &mut self,
        loop_statement: Statement,
        initial_value: Expression,
    ) -> LocalBinding {
        let stmt = self.statements[loop_statement].kind.expect_loop_mut();
        let ty = self.expressions[initial_value].ty;
        let binding = self.local_binding_generator.generate(ty);
        let loop_block = stmt.loop_block;

        stmt.loop_vars.push(LoopVar {
            binding,
            initial_value,
        });

        let fallback = self.make_expr_fallback_value(ty);

        self.blocks[loop_block].add_control_flow_var(binding, fallback);

        binding
    }

    pub fn remove_loop_var(&mut self, loop_statement: Statement, binding: LocalBinding) -> bool {
        let stmt = self.statements[loop_statement].kind.expect_loop_mut();

        if let Some(index) = stmt.loop_vars.iter().position(|c| c.binding == binding) {
            stmt.loop_vars.remove(index);

            let loop_block = stmt.loop_block;

            self.blocks[loop_block].remove_control_flow_var(binding);

            true
        } else {
            false
        }
    }

    pub fn add_stmt_return(
        &mut self,
        block: Block,
        position: BlockPosition,
        value: Option<Expression>,
    ) -> Statement {
        let statement = self.statements.insert(StatementData {
            kind: StatementKind::Return(Return { value }),
        });

        self.blocks[block].add_statement(position, statement);

        statement
    }

    pub fn add_stmt_expr_binding(
        &mut self,
        block: Block,
        position: BlockPosition,
        expr: Expression,
    ) -> Statement {
        let ty = self.expressions[expr].ty;
        let binding = self.local_binding_generator.generate(ty);
        let statement = self.statements.insert(StatementData {
            kind: StatementKind::ExprBinding(ExprBinding { binding, expr }),
        });

        self.blocks[block].add_statement(position, statement);

        statement
    }

    pub fn add_stmt_alloca(
        &mut self,
        block: Block,
        position: BlockPosition,
        ty: Type,
    ) -> Statement {
        let ptr_ty = self.ty.register(TypeKind::Ptr(ty));
        let binding = self.local_binding_generator.generate(ptr_ty);
        let statement = self.statements.insert(StatementData {
            kind: StatementKind::Alloca(Alloca { binding }),
        });

        self.blocks[block].add_statement(position, statement);

        statement
    }

    pub fn add_stmt_store(
        &mut self,
        block: Block,
        position: BlockPosition,
        pointer: Expression,
        value: Expression,
    ) -> Statement {
        let pointer_ty = self.expressions[pointer].ty;
        let value_ty = self.expressions[value].ty;

        let TypeKind::Ptr(pointee_ty) = *self.ty.kind(pointer_ty) else {
            panic!("expected `pointer` to be a pointer type");
        };

        assert_eq!(
            pointee_ty, value_ty,
            "the `value`'s type must match the `pointer`'s pointee type"
        );

        let statement = self.statements.insert(StatementData {
            kind: StatementKind::Store(Store { pointer, value }),
        });

        self.blocks[block].add_statement(position, statement);

        statement
    }

    pub fn remove_statement(&mut self, block: Block, statement: Statement) -> bool {
        if self.blocks[block].remove_statement(statement) {
            self.statements.remove(statement);

            true
        } else {
            false
        }
    }
}

impl Index<Expression> for Scf {
    type Output = ExpressionData;

    fn index(&self, index: Expression) -> &Self::Output {
        &self.expressions[index]
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
