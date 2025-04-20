use std::ops::Index;
use std::slice;

use rustc_hash::FxHashSet;
use serde::{Deserialize, Serialize};
use slotmap::SlotMap;

use crate::ty::{Type, TY_BOOL, TY_F32, TY_I32, TY_PTR, TY_U32};
use crate::util::thin_set::ThinSet;
use crate::{
    BinaryOperator, Function, Module, StorageBinding, UnaryOperator, UniformBinding,
    WorkgroupBinding,
};

pub trait Connectivity {
    fn value_inputs(&self) -> &[ValueInput];

    fn value_outputs(&self) -> &[ValueOutput];

    fn value_outputs_mut(&mut self) -> &mut [ValueOutput];

    fn state(&self) -> Option<&State>;

    fn state_mut(&mut self) -> Option<&mut State>;
}

slotmap::new_key_type! {
    pub struct Node;
    pub struct Region;
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct FunctionNode {
    dependencies: Vec<ValueInput>,
    output: ValueOutput,
    region: Region,
}

impl FunctionNode {
    pub fn region(&self) -> Region {
        self.region
    }

    pub fn dependencies(&self) -> &[ValueInput] {
        &self.dependencies
    }

    pub fn output(&self) -> &ValueOutput {
        &self.output
    }
}

impl Connectivity for FunctionNode {
    fn value_inputs(&self) -> &[ValueInput] {
        &self.dependencies
    }

    fn value_outputs(&self) -> &[ValueOutput] {
        slice::from_ref(&self.output)
    }

    fn value_outputs_mut(&mut self) -> &mut [ValueOutput] {
        slice::from_mut(&mut self.output)
    }

    fn state(&self) -> Option<&State> {
        None
    }

    fn state_mut(&mut self) -> Option<&mut State> {
        None
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub struct ValueInput {
    pub ty: Type,
    pub origin: ValueOrigin,
}

impl ValueInput {
    pub fn placeholder(ty: Type) -> Self {
        ValueInput {
            ty,
            origin: ValueOrigin::placeholder(),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub enum ValueOrigin {
    Argument(u32),
    Output { producer: Node, output: u32 },
}

impl ValueOrigin {
    pub fn placeholder() -> Self {
        ValueOrigin::Argument(u32::MAX)
    }

    pub fn is_placeholder(&self) -> bool {
        self == &ValueOrigin::Argument(u32::MAX)
    }
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct ValueOutput {
    pub ty: Option<Type>,
    pub users: ThinSet<ValueUser>,
}

impl ValueOutput {
    pub fn new(ty: Option<Type>) -> Self {
        ValueOutput {
            ty,
            users: ThinSet::new(),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub enum ValueUser {
    Result(u32),
    Input { consumer: Node, input: u32 },
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub enum StateOrigin {
    Argument,
    Node(Node),
}

impl StateOrigin {
    pub fn as_node(&self) -> Option<Node> {
        if let StateOrigin::Node(node) = self {
            Some(*node)
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub enum StateUser {
    Result,
    Node(Node),
}

impl StateUser {
    pub fn as_node(&self) -> Option<Node> {
        if let StateUser::Node(node) = self {
            Some(*node)
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub struct State {
    pub origin: StateOrigin,
    pub user: StateUser,
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct RegionData {
    owner: Option<Node>,
    nodes: FxHashSet<Node>,
    value_arguments: Vec<ValueOutput>,
    value_results: Vec<ValueInput>,
    state_argument: StateUser,
    state_result: StateOrigin,
}

impl RegionData {
    pub fn owner(&self) -> Node {
        self.owner.expect("region not correctly initialized")
    }

    pub fn nodes(&self) -> impl IntoIterator<Item = Node> + use<'_> {
        self.nodes.iter().copied()
    }

    pub fn value_arguments(&self) -> &[ValueOutput] {
        &self.value_arguments
    }

    pub fn value_results(&self) -> &[ValueInput] {
        &self.value_results
    }

    pub fn state_argument(&self) -> &StateUser {
        &self.state_argument
    }

    pub fn state_result(&self) -> &StateOrigin {
        &self.state_result
    }
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct NodeData {
    kind: NodeKind,
    region: Option<Region>,
}

impl NodeData {
    pub fn kind(&self) -> &NodeKind {
        &self.kind
    }
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub enum NodeKind {
    Switch(SwitchNode),
    Loop(LoopNode),
    Simple(SimpleNode),
    UniformBinding(UniformBindingNode),
    StorageBinding(StorageBindingNode),
    WorkgroupBinding(WorkgroupBindingNode),
    Function(FunctionNode),
}

impl NodeKind {
    fn expect_switch_mut(&mut self) -> &mut SwitchNode {
        if let NodeKind::Switch(n) = self {
            n
        } else {
            panic!("expected node to be switch node")
        }
    }
}

impl Connectivity for NodeData {
    fn value_inputs(&self) -> &[ValueInput] {
        match &self.kind {
            NodeKind::Switch(n) => n.value_inputs(),
            NodeKind::Loop(n) => n.value_inputs(),
            NodeKind::Simple(n) => n.value_inputs(),
            NodeKind::UniformBinding(n) => n.value_inputs(),
            NodeKind::StorageBinding(n) => n.value_inputs(),
            NodeKind::WorkgroupBinding(n) => n.value_inputs(),
            NodeKind::Function(n) => n.value_inputs(),
        }
    }

    fn value_outputs(&self) -> &[ValueOutput] {
        match &self.kind {
            NodeKind::Switch(n) => n.value_outputs(),
            NodeKind::Loop(n) => n.value_outputs(),
            NodeKind::Simple(n) => n.value_outputs(),
            NodeKind::UniformBinding(n) => n.value_outputs(),
            NodeKind::StorageBinding(n) => n.value_outputs(),
            NodeKind::WorkgroupBinding(n) => n.value_outputs(),
            NodeKind::Function(n) => n.value_outputs(),
        }
    }

    fn value_outputs_mut(&mut self) -> &mut [ValueOutput] {
        match &mut self.kind {
            NodeKind::Switch(n) => n.value_outputs_mut(),
            NodeKind::Loop(n) => n.value_outputs_mut(),
            NodeKind::Simple(n) => n.value_outputs_mut(),
            NodeKind::UniformBinding(n) => n.value_outputs_mut(),
            NodeKind::StorageBinding(n) => n.value_outputs_mut(),
            NodeKind::WorkgroupBinding(n) => n.value_outputs_mut(),
            NodeKind::Function(n) => n.value_outputs_mut(),
        }
    }

    fn state(&self) -> Option<&State> {
        match &self.kind {
            NodeKind::Switch(n) => n.state(),
            NodeKind::Loop(n) => n.state(),
            NodeKind::Simple(n) => n.state(),
            NodeKind::UniformBinding(n) => n.state(),
            NodeKind::StorageBinding(n) => n.state(),
            NodeKind::WorkgroupBinding(n) => n.state(),
            NodeKind::Function(n) => n.state(),
        }
    }

    fn state_mut(&mut self) -> Option<&mut State> {
        match &mut self.kind {
            NodeKind::Switch(n) => n.state_mut(),
            NodeKind::Loop(n) => n.state_mut(),
            NodeKind::Simple(n) => n.state_mut(),
            NodeKind::UniformBinding(n) => n.state_mut(),
            NodeKind::StorageBinding(n) => n.state_mut(),
            NodeKind::WorkgroupBinding(n) => n.state_mut(),
            NodeKind::Function(n) => n.state_mut(),
        }
    }
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct UniformBindingNode {
    binding: UniformBinding,
    output: ValueOutput,
}

impl Connectivity for UniformBindingNode {
    fn value_inputs(&self) -> &[ValueInput] {
        &[]
    }

    fn value_outputs(&self) -> &[ValueOutput] {
        slice::from_ref(&self.output)
    }

    fn value_outputs_mut(&mut self) -> &mut [ValueOutput] {
        slice::from_mut(&mut self.output)
    }

    fn state(&self) -> Option<&State> {
        None
    }

    fn state_mut(&mut self) -> Option<&mut State> {
        None
    }
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct StorageBindingNode {
    binding: StorageBinding,
    output: ValueOutput,
}

impl Connectivity for StorageBindingNode {
    fn value_inputs(&self) -> &[ValueInput] {
        &[]
    }

    fn value_outputs(&self) -> &[ValueOutput] {
        slice::from_ref(&self.output)
    }

    fn value_outputs_mut(&mut self) -> &mut [ValueOutput] {
        slice::from_mut(&mut self.output)
    }

    fn state(&self) -> Option<&State> {
        None
    }

    fn state_mut(&mut self) -> Option<&mut State> {
        None
    }
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct WorkgroupBindingNode {
    binding: WorkgroupBinding,
    output: ValueOutput,
}

impl Connectivity for WorkgroupBindingNode {
    fn value_inputs(&self) -> &[ValueInput] {
        &[]
    }

    fn value_outputs(&self) -> &[ValueOutput] {
        slice::from_ref(&self.output)
    }

    fn value_outputs_mut(&mut self) -> &mut [ValueOutput] {
        slice::from_mut(&mut self.output)
    }

    fn state(&self) -> Option<&State> {
        None
    }

    fn state_mut(&mut self) -> Option<&mut State> {
        None
    }
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct SwitchNode {
    value_inputs: Vec<ValueInput>,
    value_outputs: Vec<ValueOutput>,
    branches: Vec<Region>,
    state: Option<State>,
}

impl SwitchNode {
    pub fn predicate(&self) -> &ValueInput {
        &self.value_inputs[0]
    }

    pub fn branches(&self) -> &[Region] {
        &self.branches
    }
}

impl Connectivity for SwitchNode {
    fn value_inputs(&self) -> &[ValueInput] {
        &self.value_inputs
    }

    fn value_outputs(&self) -> &[ValueOutput] {
        &self.value_outputs
    }

    fn value_outputs_mut(&mut self) -> &mut [ValueOutput] {
        &mut self.value_outputs
    }

    fn state(&self) -> Option<&State> {
        self.state.as_ref()
    }

    fn state_mut(&mut self) -> Option<&mut State> {
        self.state.as_mut()
    }
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct LoopNode {
    value_inputs: Vec<ValueInput>,
    value_outputs: Vec<ValueOutput>,
    state: Option<State>,
    region: Region,
}

impl LoopNode {
    pub fn region(&self) -> &Region {
        &self.region
    }
}

impl Connectivity for LoopNode {
    fn value_inputs(&self) -> &[ValueInput] {
        &self.value_inputs
    }

    fn value_outputs(&self) -> &[ValueOutput] {
        &self.value_outputs
    }

    fn value_outputs_mut(&mut self) -> &mut [ValueOutput] {
        &mut self.value_outputs
    }

    fn state(&self) -> Option<&State> {
        self.state.as_ref()
    }

    fn state_mut(&mut self) -> Option<&mut State> {
        self.state.as_mut()
    }
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct OpAlloca {
    value_output: ValueOutput,
}

impl OpAlloca {
    pub fn value_output(&self) -> &ValueOutput {
        &self.value_output
    }
}

impl Connectivity for OpAlloca {
    fn value_inputs(&self) -> &[ValueInput] {
        &[]
    }

    fn value_outputs(&self) -> &[ValueOutput] {
        slice::from_ref(&self.value_output)
    }

    fn value_outputs_mut(&mut self) -> &mut [ValueOutput] {
        slice::from_mut(&mut self.value_output)
    }

    fn state(&self) -> Option<&State> {
        None
    }

    fn state_mut(&mut self) -> Option<&mut State> {
        None
    }
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct OpLoad {
    ptr_input: ValueInput,
    value_output: ValueOutput,
    state: State,
}

impl OpLoad {
    pub fn ptr_input(&self) -> &ValueInput {
        &self.ptr_input
    }

    pub fn value_output(&self) -> &ValueOutput {
        &self.value_output
    }
}

impl Connectivity for OpLoad {
    fn value_inputs(&self) -> &[ValueInput] {
        slice::from_ref(&self.ptr_input)
    }

    fn value_outputs(&self) -> &[ValueOutput] {
        slice::from_ref(&self.value_output)
    }

    fn value_outputs_mut(&mut self) -> &mut [ValueOutput] {
        slice::from_mut(&mut self.value_output)
    }

    fn state(&self) -> Option<&State> {
        Some(&self.state)
    }

    fn state_mut(&mut self) -> Option<&mut State> {
        Some(&mut self.state)
    }
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct OpStore {
    value_inputs: [ValueInput; 2],
    state: State,
}

impl OpStore {
    pub fn ptr_input(&self) -> &ValueInput {
        &self.value_inputs[0]
    }

    pub fn value_input(&self) -> &ValueInput {
        &self.value_inputs[1]
    }
}

impl Connectivity for OpStore {
    fn value_inputs(&self) -> &[ValueInput] {
        &self.value_inputs
    }

    fn value_outputs(&self) -> &[ValueOutput] {
        &[]
    }

    fn value_outputs_mut(&mut self) -> &mut [ValueOutput] {
        &mut []
    }

    fn state(&self) -> Option<&State> {
        Some(&self.state)
    }

    fn state_mut(&mut self) -> Option<&mut State> {
        Some(&mut self.state)
    }
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct OpPtrElementPtr {
    inputs: Vec<ValueInput>,
    output: ValueOutput,
}

impl OpPtrElementPtr {
    pub fn ptr(&self) -> &ValueInput {
        &self.inputs[0]
    }

    pub fn indices(&self) -> &[ValueInput] {
        &self.inputs[1..]
    }

    pub fn output(&self) -> &ValueOutput {
        &self.output
    }
}

impl Connectivity for OpPtrElementPtr {
    fn value_inputs(&self) -> &[ValueInput] {
        &self.value_inputs()
    }

    fn value_outputs(&self) -> &[ValueOutput] {
        slice::from_ref(&self.output)
    }

    fn value_outputs_mut(&mut self) -> &mut [ValueOutput] {
        slice::from_mut(&mut self.output)
    }

    fn state(&self) -> Option<&State> {
        None
    }

    fn state_mut(&mut self) -> Option<&mut State> {
        None
    }
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct OpApply {
    value_inputs: Vec<ValueInput>,
    value_output: ValueOutput,
    state: State,
}

impl OpApply {
    pub fn fn_input(&self) -> &ValueInput {
        &self.value_inputs[0]
    }

    pub fn argument_inputs(&self) -> &[ValueInput] {
        &self.value_inputs[1..]
    }

    pub fn resolve_fn(&self, module: &Module) -> Function {
        *module.ty[self.value_inputs[0].ty].expect_fn()
    }
}

impl Connectivity for OpApply {
    fn value_inputs(&self) -> &[ValueInput] {
        &self.value_inputs
    }

    fn value_outputs(&self) -> &[ValueOutput] {
        slice::from_ref(&self.value_output)
    }

    fn value_outputs_mut(&mut self) -> &mut [ValueOutput] {
        slice::from_mut(&mut self.value_output)
    }

    fn state(&self) -> Option<&State> {
        Some(&self.state)
    }

    fn state_mut(&mut self) -> Option<&mut State> {
        Some(&mut self.state)
    }
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct OpUnary {
    operator: UnaryOperator,
    input: ValueInput,
    output: ValueOutput,
}

impl OpUnary {
    pub fn operator(&self) -> UnaryOperator {
        self.operator
    }

    pub fn input(&self) -> &ValueInput {
        &self.input
    }

    pub fn output(&self) -> &ValueOutput {
        &self.output
    }
}

impl Connectivity for OpUnary {
    fn value_inputs(&self) -> &[ValueInput] {
        slice::from_ref(&self.input)
    }

    fn value_outputs(&self) -> &[ValueOutput] {
        slice::from_ref(&self.output)
    }

    fn value_outputs_mut(&mut self) -> &mut [ValueOutput] {
        slice::from_mut(&mut self.output)
    }

    fn state(&self) -> Option<&State> {
        None
    }

    fn state_mut(&mut self) -> Option<&mut State> {
        None
    }
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct OpBinary {
    operator: BinaryOperator,
    inputs: [ValueInput; 2],
    output: ValueOutput,
}

impl OpBinary {
    pub fn operator(&self) -> BinaryOperator {
        self.operator
    }

    pub fn lhs_input(&self) -> &ValueInput {
        &self.inputs[0]
    }

    pub fn rhs_input(&self) -> &ValueInput {
        &self.inputs[1]
    }

    pub fn output(&self) -> &ValueOutput {
        &self.output
    }
}

impl Connectivity for OpBinary {
    fn value_inputs(&self) -> &[ValueInput] {
        &self.inputs
    }

    fn value_outputs(&self) -> &[ValueOutput] {
        slice::from_ref(&self.output)
    }

    fn value_outputs_mut(&mut self) -> &mut [ValueOutput] {
        slice::from_mut(&mut self.output)
    }

    fn state(&self) -> Option<&State> {
        None
    }

    fn state_mut(&mut self) -> Option<&mut State> {
        None
    }
}

macro_rules! gen_const_nodes {
    ($($name:ident: $ty:ident,)*) => {
        $(
            #[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
            pub struct $name {
                value: $ty,
                output: ValueOutput,
            }

            impl $name {
                pub fn value(&self) -> $ty {
                    self.value
                }

                pub fn output(&self) -> &ValueOutput {
                    &self.output
                }
            }

            impl Connectivity for $name {
                fn value_inputs(&self) -> &[ValueInput] {
                    &[]
                }

                fn value_outputs(&self) -> &[ValueOutput] {
                    slice::from_ref(&self.output)
                }

                fn value_outputs_mut(&mut self) -> &mut [ValueOutput] {
                    slice::from_mut(&mut self.output)
                }

                fn state(&self) -> Option<&State> {
                    None
                }

                fn state_mut(&mut self) -> Option<&mut State> {
                    None
                }
            }
        )*
    };
}

gen_const_nodes! {
    ConstU32: u32,
    ConstI32: i32,
    ConstF32: f32,
    ConstBool: bool,
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct ConstPtr {
    base: ValueInput,
    output: ValueOutput,
    offset: u32,
    ty: Type,
}

impl ConstPtr {
    pub fn base(&self) -> &ValueInput {
        &self.base
    }

    pub fn offset(&self) -> u32 {
        self.offset
    }

    pub fn ty(&self) -> Type {
        self.ty
    }
}

impl Connectivity for ConstPtr {
    fn value_inputs(&self) -> &[ValueInput] {
        slice::from_ref(&self.base)
    }

    fn value_outputs(&self) -> &[ValueOutput] {
        slice::from_ref(&self.output)
    }

    fn value_outputs_mut(&mut self) -> &mut [ValueOutput] {
        slice::from_mut(&mut self.output)
    }

    fn state(&self) -> Option<&State> {
        None
    }

    fn state_mut(&mut self) -> Option<&mut State> {
        None
    }
}

macro_rules! gen_simple_node {
    ($($ty:ident,)*) => {
        #[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
        pub enum SimpleNode {
            $($ty($ty)),*
        }

        impl Connectivity for SimpleNode {
            fn value_inputs(&self) -> &[ValueInput] {
                match self {
                    $(SimpleNode::$ty(n) => n.value_inputs()),*
                }
            }

            fn value_outputs(&self) -> &[ValueOutput] {
                match self {
                    $(SimpleNode::$ty(n) => n.value_outputs()),*
                }
            }

            fn value_outputs_mut(&mut self) -> &mut [ValueOutput] {
                match self {
                    $(SimpleNode::$ty(n) => n.value_outputs_mut()),*
                }
            }

            fn state(&self) -> Option<&State> {
                match self {
                    $(SimpleNode::$ty(n) => n.state()),*
                }
            }

            fn state_mut(&mut self) -> Option<&mut State> {
                match self {
                    $(SimpleNode::$ty(n) => n.state_mut()),*
                }
            }
        }

        $(impl From<$ty> for SimpleNode {
            fn from(v: $ty) -> Self {
                SimpleNode::$ty(v)
            }
        })*
    };
}

gen_simple_node! {
    ConstU32,
    ConstI32,
    ConstF32,
    ConstBool,
    ConstPtr,
    OpAlloca,
    OpLoad,
    OpStore,
    OpPtrElementPtr,
    OpApply,
    OpUnary,
    OpBinary,
}

macro_rules! add_const_methods {
    ($($method:ident $node:ident $ty:ident $ty_handle:ident,)*) => {
        $(
            pub fn $method(&mut self, region: Region, value: $ty) -> Node {
                let node = self.nodes.insert(NodeData {
                    kind: NodeKind::Simple(
                        $node {
                            value,
                            output: ValueOutput::new(Some($ty_handle)),
                        }
                        .into(),
                    ),
                    region: Some(region),
                });

                self.regions[region].nodes.insert(node);

                node
            }
        )*
    };
}

#[derive(Clone, Default, Serialize, Deserialize, Debug)]
pub struct Rvsdg {
    regions: SlotMap<Region, RegionData>,
    nodes: SlotMap<Node, NodeData>,
}

impl Rvsdg {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn register_uniform_binding(&mut self, module: &Module, binding: UniformBinding) -> Node {
        let ty = module.uniform_bindings[binding].ty;

        self.nodes.insert(NodeData {
            kind: NodeKind::UniformBinding(UniformBindingNode {
                binding,
                output: ValueOutput::new(Some(ty)),
            }),
            region: None,
        })
    }

    pub fn register_storage_binding(&mut self, module: &Module, binding: StorageBinding) -> Node {
        let ty = module.storage_bindings[binding].ty;

        self.nodes.insert(NodeData {
            kind: NodeKind::StorageBinding(StorageBindingNode {
                binding,
                output: ValueOutput::new(Some(ty)),
            }),
            region: None,
        })
    }

    pub fn register_workgroup_binding(
        &mut self,
        module: &Module,
        binding: WorkgroupBinding,
    ) -> Node {
        let ty = module.workgroup_bindings[binding].ty;

        self.nodes.insert(NodeData {
            kind: NodeKind::WorkgroupBinding(WorkgroupBindingNode {
                binding,
                output: ValueOutput::new(Some(ty)),
            }),
            region: None,
        })
    }

    pub fn register_function(
        &mut self,
        module: &Module,
        function: Function,
        dependencies: impl IntoIterator<Item = Node>,
    ) -> (Node, Region) {
        let sig = &module.fn_sigs[function];

        let dependencies: Vec<ValueInput> = dependencies
            .into_iter()
            .map(|dep| {
                let ty = self[dep].value_outputs()[0]
                    .ty
                    .expect("expected dependency to by typed");

                ValueInput {
                    ty,
                    origin: ValueOrigin::Output {
                        producer: dep,
                        output: 0,
                    },
                }
            })
            .collect();

        let mut region_arguments: Vec<ValueOutput> = dependencies
            .iter()
            .map(|d| ValueOutput::new(Some(d.ty)))
            .collect();

        region_arguments.extend(sig.args.iter().map(|a| ValueOutput::new(Some(a.ty))));

        let region_results = sig
            .ret_ty
            .iter()
            .map(|ty| ValueInput::placeholder(*ty))
            .collect();
        let region = self.regions.insert(RegionData {
            owner: None,
            nodes: Default::default(),
            value_arguments: region_arguments,
            value_results: region_results,
            state_argument: StateUser::Result,
            state_result: StateOrigin::Argument,
        });

        let node = self.nodes.insert(NodeData {
            kind: NodeKind::Function(FunctionNode {
                dependencies,
                output: ValueOutput::new(Some(sig.ty)),
                region,
            }),
            region: None,
        });

        self.regions[region].owner = Some(node);

        (node, region)
    }

    fn validate_node_value_input(&self, region: Region, value_input: &ValueInput) {
        match &value_input.origin {
            ValueOrigin::Argument(i) => {
                let region = &self[region];

                if let Some(a) = region.value_arguments().get(*i as usize) {
                    if Some(value_input.ty) != a.ty {
                        panic!("cannot connect a node input of type `{:?}` to a region argument of type `{:?}", value_input.ty, a.ty);
                    }
                } else {
                    panic!("tried to connect to region argument `{}`, but region only has {} arguments", i, region.value_arguments().len());
                }
            }
            ValueOrigin::Output { producer, output } => {
                let producer = &self[*producer];

                if producer.region != Some(region) {
                    panic!("cannot connect a node input to a node output in a different region");
                }

                if let Some(output) = producer.value_outputs().get(*output as usize) {
                    if Some(value_input.ty) != output.ty {
                        panic!(
                            "cannot connect a node input of type `{:?}` to an output of type `{:?}",
                            value_input.ty, output.ty
                        );
                    }
                } else {
                    panic!(
                        "tried to connect to node output `{}`, but the target only has {} outputs",
                        output,
                        producer.value_outputs().len()
                    );
                }
            }
        }
    }

    fn insert_state(&mut self, region: Region, node: Node, state_origin: StateOrigin) {
        fn adjust_user_origin(
            rvsdg: &mut Rvsdg,
            region: Region,
            node: Node,
            state_user: StateUser,
        ) {
            match state_user {
                StateUser::Result => rvsdg.regions[region].state_result = StateOrigin::Node(node),
                StateUser::Node(n) => {
                    rvsdg.nodes[n].state_mut().unwrap().origin = StateOrigin::Node(node)
                }
            }
        }

        let state_user = match state_origin {
            StateOrigin::Argument => {
                let state_user = self[region].state_argument;

                self.regions[region].state_argument = StateUser::Node(node);
                adjust_user_origin(self, region, node, state_user);

                state_user
            }
            StateOrigin::Node(n) => {
                let state_user = self[n].state().unwrap().user;

                self.nodes[n].state_mut().unwrap().user = StateUser::Node(node);
                adjust_user_origin(self, region, node, state_user);

                state_user
            }
        };

        self.nodes[node].state_mut().unwrap().user = state_user;
    }

    /// Adds a switch node to the given `region`.
    ///
    /// Must supply the `value_inputs` and `value_outputs` for the node at creation. May optionally
    /// supply a `state_origin`: if supplied, then the node will be inserted into the state chain
    /// between the origin and the origin's prior user; if `None` the switch node will not be part
    /// of the state chain.
    ///
    /// The first of the `value_inputs` must be the predicate that selects which branch will be
    /// taken.
    ///
    /// The branch regions for the switch node are added after the creation of the switch node,
    /// by calling [add_switch_branch] with the [Node] handle return from this [add_switch]
    /// operation.
    pub fn add_switch(
        &mut self,
        region: Region,
        value_inputs: Vec<ValueInput>,
        value_outputs: Vec<ValueOutput>,
        state_origin: Option<StateOrigin>,
    ) -> Node {
        assert!(
            value_inputs.len() > 0,
            "a switch node must specify at least 1 value input that acts as the branch selector \
            predicate"
        );

        for input in &value_inputs {
            self.validate_node_value_input(region, input);
        }

        let node = self.nodes.insert(NodeData {
            kind: NodeKind::Switch(SwitchNode {
                value_inputs,
                value_outputs,
                branches: vec![],
                state: state_origin.map(|origin| State {
                    origin,
                    user: StateUser::Result, // Temporary value
                }),
            }),
            region: Some(region),
        });

        if let Some(state_origin) = state_origin {
            self.insert_state(region, node, state_origin);
        }

        self.regions[region].nodes.insert(node);

        node
    }

    /// Adds a branch region to the given [switch_node].
    pub fn add_switch_branch(&mut self, switch_node: Node) -> Region {
        let data = self.nodes[switch_node].kind.expect_switch_mut();

        let region = self.regions.insert(RegionData {
            owner: Some(switch_node),
            nodes: Default::default(),
            value_arguments: data
                .value_inputs
                .iter()
                // The first input is the predicate that selects which branch will be taken; this
                // input is not to be passed on to the region as an argument, so we skip it.
                .skip(1)
                .map(|input| ValueOutput {
                    ty: Some(input.ty),
                    users: Default::default(),
                })
                .collect(),
            value_results: data
                .value_outputs
                .iter()
                .map(|output| ValueInput {
                    ty: output
                        .ty
                        .expect("expect output of a node that contains a region to be typed"),
                    origin: ValueOrigin::placeholder(),
                })
                .collect(),
            state_argument: StateUser::Result,
            state_result: StateOrigin::Argument,
        });

        data.branches.push(region);

        region
    }

    pub fn add_loop(
        &mut self,
        region: Region,
        value_inputs: Vec<ValueInput>,
        state_origin: Option<StateOrigin>,
    ) -> (Node, Region) {
        for input in &value_inputs {
            self.validate_node_value_input(region, input);
        }

        // The output signature and the contained region arguments and results all match the
        // signature of a loop node's inputs, so we can derive all of these from the `value_inputs`.

        let value_outputs = value_inputs
            .iter()
            .map(|input| ValueOutput::new(Some(input.ty)))
            .collect::<Vec<_>>();

        let mut contained_region_results = vec![ValueInput {
            // First result of the region in the re-entry predicate
            ty: TY_U32,
            origin: ValueOrigin::placeholder(),
        }];

        // The remaining results match the input signature
        contained_region_results.extend(value_inputs.iter().map(|input| ValueInput {
            ty: input.ty,
            origin: ValueOrigin::placeholder(),
        }));

        let contained_region = self.regions.insert(RegionData {
            owner: None,
            nodes: Default::default(),
            value_arguments: value_outputs.clone(),
            value_results: contained_region_results,
            state_argument: StateUser::Result,
            state_result: StateOrigin::Argument,
        });

        let node = self.nodes.insert(NodeData {
            kind: NodeKind::Loop(LoopNode {
                value_inputs,
                value_outputs,
                state: state_origin.map(|origin| State {
                    origin,
                    user: StateUser::Result, // Temporary value
                }),
                region: contained_region,
            }),
            region: Some(region),
        });

        self.regions[contained_region].owner = Some(node);

        if let Some(state_origin) = state_origin {
            self.insert_state(region, node, state_origin);
        }

        self.regions[region].nodes.insert(node);

        (node, contained_region)
    }

    add_const_methods! {
        add_const_u32 ConstU32 u32 TY_U32,
        add_const_i32 ConstI32 i32 TY_I32,
        add_const_f32 ConstF32 f32 TY_F32,
        add_const_bool ConstBool bool TY_BOOL,
    }

    pub fn add_const_ptr(
        &mut self,
        region: Region,
        base: ValueInput,
        offset: u32,
        ty: Type,
    ) -> Node {
        let node = self.nodes.insert(NodeData {
            kind: NodeKind::Simple(
                ConstPtr {
                    base,
                    output: ValueOutput::new(Some(TY_PTR)),
                    offset,
                    ty,
                }
                .into(),
            ),
            region: Some(region),
        });

        self.regions[region].nodes.insert(node);

        node
    }

    pub fn add_op_alloca(&mut self, region: Region) -> Node {
        let node = self.nodes.insert(NodeData {
            kind: NodeKind::Simple(
                OpAlloca {
                    value_output: ValueOutput::new(None),
                }
                .into(),
            ),
            region: Some(region),
        });

        self.regions[region].nodes.insert(node);

        node
    }

    pub fn add_op_load(
        &mut self,
        region: Region,
        ptr_input: ValueInput,
        output_ty: Type,
        state_origin: StateOrigin,
    ) -> Node {
        self.validate_node_value_input(region, &ptr_input);

        let node = self.nodes.insert(NodeData {
            kind: NodeKind::Simple(
                OpLoad {
                    ptr_input: ptr_input,
                    value_output: ValueOutput::new(Some(output_ty)),
                    state: State {
                        origin: state_origin,
                        user: StateUser::Result, // Temp value
                    },
                }
                .into(),
            ),
            region: Some(region),
        });

        self.insert_state(region, node, state_origin);
        self.regions[region].nodes.insert(node);

        node
    }

    pub fn add_op_store(
        &mut self,
        region: Region,
        ptr_input: ValueInput,
        value_input: ValueInput,
        state_origin: StateOrigin,
    ) -> Node {
        self.validate_node_value_input(region, &ptr_input);
        self.validate_node_value_input(region, &value_input);

        let node = self.nodes.insert(NodeData {
            kind: NodeKind::Simple(
                OpStore {
                    value_inputs: [ptr_input, value_input],
                    state: State {
                        origin: state_origin,
                        user: StateUser::Result, // Temp value
                    },
                }
                .into(),
            ),
            region: Some(region),
        });

        self.insert_state(region, node, state_origin);
        self.regions[region].nodes.insert(node);

        node
    }

    pub fn add_op_ptr_element_ptr(
        &mut self,
        region: Region,
        ptr_input: ValueInput,
        index_inputs: impl IntoIterator<Item = ValueInput>,
    ) -> Node {
        let mut inputs = vec![ptr_input];

        inputs.extend(index_inputs);

        let node = self.nodes.insert(NodeData {
            kind: NodeKind::Simple(
                OpPtrElementPtr {
                    inputs,
                    output: ValueOutput::new(Some(TY_PTR)),
                }
                .into(),
            ),
            region: Some(region),
        });

        self.regions[region].nodes.insert(node);

        node
    }

    pub fn add_op_apply(
        &mut self,
        module: &Module,
        region: Region,
        fn_input: ValueInput,
        argument_inputs: impl IntoIterator<Item = ValueInput>,
        state_origin: StateOrigin,
    ) -> Node {
        let function = module.ty[fn_input.ty].expect_fn();
        let ret_ty = module.fn_sigs[*function].ret_ty;

        let mut value_inputs = vec![fn_input];

        value_inputs.extend(argument_inputs);

        let node = self.nodes.insert(NodeData {
            kind: NodeKind::Simple(
                OpApply {
                    value_inputs,
                    value_output: ValueOutput::new(ret_ty),
                    state: State {
                        origin: state_origin,
                        user: StateUser::Result, // Temp value
                    },
                }
                .into(),
            ),
            region: Some(region),
        });

        self.insert_state(region, node, state_origin);
        self.regions[region].nodes.insert(node);

        node
    }

    pub fn add_op_unary(
        &mut self,
        region: Region,
        operator: UnaryOperator,
        input: ValueInput,
    ) -> Node {
        self.validate_node_value_input(region, &input);

        let node = self.nodes.insert(NodeData {
            kind: NodeKind::Simple(
                OpUnary {
                    operator,
                    input,
                    output: ValueOutput::new(Some(input.ty)),
                }
                .into(),
            ),
            region: Some(region),
        });

        self.regions[region].nodes.insert(node);

        node
    }

    pub fn add_op_binary(
        &mut self,
        region: Region,
        operator: BinaryOperator,
        lhs_input: ValueInput,
        rhs_input: ValueInput,
    ) -> Node {
        self.validate_node_value_input(region, &lhs_input);
        self.validate_node_value_input(region, &rhs_input);

        assert_eq!(lhs_input.ty, rhs_input.ty);

        let node = self.nodes.insert(NodeData {
            kind: NodeKind::Simple(
                OpBinary {
                    operator,
                    inputs: [lhs_input, rhs_input],
                    output: ValueOutput::new(Some(lhs_input.ty)),
                }
                .into(),
            ),
            region: Some(region),
        });

        self.regions[region].nodes.insert(node);

        node
    }

    pub fn reconnect_region_result(&mut self, region: Region, result: u32, input: ValueInput) {
        let old_input = self[region].value_results[result as usize];

        if old_input.ty != input.ty {
            panic!(
                "cannot connect an output of type {:?} to a region result of type {:?}",
                input.ty, old_input.ty
            );
        }

        if let ValueOrigin::Output { producer, .. } = input.origin {
            if self[producer].region != Some(region) {
                panic!("cannot connect a region result to the output of a node that does not belong to that region");
            }
        }

        // Remove the result as a user from the old origin (if any)
        if old_input.origin.is_placeholder() {
            match old_input.origin {
                ValueOrigin::Argument(i) => self.regions[region].value_arguments[i as usize]
                    .users
                    .remove(&ValueUser::Result(result)),
                ValueOrigin::Output { producer, output } => self.nodes[producer]
                    .value_outputs_mut()[output as usize]
                    .users
                    .remove(&ValueUser::Result(result)),
            }
        }

        // Add the result as a user to the new origin
        match input.origin {
            ValueOrigin::Argument(i) => self.regions[region].value_arguments[i as usize]
                .users
                .insert(ValueUser::Result(result)),
            ValueOrigin::Output { producer, output } => self.nodes[producer].value_outputs_mut()
                [output as usize]
                .users
                .insert(ValueUser::Result(result)),
        }

        // Update the result's origin
        self.regions[region].value_results[result as usize].origin = input.origin;
    }
}

impl Index<Region> for Rvsdg {
    type Output = RegionData;

    fn index(&self, region: Region) -> &Self::Output {
        &self.regions[region]
    }
}

impl Index<Node> for Rvsdg {
    type Output = NodeData;

    fn index(&self, node: Node) -> &Self::Output {
        &self.nodes[node]
    }
}
