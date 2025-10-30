use std::fmt::Write;
use std::hash::Hash;

use indexmap::IndexSet;

use crate::scf::Scf;
use crate::ty::{
    ScalarKind, Struct, StructField, Type, TypeKind, TypeRegistry, Vector, VectorSize,
};
use crate::{
    ty, BlendSrc, Constant, ConstantKind, ConstantRegistry, Function, Interpolation,
    InterpolationSampling, InterpolationType, ResourceBinding, ShaderIOBinding, StorageBinding,
    StorageBindingRegistry, UniformBinding, UniformBindingRegistry, WorkgroupBinding,
    WorkgroupBindingRegistry,
};

const INDENT: &'static str = "    ";

struct IdWriter<K> {
    mapping: IndexSet<K>,
    prefix: &'static str,
}

impl<K> IdWriter<K>
where
    K: Eq + Hash,
{
    fn new(prefix: &'static str) -> Self {
        Self {
            mapping: Default::default(),
            prefix,
        }
    }

    fn write<W: Write>(&mut self, w: &mut W, key: K) {
        let (id, _) = self.mapping.insert_full(key);

        w.write_str(self.prefix);
        write!(w, "{}", id);
    }
}

struct WgslWriter {
    w: String,
    struct_id_writer: IdWriter<Type>,
    uniform_binding_id_writer: IdWriter<UniformBinding>,
    storage_binding_id_writer: IdWriter<StorageBinding>,
    workgroup_binding_id_writer: IdWriter<WorkgroupBinding>,
    constant_id_writer: IdWriter<Constant>,
    function_id_writer: IdWriter<Function>,
    indent_level: usize,
}

impl WgslWriter {
    fn new() -> Self {
        Self {
            w: String::new(),
            struct_id_writer: IdWriter::new("S"),
            uniform_binding_id_writer: IdWriter::new("u"),
            storage_binding_id_writer: IdWriter::new("s"),
            workgroup_binding_id_writer: IdWriter::new("w"),
            constant_id_writer: IdWriter::new("C"),
            function_id_writer: IdWriter::new("f"),
            indent_level: 0,
        }
    }

    fn increment_indent(&mut self) {
        self.indent_level += 1;
    }

    fn decrement_indent(&mut self) {
        self.indent_level -= 1;
    }

    fn write_newline(&mut self) {
        self.w.push_str("\n");

        for _ in 0..self.indent_level {
            self.w.push_str(INDENT);
        }
    }

    fn write_block_start(&mut self) {
        self.w.push_str("{");
    }

    fn write_block_end(&mut self) {
        self.w.push_str("}");
    }

    fn write_optional_space(&mut self) {
        self.w.push_str(" ");
    }

    fn write_struct_def(&mut self, registry: &TypeRegistry, ty: Type) {
        self.w.push_str("struct ");
        self.struct_id_writer.write(&mut self.w, ty);
        self.write_optional_space();
        self.write_block_start();
        self.increment_indent();
        self.write_newline();

        for (index, field) in registry.kind(ty).expect_struct().fields.iter().enumerate() {
            // TODO: field offsets, but only for ABI compatible types (which notably does not
            // include booleans, as the size does not match; 1 byte CPU-side, 4 bytes GPU-side)
            write!(&mut self.w, "_{}:", index).unwrap();
            self.write_optional_space();
            self.write_type(registry, field.ty);
            self.w.push_str(",");
            self.write_newline();
        }

        self.decrement_indent();
        self.write_block_end();
        self.write_newline();
    }

    fn write_struct_field(&mut self, registry: &TypeRegistry, index: usize, field: &StructField) {
        // TODO: field offsets, but only for ABI compatible types (which notably does not
        // include booleans, as the size does not match; 1 byte CPU-side, 4 bytes GPU-side)

        if let Some(io_binding) = &field.io_binding {
            self.write_shader_io_binding(io_binding);
            self.write_newline();
        }

        self.write_struct_field_ident(index);
        self.w.push_str(":");
        self.write_optional_space();
        self.write_type(registry, field.ty);
        self.w.push_str(",");
        self.write_newline();
    }

    fn write_struct_field_ident(&mut self, index: usize) {
        write!(&mut self.w, "_{index}").unwrap();
    }

    fn write_shader_io_binding(&mut self, io_binding: &ShaderIOBinding) {
        match io_binding {
            ShaderIOBinding::VertexIndex => self.w.push_str("@builtin(vertex_index)"),
            ShaderIOBinding::InstanceIndex => self.w.push_str("@builtin(instance_index)"),
            ShaderIOBinding::Position { invariant } => {
                self.w.push_str("@builtin(position)");

                if *invariant {
                    self.write_optional_space();
                    self.w.push_str("@invariant");
                }
            }
            ShaderIOBinding::FrontFacing => self.w.push_str("@builtin(front_facing)"),
            ShaderIOBinding::FragDepth => self.w.push_str("@builtin(frag_depth)"),
            ShaderIOBinding::SampleIndex => self.w.push_str("@builtin(sample_index)"),
            ShaderIOBinding::SampleMask => self.w.push_str("@builtin(sample_mask)"),
            ShaderIOBinding::LocalInvocationId => self.w.push_str("@builtin(local_invocation_id)"),
            ShaderIOBinding::LocalInvocationIndex => {
                self.w.push_str("@builtin(local_invocation_index)")
            }
            ShaderIOBinding::GlobalInvocationId => {
                self.w.push_str("@builtin(global_invocation_id)")
            }
            ShaderIOBinding::WorkgroupId => self.w.push_str("@builtin(workgroup_id)"),
            ShaderIOBinding::NumWorkgroups => self.w.push_str("@builtin(num_workgroups)"),
            ShaderIOBinding::Location {
                location,
                blend_src,
                interpolation,
            } => {
                write!(&mut self.w, "@location{location}");

                if let Some(blend_src) = blend_src {
                    self.write_optional_space();
                    self.write_blend_src_attribute(blend_src);
                }

                if let Some(interpolation) = interpolation {
                    self.write_optional_space();
                    self.write_interpolation_attribute(interpolation);
                }
            }
        }
    }

    fn write_blend_src_attribute(&mut self, blend_src: &BlendSrc) {
        match blend_src {
            BlendSrc::Zero => self.w.push_str("@blend_src(0)"),
            BlendSrc::One => self.w.push_str("@blend_src(1)"),
        }
    }

    fn write_interpolation_attribute(&mut self, interpolation: &Interpolation) {
        self.w.push_str("@interpolate(");
        self.write_interpolation_type(interpolation.tpe);

        if let Some(sampling) = interpolation.sampling {
            self.w.push_str(",");
            self.write_optional_space();
            self.write_interpolation_sampling(sampling);
        }
    }

    fn write_interpolation_type(&mut self, interpolation_type: InterpolationType) {
        match interpolation_type {
            InterpolationType::Perspective => self.w.push_str("perspective"),
            InterpolationType::Linear => self.w.push_str("linear"),
            InterpolationType::Flat => self.w.push_str("flat"),
        }
    }

    fn write_interpolation_sampling(&mut self, interpolation_sampling: InterpolationSampling) {
        match interpolation_sampling {
            InterpolationSampling::Center => self.w.push_str("center"),
            InterpolationSampling::Centroid => self.w.push_str("centroid"),
            InterpolationSampling::Sample => self.w.push_str("sample"),
            InterpolationSampling::First => self.w.push_str("first"),
            InterpolationSampling::Either => self.w.push_str("either"),
        }
    }

    fn write_type(&mut self, registry: &TypeRegistry, ty: Type) {
        match &*registry.kind(ty) {
            TypeKind::Scalar(kind) => self.write_scalar_ty(*kind),
            TypeKind::Atomic(kind) => self.write_atomic_ty(*kind),
            TypeKind::Vector(v) => self.write_vector_ty(v),
            TypeKind::Matrix(m) => self.write_matrix_ty(m),
            TypeKind::Array { element_ty, count } => {
                self.write_array_ty(registry, *element_ty, *count)
            }
            TypeKind::Slice { element_ty } => self.write_slice_ty(registry, *element_ty),
            TypeKind::Struct(_) => self.struct_id_writer.write(&mut self.w, ty),
            TypeKind::Ptr(_)
            | TypeKind::Enum(_)
            | TypeKind::Function(_)
            | TypeKind::Predicate
            | TypeKind::Dummy => panic!("type should not occur in final output"),
        }
    }

    fn write_scalar_ty(&mut self, kind: ScalarKind) {
        match kind {
            ScalarKind::I32 => self.w.push_str("i32"),
            ScalarKind::U32 => self.w.push_str("u32"),
            ScalarKind::F32 => self.w.push_str("f32"),
            ScalarKind::Bool => self.w.push_str("bool"),
        }
    }

    fn write_atomic_ty(&mut self, kind: ScalarKind) {
        self.w.push_str("atomic<");
        self.write_scalar_ty(kind);
        self.w.push_str(">");
    }

    fn write_vector_ty(&mut self, ty: &ty::Vector) {
        match ty.size {
            VectorSize::Two => self.w.push_str("vec2"),
            VectorSize::Three => self.w.push_str("vec3"),
            VectorSize::Four => self.w.push_str("vec4"),
        };

        self.w.push_str("<");
        self.write_scalar_ty(ty.scalar);
        self.w.push_str(">");
    }

    fn write_matrix_ty(&mut self, ty: &ty::Matrix) {
        let base = match (ty.rows, ty.columns) {
            (VectorSize::Two, VectorSize::Two) => "mat2x2",
            (VectorSize::Two, VectorSize::Three) => "mat2x3",
            (VectorSize::Two, VectorSize::Four) => "mat2x4",
            (VectorSize::Three, VectorSize::Two) => "mat3x2",
            (VectorSize::Three, VectorSize::Three) => "mat3x3",
            (VectorSize::Three, VectorSize::Four) => "mat3x4",
            (VectorSize::Four, VectorSize::Two) => "mat4x2",
            (VectorSize::Four, VectorSize::Three) => "mat4x3",
            (VectorSize::Four, VectorSize::Four) => "mat4x4",
        };

        self.w.push_str(base);
        self.w.push_str("<");
        self.write_scalar_ty(ty.scalar);
        self.w.push_str(">");
    }

    fn write_array_ty(&mut self, registry: &TypeRegistry, element_ty: Type, count: u64) {
        self.w.push_str("array<");
        self.write_type(registry, element_ty);
        self.w.push_str(",");
        self.write_optional_space();
        write!(&mut self.w, "{count}>").unwrap();
    }

    fn write_slice_ty(&mut self, registry: &TypeRegistry, element_ty: Type) {
        self.w.push_str("array<");
        self.write_type(registry, element_ty);
        self.w.push_str(">");
    }

    fn write_uniform_binding(
        &mut self,
        registry: &UniformBindingRegistry,
        ty: &TypeRegistry,
        binding: UniformBinding,
    ) {
        let data = &registry[binding];

        self.write_resource_binding_attributes(&data.resource_binding);
        self.write_newline();
        self.w.push_str("var<uniform>");
        self.write_optional_space();
        self.uniform_binding_id_writer.write(&mut self.w, binding);
        self.w.push_str(":");
        self.write_optional_space();
        self.write_type(ty, data.ty);
        self.w.push_str(";");
        self.write_newline();
    }

    fn write_storage_binding(
        &mut self,
        registry: &StorageBindingRegistry,
        ty: &TypeRegistry,
        binding: StorageBinding,
    ) {
        let data = &registry[binding];

        self.write_resource_binding_attributes(&data.resource_binding);
        self.write_newline();

        if data.writable {
            self.w.push_str("var<storage,read_write>");
        } else {
            self.w.push_str("var<storage>");
        }

        self.write_optional_space();
        self.storage_binding_id_writer.write(&mut self.w, binding);
        self.w.push_str(":");
        self.write_optional_space();
        self.write_type(ty, data.ty);
        self.w.push_str(";");
        self.write_newline();
    }

    fn write_resource_binding_attributes(&mut self, resource_binding: &ResourceBinding) {
        let group = resource_binding.group;
        let binding = resource_binding.binding;

        write!(&mut self.w, "@group({group})").unwrap();
        self.write_optional_space();
        write!(&mut self.w, "@binding({binding})").unwrap();
    }

    fn write_workgroup_binding(
        &mut self,
        registry: &WorkgroupBindingRegistry,
        ty: &TypeRegistry,
        binding: WorkgroupBinding,
    ) {
        let data = &registry[binding];

        self.w.push_str("var<workgroup>");
        self.write_optional_space();
        self.workgroup_binding_id_writer.write(&mut self.w, binding);
        self.w.push_str(":");
        self.write_optional_space();
        self.write_type(ty, data.ty);
        self.w.push_str(";");
        self.write_newline();
    }

    fn write_constant_binding(
        &mut self,
        type_registry: &TypeRegistry,
        constant_registry: &ConstantRegistry,
        constant: Constant,
    ) {
        let data = &constant_registry[constant];
        let ty = data.ty();

        self.w.push_str("const ");
        self.constant_id_writer.write(&mut self.w, constant);
        self.w.push_str(":");
        self.write_optional_space();
        self.write_type(type_registry, ty);
        self.write_optional_space();
        self.w.push_str("=");
        self.write_optional_space();
        self.write_constant_value(type_registry, constant_registry, constant);
        self.w.push_str(";");
        self.write_newline();
    }

    fn write_constant_value(
        &mut self,
        type_registry: &TypeRegistry,
        constant_registry: &ConstantRegistry,
        constant: Constant,
    ) {
        let data = &constant_registry[constant];
        let ty = data.ty();

        match constant_registry[constant].kind() {
            ConstantKind::ByteData(data) => {
                let mut writer = ConstantValueWriter { writer: self, data };

                writer.write_value(type_registry, ty, 0);
            }
            ConstantKind::Expression => todo!(),
        }
    }

    fn write_bool(&mut self, value: bool) {
        write!(&mut self.w, "{value}").unwrap();
    }

    fn write_u32(&mut self, value: u32) {
        write!(&mut self.w, "{value}u").unwrap();
    }

    fn write_i32(&mut self, value: i32) {
        if value == i32::MIN {
            // Per a comment in Naga, `-2147483648i` is not a valid WGSL token; `i32::MIN` can only
            // be represented with a negated abstract integer.
            write!(&mut self.w, "i32({value})").unwrap();
        } else {
            write!(&mut self.w, "{value}i").unwrap();
        }
    }

    fn write_f32(&mut self, value: f32) {
        write!(&mut self.w, "{value}f").unwrap();
    }
}

struct ConstantValueWriter<'a, 'b> {
    writer: &'a mut WgslWriter,
    data: &'b [u8],
}

impl ConstantValueWriter<'_, '_> {
    fn write_value(&mut self, ty_registry: &TypeRegistry, ty: Type, offset: usize) {
        match &*ty_registry.kind(ty) {
            TypeKind::Scalar(kind) => self.write_scalar_value(*kind, offset),
            TypeKind::Vector(v) => self.write_vector_value(v, offset),
            TypeKind::Matrix(m) => self.write_matrix_value(m, offset),
            TypeKind::Array { .. } => todo!(),
            TypeKind::Struct(_) => todo!(),
            TypeKind::Atomic(_)
            | TypeKind::Slice { .. }
            | TypeKind::Enum(_)
            | TypeKind::Ptr(_)
            | TypeKind::Function(_)
            | TypeKind::Predicate
            | TypeKind::Dummy => panic!("not a legal value type in a constant"),
        }
    }

    fn write_scalar_value(&mut self, kind: ScalarKind, offset: usize) {
        match kind {
            ScalarKind::I32 => self.writer.write_i32(self.read_i32(offset)),
            ScalarKind::U32 => self.writer.write_u32(self.read_u32(offset)),
            ScalarKind::F32 => self.writer.write_f32(self.read_f32(offset)),
            ScalarKind::Bool => self.writer.write_bool(self.read_bool(offset)),
        }
    }

    fn read_bool(&self, offset: usize) -> bool {
        self.data[offset] != 0
    }

    fn read_u32(&self, offset: usize) -> u32 {
        let bytes: [u8; 4] = self.data[offset..]
            .try_into()
            .expect("not enough data available at the `offset` to represent a u32 value");

        u32::from_ne_bytes(bytes)
    }

    fn read_i32(&self, offset: usize) -> i32 {
        let bytes: [u8; 4] = self.data[offset..]
            .try_into()
            .expect("not enough data available at the `offset` to represent a i32 value");

        i32::from_ne_bytes(bytes)
    }

    fn read_f32(&self, offset: usize) -> f32 {
        let bytes: [u8; 4] = self.data[offset..]
            .try_into()
            .expect("not enough data available at the `offset` to represent a f32 value");

        f32::from_ne_bytes(bytes)
    }

    fn write_vector_value(&mut self, ty: &ty::Vector, offset: usize) {
        self.writer.write_vector_ty(ty);
        self.writer.w.push_str("(");

        self.write_vector_element_value(ty.scalar, offset, 0);

        for index in 1..ty.size.to_usize() {
            self.writer.w.push_str(",");
            self.writer.write_optional_space();
            self.write_vector_element_value(ty.scalar, offset, index);
        }

        self.writer.w.push_str(")");
    }

    fn write_vector_element_value(
        &mut self,
        scalar_kind: ScalarKind,
        vector_offset: usize,
        index: usize,
    ) {
        match scalar_kind {
            ScalarKind::I32 => {
                let element_offset = vector_offset + index * 4;

                self.writer.write_i32(self.read_i32(element_offset));
            }
            ScalarKind::U32 => {
                let element_offset = vector_offset + index * 4;

                self.writer.write_u32(self.read_u32(element_offset));
            }
            ScalarKind::F32 => {
                let element_offset = vector_offset + index * 4;

                self.writer.write_f32(self.read_f32(element_offset));
            }
            ScalarKind::Bool => {
                let element_offset = vector_offset + index;

                self.writer.write_i32(self.read_i32(element_offset));
            }
        }
    }

    fn write_matrix_value(&mut self, ty: &ty::Matrix, offset: usize) {
        let col_ty = ty.column_vector();

        self.writer.write_matrix_ty(ty);
        self.writer.w.push_str("(");
        self.writer.increment_indent();

        let column_stride = match ty.rows {
            // 2 element vectors will always have a stride of 8 bytes, regardless of the element
            // type.
            VectorSize::Two => 8,
            // both 3 and 4 element vectors will always have a stride of 16 bytes, regardless
            // of the element type.
            VectorSize::Three | VectorSize::Four => 16,
        };

        for index in 0..ty.columns.to_usize() {
            let column_offset = offset + index * column_stride;

            self.writer.write_newline();
            self.write_vector_value(&col_ty, column_offset);
            self.writer.w.push_str(",");
        }

        self.writer.decrement_indent();
        self.writer.write_newline();
        self.writer.w.push_str(")");
    }
}
