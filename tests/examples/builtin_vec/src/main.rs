#![feature(stmt_expr_attributes)]

use resl::prelude::*;

#[shader_module]
pub mod shader {
    use resl::prelude::*;

    #[workgroup_shared]
    static VALUE_0: Workgroup<vec2_f32> = workgroup!(vec2_f32(0.0, 1.0));

    #[compute]
    fn main() {
        unsafe {
            if *VALUE_0.as_ref_unchecked() == vec2_f32(0.0, 1.0) {
                *VALUE_0.as_mut_unchecked() += vec2_f32(1.0, 1.0);
            }
        }
    }
}

fn main() {}
