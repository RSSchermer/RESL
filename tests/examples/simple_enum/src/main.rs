#![feature(stmt_expr_attributes)]

use risl::prelude::*;

#[shader_module]
pub mod shader {
    use risl::prelude::*;

    #[workgroup_shared]
    static VALUE: Workgroup<u32>;

    fn test(v: u32) -> Result<u32, u32> {
        if v > 10 { Ok(1) } else { Err(0) }
    }

    #[compute]
    fn main() {
        unsafe {
            *VALUE.as_mut_unchecked() = match test(*VALUE.as_ref_unchecked()) {
                Ok(v) => v,
                Err(v) => v,
            };
        }
    }
}

fn main() {}
