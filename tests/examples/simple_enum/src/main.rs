#![feature(stmt_expr_attributes)]

use resl::prelude::*;

#[shader_module]
pub mod shader {
    use resl::prelude::*;

    #[workgroup_shared]
    static VALUE: Workgroup<u32> = workgroup!(0);

    fn test(v: u32) -> Result<u32, ()> {
        if v > 10 {
            Ok(1)
        } else {
            Err(())
        }
    }

    #[compute]
    fn main() {
        unsafe {
            *VALUE.as_mut_unchecked() = if let Ok(new_value) = test(*VALUE.as_ref_unchecked()) {
                new_value
            } else {
                0
            };
        }
    }
}

fn main() {}
