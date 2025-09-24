#![feature(stmt_expr_attributes)]

use resl::prelude::*;

#[shader_module]
pub mod shader {
    use resl::prelude::*;

    struct A {
        a: u32,
        b: u32,
    }

    #[workgroup_shared]
    static VALUE_0: Workgroup<A> = workgroup!(A { a: 0, b: 1 });

    #[compute]
    fn test0(v: u32) {
        unsafe {
            VALUE_0.as_mut_unchecked().b += v;
        }
    }
}

fn main() {}
