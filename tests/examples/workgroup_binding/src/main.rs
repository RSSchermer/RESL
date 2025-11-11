#![feature(stmt_expr_attributes)]

use risl::prelude::*;

#[shader_module]
pub mod shader {
    use risl::prelude::*;

    struct A {
        _a: u32,
        b: u32,
    }

    #[workgroup_shared]
    static VALUE_0: Workgroup<A>;

    #[resource(group = 0, binding = 0)]
    static VALUES: Storage<[u32]>;

    #[compute]
    fn main(index: usize) {
        if let Some(value) = VALUES.get(index) {
            unsafe {
                VALUE_0.as_mut_unchecked().b += *value;
            }
        }
    }
}

fn main() {}
