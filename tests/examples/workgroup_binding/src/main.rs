#![feature(stmt_expr_attributes)]

#[resl::shader_module]
pub mod shader {
    struct A {
        a: u32,
        b: u32,
    }

    #[resl::workgroup]
    static mut VALUE_0: A = A { a: 0, b: 1 };

    fn test0(v: u32) {
        unsafe {
            VALUE_0.b += v;
        }
    }
}

fn main() {}
