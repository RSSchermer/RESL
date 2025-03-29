#![feature(stmt_expr_attributes)]

#[resl::shader_module]
pub mod shader {
    struct A {
        a: u32,
        b: u32,
        c: f32,
    }

    fn test<'a>(v0: &'a A, v1: &'a A, selector: bool) -> &'a u32 {
        let mut r = &v0.b;

        if selector {
            r = &v1.b;
        }

        r
    }

    fn test1(a: u32, selector: bool) -> u32 {
        let mut r = a;

        if selector {
            r += 1;
        }

        r
    }

    fn test2(v0: &A, selector: bool) -> &u32 {
        let mut r = &v0.a;

        if selector {
            r = &v0.b;
        }

        r
    }
}

fn main() {}
