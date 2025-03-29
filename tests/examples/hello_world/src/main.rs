#![feature(stmt_expr_attributes)]

#[resl::gpu]
pub fn or_3(a: u32, b: u32, c: u32) -> u32 {
    a | b | c
}

#[resl::shader_module]
pub mod shader {
    fn or_4(a: u32, b: u32, c: u32, d: u32) -> u32 {
        simple_lib::or_3(a, b, c) | d
    }

    fn test0(v: bool) -> u32 {
        let mut i = 1;

        if v {
            i = 2;
        }

        i
    }
}

fn main() {}
