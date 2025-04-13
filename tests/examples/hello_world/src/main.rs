#![feature(stmt_expr_attributes)]

#[resl::gpu]
pub fn or_3(a: u32, b: u32, c: u32) -> u32 {
    a | b | c
}

#[resl::shader_module]
pub mod shader {
    fn external_dependency(a: u32, b: u32, c: u32, d: u32) -> u32 {
        simple_lib::or_3(a, b, c) | d
    }

    fn local_assignment(v: bool) -> u32 {
        let mut i = 1;

        if v {
            i = 2;
        }

        i
    }

    struct LargeStruct {
        f_0: u32,
        f_1: u32,
        f_2: u32,
        f_3: u32,
        f_4: u32,
        f_5: u32,
        f_6: u32,
        f_7: u32,
    }

    // rustc is expected to use and indirect argument (pass-by-pointer) and indirect return value
    // (the generated function's first argument will be a pointer into which the return value is
    // to be written, and the function will not have an actual return value).
    fn large_arg(mut data: LargeStruct, add: u32) -> LargeStruct {
        data.f_0 += add;

        data
    }

    fn slice_arg(slice: &mut [u32]) -> usize {
        slice.len()
    }

    fn unsize_array() -> usize {
        let mut arr = [0u32; 4];

        slice_arg(&mut arr)
    }

    fn closure(a: u32, b: u32, c: u32) -> u32 {
        let closure = |v| v + a + b;

        closure(c)
    }
}

fn main() {}
