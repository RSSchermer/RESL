#![feature(stmt_expr_attributes)]

#[resl::shader_module]
pub mod shader {
    #[resl::workgroup]
    static mut VALUE: u32 = 0;

    #[resl::compute]
    fn main() {
        let data = [10, 20, 30, 40];

        if let Some(slice) = data.get(1..3) {
            let value = if let Some(element) = slice.get(1) {
                *element
            } else {
                0
            };

            unsafe {
                VALUE = value;
            }
        }
    }
}

fn main() {}
