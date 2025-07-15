#![feature(stmt_expr_attributes)]

#[resl::shader_module]
pub mod shader {
    #[resl::workgroup]
    static mut VALUE: u32 = 0;

    #[resl::compute]
    fn main() {
        let data = [10, 20, 30, 40];

        let value = if let Some(element) = data.get(1) {
            *element
        } else {
            0
        };

        unsafe {
            VALUE = value;
        }
    }
}

fn main() {}
