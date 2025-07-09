#![feature(stmt_expr_attributes)]

#[resl::shader_module]
pub mod shader {
    #[resl::workgroup]
    static mut VALUE: u32 = 0;

    fn test(v: u32) -> Result<u32, ()> {
        if v > 10 {
            Ok(1)
        } else {
            Err(())
        }
    }

    #[resl::compute]
    fn main() {
        unsafe {
            VALUE = if let Ok(new_value) = test(VALUE) {
                new_value
            } else {
                0
            };
        }
    }
}

fn main() {}
