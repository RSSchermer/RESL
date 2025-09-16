#![feature(stmt_expr_attributes)]

#[resl::shader_module]
pub mod shader {
    #[resl::workgroup]
    static VALUES: [u32; 4] = [10, 20, 30, 40];

    #[resl::workgroup]
    static mut VALUE: u32 = 0;

    #[resl::compute]
    fn entry_point_local_range() {
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
    
    #[resl::compute]
    fn entry_point_global_range() {
        if let Some(slice) = VALUES.get(1..3) {
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

    #[resl::compute]
    fn entry_point_slice_destructuring() {
        let slice = &VALUES;

        if let [a, tail @ ..] = slice {
            let b = tail.get(0).copied().unwrap_or(10);

            unsafe {
                VALUE = *a + b;
            }
        }
    }
}

fn main() {}
