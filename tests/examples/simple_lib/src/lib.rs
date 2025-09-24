use resl::prelude::*;

#[gpu]
pub fn or_3(a: u32, b: u32, c: u32) -> u32 {
    a | b | c
}
