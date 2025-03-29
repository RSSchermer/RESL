pub fn main() {
    let origin = if cfg!(target_os = "macos") {
        "@loader_path"
    } else {
        "$ORIGIN"
    };
    println!("cargo:rustc-link-arg-bin=reslc=-Wl,-rpath,{origin}/../toolchain/lib");
}
