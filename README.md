# The Rust-Integrated Shading Language

WIP

Test-build command template:

```shell
cargo build --bin=rislc --release && RUSTC_WRAPPER=target/release/rislc cargo build --package=test-example-hello-world
```
