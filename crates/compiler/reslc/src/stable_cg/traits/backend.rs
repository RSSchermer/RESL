use super::CodegenObject;

pub trait BackendTypes {
    type Value: CodegenObject;
    type Local: CodegenObject;
    type Function: CodegenObject;
    type BasicBlock: Copy;
    type Type: CodegenObject;
}
