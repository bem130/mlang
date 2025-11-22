extern crate alloc;

use crate::ast::DataType;
use alloc::vec::Vec;

#[derive(Clone)]
pub struct ProvidedFunction {
    pub name: &'static str,
    pub param_types: Vec<DataType>,
    pub return_type: DataType,
}

impl ProvidedFunction {
    pub fn new(name: &'static str, param_types: Vec<DataType>, return_type: DataType) -> Self {
        Self {
            name,
            param_types,
            return_type,
        }
    }
}

pub fn stdio_builtins() -> Vec<ProvidedFunction> {
    alloc::vec![
        ProvidedFunction::new("print", alloc::vec![DataType::String], DataType::Unit),
        ProvidedFunction::new("println", alloc::vec![DataType::String], DataType::Unit),
        ProvidedFunction::new("read_line", alloc::vec![], DataType::String),
    ]
}

pub fn wasm_builtins() -> Vec<ProvidedFunction> {
    alloc::vec![
        ProvidedFunction::new(
            "fd_write",
            alloc::vec![DataType::I32, DataType::I32, DataType::I32, DataType::I32],
            DataType::I32,
        ),
        ProvidedFunction::new(
            "fd_read",
            alloc::vec![DataType::I32, DataType::I32, DataType::I32, DataType::I32],
            DataType::I32,
        ),
    ]
}
