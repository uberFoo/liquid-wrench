mod utils;

use {
    liquid_wrench::{
        binary::Binary,
        x86::{Disassembler as X86Disassembler, Instruction},
        Disassembler, Disassembly, Targets,
    },
    std::collections::HashMap,
    wasm_bindgen::prelude::*,
};

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
pub fn disassemble_binary(bytes: Vec<u8>) -> JsValue {
    let mut sections = HashMap::new();
    let binary = Binary::new(bytes).expect("could not parse binary");
    for (name, section) in binary.sections {
        let x86 = Box::new(X86Disassembler::new());
        let mut disassembler = Disassembler::new(Targets::X86, x86, section.bytes, section.offset);
        sections.insert(name, disassembler.disassemble());
    }
    JsValue::from_serde(&sections).unwrap()
}

#[wasm_bindgen]
pub fn print_disassembly(d: &JsValue) -> String {
    let disassembly: Disassembly<Instruction> =
        d.into_serde().expect("could not deserialize Disassembly");
    format!("{}", disassembly)
}

#[wasm_bindgen]
pub fn print_instruction(i: &JsValue) -> String {
    let instr: Instruction = i.into_serde().expect("could not deserialize Instruction");
    format!("{}", instr)
}
