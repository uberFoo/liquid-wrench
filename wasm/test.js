const fs = require('fs');
const wasm = require('./pkg/liquid_wrench_wasm.js');
const binary = fs.readFileSync('/bin/ls');

const sections = wasm.disassemble_binary(binary);

let text = sections['__text'];
// let instrs = text.instructions;
// for (var i in instrs) {
//     let instr = instrs[i].interpretation;
//     if (instr) {
//         console.log(wasm.print_instruction(instr));
//     }
// }

console.log(wasm.print_disassembly(text));
