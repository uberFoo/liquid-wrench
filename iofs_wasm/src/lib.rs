use {
    lazy_static::lazy_static,
    liquid_wrench::{
        binary::Binary,
        x86::{Disassembler as X86Disassembler, Instruction},
        Disassembler, Disassembly, Targets,
    },
    mut_static::MutStatic,
    std::{
        collections::HashMap,
        path::{Path, PathBuf},
    },
    wasm_exports::*,
};

lazy_static! {
    pub static ref DISASSEMBLER: MutStatic<IOFSDisassembler> =
        { MutStatic::from(IOFSDisassembler::new()) };
}

pub struct IOFSDisassembler {
    root_id: Option<String>,
    file_to_disassemble: Option<FileCreate>,
}

impl IOFSDisassembler {
    fn new() -> Self {
        IOFSDisassembler {
            root_id: None,
            file_to_disassemble: None,
        }
    }

    pub fn disassemble_binary(&self, bytes: Vec<u8>) -> HashMap<String, Disassembly<Instruction>> {
        let mut sections = HashMap::new();
        let binary = Binary::new(bytes).expect("could not parse binary");
        for (name, section) in binary.sections {
            let x86 = Box::new(X86Disassembler::new());
            let mut disassembler =
                Disassembler::new(Targets::X86, x86, section.bytes, section.offset);
            sections.insert(name, disassembler.disassemble());
        }
        sections
    }
}

#[no_mangle]
pub extern "C" fn init(root_id: RefStr) {
    // initialize our struct reference
    let mut wc = DISASSEMBLER.write().unwrap();

    // Store the root id
    wc.root_id = Some(root_id.get_str().to_string());

    // Register callbacks
    register_callback(WasmMessage::FileCreate, handle_file_create);
    register_callback(WasmMessage::FileClose, handle_file_close);
}

#[no_mangle]
pub extern "C" fn handle_file_create(payload: Option<MessagePayload>) {
    if let Some(MessagePayload::FileCreate(file)) = payload {
        let mut d = DISASSEMBLER.write().unwrap();

        let file = file.unpack();
        let file_p = Path::new(&file.path);

        match file_p.extension() {
            Some(e) => {
                if e == "exe" {
                    d.file_to_disassemble = Some(file);
                }
            }
            None => (),
        }
    }
}

#[no_mangle]
pub extern "C" fn handle_file_close(_payload: Option<MessagePayload>) {
    let mut d = DISASSEMBLER.write().unwrap();

    if let Some(file) = &d.file_to_disassemble.take() {
        print(&format!("attempting to disassemble {}", file.path));

        let mut binary: Vec<u8> = vec![];
        let mut buf: [u8; 2048] = [0; 2048];
        let mut offset = 0;
        let handle = open_file(&file.id).unwrap();
        let mut l = read_file(&file.id, handle, offset, &mut buf);
        print(&format!("read {} bytes", l));
        binary.extend_from_slice(&buf);
        while l > 0 {
            offset += l;
            l = read_file(&file.id, handle, offset, &mut buf);
            print(&format!("read {} bytes", l));
            binary.extend_from_slice(&buf);
        }

        print("disassembling binary");
        let disassembled_sections = d.disassemble_binary(binary);

        let mut out_path = PathBuf::from(&file.path);
        let out_path = out_path.with_extension("disassembly");
        print(&format!("writing disassembly to {:?}", out_path));

        if let Some(root_id) = &d.root_id {
            if let Some(h) = create_file(&root_id, out_path.file_name().unwrap().to_str().unwrap())
            {
                let mut offset = 0;
                for (name, disassembly) in disassembled_sections {
                    offset += write_file(
                        &h.id,
                        h.handle,
                        offset,
                        &format!("Disassembly of section {:?}\n", name).as_bytes(),
                    );

                    offset += write_file(
                        &h.id,
                        h.handle,
                        offset,
                        &format!("{}\n", disassembly).as_bytes(),
                    );
                }
                close_file(&h.id, h.handle);
            }
        }
    }
}
