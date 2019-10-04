//! Binary file parsing, and storage.
//!
use {failure::format_err, goblin::Object, std::collections::HashMap};

/// The binary being disassembled
pub struct Binary {
    /// The code sections
    pub sections: HashMap<String, Section>,
}

/// A simple means of representing some bytes to disassemble, with their location in memory
pub struct Section {
    /// The memory offset to which the bytes are loaded.
    pub offset: usize,
    /// The bytes themselves.
    pub bytes: Vec<u8>,
}

impl Binary {
    /// Parse bytes (from a binary) returning the code sections.
    pub fn new(bytes: Vec<u8>) -> Result<Self, failure::Error> {
        let sections = match Object::parse(bytes.as_slice())? {
            Object::Elf(_elf) => return Err(format_err!("I don't do elf yet.")),
            Object::PE(pe) => parse_pe(pe, &bytes),
            Object::Mach(mach) => parse_mach(mach),
            Object::Archive(_archive) => return Err(format_err!("I don't do archives yet.")),
            Object::Unknown(magic) => return Err(format_err!("unknown magic: {:#x}", magic)),
        };

        Ok(Binary { sections })
    }
}

fn parse_mach(binary: goblin::mach::Mach) -> HashMap<String, Section> {
    const TEXT_SEGMENT: [u8; 16] = [
        0x5F, 0x5F, 0x54, 0x45, 0x58, 0x54, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00,
    ];

    let mut code_sections = HashMap::new();

    if let goblin::mach::Mach::Binary(mach) = binary {
        for seg in &mach.segments {
            if seg.segname == TEXT_SEGMENT {
                if let Ok(sections) = seg.sections() {
                    for sect in sections {
                        let mut name = String::from_utf8(sect.0.sectname.to_vec()).unwrap();
                        name = name.trim_end_matches('\u{0000}').to_string();
                        let mut bytes = vec![];
                        bytes.extend_from_slice(sect.1);

                        code_sections.insert(
                            name,
                            Section {
                                offset: sect.0.offset as usize,
                                bytes,
                            },
                        );
                    }
                }
            }
        }
    }

    code_sections
}

fn parse_pe(binary: goblin::pe::PE, bytes: &Vec<u8>) -> HashMap<String, Section> {
    const TEXT: [u8; 8] = [0x2e, 0x74, 0x65, 0x78, 0x74, 0x00, 0x00, 0x00];

    let mut code_sections = HashMap::new();

    for s in binary.sections {
        if s.name == TEXT {
            let base_addr = s.virtual_address as usize;
            let file_offset = s.pointer_to_raw_data as usize;
            let size = s.size_of_raw_data as usize;
            println!("offset {} size {}", file_offset, size);
            let mut file_bytes = vec![];
            file_bytes.extend_from_slice(&bytes[file_offset..file_offset + size]);
            println!("bytes len {}", file_bytes.len());

            code_sections.insert(
                ".text".to_string(),
                Section {
                    offset: base_addr,
                    bytes: file_bytes,
                },
            );
        }
    }

    code_sections
}
