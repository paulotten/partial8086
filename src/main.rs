#[macro_use]
extern crate enum_primitive_derive;
extern crate num_traits;

use num_traits::FromPrimitive;
use std::cmp::PartialEq;
use std::fs::File;
use std::io::Read;

const REG_WIDTH: usize = 2; // register width in bytes, 2 bytes = 16 bit

#[derive(PartialEq, Primitive)]
enum Operation {
    Add = 0,
    Or = 1,
    Adc = 2,
    Sbb = 3,
    And = 4,
    Sub = 5,
    Xor = 6,
    Cmp = 7,
}

#[derive(Clone, Copy, Debug)]
enum RegisterMemory {
    Register,
    Memory,
}

struct Cpu {
    memory: [u8; 64 * 1024],   // we only need one 64KB memory segment
    ip: u16,                   // instruction pointer
    regs: [u8; 8 * REG_WIDTH], // registers
    // flags, only need these three
    cf: bool, // carry
    zf: bool, // zero
    sf: bool, // sign
}

#[derive(Debug)]
struct Pointer {
    rm: RegisterMemory,
    offset: usize,
}

impl Cpu {
    // accumulator register
    const AX: Pointer = Pointer {
        rm: RegisterMemory::Register,
        offset: 0b000,
    };
    // base register
    const BX: Pointer = Pointer {
        rm: RegisterMemory::Register,
        offset: 0b011 * REG_WIDTH,
    };
    // stack pointer
    const SP: Pointer = Pointer {
        rm: RegisterMemory::Register,
        offset: 0b100 * REG_WIDTH,
    };
    // stack base pointer
    const BP: Pointer = Pointer {
        rm: RegisterMemory::Register,
        offset: 0b101 * REG_WIDTH,
    };
    // distination index register
    const DI: Pointer = Pointer {
        rm: RegisterMemory::Register,
        offset: 0b111 * REG_WIDTH,
    };

    fn new() -> Cpu {
        let mut cpu = Cpu {
            memory: [0; 64 * 1024],
            ip: 0,
            regs: [0; 16],
            cf: false,
            zf: false,
            sf: false,
        };

        cpu.write16(&Cpu::SP, 0x100);

        cpu
    }

    fn load_program(&mut self, filename: &str) {
        let mut f = File::open(filename).unwrap();
        let _ = f.read(&mut self.memory);
    }

    fn get_register(offset: u8) -> Pointer {
        Pointer {
            rm: RegisterMemory::Register,
            offset: (offset % 8) as usize * REG_WIDTH,
        }
    }

    fn run(&mut self) -> bool {
        let opcode = self.read_instr8();

        //println!("opcode {:#x} at {:#x}", opcode, self.ip - 1);

        match opcode {
            0x1 | 0x9 | 0x19 | 0x29 | 0x31 | 0x39 => {
                // 	16 bit register to 16 bit r/m
                let operation = match opcode {
                    0x1 => Operation::Add,
                    0x9 => Operation::And,
                    0x19 => Operation::Sbb,
                    0x29 => Operation::Sub,
                    0x31 => Operation::Xor,
                    0x39 => Operation::Cmp,
                    _ => unreachable!(),
                };

                let (rm, rg) = self.read_mod_rm();
                let rg = Cpu::get_register(rg);

                let op1 = self.read16(&rm);
                let op2 = self.read16(&rg);

                let result = self.alu(operation, op1, op2);
                self.write16(&rm, result);
            }
            0x4 => {
                // ADD 8 bit immediate to register AL
                let op1 = Cpu::sign_extend(self.read8(&Cpu::AX));
                let op2 = Cpu::sign_extend(self.read_instr8());

                let result = self.alu(Operation::Add, op1, op2);
                self.write8(&Cpu::AX, result as u8);
            }
            0x20 => {
                // AND 8 bit register, 8 bit r/m
                let (rm, rg) = self.read_mod_rm();
                let rg = Cpu::get_register(rg);

                let op1 = Cpu::sign_extend(self.read8(&rm));
                let op2 = Cpu::sign_extend(self.read8(&rg));

                let result = self.alu(Operation::And, op1, op2);
                self.write8(&rm, result as u8);
            }
            0x3c => {
                // compare 8 bit immediate to register AL
                let op1 = Cpu::sign_extend(self.read8(&Cpu::AX));
                let op2 = Cpu::sign_extend(self.read_instr8());

                self.alu(Operation::Cmp, op1, op2);
            }
            0x40..=0x47 => {
                // increment 16 bit register
                let rg = Cpu::get_register(opcode);
                let value = self.read16(&rg).wrapping_add(1);

                self.set_flags(&value);
                self.write16(&rg, value);
            }
            0x48..=0x4f => {
                // decrement 16 bit register
                let rg = Cpu::get_register(opcode);
                let value = self.read16(&rg).wrapping_sub(1);

                self.set_flags(&value);
                self.write16(&rg, value);
            }
            0x50..=0x57 => {
                // push 16 bit register
                let rg = Cpu::get_register(opcode);
                let value = self.read16(&rg);

                self.push16(value);
            }
            0x58..=0x5f => {
                // pop 16 bit register
                let rg = Cpu::get_register(opcode);
                let value = self.pop16();
                self.write16(&rg, value);
            }
            0x72 => {
                // jump short if carry
                self.jump_short(self.cf);
            }
            0x74 => {
                // jump short if zero
                self.jump_short(self.zf);
            }
            0x75 => {
                // jump short if not zero
                self.jump_short(!self.zf);
            }
            0x76 => {
                // jump short if below or equal
                self.jump_short(self.cf || self.zf);
            }
            0x77 => {
                // jump short if not below or equal
                self.jump_short(!self.cf && !self.zf);
            }
            0x79 => {
                // jump short if not sign
                self.jump_short(!self.sf);
            }
            0x80 => {
                // 8 bit arithmetic
                let (rm, operation) = self.read_mod_rm();
                let op1 = Cpu::sign_extend(self.read8(&rm));
                let op2 = Cpu::sign_extend(self.read_instr8());

                let result = self.alu(Operation::from_u8(operation).unwrap(), op1, op2);
                self.write8(&rm, result as u8);
            }
            0x81 => {
                // 16 bit arithmetic
                let (rm, operation) = self.read_mod_rm();
                let op1 = self.read16(&rm);
                let op2 = self.read_instr16();

                let result = self.alu(Operation::from_u8(operation).unwrap(), op1, op2);
                self.write16(&rm, result);
            }
            0x83 => {
                // 16 bit / 8 bit arithmetic
                let (rm, operation) = self.read_mod_rm();
                let op1 = self.read16(&rm);
                let op2 = Cpu::sign_extend(self.read_instr8());

                let result = self.alu(Operation::from_u8(operation).unwrap(), op1, op2);
                self.write16(&rm, result);
            }
            0x86 => {
                // exchange 8 bit register with 8 bit r/m
                let (rm, rg) = self.read_mod_rm();
                let rg = Cpu::get_register(rg);

                let a = self.read8(&rm);
                let b = self.read8(&rg);

                self.write8(&rm, b);
                self.write8(&rg, a);
            }
            0x88 => {
                // move 8 bit register to 8 bit r/m
                let (op1, rg) = self.read_mod_rm();
                let rg = Cpu::get_register(rg);
                let value = self.read8(&rg);

                self.write8(&op1, value);
            }
            0x89 => {
                // move 16 bit register to 16 bit r/m
                let (op1, rg) = self.read_mod_rm();
                let rg = Cpu::get_register(rg);
                let value = self.read16(&rg);

                self.write16(&op1, value);
            }
            0x8a => {
                // move 8 bit r/m to 8 bit register
                let (op1, rg) = self.read_mod_rm();
                let rg = Cpu::get_register(rg);
                let value = self.read8(&op1);

                self.write8(&rg, value);
            }
            0x8b => {
                // move 16 bit r/m to 16 bit register
                let (op1, rg) = self.read_mod_rm();
                let rg = Cpu::get_register(rg);
                let value = self.read16(&op1);

                self.write16(&rg, value);
            }
            0x90 => {
                // NOP
            }
            0x91..=0x97 => {
                // exchange 16 bit register with register AX
                let rg = Cpu::get_register(opcode);

                let a = self.read16(&Cpu::AX);
                let b = self.read16(&rg);

                self.write16(&Cpu::AX, b);
                self.write16(&rg, a);
            }
            0xb0..=0xb7 => {
                // move 8 bit immediate to 8 bit register
                let rg = Cpu::get_register(opcode);
                let value = self.read_instr8() as u16;

                self.write16(&rg, value);
            }
            0xb8..=0xbf => {
                // move 16 bit immediate to 16 bit register
                let rg = Cpu::get_register(opcode);
                let value = self.read_instr16();

                self.write16(&rg, value);
            }
            0xc3 => {
                // return near
                self.ip = self.pop16();
            }
            0xc7 => {
                // move 16 bit immediate to 16 bit r/m
                let (rm, _) = self.read_mod_rm();
                let value = self.read_instr16();

                self.write16(&rm, value);
            }
            0xe8 => {
                // call relative
                let offset = self.read_instr16();
                self.push16(self.ip);
                self.ip = self.ip.wrapping_add(offset);
            }
            0xeb => {
                // jump short relative
                self.jump_short(true);
            }
            0xf4 => {
                // halt
                return false;
            }
            0xf9 => {
                // set carry flag
                self.cf = true;
            }
            0xfe => {
                // increment | decrement 8 bit r/m
                let (rm, md) = self.read_mod_rm();
                let value = match md {
                    0 => self.read8(&rm).wrapping_add(1),
                    1 => self.read8(&rm).wrapping_sub(1),
                    _ => unreachable!(),
                };

                self.set_flags(&Cpu::sign_extend(value));
                self.write8(&rm, value);
            }
            _ => {
                println!("unsupported opcode {:#x}", opcode);
                return false;
            }
        };

        true
    }

    // ------------------------------------
    // reading from the instruction pointer
    // ------------------------------------

    fn read_instr8(&mut self) -> u8 {
        let read = self.memory[self.ip as usize];
        self.ip += 1;

        read
    }

    fn read_instr16(&mut self) -> u16 {
        let a = self.read_instr8();
        let b = self.read_instr8();

        Cpu::to16(a, b)
    }

    // --------------------------------------
    // reading and writing registers / memory
    // --------------------------------------

    fn read8(&mut self, pointer: &Pointer) -> u8 {
        match pointer.rm {
            RegisterMemory::Register => self.regs[pointer.offset],
            RegisterMemory::Memory => self.memory[pointer.offset],
        }
    }

    fn read16(&mut self, pointer: &Pointer) -> u16 {
        let rm = pointer.rm;
        let offset = pointer.offset;

        let a = self.read8(pointer);

        let offset = offset.wrapping_add(1);
        let b = self.read8(&Pointer { rm, offset });

        Cpu::to16(a, b)
    }

    fn write8(&mut self, pointer: &Pointer, value: u8) {
        match pointer.rm {
            RegisterMemory::Register => self.regs[pointer.offset] = value,
            RegisterMemory::Memory => self.memory[pointer.offset] = value,
        };
    }

    fn write16(&mut self, pointer: &Pointer, value: u16) {
        let (a, b) = Cpu::to8s(value);
        let rm = pointer.rm;
        let offset = pointer.offset;

        self.write8(pointer, a);

        let offset = offset.wrapping_add(1);
        self.write8(&Pointer { rm, offset }, b);
    }

    // ---------------------------
    // pushing / popping the stack
    // ---------------------------

    fn push8(&mut self, value: u8) {
        let sp = self.read16(&Cpu::SP);
        self.memory[sp as usize] = value;
        self.write16(&Cpu::SP, sp - 1);
    }

    fn push16(&mut self, value: u16) {
        let (a, b) = Cpu::to8s(value);

        self.push8(a);
        self.push8(b);
    }

    fn pop8(&mut self) -> u8 {
        let sp = self.read16(&Cpu::SP) + 1;
        let value = self.memory[sp as usize];
        self.write16(&Cpu::SP, sp);

        value
    }

    fn pop16(&mut self) -> u16 {
        // bytes come off the stack in the opposite order they go on
        let b = self.pop8();
        let a = self.pop8();

        Cpu::to16(a, b)
    }

    // -------------------------
    // little-endian conversions
    // -------------------------

    fn to16(a: u8, b: u8) -> u16 {
        ((b as u16) << 8) | a as u16
    }

    fn to8s(value: u16) -> (u8, u8) {
        let b: u8 = (value >> 8) as u8;
        let a: u8 = value as u8;

        (a, b)
    }

    // -------------------------

    fn read_mod_rm(&mut self) -> (Pointer, u8) {
        let mod_rm = self.read_instr8();

        let md = mod_rm >> 6; // modifier
        let rg_op = mod_rm >> 3 & 0b111; // register / opcode extension
        let rm = mod_rm & 0b111; // register / memory

        let operand: Pointer = match md {
            0b00 => match rm {
                0b001 => {
                    let bx = self.read16(&Cpu::BX);
                    let di = self.read16(&Cpu::DI);
                    let offset = bx.wrapping_add(di) as usize;

                    Pointer {
                        rm: RegisterMemory::Memory,
                        offset,
                    }
                }
                0b110 => {
                    let offset = self.read_instr16() as usize;

                    Pointer {
                        rm: RegisterMemory::Memory,
                        offset,
                    }
                }
                0b111 => {
                    let offset = self.read16(&Cpu::BX) as usize;

                    Pointer {
                        rm: RegisterMemory::Memory,
                        offset,
                    }
                }
                _ => panic!("unsupported r/m {:#b}", rm),
            },
            0b01 => match rm {
                0b011 => {
                    let bp = self.read16(&Cpu::BP);
                    let di = self.read16(&Cpu::DI);
                    let byte = self.read_instr8();
                    let offset = (bp.wrapping_add(di).wrapping_add(byte as u16)) as usize;

                    Pointer {
                        rm: RegisterMemory::Memory,
                        offset,
                    }
                }
                _ => panic!("unsupported r/m {:#b}", rm),
            },
            0b10 => match rm {
                0b101 => {
                    let di = self.read16(&Cpu::DI);
                    let word = self.read_instr16();
                    let offset = di.wrapping_add(word) as usize;

                    Pointer {
                        rm: RegisterMemory::Memory,
                        offset,
                    }
                }
                0b111 => {
                    let bx = self.read16(&Cpu::BX);
                    let word = self.read_instr16();
                    let offset = bx.wrapping_add(word) as usize;

                    Pointer {
                        rm: RegisterMemory::Memory,
                        offset,
                    }
                }
                _ => panic!("unsupported r/m {:#b}", rm),
            },
            0b11 => Cpu::get_register(rm),
            _ => panic!("unsupported mod {:#b}", md),
        };

        (operand, rg_op)
    }

    fn sign_extend(a: u8) -> u16 {
        ((a as i8) as i16) as u16
    }

    // arithmetic logic unit
    fn alu(&mut self, operation: Operation, op1: u16, op2: u16) -> u16 {
        let mut result = match operation {
            Operation::Add | Operation::Adc => {
                let mut add = op2;

                if operation == Operation::Adc && self.cf {
                    add += 1;
                }

                let result = op1.wrapping_add(add);
                self.cf = result < op1;

                result
            }
            Operation::Sub | Operation::Sbb | Operation::Cmp => {
                let mut sub = op2;

                if operation == Operation::Sbb && self.cf {
                    sub += 1;
                }

                let result = op1.wrapping_sub(sub);
                self.cf = result > op1;

                result
            }
            Operation::And => {
                self.cf = false;
                op1 & op2
            }
            Operation::Or => {
                self.cf = false;
                op1 | op2
            }
            Operation::Xor => {
                self.cf = false;
                op1 ^ op2
            }
        };

        self.set_flags(&result);

        // compare doesn't generate a result, it just sets flags
        if operation == Operation::Cmp {
            result = op1;
        }

        result
    }

    fn set_flags(&mut self, value: &u16) {
        self.zf = *value == 0;
        self.sf = *value >> 15 == 1;
    }

    fn jump_short(&mut self, condition: bool) {
        let offset = self.read_instr8() as i8;

        if condition {
            self.ip = self.ip.wrapping_add(offset as u16);
        }
    }

    fn dump_memory(&self) {
        let mut offset: usize = 0x8000;

        for _line in 0..25 {
            for _char in 0..80 {
                let mut output = ' ';

                if self.memory[offset] != 0 {
                    output = self.memory[offset] as char;
                }

                print!("{}", output);
                offset += 1;
            }
            println!();
        }
    }
}

fn main() {
    // setup CPU
    let mut cpu = Cpu::new();

    // load program
    cpu.load_program("codegolf.bin");

    // run until halt
    while cpu.run() {}

    // dump memory
    cpu.dump_memory();
}
