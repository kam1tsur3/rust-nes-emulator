use std::collections::HashMap;
use crate::opcodes;

bitflags! {
  pub struct CpuFlags: u8 {
    const CARRY             = 0b00000001;
    const ZERO              = 0b00000010;
    const INTERRUPT_DISABLE = 0b00000100;
    const DECIMAL_MODE      = 0b00001000;
    const BREAK             = 0b00010000;
    const RESERVED          = 0b00100000;
    const OVERFLOW          = 0b01000000;
    const NEGATIVE          = 0b10000000;
  }
}

const STACK: u16 = 0x0100;
const STACK_RESET: u8 = 0xfd;
pub struct CPU {
  pub register_a: u8,
  pub register_x: u8,
  pub register_y: u8,
  pub status: CpuFlags,
  pub program_counter: u16,
  pub stack_pointer: u8,
  memory: [u8; 0xFFFF]
}

#[derive(Debug)]
#[allow(non_camel_case_types)]
pub enum AddressingMode {
  Immediate,
  ZeroPage,
  ZeroPage_X,
  ZeroPage_Y,
  Absolute,
  Absolute_X,
  Absolute_Y,
  Indirect_X,
  Indirect_Y,
  NoneAddressing,
}

pub trait Mem {
  fn mem_read(&self, addr: u16) -> u8;
  fn mem_write(&mut self, addr: u16, data: u8);
  fn mem_read_u16(&self, pos: u16) -> u16 {
    let lo = self.mem_read(pos) as u16;
    let hi = self.mem_read(pos + 1) as u16;
    (hi << 8) | (lo as u16)
  }

  fn mem_write_u16(&mut self, addr: u16, data: u16) {
    let lo = (data & 0xff) as u8;
    let hi = (data >> 8) as u8;
    self.mem_write(addr, lo);
    self.mem_write(addr + 1, hi);
  }
}

impl Mem for CPU {
  fn mem_read(&self, addr:u16) -> u8 {
    self.memory[addr as usize]
  }

  fn mem_write(&mut self, addr:u16, data:u8) {
    self.memory[addr as usize] = data;
  }
}

impl CPU {
  pub fn new() -> Self {
    CPU {
      register_a: 0,
      register_x: 0,
      register_y: 0,
      stack_pointer: STACK_RESET,
      status: CpuFlags::from_bits_truncate(0b100100),
      program_counter: 0,
      memory: [0; 0xFFFF]
    }
  }

  pub fn reset(&mut self) {
    self.register_a = 0;
    self.register_x = 0;
    self.register_y = 0;
    self.stack_pointer = STACK_RESET;
    self.status = CpuFlags::from_bits_truncate(0b100100);

    self.program_counter = self.mem_read_u16(0xFFFC);
  }

  fn get_operand_address(&self, mode: &AddressingMode) -> u16 {
    match mode {
      AddressingMode::Immediate => self.program_counter,
      AddressingMode::ZeroPage => self.mem_read(self.program_counter) as u16,
      AddressingMode::Absolute => self.mem_read_u16(self.program_counter),
      AddressingMode::ZeroPage_X => {
        let pos = self.mem_read(self.program_counter);
        let addr = pos.wrapping_add(self.register_x) as u16;
        addr
      }
      AddressingMode::ZeroPage_Y => {
        let pos = self.mem_read(self.program_counter);
        let addr = pos.wrapping_add(self.register_y) as u16;
        addr
      }
      AddressingMode::Absolute_X => {
        let base = self.mem_read_u16(self.program_counter);
        let addr = base.wrapping_add(self.register_x as u16);
        addr
      }
      AddressingMode::Absolute_Y => {
        let base = self.mem_read_u16(self.program_counter);
        let addr = base.wrapping_add(self.register_y as u16);
        addr
      }
      AddressingMode::Indirect_X => {
        let base = self.mem_read(self.program_counter);

        let ptr: u8 = (base as u8).wrapping_add(self.register_x);
        // why cannot mem_read u16
        // is it happen bound read?
        let lo = self.mem_read(ptr as u16);
        let hi = self.mem_read(ptr.wrapping_add(1) as u16);
        (hi as u16) << 8 | (lo as u16)
      }
      AddressingMode::Indirect_Y => {
        let base = self.mem_read(self.program_counter);

        let lo = self.mem_read(base as u16);
        let hi = self.mem_read((base as u8).wrapping_add(1) as u16);
        let deref_base = (hi as u16) << 8 | (lo as u16);
        let deref = deref_base.wrapping_add(self.register_y as u16);
        deref
      }
      AddressingMode::NoneAddressing => {
        panic!("mode {:?} is not supported", mode);
      }
    }
  }

  pub fn load(&mut self, program: Vec<u8>) {
    self.memory[0x600 .. (0x600 + program.len())].copy_from_slice(&program[..]);
    self.mem_write_u16(0xFFFC, 0x600); // temporary???
    //self.memory[0x8000 .. (0x8000 + program.len())].copy_from_slice(&program[..]);
    //self.mem_write_u16(0xFFFC, 0x8000); // temporary???
  }
  
  pub fn load_and_run(&mut self, program: Vec<u8>) {
    self.load(program);
    self.reset();
    self.run()
  }
  
  pub fn run(&mut self) {
    self.run_with_callback(|_| {});
  }

  pub fn run_with_callback<F>(&mut self, mut callback: F) 
  where
    F: FnMut(&mut CPU),
  {
    let ref opcodes: HashMap<u8, &'static opcodes::OpCode> = *opcodes::OPCODES_MAP;
    
    loop {
      callback(self);

      let code = self.mem_read(self.program_counter);
      self.program_counter += 1;
      let program_counter_state = self.program_counter;
      let opcode = opcodes.get(&code).expect(&format!("OpCode {:x} is not recognized", code));
      //println!("opcode: {:x}", code);
      match code {
        0x00 => {
          /* todo BRK */
          return;
        }
        0x69 | 0x65 | 0x75 | 0x6d | 0x7d | 0x79 | 0x61 | 0x71 => {
          self.adc(&opcode.mode);
        }
        0xe9 | 0xe5 | 0xf5 | 0xed | 0xfd | 0xf9 | 0xe1 | 0xf1 => {
          self.sbc(&opcode.mode);
        }
        0x29 | 0x25 | 0x35 | 0x2d | 0x3d | 0x39 | 0x21 | 0x31 => {
          self.and(&opcode.mode);
        }
        0x0a | 0x06 | 0x16 | 0x0e | 0x1e => {
          self.asl(&opcode.mode);
        }
        0x4a | 0x46 | 0x56 | 0x4e | 0x5e => {
          self.lsr(&opcode.mode);
        }
        0x2a | 0x26 | 0x36 | 0x2e | 0x3e => {
          self.rol(&opcode.mode);
        }
        0x6a | 0x66 | 0x76 | 0x6e | 0x7e => {
          self.ror(&opcode.mode);
        }
        0xa9 | 0xa5 | 0xb5 | 0xad | 0xbd | 0xb9 | 0xa1 | 0xb1 => {
          self.lda(&opcode.mode);
        }
        0xa2 | 0xa6 | 0xb6 | 0xae | 0xbe => {
          self.ldx(&opcode.mode);
        }
        0xa0 | 0xa4 | 0xb4 | 0xac | 0xbc => {
          self.ldy(&opcode.mode);
        }
        0x85 | 0x95 | 0x8d | 0x9d | 0x99 | 0x81 | 0x91 => {
          self.sta(&opcode.mode);
        }
        0x86 | 0x96 | 0x8e => {
          self.stx(&opcode.mode);
        }
        0x84 | 0x94 | 0x8c => {
          self.sty(&opcode.mode);
        }
        0x24 | 0x2c => {
          self.bit(&opcode.mode);
        }
        0xc9 | 0xc5 | 0xd5 | 0xcd | 0xdd |0xd9 | 0xc1 | 0xd1 => {
          self.compare(&opcode.mode, self.register_a);
        }
        0xe0 | 0xe4 | 0xec => {
          self.compare(&opcode.mode, self.register_x);
        }
        0xc0 | 0xc4 | 0xcc => {
          self.compare(&opcode.mode, self.register_y);
        }
        0xc6 | 0xd6 | 0xce | 0xde => {
          self.dec(&opcode.mode);
        }
        0xe6 | 0xf6 | 0xee | 0xfe => {
          self.inc(&opcode.mode);
        }
        0xca => {
          self.dex();
        }
        0x88 => {
          self.dey();
        }
        0xe8 => {
          self.inx();
        }
        0xc8 => {
          self.iny();
        }
        0x49 | 0x45 | 0x55 | 0x4d | 0x5d | 0x59 | 0x41 | 0x51 => {
          self.eor(&opcode.mode);
        }
        0x09 | 0x05 | 0x15 | 0x0d | 0x1d | 0x19 | 0x01 | 0x11 => {
          self.ora(&opcode.mode);
        }

        0x90 => {
          /* BCC */
          self.branch(!self.status.contains(CpuFlags::CARRY));
        }
        0xb0 => {
          /* BCS */
          self.branch(self.status.contains(CpuFlags::CARRY));
        }
        0xf0 => {
          /* BEQ */
          self.branch(self.status.contains(CpuFlags::ZERO));
        }
        0x30 => {
          /* BMI */
          self.branch(self.status.contains(CpuFlags::NEGATIVE));
        }
        0xd0 => {
          /* BNE */
          self.branch(!self.status.contains(CpuFlags::ZERO));
        }
        0x10 => {
          /* BPL */
          self.branch(!self.status.contains(CpuFlags::NEGATIVE));
        }
        0x50 => {
          /* BVC */
          self.branch(!self.status.contains(CpuFlags::OVERFLOW));
        }
        0x70 => {
          /* BVS */
          self.branch(self.status.contains(CpuFlags::OVERFLOW));
        }
        /* JMP */
        0x4c => {
          let mem_address = self.mem_read_u16(self.program_counter);
          self.program_counter = mem_address;
        }
        0x6c => {
          /* JMP */
          let mem_address = self.mem_read_u16(self.program_counter);

          /* Rewrite bug in original 6502 */
          let upper_addr: u16 = mem_address & 0xff00;
          let lower_addr: u8 = (mem_address & 0x00ff) as u8;
          let lower_target = self.mem_read(upper_addr | (lower_addr as u16));
          let upper_target = self.mem_read(upper_addr | (lower_addr.wrapping_add(1) as u16));
          
          let next = ((upper_target as u16)<<8) | (lower_target as u16);
          self.program_counter = next;
        }
        /* JSR */
        0x20 => {
          self.push_u16(self.program_counter + 2 - 1);
          let mem_address = self.mem_read_u16(self.program_counter);
          self.program_counter = mem_address;
        }
        /* RTI */
        0x40 => {
          self.status.bits = self.pop_u8();
          self.status.remove(CpuFlags::BREAK);
          self.status.insert(CpuFlags::RESERVED);

          self.program_counter = self.pop_u16();
        }
        /* RTS */
        0x60 => {
          self.program_counter = self.pop_u16() + 1;
        }
        0xaa => self.tax(),
        0xa8 => self.tay(),
        0xba => self.tsx(),
        /* TXA */
        0x8a => self.set_register_a(self.register_x),
        /* TXS */
        0x9a => {
          self.stack_pointer = self.register_x
        }
        /* TYA */
        0x98 => self.set_register_a(self.register_y),
        /* CLC */
        0x18 => self.clear_carry_flag(),
        /* CLD */
        0xd8 => self.clear_decimal_flag(),
        /* CLI */
        0x58 => self.clear_interrupt_flag(),
        /* CLV */
        0xb8 => self.clear_overflow_flag(),
        /* SEC */
        0x38 => self.set_carry_flag(),
        /* SED */
        0xf8 => self.set_decimal_flag(),
        /* SEI */
        0x78 => self.set_interrupt_flag(),
        0x48 => {
          /* PHA */
          self.push_u8(self.register_a);
        }
        0x08 => self.php(),
        0x68 => self.pla(),
        0x28 => self.plp(),
        /* NOP */
        0xea => {}, 

        _ => todo!(),
      }

      if program_counter_state == self.program_counter {
        self.program_counter += (opcode.len - 1) as u16;
      }
    }
  }

  /* instructions  */
  fn adc(&mut self, mode: &AddressingMode) {
    let addr = self.get_operand_address(mode);
    let value = self.mem_read(addr);
    self.add_to_register_a(value);
  }

  fn sbc(&mut self, mode: &AddressingMode) {
    let addr = self.get_operand_address(mode);
    let value = self.mem_read(addr);
    self.add_to_register_a((value as i8).wrapping_neg().wrapping_sub(1) as u8);
  }

  fn and(&mut self, mode: &AddressingMode) {
    let addr = self.get_operand_address(mode);
    let data = self.mem_read(addr);
    self.set_register_a(data & self.register_a);
  }

  fn asl(&mut self, mode: &AddressingMode) {
    let value: u8;
    match mode {
      AddressingMode::NoneAddressing => {
        /* Accumulator */
        value = self.register_a;
        self.set_register_a(value<<1);
      },
      _ => {
        let addr = self.get_operand_address(mode);
        value = self.mem_read(addr);
        self.mem_write(addr, value<<1);
        self.update_zero_and_negative_flags(value<<1);
      }
    }

    if (value & 0x80) != 0 {
      self.set_carry_flag();
    } else {
      self.clear_carry_flag();
    }
  }
  
  fn lsr(&mut self, mode: &AddressingMode) {
    let value: u8;
    match mode {
      AddressingMode::NoneAddressing => {
        /* Accumulator */
        value = self.register_a;
        self.set_register_a(value >> 1);
      }, 
      _ =>  {
        let addr = self.get_operand_address(mode);
        value = self.mem_read(addr);
        
        self.update_zero_and_negative_flags(value>>1);
        self.mem_write(addr, value>>1);
      }
    }
    if value & 0x1 == 0 {
      self.clear_carry_flag();
    } else {
      self.set_carry_flag();
    }
  }

  fn rol(&mut self, mode: &AddressingMode) {
    let value: u8;
    let mut result: u8;
    match mode {
      AddressingMode::NoneAddressing => {
        value = self.register_a;
        result = value << 1;
        if self.status.contains(CpuFlags::CARRY) {
          result |= 0x1;
        }
        self.set_register_a(result);
      },
      _ => {
        let addr = self.get_operand_address(mode);
        value = self.mem_read(addr);
        result = value << 1;
        if self.status.contains(CpuFlags::CARRY) {
          result |= 0x1;
        }
        self.update_zero_and_negative_flags(result);
        self.mem_write(addr, result);
      }
    }
    if value & 0x80 == 0 {
      self.clear_carry_flag();
    } else {
      self.set_carry_flag();
    }
  }

  fn ror(&mut self, mode: &AddressingMode) {
    let value: u8;
    let mut result: u8;
    match mode {
      AddressingMode::NoneAddressing => {
        value = self.register_a;
        result = value >> 1;
        if self.status.contains(CpuFlags::CARRY) {
          result |= 0x80;
        }
        self.set_register_a(result);
      },
      _ => {
        let addr = self.get_operand_address(mode);
        value = self.mem_read(addr);
        result = value >> 1;
        if self.status.contains(CpuFlags::CARRY) {
          result |= 0x80;
        }
        self.update_zero_and_negative_flags(result);
        self.mem_write(addr, result);
      }
    }
    if value & 0x1 == 0 {
      self.clear_carry_flag();
    } else {
      self.set_carry_flag();
    }
  }

  fn lda(&mut self, mode: &AddressingMode) {
    let addr = self.get_operand_address(mode);
    let value = self.mem_read(addr);
    self.set_register_a(value);
  }

  fn ldx(&mut self, mode: &AddressingMode) {
    let addr = self.get_operand_address(mode);
    let value = self.mem_read(addr);
    self.register_x = value;
    self.update_zero_and_negative_flags(value);
  }

  fn ldy(&mut self, mode: &AddressingMode) {
    let addr = self.get_operand_address(mode);
    let value = self.mem_read(addr);
    self.register_y = value;
    self.update_zero_and_negative_flags(value);
  }

  fn sta(&mut self, mode: &AddressingMode) {
    let addr = self.get_operand_address(mode);
    self.mem_write(addr, self.register_a);
  }

  fn stx(&mut self, mode: &AddressingMode) {
    let addr = self.get_operand_address(mode);
    self.mem_write(addr, self.register_x);
  }

  fn sty(&mut self, mode: &AddressingMode) {
    let addr = self.get_operand_address(mode);
    self.mem_write(addr, self.register_y);
  }

  fn tax(&mut self) {
    self.register_x = self.register_a;
    self.update_zero_and_negative_flags(self.register_x);
  }
  
  fn tay(&mut self) {
    self.register_y = self.register_a;
    self.update_zero_and_negative_flags(self.register_y);
  }

  fn tsx(&mut self) {
    self.register_x = self.stack_pointer;
    self.update_zero_and_negative_flags(self.register_x);
  }

  fn bit(&mut self, mode: &AddressingMode) {
    let addr = self.get_operand_address(mode);
    let data = self.mem_read(addr);
    let value = data & self.register_a;
    if value == 0 { self.status.insert(CpuFlags::ZERO) }

    if (data & 0b1000_0000) != 0 {
      self.set_negative_flag();
    } else {
      self.clear_negative_flag();
    }

    if (data & 0b0100_0000) != 0 {
      self.set_overflow_flag();
    } else {
      self.clear_overflow_flag();
    }
  }

  fn dec(&mut self, mode: &AddressingMode) {
    let addr: u16 = self.get_operand_address(mode);
    let value: u8 = self.mem_read(addr);
    let result = value.wrapping_sub(1);
    self.mem_write(addr, result);
    self.update_zero_and_negative_flags(result);
  }

  fn inc(&mut self, mode: &AddressingMode) {
    let addr: u16 = self.get_operand_address(mode);
    let value: u8 = self.mem_read(addr);
    let result = value.wrapping_add(1);
    self.mem_write(addr, result);
    self.update_zero_and_negative_flags(result);
  }

  fn dex(&mut self) {
    self.register_x = self.register_x.wrapping_sub(1);
    self.update_zero_and_negative_flags(self.register_x);
  }
  
  fn dey(&mut self) {
    self.register_y = self.register_y.wrapping_sub(1);
    self.update_zero_and_negative_flags(self.register_y);
  }

  fn inx(&mut self) {
    self.register_x = self.register_x.wrapping_add(1);
    self.update_zero_and_negative_flags(self.register_x);
  }
  
  fn iny(&mut self) {
    self.register_y = self.register_y.wrapping_add(1);
    self.update_zero_and_negative_flags(self.register_y);
  }

  fn eor(&mut self, mode: &AddressingMode) { 
    let addr = self.get_operand_address(mode);
    let value = self.mem_read(addr);
    self.set_register_a(self.register_a ^ value);
  }

  fn ora(&mut self, mode: &AddressingMode) {
    let addr = self.get_operand_address(mode);
    let value = self.mem_read(addr);
    self.set_register_a(value | self.register_a);
  } 

  fn php(&mut self) {
    let mut flags = self.status.clone();
    flags.insert(CpuFlags::BREAK); 
    flags.insert(CpuFlags::RESERVED); 
    self.push_u8(flags.bits());
  }
  
  fn pla(&mut self) {
    let data = self.pop_u8();
    self.set_register_a(data);
  }
  
  fn plp(&mut self) {
    self.status.bits = self.pop_u8();
    self.status.remove(CpuFlags::BREAK);
    self.status.insert(CpuFlags::RESERVED);
  }

  /* interfaces */

  fn branch(&mut self, is_branch: bool) {
    if is_branch {
      let jump: i8 = self.mem_read(self.program_counter) as i8;
      let jump_addr = self
        .program_counter
        .wrapping_add(1)
        .wrapping_add(jump as u16);
      self.program_counter = jump_addr;
    }
  }

  fn compare(&mut self, mode: &AddressingMode, data: u8) {
    let addr = self.get_operand_address(mode);
    let value = self.mem_read(addr);
    self.update_zero_and_negative_flags(data.wrapping_sub(value));
    if data >= value {
      self.set_carry_flag();
    } else {
      self.clear_carry_flag();
    }
  }

  fn push_u8(&mut self, data: u8) {
    self.mem_write( (STACK as u16) + (self.stack_pointer as u16), data);
    self.stack_pointer = self.stack_pointer.wrapping_sub(1);
  }

  fn pop_u8(&mut self) -> u8 {
    self.stack_pointer = self.stack_pointer.wrapping_add(1);
    self.mem_read((STACK as u16) + (self.stack_pointer as u16))
  }

  fn push_u16(&mut self, data: u16) {
    let upper = (data >> 8) as u8;
    let lower = (data & 0xff) as u8;
    self.push_u8(upper);
    self.push_u8(lower);
  }
  
  fn pop_u16(&mut self) -> u16 {
    let lower = self.pop_u8();
    let upper = self.pop_u8();
    return (lower as u16) | ((upper as u16) << 8);
  }

  fn set_register_a(&mut self, value: u8) {
    self.register_a = value;
    self.update_zero_and_negative_flags(self.register_a);
  }

  fn add_to_register_a(&mut self, data: u8) {
    let sum = self.register_a as u16
      + data as u16
      + (if self.status.contains(CpuFlags::CARRY) {
        1
      } else {
        0
      }) as u16;
    let carry = sum > 0xff;

    if carry {
      self.status.insert(CpuFlags::CARRY);
    } else {
      self.status.remove(CpuFlags::CARRY);
    }

    let result = sum as u8;

    if (data ^ self.register_a) == 0 && ((result ^ self.register_a) & 0x80) != 0 {
      self.status.insert(CpuFlags::OVERFLOW);
    } else {
      self.status.remove(CpuFlags::OVERFLOW);
    }

    self.set_register_a(result);
  }

  fn update_zero_and_negative_flags(&mut self, result: u8) {
    if result == 0 {
      self.status.insert(CpuFlags::ZERO)
    } else {
      self.status.remove(CpuFlags::ZERO)
    }
    
    if result & 0b1000_0000 != 0 {
      self.status.insert(CpuFlags::NEGATIVE)
    } else {
      self.status.remove(CpuFlags::NEGATIVE)
    }
  }

  fn set_carry_flag(&mut self) {
    self.status.insert(CpuFlags::CARRY)
  }

  fn clear_carry_flag(&mut self) {
    self.status.remove(CpuFlags::CARRY)
  }

  fn set_decimal_flag(&mut self) {
    self.status.insert(CpuFlags::DECIMAL_MODE)
  }
  
  fn clear_decimal_flag(&mut self) {
    self.status.remove(CpuFlags::DECIMAL_MODE)
  }

  fn set_interrupt_flag(&mut self) {
    self.status.insert(CpuFlags::INTERRUPT_DISABLE)
  }

  fn clear_interrupt_flag(&mut self) {
    self.status.remove(CpuFlags::INTERRUPT_DISABLE)
  }

  fn set_overflow_flag(&mut self) {
    self.status.insert(CpuFlags::OVERFLOW)
  }

  fn clear_overflow_flag(&mut self) {
    self.status.remove(CpuFlags::OVERFLOW)
  }

  fn set_negative_flag(&mut self) {
    self.status.insert(CpuFlags::NEGATIVE)
  }

  fn clear_negative_flag(&mut self) {
    self.status.remove(CpuFlags::NEGATIVE)
  }

}

#[cfg(test)]
mod tests {
  use super::*;
  #[test]
  fn test_0xa9_lda_immidiate_load_data() {
    let mut cpu = CPU::new();
    cpu.load_and_run(vec![0xa9, 0x05, 0x00]);
    assert_eq!(cpu.register_a, 0x05);
    assert!(cpu.status.bits() & 0b0000_0010 == 0b00);
    assert!(cpu.status.bits() & 0b1000_0000 == 0b00);
  }

  #[test]
  fn test_0xa9_lda_zero_flag() {
    let mut cpu = CPU::new();
    cpu.load_and_run(vec![0xa9,0x00,0x00]);
    assert!(cpu.status.bits() & 0b0000_0010 == 0b10);
  }

  # [test]
  fn test_0xaa_tax_move_a_to_x() {
    let mut cpu = CPU::new();
    cpu.load_and_run(vec![0xa9,0x0a,0xaa,0x00]);
    assert_eq!(cpu.register_x, 10);
  }

  #[test]
  fn test_0x69_add_immediate() {
    let mut cpu = CPU::new();
    cpu.load_and_run(vec![0xa9,0x01,0x69,0x24,0x00]);
    assert_eq!(cpu.register_a, 0x25);
  }

  #[test]
  fn test_0x29_and_immediate() {
    let mut cpu = CPU::new();
    cpu.load_and_run(vec![0xa9,0xdd,0x29,0x0f,0x00]);
    assert_eq!(cpu.register_a, 0xdd&0xf);
  }

  # [test]
  fn test_5_ops_working_together() {
    let mut cpu = CPU::new();
    cpu.load_and_run(vec![0xa9, 0xc0, 0xaa, 0xe8, 0x00]);

    assert_eq!(cpu.register_x, 0xc1)
  }

  #[test]
  fn test_inx_overflow() {
    let mut cpu = CPU::new();
    cpu.load_and_run(vec![0xa9, 0xff, 0xaa, 0xe8, 0xe8, 0x00]);

    assert_eq!(cpu.register_x, 1)
  }

  #[test]
  fn test_lda_from_memory() {
    let mut cpu = CPU::new();
    cpu.mem_write(0x10, 0x55);
    cpu.load_and_run(vec![0xa5, 0x10, 0x00]);

    assert_eq!(cpu.register_a, 0x55);
  }
  
  #[test]
  fn test_asl_accumlator() {
    let mut cpu = CPU::new();
    cpu.load_and_run(vec![0xa9,0xc0, 0x0a, 0x00]);

    assert_eq!(cpu.register_a, 0x80);
    assert_eq!(cpu.status.contains(CpuFlags::CARRY), true);
  }

  #[test]
  fn test_asl_zeropage() {
    let mut cpu = CPU::new();
    cpu.mem_write(0x10, 0x0f);
    cpu.load_and_run(vec![0x06, 0x10, 0x00]);

    assert_eq!(cpu.mem_read(0x10), 0x1e);
    assert_eq!(cpu.status.contains(CpuFlags::CARRY), false);
  }

  #[test]
  fn test_bcc() {
    let mut cpu = CPU::new();
    cpu.load_and_run(vec![0x90,0x02,0x00,0x00,0x00,0x00,0x00]);
    assert_eq!(cpu.program_counter, 0x8004+0x1);
  }
}