use crate::cpu::AddressingMode;
use crate::cpu::Mem;
use crate::cpu::CPU;
use crate::opcodes;
use std::collections::HashMap;

use std::fmt::Write;

pub fn trace(cpu: &mut CPU) -> String {
  //  byte_codes
  let mut byte_codes = String::new();
  let ref opcodes: HashMap<u8, &'static opcodes::OpCode> = *opcodes::OPCODES_MAP;

  let code = cpu.mem_read(cpu.program_counter);
  let opcode = opcodes.get(&code).expect("No opcode");
  for i in 0..opcode.len {
    write!(byte_codes,"{:02x} ", cpu.mem_read(cpu.program_counter+(i as u16)));
  }

  // asm_str
  let mut asm_str = String::new();
  write!(asm_str, "{} ", opcode.mnemonic);
  let (mem_addr, stored_value) = match opcode.mode {
    AddressingMode::Immediate | AddressingMode::NoneAddressing => (0,0),
    _ => {
      let addr = cpu.get_absolute_address(&opcode.mode, cpu.program_counter+1);
      (addr, cpu.mem_read(addr))
    }
  };

  let _ = match opcode.len {
    1 => match opcode.code {
      // Accumulator
      0x0a | 0x4a | 0x2a | 0x6a => write!(asm_str, "A "),
      _ => write!(asm_str,""),
    },
    2 => {
      let address = cpu.mem_read(cpu.program_counter + 1);
      match opcode.mode {
        AddressingMode::Immediate => 
          write!(asm_str, "#${:02x}", address),
        AddressingMode::ZeroPage => 
          write!(asm_str, "${:02x} = {:02x}", mem_addr, stored_value),
        AddressingMode::ZeroPage_X => 
          write!(asm_str, "${:02x},X @ {:02x} = {:02x}", address, mem_addr, stored_value),
        AddressingMode::ZeroPage_Y => 
          write!(asm_str, "${:02x},Y @ {:02x} = {:02x}", address, mem_addr, stored_value),
        AddressingMode::Indirect_X => 
          write!(asm_str, "(${:02x},X) @ {:02x} = {:04x} = {:02x}", 
            address, address.wrapping_add(cpu.register_x), mem_addr, stored_value
          ),
        AddressingMode::Indirect_Y => 
          write!(asm_str, "(${:02x}),Y = {:04x} @ {:04x} = {:02x}", 
            address, mem_addr.wrapping_sub(cpu.register_y as u16), mem_addr, stored_value
          ),
        AddressingMode::NoneAddressing => {
          // BNE, BVS, etc...
          let jump: usize = (cpu.program_counter as usize + 2).wrapping_add((address as i8) as usize);
          write!(asm_str,"${:04x}", jump)
        }
        _ => panic!("unexpected addressing mode {:?} has ops-len 2. code {:02x}", opcode.mode, opcode.code)
      }
    }
    3 => {
      let address_lo = cpu.mem_read(cpu.program_counter + 1);
      let address_hi = cpu.mem_read(cpu.program_counter + 2);

      let address = cpu.mem_read_u16(cpu.program_counter + 1);
      match opcode.mode {
        AddressingMode::NoneAddressing => {
          if opcode.code == 0x6c {
            // jmp indirect (6502 has a bug)
            let jmp_addr = if address & 0x00FF == 0x00FF {
              let lo = cpu.mem_read(address);
              let hi = cpu.mem_read(address & 0xFF00);
              (hi as u16) << 8 | (lo as u16)
            } else {
              cpu.mem_read_u16(address)  
            };

            write!(asm_str, "(${:04x}) = {:04x}", address, jmp_addr)
          } else {
            write!(asm_str, "${:04x}", address)
          }
        }
        AddressingMode::Absolute => write!(asm_str, "${:04x} = {:02x}", mem_addr, stored_value),
        AddressingMode::Absolute_X => write!(asm_str, "${:04x},X @ {:04x} = {:02x}", address, mem_addr, stored_value),
        AddressingMode::Absolute_Y => write!(asm_str, "${:04x},Y @ {:04x} = {:02x}", address, mem_addr, stored_value),
        _ => panic!("unexpected addressing mode {:?} has ops-len 3. code {:02x}", opcode.mode, opcode.code),
      }
    } 
    _ => panic!("invalid opcode length")
  };

  format!(
      "{:04x}  {:9} {:31} A:{:02x} X:{:02x} Y:{:02x} P:{:02x} SP:{:02x}",
      cpu.program_counter, byte_codes, asm_str, cpu.register_a, cpu.register_x, cpu.register_y, cpu.status, cpu.stack_pointer,
  )
  .to_ascii_uppercase()

}

#[cfg(test)]
mod test {
  use super::*;
  use crate::bus::Bus;
  use crate::cartridge::test::test_rom;

  #[test]
  fn test_format_trace() {
    let mut bus = Bus::new(test_rom());
    bus.mem_write(100, 0xa2);
    bus.mem_write(101, 0x01);
    bus.mem_write(102, 0xca);
    bus.mem_write(103, 0x88);
    bus.mem_write(104, 0x00);

    let mut cpu = CPU::new(bus);
    cpu.program_counter = 0x64;
    cpu.register_a = 1;
    cpu.register_x = 2;
    cpu.register_y = 3;
    let mut result: Vec<String> = vec![];
    cpu.run_with_callback(|cpu| {
      result.push(trace(cpu));
    });

    assert_eq!(
      "0064  A2 01     LDX #$01                        A:01 X:02 Y:03 P:24 SP:FD",
      result[0]
    );
    assert_eq!(
      "0066  CA        DEX                             A:01 X:01 Y:03 P:24 SP:FD",
      result[1]
    );
    assert_eq!(
      "0067  88        DEY                             A:01 X:00 Y:03 P:26 SP:FD",
      result[2]
    );
  }

  #[test]
  fn test_format_mem_access() {
    let mut bus = Bus::new(test_rom());
    // ORA ($33), Y
    bus.mem_write(100, 0x11);
    bus.mem_write(101, 0x33);

    //data
    bus.mem_write(0x33, 00);
    bus.mem_write(0x34, 04);

    //target cell
    bus.mem_write(0x400, 0xAA);

    let mut cpu = CPU::new(bus);
    cpu.program_counter = 0x64;
    cpu.register_y = 0;
    let mut result: Vec<String> = vec![];
    cpu.run_with_callback(|cpu| {
        result.push(trace(cpu));
    });
    assert_eq!(
        "0064  11 33     ORA ($33),Y = 0400 @ 0400 = AA  A:00 X:00 Y:00 P:24 SP:FD",
        result[0]
    );
  }
}