use crate::cartridge::Mirroring;
use registers::control::ControlRegister;
use registers::mask::MaskRegister;
use registers::scroll::ScrollRegister;
use registers::addr::AddrRegister;
use registers::status::StatusRegister;

pub mod registers;
pub struct NesPPU {
  pub chr_rom: Vec<u8>,
  pub mirroring: Mirroring,
  pub ctrl: ControlRegister,
  pub mask: MaskRegister, /* $0x2001 */
  pub status: StatusRegister,
  pub scroll: ScrollRegister,
  pub addr: AddrRegister,
  pub vram: [u8; 2048],

  pub oam_addr: u8,
  pub oam_data: [u8; 256],
  pub palette_table: [u8; 32],
  
  internal_data_buf: u8,
}

pub trait PPU {
  fn write_to_ctrl(&mut self, value: u8);
  fn write_to_mask(&mut self, value: u8);
  fn read_status(&self) -> u8;
  fn write_to_scroll(&mut self, value: u8);

  fn write_to_oam_addr(&mut self, value: u8);
  fn write_to_oam_data(&mut self, value: u8);
  fn read_oam_data(&self) -> u8;
  fn wriet_oam_dma(&mut self, data: &[u8; 256]);

  fn write_to_ppu_addr(&mut self, value: u8);
  fn read_data(&mut self) -> u8;
  fn write_data(&mut self, value: u8);
}

impl NesPPU {
  pub fn new_empty_rom() -> Self {
    NesPPU::new(vec![0; 2048], Mirroring::HORIZONTAL)
  }

  pub fn new(chr_rom: Vec<u8>, mirroring: Mirroring) -> Self {
    NesPPU {
      chr_rom: chr_rom,
      mirroring: mirroring,
      ctrl: ControlRegister::new(),
      mask: MaskRegister::new(),
      status: StatusRegister::new(),
      scroll: ScrollRegister::new(),
      addr: AddrRegister::new(),
      vram: [0; 2048],
      oam_addr: 0,
      oam_data: [0; 64 * 4],
      palette_table: [0; 32],
      internal_data_buf: 0,
    }
  }
 
  fn increment_vram_addr(&mut self) {
    self.addr.increment(self.ctrl.vram_addr_increment());
  }
  
  fn mirror_vram_addr(&self, addr: u16) -> u16{
    let mirrored_vram = addr & 0b10_1111_1111_1111;
    let vram_index = mirrored_vram - 0x2000;
    let name_table = vram_index / 0x400;
    match (&self.mirroring, name_table) {
      (Mirroring::VERTICAL, 2)   | (Mirroring::VERTICAL, 3)   => vram_index - 0x800,
      (Mirroring::HORIZONTAL, 1) | (Mirroring::HORIZONTAL, 2) => vram_index - 0x400,
      (Mirroring::HORIZONTAL, 3) => vram_index - 0x800,
      _ => vram_index,
    }
  }
}

impl PPU for NesPPU {
  fn read_status(&self) -> u8 {
    self.status.get()
  }

  fn write_to_ppu_addr(&mut self, value: u8) {
    self.addr.update(value);
  }

  fn write_to_ctrl(&mut self, value: u8) {
    self.ctrl.update(value);
  }

  fn write_to_mask(&mut self, value: u8){
    self.mask.update(value);
  }

  fn write_to_scroll(&mut self, value: u8){
    self.scroll.update(value);
  }

  fn read_data(&mut self) -> u8 {
    let addr = self.addr.get();
    self.increment_vram_addr();

    match addr {
      0..=0x1fff => {
        let result = self.internal_data_buf;
        self.internal_data_buf = self.chr_rom[addr as usize];
        result
      }
      0x2000..=0x2fff => {
        let result = self.internal_data_buf;
        self.internal_data_buf = self.vram[self.mirror_vram_addr(addr) as usize];
        result
      }
      0x3000..=0x3eff => panic!("addr space 0x3000..0x3eff is not expected to be used, requested = {}", addr),
      /* mirror palette */
      0x3f10 | 0x3f14 | 0x3f18 | 0x3f1c => {
        let addr_mirror = addr - 0x10;
        self.palette_table[(addr_mirror - 0x3f00) as usize]
      }
      0x3f00..=0x3fff => {
        self.palette_table[(addr - 0x3f00) as usize]
      }
      _ => panic!("unexpected access to mirrored space {}", addr),
    }
  }

  fn write_data(&mut self, data: u8) {
    let addr = self.addr.get();
    self.increment_vram_addr();

    match addr {
      0..=0x1fff => panic!("can't write on chr_rom"),
      0x2000..=0x2fff => {
        self.vram[self.mirror_vram_addr(addr) as usize] = data;
      }
      0x3000..=0x3eff => panic!("addr space 0x3000..0x3eff is not expected to be used, requested = {}", addr),
      0x3f10 | 0x3f14 | 0x3f18 | 0x3f1c => {
        let addr_mirror = addr -0x10;
        self.palette_table[(addr_mirror - 0x3f00) as usize] = data;
      }
      0x3f00..=0x3fff =>  {
        self.palette_table[(addr - 0x3f00) as usize] = data;
      }
      _ => panic!("unexpected access to mirrored space{}", addr),
    }
  }
 
  fn write_to_oam_addr(&mut self, value: u8){
    self.oam_addr = value;
  }

  fn write_to_oam_data(&mut self, value: u8){
    self.oam_data[self.oam_addr as usize] = value;
    self.oam_addr = self.oam_addr.wrapping_add(1);
  }

  fn read_oam_data(&self) -> u8 {
    self.oam_data[self.oam_addr as usize]
  }

  fn wriet_oam_dma(&mut self, data: &[u8; 256]) {
    for x in data.iter() {
      self.write_to_oam_data(*x);
    }
  }
}