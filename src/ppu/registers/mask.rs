/* PPU status register at 0x2001 */
bitflags! {
  pub struct MaskRegister: u8 {
    const GREYSCALE                 = 0b00000001;
    const LEFTMOST_8PXL_BACKGROUND  = 0b00000010;
    const LEFTMOST_8PXL_SPRITE      = 0b00000100;
    const SHOW_BACKGROUND           = 0b00001000;
    const SHOW_SPRITES              = 0b00010000;
    const EMPHASISE_RED             = 0b00100000;
    const EMPHASISE_GREEN           = 0b01000000;
    const EMPHASISE_BLUE            = 0b10000000;
  }
}

pub enum Color {
  Red,
  Green,
  Blue,
}

impl MaskRegister {
  pub fn new() -> Self {
    MaskRegister::from_bits_truncate(0b00000000)
  }

  pub fn update(&mut self, data: u8) {
    self.bits = data;
  }
}

