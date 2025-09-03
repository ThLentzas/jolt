pub fn is_rfc_whitespace(byte: u8) -> bool {
    matches!(byte, b'\t' | b'\n' | b'\r' | b' ')
}

pub fn map_to_text(byte: u8) -> &'static str {
    match byte {
        0x00 => "<NUL>",
        0x01 => "<SOH>",
        0x02 => "<STX>",
        0x03 => "<ETX>",
        0x04 => "<EOT>",
        0x05 => "<ENQ>",
        0x06 => "<ACK>",
        0x07 => "<BEL>",
        0x08 => "<BS>",
        0x09 => "<TAB>",
        0x0A => "<LF>",
        0x0B => "<VT>",
        0x0C => "<FF>",
        0x0D => "<CR>",
        0x0E => "<SO>",
        0x0F => "<SI>",
        0x10 => "<DLE>",
        0x11 => "<DC1>",
        0x12 => "<DC2>",
        0x13 => "<DC3>",
        0x14 => "<DC4>",
        0x15 => "<NAK>",
        0x16 => "<SYN>",
        0x17 => "<ETB>",
        0x18 => "<CAN>",
        0x19 => "<EM>",
        0x1A => "<SUB>",
        0x1B => "<ESC>",
        0x1C => "<FS>",
        0x1D => "<GS>",
        0x1E => "<RS>",
        0x1F => "<US>",
        0x7F => "<DEL>",
        _ => "Unknown Control Character",
    }
}
