arch n64.cpu
endian msb

cart_read_rom:
cart_read_ram:
ppu_read_ram:
ppu_read_vram:

bus_read_table:
    dw cart_read_rom        // $0000-$0fff    // TODO: support for boot rom?
    dw cart_read_rom        // $1000-$7fff
    dw cart_read_rom
    dw cart_read_rom
    dw cart_read_rom
    dw cart_read_rom
    dw cart_read_rom
    dw cart_read_rom
    dw ppu_read_vram        // $8000-$9fff
    dw ppu_read_vram
    dw cart_read_ram        // $a000-$bfff
    dw cart_read_ram
    dw bus_read_wram_0      // $c000-$cfff
    dw bus_read_wram_1_7    // $d000-$dfff
    dw bus_read_wram_0      // $e000-$efff
    dw bus_read_fxxx        // $f000-$ffff

bus_read_io_table:
    dw joy_read_p1          // $ff00
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw timer_read_div       // $ff04
    dw timer_read_tima      // $ff05
    dw timer_read_tma       // $ff06
    dw timer_read_tac       // $ff07
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw cpu_read_if          // $ff0f
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw bus_read_io_none
    dw ppu_read_lcdc        // $ff40
    dw ppu_read_stat        // $ff41
    dw ppu_read_scy         // $ff42
    dw ppu_read_scx         // $ff43
    dw ppu_read_ly          // $ff44
    dw ppu_read_lyc         // $ff45
    dw ppu_read_dma         // $ff46
    dw ppu_read_bgp         // $ff47
    dw ppu_read_obp0        // $ff48
    dw ppu_read_obp1        // $ff49
    dw ppu_read_wy          // $ff4a
    dw ppu_read_wx          // $ff4b
    dw ppu_read_key0        // $ff4c
    dw ppu_read_key1        // $ff4d
    dw bus_read_io_none
    dw ppu_read_vbk         // $ff4f

hram:
    data_array($80)

wram:
    data_array($8000)

current_wram_bank:
    db 0

bus_read:  // byte(hword addr)
    la      t0, bus_read_table
    srl     t1, a0, 9
    andi    t1, t1, $78
    addu    t0, t0, t1
    lw      t0, 0(t0)
    jr      t0
    nop

bus_read_wram_0:  // byte(hword addr)
    la      t0, wram
    andi    t1, a0, $fff
    addu    t0, t0, t1
    jr      ra
    lbu     v0, 0(t0)

bus_read_wram_1_7:  // byte(hword addr)
    la      t0, wram
    andi    t1, a0, $fff
    addu    t0, t0, t1
    la      t1, current_wram_bank
    lbu     t1, 0(t1)
    sll     t1, t1, 12      // * $1000
    addu    t0, t0, t1
    jr      ra
    lbu     v0, 0(t0)

bus_read_fxxx:  // byte(hword addr)
    andi    t0, a0, $fff
    slti    t1, t0, $f00
    bne     t1, zero, bus_read_f000_feff
    slti    t1, t0, $f80
    beq     t1, zero, bus_read_ff80_ffff
    la      t1, bus_read_io_table
    andi    t0, a0, $ff
    sll     t0, t0, 3
    addu    t1, t1, t0
    lw      t1, 0(t1)
    jr      t1
    nop
bus_read_f000_feff:
bus_read_ff80_ffff:

bus_read_io_none:
    jr      ra
    lli     v0, $ff

bus_write:  // void(hword addr, byte data)
    la      t0, bus_write_table
    srl     t1, a0, 9
    andi    t1, t1, $78
    addu    t0, t0, t1
    lw      t0, 0(t0)
    jr      t0
    nop

bus_write_wram_0:  // void(hword addr, byte data)
    la      t0, wram
    andi    t1, a0, $fff
    addu    t0, t0, t1
    jr      ra
    sb      a1, 0(t0)

bus_write_wram_1_7:  // void(hword addr, byte data)
    la      t0, wram
    andi    t1, a0, $fff
    addu    t0, t0, t1
    la      t1, current_wram_bank
    lbu     t1, 0(t1)
    sll     t1, t1, 12      // * $1000
    addu    t0, t0, t1
    jr      ra
    sb      a1, 0(t0)