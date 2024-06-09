arch n64.cpu
endian msb

// globally reserved registers
constant cycle_counter  = k0
constant pc             = k1
constant gb_sp          = fp
constant reg_ptr        = gp

constant CPU_M_CYCLES_PER_FRAME = 17556

define B_PTR                        = cpu_b-cpu_reg_start(reg_ptr)
define BC_PTR                       = cpu_b-cpu_reg_start(reg_ptr)
define C_PTR                        = cpu_c-cpu_reg_start(reg_ptr)
define D_PTR                        = cpu_d-cpu_reg_start(reg_ptr)
define DE_PTR                       = cpu_d-cpu_reg_start(reg_ptr)
define E_PTR                        = cpu_e-cpu_reg_start(reg_ptr)
define H_PTR                        = cpu_h-cpu_reg_start(reg_ptr)
define HL_PTR                       = cpu_h-cpu_reg_start(reg_ptr)
define L_PTR                        = cpu_l-cpu_reg_start(reg_ptr)
define A_PTR                        = cpu_a-cpu_reg_start(reg_ptr)
define F_PTR                        = cpu_f-cpu_reg_start(reg_ptr)
define CARRY_PTR                    = cpu_carry-cpu_reg_start(reg_ptr)
define HALF_PTR                     = cpu_half-cpu_reg_start(reg_ptr)
define NEG_PTR                      = cpu_neg-cpu_reg_start(reg_ptr)
define ZERO_PTR                     = cpu_zero-cpu_reg_start(reg_ptr)
define PC_PTR                       = cpu_pc-cpu_reg_start(reg_ptr)
define SP_PTR                       = cpu_sp-cpu_reg_start(reg_ptr)
define IE_PTR                       = cpu_ie-cpu_reg_start(reg_ptr)
define IF_PTR                       = cpu_if-cpu_reg_start(reg_ptr)
define IME_PTR                      = cpu_ime-cpu_reg_start(reg_ptr)
define HALTED_PTR                   = cpu_halted-cpu_reg_start(reg_ptr)
define SHOULD_CHECK_INTERRUPTS_PTR  = cpu_should_check_interrupts-cpu_reg_start(reg_ptr)

cpu_reg_start:
cpu_b:
    db 0

cpu_c:
    db 0

cpu_d:
    db 0

cpu_e:
    db 0

cpu_h:
    db 0

cpu_l:
    db 0

fill(1)  // gap for (HL) operand in instruction encoding

cpu_a:
    db 0

cpu_f:
cpu_carry:
    db 0

cpu_half:
    db 0

cpu_neg:
    db 0

cpu_zero:
    db 0

cpu_pc:
    dh 0

cpu_sp:
    dh 0

cpu_ie:
    db 0

cpu_if:
    db 0

cpu_ime:
    db 0

cpu_halted:
    db 0

cpu_should_check_interrupts:
    db 0 

cpu_instr_table:
    dw cpu_nop      , cpu_ld_bc_u16, cpu_ld_bc_a , cpu_inc_bc, cpu_inc_r8 , cpu_dec_r8 , cpu_ld_r8_u8, cpu_rlca
    dw cpu_ld_u16_sp, cpu_add_hl_bc, cpu_ld_a_bc , cpu_dec_bc, cpu_inc_r8 , cpu_dec_r8 , cpu_ld_r8_u8, cpu_rrca
    dw cpu_stop     , cpu_ld_de_u16, cpu_ld_de_a , cpu_inc_de, cpu_inc_r8 , cpu_dec_r8 , cpu_ld_r8_u8, cpu_rla
    dw cpu_jr       , cpu_add_hl_de, cpu_ld_a_de , cpu_dec_de, cpu_inc_r8 , cpu_dec_r8 , cpu_ld_r8_u8, cpu_rra
    dw cpu_jr_nz    , cpu_ld_hl_u16, cpu_ld_hlp_a, cpu_inc_hl, cpu_inc_r8 , cpu_dec_r8 , cpu_ld_r8_u8, cpu_daa
    dw cpu_jr_z     , cpu_add_hl_hl, cpu_ld_a_hlp, cpu_dec_hl, cpu_inc_r8 , cpu_dec_r8 , cpu_ld_r8_u8, cpu_cpl
    dw cpu_jr_nc    , cpu_ld_sp_u16, cpu_ld_hlm_a, cpu_inc_sp, cpu_inc_hlm, cpu_dec_hlm, cpu_ld_r8_u8, cpu_scf
    dw cpu_jr_c     , cpu_add_hl_sp, cpu_ld_a_hlm, cpu_dec_sp, cpu_inc_r8 , cpu_dec_r8 , cpu_ld_r8_u8, cpu_ccf

    dw cpu_nop     , cpu_ld_r8_r8, cpu_ld_r8_r8, cpu_ld_r8_r8, cpu_ld_r8_r8, cpu_ld_r8_r8, cpu_ld_r8_hl, cpu_ld_r8_r8
    dw cpu_ld_r8_r8, cpu_nop     , cpu_ld_r8_r8, cpu_ld_r8_r8, cpu_ld_r8_r8, cpu_ld_r8_r8, cpu_ld_r8_hl, cpu_ld_r8_r8
    dw cpu_ld_r8_r8, cpu_ld_r8_r8, cpu_nop     , cpu_ld_r8_r8, cpu_ld_r8_r8, cpu_ld_r8_r8, cpu_ld_r8_hl, cpu_ld_r8_r8
    dw cpu_ld_r8_r8, cpu_ld_r8_r8, cpu_ld_r8_r8, cpu_nop     , cpu_ld_r8_r8, cpu_ld_r8_r8, cpu_ld_r8_hl, cpu_ld_r8_r8
    dw cpu_ld_r8_r8, cpu_ld_r8_r8, cpu_ld_r8_r8, cpu_ld_r8_r8, cpu_nop     , cpu_ld_r8_r8, cpu_ld_r8_hl, cpu_ld_r8_r8
    dw cpu_ld_r8_r8, cpu_ld_r8_r8, cpu_ld_r8_r8, cpu_ld_r8_r8, cpu_ld_r8_r8, cpu_nop     , cpu_ld_r8_hl, cpu_ld_r8_r8
    dw cpu_ld_hl_r8, cpu_ld_hl_r8, cpu_ld_hl_r8, cpu_ld_hl_r8, cpu_ld_hl_r8, cpu_ld_hl_r8, cpu_halt    , cpu_ld_hl_r8
    dw cpu_ld_r8_r8, cpu_ld_r8_r8, cpu_ld_r8_r8, cpu_ld_r8_r8, cpu_ld_r8_r8, cpu_ld_r8_r8, cpu_ld_r8_hl, cpu_nop

    dw cpu_add_r8, cpu_add_r8, cpu_add_r8, cpu_add_r8, cpu_add_r8, cpu_add_r8, cpu_add_hl, cpu_add_r8
    dw cpu_adc_r8, cpu_adc_r8, cpu_adc_r8, cpu_adc_r8, cpu_adc_r8, cpu_adc_r8, cpu_adc_hl, cpu_adc_r8
    dw cpu_sub_r8, cpu_sub_r8, cpu_sub_r8, cpu_sub_r8, cpu_sub_r8, cpu_sub_r8, cpu_sub_hl, cpu_sub_r8
    dw cpu_sbc_r8, cpu_sbc_r8, cpu_sbc_r8, cpu_sbc_r8, cpu_sbc_r8, cpu_sbc_r8, cpu_sbc_hl, cpu_sbc_r8
    dw cpu_and_r8, cpu_and_r8, cpu_and_r8, cpu_and_r8, cpu_and_r8, cpu_and_r8, cpu_and_hl, cpu_and_r8
    dw cpu_xor_r8, cpu_xor_r8, cpu_xor_r8, cpu_xor_r8, cpu_xor_r8, cpu_xor_r8, cpu_xor_hl, cpu_xor_r8
    dw cpu_or_r8 , cpu_or_r8 , cpu_or_r8 , cpu_or_r8 , cpu_or_r8 , cpu_or_r8 , cpu_or_hl , cpu_or_r8 
    dw cpu_cp_r8 , cpu_cp_r8 , cpu_cp_r8 , cpu_cp_r8 , cpu_cp_r8 , cpu_cp_r8 , cpu_cp_hl , cpu_cp_r8 

    dw cpu_ret_nz     , cpu_pop_bc  , cpu_jp_u16_nz, cpu_jp_u16 , cpu_call_nz, cpu_push_bc, cpu_add_u8, cpu_rst
    dw cpu_ret_z      , cpu_ret     , cpu_jp_u16_z , cpu_cb     , cpu_call_z , cpu_call   , cpu_adc_u8, cpu_rst
    dw cpu_ret_nc     , cpu_pop_de  , cpu_jp_u16_nc, cpu_illegal, cpu_call_nc, cpu_push_de, cpu_sub_u8, cpu_rst
    dw cpu_ret_c      , cpu_reti    , cpu_jp_u16_c , cpu_illegal, cpu_call_c , cpu_illegal, cpu_sbc_u8, cpu_rst
    dw cpu_ldh_u8_a   , cpu_pop_hl  , cpu_ldh_c_a  , cpu_illegal, cpu_illegal, cpu_push_hl, cpu_and_u8, cpu_rst
    dw cpu_add_sp     , cpu_jp_hl   , cpu_ld_u16_a , cpu_illegal, cpu_illegal, cpu_illegal, cpu_xor_u8, cpu_rst
    dw cpu_ldh_a_u8   , cpu_pop_af  , cpu_ldh_a_c  , cpu_di     , cpu_illegal, cpu_push_af, cpu_or_u8 , cpu_rst
    dw cpu_ld_hl_sp_s8, cpu_ld_sp_hl, cpu_ld_a_u16 , cpu_ei     , cpu_illegal, cpu_illegal, cpu_cp_u8 , cpu_rst

cpu_cb_table:
    dw cpu_rlc, cpu_rrc, cpu_rl , cpu_rr , cpu_sla, cpu_sra, cpu_swap, cpu_srl
    dw cpu_bit, cpu_bit, cpu_bit, cpu_bit, cpu_bit, cpu_bit, cpu_bit , cpu_bit
    dw cpu_res, cpu_res, cpu_res, cpu_res, cpu_res, cpu_res, cpu_res , cpu_res
    dw cpu_set, cpu_set, cpu_set, cpu_set, cpu_set, cpu_set, cpu_set , cpu_set

cpu_cycle_table:
    db 1, 3, 2, 2, 1, 1, 2, 1
    db 5, 2, 2, 2, 1, 1, 2, 1
    db 1, 3, 2, 2, 1, 1, 2, 1
    db 2, 2, 2, 2, 1, 1, 2, 1
    db 2, 3, 2, 2, 1, 1, 2, 1
    db 2, 2, 2, 2, 1, 1, 2, 1
    db 2, 3, 2, 2, 3, 3, 3, 1
    db 2, 2, 2, 2, 1, 1, 2, 1

    db 1, 1, 1, 1, 1, 1, 2, 1
    db 1, 1, 1, 1, 1, 1, 2, 1
    db 1, 1, 1, 1, 1, 1, 2, 1
    db 1, 1, 1, 1, 1, 1, 2, 1
    db 1, 1, 1, 1, 1, 1, 2, 1
    db 1, 1, 1, 1, 1, 1, 2, 1
    db 2, 2, 2, 2, 2, 2, 1, 2
    db 1, 1, 1, 1, 1, 1, 2, 1

    db 1, 1, 1, 1, 1, 1, 2, 1
    db 1, 1, 1, 1, 1, 1, 2, 1
    db 1, 1, 1, 1, 1, 1, 2, 1
    db 1, 1, 1, 1, 1, 1, 2, 1
    db 1, 1, 1, 1, 1, 1, 2, 1
    db 1, 1, 1, 1, 1, 1, 2, 1
    db 1, 1, 1, 1, 1, 1, 2, 1
    db 1, 1, 1, 1, 1, 1, 2, 1

macro op_r8(op) {
    andi    t0, a0, 7
    addu    t0, t0, reg_ptr
    j       {op}
    lbu     a0, 0(t0)
}

macro op_hl(op) {
    daddiu  sp, sp, -8
    sd      ra, 0(sp)
    jal     cpu_read
    lhu     a0, {HL_PTR}
    ld      ra, 0(sp)
    daddiu  sp, sp, 8
    j       {op}
    move    a0, v0
}

macro op_u8(op) {
    sd      ra, -8(sp)
    jal     cpu_read8
    daddiu  sp, sp, -8
    ld      ra, 0(sp)
    daddiu  sp, sp, 8
    j       {op}
    move    a0, v0
}

cpu_run:  // void()
    daddiu  sp, sp, -32
    sd      ra, 0(sp)
    sd      s0, 8(sp)
    sd      s1, 16(sp)
    sd      s2, 24(sp)
    la      s0, cpu_instr_table
    la      s1, cpu_cycle_table
    li      s2, CPU_M_CYCLES_PER_FRAME
    move    cycle_counter, zero
cpu_run_loop:
    // check interrupts
    lb      t0, {SHOULD_CHECK_INTERRUPTS_PTR}
    beq     t0, zero, cpu_fetch_instr
    lb      t0, {IME_PTR}
    beq     t0, zero, cpu_check_halted
    lbu     t0, {IE_PTR}
    lbu     t1, {IF_PTR}
    and     t0, t0, t1
    beql    t0, zero, cpu_check_halted
    nop
    jal     cpu_push_pc
    addiu   cycle_counter, cycle_counter, 5
    lli     t2, 1
    lli     pc, $38
cpu_check_interrupt:
    addiu   pc, pc, 8
    and     t3, t0, t2
    beql    t2, zero, cpu_check_interrupt
    sll     t2, t2, 1
    xori    t2, t2, $ff
    and     t1, t1, t2
    sb      t1, {IF_PTR}
    j       cpu_fetch_instr
    sb      zero, {IME_PTR}
cpu_check_halted:
    lb      t0, {HALTED_PTR}
    beq     t0, zero, cpu_fetch_instr
    addiu   cycle_counter, cycle_counter, t0
    lbu     t0, {IE_PTR}
    lbu     t1, {IF_PTR}
    and     t0, t0, t1
    bnel    t0, zero, cpu_fetch_instr
    sb      zero, {HALTED_PTR}
cpu_fetch_instr:
    jal     cpu_read
    move    a0, pc
    addiu   pc, pc, 1
    andi    pc, pc, $ffff
    addu    t0, v0, s1
    lb      t0, 0(t0)
    addu    cycle_counter, cycle_counter, t0
    sll     t0, v0, 3
    addu    t0, t0, s0
    lw      t0, 0(t0)
    jalr    t0
    move    a0, v0
    slt     t0, cycle_counter, s2
    bnel    t0, zero, cpu_run_loop
    nop
    ld      ra, 0(sp)
    ld      s0, 8(sp)
    ld      s1, 16(sp)
    ld      s2, 24(sp)
    jr      ra
    daddiu  sp, sp, 32

cpu_adc:  // void(byte operand)
    lbu     t0, {A_PTR}
    lb      t1, {CARRY_PTR}
    addu    t2, t0, a0
    addu    t2, t2, t1
    srl     t3, t2, 8
    sb      t3, {CARRY_PTR}
    sb      zero, {NEG_PTR}
    andi    t3, t0, 15
    andi    t4, a0, 15
    addu    t3, t3, t4
    addu    t3, t3, t1
    srl     t3, t3, 4
    sb      t3, {HALF_PTR}
    andi    t0, t2, $ff
    slti    t1, t0, 1
    sb      t1, {ZERO_PTR}
    jr      ra
    sb      t0, {A_PTR}

cpu_adc_hl:  // void()
    op_hl(cpu_adc)

cpu_adc_r8:  // void(byte opcode)
    op_r8(cpu_adc)

cpu_adc_u8:  // void()
    op_u8(cpu_adc)

cpu_add:  // void(byte operand)
    lbu     t0, {A_PTR}
    addu    t1, t0, a0
    srl     t2, t1, 8
    sb      t2, {CARRY_PTR}
    sb      zero, {NEG_PTR}
    andi    t2, t0, 15
    andi    t3, a0, 15
    addu    t2, t2, t3
    srl     t2, t2, 4
    sb      t2, {HALF_PTR}
    andi    t0, t1, $ff
    slti    t1, t0, 1
    sb      t1, {ZERO_PTR}
    jr      ra
    sb      t0, {A_PTR}

cpu_add_hl:  // void()
    op_hl(cpu_add)

cpu_add_r8:  // void(byte opcode)
    op_r8(cpu_add)

cpu_add_u8:  // void()
    op_u8(cpu_add)

cpu_add_hl_r16:  // void(hword r16)
    lhu     t0, {HL_PTR}
    addu    t1, t0, a0
    sh      t1, {HL_PTR}
    srl     t1, t1, 16
    sb      t1, {CARRY_PTR}
    andi    t0, t0, $fff
    andi    a0, a0, $fff
    addu    t0, t0, a0
    srl     t0, t0, 12
    sb      t0, {HALF_PTR}
    jr      ra
    sb      zero, {NEG_PTR}

cpu_add_hl_bc:  // void()
    j       cpu_add_hl_r16
    lhu     a0, {BC_PTR}

cpu_add_hl_de:  // void()
    j       cpu_add_hl_r16
    lhu     a0, {DE_PTR}

cpu_add_hl_hl:  // void()
    j       cpu_add_hl_r16
    lhu     a0, {HL_PTR}

cpu_add_hl_sp:  // void()
    j       cpu_add_hl_r16
    move    a0, gb_sp

cpu_add_sp:  // void()
    sd      ra, -8(sp)
    jal     cpu_read8
    daddiu  sp, sp, -8
    sll     t0, v0, 24
    sra     t0, t0, 24
    addu    t0, gb_sp, t0
    andi    t1, gb_sp, $ff
    addu    t2, t1, v0
    srl     t2, t2, 8
    sb      t2, {CARRY_PTR}
    andi    t1, t1, $f
    andi    v0, v0, $f
    addu    t2, t1, v0
    srl     t2, t2, 4
    sb      t2, {HALF_PTR}
    sh      zero, {NEG_PTR}   // NEG := 0, ZERO := 0
    andi    gb_sp, t0, $ffff
    ld      ra, 0(sp)
    jr      ra
    daddiu  sp, sp, 8

cpu_and:  // void(byte operand)
    lbu     t0, {A_PTR}
    and     t0, t0, a0
    sw      zero, {F_PTR}
    slti    t1, t0, 1
    sb      t1, {ZERO_PTR}
    lli     t1, 1
    sb      t1, {HALF_PTR}
    jr      ra
    sb      t0, {A_PTR}

cpu_and_hl:  // void()
    op_hl(cpu_and)

cpu_and_r8:  // void(byte opcode)
    op_r8(cpu_and)

cpu_and_u8:  // void()
    op_u8(cpu_and)

cpu_cp:  // void(byte operand)
    lbu     t0, {A_PTR}
    subu    t1, t0, a0
    sltiu   t1, t1, 1
    sb      t1, {ZERO_PTR}
    slt     t1, t0, a0
    sb      t1, {CARRY_PTR}
    andi    t1, t0, 15
    andi    a0, a0, 15
    slt     t1, t1, a0
    sb      t1, {HALF_PTR}
    lli     t1, 1
    jr      ra
    sb      t1, {NEG_PTR}

cpu_cp_hl:  // void()
    op_hl(cpu_cp)

cpu_cp_r8:  // void(byte opcode)
    op_r8(cpu_cp)

cpu_cp_u8:  // void()
    op_u8(cpu_cp)

cpu_or:  // void(byte operand)
    lbu     t0, {A_PTR}
    or      t0, t0, a0
    sw      zero, {F_PTR}
    slti    t1, t0, 1
    sb      t1, {ZERO_PTR}
    jr      ra
    sb      t0, {A_PTR}

cpu_or_hl:  // void()
    op_hl(cpu_or)

cpu_or_r8:  // void(byte opcode)
    op_r8(cpu_or)

cpu_or_u8:  // void()
    op_u8(cpu_or)

cpu_sbc:  // void(byte operand)
    lbu     t0, {A_PTR}
    lb      t1, {CARRY_PTR}
    subu    t2, t0, a0
    subu    t2, t2, t1
    slt     t3, t2, zero
    sb      t3, {CARRY_PTR}
    lli     t3, 1
    sb      t3, {NEG_PTR}
    andi    t3, t0, 15
    andi    t4, a0, 15
    subu    t3, t3, t4
    subu    t3, t3, t1
    slt     t3, t3, zero
    sb      t3, {HALF_PTR}
    andi    t0, t2, $ff
    slti    t1, t0, 1
    sb      t1, {ZERO_PTR}
    jr      ra
    sb      t0, {A_PTR}

cpu_sbc_hl:  // void()
    op_hl(cpu_sbc)

cpu_sbc_r8:  // void(byte opcode)
    op_r8(cpu_sbc)

cpu_sbc_u8:  // void()
    op_u8(cpu_sbc)

cpu_sub:  // void(byte operand)
    lbu     t0, {A_PTR}
    subu    t1, t0, a0
    slt     t2, t1, zero
    sb      t2, {CARRY_PTR}
    lli     t2, 1
    sb      t2, {NEG_PTR}
    andi    t2, t0, 15
    andi    t3, a0, 15
    subu    t2, t2, t3
    slt     t2, t2, zero
    sb      t2, {HALF_PTR}
    andi    t0, t1, $ff
    slti    t1, t0, 1
    sb      t1, {ZERO_PTR}
    jr      ra
    sb      t0, {A_PTR}

cpu_sub_hl:  // void()
    op_hl(cpu_sub)

cpu_sub_r8:  // void(byte opcode)
    op_r8(cpu_sub)

cpu_sub_u8:  // void()
    op_u8(cpu_sub)

cpu_xor:  // void(byte operand)
    lbu     t0, {A_PTR}
    xor     t0, t0, a0
    sw      zero, {F_PTR}
    slti    t1, t0, 1
    sb      t1, {ZERO_PTR}
    jr      ra
    sb      t0, {A_PTR}

cpu_xor_hl:  // void()
    op_hl(cpu_xor)

cpu_xor_r8:  // void(byte opcode)
    op_r8(cpu_xor)

cpu_xor_u8:  // void()
    op_u8(cpu_xor)

cpu_ld_r8_r8:  // void(byte opcode)
    andi    t0, a0, 7
    addu    t0, t0, reg_ptr
    lbu     t0, 0(t0)
    addiu   t1, a0, -64
    srl     t1, t1, 3
    addu    t1, t1, reg_ptr
    jr      ra
    sb      t0, 0(t1)

cpu_ld_hl_r8:  // void(byte opcode)
    andi    t0, a0, 7
    addu    t0, t0, reg_ptr
    lbu     a1, 0(t0)
    j       cpu_write
    lhu     a0, {HL_PTR}

cpu_ld_r8_hl:  // void(byte opcode)
    daddiu  sp, sp, -16
    sd      ra, 0(sp)
    sd      a0, 8(sp)
    jal     cpu_read
    lhu     a0, {HL_PTR}
    ld      ra, 0(sp)
    ld      t0, 8(sp)
    daddiu  sp, sp, 16
    addiu   t0, t0, -64
    srl     t0, t0, 3
    addu    t0, t0, reg_ptr
    jr      ra
    sb      v0, 0(t0)

cpu_ld_r8_u8:  // void(byte opcode)
    daddiu  sp, sp, -16
    sd      ra, 0(sp)
    jal     cpu_read8
    sd      a0, 8(sp)
    ld      ra, 0(sp)
    ld      t0, 8(sp)
    daddiu  sp, sp, 16
    srl     t0, t0, 3
    addu    t0, t0, reg_ptr
    jr      ra
    sb      v0, 0(t0)

cpu_ld_hl_u8:  // void()
    sd      ra, -8(sp)
    jal     cpu_read8
    daddiu  sp, sp, -8
    ld      ra, 0(sp)
    daddiu  sp, sp, 8
    lhu     a0, {HL_PTR}
    j       cpu_write
    move    a1, v0

cpu_ld_bc_u16:  // void()
    sd      ra, -8(sp)
    jal     cpu_read16
    daddiu  sp, sp, -8
    ld      ra, 0(sp)
    daddiu  sp, sp, 8
    jr      ra
    sh      v0, {BC_PTR}

cpu_ld_de_u16:  // void()
    sd      ra, -8(sp)
    jal     cpu_read16
    daddiu  sp, sp, -8
    ld      ra, 0(sp)
    daddiu  sp, sp, 8
    jr      ra
    sh      v0, {DE_PTR}

cpu_ld_hl_u16:  // void()
    sd      ra, -8(sp)
    jal     cpu_read16
    daddiu  sp, sp, -8
    ld      ra, 0(sp)
    daddiu  sp, sp, 8
    jr      ra
    sh      v0, {HL_PTR}

cpu_ld_u16_sp:  // void()
    sd      ra, -8(sp)
    jal     cpu_read16
    daddiu  sp, sp, -16
    sd      v0, 8(sp)
    move    a0, v0
    jal     cpu_write
    andi    a1, gb_sp, $ff
    ld      a0, 8(sp)
    addiu   a0, a0, 1
    andi    a0, a0, $ffff
    srl     a1, gb_sp, 8
    ld      ra, 0(sp)
    j       cpu_write
    daddiu  sp, sp, 16

cpu_ld_sp_u16:  // void()
    sd      ra, -8(sp)
    jal     cpu_read16
    daddiu  sp, sp, -8
    ld      ra, 0(sp)
    daddiu  sp, sp, 8
    jr      ra
    move    gb_sp, v0

cpu_ld_sp_hl:  // void()
    jr      ra
    lhu     gb_sp, {HL_PTR}

cpu_ld_bc_a:  // void()
    lhu     a0, {BC_PTR}
    j       cpu_write
    lbu     a1, {A_PTR}

cpu_ld_de_a:  // void()
    lhu     a0, {DE_PTR}
    j       cpu_write
    lbu     a1, {A_PTR}

cpu_ld_a_bc:  // void()
    daddiu  sp, sp, -8
    sd      ra, 0(sp)
    jal     cpu_read
    lhu     a0, {BC_PTR}
    ld      ra, 0(sp)
    daddiu  sp, sp, 8
    jr      ra
    sb      v0, {A_PTR}

cpu_ld_a_de:  // void()
    daddiu  sp, sp, -8
    sd      ra, 0(sp)
    jal     cpu_read
    lhu     a0, {DE_PTR}
    ld      ra, 0(sp)
    daddiu  sp, sp, 8
    jr      ra
    sb      v0, {A_PTR}

cpu_ld_u16_a:  // void()
    sd      ra, -8(sp)
    jal     cpu_read16
    daddiu  sp, sp, -8
    ld      ra, 0(sp)
    daddiu  sp, sp, 8
    move    a0, v0
    j       cpu_write
    lbu     a1, {A_PTR}

cpu_ld_a_u16:  // void()
    sd      ra, -8(sp)
    jal     cpu_read16
    daddiu  sp, sp, -8
    jal     cpu_read
    move    a0, v0
    ld      ra, 0(sp)
    daddiu  sp, sp, 8
    jr      ra
    sb      v0, {A_PTR}

cpu_ld_hl_sp_s8:  // void()
    sd      ra, -8(sp)
    jal     cpu_read8
    daddiu  sp, sp, -8
    sll     t0, v0, 24
    sra     t0, t0, 24
    addu    t0, gb_sp, t0
    sh      t0, {HL_PTR}
    andi    t1, gb_sp, $ff
    addu    t2, t1, v0
    srl     t2, t2, 8
    sb      t2, {CARRY_PTR}
    andi    t1, t1, $f
    andi    v0, v0, $f
    addu    t2, t1, v0
    srl     t2, t2, 4
    sb      t2, {HALF_PTR}
    sh      zero, {NEG_PTR}   // NEG := 0, ZERO := 0
    ld      ra, 0(sp)
    jr      ra
    daddiu  sp, sp, 8

// Load A into the byte at address HL, and thereafter increment HL by 1
cpu_ld_hlp_a:  // void()
    lhu     a0, {HL_PTR}
    addiu   t0, a0, 1
    sh      t0, {HL_PTR}
    j       cpu_write
    lbu     a1, {A_PTR}

// Load A into the byte at address HL, and thereafter decrement HL by 1
cpu_ld_hlm_a:  // void()
    lhu     a0, {HL_PTR}
    addiu   t0, a0, -1
    sh      t0, {HL_PTR}
    j       cpu_write
    lbu     a1, {A_PTR}

cpu_ld_a_hlp:  // void()
    daddiu  sp, sp, -8
    sd      ra, 0(sp)
    jal     cpu_read
    lhu     a0, {HL_PTR}
    lhu     t0, {HL_PTR}
    addiu   t0, t0, 1
    sh      t0, {HL_PTR}
    sb      v0, {A_PTR}
    ld      ra, 0(sp)
    jr      ra
    daddiu  sp, sp, 8

cpu_ld_a_hlm:  // void()
    daddiu  sp, sp, -8
    sd      ra, 0(sp)
    jal     cpu_read
    lhu     a0, {HL_PTR}
    lhu     t0, {HL_PTR}
    addiu   t0, t0, -1
    sh      t0, {HL_PTR}
    sb      v0, {A_PTR}
    ld      ra, 0(sp)
    jr      ra
    daddiu  sp, sp, 8

cpu_ldh_u8_a:  // void()
    sd      ra, -8(sp)
    jal     cpu_read8
    daddiu  sp, sp, -8
    ld      ra, 0(sp)
    daddiu  sp, sp, 8
    move    a0, v0
    j       cpu_write_page_ff
    lbu     a1, {A_PTR}

cpu_ldh_a_u8:  // void()
    sd      ra, -8(sp)
    jal     cpu_read8
    daddiu  sp, sp, -8
    jal     cpu_read_page_ff
    move    a0, v0
    sb      v0, {A_PTR}
    ld      ra, 0(sp)
    jr      ra
    daddiu  sp, sp, 8

cpu_ldh_c_a:  // void()
    lbu     a0, {C_PTR}
    j       cpu_write_page_ff
    lbu     a1, {A_PTR}

cpu_ldh_a_c:  // void()
    daddiu  sp, sp, -8
    sd      ra, 0(sp)
    jal     cpu_read_page_ff
    lbu     a0, {C_PTR}
    sb      v0, {A_PTR}
    ld      ra, 0(sp)
    jr      ra
    daddiu  sp, sp, 8

cpu_rl:  // void(byte opcode)
    srl     t0, a0, 3
    addu    t0, t0, reg_ptr
    lbu     t1, 0(t0)
    lbu     t2, {CARRY_PTR}
    srl     t3, t1, 7
    sb      t3, {CARRY_PTR}
    sll     t1, t1, 1
    or      t1, t1, t2
    andi    t1, t1, $ff
    slti    t2, t1, 1
    sb      t2, {ZERO_PTR}
    sh      zero, {HALF_PTR}  // HALF + NEG
    jr      ra
    sb      t1, 0(t0)

cpu_rl_hl:  // void()
    daddiu  sp, sp, -8
    sd      ra, 0(sp)
    jal     cpu_read
    lhu     a0, {HL_PTR}
    lbu     t2, {CARRY_PTR}
    srl     t3, v0, 7
    sb      t3, {CARRY_PTR}
    sll     a1, v0, 1
    or      a1, a1, t2
    andi    a1, a1, $ff
    slti    t2, a1, 1
    sb      t2, {ZERO_PTR}
    sh      zero, {HALF_PTR}  // HALF + NEG
    ld      ra, 0(sp)
    daddiu  sp, sp, 8
    j       cpu_write
    lhu     a0, {HL_PTR}

cpu_rla:  // void()
    lbu     t0, {A_PTR}
    lbu     t1, {CARRY_PTR}
    sw      zero, {F_PTR}
    srl     t2, t0, 7
    sb      t2, {CARRY_PTR}
    sll     t0, t0, 1
    or      t0, t0, t1
    jr      ra
    sb      t0, {A_PTR}

cpu_rlc:  // void(byte opcode)
    srl     t0, a0, 3
    addu    t0, t0, reg_ptr
    lbu     t1, 0(t0)
    srl     t2, t1, 7
    sb      t2, {CARRY_PTR}
    sll     t1, t1, 1
    or      t1, t1, t2
    andi    t1, t1, $ff
    slti    t2, t1, 1
    sb      t2, {ZERO_PTR}
    sh      zero, {HALF_PTR}  // HALF + NEG
    jr      ra
    sb      t1, 0(t0)

cpu_rlca:  // void()
    lbu     t0, {A_PTR}
    sw      zero, {F_PTR}
    srl     t1, t0, 7
    sb      t1, {CARRY_PTR}
    sll     t0, t0, 1
    or      t0, t0, t1
    jr      ra
    sb      t0, {A_PTR}

cpu_rr:  // void(byte opcode)
    srl     t0, a0, 3
    addu    t0, t0, reg_ptr
    lbu     t1, 0(t0)
    andi    t2, t1, 1
    srl     t1, t1, 1
    lb      t3, {CARRY_PTR}
    sll     t3, t3, 7
    or      t1, t1, t3
    sb      t2, {CARRY_PTR}
    slti    t2, t1, 1
    sb      t2, {NEG_PTR}
    sh      zero, {HALF_PTR}  // HALF + NEG
    jr      ra
    sb      t1, 0(t0)

cpu_rra:  // void()
    lbu     t0, {A_PTR}
    sw      zero, {F_PTR}
    andi    t1, t0, 1
    srl     t0, t0, 1
    lb      t2, {CARRY_PTR}
    sll     t2, t2, 7
    or      t0, t0, t2
    sb      t1, {CARRY_PTR}
    jr      ra
    sb      t0, {A_PTR}

cpu_rrc:  // void(byte opcode)
    srl     t0, a0, 3
    addu    t0, t0, reg_ptr
    lbu     t1, 0(t0)
    andi    t2, t1, 1
    sb      t2, {CARRY_PTR}
    srl     t1, t1, 1
    sll     t2, t2, 7
    or      t1, t1, t2
    slti    t2, t1, 1
    sb      t2, {NEG_PTR}
    sh      zero, {HALF_PTR}  // HALF + NEG
    jr      ra
    sb      t1, 0(t0)

cpu_rrca:  // void()
    lbu     t0, {A_PTR}
    sw      zero, {F_PTR}
    andi    t1, t0, 1
    sb      t1, {CARRY_PTR}
    srl     t0, t0, 1
    sll     t1, t1, 7
    or      t0, t0, t1
    jr      ra
    sb      t0, {A_PTR}

cpu_sla:  // void(byte opcode)
    andi    t0, a0, 7
    addu    t0, t0, reg_ptr
    lbu     t1, 0(t0)
    srl     t2, t1, 7
    sb      t2, {CARRY_PTR}
    sll     t1, t1, 1
    andi    t1, t1, $ff
    slti    t2, t1, 1
    sb      t2, {NEG_PTR}
    sh      zero, {HALF_PTR}  // HALF + NEG
    jr      ra
    sb      t1, 0(t0)

cpu_sra:  // void(byte opcode)
    andi    t0, a0, 7
    addu    t0, t0, reg_ptr
    lbu     t1, 0(t0)
    andi    t2, t1, 1
    sb      t2, {CARRY_PTR}
    sll     t1, t1, 24
    sra     t1, t1, 25
    andi    t1, t1, $ff
    slti    t2, t1, 1
    sb      t2, {NEG_PTR}
    sh      zero, {HALF_PTR}  // HALF + NEG
    jr      ra
    sb      t1, 0(t0)

cpu_srl:  // void(byte opcode)
    andi    t0, a0, 7
    addu    t0, t0, reg_ptr
    lbu     t1, 0(t0)
    andi    t2, t1, 1
    sb      t2, {CARRY_PTR}
    srl     t1, t1, 1
    slti    t2, t1, 1
    sb      t2, {NEG_PTR}
    sh      zero, {HALF_PTR}  // HALF + NEG
    jr      ra
    sb      t1, 0(t0)

cpu_bit:  // void(byte opcode)
    andi    t0, a0, 7
    addu    t0, t0, reg_ptr
    lbu     t1, 0(t0)
    addiu   t2, a0, -64
    srl     t2, t2, 3
    srlv    t1, t1, t2
    andi    t1, t1, 1
    slti    t1, t1, 1
    sb      t1, {ZERO_PTR}
    lli     t1, $100
    jr      ra
    sh      t1, {HALF_PTR}    // HALF := 1, NEG := 0

cpu_bit_hl:  // void(byte opcode)
    daddiu  sp, sp, -16
    sd      ra, 0(sp)
    sd      a0, 8(sp)
    jal     cpu_read
    lhu     a0, {HL_PTR}
    ld      ra, 0(sp)
    ld      a0, 8(sp)
    move    a1, v0
    j       cpu_bit
    daddiu  sp, sp, 16

cpu_res:  // void(byte opcode)
    andi    t0, a0, 7
    addu    t0, t0, reg_ptr
    lbu     t1, 0(t0)
    addiu   t2, a0, -128
    srl     t2, t2, 3
    lli     t3, 1
    sllv    t3, t3, t2
    xori    t3, t3, $ff
    andi    t1, t1, t3
    jr      ra
    sb      t1, 0(t0)

cpu_set:  // void(byte opcode)
    andi    t0, a0, 7
    addu    t0, t0, reg_ptr
    lbu     t1, 0(t0)
    addiu   t2, a0, -$c0
    srl     t2, t2, 3
    lli     t3, 1
    sllv    t3, t3, t2
    or      t1, t1, t3
    jr      ra
    sb      t1, 0(t0)

cpu_swap:  // void(byte opcode)
    andi    t0, a0, 7
    addu    t0, t0, reg_ptr
    lbu     t1, 0(t0)
    andi    t2, t1, 15
    sll     t2, t2, 4
    srl     t1, t1, 4
    or      t1, t1, t2
    sw      zero, {F_PTR}
    slti    t2, t1, 1
    sb      t2, {ZERO_PTR}
    jr      ra
    sb      t1, 0(t0)

cpu_ccf:  // void()
    lb      t0, {CARRY_PTR}
    slti    t0, t0, 1
    sb      t0, {CARRY_PTR}
    sb      zero, {NEG_PTR}
    jr      ra
    sb      zero, {HALF_PTR}

cpu_cpl:  // void()
    lbu     t0, {A_PTR}
    xori    t0, t0, $ff
    sb      t0, {A_PTR}
    lli     t0, 1
    sb      t0, {NEG_PTR}
    jr      ra
    sb      t0, {HALF_PTR}

cpu_di:  // void()
    jr      ra
    sb      zero, {IME_PTR}

cpu_ei:  // void()  // todo: ime is set with one instr delay
    lli     t0, 1
    sb      t0, {IME_PTR}
    jr      ra
    sb      t0, {SHOULD_CHECK_INTERRUPTS_PTR}

cpu_dec_r8:  // void(byte opcode)
    srl     t0, a0, 3
    addu    t0, t0, reg_ptr
    lbu     t1, 0(t0)
    lli     t2, 1
    sb      t2, {NEG_PTR}
    andi    t2, t1, 15
    slti    t2, t2, 1
    sb      t2, {HALF_PTR}
    addiu   t1, t1, -1
    andi    t1, t1, $ff
    sltiu   t2, t1, 1
    sb      t2, {ZERO_PTR}
    jr      ra
    sb      t1, 0(t0)

cpu_dec_hlm:  // void()
    daddiu  sp, sp, -8
    sd      ra, 0(sp)
    jal     cpu_read
    lhu     a0, {HL_PTR}
    lli     t0, 1
    sb      t0, {NEG_PTR}
    andi    t0, v0, 15
    slti    t0, t0, 1
    sb      t0, {HALF_PTR}
    addiu   v0, v0, -1
    andi    v0, v0, $ff
    sltiu   t0, v0, 1
    sb      t0, {ZERO_PTR}
    lhu     a0, {HL_PTR}
    move    a1, v0
    ld      ra, 0(sp)
    j       cpu_write
    daddiu  sp, sp, 8

cpu_dec_bc:  // void()
    lhu     t0, {BC_PTR}
    addiu   t0, t0, -1
    jr      ra
    sh      t0, {BC_PTR}

cpu_dec_de:  // void()
    lhu     t0, {DE_PTR}
    addiu   t0, t0, -1
    jr      ra
    sh      t0, {DE_PTR}

cpu_dec_hl:  // void()
    lhu     t0, {HL_PTR}
    addiu   t0, t0, -1
    jr      ra
    sh      t0, {HL_PTR}

cpu_dec_sp:  // void()
    addiu   gb_sp, gb_sp, -1
    jr      ra
    andi    gb_sp, gb_sp, $ffff

cpu_inc_r8:  // void(byte opcode)
    srl     t0, a0, 3
    addu    t0, t0, reg_ptr
    lbu     t1, 0(t0)
    sb      zero, {NEG_PTR}
    xori    t2, t1, $ff
    andi    t2, t2, 15
    slti    t2, t2, 1
    sb      t2, {HALF_PTR}
    addiu   t1, t1, 1
    andi    t1, t1, $ff
    sltiu   t2, t1, 1
    sb      t2, {ZERO_PTR}
    jr      ra
    sb      t1, 0(t0)

cpu_inc_hlm:  // void()
    daddiu  sp, sp, -8
    sd      ra, 0(sp)
    jal     cpu_read
    lhu     a0, {HL_PTR}
    sb      zero, {NEG_PTR}
    xori    t0, v0, $ff
    andi    t0, t0, 15
    slti    t0, t0, 1
    sb      t0, {HALF_PTR}
    addiu   v0, v0, 1
    andi    v0, v0, $ff
    sltiu   t0, v0, 1
    sb      t0, {ZERO_PTR}
    lhu     a0, {HL_PTR}
    move    a1, v0
    ld      ra, 0(sp)
    j       cpu_write
    daddiu  sp, sp, 8

cpu_inc_bc:  // void()
    lhu     t0, {BC_PTR}
    addiu   t0, t0, 1
    jr      ra
    sh      t0, {BC_PTR}

cpu_inc_de:  // void()
    lhu     t0, {DE_PTR}
    addiu   t0, t0, 1
    jr      ra
    sh      t0, {DE_PTR}

cpu_inc_hl:  // void()
    lhu     t0, {HL_PTR}
    addiu   t0, t0, 1
    jr      ra
    sh      t0, {HL_PTR}

cpu_inc_sp:  // void()
    addiu   gb_sp, gb_sp, 1
    jr      ra
    andi    gb_sp, gb_sp, $ffff

cpu_cb:  // void()
    daddiu  sp, sp, -8
    sd      ra, 0(sp)
    jal     cpu_read8
    nop
    la      t0, cpu_cb_table
    andi    v0, v0, $f8
    addu    t0, t0, v0
    ld      ra, 0(sp)
    jr      t0
    daddiu  sp, sp, 8

cpu_daa:  // void()
    lbu     t0, {A_PTR}
    lb      t1, {NEG_PTR}
    beq     t1, zero, cpu_daa_2
    lb      t1, {CARRY_PTR}
    bnel    t1, zero, cpu_daa_0
    addiu   t0, t0, -$60
cpu_daa_0:
    lb      t1, {HALF_PTR}
    bnel    t1, zero, cpu_daa_1
    addiu   t0, t0, -6
cpu_daa_1:
    j       cpu_daa_4
    nop
cpu_daa_2:
    lli     t2, $99
    slt     t2, t2, t0
    or      t1, t1, t2
    beq     t1, zero, cpu_daa_3
    lli     t1, 1
    sb      t1, {CARRY_PTR}
    addiu   t0, t0, $60
cpu_daa_3:
    lb      t1, {HALF_PTR}
    andi    t2, t0, 15
    lli     t3, 9
    slt     t2, t3, t2
    or      t1, t1, t2
    bnel    t1, zero, cpu_daa_4
    addiu   t0, t0, 6
cpu_daa_4:
    andi    t0, t0, $ff
    slti    t1, t0, 1
    sb      t1, {ZERO_PTR}
    sb      zero, {HALF_PTR}
    jr      ra
    sb      t0, {A_PTR}

cpu_nop:  // void()
    jr      ra
    nop

cpu_pop_af:  // void()
    daddiu  sp, sp, -8
    sd      ra, 0(sp)
    jal     cpu_read
    move    a0, gb_sp
    srl     t0, v0, 7
    srl     t1, v0, 6
    srl     t2, v0, 5
    srl     t3, v0, 4
    andi    t1, t1, 1
    andi    t2, t2, 1
    andi    t3, t3, 1
    sb      t0, {ZERO_PTR}
    sb      t1, {NEG_PTR}
    sb      t2, {HALF_PTR}
    sb      t3, {CARRY_PTR}
    addiu   gb_sp, gb_sp, 1
    jal     cpu_read
    andi    a0, gb_sp, $ffff
    sb      v0, {A_PTR}
    addiu   gb_sp, gb_sp, 1
    andi    gb_sp, gb_sp, $ffff
    ld      ra, 0(sp)
    jr      ra
    daddiu  sp, sp, 8

cpu_pop_bc:  // void()
    daddiu  sp, sp, -8
    sd      ra, 0(sp)
    jal     cpu_read
    move    a0, gb_sp
    sb      v0, {C_PTR}
    addiu   gb_sp, gb_sp, 1
    jal     cpu_read
    andi    a0, gb_sp, $ffff
    sb      v0, {B_PTR}
    addiu   gb_sp, gb_sp, 1
    andi    gb_sp, gb_sp, $ffff
    ld      ra, 0(sp)
    jr      ra
    daddiu  sp, sp, 8

cpu_pop_de:  // void()
    daddiu  sp, sp, -8
    sd      ra, 0(sp)
    jal     cpu_read
    move    a0, gb_sp
    sb      v0, {E_PTR}
    addiu   gb_sp, gb_sp, 1
    jal     cpu_read
    andi    a0, gb_sp, $ffff
    sb      v0, {D_PTR}
    addiu   gb_sp, gb_sp, 1
    andi    gb_sp, gb_sp, $ffff
    ld      ra, 0(sp)
    jr      ra
    daddiu  sp, sp, 8

cpu_pop_hl:  // void()
    daddiu  sp, sp, -8
    sd      ra, 0(sp)
    jal     cpu_read
    move    a0, gb_sp
    sb      v0, {L_PTR}
    addiu   gb_sp, gb_sp, 1
    jal     cpu_read
    andi    a0, gb_sp, $ffff
    sb      v0, {H_PTR}
    addiu   gb_sp, gb_sp, 1
    andi    gb_sp, gb_sp, $ffff
    ld      ra, 0(sp)
    jr      ra
    daddiu  sp, sp, 8

cpu_push_af:  // void()
    daddiu  sp, sp, -8
    sd      ra, 0(sp)
    addiu   gb_sp, gb_sp, -1
    andi    a0, gb_sp, $ffff
    jal     cpu_write
    lbu     a1, {A_PTR}
    addiu   gb_sp, gb_sp, -1
    andi    gb_sp, gb_sp, $ffff
    move    a0, gb_sp
    lb      a1, {ZERO_PTR}
    lb      t0, {NEG_PTR}
    lb      t1, {HALF_PTR}
    lb      t2, {CARRY_PTR}
    sll     a1, a1, 7
    sll     t0, t0, 6
    sll     t1, t1, 5
    sll     t2, t2, 4
    or      a1, a1, t0
    or      a1, a1, t1
    or      a1, a1, t2
    ld      ra, 0(sp)
    j       cpu_write
    daddiu  sp, sp, 8

cpu_push_bc:  // void()
    daddiu  sp, sp, -8
    sd      ra, 0(sp)
    addiu   gb_sp, gb_sp, -1
    andi    a0, gb_sp, $ffff
    jal     cpu_write
    lbu     a1, {B_PTR}
    addiu   gb_sp, gb_sp, -1
    andi    gb_sp, gb_sp, $ffff
    move    a0, gb_sp
    lbu     a1, {C_PTR}
    ld      ra, 0(sp)
    j       cpu_write
    daddiu  sp, sp, 8

cpu_push_de:  // void()
    daddiu  sp, sp, -8
    sd      ra, 0(sp)
    addiu   gb_sp, gb_sp, -1
    andi    a0, gb_sp, $ffff
    jal     cpu_write
    lbu     a1, {D_PTR}
    addiu   gb_sp, gb_sp, -1
    andi    gb_sp, gb_sp, $ffff
    move    a0, gb_sp
    lbu     a1, {E_PTR}
    ld      ra, 0(sp)
    j       cpu_write
    daddiu  sp, sp, 8

cpu_push_hl:  // void()
    daddiu  sp, sp, -8
    sd      ra, 0(sp)
    addiu   gb_sp, gb_sp, -1
    andi    a0, gb_sp, $ffff
    jal     cpu_write
    lbu     a1, {H_PTR}
    addiu   gb_sp, gb_sp, -1
    andi    gb_sp, gb_sp, $ffff
    move    a0, gb_sp
    lbu     a1, {L_PTR}
    ld      ra, 0(sp)
    j       cpu_write
    daddiu  sp, sp, 8

cpu_push_pc:  // void()
    daddiu  sp, sp, -8
    sd      ra, 0(sp)
    addiu   gb_sp, gb_sp, -1
    andi    a0, gb_sp, $ffff
    jal     cpu_write
    srl     a1, pc, 8
    addiu   gb_sp, gb_sp, -1
    andi    gb_sp, gb_sp, $ffff
    move    a0, gb_sp
    andi    a1, pc, $ff
    ld      ra, 0(sp)
    j       cpu_write
    daddiu  sp, sp, 8

cpu_scf:  // void()
    sh      zero, {HALF_PTR}  // HALF + NEG
    lli     t0, 1
    jr      ra
    sb      t0, {CARRY_PTR}

cpu_call:  // void()
    sd      ra, -8(sp)
    jal     cpu_read16
    daddiu  sp, sp, -16
    jal     cpu_push_pc
    sd      v0, 8(sp)
    ld      ra, 0(sp)
    ld      pc, 8(sp)
    jr      ra
    daddiu  sp, sp, 16

cpu_call_c:  // void()
    lb      t0, {CARRY_PTR}
    bnel    t0, zero, cpu_call
    nop
    addiu   pc, pc, 2
    jr      ra
    andi    pc, pc, $ffff

cpu_call_nc:  // void()
    lb      t0, {CARRY_PTR}
    beql    t0, zero, cpu_call
    nop
    addiu   pc, pc, 2
    jr      ra
    andi    pc, pc, $ffff

cpu_call_nz:  // void()
    lb      t0, {ZERO_PTR}
    beql    t0, zero, cpu_call
    nop
    addiu   pc, pc, 2
    jr      ra
    andi    pc, pc, $ffff

cpu_call_z:  // void()
    lb      t0, {ZERO_PTR}
    bnel    t0, zero, cpu_call
    nop
    addiu   pc, pc, 2
    jr      ra
    andi    pc, pc, $ffff

cpu_ret:  // void()
    daddiu  sp, sp, -8
    sd      ra, 0(sp)
    jal     cpu_read
    move    a0, gb_sp
    move    pc, v0
    addiu   gb_sp, gb_sp, 1
    jal     cpu_read
    andi    a0, gb_sp, $ffff
    sll     v0, v0, 8
    or      pc, pc, v0
    addiu   gb_sp, gb_sp, 1
    jr      ra
    andi    gb_sp, gb_sp, $ffff

cpu_ret_c:  //  void()
    lb      t0, {CARRY_PTR}
    bnel    t0, zero, cpu_ret
    nop
    jr      ra
    nop

cpu_ret_nc:  //  void()
    lb      t0, {CARRY_PTR}
    beql    t0, zero, cpu_ret
    nop
    jr      ra
    nop

cpu_ret_nz:  //  void()
    lb      t0, {ZERO_PTR}
    beql    t0, zero, cpu_ret
    nop
    jr      ra
    nop

cpu_ret_z:  //  void()
    lb      t0, {ZERO_PTR}
    bnel    t0, zero, cpu_ret
    nop
    jr      ra
    nop

cpu_reti:  //  void()
    la      t0, cpu_ime
    lli     t1, 1
    j       cpu_ret
    sb      t1, 0(t0)

cpu_jr:  // void()
    sd      ra, -8(sp)
    jal     cpu_read8
    daddiu  sp, sp, -8
    sll     v0, v0, 24
    sra     v0, v0, 24
    addu    pc, pc, v0
    andi    pc, pc, $ffff
    ld      ra, 0(sp)
    jr      ra
    daddiu  sp, sp, 8

cpu_jr_c:  // void()
    lb      t0, {CARRY_PTR}
    bnel    t0, zero, cpu_jr
    nop
    addiu   pc, pc, 1
    jr      ra
    andi    pc, pc, $ffff

cpu_jr_nc:  // void()
    lb      t0, {CARRY_PTR}
    beql    t0, zero, cpu_jr
    nop
    addiu   pc, pc, 1
    jr      ra
    andi    pc, pc, $ffff

cpu_jr_nz:  // void()
    lb      t0, {ZERO_PTR}
    beql    t0, zero, cpu_jr
    nop
    addiu   pc, pc, 1
    jr      ra
    andi    pc, pc, $ffff

cpu_jr_z:  // void()
    lb      t0, {ZERO_PTR}
    bnel    t0, zero, cpu_jr
    nop
    addiu   pc, pc, 1
    jr      ra
    andi    pc, pc, $ffff

cpu_jp_u16:  // void()
    daddiu  sp, sp, -8
    sd      ra, 0(sp)
    jal     cpu_read16
    nop
    move    pc, v0
    ld      ra, 0(sp)
    jr      ra
    daddiu  sp, sp, 8

cpu_jp_u16_c:  // void()
    lb      t0, {CARRY_PTR}
    bnel    t0, zero, cpu_jp_u16
    nop
    addiu   pc, pc, 2
    jr      ra
    andi    pc, pc, $ffff

cpu_jp_u16_nc:  // void()
    lb      t0, {CARRY_PTR}
    beql    t0, zero, cpu_jp_u16
    nop
    addiu   pc, pc, 2
    jr      ra
    andi    pc, pc, $ffff

cpu_jp_u16_nz:  // void()
    lb      t0, {ZERO_PTR}
    beql    t0, zero, cpu_jp_u16
    nop
    addiu   pc, pc, 2
    jr      ra
    andi    pc, pc, $ffff

cpu_jp_u16_z:  // void()
    lb      t0, {ZERO_PTR}
    bnel    t0, zero, cpu_jp_u16
    nop
    addiu   pc, pc, 2
    jr      ra
    andi    pc, pc, $ffff

cpu_jp_hl:  // void()
    jr      ra
    lhu     pc, {HL_PTR}

cpu_rst:  // void(byte opcode)
    daddiu  sp, sp, -16
    sd      ra, 0(sp)
    jal     cpu_push_pc
    sd      a0, 8(sp)
    ld      ra, 0(sp)
    ld      pc, 8(sp)
    addiu   pc, pc, -$c7
    jr      ra
    daddiu  sp, sp, 16

cpu_illegal:  // void()
    break

cpu_halt:  // void()
    break

cpu_stop:  // void()
    break

cpu_read8:  // byte()
    move    a0, pc
    addiu   pc, pc, 1
    j       cpu_read
    andi    pc, pc, $ffff

cpu_read16:  // hword()
    daddiu  sp, sp, -16
    sd      ra, 0(sp)
    jal     cpu_read
    move    a0, pc
    sd      v0, 8(sp)
    addiu   a0, pc, 1
    jal     cpu_read
    andi    a0, a0, $ffff
    addiu   pc, pc, 2
    andi    pc, pc, $ffff
    sll     v0, v0, 8
    ld      t0, 8(sp)
    or      v0, v0, t0
    ld      ra, 0(sp)
    jr      ra
    daddiu  sp, sp, 16
