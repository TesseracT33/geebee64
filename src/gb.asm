arch n64.cpu
endian msb
output "gb.z64", create

constant N64_ROM_SIZE = $100000
fill N64_ROM_SIZE + $1000       // set rom size. minimum size (?) since the boot code will do a PI DMA of this size
origin $0                       // rom location
base $80000000                  // signed displacement against origin, used when computing the pc value for labels

include "n64.inc"
include "util.inc"

//include "n64_header.asm"
//include "n64_boot.asm"

include "bus.asm"
include "cpu.asm"

constant NES_HEIGHT = 240
constant NES_WIDTH = 256
constant NES_FRAMEBUFFER_SIZE = NES_WIDTH * NES_HEIGHT
constant NES_ROM_START_ADDR = $200
constant N64_BPP = 2
constant N64_HEIGHT = 480
constant N64_STACK_SIZE = 10 * 1024
constant N64_WIDTH = 640
constant RDRAM_FRAMEBUFFER_ADDR = (RDRAM_ROM_ADDR + N64_ROM_SIZE + N64_STACK_SIZE) | $A0000000
constant RDRAM_STACK_ADDR = (RDRAM_ROM_ADDR + N64_ROM_SIZE + N64_STACK_SIZE - 8) | $80000000
constant RDRAM_ROM_ADDR = 0

