#include <stdint.h>

typedef struct {
  uint32_t aval;
  uint32_t bval;
} svLogicVecVal;

typedef uint32_t svBitVecVal;

/* 4-state narrow (32-bit integer): return via indirect pointer.
   Returns 42 as a known 2-state value. */
void return_integer_42(svLogicVecVal* result) {
  result[0].aval = 42;
  result[0].bval = 0;
}

/* 4-state narrow (32-bit integer): return via indirect pointer.
   Returns a value with some X bits: aval=0xFF, bval=0x0F.
   Per svLogicVecVal encoding: bit has X when bval=1.
   Lyra val = aval ^ bval = 0xF0, Lyra unk = bval = 0x0F. */
void return_integer_with_x(svLogicVecVal* result) {
  result[0].aval = 0xFF;
  result[0].bval = 0x0F;
}

/* 4-state wide: return logic [127:0] via indirect pointer.
   Sets a recognizable pattern across 4 words. */
void return_logic128_pattern(svLogicVecVal* result) {
  result[0].aval = 0xDEADBEEF;
  result[0].bval = 0;
  result[1].aval = 0xCAFEBABE;
  result[1].bval = 0;
  result[2].aval = 0x12345678;
  result[2].bval = 0;
  result[3].aval = 0x9ABCDEF0;
  result[3].bval = 0;
}

/* 2-state wide: return bit [127:0] via indirect pointer. */
void return_bit128_pattern(svBitVecVal* result) {
  result[0] = 0x11223344;
  result[1] = 0x55667788;
  result[2] = 0x99AABBCC;
  result[3] = 0xDDEEFF00;
}
