#include <stdint.h>

typedef uint8_t svLogic;

typedef struct {
  uint32_t aval;
  uint32_t bval;
} svLogicVecVal;

#define sv_0 0
#define sv_1 1
#define sv_z 2
#define sv_x 3

/* scalar logic */

int echo_logic_as_int(svLogic x) {
  return (int)x;
}

void set_logic_x(svLogic* out) {
  *out = sv_x;
}

void set_logic_one(svLogic* out) {
  *out = sv_1;
}

void flip_logic(svLogic* inout) {
  if (*inout == sv_0) {
    *inout = sv_1;
  } else if (*inout == sv_1) {
    *inout = sv_0;
  }
}

svLogic return_logic_one(void) {
  return sv_1;
}

svLogic return_logic_z(void) {
  return sv_z;
}

/* logic [7:0] -- 1 word */

int read_logic8_aval(const svLogicVecVal* v) {
  return (int)(v[0].aval & 0xFF);
}

void write_logic8_pattern(svLogicVecVal* out) {
  /* aval=0xA5, bval=0x0F -> bits[3:0] have bval=1 */
  out[0].aval = 0xA5;
  out[0].bval = 0x0F;
}

void roundtrip_logic8(svLogicVecVal* inout) {
  /* XOR aval with 0xFF to flip known bits */
  inout[0].aval ^= 0xFF;
}

/* logic [31:0] -- 1 word */

int read_logic32_aval(const svLogicVecVal* v) {
  return (int)v[0].aval;
}

void write_logic32_pattern(svLogicVecVal* out) {
  out[0].aval = 0xDEADBEEF;
  out[0].bval = 0x00000000;
}

void roundtrip_logic32(svLogicVecVal* inout) {
  inout[0].aval += 1;
}

/* logic [63:0] -- 2 words */

void write_logic64_pattern(svLogicVecVal* out) {
  out[0].aval = 0x12345678;
  out[0].bval = 0x00000000;
  out[1].aval = 0xABCDEF01;
  out[1].bval = 0x00000000;
}

int read_logic64_word0_aval(const svLogicVecVal* v) {
  return (int)v[0].aval;
}

int read_logic64_word1_aval(const svLogicVecVal* v) {
  return (int)v[1].aval;
}

void roundtrip_logic64(svLogicVecVal* inout) {
  inout[0].aval += 1;
  inout[1].aval += 1;
}

/* High-bit masking test: C sets unused bits, Lyra should mask them */
void write_logic8_with_garbage(svLogicVecVal* out) {
  /* Only low 8 bits are semantic. Set high bits to garbage. */
  out[0].aval = 0xFFFFFF42;
  out[0].bval = 0xFFFFFF00;
}

/* Mixed X pattern across 2 words */
void write_logic64_mixed_x(svLogicVecVal* out) {
  /* word 0: aval=0xFF00FF00, bval=0x0F0F0F0F (X where both are 1) */
  out[0].aval = 0xFF00FF00;
  out[0].bval = 0x0F0F0F0F;
  /* word 1: aval=0x00000001, bval=0x00000000 (known 1 in bit 32) */
  out[1].aval = 0x00000001;
  out[1].bval = 0x00000000;
}

/* Scalar 4-value round-trip: C echoes the exact svLogic encoding back */
svLogic echo_logic(svLogic x) {
  return x;
}

/* logic [40:0] -- 2-word path with partial high-word masking (41 bits) */

void write_logic41_pattern(svLogicVecVal* out) {
  out[0].aval = 0xAAAAAAAA;
  out[0].bval = 0x00000000;
  /* word 1: only low 9 bits are semantic (41 - 32 = 9) */
  out[1].aval = 0x000001FF;
  out[1].bval = 0x00000000;
}

void write_logic41_with_garbage(svLogicVecVal* out) {
  out[0].aval = 0xAAAAAAAA;
  out[0].bval = 0x00000000;
  /* Semantic bits 32..40: aval=0x1FF, bval=0x000 (known all-ones).
     Garbage in bits 9..31 of word 1 (beyond semantic width). */
  out[1].aval = 0xFFFF01FF;
  out[1].bval = 0xFFFF0000;
}

int read_logic41_word0_aval(const svLogicVecVal* v) {
  return (int)v[0].aval;
}

int read_logic41_word1_aval(const svLogicVecVal* v) {
  return (int)v[1].aval;
}
