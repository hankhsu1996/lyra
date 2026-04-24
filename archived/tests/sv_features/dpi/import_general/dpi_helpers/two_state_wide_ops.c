#include <stdint.h>

typedef uint32_t svBitVecVal;

/* bit [127:0] -- 4 words */

void write_bit128_pattern(svBitVecVal* out) {
  out[0] = 0x11111111;
  out[1] = 0x22222222;
  out[2] = 0x33333333;
  out[3] = 0x44444444;
}

int read_bit128_word0(const svBitVecVal* v) {
  return (int)v[0];
}

int read_bit128_word3(const svBitVecVal* v) {
  return (int)v[3];
}

void roundtrip_bit128(svBitVecVal* inout) {
  inout[0] += 1;
  inout[3] += 1;
}

/* bit [65:0] -- 3 words, partial high word (2 semantic bits in word 2) */

void write_bit66_pattern(svBitVecVal* out) {
  out[0] = 0xAAAAAAAA;
  out[1] = 0xBBBBBBBB;
  out[2] = 0x00000003;
}

int read_bit66_word0(const svBitVecVal* v) {
  return (int)v[0];
}

int read_bit66_word2(const svBitVecVal* v) {
  return (int)v[2];
}

void write_bit66_with_garbage(svBitVecVal* out) {
  out[0] = 0xAAAAAAAA;
  out[1] = 0xBBBBBBBB;
  /* word 2: only 2 semantic bits. Garbage in high bits. */
  out[2] = 0xFFFFFFFF;
}
