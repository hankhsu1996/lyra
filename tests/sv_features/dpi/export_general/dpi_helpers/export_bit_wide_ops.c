#include <stdint.h>

typedef uint32_t svBitVecVal;

extern int read_bit128_word0_export(const svBitVecVal* v);

int call_read_bit128_word0_export(void) {
  svBitVecVal vec[4];
  vec[0] = 0x11111111;
  vec[1] = 0x22222222;
  vec[2] = 0x33333333;
  vec[3] = 0x44444444;
  return read_bit128_word0_export(vec);
}

extern void write_bit128_export(svBitVecVal* out);

int call_write_bit128_export(void) {
  svBitVecVal vec[4];
  vec[0] = 0xDEADBEEF;
  vec[1] = 0xDEADBEEF;
  vec[2] = 0xDEADBEEF;
  vec[3] = 0xDEADBEEF;
  write_bit128_export(vec);
  return (vec[0] == 0x11111111 && vec[3] == 0x44444444) ? 1 : 0;
}

extern void inc_bit128_export(svBitVecVal* inout);

int call_inc_bit128_export(void) {
  svBitVecVal vec[4];
  vec[0] = 0x11111111;
  vec[1] = 0x22222222;
  vec[2] = 0x33333333;
  vec[3] = 0x44444444;
  inc_bit128_export(vec);
  return (vec[0] == 0x11111112 && vec[3] == 0x44444445) ? 1 : 0;
}
