#include <stdint.h>

/* Minimal canonical DPI-C representation (LRM Annex H.10.1). A user who does
   not include svdpi.h declares the same standardized, binary-compatible layout.
   aval == the value plane, bval == the X/Z (unknown) plane. */
typedef unsigned char svLogic; /* sv_0=0 sv_1=1 sv_z=2 sv_x=3 */
typedef uint32_t svBitVecVal;  /* a 32-bit chunk of a packed bit vector */
typedef struct {
  uint32_t aval;
  uint32_t bval;
} svLogicVecVal; /* a 32-bit chunk of a packed logic vector */

/* Scalar 4-state logic, by value both ways. */
svLogic scalar_id(svLogic x) {
  return x;
}

/* 4-state vector input: count bits that are X or Z (bval set). */
int count_unknown(const svLogicVecVal* v) {
  return __builtin_popcount(v[0].bval & 0xFF);
}

/* 4-state vector output: low nibble known 1s, high nibble Z. */
void set_zx(svLogicVecVal* v) {
  v[0].aval = 0x0F;
  v[0].bval = 0xF0;
}

/* 4-state vector inout: invert the known bits, leave X/Z untouched. */
void known_not(svLogicVecVal* v) {
  for (int i = 0; i < 4; ++i) {
    if (((v[0].bval >> i) & 1) == 0) {
      v[0].aval ^= (uint32_t)(1 << i);
    }
  }
}

/* integer (4-state 32-bit) inputs by pointer; int result by value. */
int int_add(const svLogicVecVal* a, const svLogicVecVal* b) {
  return (int)(a[0].aval + b[0].aval);
}

/* 2-state wide: copy the low 32 bits of a 128-bit value out. */
void low32(const svBitVecVal* v, svBitVecVal* r) {
  r[0] = v[0];
}

/* The bit[N] ABI fix: an 8-bit packed bit vector crosses as svBitVecVal*,
   not as a by-value char. byte result is by value. */
signed char b8_sum(const svBitVecVal* a, const svBitVecVal* b) {
  return (signed char)((a[0] + b[0]) & 0xFF);
}
