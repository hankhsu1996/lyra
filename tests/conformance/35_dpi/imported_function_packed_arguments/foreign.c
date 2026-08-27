/* A canonical chunk holds 32 bits: `aval` is the value plane and `bval` the
   unknown plane, so the pair (0,0) is 0, (1,0) is 1, (0,1) is z and (1,1) is x
   (LRM Annex H.7.7, H.10.1.2). The bits above a value's width are undetermined
   on the way in and are masked off here rather than read. */
#include "dpi.h"

/* Both planes reach the answer, in separate decimal places, so the design can
   tell a lost unknown plane from a lost value plane. */
int32_t plane_digest(const svLogicVecVal* v) {
  return (int32_t)(((v[0].bval & 0xFFu) * 1000u) + (v[0].aval & 0xFFu));
}

/* An output packed value is written into the caller's canonical buffer; each
   plane is taken from a different part of the argument, so neither can be
   produced without it. */
void set_pattern(int32_t seed, svLogicVecVal* v) {
  v[0].aval = (uint32_t)seed & 0xFFu;
  v[0].bval = ((uint32_t)seed >> 8) & 0xFFu;
}

/* An inout packed value arrives holding the actual and leaves holding the
   answer: the known bits are complemented and the unknown ones are left as
   they are, which is only possible if both planes crossed inward. */
void invert_known(svLogicVecVal* v) {
  uint32_t known = ~v[0].bval & 0xFu;
  v[0].aval = (v[0].aval ^ known) & 0xFu;
  v[0].bval &= 0xFu;
}

int32_t integer_mix(const svLogicVecVal* a, const svLogicVecVal* b) {
  return (int32_t)((a[0].aval * 1000u) + b[0].aval);
}

/* Every chunk of the 128-bit argument carries a different weight, so a buffer
   read at the wrong end or with the wrong chunk count changes the total. */
void chunk_weights(const svBitVecVal* v, svBitVecVal* total) {
  total[0] = (v[0] * 1u) + (v[1] * 2u) + (v[2] * 3u) + (v[3] * 4u);
}

/* Eight bits wide and still a canonical buffer, because the formal was
   declared as a packed array rather than as a byte (LRM 35.6.1.1). */
int8_t vector_byte_mix(const svBitVecVal* a, const svBitVecVal* b) {
  return (int8_t)((((a[0] & 0xFFu) * 3u) + (b[0] & 0xFFu)) & 0xFFu);
}
