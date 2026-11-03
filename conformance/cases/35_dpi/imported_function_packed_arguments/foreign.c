/* A canonical chunk holds 32 bits: `aval` is the value plane and `bval` the
   unknown plane, so the pair (0,0) is 0, (1,0) is 1, (0,1) is z and (1,1) is x
   (LRM Annex H.7.7, H.10.1.2). The bits above a value's width are undetermined
   on the way in and are masked off here rather than read. */
#include <svdpi.h>

/* Both planes reach the answer, in separate decimal places, so the design can
   tell a lost unknown plane from a lost value plane. */
int32_t plane_digest(const svLogicVecVal* v) {
  return (int32_t)(((v[0].bval & 0xFFU) * 1000U) + (v[0].aval & 0xFFU));
}

/* An output packed value is written into the caller's canonical buffer; each
   plane is taken from a different part of the argument, so neither can be
   produced without it. */
void set_pattern(int32_t seed, svLogicVecVal* v) {
  v[0].aval = (uint32_t)seed & 0xFFU;
  v[0].bval = ((uint32_t)seed >> 8) & 0xFFU;
}

/* An inout packed value arrives holding the actual and leaves holding the
   answer: the known bits are complemented and the unknown ones are left as
   they are, which is only possible if both planes crossed inward. */
void invert_known(svLogicVecVal* v) {
  uint32_t known = ~v[0].bval & 0xFU;
  v[0].aval = (v[0].aval ^ known) & 0xFU;
  v[0].bval &= 0xFU;
}

int32_t integer_mix(const svLogicVecVal* a, const svLogicVecVal* b) {
  return (int32_t)((a[0].aval * 1000U) + b[0].aval);
}

/* Every chunk of the 128-bit argument carries a different weight, so a buffer
   read at the wrong end or with the wrong chunk count changes the total. */
void chunk_weights(const svBitVecVal* v, svBitVecVal* total) {
  total[0] = (v[0] * 1U) + (v[1] * 2U) + (v[2] * 3U) + (v[3] * 4U);
}

/* Eight bits wide and still a canonical buffer, because the formal was
   declared as a packed array rather than as a byte (LRM 35.6.1.1). */
int8_t vector_byte_mix(const svBitVecVal* a, const svBitVecVal* b) {
  return (int8_t)((((a[0] & 0xFFU) * 3U) + (b[0] & 0xFFU)) & 0xFFU);
}

/* A structure of sixteen bits, its first member in the top four; the answer
   puts each member in its own place, so members read in the wrong order or
   at the wrong end show. */
int32_t pair_mix(const svBitVecVal* p) {
  uint32_t high = (p[0] >> 12) & 0xFU;
  uint32_t low = p[0] & 0xFFFU;
  return (int32_t)((high << 16) | low);
}

/* The two members of a four-state structure trade places, each with both of
   its planes. */
void swap_nibbles(svLogicVecVal* n) {
  uint32_t aval = n[0].aval & 0xFFU;
  uint32_t bval = n[0].bval & 0xFFU;
  n[0].aval = ((aval << 4) | (aval >> 4)) & 0xFFU;
  n[0].bval = ((bval << 4) | (bval >> 4)) & 0xFFU;
}

/* A packed union is as wide as each of its members, and the first element of
   a packed array of halves is its most significant one. */
int32_t octet_halves(const svBitVecVal* o) {
  return (int32_t)((((o[0] >> 4) & 0xFU) * 100U) + (o[0] & 0xFU));
}

/* A tagged union of an eight-bit and a four-bit member is nine bits: the tag
   in the top one, 0 for the first member and 1 for the second, and the member
   in the low bits. Bits between the two are undefined and are not read. */
int32_t tag_and_member(const svBitVecVal* t) {
  uint32_t tag = (t[0] >> 8) & 1U;
  uint32_t member = tag == 0U ? (t[0] & 0xFFU) : (t[0] & 0xFU);
  return (int32_t)((tag * 1000U) + member);
}

/* An enumeration over `logic [3:0]` is a four-state packed array here, and an
   output structure is written into the caller's buffer, its first member in
   the high bits. An unknown bit of the level would show in the answer. */
void pair_of_level(const svLogicVecVal* l, svBitVecVal* p) {
  uint32_t level = (l[0].aval | l[0].bval) & 0xFU;
  p[0] = (level << 12) | 0x456U;
}

/* An enumeration over `bit [5:0]` is that packed array here. */
void advance_state(const svBitVecVal* s, svBitVecVal* next) {
  next[0] = (s[0] & 0x3FU) == 5U ? 33U : 0U;
}
