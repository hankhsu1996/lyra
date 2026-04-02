/* DPI companion C code exercising svdpi.h runtime utility functions. */

#include <svdpi.h>

int test_svdpi_version(void) {
  const char* v = svDpiVersion();
  return v != 0;
}

int test_bitsel_roundtrip(void) {
  svBitVecVal words[2] = {0, 0};

  svPutBitselBit(&words[0], 0, 1);
  svPutBitselBit(&words[0], 5, 1);
  svPutBitselBit(&words[0], 31, 1);
  svPutBitselBit(words, 32, 1);

  if (svGetBitselBit(words, 0) != 1) return 0;
  if (svGetBitselBit(words, 1) != 0) return 0;
  if (svGetBitselBit(words, 5) != 1) return 0;
  if (svGetBitselBit(words, 31) != 1) return 0;
  if (svGetBitselBit(words, 32) != 1) return 0;
  if (svGetBitselBit(words, 33) != 0) return 0;

  return 1;
}

int test_bitsel_logic_roundtrip(void) {
  svLogicVecVal words[1];
  words[0].aval = 0;
  words[0].bval = 0;

  svPutBitselLogic(words, 0, sv_0);
  svPutBitselLogic(words, 1, sv_1);
  svPutBitselLogic(words, 2, sv_z);
  svPutBitselLogic(words, 3, sv_x);

  if (svGetBitselLogic(words, 0) != sv_0) return 0;
  if (svGetBitselLogic(words, 1) != sv_1) return 0;
  if (svGetBitselLogic(words, 2) != sv_z) return 0;
  if (svGetBitselLogic(words, 3) != sv_x) return 0;

  return 1;
}

int test_partsel_bit_roundtrip(void) {
  svBitVecVal src[2] = {0xABCD1234, 0x5678EF00};
  svBitVecVal dst[1] = {0xDEADBEEF};

  /* Extract bits [8:8+16-1] from src into dst[0:15]. */
  svGetPartselBit(dst, src, 8, 16);

  /* dst should have bits 0-15 from src[8..23] = 0xD123, bits 16-31 untouched. */
  /* src[8..23]: src word0 bits 8-23 = (0xABCD1234 >> 8) & 0xFFFF = 0xCD12 */
  svBitVecVal expected = (0xDEADBEEF & 0xFFFF0000) | 0x0000CD12;
  if (dst[0] != expected) return 0;

  return 1;
}

int test_partsel_logic_roundtrip(void) {
  svLogicVecVal src[1];
  src[0].aval = 0x0000000F;
  src[0].bval = 0x0000000A;

  svLogicVecVal dst[1];
  dst[0].aval = 0xDEADBEEF;
  dst[0].bval = 0xCAFEBABE;

  /* Extract bits [0:4-1] from src into dst[0:3]. */
  svGetPartselLogic(dst, src, 0, 4);

  /* dst aval bits 0-3 = src aval bits 0-3 = 0xF, bits 4-31 untouched. */
  if ((dst[0].aval & 0xF) != 0xF) return 0;
  if ((dst[0].aval & 0xFFFFFFF0) != (0xDEADBEEF & 0xFFFFFFF0)) return 0;
  if ((dst[0].bval & 0xF) != 0xA) return 0;
  if ((dst[0].bval & 0xFFFFFFF0) != (0xCAFEBABE & 0xFFFFFFF0)) return 0;

  return 1;
}

int test_cross_word_boundary(void) {
  /* Bit-select across word boundary (bits 31 and 32). */
  svBitVecVal words[3] = {0, 0, 0};
  svPutBitselBit(words, 31, 1);
  svPutBitselBit(words, 32, 1);
  svPutBitselBit(words, 63, 1);
  svPutBitselBit(words, 64, 1);
  if (svGetBitselBit(words, 31) != 1) return 0;
  if (svGetBitselBit(words, 32) != 1) return 0;
  if (svGetBitselBit(words, 63) != 1) return 0;
  if (svGetBitselBit(words, 64) != 1) return 0;
  if (svGetBitselBit(words, 30) != 0) return 0;
  if (svGetBitselBit(words, 33) != 0) return 0;
  if (svGetBitselBit(words, 62) != 0) return 0;
  if (svGetBitselBit(words, 65) != 0) return 0;

  /* Part-select spanning word boundary: bits [28:28+8-1] = bits 28..35. */
  svBitVecVal src2[2] = {0xF0000000, 0x0000000F};
  svBitVecVal dst2[1] = {0xDEADBEEF};
  svGetPartselBit(dst2, src2, 28, 8);
  /* src bits 28..31 from word0 = 0xF, bits 32..35 from word1 = 0xF -> 0xFF */
  svBitVecVal expected2 = (0xDEADBEEF & 0xFFFFFF00) | 0xFF;
  if (dst2[0] != expected2) return 0;

  /* Logic bit-select across word boundary. */
  svLogicVecVal lwords[2];
  lwords[0].aval = 0; lwords[0].bval = 0;
  lwords[1].aval = 0; lwords[1].bval = 0;
  svPutBitselLogic(lwords, 31, sv_z);
  svPutBitselLogic(lwords, 32, sv_x);
  if (svGetBitselLogic(lwords, 31) != sv_z) return 0;
  if (svGetBitselLogic(lwords, 32) != sv_x) return 0;

  return 1;
}

int test_legacy_named_select(void) {
  svBitVecVal words[1] = {0};
  svPutBitselBit(words, 3, 1);

  /* Legacy-named should produce same result. */
  if (svGetSelectBit(words, 3) != svGetBitselBit(words, 3)) return 0;
  if (svGetSelectBit(words, 0) != svGetBitselBit(words, 0)) return 0;

  svPutSelectBit(words, 7, 1);
  if (svGetBitselBit(words, 7) != 1) return 0;

  svLogicVecVal lwords[1];
  lwords[0].aval = 0; lwords[0].bval = 0;
  svPutBitselLogic(lwords, 2, sv_x);
  if (svGetSelectLogic(lwords, 2) != svGetBitselLogic(lwords, 2)) return 0;

  return 1;
}

int test_legacy_named_bitvec32(void) {
  svBitVecVal actual[2] = {0x12345678, 0xABCDEF00};
  svBitVec32 canonical[2] = {0, 0};

  svGetBitVec32(canonical, actual, 64);
  if (canonical[0] != 0x12345678) return 0;
  if (canonical[1] != 0xABCDEF00) return 0;

  svBitVecVal actual2[2] = {0, 0};
  svPutBitVec32(actual2, canonical, 64);
  if (actual2[0] != 0x12345678) return 0;
  if (actual2[1] != 0xABCDEF00) return 0;

  return 1;
}

int test_legacy_named_logicvec32(void) {
  svLogicVecVal actual[1];
  actual[0].aval = 0x0000FFFF;
  actual[0].bval = 0xFFFF0000;

  svLogicVec32 canonical[1];
  svGetLogicVec32(canonical, actual, 32);
  /* .c = aval, .d = bval */
  if (canonical[0].c != 0x0000FFFF) return 0;
  if (canonical[0].d != 0xFFFF0000) return 0;

  svLogicVecVal actual2[1];
  actual2[0].aval = 0;
  actual2[0].bval = 0;
  svPutLogicVec32(actual2, canonical, 32);
  if (actual2[0].aval != 0x0000FFFF) return 0;
  if (actual2[0].bval != 0xFFFF0000) return 0;

  return 1;
}

int test_legacy_named_partial_width(void) {
  /* Width 33: 2 words, only bit 0 in tail word is live. */
  svBitVecVal src33[2] = {0xFFFFFFFF, 0x00000003};
  svBitVec32 dst33[2] = {0xAAAAAAAA, 0xDEADBEEF};
  svGetBitVec32(dst33, src33, 33);
  if (dst33[0] != 0xFFFFFFFF) return 0;
  /* Only bit 0 changes; bits 1-31 of tail word preserved. */
  if (dst33[1] != ((0xDEADBEEF & ~1u) | (0x00000003 & 1u))) return 0;

  /* svPutBitVec32 width 33: reverse direction. */
  svBitVec32 src33b[2] = {0x12345678, 0x00000003};
  svBitVecVal dst33b[2] = {0xAAAAAAAA, 0xDEADBEEF};
  svPutBitVec32(dst33b, src33b, 33);
  if (dst33b[0] != 0x12345678) return 0;
  if (dst33b[1] != ((0xDEADBEEF & ~1u) | (0x00000003 & 1u))) return 0;

  /* svGetLogicVec32 width 33: .c/.d <- .aval/.bval, tail masked. */
  svLogicVecVal lsrc33[2];
  lsrc33[0].aval = 0x0000FFFF; lsrc33[0].bval = 0xFFFF0000;
  lsrc33[1].aval = 0x00000003; lsrc33[1].bval = 0x00000001;
  svLogicVec32 ldst33[2];
  ldst33[0].c = 0xAAAAAAAA; ldst33[0].d = 0xBBBBBBBB;
  ldst33[1].c = 0xDEADBEEF; ldst33[1].d = 0xCAFEBABE;
  svGetLogicVec32(ldst33, lsrc33, 33);
  if (ldst33[0].c != 0x0000FFFF) return 0;
  if (ldst33[0].d != 0xFFFF0000) return 0;
  if (ldst33[1].c != ((0xDEADBEEF & ~1u) | (0x00000003 & 1u))) return 0;
  if (ldst33[1].d != ((0xCAFEBABE & ~1u) | (0x00000001 & 1u))) return 0;

  /* svPutLogicVec32 width 33: .aval/.bval <- .c/.d, tail masked. */
  svLogicVec32 lsrc33b[2];
  lsrc33b[0].c = 0x0000FFFF; lsrc33b[0].d = 0xFFFF0000;
  lsrc33b[1].c = 0x00000003; lsrc33b[1].d = 0x00000001;
  svLogicVecVal ldst33b[2];
  ldst33b[0].aval = 0xAAAAAAAA; ldst33b[0].bval = 0xBBBBBBBB;
  ldst33b[1].aval = 0xDEADBEEF; ldst33b[1].bval = 0xCAFEBABE;
  svPutLogicVec32(ldst33b, lsrc33b, 33);
  if (ldst33b[0].aval != 0x0000FFFF) return 0;
  if (ldst33b[0].bval != 0xFFFF0000) return 0;
  if (ldst33b[1].aval != ((0xDEADBEEF & ~1u) | (0x00000003 & 1u))) return 0;
  if (ldst33b[1].bval != ((0xCAFEBABE & ~1u) | (0x00000001 & 1u))) return 0;

  /* Width 65: 3 words, bit 0 in third word is live. mask = 0x00000001. */
  svBitVecVal src65[3] = {0x11111111, 0x22222222, 0x00000007};
  svBitVec32 dst65[3] = {0xAAAAAAAA, 0xBBBBBBBB, 0xDEADBEEF};
  svGetBitVec32(dst65, src65, 65);
  if (dst65[0] != 0x11111111) return 0;
  if (dst65[1] != 0x22222222) return 0;
  if (dst65[2] != ((0xDEADBEEF & ~1u) | (0x00000007 & 1u))) return 0;

  svBitVec32 src65b[3] = {0x11111111, 0x22222222, 0x00000007};
  svBitVecVal dst65b[3] = {0xAAAAAAAA, 0xBBBBBBBB, 0xDEADBEEF};
  svPutBitVec32(dst65b, src65b, 65);
  if (dst65b[0] != 0x11111111) return 0;
  if (dst65b[1] != 0x22222222) return 0;
  if (dst65b[2] != ((0xDEADBEEF & ~1u) | (0x00000007 & 1u))) return 0;

  svLogicVecVal lsrc65[3];
  lsrc65[0].aval = 0x11111111; lsrc65[0].bval = 0x22222222;
  lsrc65[1].aval = 0x33333333; lsrc65[1].bval = 0x44444444;
  lsrc65[2].aval = 0x00000003; lsrc65[2].bval = 0x00000001;
  svLogicVec32 ldst65[3];
  ldst65[0].c = 0xAAAAAAAA; ldst65[0].d = 0xBBBBBBBB;
  ldst65[1].c = 0xCCCCCCCC; ldst65[1].d = 0xDDDDDDDD;
  ldst65[2].c = 0xDEADBEEF; ldst65[2].d = 0xCAFEBABE;
  svGetLogicVec32(ldst65, lsrc65, 65);
  if (ldst65[0].c != 0x11111111) return 0;
  if (ldst65[0].d != 0x22222222) return 0;
  if (ldst65[1].c != 0x33333333) return 0;
  if (ldst65[1].d != 0x44444444) return 0;
  if (ldst65[2].c != ((0xDEADBEEF & ~1u) | (0x00000003 & 1u))) return 0;
  if (ldst65[2].d != ((0xCAFEBABE & ~1u) | (0x00000001 & 1u))) return 0;

  svLogicVec32 lsrc65b[3];
  lsrc65b[0].c = 0x11111111; lsrc65b[0].d = 0x22222222;
  lsrc65b[1].c = 0x33333333; lsrc65b[1].d = 0x44444444;
  lsrc65b[2].c = 0x00000003; lsrc65b[2].d = 0x00000001;
  svLogicVecVal ldst65b[3];
  ldst65b[0].aval = 0xAAAAAAAA; ldst65b[0].bval = 0xBBBBBBBB;
  ldst65b[1].aval = 0xCCCCCCCC; ldst65b[1].bval = 0xDDDDDDDD;
  ldst65b[2].aval = 0xDEADBEEF; ldst65b[2].bval = 0xCAFEBABE;
  svPutLogicVec32(ldst65b, lsrc65b, 65);
  if (ldst65b[0].aval != 0x11111111) return 0;
  if (ldst65b[0].bval != 0x22222222) return 0;
  if (ldst65b[1].aval != 0x33333333) return 0;
  if (ldst65b[1].bval != 0x44444444) return 0;
  if (ldst65b[2].aval != ((0xDEADBEEF & ~1u) | (0x00000003 & 1u))) return 0;
  if (ldst65b[2].bval != ((0xCAFEBABE & ~1u) | (0x00000001 & 1u))) return 0;

  return 1;
}

int test_legacy_named_partselect(void) {
  /* Bit part-select via legacy-named API. */
  svBitVecVal src_bits[2] = {0xABCD1234, 0x5678EF00};
  svBitVec32 out_bits = 0xDEADBEEF;
  svGetPartSelectBit(&out_bits, src_bits, 8, 16);
  if (out_bits != ((0xDEADBEEF & 0xFFFF0000u) | 0x0000CD12u)) return 0;

  /* Logic part-select: .c/.d <- .aval/.bval mapping. */
  svLogicVecVal src_logic[1];
  src_logic[0].aval = 0x0000000F;
  src_logic[0].bval = 0x0000000A;
  svLogicVec32 out_logic;
  out_logic.c = 0xDEADBEEF;
  out_logic.d = 0xCAFEBABE;
  svGetPartSelectLogic(&out_logic, src_logic, 0, 4);
  if ((out_logic.c & 0xF) != 0xF) return 0;
  if ((out_logic.d & 0xF) != 0xA) return 0;
  if ((out_logic.c & 0xFFFFFFF0u) != (0xDEADBEEF & 0xFFFFFFF0u)) return 0;
  if ((out_logic.d & 0xFFFFFFF0u) != (0xCAFEBABE & 0xFFFFFFF0u)) return 0;

  /* Bit put-part-select via legacy-named API. */
  svBitVecVal dst_bits[2] = {0xDEADBEEF, 0xCAFEBABE};
  svBitVec32 in_bits = 0x00001234;
  svPutPartSelectBit(dst_bits, in_bits, 8, 16);
  if (dst_bits[0] != ((0xDEADBEEF & ~0x00FFFF00u) | 0x00123400u)) return 0;
  if (dst_bits[1] != 0xCAFEBABE) return 0;

  /* Logic put-part-select via legacy-named API. */
  svLogicVecVal dst_lps[1];
  dst_lps[0].aval = 0xDEADBEEF;
  dst_lps[0].bval = 0xCAFEBABE;
  svLogicVec32 in_lps;
  in_lps.c = 0x0000000F;
  in_lps.d = 0x0000000A;
  svPutPartSelectLogic(dst_lps, in_lps, 4, 4);
  if ((dst_lps[0].aval & 0xF0) != 0xF0) return 0;
  if ((dst_lps[0].bval & 0xF0) != 0xA0) return 0;
  if ((dst_lps[0].aval & 0xFFFFFF0Fu) != (0xDEADBEEF & 0xFFFFFF0Fu)) return 0;
  if ((dst_lps[0].bval & 0xFFFFFF0Fu) != (0xCAFEBABE & 0xFFFFFF0Fu)) return 0;

  return 1;
}

/* D6b scope API tests. These run during simulation when the DPI export
   context is active, so svGetScope/svSetScope/etc. are available. */

#include <string.h>

int test_scope_get_returns_null_initially(void) {
  svScope s = svGetScope();
  return s == 0;
}

int test_scope_from_name_valid(void) {
  svScope s = svGetScopeFromName("Test");
  return s != 0;
}

int test_scope_from_name_invalid(void) {
  svScope s = svGetScopeFromName("NonExistent.Path");
  return s == 0;
}

int test_scope_name_roundtrip(void) {
  svScope s = svGetScopeFromName("Test");
  if (s == 0) return 0;
  const char* name = svGetNameFromScope(s);
  if (name == 0) return 0;
  return strcmp(name, "Test") == 0;
}

int test_scope_set_get_roundtrip(void) {
  svScope s = svGetScopeFromName("Test");
  if (s == 0) return 0;
  svScope prev = svSetScope(s);
  svScope cur = svGetScope();
  if (cur != s) return 0;
  svSetScope(prev);
  return 1;
}

int test_scope_set_returns_previous(void) {
  svScope s = svGetScopeFromName("Test");
  if (s == 0) return 0;
  svScope prev1 = svSetScope(s);
  svScope prev2 = svSetScope(0);
  if (prev2 != s) return 0;
  svScope cur = svGetScope();
  if (cur != 0) return 0;
  (void)prev1;
  return 1;
}

int test_scope_user_data_roundtrip(void) {
  svScope s = svGetScopeFromName("Test");
  if (s == 0) return 0;
  int dummy_key = 42;
  int dummy_data = 99;
  int rc = svPutUserData(s, &dummy_key, &dummy_data);
  if (rc != 0) return 0;
  void* got = svGetUserData(s, &dummy_key);
  if (got != &dummy_data) return 0;
  return 1;
}

int test_scope_user_data_null_scope(void) {
  int dummy_key = 1;
  int rc = svPutUserData(0, &dummy_key, &dummy_key);
  return rc == -1;
}

int test_scope_name_from_null(void) {
  const char* name = svGetNameFromScope(0);
  return name == 0;
}

int test_size_helpers(void) {
  if (svSizeOfBitPackedArr(1) != 4) return 0;
  if (svSizeOfBitPackedArr(32) != 4) return 0;
  if (svSizeOfBitPackedArr(33) != 8) return 0;
  if (svSizeOfBitPackedArr(64) != 8) return 0;
  if (svSizeOfBitPackedArr(65) != 12) return 0;

  if (svSizeOfLogicPackedArr(1) != 8) return 0;
  if (svSizeOfLogicPackedArr(32) != 8) return 0;
  if (svSizeOfLogicPackedArr(33) != 16) return 0;
  if (svSizeOfLogicPackedArr(64) != 16) return 0;
  if (svSizeOfLogicPackedArr(65) != 24) return 0;

  return 1;
}
