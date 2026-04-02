// svdpi.h runtime symbol surface for Lyra (D6a).
//
// Implements all 99 function symbols declared by svdpi.h. Stateless packed-
// array utility entrypoints have working implementations. All scope-dependent,
// time-dependent, and open-array APIs are present as hard-error traps with
// explicit diagnostic messages naming the owning queue item.
//
// Contract: all utility helpers assume valid caller-provided buffers and bit
// indices. No null checks, no bounds checks. These are raw ABI utilities per
// IEEE 1800-2023 Section 35.5.

#include <cstdint>
#include <cstdio>
#include <cstdlib>

#include "svdpi.h"

namespace {

// Trap for unsupported svdpi APIs. Fixed format for grep-ability.
[[noreturn]] void TrapUnsupportedSvdpi(const char* api, const char* owner) {
  std::fputs("fatal: unsupported svdpi API ", stderr);
  std::fputs(api, stderr);
  std::fputs("; owned by ", stderr);
  std::fputs(owner, stderr);
  std::fputs("\n", stderr);
  std::abort();
}

auto TailMaskForWidth(int width) -> uint32_t {
  int live = width % 32;
  if (live == 0) return 0xFFFFFFFFU;
  return (1U << live) - 1U;
}

auto GetWordIndex(int bit_index) -> int {
  return bit_index / 32;
}
auto GetBitIndex(int bit_index) -> int {
  return bit_index % 32;
}

auto ReadBit(const svBitVecVal* words, int bit_index) -> svBit {
  return static_cast<svBit>(
      (words[GetWordIndex(bit_index)] >> GetBitIndex(bit_index)) & 1);
}

void WriteBit(svBitVecVal* words, int bit_index, svBit value) {
  int w = GetWordIndex(bit_index);
  int b = GetBitIndex(bit_index);
  words[w] = (words[w] & ~(1U << b)) | ((value & 1U) << b);
}

auto ReadLogic(const svLogicVecVal* words, int bit_index) -> svLogic {
  int w = GetWordIndex(bit_index);
  int b = GetBitIndex(bit_index);
  unsigned a = (words[w].aval >> b) & 1;
  unsigned bv = (words[w].bval >> b) & 1;
  // (aval=0,bval=0)->sv_0, (1,0)->sv_1, (0,1)->sv_z, (1,1)->sv_x
  return static_cast<svLogic>((bv << 1) | a);
}

void WriteLogic(svLogicVecVal* words, int bit_index, svLogic value) {
  int w = GetWordIndex(bit_index);
  int b = GetBitIndex(bit_index);
  // Mask to 2 bits: higher bits in value are ignored.
  unsigned a = value & 1U;
  unsigned bv = (value >> 1) & 1U;
  words[w].aval = (words[w].aval & ~(1U << b)) | (a << b);
  words[w].bval = (words[w].bval & ~(1U << b)) | (bv << b);
}

// Copy width bits from src[lsb..lsb+width-1] to dst[0..width-1].
// Only modifies the targeted bits in dst.
void CopyBitPartselOut(
    svBitVecVal* dst, const svBitVecVal* src, int lsb, int width) {
  for (int j = 0; j < width; ++j) {
    WriteBit(dst, j, ReadBit(src, lsb + j));
  }
}

void CopyLogicPartselOut(
    svLogicVecVal* dst, const svLogicVecVal* src, int lsb, int width) {
  for (int j = 0; j < width; ++j) {
    WriteLogic(dst, j, ReadLogic(src, lsb + j));
  }
}

// Copy width bits from src[0..width-1] to dst[lsb..lsb+width-1].
// Only modifies the targeted bits in dst.
void CopyBitPartselIn(
    svBitVecVal* dst, const svBitVecVal* src, int lsb, int width) {
  for (int j = 0; j < width; ++j) {
    WriteBit(dst, lsb + j, ReadBit(src, j));
  }
}

void CopyLogicPartselIn(
    svLogicVecVal* dst, const svLogicVecVal* src, int lsb, int width) {
  for (int j = 0; j < width; ++j) {
    WriteLogic(dst, lsb + j, ReadLogic(src, j));
  }
}

}  // namespace

// ---------------------------------------------------------------------------
// Version API
// ---------------------------------------------------------------------------

extern "C" const char* svDpiVersion(void) {
  return "1800-2023";
}

// ---------------------------------------------------------------------------
// Core packed utility entrypoints
// ---------------------------------------------------------------------------

extern "C" svBit svGetBitselBit(const svBitVecVal* s, int i) {
  return ReadBit(s, i);
}

extern "C" svLogic svGetBitselLogic(const svLogicVecVal* s, int i) {
  return ReadLogic(s, i);
}

extern "C" void svPutBitselBit(svBitVecVal* d, int i, svBit s) {
  WriteBit(d, i, s);
}

extern "C" void svPutBitselLogic(svLogicVecVal* d, int i, svLogic s) {
  WriteLogic(d, i, s);
}

extern "C" void svGetPartselBit(
    svBitVecVal* d, const svBitVecVal* s, int i, int w) {
  CopyBitPartselOut(d, s, i, w);
}

extern "C" void svGetPartselLogic(
    svLogicVecVal* d, const svLogicVecVal* s, int i, int w) {
  CopyLogicPartselOut(d, s, i, w);
}

extern "C" void svPutPartselBit(
    svBitVecVal* d, const svBitVecVal s, int i, int w) {
  CopyBitPartselIn(d, &s, i, w);
}

extern "C" void svPutPartselLogic(
    svLogicVecVal* d, const svLogicVecVal s, int i, int w) {
  CopyLogicPartselIn(d, &s, i, w);
}

// ---------------------------------------------------------------------------
// Legacy-named standard svdpi.h entrypoints
//
// These adapt the older svdpi.h API surface (void*-typed svBitPackedArrRef /
// svLogicPackedArrRef, svBitVec32/svLogicVec32 types) to the core helpers.
// reinterpret_cast and pointer arithmetic are inherent to the void* typedefs
// mandated by the C standard header.
// ---------------------------------------------------------------------------

extern "C" int svSizeOfBitPackedArr(int width) {
  return ((width + 31) / 32) * static_cast<int>(sizeof(svBitVecVal));
}

extern "C" int svSizeOfLogicPackedArr(int width) {
  return ((width + 31) / 32) * static_cast<int>(sizeof(svLogicVecVal));
}

extern "C" svBit svGetSelectBit(const svBitPackedArrRef s, int i) {
  return svGetBitselBit(reinterpret_cast<const svBitVecVal*>(s), i);
}

extern "C" svLogic svGetSelectLogic(const svLogicPackedArrRef s, int i) {
  return svGetBitselLogic(reinterpret_cast<const svLogicVecVal*>(s), i);
}

extern "C" void svPutSelectBit(svBitPackedArrRef d, int i, svBit s) {
  svPutBitselBit(reinterpret_cast<svBitVecVal*>(d), i, s);
}

extern "C" void svPutSelectLogic(svLogicPackedArrRef d, int i, svLogic s) {
  svPutBitselLogic(reinterpret_cast<svLogicVecVal*>(d), i, s);
}

extern "C" void svGetPartSelectBit(
    svBitVec32* d, const svBitPackedArrRef s, int i, int w) {
  svGetPartselBit(d, reinterpret_cast<const svBitVecVal*>(s), i, w);
}

extern "C" void svGetPartSelectLogic(
    svLogicVec32* d, const svLogicPackedArrRef s, int i, int w) {
  svLogicVecVal tmp;
  tmp.aval = d->c;
  tmp.bval = d->d;
  svGetPartselLogic(&tmp, reinterpret_cast<const svLogicVecVal*>(s), i, w);
  d->c = tmp.aval;
  d->d = tmp.bval;
}

extern "C" void svPutPartSelectBit(
    svBitPackedArrRef d, const svBitVec32 s, int i, int w) {
  svPutPartselBit(reinterpret_cast<svBitVecVal*>(d), s, i, w);
}

extern "C" void svPutPartSelectLogic(
    svLogicPackedArrRef d, const svLogicVec32 s, int i, int w) {
  svLogicVecVal tmp;
  tmp.aval = s.c;
  tmp.bval = s.d;
  svPutPartselLogic(reinterpret_cast<svLogicVecVal*>(d), tmp, i, w);
}

extern "C" svBitVec32 svGetBits(const svBitPackedArrRef s, int i, int w) {
  const auto* src = reinterpret_cast<const svBitVecVal*>(s);
  svBitVec32 result = 0;
  for (int j = 0; j < w; ++j) {
    result |= static_cast<svBitVec32>(ReadBit(src, i + j)) << j;
  }
  return result;
}

extern "C" svBitVec32 svGet32Bits(const svBitPackedArrRef s, int i) {
  return svGetBits(s, i, 32);
}

extern "C" uint64_t svGet64Bits(const svBitPackedArrRef s, int i) {
  auto low = static_cast<uint64_t>(svGetBits(s, i, 32));
  auto high = static_cast<uint64_t>(svGetBits(s, i + 32, 32));
  return low | (high << 32);
}

extern "C" void svPutBitVec32(svBitPackedArrRef d, const svBitVec32* s, int w) {
  auto* dst = reinterpret_cast<svBitVecVal*>(d);
  int n = (w + 31) / 32;
  if (n == 0) return;
  for (int i = 0; i + 1 < n; ++i) {
    dst[i] = s[i];
  }
  uint32_t mask = TailMaskForWidth(w);
  dst[n - 1] = (dst[n - 1] & ~mask) | (s[n - 1] & mask);
}

extern "C" void svGetBitVec32(svBitVec32* d, const svBitPackedArrRef s, int w) {
  const auto* src = reinterpret_cast<const svBitVecVal*>(s);
  int n = (w + 31) / 32;
  if (n == 0) return;
  for (int i = 0; i + 1 < n; ++i) {
    d[i] = src[i];
  }
  uint32_t mask = TailMaskForWidth(w);
  d[n - 1] = (d[n - 1] & ~mask) | (src[n - 1] & mask);
}

extern "C" void svPutLogicVec32(
    svLogicPackedArrRef d, const svLogicVec32* s, int w) {
  auto* dst = reinterpret_cast<svLogicVecVal*>(d);
  int n = (w + 31) / 32;
  if (n == 0) return;
  for (int i = 0; i + 1 < n; ++i) {
    dst[i].aval = s[i].c;
    dst[i].bval = s[i].d;
  }
  uint32_t mask = TailMaskForWidth(w);
  dst[n - 1].aval = (dst[n - 1].aval & ~mask) | (s[n - 1].c & mask);
  dst[n - 1].bval = (dst[n - 1].bval & ~mask) | (s[n - 1].d & mask);
}

extern "C" void svGetLogicVec32(
    svLogicVec32* d, const svLogicPackedArrRef s, int w) {
  const auto* src = reinterpret_cast<const svLogicVecVal*>(s);
  int n = (w + 31) / 32;
  if (n == 0) return;
  for (int i = 0; i + 1 < n; ++i) {
    d[i].c = src[i].aval;
    d[i].d = src[i].bval;
  }
  uint32_t mask = TailMaskForWidth(w);
  d[n - 1].c = (d[n - 1].c & ~mask) | (src[n - 1].aval & mask);
  d[n - 1].d = (d[n - 1].d & ~mask) | (src[n - 1].bval & mask);
}

// ---------------------------------------------------------------------------
// Trapped APIs
//
// All trap functions below call [[noreturn]] TrapUnsupportedSvdpi. Signatures
// must match svdpi.h exactly; clang-tidy false-positives on parameter naming
// (snake_case vs header's camelCase) and const-eligibility (unused params in
// [[noreturn]] bodies) are suppressed for the entire trap section.
// ---------------------------------------------------------------------------

// -- scope (D6b) --

extern "C" svScope svGetScope(void) {
  TrapUnsupportedSvdpi("svGetScope", "D6b");
}

extern "C" svScope svSetScope(const svScope scope) {
  (void)scope;
  TrapUnsupportedSvdpi("svSetScope", "D6b");
}

extern "C" const char* svGetNameFromScope(const svScope scope) {
  (void)scope;
  TrapUnsupportedSvdpi("svGetNameFromScope", "D6b");
}

extern "C" svScope svGetScopeFromName(const char* scopeName) {
  (void)scopeName;
  TrapUnsupportedSvdpi("svGetScopeFromName", "D6b");
}

extern "C" int svPutUserData(
    const svScope scope, void* userKey, void* userData) {
  (void)scope;
  (void)userKey;
  (void)userData;
  TrapUnsupportedSvdpi("svPutUserData", "D6b");
}

extern "C" void* svGetUserData(const svScope scope, void* userKey) {
  (void)scope;
  (void)userKey;
  TrapUnsupportedSvdpi("svGetUserData", "D6b");
}

// ---------------------------------------------------------------------------
// Trapped: time APIs (later time-query work)
// ---------------------------------------------------------------------------

extern "C" int svGetTime(const svScope scope, svTimeVal* time) {
  (void)scope;
  (void)time;
  TrapUnsupportedSvdpi("svGetTime", "later time-query work");
}

extern "C" int svGetTimeUnit(const svScope scope, int32_t* time_unit) {
  (void)scope;
  (void)time_unit;
  TrapUnsupportedSvdpi("svGetTimeUnit", "later time-query work");
}

extern "C" int svGetTimePrecision(
    const svScope scope, int32_t* time_precision) {
  (void)scope;
  (void)time_precision;
  TrapUnsupportedSvdpi("svGetTimePrecision", "later time-query work");
}

// ---------------------------------------------------------------------------
// Trapped: caller-info API (later caller-info metadata work)
// ---------------------------------------------------------------------------

extern "C" int svGetCallerInfo(const char** fileName, int* lineNumber) {
  (void)fileName;
  (void)lineNumber;
  TrapUnsupportedSvdpi("svGetCallerInfo", "later caller-info metadata work");
}

// ---------------------------------------------------------------------------
// Trapped: disable-state APIs (D7a)
// ---------------------------------------------------------------------------

extern "C" int svIsDisabledState(void) {
  TrapUnsupportedSvdpi("svIsDisabledState", "D7a");
}

extern "C" void svAckDisabledState(void) {
  TrapUnsupportedSvdpi("svAckDisabledState", "D7a");
}

// ---------------------------------------------------------------------------
// Trapped: open-array query (later DPI open-array work)
// ---------------------------------------------------------------------------

// Open-array trap functions: all parameters are unused (body calls
// [[noreturn]] trap). Naming 300+ positional parameters across 61 functions
// adds noise without value.

extern "C" int svLeft(const svOpenArrayHandle h, int d) {
  (void)h;
  (void)d;
  TrapUnsupportedSvdpi("svLeft", "later DPI open-array work");
}

extern "C" int svRight(const svOpenArrayHandle h, int d) {
  (void)h;
  (void)d;
  TrapUnsupportedSvdpi("svRight", "later DPI open-array work");
}

extern "C" int svLow(const svOpenArrayHandle h, int d) {
  (void)h;
  (void)d;
  TrapUnsupportedSvdpi("svLow", "later DPI open-array work");
}

extern "C" int svHigh(const svOpenArrayHandle h, int d) {
  (void)h;
  (void)d;
  TrapUnsupportedSvdpi("svHigh", "later DPI open-array work");
}

extern "C" int svIncrement(const svOpenArrayHandle h, int d) {
  (void)h;
  (void)d;
  TrapUnsupportedSvdpi("svIncrement", "later DPI open-array work");
}

extern "C" int svSize(const svOpenArrayHandle h, int d) {
  (void)h;
  (void)d;
  TrapUnsupportedSvdpi("svSize", "later DPI open-array work");
}

extern "C" int svDimensions(const svOpenArrayHandle h) {
  (void)h;
  TrapUnsupportedSvdpi("svDimensions", "later DPI open-array work");
}

// ---------------------------------------------------------------------------
// Trapped: open-array pointer access (later DPI open-array work)
// ---------------------------------------------------------------------------

extern "C" void* svGetArrayPtr(const svOpenArrayHandle h) {
  (void)h;
  TrapUnsupportedSvdpi("svGetArrayPtr", "later DPI open-array work");
}

extern "C" int svSizeOfArray(const svOpenArrayHandle h) {
  (void)h;
  TrapUnsupportedSvdpi("svSizeOfArray", "later DPI open-array work");
}

extern "C" void* svGetArrElemPtr(const svOpenArrayHandle h, int indx1, ...) {
  (void)h;
  (void)indx1;
  TrapUnsupportedSvdpi("svGetArrElemPtr", "later DPI open-array work");
}

extern "C" void* svGetArrElemPtr1(const svOpenArrayHandle h, int indx1) {
  (void)h;
  (void)indx1;
  TrapUnsupportedSvdpi("svGetArrElemPtr1", "later DPI open-array work");
}

extern "C" void* svGetArrElemPtr2(
    const svOpenArrayHandle h, int indx1, int indx2) {
  (void)h;
  (void)indx1;
  (void)indx2;
  TrapUnsupportedSvdpi("svGetArrElemPtr2", "later DPI open-array work");
}

extern "C" void* svGetArrElemPtr3(
    const svOpenArrayHandle h, int indx1, int indx2, int indx3) {
  (void)h;
  (void)indx1;
  (void)indx2;
  (void)indx3;
  TrapUnsupportedSvdpi("svGetArrElemPtr3", "later DPI open-array work");
}

// ---------------------------------------------------------------------------
// Trapped: open-array packed VecVal copy (later DPI open-array work)
// ---------------------------------------------------------------------------

extern "C" void svPutBitArrElemVecVal(
    const svOpenArrayHandle d, const svBitVecVal* s, int indx1, ...) {
  (void)d;
  (void)s;
  (void)indx1;
  TrapUnsupportedSvdpi("svPutBitArrElemVecVal", "later DPI open-array work");
}

extern "C" void svPutBitArrElem1VecVal(
    const svOpenArrayHandle d, const svBitVecVal* s, int indx1) {
  (void)d;
  (void)s;
  (void)indx1;
  TrapUnsupportedSvdpi("svPutBitArrElem1VecVal", "later DPI open-array work");
}

extern "C" void svPutBitArrElem2VecVal(
    const svOpenArrayHandle d, const svBitVecVal* s, int indx1, int indx2) {
  (void)d;
  (void)s;
  (void)indx1;
  (void)indx2;
  TrapUnsupportedSvdpi("svPutBitArrElem2VecVal", "later DPI open-array work");
}

extern "C" void svPutBitArrElem3VecVal(
    const svOpenArrayHandle d, const svBitVecVal* s, int indx1, int indx2,
    int indx3) {
  (void)d;
  (void)s;
  (void)indx1;
  (void)indx2;
  (void)indx3;
  TrapUnsupportedSvdpi("svPutBitArrElem3VecVal", "later DPI open-array work");
}

extern "C" void svPutLogicArrElemVecVal(
    const svOpenArrayHandle d, const svLogicVecVal* s, int indx1, ...) {
  (void)d;
  (void)s;
  (void)indx1;
  TrapUnsupportedSvdpi("svPutLogicArrElemVecVal", "later DPI open-array work");
}

extern "C" void svPutLogicArrElem1VecVal(
    const svOpenArrayHandle d, const svLogicVecVal* s, int indx1) {
  (void)d;
  (void)s;
  (void)indx1;
  TrapUnsupportedSvdpi("svPutLogicArrElem1VecVal", "later DPI open-array work");
}

extern "C" void svPutLogicArrElem2VecVal(
    const svOpenArrayHandle d, const svLogicVecVal* s, int indx1, int indx2) {
  (void)d;
  (void)s;
  (void)indx1;
  (void)indx2;
  TrapUnsupportedSvdpi("svPutLogicArrElem2VecVal", "later DPI open-array work");
}

extern "C" void svPutLogicArrElem3VecVal(
    const svOpenArrayHandle d, const svLogicVecVal* s, int indx1, int indx2,
    int indx3) {
  (void)d;
  (void)s;
  (void)indx1;
  (void)indx2;
  (void)indx3;
  TrapUnsupportedSvdpi("svPutLogicArrElem3VecVal", "later DPI open-array work");
}

extern "C" void svGetBitArrElemVecVal(
    svBitVecVal* d, const svOpenArrayHandle s, int indx1, ...) {
  (void)d;
  (void)s;
  (void)indx1;
  TrapUnsupportedSvdpi("svGetBitArrElemVecVal", "later DPI open-array work");
}

extern "C" void svGetBitArrElem1VecVal(
    svBitVecVal* d, const svOpenArrayHandle s, int indx1) {
  (void)d;
  (void)s;
  (void)indx1;
  TrapUnsupportedSvdpi("svGetBitArrElem1VecVal", "later DPI open-array work");
}

extern "C" void svGetBitArrElem2VecVal(
    svBitVecVal* d, const svOpenArrayHandle s, int indx1, int indx2) {
  (void)d;
  (void)s;
  (void)indx1;
  (void)indx2;
  TrapUnsupportedSvdpi("svGetBitArrElem2VecVal", "later DPI open-array work");
}

extern "C" void svGetBitArrElem3VecVal(
    svBitVecVal* d, const svOpenArrayHandle s, int indx1, int indx2,
    int indx3) {
  (void)d;
  (void)s;
  (void)indx1;
  (void)indx2;
  (void)indx3;
  TrapUnsupportedSvdpi("svGetBitArrElem3VecVal", "later DPI open-array work");
}

extern "C" void svGetLogicArrElemVecVal(
    svLogicVecVal* d, const svOpenArrayHandle s, int indx1, ...) {
  (void)d;
  (void)s;
  (void)indx1;
  TrapUnsupportedSvdpi("svGetLogicArrElemVecVal", "later DPI open-array work");
}

extern "C" void svGetLogicArrElem1VecVal(
    svLogicVecVal* d, const svOpenArrayHandle s, int indx1) {
  (void)d;
  (void)s;
  (void)indx1;
  TrapUnsupportedSvdpi("svGetLogicArrElem1VecVal", "later DPI open-array work");
}

extern "C" void svGetLogicArrElem2VecVal(
    svLogicVecVal* d, const svOpenArrayHandle s, int indx1, int indx2) {
  (void)d;
  (void)s;
  (void)indx1;
  (void)indx2;
  TrapUnsupportedSvdpi("svGetLogicArrElem2VecVal", "later DPI open-array work");
}

extern "C" void svGetLogicArrElem3VecVal(
    svLogicVecVal* d, const svOpenArrayHandle s, int indx1, int indx2,
    int indx3) {
  (void)d;
  (void)s;
  (void)indx1;
  (void)indx2;
  (void)indx3;
  TrapUnsupportedSvdpi("svGetLogicArrElem3VecVal", "later DPI open-array work");
}

// ---------------------------------------------------------------------------
// Trapped: open-array scalar element access (later DPI open-array work)
// ---------------------------------------------------------------------------

extern "C" svBit svGetBitArrElem(const svOpenArrayHandle s, int indx1, ...) {
  (void)s;
  (void)indx1;
  TrapUnsupportedSvdpi("svGetBitArrElem", "later DPI open-array work");
}

extern "C" svBit svGetBitArrElem1(const svOpenArrayHandle s, int indx1) {
  (void)s;
  (void)indx1;
  TrapUnsupportedSvdpi("svGetBitArrElem1", "later DPI open-array work");
}

extern "C" svBit svGetBitArrElem2(
    const svOpenArrayHandle s, int indx1, int indx2) {
  (void)s;
  (void)indx1;
  (void)indx2;
  TrapUnsupportedSvdpi("svGetBitArrElem2", "later DPI open-array work");
}

extern "C" svBit svGetBitArrElem3(
    const svOpenArrayHandle s, int indx1, int indx2, int indx3) {
  (void)s;
  (void)indx1;
  (void)indx2;
  (void)indx3;
  TrapUnsupportedSvdpi("svGetBitArrElem3", "later DPI open-array work");
}

extern "C" svLogic svGetLogicArrElem(
    const svOpenArrayHandle s, int indx1, ...) {
  (void)s;
  (void)indx1;
  TrapUnsupportedSvdpi("svGetLogicArrElem", "later DPI open-array work");
}

extern "C" svLogic svGetLogicArrElem1(const svOpenArrayHandle s, int indx1) {
  (void)s;
  (void)indx1;
  TrapUnsupportedSvdpi("svGetLogicArrElem1", "later DPI open-array work");
}

extern "C" svLogic svGetLogicArrElem2(
    const svOpenArrayHandle s, int indx1, int indx2) {
  (void)s;
  (void)indx1;
  (void)indx2;
  TrapUnsupportedSvdpi("svGetLogicArrElem2", "later DPI open-array work");
}

extern "C" svLogic svGetLogicArrElem3(
    const svOpenArrayHandle s, int indx1, int indx2, int indx3) {
  (void)s;
  (void)indx1;
  (void)indx2;
  (void)indx3;
  TrapUnsupportedSvdpi("svGetLogicArrElem3", "later DPI open-array work");
}

extern "C" void svPutLogicArrElem(
    const svOpenArrayHandle d, svLogic value, int indx1, ...) {
  (void)d;
  (void)value;
  (void)indx1;
  TrapUnsupportedSvdpi("svPutLogicArrElem", "later DPI open-array work");
}

extern "C" void svPutLogicArrElem1(
    const svOpenArrayHandle d, svLogic value, int indx1) {
  (void)d;
  (void)value;
  (void)indx1;
  TrapUnsupportedSvdpi("svPutLogicArrElem1", "later DPI open-array work");
}

extern "C" void svPutLogicArrElem2(
    const svOpenArrayHandle d, svLogic value, int indx1, int indx2) {
  (void)d;
  (void)value;
  (void)indx1;
  (void)indx2;
  TrapUnsupportedSvdpi("svPutLogicArrElem2", "later DPI open-array work");
}

extern "C" void svPutLogicArrElem3(
    const svOpenArrayHandle d, svLogic value, int indx1, int indx2, int indx3) {
  (void)d;
  (void)value;
  (void)indx1;
  (void)indx2;
  (void)indx3;
  TrapUnsupportedSvdpi("svPutLogicArrElem3", "later DPI open-array work");
}

extern "C" void svPutBitArrElem(
    const svOpenArrayHandle d, svBit value, int indx1, ...) {
  (void)d;
  (void)value;
  (void)indx1;
  TrapUnsupportedSvdpi("svPutBitArrElem", "later DPI open-array work");
}

extern "C" void svPutBitArrElem1(
    const svOpenArrayHandle d, svBit value, int indx1) {
  (void)d;
  (void)value;
  (void)indx1;
  TrapUnsupportedSvdpi("svPutBitArrElem1", "later DPI open-array work");
}

extern "C" void svPutBitArrElem2(
    const svOpenArrayHandle d, svBit value, int indx1, int indx2) {
  (void)d;
  (void)value;
  (void)indx1;
  (void)indx2;
  TrapUnsupportedSvdpi("svPutBitArrElem2", "later DPI open-array work");
}

extern "C" void svPutBitArrElem3(
    const svOpenArrayHandle d, svBit value, int indx1, int indx2, int indx3) {
  (void)d;
  (void)value;
  (void)indx1;
  (void)indx2;
  (void)indx3;
  TrapUnsupportedSvdpi("svPutBitArrElem3", "later DPI open-array work");
}

// ---------------------------------------------------------------------------
// Trapped: legacy-named Vec32 open-array entrypoints (later DPI open-array
// work)
// ---------------------------------------------------------------------------

extern "C" void svPutBitArrElemVec32(
    const svOpenArrayHandle d, const svBitVec32* s, int indx1, ...) {
  (void)d;
  (void)s;
  (void)indx1;
  TrapUnsupportedSvdpi("svPutBitArrElemVec32", "later DPI open-array work");
}

extern "C" void svPutBitArrElem1Vec32(
    const svOpenArrayHandle d, const svBitVec32* s, int indx1) {
  (void)d;
  (void)s;
  (void)indx1;
  TrapUnsupportedSvdpi("svPutBitArrElem1Vec32", "later DPI open-array work");
}

extern "C" void svPutBitArrElem2Vec32(
    const svOpenArrayHandle d, const svBitVec32* s, int indx1, int indx2) {
  (void)d;
  (void)s;
  (void)indx1;
  (void)indx2;
  TrapUnsupportedSvdpi("svPutBitArrElem2Vec32", "later DPI open-array work");
}

extern "C" void svPutBitArrElem3Vec32(
    const svOpenArrayHandle d, const svBitVec32* s, int indx1, int indx2,
    int indx3) {
  (void)d;
  (void)s;
  (void)indx1;
  (void)indx2;
  (void)indx3;
  TrapUnsupportedSvdpi("svPutBitArrElem3Vec32", "later DPI open-array work");
}

extern "C" void svPutLogicArrElemVec32(
    const svOpenArrayHandle d, const svLogicVec32* s, int indx1, ...) {
  (void)d;
  (void)s;
  (void)indx1;
  TrapUnsupportedSvdpi("svPutLogicArrElemVec32", "later DPI open-array work");
}

extern "C" void svPutLogicArrElem1Vec32(
    const svOpenArrayHandle d, const svLogicVec32* s, int indx1) {
  (void)d;
  (void)s;
  (void)indx1;
  TrapUnsupportedSvdpi("svPutLogicArrElem1Vec32", "later DPI open-array work");
}

extern "C" void svPutLogicArrElem2Vec32(
    const svOpenArrayHandle d, const svLogicVec32* s, int indx1, int indx2) {
  (void)d;
  (void)s;
  (void)indx1;
  (void)indx2;
  TrapUnsupportedSvdpi("svPutLogicArrElem2Vec32", "later DPI open-array work");
}

extern "C" void svPutLogicArrElem3Vec32(
    const svOpenArrayHandle d, const svLogicVec32* s, int indx1, int indx2,
    int indx3) {
  (void)d;
  (void)s;
  (void)indx1;
  (void)indx2;
  (void)indx3;
  TrapUnsupportedSvdpi("svPutLogicArrElem3Vec32", "later DPI open-array work");
}

extern "C" void svGetBitArrElemVec32(
    svBitVec32* d, const svOpenArrayHandle s, int indx1, ...) {
  (void)d;
  (void)s;
  (void)indx1;
  TrapUnsupportedSvdpi("svGetBitArrElemVec32", "later DPI open-array work");
}

extern "C" void svGetBitArrElem1Vec32(
    svBitVec32* d, const svOpenArrayHandle s, int indx1) {
  (void)d;
  (void)s;
  (void)indx1;
  TrapUnsupportedSvdpi("svGetBitArrElem1Vec32", "later DPI open-array work");
}

extern "C" void svGetBitArrElem2Vec32(
    svBitVec32* d, const svOpenArrayHandle s, int indx1, int indx2) {
  (void)d;
  (void)s;
  (void)indx1;
  (void)indx2;
  TrapUnsupportedSvdpi("svGetBitArrElem2Vec32", "later DPI open-array work");
}

extern "C" void svGetBitArrElem3Vec32(
    svBitVec32* d, const svOpenArrayHandle s, int indx1, int indx2, int indx3) {
  (void)d;
  (void)s;
  (void)indx1;
  (void)indx2;
  (void)indx3;
  TrapUnsupportedSvdpi("svGetBitArrElem3Vec32", "later DPI open-array work");
}

extern "C" void svGetLogicArrElemVec32(
    svLogicVec32* d, const svOpenArrayHandle s, int indx1, ...) {
  (void)d;
  (void)s;
  (void)indx1;
  TrapUnsupportedSvdpi("svGetLogicArrElemVec32", "later DPI open-array work");
}

extern "C" void svGetLogicArrElem1Vec32(
    svLogicVec32* d, const svOpenArrayHandle s, int indx1) {
  (void)d;
  (void)s;
  (void)indx1;
  TrapUnsupportedSvdpi("svGetLogicArrElem1Vec32", "later DPI open-array work");
}

extern "C" void svGetLogicArrElem2Vec32(
    svLogicVec32* d, const svOpenArrayHandle s, int indx1, int indx2) {
  (void)d;
  (void)s;
  (void)indx1;
  (void)indx2;
  TrapUnsupportedSvdpi("svGetLogicArrElem2Vec32", "later DPI open-array work");
}

extern "C" void svGetLogicArrElem3Vec32(
    svLogicVec32* d, const svOpenArrayHandle s, int indx1, int indx2,
    int indx3) {
  (void)d;
  (void)s;
  (void)indx1;
  (void)indx2;
  (void)indx3;
  TrapUnsupportedSvdpi("svGetLogicArrElem3Vec32", "later DPI open-array work");
}
