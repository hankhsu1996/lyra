#include "lyra/value/dpi_canonical.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <span>

#include "lyra/value/integral_words.hpp"
#include "svdpi.h"

namespace lyra::value {

DpiBitBuffer::DpiBitBuffer(const ConstIntegralView& sv)
    : groups_(CanonicalGroups(sv.width)) {
  WriteCanonicalBitVec(groups_.data(), sv);
}

auto DpiBitBuffer::Data() -> svBitVecVal* {
  return groups_.data();
}

DpiLogicBuffer::DpiLogicBuffer(const ConstIntegralView& sv)
    : groups_(CanonicalGroups(sv.width)) {
  WriteCanonicalLogicVec(groups_.data(), sv);
}

auto DpiLogicBuffer::Data() -> svLogicVecVal* {
  return groups_.data();
}

}  // namespace lyra::value

namespace {

constexpr std::uint32_t kGroupBits = 32;

// Where `width` bits from bit `start` of a canonical vector lie: the group the
// first of them is in, how far into it, and how many groups they reach, which
// is two where they run into the next one. A part-select is at most a group
// wide (LRM Annex H.11.5), and a width the standard does not admit is taken as
// the nearest one it does, so no shift below runs past a word.
struct Window {
  std::size_t first;
  std::uint32_t offset;
  std::size_t groups;
  std::uint32_t width;
};

constexpr auto WindowAt(int start, int width) -> Window {
  const auto at = static_cast<std::uint32_t>(start);
  const std::uint32_t offset = at % kGroupBits;
  const auto bits = static_cast<std::uint32_t>(
      std::clamp(width, 0, static_cast<int>(kGroupBits)));
  return Window{
      .first = at / kGroupBits,
      .offset = offset,
      .groups = (offset + bits + kGroupBits - 1U) / kGroupBits,
      .width = bits};
}

// The plane of a group a window is read or written in: the group itself for a
// 2-state vector, and either of a 4-state group's two.
constexpr auto kBits = [](auto& group) -> auto& { return group; };
constexpr auto kValuePlane = [](auto& group) -> auto& { return group.aval; };
constexpr auto kUnknownPlane = [](auto& group) -> auto& { return group.bval; };

// The window's bits of one plane, at the low end of the answer, as LLVM's
// `APInt::extractBits` takes them. Only the groups the window reaches are read.
template <typename Group, typename Plane>
auto ExtractBits(const Group* vector, Window window, Plane plane)
    -> std::uint32_t {
  const std::span<const Group> reached =
      std::span<const Group>{vector, window.first + window.groups}.subspan(
          window.first);
  std::uint64_t bits = 0;
  for (std::size_t k = 0; k < reached.size(); ++k) {
    bits |= std::uint64_t{plane(reached[k])} << (kGroupBits * k);
  }
  return static_cast<std::uint32_t>(
      (bits >> window.offset) & lyra::value::LowBits(window.width));
}

// The low bits of `bits` written into the window of one plane, as
// `APInt::insertBits` writes them: every bit of a group around the window is
// kept as it was.
template <typename Group, typename Plane>
void InsertBits(Group* vector, Window window, std::uint32_t bits, Plane plane) {
  const std::span<Group> reached =
      std::span<Group>{vector, window.first + window.groups}.subspan(
          window.first);
  const std::uint64_t mask = lyra::value::LowBits(window.width)
                             << window.offset;
  const std::uint64_t placed = (std::uint64_t{bits} << window.offset) & mask;
  for (std::size_t k = 0; k < reached.size(); ++k) {
    const std::size_t shift = kGroupBits * k;
    std::uint32_t& group = plane(reached[k]);
    group = static_cast<std::uint32_t>(
        (group & ~(mask >> shift)) | (placed >> shift));
  }
}

// An svLogic scalar from its value and unknown bits, and back (LRM Annex
// H.10.1.1).
constexpr auto SvLogicOf(std::uint32_t value, std::uint32_t unknown)
    -> svLogic {
  return static_cast<svLogic>(
      (value << lyra::value::kScalarValueBit) |
      (unknown << lyra::value::kScalarUnknownBit));
}
constexpr auto ValueBitOf(svLogic scalar) -> std::uint32_t {
  return (scalar >> lyra::value::kScalarValueBit) & 1U;
}
constexpr auto UnknownBitOf(svLogic scalar) -> std::uint32_t {
  return (scalar >> lyra::value::kScalarUnknownBit) & 1U;
}

}  // namespace

// The utilities for the canonical representation (LRM Annex H.11.5) and the
// version that names it (Annex H.10.1.3), linked into the simulation binary and
// resolved against the user's C by name. A vector is numbered from 0 at its
// least significant bit; a part-select is up to 32 bits and is read into the
// low bits of its destination group, which keeps the bits above it, or written
// from the low bits of its source group into the selection, keeping the bits
// around it. A selection the vector does not hold is undetermined by the
// standard, and nothing here reads a group the selection does not reach.
extern "C" {

auto svDpiVersion() -> const char* {
  return "1800-2005";
}

auto svGetBitselBit(const svBitVecVal* s, int i) -> svBit {
  return static_cast<svBit>(ExtractBits(s, WindowAt(i, 1), kBits));
}

auto svGetBitselLogic(const svLogicVecVal* s, int i) -> svLogic {
  return SvLogicOf(
      ExtractBits(s, WindowAt(i, 1), kValuePlane),
      ExtractBits(s, WindowAt(i, 1), kUnknownPlane));
}

void svPutBitselBit(svBitVecVal* d, int i, svBit s) {
  InsertBits(d, WindowAt(i, 1), s, kBits);
}

void svPutBitselLogic(svLogicVecVal* d, int i, svLogic s) {
  InsertBits(d, WindowAt(i, 1), ValueBitOf(s), kValuePlane);
  InsertBits(d, WindowAt(i, 1), UnknownBitOf(s), kUnknownPlane);
}

void svGetPartselBit(svBitVecVal* d, const svBitVecVal* s, int i, int w) {
  InsertBits(d, WindowAt(0, w), ExtractBits(s, WindowAt(i, w), kBits), kBits);
}

void svGetPartselLogic(svLogicVecVal* d, const svLogicVecVal* s, int i, int w) {
  InsertBits(
      d, WindowAt(0, w), ExtractBits(s, WindowAt(i, w), kValuePlane),
      kValuePlane);
  InsertBits(
      d, WindowAt(0, w), ExtractBits(s, WindowAt(i, w), kUnknownPlane),
      kUnknownPlane);
}

void svPutPartselBit(svBitVecVal* d, const svBitVecVal s, int i, int w) {
  InsertBits(d, WindowAt(i, w), s, kBits);
}

void svPutPartselLogic(svLogicVecVal* d, const svLogicVecVal s, int i, int w) {
  InsertBits(d, WindowAt(i, w), s.aval, kValuePlane);
  InsertBits(d, WindowAt(i, w), s.bval, kUnknownPlane);
}

}  // extern "C"
