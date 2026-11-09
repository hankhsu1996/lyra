#include "lyra/value/dpi_open_array.hpp"

#include <array>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <type_traits>
#include <variant>
#include <vector>

#include "lyra/base/overloaded.hpp"
#include "lyra/value/dpi_canonical.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"
#include "svdpi.h"

namespace lyra::value {

DpiOpenArray::DpiOpenArray(
    std::span<const std::int64_t> bounds, std::uint64_t element_width,
    StateDomain element_domain, bool addressable_elements)
    : dims_(UnpackedRangesOf(bounds)),
      element_width_(static_cast<std::uint32_t>(element_width)),
      addressable_elements_(addressable_elements) {
  const std::size_t groups = ElementCount() * GroupsPerElement();
  switch (element_domain) {
    case StateDomain::kTwoState:
      storage_ = std::vector<svBitVecVal>(groups);
      break;
    case StateDomain::kFourState:
      storage_ = std::vector<svLogicVecVal>(groups);
      break;
  }
}

auto DpiOpenArray::ElementCount() const -> std::size_t {
  std::size_t count = 1;
  for (const UnpackedRange& dim : dims_) {
    count *= dim.Count();
  }
  return count;
}

auto DpiOpenArray::Bounds(int dimension) const -> UnpackedRange {
  if (dimension == 0) {
    return UnpackedRange{
        .left = static_cast<std::int64_t>(element_width_) - 1, .right = 0};
  }
  if (dimension < 0 || static_cast<std::size_t>(dimension) > dims_.size()) {
    return UnpackedRange{.left = 0, .right = -1};
  }
  return dims_[static_cast<std::size_t>(dimension) - 1];
}

auto DpiOpenArray::OrdinalAt(std::size_t dimension, std::size_t position) const
    -> std::size_t {
  const UnpackedRange& range = dims_[dimension];
  return range.IsAscending() ? position : range.Count() - 1U - position;
}

auto DpiOpenArray::PositionOf(std::span<const int> indices) const
    -> std::optional<std::size_t> {
  if (indices.size() != dims_.size()) {
    return std::nullopt;
  }
  std::size_t position = 0;
  for (std::size_t d = 0; d < dims_.size(); ++d) {
    const UnpackedRange& range = dims_[d];
    const std::int64_t offset = std::int64_t{indices[d]} - range.Low();
    if (offset < 0 || static_cast<std::size_t>(offset) >= range.Count()) {
      return std::nullopt;
    }
    position = (position * range.Count()) + static_cast<std::size_t>(offset);
  }
  return position;
}

auto DpiOpenArray::AddressAt(std::size_t position) -> void* {
  if (!addressable_elements_) {
    return nullptr;
  }
  const std::size_t groups = GroupsPerElement();
  return std::visit(
      [&](auto& words) -> void* {
        return std::span{words}.subspan(position * groups, groups).data();
      },
      storage_);
}

auto DpiOpenArray::Image() -> void* {
  return AddressAt(0);
}

auto DpiOpenArray::ImageSizeInBytes() const -> std::size_t {
  if (!addressable_elements_) {
    return 0;
  }
  return std::visit(
      [](const auto& words) -> std::size_t {
        using Word = typename std::remove_cvref_t<decltype(words)>::value_type;
        return words.size() * sizeof(Word);
      },
      storage_);
}

auto DpiOpenArray::ElementAddress(std::span<const int> indices) -> void* {
  const std::optional<std::size_t> position = PositionOf(indices);
  return position.has_value() ? AddressAt(*position) : nullptr;
}

void DpiOpenArray::WriteElement(
    std::size_t position, const ConstIntegralView& value) {
  const std::size_t groups = GroupsPerElement();
  std::visit(
      Overloaded{
          [&](std::vector<svBitVecVal>& image) {
            WriteCanonicalBitVec(
                std::span{image}.subspan(position * groups, groups).data(),
                value);
          },
          [&](std::vector<svLogicVecVal>& image) {
            WriteCanonicalLogicVec(
                std::span{image}.subspan(position * groups, groups).data(),
                value);
          }},
      storage_);
}

void DpiOpenArray::ReadElement(
    std::size_t position, const IntegralView& out) const {
  const std::size_t groups = GroupsPerElement();
  std::visit(
      Overloaded{
          [&](const std::vector<svBitVecVal>& image) {
            ReadCanonicalBitVec(
                std::span{image}.subspan(position * groups, groups).data(),
                out);
          },
          [&](const std::vector<svLogicVecVal>& image) {
            ReadCanonicalLogicVec(
                std::span{image}.subspan(position * groups, groups).data(),
                out);
          }},
      storage_);
}

}  // namespace lyra::value

namespace {

using lyra::value::DpiOpenArray;

[[nodiscard]] auto Array(svOpenArrayHandle handle) -> DpiOpenArray* {
  return static_cast<DpiOpenArray*>(handle);
}

// The declared bounds a query reports, or the empty range for a null handle --
// the svdpi surface answers rather than faults.
[[nodiscard]] auto Bounds(svOpenArrayHandle handle, int dimension)
    -> lyra::value::UnpackedRange {
  if (handle == nullptr) {
    return lyra::value::UnpackedRange{.left = 0, .right = -1};
  }
  return Array(handle)->Bounds(dimension);
}

// The element the foreign side named, in the word type the calling entry works
// in. The entries differ only in how many indices they name and which word type
// they carry, so each resolves its element through here and then does its own
// one operation on the result. An empty result is every way the named element
// can fail to exist -- a null handle, an index outside its dimension, the wrong
// index count, the other state domain -- so no entry tests those separately.
template <typename Word, typename... Indices>
[[nodiscard]] auto ElementOf(svOpenArrayHandle handle, Indices... indices)
    -> std::span<Word> {
  const std::array<int, sizeof...(Indices)> at{indices...};
  return handle == nullptr ? std::span<Word>{}
                           : Array(handle)->Element<Word>(at);
}

template <typename... Indices>
[[nodiscard]] auto AddressOf(svOpenArrayHandle handle, Indices... indices)
    -> void* {
  const std::array<int, sizeof...(Indices)> at{indices...};
  return handle == nullptr ? nullptr : Array(handle)->ElementAddress(at);
}

// Copies one element's canonical groups between the image and a caller buffer.
// An element that does not exist leaves the destination untouched, the same
// non-answer the addressing entries give as a null.
template <typename Word>
void CopyOut(std::span<Word> source, Word* destination) {
  if (source.empty() || destination == nullptr) {
    return;
  }
  const std::span<Word> out{destination, source.size()};
  for (std::size_t i = 0; i < source.size(); ++i) {
    out[i] = source[i];
  }
}

template <typename Word>
void CopyIn(std::span<Word> destination, const Word* source) {
  if (destination.empty() || source == nullptr) {
    return;
  }
  const std::span<const Word> in{source, destination.size()};
  for (std::size_t i = 0; i < destination.size(); ++i) {
    destination[i] = in[i];
  }
}

// A one-bit element's `svBit` / `svLogic` scalar is the select of its bit 0
// (Annex H.10.1.1, H.11.5).
[[nodiscard]] auto ScalarBitOf(std::span<svBitVecVal> element) -> svBit {
  return element.empty() ? 0U : svGetBitselBit(element.data(), 0);
}

[[nodiscard]] auto ScalarLogicOf(std::span<svLogicVecVal> element) -> svLogic {
  return element.empty() ? 0U : svGetBitselLogic(element.data(), 0);
}

void PutScalarBit(std::span<svBitVecVal> element, svBit value) {
  if (!element.empty()) {
    svPutBitselBit(element.data(), 0, value);
  }
}

void PutScalarLogic(std::span<svLogicVecVal> element, svLogic value) {
  if (!element.empty()) {
    svPutBitselLogic(element.data(), 0, value);
  }
}

}  // namespace

// The Annex H.12 open-array surface, linked into the simulation binary and
// resolved against the user's C by name. A handle is the canonical image the
// call site built from the actual; every entry takes the declared indices the
// SV source uses and resolves them against the image's own dimensions. Errors
// follow the svdpi contract -- a null handle, a dimension the array does not
// have, or an index outside its range yields a zero, a null, or an untouched
// destination rather than a fault -- and never throw across the C boundary.
//
// The indexing entries are the one-, two-, and three-index forms. Annex H.12.3
// also defines a variable-argument form of each for an array of arbitrarily
// many dimensions; a design that would need one is rejected at its declaration
// instead, so no such entry is published here.
extern "C" {

auto svDimensions(const svOpenArrayHandle h) -> int {
  return h == nullptr ? 0 : Array(h)->Dimensions();
}

auto svLeft(const svOpenArrayHandle h, int d) -> int {
  return static_cast<int>(Bounds(h, d).left);
}

auto svRight(const svOpenArrayHandle h, int d) -> int {
  return static_cast<int>(Bounds(h, d).right);
}

auto svLow(const svOpenArrayHandle h, int d) -> int {
  return static_cast<int>(Bounds(h, d).Low());
}

auto svHigh(const svOpenArrayHandle h, int d) -> int {
  return static_cast<int>(Bounds(h, d).High());
}

auto svIncrement(const svOpenArrayHandle h, int d) -> int {
  return Bounds(h, d).IsAscending() ? -1 : 1;
}

auto svSize(const svOpenArrayHandle h, int d) -> int {
  return static_cast<int>(Bounds(h, d).Count());
}

auto svGetArrayPtr(const svOpenArrayHandle h) -> void* {
  return h == nullptr ? nullptr : Array(h)->Image();
}

auto svSizeOfArray(const svOpenArrayHandle h) -> int {
  return h == nullptr ? 0 : static_cast<int>(Array(h)->ImageSizeInBytes());
}

auto svGetArrElemPtr1(const svOpenArrayHandle h, int indx1) -> void* {
  return AddressOf(h, indx1);
}

auto svGetArrElemPtr2(const svOpenArrayHandle h, int indx1, int indx2)
    -> void* {
  return AddressOf(h, indx1, indx2);
}

auto svGetArrElemPtr3(
    const svOpenArrayHandle h, int indx1, int indx2, int indx3) -> void* {
  return AddressOf(h, indx1, indx2, indx3);
}

void svGetBitArrElem1VecVal(
    svBitVecVal* d, const svOpenArrayHandle s, int indx1) {
  CopyOut(ElementOf<svBitVecVal>(s, indx1), d);
}

void svGetBitArrElem2VecVal(
    svBitVecVal* d, const svOpenArrayHandle s, int indx1, int indx2) {
  CopyOut(ElementOf<svBitVecVal>(s, indx1, indx2), d);
}

void svGetBitArrElem3VecVal(
    svBitVecVal* d, const svOpenArrayHandle s, int indx1, int indx2,
    int indx3) {
  CopyOut(ElementOf<svBitVecVal>(s, indx1, indx2, indx3), d);
}

void svGetLogicArrElem1VecVal(
    svLogicVecVal* d, const svOpenArrayHandle s, int indx1) {
  CopyOut(ElementOf<svLogicVecVal>(s, indx1), d);
}

void svGetLogicArrElem2VecVal(
    svLogicVecVal* d, const svOpenArrayHandle s, int indx1, int indx2) {
  CopyOut(ElementOf<svLogicVecVal>(s, indx1, indx2), d);
}

void svGetLogicArrElem3VecVal(
    svLogicVecVal* d, const svOpenArrayHandle s, int indx1, int indx2,
    int indx3) {
  CopyOut(ElementOf<svLogicVecVal>(s, indx1, indx2, indx3), d);
}

void svPutBitArrElem1VecVal(
    const svOpenArrayHandle d, const svBitVecVal* s, int indx1) {
  CopyIn(ElementOf<svBitVecVal>(d, indx1), s);
}

void svPutBitArrElem2VecVal(
    const svOpenArrayHandle d, const svBitVecVal* s, int indx1, int indx2) {
  CopyIn(ElementOf<svBitVecVal>(d, indx1, indx2), s);
}

void svPutBitArrElem3VecVal(
    const svOpenArrayHandle d, const svBitVecVal* s, int indx1, int indx2,
    int indx3) {
  CopyIn(ElementOf<svBitVecVal>(d, indx1, indx2, indx3), s);
}

void svPutLogicArrElem1VecVal(
    const svOpenArrayHandle d, const svLogicVecVal* s, int indx1) {
  CopyIn(ElementOf<svLogicVecVal>(d, indx1), s);
}

void svPutLogicArrElem2VecVal(
    const svOpenArrayHandle d, const svLogicVecVal* s, int indx1, int indx2) {
  CopyIn(ElementOf<svLogicVecVal>(d, indx1, indx2), s);
}

void svPutLogicArrElem3VecVal(
    const svOpenArrayHandle d, const svLogicVecVal* s, int indx1, int indx2,
    int indx3) {
  CopyIn(ElementOf<svLogicVecVal>(d, indx1, indx2, indx3), s);
}

auto svGetBitArrElem1(const svOpenArrayHandle s, int indx1) -> svBit {
  return ScalarBitOf(ElementOf<svBitVecVal>(s, indx1));
}

auto svGetBitArrElem2(const svOpenArrayHandle s, int indx1, int indx2)
    -> svBit {
  return ScalarBitOf(ElementOf<svBitVecVal>(s, indx1, indx2));
}

auto svGetBitArrElem3(
    const svOpenArrayHandle s, int indx1, int indx2, int indx3) -> svBit {
  return ScalarBitOf(ElementOf<svBitVecVal>(s, indx1, indx2, indx3));
}

auto svGetLogicArrElem1(const svOpenArrayHandle s, int indx1) -> svLogic {
  return ScalarLogicOf(ElementOf<svLogicVecVal>(s, indx1));
}

auto svGetLogicArrElem2(const svOpenArrayHandle s, int indx1, int indx2)
    -> svLogic {
  return ScalarLogicOf(ElementOf<svLogicVecVal>(s, indx1, indx2));
}

auto svGetLogicArrElem3(
    const svOpenArrayHandle s, int indx1, int indx2, int indx3) -> svLogic {
  return ScalarLogicOf(ElementOf<svLogicVecVal>(s, indx1, indx2, indx3));
}

void svPutBitArrElem1(const svOpenArrayHandle d, svBit value, int indx1) {
  PutScalarBit(ElementOf<svBitVecVal>(d, indx1), value);
}

void svPutBitArrElem2(
    const svOpenArrayHandle d, svBit value, int indx1, int indx2) {
  PutScalarBit(ElementOf<svBitVecVal>(d, indx1, indx2), value);
}

void svPutBitArrElem3(
    const svOpenArrayHandle d, svBit value, int indx1, int indx2, int indx3) {
  PutScalarBit(ElementOf<svBitVecVal>(d, indx1, indx2, indx3), value);
}

void svPutLogicArrElem1(const svOpenArrayHandle d, svLogic value, int indx1) {
  PutScalarLogic(ElementOf<svLogicVecVal>(d, indx1), value);
}

void svPutLogicArrElem2(
    const svOpenArrayHandle d, svLogic value, int indx1, int indx2) {
  PutScalarLogic(ElementOf<svLogicVecVal>(d, indx1, indx2), value);
}

void svPutLogicArrElem3(
    const svOpenArrayHandle d, svLogic value, int indx1, int indx2, int indx3) {
  PutScalarLogic(ElementOf<svLogicVecVal>(d, indx1, indx2, indx3), value);
}

}  // extern "C"
