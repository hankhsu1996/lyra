#include "lyra/value/dpi_open_array.hpp"

#include <array>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <type_traits>
#include <variant>
#include <vector>

#include "lyra/value/dpi_canonical.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::value {

void DpiOpenArray::Shape(
    std::span<const PackedArray> bounds, const PackedArray& leaf,
    bool addressable_elements) {
  addressable_elements_ = addressable_elements;
  element_width_ = static_cast<std::uint32_t>(leaf.BitWidth());
  const std::size_t dimensions = bounds.size() / 2;
  dims_.reserve(dimensions);
  for (std::size_t d = 0; d < dimensions; ++d) {
    dims_.push_back(
        UnpackedRange{
            .left = bounds[2 * d].ToInt64(),
            .right = bounds[(2 * d) + 1].ToInt64()});
  }
  const std::size_t words = ElementCount() * GroupsPerElement();
  if (leaf.IsFourState()) {
    storage_ = std::vector<svLogicVecVal>(words);
  } else {
    storage_ = std::vector<svBitVecVal>(words);
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

void DpiOpenArray::WriteLeaf(const PackedArray& value, std::size_t position) {
  const std::span<svBitVecVal> bits = GroupsAt<svBitVecVal>(position);
  if (!bits.empty()) {
    WriteCanonicalBitVec(bits.data(), value);
    return;
  }
  WriteCanonicalLogicVec(GroupsAt<svLogicVecVal>(position).data(), value);
}

auto DpiOpenArray::ReadLeaf(
    const PackedArray& prototype, std::size_t position) const -> PackedArray {
  const std::span<const svBitVecVal> bits = GroupsAt<svBitVecVal>(position);
  if (!bits.empty()) {
    return ReadCanonicalBitVec(bits.data(), prototype.Type());
  }
  return ReadCanonicalLogicVec(
      GroupsAt<svLogicVecVal>(position).data(), prototype.Type());
}

}  // namespace lyra::value

namespace {

using lyra::value::DpiOpenArray;

[[nodiscard]] auto Array(void* handle) -> DpiOpenArray* {
  return static_cast<DpiOpenArray*>(handle);
}

// The declared bounds a query reports, or the empty range for a null handle --
// the svdpi surface answers rather than faults.
[[nodiscard]] auto Bounds(void* handle, int dimension)
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
[[nodiscard]] auto ElementOf(void* handle, Indices... indices)
    -> std::span<Word> {
  const std::array<int, sizeof...(Indices)> at{indices...};
  return handle == nullptr ? std::span<Word>{}
                           : Array(handle)->Element<Word>(at);
}

template <typename... Indices>
[[nodiscard]] auto AddressOf(void* handle, Indices... indices) -> void* {
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

// A one-bit element's `svBit` / `svLogic` scalar encoding (Annex H.10.1.1):
// `value | unknown << 1`, read from and written to the element's first group.
[[nodiscard]] auto ScalarBitOf(std::span<svBitVecVal> element)
    -> unsigned char {
  return element.empty() ? 0U
                         : static_cast<unsigned char>(element.front() & 1U);
}

[[nodiscard]] auto ScalarLogicOf(std::span<svLogicVecVal> element)
    -> unsigned char {
  if (element.empty()) {
    return 0U;
  }
  const svLogicVecVal& group = element.front();
  return static_cast<unsigned char>(
      (group.aval & 1U) | ((group.bval & 1U) << 1U));
}

void PutScalarBit(std::span<svBitVecVal> element, unsigned char value) {
  if (!element.empty()) {
    element.front() = value & 1U;
  }
}

void PutScalarLogic(std::span<svLogicVecVal> element, unsigned char value) {
  if (!element.empty()) {
    element.front().aval = value & 1U;
    element.front().bval = (value >> 1U) & 1U;
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

auto svDimensions(void* handle) -> int {
  return handle == nullptr ? 0 : Array(handle)->Dimensions();
}

auto svLeft(void* handle, int d) -> int {
  return static_cast<int>(Bounds(handle, d).left);
}

auto svRight(void* handle, int d) -> int {
  return static_cast<int>(Bounds(handle, d).right);
}

auto svLow(void* handle, int d) -> int {
  return static_cast<int>(Bounds(handle, d).Low());
}

auto svHigh(void* handle, int d) -> int {
  const lyra::value::UnpackedRange range = Bounds(handle, d);
  return static_cast<int>(range.IsAscending() ? range.right : range.left);
}

auto svIncrement(void* handle, int d) -> int {
  return Bounds(handle, d).IsAscending() ? -1 : 1;
}

auto svSize(void* handle, int d) -> int {
  return static_cast<int>(Bounds(handle, d).Count());
}

auto svGetArrayPtr(void* handle) -> void* {
  return handle == nullptr ? nullptr : Array(handle)->Image();
}

auto svSizeOfArray(void* handle) -> int {
  return handle == nullptr
             ? 0
             : static_cast<int>(Array(handle)->ImageSizeInBytes());
}

auto svGetArrElemPtr1(void* handle, int i1) -> void* {
  return AddressOf(handle, i1);
}

auto svGetArrElemPtr2(void* handle, int i1, int i2) -> void* {
  return AddressOf(handle, i1, i2);
}

auto svGetArrElemPtr3(void* handle, int i1, int i2, int i3) -> void* {
  return AddressOf(handle, i1, i2, i3);
}

auto svGetBitArrElem1VecVal(svBitVecVal* d, void* handle, int i1) -> void {
  CopyOut(ElementOf<svBitVecVal>(handle, i1), d);
}

auto svGetBitArrElem2VecVal(svBitVecVal* d, void* handle, int i1, int i2)
    -> void {
  CopyOut(ElementOf<svBitVecVal>(handle, i1, i2), d);
}

auto svGetBitArrElem3VecVal(
    svBitVecVal* d, void* handle, int i1, int i2, int i3) -> void {
  CopyOut(ElementOf<svBitVecVal>(handle, i1, i2, i3), d);
}

auto svGetLogicArrElem1VecVal(svLogicVecVal* d, void* handle, int i1) -> void {
  CopyOut(ElementOf<svLogicVecVal>(handle, i1), d);
}

auto svGetLogicArrElem2VecVal(svLogicVecVal* d, void* handle, int i1, int i2)
    -> void {
  CopyOut(ElementOf<svLogicVecVal>(handle, i1, i2), d);
}

auto svGetLogicArrElem3VecVal(
    svLogicVecVal* d, void* handle, int i1, int i2, int i3) -> void {
  CopyOut(ElementOf<svLogicVecVal>(handle, i1, i2, i3), d);
}

auto svPutBitArrElem1VecVal(void* handle, const svBitVecVal* s, int i1)
    -> void {
  CopyIn(ElementOf<svBitVecVal>(handle, i1), s);
}

auto svPutBitArrElem2VecVal(void* handle, const svBitVecVal* s, int i1, int i2)
    -> void {
  CopyIn(ElementOf<svBitVecVal>(handle, i1, i2), s);
}

auto svPutBitArrElem3VecVal(
    void* handle, const svBitVecVal* s, int i1, int i2, int i3) -> void {
  CopyIn(ElementOf<svBitVecVal>(handle, i1, i2, i3), s);
}

auto svPutLogicArrElem1VecVal(void* handle, const svLogicVecVal* s, int i1)
    -> void {
  CopyIn(ElementOf<svLogicVecVal>(handle, i1), s);
}

auto svPutLogicArrElem2VecVal(
    void* handle, const svLogicVecVal* s, int i1, int i2) -> void {
  CopyIn(ElementOf<svLogicVecVal>(handle, i1, i2), s);
}

auto svPutLogicArrElem3VecVal(
    void* handle, const svLogicVecVal* s, int i1, int i2, int i3) -> void {
  CopyIn(ElementOf<svLogicVecVal>(handle, i1, i2, i3), s);
}

auto svGetBitArrElem1(void* handle, int i1) -> unsigned char {
  return ScalarBitOf(ElementOf<svBitVecVal>(handle, i1));
}

auto svGetBitArrElem2(void* handle, int i1, int i2) -> unsigned char {
  return ScalarBitOf(ElementOf<svBitVecVal>(handle, i1, i2));
}

auto svGetBitArrElem3(void* handle, int i1, int i2, int i3) -> unsigned char {
  return ScalarBitOf(ElementOf<svBitVecVal>(handle, i1, i2, i3));
}

auto svGetLogicArrElem1(void* handle, int i1) -> unsigned char {
  return ScalarLogicOf(ElementOf<svLogicVecVal>(handle, i1));
}

auto svGetLogicArrElem2(void* handle, int i1, int i2) -> unsigned char {
  return ScalarLogicOf(ElementOf<svLogicVecVal>(handle, i1, i2));
}

auto svGetLogicArrElem3(void* handle, int i1, int i2, int i3) -> unsigned char {
  return ScalarLogicOf(ElementOf<svLogicVecVal>(handle, i1, i2, i3));
}

auto svPutBitArrElem1(void* handle, unsigned char value, int i1) -> void {
  PutScalarBit(ElementOf<svBitVecVal>(handle, i1), value);
}

auto svPutBitArrElem2(void* handle, unsigned char value, int i1, int i2)
    -> void {
  PutScalarBit(ElementOf<svBitVecVal>(handle, i1, i2), value);
}

auto svPutBitArrElem3(void* handle, unsigned char value, int i1, int i2, int i3)
    -> void {
  PutScalarBit(ElementOf<svBitVecVal>(handle, i1, i2, i3), value);
}

auto svPutLogicArrElem1(void* handle, unsigned char value, int i1) -> void {
  PutScalarLogic(ElementOf<svLogicVecVal>(handle, i1), value);
}

auto svPutLogicArrElem2(void* handle, unsigned char value, int i1, int i2)
    -> void {
  PutScalarLogic(ElementOf<svLogicVecVal>(handle, i1, i2), value);
}

auto svPutLogicArrElem3(
    void* handle, unsigned char value, int i1, int i2, int i3) -> void {
  PutScalarLogic(ElementOf<svLogicVecVal>(handle, i1, i2, i3), value);
}

}  // extern "C"
