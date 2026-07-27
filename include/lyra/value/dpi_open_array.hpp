#pragma once

#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/value/dpi_canonical.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/unpacked_array.hpp"

namespace lyra::value {

namespace detail {

// The leftmost leaf of a nest of unpacked layers, whose declared width and
// state domain every leaf of the nest shares.
[[nodiscard]] inline auto FirstLeaf(const PackedArray& value)
    -> const PackedArray& {
  return value;
}
template <typename T>
[[nodiscard]] auto FirstLeaf(const UnpackedArray<T>& value)
    -> const PackedArray& {
  return FirstLeaf(value.RawAt(0));
}

}  // namespace detail

// A DPI-C open array as the foreign side sees it (LRM 35.5.6.1, Annex H.12): a
// canonical image of the whole actual, plus the coordinate system of each
// dimension. It is an ABI temporary of one foreign call -- built from the SV
// value before the call, read back into an SV value after -- and owns its
// storage, so nothing here aliases the actual and no foreign write reaches SV
// storage directly.
//
// The image is a sequence of elements, each in the canonical form of its
// declared width (Annex H.7.7), so its storage element type is the 32-bit group
// of the array's own state domain. Elements run in ascending declared-index
// order per dimension, the C layout order Annex H.7.3 fixes, which reverses
// relative to SV element order for a descending declared range.
//
// Dimension 0 is the sole packed dimension, always normalized to `[width-1:0]`;
// dimensions 1 and up are the unpacked ones, each reporting the declared range
// the call site supplied (Annex H.7.5, H.7.6).
class DpiOpenArray {
 public:
  // `bounds` is the declared `(left, right)` pair of each unpacked dimension,
  // outermost first -- empty where the actual is a single packed value.
  // `addressable_elements` says an individual value of the element type crosses
  // in the same canonical form the image holds it in, which is what lets the
  // foreign side take the address of the array or of one element (Annex
  // H.12.4).
  template <typename T>
  DpiOpenArray(
      const T& sv, std::span<const PackedArray> bounds,
      bool addressable_elements) {
    Shape(bounds, detail::FirstLeaf(sv), addressable_elements);
    std::size_t position = 0;
    Fill(sv, 0, position);
  }

  // The SV value the image now holds, shaped like `prototype` -- the write-back
  // of an `output` or `inout` open array. Reading through a prototype is what
  // gives each element its declared width, signedness, and state domain, since
  // the canonical form carries none of them.
  template <typename T>
  [[nodiscard]] auto ToValue(const T& prototype) const -> T {
    std::size_t position = 0;
    return Rebuild(prototype, 0, position);
  }

  [[nodiscard]] auto Handle() -> svOpenArrayHandle {
    return this;
  }

  // The number of unpacked dimensions; the packed dimension is dimension 0 and
  // is not counted (Annex H.12.2).
  [[nodiscard]] auto Dimensions() const -> int {
    return static_cast<int>(dims_.size());
  }

  // The declared bounds of one dimension, or the empty range for a dimension
  // the array does not have -- from which every query derives a zero size and a
  // zero bound, so no query carries an out-of-range arm of its own.
  [[nodiscard]] auto Bounds(int dimension) const -> UnpackedRange;

  // The address and byte size of the image as one array. Both answer with
  // nothing where an element's canonical form is not how an individual value of
  // its type crosses, which is the null address Annex H.12.4 prescribes; they
  // travel together so neither caller re-checks the condition.
  [[nodiscard]] auto Image() -> void*;
  [[nodiscard]] auto ImageSizeInBytes() const -> std::size_t;

  // The address of one element, or null under the condition above or for
  // indices this array has no element at.
  [[nodiscard]] auto ElementAddress(std::span<const int> indices) -> void*;

  // The canonical groups of the element at the given declared indices, one
  // index per unpacked dimension and outermost first, in the state domain the
  // caller asked for. Empty for an index outside its dimension, an index count
  // the array does not have, or a state domain that is not the image's -- each
  // a call the foreign side made against a shape this array does not have.
  template <typename Word>
  [[nodiscard]] auto Element(std::span<const int> indices) -> std::span<Word> {
    const std::optional<std::size_t> position = PositionOf(indices);
    return position.has_value() ? GroupsAt<Word>(*position) : std::span<Word>{};
  }

 private:
  // Fixes the coordinate system, the element shape, and the storage the image
  // needs, all of which follow from the bounds and one leaf.
  void Shape(
      std::span<const PackedArray> bounds, const PackedArray& leaf,
      bool addressable_elements);

  // The 32-bit groups one element occupies in canonical form (Annex H.7.7).
  [[nodiscard]] auto GroupsPerElement() const -> std::size_t {
    return (static_cast<std::size_t>(element_width_) + 31U) / 32U;
  }

  [[nodiscard]] auto ElementCount() const -> std::size_t;

  // The image position of the element at the given declared indices, or nothing
  // where the index count or any index does not fit this array.
  [[nodiscard]] auto PositionOf(std::span<const int> indices) const
      -> std::optional<std::size_t>;

  // The address of the element at an image position, or null where the foreign
  // side may not address elements at all.
  [[nodiscard]] auto AddressAt(std::size_t position) -> void*;

  // The storage ordinal of the element at C position `position` in a dimension:
  // the two agree while the declared range ascends and mirror each other while
  // it descends, since SV element order runs from the left bound (LRM 7.6) and
  // the C layout runs from the lowest index (Annex H.7.3).
  [[nodiscard]] auto OrdinalAt(
      std::size_t dimension, std::size_t position) const -> std::size_t;

  // The one place an image position becomes storage. Empty where the image
  // holds the other state domain, so a caller that asked for the wrong word
  // type gets the same non-answer as a caller that asked for a missing element.
  template <typename Word>
  [[nodiscard]] auto GroupsAt(std::size_t position) -> std::span<Word> {
    auto* words = std::get_if<std::vector<Word>>(&storage_);
    if (words == nullptr) {
      return {};
    }
    const std::size_t groups = GroupsPerElement();
    return std::span{*words}.subspan(position * groups, groups);
  }
  template <typename Word>
  [[nodiscard]] auto GroupsAt(std::size_t position) const
      -> std::span<const Word> {
    const auto* words = std::get_if<std::vector<Word>>(&storage_);
    if (words == nullptr) {
      return {};
    }
    const std::size_t groups = GroupsPerElement();
    return std::span{*words}.subspan(position * groups, groups);
  }

  // One element's canonical groups at an image position, in whichever state
  // domain the image holds.
  void WriteLeaf(const PackedArray& value, std::size_t position);
  [[nodiscard]] auto ReadLeaf(
      const PackedArray& prototype, std::size_t position) const -> PackedArray;

  // Walks a value against the dimensions, descending one layer per dimension
  // and visiting the leaves in C layout order so `position` advances with the
  // image. A leaf ends the walk, so the innermost layer consumes no dimension.
  template <typename T>
  void Fill(const T& value, std::size_t dimension, std::size_t& position) {
    if constexpr (std::is_same_v<T, PackedArray>) {
      WriteLeaf(value, position);
      ++position;
    } else {
      for (std::size_t p = 0; p < value.RawSize(); ++p) {
        Fill(value.RawAt(OrdinalAt(dimension, p)), dimension + 1, position);
      }
    }
  }

  template <typename T>
  [[nodiscard]] auto Rebuild(
      const T& prototype, std::size_t dimension, std::size_t& position) const
      -> T {
    if constexpr (std::is_same_v<T, PackedArray>) {
      const PackedArray value = ReadLeaf(prototype, position);
      ++position;
      return value;
    } else {
      const std::size_t count = prototype.RawSize();
      std::vector<typename T::ElementType> elements(count);
      for (std::size_t p = 0; p < count; ++p) {
        const std::size_t ordinal = OrdinalAt(dimension, p);
        elements[ordinal] =
            Rebuild(prototype.RawAt(ordinal), dimension + 1, position);
      }
      typename T::ElementType element_default = prototype.RawAt(0);
      element_default.ResetToDefault();
      return T(std::move(element_default), elements);
    }
  }

  std::variant<std::vector<svBitVecVal>, std::vector<svLogicVecVal>> storage_;
  std::vector<UnpackedRange> dims_;
  std::uint32_t element_width_ = 0;
  bool addressable_elements_ = false;
};

}  // namespace lyra::value
