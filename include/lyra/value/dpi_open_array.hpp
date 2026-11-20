#pragma once

#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <variant>
#include <vector>

#include "lyra/value/dpi_canonical.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/unpacked_range.hpp"
#include "svdpi.h"

namespace lyra::value {

// A DPI-C open array as the foreign side sees it (LRM 35.5.6.1, Annex H.12): a
// canonical image of the whole actual, plus the coordinate system of each
// dimension. It is an ABI temporary of one foreign call -- built clear and
// filled from the SV value by the actual's own code before the call, and read
// back into an SV value the same way after -- and owns its storage, so nothing
// here aliases the actual and no foreign write reaches SV storage directly.
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
  // An image with every element clear, each element `element_width` bits in
  // the canonical form of the four-state domain where `four_state` says and of
  // the two-state one otherwise. `bounds` states the declared range
  // of each unpacked dimension, left bound then right, outermost first -- empty
  // where the actual is a single packed value. `addressable_elements` says an
  // individual value of the element type crosses in the same canonical form
  // the image holds it in, which is what lets the foreign side take the
  // address of the array or of one element (Annex H.12.4). That is a fact of
  // the formal's declaration and not of the actual's type: a formal whose
  // packed dimension is unsized makes every element cross as a canonical
  // vector whatever the element is.
  DpiOpenArray(
      std::span<const std::int64_t> bounds, std::uint64_t element_width,
      bool four_state, bool addressable_elements);

  // The image's elements in its own order, as the actual's own code walks
  // them to fill the image and to read it back: how many there are, the
  // storage ordinal in the actual's unpacked dimension `dimension` that the
  // element `k` lies at, and the element `k` written from a value as wide as
  // an element and read back into one (Annex H.7.3 gives a packed element one
  // canonical representation, a flat vector of its own width).
  [[nodiscard]] auto ElementCount() const -> std::int64_t;
  [[nodiscard]] auto Ordinal(std::int64_t k, std::int64_t dimension) const
      -> std::int64_t;
  void Put(std::int64_t k, const ConstIntegralView& value);
  void Word(std::int64_t k, const IntegralView& out) const;
  template <IntegralValue T>
  void Put(std::int64_t k, const T& value) {
    Put(k, value.AsWords().View());
  }
  template <IntegralValue T>
  [[nodiscard]] auto Word(std::int64_t k) const -> T {
    T read = T::Zero();
    Word(k, read.AsMutableWords().MutableView());
    return read;
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
  [[nodiscard]] auto GroupsPerElement() const -> std::size_t {
    return CanonicalGroups(element_width_);
  }

  // The storage ordinal of the element at C position `position` in a dimension:
  // the two agree while the declared range ascends and mirror each other while
  // it descends, since SV element order runs from the left bound (LRM 7.6) and
  // the C layout runs from the lowest index (Annex H.7.3).
  [[nodiscard]] auto OrdinalAt(
      std::size_t dimension, std::size_t position) const -> std::size_t;

  // The image position of the element at the given declared indices, or nothing
  // where the index count or any index does not fit this array.
  [[nodiscard]] auto PositionOf(std::span<const int> indices) const
      -> std::optional<std::size_t>;

  // The address of the element at an image position, or null where the foreign
  // side may not address elements at all.
  [[nodiscard]] auto AddressAt(std::size_t position) -> void*;

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

  std::variant<std::vector<svBitVecVal>, std::vector<svLogicVecVal>> storage_;
  std::vector<UnpackedRange> dims_;
  std::uint32_t element_width_ = 0;
  bool addressable_elements_ = false;
};

}  // namespace lyra::value
