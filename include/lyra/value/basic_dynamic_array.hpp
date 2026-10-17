#pragma once

#include <algorithm>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <new>
#include <optional>
#include <span>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/value/element_policy.hpp"
#include "lyra/value/element_sequence.hpp"
#include "lyra/value/formation.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/position.hpp"

namespace lyra::value {

// A dynamic array (LRM 7.5), written over what its element answers: compiled
// with the element's C++ type for the C++ backend, and in the library, with
// the element type's table, for the execution backend. Every element is handed
// in and out by its address.
//
// The elements are one contiguous array per generation, each at its index times
// the element's size from the start, so an element is one address computation
// away. An index names the same storage for the generation's whole life, and an
// ordering method permutes the values the elements hold rather than the
// elements; a change of size makes a new generation.
//
// A position arrives as what the program's position value names: a number, or
// none where it holds x or z.
template <ElementPolicy Elem>
class BasicDynamicArray {
 public:
  BasicDynamicArray()
    requires std::default_initializable<Elem>
  = default;
  explicit BasicDynamicArray(Elem elem) : elem_(std::move(elem)) {
  }

  // LRM 7.5.1 `new[N]` and `new[N](from)`: `count` elements, the first of them
  // copies of `from`'s first elements where it is given, and the rest the
  // element default. It delegates, so that the array is already one whose
  // destructor runs when a copy throws, ending the elements built before it.
  BasicDynamicArray(Elem elem, std::size_t count, const BasicDynamicArray* from)
      : BasicDynamicArray(std::move(elem)) {
    Allocate(count);
    const std::size_t kept =
        from == nullptr ? 0 : std::min(count, from->count_);
    for (std::size_t i = 0; i < count; ++i) {
      elem_.Copy(i < kept ? from->At(i) : elem_.Default(), Place(i));
      count_ = i + 1;
    }
  }

  // `count` elements, the one at index `i` built by `build(i, out)` in the
  // storage `out` it is handed.
  template <typename Build>
  [[nodiscard]] static auto Built(Elem elem, std::size_t count, Build build)
      -> BasicDynamicArray {
    BasicDynamicArray out(std::move(elem));
    out.Allocate(count);
    for (std::size_t i = 0; i < count; ++i) {
      build(i, out.Place(i));
      out.count_ = i + 1;
    }
    return out;
  }

  BasicDynamicArray(const BasicDynamicArray& other)
      : BasicDynamicArray(other.elem_, other.count_, &other) {
  }
  BasicDynamicArray(BasicDynamicArray&& other) noexcept
      : elem_(std::move(other.elem_)),
        data_(std::exchange(other.data_, {})),
        count_(std::exchange(other.count_, 0)) {
  }
  auto operator=(const BasicDynamicArray& other) -> BasicDynamicArray& {
    if (this != &other) {
      BasicDynamicArray copy(other);
      Swap(copy);
    }
    return *this;
  }
  auto operator=(BasicDynamicArray&& other) noexcept -> BasicDynamicArray& {
    if (this != &other) {
      BasicDynamicArray taken(std::move(other));
      Swap(taken);
    }
    return *this;
  }
  ~BasicDynamicArray() {
    for (std::size_t i = count_; i-- > 0;) {
      elem_.Destroy(At(i));
    }
    if (!data_.empty()) {
      ::operator delete(data_.data(), std::align_val_t{elem_.Align()});
    }
  }

  [[nodiscard]] auto Element() const -> const Elem& {
    return elem_;
  }
  [[nodiscard]] auto Count() const -> std::size_t {
    return count_;
  }
  [[nodiscard]] auto At(std::size_t index) const -> const void* {
    return Place(index);
  }
  [[nodiscard]] auto At(std::size_t index) -> void* {
    return Place(index);
  }

  // LRM 7.4.5: the element a position names, the element default where it
  // names none.
  [[nodiscard]] auto ElementAt(std::optional<std::int64_t> position) const
      -> const void* {
    const auto ordinal = ElementOrdinal(position, count_);
    return ordinal ? At(*ordinal) : elem_.Default();
  }

  // LRM 7.4.5: the element a position names, or, where it names none, storage
  // no read reaches, so a write there is discarded.
  [[nodiscard]] auto ExistingAt(std::optional<std::int64_t> position) -> void* {
    Formation formed{};
    return ElementMut(position, formed);
  }

  // The same, saying whether the position named an element: an array never
  // grows by being written, so a write either lands in an existing element or
  // nowhere.
  [[nodiscard]] auto ElementMut(
      std::optional<std::int64_t> position, Formation& formed) -> void* {
    const auto ordinal = ElementOrdinal(position, count_);
    formed = ordinal ? Formation::kExisting : Formation::kNowhere;
    return ordinal ? At(*ordinal) : elem_.DiscardedWrite();
  }

  // LRM 11.4.11: the two arms of a conditional operator whose condition is
  // ambiguous, combined element by element -- an element the arms agree on
  // survives, and one they disagree on, or cannot know, takes the element
  // default (Table 7-1). Arms of unequal size put no elements in
  // correspondence, so the result is the array of no elements that is such an
  // array's own default.
  [[nodiscard]] auto MergeConditional(const BasicDynamicArray& other) const
      -> BasicDynamicArray {
    const std::size_t paired = count_ == other.count_ ? count_ : 0;
    return Built(elem_, paired, [&](std::size_t i, void* out) {
      elem_.Copy(
          elem_.Agree(At(i), other.At(i)) ? At(i) : elem_.Default(), out);
    });
  }

  // Net resolution element by element under a truth table (LRM 6.6). LRM
  // 6.7.1 composes a net over an unpacked array out of its elements' bits, so
  // folding two contributions is folding each element pair.
  [[nodiscard]] auto Resolved(
      NetResolution fold, const BasicDynamicArray& other) const
      -> BasicDynamicArray {
    RequireNetPartner(other);
    return Built(elem_, count_, [&](std::size_t i, void* out) {
      elem_.Resolve(fold, At(i), other.At(i), out);
    });
  }

  // What a stronger contribution leaves a weaker one, element by element (LRM
  // 28.12.1).
  [[nodiscard]] auto Dominating(const BasicDynamicArray& weaker) const
      -> BasicDynamicArray {
    RequireNetPartner(weaker);
    return Built(elem_, count_, [&](std::size_t i, void* out) {
      elem_.Dominating(At(i), weaker.At(i), out);
    });
  }

  // These elements' shapes with every bit set to `fill` (LRM 6.7.1).
  template <typename Fill>
  [[nodiscard]] auto FilledLike(const Fill& fill) const -> BasicDynamicArray {
    return Built(elem_, count_, [&](std::size_t i, void* out) {
      elem_.FilledLike(At(i), fill, out);
    });
  }

  // LRM 6.24.3: the elements' own streams written into `stream` below its
  // `filled` most significant positions, the element at index 0 most
  // significant -- the order a `foreach` traverses them in (LRM 11.4.14.1).
  // Answers how many are filled after them.
  [[nodiscard]] auto WriteToStream(
      const IntegralView& stream, std::uint64_t filled) const -> std::uint64_t {
    for (std::size_t i = 0; i < count_; ++i) {
      filled = elem_.WriteToStream(At(i), stream, filled);
    }
    return filled;
  }

  // LRM 7.4.5 / 7.4.6: the `count` elements from `start`, each the element
  // default where it lies outside the array, every one of them where `start`
  // names no position.
  [[nodiscard]] auto SliceElements(
      std::optional<std::int64_t> start, std::size_t count) const
      -> std::vector<const void*> {
    return detail::SequenceSliceElements(*this, start, count);
  }

  // LRM 7.6: the `count` elements from `start` take `replacement`, element for
  // element; an element outside the array is skipped and a start naming no
  // position writes nothing. Answers whether any element took a different
  // value (LRM 4.3).
  auto AssignSlice(
      std::optional<std::int64_t> start, std::size_t count,
      std::span<const void* const> replacement) -> bool {
    return detail::SequenceAssignSlice(*this, start, count, replacement);
  }

  // A generation of these elements followed by copies of the ones `values`
  // names (LRM 10.10, 7.6).
  [[nodiscard]] auto Extended(std::span<const void* const> values) const
      -> BasicDynamicArray {
    BasicDynamicArray out(elem_);
    out.Allocate(count_ + values.size());
    for (std::size_t i = 0; i < count_; ++i) {
      elem_.Copy(At(i), out.Place(i));
      out.count_ = i + 1;
    }
    for (const void* value : values) {
      elem_.Copy(value, out.Place(out.count_));
      ++out.count_;
    }
    return out;
  }

  // LRM 7.5.3: the empty generation.
  void Delete() {
    *this = BasicDynamicArray(elem_);
  }

  // Puts the value that was at `order[k]` at index `k` (LRM 7.12.2).
  void Permute(std::span<const std::size_t> order) {
    const BasicDynamicArray held(elem_, count_, this);
    detail::SequencePermute(*this, held, order);
  }

 private:
  // A net fixes the shape of every contribution to it (LRM 6.7.1), so two
  // contributions of different element counts are a lowering defect.
  void RequireNetPartner(const BasicDynamicArray& other) const {
    if (other.count_ != count_) {
      throw InternalError(
          "two contributions to one net hold different element counts -- "
          "please report this as a bug");
    }
  }

  void Swap(BasicDynamicArray& other) noexcept {
    std::swap(elem_, other.elem_);
    std::swap(data_, other.data_);
    std::swap(count_, other.count_);
  }

  void Allocate(std::size_t count) {
    const std::size_t bytes = count * elem_.Size();
    if (bytes != 0) {
      data_ = {
          static_cast<std::byte*>(
              ::operator new(bytes, std::align_val_t{elem_.Align()})),
          bytes};
    }
  }

  [[nodiscard]] auto Place(std::size_t index) const -> std::byte* {
    return &data_[index * elem_.Size()];
  }

  Elem elem_;
  std::span<std::byte> data_;
  std::size_t count_ = 0;
};

}  // namespace lyra::value
