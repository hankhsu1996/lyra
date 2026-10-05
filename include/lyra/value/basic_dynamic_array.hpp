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
#include "lyra/value/net_resolution.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/position.hpp"

namespace lyra::value {

// A dynamic array (LRM 7.5), written once over what its element answers:
// compiled with the element's C++ type for the C++ backend, and once in the
// library, with the element type's table, for the execution backend. Every
// element is handed in and out by its address.
//
// The elements are one contiguous array per generation, each at its index times
// the element's size from the start, so an element is one address computation
// away. An index names the same storage for the generation's whole life, and an
// ordering method permutes the values the elements hold rather than the
// elements; a change of size makes a new generation.
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
  // element default.
  BasicDynamicArray(Elem elem, std::size_t count, const BasicDynamicArray* from)
      : elem_(std::move(elem)) {
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
        count_(std::exchange(other.count_, 0)),
        discard_(std::exchange(other.discard_, nullptr)) {
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
    if (discard_ != nullptr) {
      FreeElement(elem_, discard_);
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
  [[nodiscard]] auto ElementAt(const PackedArray& position) const -> const
      void* {
    const auto ordinal = ElementOrdinal(position, count_);
    return ordinal ? At(*ordinal) : elem_.Default();
  }

  // LRM 7.4.5: the element a position names, or, where it names none, storage
  // no read reaches, so a write there is discarded.
  [[nodiscard]] auto ExistingAt(const PackedArray& position) -> void* {
    Formation formed{};
    return ElementRef(position, formed);
  }

  // The same, saying whether the position named an element: an array never
  // grows by being written, so a write either lands in an existing element or
  // nowhere.
  [[nodiscard]] auto ElementRef(const PackedArray& position, Formation& formed)
      -> void* {
    const auto ordinal = ElementOrdinal(position, count_);
    formed = ordinal ? Formation::kExisting : Formation::kNowhere;
    return ordinal ? At(*ordinal) : DiscardTarget(elem_, discard_);
  }

  // LRM 11.4.11: the two arms of a conditional operator whose condition is
  // ambiguous, combined element by element -- an element the arms agree on
  // survives, and one they disagree on, or cannot know, takes the element
  // default (Table 7-1). Arms of unequal size put no elements in
  // correspondence, so every element takes that default.
  [[nodiscard]] auto MergeConditional(const BasicDynamicArray& other) const
      -> BasicDynamicArray {
    const bool paired = count_ == other.count_;
    return Built(elem_, count_, [&](std::size_t i, void* out) {
      const bool agree =
          paired && detail::ElementsAgree(elem_, At(i), other.At(i));
      elem_.Copy(agree ? At(i) : elem_.Default(), out);
    });
  }

  // Net resolution element by element under each truth table (LRM 6.6). LRM
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

  // These elements' shapes with every bit set to `fill` (LRM 6.7.1). The
  // element default stays this array's, which an invalid-index read returns
  // under LRM 7.4.5 whether the array is a net or a variable.
  [[nodiscard]] auto FilledLike(const PackedArray& fill) const
      -> BasicDynamicArray {
    return Built(elem_, count_, [&](std::size_t i, void* out) {
      elem_.FilledLike(At(i), fill, out);
    });
  }

  // LRM 6.24.3: the elements' own streams laid end to end, the element at
  // index 0 most significant -- the order a `foreach` traverses them in (LRM
  // 11.4.14.1). A fixed-size array, the one kind streamed whole, holds at least
  // one element.
  [[nodiscard]] auto ToBitstream() const -> PackedArray {
    PackedArray stream = elem_.ToBitstream(At(0));
    for (std::size_t i = 1; i < count_; ++i) {
      stream = stream.Concat(elem_.ToBitstream(At(i)));
    }
    return stream;
  }

  // The inverse, each element taking its own width off the front of what is
  // left (LRM 11.4.14.3). These elements state the count and every element's
  // shape, both of which a sequence of bits carries nothing of.
  [[nodiscard]] auto FromBitstream(const PackedArray& bits) const
      -> BasicDynamicArray {
    std::uint64_t consumed = 0;
    return Built(elem_, count_, [&](std::size_t i, void* out) {
      const auto width =
          static_cast<std::uint64_t>(elem_.BitstreamWidth(At(i)).ToInt64());
      elem_.FromBitstream(BitstreamSegment(bits, consumed, width), At(i), out);
      consumed += width;
    });
  }

  // LRM 7.4.5 / 7.4.6: the `count` elements from `start`, each the element
  // default where it lies outside the array, every one of them where `start`
  // names no position.
  [[nodiscard]] auto SliceElements(
      std::optional<std::int64_t> start, std::size_t count) const
      -> std::vector<const void*> {
    std::vector<const void*> elements;
    elements.reserve(count);
    const auto size = static_cast<std::int64_t>(count_);
    for (std::size_t i = 0; i < count; ++i) {
      const std::int64_t at =
          start.has_value() ? *start + static_cast<std::int64_t>(i) : -1;
      elements.push_back(
          at >= 0 && at < size ? At(static_cast<std::size_t>(at))
                               : elem_.Default());
    }
    return elements;
  }

  // LRM 7.6: the `count` elements from `start` take `replacement`, element for
  // element; an element outside the array is skipped and a start naming no
  // position writes nothing. Answers whether any element took a different
  // value (LRM 4.3).
  auto AssignSlice(
      std::optional<std::int64_t> start, std::size_t count,
      std::span<const void* const> replacement) -> bool {
    if (!start.has_value()) {
      return false;
    }
    const auto size = static_cast<std::int64_t>(count_);
    bool moved = false;
    for (std::size_t i = 0; i < count; ++i) {
      const std::int64_t at = *start + static_cast<std::int64_t>(i);
      if (at < 0 || at >= size) {
        continue;
      }
      void* element = At(static_cast<std::size_t>(at));
      if (elem_.BitIdentical(element, replacement[i])) {
        continue;
      }
      elem_.Assign(element, replacement[i]);
      moved = true;
    }
    return moved;
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
    for (std::size_t k = 0; k < order.size(); ++k) {
      elem_.Assign(At(k), held.At(order[k]));
    }
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
    std::swap(discard_, other.discard_);
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
  void* discard_ = nullptr;
};

}  // namespace lyra::value
