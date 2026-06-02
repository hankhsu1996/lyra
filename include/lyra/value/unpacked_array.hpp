#pragma once

#include <cstddef>
#include <cstdint>
#include <initializer_list>
#include <vector>

#include "lyra/value/packed_array.hpp"

namespace lyra::value {

template <typename T>
class UnpackedArrayRef;

// SystemVerilog fixed-size unpacked array (LRM 7.4.2). One C++ container layer
// per declared unpacked dimension; multi-dim composes as
// `UnpackedArray<UnpackedArray<...>>`. Mirrors `PackedArray`'s surface for
// every op that crosses the SV / C++ boundary: `ElementAt(const PackedArray&)`
// for indexed access (no `operator[]`), `Slice(offset, count)` for the LRM
// 7.4.5 contiguous-range selector, and `operator==` / `CaseEqual` returning a
// 1-bit `PackedArray` so equality on aggregates propagates through the same
// value-type the integral surface uses.
template <typename T>
class UnpackedArray {
 public:
  UnpackedArray() = default;
  UnpackedArray(std::initializer_list<T> init) : data_(init) {
  }
  UnpackedArray(const UnpackedArray&) = default;
  UnpackedArray(UnpackedArray&&) noexcept = default;
  auto operator=(const UnpackedArray&) -> UnpackedArray& = default;
  auto operator=(UnpackedArray&&) noexcept -> UnpackedArray& = default;
  ~UnpackedArray() = default;

  // LRM 7.4.5 indexed access. The index arrives as a `PackedArray` carrying
  // the SV-side value; the wrapper canonicalizes to a flat-vector offset so
  // emitted call sites stay `(base).ElementAt(idx)` regardless of substrate.
  [[nodiscard]] auto ElementAt(const PackedArray& idx) const -> const T& {
    return data_[static_cast<std::size_t>(idx.ToInt64())];
  }
  [[nodiscard]] auto ElementAt(const PackedArray& idx) -> T& {
    return data_[static_cast<std::size_t>(idx.ToInt64())];
  }

  [[nodiscard]] auto Size() const -> std::size_t {
    return data_.size();
  }

  // LRM 11.2.2 + 11.4.5: aggregate equality folds element comparisons with
  // logical AND so X / Z in any element propagates to the 1-bit result. The
  // terminal element comparison routes through the element type's own
  // `operator==` (PackedArray for the integral leaf; UnpackedArray recursively
  // for nested aggregates). `operator!=` is the bitwise negation of the
  // equality result -- it stays a 1-bit PackedArray, not a host `bool`.
  [[nodiscard]] auto operator==(const UnpackedArray& other) const
      -> PackedArray {
    PackedArray result = data_[0] == other.data_[0];
    for (std::size_t i = 1; i < data_.size(); ++i) {
      result = result && (data_[i] == other.data_[i]);
    }
    return result;
  }
  [[nodiscard]] auto operator!=(const UnpackedArray& other) const
      -> PackedArray {
    return !(*this == other);
  }

  // LRM 11.4.5 `===`: X / Z match as values, result is always 0 or 1 (no
  // propagation). The fold mirrors `operator==`; the per-element terminal is
  // `CaseEqElement` so a 4-state PackedArray operand re-tags its 2-state raw
  // result to 4-state, keeping the aggregate state-kind consistent with the
  // operands' element type.
  [[nodiscard]] auto CaseEqual(const UnpackedArray& other) const
      -> PackedArray {
    PackedArray result = CaseEqElement(data_[0], other.data_[0]);
    for (std::size_t i = 1; i < data_.size(); ++i) {
      result = result && CaseEqElement(data_[i], other.data_[i]);
    }
    return result;
  }

  // LRM 7.4.5 / 7.4.6 contiguous-range selector. Const overload materializes
  // a fresh `UnpackedArray` (rvalue slice); the non-const overload returns an
  // `UnpackedArrayRef` proxy so `arr.Slice(i, c) = b` routes element copies
  // back to the base. Positions are outer-element offsets (a `PackedArray`
  // carrying the SV-side index value); count is the LRM-guaranteed
  // compile-time constant width.
  [[nodiscard]] auto Slice(
      const PackedArray& offset_in_outer_elements,
      std::uint32_t count_in_outer_elements) const -> UnpackedArray {
    const auto off =
        static_cast<std::size_t>(offset_in_outer_elements.ToInt64());
    UnpackedArray result;
    result.data_.assign(
        data_.begin() + off, data_.begin() + off + count_in_outer_elements);
    return result;
  }
  [[nodiscard]] auto Slice(
      const PackedArray& offset_in_outer_elements,
      std::uint32_t count_in_outer_elements) -> UnpackedArrayRef<T> {
    return UnpackedArrayRef<T>{
        *this, static_cast<std::size_t>(offset_in_outer_elements.ToInt64()),
        count_in_outer_elements};
  }

 private:
  // Per-element CaseEqual. Terminal case (integral leaf) re-tags to 4-state
  // when either operand is 4-state, because PackedArray::CaseEqual returns
  // 2-state by LRM 11.4.5 but the aggregate state-kind follows the operand
  // element type. Nested case (UnpackedArray<U>) delegates to the recursive
  // CaseEqual -- it already produces the right state.
  [[nodiscard]] static auto CaseEqElement(
      const PackedArray& a, const PackedArray& b) -> PackedArray {
    auto raw = a.CaseEqual(b);
    const bool four_state = a.IsFourState() || b.IsFourState();
    return four_state ? PackedArray::ConvertFrom(raw, 1, false, true) : raw;
  }
  template <typename U>
  [[nodiscard]] static auto CaseEqElement(
      const UnpackedArray<U>& a, const UnpackedArray<U>& b) -> PackedArray {
    return a.CaseEqual(b);
  }

  std::vector<T> data_;

  friend class UnpackedArrayRef<T>;
};

// LRM 7.6: an assignment to an unpacked slice is a single assignment to the
// entire slice. The proxy holds a non-owning pointer back to the source array
// plus the (offset, count) window; `operator=` writes `count` elements at
// `offset` from the right-hand-side value. Move-only so the proxy cannot
// outlive the source it aliases.
template <typename T>
class UnpackedArrayRef {
 public:
  UnpackedArrayRef(
      UnpackedArray<T>& base, std::size_t offset, std::size_t count)
      : base_(&base), offset_(offset), count_(count) {
  }
  UnpackedArrayRef(const UnpackedArrayRef&) = delete;
  auto operator=(const UnpackedArrayRef&) -> UnpackedArrayRef& = delete;
  UnpackedArrayRef(UnpackedArrayRef&&) noexcept = default;
  auto operator=(UnpackedArrayRef&&) noexcept -> UnpackedArrayRef& = default;
  ~UnpackedArrayRef() = default;

  [[nodiscard]] auto Clone() const -> UnpackedArray<T> {
    UnpackedArray<T> result;
    result.data_.assign(
        base_->data_.begin() + offset_,
        base_->data_.begin() + offset_ + count_);
    return result;
  }

  auto operator=(const UnpackedArray<T>& value) -> UnpackedArrayRef& {
    for (std::size_t i = 0; i < count_; ++i) {
      base_->data_[offset_ + i] = value.data_[i];
    }
    return *this;
  }

 private:
  UnpackedArray<T>* base_;
  std::size_t offset_;
  std::size_t count_;
};

}  // namespace lyra::value
