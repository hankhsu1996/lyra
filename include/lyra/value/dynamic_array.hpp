#pragma once

#include <cstddef>
#include <cstdint>
#include <span>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/value/array_case_equal.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::value {

// SystemVerilog dynamic array (LRM 7.5). Size set at run time via `new[N]` /
// `new[N](other)` constructors; default is the empty array (LRM Table 6-7).
//
// `oob_slot_` is the LRM 7.4.5 invalid-index shield. Its state is restored
// to the canonical default on every OOB access via `T::ResetToDefault`, so
// reads after an OOB write observe the LRM Table 7-1 default rather than
// whatever the prior write left in the slot. It is supplied at construction
// because `T = PackedArray` requires runtime shape parameters (bit width,
// signedness, 2/4-state) that cannot be recovered from the C++ type alone.
// See `docs/decisions/runtime-shape-and-default-value.md`.
template <typename T>
class DynamicArray {
 public:
  using ElementType = T;

  // Empty container with the shield slot seeded. Used for declarations like
  // `int arr[];` where the array starts empty but the element shape is
  // known at lowering time.
  explicit DynamicArray(T oob_slot) : oob_slot_(std::move(oob_slot)) {
  }

  // LRM 7.5.1 `new[N]`: build `n` elements, each a copy of the shield-slot
  // seed (which is also the canonical default). `n` is a longint per LRM
  // 7.5.1; the negative-N case throws at construction.
  DynamicArray(const PackedArray& n, T oob_slot)
      : oob_slot_(std::move(oob_slot)) {
    const std::int64_t n_val = n.ToInt64();
    if (n_val < 0) {
      throw InternalError(
          "DynamicArray::new[N]: size operand is negative (LRM 7.5.1)");
    }
    data_.assign(static_cast<std::size_t>(n_val), oob_slot_);
  }

  // LRM 7.5.1 `new[N](other)`: copy `other` then `resize(N, oob_slot_)` --
  // `std::vector::resize` truncates when target is smaller and pads with
  // the fill value when target is larger, covering both halves of LRM
  // 7.5.1 in one call.
  DynamicArray(const PackedArray& n, T oob_slot, const DynamicArray& src)
      : oob_slot_(std::move(oob_slot)), data_(src.data_) {
    const std::int64_t n_val = n.ToInt64();
    if (n_val < 0) {
      throw InternalError(
          "DynamicArray::new[N](src): size operand is negative (LRM 7.5.1)");
    }
    data_.resize(static_cast<std::size_t>(n_val), oob_slot_);
  }

  // LRM 10.9.1 assignment-pattern construction: shield slot seeded, size
  // taken from the pattern's element list. Mirrors `UnpackedArray`'s span
  // ctor so a single emit path produces `std::array<T, N>{...}` as the
  // second argument for either container.
  DynamicArray(T oob_slot, std::span<const T> init)
      : oob_slot_(std::move(oob_slot)), data_(init.begin(), init.end()) {
  }

  DynamicArray(const DynamicArray&) = default;
  DynamicArray(DynamicArray&&) noexcept = default;
  auto operator=(const DynamicArray&) -> DynamicArray& = default;
  auto operator=(DynamicArray&&) noexcept -> DynamicArray& = default;
  ~DynamicArray() = default;

  [[nodiscard]] auto Size() const -> std::size_t {
    return data_.size();
  }

  [[nodiscard]] auto RawAt(std::size_t i) const -> const T& {
    return data_[i];
  }

  [[nodiscard]] auto Clone() const -> DynamicArray {
    return *this;
  }

  // LRM Table 6-7: dynamic array default is the empty array. When this
  // container is itself an OOB shield slot of an outer container, the outer
  // calls this method to restore canonical state before handing out a
  // reference; any subsequent inner access then re-OOBs naturally (data_ is
  // empty), so chained access through the shield never observes a stale
  // write.
  auto ResetToDefault() -> void {
    data_.clear();
  }

  // LRM 7.4.5: an invalid index makes the access route through `oob_slot_`.
  // The slot is restored to canonical state before the reference is handed
  // out, so OOB writes that mutate it are invisible to any subsequent
  // access. The non-const overload returns a writable reference; writes
  // through it land on the shield rather than on real storage and are
  // erased on the next OOB access.
  [[nodiscard]] auto ElementAt(const PackedArray& idx) -> T& {
    if (IsInvalidIndex(idx)) {
      oob_slot_.ResetToDefault();
      return oob_slot_;
    }
    return data_[static_cast<std::size_t>(idx.ToInt64())];
  }

  [[nodiscard]] auto ElementAt(const PackedArray& idx) const -> const T& {
    if (IsInvalidIndex(idx)) {
      oob_slot_.ResetToDefault();
      return oob_slot_;
    }
    return data_[static_cast<std::size_t>(idx.ToInt64())];
  }

  // LRM 11.2.2 + 11.4.5 aggregate equality. Runtime size mismatch yields
  // 0 and matching-size empties yield 1; the LRM is silent on both, this
  // matches industry convention (Verilator). For matching non-empty sizes,
  // `==` / `!=` propagate X / Z through the `&&` reduction over element
  // comparisons; `CaseEqual` matches X / Z as values and is deterministic.
  [[nodiscard]] auto operator==(const DynamicArray& other) const
      -> PackedArray {
    if (data_.size() != other.data_.size()) {
      return PackedArray::FromInt(0, 1, false, false);
    }
    if (data_.empty()) {
      return PackedArray::FromInt(1, 1, false, false);
    }
    PackedArray result = data_[0] == other.data_[0];
    for (std::size_t i = 1; i < data_.size(); ++i) {
      result = result && (data_[i] == other.data_[i]);
    }
    return result;
  }
  [[nodiscard]] auto operator!=(const DynamicArray& other) const
      -> PackedArray {
    return !(*this == other);
  }

  [[nodiscard]] auto CaseEqual(const DynamicArray& other) const -> PackedArray {
    if (data_.size() != other.data_.size()) {
      return PackedArray::FromInt(0, 1, false, false);
    }
    if (data_.empty()) {
      return PackedArray::FromInt(1, 1, false, false);
    }
    PackedArray result = detail::ArrayCaseEqElement(data_[0], other.data_[0]);
    for (std::size_t i = 1; i < data_.size(); ++i) {
      result = result && detail::ArrayCaseEqElement(data_[i], other.data_[i]);
    }
    return result;
  }

 private:
  [[nodiscard]] auto IsInvalidIndex(const PackedArray& idx) const -> bool {
    if (idx.HasUnknown()) return true;
    const auto v = idx.ToInt64();
    return v < 0 || static_cast<std::uint64_t>(v) >=
                        static_cast<std::uint64_t>(data_.size());
  }

  mutable T oob_slot_;
  std::vector<T> data_;
};

}  // namespace lyra::value
