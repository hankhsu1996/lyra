#pragma once

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::value {

template <typename T>
class DynamicArray;
template <typename T>
class DynamicElementRef;

// SystemVerilog dynamic array (LRM 7.5). Size set at run time via `new[N]` /
// `new[N](other)` constructors; default is the empty array (LRM Table 6-7).
//
// `default_value_` carries the element type's LRM Table 6-7 default and is
// the single source for OOB synthesis (LRM 7.4.5) and resize-fill. It is
// supplied at construction -- there is no zero-argument constructor, because
// `T = PackedArray` requires runtime shape parameters that cannot be
// recovered from the C++ type alone. See `docs/decisions/runtime-shape-and-
// default-value.md`.
template <typename T>
class DynamicArray {
 public:
  using ElementType = T;

  // Empty container with default_value populated. Used for declarations like
  // `int arr[];` where the array starts empty but the element shape is known
  // at lowering time.
  explicit DynamicArray(T default_value)
      : default_value_(std::move(default_value)) {
  }

  // LRM 7.5.1 `new[N]`: build `n` elements, each a copy of `default_value`.
  // `n` is a longint per LRM 7.5.1; the negative-N case throws at
  // construction.
  DynamicArray(const PackedArray& n, T default_value)
      : default_value_(std::move(default_value)) {
    const std::int64_t n_val = n.ToInt64();
    if (n_val < 0) {
      throw InternalError(
          "DynamicArray::new[N]: size operand is negative (LRM 7.5.1)");
    }
    data_.assign(static_cast<std::size_t>(n_val), default_value_);
  }

  // LRM 7.5.1 `new[N](other)`: build `n` elements; copy the first
  // min(n, other.Size()) from `other`, pad the rest with `default_value`.
  DynamicArray(const PackedArray& n, T default_value, const DynamicArray& src)
      : default_value_(std::move(default_value)) {
    const std::int64_t n_val = n.ToInt64();
    if (n_val < 0) {
      throw InternalError(
          "DynamicArray::new[N](src): size operand is negative (LRM 7.5.1)");
    }
    const auto target = static_cast<std::size_t>(n_val);
    data_.reserve(target);
    const auto copy_count = std::min(target, src.data_.size());
    for (std::size_t i = 0; i < copy_count; ++i) {
      data_.push_back(src.data_[i]);
    }
    for (std::size_t i = copy_count; i < target; ++i) {
      data_.push_back(default_value_);
    }
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

  // LRM 7.4.5: invalid index returns the element type's Table 7-1 default.
  // `default_value_` was supplied at construction and remains valid for the
  // wrapper's lifetime, so the empty-storage case is handled uniformly.
  [[nodiscard]] auto ElementAt(const PackedArray& idx) const -> T {
    if (IsInvalidIndex(idx)) {
      return default_value_;
    }
    return data_[static_cast<std::size_t>(idx.ToInt64())];
  }

  [[nodiscard]] auto ElementAt(const PackedArray& idx) -> DynamicElementRef<T> {
    return MakeElementRef(*this, idx);
  }

 private:
  [[nodiscard]] auto IsInvalidIndex(const PackedArray& idx) const -> bool {
    if (idx.HasUnknown()) return true;
    const auto v = idx.ToInt64();
    return v < 0 || static_cast<std::uint64_t>(v) >=
                        static_cast<std::uint64_t>(data_.size());
  }

  static auto MakeElementRef(DynamicArray& self, const PackedArray& idx)
      -> DynamicElementRef<T> {
    if (self.IsInvalidIndex(idx)) {
      return DynamicElementRef<T>{self, std::size_t{0}, false};
    }
    return DynamicElementRef<T>{
        self, static_cast<std::size_t>(idx.ToInt64()), true};
  }

  T default_value_;
  std::vector<T> data_;

  friend class DynamicElementRef<T>;
};

// Write-back proxy for `DynamicArray::ElementAt` non-const. Captures index
// validity at construction; subsequent reads through `Clone()` and writes
// through `operator=` honour that validity per LRM 7.4.5 (invalid write is
// a silent no-op, invalid read returns the element default).
template <typename T>
class DynamicElementRef {
 public:
  DynamicElementRef(DynamicArray<T>& base, std::size_t offset, bool valid)
      : base_(&base), offset_(offset), valid_(valid) {
  }
  DynamicElementRef(const DynamicElementRef&) = delete;
  auto operator=(const DynamicElementRef&) -> DynamicElementRef& = delete;
  DynamicElementRef(DynamicElementRef&&) noexcept = default;
  auto operator=(DynamicElementRef&&) noexcept -> DynamicElementRef& = default;
  ~DynamicElementRef() = default;

  [[nodiscard]] auto Clone() const -> T {
    if (valid_) return base_->data_[offset_];
    return base_->default_value_;
  }

  auto operator=(const T& value) -> DynamicElementRef& {
    if (valid_) base_->data_[offset_] = value;
    return *this;
  }

  // Chain composition for multi-dim access. An invalid outer has no inner
  // element to bind to; LRM 7.5.1 multi-dim initialization order makes this
  // a user-side ordering bug rather than a supported path.
  [[nodiscard]] auto ElementAt(const PackedArray& idx) {
    if (!valid_) {
      throw InternalError(
          "DynamicArray::ElementAt: chain access through an invalid proxy");
    }
    return base_->data_[offset_].ElementAt(idx);
  }

  auto MarkInvalid() -> void {
    valid_ = false;
  }

 private:
  DynamicArray<T>* base_;
  std::size_t offset_;
  bool valid_;
};

}  // namespace lyra::value
