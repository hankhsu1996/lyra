#pragma once

#include <concepts>
#include <cstddef>
#include <cstdint>
#include <initializer_list>
#include <vector>

#include "lyra/value/packed_array.hpp"

namespace lyra::value {

template <typename T>
class UnpackedArray;
template <typename T>
class UnpackedArrayRef;
template <typename T>
class UnpackedElementRef;

// LRM Table 7-1 default for a value of the same shape as `oracle`. The shape
// oracle is needed because the type alone (`PackedArray`, `UnpackedArray<U>`)
// does not determine the value's runtime parameters (bit width, signedness,
// state-kind, nested size).
inline auto MakeDefaultLike(const PackedArray& oracle) -> PackedArray {
  return PackedArray{
      oracle.BitWidth(), oracle.IsSigned(), oracle.IsFourState()};
}

template <typename U>
auto MakeDefaultLike(const UnpackedArray<U>& oracle) -> UnpackedArray<U> {
  return oracle.DefaultLike();
}

// SystemVerilog fixed-size unpacked array (LRM 7.4.2). One C++ container layer
// per declared unpacked dimension; multi-dim composes as
// `UnpackedArray<UnpackedArray<...>>`. Mirrors `PackedArray`'s surface for
// every op that crosses the SV / C++ boundary: `ElementAt(const PackedArray&)`
// for indexed access (no `operator[]`), `Slice(offset, count)` for the LRM
// 7.4.5 contiguous-range selector, and `operator==` / `CaseEqual` returning a
// 1-bit `PackedArray` so equality on aggregates propagates through the same
// value-type the integral surface uses.
//
// LRM 7.4.5 invalid-index handling lives in this class and its non-const
// chain proxies (`UnpackedElementRef`, `UnpackedArrayRef`): any X / Z bit in
// the index, or any out-of-bounds position, makes the index invalid -- read
// returns the Table 7-1 default for the requested type, write is a silent
// no-op. Partial-OOB slices behave per-element: in-range elements participate
// normally, OOB elements use the default on read / are dropped on write.
template <typename T>
class UnpackedArray {
 public:
  using ElementType = T;

  UnpackedArray() = default;
  UnpackedArray(std::initializer_list<T> init) : data_(init) {
  }
  UnpackedArray(const UnpackedArray&) = default;
  UnpackedArray(UnpackedArray&&) noexcept = default;
  auto operator=(const UnpackedArray&) -> UnpackedArray& = default;
  auto operator=(UnpackedArray&&) noexcept -> UnpackedArray& = default;
  ~UnpackedArray() = default;

  [[nodiscard]] auto Size() const -> std::size_t {
    return data_.size();
  }

  // LRM 7.4.5: read returns Table 7-1 default on invalid index. Returns a
  // fresh value rather than a reference so the OOB case can synthesize a
  // default without aliasing storage. Symmetric with
  // `PackedArray::ElementAt(...) const -> PackedArray`.
  [[nodiscard]] auto ElementAt(const PackedArray& idx) const -> T {
    if (IsInvalidIndex(idx)) {
      return MakeDefaultLike(data_.front());
    }
    return data_[static_cast<std::size_t>(idx.ToInt64())];
  }

  // Non-const overload yields a write-back proxy. The proxy captures the
  // index's validity at construction; subsequent `operator=` / compound ops
  // / chain calls observe that captured validity. Symmetric with
  // `PackedArray::ElementAt(...) -> PackedArrayRef`.
  [[nodiscard]] auto ElementAt(const PackedArray& idx)
      -> UnpackedElementRef<T> {
    return MakeElementRef(*this, idx);
  }

  // LRM 7.4.5 contiguous-range selector. The const overload materializes a
  // fresh sub-array; partial-OOB positions yield Table 7-1 defaults at the
  // element type, X / Z offset yields a wholly-default sub-array. The
  // non-const overload returns a write-back proxy with the same invalid-index
  // policy on `operator=`.
  [[nodiscard]] auto Slice(
      const PackedArray& offset_in_outer_elements,
      std::uint32_t count_in_outer_elements) const -> UnpackedArray {
    UnpackedArray result;
    result.data_.reserve(count_in_outer_elements);
    const bool offset_unknown = offset_in_outer_elements.HasUnknown();
    const std::int64_t base =
        offset_unknown ? 0 : offset_in_outer_elements.ToInt64();
    const auto size = static_cast<std::int64_t>(data_.size());
    for (std::uint32_t i = 0; i < count_in_outer_elements; ++i) {
      const std::int64_t pos = base + static_cast<std::int64_t>(i);
      if (offset_unknown || pos < 0 || pos >= size) {
        result.data_.push_back(MakeDefaultLike(data_.front()));
      } else {
        result.data_.push_back(data_[static_cast<std::size_t>(pos)]);
      }
    }
    return result;
  }

  [[nodiscard]] auto Slice(
      const PackedArray& offset_in_outer_elements,
      std::uint32_t count_in_outer_elements) -> UnpackedArrayRef<T> {
    return UnpackedArrayRef<T>{
        *this, offset_in_outer_elements, count_in_outer_elements};
  }

  // LRM 11.2.2 + 11.4.5 aggregate equality / case-equality. Slang's binding
  // enforces equivalent operand size, so the loops over `data_` are matched.
  // `==` / `!=` propagate X / Z; `CaseEqual` returns a deterministic 0/1.
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

  [[nodiscard]] auto CaseEqual(const UnpackedArray& other) const
      -> PackedArray {
    PackedArray result = CaseEqElement(data_[0], other.data_[0]);
    for (std::size_t i = 1; i < data_.size(); ++i) {
      result = result && CaseEqElement(data_[i], other.data_[i]);
    }
    return result;
  }

  // Build a fresh array of the same size with each element defaulted from
  // this array's element 0. Used by `MakeDefaultLike` for the nested case
  // and by `Slice` for OOB element synthesis on the const path.
  [[nodiscard]] auto DefaultLike() const -> UnpackedArray {
    UnpackedArray result;
    if (data_.empty()) {
      return result;
    }
    result.data_.assign(data_.size(), MakeDefaultLike(data_.front()));
    return result;
  }

 private:
  [[nodiscard]] auto IsInvalidIndex(const PackedArray& idx) const -> bool {
    if (idx.HasUnknown()) return true;
    const auto v = idx.ToInt64();
    return v < 0 || static_cast<std::uint64_t>(v) >=
                        static_cast<std::uint64_t>(data_.size());
  }

  static auto MakeElementRef(UnpackedArray& self, const PackedArray& idx)
      -> UnpackedElementRef<T> {
    if (self.IsInvalidIndex(idx)) {
      return UnpackedElementRef<T>{self, std::size_t{0}, false};
    }
    return UnpackedElementRef<T>{
        self, static_cast<std::size_t>(idx.ToInt64()), true};
  }

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
  friend class UnpackedElementRef<T>;
};

// Write-back proxy for `UnpackedArray::ElementAt` non-const. Captures index
// validity at construction; subsequent reads through `Clone()` and writes
// through `operator=` (or compound ops, when T is `PackedArray`) honor that
// validity per LRM 7.4.5. Chain methods (`ElementAt`, `Slice`) on a proxy
// over a nested-aggregate element propagate the validity flag so an invalid
// outer index makes the entire chain a no-op write / Table 7-1 default read.
template <typename T>
class UnpackedElementRef {
 public:
  UnpackedElementRef(UnpackedArray<T>& base, std::size_t offset, bool valid)
      : base_(&base), offset_(offset), valid_(valid) {
  }
  UnpackedElementRef(const UnpackedElementRef&) = delete;
  auto operator=(const UnpackedElementRef&) -> UnpackedElementRef& = delete;
  UnpackedElementRef(UnpackedElementRef&&) noexcept = default;
  auto operator=(UnpackedElementRef&&) noexcept
      -> UnpackedElementRef& = default;
  ~UnpackedElementRef() = default;

  [[nodiscard]] auto Clone() const -> T {
    if (valid_) {
      return base_->data_[offset_];
    }
    return MakeDefaultLike(base_->data_.front());
  }

  auto operator=(const T& value) -> UnpackedElementRef& {
    if (valid_) {
      base_->data_[offset_] = value;
    }
    return *this;
  }

  // Compound assignment for PackedArray-element arrays. The load-modify-store
  // pattern is gated by `valid_` so an invalid index is a silent no-op end to
  // end, including the read that would otherwise observe garbage memory.
  auto operator+=(const PackedArray& rhs) -> UnpackedElementRef&
    requires std::same_as<T, PackedArray>
  {
    if (valid_) base_->data_[offset_] += rhs;
    return *this;
  }
  auto operator-=(const PackedArray& rhs) -> UnpackedElementRef&
    requires std::same_as<T, PackedArray>
  {
    if (valid_) base_->data_[offset_] -= rhs;
    return *this;
  }
  auto operator*=(const PackedArray& rhs) -> UnpackedElementRef&
    requires std::same_as<T, PackedArray>
  {
    if (valid_) base_->data_[offset_] *= rhs;
    return *this;
  }
  auto operator/=(const PackedArray& rhs) -> UnpackedElementRef&
    requires std::same_as<T, PackedArray>
  {
    if (valid_) base_->data_[offset_] /= rhs;
    return *this;
  }
  auto operator%=(const PackedArray& rhs) -> UnpackedElementRef&
    requires std::same_as<T, PackedArray>
  {
    if (valid_) base_->data_[offset_] %= rhs;
    return *this;
  }
  auto operator&=(const PackedArray& rhs) -> UnpackedElementRef&
    requires std::same_as<T, PackedArray>
  {
    if (valid_) base_->data_[offset_] &= rhs;
    return *this;
  }
  auto operator|=(const PackedArray& rhs) -> UnpackedElementRef&
    requires std::same_as<T, PackedArray>
  {
    if (valid_) base_->data_[offset_] |= rhs;
    return *this;
  }
  auto operator^=(const PackedArray& rhs) -> UnpackedElementRef&
    requires std::same_as<T, PackedArray>
  {
    if (valid_) base_->data_[offset_] ^= rhs;
    return *this;
  }
  auto ShiftLeftAssign(const PackedArray& rhs) -> UnpackedElementRef&
    requires std::same_as<T, PackedArray>
  {
    if (valid_) base_->data_[offset_].ShiftLeftAssign(rhs);
    return *this;
  }
  auto LogicalShiftRightAssign(const PackedArray& rhs) -> UnpackedElementRef&
    requires std::same_as<T, PackedArray>
  {
    if (valid_) base_->data_[offset_].LogicalShiftRightAssign(rhs);
    return *this;
  }
  auto ArithmeticShiftRightAssign(const PackedArray& rhs) -> UnpackedElementRef&
    requires std::same_as<T, PackedArray>
  {
    if (valid_) base_->data_[offset_].ArithmeticShiftRightAssign(rhs);
    return *this;
  }

  // Chain composition for nested aggregates. The validity flag propagates: an
  // invalid outer index makes the inner proxy invalid too, regardless of the
  // inner index's value. The inner proxy still needs a real `UnpackedArray<U>`
  // to bind to for shape oracle; we use `data_[0]` (always present in
  // fixed-size unpacked arrays per LRM 7.4.2) when the outer is invalid.
  // Chain composition for nested aggregates. Returned type is deduced so
  // `typename T::ElementType` only fires at call-site instantiation; for a
  // leaf-element proxy (T = PackedArray) the method is declared but never
  // called from emit, so the dependent-name lookup never resolves.
  [[nodiscard]] auto ElementAt(const PackedArray& idx) {
    auto& inner = valid_ ? base_->data_[offset_] : base_->data_.front();
    auto proxy = inner.ElementAt(idx);
    if (!valid_) proxy.MarkInvalid();
    return proxy;
  }

  [[nodiscard]] auto Slice(
      const PackedArray& offset_in_outer_elements,
      std::uint32_t count_in_outer_elements) {
    auto& inner = valid_ ? base_->data_[offset_] : base_->data_.front();
    auto proxy = inner.Slice(offset_in_outer_elements, count_in_outer_elements);
    if (!valid_) proxy.MarkInvalid();
    return proxy;
  }

  // Drop validity after construction. Used by chain composition when the
  // outer proxy is invalid -- the inner proxy is constructed normally then
  // demoted so any subsequent write is dropped.
  auto MarkInvalid() -> void {
    valid_ = false;
  }

 private:
  UnpackedArray<T>* base_;
  std::size_t offset_;
  bool valid_;
};

// LRM 7.6: an assignment to an unpacked slice is a single assignment to the
// entire slice. The proxy holds a non-owning pointer back to the source array
// plus the (offset, count) window. Invalid offset (X / Z, fully OOB) makes
// `Clone()` return a wholly-default sub-array and `operator=` a no-op;
// partial-OOB behaves per-element. Move-only so the proxy cannot outlive the
// source it aliases.
template <typename T>
class UnpackedArrayRef {
 public:
  UnpackedArrayRef(
      UnpackedArray<T>& base, const PackedArray& offset, std::uint32_t count)
      : base_(&base),
        offset_unknown_(offset.HasUnknown()),
        offset_(offset.HasUnknown() ? 0 : offset.ToInt64()),
        count_(count) {
  }
  UnpackedArrayRef(const UnpackedArrayRef&) = delete;
  auto operator=(const UnpackedArrayRef&) -> UnpackedArrayRef& = delete;
  UnpackedArrayRef(UnpackedArrayRef&&) noexcept = default;
  auto operator=(UnpackedArrayRef&&) noexcept -> UnpackedArrayRef& = default;
  ~UnpackedArrayRef() = default;

  [[nodiscard]] auto Clone() const -> UnpackedArray<T> {
    UnpackedArray<T> result;
    result.data_.reserve(count_);
    const auto size = static_cast<std::int64_t>(base_->data_.size());
    for (std::uint32_t i = 0; i < count_; ++i) {
      const std::int64_t pos = offset_ + static_cast<std::int64_t>(i);
      if (!valid_ || offset_unknown_ || pos < 0 || pos >= size) {
        result.data_.push_back(MakeDefaultLike(base_->data_.front()));
      } else {
        result.data_.push_back(base_->data_[static_cast<std::size_t>(pos)]);
      }
    }
    return result;
  }

  auto operator=(const UnpackedArray<T>& value) -> UnpackedArrayRef& {
    if (!valid_ || offset_unknown_) return *this;
    const auto size = static_cast<std::int64_t>(base_->data_.size());
    for (std::uint32_t i = 0; i < count_; ++i) {
      const std::int64_t pos = offset_ + static_cast<std::int64_t>(i);
      if (pos < 0 || pos >= size) continue;
      base_->data_[static_cast<std::size_t>(pos)] = value.data_[i];
    }
    return *this;
  }

  auto MarkInvalid() -> void {
    valid_ = false;
  }

 private:
  UnpackedArray<T>* base_;
  bool offset_unknown_;
  std::int64_t offset_;
  std::uint32_t count_;
  bool valid_ = true;
};

}  // namespace lyra::value
