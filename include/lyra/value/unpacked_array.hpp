#pragma once

#include <cstddef>
#include <cstdint>
#include <span>
#include <string>
#include <utility>
#include <vector>

#include "lyra/value/array_case_equal.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::value {

template <typename T>
class UnpackedArray;
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
//
// `oob_slot_` is the LRM 7.4.5 invalid-index shield. Its state is restored
// to the canonical default on every OOB access via `T::ResetToDefault`, so
// reads after an OOB write observe the LRM Table 7-1 default rather than
// whatever the prior write left in the slot. It is supplied at construction
// because `T = PackedArray` requires runtime shape parameters (bit width,
// signedness, 2/4-state) that cannot be recovered from the C++ type alone.
// See `docs/decisions/runtime-shape-and-default-value.md`.
template <typename T>
class UnpackedArray {
 public:
  using ElementType = T;

  // Sentinel "uninitialized" form -- empty container with default-constructed
  // OOB slot. Used as the declared default state of a `Var<UnpackedArray<T>>`
  // field; the first MIR-level assignment overwrites the whole array (LRM
  // 10.5 variable initialization).
  UnpackedArray() = default;

  // Empty container with the shield slot seeded. Internal use only -- SV
  // fixed-size arrays are always non-empty; this form serves `Slice` and
  // similar paths that build a fresh `UnpackedArray` element-by-element.
  explicit UnpackedArray(T oob_slot) : oob_slot_(std::move(oob_slot)) {
  }

  // Element-list construction: shield slot seeded, plus the explicit
  // initial elements (LRM 10.9 assignment pattern lowering). The element
  // list is taken as a span so the emit side can hand in a `std::array<T,
  // N>{...}` literal whose self-determined type is unambiguous, rather
  // than relying on context-dependent braced-init binding.
  UnpackedArray(T oob_slot, std::span<const T> init)
      : oob_slot_(std::move(oob_slot)), data_(init.begin(), init.end()) {
  }

  UnpackedArray(const UnpackedArray&) = default;
  UnpackedArray(UnpackedArray&&) noexcept = default;
  auto operator=(const UnpackedArray&) -> UnpackedArray& = default;
  auto operator=(UnpackedArray&&) noexcept -> UnpackedArray& = default;
  ~UnpackedArray() = default;

  [[nodiscard]] auto Size() const -> std::size_t {
    return data_.size();
  }

  // Flat-storage element read. `i` is in [0, Size()); no SV-index translation,
  // no invalid-index handling. Sole consumer is the aggregate-format path
  // (`Formatter<UnpackedArray<T>>::Format`), which traverses storage to defer
  // each element to its own `Formatter` specialization. SV-semantics access
  // goes through `ElementAt(PackedArray)`.
  [[nodiscard]] auto RawAt(std::size_t i) const -> const T& {
    return data_[i];
  }

  [[nodiscard]] auto Clone() const -> UnpackedArray {
    return *this;
  }

  // LRM Table 7-1 default for a fixed-size unpacked array is "Array, all of
  // whose elements have the value specified in this table for that array's
  // element type." When this container is itself the OOB shield slot of an
  // outer container, the outer calls this method to restore the canonical
  // all-defaults state before handing out a reference. This is O(N) in the
  // unpacked dim and is the LRM-mandated cost of "all elements at default."
  auto ResetToDefault() -> void {
    for (auto& elem : data_) {
      elem.ResetToDefault();
    }
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

  // LRM 7.4.5 contiguous-range selector. The const overload materializes a
  // fresh sub-array; partial-OOB positions yield the canonical default,
  // X / Z offset yields a wholly-default sub-array. The non-const overload
  // returns a write-back proxy with the same invalid-index policy on
  // `operator=`.
  [[nodiscard]] auto Slice(const PackedArray& offset, std::uint32_t count) const
      -> UnpackedArray {
    const T canonical = MakeCanonicalElement();
    UnpackedArray result(canonical);
    if (offset.HasUnknown()) {
      result.data_.assign(count, canonical);
      return result;
    }
    result.data_.reserve(count);
    const auto base = offset.ToInt64();
    const auto size = static_cast<std::int64_t>(data_.size());
    for (std::uint32_t i = 0; i < count; ++i) {
      const auto pos = base + static_cast<std::int64_t>(i);
      const bool in_bounds = pos >= 0 && pos < size;
      result.data_.push_back(
          in_bounds ? data_[static_cast<std::size_t>(pos)] : canonical);
    }
    return result;
  }

  [[nodiscard]] auto Slice(const PackedArray& offset, std::uint32_t count)
      -> UnpackedArrayRef<T> {
    return UnpackedArrayRef<T>{*this, offset, count};
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

  // Returns a fresh `T` at this container's element shape in LRM Table 7-1
  // canonical state. Used by paths that need a default element disconnected
  // from the OOB shield identity (Slice fill, Slice-ref Clone).
  [[nodiscard]] auto MakeCanonicalElement() const -> T {
    T fresh = oob_slot_;
    fresh.ResetToDefault();
    return fresh;
  }

  mutable T oob_slot_;
  std::vector<T> data_;

  friend class UnpackedArrayRef<T>;
};

// LRM 7.6: an assignment to an unpacked slice is a single assignment to the
// entire slice. The proxy holds a non-owning pointer back to the source array
// plus the (offset, count) window. X / Z offset makes `Clone()` return a
// wholly-default sub-array and `operator=` a no-op; partial-OOB behaves
// per-element. Move-only so the proxy cannot outlive the source it aliases.
template <typename T>
class UnpackedArrayRef {
 public:
  UnpackedArrayRef(
      UnpackedArray<T>& base, const PackedArray& offset, std::uint32_t count)
      : base_(&base),
        offset_unknown_(offset.HasUnknown()),
        offset_(offset_unknown_ ? 0 : offset.ToInt64()),
        count_(count) {
  }
  UnpackedArrayRef(const UnpackedArrayRef&) = delete;
  auto operator=(const UnpackedArrayRef&) -> UnpackedArrayRef& = delete;
  UnpackedArrayRef(UnpackedArrayRef&&) noexcept = default;
  auto operator=(UnpackedArrayRef&&) noexcept -> UnpackedArrayRef& = default;
  ~UnpackedArrayRef() = default;

  [[nodiscard]] auto Clone() const -> UnpackedArray<T> {
    const T canonical = base_->MakeCanonicalElement();
    UnpackedArray<T> result(canonical);
    if (offset_unknown_) {
      result.data_.assign(count_, canonical);
      return result;
    }
    result.data_.reserve(count_);
    const auto size = static_cast<std::int64_t>(base_->data_.size());
    for (std::uint32_t i = 0; i < count_; ++i) {
      const auto pos = offset_ + static_cast<std::int64_t>(i);
      const bool in_bounds = pos >= 0 && pos < size;
      result.data_.push_back(
          in_bounds ? base_->data_[static_cast<std::size_t>(pos)] : canonical);
    }
    return result;
  }

  auto operator=(const UnpackedArray<T>& value) -> UnpackedArrayRef& {
    if (offset_unknown_) return *this;
    const auto size = static_cast<std::int64_t>(base_->data_.size());
    for (std::uint32_t i = 0; i < count_; ++i) {
      const auto pos = offset_ + static_cast<std::int64_t>(i);
      if (pos < 0 || pos >= size) continue;
      base_->data_[static_cast<std::size_t>(pos)] = value.data_[i];
    }
    return *this;
  }

 private:
  UnpackedArray<T>* base_;
  bool offset_unknown_;
  std::int64_t offset_;
  std::uint32_t count_;
};

// LRM 21.2.1.6 aggregate format. Per the per-type Formatter trait the
// container walks its own elements and recursively defers each element to
// `Format(spec, MakeFormatArg(elem))`. Aggregates only ever carry
// `kAssignmentPattern`, which rewrites to `kDecimal` at the integral leaf
// and never reaches a context-bound kind (`%t` is rejected on aggregate
// operands upstream), so no `FormatContext` is threaded through. Multi-dim
// and mixed-container nesting (`int arr[3][]`) fall out because every
// nested element type carries its own Formatter.
template <typename T>
struct Formatter<UnpackedArray<T>> {
  static auto Format(const FormatSpec& spec, const UnpackedArray<T>& value)
      -> std::string {
    std::string out = "'{";
    for (std::size_t i = 0; i < value.Size(); ++i) {
      if (i != 0) {
        out += ", ";
      }
      out += lyra::value::Format(spec, MakeFormatArg(value.RawAt(i)));
    }
    out += "}";
    return out;
  }
};

}  // namespace lyra::value
