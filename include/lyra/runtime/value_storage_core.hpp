#pragma once

#include <concepts>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::runtime {

// The storage half of a value cell, with no observation attached: it owns one
// value of the declared representation and preserves that representation across
// writes (LRM 4.3 / value-store-discipline). It is the shared substrate under
// an observable signal cell (`Var<T>`, which adds subscriber wakeup) and a
// non-observable procedural cell (`ActivationValueCell<T>`). Non-copyable and
// non-movable: a cell is addressed, so its identity is its address.
template <value::LyraValue T>
class ValueStorageCore {
 public:
  ValueStorageCore() = default;
  ValueStorageCore(const ValueStorageCore&) = delete;
  auto operator=(const ValueStorageCore&) -> ValueStorageCore& = delete;
  ValueStorageCore(ValueStorageCore&&) = delete;
  auto operator=(ValueStorageCore&&) -> ValueStorageCore& = delete;
  ~ValueStorageCore() = default;

  [[nodiscard]] auto Get() const noexcept -> const T& {
    return value_;
  }

  // True once the cell carries its declared representation. A `PackedArray` has
  // an explicit uninitialized state fixed at first write; every other value
  // type is usable immediately.
  [[nodiscard]] auto IsInstalled() const noexcept -> bool {
    if constexpr (std::same_as<T, value::PackedArray>) {
      return !value_.IsUninitialized();
    } else {
      return true;
    }
  }

  // Installs the declared representation and initial contents from a prototype
  // -- the one-time write that fixes the cell's type. Only the prototype's
  // representation is load-bearing; its contents become the initial contents.
  void Install(T prototype) {
    value_ = std::move(prototype);
  }

  // Overwrites the contents with a value already at the declared
  // representation, reporting whether the stored value changed per LRM 9.4.2
  // `===`. A `PackedArray` whose representation does not match the cell's is a
  // missing upstream conversion, surfaced as a compiler bug rather than
  // silently reshaping the cell.
  auto Overwrite(const T& value) -> bool {
    if constexpr (std::same_as<T, value::PackedArray>) {
      if (!value_.SameRepresentation(value)) {
        throw InternalError(
            "ValueStorageCore::Overwrite: stored value's representation does "
            "not match the cell's declared type; a required conversion was not "
            "emitted");
      }
    }
    const bool changed = !value_.IsBitIdentical(value);
    value_ = value;
    return changed;
  }

 private:
  T value_{};
};

}  // namespace lyra::runtime
