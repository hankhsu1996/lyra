#pragma once

#include <cstddef>
#include <memory>
#include <utility>

#include "lyra/value/concepts.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::value {

// Runtime representation of a handle to an object the simulator owns (LRM 8.3,
// and the LRM 9.7 `process` a handle names): the object's address together with
// a share of its ownership, with the object's own type erased.
//
// A handle's value is both of those, so every copy copies both and the object
// lives as long as any handle names it.
//
// Erasing the type lets one representation serve every object a handle can name
// -- a class object, and the process node LRM 9.7 hands back -- with no base
// class in common. The control block keeps the real deleter, so releasing the
// last share is exact; an entry that knows the concrete type recovers it, and
// that entry is the only place the type is needed.
//
// Its operator surface is LRM Table 11-1's "Any data type" row, as a chandle's
// is: equality yields a 1-bit `PackedArray`, `===` / `!==` carry the same
// meaning as `==` / `!=`, and no relational operator is defined. Identity is
// the address, which is the object's identity because SystemVerilog classes are
// singly inherited (LRM 8.13) -- a handle to a base and a handle to the derived
// object hold the same address.
class ManagedRef {
 public:
  ManagedRef() = default;

  // `null` is the only literal a handle admits (LRM 8.4), and it arrives as the
  // host `nullptr` a null literal renders to. Implicit so a comparison against
  // `null` and a store of `null` each bind without a cast. The parameter is a
  // type tag carrying no value, so it stays unnamed.
  // NOLINTNEXTLINE(google-explicit-constructor,readability-named-parameter)
  ManagedRef(std::nullptr_t) {
  }

  explicit ManagedRef(std::shared_ptr<void> share) : share_(std::move(share)) {
  }

  // The share this handle holds, which an entry knowing the object's type casts
  // back to a typed owner before calling into it.
  [[nodiscard]] auto Share() const -> const std::shared_ptr<void>& {
    return share_;
  }

  // LRM 11.4.5 `==` / `!=` (Any data type), compared as object identity.
  [[nodiscard]] auto operator==(const ManagedRef& o) const -> PackedArray {
    return PackedArray::Bit(share_.get() == o.share_.get());
  }
  [[nodiscard]] auto operator!=(const ManagedRef& o) const -> PackedArray {
    return PackedArray::Bit(share_.get() != o.share_.get());
  }

  // LRM 11.4.5: `===` / `!==` on a handle carry the same semantics as `==` /
  // `!=`. Coincides with `IsBitIdentical` because a handle's value is which
  // object it names.
  [[nodiscard]] auto CaseEqual(const ManagedRef& o) const -> PackedArray {
    return PackedArray::Bit(share_.get() == o.share_.get());
  }

  // LRM 9.4.2 update event predicate (engine change-detection hook).
  [[nodiscard]] auto IsBitIdentical(const ManagedRef& o) const -> bool {
    return share_.get() == o.share_.get();
  }

  // A handle names an object and has no X/Z plane.
  [[nodiscard]] static auto HasUnknown() -> bool {
    return false;
  }

  [[nodiscard]] static auto IsUnknown() -> PackedArray {
    return PackedArray::Bit(false);
  }

  // LRM 8.4 / Table 6-7: an uninitialized handle is null. Dropping the share is
  // what releases the object if this was the last handle to it.
  auto ResetToDefault() -> void {
    share_.reset();
  }

  // LRM 8.4: a handle tested for a boolean value is 0 when null, 1 otherwise.
  explicit operator bool() const {
    return share_ != nullptr;
  }

 private:
  std::shared_ptr<void> share_;
};

static_assert(LyraValue<ManagedRef>);
static_assert(CaseEqualComparable<ManagedRef>);
static_assert(Defaultable<ManagedRef>);
static_assert(!Ordered<ManagedRef>);
static_assert(!WildcardComparable<ManagedRef>);

}  // namespace lyra::value
