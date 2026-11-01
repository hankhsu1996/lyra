#pragma once

#include <type_traits>

#include "lyra/support/layout.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/scalar.hpp"

namespace lyra::value {

// A reference that owns nothing: the address of an object that lives for its
// own reasons, or null. It is what a `chandle` holds (LRM 6.14), the pointer a
// foreign call handed back, and what a virtual interface holds (LRM 25.9), the
// instance it names, which lives as long as the simulation.
//
// It is a value type rather than a bare host pointer because a variable keeps
// one in a cell, an associative array keys or holds one, and a class or a
// structure has one as a member, and each of those holds a `LyraValue`. Its
// operator surface is LRM Table 11-1's "Any data type" row: equality yields a
// bit, the value holding no x or z. LRM 6.14 gives `===` / `!==` the same
// meaning as `==` / `!=`, so `CaseEqual` is identity as `IsBitIdentical` is.
// No relational operator is defined on one, so `Ordered` does not hold.
//
// One shape serves every pointee, as an object reference does: the program
// point reaching through one names the class it assumes, so one cell holds the
// same reference whichever class a unit sees it as.
class UnmanagedRef {
 public:
  UnmanagedRef() = default;

  explicit UnmanagedRef(void* p) : p_(p) {
  }

  // The address, as a foreign call takes a chandle (LRM 35.5.6).
  explicit operator void*() const {
    return p_;
  }

  // The object, as the class the program point reaching through the reference
  // assumes. What reaches through one has already refused a null one (LRM
  // 25.9).
  template <typename T>
  [[nodiscard]] auto Deref() const -> T& {
    return *static_cast<T*>(p_);
  }

  // LRM 11.4.5 `==` / `!=` (Any data type), compared as identity.
  [[nodiscard]] auto operator==(const UnmanagedRef& o) const -> Bit {
    return Bit::FromBool(p_ == o.p_);
  }
  [[nodiscard]] auto operator!=(const UnmanagedRef& o) const -> Bit {
    return Bit::FromBool(p_ != o.p_);
  }

  [[nodiscard]] auto CaseEqual(const UnmanagedRef& o) const -> Bit {
    return Bit::FromBool(p_ == o.p_);
  }

  // LRM 9.4.2 update event predicate (engine change-detection hook).
  [[nodiscard]] auto IsBitIdentical(const UnmanagedRef& o) const -> bool {
    return p_ == o.p_;
  }

  [[nodiscard]] static auto HasUnknown() -> bool {
    return false;
  }

  // LRM 6.14: a chandle tested for a boolean value is 0 when null, 1 otherwise,
  // and a virtual interface the same way (LRM 25.9).
  explicit operator bool() const {
    return p_ != nullptr;
  }

  // The address, for ordering the keys of an associative array (LRM 6.14).
  [[nodiscard]] auto Address() const -> const void* {
    return p_;
  }

 private:
  void* p_ = nullptr;
};

// Generated code lays a reference owning nothing out as the scalar it is and
// copies it as its bytes, so the class is that scalar and nothing besides.
static_assert(
    support::Layout{
        .size = sizeof(UnmanagedRef), .align = alignof(UnmanagedRef)} ==
        FactsOf(UnmanagedRefScalar{}).layout &&
    std::is_trivially_copyable_v<UnmanagedRef>);
static_assert(LyraValue<UnmanagedRef>);
static_assert(CaseEqualComparable<UnmanagedRef>);
static_assert(!Ordered<UnmanagedRef>);

}  // namespace lyra::value
