#pragma once

#include <compare>
#include <cstdint>
#include <variant>

#include "lyra/base/pool_id.hpp"
#include "lyra/hir/published_callable.hpp"

namespace lyra::hir {

// Which of the virtual methods a class introduces one is, counted in the order
// the class declares them. The class that introduces it and the unit that
// dispatches on it both count it out of the same list, so neither states it to
// the other.
struct PublishedBehaviorId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const PublishedBehaviorId&) const
      -> std::strong_ordering = default;
};

// A method of the class rather than of an object of it (LRM 8.10): a call
// hands it no object, so there is none to decide what runs.
struct TypeAssociated {
  auto operator==(const TypeAssociated&) const -> bool = default;
};

// A method of an object that the object gets no say in: a call runs what the
// class the call names declares (LRM 8.14), so it holds no position in any
// table.
struct NotVirtual {
  auto operator==(const NotVirtual&) const -> bool = default;
};

// The first declaration of a virtual method along the classes this one extends
// (LRM 8.20), which opens a position every class extending this one keeps.
// Pure where it is declared without a body (LRM 8.21), which leaves the
// position for a class extending this one to fill.
struct IntroducesVirtual {
  bool is_pure = false;

  auto operator==(const IntroducesVirtual&) const -> bool = default;
};

// A method overriding the virtual method of its name that a class this one
// extends introduced, whether or not the source wrote `virtual` (LRM 8.20). A
// pure one restates the method and gives it no body, so the position keeps
// what the class extended put there.
struct OverridesVirtual {
  bool is_pure = false;

  auto operator==(const OverridesVirtual&) const -> bool = default;
};

// What a call to a method is made on and who decides the body it runs, as the
// front end settled it for the class declaring the method. It is the front
// end's answer rather than one a reader works out, because what a name
// overrides follows from every name the classes above declare, which only the
// front end sees.
using MethodDispatch = std::variant<
    TypeAssociated, NotVirtual, IntroducesVirtual, OverridesVirtual>;

// One method a class declares, as the unit declaring the class published it.
// The prototype is what a call to the method is made through -- its name, the
// call protocol, each formal's direction and type, and the result -- which is
// also what a body overriding the method or forwarding to it takes and
// completes with.
struct PublishedMethod {
  PublishedCallable prototype;
  MethodDispatch dispatch;

  auto operator==(const PublishedMethod&) const -> bool = default;
};

}  // namespace lyra::hir
