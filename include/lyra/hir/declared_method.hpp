#pragma once

#include <optional>
#include <string>
#include <variant>

#include "lyra/base/overloaded.hpp"
#include "lyra/hir/class_id.hpp"
#include "lyra/hir/covergroup_ref.hpp"
#include "lyra/hir/randomization_hook.hpp"
#include "lyra/hir/subroutine_prototype.hpp"

namespace lyra::hir {

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

// A behavior (LRM 8.20) as anything outside the class introducing it names it:
// by that class and the name it was introduced under. Its position is found
// where that class's declaration is read.
struct NamedBehavior {
  ClassId introducer;
  std::string name;

  auto operator==(const NamedBehavior&) const -> bool = default;
};

// A method overriding a virtual method of a class this one extends, whether or
// not the source wrote `virtual` (LRM 8.20), naming the behavior it overrides
// by the class that introduced it and the name it was introduced under: the
// method's own for a method, and for the method making an embedded covergroup
// the one making the first covergroup of its lineage (LRM 19.4.1). The class
// declaring the method finds the introducer once along its own lineage, and
// every unit reading the method reads that answer rather than walking the
// lineage again; a position in the introducer is found where its declaration
// is read, which the declaration of this one may not do. A pure one restates
// the method and gives it no body, so the position keeps what the class
// extended put there.
struct OverridesVirtual {
  bool is_pure = false;
  NamedBehavior overridden;

  auto operator==(const OverridesVirtual&) const -> bool = default;
};

// What a call to a method is made on and who decides the body it runs, as the
// front end settled it for the class declaring the method. It is the front
// end's answer rather than one a reader works out, because what a name
// overrides follows from every name the classes above declare, which only the
// front end sees.
using MethodDispatch = std::variant<
    TypeAssociated, NotVirtual, IntroducesVirtual, OverridesVirtual>;

// Whether the method leaves its behavior to a class extending this one (LRM
// 8.21), and so has no body.
[[nodiscard]] inline auto IsLeftAbstract(const MethodDispatch& dispatch)
    -> bool {
  return std::visit(
      Overloaded{
          [](const TypeAssociated&) { return false; },
          [](const NotVirtual&) { return false; },
          [](const IntroducesVirtual& introduces) {
            return introduces.is_pure;
          },
          [](const OverridesVirtual& overrides) { return overrides.is_pure; }},
      dispatch);
}

// One method a class declares, as every unit naming the class reads it.
// The prototype is what a call to the method is made through -- its name, the
// call protocol, each formal's direction and type, and the result -- which is
// also what a body overriding the method takes and completes with.
struct DeclaredMethod {
  SubroutinePrototype prototype;
  MethodDispatch dispatch;
  // The hook this method takes the place of, in the class and in each class
  // extending it that declares none of its own (LRM 18.6.2), as the front end
  // resolved it.
  std::optional<RandomizationHook> replaces;
  // The covergroup the method makes, where it is the method a class embedding
  // a covergroup makes it by (LRM 19.4.1), which no method of the source
  // stands for.
  std::optional<CovergroupId> makes;

  auto operator==(const DeclaredMethod&) const -> bool = default;
};

}  // namespace lyra::hir
