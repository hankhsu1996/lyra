#pragma once

#include <optional>
#include <string>
#include <vector>

#include "lyra/hir/callee_interface.hpp"
#include "lyra/hir/param_direction.hpp"
#include "lyra/hir/type_id.hpp"

namespace lyra::hir {

// What a call of a subroutine is made through, whichever unit declares it: its
// name, the interface a call passes arguments and awaits completion through,
// and the result its completion yields, standing as the actual of a formal of
// `result_direction`: an `input` the call takes as a value, or, from the
// function evaluating a formal's default, that formal's (LRM 13.5.3). A method
// of a class (LRM 8.6), a subroutine an instance offers (LRM 25.7) and one a
// namespace declares (LRM 26.3) each have one, and a body overriding a method
// takes the same.
struct SubroutinePrototype {
  std::string name;
  CalleeInterface interface;
  TypeId result_type;
  ParamDirection result_direction = ParamDirection::kInput;

  auto operator==(const SubroutinePrototype&) const -> bool = default;
};

// A subroutine as the scope declaring it lists it: the prototype a call is made
// through, and, at the position of each formal that has a default, which of
// that scope's own subroutines evaluates it, named by `Id`, its position there
// (LRM 13.5.3). The evaluator is the declaring scope's whichever unit calls the
// subroutine, so its position is known where the scope is published.
template <typename Id>
struct DeclaredSubroutine {
  SubroutinePrototype prototype;
  std::vector<std::optional<Id>> defaults;

  auto operator==(const DeclaredSubroutine&) const -> bool = default;
};

}  // namespace lyra::hir
