#pragma once

#include <string>

#include "lyra/hir/callee_interface.hpp"
#include "lyra/hir/type_id.hpp"

namespace lyra::hir {

// What a call of a subroutine is made through, whichever unit declares it: its
// name, the interface a call passes arguments and awaits completion through,
// and the result its completion yields. A method of a class (LRM 8.6), a
// subroutine an instance offers (LRM 25.7) and one a namespace declares (LRM
// 26.3) each have one, and a body overriding a method or forwarding to it takes
// the same.
struct SubroutinePrototype {
  std::string name;
  CalleeInterface interface;
  TypeId result_type;

  auto operator==(const SubroutinePrototype&) const -> bool = default;
};

}  // namespace lyra::hir
