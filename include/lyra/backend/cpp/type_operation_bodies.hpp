#pragma once

#include <functional>
#include <optional>
#include <vector>

#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/code_source.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::backend::cpp {

// The bodies of the items of `self_type`'s implementation of the library's
// traits, which the struct holding it declares item by item and the type's
// own code file defines, with the free functions they call.
struct ImplBodies {
  mir::TypeId self_type;
  std::vector<mir::TypeOperationBody> items;
  std::vector<mir::TypeOperationBody> functions;
};

// What this target needs of one unit: the implementation of each container
// type the unit names, with its items' bodies, and the free functions the
// unit's own code calls, which its code file defines for itself.
struct TypeOperationBodies {
  std::vector<ImplBodies> impls;
  std::vector<mir::TypeOperationBody> functions;
};

// The body built for an operation of a unit's type, rewritten as the passes
// ask, none where no body answers it.
using BodyBuilder = std::function<std::optional<mir::CallableCode>(
    const mir::TypeOperationSource&)>;

// What this target needs of the operations MIR builds for `unit`'s types: for
// every container type (LRM 7.5) the unit names, the body of each item of its
// implementation of the library's traits, which MIR states and the library's
// class for the type calls, so every unit naming it defines the same ones; and
// each free function a file's code calls, followed through the bodies it calls
// in turn, as rustc's collector follows a generic function's calls.
[[nodiscard]] auto TypeOperationBodiesOf(
    const mir::CompilationUnit& unit, const BodyBuilder& build)
    -> TypeOperationBodies;

}  // namespace lyra::backend::cpp
