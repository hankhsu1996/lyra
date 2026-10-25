#pragma once

#include <span>

#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

// `type` with every type parameter in it replaced by the type `arguments`
// names at that parameter's position, as clang substitutes template arguments
// into a dependent type and rustc instantiates a generic type with its generic
// arguments. A type naming no parameter is itself.
[[nodiscard]] auto SubstituteTypeParameters(
    const TypePool& types, TypeId type, std::span<const TypeId> arguments)
    -> TypeId;

// The concrete form of the generic body `code` at `arguments`: every type in it
// substituted, and every operation reached through a parameter's bound -- a
// call of the closure, a keeper's allocation and commit -- made the one the
// type at that parameter answers, as rustc instantiates a generic body's MIR
// for one instance before it is compiled, and as clang names the step
// (`Sema::InstantiateFunctionDefinition`). Every id in it stays what it was.
[[nodiscard]] auto InstantiateDefinition(
    const TypePool& types, const CallableCode& code,
    std::span<const TypeId> arguments) -> CallableCode;

}  // namespace lyra::mir
