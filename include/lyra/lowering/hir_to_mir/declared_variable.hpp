#pragma once

#include <optional>
#include <string>

#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {
class CompilationUnit;
struct Block;
}  // namespace lyra::mir

namespace lyra::lowering::hir_to_mir {

// A variable of a body's own lifetime, as the source declared it: a local, a
// formal, a function's result. Where the body can wait, any process that can
// reach the variable may wait on it (LRM 9.4.2) -- the branches the body forks
// and the subroutines it lends it to all can (LRM 6.21, 13.5.2) -- so it lives
// in a cell that reports every write, as a design variable does. Where the
// body cannot, nothing is there to be told, and it is the value itself.
struct DeclaredVariable {
  mir::LocalId local;
  // The cell, or the value where the body cannot wait.
  mir::TypeId type;
};

// Declares the variable in `block`. A cell is built where the declaration is
// reached and holds nothing until it is initialized; a value comes into being
// with its first value.
auto DeclareVariable(
    mir::CompilationUnit& unit, CallableBindings& bindings, mir::Block& block,
    BindingOriginId origin, const std::optional<std::string>& name,
    mir::TypeId value_type, bool body_can_wait) -> DeclaredVariable;

// Starts the variable at `value`. A declaration reached again begins the
// variable afresh in the one cell, which is what initializing an installed cell
// does.
auto InitializeVariable(
    mir::CompilationUnit& unit, mir::Block& block,
    const DeclaredVariable& variable, mir::ExprId value) -> mir::Stmt;

// A read of what the variable holds.
auto ReadVariable(
    mir::CompilationUnit& unit, mir::Block& block,
    const DeclaredVariable& variable) -> mir::ExprId;

}  // namespace lyra::lowering::hir_to_mir
