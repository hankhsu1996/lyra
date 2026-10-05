#pragma once

#include <span>

#include "lyra/mir/class.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// The address of the definition every object of `of` carries, as a value of
// `block`: the constant the unit declaring the class emits, which a scope's
// constructor hands the part every scope shares.
auto BuildDefinitionRead(
    const mir::CompilationUnit& unit, mir::Block& block,
    const mir::DeclaredClassRef& of) -> mir::ExprId;

// Each function below states the definition of `cls` -- a class of `unit`,
// every body it declares complete -- as the constant its unit emits, adding to
// `cls` the constants and bodies that takes. Which one applies follows from
// whether instances of the class stand in the design hierarchy.

// A class of the source language, or what a unit published of the object one
// of its scopes is: what it extends, and nothing else.
void StateNamedClassDefinition(
    const mir::CompilationUnit& unit, mir::Class& cls);

// A class an instance of the design hierarchy is built of: its timescale, and
// `exports`, the names it answers a foreign caller with (LRM 35.5.3). Each
// body is handed the scope.
void StateScopeDefinition(
    const mir::CompilationUnit& unit, mir::ClassId id, mir::Class& cls,
    std::span<const mir::NamedCallable> exports);

}  // namespace lyra::lowering::hir_to_mir
