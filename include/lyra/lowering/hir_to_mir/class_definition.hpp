#pragma once

#include <functional>
#include <map>
#include <span>
#include <string>

#include "lyra/hir/body_of_unit.hpp"
#include "lyra/lowering/hir_to_mir/design_instantiations.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/declared_class.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"

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

// States, as constants of class `id` of the design's root unit, the
// construction table of every body `instantiations` names that an object is
// made of below `own` -- the bodies the root makes at its own instantiations,
// in its table's order -- and that of the root itself, which it answers with.
// Each body's table is stated once however many instantiations make it, and an
// entry holds that body's object entry, at the type `entry_types` gives its
// unit's entry, with its prototype erased.
auto StateConstructionTables(
    mir::CompilationUnit& unit, mir::ClassId id, mir::Class& cls,
    std::span<const hir::BodyOfUnit> own,
    const DesignInstantiations& instantiations,
    const std::map<std::string, mir::TypeId, std::less<>>& entry_types)
    -> mir::ClassConstantRef;

}  // namespace lyra::lowering::hir_to_mir
