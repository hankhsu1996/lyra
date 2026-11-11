#pragma once

#include <cstdint>
#include <functional>
#include <map>
#include <optional>
#include <span>
#include <string>

#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/dumped_declaration.hpp"
#include "lyra/hir/lowering_ref.hpp"
#include "lyra/hir/owned_child_step.hpp"
#include "lyra/lowering/hir_to_mir/design_instantiations.hpp"
#include "lyra/mir/block_body_ordinal.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/constant_id.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/scope_kind.hpp"

namespace lyra::lowering::hir_to_mir {

// The address of the definition every object of `of` carries, as a value of
// `block`: the constant the unit declaring the class emits.
auto BuildDefinitionRead(
    const mir::CompilationUnit& unit, mir::Builder block, mir::ClassId of)
    -> mir::ExprId;

// States the definition of every class `unit` defines, each the constant its
// unit emits, and the constants of the unit that definition points at. A
// definition reads what the class's whole lineage gives -- the body each
// virtual method runs, each interface class's, each of what the runtime
// library asks of an object -- so it is stated once every class of the unit is
// complete.
void StateObjectDefinitions(mir::CompilationUnit& unit);

// The address, as a value of `block`, of the record of one thing a value dump
// may record of an instance (LRM 21.7.1.2), held as a constant of the unit,
// naming the record of the block, task or function `enclosing` it.
auto DumpEntryAddress(
    const mir::CompilationUnit& unit, mir::Builder block,
    const hir::DumpedDeclaration& declaration,
    std::optional<mir::ConstantId> enclosing) -> mir::ExprId;

// The record of the definition every scope of the unit `hir` comes from
// (LRM 37.10), as a constant of `unit`: the unit's source name and what the
// definition states of itself.
auto DefinitionRecordConstant(
    const mir::CompilationUnit& unit, const hir::CompilationUnit& hir)
    -> mir::ConstantId;

// The record of a scope one scope declares that the run builds, as a constant
// of `unit`: its kind, the name it declares, and whether that name is the one
// LRM 27.6 gives.
auto ElaboratedScopeRecordConstant(
    const mir::CompilationUnit& unit, support::ElaboratedScopeKind kind,
    const std::string& name, bool implicitly_named) -> mir::ConstantId;

// The record of a scope one scope declares that the run builds nothing for, as
// a constant of `unit`: its kind, the name it declares, if any, the record of
// the one it lies inside, if any, and where it stands among the declaring
// class's lexical records.
auto LexicalScopeRecordConstant(
    const mir::CompilationUnit& unit, support::LexicalScopeKind kind,
    const std::optional<std::string>& name,
    std::optional<mir::ConstantId> enclosing, std::uint32_t position)
    -> mir::ConstantId;

// The address, as a value of `block`, of what a package or the compilation
// unit's scope states of itself, held as a constant of the unit.
auto ScopeDeclarationsAddress(
    const mir::CompilationUnit& unit, mir::Builder block,
    const mir::ScopeDeclarations& scope_declarations) -> mir::ExprId;

// The address of `constant`, a constant of the unit, as a value of `block`.
auto ConstantAddressIn(
    const mir::CompilationUnit& unit, mir::Builder block,
    mir::ConstantId constant) -> mir::ExprId;

// The type of the entry that makes a block of a block body (LRM 27), answering
// an owning pointer to the block as the base every scope extends. It takes
// where the block hangs, what it adds to a hierarchical name -- the
// declaration it fills, which the scope declaring the generate construct
// states, with its index -- and the construction table it is built with; a
// block of a loop takes the index it stands at too, as the integer parameter
// the language makes of it (LRM 27.4) and the one value a block is ever
// handed, since it has no parameter ports (Syntax 27-1). Which body a place
// builds is its object's, so the scope building one, the body defining the
// entry and the design root listing it each state the type from here, from
// whether the block is handed its index, the one fact all three know.
auto BlockEntryType(const mir::CompilationUnit& unit, bool handed_its_index)
    -> mir::TypeId;

// Where the block body `body` stands among its unit's, which is the order the
// unit holds them in and the order it publishes their entries in.
[[nodiscard]] inline auto BlockBodyOrdinalOf(hir::BlockBodyId body)
    -> mir::BlockBodyOrdinal {
  return mir::BlockBodyOrdinal{body.value};
}

// States, as constants of the design's root unit, the construction table of
// the root's own lowering `root`, which it answers with, and of every lowering
// `instantiations` holds that an object is made of below it; tables holding
// the same entries are one constant. An instantiation's entry holds the object
// entry of the body that lowering is, at the type `entry_types` gives its
// unit's entry, and a block's the entry that body publishes for the block body
// built there, each with its prototype erased.
auto StateConstructionTables(
    const mir::CompilationUnit& unit, const hir::LoweringRef& root,
    const DesignInstantiations& instantiations,
    const std::map<std::string, mir::TypeId, std::less<>>& entry_types)
    -> mir::ConstantId;

}  // namespace lyra::lowering::hir_to_mir
