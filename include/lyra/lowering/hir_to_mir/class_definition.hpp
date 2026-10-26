#pragma once

#include <functional>
#include <map>
#include <span>
#include <string>

#include "lyra/hir/dumped_declaration.hpp"
#include "lyra/hir/owned_child_step.hpp"
#include "lyra/hir/specialization_ref.hpp"
#include "lyra/lowering/hir_to_mir/design_instantiations.hpp"
#include "lyra/mir/block_body_ordinal.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

// The address of the definition every object of `of` carries, as a value of
// `block`: the constant the unit declaring the class emits, which a scope's
// constructor hands the part every scope shares.
auto BuildDefinitionRead(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ClassId of)
    -> mir::ExprId;

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

// The address, as a value of `block`, of what class `id` states of one thing a
// value dump may record of its instances, and of one named block, task or
// function around such things (LRM 21.7.1.2), each held as a constant of the
// class.
auto DumpEntryAddress(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ClassId id,
    mir::Class& cls, const hir::DumpedDeclaration& declaration) -> mir::ExprId;
auto DumpBlockAddress(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ClassId id,
    mir::Class& cls, const hir::DumpedBlock& dumped) -> mir::ExprId;

// The type of the entry that makes a block of a block body (LRM 27): where the
// block hangs, the construction table it is built with, then, for a block of a
// loop, the index it stands at -- which the language makes an integer
// parameter of the block (LRM 27.4), and the one value a block is ever handed,
// since it has no parameter ports (Syntax 27-1) -- answering an owning pointer
// to the block as the base every scope extends. Which body a place builds is
// its object's, so the scope building one, the body defining the entry and the
// design root listing it each state the type from here, from whether the block
// is handed its index, the one fact all three know.
auto BlockEntryType(mir::CompilationUnit& unit, bool handed_its_index)
    -> mir::TypeId;

// Where the block body `body` stands among its unit's, which is the order the
// unit holds them in and the order it publishes their entries in.
[[nodiscard]] inline auto BlockBodyOrdinalOf(hir::BlockBodyId body)
    -> mir::BlockBodyOrdinal {
  return mir::BlockBodyOrdinal{body.value};
}

// States, as constants of class `id` of the design's root unit, the
// construction table of the root's own specialization `root`, which it answers
// with, and of every specialization `instantiations` holds that an object is
// made of below it. Each specialization's table is stated once however many
// instantiations make it. An instantiation's entry holds the object entry of
// the implementation that specialization is, at the type `entry_types` gives
// its unit's entry, and a block's the entry that implementation publishes for
// the block body built there, each with its prototype erased.
auto StateConstructionTables(
    mir::CompilationUnit& unit, mir::ClassId id, mir::Class& cls,
    const hir::SpecializationRef& root,
    const DesignInstantiations& instantiations,
    const std::map<std::string, mir::TypeId, std::less<>>& entry_types)
    -> mir::ClassConstantRef;

}  // namespace lyra::lowering::hir_to_mir
