#pragma once

#include <map>
#include <vector>

#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/owned_child_step.hpp"
#include "lyra/hir/procedural_scope.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/constant_id.hpp"

namespace lyra::lowering::hir_to_mir {

// The scopes one scope of the design hierarchy declares, as the class realizing
// it states them (LRM 37.12, 37.85): a record of each, a constant of the unit,
// those the run builds and those it builds nothing for each in the order the
// class lists them, and which record each declaration of the scope is, by its
// own id. A generate construct has the record of the loop
// whose blocks it counts out, or of each block it may build, at the position
// the construct names it. A procedural scope has one only where LRM 37.12
// makes it a scope: a task, a function, a `for` or a `foreach` declaring its
// variables, and a block or a fork the source named or that declares
// something. A subroutine the scope imports from C is one of its tasks and
// functions too (LRM 37.41 detail 6), though it has no procedural scope.
struct ScopeTable {
  std::vector<mir::ConstantId> elaborated;
  std::vector<mir::ConstantId> lexical;
  std::map<hir::InstanceMemberId, mir::ConstantId> instances;
  std::map<hir::GenerateId, std::vector<mir::ConstantId>> generates;
  std::map<hir::ProceduralScopeId, mir::ConstantId> procedural_scopes;
};

// The table of the scopes `scope` declares, as constants of `unit`; `hir`
// is the unit `scope` was lowered in, which holds the imports it declares.
[[nodiscard]] auto StateScopeTable(
    const mir::CompilationUnit& unit, const hir::CompilationUnit& hir,
    const hir::StructuralScope& scope) -> ScopeTable;

}  // namespace lyra::lowering::hir_to_mir
