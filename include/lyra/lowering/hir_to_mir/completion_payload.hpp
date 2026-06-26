#pragma once

#include <vector>

#include "lyra/hir/subroutine.hpp"
#include "lyra/lowering/hir_to_mir/module_lowerer.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// The completion-payload component types of a subroutine, in payload order: the
// function return (when the subroutine is a non-void function) followed by each
// `output` / `inout` formal in declaration order. The single source of truth
// the callee body and every call site share, so neither re-derives the layout
// independently (LRM 13.5).
auto CompletionComponentTypes(
    const ModuleLowerer& module, const hir::StructuralSubroutineDecl& decl)
    -> std::vector<mir::TypeId>;

// Normalizes a completion payload by component count: no component is `Void`,
// one is that component's bare type, two or more a tuple of them.
auto NormalizeCompletionPayload(
    mir::CompilationUnit& unit, const std::vector<mir::TypeId>& components)
    -> mir::TypeId;

}  // namespace lyra::lowering::hir_to_mir
