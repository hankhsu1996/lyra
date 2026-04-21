#pragma once

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/hir/module_body.hpp"
#include "lyra/hir/routine.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/lowering/origin_map.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/constructor.hpp"
#include "lyra/mir/routine.hpp"

namespace lyra::lowering::hir_to_mir {

class DecisionSiteAllocator;

// Compute the return policy for a given return type.
// Value aggregates use sret out-param; scalars and handles use direct.
auto ComputeReturnPolicy(TypeId return_type, const TypeArena& types)
    -> mir::ReturnPolicy;

// Build a frozen FunctionSignature from HIR function metadata.
// Must be called at pre-allocation time (Phase 1).
// Computes return_policy based on return type characteristics.
auto BuildFunctionSignature(
    const hir::Function& function, const SymbolTable& symbol_table,
    const TypeArena& type_arena) -> mir::FunctionSignature;

auto LowerFunctionBody(
    const hir::Function& function, const LoweringInput& input,
    mir::Arena& mir_arena, const DeclView& decl_view, OriginMap* origin_map,
    DecisionSiteAllocator* decision_allocator = nullptr)
    -> Result<mir::Function>;

// Build a frozen FunctionSignature for a task (void return, same param rules).
// Tasks are lowered as mir::Function at the immediate callable level.
auto BuildTaskSignature(
    const hir::Task& task, const SymbolTable& symbol_table, TypeId void_type)
    -> mir::FunctionSignature;

auto LowerTaskBody(
    const hir::Task& task, const LoweringInput& input, mir::Arena& mir_arena,
    const DeclView& decl_view, OriginMap* origin_map,
    DecisionSiteAllocator* decision_allocator = nullptr)
    -> Result<mir::Function>;

// Lower a HIR constructor body into a body-owned MIR Constructor. Reuses the
// generic MIR executable substrate (basic blocks, statements, operands) via
// LowerStatement. Not a user-callable function: no signature, no parameters,
// no return slot.
//
// The `hir_body` argument anchors the body-local HIR arena and constant
// arena. The lowering context is wired to those body-local stores rather
// than relying on `input.hir_arena` already pointing at the correct body;
// this keeps constructor lowering self-anchored alongside functions and
// processes.
auto LowerConstructorBody(
    const hir::ModuleBody& hir_body, const LoweringInput& input,
    mir::Arena& mir_arena, const DeclView& decl_view, OriginMap* origin_map)
    -> Result<mir::Constructor>;

}  // namespace lyra::lowering::hir_to_mir
