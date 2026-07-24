#pragma once

#include <optional>
#include <string>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/param_direction.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/enclosing_hops.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::hir {
struct CallExpr;
}  // namespace lyra::hir

namespace lyra::lowering::hir_to_mir {

class ProcessLowerer;

// The callee-interface facts a writeback call needs, recomputed uniformly for
// an intra-unit callee (from its HIR declaration) and a cross-unit one (from
// the by-name reference's recorded signature). An output / inout value rides
// the callee's completion payload; the result type and each argument's formal
// type shape that payload and its writeback assignments. `receiver_hops` is the
// enclosing-scope level of an intra-unit callee's receiver; it is absent for a
// receiver-less cross-unit callee, which takes the engine services in that
// position instead.
struct WritebackCall {
  struct Arg {
    hir::ParamDirection direction = hir::ParamDirection::kInput;
    mir::TypeId formal_type{};
  };
  bool is_task = false;
  std::optional<mir::TypeId> result_type;
  std::vector<Arg> args;
  mir::Callee callee;
  std::optional<mir::EnclosingHops> receiver_hops;
};

// Plans the writeback desugar for a call whose callee carries an output or
// inout formal (LRM 13.5) -- an intra-unit structural subroutine or a
// cross-unit package / `$unit` subroutine. Returns nullopt for a value-only
// call (a system / builtin / imported callee, an all-`input` user callee, or a
// ref-only user callee, none of which copies anything back): those lower
// through the ordinary call path. `call_result_type` is the call's own result
// type (the enclosing expression's type); it is the payload's result component
// unless it is void, in which case the callee is a task or void function that
// contributes none.
auto PlanWritebackCall(
    ProcessLowerer& process, const hir::CallExpr& call,
    mir::TypeId call_result_type) -> std::optional<WritebackCall>;

// Lowers a writeback call in statement position (LRM 13.5): a block wrapper
// that binds the actuals, calls the callee (awaiting a task), and writes each
// output / inout value -- and the function result when `assign_target` is
// present -- back to its actual place. `assign_target` is the lvalue of a `lhs
// = f(...)` statement, or nullopt for a bare call.
auto LowerWritebackCallStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::CallExpr& call, const WritebackCall& plan,
    std::optional<hir::ExprId> assign_target) -> diag::Result<mir::Stmt>;

// Lowers a writeback call in expression position (LRM 13.4): an
// immediately-invoked closure that writes the outputs back and yields the
// function result. Only a nonvoid function reaches here -- a task and a void
// function are statements -- and a function never suspends, so the closure is
// synchronous.
auto LowerWritebackCallExpr(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call,
    const WritebackCall& plan) -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
