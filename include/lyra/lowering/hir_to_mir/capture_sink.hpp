#pragma once

#include <string>
#include <utility>
#include <vector>

#include "lyra/lowering/hir_to_mir/procedural_depth.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/procedural_var.hpp"

namespace lyra::lowering::hir_to_mir {

// One captured variable, identified while a closure body is lowered. The sink
// has already allocated `binding` in the body and rewritten the body's
// references to read it; `source` reaches the captured variable from the
// enclosing scope and is the same expression whichever way the caller uses it
// (aliased for a by-reference capture, read for a by-value one). `decl_depth`
// is the captured variable's declaration depth -- the metadata a caller reads
// to decide the capture kind. Turning a request into a `mir::Capture` is the
// caller's job; the sink never decides by value versus by reference.
struct CaptureRequest {
  mir::ProceduralVarId var;
  ProceduralDepth decl_depth;
  mir::ProceduralVarId binding;
  mir::ExprId source;
};

// Identity-only capture collector for a closure body. Installed on
// ProcessLoweringState for the duration of a single closure construction:
// during the body's lowering, any procedural-var reference that resolves above
// the sink's boundary is rerouted here -- a binding in the body scope is
// allocated (deduplicated by identity), the reference is rewritten to read that
// binding, and a CaptureRequest is recorded. The sink never inspects how the
// reference is used or which storage the capture should hold; that is the
// caller's concern. A fork branch snapshots its block_item_declarations by
// value and aliases enclosing-process variables by reference; the
// `$sscanf` / `$fscanf` sync IIFE aliases everything by reference -- each reads
// the requests and assembles its own captures.
class CaptureSink {
 public:
  CaptureSink(
      ProceduralDepth boundary_depth, ProceduralScopeLoweringState& body,
      ProceduralScopeLoweringState& outer)
      : boundary_depth_(boundary_depth), body_(&body), outer_(&outer) {
  }

  [[nodiscard]] auto BoundaryDepth() const -> ProceduralDepth {
    return boundary_depth_;
  }

  // Capture `var` (declared at `decl_depth`) and return the body-side reference
  // -- a binding local to the body, read at `current_depth`.
  auto Capture(
      mir::ProceduralVarId var, ProceduralDepth decl_depth, mir::TypeId type,
      ProceduralDepth current_depth) -> mir::ProceduralVarRef {
    mir::ProceduralVarId binding = FindOrCreate(var, decl_depth, type);
    return mir::ProceduralVarRef{
        .hops = current_depth - boundary_depth_, .var = binding};
  }

  auto TakeRequests() -> std::vector<CaptureRequest> {
    return std::move(requests_);
  }

 private:
  auto FindOrCreate(
      mir::ProceduralVarId var, ProceduralDepth decl_depth, mir::TypeId type)
      -> mir::ProceduralVarId {
    for (const auto& request : requests_) {
      if (request.decl_depth == decl_depth && request.var == var) {
        return request.binding;
      }
    }
    const mir::ProceduralVarId binding = body_->AddProceduralVar(
        mir::ProceduralVarDecl{
            .name = "_lyra_cap_" + std::to_string(requests_.size()),
            .type = type});
    // The source is evaluated one scope outside the body (where the capture is
    // spawned), so its hops run from there down to the variable.
    const mir::ExprId source = outer_->AddExpr(
        mir::Expr{
            .data =
                mir::ProceduralVarRef{
                    .hops = boundary_depth_.Outer() - decl_depth, .var = var},
            .type = type});
    requests_.push_back(
        CaptureRequest{
            .var = var,
            .decl_depth = decl_depth,
            .binding = binding,
            .source = source});
    return binding;
  }

  ProceduralDepth boundary_depth_;
  ProceduralScopeLoweringState* body_;
  ProceduralScopeLoweringState* outer_;
  std::vector<CaptureRequest> requests_;
};

}  // namespace lyra::lowering::hir_to_mir
