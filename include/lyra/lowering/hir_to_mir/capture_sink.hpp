#pragma once

#include <cstdint>
#include <string>
#include <utility>
#include <vector>

#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/closure.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/procedural_hops.hpp"
#include "lyra/mir/procedural_var.hpp"

namespace lyra::lowering::hir_to_mir {

// Identity-only capture collector for a closure body. Installed on
// ProcessLoweringState for the duration of a single closure construction:
// during the body's lowering, any procedural-var reference that resolves
// above the sink's boundary is rerouted here -- a binding in the body
// scope is allocated (deduplicated by identity) and the reference is
// rewritten to read that binding. The sink never inspects how the
// reference is used; closure semantics are the caller's concern.
//
// Captures emit as by-reference. Lyra's closure use cases that need this
// mechanism (sync IIFE for `$sscanf` / `$fscanf`, fork branches per LRM
// 6.21) are precisely those where the enclosing storage outlives the body,
// so aliasing is correct for both reads and writes. Capture mechanisms
// that require a snapshot semantic (the NBA RHS, for instance) belong in
// a distinct flow that evaluates the value at submission time and stores
// it independently of any reference to enclosing storage; this sink is
// not that flow.
class CaptureSink {
 public:
  CaptureSink(
      std::uint32_t boundary_depth, ProceduralScopeLoweringState& body,
      ProceduralScopeLoweringState& outer)
      : boundary_depth_(boundary_depth), body_(&body), outer_(&outer) {
  }

  [[nodiscard]] auto BoundaryDepth() const -> std::uint32_t {
    return boundary_depth_;
  }

  // Capture `outer_var` (declared `decl_depth` scopes deep) and return the
  // body-side reference -- a binding local to the body, read at
  // `current_depth`.
  auto Capture(
      mir::ProceduralVarId outer_var, std::uint32_t decl_depth,
      mir::TypeId type, std::uint32_t current_depth) -> mir::ProceduralVarRef {
    mir::ProceduralVarId binding = FindOrCreate(outer_var, decl_depth, type);
    return mir::ProceduralVarRef{
        .hops = mir::ProceduralHops{.value = current_depth - boundary_depth_},
        .var = binding};
  }

  auto TakeCaptures() -> std::vector<mir::Capture> {
    return std::move(captures_);
  }

 private:
  struct Entry {
    std::uint32_t decl_depth;
    mir::ProceduralVarId outer_var;
    mir::ProceduralVarId binding;
  };

  auto FindOrCreate(
      mir::ProceduralVarId outer_var, std::uint32_t decl_depth,
      mir::TypeId type) -> mir::ProceduralVarId {
    for (const auto& entry : captured_) {
      if (entry.decl_depth == decl_depth && entry.outer_var == outer_var) {
        return entry.binding;
      }
    }
    const mir::ProceduralVarId binding = body_->AddProceduralVar(
        mir::ProceduralVarDecl{
            .name = "_lyra_cap_" + std::to_string(captures_.size()),
            .type = type});
    const mir::ExprId target = outer_->AddExpr(
        mir::Expr{
            .data =
                mir::ProceduralVarRef{
                    .hops =
                        mir::ProceduralHops{
                            .value = (boundary_depth_ - 1) - decl_depth},
                    .var = outer_var},
            .type = type});
    captures_.emplace_back(
        mir::ByReferenceCapture{.target = target, .binding = binding});
    captured_.push_back(
        Entry{
            .decl_depth = decl_depth,
            .outer_var = outer_var,
            .binding = binding});
    return binding;
  }

  std::uint32_t boundary_depth_;
  ProceduralScopeLoweringState* body_;
  ProceduralScopeLoweringState* outer_;
  std::vector<mir::Capture> captures_;
  std::vector<Entry> captured_;
};

}  // namespace lyra::lowering::hir_to_mir
