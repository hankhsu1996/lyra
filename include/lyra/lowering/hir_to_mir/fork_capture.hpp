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

// Collects a fork branch's by-reference captures as the branch body is lowered
// (LRM 6.21: the process activation outlives the branch, so the shared storage
// stays live). Installed on ProcessLoweringState for the duration of one
// branch: a body reference that resolves above the branch boundary is captured
// here and rewritten on the spot to a binding local to the branch, so the
// closure carries its captures by the time it is built -- never by a post-hoc
// rewrite. `root` receives the binding vars; `outer` (the fork's enclosing
// scope) receives the captured lvalues.
class ForkCaptureSink {
 public:
  ForkCaptureSink(
      std::uint32_t boundary_depth, ProceduralScopeLoweringState& root,
      ProceduralScopeLoweringState& outer)
      : boundary_depth_(boundary_depth), root_(&root), outer_(&outer) {
  }

  [[nodiscard]] auto BoundaryDepth() const -> std::uint32_t {
    return boundary_depth_;
  }

  // Captures the enclosing variable `outer_var` (declared `decl_depth` scopes
  // deep) and returns the body reference -- a binding local to the branch root,
  // read at `current_depth`.
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
    const mir::ProceduralVarId binding = root_->AddProceduralVar(
        mir::ProceduralVarDecl{
            .name = "_lyra_fork_cap_" + std::to_string(captures_.size()),
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
  ProceduralScopeLoweringState* root_;
  ProceduralScopeLoweringState* outer_;
  std::vector<mir::Capture> captures_;
  std::vector<Entry> captured_;
};

}  // namespace lyra::lowering::hir_to_mir
