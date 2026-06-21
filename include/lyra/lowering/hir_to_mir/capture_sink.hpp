#pragma once

#include <optional>
#include <string>
#include <utility>
#include <vector>

#include "lyra/lowering/hir_to_mir/block_depth.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::mir {
class CompilationUnit;
}  // namespace lyra::mir

namespace lyra::lowering::hir_to_mir {

// One captured variable, identified while a closure body is lowered. The sink
// has allocated `binding` in the body and rewritten the body's references to
// read it; `source` is the enclosing-scope expression bound into the closure --
// a plain read of the captured variable for a by-value snapshot, or a
// reference-construct (`Ref<T>`) for a by-reference alias. The caller binds
// `source` to `binding` uniformly; the snapshot-versus-alias choice is already
// realized in `binding`'s type and in `source`.
struct CaptureRequest {
  mir::LocalId var;
  BlockDepth decl_depth;
  mir::LocalId binding;
  mir::ExprId source;
};

// Identity-only capture collector for a closure body. Installed on
// ProcessLowerer for the duration of a single closure construction: during the
// body's lowering, any procedural-var reference that resolves above the sink's
// boundary is rerouted here -- a binding in the body scope is allocated
// (deduplicated by identity), the reference is rewritten to read that binding,
// and a CaptureRequest is recorded. A capture declared at `by_value_depth` is a
// by-value snapshot (a value-typed binding); any other capture aliases the
// enclosing storage, so its binding is a `RefType` and its `source` is a
// reference-construct, and the body's reads / writes route through the live
// cell (LRM 6.21). A fork branch passes its block_item_declaration depth (those
// locals snapshot; deeper enclosing variables alias); the `$sscanf` / `$fscanf`
// sync IIFE and the with-clause body pass no by-value depth (every capture
// aliases).
class CaptureSink {
 public:
  CaptureSink(
      BlockDepth boundary_depth, mir::Block& body, mir::Block& outer,
      mir::CompilationUnit& unit,
      std::optional<BlockDepth> by_value_depth = std::nullopt)
      : boundary_depth_(boundary_depth),
        body_(&body),
        outer_(&outer),
        unit_(&unit),
        by_value_depth_(by_value_depth) {
  }

  [[nodiscard]] auto BoundaryDepth() const -> BlockDepth {
    return boundary_depth_;
  }

  // Capture `var` (declared at `decl_depth`) and return the body-side reference
  // -- a binding local to the body, read at `current_depth`.
  auto Capture(
      mir::LocalId var, BlockDepth decl_depth, mir::TypeId type,
      BlockDepth current_depth) -> mir::LocalRef {
    mir::LocalId binding = FindOrCreate(var, decl_depth, type);
    return mir::LocalRef{
        .hops = current_depth - boundary_depth_, .var = binding};
  }

  auto TakeRequests() -> std::vector<CaptureRequest> {
    return std::move(requests_);
  }

 private:
  auto FindOrCreate(mir::LocalId var, BlockDepth decl_depth, mir::TypeId type)
      -> mir::LocalId {
    for (const auto& request : requests_) {
      if (request.decl_depth == decl_depth && request.var == var) {
        return request.binding;
      }
    }
    // The cell read is evaluated one scope outside the body (where the capture
    // is spawned), so its hops run from there down to the variable.
    const mir::ExprId cell = outer_->AddExpr(
        mir::Expr{
            .data =
                mir::LocalRef{
                    .hops = boundary_depth_.Outer() - decl_depth, .var = var},
            .type = type});
    const std::string name = "_lyra_cap_" + std::to_string(requests_.size());
    mir::LocalId binding{};
    mir::ExprId source{};
    if (by_value_depth_ == decl_depth) {
      // By-value snapshot: the binding owns a copy; the source is the read.
      binding = body_->AddLocal(mir::LocalDecl{.name = name, .type = type});
      source = cell;
    } else {
      // By-reference alias: the source constructs a reference to the cell and
      // the binding holds that reference (its `RefType`).
      source = BuildReferenceArg(*unit_, *outer_, cell, type);
      binding = body_->AddLocal(
          mir::LocalDecl{.name = name, .type = outer_->GetExpr(source).type});
    }
    requests_.push_back(
        CaptureRequest{
            .var = var,
            .decl_depth = decl_depth,
            .binding = binding,
            .source = source});
    return binding;
  }

  BlockDepth boundary_depth_;
  mir::Block* body_;
  mir::Block* outer_;
  mir::CompilationUnit* unit_;
  std::optional<BlockDepth> by_value_depth_;
  std::vector<CaptureRequest> requests_;
};

}  // namespace lyra::lowering::hir_to_mir
