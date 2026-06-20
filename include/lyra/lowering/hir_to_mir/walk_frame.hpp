#pragma once

#include <cstdint>
#include <optional>

#include "lyra/base/internal_error.hpp"
#include "lyra/lowering/hir_to_mir/procedural_depth.hpp"
#include "lyra/mir/procedural_var.hpp"
#include "lyra/mir/structural_hops.hpp"

namespace lyra::mir {
struct StructuralScope;
struct ProceduralScope;
}  // namespace lyra::mir

namespace lyra::lowering::hir_to_mir {

class CaptureSink;

// Singly-linked node carrying a structural scope's parent chain so a leaf
// reference can read the declared type of a `StructuralVar` at `hops > 0`.
// Each node lives on the stack of the `StructuralScopeLowerer::Run` that
// pushed it; the chain extends one node per scope opened during traversal.
// Mirrors `RenderContext`'s `structural_parent_` link so lowering and render
// reach the same `mir::StructuralVarDecl` for any (hops, var) reference.
struct ScopeChainNode {
  const mir::StructuralScope* scope;
  const ScopeChainNode* parent;
};

// How a HIR `LoopVarRef` resolves at the structural-scope dispatcher. The two
// values correspond to the two structural contexts a constructor expression
// can sit in: a for-generate header (where the loop variable lowers to the
// constructor's procedural induction var) and a generate-control / continuous
// assign (where the loop variable lowers to a structural param on the
// constructed child object). Meaningful only in `StructuralScopeLowerer::
// LowerExpr`; ignored by the procedural-side dispatcher.
enum class LoopVarLoweringMode : std::uint8_t {
  kProceduralInduction,
  kStructuralParam,
};

// Per-recursion traversal context for HIR-to-MIR. Carried by value through
// every dispatcher method and per-kind handler. Walk-invariant facts (the
// compilation unit being constructed, builtins) live on the Lowerer class,
// not here. WalkFrame holds only state that genuinely changes from one
// recursion to the next.
//
// Handlers reach nested write targets through `frame.current_*_scope->Add...`;
// writes to the root output go through the unit's own append-only API reached
// via `module.Unit().AddType(...)` for synthesized types,
// `module.Unit().AllocateDeferredCheckSiteId()` for deferred-check site ids.
struct WalkFrame {
  // The current structural-scope write target. Set when a structural-scope
  // task constructs its scope and entered via `WithStructuralScope`. Null
  // outside structural-scope handlers.
  mir::StructuralScope* current_structural_scope = nullptr;

  // Outer structural scopes reached by climbing `parent` links, in the same
  // order as the lowerer `parent_` chain. Populated by `WithStructuralScope`
  // when a `StructuralScopeLowerer::Run` opens a new scope. Read via
  // `StructuralScopeAtHops` to resolve a `StructuralVar` reference at
  // `hops > 0`.
  const ScopeChainNode* outer_structural_scopes = nullptr;

  // The current procedural-scope write target. Set when a walker opens a new
  // procedural scope (process body, nested block body, fork branch body,
  // closure body) and entered via `WithProceduralScope`. Null outside a
  // procedural context.
  mir::ProceduralScope* current_procedural_scope = nullptr;

  // The current procedural-block-nesting depth measured from the process root
  // (depth 0). Increments when a walker descends into a nested
  // `mir::ProceduralScope`. Used by `LowerProceduralVarRefExpr` to compute the
  // hop count from the reading site back to the declaration depth.
  ProceduralDepth procedural_depth{};

  // The active closure capture sink while a closure body is being lowered. A
  // procedural-var reference whose declaration sits above the sink's boundary
  // routes through it so the closure's captures are composed as the body is
  // built. Null outside a closure body.
  CaptureSink* active_closure = nullptr;

  // The iterator-index binding active while lowering an array-method
  // with-clause body (LRM 7.12.4). Read by the `item.index` reference
  // lowering. Empty outside a with-clause body.
  std::optional<mir::ProceduralVarId> active_index_binding;

  // The `self` binding (mir.md invariant 11) at the root of the current body.
  // Set at body entry (process / subroutine / constructor / closure) and
  // unchanged through the body walk; updated on entry into a fresh body to
  // that body's own self id. Empty before any body has been entered. The
  // declaration depth records where the binding was declared so a deeper
  // reader can compute hops as `current_depth - self_decl_depth`.
  std::optional<mir::ProceduralVarId> self_binding;
  ProceduralDepth self_decl_depth{};

  // Whether the enclosing callable body suspends -- a process, a task, or a
  // closure synthesized to run as a separate concurrent process (fork branch).
  // Set at body entry and inherited unchanged through nested procedural scopes;
  // a closure entry re-establishes it from the closure's own `is_coroutine`.
  // The `ReturnStmt` lowering reads this to fill the stmt's
  // `is_coroutine_return` attribute (a C++ render hint, ignored by LIR / LLVM).
  bool is_coroutine_body = false;

  // How a `LoopVarRef` resolves in `StructuralScopeLowerer::LowerExpr`. Set
  // by the caller before dispatching a constructor expression. The default
  // (`kStructuralParam`) matches the common generate-control / continuous
  // assign context; for-generate header lowering switches to
  // `kProceduralInduction`. Ignored by `ProcessLowerer::LowerExpr`.
  LoopVarLoweringMode loop_var_mode = LoopVarLoweringMode::kStructuralParam;

  // True while lowering an assignment's left-hand side. A queue
  // element-select dispatches to its write-side callee (LRM 7.10.1
  // append-aware) under this flag; the index and other rvalue
  // sub-expressions clear it. Read only by the queue element-select
  // lowering -- the other selector forms have explicit LHS entry points.
  bool is_lvalue_target = false;

  // Pushes `scope` as the current structural scope and links the previous
  // `current_structural_scope` into the outer chain through `chain_node`,
  // which the caller stack-allocates so its lifetime spans the descent.
  [[nodiscard]] auto WithStructuralScope(
      mir::StructuralScope* scope, ScopeChainNode& chain_node) const
      -> WalkFrame {
    chain_node.scope = current_structural_scope;
    chain_node.parent = outer_structural_scopes;
    WalkFrame next = *this;
    next.current_structural_scope = scope;
    next.outer_structural_scopes = current_structural_scope != nullptr
                                       ? &chain_node
                                       : outer_structural_scopes;
    return next;
  }

  // Resolves the structural scope at `hops`: 0 yields the current one, N
  // walks N steps through `outer_structural_scopes`.
  [[nodiscard]] auto StructuralScopeAtHops(mir::StructuralHops hops) const
      -> const mir::StructuralScope& {
    if (hops.value == 0) {
      if (current_structural_scope == nullptr) {
        throw InternalError(
            "WalkFrame::StructuralScopeAtHops: no current structural scope");
      }
      return *current_structural_scope;
    }
    const ScopeChainNode* node = outer_structural_scopes;
    for (std::uint32_t step = 1; step < hops.value && node != nullptr; ++step) {
      node = node->parent;
    }
    if (node == nullptr || node->scope == nullptr) {
      throw InternalError(
          "WalkFrame::StructuralScopeAtHops: hops exceed chain depth");
    }
    return *node->scope;
  }

  [[nodiscard]] auto WithProceduralScope(mir::ProceduralScope* scope) const
      -> WalkFrame {
    WalkFrame next = *this;
    next.current_procedural_scope = scope;
    return next;
  }

  [[nodiscard]] auto Deeper() const -> WalkFrame {
    WalkFrame next = *this;
    next.procedural_depth = procedural_depth.Inner();
    return next;
  }

  [[nodiscard]] auto WithClosure(CaptureSink* sink) const -> WalkFrame {
    WalkFrame next = *this;
    next.active_closure = sink;
    return next;
  }

  [[nodiscard]] auto WithIndexBinding(mir::ProceduralVarId id) const
      -> WalkFrame {
    WalkFrame next = *this;
    next.active_index_binding = id;
    return next;
  }

  [[nodiscard]] auto WithLoopVarMode(LoopVarLoweringMode mode) const
      -> WalkFrame {
    WalkFrame next = *this;
    next.loop_var_mode = mode;
    return next;
  }

  [[nodiscard]] auto WithSelfBinding(
      mir::ProceduralVarId id, ProceduralDepth decl_depth) const -> WalkFrame {
    WalkFrame next = *this;
    next.self_binding = id;
    next.self_decl_depth = decl_depth;
    return next;
  }

  [[nodiscard]] auto WithCoroutineBody(bool is_coroutine) const -> WalkFrame {
    WalkFrame next = *this;
    next.is_coroutine_body = is_coroutine;
    return next;
  }

  [[nodiscard]] auto WithLvalueTarget(bool is_target) const -> WalkFrame {
    WalkFrame next = *this;
    next.is_lvalue_target = is_target;
    return next;
  }
};

}  // namespace lyra::lowering::hir_to_mir
