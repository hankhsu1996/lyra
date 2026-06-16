#pragma once

#include <cstdint>
#include <optional>

#include "lyra/lowering/hir_to_mir/procedural_depth.hpp"
#include "lyra/mir/procedural_var.hpp"

namespace lyra::mir {
struct StructuralScope;
struct ProceduralScope;
}  // namespace lyra::mir

namespace lyra::lowering::hir_to_mir {

class CaptureSink;

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

  // The current procedural-scope write target. Set when a walker opens a new
  // procedural scope (process body, nested block body, fork branch body,
  // closure body) and entered via `WithProceduralScope`. Null outside a
  // procedural context.
  mir::ProceduralScope* current_procedural_scope = nullptr;

  // The process's static-frame scope. Static-lifetime locals are collected
  // here (LRM 6.21 / 13.3.1), regardless of which nested block declared them.
  // Set once at process / subroutine entry and unchanged through the body
  // walk. Null for a synthesized continuous-assign process that declares no
  // locals.
  mir::ProceduralScope* static_frame_scope = nullptr;

  // The current procedural-block-nesting depth measured from the process root
  // (depth 0). Increments when a walker descends into a nested
  // `mir::ProceduralScope`. Used by `ProcessLowerer::TranslateProceduralVar`
  // to compute the hop count from the reading site back to the declaration
  // depth.
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
  // that body's own self id. Empty before any body has been entered.
  std::optional<mir::ProceduralVarId> self_binding;

  // How a `LoopVarRef` resolves in `StructuralScopeLowerer::LowerExpr`. Set
  // by the caller before dispatching a constructor expression. The default
  // (`kStructuralParam`) matches the common generate-control / continuous
  // assign context; for-generate header lowering switches to
  // `kProceduralInduction`. Ignored by `ProcessLowerer::LowerExpr`.
  LoopVarLoweringMode loop_var_mode = LoopVarLoweringMode::kStructuralParam;

  [[nodiscard]] auto WithStructuralScope(mir::StructuralScope* scope) const
      -> WalkFrame {
    WalkFrame next = *this;
    next.current_structural_scope = scope;
    return next;
  }

  [[nodiscard]] auto WithProceduralScope(mir::ProceduralScope* scope) const
      -> WalkFrame {
    WalkFrame next = *this;
    next.current_procedural_scope = scope;
    return next;
  }

  [[nodiscard]] auto WithStaticFrameScope(mir::ProceduralScope* scope) const
      -> WalkFrame {
    WalkFrame next = *this;
    next.static_frame_scope = scope;
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

  [[nodiscard]] auto WithSelfBinding(mir::ProceduralVarId id) const
      -> WalkFrame {
    WalkFrame next = *this;
    next.self_binding = id;
    return next;
  }
};

}  // namespace lyra::lowering::hir_to_mir
