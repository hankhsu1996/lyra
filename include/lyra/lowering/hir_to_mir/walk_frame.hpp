#pragma once

#include <cstdint>
#include <optional>

#include "lyra/base/internal_error.hpp"
#include "lyra/lowering/hir_to_mir/block_depth.hpp"
#include "lyra/mir/enclosing_hops.hpp"
#include "lyra/mir/local.hpp"

namespace lyra::mir {
struct Class;
struct Block;
}  // namespace lyra::mir

namespace lyra::lowering::hir_to_mir {

class CaptureSink;
class IterationBindingRegistry;

// Singly-linked node carrying a class's parent chain so a leaf reference
// can read the declared type of a member at `hops > 0`. Each node lives on
// the stack of the `ClassLowerer::Run` that pushed it; the chain extends
// one node per class opened during traversal. This is the construction-side
// half of the shared walk position: the rendering fold's read-only
// `ScopeView` resolves the same (hops, var) reference by climbing its own
// parent link, so both reach the same `mir::MemberDecl`.
struct ScopeChainNode {
  const mir::Class* cls;
  const ScopeChainNode* parent;
};

// How a HIR `LoopVarRef` resolves in the `ClassLowerer` dispatcher. The two
// values correspond to the two structural contexts a constructor expression
// can sit in: a for-generate header (where the loop variable lowers to the
// constructor's induction local) and a generate-control / continuous assign
// (where the loop variable lowers to a structural param on the constructed
// child object). Meaningful only in `ClassLowerer::LowerExpr`; ignored by the
// `ProcessLowerer` dispatcher.
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
// A handler writes nested IR through the frame's current targets; output that
// belongs to the compilation unit as a whole -- a synthesized type, a
// deferred-check site -- is appended to the unit directly, since it has no
// place on a per-recursion frame.
struct WalkFrame {
  // The current class write target. Set when a class-constructing task builds
  // its class and entered via `WithClass`. Null outside class handlers.
  mir::Class* current_class = nullptr;

  // Outer classes reached by climbing `parent` links, in the same order as the
  // lowerer `parent_` chain. Populated by `WithClass` when a
  // `ClassLowerer::Run` opens a new class. Read via `EnclosingClassAtHops` to
  // resolve a member reference at `hops > 0`.
  const ScopeChainNode* outer_classes = nullptr;

  // The current block write target. Set when a walker opens a new block
  // (process body, nested block body, fork branch body, closure body) and
  // entered via `WithBlock`. Null outside a block.
  mir::Block* current_block = nullptr;

  // The current block-nesting depth measured from the body root (depth 0).
  // Increments when a walker descends into a nested `mir::Block`. Used by
  // `LowerProceduralVarRefExpr` to compute the hop count from the reading site
  // back to the declaration depth.
  BlockDepth block_depth{};

  // The capture sink of the closure body being lowered. A local reference whose
  // declaration sits above the sink's boundary routes through it so the
  // closure's captures are composed as the body is built. Null outside a
  // closure body.
  CaptureSink* capture_sink = nullptr;

  // The array-method `with`-clause iteration parameters reachable while
  // lowering a clause body (LRM 7.12.4), keyed by clause identity. An `item` /
  // `item.index` reference resolves through it by identity, so a clause nested
  // in the body can still reach an outer clause's parameter. Null outside a
  // with-clause body.
  IterationBindingRegistry* iteration_bindings = nullptr;

  // The `self` binding at the root of the current body. Set at body entry
  // (process / method / constructor / closure) and unchanged through the
  // body walk; updated on entry into a fresh body to that body's own self
  // id. Empty before any body has been entered. The declaration depth
  // records where the binding was declared so a deeper
  // reader can compute hops as `current_depth - self_decl_depth`.
  std::optional<mir::LocalId> self_binding;
  BlockDepth self_decl_depth{};

  // Whether the enclosing callable body suspends -- a process, a task, or a
  // closure synthesized to run as a separate concurrent process (fork branch).
  // Set at body entry and inherited unchanged through nested blocks;
  // a closure entry re-establishes it from the closure's own `is_coroutine`.
  // The `ReturnStmt` lowering reads this to fill the stmt's
  // `is_coroutine_return` attribute (a C++ render hint, ignored by LIR / LLVM).
  bool is_coroutine_body = false;

  // How a `LoopVarRef` resolves in `ClassLowerer::LowerExpr`. Set
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

  // Pushes `cls` as the current class and links the previous `current_class`
  // into the outer chain through `chain_node`, which the caller stack-allocates
  // so its lifetime spans the descent.
  [[nodiscard]] auto WithClass(
      mir::Class* cls, ScopeChainNode& chain_node) const -> WalkFrame {
    chain_node.cls = current_class;
    chain_node.parent = outer_classes;
    WalkFrame next = *this;
    next.current_class = cls;
    next.outer_classes = current_class != nullptr ? &chain_node : outer_classes;
    return next;
  }

  // Resolves the class at `hops`: 0 yields the current one, N walks N steps
  // through `outer_classes`.
  [[nodiscard]] auto EnclosingClassAtHops(mir::EnclosingHops hops) const
      -> const mir::Class& {
    if (hops.value == 0) {
      if (current_class == nullptr) {
        throw InternalError(
            "WalkFrame::EnclosingClassAtHops: no current class");
      }
      return *current_class;
    }
    const ScopeChainNode* node = outer_classes;
    for (std::uint32_t step = 1; step < hops.value && node != nullptr; ++step) {
      node = node->parent;
    }
    if (node == nullptr || node->cls == nullptr) {
      throw InternalError(
          "WalkFrame::EnclosingClassAtHops: hops exceed chain depth");
    }
    return *node->cls;
  }

  [[nodiscard]] auto WithBlock(mir::Block* block) const -> WalkFrame {
    WalkFrame next = *this;
    next.current_block = block;
    return next;
  }

  [[nodiscard]] auto Deeper() const -> WalkFrame {
    WalkFrame next = *this;
    next.block_depth = block_depth.Inner();
    return next;
  }

  // Descend `extras` block-nesting levels at once. A construct lowered as a
  // chain of nested scopes (a case cascade's if-then-else chain) enters its
  // Nth body `extras = N` levels below the wrapper.
  [[nodiscard]] auto DeeperBy(std::size_t extras) const -> WalkFrame {
    WalkFrame next = *this;
    for (std::size_t i = 0; i < extras; ++i) {
      next.block_depth = next.block_depth.Inner();
    }
    return next;
  }

  [[nodiscard]] auto WithCaptureSink(CaptureSink* sink) const -> WalkFrame {
    WalkFrame next = *this;
    next.capture_sink = sink;
    return next;
  }

  // Threads the registry of active `with`-clause iteration parameters into the
  // body subtree being lowered (LRM 7.12.4).
  [[nodiscard]] auto WithIterationBindings(
      IterationBindingRegistry* registry) const -> WalkFrame {
    WalkFrame next = *this;
    next.iteration_bindings = registry;
    return next;
  }

  [[nodiscard]] auto WithLoopVarMode(LoopVarLoweringMode mode) const
      -> WalkFrame {
    WalkFrame next = *this;
    next.loop_var_mode = mode;
    return next;
  }

  [[nodiscard]] auto WithSelfBinding(
      mir::LocalId id, BlockDepth decl_depth) const -> WalkFrame {
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
