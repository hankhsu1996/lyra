#pragma once

#include <cstdint>

#include "lyra/base/internal_error.hpp"
#include "lyra/mir/enclosing_hops.hpp"

namespace lyra::mir {
struct Class;
struct Block;
}  // namespace lyra::mir

namespace lyra::lowering::hir_to_mir {

class CallableBindings;

// Singly-linked node carrying a class's parent chain so a leaf reference
// can read the declared type of a member at `hops > 0`. Each node lives on
// the stack frame of the construction walk that pushed it; the chain extends
// one node per class opened during traversal. This is the construction-side
// half of the shared walk position: the rendering fold's read-only
// `ScopeView` resolves the same (hops, var) reference by climbing its own
// parent link, so both reach the same `mir::FieldDecl`.
struct ScopeChainNode {
  const mir::Class* cls;
  const ScopeChainNode* parent;
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
  // lowerer `parent_` chain. Populated by `WithClass` when the construction
  // walk opens a new class. Read via `EnclosingClassAtHops` to resolve a member
  // reference at `hops > 0`. This is the object-graph axis, distinct from the
  // lexical binding axis carried by `bindings`.
  const ScopeChainNode* outer_classes = nullptr;

  // The current block write target. Set when a walker opens a new block
  // (process body, nested block body, fork branch body, closure body) and
  // entered via `WithBlock`. Null outside a block. A block places statements
  // and exprs; it does not resolve a reference to its binding.
  mir::Block* current_block = nullptr;

  // The binding-resolution context of the callable body being lowered: a
  // reference resolves to a binding through it, and entering a closure body
  // installs a child context whose parent is this one (so a capture forwards
  // one callable boundary at a time). Stable through nested blocks of one
  // callable; replaced at a callable boundary.
  CallableBindings* bindings = nullptr;

  // Whether the enclosing callable body suspends -- a process, a task, or a
  // closure synthesized to run as a separate concurrent process (fork branch).
  // Set at body entry and inherited unchanged through nested blocks;
  // a closure entry re-establishes it from the closure's own `is_coroutine`.
  // The `ReturnStmt` lowering reads this to fill the stmt's
  // `is_coroutine_return` attribute (a C++ render hint, ignored by LIR / LLVM).
  bool is_coroutine_body = false;

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

  [[nodiscard]] auto WithBindings(CallableBindings* callable_bindings) const
      -> WalkFrame {
    WalkFrame next = *this;
    next.bindings = callable_bindings;
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
