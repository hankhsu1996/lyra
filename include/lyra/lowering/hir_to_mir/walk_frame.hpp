#pragma once

#include <cstdint>
#include <optional>

#include "lyra/base/internal_error.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/enclosing_hops.hpp"
#include "lyra/mir/field.hpp"

namespace lyra::mir {
struct Class;
struct Block;
}  // namespace lyra::mir

namespace lyra::lowering::hir_to_mir {

class CallableBindings;

// A class the walk can name, and the identity it was minted under. Reaching one
// is reaching the other: a reference to a member reads its type off the
// declaration and qualifies its target with the owner, so a walk that resolved
// only one of the two would immediately go looking for the other.
struct EnclosingClass {
  const mir::Class* cls = nullptr;
  mir::ClassId id{};
};

// Singly-linked node carrying a class's parent chain so a leaf reference
// can read the declared type of a member at `hops > 0`. Each node lives on
// the stack frame of the construction walk that pushed it; the chain extends
// one node per class opened during traversal. This is the construction-side
// half of the shared walk position: the rendering fold's read-only
// `ScopeView` resolves the same (hops, var) reference by climbing its own
// parent link, so both reach the same `mir::FieldDecl`.
struct ScopeChainNode {
  const mir::Class* cls = nullptr;
  mir::ClassId cls_id{};
  const ScopeChainNode* parent = nullptr;
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

  // Registry identity of `current_class`. Set together with `current_class` by
  // `WithClass`; carried alongside so an owner-qualified field or method
  // target names the enclosing class arena without a reverse lookup.
  mir::ClassId current_class_id{};

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

  // The enclosing class's borrowed handle on the name node of the procedural
  // scope being walked. A construct that means "the scope I am in" -- the LRM
  // 21.2.1.5 `%m` hierarchical name -- reads it off this body's `self`. Absent
  // where the scope owns no node and where no scope is open at all; both mean
  // the same thing to a reader, which is that the enclosing object answers for
  // the name.
  std::optional<mir::FieldId> scope_name_borrowed_handle;

  // Pushes `cls` as the current class and links the previous `current_class`
  // into the outer chain through `chain_node`, which the caller stack-allocates
  // so its lifetime spans the descent.
  [[nodiscard]] auto WithClass(
      mir::Class* cls, mir::ClassId cls_id, ScopeChainNode& chain_node) const
      -> WalkFrame {
    chain_node.cls = current_class;
    chain_node.cls_id = current_class_id;
    chain_node.parent = outer_classes;
    WalkFrame next = *this;
    next.current_class = cls;
    next.current_class_id = cls_id;
    next.outer_classes = current_class != nullptr ? &chain_node : outer_classes;
    return next;
  }

  // Enters a procedural scope, adopting the name node the shape phase gave it.
  [[nodiscard]] auto WithScopeNameBorrowedHandle(
      std::optional<mir::FieldId> borrowed_handle) const -> WalkFrame {
    WalkFrame next = *this;
    next.scope_name_borrowed_handle = borrowed_handle;
    return next;
  }

  // The class at `hops` -- 0 is the one being written, N climbs N steps out --
  // and the identity it was minted under. One answer, because a member
  // reference needs both: the declaration to read the member's type off, and
  // the owner to qualify the target with.
  [[nodiscard]] auto EnclosingClassAtHops(mir::EnclosingHops hops) const
      -> EnclosingClass {
    if (hops.value == 0) {
      if (current_class == nullptr) {
        throw InternalError(
            "WalkFrame::EnclosingClassAtHops: no current class");
      }
      return EnclosingClass{.cls = current_class, .id = current_class_id};
    }
    const ScopeChainNode* node = outer_classes;
    for (std::uint32_t step = 1; step < hops.value && node != nullptr; ++step) {
      node = node->parent;
    }
    if (node == nullptr || node->cls == nullptr) {
      throw InternalError(
          "WalkFrame::EnclosingClassAtHops: hops exceed chain depth");
    }
    return EnclosingClass{.cls = node->cls, .id = node->cls_id};
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
};

}  // namespace lyra::lowering::hir_to_mir
