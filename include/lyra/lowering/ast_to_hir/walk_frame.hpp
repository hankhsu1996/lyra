#pragma once

#include <compare>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <ranges>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/loop_label_id.hpp"
#include "lyra/hir/structural_hops.hpp"
#include "lyra/hir/with_clause_id.hpp"

namespace lyra::hir {
struct Expr;
struct StructuralScope;
struct ProceduralBody;
}  // namespace lyra::hir

namespace slang::ast {
class Symbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// Lowering-only identity for a structural scope on the walk. Monotonically
// assigned by ModuleLowerer when a scope is entered; never reused; never
// stored in HIR. Used to compute structural hops between scopes.
struct ScopeFrameId {
  std::uint32_t value;
  auto operator<=>(const ScopeFrameId&) const -> std::strong_ordering = default;
};

// One active array-method `with` clause whose body is being lowered: its slang
// iterator symbol paired with the HIR identity assigned to the clause. A
// reference whose symbol matches the iterator resolves to this clause's
// element and index bindings.
struct ActiveIterationClause {
  const slang::ast::Symbol* element;
  hir::WithClauseId clause;
};

// Per-recursion traversal context for AST-to-HIR. Carried by value through
// every dispatcher method and per-kind handler. Push/pop discipline: entering
// a deeper structural scope or a procedural body constructs a new frame with
// the corresponding pointer pushed and the structural chain extended; the
// caller's frame is unchanged when the recursion returns.
//
// Walk-invariant facts (the unit being built, source mapper, builtins) live
// on the Lowerer class, not here. WalkFrame holds only state that genuinely
// changes from one recursion to the next.
//
// The write target for the current handler is reached through
// `current_structural_scope` (inside a structural-scope task) or
// `current_procedural_body` (inside a process or subroutine task). Exactly
// one is non-null at any walker entry, determined by the surrounding Lowerer
// context. Handlers writing into a nested scope go through these pointers;
// writes to the root output go through narrow methods on ModuleLowerer
// (`module.InternType(...)` for type dedup, etc.).
struct WalkFrame {
  // The structural-scope nesting chain. Each entry is a ScopeFrameId minted
  // by ModuleLowerer when a scope is entered. Used to compute structural
  // hops: HopsTo(target) walks back to find target in the chain.
  std::vector<ScopeFrameId> structural_chain;

  // The current expression write target: the expr arena every expression
  // handler appends lowered sub-expressions into, regardless of procedural or
  // structural context. Set alongside the owning write target on scope/body
  // entry. Expression handlers reach it through `Exprs()` and never touch the
  // owning pointers below.
  base::Arena<hir::Expr, hir::ExprId>* current_exprs = nullptr;

  // The current structural-scope write target for member and generate handlers.
  // Set when a StructuralScope task constructs its scope on the stack and
  // entered via `WithStructuralFrame`. Null outside structural-scope handlers.
  hir::StructuralScope* current_structural_scope = nullptr;

  // The current procedural-body write target for statement and local handlers.
  // Set when a ProcessLowerer constructs its body on the stack and entered via
  // `WithProceduralBody`. Null outside a process or subroutine body.
  hir::ProceduralBody* current_procedural_body = nullptr;

  // Nonzero while a fork-join branch body is being lowered. A procedural-var
  // reference inside a branch is rejected (FJ1 has no per-branch local
  // storage yet). Counter so a nested fork stays rejected correctly. Zero
  // outside a process body.
  std::uint32_t fork_branch_depth = 0;

  // Non-local break target for the innermost enclosing loop. A `foreach`
  // lowers to nested loops, so a `break` whose innermost SystemVerilog loop is
  // that foreach must leave the whole nest -- it carries the outermost loop's
  // label. Set while lowering a foreach body; reset to nullopt while lowering
  // an ordinary loop body (whose break is a plain innermost exit). When a break
  // consumes the label, `innermost_break_used` is flipped so the foreach knows
  // to mark the outer loop as a landing target.
  std::optional<hir::LoopLabelId> innermost_break_label = std::nullopt;
  bool* innermost_break_used = nullptr;

  // The queue base whose `$` (LRM 7.10 last index) is resolved while lowering
  // an element-select index or slice bound: `$` lowers to `size(base) - 1`.
  // Null outside a queue index / bound expression. Re-set per select, so each
  // `$` in a nested `q[r[$]]` binds to the array its own select indexes.
  std::optional<hir::ExprId> dollar_base = std::nullopt;

  // The active LRM 7.12 array-method `with`-clause iterators whose bodies are
  // being lowered, each enclosing clause kept so an inner body can still name
  // an outer iterator. A reference matching a clause's element symbol resolves
  // to that clause's `IterationBindingRef`, not the procedural-var path;
  // foreach loop variables are also slang Iterator symbols, so the match is by
  // symbol identity. Empty outside a with-clause body.
  std::vector<ActiveIterationClause> active_iteration_clauses;

  [[nodiscard]] auto Current() const -> ScopeFrameId {
    if (structural_chain.empty()) {
      throw InternalError("WalkFrame::Current: empty structural chain");
    }
    return structural_chain.back();
  }

  [[nodiscard]] auto Depth() const -> std::size_t {
    return structural_chain.size();
  }

  [[nodiscard]] auto HopsTo(ScopeFrameId target) const
      -> std::optional<hir::StructuralHops> {
    std::uint32_t hops = 0;
    for (const auto frame : structural_chain | std::views::reverse) {
      if (frame == target) {
        return hir::StructuralHops{.value = hops};
      }
      ++hops;
    }
    return std::nullopt;
  }

  // The expr arena the current scope or body appends lowered sub-expressions
  // into. Reached by every expression handler regardless of context.
  [[nodiscard]] auto Exprs() const -> base::Arena<hir::Expr, hir::ExprId>& {
    if (current_exprs == nullptr) {
      throw InternalError("WalkFrame::Exprs: no expression write target");
    }
    return *current_exprs;
  }

  // Pushes a new structural scope onto the chain and points
  // current_structural_scope at the new scope. The scope is owned by the
  // caller's stack frame (typically a Lowerer's Run); this frame just
  // borrows it for the duration of the walk. `exprs` is the scope's expr arena,
  // passed by the caller (which holds the complete scope type) so this header
  // stays free of the HIR scope definition.
  [[nodiscard]] auto WithStructuralFrame(
      ScopeFrameId child_frame, hir::StructuralScope* scope,
      base::Arena<hir::Expr, hir::ExprId>* exprs) const -> WalkFrame {
    WalkFrame next = *this;
    next.structural_chain.push_back(child_frame);
    next.current_structural_scope = scope;
    next.current_exprs = exprs;
    return next;
  }

  // Sets the current procedural body. Used by ProcessLowerer::Run when it
  // stack-allocates a hir::ProceduralBody and dispatches into it. The body
  // pointer is unchanged for the lifetime of the process / subroutine walk;
  // nested control flow does not push procedural bodies because HIR's
  // procedural body is flat. `exprs` is the body's expr arena, passed by the
  // caller for the same reason as `WithStructuralFrame`.
  [[nodiscard]] auto WithProceduralBody(
      hir::ProceduralBody* body,
      base::Arena<hir::Expr, hir::ExprId>* exprs) const -> WalkFrame {
    WalkFrame next = *this;
    next.current_procedural_body = body;
    next.current_exprs = exprs;
    return next;
  }

  [[nodiscard]] auto WithForkBranch() const -> WalkFrame {
    WalkFrame next = *this;
    ++next.fork_branch_depth;
    return next;
  }

  // Binds `base` as the queue whose `$` resolves to its last index while the
  // index / slice-bound subtree is lowered (LRM 7.10).
  [[nodiscard]] auto WithDollarBase(hir::ExprId base) const -> WalkFrame {
    WalkFrame next = *this;
    next.dollar_base = base;
    return next;
  }

  // Marks the given `with` clause and its iterator symbol active while its body
  // subtree is lowered (LRM 7.12.4). Pushed so a clause nested in the body sees
  // every enclosing clause.
  [[nodiscard]] auto WithIterationClause(
      const slang::ast::Symbol& element, hir::WithClauseId clause) const
      -> WalkFrame {
    WalkFrame next = *this;
    next.active_iteration_clauses.push_back(
        ActiveIterationClause{.element = &element, .clause = clause});
    return next;
  }

  // The active clause whose iterator is the given symbol, if any. Both an
  // element read (`item`) and an index read (`item.index`, keyed by its
  // receiver) resolve their clause through this.
  [[nodiscard]] auto FindIterationClause(const slang::ast::Symbol& sym) const
      -> std::optional<hir::WithClauseId> {
    for (const auto& active : active_iteration_clauses) {
      if (active.element == &sym) {
        return active.clause;
      }
    }
    return std::nullopt;
  }

  // Establishes `label` as the break target for the body being lowered. `used`
  // points at a flag the foreach owns; a break that consumes the label flips
  // it. Used by the foreach lowering for its nested loop body.
  [[nodiscard]] auto WithBreakLabel(hir::LoopLabelId label, bool* used) const
      -> WalkFrame {
    WalkFrame next = *this;
    next.innermost_break_label = label;
    next.innermost_break_used = used;
    return next;
  }

  // Clears the foreach break target. An ordinary loop body uses this so a break
  // inside it is the plain innermost exit, not an escape to an enclosing
  // foreach.
  [[nodiscard]] auto WithoutBreakLabel() const -> WalkFrame {
    WalkFrame next = *this;
    next.innermost_break_label = std::nullopt;
    next.innermost_break_used = nullptr;
    return next;
  }

  [[nodiscard]] auto InForkBranch() const -> bool {
    return fork_branch_depth > 0;
  }
};

}  // namespace lyra::lowering::ast_to_hir
