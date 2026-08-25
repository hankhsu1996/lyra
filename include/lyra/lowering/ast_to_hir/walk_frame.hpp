#pragma once

#include <compare>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <ranges>
#include <string>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/registry.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/loop_label_id.hpp"
#include "lyra/hir/pattern_id.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/procedural_scope.hpp"
#include "lyra/hir/structural_hops.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/with_clause_id.hpp"

namespace lyra::hir {
struct Expr;
struct Pattern;
}  // namespace lyra::hir

namespace slang::ast {
class Symbol;
class Scope;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// Lowering-only identity for a structural scope on the walk. Monotonically
// assigned by UnitLowerer when a scope is entered; never reused; never
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

// A procedural scope while its own contents are still being walked. Its
// identity is already settled -- a scope a name can reach from elsewhere is
// minted before any body lowers, so a `disable` naming a block or task
// (LRM 9.6.2) resolves whichever body lowers first -- but which scopes nest
// inside it is known only once its subtree is finished; until then that lives
// here, on the stack frame that opened it.
struct OpenProceduralScope {
  OpenProceduralScope(
      hir::ProceduralScopeId id, hir::ProceduralScopeKind kind,
      std::optional<std::string> source_name)
      : id(id), kind(kind), source_name(std::move(source_name)) {
  }

  hir::ProceduralScopeId id;
  hir::ProceduralScopeKind kind;
  // Absent when the source named nothing; the stand-in is settled at sealing.
  std::optional<std::string> source_name;
  // Never supplied by whoever opens the scope: both fill in as the walk
  // descends.
  std::vector<hir::ProceduralVarId> declarations;
  std::vector<hir::ProceduralScopeId> children;
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
// writes to the root output go through narrow methods on `UnitLowerer`
// (interning a type for dedup, etc.).
struct WalkFrame {
  // The structural-scope nesting chain. Each entry is a ScopeFrameId minted
  // by UnitLowerer when a scope is entered. Used to compute structural
  // hops: HopsTo(target) walks back to find target in the chain.
  std::vector<ScopeFrameId> structural_chain;

  // The reader's position in slang's elaborated hierarchy: the slang scope of
  // the innermost structural scope on the chain. The structural_chain gives
  // hops only within this compilation unit; this locates the reader in the
  // whole-design hierarchy (across unit boundaries at the InstanceBody
  // transition), so a reference route to any target -- including one that
  // climbs out of this unit -- is computed as a reader-to-target relationship
  // rather than reclassified from a lexical form. Null before the root scope
  // is entered.
  const slang::ast::Scope* reader_scope = nullptr;

  // The current expression write target: the expr arena every expression
  // handler appends lowered sub-expressions into, regardless of procedural or
  // structural context. Set alongside the owning write target on scope/body
  // entry. Expression handlers reach it through `Exprs()` and never touch the
  // owning pointers below.
  base::Arena<hir::Expr, hir::ExprId>* current_exprs = nullptr;

  // The current pattern write target, set on the same entries and for the
  // same reason as the expression one above: a pattern's meaning does not
  // depend on whether a procedural body or a structural scope encloses it, so
  // one handler serves both and appends here (LRM 12.6).
  base::Arena<hir::Pattern, hir::PatternId>* current_patterns = nullptr;

  // The current structural-scope write target for member and generate handlers.
  // Set when a StructuralScope task constructs its scope on the stack and
  // entered via `WithStructuralFrame`. Null outside structural-scope handlers.
  hir::StructuralScope* current_structural_scope = nullptr;

  // The registry owning the lexical procedural scopes (LRM 9.3.4) of every body
  // declared in the enclosing declaration scope -- a structural scope or a
  // class. Set on entry to either, so a body's scope tree is registered the
  // same way wherever the body was declared.
  base::Registry<hir::ProceduralScopeDecl, hir::ProceduralScopeId>*
      current_procedural_scopes = nullptr;

  // The current procedural-body write target for statement and local handlers.
  // Set when a ProcessLowerer constructs its body on the stack and entered via
  // `WithProceduralBody`. Null outside a process or subroutine body.
  hir::ProceduralBody* current_procedural_body = nullptr;

  // Nonzero while a fork-join branch body is being lowered. Counter so a nested
  // fork is tracked correctly. Zero outside a process body.
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

  // The lexical procedural scope whose contents are currently being walked.
  // Owned by the caller that opened it; null on a frame that is not inside a
  // procedural body, which is why a handler reached inside one dereferences it
  // unconditionally -- a null here is a caller bug, not a runtime branch.
  OpenProceduralScope* open_scope = nullptr;

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

  [[nodiscard]] auto Patterns() const
      -> base::Arena<hir::Pattern, hir::PatternId>& {
    if (current_patterns == nullptr) {
      throw InternalError("WalkFrame::Patterns: no pattern write target");
    }
    return *current_patterns;
  }

  // The registry a body's lexical scopes are sealed into. Every frame that
  // lowers procedural-body content has one; reaching it without one is a caller
  // bug.
  [[nodiscard]] auto ProceduralScopes() const
      -> base::Registry<hir::ProceduralScopeDecl, hir::ProceduralScopeId>& {
    if (current_procedural_scopes == nullptr) {
      throw InternalError(
          "WalkFrame::ProceduralScopes: no procedural-scope write target");
    }
    return *current_procedural_scopes;
  }

  // Points the frame at the declaration scope that owns the procedural scopes
  // of the bodies lowered below it. Called on entry to a structural scope and
  // on entry to a class, the two declaration scopes that own bodies.
  [[nodiscard]] auto WithProceduralScopeOwner(
      base::Registry<hir::ProceduralScopeDecl, hir::ProceduralScopeId>* scopes)
      const -> WalkFrame {
    WalkFrame next = *this;
    next.current_procedural_scopes = scopes;
    return next;
  }

  // Pushes a new structural scope onto the chain and makes it the write
  // target. The scope is owned by the caller's stack frame (typically a
  // Lowerer's Run); this frame just borrows it for the duration of the walk.
  // The expression and pattern arenas come off the scope itself, so no caller
  // can pair one scope with another's arenas.
  [[nodiscard]] auto WithStructuralFrame(
      ScopeFrameId child_frame, const slang::ast::Scope* slang_scope,
      hir::StructuralScope* scope) const -> WalkFrame {
    WalkFrame next = *this;
    next.structural_chain.push_back(child_frame);
    next.reader_scope = slang_scope;
    next.current_structural_scope = scope;
    next.current_exprs = &scope->exprs;
    next.current_patterns = &scope->patterns;
    return next;
  }

  // Makes a procedural body the write target, for the whole of a process or
  // subroutine walk: nested control flow does not push a second body, because
  // HIR's procedural body is flat. Its arenas come off the body for the same
  // reason as above.
  [[nodiscard]] auto WithProceduralBody(hir::ProceduralBody* body) const
      -> WalkFrame {
    WalkFrame next = *this;
    next.current_procedural_body = body;
    next.current_exprs = &body->exprs;
    next.current_patterns = &body->patterns;
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

  // Descends into a scope the caller just opened, so declarations and nested
  // scopes reached below attach to it rather than to the one it nests in.
  [[nodiscard]] auto WithOpenScope(OpenProceduralScope* scope) const
      -> WalkFrame {
    WalkFrame next = *this;
    next.open_scope = scope;
    return next;
  }

  // The scope the current handler's declarations belong to.
  [[nodiscard]] auto OpenScope() const -> OpenProceduralScope& {
    if (open_scope == nullptr) {
      throw InternalError("WalkFrame::OpenScope: no open procedural scope");
    }
    return *open_scope;
  }

  // Fills a finished scope's contents into the identity minted for it and hangs
  // it under the scope it nests in, which is the one this frame still has open.
  // Called on the frame that was current *before* the scope opened, so being
  // defined and being reachable from the enclosing scope are one step and
  // neither can be forgotten. A body root has no enclosing scope and is simply
  // defined.
  [[nodiscard]] auto SealScope(OpenProceduralScope scope) const
      -> hir::ProceduralScopeId {
    ProceduralScopes().Define(
        scope.id, hir::ProceduralScopeDecl{
                      .kind = scope.kind,
                      .source_name = std::move(scope.source_name),
                      .declarations = std::move(scope.declarations),
                      .child_scopes = std::move(scope.children)});
    if (open_scope != nullptr) {
      open_scope->children.push_back(scope.id);
    }
    return scope.id;
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
