#pragma once

#include <compare>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <ranges>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/structural_hops.hpp"

namespace lyra::hir {
struct StructuralScope;
struct ProceduralBody;
}  // namespace lyra::hir

namespace lyra::lowering::ast_to_hir {

// Lowering-only identity for a structural scope on the walk. Monotonically
// assigned by ModuleLowerer when a scope is entered; never reused; never
// stored in HIR. Used to compute structural hops between scopes.
struct ScopeFrameId {
  std::uint32_t value;
  auto operator<=>(const ScopeFrameId&) const -> std::strong_ordering = default;
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

  // The current structural-scope write target. Set when a StructuralScope
  // task constructs its scope on the stack and entered via
  // `WithStructuralFrame`. Null outside structural-scope handlers.
  hir::StructuralScope* current_structural_scope = nullptr;

  // The current procedural-body write target. Set when a ProcessLowerer
  // constructs its body on the stack and entered via `WithProceduralBody`.
  // Null outside a process or subroutine body.
  hir::ProceduralBody* current_procedural_body = nullptr;

  // Nonzero while a fork-join branch body is being lowered. A procedural-var
  // reference inside a branch is rejected (FJ1 has no per-branch local
  // storage yet). Counter so a nested fork stays rejected correctly. Zero
  // outside a process body.
  std::uint32_t fork_branch_depth = 0;

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

  // Pushes a new structural scope onto the chain and points
  // current_structural_scope at the new scope. The scope is owned by the
  // caller's stack frame (typically a Lowerer's Run); this frame just
  // borrows it for the duration of the walk.
  [[nodiscard]] auto WithStructuralFrame(
      ScopeFrameId child_frame, hir::StructuralScope* scope) const
      -> WalkFrame {
    WalkFrame next = *this;
    next.structural_chain.push_back(child_frame);
    next.current_structural_scope = scope;
    return next;
  }

  // Sets the current procedural body. Used by ProcessLowerer::Run when it
  // stack-allocates a hir::ProceduralBody and dispatches into it. The body
  // pointer is unchanged for the lifetime of the process / subroutine walk;
  // nested control flow does not push procedural bodies because HIR's
  // procedural body is flat.
  [[nodiscard]] auto WithProceduralBody(hir::ProceduralBody* body) const
      -> WalkFrame {
    WalkFrame next = *this;
    next.current_procedural_body = body;
    return next;
  }

  [[nodiscard]] auto WithForkBranch() const -> WalkFrame {
    WalkFrame next = *this;
    ++next.fork_branch_depth;
    return next;
  }

  [[nodiscard]] auto InForkBranch() const -> bool {
    return fork_branch_depth > 0;
  }
};

}  // namespace lyra::lowering::ast_to_hir
