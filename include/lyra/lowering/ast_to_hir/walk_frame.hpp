#pragma once

#include <compare>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <ranges>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/structural_hops.hpp"

namespace lyra::lowering::ast_to_hir {

// Lowering-only identity for a structural scope on the walk. Monotonically
// assigned by ModuleLowerer when a scope is entered; never reused; never
// stored in HIR. Used to compute structural hops between scopes.
struct ScopeFrameId {
  std::uint32_t value;
  auto operator<=>(const ScopeFrameId&) const -> std::strong_ordering = default;
};

// Per-recursion traversal context for AST-to-HIR. Carried by value through
// every walker method on every Lowerer class. Push/pop discipline: entering a
// deeper structural scope constructs a new frame with the chain extended;
// entering a fork-join branch constructs one with the depth incremented; the
// caller's frame is unchanged when the recursion returns.
struct WalkFrame {
  // The structural-scope nesting chain. Each entry is a ScopeFrameId minted
  // by ModuleLowerer when a scope is entered. Used to compute structural
  // hops: HopsTo(target) walks back to find target in the chain.
  std::vector<ScopeFrameId> structural_chain;

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

  [[nodiscard]] auto WithStructuralFrame(ScopeFrameId child_frame) const
      -> WalkFrame {
    WalkFrame next = *this;
    next.structural_chain.push_back(child_frame);
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
