#include "lyra/lowering/ast_to_hir/one_body.hpp"

#include <cstddef>
#include <optional>
#include <span>
#include <utility>
#include <variant>

#include "lyra/base/overloaded.hpp"
#include "lyra/base/registry.hpp"
#include "lyra/hir/structural_scope.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

auto MergeBlocks(const hir::StructuralScope& a, const hir::StructuralScope& b)
    -> std::optional<hir::StructuralScope>;

// One conditional construct as two blocks left it. The alternatives and what
// selects each are the source's, so both blocks state the same ones in the
// same order; what differs is which of them each index selected a body for.
// Merging keeps every body either produced, and where both selected the same
// alternative, that alternative's own scopes have to be one body in their turn.
auto MergeChoices(
    const hir::Generate& a, const hir::BlocksChoose& chosen_a,
    const hir::Generate& b, const hir::BlocksChoose& chosen_b)
    -> std::optional<hir::Generate> {
  if (chosen_a.alternatives.size() != chosen_b.alternatives.size()) {
    return std::nullopt;
  }

  // What selects each alternative is the source's, so the two blocks state the
  // same choices under the same names; reading one against the other says so
  // over the whole tree rather than over a list someone has to keep up to date.
  if (!(chosen_a.choices == chosen_b.choices) ||
      chosen_a.root != chosen_b.root) {
    return std::nullopt;
  }

  hir::Generate merged{};
  hir::BlocksChoose chosen;
  chosen.choices = chosen_a.choices;
  chosen.root = chosen_a.root;
  for (std::size_t at = 0; at < chosen_a.alternatives.size(); ++at) {
    const std::optional<hir::StructuralScopeId> from_a =
        chosen_a.alternatives[at];
    const std::optional<hir::StructuralScopeId> from_b =
        chosen_b.alternatives[at];

    // An alternative receives nothing (Syntax 27-1), so its scope is the
    // whole of what the two blocks can disagree on.
    std::optional<hir::StructuralScope> body;
    if (from_a.has_value()) body = a.blocks.Get(*from_a).scope;
    if (from_b.has_value()) {
      const hir::StructuralScope& theirs = b.blocks.Get(*from_b).scope;
      if (!body.has_value()) {
        body = theirs;
      } else {
        body = MergeBlocks(*body, theirs);
        if (!body.has_value()) return std::nullopt;
      }
    }
    std::optional<hir::StructuralScopeId> block;
    if (body.has_value()) {
      block = merged.blocks.Add(
          hir::GenerateBlock{.scope = *std::move(body), .arguments = {}});
    }
    chosen.alternatives.push_back(block);
  }
  merged.counting = std::move(chosen);
  return merged;
}

// One construct that selects nothing, as two blocks left it: a loop, blocks
// standing on their own, or a single block. Both state the same scopes in the
// same positions, counted out and supplied the same way, so each scope is
// merged with the one standing where it does. A selection can sit at any depth
// beneath them, which is why agreeing here is asked of what the scopes hold
// rather than of the scopes whole.
auto MergeBlockForBlock(const hir::Generate& a, const hir::Generate& b)
    -> std::optional<hir::Generate> {
  if (a.counting != b.counting || a.blocks.size() != b.blocks.size()) {
    return std::nullopt;
  }

  hir::Generate merged{};
  merged.counting = a.counting;
  for (const hir::StructuralScopeId id : a.blocks.Ids()) {
    const hir::GenerateBlock& ours = a.blocks.Get(id);
    const hir::GenerateBlock& theirs = b.blocks.Get(id);
    if (ours.arguments != theirs.arguments) return std::nullopt;
    auto body = MergeBlocks(ours.scope, theirs.scope);
    if (!body.has_value()) return std::nullopt;
    merged.blocks.Add(
        hir::GenerateBlock{
            .scope = *std::move(body), .arguments = ours.arguments});
  }
  return merged;
}

auto MergeGenerates(const hir::Generate& a, const hir::Generate& b)
    -> std::optional<hir::Generate> {
  return std::visit(
      Overloaded{
          [&](const hir::BlocksChoose& chosen_a)
              -> std::optional<hir::Generate> {
            const auto* chosen_b = std::get_if<hir::BlocksChoose>(&b.counting);
            if (chosen_b == nullptr) return std::nullopt;
            return MergeChoices(a, chosen_a, b, *chosen_b);
          },
          [&](const hir::BlocksRepeat&) -> std::optional<hir::Generate> {
            return MergeBlockForBlock(a, b);
          },
          [&](const hir::BlocksStandAlone&) -> std::optional<hir::Generate> {
            return MergeBlockForBlock(a, b);
          },
          [&](const hir::SingleBlock&) -> std::optional<hir::Generate> {
            return MergeBlockForBlock(a, b);
          }},
      a.counting);
}

auto MergeBlocks(const hir::StructuralScope& a, const hir::StructuralScope& b)
    -> std::optional<hir::StructuralScope> {
  if (a.generates.size() != b.generates.size()) return std::nullopt;

  // A selection sits in a generate construct or beneath one, so the generates
  // are one part allowed to differ; what the two blocks published is the
  // other, since each block's class and the blocks below it have names of their
  // own, and everything else it states follows from the declarations compared
  // here. Reading one block with the other's in place of its own says whether
  // anything else did, over every field there is rather than over a list
  // someone has to keep up to date.
  hir::StructuralScope merged = a;
  merged.generates = b.generates;
  merged.published = b.published;
  if (!(merged == b)) return std::nullopt;

  // The one scope stands for both blocks, so it answers to the names of each.
  merged.published = a.published;
  merged.published.aliases.push_back(b.published.signature.class_name);
  merged.published.aliases.insert(
      merged.published.aliases.end(), b.published.aliases.begin(),
      b.published.aliases.end());
  base::Registry<hir::Generate, hir::GenerateId> generates;
  for (const hir::GenerateId id : a.generates.Ids()) {
    auto one = MergeGenerates(a.generates.Get(id), b.generates.Get(id));
    if (!one) return std::nullopt;
    generates.Add(*std::move(one));
  }
  merged.generates = std::move(generates);
  return merged;
}

}  // namespace

auto OneBodyOf(std::span<const hir::StructuralScope> blocks)
    -> std::optional<hir::StructuralScope> {
  if (blocks.empty()) return std::nullopt;
  std::optional<hir::StructuralScope> body = blocks.front();
  for (const hir::StructuralScope& block : blocks.subspan(1)) {
    body = MergeBlocks(*body, block);
    if (!body.has_value()) return std::nullopt;
  }
  return body;
}

}  // namespace lyra::lowering::ast_to_hir
