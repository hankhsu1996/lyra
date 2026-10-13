#include "lyra/lowering/ast_to_hir/one_body.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/overloaded.hpp"
#include "lyra/base/registry.hpp"
#include "lyra/hir/structural_scope.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

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
        body = OneBodyOf(*body, theirs);
        if (!body.has_value()) return std::nullopt;
      }
    }
    std::optional<hir::StructuralScopeId> block;
    if (body.has_value()) {
      block = merged.blocks.Add(hir::GenerateBlock{.scope = *std::move(body)});
    }
    chosen.alternatives.push_back(block);
  }
  merged.counting = std::move(chosen);
  return merged;
}

// One loop a construction runs, as two blocks left it. The loop is the
// source's, so both state the same one; what differs is which blocks each of
// them counted out of it, which is no part of what either states, so the
// bodies of both are taken into one.
auto MergeLoops(
    const hir::Generate& a, const hir::BlocksRepeat& loop_a,
    const hir::Generate& b, const hir::BlocksRepeat& loop_b)
    -> std::optional<hir::Generate> {
  hir::BlocksRepeat merged_loop = loop_a;
  merged_loop.bodies.clear();
  hir::BlocksRepeat bare_b = loop_b;
  bare_b.bodies.clear();
  if (merged_loop != bare_b) return std::nullopt;

  LoopBodies bodies;
  for (const auto& [gen, loop] :
       {std::pair{&a, &loop_a}, std::pair{&b, &loop_b}}) {
    for (const hir::BodyBuiltAt& built : loop->bodies) {
      if (!bodies.Take(
              gen->blocks.Get(built.body).scope, built.code_arguments,
              built.indices)) {
        return std::nullopt;
      }
    }
  }
  return std::move(bodies).Build(std::move(merged_loop));
}

// A block no loop and no conditional produced, as two blocks left it. A
// selection can sit at any depth beneath it, which is why agreeing here is
// asked of what the block holds rather than of the block whole.
auto MergeSingleBlocks(const hir::Generate& a, const hir::Generate& b)
    -> std::optional<hir::Generate> {
  if (a.counting != b.counting || a.blocks.size() != b.blocks.size()) {
    return std::nullopt;
  }

  hir::Generate merged{};
  merged.counting = a.counting;
  for (const hir::StructuralScopeId id : a.blocks.Ids()) {
    auto body = OneBodyOf(a.blocks.Get(id).scope, b.blocks.Get(id).scope);
    if (!body.has_value()) return std::nullopt;
    merged.blocks.Add(hir::GenerateBlock{.scope = *std::move(body)});
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
          [&](const hir::BlocksRepeat& loop_a) -> std::optional<hir::Generate> {
            const auto* loop_b = std::get_if<hir::BlocksRepeat>(&b.counting);
            if (loop_b == nullptr) return std::nullopt;
            return MergeLoops(a, loop_a, b, *loop_b);
          },
          [&](const hir::SingleBlock&) -> std::optional<hir::Generate> {
            return MergeSingleBlocks(a, b);
          }},
      a.counting);
}

}  // namespace

auto OneBodyOf(const hir::StructuralScope& a, const hir::StructuralScope& b)
    -> std::optional<hir::StructuralScope> {
  if (a.published.signature.class_path != b.published.signature.class_path ||
      a.generates.size() != b.generates.size()) {
    return std::nullopt;
  }

  // A selection sits in a generate construct or beneath one, so the generates
  // are the one part allowed to differ. Reading one block with the other's in
  // place of its own says whether anything else did, over every field there is
  // rather than over a list someone has to keep up to date. What a block
  // published is among those fields, so two blocks that are objects of
  // different classes are never one body.
  hir::StructuralScope merged = a;
  merged.generates = b.generates;
  if (!(merged == b)) return std::nullopt;

  base::Registry<hir::Generate, hir::GenerateId> generates;
  for (const hir::GenerateId id : a.generates.Ids()) {
    auto one = MergeGenerates(a.generates.Get(id), b.generates.Get(id));
    if (!one) return std::nullopt;
    generates.Add(*std::move(one));
  }
  merged.generates = std::move(generates);
  return merged;
}

auto LoopBodies::Take(
    hir::StructuralScope scope, std::optional<std::uint64_t> code_arguments,
    std::span<const std::int64_t> indices) -> bool {
  const auto [named, first] = named_.try_emplace(
      std::pair{scope.published.signature.class_path, code_arguments},
      bodies_.size());
  const std::size_t body = named->second;
  if (first) {
    bodies_.push_back(std::move(scope));
    code_arguments_.push_back(code_arguments);
  } else {
    auto one = OneBodyOf(bodies_[body], scope);
    if (!one.has_value()) return false;
    bodies_[body] = *std::move(one);
  }
  return std::ranges::all_of(indices, [&](std::int64_t index) {
    return standing_at_.try_emplace(index, body).first->second == body;
  });
}

auto LoopBodies::Build(hir::BlocksRepeat loop) && -> hir::Generate {
  hir::Generate gen{};
  for (std::size_t at = 0; at < bodies_.size(); ++at) {
    loop.bodies.push_back(
        hir::BodyBuiltAt{
            .body = gen.blocks.Add(
                hir::GenerateBlock{.scope = std::move(bodies_[at])}),
            .code_arguments = code_arguments_[at],
            .indices = {}});
  }
  for (const auto& [index, body] : standing_at_) {
    loop.bodies[body].indices.push_back(index);
  }
  gen.counting = std::move(loop);
  return gen;
}

}  // namespace lyra::lowering::ast_to_hir
