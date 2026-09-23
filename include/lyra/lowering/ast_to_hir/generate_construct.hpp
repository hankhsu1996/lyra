#pragma once

#include <cstdint>
#include <vector>

#include <slang/ast/Scope.h>
#include <slang/ast/symbols/BlockSymbols.h>

#include "lyra/hir/owned_child_ref.hpp"

namespace lyra::lowering::ast_to_hir {

// How the blocks a generate construct produced are recovered from the members
// of the scope holding them.
//
// A conditional generate is one construct however many alternative blocks it
// holds (LRM 27.5), and a directly nested conditional's blocks belong to the
// outer construct as well. The front end publishes none of that as a grouping:
// it publishes the blocks as ordinary members of the enclosing scope, each
// carrying the index of the construct that produced it. So a walk over those
// members meets one construct several times, and these answer what it has to
// know each time -- whether this is the visit that handles the construct, which
// blocks the construct holds, and how a name identifies one of them.

// Whether a conditional generate produced this block, which is what makes it
// one alternative of a set rather than a block standing on its own.
[[nodiscard]] inline auto IsAlternative(
    const slang::ast::GenerateBlockSymbol& block) -> bool {
  using slang::ast::GenerateBranchKind;
  switch (block.branchKind) {
    case GenerateBranchKind::IfTrue:
    case GenerateBranchKind::IfFalse:
    case GenerateBranchKind::CaseItem:
    case GenerateBranchKind::CaseDefault:
      return true;
    case GenerateBranchKind::LoopIteration:
    case GenerateBranchKind::IllegalUnconditional:
      return false;
  }
  return false;
}

// Every block one construct produced, in the order the source wrote them,
// whether or not this elaboration selected each. The ones no elaboration
// selected are present and marked rather than dropped, which is what lets a
// construct state every alternative wherever it stands.
[[nodiscard]] inline auto AlternativesOfConstruct(
    const slang::ast::GenerateBlockSymbol& first)
    -> std::vector<const slang::ast::GenerateBlockSymbol*> {
  std::vector<const slang::ast::GenerateBlockSymbol*> alternatives;
  const slang::ast::Scope* scope = first.getParentScope();
  if (scope == nullptr) return {&first};
  for (const auto& member : scope->members()) {
    const auto* block = member.as_if<slang::ast::GenerateBlockSymbol>();
    if (block == nullptr || block->constructIndex != first.constructIndex) {
      continue;
    }
    alternatives.push_back(block);
  }
  return alternatives;
}

// Whether the visit at this block is the visit that handles its construct,
// which is the first block the construct produced. A block no conditional
// produced is a construct of one and always answers yes.
[[nodiscard]] inline auto OpensItsConstruct(
    const slang::ast::GenerateBlockSymbol& block) -> bool {
  const slang::ast::Scope* scope = block.getParentScope();
  if (scope == nullptr) return true;
  for (const auto& member : scope->members()) {
    const auto* other = member.as_if<slang::ast::GenerateBlockSymbol>();
    if (other == nullptr || other->constructIndex != block.constructIndex) {
      continue;
    }
    return other == &block;
  }
  return true;
}

// How a name identifies one block of a construct, given the position that
// block holds among the construct's own. A conditional selects at most one of
// an ordered set of alternatives, so what identifies one is the position the
// source wrote it at; every other construct declares its blocks positionally,
// and one that declares a single block is the one-element case of that.
[[nodiscard]] inline auto NamedBlockOf(
    const slang::ast::GenerateBlockSymbol& block, std::uint32_t position)
    -> hir::NamedBlock {
  if (!IsAlternative(block)) return hir::BlockAtIndex{.index = position};
  return hir::BlockAsAlternative{.position = position};
}

}  // namespace lyra::lowering::ast_to_hir
