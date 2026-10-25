#pragma once

#include <algorithm>
#include <cstdint>
#include <format>
#include <iterator>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include <slang/ast/HierarchicalReference.h>
#include <slang/ast/Scope.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/ParameterSymbols.h>
#include <slang/numeric/SVInt.h>

#include "lyra/base/internal_error.hpp"

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
// know each time -- whether this is the visit that handles the construct,
// which blocks the construct holds, and which value a loop's block stands at.

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

// The construct `written` stands for in `elaboration`, another elaboration of
// the text holding it: the generate the source wrote at the same place, which
// the front end numbers alike in every elaboration of that text. For a
// conditional that is the first block of the construct, as it is in the
// elaboration being walked.
template <typename Construct>
[[nodiscard]] auto CounterpartIn(
    const slang::ast::Scope& elaboration, const Construct& written)
    -> const Construct& {
  for (const auto& member : elaboration.members()) {
    const auto* counterpart = member.as_if<Construct>();
    if (counterpart != nullptr &&
        counterpart->constructIndex == written.constructIndex) {
      return *counterpart;
    }
  }
  throw InternalError(
      "CounterpartIn: every elaboration of one text holds every generate "
      "construct the text wrote");
}

// The value a loop's index stood at for `block`, one of the blocks the loop
// counted out, which is how a name selects it (LRM 27.4). A genvar holds an
// integer (LRM 27.4), so the value always fits.
[[nodiscard]] inline auto LoopIndexOf(
    const slang::ast::GenerateBlockSymbol& block) -> std::int64_t {
  const slang::SVInt* index = block.getArrayIndex();
  const std::optional<std::int64_t> value =
      index == nullptr ? std::nullopt : index->as<std::int64_t>();
  if (!value.has_value()) {
    throw InternalError(
        "LoopIndexOf: a block a loop counted out carries the value its index "
        "stood at");
  }
  return *value;
}

// The select a hierarchical path element wrote for a block of a loop generate
// (LRM 23.6, 27.4), as the front end bound it; null for any other element.
[[nodiscard]] inline auto LoopBlockSelectOf(
    const slang::ast::HierarchicalReference::Element& element)
    -> const slang::ast::Expression* {
  const auto* block = element.symbol->as_if<slang::ast::GenerateBlockSymbol>();
  return block != nullptr && block->getArrayIndex() != nullptr
             ? element.leftExpr
             : nullptr;
}

// The implicit localparam the index name denotes inside one block of a loop
// (LRM 27.4), or nothing for a block no loop counted out. Every block declares
// its own, holding the value the index had when that block elaborated. The
// clause gives it the genvar's name, and the front end marks which declaration
// it is; the mark is what this asks, because a block's other parameters share
// the loop's name with nothing and must not be taken for it.
[[nodiscard]] inline auto LoopIndexParameterOf(
    const slang::ast::GenerateBlockSymbol& block)
    -> const slang::ast::ParameterSymbol* {
  for (const auto& member : block.members()) {
    const auto* parameter = member.as_if<slang::ast::ParameterSymbol>();
    if (parameter != nullptr && parameter->isFromGenvar()) {
      return parameter;
    }
  }
  return nullptr;
}

// Where among its construct's alternatives the source wrote `block`, counted
// from the first. A block no conditional produced is the one its construct
// holds.
[[nodiscard]] inline auto AlternativePositionOf(
    const slang::ast::GenerateBlockSymbol& block) -> std::uint32_t {
  if (!IsAlternative(block)) return 0;
  const auto alternatives = AlternativesOfConstruct(block);
  const auto position = std::ranges::find(alternatives, &block);
  return static_cast<std::uint32_t>(
      std::distance(alternatives.begin(), position));
}

// The label a generate block answers to (LRM 27.6), which every block instance
// built from that text shares: its own for a block standing on its own or
// chosen by a conditional, and for a loop's block the label of its construct
// (LRM 27.4).
[[nodiscard]] inline auto GenerateBlockLabel(
    const slang::ast::GenerateBlockSymbol& block) -> std::string {
  if (block.getArrayIndex() != nullptr) {
    const slang::ast::Scope* array = block.getHierarchicalParent();
    return std::string{
        array == nullptr ? std::string_view{} : array->asSymbol().name};
  }
  return block.getExternalName();
}

// The name LRM 27.6 gives a generate construct's block or loop in the scope
// holding it, whether or not the block was built: a block the source left
// unnamed is named by where its construct stands, and the front end spells
// that name only on the blocks it built.
[[nodiscard]] inline auto ExternalNameOfGenerate(
    const slang::ast::Symbol& member) -> std::optional<std::string> {
  if (const auto* block = member.as_if<slang::ast::GenerateBlockSymbol>()) {
    return block->getExternalName();
  }
  if (const auto* loop = member.as_if<slang::ast::GenerateBlockArraySymbol>()) {
    return loop->getExternalName();
  }
  return std::nullopt;
}

// Which one `block` is among the generate blocks of its scope that carry its
// label, counted in the order the source wrote them: zero for the first, and
// so for a label nothing shares. Alternatives may share a label, those of one
// conditional and those of two of which a scope builds only one (LRM 27.5), and
// the count runs over every alternative the source wrote, built or not, so
// every elaboration of the scope reaches the same number. The blocks of a loop
// are one definition (LRM 27.4) and take the loop's count.
[[nodiscard]] inline auto LabelDisambiguatorOf(
    const slang::ast::GenerateBlockSymbol& block) -> std::uint32_t {
  const slang::ast::Symbol* written = &block;
  if (block.getArrayIndex() != nullptr) {
    const slang::ast::Scope* loop = block.getParentScope();
    if (loop == nullptr) return 0;
    written = &loop->asSymbol();
  }
  const slang::ast::Scope* scope = written->getParentScope();
  if (scope == nullptr) return 0;
  const std::optional<std::string> label = ExternalNameOfGenerate(*written);
  std::uint32_t earlier = 0;
  for (const auto& member : scope->members()) {
    if (&member == written) return earlier;
    if (ExternalNameOfGenerate(member) == label) ++earlier;
  }
  throw InternalError(
      "LabelDisambiguatorOf: a generate block is a member of the scope "
      "holding it");
}

// One block instance as the path of an instance below it spells it: its label,
// which one it is among the blocks of its scope sharing that label where it is
// not the first (LRM 27.5), and for a loop's block the index it elaborated at
// (LRM 27.4).
[[nodiscard]] inline auto BlockInstancePathName(
    const slang::ast::GenerateBlockSymbol& block) -> std::string {
  std::string name = GenerateBlockLabel(block);
  if (const std::uint32_t which = LabelDisambiguatorOf(block); which != 0) {
    name += std::format("#{}", which);
  }
  if (block.getArrayIndex() != nullptr) {
    name += std::format("[{}]", LoopIndexOf(block));
  }
  return name;
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

}  // namespace lyra::lowering::ast_to_hir
