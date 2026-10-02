#include "lyra/lowering/ast_to_hir/selection_tree.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <memory>
#include <optional>
#include <span>
#include <utility>
#include <variant>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/symbols/BlockSymbols.h>

#include "lyra/base/arena.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

[[nodiscard]] auto ByLabel(slang::ast::GenerateBranchKind kind) -> bool {
  return kind == slang::ast::GenerateBranchKind::CaseItem ||
         kind == slang::ast::GenerateBranchKind::CaseDefault;
}

auto LowerAndAdd(
    StructuralScopeLowerer& lowerer, WalkFrame frame,
    const slang::ast::Expression& expr) -> diag::Result<hir::ExprId> {
  auto lowered = lowerer.LowerExpr(expr, frame);
  if (!lowered) return std::unexpected(std::move(lowered.error()));
  return frame.Exprs().Add(*std::move(lowered));
}

}  // namespace

auto SelectionTree::Place(
    StructuralScopeLowerer& lowerer, WalkFrame frame,
    std::span<const slang::ast::GenerateSelection> path, std::uint32_t position)
    -> diag::Result<void> {
  using slang::ast::GenerateBranchKind;

  Branch* branch = &root_;
  for (const slang::ast::GenerateSelection& level : path) {
    if (level.condition == nullptr) {
      throw InternalError(
          "SelectionTree::Place: a conditional generate states nothing that "
          "selects its blocks");
    }
    auto reached = ChoiceAt(lowerer, frame, *branch, level);
    if (!reached) return std::unexpected(std::move(reached.error()));
    Choice& choice = *choices_[*reached];
    switch (level.kind) {
      case GenerateBranchKind::IfTrue:
        branch = &std::get<OnCondition>(choice.of).holds;
        continue;
      case GenerateBranchKind::IfFalse:
        branch = &std::get<OnCondition>(choice.of).fails;
        continue;
      case GenerateBranchKind::CaseItem: {
        auto item = ItemAt(
            lowerer, frame, std::get<OnLabel>(choice.of), level.caseItems);
        if (!item) return std::unexpected(std::move(item.error()));
        branch = &std::get<OnLabel>(choice.of).items[*item].stands;
        continue;
      }
      case GenerateBranchKind::CaseDefault:
        branch = &std::get<OnLabel>(choice.of).otherwise;
        continue;
      case GenerateBranchKind::LoopIteration:
      case GenerateBranchKind::IllegalUnconditional:
        break;
    }
    throw InternalError(
        "SelectionTree::Place: a level of a block's selection was reached by "
        "no conditional");
  }
  branch->to = hir::AlternativeStands{.position = position};
  return {};
}

auto SelectionTree::Compose(
    std::vector<std::optional<hir::StructuralScopeId>> alternatives) const
    -> hir::BlocksChoose {
  hir::BlocksChoose chosen;
  chosen.root = ComposeChoice(std::get<std::size_t>(root_.to), chosen.choices);
  chosen.alternatives = std::move(alternatives);
  return chosen;
}

auto SelectionTree::ChoiceAt(
    StructuralScopeLowerer& lowerer, WalkFrame frame, Branch& branch,
    const slang::ast::GenerateSelection& level) -> diag::Result<std::size_t> {
  if (const auto* seen = std::get_if<std::size_t>(&branch.to)) {
    if (choices_[*seen]->key != level.condition) {
      throw InternalError(
          "SelectionTree::ChoiceAt: two alternatives of one construct "
          "disagree about which conditional selects them");
    }
    return *seen;
  }
  auto condition = LowerAndAdd(lowerer, frame, *level.condition);
  if (!condition) return std::unexpected(std::move(condition.error()));
  const std::size_t at = choices_.size();
  auto fresh = std::make_unique<Choice>();
  fresh->key = level.condition;
  if (ByLabel(level.kind)) {
    fresh->of = OnLabel{.selector = *condition, .items = {}, .otherwise = {}};
  } else {
    fresh->of = OnCondition{.condition = *condition, .holds = {}, .fails = {}};
  }
  choices_.push_back(std::move(fresh));
  branch.to = at;
  return at;
}

auto SelectionTree::ItemAt(
    StructuralScopeLowerer& lowerer, WalkFrame frame, OnLabel& choice,
    std::span<const slang::ast::Expression* const> labels)
    -> diag::Result<std::size_t> {
  for (std::size_t at = 0; at < choice.items.size(); ++at) {
    if (choice.items[at].key == labels.data()) return at;
  }
  std::vector<hir::ExprId> lowered;
  lowered.reserve(labels.size());
  for (const slang::ast::Expression* label : labels) {
    auto one = LowerAndAdd(lowerer, frame, *label);
    if (!one) return std::unexpected(std::move(one.error()));
    lowered.push_back(*one);
  }
  // The front end creates a block per item in the order the source wrote
  // them, so appending as they arrive is what puts the items in that order.
  choice.items.push_back(
      Item{.key = labels.data(), .labels = std::move(lowered), .stands = {}});
  return choice.items.size() - 1;
}

auto SelectionTree::Resolved(
    const Branch& branch,
    base::Arena<hir::SelectionChoice, hir::SelectionChoiceId>& into) const
    -> hir::SelectionBranch {
  return std::visit(
      Overloaded{
          [](const hir::NothingStands& nothing) -> hir::SelectionBranch {
            return nothing;
          },
          [](const hir::AlternativeStands& stands) -> hir::SelectionBranch {
            return stands;
          },
          [&](std::size_t at) -> hir::SelectionBranch {
            return ComposeChoice(at, into);
          }},
      branch.to);
}

auto SelectionTree::ComposeChoice(
    std::size_t at,
    base::Arena<hir::SelectionChoice, hir::SelectionChoiceId>& into) const
    -> hir::SelectionChoiceId {
  const Choice& choice = *choices_[at];
  if (const auto* on = std::get_if<OnCondition>(&choice.of)) {
    return into.Add(
        hir::SelectionChoice{hir::ChoiceOnCondition{
            .condition = on->condition,
            .holds = Resolved(on->holds, into),
            .fails = Resolved(on->fails, into)}});
  }
  const auto& on = std::get<OnLabel>(choice.of);
  std::vector<hir::LabeledItem> items;
  items.reserve(on.items.size());
  for (const Item& item : on.items) {
    items.push_back(
        hir::LabeledItem{
            .labels = item.labels, .stands = Resolved(item.stands, into)});
  }
  return into.Add(
      hir::SelectionChoice{hir::ChoiceOnLabel{
          .selector = on.selector,
          .items = std::move(items),
          .otherwise = Resolved(on.otherwise, into)}});
}

}  // namespace lyra::lowering::ast_to_hir
