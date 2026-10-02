#pragma once

#include <cstddef>
#include <cstdint>
#include <memory>
#include <optional>
#include <span>
#include <variant>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/symbols/BlockSymbols.h>

#include "lyra/base/arena.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace lyra::lowering::ast_to_hir {

class StructuralScopeLowerer;

// The choices of one conditional generate while the tree of them is still
// being discovered. The front end states each block's own path from the
// outermost construct down (LRM 27.5), so the shape is recovered by walking
// every path and sharing the prefix the paths agree on -- which is what leaves
// each condition stated once however many alternatives stand under it.
//
// It is built here, where a branch can still be filled in after the choice
// holding it exists, and composed into the compiled form at the end. Every
// choice is held behind its own pointer so that discovering a further one
// leaves the branches already found where they are.
class SelectionTree {
 public:
  // Records that `position` stands at the end of `path`, creating whatever
  // choices that path passes through and have not been seen yet.
  auto Place(
      StructuralScopeLowerer& lowerer, WalkFrame frame,
      std::span<const slang::ast::GenerateSelection> path,
      std::uint32_t position) -> diag::Result<void>;

  // The compiled form over `alternatives`, one entry per position placed.
  // Children come before the choices holding them so a branch never names one
  // that does not exist yet. The outermost is always a choice, because a
  // construct is what puts alternatives here at all.
  [[nodiscard]] auto Compose(
      std::vector<std::optional<hir::StructuralScopeId>> alternatives) const
      -> hir::BlocksChoose;

 private:
  // Nothing stands here until something is placed, which is what the first
  // alternative of the variant already means.
  struct Branch {
    std::variant<hir::NothingStands, hir::AlternativeStands, std::size_t> to;
  };

  struct OnCondition {
    hir::ExprId condition;
    Branch holds;
    Branch fails;
  };

  struct Item {
    // What the front end identifies this item by: one span per item, shared by
    // every block beneath it.
    const slang::ast::Expression* const* key = nullptr;
    std::vector<hir::ExprId> labels;
    Branch stands;
  };

  struct OnLabel {
    hir::ExprId selector;
    std::vector<Item> items;
    Branch otherwise;
  };

  struct Choice {
    // What the front end identifies the construct by, the same at every level
    // of every path through it.
    const slang::ast::Expression* key = nullptr;
    std::variant<OnCondition, OnLabel> of;
  };

  auto ChoiceAt(
      StructuralScopeLowerer& lowerer, WalkFrame frame, Branch& branch,
      const slang::ast::GenerateSelection& level) -> diag::Result<std::size_t>;

  static auto ItemAt(
      StructuralScopeLowerer& lowerer, WalkFrame frame, OnLabel& choice,
      std::span<const slang::ast::Expression* const> labels)
      -> diag::Result<std::size_t>;

  [[nodiscard]] auto Resolved(
      const Branch& branch,
      base::Arena<hir::SelectionChoice, hir::SelectionChoiceId>& into) const
      -> hir::SelectionBranch;

  auto ComposeChoice(
      std::size_t at,
      base::Arena<hir::SelectionChoice, hir::SelectionChoiceId>& into) const
      -> hir::SelectionChoiceId;

  Branch root_;
  std::vector<std::unique_ptr<Choice>> choices_;
};

}  // namespace lyra::lowering::ast_to_hir
