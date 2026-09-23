#include <cstddef>
#include <cstdint>
#include <expected>
#include <memory>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/ParameterSymbols.h>
#include <slang/ast/symbols/ValueSymbol.h>
#include <slang/ast/types/Type.h>
#include <slang/numeric/SVInt.h>

#include "lyra/base/arena.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/base/registry.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/ast_to_hir/generate_construct.hpp"
#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// Lowers one generate block -- a loop iteration, an `if` / `case` arm, or a
// bare block -- into a fresh concrete structural scope. The source label is
// settled here, on the scope itself; the hierarchy index is not, because a
// loop's blocks differ in it by definition and the scopes are compared with
// each other before anything knows which of them survives.
// `construction_value`, when present, is the block's own index, declared so
// that a name reaching it reads what construction supplied instead of folding
// to one block's value.
auto LowerGenerateScope(
    UnitLowerer& unit_lowerer, const slang::ast::GenerateBlockSymbol& block,
    std::string_view source_name, WalkFrame frame,
    StructuralScopeLowerer::ConstructionValue* construction_value = nullptr)
    -> diag::Result<hir::StructuralScope> {
  StructuralScopeLowerer child(unit_lowerer, block);
  auto scope_or = child.Run(frame, construction_value);
  if (!scope_or) return std::unexpected(std::move(scope_or.error()));
  scope_or->source_name = std::string{source_name};
  return scope_or;
}

// The implicit localparam the index name denotes inside one block (LRM 27.4).
// Every block declares its own, holding the value the index had when that block
// elaborated. The clause gives it the genvar's name, and the front end marks
// which declaration it is; the mark is what this asks, because a block's other
// parameters share the loop's name with nothing and must not be taken for it.
auto BlockIndexParameter(const slang::ast::GenerateBlockSymbol& block)
    -> const slang::ast::ParameterSymbol* {
  for (const auto& member : block.members()) {
    const auto* parameter = member.as_if<slang::ast::ParameterSymbol>();
    if (parameter != nullptr && parameter->isFromGenvar()) {
      return parameter;
    }
  }
  return nullptr;
}

// Whether the loop the source wrote survived elaboration as something a
// construction can run: it counts out at least one block, it is named, and it
// has the three expressions that carry the index from one block to the next.
// How the step is written is not among the conditions, because every form LRM
// 27.4 admits for one states where the index goes next and is carried down as
// written.
auto LoopSurvivedElaboration(const slang::ast::GenerateBlockArraySymbol& array)
    -> bool {
  return array.valid && !array.entries.empty() && !array.name.empty() &&
         array.loopVariable != nullptr && array.initialExpression != nullptr &&
         array.stopExpression != nullptr && array.iterExpression != nullptr;
}

// The loop itself, for a generate whose blocks turned out to be one body: the
// index declared by the scope holding the generate, and the three expressions
// that reach it the way any name reaches a declaration -- the condition reading
// it, the step writing it.
auto BuildTheLoop(
    StructuralScopeLowerer& lowerer,
    const slang::ast::GenerateBlockArraySymbol& array, WalkFrame frame,
    hir::StructuralDataObjectId body_index) -> diag::Result<hir::BlocksRepeat> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  const slang::ast::ValueSymbol& loop_variable = *array.loopVariable;
  const auto span =
      unit_lowerer.SourceMapper().PointSpanOf(loop_variable.location);

  auto index_type = unit_lowerer.InternType(loop_variable.getType(), span);
  if (!index_type) return std::unexpected(std::move(index_type.error()));
  const hir::StructuralDataObjectId variable =
      frame.current_structural_scope->structural_data_objects.Add(
          hir::StructuralDataObjectDecl{
              .name = std::string{loop_variable.name},
              .type = *index_type,
              .kind = hir::StructuralGenvarDecl{}});
  unit_lowerer.MapStructuralDataObjectBinding(
      loop_variable, lowerer.Frame(), variable, *index_type);

  const auto lower =
      [&](const slang::ast::Expression& expr) -> diag::Result<hir::ExprId> {
    auto lowered = lowerer.LowerExpr(expr, frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    return frame.Exprs().Add(*std::move(lowered));
  };
  auto initial = lower(*array.initialExpression);
  if (!initial) return std::unexpected(std::move(initial.error()));
  auto condition = lower(*array.stopExpression);
  if (!condition) return std::unexpected(std::move(condition.error()));
  auto step = lower(*array.iterExpression);
  if (!step) return std::unexpected(std::move(step.error()));

  return hir::BlocksRepeat{
      .variable = variable,
      .initial = *initial,
      .condition = *condition,
      .step = *step,
      .index = body_index};
}

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
      std::uint32_t position) -> diag::Result<void> {
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

  // The compiled form, children before the choices holding them so a branch
  // never names one that does not exist yet. The outermost is always a choice,
  // because a construct is what puts alternatives here at all.
  auto Compose(hir::BlocksChoose& into) const -> void {
    into.root = Compose(std::get<std::size_t>(root_.to), into.choices);
  }

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

  [[nodiscard]] static auto ByLabel(slang::ast::GenerateBranchKind kind)
      -> bool {
    return kind == slang::ast::GenerateBranchKind::CaseItem ||
           kind == slang::ast::GenerateBranchKind::CaseDefault;
  }

  auto ChoiceAt(
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
      fresh->of =
          OnCondition{.condition = *condition, .holds = {}, .fails = {}};
    }
    choices_.push_back(std::move(fresh));
    branch.to = at;
    return at;
  }

  static auto ItemAt(
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

  static auto LowerAndAdd(
      StructuralScopeLowerer& lowerer, WalkFrame frame,
      const slang::ast::Expression& expr) -> diag::Result<hir::ExprId> {
    auto lowered = lowerer.LowerExpr(expr, frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    return frame.Exprs().Add(*std::move(lowered));
  }

  auto Resolved(
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
              return Compose(at, into);
            }},
        branch.to);
  }

  auto Compose(
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

  Branch root_;
  std::vector<std::unique_ptr<Choice>> choices_;
};

auto MergeBlocks(const hir::StructuralScope& a, const hir::StructuralScope& b)
    -> std::optional<hir::StructuralScope>;

// One conditional construct as two of a loop's blocks left it. The alternatives
// and what selects each are the source's, so both blocks state the same ones in
// the same order; what differs is which of them each index selected a body for.
// Merging keeps every body either produced, and where both selected the same
// alternative, that alternative's own scopes have to be one body in their turn.
auto MergeGenerates(const hir::Generate& a, const hir::Generate& b)
    -> std::optional<hir::Generate> {
  if (a == b) return a;
  const auto* chosen_a = std::get_if<hir::BlocksChoose>(&a.counting);
  const auto* chosen_b = std::get_if<hir::BlocksChoose>(&b.counting);
  if (chosen_a == nullptr || chosen_b == nullptr) return std::nullopt;
  if (chosen_a->alternatives.size() != chosen_b->alternatives.size()) {
    return std::nullopt;
  }

  // What selects each alternative is the source's, so the two blocks state the
  // same choices under the same names; reading one against the other says so
  // over the whole tree rather than over a list someone has to keep up to date.
  if (!(chosen_a->choices == chosen_b->choices) ||
      chosen_a->root != chosen_b->root) {
    return std::nullopt;
  }

  hir::Generate merged{};
  hir::BlocksChoose chosen;
  chosen.choices = chosen_a->choices;
  chosen.root = chosen_a->root;
  for (std::size_t at = 0; at < chosen_a->alternatives.size(); ++at) {
    const std::optional<hir::StructuralScopeId> from_a =
        chosen_a->alternatives[at];
    const std::optional<hir::StructuralScopeId> from_b =
        chosen_b->alternatives[at];

    std::optional<hir::StructuralScope> body;
    if (from_a.has_value()) body = a.child_scopes.Get(*from_a);
    if (from_b.has_value()) {
      const hir::StructuralScope& theirs = b.child_scopes.Get(*from_b);
      if (!body.has_value()) {
        body = theirs;
      } else {
        body = MergeBlocks(*body, theirs);
        if (!body.has_value()) return std::nullopt;
      }
    }
    std::optional<hir::StructuralScopeId> block;
    if (body.has_value()) block = merged.child_scopes.Add(*std::move(body));
    chosen.alternatives.push_back(block);
  }
  merged.counting = std::move(chosen);
  return merged;
}

// Two blocks of one loop are one body when they differ only in which
// alternative of a conditional written inside them stood, because what selects
// an alternative is an expression reading the index and the index is a value
// every construction is handed (LRM 27.4, 27.5). What that leaves is one block
// holding every alternative either of them selected. Anything else that
// differs is a difference no construction can supply, and then the blocks are
// children in their own right.
auto MergeBlocks(const hir::StructuralScope& a, const hir::StructuralScope& b)
    -> std::optional<hir::StructuralScope> {
  if (a == b) return a;
  if (a.generates.size() != b.generates.size()) return std::nullopt;

  // The conditionals are the one place a selection can sit, so they are the
  // one part allowed to differ. Reading one block with the other's in place of
  // its own says whether anything else did, over every field there is rather
  // than over a list someone has to keep up to date.
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

}  // namespace

auto StructuralScopeLowerer::BuildGenerateFromArray(
    const slang::ast::GenerateBlockArraySymbol& array, WalkFrame frame)
    -> diag::Result<hir::Generate> {
  // Every block is lowered, and the index reaches each one as a value its
  // construction supplies rather than as a constant folded into it. That is
  // what makes one body possible at all, and it is also what makes the blocks
  // comparable: two that differ in nothing else then lower to the same scope.
  //
  // Whether one scope serves them all is what the lowered scopes say, and the
  // comparison is derived from the node definitions rather than written, so a
  // field added anywhere below is compared without anyone remembering to.
  std::vector<hir::StructuralScope> blocks;
  blocks.reserve(array.entries.size());
  std::optional<hir::StructuralDataObjectId> first_index;
  for (const auto* entry : array.entries) {
    ConstructionValue index{.parameter = BlockIndexParameter(*entry)};
    auto scope_or = LowerGenerateScope(
        *owner_, *entry, array.name, frame,
        index.parameter == nullptr ? nullptr : &index);
    if (!scope_or) return std::unexpected(std::move(scope_or.error()));
    if (blocks.empty() && index.parameter != nullptr) {
      first_index = index.declared;
    }
    blocks.push_back(*std::move(scope_or));
  }

  std::optional<hir::StructuralScope> one_body;
  if (LoopSurvivedElaboration(array) && first_index.has_value()) {
    one_body = blocks.front();
    for (std::size_t at = 1; one_body.has_value() && at < blocks.size(); ++at) {
      one_body = MergeBlocks(*one_body, blocks[at]);
    }
  }

  hir::Generate gen{};
  if (one_body.has_value()) {
    auto counting = BuildTheLoop(*this, array, frame, *first_index);
    if (!counting) return std::unexpected(std::move(counting.error()));
    gen.child_scopes.Add(*std::move(one_body));
    gen.counting = *std::move(counting);
    return gen;
  }

  // Blocks that disagree are children in their own right, and the hierarchy
  // index each was elaborated at is what tells them apart -- which is why it
  // takes no part in the comparison above and is stamped on only once the
  // comparison is over.
  for (std::size_t at = 0; at < blocks.size(); ++at) {
    const slang::SVInt* array_index = array.entries[at]->getArrayIndex();
    if (array_index == nullptr) {
      throw InternalError(
          "StructuralScopeLowerer::BuildGenerateFromArray: loop iteration "
          "entry carries no array index");
    }
    blocks[at].index = array_index->as<std::int64_t>().value_or(0);
    gen.child_scopes.Add(std::move(blocks[at]));
  }
  return gen;
}

auto StructuralScopeLowerer::BuildGenerateFromBlock(
    const slang::ast::GenerateBlockSymbol& block, WalkFrame frame)
    -> diag::Result<hir::Generate> {
  // A block no conditional produced stands on its own, and there is exactly
  // one of it.
  if (!IsAlternative(block)) {
    hir::Generate gen{};
    auto scope_or = LowerGenerateScope(*owner_, block, block.name, frame);
    if (!scope_or) return std::unexpected(std::move(scope_or.error()));
    gen.child_scopes.Add(*std::move(scope_or));
    return gen;
  }

  // What selects an alternative is the source's and is stated wherever the
  // construct stands, so every alternative is lowered here whether or not this
  // elaboration selected it (LRM 27.5); a body is lowered only for the one
  // that was, because an unselected block is not part of the model and its own
  // names need not resolve.
  hir::Generate gen{};
  hir::BlocksChoose chosen;
  SelectionTree selection;
  for (const auto* arm : AlternativesOfConstruct(block)) {
    const auto position =
        static_cast<std::uint32_t>(chosen.alternatives.size());
    auto placed = selection.Place(*this, frame, arm->selectionPath, position);
    if (!placed) return std::unexpected(std::move(placed.error()));
    std::optional<hir::StructuralScopeId> body;
    if (!arm->isUninstantiated) {
      auto scope_or = LowerGenerateScope(*owner_, *arm, arm->name, frame);
      if (!scope_or) return std::unexpected(std::move(scope_or.error()));
      body = gen.child_scopes.Add(*std::move(scope_or));
    }
    chosen.alternatives.push_back(body);
  }
  selection.Compose(chosen);
  gen.counting = std::move(chosen);
  return gen;
}

}  // namespace lyra::lowering::ast_to_hir
