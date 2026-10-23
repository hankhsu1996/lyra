#include "lyra/lowering/ast_to_hir/covergroup_decl.hpp"

#include <cstddef>
#include <expected>
#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include <slang/ast/ASTVisitor.h>
#include <slang/ast/Compilation.h>
#include <slang/ast/Expression.h>
#include <slang/ast/TimingControl.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/ClassSymbols.h>
#include <slang/ast/symbols/CoverSymbols.h>
#include <slang/ast/symbols/ParameterSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/hir/covergroup.hpp"
#include "lyra/hir/procedural_scope.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/statement/timing.hpp"
#include "lyra/lowering/ast_to_hir/std_classes.hpp"
#include "lyra/lowering/ast_to_hir/subroutine_decl.hpp"
#include "lyra/lowering/ast_to_hir/unit_identity.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

auto Fail(diag::SourceSpan span, std::string_view what) {
  return diag::Fail(
      span, diag::DiagCode::kUnsupportedClassFeature, "{} is not yet supported",
      what);
}

// The instance or the class a symbol is declared in: what replicates it.
auto ReplicatorOf(const slang::ast::Symbol& symbol)
    -> const slang::ast::Symbol* {
  for (const slang::ast::Scope* scope = symbol.getParentScope();
       scope != nullptr;) {
    const slang::ast::Symbol& at = scope->asSymbol();
    if (at.kind == slang::ast::SymbolKind::InstanceBody ||
        at.kind == slang::ast::SymbolKind::ClassType) {
      return &at;
    }
    scope = at.getParentScope();
  }
  return nullptr;
}

// The variable a `with` filter names its value by (LRM 19.5.1.1, 19.6.1.2),
// which the front end declares for the filter's expression alone: every one
// the expression names.
auto IteratorsOf(const slang::ast::Expression& filter)
    -> std::vector<const slang::ast::IteratorSymbol*> {
  struct FindsIterators
      : slang::ast::ASTVisitor<
            FindsIterators, slang::ast::VisitFlags::Expressions> {
    std::vector<const slang::ast::IteratorSymbol*> found;
    void handle(const slang::ast::NamedValueExpression& named) {
      if (const auto* iterator =
              named.symbol.as_if<slang::ast::IteratorSymbol>()) {
        found.push_back(iterator);
      }
    }
  };
  FindsIterators finder;
  filter.visit(finder);
  return finder.found;
}

// The name of each coverpoint and each cross the covergroup declares, in
// source order. An item the source gave no name is named only for its report
// and the database (LRM 19.5), by a name holding a space, which no identifier
// does, escaped ones included (LRM 5.6.1), so no label of the source can be it.
struct ItemNames {
  std::vector<std::string> coverpoints;
  std::vector<std::string> crosses;
};

auto ItemNamesOf(const slang::ast::CovergroupBodySymbol& body) -> ItemNames {
  ItemNames names;
  std::size_t unnamed = 0;
  const auto name_of = [&](const slang::ast::Symbol& item,
                           std::string_view kind) -> std::string {
    if (!item.name.empty()) return std::string{item.name};
    return std::format("{} {}", kind, unnamed++);
  };
  for (const auto& point : body.membersOfType<slang::ast::CoverpointSymbol>()) {
    names.coverpoints.push_back(name_of(point, "coverpoint"));
  }
  for (const auto& cross : body.membersOfType<slang::ast::CoverCrossSymbol>()) {
    names.crosses.push_back(name_of(cross, "cross"));
  }
  return names;
}

// The covergroup `covergroup` extends, where it extends one (LRM 19.4.1).
auto BaseOf(const slang::ast::CovergroupType& covergroup)
    -> const slang::ast::CovergroupType* {
  const slang::ast::Type* base = covergroup.getBaseGroup();
  if (base == nullptr || base->isError()) return nullptr;
  return &base->as<slang::ast::CovergroupType>();
}

// The class embedding `covergroup`, where a class declares it (LRM 19.4).
auto EmbeddingOf(const slang::ast::CovergroupType& covergroup)
    -> const slang::ast::ClassType* {
  return covergroup.getParentScope()->asSymbol().as_if<slang::ast::ClassType>();
}

// The formals of a covergroup, and those of its `sample` (LRM 19.8.1), which
// one extending another takes from the first of its lineage (LRM 19.4.1).
auto FormalsOf(const slang::ast::CovergroupType& first)
    -> std::span<const slang::ast::FormalArgumentSymbol* const> {
  return first.getArguments();
}

auto SampleFormalsOf(const slang::ast::CovergroupType& first)
    -> std::vector<const slang::ast::FormalArgumentSymbol*> {
  std::vector<const slang::ast::FormalArgumentSymbol*> formals;
  for (const auto& formal :
       first.membersOfType<slang::ast::FormalArgumentSymbol>()) {
    if (formal.flags.has(slang::ast::VariableFlags::CoverageSampleFormal)) {
      formals.push_back(&formal);
    }
  }
  return formals;
}

// The covergroups a covergroup is built over, from the first of its lineage to
// itself (LRM 19.4.1).
auto LineageOf(const slang::ast::CovergroupType& covergroup)
    -> std::vector<const slang::ast::CovergroupType*> {
  std::vector<const slang::ast::CovergroupType*> lineage;
  for (const slang::ast::CovergroupType* level = &covergroup; level != nullptr;
       level = BaseOf(*level)) {
    lineage.insert(lineage.begin(), level);
  }
  return lineage;
}

// The type a coverpoint's values are compared at (LRM 19.5.7), which the class
// counting them is specialized on: an enumeration's own where the coverpoint is
// one, which the coverpoint's declared type, its base type, does not say.
auto ValueTypeOf(const slang::ast::ClassType& counted_by)
    -> const slang::ast::Type& {
  const slang::ast::Symbol* parameter = counted_by.find("T");
  if (parameter == nullptr ||
      parameter->kind != slang::ast::SymbolKind::TypeParameter) {
    return counted_by.getCompilation().getRealType();
  }
  return parameter->as<slang::ast::TypeParameterSymbol>().targetType.getType();
}

// Builds one covergroup's declaration: its body, the root scope its variables
// are declared in, and the frame its expressions are lowered under.
class DeclBuilder {
 public:
  DeclBuilder(
      UnitLowerer& owner, const slang::ast::CovergroupType& covergroup,
      const WalkFrame& declaring_frame, diag::SourceSpan span)
      : owner_(&owner),
        covergroup_(&covergroup),
        span_(span),
        lowerer_(owner, covergroup),
        root_(
            decl_.procedural_scopes.Declare(),
            hir::ProceduralScopeKind::kSubroutineRoot,
            std::string{CovergroupNameOf(covergroup)}) {
    decl_.body = owner.MakeProceduralBody(covergroup);
    frame_ =
        declaring_frame
            .WithProceduralScopeOwner(&covergroup, &decl_.procedural_scopes)
            .WithProceduralBody(&decl_.body)
            .WithOpenScope(&root_);
  }
  DeclBuilder(const DeclBuilder&) = delete;
  DeclBuilder(DeclBuilder&&) = delete;
  auto operator=(const DeclBuilder&) -> DeclBuilder& = delete;
  auto operator=(DeclBuilder&&) -> DeclBuilder& = delete;
  ~DeclBuilder() = default;

  auto Build() -> diag::Result<hir::CovergroupDecl> {
    const std::vector<const slang::ast::CovergroupType*> lineage =
        LineageOf(*covergroup_);
    const slang::ast::CovergroupType& first = *lineage.front();

    // A covergroup a class embeds reads that object's members (LRM 19.4).
    if (const slang::ast::ClassType* embedding = EmbeddingOf(*covergroup_)) {
      auto type = owner_->InternType(*embedding, span_);
      if (!type) return std::unexpected(std::move(type.error()));
      const hir::ProceduralVarId object = Anonymous(*type);
      decl_.embedding = object;
      frame_ = frame_.WithEmbedding(
          CovergroupEmbedding{.cls = embedding, .object = object});
    }
    // One extending another takes its formals and those of its `sample`
    // (LRM 19.4.1), which are what its expressions name.
    for (const slang::ast::FormalArgumentSymbol* formal : FormalsOf(first)) {
      auto var = Variable(*formal);
      if (!var) return std::unexpected(std::move(var.error()));
      decl_.formals.push_back(
          hir::SubroutineParam{
              .var = *var, .direction = ParamDirectionOf(*formal)});
    }
    for (const slang::ast::FormalArgumentSymbol* formal :
         SampleFormalsOf(first)) {
      auto var = Variable(*formal);
      if (!var) return std::unexpected(std::move(var.error()));
      decl_.sample_formals.push_back(
          hir::SubroutineParam{
              .var = *var, .direction = ParamDirectionOf(*formal)});
    }
    if (lineage.size() > 1) {
      auto base =
          owner_->ResolveCovergroupRef(*lineage[lineage.size() - 2], span_);
      if (!base) return std::unexpected(std::move(base.error()));
      decl_.base = *base;
    }
    // One extending another is sampled on the event of the first of its
    // lineage (LRM 19.4.1), which that one starts.
    auto sampling = Sampling(first);
    if (!sampling) return std::unexpected(std::move(sampling.error()));
    decl_.sampling = *std::move(sampling);

    const slang::ast::CovergroupBodySymbol& body = covergroup_->getBody();
    auto options = Options(body.options);
    if (!options) return std::unexpected(std::move(options.error()));
    decl_.options = *std::move(options);
    ItemNames names = ItemNamesOf(body);
    std::size_t point_index = 0;
    for (const auto& point :
         body.membersOfType<slang::ast::CoverpointSymbol>()) {
      auto lowered =
          Coverpoint(point, std::move(names.coverpoints[point_index++]));
      if (!lowered) return std::unexpected(std::move(lowered.error()));
      decl_.coverpoints.push_back(*std::move(lowered));
    }
    std::size_t cross_index = 0;
    for (const auto& cross :
         body.membersOfType<slang::ast::CoverCrossSymbol>()) {
      auto lowered = Cross(cross, std::move(names.crosses[cross_index++]));
      if (!lowered) return std::unexpected(std::move(lowered.error()));
      decl_.crosses.push_back(*std::move(lowered));
    }
    decl_.body.root_scope = frame_.SealScope(std::move(root_));
    decl_.span = span_;
    return std::move(decl_);
  }

 private:
  auto AddLowered(const slang::ast::Expression& expr)
      -> diag::Result<hir::ExprId> {
    auto lowered = lowerer_.LowerExpr(expr, frame_);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    return decl_.body.exprs.Add(*std::move(lowered));
  }

  auto Variable(const slang::ast::VariableSymbol& variable)
      -> diag::Result<hir::ProceduralVarId> {
    auto type = owner_->InternType(variable.getType(), span_);
    if (!type) return std::unexpected(std::move(type.error()));
    return lowerer_.AddProceduralVar(frame_, decl_.body, variable, *type);
  }

  auto Anonymous(hir::TypeId type) -> hir::ProceduralVarId {
    const hir::ProceduralVarId var = decl_.body.procedural_vars.Declare();
    decl_.body.procedural_vars.Define(
        var, hir::ProceduralVarDecl{.name = std::nullopt, .type = type});
    root_.declarations.push_back(var);
    return var;
  }

  auto Sampling(const slang::ast::CovergroupType& first)
      -> diag::Result<hir::CoverageSampling> {
    const slang::ast::TimingControl* event = first.getCoverageEvent();
    if (event == nullptr) return hir::CoverageSampling{hir::SampledByCall{}};
    if (event->kind != slang::ast::TimingControlKind::BlockEventList) {
      auto control = LowerEventControl(lowerer_, frame_, *event, span_);
      if (!control) return std::unexpected(std::move(control.error()));
      return hir::CoverageSampling{
          hir::SampledOnEvent{.event = *std::move(control)}};
    }
    // A block of the scope or class the covergroup belongs to is what
    // observes it; another one is code of another scope (LRM 19.3).
    hir::SampledAtBlocks at_blocks;
    const slang::ast::Symbol* home = ReplicatorOf(first);
    const slang::ast::Scope* replicating = &ReplicatingScope(first);
    for (const slang::ast::BlockEventListControl::Event& block :
         event->as<slang::ast::BlockEventListControl>().events) {
      const slang::ast::Symbol* target = block.target->getSymbolReference();
      if (target == nullptr || ReplicatorOf(*target) != home ||
          &ReplicatingScope(*target) != replicating) {
        return Fail(
            span_,
            "a covergroup sampled where a block of another scope or class "
            "begins or ends");
      }
      at_blocks.events.push_back(
          hir::BlockEvent{
              .block = owner_->LookupProceduralScope(*target),
              .at_begin = block.isBegin});
    }
    return hir::CoverageSampling{std::move(at_blocks)};
  }

  auto Options(std::span<const slang::ast::CoverageOptionSetter> setters)
      -> diag::Result<std::vector<hir::OptionSetting>> {
    std::vector<hir::OptionSetting> options;
    for (const slang::ast::CoverageOptionSetter& setter : setters) {
      const auto& assigned =
          setter.getExpression().as<slang::ast::AssignmentExpression>();
      auto value = AddLowered(assigned.right());
      if (!value) return std::unexpected(std::move(value.error()));
      options.push_back(
          hir::OptionSetting{
              .type_option = setter.isTypeOption(),
              .field = std::string{setter.getName()},
              .value = *value});
    }
    return options;
  }

  auto Element(const slang::ast::Expression& value)
      -> diag::Result<hir::BinElement> {
    const auto* range = value.as_if<slang::ast::ValueRangeExpression>();
    if (range == nullptr) {
      auto lowered = AddLowered(value);
      if (!lowered) return std::unexpected(std::move(lowered.error()));
      return hir::BinElement{hir::BinValue{.value = *lowered}};
    }
    const auto end = [&](const slang::ast::Expression& written)
        -> diag::Result<std::optional<hir::ExprId>> {
      if (written.kind == slang::ast::ExpressionKind::UnboundedLiteral) {
        return std::optional<hir::ExprId>{};
      }
      auto lowered = AddLowered(written);
      if (!lowered) return std::unexpected(std::move(lowered.error()));
      return std::optional<hir::ExprId>{*lowered};
    };
    auto left = end(range->left());
    if (!left) return std::unexpected(std::move(left.error()));
    auto right = end(range->right());
    if (!right) return std::unexpected(std::move(right.error()));
    switch (range->rangeKind) {
      case slang::ast::ValueRangeKind::Simple:
        return hir::BinElement{hir::BinRange{.low = *left, .high = *right}};
      case slang::ast::ValueRangeKind::AbsoluteTolerance:
      case slang::ast::ValueRangeKind::RelativeTolerance:
        return hir::BinElement{hir::BinTolerance{
            .center = **left,
            .tolerance = **right,
            .in_percent = range->rangeKind ==
                          slang::ast::ValueRangeKind::RelativeTolerance}};
    }
    throw InternalError("DeclBuilder::Element: unknown kind of value range");
  }

  auto Elements(std::span<const slang::ast::Expression* const> values)
      -> diag::Result<std::vector<hir::BinElement>> {
    std::vector<hir::BinElement> elements;
    for (const slang::ast::Expression* value : values) {
      auto element = Element(*value);
      if (!element) return std::unexpected(std::move(element.error()));
      elements.push_back(*std::move(element));
    }
    return elements;
  }

  static auto KindOf(const slang::ast::CoverageBinSymbol& bin) -> hir::BinKind {
    switch (bin.binsKind) {
      case slang::ast::CoverageBinSymbol::Bins:
        return hir::BinKind::kBins;
      case slang::ast::CoverageBinSymbol::IgnoreBins:
        return hir::BinKind::kIgnoreBins;
      case slang::ast::CoverageBinSymbol::IllegalBins:
        return hir::BinKind::kIllegalBins;
    }
    throw InternalError("DeclBuilder::KindOf: unknown kind of bins");
  }

  static auto RepetitionOf(
      slang::ast::CoverageBinSymbol::TransRangeList::RepeatKind kind)
      -> hir::StepRepetition {
    using Kind = slang::ast::CoverageBinSymbol::TransRangeList::RepeatKind;
    switch (kind) {
      case Kind::None:
        return hir::StepRepetition::kOnce;
      case Kind::Consecutive:
        return hir::StepRepetition::kConsecutive;
      case Kind::GoTo:
        return hir::StepRepetition::kGoTo;
      case Kind::Nonconsecutive:
        return hir::StepRepetition::kNonconsecutive;
    }
    throw InternalError(
        "DeclBuilder::RepetitionOf: unknown kind of repetition");
  }

  auto Optional(const slang::ast::Expression* expr)
      -> diag::Result<std::optional<hir::ExprId>> {
    if (expr == nullptr) return std::optional<hir::ExprId>{};
    auto lowered = AddLowered(*expr);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    return std::optional<hir::ExprId>{*lowered};
  }

  auto Form(const slang::ast::CoverageBinSymbol& bin)
      -> diag::Result<hir::BinForm> {
    if (bin.isDefaultSequence) return hir::BinForm{hir::DefaultSequenceBins{}};
    if (bin.isDefault) {
      return hir::BinForm{hir::DefaultBins{.per_value = bin.isArray}};
    }
    if (!bin.getTransList().empty()) {
      hir::TransitionBins transitions{
          .per_sequence = bin.isArray,
          .wildcard = bin.isWildcard,
          .sequences = {}};
      for (const auto& sequence : bin.getTransList()) {
        std::vector<hir::TransitionStep> steps;
        for (const auto& step : sequence) {
          auto values = Elements(step.items);
          if (!values) return std::unexpected(std::move(values.error()));
          auto least = Optional(step.repeatFrom);
          if (!least) return std::unexpected(std::move(least.error()));
          auto most = Optional(step.repeatTo);
          if (!most) return std::unexpected(std::move(most.error()));
          steps.push_back(
              hir::TransitionStep{
                  .values = *std::move(values),
                  .repetition = RepetitionOf(step.repeatKind),
                  .least = *least,
                  .most = *most});
        }
        transitions.sequences.push_back(std::move(steps));
      }
      return hir::BinForm{std::move(transitions)};
    }
    hir::BinArray array = hir::OneBin{};
    if (bin.isArray) {
      array = hir::BinPerValue{};
      if (const slang::ast::Expression* count = bin.getNumberOfBinsExpr()) {
        auto lowered = AddLowered(*count);
        if (!lowered) return std::unexpected(std::move(lowered.error()));
        array = hir::FixedBins{.count = *lowered};
      }
    }
    auto elements = Elements(bin.getValues());
    if (!elements) return std::unexpected(std::move(elements.error()));
    auto set = Optional(bin.getSetCoverageExpr());
    if (!set) return std::unexpected(std::move(set.error()));
    std::optional<hir::BinFilter> filter;
    if (const slang::ast::Expression* with = bin.getWithExpr()) {
      const std::vector<const slang::ast::IteratorSymbol*> items =
          IteratorsOf(*with);
      if (items.empty()) {
        throw InternalError(
            "DeclBuilder::Form: a 'with' filter names its value, which the "
            "front end binds whether or not the filter reads it");
      }
      auto item = Variable(*items.front());
      if (!item) return std::unexpected(std::move(item.error()));
      auto keeps = AddLowered(*with);
      if (!keeps) return std::unexpected(std::move(keeps.error()));
      filter = hir::BinFilter{.item = *item, .keeps = *keeps};
    }
    return hir::BinForm{hir::ValueBins{
        .array = array,
        .wildcard = bin.isWildcard,
        .elements = *std::move(elements),
        .set = *set,
        .filter = filter}};
  }

  auto Construct(const slang::ast::CoverageBinSymbol& bin)
      -> diag::Result<hir::BinConstruct> {
    auto form = Form(bin);
    if (!form) return std::unexpected(std::move(form.error()));
    auto iff = Optional(bin.getIffExpr());
    if (!iff) return std::unexpected(std::move(iff.error()));
    return hir::BinConstruct{
        .name = std::string{bin.name},
        .kind = KindOf(bin),
        .iff = *iff,
        .form = *std::move(form)};
  }

  auto Coverpoint(const slang::ast::CoverpointSymbol& point, std::string name)
      -> diag::Result<hir::Coverpoint> {
    auto expression = AddLowered(point.getCoverageExpr());
    if (!expression) return std::unexpected(std::move(expression.error()));
    auto iff = Optional(point.getIffExpr());
    if (!iff) return std::unexpected(std::move(iff.error()));
    auto options = Options(point.options);
    if (!options) return std::unexpected(std::move(options.error()));
    std::vector<hir::BinConstruct> bins;
    for (const auto& bin :
         point.membersOfType<slang::ast::CoverageBinSymbol>()) {
      auto construct = Construct(bin);
      if (!construct) return std::unexpected(std::move(construct.error()));
      bins.push_back(*std::move(construct));
    }
    return hir::Coverpoint{
        .name = std::move(name),
        .expression = *expression,
        .iff = *iff,
        .options = *std::move(options),
        .bins = std::move(bins)};
  }

  auto Matches(const slang::ast::Expression* matches)
      -> diag::Result<hir::MatchesRequired> {
    if (matches == nullptr) return hir::MatchesRequired{hir::OneMatch{}};
    if (matches->kind == slang::ast::ExpressionKind::UnboundedLiteral) {
      return hir::MatchesRequired{hir::EveryMatch{}};
    }
    auto lowered = AddLowered(*matches);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    return hir::MatchesRequired{*lowered};
  }

  auto Select(
      const slang::ast::CoverCrossSymbol& cross,
      const slang::ast::BinsSelectExpr& expr,
      base::Arena<hir::CrossSelect, hir::CrossSelectId>& selects)
      -> diag::Result<hir::CrossSelectId> {
    const auto add = [&](hir::CrossSelect select) {
      return selects.Add(std::move(select));
    };
    switch (expr.kind) {
      case slang::ast::BinsSelectExprKind::Condition: {
        const auto& condition = expr.as<slang::ast::ConditionBinsSelectExpr>();
        const slang::ast::Symbol* target = &condition.target;
        std::optional<std::string> bin;
        if (target->kind == slang::ast::SymbolKind::CoverageBin) {
          bin = std::string{target->name};
          target = &target->getParentScope()->asSymbol();
        }
        auto intersect = Elements(condition.intersects);
        if (!intersect) return std::unexpected(std::move(intersect.error()));
        return add(
            hir::SelectBinsOf{
                .point = std::string{target->name},
                .bin = std::move(bin),
                .intersect = *std::move(intersect)});
      }
      case slang::ast::BinsSelectExprKind::Unary: {
        auto operand = Select(
            cross, expr.as<slang::ast::UnaryBinsSelectExpr>().expr, selects);
        if (!operand) return operand;
        return add(hir::SelectNot{.operand = *operand});
      }
      case slang::ast::BinsSelectExprKind::Binary: {
        const auto& binary = expr.as<slang::ast::BinaryBinsSelectExpr>();
        auto left = Select(cross, binary.left, selects);
        if (!left) return left;
        auto right = Select(cross, binary.right, selects);
        if (!right) return right;
        switch (binary.op) {
          case slang::ast::BinaryBinsSelectExpr::And:
            return add(hir::SelectAnd{.left = *left, .right = *right});
          case slang::ast::BinaryBinsSelectExpr::Or:
            return add(hir::SelectOr{.left = *left, .right = *right});
        }
        throw InternalError("DeclBuilder::Select: unknown binary operator");
      }
      case slang::ast::BinsSelectExprKind::CrossId:
        return add(hir::SelectAll{});
      case slang::ast::BinsSelectExprKind::SetExpr: {
        const auto& set = expr.as<slang::ast::SetExprBinsSelectExpr>();
        const slang::ast::Type* element = set.expr.type->getArrayElementType();
        const auto* fields = element == nullptr
                                 ? nullptr
                                 : element->getCanonicalType()
                                       .as_if<slang::ast::UnpackedStructType>();
        if (fields == nullptr) {
          throw InternalError(
              "DeclBuilder::Select: the front end admits only the cross's "
              "queue of value tuples as its set expression");
        }
        // The front end declares the tuple's structure with no fields, so
        // there is no component of a tuple to read.
        if (fields->fields.size() != cross.targets.size()) {
          return Fail(
              span_, "a cross bin given by a set expression of value tuples");
        }
        auto tuples = AddLowered(set.expr);
        if (!tuples) return std::unexpected(std::move(tuples.error()));
        auto matches = Matches(set.matchesExpr);
        if (!matches) return std::unexpected(std::move(matches.error()));
        return add(hir::SelectSet{.tuples = *tuples, .matches = *matches});
      }
      case slang::ast::BinsSelectExprKind::WithFilter: {
        const auto& with = expr.as<slang::ast::BinSelectWithFilterExpr>();
        for (const slang::ast::CoverpointSymbol* point : cross.targets) {
          if (point->getType().isFloating()) {
            return Fail(
                span_,
                "a cross's 'with' filter over a coverpoint of real values");
          }
        }
        auto candidates = Select(cross, with.expr, selects);
        if (!candidates) return candidates;
        const std::vector<const slang::ast::IteratorSymbol*> iterators =
            IteratorsOf(with.filter);
        std::vector<std::optional<hir::ProceduralVarId>> values(
            cross.targets.size());
        for (std::size_t d = 0; d < cross.targets.size(); ++d) {
          for (const slang::ast::IteratorSymbol* iterator : iterators) {
            if (iterator->name != cross.targets[d]->name) continue;
            auto var = Variable(*iterator);
            if (!var) return std::unexpected(std::move(var.error()));
            values[d] = *var;
          }
        }
        auto filter = AddLowered(with.filter);
        if (!filter) return std::unexpected(std::move(filter.error()));
        auto matches = Matches(with.matchesExpr);
        if (!matches) return std::unexpected(std::move(matches.error()));
        return add(
            hir::SelectWith{
                .candidates = *candidates,
                .values = std::move(values),
                .filter = *filter,
                .matches = *matches});
      }
      case slang::ast::BinsSelectExprKind::Invalid:
        break;
    }
    throw InternalError(
        "DeclBuilder::Select: the front end hands over no invalid select "
        "expression it accepted");
  }

  auto Cross(const slang::ast::CoverCrossSymbol& cross, std::string name)
      -> diag::Result<hir::Cross> {
    hir::Cross lowered{
        .name = std::move(name),
        .targets = {},
        .iff = std::nullopt,
        .options = {},
        .selects = {},
        .bins = {}};
    for (const slang::ast::CoverpointSymbol* target : cross.targets) {
      lowered.targets.emplace_back(target->name);
    }
    auto iff = Optional(cross.getIffExpr());
    if (!iff) return std::unexpected(std::move(iff.error()));
    lowered.iff = *iff;
    auto options = Options(cross.options);
    if (!options) return std::unexpected(std::move(options.error()));
    lowered.options = *std::move(options);
    for (const auto& body :
         cross.membersOfType<slang::ast::CoverCrossBodySymbol>()) {
      for (const auto& bin :
           body.membersOfType<slang::ast::CoverageBinSymbol>()) {
        const slang::ast::BinsSelectExpr* select = bin.getCrossSelectExpr();
        if (select == nullptr) {
          throw InternalError(
              "DeclBuilder::Cross: every bin of a cross is given by a select "
              "expression");
        }
        auto selected = Select(cross, *select, lowered.selects);
        if (!selected) return std::unexpected(std::move(selected.error()));
        auto bin_iff = Optional(bin.getIffExpr());
        if (!bin_iff) return std::unexpected(std::move(bin_iff.error()));
        lowered.bins.push_back(
            hir::CrossBin{
                .name = std::string{bin.name},
                .kind = KindOf(bin),
                .select = *selected,
                .iff = *bin_iff});
      }
    }
    return lowered;
  }

  UnitLowerer* owner_;
  const slang::ast::CovergroupType* covergroup_;
  diag::SourceSpan span_;
  hir::CovergroupDecl decl_;
  ProcessLowerer lowerer_;
  OpenProceduralScope root_;
  WalkFrame frame_;
};

}  // namespace

auto LowerCovergroupDecl(
    UnitLowerer& unit_lowerer, const slang::ast::CovergroupType& covergroup,
    const WalkFrame& declaring_frame, diag::SourceSpan span)
    -> diag::Result<hir::CovergroupDecl> {
  DeclBuilder builder(unit_lowerer, covergroup, declaring_frame, span);
  return builder.Build();
}

auto UnitLowerer::ResolveCovergroupRef(
    const slang::ast::CovergroupType& covergroup, diag::SourceSpan span)
    -> diag::Result<hir::CovergroupRef> {
  if (const auto it = covergroup_cache_.find(&covergroup);
      it != covergroup_cache_.end()) {
    return it->second;
  }
  if (!Owns(covergroup)) {
    hir::ExternalCovergroupRef external{
        .unit_name =
            CompilationUnitName(UnitHomeOf(covergroup), Specialization()),
        .path = DefPathOf(covergroup, Specialization())};
    if (auto named = NameCoverageLibrary(span); !named) {
      return std::unexpected(std::move(named.error()));
    }
    const auto [it, _] = covergroup_cache_.emplace(
        &covergroup, hir::CovergroupRef{std::move(external)});
    return it->second;
  }
  auto id = InternLocalCovergroup(covergroup, span);
  if (!id) return std::unexpected(std::move(id.error()));
  return hir::CovergroupRef{hir::LocalCovergroupRef{.covergroup = *id}};
}

auto UnitLowerer::InternLocalCovergroup(
    const slang::ast::CovergroupType& covergroup, diag::SourceSpan span)
    -> diag::Result<hir::CovergroupId> {
  if (const auto it = covergroup_cache_.find(&covergroup);
      it != covergroup_cache_.end()) {
    return std::get<hir::LocalCovergroupRef>(it->second).covergroup;
  }
  if (auto named = NameCoverageLibrary(span); !named) {
    return std::unexpected(std::move(named.error()));
  }
  // A covergroup is one declaration of its unit wherever its text was
  // elaborated, as a class is: a copy at a path already declared is that
  // covergroup.
  support::DefPath path = DefPathOf(covergroup, Specialization());
  const auto declared = own_covergroups_by_path_.find(path);
  const hir::CovergroupId id = declared == own_covergroups_by_path_.end()
                                   ? unit_.covergroups.Declare(std::move(path))
                                   : std::get<hir::LocalCovergroupRef>(
                                         covergroup_cache_.at(declared->second))
                                         .covergroup;
  covergroup_cache_.emplace(
      &covergroup,
      hir::CovergroupRef{hir::LocalCovergroupRef{.covergroup = id}});
  own_covergroups_by_path_.emplace(unit_.covergroups.PathOf(id), &covergroup);
  // Its expressions reach what encloses it as a body of the scope replicating
  // it does, so it lowers with that scope (LRM 6.22, 19.3).
  const slang::ast::Scope& replicating = ReplicatingScope(covergroup);
  covergroups_by_scope_[&replicating].push_back(id);
  // One sampled where a block of the instance begins or ends is told so by
  // a cell the instance keeps for the covergroup's type (LRM 19.3); one a class
  // embeds is told through the object holding it.
  const slang::ast::TimingControl* event = covergroup.getCoverageEvent();
  if (EmbeddingOf(covergroup) == nullptr && BelongsToAnInstance(covergroup) &&
      event != nullptr &&
      event->kind == slang::ast::TimingControlKind::BlockEventList) {
    scope_publications_.at(&replicating)
        .members.emplace_back(
            ScopePublicationRecord::CovergroupEvent{.symbol = &covergroup});
  }
  pending_covergroups_[&replicating].push_back(
      PendingCovergroup{
          .covergroup = &covergroup,
          .id = id,
          .span = span,
          .replicating_scope = &replicating});
  return id;
}

auto UnitLowerer::ExternalCovergroupOf(
    const std::string& unit_name, const support::DefPath& path)
    -> const hir::PublishedCovergroup& {
  for (const hir::ExternalCovergroup& held : unit_.external_covergroups) {
    if (held.unit_name == unit_name && held.published.path == path) {
      return held.published;
    }
  }
  const hir::UnitSignature* signature = Signatures().Find(unit_name);
  const hir::PublishedCovergroup* published =
      signature == nullptr ? nullptr : signature->FindCovergroup(path);
  if (published == nullptr) {
    throw InternalError(
        std::format(
            "UnitLowerer::ExternalCovergroupOf: '{}' published no covergroup "
            "'{}', and every covergroup a unit declares is published",
            unit_name, support::DisplayOf(path)));
  }
  hir::TypeImportMemo memo;
  hir::TypeImporter importer(signature->types, std::nullopt, unit_.types, memo);
  hir::PublishedCovergroup record = importer.Import(*published);
  // The step to MIR lays the covergroup out from what it extends, the class
  // embedding it and the classes counting its coverpoints, so reading it reads
  // each of those too. One of this unit is its own, with nothing to read.
  if (record.base.has_value()) {
    if (const auto* base =
            std::get_if<hir::ExternalCovergroupRef>(&*record.base)) {
      ExternalCovergroupOf(base->unit_name, base->path);
    }
  }
  const auto read_class = [&](const hir::ClassRef& ref) {
    if (const auto* external = std::get_if<hir::ExternalClassRef>(&ref)) {
      ExternalClassOf(external->unit_name, external->class_path);
    }
  };
  if (record.embedding.has_value()) read_class(*record.embedding);
  for (const hir::PublishedCoverpoint& point : record.coverpoints) {
    read_class(point.library_class);
  }
  unit_.external_covergroups.push_back(
      hir::ExternalCovergroup{
          .unit_name = unit_name, .published = std::move(record)});
  return unit_.external_covergroups.back().published;
}

void UnitLowerer::ReadSignaturesOfNamedCovergroups() {
  // Every covergroup another unit declares that a type of this unit names,
  // whichever way the type arrived: written here, or taken in with another
  // unit's signature. Reading one can take in more types, so the walk runs
  // until it meets none it has not read.
  for (std::size_t read = 0; read != unit_.types.size();) {
    const std::size_t reached = unit_.types.size();
    for (const hir::TypeId id : unit_.types.Ids()) {
      if (id.value < read) continue;
      const auto* handle = unit_.types.Get(id).As<hir::CovergroupHandleType>();
      if (handle == nullptr) continue;
      if (const auto* external =
              std::get_if<hir::ExternalCovergroupRef>(&handle->covergroup)) {
        ExternalCovergroupOf(external->unit_name, external->path);
      }
    }
    read = reached;
  }
}

auto UnitLowerer::PublishedCovergroupOf(
    const slang::ast::CovergroupType& covergroup, diag::SourceSpan span)
    -> diag::Result<hir::PublishedCovergroup> {
  auto id = InternLocalCovergroup(covergroup, span);
  if (!id) return std::unexpected(std::move(id.error()));
  if (const auto& held = unit_.covergroups.PublishedOf(*id)) return *held;
  const slang::ast::CovergroupType& first = *LineageOf(covergroup).front();
  const auto formal = [&](const slang::ast::FormalArgumentSymbol& declared)
      -> diag::Result<hir::PublishedCovergroupFormal> {
    auto type = InternType(declared.getType(), span);
    if (!type) return std::unexpected(std::move(type.error()));
    return hir::PublishedCovergroupFormal{
        .name = std::string{declared.name},
        .type = *type,
        .direction = ParamDirectionOf(declared)};
  };
  hir::PublishedCovergroup published{
      .path = DefPathOf(covergroup, Specialization()),
      .formals = {},
      .sample_formals = {},
      .embedding = std::nullopt,
      .coverpoints = {},
      .crosses = {},
      .base = std::nullopt};
  for (const slang::ast::FormalArgumentSymbol* declared : FormalsOf(first)) {
    auto lowered = formal(*declared);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    published.formals.push_back(*std::move(lowered));
  }
  for (const slang::ast::FormalArgumentSymbol* declared :
       SampleFormalsOf(first)) {
    auto lowered = formal(*declared);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    published.sample_formals.push_back(*std::move(lowered));
  }
  if (const slang::ast::ClassType* embedding = EmbeddingOf(covergroup)) {
    auto ref = ResolveClassRef(*embedding, span);
    if (!ref) return std::unexpected(std::move(ref.error()));
    published.embedding = *std::move(ref);
  }
  const slang::ast::CovergroupBodySymbol& body = covergroup.getBody();
  ItemNames names = ItemNamesOf(body);
  std::size_t point_index = 0;
  for (const auto& point : body.membersOfType<slang::ast::CoverpointSymbol>()) {
    const slang::ast::ClassType& counted_by =
        CoverpointClassOf(Coverage(), point);
    auto library_class = ResolveClassRef(counted_by, span);
    if (!library_class)
      return std::unexpected(std::move(library_class.error()));
    auto value_type = InternType(ValueTypeOf(counted_by), span);
    if (!value_type) return std::unexpected(std::move(value_type.error()));
    published.coverpoints.push_back(
        hir::PublishedCoverpoint{
            .name = std::move(names.coverpoints[point_index++]),
            .value_type = *value_type,
            .library_class = *std::move(library_class)});
  }
  published.crosses = std::move(names.crosses);
  if (const slang::ast::CovergroupType* base = BaseOf(covergroup)) {
    auto ref = ResolveCovergroupRef(*base, span);
    if (!ref) return std::unexpected(std::move(ref.error()));
    published.base = *std::move(ref);
  }
  unit_.covergroups.Publish(*id, published);
  return published;
}

auto UnitLowerer::TakeReplicatedCovergroups(const slang::ast::Scope& scope)
    -> std::vector<hir::CovergroupId> {
  const auto it = covergroups_by_scope_.find(&scope);
  if (it == covergroups_by_scope_.end()) return {};
  return std::move(it->second);
}

auto UnitLowerer::PopulateCovergroupsReplicatedBy(
    const slang::ast::Scope& scope) -> diag::Result<void> {
  const auto pending = pending_covergroups_.find(&scope);
  if (pending == pending_covergroups_.end()) return {};
  std::vector<PendingCovergroup> batch = std::move(pending->second);
  pending_covergroups_.erase(pending);
  for (const PendingCovergroup& covergroup : batch) {
    if (unit_.covergroups.IsDefined(covergroup.id)) continue;
    const WalkFrame frame = WalkFrame{}.WithDeclaringScope(
        DeclaringScopeChain(*covergroup.replicating_scope),
        covergroup.replicating_scope);
    auto decl = LowerCovergroupDecl(
        *this, *covergroup.covergroup, frame, covergroup.span);
    if (!decl) return std::unexpected(std::move(decl.error()));
    unit_.covergroups.Define(covergroup.id, *std::move(decl));
    auto published =
        PublishedCovergroupOf(*covergroup.covergroup, covergroup.span);
    if (!published) return std::unexpected(std::move(published.error()));
  }
  return {};
}

auto UnitLowerer::PublishedFactoryOf(
    const slang::ast::CovergroupType& covergroup, diag::SourceSpan span)
    -> diag::Result<hir::PublishedMethod> {
  const slang::ast::CovergroupType& first = *LineageOf(covergroup).front();
  hir::ExternalCalleeInterface interface{
      .kind = hir::SubroutineKind::kFunction, .params = {}};
  for (const slang::ast::FormalArgumentSymbol* formal : FormalsOf(first)) {
    auto type = InternType(formal->getType(), span);
    if (!type) return std::unexpected(std::move(type.error()));
    interface.params.push_back(
        hir::ExternalCalleeParam{
            .direction = ParamDirectionOf(*formal), .type = *type});
  }
  auto made = InternType(first, span);
  if (!made) return std::unexpected(std::move(made.error()));
  return hir::PublishedMethod{
      .prototype =
          hir::PublishedCallable{
              .name = hir::CovergroupFactoryName(
                  DefPathOf(covergroup, Specialization())),
              .interface = std::move(interface),
              .result_type = *made},
      .dispatch = BaseOf(covergroup) == nullptr
                      ? hir::MethodDispatch{hir::IntroducesVirtual{}}
                      : hir::MethodDispatch{hir::OverridesVirtual{}}};
}

auto UnitLowerer::NameCoverageLibrary(diag::SourceSpan span)
    -> diag::Result<void> {
  if (unit_.coverage_library.has_value()) return {};
  const CoverageLibrary& library = Coverage();
  const auto named = [&](const slang::ast::ClassType& cls) {
    return ResolveClassRef(cls, span);
  };
  auto registry = named(*library.registry);
  if (!registry) return std::unexpected(std::move(registry.error()));
  auto group_type = named(*library.group_type);
  if (!group_type) return std::unexpected(std::move(group_type.error()));
  auto group_instance = named(*library.group_instance);
  if (!group_instance) {
    return std::unexpected(std::move(group_instance.error()));
  }
  auto cross = named(*library.cross);
  if (!cross) return std::unexpected(std::move(cross.error()));
  auto selection = named(*library.selection);
  if (!selection) return std::unexpected(std::move(selection.error()));
  unit_.coverage_library = hir::CoverageLibraryRefs{
      .registry = *std::move(registry),
      .group_type = *std::move(group_type),
      .group_instance = *std::move(group_instance),
      .cross = *std::move(cross),
      .selection = *std::move(selection)};
  return {};
}

auto InheritedCovergroupProperty(const slang::ast::ClassPropertySymbol& prop)
    -> const slang::ast::ClassPropertySymbol* {
  const auto* covergroup =
      prop.getType().getCanonicalType().as_if<slang::ast::CovergroupType>();
  if (covergroup == nullptr || BaseOf(*covergroup) == nullptr) return nullptr;
  const auto& embedding =
      prop.getParentScope()->asSymbol().as<slang::ast::ClassType>();
  for (const slang::ast::Type* base = embedding.getBaseClass();
       base != nullptr && !base->isError();) {
    const auto& cls = base->getCanonicalType().as<slang::ast::ClassType>();
    if (const slang::ast::Symbol* found = cls.find(prop.name);
        found != nullptr &&
        found->kind == slang::ast::SymbolKind::ClassProperty) {
      const auto& held = found->as<slang::ast::ClassPropertySymbol>();
      const slang::ast::ClassPropertySymbol* further =
          InheritedCovergroupProperty(held);
      return further != nullptr ? further : &held;
    }
    base = cls.getBaseClass();
  }
  throw InternalError(
      "InheritedCovergroupProperty: a covergroup extends one no base class "
      "embeds, which the front end refuses (LRM 19.4.1)");
}

}  // namespace lyra::lowering::ast_to_hir
