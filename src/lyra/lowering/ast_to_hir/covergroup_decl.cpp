#include "lyra/lowering/ast_to_hir/covergroup_decl.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
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

#include "lyra/base/diag_message.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/frontend/slang_source_span.hpp"
#include "lyra/hir/covergroup.hpp"
#include "lyra/hir/procedural_scope.hpp"
#include "lyra/hir/subroutine_prototype.hpp"
#include "lyra/lowering/ast_to_hir/coverage_items.hpp"
#include "lyra/lowering/ast_to_hir/coverage_library.hpp"
#include "lyra/lowering/ast_to_hir/fixed_name.hpp"
#include "lyra/lowering/ast_to_hir/integral_constant.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/statement/timing.hpp"
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
            hir::ProceduralScopeKind::kCovergroup,
            std::string{CovergroupNameOf(covergroup)}) {
    decl_.declared_in = FixedNameOf(*covergroup.getParentScope());
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
          hir::CovergroupFormal{
              .var = *var, .direction = ParamDirectionOf(*formal)});
    }
    for (const slang::ast::FormalArgumentSymbol* formal :
         SampleFormalsOf(first)) {
      auto var = Variable(*formal);
      if (!var) return std::unexpected(std::move(var.error()));
      decl_.sample_formals.push_back(
          hir::CovergroupFormal{
              .var = *var, .direction = ParamDirectionOf(*formal)});
    }
    if (lineage.size() > 1) {
      auto base =
          owner_->ResolveCovergroup(*lineage[lineage.size() - 2], span_);
      if (!base) return std::unexpected(std::move(base.error()));
      // The step to MIR walks the lineage over what each covergroup of it
      // published, and no type need name the one extended, so another unit's
      // is read here.
      if (!owner_->Unit().covergroups.IsOwn(*base)) {
        owner_->CovergroupDeclarationOf(*base);
      }
      decl_.base = *base;
    }
    // One extending another is sampled on the event of the first of its
    // lineage (LRM 19.4.1), which that one starts.
    auto sampling = Sampling(first);
    if (!sampling) return std::unexpected(std::move(sampling.error()));
    decl_.sampling = *std::move(sampling);

    const slang::ast::CovergroupBodySymbol& body = covergroup_->getBody();
    auto options = Options(body.options, OptionsHolder::kCovergroup);
    if (!options) return std::unexpected(std::move(options.error()));
    decl_.options = *std::move(options);
    // An item the source named nothing is numbered after those of the
    // covergroups this one extends, as the lineage holds them.
    std::size_t unnamed_above = 0;
    for (std::size_t level = 0; level + 1 < lineage.size(); ++level) {
      unnamed_above =
          ReportNamesOf(
              owner_->ItemsOf(lineage[level]->getBody()), unnamed_above)
              .unnamed;
    }
    const CoverageItems& items = owner_->ItemsOf(body);
    const ReportNames names = ReportNamesOf(items, unnamed_above);
    for (std::size_t i = 0; i < items.coverpoints.size(); ++i) {
      auto lowered = Coverpoint(*items.coverpoints[i], names.coverpoints[i]);
      if (!lowered) return std::unexpected(std::move(lowered.error()));
      decl_.coverpoints.Add(*std::move(lowered));
    }
    for (std::size_t i = 0; i < items.crosses.size(); ++i) {
      auto lowered = Cross(*items.crosses[i], names.crosses[i]);
      if (!lowered) return std::unexpected(std::move(lowered.error()));
      decl_.crosses.Add(*std::move(lowered));
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
      auto control =
          LowerEventControl(lowerer_, frame_, ClockingEvent{event}, span_);
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

  auto Options(
      std::span<const slang::ast::CoverageOptionSetter> setters,
      OptionsHolder holder) -> diag::Result<std::vector<hir::OptionSetting>> {
    std::vector<hir::OptionSetting> options;
    for (const slang::ast::CoverageOptionSetter& setter : setters) {
      const auto& assigned =
          setter.getExpression().as<slang::ast::AssignmentExpression>();
      auto value = AddLowered(assigned.right());
      if (!value) return std::unexpected(std::move(value.error()));
      options.push_back(
          hir::OptionSetting{
              .type_option = setter.isTypeOption(),
              .member = OptionMemberOf(
                  owner_->Coverage(), holder, setter.isTypeOption(),
                  setter.getName()),
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

  // The item of the same kind and name a covergroup this one extends declares,
  // nearest it along the lineage, which this one's item replaces in the count
  // (LRM 19.4.1); nothing for an item with no name the source can write.
  template <typename Item>
  auto ReplacedItem(
      const Item& item, std::vector<const Item*> CoverageItems::* of)
      -> const Item* {
    if (item.name.empty()) return nullptr;
    for (const slang::ast::CovergroupType* base = BaseOf(*covergroup_);
         base != nullptr; base = BaseOf(*base)) {
      for (const Item* candidate : owner_->ItemsOf(base->getBody()).*of) {
        if (candidate->name == item.name) return candidate;
      }
    }
    return nullptr;
  }

  auto Coverpoint(const slang::ast::CoverpointSymbol& point, std::string name)
      -> diag::Result<hir::Coverpoint> {
    auto expression = AddLowered(point.getCoverageExpr());
    if (!expression) return std::unexpected(std::move(expression.error()));
    auto iff = Optional(point.getIffExpr());
    if (!iff) return std::unexpected(std::move(iff.error()));
    auto options = Options(point.options, OptionsHolder::kCoverpoint);
    if (!options) return std::unexpected(std::move(options.error()));
    std::vector<hir::BinConstruct> bins;
    for (const auto& bin :
         point.membersOfType<slang::ast::CoverageBinSymbol>()) {
      auto construct = Construct(bin);
      if (!construct) return std::unexpected(std::move(construct.error()));
      bins.push_back(*std::move(construct));
    }
    std::optional<hir::CoverpointRef> replaces;
    if (const auto* replaced =
            ReplacedItem(point, &CoverageItems::coverpoints)) {
      auto ref = owner_->CoverpointRefOf(*replaced, span_);
      if (!ref) return std::unexpected(std::move(ref.error()));
      replaces = *ref;
    }
    return hir::Coverpoint{
        .name = std::move(name),
        .expression = *expression,
        .iff = *iff,
        .options = *std::move(options),
        .bins = std::move(bins),
        .replaces = replaces};
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
        const diag::SourceSpan at =
            expr.syntax != nullptr
                ? frontend::SpanOf(expr.syntax->sourceRange())
                : span_;
        // The front end warns of a cross named here where it is told to
        // tolerate it, and hands the cross over (LRM 19.6.1 names a
        // coverpoint or one of its bins).
        if (target->kind == slang::ast::SymbolKind::CoverCross) {
          return diag::Fail(
              at, diag::DiagCode::kErrorInvalidFrontendSemantics,
              "'binsof' names the cross '{}', and selects only a coverpoint's "
              "bins",
              target->name);
        }
        std::optional<hir::BinConstructId> construct;
        if (const auto* bin = target->as_if<slang::ast::CoverageBinSymbol>()) {
          construct = BinConstructOf(*bin);
          target = &target->getParentScope()->asSymbol();
        }
        const auto& selected = target->as<slang::ast::CoverpointSymbol>();
        // A cross's bins are sets of its tuples, which no bin of a coverpoint
        // it does not cross names (LRM 19.6.1).
        if (!std::ranges::contains(cross.targets, &selected)) {
          return std::unexpected(
              diag::Make(
                  at, diag::DiagCode::kErrorBinsofOutsideItsCross,
                  base::DiagMessage(
                      "'binsof' selects the bins of '{}', which the cross "
                      "'{}' does not cross",
                      selected.name, cross.name))
                  .WithNote(
                      "a cross bin selects tuples of the cross, so 'binsof' "
                      "names one of the coverpoints it crosses"));
        }
        auto point = owner_->CoverpointRefOf(selected, at);
        if (!point) return std::unexpected(std::move(point.error()));
        auto intersect = Elements(condition.intersects);
        if (!intersect) return std::unexpected(std::move(intersect.error()));
        return add(
            hir::SelectBinsOf{
                .point = *point,
                .construct = construct,
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
        .bins = {},
        .replaces = std::nullopt};
    for (const slang::ast::CoverpointSymbol* target : cross.targets) {
      auto point = owner_->CoverpointRefOf(*target, span_);
      if (!point) return std::unexpected(std::move(point.error()));
      lowered.targets.push_back(*point);
    }
    if (const auto* replaced = ReplacedItem(cross, &CoverageItems::crosses)) {
      auto ref = owner_->CrossRefOf(*replaced, span_);
      if (!ref) return std::unexpected(std::move(ref.error()));
      lowered.replaces = *ref;
    }
    auto iff = Optional(cross.getIffExpr());
    if (!iff) return std::unexpected(std::move(iff.error()));
    lowered.iff = *iff;
    auto options = Options(cross.options, OptionsHolder::kCross);
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

auto UnitLowerer::ResolveCovergroup(
    const slang::ast::CovergroupType& covergroup, diag::SourceSpan span)
    -> diag::Result<hir::CovergroupId> {
  if (const auto it = covergroup_cache_.find(&covergroup);
      it != covergroup_cache_.end()) {
    return it->second;
  }
  if (!Owns(covergroup)) {
    if (auto named = NameCoverageLibrary(span); !named) {
      return std::unexpected(std::move(named.error()));
    }
    const hir::CovergroupId id = unit_.covergroups.Import(
        hir::CovergroupKey{
            .unit_name = CompilationUnitName(UnitHomeOf(covergroup), Names()),
            .path = DefPathOf(covergroup, Names())});
    covergroup_cache_.emplace(&covergroup, id);
    return id;
  }
  return InternOwnCovergroup(covergroup, span);
}

auto UnitLowerer::InternOwnCovergroup(
    const slang::ast::CovergroupType& covergroup, diag::SourceSpan span)
    -> diag::Result<hir::CovergroupId> {
  if (const auto it = covergroup_cache_.find(&covergroup);
      it != covergroup_cache_.end()) {
    return it->second;
  }
  if (auto named = NameCoverageLibrary(span); !named) {
    return std::unexpected(std::move(named.error()));
  }
  // A covergroup is one declaration of its unit wherever its text was
  // elaborated, as a class is: a copy at a path already declared is that
  // covergroup.
  const hir::CovergroupId id =
      unit_.covergroups.DeclareOwn(DefPathOf(covergroup, Names()));
  covergroup_cache_.emplace(&covergroup, id);
  // Its expressions reach what encloses it as a body of the scope replicating
  // it does, so it lowers with that scope (LRM 6.22, 19.3).
  const slang::ast::Scope& replicating = ReplicatingScope(covergroup);
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

auto UnitLowerer::CovergroupDeclarationOf(hir::CovergroupId covergroup)
    -> const hir::PublishedCovergroup& {
  if (unit_.covergroups.IsPublished(covergroup)) {
    return unit_.covergroups.Published(covergroup);
  }
  const hir::CovergroupKey& key = unit_.covergroups.KeyOf(covergroup);
  const hir::UnitSignature* signature = Signatures().Find(key.unit_name);
  const hir::PublishedCovergroup* published =
      signature == nullptr ? nullptr : signature->FindCovergroup(key.path);
  if (published == nullptr) {
    throw InternalError(
        std::format(
            "UnitLowerer::CovergroupDeclarationOf: '{}' published no "
            "covergroup "
            "'{}', and every covergroup a unit declares is published",
            key.unit_name, support::DisplayOf(key.path)));
  }
  hir::TypeImporter importer = ReadingImporter(*signature);
  hir::PublishedCovergroup record = importer.Import(*published);
  // The step to MIR lays the covergroup out from what it extends, the class
  // embedding it and the classes counting its coverpoints, so reading it reads
  // each of those too. One of this unit is its own, with nothing to read,
  // including one a lineage another unit published passes back through.
  if (record.base.has_value() && !unit_.covergroups.IsOwn(*record.base)) {
    CovergroupDeclarationOf(*record.base);
  }
  const auto read_class = [&](hir::ClassId cls) {
    if (!unit_.classes.IsOwn(cls)) {
      DeclarationOf(cls);
    }
  };
  if (record.embedding.has_value()) read_class(*record.embedding);
  for (const hir::PublishedCoverpoint& point : record.coverpoints) {
    read_class(point.library_class);
  }
  unit_.covergroups.Publish(covergroup, std::move(record));
  return unit_.covergroups.Published(covergroup);
}

auto UnitLowerer::ReadSignaturesOfNamedCovergroups() -> diag::Result<void> {
  // Every covergroup another unit declares that this unit names, whichever way
  // the name arrived: a type written here or taken in with another unit's
  // signature, or a call of a method of its type, which evaluates no instance
  // of it and so states no type (LRM 19.8). Reading one can take in more, so
  // the walk runs until it meets none it has not read. A unit naming one is
  // laid out over the coverage library as the declaring unit is.
  for (std::size_t read = 0; read != unit_.covergroups.size();) {
    const std::size_t reached = unit_.covergroups.size();
    for (const hir::CovergroupId id : unit_.covergroups.Ids()) {
      if (id.value < read || unit_.covergroups.IsOwn(id)) continue;
      if (auto named = NameCoverageLibrary(diag::SourceSpan{}); !named) {
        return std::unexpected(std::move(named.error()));
      }
      CovergroupDeclarationOf(id);
    }
    read = reached;
  }
  return {};
}

auto UnitLowerer::PublishedCovergroupOf(
    const slang::ast::CovergroupType& covergroup, diag::SourceSpan span)
    -> diag::Result<hir::PublishedCovergroup> {
  auto id = InternOwnCovergroup(covergroup, span);
  if (!id) return std::unexpected(std::move(id.error()));
  if (unit_.covergroups.IsPublished(*id)) {
    return unit_.covergroups.Published(*id);
  }
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
      .path = DefPathOf(covergroup, Names()),
      .formals = {},
      .sample_formals = {},
      .embedding = std::nullopt,
      .coverpoints = {},
      .cross_count = 0,
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
    auto embedding_id = ResolveClass(*embedding, span);
    if (!embedding_id) return std::unexpected(std::move(embedding_id.error()));
    published.embedding = *embedding_id;
  }
  const CoverageItems& items = ItemsOf(covergroup.getBody());
  for (const slang::ast::CoverpointSymbol* point : items.coverpoints) {
    const slang::ast::ClassType& counted_by =
        CoverpointClassOf(Coverage(), *point);
    auto library_class = ResolveClass(counted_by, span);
    if (!library_class)
      return std::unexpected(std::move(library_class.error()));
    // The coverpoint is an object of this class, made and sampled through its
    // methods, so the class is read.
    ReadDeclarationLater(*library_class);
    NameCoverpointClass(counted_by, *library_class);
    auto value_type = InternType(ValueTypeOf(counted_by), span);
    if (!value_type) return std::unexpected(std::move(value_type.error()));
    published.coverpoints.push_back(
        hir::PublishedCoverpoint{
            .value_type = *value_type, .library_class = *library_class});
  }
  published.cross_count = static_cast<std::uint32_t>(items.crosses.size());
  if (const slang::ast::CovergroupType* base = BaseOf(covergroup)) {
    auto base_id = ResolveCovergroup(*base, span);
    if (!base_id) return std::unexpected(std::move(base_id.error()));
    published.base = *base_id;
  }
  unit_.covergroups.Publish(*id, published);
  return published;
}

auto UnitLowerer::PopulateCovergroupsReplicatedBy(
    const slang::ast::Scope& scope)
    -> diag::Result<std::vector<hir::ReplicatedCovergroup>> {
  const auto pending = pending_covergroups_.find(&scope);
  if (pending == pending_covergroups_.end()) return {};
  std::vector<PendingCovergroup> batch = std::move(pending->second);
  pending_covergroups_.erase(pending);
  std::vector<hir::ReplicatedCovergroup> replicated;
  replicated.reserve(batch.size());
  for (const PendingCovergroup& covergroup : batch) {
    const WalkFrame frame = WalkFrame{}.WithDeclaringScope(
        DeclaringScopeChain(*covergroup.replicating_scope),
        covergroup.replicating_scope);
    auto decl = LowerCovergroupDecl(
        *this, *covergroup.covergroup, frame, covergroup.span);
    if (!decl) return std::unexpected(std::move(decl.error()));
    replicated.push_back(
        hir::ReplicatedCovergroup{
            .id = covergroup.id, .code = *std::move(decl)});
    if (unit_.covergroups.IsPublished(covergroup.id)) continue;
    auto published =
        PublishedCovergroupOf(*covergroup.covergroup, covergroup.span);
    if (!published) return std::unexpected(std::move(published.error()));
  }
  return replicated;
}

auto UnitLowerer::PublishedFactoryOf(
    const slang::ast::CovergroupType& covergroup, diag::SourceSpan span)
    -> diag::Result<hir::DeclaredMethod> {
  const slang::ast::CovergroupType& first = *LineageOf(covergroup).front();
  hir::CalleeInterface interface{
      .kind = hir::SubroutineKind::kFunction, .params = {}};
  for (const slang::ast::FormalArgumentSymbol* formal : FormalsOf(first)) {
    auto type = InternType(formal->getType(), span);
    if (!type) return std::unexpected(std::move(type.error()));
    interface.params.push_back(
        hir::CalleeParam{
            .direction = ParamDirectionOf(*formal), .type = *type});
  }
  auto made = InternType(first, span);
  if (!made) return std::unexpected(std::move(made.error()));
  auto own = InternOwnCovergroup(covergroup, span);
  if (!own) return std::unexpected(std::move(own.error()));
  // An extension's factory overrides the one the class embedding the first
  // covergroup of the lineage introduced, the method making that covergroup.
  hir::MethodDispatch dispatch = hir::IntroducesVirtual{};
  if (BaseOf(covergroup) != nullptr) {
    const slang::ast::ClassType& embedding = *EmbeddingOf(first);
    auto introducer = ResolveClass(embedding, span);
    if (!introducer) return std::unexpected(std::move(introducer.error()));
    dispatch = hir::OverridesVirtual{
        .is_pure = false,
        .overridden = hir::ClassMethodTarget{
            .owner = *introducer,
            .method = MethodsOf(embedding).PositionOf(first)}};
  }
  std::vector<std::optional<hir::MethodId>> defaults(interface.params.size());
  return hir::DeclaredMethod{
      .prototype =
          hir::SubroutinePrototype{
              .name =
                  hir::CovergroupFactoryName(DefPathOf(covergroup, Names())),
              .interface = std::move(interface),
              .result_type = *made},
      .defaults = std::move(defaults),
      .dispatch = dispatch,
      .replaces = std::nullopt,
      .makes = *own};
}

auto UnitLowerer::NameCoverageLibrary(diag::SourceSpan span)
    -> diag::Result<void> {
  if (names_coverage_library_) return {};
  const CoverageLibrary& library = Coverage();
  // A covergroup is lowered into calls of these classes' methods and objects
  // of them, so each is read, not only named.
  for (const slang::ast::ClassType* cls :
       {library.registry, library.group_type, library.group_instance,
        library.item, library.coverpoint, library.cross, library.selection}) {
    auto id = ResolveClass(*cls, span);
    if (!id) return std::unexpected(std::move(id.error()));
    ReadDeclarationLater(*id);
  }
  names_coverage_library_ = true;
  return {};
}

void UnitLowerer::NameCoverpointClass(
    const slang::ast::ClassType& counting, hir::ClassId id) {
  if (std::ranges::contains(
          coverpoint_classes_, &counting,
          [](const auto& named) { return named.first; })) {
    return;
  }
  coverpoint_classes_.emplace_back(&counting, id);
}

void UnitLowerer::ResolveCoverageLibrary() {
  if (!names_coverage_library_) return;
  const CoverageLibrary& library = Coverage();
  const diag::SourceSpan span{};
  // The library is Lyra's own text, so a member it lacks or a class it cannot
  // name is a defect of Lyra's.
  const auto member = [](const slang::ast::ClassType& cls,
                         std::string_view name) -> LibraryMember {
    const std::optional<LibraryMember> found = FindLibraryMember(cls, name);
    if (!found.has_value()) {
      throw InternalError(
          std::format(
              "Lyra's coverage library declares no member '{}' of the class "
              "'{}'",
              name, cls.name));
    }
    return *found;
  };
  const auto required = [&](auto resolved, std::string_view name) {
    if (!resolved) {
      throw InternalError(
          std::format(
              "the coverage library's member '{}' names nothing this unit can "
              "name -- please report this as a bug",
              name));
    }
    return *resolved;
  };
  const auto cls = [&](const slang::ast::ClassType& library_class) {
    return required(ResolveClass(library_class, span), library_class.name);
  };
  const auto method = [&](const slang::ast::ClassType& library_class,
                          std::string_view name) {
    const LibraryMember found = member(library_class, name);
    return required(
        MakeClassMethodTarget(
            *found.owner, found.member->as<slang::ast::SubroutineSymbol>(),
            span),
        name);
  };
  const auto property = [&](const slang::ast::ClassType& library_class,
                            std::string_view name) {
    const LibraryMember found = member(library_class, name);
    return required(
        MakeClassPropertyTarget(
            *found.owner, found.member->as<slang::ast::ClassPropertySymbol>(),
            span),
        name);
  };
  const auto enumerator = [&](std::string_view enumeration,
                              std::string_view value) {
    const slang::ast::EnumValueSymbol& declared =
        LibraryEnumeratorOf(library, enumeration, value);
    return hir::LibraryEnumerator{
        .type = required(InternType(declared.getType(), span), value),
        .value = LowerSVIntToIntegralConstant(declared.getValue().integer())};
  };
  hir::CoverageLibraryMembers members{
      .registry =
          {
              .cls = cls(*library.registry),
              .type_named = method(*library.registry, "type_named"),
              .claim_database_writing =
                  method(*library.registry, "claim_database_writing"),
              .write_database = method(*library.registry, "write_database"),
              .get_coverage = method(*library.registry, "get_coverage"),
              .set_coverage_db_name =
                  method(*library.registry, "set_coverage_db_name"),
              .load_coverage_db = method(*library.registry, "load_coverage_db"),
          },
      .group_type =
          {
              .cls = cls(*library.group_type),
              .make_instance = method(*library.group_type, "make_instance"),
              .declare_coverpoint =
                  method(*library.group_type, "declare_coverpoint"),
              .declare_cross = method(*library.group_type, "declare_cross"),
              .declare_replaced =
                  method(*library.group_type, "declare_replaced"),
              .get_coverage = method(*library.group_type, "get_coverage"),
              .coverage = method(*library.group_type, "coverage"),
              .get_item_coverage =
                  method(*library.group_type, "get_item_coverage"),
              .item_coverage = method(*library.group_type, "item_coverage"),
              .declared = property(*library.group_type, "declared"),
              .type_option = property(*library.group_type, "type_option"),
              .coverpoint_type_options =
                  property(*library.group_type, "coverpoint_type_options"),
              .cross_type_options =
                  property(*library.group_type, "cross_type_options"),
          },
      .group_instance =
          {
              .cls = cls(*library.group_instance),
              .strobed = method(*library.group_instance, "strobed"),
              .claim_strobe = method(*library.group_instance, "claim_strobe"),
              .strobe_taken = method(*library.group_instance, "strobe_taken"),
              .get_inst_coverage =
                  method(*library.group_instance, "get_inst_coverage"),
              .inst_coverage = method(*library.group_instance, "inst_coverage"),
              .set_inst_name = method(*library.group_instance, "set_inst_name"),
              .start = method(*library.group_instance, "start"),
              .stop = method(*library.group_instance, "stop"),
              .option = property(*library.group_instance, "option"),
              .block_event = property(*library.group_instance, "block_event"),
              .collecting = property(*library.group_instance, "collecting"),
          },
      .item =
          {
              .cls = cls(*library.item),
              .get_inst_coverage = method(*library.item, "get_inst_coverage"),
              .inst_coverage = method(*library.item, "inst_coverage"),
              .start = method(*library.item, "start"),
              .stop = method(*library.item, "stop"),
          },
      .coverpoint =
          {
              .cls = cls(*library.coverpoint),
              .all_numbers = method(*library.coverpoint, "all_numbers"),
              .numbers_of_construct =
                  method(*library.coverpoint, "numbers_of_construct"),
              .skip = method(*library.coverpoint, "skip"),
              .option = property(*library.coverpoint, "option"),
          },
      .cross =
          {
              .cls = cls(*library.cross),
              .cross_coverpoint = method(*library.cross, "cross_coverpoint"),
              .add_bin = method(*library.cross, "add_bin"),
              .finish = method(*library.cross, "finish"),
              .sample = method(*library.cross, "sample"),
              .gate = method(*library.cross, "gate"),
              .select_all = method(*library.cross, "select_all"),
              .select_numbers = method(*library.cross, "select_numbers"),
              .select_outside = method(*library.cross, "select_outside"),
              .both_numbers = method(*library.cross, "both_numbers"),
              .begin_filter = method(*library.cross, "begin_filter"),
              .candidate_tuples = method(*library.cross, "candidate_tuples"),
              .offer = method(*library.cross, "offer"),
              .component = method(*library.cross, "component"),
              .end_filter = method(*library.cross, "end_filter"),
              .value_component = method(*library.cross, "value_component"),
              .end_value_tuple = method(*library.cross, "end_value_tuple"),
              .option = property(*library.cross, "option"),
          },
      .selection =
          {
              .cls = cls(*library.selection),
              .both = method(*library.selection, "both"),
              .either = method(*library.selection, "either"),
          },
      .enumerators =
          {.bins = enumerator("coverage_bin_kind", "COVERAGE_BINS"),
           .ignore_bins =
               enumerator("coverage_bin_kind", "COVERAGE_IGNORE_BINS"),
           .illegal_bins =
               enumerator("coverage_bin_kind", "COVERAGE_ILLEGAL_BINS"),
           .one_bin = enumerator("coverage_bin_array", "COVERAGE_ONE_BIN"),
           .bin_per_value =
               enumerator("coverage_bin_array", "COVERAGE_BIN_PER_VALUE"),
           .fixed_bins =
               enumerator("coverage_bin_array", "COVERAGE_FIXED_BINS"),
           .once = enumerator("coverage_repetition", "COVERAGE_ONCE"),
           .consecutive =
               enumerator("coverage_repetition", "COVERAGE_CONSECUTIVE"),
           .go_to = enumerator("coverage_repetition", "COVERAGE_GOTO"),
           .nonconsecutive =
               enumerator("coverage_repetition", "COVERAGE_NONCONSECUTIVE")},
      .coverpoint_classes = {}};
  for (const auto& [counting, id] : coverpoint_classes_) {
    const auto of = [&](std::string_view name) {
      return method(*counting, name);
    };
    std::optional<hir::IntegralCoverpointClassMembers> integral;
    // The real coverpoint has none of what only an integral one takes.
    if (counting != library.real_coverpoint) {
      integral = hir::IntegralCoverpointClassMembers{
          .add_wildcard = of("add_wildcard"),
          .add_default_sequence_bin = of("add_default_sequence_bin"),
          .begin_transition_bins = of("begin_transition_bins"),
          .begin_sequence = of("begin_sequence"),
          .begin_step = of("begin_step"),
          .end_step = of("end_step"),
          .end_sequence = of("end_sequence"),
          .end_transition_bins = of("end_transition_bins"),
          .pending_values = of("pending_values"),
          .keep_value = of("keep_value"),
          .name_enumeration_value = of("name_enumeration_value"),
          .values_in_bin = of("values_in_bin"),
      };
    }
    members.coverpoint_classes.emplace(
        id, hir::CoverpointClassMembers{
                .add_value = of("add_value"),
                .add_every_value = of("add_every_value"),
                .add_range = of("add_range"),
                .add_range_from_least = of("add_range_from_least"),
                .add_range_to_greatest = of("add_range_to_greatest"),
                .add_tolerance = of("add_tolerance"),
                .add_default_bin = of("add_default_bin"),
                .begin_bins = of("begin_bins"),
                .end_bins = of("end_bins"),
                .finish = of("finish"),
                .sample = of("sample"),
                .gate = of("gate"),
                .begin_selection = of("begin_selection"),
                .selected_bins = of("selected_bins"),
                .numbers_containing = of("numbers_containing"),
                .text_of = of("text_of"),
                .integral = std::move(integral)});
  }
  unit_.coverage_library = std::move(members);
}

auto UnitLowerer::ItemsOf(const slang::ast::CovergroupBodySymbol& body)
    -> const CoverageItems& {
  const auto [it, entered] = coverage_items_.try_emplace(&body);
  if (entered) it->second = CoverageItemsOf(body);
  return it->second;
}

auto UnitLowerer::CoverpointRefOf(
    const slang::ast::CoverpointSymbol& point, diag::SourceSpan span)
    -> diag::Result<hir::CoverpointRef> {
  const slang::ast::CovergroupType& declaring = CovergroupDeclaring(point);
  auto covergroup = ResolveCovergroup(declaring, span);
  if (!covergroup) return std::unexpected(std::move(covergroup.error()));
  return hir::CoverpointRef{
      .covergroup = *covergroup,
      .coverpoint = ItemsOf(declaring.getBody()).PositionOf(point)};
}

auto UnitLowerer::CrossRefOf(
    const slang::ast::CoverCrossSymbol& cross, diag::SourceSpan span)
    -> diag::Result<hir::CrossRef> {
  const slang::ast::CovergroupType& declaring = CovergroupDeclaring(cross);
  auto covergroup = ResolveCovergroup(declaring, span);
  if (!covergroup) return std::unexpected(std::move(covergroup.error()));
  return hir::CrossRef{
      .covergroup = *covergroup,
      .cross = ItemsOf(declaring.getBody()).PositionOf(cross)};
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
