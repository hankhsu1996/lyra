#include "lyra/lowering/ast_to_hir/statement/assertions.hpp"

#include <expected>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include <slang/ast/SemanticFacts.h>
#include <slang/ast/Statement.h>
#include <slang/ast/TimingControl.h>
#include <slang/ast/expressions/AssertionExpr.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/statements/MiscStatements.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/MemberSymbols.h>
#include <slang/syntax/SyntaxNode.h>

#include "lyra/base/diag_message.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/frontend/slang_source_span.hpp"
#include "lyra/hir/expr_builders.hpp"
#include "lyra/lowering/ast_to_hir/inferred_clocking.hpp"
#include "lyra/lowering/ast_to_hir/reads.hpp"
#include "lyra/lowering/ast_to_hir/sampled_reads.hpp"
#include "lyra/lowering/ast_to_hir/statement/timing.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// One arm of an action block (LRM 16.3). An absent arm is what the source
// wrote, not a failure to lower one.
auto LowerActionArm(
    ProcessLowerer& proc, WalkFrame frame, const slang::ast::Statement* action)
    -> diag::Result<std::optional<hir::Stmt>> {
  if (action == nullptr) {
    return std::optional<hir::Stmt>{};
  }
  auto stmt_or = proc.LowerStmt(*action, frame);
  if (!stmt_or) return std::unexpected(std::move(stmt_or.error()));
  return std::optional<hir::Stmt>(*std::move(stmt_or));
}

auto AddActionArm(hir::ProceduralBody& body, std::optional<hir::Stmt> arm)
    -> std::optional<hir::StmtId> {
  if (!arm.has_value()) {
    return std::nullopt;
  }
  return body.stmts.Add(*std::move(arm));
}

auto AssertStmtOf(
    hir::AssertionDirective directive, hir::AssertionTiming timing,
    hir::ExprId condition, std::optional<hir::StmtId> pass_stmt,
    std::optional<hir::StmtId> fail_stmt, diag::SourceSpan span) -> hir::Stmt {
  return hir::Stmt{
      .label = std::nullopt,
      .data =
          hir::AssertStmt{
              .directive = directive,
              .timing = timing,
              .condition = condition,
              .pass_stmt = pass_stmt,
              .fail_stmt = fail_stmt},
      .span = span};
}

// LRM 16.4: `#0` defers the action to the Observed region and matures it in
// Reactive, `final` defers it to Postponed; an assertion with no qualifier runs
// its action inline.
auto TimingOf(const slang::ast::ImmediateAssertionStatement& as)
    -> hir::AssertionTiming {
  if (!as.isDeferred) return hir::AssertionTiming::kSimple;
  return as.isFinal ? hir::AssertionTiming::kFinal
                    : hir::AssertionTiming::kObserved;
}

// Where a sequence or property node was written, so a refusal underlines the
// operator rather than the whole assertion -- including inside the body the
// front end expanded a named instance into.
//
// This is the one node family the front end gives no source range of its own;
// a statement, an expression, a pattern and a timing control each carry one. It
// offers the syntax the node was built from instead, and reports its own
// diagnostics about these nodes through it, so that is the channel a location
// comes from here. A node built from no syntax has none, and the nearest
// enclosing span answers for it.
auto NodeSpan(const slang::ast::AssertionExpr& expr, diag::SourceSpan enclosing)
    -> diag::SourceSpan {
  if (expr.syntax == nullptr) {
    return enclosing;
  }
  return frontend::SpanOf(expr.syntax->sourceRange());
}

auto RefuseAssertionForm(diag::SourceSpan span, base::DiagFormat<> what)
    -> std::unexpected<diag::Diagnostic> {
  return std::unexpected(
      diag::Make(
          span, diag::DiagCode::kUnsupportedStatementForm,
          "{} is not yet supported in a concurrent assertion",
          base::DiagMessage(what).Text())
          .WithNote("pass '--assertions skip' to leave the assertion out"));
}

// LRM 16.7's `$` is a window with no end, which an automaton spelling a window
// as one position per tick has no shape for. Nothing here carries that yet, so
// it is refused where the bound is read.
auto TickRangeOf(const slang::ast::SequenceRange& range, diag::SourceSpan span)
    -> diag::Result<hir::TickRange> {
  if (!range.max.has_value()) {
    return RefuseAssertionForm(span, "a '$' bound on a delay or repetition");
  }
  return hir::TickRange{.min = range.min, .max = *range.max};
}

// The named sequence or property instance a node stands at (LRM 16.8, 16.12),
// and nothing where it stands at anything else. Only a `Simple` node stands at
// one, so a caller holding an answer may read that form off the node.
auto InstanceOf(const slang::ast::AssertionExpr& expr)
    -> const slang::ast::AssertionInstanceExpression* {
  if (expr.kind != slang::ast::AssertionExprKind::Simple) {
    return nullptr;
  }
  const auto& simple = expr.as<slang::ast::SimpleAssertionExpr>();
  if (simple.expr.kind != slang::ast::ExpressionKind::AssertionInstance) {
    return nullptr;
  }
  return &simple.expr.as<slang::ast::AssertionInstanceExpression>();
}

// The hierarchical name an instance reached its declaration by, or nothing
// where it named one of its own scope.
auto ReachedThrough(const slang::ast::AssertionInstanceExpression& instance)
    -> const slang::ast::HierarchicalReference* {
  return instance.hierRef.target != nullptr ? &instance.hierRef : nullptr;
}

// The body an instance stands for, which the front end has already expanded
// with the actual arguments substituted for the formals, so nothing here
// re-performs the substitution. Absent where a repetition is written on the
// instance: that repetition belongs to the sequence standing here, so what
// stands here is more than the body.
auto InstanceBodyOf(const slang::ast::AssertionExpr& expr)
    -> const slang::ast::AssertionExpr* {
  const auto* instance = InstanceOf(expr);
  if (instance == nullptr ||
      expr.as<slang::ast::SimpleAssertionExpr>().repetition.has_value()) {
    return nullptr;
  }
  return &instance->body;
}

// Whether an instance materializes local variables (LRM 16.10). One that does
// is what makes two paths reaching the same point in it distinguishable, which
// is the premise an evaluation's position set rests on.
auto CarriesLocalVars(const slang::ast::AssertionExpr& expr) -> bool {
  const auto* instance = InstanceOf(expr);
  return instance != nullptr && !instance->localVars.empty();
}

// The clocking event of the clocking block an instance's declaration stands
// in, which clocks the sequence or property it declares (LRM 16.16 b), as
// `frame` reaches it; nothing for any other node. The declaration is reached by
// a hierarchical name, by a name a body reached that way wrote, or plainly; a
// block such a name finds outside the reader's own instance clocks by what the
// instance declaring it publishes.
auto DeclaredClockOf(
    UnitLowerer& owner, const slang::ast::AssertionExpr& expr,
    const WalkFrame& frame) -> std::optional<ClockingEvent> {
  const auto* instance = InstanceOf(expr);
  if (instance == nullptr) {
    return std::nullopt;
  }
  const slang::ast::Scope* scope = instance->symbol.getParentScope();
  if (scope == nullptr ||
      scope->asSymbol().kind != slang::ast::SymbolKind::ClockingBlock) {
    return std::nullopt;
  }
  const auto& block = scope->asSymbol().as<slang::ast::ClockingBlockSymbol>();
  ClockOfAnotherInstance reach{
      .block = &block,
      .declaration = &instance->symbol,
      .reached_through = ReachedThrough(*instance)};
  if (reach.reached_through == nullptr) {
    if (const ExpansionInstance* through =
            ExpansionDeclaringBeside(frame, instance->symbol)) {
      reach.declaration = through->declaration;
      reach.reached_through = through->reached_through;
    }
  }
  if (reach.reached_through != nullptr &&
      !NamesOwnDeclaration(
          owner.StartOf(frame, *reach.declaration, *reach.reached_through))) {
    return ClockingEvent{reach};
  }
  return ClockingEvent{&block.getEvent()};
}

// Whether a clocking event met inside an assertion's body is another clock
// than the one in force there, which makes the assertion multiclocked (LRM
// 16.13). One identical to it changes nothing (LRM 16.13.3).
auto BeginsAnotherClock(
    const ClockingEvent& event, const WalkFrame& frame, diag::SourceSpan span)
    -> diag::Result<bool> {
  if (!frame.assertion_clock.has_value()) {
    throw InternalError(
        "BeginsAnotherClock: an assertion's body lowered with no clock");
  }
  auto same = IsSameClock(frame, *frame.assertion_clock, event, span);
  if (!same) return std::unexpected(std::move(same.error()));
  return !*same;
}

// An instance standing inside an assertion's body: the body it stands for, and
// the frame that body is lowered in, which records where the instance stood.
struct EnteredInstance {
  const slang::ast::AssertionExpr* body;
  WalkFrame frame;
};

// Nothing where no instance's body stands in place of `expr`. An instance whose
// declaration is clocked otherwise than the assertion is refused rather than
// read under the wrong clock.
auto EnterNestedInstance(
    UnitLowerer& owner, const slang::ast::AssertionExpr& expr,
    const WalkFrame& frame, diag::SourceSpan span)
    -> diag::Result<std::optional<EnteredInstance>> {
  const slang::ast::AssertionExpr* body = InstanceBodyOf(expr);
  if (body == nullptr) {
    return std::optional<EnteredInstance>{};
  }
  if (const std::optional<ClockingEvent> declared =
          DeclaredClockOf(owner, expr, frame)) {
    auto another = BeginsAnotherClock(*declared, frame, span);
    if (!another) return std::unexpected(std::move(another.error()));
    if (*another) {
      return RefuseAssertionForm(
          span,
          "a sequence or property clocked by a clocking block other than "
          "the assertion's clock");
    }
  }
  return std::optional<EnteredInstance>{EnteredInstance{
      .body = body,
      .frame = frame.WithExpansionInstance(
          InstanceOf(expr)->symbol, frame.assertion_clock,
          ReachedThrough(*InstanceOf(expr)))}};
}

auto AddSequence(const WalkFrame& frame, hir::SequenceExpr expr)
    -> hir::SequenceExprId {
  return frame.SequenceExprs().Add(std::move(expr));
}

auto AddProperty(const WalkFrame& frame, hir::PropertyExpr expr)
    -> hir::PropertyExprId {
  return frame.PropertyExprs().Add(std::move(expr));
}

// LRM 16.9.2: a repetition written on a sequence element. Only the consecutive
// form is carried, and only where every match spans at least one tick: an
// empty match spans none, and concatenating or implying from one follows rules
// of its own (LRM 16.9.2.1, 16.12.7) that the automaton does not yet apply.
auto ApplyRepetition(
    const WalkFrame& frame, hir::SequenceExpr body,
    const std::optional<slang::ast::SequenceRepetition>& repetition,
    diag::SourceSpan span) -> diag::Result<hir::SequenceExpr> {
  if (!repetition.has_value()) {
    return body;
  }
  if (repetition->kind != slang::ast::SequenceRepetition::Consecutive) {
    return RefuseAssertionForm(
        span, "a nonconsecutive or goto repetition ('[=]', '[->]')");
  }
  if (repetition->range.min == 0) {
    return RefuseAssertionForm(span, "a repetition that admits an empty match");
  }
  auto count_or = TickRangeOf(repetition->range, span);
  if (!count_or) return std::unexpected(std::move(count_or.error()));
  return hir::SequenceExpr{
      .data =
          hir::SequenceRepetition{
              .body = AddSequence(frame, std::move(body)), .count = *count_or},
      .span = span};
}

auto LowerSequenceExpr(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::AssertionExpr& expr, diag::SourceSpan enclosing)
    -> diag::Result<hir::SequenceExpr>;

// LRM 16.5.1: an expression an assertion reads is evaluated over the sampled
// value of each variable it names, so what it reads is armed where the
// assertion is read, as a sampled value function arms what its operand reads.
auto LowerSampledExpr(
    ProcessLowerer& proc, const WalkFrame& frame,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto armed = ArmSampledReads(proc.Owner(), frame, expr, span);
  if (!armed) return std::unexpected(std::move(armed.error()));
  return proc.LowerExpr(expr, frame);
}

// The head of a concatenation whose source wrote no left operand. LRM 16.7
// reads `##n s` as `` `true ##n s``, so the delay measures from a tick that
// matches unconditionally rather than from nothing.
auto BuildUnconditionalTick(
    ProcessLowerer& proc, const WalkFrame& frame, diag::SourceSpan span)
    -> hir::SequenceExprId {
  const hir::ExprId one = frame.Exprs().Add(
      hir::MakeIntLiteral(1, proc.Owner().Unit().builtins.int_type, span));
  return frame.SequenceExprs().Add(
      hir::SequenceExpr{
          .data = hir::SequenceBoolean{.condition = one}, .span = span});
}

auto LowerSequenceConcat(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::SequenceConcatExpr& concat, diag::SourceSpan span)
    -> diag::Result<hir::SequenceExpr> {
  if (concat.elements.empty()) {
    throw InternalError(
        "LowerSequenceConcat: a sequence concatenation with no elements "
        "matches nothing the grammar admits (LRM 16.7)");
  }
  const auto& first = concat.elements.front();
  auto head_or = LowerSequenceExpr(proc, frame, *first.sequence, span);
  if (!head_or) return std::unexpected(std::move(head_or.error()));
  hir::SequenceExpr result = *std::move(head_or);
  // A leading delay measures from the unconditional tick above; a zero-length
  // one is the same sequence written either way, so it needs no case.
  if (first.delay.min != 0 || first.delay.max != 0) {
    auto delay_or = TickRangeOf(first.delay, span);
    if (!delay_or) return std::unexpected(std::move(delay_or.error()));
    result = hir::SequenceExpr{
        .data =
            hir::SequenceDelay{
                .head = BuildUnconditionalTick(proc, frame, span),
                .tail = AddSequence(frame, std::move(result)),
                .delay = *delay_or},
        .span = span};
  }

  for (const auto& element : concat.elements.subspan(1)) {
    auto tail_or = LowerSequenceExpr(proc, frame, *element.sequence, span);
    if (!tail_or) return std::unexpected(std::move(tail_or.error()));
    auto delay_or = TickRangeOf(element.delay, span);
    if (!delay_or) return std::unexpected(std::move(delay_or.error()));
    result = hir::SequenceExpr{
        .data =
            hir::SequenceDelay{
                .head = AddSequence(frame, std::move(result)),
                .tail = AddSequence(frame, *std::move(tail_or)),
                .delay = *delay_or},
        .span = span};
  }
  return result;
}

auto LowerSequenceExpr(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::AssertionExpr& expr, diag::SourceSpan enclosing)
    -> diag::Result<hir::SequenceExpr> {
  const diag::SourceSpan span = NodeSpan(expr, enclosing);
  // LRM 16.10.
  if (CarriesLocalVars(expr)) {
    return RefuseAssertionForm(span, "a local variable");
  }
  auto entered_or = EnterNestedInstance(proc.Owner(), expr, frame, span);
  if (!entered_or) return std::unexpected(std::move(entered_or.error()));
  if (entered_or->has_value()) {
    return LowerSequenceExpr(
        proc, (*entered_or)->frame, *(*entered_or)->body, span);
  }

  switch (expr.kind) {
    case slang::ast::AssertionExprKind::Simple: {
      const auto& simple = expr.as<slang::ast::SimpleAssertionExpr>();
      auto cond_or = LowerSampledExpr(proc, frame, simple.expr, span);
      if (!cond_or) return std::unexpected(std::move(cond_or.error()));
      hir::SequenceExpr boolean{
          .data =
              hir::SequenceBoolean{
                  .condition = frame.Exprs().Add(*std::move(cond_or))},
          .span = span};
      return ApplyRepetition(
          frame, std::move(boolean), simple.repetition, span);
    }
    case slang::ast::AssertionExprKind::SequenceConcat:
      return LowerSequenceConcat(
          proc, frame, expr.as<slang::ast::SequenceConcatExpr>(), span);
    case slang::ast::AssertionExprKind::SequenceWithMatch: {
      const auto& with = expr.as<slang::ast::SequenceWithMatchExpr>();
      if (!with.matchItems.empty()) {
        // LRM 16.11.
        return RefuseAssertionForm(
            span, "a subroutine call or assignment attached to a match");
      }
      auto inner_or = LowerSequenceExpr(proc, frame, with.expr, span);
      if (!inner_or) return std::unexpected(std::move(inner_or.error()));
      return ApplyRepetition(
          frame, *std::move(inner_or), with.repetition, span);
    }
    // `and` and `or` compose sequences here and properties where a property
    // stands, and the front end spells both with this node; standing here, they
    // are the composition of sequences (LRM 16.9.5, 16.9.7).
    case slang::ast::AssertionExprKind::Binary:
      return RefuseAssertionForm(
          span,
          "composing operands with 'and', 'or', 'intersect', 'throughout' or "
          "'within'");
    case slang::ast::AssertionExprKind::FirstMatch:
      return RefuseAssertionForm(span, "'first_match'");
    case slang::ast::AssertionExprKind::Clocking: {
      const auto& clocking = expr.as<slang::ast::ClockingAssertionExpr>();
      // LRM 16.13.
      auto another =
          BeginsAnotherClock(ClockingEvent{&clocking.clocking}, frame, span);
      if (!another) return std::unexpected(std::move(another.error()));
      if (*another) {
        return RefuseAssertionForm(
            span, "a sequence carrying a clocking event of its own");
      }
      return LowerSequenceExpr(proc, frame, clocking.expr, span);
    }

    // The property-only operators. The front end holds a sequence position to
    // sequence forms, so one of these arriving here means what it accepts has
    // moved rather than that a program wrote one.
    case slang::ast::AssertionExprKind::Unary:
    case slang::ast::AssertionExprKind::StrongWeak:
    case slang::ast::AssertionExprKind::Abort:
    case slang::ast::AssertionExprKind::Conditional:
    case slang::ast::AssertionExprKind::Case:
    case slang::ast::AssertionExprKind::DisableIff:
    case slang::ast::AssertionExprKind::Invalid:
      break;
  }
  throw InternalError(
      "LowerSequenceExpr: a property operator stands where the grammar admits "
      "only a sequence (LRM 16.7)");
}

auto LowerPropertyExpr(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::AssertionExpr& expr, hir::SequenceStrength strength,
    diag::SourceSpan enclosing) -> diag::Result<hir::PropertyExpr>;

// The sequence an implication or a followed-by is triggered by and the
// property it then asks of (LRM 16.12.7, 16.12.9).
struct AntecedentAndConsequent {
  hir::SequenceExprId antecedent;
  hir::PropertyExprId consequent;
};

auto LowerAntecedentAndConsequent(
    ProcessLowerer& proc, const WalkFrame& frame,
    const slang::ast::BinaryAssertionExpr& binary,
    hir::SequenceStrength strength, diag::SourceSpan span)
    -> diag::Result<AntecedentAndConsequent> {
  auto antecedent_or = LowerSequenceExpr(proc, frame, binary.left, span);
  if (!antecedent_or) return std::unexpected(std::move(antecedent_or.error()));
  auto consequent_or =
      LowerPropertyExpr(proc, frame, binary.right, strength, span);
  if (!consequent_or) return std::unexpected(std::move(consequent_or.error()));
  return AntecedentAndConsequent{
      .antecedent = AddSequence(frame, *std::move(antecedent_or)),
      .consequent = AddProperty(frame, *std::move(consequent_or))};
}

auto LowerImplication(
    ProcessLowerer& proc, const WalkFrame& frame,
    const slang::ast::BinaryAssertionExpr& binary, hir::ConsequentStart start,
    hir::SequenceStrength strength, diag::SourceSpan span)
    -> diag::Result<hir::PropertyExpr> {
  auto joined_or =
      LowerAntecedentAndConsequent(proc, frame, binary, strength, span);
  if (!joined_or) return std::unexpected(std::move(joined_or.error()));
  return hir::PropertyExpr{
      .data =
          hir::PropertyImplication{
              .antecedent = joined_or->antecedent,
              .consequent = joined_or->consequent,
              .start = start},
      .span = span};
}

auto LowerFollowedBy(
    ProcessLowerer& proc, const WalkFrame& frame,
    const slang::ast::BinaryAssertionExpr& binary, hir::ConsequentStart start,
    hir::SequenceStrength strength, diag::SourceSpan span)
    -> diag::Result<hir::PropertyExpr> {
  auto joined_or =
      LowerAntecedentAndConsequent(proc, frame, binary, strength, span);
  if (!joined_or) return std::unexpected(std::move(joined_or.error()));
  return hir::PropertyExpr{
      .data =
          hir::PropertyFollowedBy{
              .antecedent = joined_or->antecedent,
              .consequent = joined_or->consequent,
              .start = start},
      .span = span};
}

// Both operands are properties evaluated from the same start, a sequence among
// them standing as a property at the strength the statement gives it. Where
// both are sequences the front end spells the node as it spells sequence
// composition; standing as a property, `s1 and s2` holds exactly where both
// sequences do and `s1 or s2` where either does, so it is this connective
// (LRM 16.12.4, 16.12.5).
auto LowerConnective(
    ProcessLowerer& proc, const WalkFrame& frame,
    const slang::ast::BinaryAssertionExpr& binary,
    hir::PropertyConnective connective, hir::SequenceStrength strength,
    diag::SourceSpan span) -> diag::Result<hir::PropertyExpr> {
  auto lhs_or = LowerPropertyExpr(proc, frame, binary.left, strength, span);
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
  auto rhs_or = LowerPropertyExpr(proc, frame, binary.right, strength, span);
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  return hir::PropertyExpr{
      .data =
          hir::PropertyBinary{
              .connective = connective,
              .lhs = AddProperty(frame, *std::move(lhs_or)),
              .rhs = AddProperty(frame, *std::move(rhs_or))},
      .span = span};
}

// A property standing where the source may write none, such as an if
// property's else arm.
auto LowerOptionalProperty(
    ProcessLowerer& proc, const WalkFrame& frame,
    const slang::ast::AssertionExpr* expr, hir::SequenceStrength strength,
    diag::SourceSpan span) -> diag::Result<std::optional<hir::PropertyExpr>> {
  if (expr == nullptr) {
    return std::optional<hir::PropertyExpr>{};
  }
  auto property_or = LowerPropertyExpr(proc, frame, *expr, strength, span);
  if (!property_or) return std::unexpected(std::move(property_or.error()));
  return std::optional<hir::PropertyExpr>{*std::move(property_or)};
}

auto AddOptionalProperty(
    const WalkFrame& frame, std::optional<hir::PropertyExpr> property)
    -> std::optional<hir::PropertyExprId> {
  return std::move(property).transform([&](hir::PropertyExpr written) {
    return AddProperty(frame, std::move(written));
  });
}

auto LowerConditionalProperty(
    ProcessLowerer& proc, const WalkFrame& frame,
    const slang::ast::ConditionalAssertionExpr& conditional,
    hir::SequenceStrength strength, diag::SourceSpan span)
    -> diag::Result<hir::PropertyExpr> {
  auto condition_or =
      LowerSampledExpr(proc, frame, conditional.condition, span);
  if (!condition_or) return std::unexpected(std::move(condition_or.error()));
  auto then_or =
      LowerPropertyExpr(proc, frame, conditional.ifExpr, strength, span);
  if (!then_or) return std::unexpected(std::move(then_or.error()));
  auto else_or =
      LowerOptionalProperty(proc, frame, conditional.elseExpr, strength, span);
  if (!else_or) return std::unexpected(std::move(else_or.error()));
  return hir::PropertyExpr{
      .data =
          hir::PropertyConditional{
              .condition = frame.Exprs().Add(*std::move(condition_or)),
              .then_property = AddProperty(frame, *std::move(then_or)),
              .else_property = AddOptionalProperty(frame, *std::move(else_or))},
      .span = span};
}

auto LowerCaseProperty(
    ProcessLowerer& proc, const WalkFrame& frame,
    const slang::ast::CaseAssertionExpr& case_property,
    hir::SequenceStrength strength, diag::SourceSpan span)
    -> diag::Result<hir::PropertyExpr> {
  auto subject_or = LowerSampledExpr(proc, frame, case_property.expr, span);
  if (!subject_or) return std::unexpected(std::move(subject_or.error()));
  std::vector<hir::PropertyCaseItem> items;
  items.reserve(case_property.items.size());
  for (const auto& group : case_property.items) {
    hir::PropertyCaseItem item{.labels = {}, .property = {}};
    item.labels.reserve(group.expressions.size());
    for (const slang::ast::Expression* label : group.expressions) {
      auto label_or = LowerSampledExpr(proc, frame, *label, span);
      if (!label_or) return std::unexpected(std::move(label_or.error()));
      item.labels.push_back(frame.Exprs().Add(*std::move(label_or)));
    }
    auto property_or =
        LowerPropertyExpr(proc, frame, *group.body, strength, span);
    if (!property_or) return std::unexpected(std::move(property_or.error()));
    item.property = AddProperty(frame, *std::move(property_or));
    items.push_back(std::move(item));
  }
  auto default_or = LowerOptionalProperty(
      proc, frame, case_property.defaultCase, strength, span);
  if (!default_or) return std::unexpected(std::move(default_or.error()));
  return hir::PropertyExpr{
      .data =
          hir::PropertyCase{
              .subject = frame.Exprs().Add(*std::move(subject_or)),
              .items = std::move(items),
              .default_property =
                  AddOptionalProperty(frame, *std::move(default_or))},
      .span = span};
}

auto LowerSequenceAsProperty(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::AssertionExpr& expr, hir::SequenceStrength strength,
    diag::SourceSpan span) -> diag::Result<hir::PropertyExpr> {
  auto sequence_or = LowerSequenceExpr(proc, frame, expr, span);
  if (!sequence_or) return std::unexpected(std::move(sequence_or.error()));
  return hir::PropertyExpr{
      .data =
          hir::PropertySequence{
              .sequence = AddSequence(frame, *std::move(sequence_or)),
              .strength = strength},
      .span = span};
}

auto LowerPropertyExpr(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::AssertionExpr& expr, hir::SequenceStrength strength,
    diag::SourceSpan enclosing) -> diag::Result<hir::PropertyExpr> {
  const diag::SourceSpan span = NodeSpan(expr, enclosing);
  // LRM 16.10.
  if (CarriesLocalVars(expr)) {
    return RefuseAssertionForm(span, "a local variable");
  }
  auto entered_or = EnterNestedInstance(proc.Owner(), expr, frame, span);
  if (!entered_or) return std::unexpected(std::move(entered_or.error()));
  if (entered_or->has_value()) {
    return LowerPropertyExpr(
        proc, (*entered_or)->frame, *(*entered_or)->body, strength, span);
  }

  switch (expr.kind) {
    case slang::ast::AssertionExprKind::Binary: {
      const auto& binary = expr.as<slang::ast::BinaryAssertionExpr>();
      switch (binary.op) {
        case slang::ast::BinaryAssertionOperator::OverlappedImplication:
          return LowerImplication(
              proc, frame, binary, hir::ConsequentStart::kSameTick, strength,
              span);
        case slang::ast::BinaryAssertionOperator::NonOverlappedImplication:
          return LowerImplication(
              proc, frame, binary, hir::ConsequentStart::kNextTick, strength,
              span);
        case slang::ast::BinaryAssertionOperator::OverlappedFollowedBy:
          return LowerFollowedBy(
              proc, frame, binary, hir::ConsequentStart::kSameTick, strength,
              span);
        case slang::ast::BinaryAssertionOperator::NonOverlappedFollowedBy:
          return LowerFollowedBy(
              proc, frame, binary, hir::ConsequentStart::kNextTick, strength,
              span);
        case slang::ast::BinaryAssertionOperator::And:
          return LowerConnective(
              proc, frame, binary, hir::PropertyConnective::kAnd, strength,
              span);
        case slang::ast::BinaryAssertionOperator::Or:
          return LowerConnective(
              proc, frame, binary, hir::PropertyConnective::kOr, strength,
              span);
        case slang::ast::BinaryAssertionOperator::Implies:
          return LowerConnective(
              proc, frame, binary, hir::PropertyConnective::kImplies, strength,
              span);
        case slang::ast::BinaryAssertionOperator::Iff:
          return LowerConnective(
              proc, frame, binary, hir::PropertyConnective::kIff, strength,
              span);
        case slang::ast::BinaryAssertionOperator::Until:
        case slang::ast::BinaryAssertionOperator::SUntil:
        case slang::ast::BinaryAssertionOperator::UntilWith:
        case slang::ast::BinaryAssertionOperator::SUntilWith:
          return RefuseAssertionForm(
              span,
              "a property operator over time ('until', 's_until', "
              "'until_with', 's_until_with')");

        // Composition the grammar admits only on sequences, which the
        // sequence path names.
        case slang::ast::BinaryAssertionOperator::Intersect:
        case slang::ast::BinaryAssertionOperator::Throughout:
        case slang::ast::BinaryAssertionOperator::Within:
          break;
      }
      return LowerSequenceAsProperty(proc, frame, expr, strength, span);
    }
    case slang::ast::AssertionExprKind::StrongWeak: {
      const auto& named = expr.as<slang::ast::StrongWeakAssertionExpr>();
      const hir::SequenceStrength named_strength =
          named.strength == slang::ast::StrongWeakAssertionExpr::Strong
              ? hir::SequenceStrength::kStrong
              : hir::SequenceStrength::kWeak;
      return LowerSequenceAsProperty(
          proc, frame, named.expr, named_strength, span);
    }
    case slang::ast::AssertionExprKind::Unary: {
      const auto& unary = expr.as<slang::ast::UnaryAssertionExpr>();
      switch (unary.op) {
        // The operand's sequences keep the strength written for them or the
        // statement's; what `not` exchanges is what a strength asks (LRM
        // 16.12.3).
        case slang::ast::UnaryAssertionOperator::Not: {
          auto operand_or =
              LowerPropertyExpr(proc, frame, unary.expr, strength, span);
          if (!operand_or)
            return std::unexpected(std::move(operand_or.error()));
          return hir::PropertyExpr{
              .data =
                  hir::PropertyNot{
                      .operand = AddProperty(frame, *std::move(operand_or))},
              .span = span};
        }
        case slang::ast::UnaryAssertionOperator::NextTime:
        case slang::ast::UnaryAssertionOperator::SNextTime:
        case slang::ast::UnaryAssertionOperator::Always:
        case slang::ast::UnaryAssertionOperator::SAlways:
        case slang::ast::UnaryAssertionOperator::Eventually:
        case slang::ast::UnaryAssertionOperator::SEventually:
          break;
      }
      return RefuseAssertionForm(
          span,
          "a property operator over time ('nexttime', 'always', "
          "'eventually')");
    }
    case slang::ast::AssertionExprKind::Conditional:
      return LowerConditionalProperty(
          proc, frame, expr.as<slang::ast::ConditionalAssertionExpr>(),
          strength, span);
    case slang::ast::AssertionExprKind::Case:
      return LowerCaseProperty(
          proc, frame, expr.as<slang::ast::CaseAssertionExpr>(), strength,
          span);
    case slang::ast::AssertionExprKind::Abort:
      return RefuseAssertionForm(
          span, "an abort operator ('accept_on', 'reject_on')");
    case slang::ast::AssertionExprKind::Clocking: {
      const auto& clocking = expr.as<slang::ast::ClockingAssertionExpr>();
      // LRM 16.13.
      auto another =
          BeginsAnotherClock(ClockingEvent{&clocking.clocking}, frame, span);
      if (!another) return std::unexpected(std::move(another.error()));
      if (*another) {
        return RefuseAssertionForm(
            span, "a property carrying more than one clocking event");
      }
      return LowerPropertyExpr(proc, frame, clocking.expr, strength, span);
    }
    case slang::ast::AssertionExprKind::DisableIff:
      return RefuseAssertionForm(
          span, "a 'disable iff' clause anywhere but on the property itself");

    // A sequence standing where a property does, which LRM 16.12.2 reads at
    // the strength the statement gives it.
    case slang::ast::AssertionExprKind::Simple:
    case slang::ast::AssertionExprKind::SequenceConcat:
    case slang::ast::AssertionExprKind::SequenceWithMatch:
    case slang::ast::AssertionExprKind::FirstMatch:
      return LowerSequenceAsProperty(proc, frame, expr, strength, span);

    case slang::ast::AssertionExprKind::Invalid:
      break;
  }
  throw InternalError(
      "LowerPropertyExpr: the front end reports a malformed assertion "
      "expression rather than handing one on");
}

// LRM 16.12 property_spec, unwrapped from the outside in: the clocking event,
// then the disable condition, then the property, with what the source left
// unwritten resolved. A named property instance is unwrapped alongside them,
// because the clocking event a property declares -- in its body, or by being
// declared in a clocking block (LRM 16.16 b) -- leads the assertion.
struct PropertySpecParts {
  // A statement left with no clock is refused before its parts exist. One the
  // source wrote is resolved where the instances it may name are recorded.
  ClockingEvent clock;
  const slang::ast::Expression* disable_condition;
  const slang::ast::AssertionExpr* body;
  // The frame the clock, the disable condition, the body and the action blocks
  // are lowered in, which records the instances unwrapped on the way to the
  // body, each where it stood.
  WalkFrame body_frame;
};

// The leading clock starts as the clock in force where the statement stands,
// and each clocking event met replaces it (LRM 16.13.3). `inherited_disable` is
// the condition the statement takes where its property states none. A statement
// left with no clock has no ticks to be evaluated at, and is refused.
auto SplitPropertySpec(
    ProcessLowerer& proc, const WalkFrame& frame,
    const slang::ast::AssertionExpr& spec,
    const slang::ast::Expression* inherited_disable, diag::SourceSpan span)
    -> diag::Result<PropertySpecParts> {
  std::optional<ClockingEvent> clock = ClockInForce(proc, frame);
  const slang::ast::Expression* written_disable = nullptr;
  WalkFrame body_frame = frame;
  const slang::ast::AssertionExpr* cursor = &spec;
  while (true) {
    if (cursor->kind == slang::ast::AssertionExprKind::Clocking) {
      const auto& clocking = cursor->as<slang::ast::ClockingAssertionExpr>();
      clock = ClockingEvent{&clocking.clocking};
      cursor = &clocking.expr;
      continue;
    }
    if (cursor->kind == slang::ast::AssertionExprKind::DisableIff &&
        written_disable == nullptr) {
      const auto& disable = cursor->as<slang::ast::DisableIffAssertionExpr>();
      written_disable = &disable.condition;
      cursor = &disable.expr;
      continue;
    }
    if (const auto* body = InstanceBodyOf(*cursor);
        body != nullptr && !CarriesLocalVars(*cursor)) {
      body_frame = body_frame.WithExpansionInstance(
          InstanceOf(*cursor)->symbol, clock,
          ReachedThrough(*InstanceOf(*cursor)));
      if (std::optional<ClockingEvent> declared =
              DeclaredClockOf(proc.Owner(), *cursor, body_frame)) {
        clock = *std::move(declared);
      }
      cursor = body;
      continue;
    }
    break;
  }
  if (!clock.has_value()) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedStatementForm,
        "this concurrent assertion names no clocking event and none is settled "
        "where it is written, so there are no ticks for it to be evaluated at");
  }
  const slang::ast::Expression* disable =
      written_disable != nullptr
          ? ResolveWrittenDisable(*frame.reader_scope, *written_disable)
          : inherited_disable;
  return PropertySpecParts{
      .clock = *std::move(clock),
      .disable_condition = disable,
      .body = cursor,
      .body_frame = std::move(body_frame)};
}

// The disable condition is read outside the assertion's clock: a sampled
// value function there counts ticks of a clock it writes, which the front end
// requires (LRM 16.9.3).
auto LowerPropertySpec(
    ProcessLowerer& proc, const PropertySpecParts& parts,
    hir::SequenceStrength strength, diag::SourceSpan span)
    -> diag::Result<hir::PropertySpec> {
  const WalkFrame& frame = parts.body_frame;
  auto event_or = LowerEventControl(proc, frame, parts.clock, span);
  if (!event_or) return std::unexpected(std::move(event_or.error()));

  // LRM 16.12 tests the disable condition across the whole interval from the
  // start of an attempt to its end, not at that interval's ticks, so what it
  // reads is carried here the way every other construct sensitive to a value
  // carries it. A condition that rises and falls between two ticks is visible
  // no other way.
  std::optional<hir::DisableCondition> disable;
  if (parts.disable_condition != nullptr) {
    auto cond_or = proc.LowerExpr(*parts.disable_condition, frame);
    if (!cond_or) return std::unexpected(std::move(cond_or.error()));
    auto sensitivity_or =
        CellsOfWaitedExpression(proc, frame, *parts.disable_condition);
    if (!sensitivity_or) {
      return std::unexpected(std::move(sensitivity_or.error()));
    }
    disable = hir::DisableCondition{
        .condition = frame.Exprs().Add(*std::move(cond_or)),
        .sensitivity = *std::move(sensitivity_or)};
  }

  auto body_or = LowerPropertyExpr(
      proc, frame.WithAssertionClock(parts.clock), *parts.body, strength, span);
  if (!body_or) return std::unexpected(std::move(body_or.error()));

  return hir::PropertySpec{
      .clock = *std::move(event_or),
      .disable = std::move(disable),
      .body = AddProperty(frame, *std::move(body_or))};
}

// LRM 16.17: the process reaching an expect waits for one evaluation of its
// property, read as a cover property reads its sequence (Annex F.3.4.3.1), and
// a failure with no else clause is the tool's own `$error` (LRM 20.10). An
// expect is no concurrent assertion statement, so it inherits no default
// disable condition (LRM 16.15).
auto LowerExpectStmt(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::ConcurrentAssertionStatement& as, diag::SourceSpan span)
    -> diag::Result<hir::Stmt> {
  auto parts_or =
      SplitPropertySpec(proc, frame, as.propertySpec, nullptr, span);
  if (!parts_or) return std::unexpected(std::move(parts_or.error()));
  auto spec_or =
      LowerPropertySpec(proc, *parts_or, hir::SequenceStrength::kStrong, span);
  if (!spec_or) return std::unexpected(std::move(spec_or.error()));

  // A sampled value function in an action block counts ticks of the
  // assertion's leading clock (LRM 16.9.3).
  const WalkFrame clocked =
      parts_or->body_frame.WithAssertionClock(parts_or->clock);
  auto pass_or = LowerActionArm(proc, clocked, as.ifTrue);
  if (!pass_or) return std::unexpected(std::move(pass_or.error()));

  auto fail_or = LowerActionArm(proc, clocked, as.ifFalse);
  if (!fail_or) return std::unexpected(std::move(fail_or.error()));

  hir::ProceduralBody& body = *frame.current_procedural_body;
  return hir::Stmt{
      .label = std::nullopt,
      .data =
          hir::ExpectStmt{
              .spec = *std::move(spec_or),
              .pass_stmt = AddActionArm(body, *std::move(pass_or)),
              .fail_stmt = AddActionArm(body, *std::move(fail_or))},
      .span = span};
}

}  // namespace

auto EvaluatedInSimulation(
    const slang::ast::ConcurrentAssertionStatement& assertion,
    support::AssertionPolicy policy) -> bool {
  switch (assertion.assertionKind) {
    // LRM 16.14.4: `restrict` is the one directive simulation does not
    // verify, so no policy value makes it evaluate.
    case slang::ast::AssertionKind::Restrict:
      return false;
    // LRM 16.17: the process waits at an expect for its answer, so no policy
    // value removes it.
    case slang::ast::AssertionKind::Expect:
      return true;
    case slang::ast::AssertionKind::Assert:
    case slang::ast::AssertionKind::Assume:
    case slang::ast::AssertionKind::CoverProperty:
    case slang::ast::AssertionKind::CoverSequence:
      break;
  }
  return !support::ElidesAssertions(policy);
}

auto StaticConcurrentAssertionOf(const slang::ast::ProceduralBlockSymbol& proc)
    -> StaticConcurrentAssertion {
  if (proc.procedureKind != slang::ast::ProceduralBlockKind::Always) {
    return {};
  }
  // The front end hoists a procedure's outermost block and may wrap a lone
  // statement in a list, so the shape is read through those rather than off the
  // body node.
  StaticConcurrentAssertion found;
  const slang::ast::Statement* stmt = &proc.getBody();
  while (true) {
    if (stmt->kind == slang::ast::StatementKind::Block) {
      const auto& block = stmt->as<slang::ast::BlockStatement>();
      if (block.blockSymbol != nullptr && !block.blockSymbol->name.empty()) {
        found.named_block = block.blockSymbol;
      }
      stmt = &block.body;
      continue;
    }
    if (stmt->kind == slang::ast::StatementKind::List) {
      const auto& list = stmt->as<slang::ast::StatementList>();
      if (list.list.size() != 1) {
        return {};
      }
      stmt = list.list.front();
      continue;
    }
    break;
  }
  if (stmt->kind != slang::ast::StatementKind::ConcurrentAssertion) {
    return {};
  }
  // An `always` around an expect waits at it on every pass, which is a
  // procedure and not the equivalence LRM 16.14.5 states for assert, assume
  // and cover.
  const auto& assertion = stmt->as<slang::ast::ConcurrentAssertionStatement>();
  if (assertion.assertionKind == slang::ast::AssertionKind::Expect) {
    return {};
  }
  found.assertion = &assertion;
  return found;
}

auto LowerImmediateAssertionStmt(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::ImmediateAssertionStatement& as, diag::SourceSpan span)
    -> diag::Result<hir::Stmt> {
  const hir::AssertionTiming timing = TimingOf(as);

  auto cond_or = proc.LowerExpr(as.cond, frame);
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  const hir::ExprId condition = frame.Exprs().Add(*std::move(cond_or));

  auto pass_or = LowerActionArm(proc, frame, as.ifTrue);
  if (!pass_or) return std::unexpected(std::move(pass_or.error()));

  auto fail_or = LowerActionArm(proc, frame, as.ifFalse);
  if (!fail_or) return std::unexpected(std::move(fail_or.error()));

  hir::ProceduralBody& body = *frame.current_procedural_body;
  const std::optional<hir::StmtId> pass_stmt =
      AddActionArm(body, *std::move(pass_or));
  const std::optional<hir::StmtId> fail_stmt =
      AddActionArm(body, *std::move(fail_or));

  switch (as.assertionKind) {
    case slang::ast::AssertionKind::Assert:
      return AssertStmtOf(
          hir::AssertionDirective::kAssert, timing, condition, pass_stmt,
          fail_stmt, span);
    case slang::ast::AssertionKind::Assume:
      return AssertStmtOf(
          hir::AssertionDirective::kAssume, timing, condition, pass_stmt,
          fail_stmt, span);
    case slang::ast::AssertionKind::CoverProperty:
      return hir::Stmt{
          .label = std::nullopt,
          .data =
              hir::CoverStmt{
                  .timing = timing,
                  .condition = condition,
                  .pass_stmt = pass_stmt},
          .span = span};
    case slang::ast::AssertionKind::CoverSequence:
    case slang::ast::AssertionKind::Restrict:
    case slang::ast::AssertionKind::Expect:
      break;
  }
  throw InternalError(
      "LowerImmediateAssertionStmt: an immediate assertion statement carried "
      "a directive that has no immediate form (LRM 16.2)");
}

auto LowerConcurrentAssertion(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::ConcurrentAssertionStatement& as, diag::SourceSpan span)
    -> diag::Result<hir::ConcurrentAssertion> {
  // LRM 16.12.2: a sequence standing as the property is read weakly where the
  // statement states an obligation and strongly where it names a coverage goal.
  const hir::SequenceStrength strength =
      as.assertionKind == slang::ast::AssertionKind::CoverProperty
          ? hir::SequenceStrength::kStrong
          : hir::SequenceStrength::kWeak;

  // A concurrent assertion inherits its scope's default disable condition
  // where its property states none (LRM 16.15).
  auto parts_or = SplitPropertySpec(
      proc, frame, as.propertySpec,
      DefaultDisableCondition(*frame.reader_scope), span);
  if (!parts_or) return std::unexpected(std::move(parts_or.error()));
  auto spec_or = LowerPropertySpec(proc, *parts_or, strength, span);
  if (!spec_or) return std::unexpected(std::move(spec_or.error()));

  // A sampled value function in an action block counts ticks of the
  // assertion's leading clock (LRM 16.9.3).
  const WalkFrame clocked =
      parts_or->body_frame.WithAssertionClock(parts_or->clock);
  auto pass_or = LowerActionArm(proc, clocked, as.ifTrue);
  if (!pass_or) return std::unexpected(std::move(pass_or.error()));

  auto fail_or = LowerActionArm(proc, clocked, as.ifFalse);
  if (!fail_or) return std::unexpected(std::move(fail_or.error()));

  hir::ProceduralBody& body = *frame.current_procedural_body;
  const std::optional<hir::StmtId> pass_stmt =
      AddActionArm(body, *std::move(pass_or));
  const std::optional<hir::StmtId> fail_stmt =
      AddActionArm(body, *std::move(fail_or));

  const auto assert_form =
      [&](hir::AssertionDirective directive) -> hir::ConcurrentAssertion {
    return hir::ConcurrentAssertStmt{
        .directive = directive,
        .spec = *std::move(spec_or),
        .pass_stmt = pass_stmt,
        .fail_stmt = fail_stmt};
  };

  switch (as.assertionKind) {
    case slang::ast::AssertionKind::Assert:
      return assert_form(hir::AssertionDirective::kAssert);
    case slang::ast::AssertionKind::Assume:
      return assert_form(hir::AssertionDirective::kAssume);
    case slang::ast::AssertionKind::CoverProperty:
      return hir::ConcurrentAssertion{hir::ConcurrentCoverStmt{
          .spec = *std::move(spec_or), .pass_stmt = pass_stmt}};
    // A cover sequence counts every match of an attempt rather than the
    // attempt (LRM 16.14.3).
    case slang::ast::AssertionKind::CoverSequence:
      return RefuseAssertionForm(span, "a 'cover sequence' statement");
    case slang::ast::AssertionKind::Restrict:
      throw InternalError(
          "LowerConcurrentAssertion: a restrict is not checked in simulation, "
          "so it is left out where the statement is read and never reaches a "
          "lowering of the property it states");
    case slang::ast::AssertionKind::Expect:
      break;
  }
  throw InternalError(
      "LowerConcurrentAssertion: an expect statement is a wait rather than a "
      "concurrent assertion item, and is lowered as the statement it is (LRM "
      "16.17)");
}

auto LowerConcurrentAssertionStmt(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::ConcurrentAssertionStatement& as, diag::SourceSpan span)
    -> diag::Result<hir::Stmt> {
  if (as.assertionKind == slang::ast::AssertionKind::Expect) {
    return LowerExpectStmt(proc, frame, as, span);
  }
  auto assertion_or = LowerConcurrentAssertion(proc, frame, as, span);
  if (!assertion_or) return std::unexpected(std::move(assertion_or.error()));
  return std::visit(
      [&](auto form) {
        return hir::Stmt{
            .label = std::nullopt,
            .data = hir::StmtData{std::move(form)},
            .span = span};
      },
      *std::move(assertion_or));
}

}  // namespace lyra::lowering::ast_to_hir
