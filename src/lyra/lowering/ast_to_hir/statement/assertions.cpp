#include "lyra/lowering/ast_to_hir/statement/assertions.hpp"

#include <expected>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <variant>

#include <slang/ast/SemanticFacts.h>
#include <slang/ast/Statement.h>
#include <slang/ast/TimingControl.h>
#include <slang/ast/expressions/AssertionExpr.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/statements/MiscStatements.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/syntax/SyntaxNode.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/hir/expr_builders.hpp"
#include "lyra/lowering/ast_to_hir/sensitivity.hpp"
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
auto NodeSpan(
    const ProcessLowerer& proc, const slang::ast::AssertionExpr& expr,
    diag::SourceSpan enclosing) -> diag::SourceSpan {
  if (expr.syntax == nullptr) {
    return enclosing;
  }
  return proc.Owner().SourceMapper().SpanOf(expr.syntax->sourceRange());
}

auto RefuseAssertionForm(diag::SourceSpan span, std::string_view what)
    -> std::unexpected<diag::Diagnostic> {
  return diag::Fail(
      span, diag::DiagCode::kUnsupportedStatementForm,
      std::string{what} +
          " is not yet supported in a concurrent assertion; pass --assertions "
          "skip to elide it");
}

// LRM 16.7's `$` is a window with no end, so an evaluation that enters one can
// outlive every tick the trace has and is answered by what the statement
// demands of a pending result rather than by a tick. Nothing here carries that
// yet, so it is refused where the bound is read.
auto TickRangeOf(const slang::ast::SequenceRange& range, diag::SourceSpan span)
    -> diag::Result<hir::TickRange> {
  if (!range.max.has_value()) {
    return RefuseAssertionForm(span, "a `$` bound on a delay or repetition");
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

auto AddSequence(const WalkFrame& frame, hir::SequenceExpr expr)
    -> hir::SequenceExprId {
  return frame.SequenceExprs().Add(std::move(expr));
}

auto AddProperty(const WalkFrame& frame, hir::PropertyExpr expr)
    -> hir::PropertyExprId {
  return frame.PropertyExprs().Add(std::move(expr));
}

// LRM 16.9.2: a repetition written on a sequence element. Only the consecutive
// form is carried, and only where every match spans at least one tick -- a
// repetition admitting an empty match is what makes an evaluation's position
// set insufficient to say where it is.
auto ApplyRepetition(
    const WalkFrame& frame, hir::SequenceExpr body,
    const std::optional<slang::ast::SequenceRepetition>& repetition,
    diag::SourceSpan span) -> diag::Result<hir::SequenceExpr> {
  if (!repetition.has_value()) {
    return body;
  }
  if (repetition->kind != slang::ast::SequenceRepetition::Consecutive) {
    return RefuseAssertionForm(
        span, "a nonconsecutive or goto repetition (`[=]`, `[->]`)");
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
  const diag::SourceSpan span = NodeSpan(proc, expr, enclosing);
  if (CarriesLocalVars(expr)) {
    return RefuseAssertionForm(span, "a local variable (LRM 16.10)");
  }
  if (const auto* body = InstanceBodyOf(expr); body != nullptr) {
    return LowerSequenceExpr(proc, frame, *body, span);
  }

  switch (expr.kind) {
    case slang::ast::AssertionExprKind::Simple: {
      const auto& simple = expr.as<slang::ast::SimpleAssertionExpr>();
      auto cond_or = proc.LowerExpr(simple.expr, frame);
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
        return RefuseAssertionForm(
            span,
            "a subroutine call or assignment attached to a match (LRM "
            "16.11)");
      }
      auto inner_or = LowerSequenceExpr(proc, frame, with.expr, span);
      if (!inner_or) return std::unexpected(std::move(inner_or.error()));
      return ApplyRepetition(
          frame, *std::move(inner_or), with.repetition, span);
    }
    // `and` and `or` compose sequences here and properties one level up, and
    // the front end spells both with this node, so the refusal names the
    // operator and leaves the level to the source.
    case slang::ast::AssertionExprKind::Binary:
      return RefuseAssertionForm(
          span,
          "composing operands with `and`, `or`, `intersect`, `throughout` or "
          "`within`");
    case slang::ast::AssertionExprKind::FirstMatch:
      return RefuseAssertionForm(span, "`first_match`");
    case slang::ast::AssertionExprKind::Clocking:
      return RefuseAssertionForm(
          span, "a sequence carrying a clocking event of its own (LRM 16.13)");

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

auto LowerImplication(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::BinaryAssertionExpr& binary, hir::ImplicationStart start,
    hir::SequenceStrength strength, diag::SourceSpan span)
    -> diag::Result<hir::PropertyExpr> {
  auto antecedent_or = LowerSequenceExpr(proc, frame, binary.left, span);
  if (!antecedent_or) return std::unexpected(std::move(antecedent_or.error()));
  auto consequent_or =
      LowerPropertyExpr(proc, frame, binary.right, strength, span);
  if (!consequent_or) return std::unexpected(std::move(consequent_or.error()));
  return hir::PropertyExpr{
      .data =
          hir::PropertyImplication{
              .antecedent = AddSequence(frame, *std::move(antecedent_or)),
              .consequent = AddProperty(frame, *std::move(consequent_or)),
              .start = start},
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
  const diag::SourceSpan span = NodeSpan(proc, expr, enclosing);
  if (CarriesLocalVars(expr)) {
    return RefuseAssertionForm(span, "a local variable (LRM 16.10)");
  }
  if (const auto* body = InstanceBodyOf(expr); body != nullptr) {
    return LowerPropertyExpr(proc, frame, *body, strength, span);
  }

  switch (expr.kind) {
    case slang::ast::AssertionExprKind::Binary: {
      const auto& binary = expr.as<slang::ast::BinaryAssertionExpr>();
      switch (binary.op) {
        case slang::ast::BinaryAssertionOperator::OverlappedImplication:
          return LowerImplication(
              proc, frame, binary, hir::ImplicationStart::kSameTick, strength,
              span);
        case slang::ast::BinaryAssertionOperator::NonOverlappedImplication:
          return LowerImplication(
              proc, frame, binary, hir::ImplicationStart::kNextTick, strength,
              span);

        // Operators only a property carries, so the refusal names them rather
        // than the sequence composition the same node kind also spells.
        case slang::ast::BinaryAssertionOperator::Iff:
        case slang::ast::BinaryAssertionOperator::Implies:
        case slang::ast::BinaryAssertionOperator::Until:
        case slang::ast::BinaryAssertionOperator::SUntil:
        case slang::ast::BinaryAssertionOperator::UntilWith:
        case slang::ast::BinaryAssertionOperator::SUntilWith:
          return RefuseAssertionForm(
              span,
              "a property connective (`implies`, `iff`, `until`, `s_until`)");
        case slang::ast::BinaryAssertionOperator::OverlappedFollowedBy:
        case slang::ast::BinaryAssertionOperator::NonOverlappedFollowedBy:
          return RefuseAssertionForm(
              span, "a followed-by operator (`#-#`, `#=#`)");

        // Composition the grammar admits on sequences, which the sequence
        // path already names.
        case slang::ast::BinaryAssertionOperator::And:
        case slang::ast::BinaryAssertionOperator::Or:
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
    case slang::ast::AssertionExprKind::Unary:
      return RefuseAssertionForm(
          span,
          "a property operator over time (`not`, `nexttime`, `always`, "
          "`eventually`)");
    case slang::ast::AssertionExprKind::Conditional:
    case slang::ast::AssertionExprKind::Case:
      return RefuseAssertionForm(span, "a property selecting between arms");
    case slang::ast::AssertionExprKind::Abort:
      return RefuseAssertionForm(
          span, "an abort operator (`accept_on`, `reject_on`)");
    case slang::ast::AssertionExprKind::Clocking:
      return RefuseAssertionForm(
          span, "a property carrying more than one clocking event (LRM 16.13)");
    case slang::ast::AssertionExprKind::DisableIff:
      return RefuseAssertionForm(
          span, "a `disable iff` clause anywhere but on the property itself");

    // A sequence standing where a property does, which LRM 16.12.2 reads at
    // the strength the statement demands.
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
// then the disable condition, then the property. A named property instance is
// unwrapped alongside them, because the clocking event a property declares is
// the one the assertion counts ticks of (LRM 16.13.3).
struct PropertySpecParts {
  const slang::ast::TimingControl* clock = nullptr;
  const slang::ast::Expression* disable_condition = nullptr;
  const slang::ast::AssertionExpr* body = nullptr;
};

auto SplitPropertySpec(const slang::ast::AssertionExpr& spec)
    -> PropertySpecParts {
  PropertySpecParts parts;
  const slang::ast::AssertionExpr* cursor = &spec;
  while (true) {
    if (cursor->kind == slang::ast::AssertionExprKind::Clocking) {
      const auto& clocking = cursor->as<slang::ast::ClockingAssertionExpr>();
      if (parts.clock != nullptr) {
        break;
      }
      parts.clock = &clocking.clocking;
      cursor = &clocking.expr;
      continue;
    }
    if (cursor->kind == slang::ast::AssertionExprKind::DisableIff) {
      const auto& disable = cursor->as<slang::ast::DisableIffAssertionExpr>();
      if (parts.disable_condition != nullptr) {
        break;
      }
      parts.disable_condition = &disable.condition;
      cursor = &disable.expr;
      continue;
    }
    if (const auto* body = InstanceBodyOf(*cursor);
        body != nullptr && !CarriesLocalVars(*cursor)) {
      cursor = body;
      continue;
    }
    break;
  }
  parts.body = cursor;
  return parts;
}

auto LowerPropertySpec(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::AssertionExpr& spec, hir::SequenceStrength strength,
    diag::SourceSpan span) -> diag::Result<hir::PropertySpec> {
  const PropertySpecParts parts = SplitPropertySpec(spec);

  // Where the source wrote no clocking event, the one the enclosing procedure
  // settles is the assertion's (LRM 16.14.6), and the front end has already
  // applied that rule and the default clocking behind it (LRM 14.12).
  const slang::ast::TimingControl* clock =
      parts.clock != nullptr
          ? parts.clock
          : proc.Owner().InferredProcedureClock(proc.ContainingSymbol());
  if (clock == nullptr) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedStatementForm,
        "this concurrent assertion names no clocking event and none is settled "
        "where it is written, so there are no ticks for it to be evaluated at");
  }

  auto event_or = LowerEventControl(proc, frame, *clock, span);
  if (!event_or) return std::unexpected(std::move(event_or.error()));
  auto* value_change = std::get_if<hir::EventControl>(&*event_or);
  if (value_change == nullptr) {
    return RefuseAssertionForm(
        span,
        "a clocking event that is a named event rather than a value "
        "change");
  }

  std::optional<hir::ExprId> disable_condition;
  if (parts.disable_condition != nullptr) {
    auto cond_or = proc.LowerExpr(*parts.disable_condition, frame);
    if (!cond_or) return std::unexpected(std::move(cond_or.error()));
    disable_condition = frame.Exprs().Add(*std::move(cond_or));
  }

  auto body_or = LowerPropertyExpr(proc, frame, *parts.body, strength, span);
  if (!body_or) return std::unexpected(std::move(body_or.error()));

  return hir::PropertySpec{
      .clock = *std::move(value_change),
      .disable_condition = disable_condition,
      .body = AddProperty(frame, *std::move(body_or))};
}

}  // namespace

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
  found.assertion = &stmt->as<slang::ast::ConcurrentAssertionStatement>();
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
      if (timing != hir::AssertionTiming::kSimple) {
        return diag::Fail(
            span, diag::DiagCode::kUnsupportedStatementForm,
            "a deferred cover statement holds its recorded hit back to a later "
            "region of the time step, which is not yet supported; pass "
            "--assertions skip to elide it");
      }
      return hir::Stmt{
          .label = std::nullopt,
          .data =
              hir::CoverStmt{.condition = condition, .pass_stmt = pass_stmt},
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

  auto spec_or =
      LowerPropertySpec(proc, frame, as.propertySpec, strength, span);
  if (!spec_or) return std::unexpected(std::move(spec_or.error()));

  auto pass_or = LowerActionArm(proc, frame, as.ifTrue);
  if (!pass_or) return std::unexpected(std::move(pass_or.error()));

  auto fail_or = LowerActionArm(proc, frame, as.ifFalse);
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
    case slang::ast::AssertionKind::CoverSequence:
      return RefuseAssertionForm(
          span,
          "a cover sequence statement, which counts every match of an "
          "attempt rather than the attempt");
    case slang::ast::AssertionKind::Restrict:
      return RefuseAssertionForm(
          span,
          "a restrict statement, which constrains a formal tool and is "
          "not verified in simulation");
    case slang::ast::AssertionKind::Expect:
      break;
  }
  throw InternalError(
      "LowerConcurrentAssertion: an expect statement is a wait rather than a "
      "concurrent assertion item (LRM 16.17)");
}

}  // namespace lyra::lowering::ast_to_hir
