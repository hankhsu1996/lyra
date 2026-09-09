#include "lyra/lowering/ast_to_hir/expression/assignment.hpp"

#include <expected>
#include <optional>
#include <string>
#include <string_view>
#include <utility>

#include <slang/ast/Expression.h>
#include <slang/ast/TimingControl.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/expressions/OperatorExpressions.h>
#include <slang/ast/expressions/SelectExpressions.h>
#include <slang/ast/symbols/MemberSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/Type.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/hir/conversion.hpp"
#include "lyra/lowering/ast_to_hir/expression/references.hpp"
#include "lyra/lowering/ast_to_hir/expression/slang_atoms.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/statement/timing.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// LRM 9.4.5: when the update happens. A nonblocking assignment's control says
// which slot's NBA region the update lands in and leaves the procedure running;
// a blocking one suspends the procedure, which is a statement rather than an
// expression, so it is expanded into the equivalent statement sequence before
// any expression is built.
auto LowerAssignTiming(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::AssignmentExpression& as, diag::SourceSpan span)
    -> diag::Result<hir::EffectTiming> {
  if (as.timingControl == nullptr) {
    return as.isNonBlocking() ? hir::EffectTiming{hir::NonBlockingEffect{}}
                              : hir::EffectTiming{hir::ImmediateEffect{}};
  }
  if (!as.isNonBlocking()) {
    throw InternalError(
        "LowerAssignTiming: a blocking assignment carrying an "
        "intra-assignment timing control reached expression lowering "
        "unexpanded");
  }
  auto control = LowerDelayOrEventControl(proc, frame, *as.timingControl, span);
  if (!control) return std::unexpected(std::move(control.error()));
  return hir::EffectTiming{
      hir::NonBlockingEffect{.control = *std::move(control)}};
}

}  // namespace

auto LowerAssignmentExprProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::AssignmentExpression& as, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto& unit_lowerer = proc.Owner();
  auto validate = ValidateAssignableImpl(unit_lowerer, true, as.left());
  if (!validate) return std::unexpected(std::move(validate.error()));

  // A name a modport offers stands for an expression the interface evaluates
  // (LRM 25.5.4), so assigning to it is the interface carrying that assignment
  // out. Reaching it needs the value first, which is what the call takes.
  if (const auto* offered = NameOfferedByModport(as.left())) {
    if (as.op.has_value() || as.isNonBlocking()) {
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedExpressionForm,
          "a compound or nonblocking assignment to a name a view offers is "
          "not yet supported");
    }
    auto rhs_or = proc.LowerExpr(as.right(), frame);
    if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
    return LowerModportPortWrite(
        unit_lowerer, frame, *offered, frame.Exprs().Add(*std::move(rhs_or)),
        span);
  }

  auto lhs_or = proc.LowerExpr(as.left(), frame);
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
  const hir::ExprId lhs_id = frame.Exprs().Add(*std::move(lhs_or));

  auto type_id = unit_lowerer.InternType(*as.type, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));

  auto timing_or = LowerAssignTiming(proc, frame, as, span);
  if (!timing_or) return std::unexpected(std::move(timing_or.error()));
  const hir::EffectTiming timing = *std::move(timing_or);

  if (!as.op.has_value()) {
    auto rhs_or = proc.LowerExpr(as.right(), frame);
    if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
    const hir::ExprId rhs_id = frame.Exprs().Add(*std::move(rhs_or));
    return hir::Expr{
        .type = *type_id,
        .data =
            hir::AssignExpr{
                .timing = timing,
                .lhs = lhs_id,
                .compound_op = std::nullopt,
                .rhs = rhs_id},
        .span = span,
    };
  }

  if (as.isNonBlocking()) {
    throw InternalError(
        "LowerAssignmentExprProc: compound assignment with non-blocking "
        "operator is not a legal SV form (LRM A.6.2 grammar)");
  }

  const auto& bare_user_rhs = BareCompoundUserRhs(as.right());
  auto rhs_or = proc.LowerExpr(bare_user_rhs, frame);
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  hir::Expr rhs_expr = *std::move(rhs_or);
  if (rhs_expr.type.value != type_id->value) {
    const hir::ExprId inner_id = frame.Exprs().Add(std::move(rhs_expr));
    rhs_expr = hir::Expr{
        .type = *type_id,
        .data =
            hir::ConversionExpr{
                .kind = hir::ConversionKind::kImplicit, .operand = inner_id},
        .span = span,
    };
  }
  const hir::ExprId rhs_id = frame.Exprs().Add(std::move(rhs_expr));
  return hir::Expr{
      .type = *type_id,
      .data =
          hir::AssignExpr{
              .timing = timing,
              .lhs = lhs_id,
              .compound_op = LowerBinaryOp(*as.op),
              .rhs = rhs_id},
      .span = span,
  };
}

auto LowerIncDecExprProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::UnaryExpression& un, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto& unit_lowerer = proc.Owner();
  auto validate = ValidateAssignableImpl(unit_lowerer, true, un.operand());
  if (!validate) return std::unexpected(std::move(validate.error()));

  auto target_or = proc.LowerExpr(un.operand(), frame);
  if (!target_or) return std::unexpected(std::move(target_or.error()));
  const hir::ExprId target_id = frame.Exprs().Add(*std::move(target_or));

  auto type_id = unit_lowerer.InternType(*un.type, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));

  return hir::Expr{
      .type = *type_id,
      .data =
          hir::IncDecExpr{.op = LowerSlangIncDecOp(un.op), .target = target_id},
      .span = span,
  };
}

auto ValidateAssignableImpl(
    UnitLowerer& unit_lowerer, bool procedural_context,
    const slang::ast::Expression& expr) -> diag::Result<void> {
  using EK = slang::ast::ExpressionKind;
  const auto& mapper = unit_lowerer.SourceMapper();
  const auto span = mapper.SpanOf(expr.sourceRange);
  auto reject = [&](std::string_view why) -> diag::Result<void> {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedAssignmentTarget, std::string{why});
  };

  switch (expr.kind) {
    case EK::NamedValue: {
      const auto& nv = expr.as<slang::ast::NamedValueExpression>();
      const auto& sym = nv.symbol;
      // A class property named without a handle (LRM 8.4) is written through
      // the enclosing method's receiver; it appears only in a method body, so
      // it is always a legal procedural assignment target.
      if (sym.kind == slang::ast::SymbolKind::ClassProperty) {
        return {};
      }
      // `this` (LRM 8.11) reaches this walk only as the base a member write
      // qualifies, never as the target: the front end refuses an assignment to
      // the handle itself, and what decides a member write is the member.
      if (NamesCurrentInstance(nv)) {
        return {};
      }
      if (sym.kind == slang::ast::SymbolKind::Net) {
        // A net is driven only by a continuous assignment (LRM 6.5); a
        // procedural write to a net is illegal.
        if (procedural_context) {
          return reject("a net is not a legal procedural assignment target");
        }
        if (!unit_lowerer
                 .LookupStructuralDataObjectBinding(
                     sym.as<slang::ast::ValueSymbol>())
                 .has_value()) {
          return reject(
              "continuous-assignment target must be a structural signal");
        }
        return {};
      }
      if (sym.kind != slang::ast::SymbolKind::Variable &&
          sym.kind != slang::ast::SymbolKind::FormalArgument) {
        return reject("assignment target must be a variable reference");
      }
      const auto& var = sym.as<slang::ast::VariableSymbol>();
      if (!procedural_context) {
        // LRM 10.3: continuous-assign target must resolve to a structural var.
        if (!unit_lowerer.LookupStructuralDataObjectBinding(var).has_value()) {
          return reject(
              "continuous-assignment target must be a structural variable");
        }
      }
      return {};
    }
    case EK::ElementSelect:
      return ValidateAssignableImpl(
          unit_lowerer, procedural_context,
          expr.as<slang::ast::ElementSelectExpression>().value());
    case EK::RangeSelect:
      return ValidateAssignableImpl(
          unit_lowerer, procedural_context,
          expr.as<slang::ast::RangeSelectExpression>().value());
    case EK::MemberAccess: {
      const auto& ma = expr.as<slang::ast::MemberAccessExpression>();
      if (ma.member.kind != slang::ast::SymbolKind::Field &&
          ma.member.kind != slang::ast::SymbolKind::ClassProperty) {
        return reject(
            "member access target is not a struct field or class "
            "property");
      }
      return ValidateAssignableImpl(
          unit_lowerer, procedural_context, ma.value());
    }
    case EK::HierarchicalValue: {
      const auto& hv = expr.as<slang::ast::HierarchicalValueExpression>();
      // A name a modport offers belongs to that view rather than to the
      // interface's declarations (LRM 25.5.4), so there is no declaration here
      // to check a write against; which storage it reaches is settled where
      // the reference lowers, and the front end has already refused a write
      // the view's direction does not permit.
      const bool offered_by_a_view =
          hv.ref.isViaIfacePort() &&
          hv.symbol.kind == slang::ast::SymbolKind::ModportPort;
      if (!offered_by_a_view) {
        auto declaration = ResolveNamedDeclaration(hv.symbol, span);
        if (!declaration) {
          return std::unexpected(std::move(declaration.error()));
        }
        if ((*declaration)->kind != slang::ast::SymbolKind::Variable) {
          return reject("assignment target must be a variable reference");
        }
      }
      if (!procedural_context) {
        return reject(
            "continuous-assignment target must be a structural variable");
      }
      return {};
    }
    case EK::Concatenation: {
      const auto& cc = expr.as<slang::ast::ConcatenationExpression>();
      for (const auto* op : cc.operands()) {
        if (op->kind == EK::Replication) {
          const auto op_span = mapper.SpanOf(op->sourceRange);
          return diag::Fail(
              op_span, diag::DiagCode::kUnsupportedAssignmentTarget,
              "replication is not allowed inside a destructuring "
              "assignment target (LRM 11.4.12.1)");
        }
        auto sub =
            ValidateAssignableImpl(unit_lowerer, procedural_context, *op);
        if (!sub) return sub;
      }
      return {};
    }
    default:
      return reject("assignment target is not supported yet");
  }
}

}  // namespace lyra::lowering::ast_to_hir
