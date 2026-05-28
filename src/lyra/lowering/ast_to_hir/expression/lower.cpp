#include "lyra/lowering/ast_to_hir/expression/lower.hpp"

#include <expected>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/SemanticFacts.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/SystemSubroutine.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/CallExpression.h>
#include <slang/ast/expressions/ConversionExpression.h>
#include <slang/ast/expressions/LiteralExpressions.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/expressions/OperatorExpressions.h>
#include <slang/ast/expressions/SelectExpressions.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>
#include <slang/numeric/ConstantValue.h>
#include <slang/numeric/SVInt.h>
#include <slang/numeric/Time.h>
#include <slang/syntax/AllSyntax.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/conversion.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/unary_op.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/ast_to_hir/facts.hpp"
#include "lyra/lowering/ast_to_hir/integral_constant.hpp"
#include "lyra/lowering/ast_to_hir/state.hpp"
#include "lyra/lowering/ast_to_hir/type.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

auto LowerSlangLiteralBase(const slang::syntax::SyntaxNode* syntax)
    -> hir::IntegerLiteralBase {
  if (syntax != nullptr &&
      syntax->kind == slang::syntax::SyntaxKind::IntegerVectorExpression) {
    const auto& iv = syntax->as<slang::syntax::IntegerVectorExpressionSyntax>();
    switch (iv.base.numericFlags().base()) {
      case slang::LiteralBase::Binary:
        return hir::IntegerLiteralBase::kBinary;
      case slang::LiteralBase::Octal:
        return hir::IntegerLiteralBase::kOctal;
      case slang::LiteralBase::Decimal:
        return hir::IntegerLiteralBase::kDecimal;
      case slang::LiteralBase::Hex:
        return hir::IntegerLiteralBase::kHexadecimal;
    }
  }
  return hir::IntegerLiteralBase::kDecimal;
}

auto LowerConversionKind(slang::ast::ConversionKind k) -> hir::ConversionKind {
  switch (k) {
    case slang::ast::ConversionKind::Implicit:
      return hir::ConversionKind::kImplicit;
    case slang::ast::ConversionKind::Propagated:
      return hir::ConversionKind::kPropagated;
    case slang::ast::ConversionKind::StreamingConcat:
      return hir::ConversionKind::kStreamingConcat;
    case slang::ast::ConversionKind::Explicit:
      return hir::ConversionKind::kExplicit;
    case slang::ast::ConversionKind::BitstreamCast:
      return hir::ConversionKind::kBitstreamCast;
  }
  throw InternalError("LowerConversionKind: unknown slang ConversionKind");
}

auto LowerBinaryOp(slang::ast::BinaryOperator op) -> hir::BinaryOp {
  switch (op) {
    case slang::ast::BinaryOperator::Add:
      return hir::BinaryOp::kAdd;
    case slang::ast::BinaryOperator::Subtract:
      return hir::BinaryOp::kSub;
    case slang::ast::BinaryOperator::Multiply:
      return hir::BinaryOp::kMul;
    case slang::ast::BinaryOperator::Divide:
      return hir::BinaryOp::kDiv;
    case slang::ast::BinaryOperator::Mod:
      return hir::BinaryOp::kMod;
    case slang::ast::BinaryOperator::Power:
      return hir::BinaryOp::kPower;
    case slang::ast::BinaryOperator::BinaryAnd:
      return hir::BinaryOp::kBitwiseAnd;
    case slang::ast::BinaryOperator::BinaryOr:
      return hir::BinaryOp::kBitwiseOr;
    case slang::ast::BinaryOperator::BinaryXor:
      return hir::BinaryOp::kBitwiseXor;
    case slang::ast::BinaryOperator::BinaryXnor:
      return hir::BinaryOp::kBitwiseXnor;
    case slang::ast::BinaryOperator::Equality:
      return hir::BinaryOp::kEquality;
    case slang::ast::BinaryOperator::Inequality:
      return hir::BinaryOp::kInequality;
    case slang::ast::BinaryOperator::CaseEquality:
      return hir::BinaryOp::kCaseEquality;
    case slang::ast::BinaryOperator::CaseInequality:
      return hir::BinaryOp::kCaseInequality;
    case slang::ast::BinaryOperator::WildcardEquality:
      return hir::BinaryOp::kWildcardEquality;
    case slang::ast::BinaryOperator::WildcardInequality:
      return hir::BinaryOp::kWildcardInequality;
    case slang::ast::BinaryOperator::GreaterThanEqual:
      return hir::BinaryOp::kGreaterEqual;
    case slang::ast::BinaryOperator::GreaterThan:
      return hir::BinaryOp::kGreaterThan;
    case slang::ast::BinaryOperator::LessThanEqual:
      return hir::BinaryOp::kLessEqual;
    case slang::ast::BinaryOperator::LessThan:
      return hir::BinaryOp::kLessThan;
    case slang::ast::BinaryOperator::LogicalAnd:
      return hir::BinaryOp::kLogicalAnd;
    case slang::ast::BinaryOperator::LogicalOr:
      return hir::BinaryOp::kLogicalOr;
    case slang::ast::BinaryOperator::LogicalImplication:
      return hir::BinaryOp::kLogicalImplication;
    case slang::ast::BinaryOperator::LogicalEquivalence:
      return hir::BinaryOp::kLogicalEquivalence;
    case slang::ast::BinaryOperator::LogicalShiftLeft:
      return hir::BinaryOp::kLogicalShiftLeft;
    case slang::ast::BinaryOperator::LogicalShiftRight:
      return hir::BinaryOp::kLogicalShiftRight;
    case slang::ast::BinaryOperator::ArithmeticShiftLeft:
      return hir::BinaryOp::kArithmeticShiftLeft;
    case slang::ast::BinaryOperator::ArithmeticShiftRight:
      return hir::BinaryOp::kArithmeticShiftRight;
  }
  throw InternalError("LowerBinaryOp: unknown slang BinaryOperator");
}

auto LowerUnaryOp(slang::ast::UnaryOperator op, diag::SourceSpan span)
    -> diag::Result<hir::UnaryOp> {
  switch (op) {
    case slang::ast::UnaryOperator::Plus:
      return hir::UnaryOp::kPlus;
    case slang::ast::UnaryOperator::Minus:
      return hir::UnaryOp::kMinus;
    case slang::ast::UnaryOperator::BitwiseNot:
      return hir::UnaryOp::kBitwiseNot;
    case slang::ast::UnaryOperator::LogicalNot:
      return hir::UnaryOp::kLogicalNot;
    case slang::ast::UnaryOperator::BitwiseAnd:
      return hir::UnaryOp::kReductionAnd;
    case slang::ast::UnaryOperator::BitwiseOr:
      return hir::UnaryOp::kReductionOr;
    case slang::ast::UnaryOperator::BitwiseXor:
      return hir::UnaryOp::kReductionXor;
    case slang::ast::UnaryOperator::BitwiseNand:
      return hir::UnaryOp::kReductionNand;
    case slang::ast::UnaryOperator::BitwiseNor:
      return hir::UnaryOp::kReductionNor;
    case slang::ast::UnaryOperator::BitwiseXnor:
      return hir::UnaryOp::kReductionXnor;
    case slang::ast::UnaryOperator::Preincrement:
    case slang::ast::UnaryOperator::Predecrement:
    case slang::ast::UnaryOperator::Postincrement:
    case slang::ast::UnaryOperator::Postdecrement:
      return diag::Unsupported(
          span, diag::DiagCode::kUnsupportedExpressionForm,
          "increment and decrement expressions are not supported yet",
          diag::UnsupportedCategory::kOperation);
  }
  throw InternalError("LowerUnaryOp: unknown slang UnaryOperator");
}

auto MakeIntegerLiteralExpr(
    const slang::ast::IntegerLiteral& lit, hir::TypeId type,
    diag::SourceSpan span) -> hir::Expr {
  return hir::Expr{
      .type = type,
      .data =
          hir::PrimaryExpr{
              .data =
                  hir::IntegerLiteral{
                      .value = LowerSVIntToIntegralConstant(lit.getValue()),
                      .base = LowerSlangLiteralBase(lit.syntax),
                      .declared_unsized = lit.isDeclaredUnsized,
                  }},
      .span = span,
  };
}

auto MakeUnbasedUnsizedLiteralExpr(
    const slang::ast::UnbasedUnsizedIntegerLiteral& lit, hir::TypeId type,
    diag::SourceSpan span) -> hir::Expr {
  return hir::Expr{
      .type = type,
      .data =
          hir::PrimaryExpr{
              .data =
                  hir::IntegerLiteral{
                      .value = LowerSVIntToIntegralConstant(lit.getValue()),
                      .base = hir::IntegerLiteralBase::kUnbased,
                      .declared_unsized = true,
                  }},
      .span = span,
  };
}

auto MakeEnumValueExpr(
    const slang::ast::EnumValueSymbol& sym, hir::TypeId type,
    diag::SourceSpan span) -> hir::Expr {
  const auto& cv = sym.getValue();
  if (!cv.isInteger()) {
    throw InternalError("MakeEnumValueExpr: enum value is not integral");
  }
  return hir::Expr{
      .type = type,
      .data =
          hir::PrimaryExpr{
              .data =
                  hir::IntegerLiteral{
                      .value = LowerSVIntToIntegralConstant(cv.integer()),
                      .base = hir::IntegerLiteralBase::kDecimal,
                      .declared_unsized = false,
                  }},
      .span = span,
  };
}

auto MakeStringLiteralExpr(
    std::string text, hir::TypeId type, diag::SourceSpan span) -> hir::Expr {
  return hir::Expr{
      .type = type,
      .data =
          hir::PrimaryExpr{
              .data = hir::StringLiteral{.value = std::move(text)}},
      .span = span,
  };
}

auto LowerEnumMethodName(std::string_view name)
    -> std::optional<hir::BuiltinMethodKind> {
  if (name == "first") return hir::BuiltinMethodKind::kEnumFirst;
  if (name == "last") return hir::BuiltinMethodKind::kEnumLast;
  if (name == "num") return hir::BuiltinMethodKind::kEnumNum;
  if (name == "next") return hir::BuiltinMethodKind::kEnumNext;
  if (name == "prev") return hir::BuiltinMethodKind::kEnumPrev;
  if (name == "name") return hir::BuiltinMethodKind::kEnumName;
  return std::nullopt;
}

auto LowerTimeUnit(slang::TimeUnit u) -> hir::TimeScale {
  switch (u) {
    case slang::TimeUnit::Femtoseconds:
      return hir::TimeScale::kFs;
    case slang::TimeUnit::Picoseconds:
      return hir::TimeScale::kPs;
    case slang::TimeUnit::Nanoseconds:
      return hir::TimeScale::kNs;
    case slang::TimeUnit::Microseconds:
      return hir::TimeScale::kUs;
    case slang::TimeUnit::Milliseconds:
      return hir::TimeScale::kMs;
    case slang::TimeUnit::Seconds:
      return hir::TimeScale::kS;
  }
  throw InternalError("LowerTimeUnit: unknown slang TimeUnit");
}

auto MakeTimeLiteralExpr(
    double value, hir::TimeScale scale, hir::TypeId type, diag::SourceSpan span)
    -> hir::Expr {
  return hir::Expr{
      .type = type,
      .data =
          hir::PrimaryExpr{
              .data = hir::TimeLiteral{.value = value, .scale = scale}},
      .span = span,
  };
}

auto MakeRealLiteralExpr(double value, hir::TypeId type, diag::SourceSpan span)
    -> hir::Expr {
  return hir::Expr{
      .type = type,
      .data = hir::PrimaryExpr{.data = hir::RealLiteral{.value = value}},
      .span = span,
  };
}

auto MakeRefExpr(hir::Primary ref, hir::TypeId type, diag::SourceSpan span)
    -> hir::Expr {
  return hir::Expr{
      .type = type,
      .data = hir::PrimaryExpr{.data = std::move(ref)},
      .span = span,
  };
}

// Project an Lvalue (write context) into a Primary (read context). Lvalue and
// Primary share the underlying ref types verbatim, so the projection is just
// "lift the variant alternative". No data conversion.
auto LvalueToPrimary(const hir::Lvalue& lvalue) -> hir::Primary {
  return std::visit(
      [](const auto& ref) -> hir::Primary { return ref; }, lvalue);
}

auto FromSlangSubroutineKind(slang::ast::SubroutineKind k)
    -> support::SystemSubroutineKind {
  switch (k) {
    case slang::ast::SubroutineKind::Function:
      return support::SystemSubroutineKind::kFunction;
    case slang::ast::SubroutineKind::Task:
      return support::SystemSubroutineKind::kTask;
  }
  throw InternalError("FromSlangSubroutineKind: unknown SubroutineKind");
}

auto MakeReturnConventionType(
    UnitLoweringState& unit_state, support::ReturnConvention conv)
    -> hir::TypeId {
  switch (conv) {
    case support::ReturnConvention::kVoid:
      return unit_state.VoidTypeId();
  }
  throw InternalError("MakeReturnConventionType: unknown ReturnConvention");
}

auto TypeIdOfSlangExpr(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    const slang::ast::Expression& e) -> diag::Result<hir::TypeId> {
  const auto& mapper = unit_facts.SourceMapper();
  return unit_state.GetTypeId(*e.type, mapper.SpanOf(e.sourceRange));
}

auto LowerNamedValueProc(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    const ProcessLoweringState& proc_state, const ScopeStack& stack,
    const slang::ast::NamedValueExpression& named) -> diag::Result<hir::Expr> {
  const auto& mapper = unit_facts.SourceMapper();
  const auto span = mapper.SpanOf(named.sourceRange);
  const auto& sym = named.symbol;

  if (sym.kind == slang::ast::SymbolKind::EnumValue) {
    auto type_id = TypeIdOfSlangExpr(unit_facts, unit_state, named);
    if (!type_id) return std::unexpected(std::move(type_id.error()));
    return MakeEnumValueExpr(
        sym.as<slang::ast::EnumValueSymbol>(), *type_id, span);
  }

  if (sym.kind == slang::ast::SymbolKind::Variable ||
      sym.kind == slang::ast::SymbolKind::Parameter) {
    const auto& value_sym = sym.as<slang::ast::ValueSymbol>();
    if (auto loop_binding = unit_state.LookupLoopVarBinding(value_sym)) {
      const auto hops = stack.HopsTo(loop_binding->home_frame);
      if (!hops.has_value()) {
        throw InternalError(
            "LowerProcExpr: loop-var binding home frame is not on the current "
            "scope stack");
      }
      return MakeRefExpr(
          hir::LoopVarRef{.hops = *hops, .loop_var = loop_binding->loop_var_id},
          loop_binding->type, span);
    }
  }

  if (sym.kind != slang::ast::SymbolKind::Variable) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedNonVariableNamedReference,
        "reference to non-variable declaration is not supported",
        diag::UnsupportedCategory::kFeature);
  }
  const auto& var = sym.as<slang::ast::VariableSymbol>();

  if (auto local = proc_state.LookupProceduralVar(var)) {
    const hir::TypeId type_id = proc_state.GetProceduralVarType(*local);
    return MakeRefExpr(hir::ProceduralVarRef{.var = *local}, type_id, span);
  }

  const auto binding = unit_state.LookupStructuralVarBinding(var);
  if (!binding.has_value()) {
    throw InternalError(
        "LowerProcExpr: variable was not bound during scope lowering");
  }
  const auto hops = stack.HopsTo(binding->home_frame);
  if (!hops.has_value()) {
    throw InternalError(
        "LowerProcExpr: variable home frame is not on the current scope stack");
  }
  return MakeRefExpr(
      hir::StructuralVarRef{.hops = *hops, .var = binding->var_id},
      binding->type, span);
}

auto LowerNamedValueStructural(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    const ScopeStack& stack, const slang::ast::NamedValueExpression& named)
    -> diag::Result<hir::Expr> {
  const auto& mapper = unit_facts.SourceMapper();
  const auto span = mapper.SpanOf(named.sourceRange);
  const auto& sym = named.symbol;
  if (sym.kind == slang::ast::SymbolKind::EnumValue) {
    auto type_id = TypeIdOfSlangExpr(unit_facts, unit_state, named);
    if (!type_id) return std::unexpected(std::move(type_id.error()));
    return MakeEnumValueExpr(
        sym.as<slang::ast::EnumValueSymbol>(), *type_id, span);
  }
  if (sym.kind == slang::ast::SymbolKind::Variable ||
      sym.kind == slang::ast::SymbolKind::Parameter) {
    const auto& value_sym = sym.as<slang::ast::ValueSymbol>();
    if (auto loop_binding = unit_state.LookupLoopVarBinding(value_sym)) {
      const auto hops = stack.HopsTo(loop_binding->home_frame);
      if (!hops.has_value()) {
        throw InternalError(
            "LowerStructuralExpr: loop-var binding home frame is not on "
            "the current scope stack");
      }
      return MakeRefExpr(
          hir::LoopVarRef{.hops = *hops, .loop_var = loop_binding->loop_var_id},
          loop_binding->type, span);
    }
  }
  if (sym.kind != slang::ast::SymbolKind::Variable) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedNonVariableNamedReference,
        "reference to non-variable declaration is not supported",
        diag::UnsupportedCategory::kFeature);
  }
  const auto& var = sym.as<slang::ast::VariableSymbol>();
  const auto binding = unit_state.LookupStructuralVarBinding(var);
  if (!binding.has_value()) {
    throw InternalError(
        "LowerStructuralExpr: variable was not bound during scope lowering");
  }
  const auto hops = stack.HopsTo(binding->home_frame);
  if (!hops.has_value()) {
    throw InternalError(
        "LowerStructuralExpr: variable home frame is not on the current "
        "scope stack");
  }
  return MakeRefExpr(
      hir::StructuralVarRef{.hops = *hops, .var = binding->var_id},
      binding->type, span);
}

auto LowerLValueReferenceExpr(
    diag::SourceSpan span,
    const std::optional<hir::Lvalue>& compound_lvalue_context,
    diag::Result<hir::TypeId> type_id, const char* origin)
    -> diag::Result<hir::Expr> {
  if (!compound_lvalue_context.has_value()) {
    throw InternalError(
        std::string{origin} +
        ": slang LValueReference encountered outside compound-assignment RHS");
  }
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return MakeRefExpr(LvalueToPrimary(*compound_lvalue_context), *type_id, span);
}

auto LowerConversionExprProc(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ProcessLoweringState& proc_state, const ScopeStack& stack,
    const std::optional<hir::Lvalue>& compound_lvalue_context,
    const slang::ast::ConversionExpression& conv,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto operand_or = LowerProcExpr(
      unit_facts, unit_state, proc_state, stack, conv.operand(),
      compound_lvalue_context);
  if (!operand_or) return std::unexpected(std::move(operand_or.error()));
  const hir::ExprId operand_id = proc_state.AddExpr(*std::move(operand_or));
  auto type_id = TypeIdOfSlangExpr(unit_facts, unit_state, expr);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return hir::Expr{
      .type = *type_id,
      .data =
          hir::ConversionExpr{
              .operand = operand_id,
              .kind = LowerConversionKind(conv.conversionKind),
          },
      .span = span,
  };
}

auto LowerUnaryExprProc(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ProcessLoweringState& proc_state, const ScopeStack& stack,
    const std::optional<hir::Lvalue>& compound_lvalue_context,
    const slang::ast::UnaryExpression& un, const slang::ast::Expression& expr,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  auto op_or = LowerUnaryOp(un.op, span);
  if (!op_or) return std::unexpected(std::move(op_or.error()));
  auto operand_or = LowerProcExpr(
      unit_facts, unit_state, proc_state, stack, un.operand(),
      compound_lvalue_context);
  if (!operand_or) return std::unexpected(std::move(operand_or.error()));
  const hir::ExprId operand_id = proc_state.AddExpr(*std::move(operand_or));
  auto type_id = TypeIdOfSlangExpr(unit_facts, unit_state, expr);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return hir::Expr{
      .type = *type_id,
      .data = hir::UnaryExpr{.op = *op_or, .operand = operand_id},
      .span = span,
  };
}

auto LowerBinaryExprProc(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ProcessLoweringState& proc_state, const ScopeStack& stack,
    const std::optional<hir::Lvalue>& compound_lvalue_context,
    const slang::ast::BinaryExpression& bin, const slang::ast::Expression& expr,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  auto lhs_or = LowerProcExpr(
      unit_facts, unit_state, proc_state, stack, bin.left(),
      compound_lvalue_context);
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
  const hir::ExprId lhs_id = proc_state.AddExpr(*std::move(lhs_or));
  auto rhs_or = LowerProcExpr(
      unit_facts, unit_state, proc_state, stack, bin.right(),
      compound_lvalue_context);
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  const hir::ExprId rhs_id = proc_state.AddExpr(*std::move(rhs_or));
  auto type_id = TypeIdOfSlangExpr(unit_facts, unit_state, expr);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return hir::Expr{
      .type = *type_id,
      .data =
          hir::BinaryExpr{
              .op = LowerBinaryOp(bin.op),
              .lhs = lhs_id,
              .rhs = rhs_id,
          },
      .span = span,
  };
}

auto LowerConditionalExprProc(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ProcessLoweringState& proc_state, const ScopeStack& stack,
    const std::optional<hir::Lvalue>& compound_lvalue_context,
    const slang::ast::ConditionalExpression& cond,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  if (cond.conditions.size() != 1) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "conditional operator with `&&&` multi-condition is not yet "
        "supported",
        diag::UnsupportedCategory::kFeature);
  }
  if (cond.conditions[0].pattern != nullptr) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "conditional operator with `matches` pattern is not yet supported",
        diag::UnsupportedCategory::kFeature);
  }
  auto cond_or = LowerProcExpr(
      unit_facts, unit_state, proc_state, stack, *cond.conditions[0].expr,
      compound_lvalue_context);
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  const hir::ExprId cond_id = proc_state.AddExpr(*std::move(cond_or));
  auto then_or = LowerProcExpr(
      unit_facts, unit_state, proc_state, stack, cond.left(),
      compound_lvalue_context);
  if (!then_or) return std::unexpected(std::move(then_or.error()));
  const hir::ExprId then_id = proc_state.AddExpr(*std::move(then_or));
  auto else_or = LowerProcExpr(
      unit_facts, unit_state, proc_state, stack, cond.right(),
      compound_lvalue_context);
  if (!else_or) return std::unexpected(std::move(else_or.error()));
  const hir::ExprId else_id = proc_state.AddExpr(*std::move(else_or));
  auto type_id = TypeIdOfSlangExpr(unit_facts, unit_state, expr);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return hir::Expr{
      .type = *type_id,
      .data =
          hir::ConditionalExpr{
              .condition = cond_id,
              .then_value = then_id,
              .else_value = else_id,
          },
      .span = span,
  };
}

auto LowerCallExprProc(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ProcessLoweringState& proc_state, const ScopeStack& stack,
    const std::optional<hir::Lvalue>& compound_lvalue_context,
    const slang::ast::CallExpression& call, const slang::ast::Expression& expr,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  std::vector<hir::ExprId> arg_ids;
  arg_ids.reserve(call.arguments().size());
  std::optional<hir::TypeId> receiver_type;
  for (std::size_t i = 0; i < call.arguments().size(); ++i) {
    auto arg_or = LowerProcExpr(
        unit_facts, unit_state, proc_state, stack, *call.arguments()[i],
        compound_lvalue_context);
    if (!arg_or) return std::unexpected(std::move(arg_or.error()));
    if (i == 0) {
      receiver_type = arg_or->type;
    }
    arg_ids.push_back(proc_state.AddExpr(*std::move(arg_or)));
  }

  if (call.isSystemCall()) {
    const auto& info =
        std::get<slang::ast::CallExpression::SystemCallInfo>(call.subroutine);
    const std::string_view name = info.subroutine->name;

    if (receiver_type.has_value() &&
        unit_state.GetType(*receiver_type).IsEnum()) {
      if (auto kind = LowerEnumMethodName(name); kind.has_value()) {
        // `next` / `prev` have an optional `int unsigned step = 1` (LRM
        // 6.19.5.3/4). When the user omits the step, the lowering hands the
        // backend a single-argument call and lets the backend's intrinsic
        // mechanism supply the default (C++ default-argument; LLVM constant
        // 1; etc.). No synthesized literal is injected at the HIR boundary.
        auto type_id = TypeIdOfSlangExpr(unit_facts, unit_state, expr);
        if (!type_id) return std::unexpected(std::move(type_id.error()));
        return hir::Expr{
            .type = *type_id,
            .data =
                hir::CallExpr{
                    .callee = hir::BuiltinMethodRef{.kind = *kind},
                    .arguments = std::move(arg_ids),
                },
            .span = span,
        };
      }
    }

    if (receiver_type.has_value() &&
        std::holds_alternative<hir::EventType>(
            unit_state.GetType(*receiver_type).data) &&
        name == "triggered") {
      // LRM 15.5.3: `e.triggered` returns true for the duration of the time
      // slot in which the event was last triggered. Result type is bit (1'b0
      // / 1'b1) -- slang already typed the expression; we just route the
      // call through the named-event method.
      auto type_id = TypeIdOfSlangExpr(unit_facts, unit_state, expr);
      if (!type_id) return std::unexpected(std::move(type_id.error()));
      return hir::Expr{
          .type = *type_id,
          .data =
              hir::CallExpr{
                  .callee =
                      hir::BuiltinMethodRef{
                          .kind = hir::BuiltinMethodKind::kNamedEventTriggered,
                      },
                  .arguments = std::move(arg_ids),
              },
          .span = span,
      };
    }

    const auto* desc = support::FindSystemSubroutine(name);
    if (desc == nullptr) {
      throw InternalError(
          std::string{"AST->HIR call: unresolved system subroutine '"} +
          std::string{name} + "' after slang resolution");
    }
    const auto frontend_kind = FromSlangSubroutineKind(info.subroutine->kind);
    if (desc->kind != frontend_kind) {
      throw InternalError(
          std::string{"AST->HIR call: registry/frontend kind mismatch for '"} +
          std::string{name} + "'");
    }
    if (!desc->arg_policy.Accepts(arg_ids.size())) {
      throw InternalError(
          std::string{
              "AST->HIR call: arg count outside descriptor policy for '"} +
          std::string{name} + "'");
    }

    const auto result_type =
        MakeReturnConventionType(unit_state, desc->result_conv);
    return hir::Expr{
        .type = result_type,
        .data =
            hir::CallExpr{
                .callee = hir::SystemSubroutineRef{.id = desc->id},
                .arguments = std::move(arg_ids),
            },
        .span = span,
    };
  }

  const auto* sym =
      std::get<const slang::ast::SubroutineSymbol*>(call.subroutine);
  if (sym == nullptr) {
    throw InternalError(
        "AST->HIR call: user call missing resolved SubroutineSymbol");
  }
  const auto binding = unit_state.LookupSubroutineBinding(*sym);
  if (!binding.has_value()) {
    throw InternalError(
        "AST->HIR call: resolved user subroutine has no registered HIR "
        "binding");
  }
  const auto hops = stack.HopsTo(binding->owner_frame);
  if (!hops.has_value()) {
    throw InternalError(
        "AST->HIR call: user subroutine owner frame is not on the current "
        "scope stack");
  }
  auto result_type = TypeIdOfSlangExpr(unit_facts, unit_state, expr);
  if (!result_type) return std::unexpected(std::move(result_type.error()));
  return hir::Expr{
      .type = *result_type,
      .data =
          hir::CallExpr{
              .callee =
                  hir::StructuralSubroutineRef{
                      .hops = *hops, .subroutine = binding->subroutine_id},
              .arguments = std::move(arg_ids),
          },
      .span = span,
  };
}

auto LowerAssignmentExprProc(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ProcessLoweringState& proc_state, const ScopeStack& stack,
    const slang::ast::AssignmentExpression& as,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  const auto kind = as.isNonBlocking() ? hir::AssignKind::kNonBlocking
                                       : hir::AssignKind::kBlocking;

  auto lhs_or =
      LowerProcLvalue(unit_facts, unit_state, proc_state, stack, as.left());
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));

  auto rhs_or = LowerProcExpr(
      unit_facts, unit_state, proc_state, stack, as.right(), *lhs_or);
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  const hir::ExprId rhs_id = proc_state.AddExpr(*std::move(rhs_or));

  auto type_id = TypeIdOfSlangExpr(unit_facts, unit_state, expr);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return hir::Expr{
      .type = *type_id,
      .data =
          hir::AssignExpr{
              .kind = kind, .lhs = *std::move(lhs_or), .rhs = rhs_id},
      .span = span,
  };
}

auto LowerInsideExprProc(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ProcessLoweringState& proc_state, const ScopeStack& stack,
    const std::optional<hir::Lvalue>& compound_lvalue_context,
    const slang::ast::InsideExpression& in, const slang::ast::Expression& expr,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  const auto& mapper = unit_facts.SourceMapper();

  auto lhs_or = LowerProcExpr(
      unit_facts, unit_state, proc_state, stack, in.left(),
      compound_lvalue_context);
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
  const hir::ExprId lhs_id = proc_state.AddExpr(*std::move(lhs_or));

  std::vector<hir::InsideItem> items;
  items.reserve(in.rangeList().size());
  for (const auto* item : in.rangeList()) {
    if (item->kind == slang::ast::ExpressionKind::ValueRange) {
      const auto& vr = item->as<slang::ast::ValueRangeExpression>();
      if (vr.rangeKind != slang::ast::ValueRangeKind::Simple) {
        return diag::Unsupported(
            mapper.SpanOf(vr.sourceRange),
            diag::DiagCode::kUnsupportedExpressionForm,
            "tolerance-range form in inside operator is not yet supported",
            diag::UnsupportedCategory::kFeature);
      }
      auto lo_or = LowerProcExpr(
          unit_facts, unit_state, proc_state, stack, vr.left(),
          compound_lvalue_context);
      if (!lo_or) return std::unexpected(std::move(lo_or.error()));
      const hir::ExprId lo_id = proc_state.AddExpr(*std::move(lo_or));
      auto hi_or = LowerProcExpr(
          unit_facts, unit_state, proc_state, stack, vr.right(),
          compound_lvalue_context);
      if (!hi_or) return std::unexpected(std::move(hi_or.error()));
      const hir::ExprId hi_id = proc_state.AddExpr(*std::move(hi_or));
      items.emplace_back(hir::InsideRangePair{.lo = lo_id, .hi = hi_id});
    } else {
      auto val_or = LowerProcExpr(
          unit_facts, unit_state, proc_state, stack, *item,
          compound_lvalue_context);
      if (!val_or) return std::unexpected(std::move(val_or.error()));
      const hir::ExprId val_id = proc_state.AddExpr(*std::move(val_or));
      items.emplace_back(val_id);
    }
  }

  auto type_id = TypeIdOfSlangExpr(unit_facts, unit_state, expr);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return hir::Expr{
      .type = *type_id,
      .data = hir::InsideExpr{.lhs = lhs_id, .items = std::move(items)},
      .span = span,
  };
}

auto LowerElementSelectExprProc(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ProcessLoweringState& proc_state, const ScopeStack& stack,
    const std::optional<hir::Lvalue>& compound_lvalue_context,
    const slang::ast::ElementSelectExpression& sel,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  if (!sel.value().type->isIntegral() || expr.type->getBitWidth() != 1) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "element-select is only supported on a 1D integral operand with a "
        "1-bit result (bit-select)",
        diag::UnsupportedCategory::kOperation);
  }
  auto base_or = LowerProcExpr(
      unit_facts, unit_state, proc_state, stack, sel.value(),
      compound_lvalue_context);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const hir::ExprId base_id = proc_state.AddExpr(*std::move(base_or));

  auto idx_or = LowerProcExpr(
      unit_facts, unit_state, proc_state, stack, sel.selector(),
      compound_lvalue_context);
  if (!idx_or) return std::unexpected(std::move(idx_or.error()));
  const hir::ExprId idx_id = proc_state.AddExpr(*std::move(idx_or));

  auto type_id = TypeIdOfSlangExpr(unit_facts, unit_state, expr);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return hir::Expr{
      .type = *type_id,
      .data =
          hir::ElementSelectExpr{
              .base_value = base_id,
              .index = idx_id,
          },
      .span = span,
  };
}

auto LowerRangeSelectExprProc(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ProcessLoweringState& proc_state, const ScopeStack& stack,
    const std::optional<hir::Lvalue>& compound_lvalue_context,
    const slang::ast::RangeSelectExpression& sel,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto base_or = LowerProcExpr(
      unit_facts, unit_state, proc_state, stack, sel.value(),
      compound_lvalue_context);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const hir::ExprId base_id = proc_state.AddExpr(*std::move(base_or));

  auto left_or = LowerProcExpr(
      unit_facts, unit_state, proc_state, stack, sel.left(),
      compound_lvalue_context);
  if (!left_or) return std::unexpected(std::move(left_or.error()));
  const hir::ExprId left_id = proc_state.AddExpr(*std::move(left_or));

  auto right_or = LowerProcExpr(
      unit_facts, unit_state, proc_state, stack, sel.right(),
      compound_lvalue_context);
  if (!right_or) return std::unexpected(std::move(right_or.error()));
  const hir::ExprId right_id = proc_state.AddExpr(*std::move(right_or));

  hir::RangeBounds bounds = [&]() -> hir::RangeBounds {
    switch (sel.getSelectionKind()) {
      case slang::ast::RangeSelectionKind::Simple:
        return hir::RangeConstantBounds{
            .msb_expr = left_id, .lsb_expr = right_id};
      case slang::ast::RangeSelectionKind::IndexedUp:
        return hir::RangeIndexedUpBounds{
            .base_index = left_id, .width = right_id};
      case slang::ast::RangeSelectionKind::IndexedDown:
        return hir::RangeIndexedDownBounds{
            .base_index = left_id, .width = right_id};
    }
    throw InternalError(
        "LowerRangeSelectExprProc: unknown slang RangeSelectionKind");
  }();

  auto type_id = TypeIdOfSlangExpr(unit_facts, unit_state, expr);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return hir::Expr{
      .type = *type_id,
      .data =
          hir::RangeSelectExpr{
              .base_value = base_id,
              .bounds = std::move(bounds),
          },
      .span = span,
  };
}

auto LowerConversionExprStructural(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ScopeLoweringState& scope_state, const ScopeStack& stack,
    const std::optional<hir::Lvalue>& compound_lvalue_context,
    const slang::ast::ConversionExpression& conv,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto operand_or = LowerStructuralExpr(
      unit_facts, unit_state, scope_state, stack, conv.operand(),
      compound_lvalue_context);
  if (!operand_or) return std::unexpected(std::move(operand_or.error()));
  const hir::ExprId operand_id = scope_state.AddExpr(*std::move(operand_or));
  auto type_id = TypeIdOfSlangExpr(unit_facts, unit_state, expr);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return hir::Expr{
      .type = *type_id,
      .data =
          hir::ConversionExpr{
              .operand = operand_id,
              .kind = LowerConversionKind(conv.conversionKind),
          },
      .span = span,
  };
}

auto LowerUnaryExprStructural(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ScopeLoweringState& scope_state, const ScopeStack& stack,
    const std::optional<hir::Lvalue>& compound_lvalue_context,
    const slang::ast::UnaryExpression& un, const slang::ast::Expression& expr,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  auto op_or = LowerUnaryOp(un.op, span);
  if (!op_or) return std::unexpected(std::move(op_or.error()));
  auto operand_or = LowerStructuralExpr(
      unit_facts, unit_state, scope_state, stack, un.operand(),
      compound_lvalue_context);
  if (!operand_or) return std::unexpected(std::move(operand_or.error()));
  const hir::ExprId operand_id = scope_state.AddExpr(*std::move(operand_or));
  auto type_id = TypeIdOfSlangExpr(unit_facts, unit_state, expr);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return hir::Expr{
      .type = *type_id,
      .data = hir::UnaryExpr{.op = *op_or, .operand = operand_id},
      .span = span,
  };
}

auto LowerBinaryExprStructural(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ScopeLoweringState& scope_state, const ScopeStack& stack,
    const std::optional<hir::Lvalue>& compound_lvalue_context,
    const slang::ast::BinaryExpression& bin, const slang::ast::Expression& expr,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  auto lhs_or = LowerStructuralExpr(
      unit_facts, unit_state, scope_state, stack, bin.left(),
      compound_lvalue_context);
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
  const hir::ExprId lhs_id = scope_state.AddExpr(*std::move(lhs_or));
  auto rhs_or = LowerStructuralExpr(
      unit_facts, unit_state, scope_state, stack, bin.right(),
      compound_lvalue_context);
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  const hir::ExprId rhs_id = scope_state.AddExpr(*std::move(rhs_or));
  auto type_id = TypeIdOfSlangExpr(unit_facts, unit_state, expr);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return hir::Expr{
      .type = *type_id,
      .data =
          hir::BinaryExpr{
              .op = LowerBinaryOp(bin.op),
              .lhs = lhs_id,
              .rhs = rhs_id,
          },
      .span = span,
  };
}

auto LowerConditionalExprStructural(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ScopeLoweringState& scope_state, const ScopeStack& stack,
    const std::optional<hir::Lvalue>& compound_lvalue_context,
    const slang::ast::ConditionalExpression& cond,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  if (cond.conditions.size() != 1) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedStructuralExpressionForm,
        "conditional operator with `&&&` multi-condition is not yet "
        "supported",
        diag::UnsupportedCategory::kFeature);
  }
  if (cond.conditions[0].pattern != nullptr) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedStructuralExpressionForm,
        "conditional operator with `matches` pattern is not yet supported",
        diag::UnsupportedCategory::kFeature);
  }
  auto cond_or = LowerStructuralExpr(
      unit_facts, unit_state, scope_state, stack, *cond.conditions[0].expr,
      compound_lvalue_context);
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  const hir::ExprId cond_id = scope_state.AddExpr(*std::move(cond_or));
  auto then_or = LowerStructuralExpr(
      unit_facts, unit_state, scope_state, stack, cond.left(),
      compound_lvalue_context);
  if (!then_or) return std::unexpected(std::move(then_or.error()));
  const hir::ExprId then_id = scope_state.AddExpr(*std::move(then_or));
  auto else_or = LowerStructuralExpr(
      unit_facts, unit_state, scope_state, stack, cond.right(),
      compound_lvalue_context);
  if (!else_or) return std::unexpected(std::move(else_or.error()));
  const hir::ExprId else_id = scope_state.AddExpr(*std::move(else_or));
  auto type_id = TypeIdOfSlangExpr(unit_facts, unit_state, expr);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return hir::Expr{
      .type = *type_id,
      .data =
          hir::ConditionalExpr{
              .condition = cond_id,
              .then_value = then_id,
              .else_value = else_id,
          },
      .span = span,
  };
}

}  // namespace

auto LowerProcExpr(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ProcessLoweringState& proc_state, const ScopeStack& stack,
    const slang::ast::Expression& expr,
    std::optional<hir::Lvalue> compound_lvalue_context)
    -> diag::Result<hir::Expr> {
  const auto& mapper = unit_facts.SourceMapper();
  const auto span = mapper.SpanOf(expr.sourceRange);

  switch (expr.kind) {
    case slang::ast::ExpressionKind::IntegerLiteral: {
      auto type_id = TypeIdOfSlangExpr(unit_facts, unit_state, expr);
      if (!type_id) return std::unexpected(std::move(type_id.error()));
      return MakeIntegerLiteralExpr(
          expr.as<slang::ast::IntegerLiteral>(), *type_id, span);
    }

    case slang::ast::ExpressionKind::UnbasedUnsizedIntegerLiteral: {
      auto type_id = TypeIdOfSlangExpr(unit_facts, unit_state, expr);
      if (!type_id) return std::unexpected(std::move(type_id.error()));
      return MakeUnbasedUnsizedLiteralExpr(
          expr.as<slang::ast::UnbasedUnsizedIntegerLiteral>(), *type_id, span);
    }

    case slang::ast::ExpressionKind::StringLiteral: {
      const auto& sl = expr.as<slang::ast::StringLiteral>();
      auto type_id = TypeIdOfSlangExpr(unit_facts, unit_state, expr);
      if (!type_id) return std::unexpected(std::move(type_id.error()));
      return MakeStringLiteralExpr(std::string{sl.getValue()}, *type_id, span);
    }

    case slang::ast::ExpressionKind::TimeLiteral: {
      const auto& tl = expr.as<slang::ast::TimeLiteral>();
      auto type_id = TypeIdOfSlangExpr(unit_facts, unit_state, expr);
      if (!type_id) return std::unexpected(std::move(type_id.error()));
      return MakeTimeLiteralExpr(
          tl.getValue(), LowerTimeUnit(tl.getScale().base.unit), *type_id,
          span);
    }

    case slang::ast::ExpressionKind::RealLiteral: {
      const auto& rl = expr.as<slang::ast::RealLiteral>();
      auto type_id = TypeIdOfSlangExpr(unit_facts, unit_state, expr);
      if (!type_id) return std::unexpected(std::move(type_id.error()));
      return MakeRealLiteralExpr(rl.getValue(), *type_id, span);
    }

    case slang::ast::ExpressionKind::NamedValue:
      return LowerNamedValueProc(
          unit_facts, unit_state, proc_state, stack,
          expr.as<slang::ast::NamedValueExpression>());

    case slang::ast::ExpressionKind::LValueReference:
      return LowerLValueReferenceExpr(
          span, compound_lvalue_context,
          TypeIdOfSlangExpr(unit_facts, unit_state, expr), "LowerProcExpr");

    case slang::ast::ExpressionKind::Conversion:
      return LowerConversionExprProc(
          unit_facts, unit_state, proc_state, stack, compound_lvalue_context,
          expr.as<slang::ast::ConversionExpression>(), expr, span);

    case slang::ast::ExpressionKind::UnaryOp:
      return LowerUnaryExprProc(
          unit_facts, unit_state, proc_state, stack, compound_lvalue_context,
          expr.as<slang::ast::UnaryExpression>(), expr, span);

    case slang::ast::ExpressionKind::BinaryOp:
      return LowerBinaryExprProc(
          unit_facts, unit_state, proc_state, stack, compound_lvalue_context,
          expr.as<slang::ast::BinaryExpression>(), expr, span);

    case slang::ast::ExpressionKind::ConditionalOp:
      return LowerConditionalExprProc(
          unit_facts, unit_state, proc_state, stack, compound_lvalue_context,
          expr.as<slang::ast::ConditionalExpression>(), expr, span);

    case slang::ast::ExpressionKind::Call:
      return LowerCallExprProc(
          unit_facts, unit_state, proc_state, stack, compound_lvalue_context,
          expr.as<slang::ast::CallExpression>(), expr, span);

    case slang::ast::ExpressionKind::Assignment:
      return LowerAssignmentExprProc(
          unit_facts, unit_state, proc_state, stack,
          expr.as<slang::ast::AssignmentExpression>(), expr, span);

    case slang::ast::ExpressionKind::Inside:
      return LowerInsideExprProc(
          unit_facts, unit_state, proc_state, stack, compound_lvalue_context,
          expr.as<slang::ast::InsideExpression>(), expr, span);

    case slang::ast::ExpressionKind::ElementSelect:
      return LowerElementSelectExprProc(
          unit_facts, unit_state, proc_state, stack, compound_lvalue_context,
          expr.as<slang::ast::ElementSelectExpression>(), expr, span);

    case slang::ast::ExpressionKind::RangeSelect:
      return LowerRangeSelectExprProc(
          unit_facts, unit_state, proc_state, stack, compound_lvalue_context,
          expr.as<slang::ast::RangeSelectExpression>(), expr, span);

    default:
      return diag::Unsupported(
          span, diag::DiagCode::kUnsupportedExpressionForm,
          "this expression form is not supported yet",
          diag::UnsupportedCategory::kOperation);
  }
}

auto LowerStructuralExpr(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ScopeLoweringState& scope_state, const ScopeStack& stack,
    const slang::ast::Expression& expr,
    std::optional<hir::Lvalue> compound_lvalue_context)
    -> diag::Result<hir::Expr> {
  const auto& mapper = unit_facts.SourceMapper();
  const auto span = mapper.SpanOf(expr.sourceRange);

  switch (expr.kind) {
    case slang::ast::ExpressionKind::IntegerLiteral: {
      auto type_id = TypeIdOfSlangExpr(unit_facts, unit_state, expr);
      if (!type_id) return std::unexpected(std::move(type_id.error()));
      return MakeIntegerLiteralExpr(
          expr.as<slang::ast::IntegerLiteral>(), *type_id, span);
    }

    case slang::ast::ExpressionKind::UnbasedUnsizedIntegerLiteral: {
      auto type_id = TypeIdOfSlangExpr(unit_facts, unit_state, expr);
      if (!type_id) return std::unexpected(std::move(type_id.error()));
      return MakeUnbasedUnsizedLiteralExpr(
          expr.as<slang::ast::UnbasedUnsizedIntegerLiteral>(), *type_id, span);
    }

    case slang::ast::ExpressionKind::RealLiteral: {
      const auto& rl = expr.as<slang::ast::RealLiteral>();
      auto type_id = TypeIdOfSlangExpr(unit_facts, unit_state, expr);
      if (!type_id) return std::unexpected(std::move(type_id.error()));
      return MakeRealLiteralExpr(rl.getValue(), *type_id, span);
    }

    case slang::ast::ExpressionKind::NamedValue:
      return LowerNamedValueStructural(
          unit_facts, unit_state, stack,
          expr.as<slang::ast::NamedValueExpression>());

    case slang::ast::ExpressionKind::LValueReference:
      return LowerLValueReferenceExpr(
          span, compound_lvalue_context,
          TypeIdOfSlangExpr(unit_facts, unit_state, expr),
          "LowerStructuralExpr");

    case slang::ast::ExpressionKind::Conversion:
      return LowerConversionExprStructural(
          unit_facts, unit_state, scope_state, stack, compound_lvalue_context,
          expr.as<slang::ast::ConversionExpression>(), expr, span);

    case slang::ast::ExpressionKind::UnaryOp:
      return LowerUnaryExprStructural(
          unit_facts, unit_state, scope_state, stack, compound_lvalue_context,
          expr.as<slang::ast::UnaryExpression>(), expr, span);

    case slang::ast::ExpressionKind::BinaryOp:
      return LowerBinaryExprStructural(
          unit_facts, unit_state, scope_state, stack, compound_lvalue_context,
          expr.as<slang::ast::BinaryExpression>(), expr, span);

    case slang::ast::ExpressionKind::ConditionalOp:
      return LowerConditionalExprStructural(
          unit_facts, unit_state, scope_state, stack, compound_lvalue_context,
          expr.as<slang::ast::ConditionalExpression>(), expr, span);

    default:
      return diag::Unsupported(
          span, diag::DiagCode::kUnsupportedStructuralExpressionForm,
          "this structural expression form is not supported yet",
          diag::UnsupportedCategory::kFeature);
  }
}

namespace {

// Extract the Lvalue from a lowered Expr whose Primary names a writable
// storage location, or emit the "assignment target not supported" diagnostic
// otherwise (e.g. literal, function call, binary expression).
auto LvalueFromLoweredExpr(hir::Expr lowered, diag::SourceSpan span)
    -> diag::Result<hir::Lvalue> {
  auto unsupported = [&] {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedAssignmentTarget,
        "assignment target is not supported yet",
        diag::UnsupportedCategory::kFeature);
  };
  auto* primary = std::get_if<hir::PrimaryExpr>(&lowered.data);
  if (primary == nullptr) return unsupported();
  return std::visit(
      Overloaded{
          [&](const hir::StructuralVarRef& r) -> diag::Result<hir::Lvalue> {
            return r;
          },
          [&](const hir::ProceduralVarRef& r) -> diag::Result<hir::Lvalue> {
            return r;
          },
          [&](const hir::LoopVarRef& r) -> diag::Result<hir::Lvalue> {
            return r;
          },
          [&](const auto&) -> diag::Result<hir::Lvalue> {
            return unsupported();
          },
      },
      primary->data);
}

}  // namespace

auto LowerProcLvalue(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ProcessLoweringState& proc_state, const ScopeStack& stack,
    const slang::ast::Expression& expr) -> diag::Result<hir::Lvalue> {
  auto lowered = LowerProcExpr(
      unit_facts, unit_state, proc_state, stack, expr, std::nullopt);
  if (!lowered) return std::unexpected(std::move(lowered.error()));
  return LvalueFromLoweredExpr(
      *std::move(lowered), unit_facts.SourceMapper().SpanOf(expr.sourceRange));
}

}  // namespace lyra::lowering::ast_to_hir
