#include "lyra/lowering/ast_to_hir/expression/lower.hpp"

#include <cstdint>
#include <expected>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/HierarchicalReference.h>
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
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/ParameterSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>
#include <slang/numeric/ConstantValue.h>
#include <slang/numeric/SVInt.h>
#include <slang/numeric/Time.h>
#include <slang/syntax/AllSyntax.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/conversion.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/ast_to_hir/expression/slang_atoms.hpp"
#include "lyra/lowering/ast_to_hir/facts.hpp"
#include "lyra/lowering/ast_to_hir/integral_constant.hpp"
#include "lyra/lowering/ast_to_hir/state.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

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

auto MakeParameterValueExpr(
    const slang::ast::ParameterSymbol& sym, hir::TypeId type,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  const auto& cv = sym.getValue();
  if (cv.isInteger()) {
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
  if (cv.isReal()) {
    return hir::Expr{
        .type = type,
        .data = hir::PrimaryExpr{.data = hir::RealLiteral{.value = cv.real()}},
        .span = span,
    };
  }
  if (cv.isShortReal()) {
    return hir::Expr{
        .type = type,
        .data =
            hir::PrimaryExpr{
                .data =
                    hir::RealLiteral{
                        .value = static_cast<double>(cv.shortReal())}},
        .span = span,
    };
  }
  if (cv.isString()) {
    return hir::Expr{
        .type = type,
        .data = hir::PrimaryExpr{.data = hir::StringLiteral{.value = cv.str()}},
        .span = span,
    };
  }
  return diag::Unsupported(
      span, diag::DiagCode::kUnsupportedExpressionForm,
      "parameter of aggregate type is not yet supported",
      diag::UnsupportedCategory::kFeature);
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

auto MakeReturnConventionType(
    UnitLoweringState& unit_state, support::ReturnConvention conv)
    -> hir::TypeId {
  switch (conv) {
    case support::ReturnConvention::kVoid:
      return unit_state.VoidTypeId();
    case support::ReturnConvention::kInt32:
      return unit_state.Int32TypeId();
    case support::ReturnConvention::kInteger:
      return unit_state.IntegerTypeId();
    case support::ReturnConvention::kString:
      return unit_state.StringTypeId();
    case support::ReturnConvention::kTime64:
      return unit_state.TimeTypeId();
    case support::ReturnConvention::kRealTime:
      return unit_state.RealTimeTypeId();
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

  if (sym.kind == slang::ast::SymbolKind::Parameter) {
    auto type_id = TypeIdOfSlangExpr(unit_facts, unit_state, named);
    if (!type_id) return std::unexpected(std::move(type_id.error()));
    return MakeParameterValueExpr(
        sym.as<slang::ast::ParameterSymbol>(), *type_id, span);
  }

  // Subroutine formals (LRM 13.5) and foreach iterators (LRM 12.7.3) are
  // VariableSymbol subclasses and route through the same procedural-var
  // binding as ordinary body locals.
  if (sym.kind != slang::ast::SymbolKind::Variable &&
      sym.kind != slang::ast::SymbolKind::FormalArgument &&
      sym.kind != slang::ast::SymbolKind::Iterator) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedNonVariableNamedReference,
        "reference to non-variable declaration is not supported",
        diag::UnsupportedCategory::kFeature);
  }
  const auto& var = sym.as<slang::ast::VariableSymbol>();

  if (auto local = proc_state.LookupProceduralVar(var)) {
    if (proc_state.InForkBranch()) {
      return diag::Unsupported(
          span, diag::DiagCode::kUnsupportedForkJoinForm,
          "a fork-join branch referencing a procedural variable is not yet "
          "supported; branches may touch only module-scope signals",
          diag::UnsupportedCategory::kFeature);
    }
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

// LRM 23.6 hierarchical reference. The head of the navigation differs by
// direction: a downward path (`c.x`, `m.l.x`, `c[1].x`) starts at a local child
// member; an upward path (`Top.g`, `Top.sib.y`) starts by climbing the parent
// chain at construction to the named ancestor instance. The shared tail
// (`path.subspan(1)`) and the resolve-once slot are common
// (reference_resolution.md, docs/decisions/upward-reference-resolution.md).
auto LowerHierarchicalValueProc(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    const ScopeStack& stack, const slang::ast::HierarchicalValueExpression& hve)
    -> diag::Result<hir::Expr> {
  const auto span = unit_facts.SourceMapper().SpanOf(hve.sourceRange);
  const auto& ref = hve.ref;

  if (ref.isViaIfacePort()) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "interface-port hierarchical reference is not yet supported",
        diag::UnsupportedCategory::kOperation);
  }

  const auto& target = hve.symbol;
  if (target.kind != slang::ast::SymbolKind::Variable) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "cross-unit reference to a non-variable declaration is not yet "
        "supported",
        diag::UnsupportedCategory::kOperation);
  }

  auto type_id = TypeIdOfSlangExpr(unit_facts, unit_state, hve);
  if (!type_id) return std::unexpected(std::move(type_id.error()));

  // ref.path is slang's resolved top-down navigation: path.front() is the head
  // (a local member downward, the named ancestor upward); the rest navigate
  // down to the leaf (path.back() is `target`), each step a named member or an
  // instance-array index. The tail is shared by both directions.
  std::vector<hir::PathStep> path;
  path.reserve(ref.path.size() - 1);
  for (const auto& step : ref.path.subspan(1)) {
    if (std::holds_alternative<std::string_view>(step.selector)) {
      path.emplace_back(
          hir::MemberHop{
              std::string{std::get<std::string_view>(step.selector)}});
    } else if (std::holds_alternative<std::int32_t>(step.selector)) {
      path.emplace_back(
          hir::IndexHop{static_cast<std::uint32_t>(
              std::get<std::int32_t>(step.selector))});
    } else {
      return diag::Unsupported(
          span, diag::DiagCode::kUnsupportedExpressionForm,
          "instance-array range select in a hierarchical path is not yet "
          "supported",
          diag::UnsupportedCategory::kOperation);
    }
  }

  const auto& var = target.as<slang::ast::VariableSymbol>();
  const slang::ast::Symbol& head_sym = *ref.path.front().symbol;

  if (ref.isUpward()) {
    // An upward reference (LRM 23.8) climbs the parent chain to the ancestor
    // named by the reference's first source component, then fetches the leaf
    // signal from it by name. A named generate / procedural block ancestor or a
    // `$root`-anchored path (neither a module instance) is not yet supported.
    if (head_sym.kind != slang::ast::SymbolKind::Instance) {
      return diag::Unsupported(
          span, diag::DiagCode::kUnsupportedExpressionForm,
          "upward hierarchical reference whose target ancestor is not a module "
          "instance is not yet supported",
          diag::UnsupportedCategory::kOperation);
    }
    // First cut: the leaf sits directly on the matched ancestor (path is
    // ancestor + leaf). Descending through a child instance after the climb
    // needs a separate by-name child-navigation surface.
    if (ref.path.size() != 2) {
      return diag::Unsupported(
          span, diag::DiagCode::kUnsupportedExpressionForm,
          "upward hierarchical reference that descends through a child "
          "instance "
          "is not yet supported",
          diag::UnsupportedCategory::kOperation);
    }
    // The slot lives in the owning structural scope and the climb starts from
    // its object. A reference inside a generate block would need its slot on a
    // nested generate class; restrict to the unit root for now.
    if (stack.Depth() != 1) {
      return diag::Unsupported(
          span, diag::DiagCode::kUnsupportedExpressionForm,
          "upward hierarchical reference inside a generate block is not yet "
          "supported",
          diag::UnsupportedCategory::kOperation);
    }
    // The climb key is the ancestor's module name (LRM 23.8) -- class-level, so
    // one artifact serves every instance regardless of depth. It comes from the
    // resolved ancestor symbol, never from syntax (a wrapped sub-expression
    // like `Top.g[3]` may carry none).
    std::string ancestor_name{
        head_sym.as<slang::ast::InstanceSymbol>().getDefinition().name};
    std::string signal_name{ref.path.back().symbol->name};
    return MakeCrossUnitMemberRef(
        unit_state, var, stack.Current(),
        hir::UpwardHead{
            .ancestor_name = std::move(ancestor_name),
            .signal_name = std::move(signal_name)},
        {}, *type_id, span);
  }

  // Downward: the head is an owned instance / instance-array member this unit's
  // scope directly binds (the pre-pass always binds it). A missing binding is a
  // compiler-bug invariant. Crossing a generate-block scope is not yet
  // supported.
  const bool head_is_instance_member =
      head_sym.kind == slang::ast::SymbolKind::Instance ||
      head_sym.kind == slang::ast::SymbolKind::InstanceArray;
  if (!head_is_instance_member) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "hierarchical reference through a generate-block scope is not yet "
        "supported",
        diag::UnsupportedCategory::kOperation);
  }
  const auto head_binding = unit_state.LookupInstanceMemberBinding(head_sym);
  if (!head_binding.has_value()) {
    throw InternalError(
        "LowerHierarchicalValueProc: downward head member has no binding");
  }
  const auto hops = stack.HopsTo(head_binding->home_frame);
  if (!hops.has_value() || hops->value != 0) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "hierarchical reference across a generate-block scope boundary is not "
        "yet supported",
        diag::UnsupportedCategory::kOperation);
  }
  return MakeCrossUnitMemberRef(
      unit_state, var, head_binding->home_frame,
      hir::DownwardHead{.instance = head_binding->member_id}, std::move(path),
      *type_id, span);
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
  if (sym.kind == slang::ast::SymbolKind::Parameter) {
    auto type_id = TypeIdOfSlangExpr(unit_facts, unit_state, named);
    if (!type_id) return std::unexpected(std::move(type_id.error()));
    return MakeParameterValueExpr(
        sym.as<slang::ast::ParameterSymbol>(), *type_id, span);
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

auto LowerConversionExprProc(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ProcessLoweringState& proc_state, const ScopeStack& stack,
    const slang::ast::ConversionExpression& conv,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto operand_or =
      LowerProcExpr(unit_facts, unit_state, proc_state, stack, conv.operand());
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
    const slang::ast::UnaryExpression& un, const slang::ast::Expression& expr,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  const hir::UnaryOp op = LowerUnaryOp(un.op);
  auto operand_or =
      LowerProcExpr(unit_facts, unit_state, proc_state, stack, un.operand());
  if (!operand_or) return std::unexpected(std::move(operand_or.error()));
  const hir::ExprId operand_id = proc_state.AddExpr(*std::move(operand_or));
  auto type_id = TypeIdOfSlangExpr(unit_facts, unit_state, expr);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return hir::Expr{
      .type = *type_id,
      .data = hir::UnaryExpr{.op = op, .operand = operand_id},
      .span = span,
  };
}

auto LowerBinaryExprProc(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ProcessLoweringState& proc_state, const ScopeStack& stack,
    const slang::ast::BinaryExpression& bin, const slang::ast::Expression& expr,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  auto lhs_or =
      LowerProcExpr(unit_facts, unit_state, proc_state, stack, bin.left());
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
  const hir::ExprId lhs_id = proc_state.AddExpr(*std::move(lhs_or));
  auto rhs_or =
      LowerProcExpr(unit_facts, unit_state, proc_state, stack, bin.right());
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
      unit_facts, unit_state, proc_state, stack, *cond.conditions[0].expr);
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  const hir::ExprId cond_id = proc_state.AddExpr(*std::move(cond_or));
  auto then_or =
      LowerProcExpr(unit_facts, unit_state, proc_state, stack, cond.left());
  if (!then_or) return std::unexpected(std::move(then_or.error()));
  const hir::ExprId then_id = proc_state.AddExpr(*std::move(then_or));
  auto else_or =
      LowerProcExpr(unit_facts, unit_state, proc_state, stack, cond.right());
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
    const slang::ast::CallExpression& call, const slang::ast::Expression& expr,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  std::vector<std::optional<hir::ExprId>> arg_ids;
  arg_ids.reserve(call.arguments().size());
  std::optional<hir::TypeId> receiver_type;
  for (std::size_t i = 0; i < call.arguments().size(); ++i) {
    // LRM 13.5: slang models an `output` / `inout` actual as an
    // AssignmentExpression whose right side is an EmptyArgument placeholder;
    // the actual lvalue is the left side. HIR carries just that lvalue -- the
    // copy-in / copy-out is synthesized at HIR-to-MIR from the formal's
    // direction.
    const slang::ast::Expression* arg = call.arguments()[i];
    if (arg->kind == slang::ast::ExpressionKind::Assignment) {
      const auto& as = arg->as<slang::ast::AssignmentExpression>();
      if (as.right().kind == slang::ast::ExpressionKind::EmptyArgument) {
        arg = &as.left();
      }
    }
    // LRM 21.3.4.4 form 2d: a standalone EmptyArgument marks a positional
    // elision (`$fread(mem, fd, , count)`). Surface as `std::nullopt` so the
    // per-subroutine HIR-to-MIR handler can decide whether elision is valid
    // at this position; positions stay aligned with slang's arg list.
    if (arg->kind == slang::ast::ExpressionKind::EmptyArgument) {
      arg_ids.emplace_back(std::nullopt);
      continue;
    }
    auto arg_or =
        LowerProcExpr(unit_facts, unit_state, proc_state, stack, *arg);
    if (!arg_or) return std::unexpected(std::move(arg_or.error()));
    if (i == 0) {
      receiver_type = arg_or->type;
    }
    arg_ids.emplace_back(proc_state.AddExpr(*std::move(arg_or)));
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
                    .callee = hir::BuiltinMethodRef{.method = *kind},
                    .arguments = std::move(arg_ids),
                },
            .span = span,
        };
      }
    }

    if (receiver_type.has_value() &&
        unit_state.GetType(*receiver_type).Kind() == hir::TypeKind::kString) {
      if (auto kind = LowerStringMethodName(name); kind.has_value()) {
        // LRM 6.16.1 through 6.16.15 -- string intrinsic methods. The
        // receiver is arguments[0]; remaining arguments are the SV method
        // parameters (e.g. substr's `i, j`).
        auto type_id = TypeIdOfSlangExpr(unit_facts, unit_state, expr);
        if (!type_id) return std::unexpected(std::move(type_id.error()));
        return hir::Expr{
            .type = *type_id,
            .data =
                hir::CallExpr{
                    .callee = hir::BuiltinMethodRef{.method = *kind},
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
                          .method = hir::EventMethodKind::kTriggered,
                      },
                  .arguments = std::move(arg_ids),
              },
          .span = span,
      };
    }

    if (receiver_type.has_value() &&
        std::holds_alternative<hir::DynamicArrayType>(
            unit_state.GetType(*receiver_type).data)) {
      if (auto kind = LowerArrayMethodName(name); kind.has_value()) {
        // LRM 7.5.2 / 7.5.3 dynamic-array methods plus LRM 7.12.2 / 7.12.3
        // no-`with` subset. Receiver is arguments[0]; this PR's surface
        // takes no further arguments. Slang upstream gates element-type
        // constraints (integral for reductions, comparable for sort), so
        // any call landing here is well-typed.
        auto type_id = TypeIdOfSlangExpr(unit_facts, unit_state, expr);
        if (!type_id) return std::unexpected(std::move(type_id.error()));
        return hir::Expr{
            .type = *type_id,
            .data =
                hir::CallExpr{
                    .callee = hir::BuiltinMethodRef{.method = *kind},
                    .arguments = std::move(arg_ids),
                },
            .span = span,
        };
      }
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
  auto validate = ValidateAssignableSlangExpr(
      unit_facts, unit_state, &proc_state, as.left());
  if (!validate) return std::unexpected(std::move(validate.error()));
  auto lhs_or =
      LowerProcExpr(unit_facts, unit_state, proc_state, stack, as.left());
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
  const hir::ExprId lhs_id = proc_state.AddExpr(*std::move(lhs_or));

  auto type_id = TypeIdOfSlangExpr(unit_facts, unit_state, expr);
  if (!type_id) return std::unexpected(std::move(type_id.error()));

  const auto kind = as.isNonBlocking() ? hir::AssignKind::kNonBlocking
                                       : hir::AssignKind::kBlocking;

  if (!as.op.has_value()) {
    auto rhs_or =
        LowerProcExpr(unit_facts, unit_state, proc_state, stack, as.right());
    if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
    const hir::ExprId rhs_id = proc_state.AddExpr(*std::move(rhs_or));
    return hir::Expr{
        .type = *type_id,
        .data =
            hir::AssignExpr{
                .kind = kind,
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
  auto rhs_or =
      LowerProcExpr(unit_facts, unit_state, proc_state, stack, bare_user_rhs);
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  hir::Expr rhs_expr = *std::move(rhs_or);
  if (rhs_expr.type.value != type_id->value) {
    const hir::ExprId inner_id = proc_state.AddExpr(std::move(rhs_expr));
    rhs_expr = hir::Expr{
        .type = *type_id,
        .data =
            hir::ConversionExpr{
                .operand = inner_id, .kind = hir::ConversionKind::kImplicit},
        .span = span,
    };
  }
  const hir::ExprId rhs_id = proc_state.AddExpr(std::move(rhs_expr));
  return hir::Expr{
      .type = *type_id,
      .data =
          hir::AssignExpr{
              .kind = kind,
              .lhs = lhs_id,
              .compound_op = LowerBinaryOp(*as.op),
              .rhs = rhs_id},
      .span = span,
  };
}

auto LowerIncDecExprProc(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ProcessLoweringState& proc_state, const ScopeStack& stack,
    const slang::ast::UnaryExpression& un, const slang::ast::Expression& expr,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  auto validate = ValidateAssignableSlangExpr(
      unit_facts, unit_state, &proc_state, un.operand());
  if (!validate) return std::unexpected(std::move(validate.error()));

  auto target_or =
      LowerProcExpr(unit_facts, unit_state, proc_state, stack, un.operand());
  if (!target_or) return std::unexpected(std::move(target_or.error()));
  const hir::ExprId target_id = proc_state.AddExpr(*std::move(target_or));

  auto type_id = TypeIdOfSlangExpr(unit_facts, unit_state, expr);
  if (!type_id) return std::unexpected(std::move(type_id.error()));

  return hir::Expr{
      .type = *type_id,
      .data =
          hir::IncDecExpr{.op = LowerSlangIncDecOp(un.op), .target = target_id},
      .span = span,
  };
}

auto LowerInsideExprProc(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ProcessLoweringState& proc_state, const ScopeStack& stack,
    const slang::ast::InsideExpression& in, const slang::ast::Expression& expr,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  auto lhs_or =
      LowerProcExpr(unit_facts, unit_state, proc_state, stack, in.left());
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
  const hir::ExprId lhs_id = proc_state.AddExpr(*std::move(lhs_or));

  std::vector<hir::InsideItem> items;
  items.reserve(in.rangeList().size());
  for (const auto* item : in.rangeList()) {
    auto item_or =
        LowerInsideItem(unit_facts, unit_state, proc_state, stack, *item);
    if (!item_or) return std::unexpected(std::move(item_or.error()));
    items.push_back(*std::move(item_or));
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
    const slang::ast::ElementSelectExpression& sel,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  if (!sel.value().type->isIntegral() && !sel.value().type->isUnpackedArray()) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "element-select on non-integral operand is not yet supported",
        diag::UnsupportedCategory::kOperation);
  }

  auto base_or =
      LowerProcExpr(unit_facts, unit_state, proc_state, stack, sel.value());
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const hir::ExprId base_id = proc_state.AddExpr(*std::move(base_or));

  auto idx_or =
      LowerProcExpr(unit_facts, unit_state, proc_state, stack, sel.selector());
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
    const slang::ast::RangeSelectExpression& sel,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  if (!sel.value().type->isIntegral() && !sel.value().type->isUnpackedArray()) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "range-select on non-integral, non-unpacked operand is not yet "
        "supported",
        diag::UnsupportedCategory::kOperation);
  }

  auto base_or =
      LowerProcExpr(unit_facts, unit_state, proc_state, stack, sel.value());
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const hir::ExprId base_id = proc_state.AddExpr(*std::move(base_or));

  auto left_or =
      LowerProcExpr(unit_facts, unit_state, proc_state, stack, sel.left());
  if (!left_or) return std::unexpected(std::move(left_or.error()));
  const hir::ExprId left_id = proc_state.AddExpr(*std::move(left_or));

  auto right_or =
      LowerProcExpr(unit_facts, unit_state, proc_state, stack, sel.right());
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

auto LowerMemberAccessExprProc(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ProcessLoweringState& proc_state, const ScopeStack& stack,
    const slang::ast::MemberAccessExpression& sel,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  const auto* field = &sel.member.as<slang::ast::FieldSymbol>();
  if (sel.member.kind != slang::ast::SymbolKind::Field) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "member access target is not a struct field",
        diag::UnsupportedCategory::kOperation);
  }
  auto base_or =
      LowerProcExpr(unit_facts, unit_state, proc_state, stack, sel.value());
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const hir::ExprId base_id = proc_state.AddExpr(*std::move(base_or));
  auto type_id = TypeIdOfSlangExpr(unit_facts, unit_state, expr);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return hir::Expr{
      .type = *type_id,
      .data =
          hir::MemberAccessExpr{
              .base_value = base_id,
              .field_index = field->fieldIndex,
          },
      .span = span,
  };
}

auto LowerConcatExprProc(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ProcessLoweringState& proc_state, const ScopeStack& stack,
    const slang::ast::ConcatenationExpression& cc,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto type_id = TypeIdOfSlangExpr(unit_facts, unit_state, expr);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  const auto kind = unit_state.GetType(*type_id).Kind();
  if (kind != hir::TypeKind::kString && kind != hir::TypeKind::kPackedArray) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "concatenation result type is neither string nor packed (LRM 11.4.12)",
        diag::UnsupportedCategory::kOperation);
  }
  std::vector<hir::ExprId> operand_ids;
  operand_ids.reserve(cc.operands().size());
  for (const auto* op : cc.operands()) {
    // LRM 11.4.12.1: a zero-multiplier replication contributes no bits to
    // the enclosing concatenation. Slang types such Replication nodes as
    // `void`; recognize that exact AST shape so we drop only the documented
    // zero-rep case and let any other unexpected void surface as an error
    // through the normal type-kind check downstream.
    if (op->kind == slang::ast::ExpressionKind::Replication &&
        op->type->isVoid()) {
      continue;
    }
    auto operand_or =
        LowerProcExpr(unit_facts, unit_state, proc_state, stack, *op);
    if (!operand_or) return std::unexpected(std::move(operand_or.error()));
    operand_ids.push_back(proc_state.AddExpr(*std::move(operand_or)));
  }
  return hir::Expr{
      .type = *type_id,
      .data = hir::ConcatExpr{.operands = std::move(operand_ids)},
      .span = span,
  };
}

auto LowerReplicationExprProc(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ProcessLoweringState& proc_state, const ScopeStack& stack,
    const slang::ast::ReplicationExpression& rp,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto type_id = TypeIdOfSlangExpr(unit_facts, unit_state, expr);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  const auto kind = unit_state.GetType(*type_id).Kind();
  if (kind != hir::TypeKind::kString && kind != hir::TypeKind::kPackedArray) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "replication result type is neither string nor packed "
        "(LRM 11.4.12.1)",
        diag::UnsupportedCategory::kOperation);
  }
  auto count_or =
      LowerProcExpr(unit_facts, unit_state, proc_state, stack, rp.count());
  if (!count_or) return std::unexpected(std::move(count_or.error()));
  const hir::ExprId count_id = proc_state.AddExpr(*std::move(count_or));
  auto concat_or =
      LowerProcExpr(unit_facts, unit_state, proc_state, stack, rp.concat());
  if (!concat_or) return std::unexpected(std::move(concat_or.error()));
  const hir::ExprId concat_id = proc_state.AddExpr(*std::move(concat_or));
  return hir::Expr{
      .type = *type_id,
      .data = hir::ReplicationExpr{.count = count_id, .concat = concat_id},
      .span = span,
  };
}

auto LowerAssignmentPatternFromElementsProc(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ProcessLoweringState& proc_state, const ScopeStack& stack,
    const slang::ast::AssignmentPatternExpressionBase& ap,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto type_id = TypeIdOfSlangExpr(unit_facts, unit_state, expr);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  std::vector<hir::ExprId> element_ids;
  element_ids.reserve(ap.elements().size());
  for (const auto* elem : ap.elements()) {
    auto lowered =
        LowerProcExpr(unit_facts, unit_state, proc_state, stack, *elem);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    element_ids.push_back(proc_state.AddExpr(*std::move(lowered)));
  }
  return hir::Expr{
      .type = *type_id,
      .data = hir::AssignmentPatternExpr{.elements = std::move(element_ids)},
      .span = span,
  };
}

auto LowerReplicatedAssignmentPatternExprProc(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ProcessLoweringState& proc_state, const ScopeStack& stack,
    const slang::ast::ReplicatedAssignmentPatternExpression& rp,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto type_id = TypeIdOfSlangExpr(unit_facts, unit_state, expr);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  auto count_or =
      LowerProcExpr(unit_facts, unit_state, proc_state, stack, rp.count());
  if (!count_or) return std::unexpected(std::move(count_or.error()));
  const hir::ExprId count_id = proc_state.AddExpr(*std::move(count_or));
  std::vector<hir::ExprId> item_ids;
  item_ids.reserve(rp.elements().size());
  for (const auto* elem : rp.elements()) {
    auto lowered =
        LowerProcExpr(unit_facts, unit_state, proc_state, stack, *elem);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    item_ids.push_back(proc_state.AddExpr(*std::move(lowered)));
  }
  return hir::Expr{
      .type = *type_id,
      .data =
          hir::AssignmentPatternReplicationExpr{
              .count = count_id,
              .items = std::move(item_ids),
          },
      .span = span,
  };
}

// LRM 7.5.1 `new[N]` / `new[N](other)` dynamic-array constructor. Result type
// is the dynamic array type slang resolved for the expression; the optional
// initializer is `(other)` -- the LRM 7.5.1 source array for the copy-with-
// pad-or-truncate form.
auto LowerNewArrayExprProc(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ProcessLoweringState& proc_state, const ScopeStack& stack,
    const slang::ast::NewArrayExpression& na,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto type_id = TypeIdOfSlangExpr(unit_facts, unit_state, expr);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  auto size_or =
      LowerProcExpr(unit_facts, unit_state, proc_state, stack, na.sizeExpr());
  if (!size_or) return std::unexpected(std::move(size_or.error()));
  const hir::ExprId size_id = proc_state.AddExpr(*std::move(size_or));
  std::optional<hir::ExprId> initializer_id;
  if (na.initExpr() != nullptr) {
    auto init_or = LowerProcExpr(
        unit_facts, unit_state, proc_state, stack, *na.initExpr());
    if (!init_or) return std::unexpected(std::move(init_or.error()));
    initializer_id = proc_state.AddExpr(*std::move(init_or));
  }
  return hir::Expr{
      .type = *type_id,
      .data =
          hir::DynamicArrayNewExpr{
              .size = size_id,
              .initializer = initializer_id,
          },
      .span = span,
  };
}

auto LowerElementSelectExprStructural(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ScopeLoweringState& scope_state, const ScopeStack& stack,
    const slang::ast::ElementSelectExpression& sel,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  if (!sel.value().type->isIntegral()) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedStructuralExpressionForm,
        "element-select on non-integral operand is not yet supported",
        diag::UnsupportedCategory::kOperation);
  }
  auto base_or = LowerStructuralExpr(
      unit_facts, unit_state, scope_state, stack, sel.value());
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const hir::ExprId base_id = scope_state.AddExpr(*std::move(base_or));
  auto idx_or = LowerStructuralExpr(
      unit_facts, unit_state, scope_state, stack, sel.selector());
  if (!idx_or) return std::unexpected(std::move(idx_or.error()));
  const hir::ExprId idx_id = scope_state.AddExpr(*std::move(idx_or));
  auto type_id = TypeIdOfSlangExpr(unit_facts, unit_state, expr);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return hir::Expr{
      .type = *type_id,
      .data = hir::ElementSelectExpr{.base_value = base_id, .index = idx_id},
      .span = span,
  };
}

auto LowerRangeSelectExprStructural(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ScopeLoweringState& scope_state, const ScopeStack& stack,
    const slang::ast::RangeSelectExpression& sel,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  if (!sel.value().type->isIntegral() && !sel.value().type->isUnpackedArray()) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedStructuralExpressionForm,
        "range-select on non-integral, non-unpacked operand is not yet "
        "supported",
        diag::UnsupportedCategory::kOperation);
  }
  auto base_or = LowerStructuralExpr(
      unit_facts, unit_state, scope_state, stack, sel.value());
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const hir::ExprId base_id = scope_state.AddExpr(*std::move(base_or));
  auto left_or = LowerStructuralExpr(
      unit_facts, unit_state, scope_state, stack, sel.left());
  if (!left_or) return std::unexpected(std::move(left_or.error()));
  const hir::ExprId left_id = scope_state.AddExpr(*std::move(left_or));
  auto right_or = LowerStructuralExpr(
      unit_facts, unit_state, scope_state, stack, sel.right());
  if (!right_or) return std::unexpected(std::move(right_or.error()));
  const hir::ExprId right_id = scope_state.AddExpr(*std::move(right_or));
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
        "LowerRangeSelectExprStructural: unknown slang RangeSelectionKind");
  }();
  auto type_id = TypeIdOfSlangExpr(unit_facts, unit_state, expr);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return hir::Expr{
      .type = *type_id,
      .data =
          hir::RangeSelectExpr{
              .base_value = base_id, .bounds = std::move(bounds)},
      .span = span,
  };
}

auto LowerMemberAccessExprStructural(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ScopeLoweringState& scope_state, const ScopeStack& stack,
    const slang::ast::MemberAccessExpression& sel,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  if (sel.member.kind != slang::ast::SymbolKind::Field) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedStructuralExpressionForm,
        "member access target is not a struct field",
        diag::UnsupportedCategory::kOperation);
  }
  const auto& field = sel.member.as<slang::ast::FieldSymbol>();
  auto base_or = LowerStructuralExpr(
      unit_facts, unit_state, scope_state, stack, sel.value());
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const hir::ExprId base_id = scope_state.AddExpr(*std::move(base_or));
  auto type_id = TypeIdOfSlangExpr(unit_facts, unit_state, expr);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return hir::Expr{
      .type = *type_id,
      .data =
          hir::MemberAccessExpr{
              .base_value = base_id,
              .field_index = field.fieldIndex,
          },
      .span = span,
  };
}

auto LowerConcatExprStructural(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ScopeLoweringState& scope_state, const ScopeStack& stack,
    const slang::ast::ConcatenationExpression& cc,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto type_id = TypeIdOfSlangExpr(unit_facts, unit_state, expr);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  const auto kind = unit_state.GetType(*type_id).Kind();
  if (kind != hir::TypeKind::kString && kind != hir::TypeKind::kPackedArray) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedStructuralExpressionForm,
        "concatenation result type is neither string nor packed (LRM 11.4.12)",
        diag::UnsupportedCategory::kOperation);
  }
  std::vector<hir::ExprId> operand_ids;
  operand_ids.reserve(cc.operands().size());
  for (const auto* op : cc.operands()) {
    if (op->kind == slang::ast::ExpressionKind::Replication &&
        op->type->isVoid()) {
      continue;
    }
    auto operand_or =
        LowerStructuralExpr(unit_facts, unit_state, scope_state, stack, *op);
    if (!operand_or) return std::unexpected(std::move(operand_or.error()));
    operand_ids.push_back(scope_state.AddExpr(*std::move(operand_or)));
  }
  return hir::Expr{
      .type = *type_id,
      .data = hir::ConcatExpr{.operands = std::move(operand_ids)},
      .span = span,
  };
}

auto LowerAssignmentPatternFromElementsStructural(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ScopeLoweringState& scope_state, const ScopeStack& stack,
    const slang::ast::AssignmentPatternExpressionBase& ap,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto type_id = TypeIdOfSlangExpr(unit_facts, unit_state, expr);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  std::vector<hir::ExprId> element_ids;
  element_ids.reserve(ap.elements().size());
  for (const auto* elem : ap.elements()) {
    auto lowered =
        LowerStructuralExpr(unit_facts, unit_state, scope_state, stack, *elem);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    element_ids.push_back(scope_state.AddExpr(*std::move(lowered)));
  }
  return hir::Expr{
      .type = *type_id,
      .data = hir::AssignmentPatternExpr{.elements = std::move(element_ids)},
      .span = span,
  };
}

auto LowerReplicatedAssignmentPatternExprStructural(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ScopeLoweringState& scope_state, const ScopeStack& stack,
    const slang::ast::ReplicatedAssignmentPatternExpression& rp,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto type_id = TypeIdOfSlangExpr(unit_facts, unit_state, expr);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  auto count_or = LowerStructuralExpr(
      unit_facts, unit_state, scope_state, stack, rp.count());
  if (!count_or) return std::unexpected(std::move(count_or.error()));
  const hir::ExprId count_id = scope_state.AddExpr(*std::move(count_or));
  std::vector<hir::ExprId> item_ids;
  item_ids.reserve(rp.elements().size());
  for (const auto* elem : rp.elements()) {
    auto lowered =
        LowerStructuralExpr(unit_facts, unit_state, scope_state, stack, *elem);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    item_ids.push_back(scope_state.AddExpr(*std::move(lowered)));
  }
  return hir::Expr{
      .type = *type_id,
      .data =
          hir::AssignmentPatternReplicationExpr{
              .count = count_id,
              .items = std::move(item_ids),
          },
      .span = span,
  };
}

auto LowerConversionExprStructural(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ScopeLoweringState& scope_state, const ScopeStack& stack,
    const slang::ast::ConversionExpression& conv,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto operand_or = LowerStructuralExpr(
      unit_facts, unit_state, scope_state, stack, conv.operand());
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
    const slang::ast::UnaryExpression& un, const slang::ast::Expression& expr,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  const hir::UnaryOp op = LowerUnaryOp(un.op);
  auto operand_or = LowerStructuralExpr(
      unit_facts, unit_state, scope_state, stack, un.operand());
  if (!operand_or) return std::unexpected(std::move(operand_or.error()));
  const hir::ExprId operand_id = scope_state.AddExpr(*std::move(operand_or));
  auto type_id = TypeIdOfSlangExpr(unit_facts, unit_state, expr);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return hir::Expr{
      .type = *type_id,
      .data = hir::UnaryExpr{.op = op, .operand = operand_id},
      .span = span,
  };
}

auto LowerBinaryExprStructural(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ScopeLoweringState& scope_state, const ScopeStack& stack,
    const slang::ast::BinaryExpression& bin, const slang::ast::Expression& expr,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  auto lhs_or = LowerStructuralExpr(
      unit_facts, unit_state, scope_state, stack, bin.left());
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
  const hir::ExprId lhs_id = scope_state.AddExpr(*std::move(lhs_or));
  auto rhs_or = LowerStructuralExpr(
      unit_facts, unit_state, scope_state, stack, bin.right());
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
      unit_facts, unit_state, scope_state, stack, *cond.conditions[0].expr);
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  const hir::ExprId cond_id = scope_state.AddExpr(*std::move(cond_or));
  auto then_or = LowerStructuralExpr(
      unit_facts, unit_state, scope_state, stack, cond.left());
  if (!then_or) return std::unexpected(std::move(then_or.error()));
  const hir::ExprId then_id = scope_state.AddExpr(*std::move(then_or));
  auto else_or = LowerStructuralExpr(
      unit_facts, unit_state, scope_state, stack, cond.right());
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

auto MakeCrossUnitMemberRef(
    UnitLoweringState& unit_state, const slang::ast::ValueSymbol& target,
    ScopeFrameId home_frame, hir::CrossUnitRefHead head,
    std::vector<hir::PathStep> path, hir::TypeId type, diag::SourceSpan span)
    -> hir::Expr {
  const hir::CrossUnitRefId slot = unit_state.MapOrGetCrossUnitRef(
      target, home_frame, std::move(head), std::move(path), type);
  return MakeRefExpr(hir::CrossUnitVarRef{.id = slot}, type, span);
}

auto LowerInsideItem(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ProcessLoweringState& proc_state, const ScopeStack& stack,
    const slang::ast::Expression& item_expr) -> diag::Result<hir::InsideItem> {
  if (item_expr.kind == slang::ast::ExpressionKind::ValueRange) {
    const auto& vr = item_expr.as<slang::ast::ValueRangeExpression>();
    if (vr.rangeKind != slang::ast::ValueRangeKind::Simple) {
      return diag::Unsupported(
          unit_facts.SourceMapper().SpanOf(vr.sourceRange),
          diag::DiagCode::kUnsupportedExpressionForm,
          "tolerance-range form in inside operator is not yet supported",
          diag::UnsupportedCategory::kFeature);
    }
    auto lo_or =
        LowerProcExpr(unit_facts, unit_state, proc_state, stack, vr.left());
    if (!lo_or) return std::unexpected(std::move(lo_or.error()));
    const hir::ExprId lo_id = proc_state.AddExpr(*std::move(lo_or));
    auto hi_or =
        LowerProcExpr(unit_facts, unit_state, proc_state, stack, vr.right());
    if (!hi_or) return std::unexpected(std::move(hi_or.error()));
    const hir::ExprId hi_id = proc_state.AddExpr(*std::move(hi_or));
    return hir::InsideRangePair{.lo = lo_id, .hi = hi_id};
  }
  auto val_or =
      LowerProcExpr(unit_facts, unit_state, proc_state, stack, item_expr);
  if (!val_or) return std::unexpected(std::move(val_or.error()));
  return proc_state.AddExpr(*std::move(val_or));
}

auto LowerProcExpr(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ProcessLoweringState& proc_state, const ScopeStack& stack,
    const slang::ast::Expression& expr) -> diag::Result<hir::Expr> {
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

    case slang::ast::ExpressionKind::HierarchicalValue:
      return LowerHierarchicalValueProc(
          unit_facts, unit_state, stack,
          expr.as<slang::ast::HierarchicalValueExpression>());

    case slang::ast::ExpressionKind::LValueReference:
      throw InternalError(
          "LowerProcExpr: slang LValueReference must not reach HIR; "
          "compound assignment is lowered as a single AssignExpr with "
          "compound_op, and the LValueReference-bearing BinaryOp tree slang "
          "constructed is discarded at AST -> HIR");

    case slang::ast::ExpressionKind::Conversion:
      return LowerConversionExprProc(
          unit_facts, unit_state, proc_state, stack,
          expr.as<slang::ast::ConversionExpression>(), expr, span);

    case slang::ast::ExpressionKind::UnaryOp: {
      const auto& un = expr.as<slang::ast::UnaryExpression>();
      if (slang::ast::OpInfo::isLValue(un.op)) {
        return LowerIncDecExprProc(
            unit_facts, unit_state, proc_state, stack, un, expr, span);
      }
      return LowerUnaryExprProc(
          unit_facts, unit_state, proc_state, stack, un, expr, span);
    }

    case slang::ast::ExpressionKind::BinaryOp:
      return LowerBinaryExprProc(
          unit_facts, unit_state, proc_state, stack,
          expr.as<slang::ast::BinaryExpression>(), expr, span);

    case slang::ast::ExpressionKind::ConditionalOp:
      return LowerConditionalExprProc(
          unit_facts, unit_state, proc_state, stack,
          expr.as<slang::ast::ConditionalExpression>(), expr, span);

    case slang::ast::ExpressionKind::Call:
      return LowerCallExprProc(
          unit_facts, unit_state, proc_state, stack,
          expr.as<slang::ast::CallExpression>(), expr, span);

    case slang::ast::ExpressionKind::Assignment:
      return LowerAssignmentExprProc(
          unit_facts, unit_state, proc_state, stack,
          expr.as<slang::ast::AssignmentExpression>(), expr, span);

    case slang::ast::ExpressionKind::Inside:
      return LowerInsideExprProc(
          unit_facts, unit_state, proc_state, stack,
          expr.as<slang::ast::InsideExpression>(), expr, span);

    case slang::ast::ExpressionKind::ElementSelect:
      return LowerElementSelectExprProc(
          unit_facts, unit_state, proc_state, stack,
          expr.as<slang::ast::ElementSelectExpression>(), expr, span);

    case slang::ast::ExpressionKind::RangeSelect:
      return LowerRangeSelectExprProc(
          unit_facts, unit_state, proc_state, stack,
          expr.as<slang::ast::RangeSelectExpression>(), expr, span);

    case slang::ast::ExpressionKind::MemberAccess:
      return LowerMemberAccessExprProc(
          unit_facts, unit_state, proc_state, stack,
          expr.as<slang::ast::MemberAccessExpression>(), expr, span);

    case slang::ast::ExpressionKind::Concatenation:
      return LowerConcatExprProc(
          unit_facts, unit_state, proc_state, stack,
          expr.as<slang::ast::ConcatenationExpression>(), expr, span);

    case slang::ast::ExpressionKind::Replication:
      return LowerReplicationExprProc(
          unit_facts, unit_state, proc_state, stack,
          expr.as<slang::ast::ReplicationExpression>(), expr, span);

    case slang::ast::ExpressionKind::SimpleAssignmentPattern: {
      const auto& sap =
          expr.as<slang::ast::SimpleAssignmentPatternExpression>();
      if (sap.isLValue) {
        return diag::Unsupported(
            span, diag::DiagCode::kUnsupportedAssignmentPatternKind,
            "assignment pattern as LHS destructuring is not yet supported",
            diag::UnsupportedCategory::kOperation);
      }
      return LowerAssignmentPatternFromElementsProc(
          unit_facts, unit_state, proc_state, stack, sap, expr, span);
    }

    case slang::ast::ExpressionKind::StructuredAssignmentPattern: {
      // Slang accepts `'{idx:val, ...}` for a dynamic-array target as long
      // as indices cover a dense `0..N-1`. The legal subset is equivalent to
      // positional and adds no expressive power; rejecting it here keeps the
      // dispatch narrow until a consumer needs the structured form.
      if (expr.type->getCanonicalType().kind ==
          slang::ast::SymbolKind::DynamicArrayType) {
        return diag::Unsupported(
            span, diag::DiagCode::kUnsupportedAssignmentPatternKind,
            "index-keyed assignment pattern on a dynamic array is not yet "
            "supported; use positional form",
            diag::UnsupportedCategory::kOperation);
      }
      return LowerAssignmentPatternFromElementsProc(
          unit_facts, unit_state, proc_state, stack,
          expr.as<slang::ast::StructuredAssignmentPatternExpression>(), expr,
          span);
    }

    case slang::ast::ExpressionKind::ReplicatedAssignmentPattern:
      return LowerReplicatedAssignmentPatternExprProc(
          unit_facts, unit_state, proc_state, stack,
          expr.as<slang::ast::ReplicatedAssignmentPatternExpression>(), expr,
          span);

    case slang::ast::ExpressionKind::NewArray:
      return LowerNewArrayExprProc(
          unit_facts, unit_state, proc_state, stack,
          expr.as<slang::ast::NewArrayExpression>(), expr, span);

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
    const slang::ast::Expression& expr) -> diag::Result<hir::Expr> {
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

    case slang::ast::ExpressionKind::StringLiteral: {
      const auto& sl = expr.as<slang::ast::StringLiteral>();
      auto type_id = TypeIdOfSlangExpr(unit_facts, unit_state, expr);
      if (!type_id) return std::unexpected(std::move(type_id.error()));
      return MakeStringLiteralExpr(std::string{sl.getValue()}, *type_id, span);
    }

    case slang::ast::ExpressionKind::NamedValue:
      return LowerNamedValueStructural(
          unit_facts, unit_state, stack,
          expr.as<slang::ast::NamedValueExpression>());

    case slang::ast::ExpressionKind::LValueReference:
      throw InternalError(
          "LowerStructuralExpr: slang LValueReference must not reach HIR; "
          "generate-iter compound is rewritten as the next-value BinaryExpr "
          "directly");

    case slang::ast::ExpressionKind::Conversion:
      return LowerConversionExprStructural(
          unit_facts, unit_state, scope_state, stack,
          expr.as<slang::ast::ConversionExpression>(), expr, span);

    case slang::ast::ExpressionKind::UnaryOp: {
      const auto& un = expr.as<slang::ast::UnaryExpression>();
      if (slang::ast::OpInfo::isLValue(un.op)) {
        return diag::Unsupported(
            span, diag::DiagCode::kUnsupportedStructuralExpressionForm,
            "increment / decrement is not legal outside procedural code "
            "(LRM 11.3.6, 11.4.2)",
            diag::UnsupportedCategory::kOperation);
      }
      return LowerUnaryExprStructural(
          unit_facts, unit_state, scope_state, stack, un, expr, span);
    }

    case slang::ast::ExpressionKind::BinaryOp:
      return LowerBinaryExprStructural(
          unit_facts, unit_state, scope_state, stack,
          expr.as<slang::ast::BinaryExpression>(), expr, span);

    case slang::ast::ExpressionKind::ConditionalOp:
      return LowerConditionalExprStructural(
          unit_facts, unit_state, scope_state, stack,
          expr.as<slang::ast::ConditionalExpression>(), expr, span);

    case slang::ast::ExpressionKind::ElementSelect:
      return LowerElementSelectExprStructural(
          unit_facts, unit_state, scope_state, stack,
          expr.as<slang::ast::ElementSelectExpression>(), expr, span);

    case slang::ast::ExpressionKind::RangeSelect:
      return LowerRangeSelectExprStructural(
          unit_facts, unit_state, scope_state, stack,
          expr.as<slang::ast::RangeSelectExpression>(), expr, span);

    case slang::ast::ExpressionKind::MemberAccess:
      return LowerMemberAccessExprStructural(
          unit_facts, unit_state, scope_state, stack,
          expr.as<slang::ast::MemberAccessExpression>(), expr, span);

    case slang::ast::ExpressionKind::Concatenation:
      return LowerConcatExprStructural(
          unit_facts, unit_state, scope_state, stack,
          expr.as<slang::ast::ConcatenationExpression>(), expr, span);

    case slang::ast::ExpressionKind::SimpleAssignmentPattern: {
      const auto& sap =
          expr.as<slang::ast::SimpleAssignmentPatternExpression>();
      if (sap.isLValue) {
        return diag::Unsupported(
            span, diag::DiagCode::kUnsupportedAssignmentPatternKind,
            "assignment pattern as LHS destructuring is not yet supported",
            diag::UnsupportedCategory::kOperation);
      }
      return LowerAssignmentPatternFromElementsStructural(
          unit_facts, unit_state, scope_state, stack, sap, expr, span);
    }

    case slang::ast::ExpressionKind::StructuredAssignmentPattern: {
      if (expr.type->getCanonicalType().kind ==
          slang::ast::SymbolKind::DynamicArrayType) {
        return diag::Unsupported(
            span, diag::DiagCode::kUnsupportedAssignmentPatternKind,
            "index-keyed assignment pattern on a dynamic array is not yet "
            "supported; use positional form",
            diag::UnsupportedCategory::kOperation);
      }
      return LowerAssignmentPatternFromElementsStructural(
          unit_facts, unit_state, scope_state, stack,
          expr.as<slang::ast::StructuredAssignmentPatternExpression>(), expr,
          span);
    }

    case slang::ast::ExpressionKind::ReplicatedAssignmentPattern:
      return LowerReplicatedAssignmentPatternExprStructural(
          unit_facts, unit_state, scope_state, stack,
          expr.as<slang::ast::ReplicatedAssignmentPatternExpression>(), expr,
          span);

    default:
      return diag::Unsupported(
          span, diag::DiagCode::kUnsupportedStructuralExpressionForm,
          "this structural expression form is not supported yet",
          diag::UnsupportedCategory::kFeature);
  }
}

auto ValidateAssignableSlangExpr(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    const ProcessLoweringState* proc_state, const slang::ast::Expression& expr)
    -> diag::Result<void> {
  using EK = slang::ast::ExpressionKind;
  const auto& mapper = unit_facts.SourceMapper();
  const auto span = mapper.SpanOf(expr.sourceRange);
  auto reject = [&](std::string_view why) -> diag::Result<void> {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedAssignmentTarget, std::string{why},
        diag::UnsupportedCategory::kFeature);
  };

  switch (expr.kind) {
    case EK::NamedValue: {
      const auto& nv = expr.as<slang::ast::NamedValueExpression>();
      const auto& sym = nv.symbol;
      // A subroutine formal (LRM 13.5) is a VariableSymbol subclass with its
      // own SymbolKind; an `output` / `inout` formal is a legal assignment
      // target inside the body, resolved through the same procedural-var
      // binding as a body local.
      if (sym.kind != slang::ast::SymbolKind::Variable &&
          sym.kind != slang::ast::SymbolKind::FormalArgument) {
        return reject("assignment target must be a variable reference");
      }
      const auto& var = sym.as<slang::ast::VariableSymbol>();
      if (unit_state.LookupLoopVarBinding(var).has_value()) {
        return reject(
            "generate loop variable is not a legal assignment target");
      }
      if (proc_state == nullptr) {
        // Continuous-assign context (LRM 10.3): the target must resolve to
        // a structural var; procedural locals declared inside a process /
        // subroutine are unreachable here.
        if (!unit_state.LookupStructuralVarBinding(var).has_value()) {
          return reject(
              "continuous-assignment target must be a structural variable");
        }
      }
      return {};
    }
    case EK::ElementSelect:
      return ValidateAssignableSlangExpr(
          unit_facts, unit_state, proc_state,
          expr.as<slang::ast::ElementSelectExpression>().value());
    case EK::RangeSelect:
      return ValidateAssignableSlangExpr(
          unit_facts, unit_state, proc_state,
          expr.as<slang::ast::RangeSelectExpression>().value());
    case EK::MemberAccess: {
      const auto& ma = expr.as<slang::ast::MemberAccessExpression>();
      if (ma.member.kind != slang::ast::SymbolKind::Field) {
        return reject("member access target is not a struct field");
      }
      return ValidateAssignableSlangExpr(
          unit_facts, unit_state, proc_state, ma.value());
    }
    case EK::HierarchicalValue: {
      // A downward cross-unit write (`c.x = ...`, `m.l.x = ...`). The path
      // guards and the slot creation run during lowering
      // (LowerHierarchicalValueProc on the assignment target); validation only
      // confirms the target is a variable. A continuous-assignment context has
      // no process, and cross-unit continuous-assign targets are deferred.
      const auto& hv = expr.as<slang::ast::HierarchicalValueExpression>();
      if (hv.symbol.kind != slang::ast::SymbolKind::Variable) {
        return reject("assignment target must be a variable reference");
      }
      if (proc_state == nullptr) {
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
          return diag::Unsupported(
              op_span, diag::DiagCode::kUnsupportedAssignmentTarget,
              "replication is not allowed inside a destructuring "
              "assignment target (LRM 11.4.12.1)",
              diag::UnsupportedCategory::kFeature);
        }
        auto sub = ValidateAssignableSlangExpr(
            unit_facts, unit_state, proc_state, *op);
        if (!sub) return sub;
      }
      return {};
    }
    default:
      return reject("assignment target is not supported yet");
  }
}

}  // namespace lyra::lowering::ast_to_hir
