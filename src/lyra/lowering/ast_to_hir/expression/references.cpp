#include "lyra/lowering/ast_to_hir/expression/references.hpp"

#include <cstdint>
#include <expected>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/HierarchicalReference.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/MemberSymbols.h>
#include <slang/ast/symbols/ParameterSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>
#include <slang/numeric/ConstantValue.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/kind.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/ast_to_hir/integral_constant.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// HIR primitive constructor used by every named-value reference. Local to the
// references subsystem (no other consumer); other subsystems do not need it.
auto MakeRefExpr(hir::Primary ref, hir::TypeId type, diag::SourceSpan span)
    -> hir::Expr {
  return hir::Expr{
      .type = type,
      .data = hir::PrimaryExpr{.data = std::move(ref)},
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

}  // namespace

auto LowerNamedValueProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::NamedValueExpression& named) -> diag::Result<hir::Expr> {
  auto& module = proc.Module();
  const auto& mapper = module.SourceMapper();
  const auto span = mapper.SpanOf(named.sourceRange);
  const auto& sym = named.symbol;

  if (sym.kind == slang::ast::SymbolKind::EnumValue) {
    auto type_id = module.InternType(*named.type, span);
    if (!type_id) return std::unexpected(std::move(type_id.error()));
    return MakeEnumValueExpr(
        sym.as<slang::ast::EnumValueSymbol>(), *type_id, span);
  }

  if (sym.kind == slang::ast::SymbolKind::Variable ||
      sym.kind == slang::ast::SymbolKind::Parameter) {
    const auto& value_sym = sym.as<slang::ast::ValueSymbol>();
    if (auto loop_binding = module.LookupLoopVarBinding(value_sym)) {
      const auto hops = frame.HopsTo(loop_binding->home_frame);
      if (!hops.has_value()) {
        throw InternalError(
            "LowerNamedValueProc: loop-var binding home frame is not on the "
            "current scope stack");
      }
      return MakeRefExpr(
          hir::LoopVarRef{.hops = *hops, .loop_var = loop_binding->loop_var_id},
          loop_binding->type, span);
    }
  }

  if (sym.kind == slang::ast::SymbolKind::Parameter) {
    auto type_id = module.InternType(*named.type, span);
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

  if (auto local = proc.LookupProceduralVar(var)) {
    // A branch reaches its enclosing process's variables through a by-reference
    // capture computed at HIR-to-MIR: the process activation outlives the
    // spawned branch (LRM 6.21), so the shared storage stays live. A fork
    // inside a task is deferred -- the task activation does not yet outlive its
    // spawned branches.
    if (frame.InForkBranch() &&
        proc.ContainingSymbol().kind == slang::ast::SymbolKind::Subroutine) {
      return diag::Unsupported(
          span, diag::DiagCode::kUnsupportedForkJoinForm,
          "a fork-join branch inside a task referencing a procedural variable "
          "is not yet supported",
          diag::UnsupportedCategory::kFeature);
    }
    const hir::TypeId type_id =
        frame.current_procedural_body->procedural_vars.at(local->value).type;
    return MakeRefExpr(hir::ProceduralVarRef{.var = *local}, type_id, span);
  }

  const auto binding = module.LookupStructuralVarBinding(var);
  if (!binding.has_value()) {
    throw InternalError(
        "LowerNamedValueProc: variable was not bound during scope lowering");
  }
  const auto hops = frame.HopsTo(binding->home_frame);
  if (!hops.has_value()) {
    throw InternalError(
        "LowerNamedValueProc: variable home frame is not on the current scope "
        "stack");
  }
  return MakeRefExpr(
      hir::StructuralVarRef{.hops = *hops, .var = binding->var_id},
      binding->type, span);
}

// LRM 23.6 hierarchical reference. The head of the navigation differs by
// direction: a downward path (`c.x`, `c[1].x`, `g[1].u.x`) starts at a local
// owned child -- an instance member or a generate block; an upward path
// (`Top.g`, `Top.sib.y`) starts by climbing the parent chain at construction to
// the named ancestor instance. The shared tail (`path.subspan(1)`) and the
// resolve-once slot are common (reference_resolution.md,
// docs/decisions/upward-reference-resolution.md).
auto LowerHierarchicalValueProc(
    ModuleLowerer& module, WalkFrame frame,
    const slang::ast::HierarchicalValueExpression& hve)
    -> diag::Result<hir::Expr> {
  const auto span = module.SourceMapper().SpanOf(hve.sourceRange);
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

  auto type_id = module.InternType(*hve.type, span);
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
    // An upward reference (LRM 23.8) climbs the parent chain to the named
    // ancestor, then shares `path` with the downward direction to reach the
    // leaf. A named generate / procedural block ancestor or a `$root`-anchored
    // path (neither a module instance) is not yet supported.
    if (head_sym.kind != slang::ast::SymbolKind::Instance) {
      return diag::Unsupported(
          span, diag::DiagCode::kUnsupportedExpressionForm,
          "upward hierarchical reference whose target ancestor is not a module "
          "instance is not yet supported",
          diag::UnsupportedCategory::kOperation);
    }
    // The climb key is the ancestor's module name (LRM 23.8) -- class-level, so
    // one artifact serves every instance regardless of depth. It comes from the
    // resolved ancestor symbol, never from syntax (a wrapped sub-expression
    // like `Top.g[3]` may carry none). The extern member is synthesized on the
    // referrer's own structural scope (`frame.Current()`), whether that is the
    // unit root or a generate block: its `ExternUp` climbs that object's own
    // parent chain at Bind, so a reference written inside a generate block
    // resolves the same way (its member rides the generate-scope class).
    std::string ancestor_name{
        head_sym.as<slang::ast::InstanceSymbol>().getDefinition().name};
    return module.MakeCrossUnitMemberRef(
        var, frame.Current(),
        hir::UpwardHead{.ancestor_name = std::move(ancestor_name)},
        std::move(path), *type_id, span);
  }

  // Downward: the head is an owned child this unit's scope declares -- an
  // instance / instance-array member, or a generate block (LRM 27). Both are
  // bound before any process body is lowered, so a reference resolves
  // regardless of source order; a missing binding is a compiler-bug invariant.
  const bool head_is_owned_child =
      head_sym.kind == slang::ast::SymbolKind::Instance ||
      head_sym.kind == slang::ast::SymbolKind::InstanceArray ||
      head_sym.kind == slang::ast::SymbolKind::GenerateBlock ||
      head_sym.kind == slang::ast::SymbolKind::GenerateBlockArray;
  if (!head_is_owned_child) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "hierarchical reference through this scope kind is not yet supported",
        diag::UnsupportedCategory::kOperation);
  }
  const auto binding = module.LookupOwnedChildBinding(head_sym);
  if (!binding.has_value()) {
    throw InternalError(
        "LowerHierarchicalValueProc: downward owned-child head has no binding");
  }

  // The owner navigates its own storage, so the head must live on the
  // referencing frame. A reference reaching the owned child from a nested
  // generate frame (`hops != 0`) is a separate, unsupported case.
  const auto hops = frame.HopsTo(binding->home_frame);
  if (!hops.has_value() || hops->value != 0) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "hierarchical reference reaching an owned child from a nested generate "
        "scope is not yet supported",
        diag::UnsupportedCategory::kOperation);
  }
  return module.MakeCrossUnitMemberRef(
      var, binding->home_frame, binding->head, std::move(path), *type_id, span);
}

auto LowerNamedValueStructural(
    ModuleLowerer& module, WalkFrame frame,
    const slang::ast::NamedValueExpression& named) -> diag::Result<hir::Expr> {
  const auto& mapper = module.SourceMapper();
  const auto span = mapper.SpanOf(named.sourceRange);
  const auto& sym = named.symbol;
  if (sym.kind == slang::ast::SymbolKind::EnumValue) {
    auto type_id = module.InternType(*named.type, span);
    if (!type_id) return std::unexpected(std::move(type_id.error()));
    return MakeEnumValueExpr(
        sym.as<slang::ast::EnumValueSymbol>(), *type_id, span);
  }
  if (sym.kind == slang::ast::SymbolKind::Variable ||
      sym.kind == slang::ast::SymbolKind::Parameter) {
    const auto& value_sym = sym.as<slang::ast::ValueSymbol>();
    if (auto loop_binding = module.LookupLoopVarBinding(value_sym)) {
      const auto hops = frame.HopsTo(loop_binding->home_frame);
      if (!hops.has_value()) {
        throw InternalError(
            "LowerNamedValueStructural: loop-var binding home frame is not on "
            "the current scope stack");
      }
      return MakeRefExpr(
          hir::LoopVarRef{.hops = *hops, .loop_var = loop_binding->loop_var_id},
          loop_binding->type, span);
    }
  }
  if (sym.kind == slang::ast::SymbolKind::Parameter) {
    auto type_id = module.InternType(*named.type, span);
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
  const auto binding = module.LookupStructuralVarBinding(var);
  if (!binding.has_value()) {
    throw InternalError(
        "LowerNamedValueStructural: variable was not bound during scope "
        "lowering");
  }
  const auto hops = frame.HopsTo(binding->home_frame);
  if (!hops.has_value()) {
    throw InternalError(
        "LowerNamedValueStructural: variable home frame is not on the current "
        "scope stack");
  }
  return MakeRefExpr(
      hir::StructuralVarRef{.hops = *hops, .var = binding->var_id},
      binding->type, span);
}

}  // namespace lyra::lowering::ast_to_hir
