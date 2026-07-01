#include "lyra/lowering/ast_to_hir/expression/references.hpp"

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
#include <slang/ast/Symbol.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/symbols/ClassSymbols.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/MemberSymbols.h>
#include <slang/ast/symbols/ParameterSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>
#include <slang/numeric/ConstantValue.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/hir/expr_builders.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/ast_to_hir/constant_value.hpp"
#include "lyra/lowering/ast_to_hir/expression/selects.hpp"
#include "lyra/lowering/ast_to_hir/integral_constant.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

auto MakeEnumValueExpr(
    const slang::ast::EnumValueSymbol& sym, hir::TypeId type,
    diag::SourceSpan span) -> hir::Expr {
  const auto& cv = sym.getValue();
  if (!cv.isInteger()) {
    throw InternalError("MakeEnumValueExpr: enum value is not integral");
  }
  return MakeIntegralLiteralExpr(cv.integer(), type, span);
}

// LRM 7.12.4: a reference to an array-method `with`-clause iteration element
// (`item`) lowers to an `IterationBindingRef` naming `clause` and the element
// role, typed by its own reference type. The element is one of the clause's two
// iteration parameters, not a variable of the enclosing scope, so neither pass
// class's variable storage is consulted.
auto MakeIterationElementRefExpr(
    ModuleLowerer& module, const slang::ast::NamedValueExpression& named,
    hir::WithClauseId clause, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto type_id = module.InternType(*named.type, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return MakeRefExpr(
      hir::IterationBindingRef{
          .clause = clause, .role = hir::IterationBindingRole::kElement},
      *type_id, span);
}

}  // namespace

auto LowerNamedValueProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::NamedValueExpression& named) -> diag::Result<hir::Expr> {
  auto& module = proc.Module();
  const auto& mapper = module.SourceMapper();
  const auto span = mapper.SpanOf(named.sourceRange);
  const auto& sym = named.symbol;

  if (auto clause = frame.FindIterationClause(sym)) {
    return MakeIterationElementRefExpr(module, named, *clause, span);
  }

  if (sym.kind == slang::ast::SymbolKind::EnumValue) {
    auto type_id = module.InternType(*named.type, span);
    if (!type_id) return std::unexpected(std::move(type_id.error()));
    return MakeEnumValueExpr(
        sym.as<slang::ast::EnumValueSymbol>(), *type_id, span);
  }

  // Inside an instance method, a class property named without an explicit
  // handle (LRM 8.4) reaches the invoking object through the method's receiver,
  // so it lowers to a receiver-relative property reference, not a body local.
  if (sym.kind == slang::ast::SymbolKind::ClassProperty) {
    auto type_id = module.InternType(*named.type, span);
    if (!type_id) return std::unexpected(std::move(type_id.error()));
    return hir::MakeRefExpr(
        hir::ClassPropertyRef{
            .field_index =
                ClassPropertyIndex(sym.as<slang::ast::ClassPropertySymbol>())},
        *type_id, span);
  }

  if (sym.kind == slang::ast::SymbolKind::Parameter) {
    auto type_id = module.InternType(*named.type, span);
    if (!type_id) return std::unexpected(std::move(type_id.error()));
    return MakeConstantValueExpr(
        module.Unit(), frame, sym.as<slang::ast::ParameterSymbol>().getValue(),
        *type_id, span);
  }

  // Subroutine formals (LRM 13.5) and foreach iterators (LRM 12.7.3) are
  // VariableSymbol subclasses and route through the same procedural-var
  // binding as ordinary body locals.
  // A net (LRM 6.5) is always a structural signal; the variable family may
  // also be a procedural body local, so try that binding for them first. A net
  // never has a procedural binding.
  if (sym.kind == slang::ast::SymbolKind::Variable ||
      sym.kind == slang::ast::SymbolKind::FormalArgument ||
      sym.kind == slang::ast::SymbolKind::Iterator) {
    const auto& var = sym.as<slang::ast::VariableSymbol>();
    if (auto local = proc.LookupProceduralVar(var)) {
      const hir::TypeId type =
          frame.current_procedural_body->procedural_vars.Get(*local).type;
      return hir::MakeRefExpr(hir::ProceduralVarRef{.var = *local}, type, span);
    }
  } else if (sym.kind != slang::ast::SymbolKind::Net) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedNonVariableNamedReference,
        "reference to non-variable declaration is not supported");
  }

  const auto binding = module.LookupStructuralDataObjectBinding(
      sym.as<slang::ast::ValueSymbol>());
  if (!binding.has_value()) {
    throw InternalError(
        "LowerNamedValueProc: value was not bound during scope lowering");
  }
  const auto hops = frame.HopsTo(binding->home_frame);
  if (!hops.has_value()) {
    throw InternalError(
        "LowerNamedValueProc: variable home frame is not on the current scope "
        "stack");
  }
  return hir::MakeRefExpr(
      hir::StructuralDataObjectRef{.hops = *hops, .var = binding->var_id},
      binding->type, span);
}

// LRM 23.6 hierarchical reference. A downward path is rooted in a local
// owned child identified by binding; an upward path carries an anchor (a
// `$root` token or a canonical named head with any per-dimension index) and
// a descent suffix that walks by name from the anchor down to the leaf
// signal.
auto LowerHierarchicalValue(
    ModuleLowerer& module, WalkFrame frame,
    const slang::ast::HierarchicalValueExpression& hve)
    -> diag::Result<hir::Expr> {
  const auto span = module.SourceMapper().SpanOf(hve.sourceRange);
  const auto& ref = hve.ref;

  if (ref.isViaIfacePort()) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "interface-port hierarchical reference is not yet supported");
  }

  const auto& target = hve.symbol;
  if (target.kind != slang::ast::SymbolKind::Variable) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "cross-unit reference to a non-variable declaration is not yet "
        "supported");
  }

  auto type_id = module.InternType(*hve.type, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));

  const auto& var = target.as<slang::ast::VariableSymbol>();
  const slang::ast::Symbol& head_sym = *ref.path.front().symbol;

  // ref.path is slang's resolved top-down navigation: path.front() is the
  // head, path.back() is `target`, and each name selector is the resolved
  // symbol's canonical name. Carrying those selectors verbatim into the HIR
  // descent makes the runtime by-name navigation hit each scope by the same
  // key the parent registered under.
  const auto build_path =
      [&](std::span<const slang::ast::HierarchicalReference::Element> steps)
      -> diag::Result<std::vector<hir::PathStep>> {
    std::vector<hir::PathStep> path;
    path.reserve(steps.size());
    for (const auto& step : steps) {
      if (std::holds_alternative<std::string_view>(step.selector)) {
        path.emplace_back(
            hir::MemberHop{
                std::string{std::get<std::string_view>(step.selector)}});
      } else if (std::holds_alternative<std::int32_t>(step.selector)) {
        path.emplace_back(
            hir::IndexHop{static_cast<std::uint32_t>(
                std::get<std::int32_t>(step.selector))});
      } else {
        return diag::Fail(
            span, diag::DiagCode::kUnsupportedExpressionForm,
            "instance-array range select in a hierarchical path is not yet "
            "supported");
      }
    }
    return path;
  };

  if (ref.isUpward()) {
    // The named arm carries the canonical head name plus any per-dimension
    // index that immediately follows it in `ref.path` (e.g. `bank[2].x`); the
    // root arm carries no key. The suffix is `ref.path` strictly past the
    // head element(s). Synthesizing the extern member on the referrer's own
    // structural scope keeps the lexical lookup origin coincident with the
    // structural class that owns this member, including for references
    // written inside a generate block.
    hir::CrossUnitRefHead anchor;
    std::span<const slang::ast::HierarchicalReference::Element> suffix_steps;
    switch (head_sym.kind) {
      case slang::ast::SymbolKind::Instance:
      case slang::ast::SymbolKind::GenerateBlock: {
        // Gather the head's per-dimension indices from the index selectors
        // immediately following `path[0]`; the next name selector marks the
        // start of the descent suffix.
        hir::UpwardNamedHead named{
            .head_name = std::string{head_sym.name}, .head_indices = {}};
        std::size_t suffix_start = 1;
        while (suffix_start < ref.path.size() &&
               std::holds_alternative<std::int32_t>(
                   ref.path[suffix_start].selector)) {
          named.head_indices.push_back(
              static_cast<std::uint32_t>(
                  std::get<std::int32_t>(ref.path[suffix_start].selector)));
          ++suffix_start;
        }
        anchor = std::move(named);
        suffix_steps = ref.path.subspan(suffix_start);
        break;
      }
      case slang::ast::SymbolKind::Root:
        anchor = hir::UpwardRootHead{};
        suffix_steps = ref.path.subspan(1);
        break;
      default:
        return diag::Fail(
            span, diag::DiagCode::kUnsupportedExpressionForm,
            "upward hierarchical reference whose head is not a module "
            "instance, "
            "a named generate block, or $root is not yet supported");
    }
    auto path = build_path(suffix_steps);
    if (!path) return std::unexpected(std::move(path.error()));
    return module.MakeCrossUnitMemberRef(
        var, frame.Current(), std::move(anchor), std::move(*path), *type_id,
        span);
  }

  auto path = build_path(ref.path.subspan(1));
  if (!path) return std::unexpected(std::move(path.error()));

  // Downward: the head is an owned child this unit's scope declares -- an
  // instance / instance-array member, a generate block (LRM 27), or a named
  // procedural block (LRM 9.3.5 / 23.9). Each is bound before any process
  // body is lowered, so a reference resolves regardless of source order; a
  // missing binding is a compiler-bug invariant.
  const bool head_is_owned_child =
      head_sym.kind == slang::ast::SymbolKind::Instance ||
      head_sym.kind == slang::ast::SymbolKind::InstanceArray ||
      head_sym.kind == slang::ast::SymbolKind::GenerateBlock ||
      head_sym.kind == slang::ast::SymbolKind::GenerateBlockArray ||
      head_sym.kind == slang::ast::SymbolKind::StatementBlock;
  if (!head_is_owned_child) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "hierarchical reference through this scope kind is not yet supported");
  }
  const auto binding = module.LookupOwnedChildBinding(head_sym);
  if (!binding.has_value()) {
    throw InternalError(
        "LowerHierarchicalValue: downward owned-child head has no binding");
  }

  const auto hops = frame.HopsTo(binding->home_frame);
  if (!hops.has_value()) {
    throw InternalError(
        "LowerHierarchicalValue: downward owned-child head's home frame is not "
        "on the current scope stack");
  }
  if (hops->value == 0) {
    return module.MakeCrossUnitMemberRef(
        var, binding->home_frame, binding->head, std::move(*path), *type_id,
        span);
  }

  // Sibling-of-ancestor: the head sits in an enclosing scope of the referrer.
  // For an intra-unit owned child (a generate block; its layout is part of
  // this unit's emitted artifact) the install climbs the runtime parent edge
  // and composes a typed downward MemberAccess chain on the enclosing class.
  // A module-instance head with `hops > 0` would require a hybrid typed-climb
  // / by-name-descent because the instance's body is in another unit;
  // unsupported here.
  const bool head_is_intra_unit =
      head_sym.kind == slang::ast::SymbolKind::GenerateBlock ||
      head_sym.kind == slang::ast::SymbolKind::GenerateBlockArray ||
      head_sym.kind == slang::ast::SymbolKind::StatementBlock;
  if (!head_is_intra_unit) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "hierarchical reference reaching an owned module instance from a "
        "nested generate scope is not yet supported");
  }
  hir::DownwardHead enclosing_head = binding->head;
  enclosing_head.hops = *hops;
  return module.MakeCrossUnitMemberRef(
      var, frame.Current(), std::move(enclosing_head), std::move(*path),
      *type_id, span);
}

auto LowerNamedValueStructural(
    ModuleLowerer& module, WalkFrame frame,
    const slang::ast::NamedValueExpression& named) -> diag::Result<hir::Expr> {
  const auto& mapper = module.SourceMapper();
  const auto span = mapper.SpanOf(named.sourceRange);
  const auto& sym = named.symbol;
  if (auto clause = frame.FindIterationClause(sym)) {
    return MakeIterationElementRefExpr(module, named, *clause, span);
  }
  if (sym.kind == slang::ast::SymbolKind::EnumValue) {
    auto type_id = module.InternType(*named.type, span);
    if (!type_id) return std::unexpected(std::move(type_id.error()));
    return MakeEnumValueExpr(
        sym.as<slang::ast::EnumValueSymbol>(), *type_id, span);
  }
  if (sym.kind == slang::ast::SymbolKind::Parameter) {
    auto type_id = module.InternType(*named.type, span);
    if (!type_id) return std::unexpected(std::move(type_id.error()));
    return MakeConstantValueExpr(
        module.Unit(), frame, sym.as<slang::ast::ParameterSymbol>().getValue(),
        *type_id, span);
  }
  if (sym.kind != slang::ast::SymbolKind::Variable &&
      sym.kind != slang::ast::SymbolKind::Net) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedNonVariableNamedReference,
        "reference to non-variable declaration is not supported");
  }
  const auto binding = module.LookupStructuralDataObjectBinding(
      sym.as<slang::ast::ValueSymbol>());
  if (!binding.has_value()) {
    throw InternalError(
        "LowerNamedValueStructural: value was not bound during scope "
        "lowering");
  }
  const auto hops = frame.HopsTo(binding->home_frame);
  if (!hops.has_value()) {
    throw InternalError(
        "LowerNamedValueStructural: variable home frame is not on the current "
        "scope stack");
  }
  return hir::MakeRefExpr(
      hir::StructuralDataObjectRef{.hops = *hops, .var = binding->var_id},
      binding->type, span);
}

}  // namespace lyra::lowering::ast_to_hir
