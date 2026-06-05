#include "lyra/lowering/ast_to_hir/port_connection.hpp"

#include <cstdint>
#include <expected>
#include <string>
#include <utility>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/Scope.h>
#include <slang/ast/SemanticFacts.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/PortSymbols.h>
#include <slang/ast/symbols/ValueSymbol.h>
#include <slang/ast/types/Type.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/kind.hpp"
#include "lyra/hir/continuous_assign.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/ast_to_hir/expression/lower.hpp"
#include "lyra/lowering/ast_to_hir/sensitivity.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

auto PortConnectionUnsupported(diag::SourceSpan span, std::string message)
    -> diag::Result<void> {
  return diag::Unsupported(
      span, diag::DiagCode::kUnsupportedPortConnectionForm, std::move(message),
      diag::UnsupportedCategory::kFeature);
}

// Descends nested array dimensions to the per-element instance.
auto ArrayElementInstance(const slang::ast::InstanceArraySymbol& array)
    -> const slang::ast::InstanceSymbol* {
  if (array.elements.empty()) {
    return nullptr;
  }
  const auto* first = array.elements.front();
  if (first->kind == slang::ast::SymbolKind::InstanceArray) {
    return ArrayElementInstance(first->as<slang::ast::InstanceArraySymbol>());
  }
  return &first->as<slang::ast::InstanceSymbol>();
}

auto LowerInstancePortConnectionsInto(
    const UnitLoweringFacts& unit_facts, ScopeLoweringState& scope_state,
    ScopeStack& stack, const slang::ast::InstanceSymbol& inst)
    -> diag::Result<void> {
  auto& unit_state = scope_state.UnitState();
  const auto span = unit_facts.SourceMapper().PointSpanOf(inst.location);

  // The instance member is bound in the pre-pass; a downward port reach cannot
  // miss it, so absence is a compiler-bug invariant.
  const auto binding = unit_state.LookupInstanceMemberBinding(inst);
  if (!binding.has_value()) {
    throw InternalError(
        "LowerInstancePortConnectionsInto: instance member has no binding");
  }

  for (const auto* conn : inst.getPortConnections()) {
    // An interface port's connection symbol is an InterfacePortSymbol, not a
    // PortSymbol; casting it as one is undefined. Interface ports are a
    // separate workstream (compilation_unit_model.md).
    if (conn->port.kind != slang::ast::SymbolKind::Port) {
      return PortConnectionUnsupported(
          span,
          "interface or non-variable port connection is not yet supported");
    }
    const auto& port = conn->port.as<slang::ast::PortSymbol>();

    if (port.direction == slang::ast::ArgumentDirection::Ref) {
      return PortConnectionUnsupported(
          span, "ref port connection is not yet supported");
    }
    if (port.direction == slang::ast::ArgumentDirection::InOut) {
      return PortConnectionUnsupported(
          span, "inout port connection is not yet supported");
    }
    if (port.isNetPort()) {
      return PortConnectionUnsupported(
          span, "net-typed port connection is not yet supported");
    }
    const auto* internal =
        port.internalSymbol == nullptr
            ? nullptr
            : port.internalSymbol->as_if<slang::ast::ValueSymbol>();
    if (internal == nullptr) {
      return PortConnectionUnsupported(
          span,
          "port not bound to a connectable variable is not yet supported");
    }
    const auto& port_type = port.getType();
    if (!port_type.isIntegral()) {
      return PortConnectionUnsupported(
          span, "non-integral port connection is not yet supported");
    }
    const auto* expr = conn->getExpression();
    if (expr == nullptr) {
      return PortConnectionUnsupported(
          span, "unconnected port default value is not yet supported");
    }

    auto type_id = unit_state.GetTypeId(port_type, span);
    if (!type_id) return std::unexpected(std::move(type_id.error()));
    hir::Expr child_ref = MakeCrossUnitMemberRef(
        unit_state, *internal, binding->home_frame,
        hir::DownwardHead{.instance = binding->member_id},
        {hir::MemberHop{std::string{internal->name}}}, *type_id, span);

    if (port.direction == slang::ast::ArgumentDirection::In) {
      // Input: the parent-side source drives the child port.
      auto rhs_or = LowerStructuralExpr(
          unit_facts, unit_state, scope_state, stack, *expr);
      if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
      const hir::ExprId lhs_id = scope_state.AddExpr(std::move(child_ref));
      const hir::ExprId rhs_id = scope_state.AddExpr(*std::move(rhs_or));
      const auto& reads = unit_facts.Sensitivity().AnalyzeReads(*expr, inst);
      scope_state.AddContinuousAssign(
          hir::ContinuousAssign{
              .span = span,
              .lhs = lhs_id,
              .rhs = rhs_id,
              .sensitivity_list =
                  TranslateSensitivityReads(reads, unit_state, stack)});
      continue;
    }

    // Output: slang models the connection as `parent_target = <port>`, with the
    // port value standing in as an EmptyArgument. The child port drives the
    // parent target.
    if (expr->kind != slang::ast::ExpressionKind::Assignment) {
      throw InternalError(
          "LowerInstancePortConnectionsInto: output port connection expression "
          "is "
          "not an assignment");
    }
    const auto& assign = expr->as<slang::ast::AssignmentExpression>();
    auto lhs_or = LowerStructuralExpr(
        unit_facts, unit_state, scope_state, stack, assign.left());
    if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
    const hir::ExprId lhs_id = scope_state.AddExpr(*std::move(lhs_or));
    const hir::ExprId rhs_id = scope_state.AddExpr(std::move(child_ref));
    const std::uint64_t width = port_type.getBitWidth();
    const std::vector<SensitivityRead> reads{
        SensitivityRead{.symbol = internal, .bit_range = {0, width - 1}}};
    scope_state.AddContinuousAssign(
        hir::ContinuousAssign{
            .span = span,
            .lhs = lhs_id,
            .rhs = rhs_id,
            .sensitivity_list =
                TranslateSensitivityReads(reads, unit_state, stack)});
  }
  return {};
}

}  // namespace

auto LowerScopePortConnectionsInto(
    const UnitLoweringFacts& unit_facts, ScopeLoweringState& scope_state,
    ScopeStack& stack, const slang::ast::Scope& slang_scope)
    -> diag::Result<void> {
  for (const auto& member : slang_scope.members()) {
    if (member.kind == slang::ast::SymbolKind::Instance) {
      auto r = LowerInstancePortConnectionsInto(
          unit_facts, scope_state, stack,
          member.as<slang::ast::InstanceSymbol>());
      if (!r) return std::unexpected(std::move(r.error()));
    } else if (member.kind == slang::ast::SymbolKind::InstanceArray) {
      const auto& array = member.as<slang::ast::InstanceArraySymbol>();
      const auto* element = ArrayElementInstance(array);
      if (element != nullptr && !element->getPortConnections().empty()) {
        return PortConnectionUnsupported(
            unit_facts.SourceMapper().PointSpanOf(array.location),
            "instance-array port connection is not yet supported");
      }
    }
  }
  return {};
}

}  // namespace lyra::lowering::ast_to_hir
