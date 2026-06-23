#include <cstdint>
#include <expected>
#include <optional>
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
#include <slang/numeric/ConstantValue.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/kind.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/ast_to_hir/constant_value.hpp"
#include "lyra/lowering/ast_to_hir/continuous_assign.hpp"
#include "lyra/lowering/ast_to_hir/module_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/sensitivity.hpp"
#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

auto PortConnectionUnsupported(diag::SourceSpan span, std::string message)
    -> diag::Result<void> {
  return diag::Unsupported(
      span, diag::DiagCode::kUnsupportedPortConnectionForm, std::move(message),
      diag::UnsupportedCategory::kFeature);
}

// Connects one instance's ports, the instance reached from its owning scope by
// navigating `head` then `index_prefix` (empty for a scalar instance, one
// `IndexHop` per dimension for an array element). Each port appends its own
// member hop, so a connection is the same continuous assignment whether the
// instance stands alone or sits at `c[i][j]` in an array.
auto ConnectElementPorts(
    StructuralScopeLowerer& scope, ModuleLowerer& module,
    const slang::ast::InstanceSymbol& inst, hir::DownwardHead head,
    ScopeFrameId home_frame, const std::vector<hir::PathStep>& index_prefix,
    WalkFrame frame) -> diag::Result<void> {
  const auto span = module.SourceMapper().PointSpanOf(inst.location);

  for (const auto* conn : inst.getPortConnections()) {
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
    auto type_id = module.InternType(port_type, span);
    if (!type_id) return std::unexpected(std::move(type_id.error()));
    if (!module.Unit().GetType(*type_id).IsValueChangeObservable()) {
      return PortConnectionUnsupported(
          span,
          "port connection of a handle / event type is not yet supported");
    }
    const auto* expr = conn->getExpression();
    if (expr == nullptr) {
      // Unconnected: an explicit empty connection (`.port()`) or a port omitted
      // with no default. The child's storage holds the data type's default
      // initial value (LRM 23.3.3.2); no parent driver is installed.
      continue;
    }

    std::vector<hir::PathStep> path = index_prefix;
    path.emplace_back(hir::MemberHop{std::string{internal->name}});
    hir::Expr child_ref = module.MakeCrossUnitMemberRef(
        *internal, home_frame, head, std::move(path), *type_id, span);

    if (port.direction == slang::ast::ArgumentDirection::In) {
      // The child port is driven by the connection's value: the parent-side
      // expression, or -- when the port was omitted -- the declared default
      // (LRM 23.2.2.4), which slang surfaces through getExpression() as the
      // port's own getInitializer() pointer. The default's names resolve in the
      // child, so it cannot be lowered in the parent frame; like a C++ default
      // argument, its already-evaluated constant is spliced in here and driven
      // once with no sensitivity.
      hir::Expr rhs;
      std::vector<SensitivityRead> reads;
      if (expr == port.getInitializer()) {
        const auto* constant = expr->getConstant();
        if (constant == nullptr) {
          throw InternalError(
              "ConnectElementPorts: port default did not fold to a "
              "constant");
        }
        auto rhs_or = MakeConstantValueExpr(*constant, *type_id, span);
        if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
        rhs = *std::move(rhs_or);
      } else {
        auto rhs_or = scope.LowerExpr(*expr, frame);
        if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
        rhs = *std::move(rhs_or);
        reads = module.Sensitivity().AnalyzeReads(*expr, inst);
      }
      frame.current_structural_scope->continuous_assigns.Add(
          BuildContinuousAssign(
              module, frame, span, std::move(child_ref), std::move(rhs),
              reads));
      continue;
    }

    // Output: slang models the connection as `parent_target = <port>`, with
    // the port value standing in as an EmptyArgument. The child port drives
    // the parent target.
    if (expr->kind != slang::ast::ExpressionKind::Assignment) {
      throw InternalError(
          "ConnectElementPorts: output port connection expression "
          "is not an assignment");
    }
    const auto& assign = expr->as<slang::ast::AssignmentExpression>();
    auto lhs_or = scope.LowerExpr(assign.left(), frame);
    if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
    // The output port observes the child's whole internal signal on any change.
    // It is not a bit-addressed read, so it carries no footprint regardless of
    // the port's data type.
    const std::vector<SensitivityRead> reads{
        SensitivityRead{.symbol = internal, .footprint = std::nullopt}};
    frame.current_structural_scope->continuous_assigns.Add(
        BuildContinuousAssign(
            module, frame, span, *std::move(lhs_or), std::move(child_ref),
            reads));
  }
  return {};
}

// Walks an instance array's elements, extending `index_prefix` by one
// `IndexHop` per dimension, and connects each leaf element's ports. slang
// distributes the connection per element (LRM 23.3.3.5), so each element
// carries its own already index-matched connection expressions; this only
// routes each to the right cell.
auto ConnectArrayElements(
    StructuralScopeLowerer& scope, ModuleLowerer& module,
    const slang::ast::InstanceArraySymbol& array, hir::DownwardHead head,
    ScopeFrameId home_frame, const std::vector<hir::PathStep>& index_prefix,
    WalkFrame frame) -> diag::Result<void> {
  for (std::uint32_t i = 0; i < array.elements.size(); ++i) {
    std::vector<hir::PathStep> element_prefix = index_prefix;
    element_prefix.emplace_back(hir::IndexHop{i});
    const auto* element = array.elements[i];
    if (element->kind == slang::ast::SymbolKind::InstanceArray) {
      auto r = ConnectArrayElements(
          scope, module, element->as<slang::ast::InstanceArraySymbol>(), head,
          home_frame, element_prefix, frame);
      if (!r) return std::unexpected(std::move(r.error()));
      continue;
    }
    auto r = ConnectElementPorts(
        scope, module, element->as<slang::ast::InstanceSymbol>(), head,
        home_frame, element_prefix, frame);
    if (!r) return std::unexpected(std::move(r.error()));
  }
  return {};
}

}  // namespace

auto StructuralScopeLowerer::PopulatePortConnections(
    const slang::ast::Scope& slang_scope, WalkFrame frame)
    -> diag::Result<void> {
  for (const auto& member : slang_scope.members()) {
    if (member.kind == slang::ast::SymbolKind::Instance) {
      // The instance member is bound in the pre-pass; a downward port reach
      // cannot miss it, so absence is a compiler-bug invariant.
      const auto binding = module_->LookupOwnedChildBinding(member);
      if (!binding.has_value()) {
        throw InternalError(
            "PopulatePortConnections: instance member has no binding");
      }
      auto r = ConnectElementPorts(
          *this, *module_, member.as<slang::ast::InstanceSymbol>(),
          binding->head, binding->home_frame, {}, frame);
      if (!r) return std::unexpected(std::move(r.error()));
    } else if (member.kind == slang::ast::SymbolKind::InstanceArray) {
      // A zero-element array (`Child c[0]`, LRM 23.3.2) constructs no element
      // and binds no member, so there is nothing to connect.
      const auto binding = module_->LookupOwnedChildBinding(member);
      if (!binding.has_value()) {
        continue;
      }
      auto r = ConnectArrayElements(
          *this, *module_, member.as<slang::ast::InstanceArraySymbol>(),
          binding->head, binding->home_frame, {}, frame);
      if (!r) return std::unexpected(std::move(r.error()));
    }
  }
  return {};
}

}  // namespace lyra::lowering::ast_to_hir
