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
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/NetType.h>
#include <slang/ast/types/Type.h>
#include <slang/numeric/ConstantValue.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/ast_to_hir/constant_value.hpp"
#include "lyra/lowering/ast_to_hir/sensitivity.hpp"
#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

auto PortConnectionUnsupported(diag::SourceSpan span, std::string message)
    -> diag::Result<void> {
  return diag::Fail(
      span, diag::DiagCode::kUnsupportedPortConnectionForm, std::move(message));
}

// Records one instance's port connections as HIR. The instance is reached
// from its owning scope by `head` with `element_indices` selecting the
// element when the head is an instance array (empty for a scalar); each
// port becomes one descent segment on the head, so a connection is recorded
// the same way whether the instance stands alone or sits at `c[i][j]` in
// an array. The child port is held as one cross-unit reference and the
// connection verbatim with its direction; HIR-to-MIR realizes it
// (LRM 23.3.3).
auto ConnectElementPorts(
    StructuralScopeLowerer& scope, UnitLowerer& unit_lowerer,
    const slang::ast::InstanceSymbol& inst, hir::DownwardHead head,
    ScopeFrameId home_frame, std::vector<std::uint32_t> element_indices,
    WalkFrame frame) -> diag::Result<void> {
  head.head_indices = std::move(element_indices);
  const auto span = unit_lowerer.SourceMapper().PointSpanOf(inst.location);

  for (const auto* conn : inst.getPortConnections()) {
    if (conn->port.kind != slang::ast::SymbolKind::Port) {
      return PortConnectionUnsupported(
          span,
          "interface or non-variable port connection is not yet supported");
    }
    const auto& port = conn->port.as<slang::ast::PortSymbol>();
    if (port.direction == slang::ast::ArgumentDirection::InOut) {
      return PortConnectionUnsupported(
          span, "inout port connection is not yet supported");
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
    auto type_id = unit_lowerer.InternType(port.getType(), span);
    if (!type_id) return std::unexpected(std::move(type_id.error()));
    if (!unit_lowerer.Unit().types.Get(*type_id).IsValueChangeObservable()) {
      return PortConnectionUnsupported(
          span,
          "port connection of a handle / event type is not yet supported");
    }
    const auto* expr = conn->getExpression();
    if (expr == nullptr) {
      // Unconnected: an explicit empty connection (`.port()`) or an omitted
      // input port with no default. The child's storage holds the data type's
      // default initial value (LRM 23.3.3.2); no parent driver is installed.
      continue;
    }
    // A net port's child member is a resolved-net cell, reached across the
    // boundary like any cross-unit cell (LRM 6.5, 23.3.3); HIR-to-MIR realizes
    // the connection as a reactive edge whose net side attaches a driver. Only
    // the `wire` / `tri` resolution is supported; other net types are rejected.
    if (port.isNetPort()) {
      switch (internal->as<slang::ast::NetSymbol>().netType.netKind) {
        case slang::ast::NetType::Wire:
        case slang::ast::NetType::Tri:
          break;
        default:
          return PortConnectionUnsupported(
              span, "this net type is not yet supported");
      }
    }

    const bool is_ref = port.direction == slang::ast::ArgumentDirection::Ref;
    if (is_ref) {
      // A `const ref` port shares storage but forbids the child writing through
      // it (LRM 23.3.3.2); the child member is a read-only reference the parent
      // still rebinds at construction, a storage shape distinct from the
      // rebindable plain `ref`, so it waits for its own cut.
      if (const auto* iv = internal->as_if<slang::ast::VariableSymbol>();
          iv != nullptr && iv->flags.has(slang::ast::VariableFlags::Const)) {
        return PortConnectionUnsupported(
            span, "const ref port connection is not yet supported");
      }
    }

    std::vector<hir::PathSegment> path;
    path.push_back(
        hir::PathSegment{.name = std::string{internal->name}, .indices = {}});
    // An input/output port reads the child cell during simulation, so it holds
    // a persistent routed reference; a `ref` port is bound once in the resolve
    // phase, so it keeps only the by-name reach.
    const auto cell_endpoint = [&]() -> hir::PortEndpoint {
      return hir::PortCellEndpoint{
          .cell = frame.Exprs().Add(unit_lowerer.MakeRoutedMemberRef(
              *internal, home_frame, head, path, *type_id, span))};
    };

    hir::PortDirection direction{};
    hir::PortEndpoint endpoint;
    hir::ExprId peer{};
    std::vector<hir::SensitivityEntry> sensitivity;

    switch (port.direction) {
      case slang::ast::ArgumentDirection::In: {
        direction = hir::PortDirection::kInput;
        endpoint = cell_endpoint();
        if (expr == port.getInitializer()) {
          // An omitted input port takes its declared default (LRM 23.2.2.4),
          // which slang surfaces through getExpression() as the port's own
          // getInitializer(); the default's names resolve in the child, so its
          // already-evaluated constant is spliced in and driven once with no
          // sensitivity, like a defaulted argument at a call site.
          const auto* constant = expr->getConstant();
          if (constant == nullptr) {
            throw InternalError(
                "ConnectElementPorts: port default did not fold to a constant");
          }
          auto peer_or = MakeConstantValueExpr(
              unit_lowerer.Unit(), frame, *constant, *type_id, span);
          if (!peer_or) return std::unexpected(std::move(peer_or.error()));
          peer = frame.Exprs().Add(*std::move(peer_or));
        } else {
          auto peer_or = scope.LowerExpr(*expr, frame);
          if (!peer_or) return std::unexpected(std::move(peer_or.error()));
          peer = frame.Exprs().Add(*std::move(peer_or));
          sensitivity = unit_lowerer.TranslateSensitivityReads(
              unit_lowerer.Sensitivity().AnalyzeReads(*expr, inst), frame);
        }
        break;
      }
      case slang::ast::ArgumentDirection::Out: {
        direction = hir::PortDirection::kOutput;
        endpoint = cell_endpoint();
        // slang models an output connection as `parent_target = <port>`, the
        // port value standing in as an EmptyArgument; the parent target is the
        // assignment's left side. The connection observes the child's whole
        // internal signal on any change.
        if (expr->kind != slang::ast::ExpressionKind::Assignment) {
          throw InternalError(
              "ConnectElementPorts: output port connection expression is not "
              "an "
              "assignment");
        }
        auto peer_or = scope.LowerExpr(
            expr->as<slang::ast::AssignmentExpression>().left(), frame);
        if (!peer_or) return std::unexpected(std::move(peer_or.error()));
        peer = frame.Exprs().Add(*std::move(peer_or));
        sensitivity = unit_lowerer.TranslateSensitivityReads(
            {SensitivityRead{.symbol = internal, .footprint = std::nullopt}},
            frame);
        break;
      }
      case slang::ast::ArgumentDirection::Ref: {
        direction = hir::PortDirection::kRef;
        endpoint = hir::RoutedPathRecipe{
            .head = head, .path = std::move(path), .type = *type_id};
        auto peer_or = scope.LowerExpr(*expr, frame);
        if (!peer_or) return std::unexpected(std::move(peer_or.error()));
        peer = frame.Exprs().Add(*std::move(peer_or));
        break;
      }
      case slang::ast::ArgumentDirection::InOut:
        throw InternalError(
            "ConnectElementPorts: inout reached the connection switch");
    }

    frame.current_structural_scope->port_connections.push_back(
        hir::PortConnection{
            .span = span,
            .direction = direction,
            .endpoint = std::move(endpoint),
            .peer = peer,
            .sensitivity = std::move(sensitivity)});
  }
  return {};
}

// Walks an instance array's elements, extending `index_prefix` by one index
// per dimension, and records each leaf element's port connections. slang
// distributes the connection per element (LRM 23.3.3.5), so each element
// carries its own already index-matched connection expressions; this only
// routes each to the right cell.
auto ConnectArrayElements(
    StructuralScopeLowerer& scope, UnitLowerer& unit_lowerer,
    const slang::ast::InstanceArraySymbol& array, hir::DownwardHead head,
    ScopeFrameId home_frame, const std::vector<std::uint32_t>& index_prefix,
    WalkFrame frame) -> diag::Result<void> {
  for (std::uint32_t i = 0; i < array.elements.size(); ++i) {
    std::vector<std::uint32_t> element_prefix = index_prefix;
    element_prefix.push_back(i);
    const auto* element = array.elements[i];
    if (element->kind == slang::ast::SymbolKind::InstanceArray) {
      auto r = ConnectArrayElements(
          scope, unit_lowerer, element->as<slang::ast::InstanceArraySymbol>(),
          head, home_frame, element_prefix, frame);
      if (!r) return std::unexpected(std::move(r.error()));
      continue;
    }
    auto r = ConnectElementPorts(
        scope, unit_lowerer, element->as<slang::ast::InstanceSymbol>(), head,
        home_frame, std::move(element_prefix), frame);
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
      const auto binding = owner_->LookupOwnedChildBinding(member);
      if (!binding.has_value()) {
        throw InternalError(
            "PopulatePortConnections: instance member has no binding");
      }
      auto r = ConnectElementPorts(
          *this, *owner_, member.as<slang::ast::InstanceSymbol>(),
          binding->head, binding->home_frame, {}, frame);
      if (!r) return std::unexpected(std::move(r.error()));
    } else if (member.kind == slang::ast::SymbolKind::InstanceArray) {
      // A zero-element array (`Child c[0]`, LRM 23.3.2) constructs no element
      // and binds no member, so there is nothing to connect.
      const auto binding = owner_->LookupOwnedChildBinding(member);
      if (!binding.has_value()) {
        continue;
      }
      auto r = ConnectArrayElements(
          *this, *owner_, member.as<slang::ast::InstanceArraySymbol>(),
          binding->head, binding->home_frame, {}, frame);
      if (!r) return std::unexpected(std::move(r.error()));
    }
  }
  return {};
}

}  // namespace lyra::lowering::ast_to_hir
