#include <cstddef>
#include <cstdint>
#include <expected>
#include <optional>
#include <ranges>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/HierarchicalReference.h>
#include <slang/ast/Scope.h>
#include <slang/ast/SemanticFacts.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/PortSymbols.h>
#include <slang/ast/symbols/ValueSymbol.h>
#include <slang/ast/types/Type.h>
#include <slang/numeric/ConstantValue.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/unit_signature.hpp"
#include "lyra/lowering/ast_to_hir/constant_value.hpp"
#include "lyra/lowering/ast_to_hir/instance_array_shape.hpp"
#include "lyra/lowering/ast_to_hir/sensitivity.hpp"
#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/unit_identity.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

auto PortConnectionUnsupported(diag::SourceSpan span, std::string message)
    -> std::unexpected<diag::Diagnostic> {
  return diag::Fail(
      span, diag::DiagCode::kUnsupportedPortConnectionForm, std::move(message));
}

// The route from this scope to a member the child published: one typed step
// onto the instance, then the position that member sits at in the object its
// signature describes (LRM 23.3.3). Every connection reaches the child's side
// this way, whichever kind of port it is.
auto PublishedMemberRecipe(
    const hir::OwnedChildStep& instance_step,
    hir::ExternalUnitObjectId child_object, hir::PublishedMemberId member,
    hir::TypeId type) -> hir::RoutedPathRecipe {
  return hir::RoutedPathRecipe{
      .head = hir::InUnitHead{.hops = {}},
      .steps = {hir::PathStep{instance_step}},
      .leaf =
          hir::SignatureMemberLeaf{.object = child_object, .member = member},
      .type = type};
}

// The route to the interface instance a connection names (LRM 25.3). An actual
// written as an interface port of a scope enclosing the connection makes that
// port the whole route: the instance behind it belongs to a unit this one
// reaches no other way, so a route around the port would describe a different
// design -- which is why the frontend's own resolution of the port to that
// instance is not what this reads. Every other actual names an instance
// somewhere on the object tree, reached by the same walk every reference
// across an instance boundary uses.
auto InterfaceActualRoute(
    UnitLowerer& unit_lowerer, const slang::ast::PortConnection& conn,
    hir::TypeId object_type, diag::SourceSpan span, WalkFrame frame)
    -> diag::Result<hir::RoutedPathRecipe> {
  const auto recipe = [&](hir::RouteHead head,
                          std::vector<hir::PathStep> steps) {
    return hir::RoutedPathRecipe{
        .head = std::move(head),
        .steps = std::move(steps),
        .leaf = hir::ScopeLeaf{},
        .type = object_type};
  };

  const slang::ast::Expression* actual = conn.getExpression();
  const auto* named =
      actual == nullptr
          ? nullptr
          : actual->as_if<slang::ast::ArbitrarySymbolExpression>();

  if (named != nullptr && named->hierRef.isViaIfacePort()) {
    const auto path = named->hierRef.path;
    // The port and nothing after it. A longer path selects something inside the
    // interface the port carries, which is not that interface.
    if (path.size() != 1) {
      return PortConnectionUnsupported(
          span,
          "an interface reached through part of another interface port is not "
          "yet supported");
    }
    const auto port = unit_lowerer.LookupInterfacePortBinding(*path[0].symbol);
    if (!port.has_value()) {
      throw InternalError(
          "InterfaceActualRoute: the actual names an interface port of a scope "
          "enclosing the connection, which this unit's own walk declared");
    }
    const auto hops = frame.HopsTo(port->home_frame);
    if (!hops.has_value()) {
      throw InternalError(
          "InterfaceActualRoute: an interface port is a member of a scope "
          "enclosing every connection that names it");
    }
    return recipe(
        hir::InUnitHead{.hops = *hops},
        {hir::PathStep{hir::InterfacePortStep{.port = port->port}}});
  }

  // Which instance an element of an instance array is given is settled while
  // the design elaborates (LRM 23.3.3.5), so the connection states it; the
  // actual's own expression names the whole array the elements were cut from.
  const slang::ast::Symbol* connected = conn.getIfaceConn().first;
  const auto* instance = connected == nullptr
                             ? nullptr
                             : connected->as_if<slang::ast::InstanceSymbol>();
  if (instance == nullptr) {
    return PortConnectionUnsupported(
        span, "this interface port connection form is not yet supported");
  }
  auto route = unit_lowerer.RouteToScope(frame, instance->body);
  if (!route.has_value()) {
    return PortConnectionUnsupported(
        span,
        "an interface port connected to an instance this scope cannot name is "
        "not yet supported");
  }
  return recipe(std::move(route->head), std::move(route->steps));
}

// Binds one interface port of a child instance to the interface instance the
// connection names (LRM 25.3). Both sides are routes resolved once in the
// resolve phase: the child's port member, reached through the step onto the
// instance, and the interface object the actual names. Nothing crosses the
// boundary as a value, so the connection installs no driver and waits on
// nothing.
auto ConnectInterfacePort(
    UnitLowerer& unit_lowerer, const hir::InterfacePortPart& published,
    const hir::UnitSignature& child_signature,
    hir::ExternalUnitObjectId child_object,
    const hir::OwnedChildStep& instance_step,
    const slang::ast::PortConnection& conn, diag::SourceSpan span,
    WalkFrame frame) -> diag::Result<void> {
  const hir::PublishedMember& member =
      hir::InstanceClassOf(child_signature).members.Get(published.member);
  // The type of what is bound is the child's own statement of which unit
  // belongs there, taken into this unit's pool, so the parent's record of the
  // connection rests on the child's promise rather than on a second reading of
  // the frontend.
  const hir::TypeId object_type =
      unit_lowerer.ImportSignatureType(child_signature, member.type);
  auto peer =
      InterfaceActualRoute(unit_lowerer, conn, object_type, span, frame);
  if (!peer) return std::unexpected(std::move(peer.error()));
  frame.current_structural_scope->port_connections.Add(
      hir::PortConnection{
          .span = span,
          .kind = hir::InterfacePortConnection{
              .endpoint = PublishedMemberRecipe(
                  instance_step, child_object, published.member, object_type),
              .peer = *std::move(peer)}});
  return {};
}

// Records one instance's port connections as HIR. The instance is reached
// from its owning scope as `child`, with `element_indices` selecting the
// element when it is an instance array (empty for a scalar); each port is a
// by-name leaf past that step, so a connection is recorded the same way
// whether the instance stands alone or sits at `c[i][j]` in an array. The
// child port is held as one cross-unit reference and the connection verbatim
// with its direction; HIR-to-MIR realizes it (LRM 23.3.3).
auto ConnectElementPorts(
    StructuralScopeLowerer& scope, UnitLowerer& unit_lowerer,
    const slang::ast::InstanceSymbol& inst,
    const hir::UnitSignature& child_signature, hir::OwnedChildRef child,
    ScopeFrameId home_frame, std::vector<std::uint32_t> element_indices,
    WalkFrame frame) -> diag::Result<void> {
  const hir::OwnedChildStep instance_step{
      .child = child, .indices = std::move(element_indices)};
  const auto span = unit_lowerer.SourceMapper().PointSpanOf(inst.location);

  // A connection reaches one part of one port, and the child states its parts
  // in the order connections arrive at them, so the two are walked in step
  // rather than searched: the direction each connection runs in is then the
  // child's own statement of it, at the granularity data actually flows.
  const auto connections = inst.getPortConnections();
  const hir::InstanceClassSignature& published_class =
      hir::InstanceClassOf(child_signature);
  const hir::ExternalUnitObjectId child_object =
      unit_lowerer.ExternalUnitObjectOf(child_signature.unit_name);
  auto published_parts = child_signature.ports |
                         std::views::transform(&hir::PortDecl::parts) |
                         std::views::join;

  std::size_t index = 0;
  for (const hir::PortPart& published : published_parts) {
    if (index >= connections.size()) {
      throw InternalError(
          "ConnectElementPorts: a unit publishes one part per connection its "
          "instances make, so the two are the same sequence");
    }
    const auto* conn = connections[index++];

    const auto* data = std::get_if<hir::DataPortPart>(&published);
    if (data == nullptr) {
      auto r = ConnectInterfacePort(
          unit_lowerer, std::get<hir::InterfacePortPart>(published),
          child_signature, child_object, instance_step, *conn, span, frame);
      if (!r) return std::unexpected(std::move(r.error()));
      continue;
    }
    if (data->direction == hir::PortDirection::kInOut) {
      return PortConnectionUnsupported(
          span, "inout port connection is not yet supported");
    }
    const auto* port = conn->port.as_if<slang::ast::PortSymbol>();
    if (port == nullptr) {
      return PortConnectionUnsupported(
          span, "non-variable port connection is not yet supported");
    }
    if (!data->member.has_value()) {
      return PortConnectionUnsupported(
          span,
          "port not bound to a connectable variable is not yet supported");
    }
    // The storage behind the part, as the child states it. Its type may be
    // wider than the part's: a port expression (LRM 23.2.2.2) connects part of
    // an internal name, and standing a projection between the connection and
    // the storage is not something the child's statement carries.
    const hir::PublishedMember& member =
        published_class.members.Get(*data->member);
    if (member.type != data->type) {
      return PortConnectionUnsupported(
          span,
          "port connected to part of an internal name is not yet supported");
    }
    const auto* internal =
        port->internalSymbol == nullptr
            ? nullptr
            : port->internalSymbol->as_if<slang::ast::ValueSymbol>();
    if (internal == nullptr) {
      throw InternalError(
          "ConnectElementPorts: the child published a member for this part, so "
          "the declaration behind it exists");
    }
    // What crosses is the type the child published, taken into this unit's own
    // pool -- so the parent's record of the connection rests on the child's
    // statement of its port and not on a second reading of the frontend.
    const hir::TypeId type_id =
        unit_lowerer.ImportSignatureType(child_signature, data->type);
    if (!unit_lowerer.Unit().types.Get(type_id).IsValueChangeObservable()) {
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

    // A `const ref` port shares storage but forbids the child writing through
    // it (LRM 23.3.3.2); the child member is a read-only reference the parent
    // still rebinds at construction, which is a storage shape of its own and
    // not the rebindable plain `ref`.
    if (data->direction == hir::PortDirection::kConstRef) {
      return PortConnectionUnsupported(
          span, "const ref port connection is not yet supported");
    }

    // Which cell that member is, is the child's own statement of it, so the
    // parent never reads the child's declaration to find out.
    const hir::RoutedPathRecipe port_recipe = PublishedMemberRecipe(
        instance_step, child_object, *data->member, type_id);
    // An input/output port reads the child cell during simulation, so it holds
    // a persistent routed reference; a `ref` port is bound once in the resolve
    // phase, so it keeps only the reach.
    const auto cell_endpoint = [&]() -> hir::PortEndpoint {
      return hir::PortCellEndpoint{
          .cell = frame.Exprs().Add(unit_lowerer.MakeRoutedMemberRef(
              *internal, home_frame,
              hir::RoutedRefDecl{
                  .recipe = port_recipe, .target_storage = member.storage},
              span))};
    };

    const hir::PortDirection direction = data->direction;
    hir::PortEndpoint endpoint;
    hir::ExprId peer{};
    std::vector<hir::SensitivityEntry> sensitivity;

    switch (direction) {
      case hir::PortDirection::kInput: {
        endpoint = cell_endpoint();
        if (expr == port->getInitializer()) {
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
              unit_lowerer.Unit(), frame, *constant, type_id, span);
          if (!peer_or) return std::unexpected(std::move(peer_or.error()));
          peer = frame.Exprs().Add(*std::move(peer_or));
        } else {
          auto peer_or = scope.LowerExpr(*expr, frame);
          if (!peer_or) return std::unexpected(std::move(peer_or.error()));
          peer = frame.Exprs().Add(*std::move(peer_or));
          auto entries = unit_lowerer.TranslateSensitivityReads(
              unit_lowerer.Sensitivity().AnalyzeReads(*expr, inst), frame,
              support::EventEdge::kAnyChange);
          if (!entries) return std::unexpected(std::move(entries.error()));
          sensitivity = *std::move(entries);
        }
        break;
      }
      case hir::PortDirection::kOutput: {
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
        auto entries = unit_lowerer.TranslateSensitivityReads(
            {SensitivityRead{.symbol = internal, .footprint = std::nullopt}},
            frame, support::EventEdge::kAnyChange);
        if (!entries) return std::unexpected(std::move(entries.error()));
        sensitivity = *std::move(entries);
        break;
      }
      case hir::PortDirection::kRef: {
        endpoint = port_recipe;
        auto peer_or = scope.LowerExpr(*expr, frame);
        if (!peer_or) return std::unexpected(std::move(peer_or.error()));
        peer = frame.Exprs().Add(*std::move(peer_or));
        break;
      }
      case hir::PortDirection::kInOut:
      case hir::PortDirection::kConstRef:
        throw InternalError(
            "ConnectElementPorts: a direction this connection rejects reached "
            "the connection switch");
    }

    frame.current_structural_scope->port_connections.Add(
        hir::PortConnection{
            .span = span,
            .kind = hir::DataPortConnection{
                .direction = direction,
                .endpoint = std::move(endpoint),
                .peer = peer,
                .sensitivity = std::move(sensitivity)}});
  }
  if (index != connections.size()) {
    throw InternalError(
        "ConnectElementPorts: a unit publishes one part per connection its "
        "instances make, so the two are the same sequence");
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
    const slang::ast::InstanceArraySymbol& array,
    const hir::UnitSignature& child_signature, hir::OwnedChildRef child,
    ScopeFrameId home_frame, const std::vector<std::uint32_t>& index_prefix,
    WalkFrame frame) -> diag::Result<void> {
  for (std::uint32_t i = 0; i < array.elements.size(); ++i) {
    std::vector<std::uint32_t> element_prefix = index_prefix;
    element_prefix.push_back(i);
    const auto* element = array.elements[i];
    if (element->kind == slang::ast::SymbolKind::InstanceArray) {
      auto r = ConnectArrayElements(
          scope, unit_lowerer, element->as<slang::ast::InstanceArraySymbol>(),
          child_signature, child, home_frame, element_prefix, frame);
      if (!r) return std::unexpected(std::move(r.error()));
      continue;
    }
    auto r = ConnectElementPorts(
        scope, unit_lowerer, element->as<slang::ast::InstanceSymbol>(),
        child_signature, child, home_frame, std::move(element_prefix), frame);
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
      const auto& inst = member.as<slang::ast::InstanceSymbol>();
      auto r = ConnectElementPorts(
          *this, *owner_, inst,
          owner_->Signatures().Instantiated(SpecializationName(inst)),
          binding->child, binding->home_frame, {}, frame);
      if (!r) return std::unexpected(std::move(r.error()));
    } else if (member.kind == slang::ast::SymbolKind::InstanceArray) {
      // A zero-element array (`Child c[0]`, LRM 23.3.2) constructs no element
      // and binds no member, so there is nothing to connect.
      const auto binding = owner_->LookupOwnedChildBinding(member);
      if (!binding.has_value()) {
        continue;
      }
      // Every element of an array is built from the one unit the array's shape
      // names, so the dependency on that unit resolves once for the whole
      // array. The shape is resolved through the same predicate the declaration
      // pass used, so the unit named here and the member built there cannot
      // drift.
      const auto& array = member.as<slang::ast::InstanceArraySymbol>();
      const auto shape = ResolveInstanceArrayShape(array);
      if (!shape.has_value()) {
        throw InternalError(
            "PopulatePortConnections: an array with a bound member has a "
            "shape, since the same predicate decided both");
      }
      auto r = ConnectArrayElements(
          *this, *owner_, array,
          owner_->Signatures().Instantiated(SpecializationName(*shape->leaf)),
          binding->child, binding->home_frame, {}, frame);
      if (!r) return std::unexpected(std::move(r.error()));
    }
  }
  return {};
}

}  // namespace lyra::lowering::ast_to_hir
