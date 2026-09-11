#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <expected>
#include <optional>
#include <ranges>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include <slang/ast/EvalContext.h>
#include <slang/ast/Expression.h>
#include <slang/ast/HierarchicalReference.h>
#include <slang/ast/Scope.h>
#include <slang/ast/SemanticFacts.h>
#include <slang/ast/ValuePath.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/PortSymbols.h>
#include <slang/ast/symbols/ValueSymbol.h>
#include <slang/ast/types/Type.h>
#include <slang/numeric/ConstantValue.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/published_target.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/unit_signature.hpp"
#include "lyra/lowering/ast_to_hir/constant_value.hpp"
#include "lyra/lowering/ast_to_hir/instance_array_shape.hpp"
#include "lyra/lowering/ast_to_hir/net_overlay.hpp"
#include "lyra/lowering/ast_to_hir/published_projection.hpp"
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
    hir::PublishedStorage storage, hir::TypeId type) -> hir::RoutedPathRecipe {
  return hir::RoutedPathRecipe{
      .head = hir::InUnitHead{.hops = {}},
      .steps = {hir::PathStep{instance_step}},
      .leaf = hir::SignatureMemberLeaf{
          .object = child_object,
          .member = member,
          .storage = std::move(storage),
          .type = type}};
}

// The member and descent a port part reaches, or why no connection can be made
// to it. A port whose expression names declared elements always reaches one
// (LRM 23.2.2.2); a port defined to reach nothing inside the unit is the other
// form the same clause admits.
auto ConnectedProjection(
    const hir::ConnectionTarget& target, diag::SourceSpan span)
    -> diag::Result<const hir::MemberProjection*> {
  return std::visit(
      Overloaded{
          [](const hir::MemberProjection& projection)
              -> diag::Result<const hir::MemberProjection*> {
            return &projection;
          },
          [&](const hir::NoInternalTarget&)
              -> diag::Result<const hir::MemberProjection*> {
            return PortConnectionUnsupported(
                span,
                "a port reaching nothing inside the child is not yet "
                "supported");
          }},
      target);
}

// Every interface instance a connection supplies, in the order the port's
// coordinates count them (LRM 23.3.3.5). A connection to a port standing for
// one instance supplies one, which is the no-dimension case of the same walk;
// an array of interfaces contributes its elements, and a nested one its
// elements' elements, so the walk flattens exactly as the coordinates do.
// Element order needs no correction here: what a connection resolves to is
// already rebased onto the range the port declared, matched left index to left
// index, so the walk takes it as it stands. Nothing when the connection
// resolves to something that is neither.
auto CollectConnectedInstances(const slang::ast::Symbol& connected)
    -> std::optional<std::vector<const slang::ast::InstanceSymbol*>> {
  if (const auto* instance = connected.as_if<slang::ast::InstanceSymbol>()) {
    return std::vector<const slang::ast::InstanceSymbol*>{instance};
  }
  const auto* array = connected.as_if<slang::ast::InstanceArraySymbol>();
  if (array == nullptr) {
    return std::nullopt;
  }
  std::vector<const slang::ast::InstanceSymbol*> instances;
  for (const auto* element : array->elements) {
    auto nested = CollectConnectedInstances(*element);
    if (!nested.has_value()) {
      return std::nullopt;
    }
    instances.insert(instances.end(), nested->begin(), nested->end());
  }
  return instances;
}

// The correspondence LRM 23.3.3.5 fixes between the positions a port stands for
// and the objects an actual supplies. One side is the child's promise and the
// other is what this unit worked out from its own declarations, and a value of
// this type exists only where the two stated the same shape -- so how many
// objects there are and which one stands at a position are both answered out of
// that agreement. A count read off one side alone agrees with itself whatever
// the other says, which is how a connection comes to pair positions that do not
// correspond with nothing able to report it.
class PairedPositions {
 public:
  // Nothing where the two shapes disagree. Each side arrives as the thing it
  // already is -- a promise read off a signature, a reach built by a walk -- so
  // neither can be passed where the other belongs.
  [[nodiscard]] static auto Meet(
      const hir::ObjectsBehindType& promised, const ScopeRoute& reach)
      -> std::optional<PairedPositions> {
    // A port binds every object it stands for, so every position of every
    // dimension it declares is in play: the same statement the actual makes
    // about what it kept, at its own width.
    std::vector<OpenDimension> port = WholeDimensions(promised.shape.dims);
    const auto kept_count = [](const OpenDimension& dim) {
      return dim.kept.count;
    };
    if (!std::ranges::equal(port, reach.open, {}, kept_count, kept_count)) {
      return std::nullopt;
    }
    return PairedPositions{std::move(port), reach.open};
  }

  // How many objects the connection binds.
  [[nodiscard]] auto Count() const -> std::uint64_t {
    std::uint64_t count = 1;
    for (const OpenDimension& dim : port_) {
      count *= dim.kept.count;
    }
    return count;
  }

  // The coordinate of the object paired with the port's `position`, outermost
  // first. Both sides count their own positions from the lower end of the range
  // each declared, so the pairing runs through the offset from the left end,
  // which is what the standard matches: the port's leftmost element is the
  // actual's leftmost, whichever way either was declared.
  [[nodiscard]] auto CoordinateAt(std::uint64_t position) const
      -> std::vector<std::uint32_t> {
    std::vector<std::uint32_t> coordinate(port_.size());
    std::uint64_t remaining = position;
    for (std::size_t dim = port_.size(); dim-- > 0;) {
      const std::uint32_t width = port_[dim].kept.count;
      const auto within = static_cast<std::uint32_t>(remaining % width);
      remaining /= width;
      coordinate[dim] =
          actual_[dim].PositionFromLeft(port_[dim].OffsetFromLeft(within));
    }
    return coordinate;
  }

 private:
  PairedPositions(
      std::vector<OpenDimension> port, std::vector<OpenDimension> actual)
      : port_(std::move(port)), actual_(std::move(actual)) {
  }

  std::vector<OpenDimension> port_;
  std::vector<OpenDimension> actual_;
};

// One route per object the port stands for, pairing each of its positions with
// one the actual left open (LRM 23.3.3.4, 23.3.3.5). A port standing for one
// instance has no open coordinate and no position to pair, which is the same
// walk over nothing rather than a case of its own.
auto RoutesToPairedObjects(
    const ScopeRoute& reach, const hir::ObjectsBehindType& behind,
    diag::SourceSpan span) -> diag::Result<std::vector<hir::RoutedPathRecipe>> {
  if (reach.steps.empty()) {
    throw InternalError(
        "RoutesToPairedObjects: a reach through an interface port starts at "
        "that port's own step, so there is always a step to pick an object out "
        "of");
  }
  const auto paired = PairedPositions::Meet(behind, reach);
  if (!paired.has_value()) {
    return PortConnectionUnsupported(
        span,
        "an interface port bound to a set of interface instances whose shape "
        "is not the one it stands for is not yet supported");
  }
  const std::uint64_t objects = paired->Count();
  std::vector<hir::RoutedPathRecipe> peers;
  peers.reserve(objects);
  for (std::uint64_t position = 0; position < objects; ++position) {
    // Every position reaches the same member and differs only in which object
    // of it the last step picks, so the route is the walk's with that one
    // coordinate written onto its end. A port standing for one instance writes
    // an empty coordinate, which leaves the walk's own route as it stands.
    const std::vector<std::uint32_t> coordinate =
        paired->CoordinateAt(position);
    std::vector<hir::PathStep> steps = reach.steps;
    std::visit(
        [&](auto& step) {
          step.indices.insert(
              step.indices.end(), coordinate.begin(), coordinate.end());
        },
        steps.back());
    peers.push_back(
        hir::RoutedPathRecipe{
            .head = reach.head,
            .steps = std::move(steps),
            .leaf = hir::ScopeLeaf{.type = behind.shape.element_type}});
  }
  return peers;
}

// The routes to the interface instances a connection names (LRM 25.3). An
// actual written as an interface port of a scope enclosing the connection is
// the port's own route; every other actual names instances somewhere on the
// object tree, reached by the same walk every reference across an instance
// boundary uses.
//
// What comes back has met the child's promise about how many objects belong
// there: one way arrives that way by construction, the other is counted against
// the promise here, which is where the two independent counts sit side by side.
auto InterfaceActualRoutes(
    UnitLowerer& unit_lowerer, const slang::ast::PortConnection& conn,
    const hir::ObjectsBehindType& behind, diag::SourceSpan span,
    WalkFrame frame) -> diag::Result<std::vector<hir::RoutedPathRecipe>> {
  const slang::ast::Expression* actual = conn.getExpression();
  const auto* named =
      actual == nullptr
          ? nullptr
          : actual->as_if<slang::ast::ArbitrarySymbolExpression>();

  if (named != nullptr && named->hierRef.isViaIfacePort()) {
    // The actual is the port itself, a part of it, or an instance the interface
    // it carries published (LRM 25.10) -- one walk reaches all three, since
    // continuing past a published member is a step of the same descent and
    // naming part of a member settles fewer of its coordinates.
    auto through =
        unit_lowerer.ReachThroughInterfacePort(frame, named->hierRef);
    if (!through.has_value()) {
      return PortConnectionUnsupported(
          span,
          "an interface reached through another interface port by a path of "
          "this shape is not yet supported");
    }
    return RoutesToPairedObjects(*through, behind, span);
  }

  // Which instance an element of an instance array is given is settled while
  // the design elaborates (LRM 23.3.3.5), so the connection states it; the
  // actual's own expression names the whole array the elements were cut from.
  const slang::ast::Symbol* connected = conn.getIfaceConn().first;
  const auto instances = connected == nullptr
                             ? std::nullopt
                             : CollectConnectedInstances(*connected);
  if (!instances.has_value()) {
    return PortConnectionUnsupported(
        span, "this interface port connection form is not yet supported");
  }
  std::vector<hir::RoutedPathRecipe> peers;
  peers.reserve(instances->size());
  for (const auto* instance : *instances) {
    auto route = unit_lowerer.RouteToScope(frame, instance->body);
    if (!route.has_value()) {
      return PortConnectionUnsupported(
          span,
          "an interface port connected to an instance this scope cannot name "
          "is not yet supported");
    }
    // A route ends at one object, so what types a peer is the element the
    // member stands for rather than the member's whole shape.
    peers.push_back(
        hir::RoutedPathRecipe{
            .head = std::move(route->head),
            .steps = std::move(route->steps),
            .leaf = hir::ScopeLeaf{.type = behind.shape.element_type}});
  }
  // How many objects the member stands for is the child's promise; how many
  // this connection supplies is what the parent worked out from the frontend.
  // Each side counted its own and they meet here -- a count taken from the
  // other would agree with it whatever it said, and the two would then build
  // different layouts with nothing able to report it.
  if (peers.size() != behind.shape.ElementCount()) {
    return PortConnectionUnsupported(
        span,
        "an interface port bound to a number of interface instances other than "
        "the number it stands for is not yet supported");
  }
  return peers;
}

// Binds one interface port of a child instance to the interface instances the
// connection names (LRM 25.3). Both sides are routes resolved once in the
// resolve phase: the child's port member, reached through the step onto the
// instance, and one route per interface object the actual names. Nothing
// crosses the boundary as a value, so the connection installs no driver and
// waits on nothing.
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
  // belongs there and how many of it, taken into this unit's pool, so the
  // parent's record of the connection rests on the child's promise rather than
  // on a second reading of the frontend.
  const hir::TypeId member_type =
      unit_lowerer.ImportSignatureType(child_signature, member.type);
  const auto behind =
      hir::ObjectsBehind(unit_lowerer.Unit().types, member_type);
  if (!behind.has_value()) {
    throw InternalError(
        "ConnectInterfacePort: an interface port's published type names the "
        "unit whose instances belong there, which is what makes it one");
  }
  auto peers = InterfaceActualRoutes(unit_lowerer, conn, *behind, span, frame);
  if (!peers) return std::unexpected(std::move(peers.error()));
  frame.current_structural_scope->port_connections.Add(
      hir::PortConnection{
          .span = span,
          .kind = hir::InterfacePortConnection{
              .endpoint = PublishedMemberRecipe(
                  instance_step, child_object, published.member, member.storage,
                  member_type),
              .peers = *std::move(peers)}});
  return {};
}

// The couplings a bidirectional port connection states (LRM 23.3.3, 23.3.3.7).
// Both sides are a sequence of runs of net positions: the actual may name a
// concatenation of nets, and the child's port may stand for part of one of its
// own declarations, which that unit answers for on its signature because only
// its own source says which part. The two are laid over each other from the
// most significant end, exactly as LRM 10.11's bit overlay rules put the
// members of an alias over one another.
auto ConnectBidirectionalPort(
    StructuralScopeLowerer& scope, const slang::ast::Symbol& eval_scope,
    const slang::ast::Expression& actual, hir::Expr child_net,
    hir::PublishedRun child_run, diag::SourceSpan span, WalkFrame frame)
    -> diag::Result<std::vector<hir::NetJoin>> {
  auto outside = NetRunsOfLvalue(
      scope, eval_scope, actual, span,
      diag::DiagCode::kUnsupportedPortConnectionForm, frame);
  if (!outside) return std::unexpected(std::move(outside.error()));
  std::uint32_t named = 0;
  for (const NetRun& run : *outside) {
    named += run.width;
  }
  if (named != child_run.width) {
    return PortConnectionUnsupported(
        span,
        "an inout port connected to a net of a different width is not yet "
        "supported");
  }
  const NetSide inside = {NetRun{
      .net = frame.Exprs().Add(std::move(child_net)),
      .offset = child_run.position,
      .width = child_run.width}};
  return CoupleSides(*outside, inside, span);
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
    const auto* port = conn->port.as_if<slang::ast::PortSymbol>();
    if (port == nullptr) {
      return PortConnectionUnsupported(
          span, "non-variable port connection is not yet supported");
    }
    auto connected = ConnectedProjection(data->target, span);
    if (!connected) return std::unexpected(std::move(connected.error()));
    const hir::MemberProjection* projection = *connected;
    // The storage behind the part, as the child states it. Its type is wider
    // than the part's whenever the child named only a piece of it (LRM
    // 23.2.2.2), and the descent between the two is published alongside.
    const hir::PublishedMember& member =
        published_class.members.Get(projection->member);
    const auto* internal =
        port->internalSymbol == nullptr
            ? nullptr
            : port->internalSymbol->as_if<slang::ast::ValueSymbol>();
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
    // parent never reads the child's declaration to find out. The route ends at
    // the member, whatever part of it the port stands for.
    const hir::RoutedPathRecipe port_recipe = PublishedMemberRecipe(
        instance_step, child_object, projection->member, member.storage,
        unit_lowerer.ImportSignatureType(child_signature, member.type));
    // An input/output port reads the child cell during simulation, so it holds
    // a persistent routed reference; a `ref` port is bound once in the resolve
    // phase, so it keeps only the reach.
    const std::vector<hir::PublishedSelector> port_path =
        ImportPublishedPath(unit_lowerer, child_signature, projection->path);
    const auto cell_endpoint = [&]() -> hir::PortEndpoint {
      return hir::PortCellEndpoint{
          .cell = frame.Exprs().Add(ProjectPublishedPath(
              unit_lowerer, frame, port_path,
              unit_lowerer.MakeRoutedMemberRef(
                  home_frame, hir::RoutedRefDecl{.recipe = port_recipe}, span),
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
              unit_lowerer.Sensitivity().AnalyzeReads(*expr, inst), frame);
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
              "ConnectElementPorts: an output port connection is stated as an "
              "assignment to the parent-side target");
        }
        if (internal == nullptr) {
          return PortConnectionUnsupported(
              span,
              "an output port whose name reaches no single declaration of the "
              "child is not yet supported");
        }
        auto peer_or = scope.LowerExpr(
            expr->as<slang::ast::AssignmentExpression>().left(), frame);
        if (!peer_or) return std::unexpected(std::move(peer_or.error()));
        peer = frame.Exprs().Add(*std::move(peer_or));
        auto entries = unit_lowerer.TranslateSensitivityReads(
            {SensitivityRead{.symbol = internal, .footprint = std::nullopt}},
            frame);
        if (!entries) return std::unexpected(std::move(entries.error()));
        sensitivity = *std::move(entries);
        break;
      }
      case hir::PortDirection::kRef: {
        // A `ref` port seals to the connected variable's own cell (LRM
        // 23.3.3.2), so what the route reaches has to be the whole of the
        // child's declaration rather than a part of it.
        if (!projection->path.empty()) {
          return PortConnectionUnsupported(
              span,
              "a ref port naming part of an internal name is not yet "
              "supported");
        }
        endpoint = port_recipe;
        auto peer_or = scope.LowerExpr(*expr, frame);
        if (!peer_or) return std::unexpected(std::move(peer_or.error()));
        peer = frame.Exprs().Add(*std::move(peer_or));
        break;
      }
      case hir::PortDirection::kInOut: {
        // A bidirectional connection is not a directional edge: it states that
        // runs of the nets on both sides are one physical net, resolving over
        // the contributions of all of them (LRM 23.3.3, 23.3.3.7), so it reads
        // nothing, drives nothing, and waits on nothing. It is therefore not a
        // data port connection at all, and is recorded as the join it is.
        if (!projection->run.has_value()) {
          return PortConnectionUnsupported(
              span,
              "an inout port standing for a part of an internal name that is "
              "no run of its positions is not yet supported");
        }
        if (!std::holds_alternative<hir::NetStorage>(member.storage)) {
          throw InternalError(
              "ConnectElementPorts: a variable data type is not permitted on "
              "either side of an inout port, so the front end rejects one");
        }
        // slang states an inout connection as an assignment to the parent-side
        // target, the way it states an output one; the actual is that
        // assignment's left side.
        if (expr->kind != slang::ast::ExpressionKind::Assignment) {
          throw InternalError(
              "ConnectElementPorts: an inout port connection is stated as an "
              "assignment to the parent-side target");
        }
        auto couplings = ConnectBidirectionalPort(
            scope, inst, expr->as<slang::ast::AssignmentExpression>().left(),
            unit_lowerer.MakeRoutedMemberRef(
                home_frame, hir::RoutedRefDecl{.recipe = port_recipe}, span),
            *projection->run, span, frame);
        if (!couplings) return std::unexpected(std::move(couplings.error()));
        for (const hir::NetJoin& coupling : *couplings) {
          frame.current_structural_scope->net_joins.push_back(coupling);
        }
        continue;
      }
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
