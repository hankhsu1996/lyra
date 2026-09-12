#include <algorithm>
#include <cstdint>
#include <expected>
#include <optional>
#include <span>
#include <string>
#include <unordered_set>
#include <utility>
#include <variant>
#include <vector>

#include <slang/ast/HierarchicalReference.h>
#include <slang/ast/Scope.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/MemberSymbols.h>
#include <slang/ast/symbols/PortSymbols.h>
#include <slang/ast/symbols/ValueSymbol.h>
#include <slang/ast/types/Type.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/expr_builders.hpp"
#include "lyra/hir/published_modport.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/ast_to_hir/connected_interface.hpp"
#include "lyra/lowering/ast_to_hir/expression/references.hpp"
#include "lyra/lowering/ast_to_hir/sensitivity.hpp"
#include "lyra/lowering/ast_to_hir/unit_identity.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// The dimensions a name left open, with the outermost narrowed to the part the
// name kept. A name that selected no part narrows nothing, which is the case
// where what it kept is already the whole of the dimension. Nothing where a
// part was named and no dimension is open to narrow -- the walk left the
// declarations behind before reaching it, so there is no shape to state the
// part against.
auto NarrowOutermost(
    std::vector<OpenDimension> open, std::optional<KeptPositions> part)
    -> std::optional<std::vector<OpenDimension>> {
  if (!part.has_value()) {
    return open;
  }
  if (open.empty()) {
    return std::nullopt;
  }
  open.front().kept = *part;
  return open;
}

// The compilation unit a value is declared directly in when that unit is a
// namespace -- a package (LRM 26.2) or the anonymous `$unit` scope (LRM
// 3.12.1) -- or nullptr when the value belongs to an instantiated scope and is
// reached by a route. A namespace unit has no instance, so its declarations are
// one program-global cell each, named rather than routed to.
auto DeclaringUnitOfValue(const slang::ast::ValueSymbol& value)
    -> const slang::ast::Symbol* {
  const slang::ast::Scope* scope = value.getParentScope();
  if (scope == nullptr) return nullptr;
  const slang::ast::Symbol& owner = scope->asSymbol();
  if (owner.kind != slang::ast::SymbolKind::Package &&
      owner.kind != slang::ast::SymbolKind::CompilationUnit) {
    return nullptr;
  }
  return &owner;
}

// Whether a scope is one procedural code opens rather than one the design
// hierarchy is built from: a `begin ... end` or a `fork ... join` (LRM 9.3.4 /
// 9.3.2), or a task or function, which LRM 23.9 puts on the path beside them.
// A declaration inside one has its cell on the structural scope that replicates
// it, so this unit's own identity for that declaration already accounts for
// every scope of this kind standing above it.
auto IsProceduralScope(const slang::ast::Symbol& symbol) -> bool {
  return symbol.kind == slang::ast::SymbolKind::StatementBlock ||
         symbol.kind == slang::ast::SymbolKind::Subroutine;
}

}  // namespace

auto UnitLowerer::MapOrGetRoutedRef(
    ScopeFrameId slot_owner_frame, hir::RoutedRefDecl decl)
    -> hir::RoutedRefId {
  auto& slots = routed_refs_by_frame_[slot_owner_frame];
  // Two references that navigate the same way to the same target are one
  // endpoint; two that reach it differently are two, and the route is what says
  // which. An object reached through a port and the same object named
  // hierarchically coincide only in the instance being lowered -- the port
  // reaches whatever it was bound to, the name reaches what it names -- so
  // sharing one endpoint between them would make the second follow the first's
  // binding.
  for (const hir::RoutedRefId id : slots.Ids()) {
    if (slots.Get(id).recipe == decl.recipe) return id;
  }
  return slots.Add(std::move(decl));
}

auto UnitLowerer::TakeRoutedRefsForFrame(ScopeFrameId slot_owner_frame)
    -> base::Arena<hir::RoutedRefDecl, hir::RoutedRefId> {
  const auto it = routed_refs_by_frame_.find(slot_owner_frame);
  if (it == routed_refs_by_frame_.end()) {
    return {};
  }
  auto out = std::move(it->second);
  routed_refs_by_frame_.erase(it);
  return out;
}

namespace {

// The slot for `decl` in `slots`, reusing one that already asks the same scope
// for the same name on the same class.
template <typename Id>
auto MapOrGetClassName(
    base::Arena<hir::ClassNameDecl, Id>& slots, hir::ClassNameDecl decl) -> Id {
  for (const Id id : slots.Ids()) {
    const hir::ClassNameDecl& at = slots.Get(id);
    if (at.head == decl.head && at.steps == decl.steps &&
        at.class_name == decl.class_name && at.name == decl.name) {
      return id;
    }
  }
  return slots.Add(std::move(decl));
}

}  // namespace

auto UnitLowerer::MapOrGetPropertyCoordinate(
    ScopeFrameId slot_owner_frame, hir::ClassNameDecl decl)
    -> hir::PropertyCoordinateId {
  return MapOrGetClassName(
      property_coordinates_by_frame_[slot_owner_frame], std::move(decl));
}

auto UnitLowerer::MapOrGetBehaviorCoordinate(
    ScopeFrameId slot_owner_frame, hir::ClassNameDecl decl)
    -> hir::BehaviorCoordinateId {
  return MapOrGetClassName(
      behavior_coordinates_by_frame_[slot_owner_frame], std::move(decl));
}

auto UnitLowerer::TakePropertyCoordinatesForFrame(ScopeFrameId slot_owner_frame)
    -> base::Arena<hir::ClassNameDecl, hir::PropertyCoordinateId> {
  const auto it = property_coordinates_by_frame_.find(slot_owner_frame);
  if (it == property_coordinates_by_frame_.end()) {
    return {};
  }
  auto out = std::move(it->second);
  property_coordinates_by_frame_.erase(it);
  return out;
}

auto UnitLowerer::TakeBehaviorCoordinatesForFrame(ScopeFrameId slot_owner_frame)
    -> base::Arena<hir::ClassNameDecl, hir::BehaviorCoordinateId> {
  const auto it = behavior_coordinates_by_frame_.find(slot_owner_frame);
  if (it == behavior_coordinates_by_frame_.end()) {
    return {};
  }
  auto out = std::move(it->second);
  behavior_coordinates_by_frame_.erase(it);
  return out;
}

auto UnitLowerer::MakeRoutedMemberRef(
    ScopeFrameId slot_owner_frame, hir::RoutedRefDecl decl,
    diag::SourceSpan span) -> hir::Expr {
  const hir::TypeId type =
      std::get<hir::EndpointCell>(hir::EndpointOf(decl.recipe.leaf)).type;
  const hir::RoutedRefId slot =
      MapOrGetRoutedRef(slot_owner_frame, std::move(decl));
  return hir::MakeRefExpr(hir::RoutedRef{.id = slot}, type, span);
}

auto UnitLowerer::LookupPublishedRouteTarget(
    const slang::ast::ValueSymbol& value, const ScopeRoute& route)
    -> std::optional<hir::RouteLeaf> {
  // Which unit's object the route landed on is what the walk that built it
  // knows; a route that landed on none reached a scope with no declaration
  // behind it, and the name it ends at was never promised to anyone.
  if (!route.unit_name.has_value()) return std::nullopt;

  const hir::UnitSignature* signature = Signatures().Find(*route.unit_name);
  if (signature == nullptr || !signature->instance_class.has_value()) {
    return std::nullopt;
  }
  const auto member_id = signature->instance_class->Find(value.name);
  if (!member_id.has_value()) return std::nullopt;

  // The name resolved against the signature; from here the route carries the
  // position, and the member's type is already in this unit's pool because the
  // record brought it there.
  const hir::ExternalUnitObjectId object =
      ExternalUnitObjectOf(signature->unit_name);
  const hir::PublishedMember& member =
      unit_.external_unit_objects.Get(object).members.Get(*member_id);
  return hir::SignatureMemberLeaf{
      .object = object,
      .member = *member_id,
      .storage = member.storage,
      .type = member.type};
}

auto UnitLowerer::ResolveRouteTarget(
    const slang::ast::ValueSymbol& value, const ScopeRoute& route)
    -> diag::Result<hir::RouteLeaf> {
  // A member the owning unit published is named against the signature this
  // unit consumed, which also states what storage the name reaches -- so
  // nothing about it is read off the unit that declared it.
  if (auto published = LookupPublishedRouteTarget(value, route)) {
    return *std::move(published);
  }

  auto type =
      InternType(value.getType(), SourceMapper().PointSpanOf(value.location));
  if (!type) return std::unexpected(std::move(type.error()));

  const hir::PublishedStorage storage = DeclarationStorage(value);

  // This unit's own identity for the target when it declares it -- and for a
  // static a name reaches through a procedural scope (LRM 23.9 puts a block, a
  // task and a function on that path alike), that identity also says every such
  // scope between the static and its structural one describes where the storage
  // sits rather than a step the route takes.
  if (const auto data_object = LookupStructuralDataObjectBinding(value)) {
    return hir::StructuralDataObjectLeaf{
        .object = data_object->var_id, .storage = storage, .type = *type};
  }
  if (const auto procedural_static = LookupProceduralStatic(value)) {
    return hir::ProceduralStaticLeaf{
        .body = procedural_static->body,
        .var = procedural_static->var,
        .type = *type};
  }
  // Nothing was published to compile against, so the name is all that crosses
  // and the runtime answers it during elaboration (LRM 23.6). What storage it
  // reaches has no statement either, which is why it is read off the frontend.
  return hir::OpaqueLeaf{
      .name = std::string{value.name},
      .storage = std::move(storage),
      .type = *type};
}

auto UnitLowerer::MakeRoutedRef(
    const slang::ast::ValueSymbol& value, ScopeFrameId slot_owner,
    ScopeRoute route) -> diag::Result<hir::ReferenceRoute> {
  auto leaf = ResolveRouteTarget(value, route);
  if (!leaf) return std::unexpected(std::move(leaf.error()));
  const hir::RoutedRefId id = MapOrGetRoutedRef(
      slot_owner, hir::RoutedRefDecl{
                      .recipe = hir::RoutedPathRecipe{
                          .head = std::move(route.head),
                          .steps = std::move(route.steps),
                          .leaf = *std::move(leaf)}});
  return hir::ReferenceRoute{hir::RoutedRef{.id = id}};
}

auto UnitLowerer::MakeRoutedObjectRef(
    ScopeFrameId slot_owner, ScopeRoute route, hir::TypeId object_type)
    -> hir::RoutedRef {
  const hir::RoutedRefId id = MapOrGetRoutedRef(
      slot_owner, hir::RoutedRefDecl{
                      .recipe = hir::RoutedPathRecipe{
                          .head = std::move(route.head),
                          .steps = std::move(route.steps),
                          .leaf = hir::ScopeLeaf{.type = object_type}}});
  return hir::RoutedRef{.id = id};
}

auto UnitLowerer::MakeRoutedCallableRef(
    ScopeFrameId slot_owner, ScopeRoute route, std::string name,
    hir::ExternalCalleeInterface interface) -> hir::RoutedRef {
  const hir::RoutedRefId id = MapOrGetRoutedRef(
      slot_owner, hir::RoutedRefDecl{
                      .recipe = hir::RoutedPathRecipe{
                          .head = std::move(route.head),
                          .steps = std::move(route.steps),
                          .leaf = hir::OpaqueCallableLeaf{
                              .name = std::move(name),
                              .interface = std::move(interface)}}});
  return hir::RoutedRef{.id = id};
}

auto UnitLowerer::MakeRoutedDisableTargetRef(
    const WalkFrame& frame, const slang::ast::Symbol& target,
    diag::SourceSpan span) -> diag::Result<hir::RoutedRef> {
  // A scope's identity indexes its declaring scope's registry, so a route
  // carrying one runs to that scope, and the procedural scopes between are
  // where the target sits rather than steps of their own -- the same reading a
  // static declared in one of them takes.
  const auto minted = LookupMintedProceduralScope(target);
  const slang::ast::Scope* walk_to =
      minted.has_value() ? minted->owner : target.as_if<slang::ast::Scope>();
  if (walk_to == nullptr) {
    throw InternalError(
        "UnitLowerer::MakeRoutedDisableTargetRef: a disable names a block or a "
        "task, each of which defines a scope");
  }
  auto route = RouteToScope(frame, *walk_to);
  if (!route.has_value()) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedStatementForm,
        "a disable of a block or task reached this way is not yet supported");
  }
  hir::RouteLeaf leaf =
      minted.has_value()
          ? hir::RouteLeaf{hir::DisableTargetLeaf{.scope = minted->scope}}
          : hir::RouteLeaf{hir::OpaqueDisableTargetLeaf{}};
  const hir::RoutedRefId id = MapOrGetRoutedRef(
      frame.Current(), hir::RoutedRefDecl{
                           .recipe = hir::RoutedPathRecipe{
                               .head = std::move(route->head),
                               .steps = std::move(route->steps),
                               .leaf = std::move(leaf)}});
  return hir::RoutedRef{.id = id};
}

auto UnitLowerer::RouteToUnitObject(
    const WalkFrame& frame, const slang::ast::InstanceBodySymbol& body,
    const slang::ast::HierarchicalReference& reference, diag::SourceSpan span)
    -> diag::Result<ScopeRoute> {
  // Going through a port reaches whatever that port was bound to, which is the
  // only reach this unit has to what stands behind it; any other name reaches
  // the same object in every instantiation, and the walk on the elaborated
  // hierarchy is what says so. Both end at one object in the instance being
  // lowered, so what tells them apart is how the name got there and never what
  // it resolved to.
  if (reference.isViaIfacePort()) {
    auto through = ReachOneThroughInterfacePort(frame, reference);
    if (!through.has_value()) {
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedExpressionForm,
          "a subroutine on an interface reached through a port by a path of "
          "this shape is not yet supported");
    }
    return *std::move(through);
  }
  auto walked = RouteToScope(frame, body);
  if (!walked.has_value()) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "a subroutine on an instance reached this way is not yet supported");
  }
  return *std::move(walked);
}

auto UnitLowerer::TranslateReferenceRoute(
    const WalkFrame& frame, const slang::ast::ValueSymbol& value)
    -> diag::Result<std::optional<hir::ReferenceRoute>> {
  // This unit's own identity for the target, if it declares it.
  const auto data_object = LookupStructuralDataObjectBinding(value);
  const auto procedural_static = LookupProceduralStatic(value);
  const std::optional<hir::StructuralHops> data_object_hops =
      data_object ? frame.HopsTo(data_object->home_frame) : std::nullopt;

  // A data object of the reader's own scope is a direct member of `self`: the
  // one shape that is no route at all, and so has no leaf and no sealed
  // endpoint.
  if (data_object_hops.has_value() && data_object_hops->value == 0) {
    return hir::ReferenceRoute{
        hir::DirectMemberRef{.var = data_object->var_id}};
  }

  const auto routed_ref = [&](ScopeFrameId slot_owner, ScopeRoute route)
      -> diag::Result<std::optional<hir::ReferenceRoute>> {
    auto reference = MakeRoutedRef(value, slot_owner, std::move(route));
    if (!reference) return std::unexpected(std::move(reference.error()));
    return *reference;
  };

  // The target's storage hangs under an ancestor scope of the same unit, so
  // the whole route is a typed climb to it: a routed reference sealed once in
  // the resolve phase rather than re-walked on each access.
  const auto in_unit_route = [&](hir::StructuralHops hops) {
    return routed_ref(
        frame.Current(), ScopeRoute{
                             .head = hir::InUnitHead{.hops = hops},
                             .steps = {},
                             .unit_name = std::nullopt,
                             .open = {}});
  };
  if (data_object_hops.has_value()) {
    return in_unit_route(*data_object_hops);
  }
  if (procedural_static) {
    if (const auto hops = frame.HopsTo(procedural_static->home_frame)) {
      return in_unit_route(*hops);
    }
  }

  // A target this unit declares reaches its storage through the scope that
  // owns it, and a leaf naming that declaration already fixes the whole
  // procedural descent, so the blocks and subroutines around it are part of
  // where the storage sits rather than steps of their own. They nest
  // contiguously under that scope, so dropping them here drops all of them.
  const slang::ast::Scope* owner = value.getHierarchicalParent();
  if (data_object || procedural_static) {
    while (owner != nullptr && IsProceduralScope(owner->asSymbol())) {
      owner = owner->asSymbol().getHierarchicalParent();
    }
  }
  if (owner == nullptr) return std::nullopt;

  auto route = RouteToScope(frame, *owner);
  if (!route.has_value()) return std::nullopt;
  return routed_ref(frame.Current(), *std::move(route));
}

auto UnitLowerer::ReachThroughInterfacePort(
    const WalkFrame& frame, const slang::ast::HierarchicalReference& reference)
    -> std::optional<ScopeRoute> {
  const auto path = reference.path;
  const auto* port =
      path.front().symbol->as_if<slang::ast::InterfacePortSymbol>();
  if (port == nullptr) {
    return std::nullopt;
  }
  // The port's own reach is the descent's first hop: its step, and the unit the
  // connection bound standing behind it. Taking it as a hop is what lets every
  // hop below take the classification every descent takes, so a name the
  // interface published is a step through its promise and one it did not is a
  // step the instance answers, exactly as they are past a module instance.
  ScopeRoute reach = RouteThroughInterfacePort(frame, *port);
  ScopeRoute route{
      .head = std::move(reach.head),
      .steps = {},
      .unit_name = std::nullopt,
      .open = {}};
  std::vector<DescentHop> hops;
  hops.reserve(path.size());
  hops.push_back(
      DescentHop{
          .step = std::move(reach.steps.front()),
          .declared_unit = std::move(reach.unit_name),
          .declared_dims = InterfacePortDimensions(*port)});

  // Whether the name ends at an object or at something inside one, which is
  // what says how far the descent runs: a name whose target is an instance, or
  // a set of them, ends there, and one whose target is a member or a subroutine
  // ends at the object that owns it.
  const bool target_is_object =
      reference.target != nullptr &&
      (reference.target->kind == slang::ast::SymbolKind::Instance ||
       reference.target->kind == slang::ast::SymbolKind::InstanceArray);

  // The contiguous part of the landing the name kept, where it named one. A
  // part of an instance array is not a scope, so nothing can be selected out of
  // it and the path says nothing after it: it applies to whatever the walk
  // ended on.
  std::optional<KeptPositions> part;

  for (std::size_t hop = 1; hop < path.size(); ++hop) {
    if (path[hop].symbol == reference.target && !target_is_object) {
      break;
    }
    // A coordinate selects out of what the hop before it reached (LRM 25.3), so
    // it is part of that hop rather than one of its own.
    if (const auto* position = std::get_if<std::int32_t>(&path[hop].selector)) {
      if (*position < 0) return std::nullopt;
      std::visit(
          [&](auto& step) {
            step.indices.push_back(static_cast<std::uint32_t>(*position));
          },
          hops.back().step);
      continue;
    }
    // A part selects out of the same hop for the same reason a coordinate does;
    // what differs is that it leaves several objects rather than one, so it
    // narrows a dimension instead of settling it.
    if (const auto* span = std::get_if<std::pair<std::int32_t, std::int32_t>>(
            &path[hop].selector)) {
      if (span->first < 0 || span->second < span->first) return std::nullopt;
      part = KeptPositions{
          .first = static_cast<std::uint32_t>(span->first),
          .count = static_cast<std::uint32_t>(span->second - span->first + 1)};
      break;
    }
    hops.push_back(
        DescentHop{
            .step =
                hir::OpaqueStep{
                    .name = std::string{path[hop].symbol->name}, .indices = {}},
            .declared_unit = std::nullopt,
            .declared_dims = {}});
    if (path[hop].symbol == reference.target) {
      break;
    }
  }
  ClassifyDescent(route, hops);
  auto open = NarrowOutermost(std::move(route.open), part);
  if (!open.has_value()) return std::nullopt;
  route.open = *std::move(open);
  return route;
}

auto UnitLowerer::RouteThroughInterfacePort(
    const WalkFrame& frame, const slang::ast::Symbol& port) const
    -> ScopeRoute {
  const auto binding = LookupInterfacePortBinding(port);
  if (!binding.has_value()) {
    throw InternalError(
        "UnitLowerer::RouteThroughInterfacePort: the path heads at a port of "
        "this unit, which the unit's own walk declared");
  }
  const auto hops = frame.HopsTo(binding->home_frame);
  if (!hops.has_value()) {
    throw InternalError(
        "UnitLowerer::RouteThroughInterfacePort: an interface port is a member "
        "of a scope enclosing every reader of it");
  }
  // The route lands on the port itself, so every object it stands for is still
  // in play; a name that picks one out of them says so in a coordinate the
  // caller walks on from here.
  return ScopeRoute{
      .head = hir::InUnitHead{.hops = *hops},
      .steps = {hir::PathStep{
          hir::InterfacePortStep{.port = binding->port, .indices = {}}}},
      .unit_name = InterfaceUnitOf(port),
      .open = WholeDimensions(InterfacePortDimensions(port))};
}

auto UnitLowerer::ReachOwnScope(
    const WalkFrame& frame, const slang::ast::Scope& target,
    diag::SourceSpan span) -> diag::Result<InUnitReach> {
  const auto refuse = [&](std::string message) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm, std::move(message));
  };
  // An ancestor of the reader is the whole reach, and the descent is empty.
  if (const auto hops = frame.HopsTo(LookupScopeFrame(target))) {
    return InUnitReach{.hops = *hops, .descent = {}};
  }

  auto route = RouteToScope(frame, target);
  if (!route.has_value()) {
    return refuse("a subroutine reached this way is not yet supported");
  }
  // Every part of the reach has to stay inside this unit's layout, because a
  // scope this artifact owns is what makes the callee's identity mean
  // anything: it indexes that scope's own registry. A callee whose identity
  // this unit holds is in this unit's own subtree, so the walk to it never
  // leaves the layout; a reach that does is one this unit could not have
  // resolved an identity for, and it is refused rather than silently taking
  // an identity from a scope that is not the one the route landed on.
  const auto* in_unit = std::get_if<hir::InUnitHead>(&route->head);
  if (in_unit == nullptr) {
    return refuse(
        "a subroutine reached by a name anchored outside this module is not "
        "yet supported");
  }
  InUnitReach reach{.hops = in_unit->hops, .descent = {}};
  reach.descent.reserve(route->steps.size());
  for (const hir::PathStep& step : route->steps) {
    const auto* owned = std::get_if<hir::OwnedChildStep>(&step);
    if (owned == nullptr ||
        !std::holds_alternative<hir::GenerateChildRef>(owned->child)) {
      return refuse(
          "a subroutine reached through a scope this module does not lay out "
          "is not yet supported");
    }
    reach.descent.push_back(owned->child);
  }
  return reach;
}

auto UnitLowerer::RouteToDeclaringScope(
    const WalkFrame& frame, const slang::ast::Scope& target,
    diag::SourceSpan span) -> diag::Result<ScopeRoute> {
  auto walked = RouteToScope(frame, target);
  if (!walked.has_value()) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "a subroutine declared in a scope reached this way is not yet "
        "supported");
  }
  return *std::move(walked);
}

auto UnitLowerer::RouteToScope(
    const WalkFrame& frame, const slang::ast::Scope& target)
    -> std::optional<ScopeRoute> {
  // The reader's elaborated ancestor scopes, across unit boundaries (slang's
  // `getHierarchicalParent` crosses the boundary at the instance-body
  // transition). The route meets the target at the deepest scope shared with
  // the reader; a target-side hop whose parent is one of these scopes is the
  // named child that shared ancestor exposes -- the head an out-of-unit or
  // sibling-subtree reference climbs to and descends from.
  std::unordered_set<const slang::ast::Scope*> reader_ancestors;
  for (const slang::ast::Scope* s = frame.reader_scope; s != nullptr;
       s = s->asSymbol().getHierarchicalParent()) {
    reader_ancestors.insert(s);
  }

  // Walk the target's owner chain, building the descent bottom-up. Each hop's
  // addressable owned child is resolved: the instance member (not its body)
  // across a unit boundary, the array member for a generate-loop iteration or
  // instance-array element with the elaborated index attached.
  //
  // A hop this unit does not declare may still be one the unit above it
  // published (LRM 25.10), which is a step through what that unit promised
  // rather than a name the runtime answers. Which unit stands above a hop is
  // what every hop before it decided, and this walk runs from the target
  // upward, so it records what each hop is and leaves the classification to a
  // forward pass over the whole descent.
  std::vector<DescentHop> descent;
  const slang::ast::Scope* scope = &target;
  while (scope != nullptr) {
    const slang::ast::Symbol* owned = &scope->asSymbol();
    // A route navigates the object tree, and a namespace unit -- a package (LRM
    // 26.2) or the `$unit` scope (LRM 3.12.1) -- has no instance and so no
    // object on it. Nothing declared there is reachable this way, whatever else
    // the walk would have found above it, so the walk answers with no route
    // rather than a head naming a scope the runtime never builds.
    if (owned->kind == slang::ast::SymbolKind::Package ||
        owned->kind == slang::ast::SymbolKind::CompilationUnit) {
      return std::nullopt;
    }
    const slang::ast::Scope* next = owned->getHierarchicalParent();
    std::vector<std::uint32_t> indices;
    // The unit an instance this one declares is built from. Reading it off the
    // declaration is this unit naming its own child; a hop it does not declare
    // is named by whatever the unit above it promised instead.
    std::optional<std::string> declared_unit;
    if (owned->kind == slang::ast::SymbolKind::InstanceBody) {
      const auto* inst =
          owned->as<slang::ast::InstanceBodySymbol>().parentInstance;
      if (inst == nullptr) return std::nullopt;
      declared_unit = SpecializationName(*inst);
      if (inst->arrayPath.empty()) {
        owned = inst;
      } else {
        // A multi-dimensional instance array nests one InstanceArray symbol per
        // dimension, but the unit registers a single array member spanning all
        // dimensions and `arrayPath` already carries every index. Climb to the
        // outermost array symbol so the head is that registered member.
        indices.assign(inst->arrayPath.begin(), inst->arrayPath.end());
        owned = &inst->getParentScope()->asSymbol();
        while (owned->getParentScope() != nullptr &&
               owned->getParentScope()->asSymbol().kind ==
                   slang::ast::SymbolKind::InstanceArray) {
          owned = &owned->getParentScope()->asSymbol();
        }
      }
      next = owned->getHierarchicalParent();
    } else if (const auto* gb = owned->as_if<slang::ast::GenerateBlockSymbol>();
               gb != nullptr && gb->getArrayIndex() != nullptr) {
      const slang::ast::Symbol& array =
          owned->getHierarchicalParent()->asSymbol();
      // The unit that declares a loop iteration declares it as a child in its
      // own right, so the iteration is the step and its elaborated position is
      // already part of that identity. Across the artifact boundary only the
      // array's source name travels, and the position picks the iteration out
      // of it (LRM 27.4).
      if (!LookupOwnedChildBinding(*owned).has_value()) {
        indices.push_back(
            static_cast<std::uint32_t>(
                gb->getArrayIndex()->as<std::int64_t>().value_or(0)));
        owned = &array;
      }
      next = array.getHierarchicalParent();
    }

    // The head is the child of the deepest scope shared with the reader: the
    // first hop whose parent scope is a reader ancestor. Everything already
    // accumulated is the descent below it. Stopping at the shared-scope child
    // rather than the first bound owned child is what makes a procedurally
    // nested head (a named block inside another) head at the block the shared
    // scope directly exposes, not the inner one.
    if (next != nullptr && reader_ancestors.contains(next)) {
      // A head whose owning scope this unit emits stays inside this unit's
      // layout: the climb to that scope is typed, and the head becomes the
      // route's first typed step.
      if (const auto obinding = LookupOwnedChildBinding(*owned)) {
        if (const auto hops = frame.HopsTo(obinding->home_frame)) {
          descent.push_back(
              DescentHop{
                  .step =
                      hir::OwnedChildStep{
                          .child = obinding->child,
                          .indices = std::move(indices)},
                  .declared_unit = std::move(declared_unit),
                  .declared_dims = {}});
          std::ranges::reverse(descent);
          ScopeRoute route{
              .head = hir::InUnitHead{.hops = *hops},
              .steps = {},
              .unit_name = std::nullopt,
              .open = {}};
          ClassifyDescent(route, descent);
          return route;
        }
      }
      // No owned-child binding: this unit does not declare the head, so it
      // lives in an ancestor compilation unit (an upward reference climbs out
      // through the reader's own instance to reach it). A generate block or a
      // named block in that other unit is reached by name across the boundary,
      // the same as an instance head. When this unit does own the head the
      // typed branch above always takes it, so reaching here is exactly the
      // cross-unit case and never a silent fallback for a local one.
      std::ranges::reverse(descent);
      ScopeRoute route{
          .head =
              hir::VisibleChildHead{
                  .head_name = std::string{owned->name},
                  .head_indices = std::move(indices)},
          .steps = {},
          .unit_name = std::nullopt,
          .open = {}};
      ClassifyDescent(route, descent);
      return route;
    }

    // A step this unit declares stays inside its layout and carries the
    // declaring scope's identity; one it does not is past the artifact
    // boundary, where the canonical name is the only identity that travels --
    // unless the unit standing above it promised the name.
    if (const auto obinding = LookupOwnedChildBinding(*owned)) {
      descent.push_back(
          DescentHop{
              .step =
                  hir::OwnedChildStep{
                      .child = obinding->child, .indices = std::move(indices)},
              .declared_unit = std::move(declared_unit),
              .declared_dims = {}});
    } else {
      descent.push_back(
          DescentHop{
              .step =
                  hir::OpaqueStep{
                      .name = std::string{owned->name},
                      .indices = std::move(indices)},
              .declared_unit = std::nullopt,
              .declared_dims = {}});
    }
    scope = next;
  }
  return std::nullopt;
}

auto UnitLowerer::PromisedObjectMember(
    const std::string& unit_name, std::string_view name) const
    -> std::optional<PublishedHop> {
  const hir::UnitSignature* signature = Signatures().Find(unit_name);
  if (signature == nullptr || !signature->instance_class.has_value()) {
    return std::nullopt;
  }
  const auto member = signature->instance_class->Find(name);
  if (!member.has_value()) {
    return std::nullopt;
  }
  // A published member a name continues through stands for objects; one
  // standing for values is a leaf and never a step. Which unit those objects
  // belong to, and how many of them there are, are the promise's own statement,
  // so both answers come out of the promise rather than out of the unit that
  // made it.
  const auto behind = hir::ObjectsBehind(
      signature->types, signature->instance_class->members.Get(*member).type);
  if (!behind.has_value()) {
    return std::nullopt;
  }
  return PublishedHop{
      .signature = signature,
      .member = *member,
      .unit_name = std::string{behind->unit_name},
      .dims = std::move(behind->shape.dims)};
}

void UnitLowerer::ClassifyDescent(
    ScopeRoute& route, std::span<DescentHop> hops) {
  // Which unit the route stands in, walked forward: a step this unit declares
  // lands where its own declaration says, and one it does not lands on what the
  // unit standing there promised. A hop is a step through a promise only while
  // the route still holds a typed pointer to ask -- past the first hop answered
  // by name there is no object to count positions out of, so what follows stays
  // by name and the route lands on no unit however deep it went.
  bool typed = std::holds_alternative<hir::InUnitHead>(route.head);
  std::optional<std::string> standing;
  // What the last step stands over, carried alongside the unit it lands in:
  // the two are one statement about that landing, and a hop coordinate settles
  // one of them, so the walk keeps both and the caller reads the end of it.
  std::vector<OpenDimension> open;
  // A coordinate the name wrote settles the outermost dimension still open, so
  // what a step leaves open is what it stands over less what it already picked.
  // A hop this unit declares nothing about stands over nothing it can state, so
  // it leaves nothing open whatever the name picked out of it.
  const auto settled = [](std::span<const hir::UnpackedRange> dims,
                          std::size_t picked) {
    return WholeDimensions(dims.subspan(std::min(picked, dims.size())));
  };
  route.steps.reserve(hops.size());
  for (DescentHop& hop : hops) {
    auto* opaque = std::get_if<hir::OpaqueStep>(&hop.step);
    if (opaque == nullptr) {
      standing = std::move(hop.declared_unit);
      open = settled(
          hop.declared_dims,
          std::visit(
              [](const auto& step) { return step.indices.size(); }, hop.step));
      route.steps.push_back(std::move(hop.step));
      continue;
    }
    const auto promised = typed && standing.has_value()
                              ? PromisedObjectMember(*standing, opaque->name)
                              : std::nullopt;
    if (!promised.has_value()) {
      typed = false;
      standing.reset();
      open.clear();
      route.steps.push_back(std::move(hop.step));
      continue;
    }
    standing = promised->unit_name;
    open = settled(promised->dims, opaque->indices.size());
    route.steps.emplace_back(
        hir::SignatureMemberStep{
            .object = ExternalUnitObjectOf(promised->signature->unit_name),
            .member = promised->member,
            .indices = std::move(opaque->indices)});
  }
  route.unit_name = typed ? std::move(standing) : std::nullopt;
  route.open = std::move(open);
}

auto UnitLowerer::ResolveValueTarget(
    const WalkFrame& frame, const slang::ast::ValueSymbol& value,
    diag::SourceSpan span) -> diag::Result<hir::ValueTarget> {
  // A namespace unit has no instance, so its cell is reached by name rather
  // than by a route out of the reader's own storage (LRM 26.2, 3.12.1). The
  // same by-name form serves a referrer in another unit and the owning unit's
  // own body, neither of which has a receiver to route through.
  if (const auto* unit = DeclaringUnitOfValue(value)) {
    auto value_type = InternType(value.getType(), span);
    if (!value_type) return std::unexpected(std::move(value_type.error()));
    return hir::ValueTarget{hir::ExternalUnitValueRef{
        .unit_name = CompilationUnitName(*unit),
        .variable_name = std::string{value.name},
        .value_type = *value_type}};
  }

  auto route = TranslateReferenceRoute(frame, value);
  if (!route) return std::unexpected(std::move(route.error()));
  if (!route->has_value()) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        std::format(
            "reaching the storage `{}` names from here is not yet supported",
            value.name));
  }
  return hir::ValueTarget{*std::move(*route)};
}

auto UnitLowerer::ResolveStaticPropertyTarget(
    const WalkFrame& frame, const slang::ast::ClassPropertySymbol& prop,
    diag::SourceSpan span) -> diag::Result<hir::StaticPropertyRef> {
  const auto& owner_class =
      prop.getParentScope()->asSymbol().as<slang::ast::ClassType>();
  auto owner_ref = ResolveClassRef(owner_class, span);
  if (!owner_ref) return std::unexpected(std::move(owner_ref.error()));
  auto declaring_hops = DeclaringScopeHopsFrom(owner_class, frame, span);
  if (!declaring_hops) {
    return std::unexpected(std::move(declaring_hops.error()));
  }
  auto value_type = InternType(prop.getType(), span);
  if (!value_type) return std::unexpected(std::move(value_type.error()));
  return hir::StaticPropertyRef{
      .target = MakeStaticPropertyTarget(*owner_ref, prop),
      .declaring_scope_hops = *declaring_hops,
      .value_type = *value_type};
}

auto UnitLowerer::ObservedThroughModport(
    const slang::ast::ModportPortSymbol& offered, const WalkFrame& frame)
    -> diag::Result<std::vector<hir::SensitivityEntry>> {
  const auto span = SourceMapper().PointSpanOf(offered.location);
  const auto refuse = [&](std::string message) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm, std::move(message));
  };
  const slang::ast::Scope* view = offered.getParentScope();
  if (view == nullptr) {
    throw InternalError(
        "UnitLowerer::ObservedThroughModport: a name a view offers is declared "
        "by the view");
  }

  // How the object was reached is a property of the reference, and a read
  // carries none -- so it is recovered from the reader: a scope holding this
  // very view on a port of its own reached it that way, because that port is
  // the whole of what stands for the interface there. The port decides it
  // before any route is tried, because climbing out of this unit and back down
  // arrives at the same object by a reach a separately compiled unit does not
  // have. A reader holding the same view on two ports has stated no choice
  // between them.
  const slang::ast::InstanceBodySymbol* reader = nullptr;
  for (const slang::ast::Scope* s = frame.reader_scope;
       s != nullptr && reader == nullptr; s = s->asSymbol().getParentScope()) {
    reader = s->asSymbol().as_if<slang::ast::InstanceBodySymbol>();
  }
  const slang::ast::InterfacePortSymbol* through = nullptr;
  for (const auto* member : reader == nullptr
                                ? std::span<const slang::ast::Symbol* const>{}
                                : reader->getPortList()) {
    const auto* port = member->as_if<slang::ast::InterfacePortSymbol>();
    if (port == nullptr ||
        ConnectedInterfaceOf(port->getConnection()).modport !=
            &view->asSymbol()) {
      continue;
    }
    if (through != nullptr) {
      return refuse(
          "waiting on a name a view offers where this scope carries that view "
          "on more than one of its own ports is not yet supported");
    }
    through = port;
  }

  // No such port, so the interface is one this scope's own hierarchy declares
  // and the object is reached by the route that reaches any name on it.
  std::optional<ScopeRoute> reached =
      through != nullptr
          ? std::optional{RouteThroughInterfacePort(frame, *through)}
          : RouteToScope(frame, *view->asSymbol().getParentScope());
  if (!reached.has_value() || !reached->unit_name.has_value()) {
    return refuse(
        "waiting on a name a view offers on an interface this scope neither "
        "declares nor carries on a port of its own is not yet supported");
  }
  ScopeRoute route = *std::move(reached);
  // A read of such a name states no coordinate, so a reach standing for several
  // instances leaves the route with nothing to say which of them changed.
  if (!route.open.empty()) {
    return refuse(
        "waiting on a name a view offers through a reach carrying a range is "
        "not yet supported");
  }
  const hir::ExternalUnitObjectId object =
      ExternalUnitObjectOf(*route.unit_name);
  const hir::PublishedModport* published = hir::FindModport(
      unit_.external_unit_objects.Get(object).modports, view->asSymbol().name);
  const hir::PublishedModportPort* name =
      published == nullptr ? nullptr : published->Find(offered.name);
  if (name == nullptr) {
    throw InternalError(
        "UnitLowerer::ObservedThroughModport: an interface publishes every "
        "view it declares and every name each view defines");
  }

  // What a change to the name is a change to. A name designating storage is
  // watched at the members it designates; one this interface computes is
  // watched at every member its expression reads, since a call shows nothing to
  // wait on.
  const std::vector<hir::PublishedMemberId> watched = std::visit(
      Overloaded{
          [](const hir::ViewDefinedPlace& place) {
            std::vector<hir::PublishedMemberId> members;
            members.reserve(place.parts.size());
            for (const hir::MemberProjection& part : place.parts) {
              members.push_back(part.member);
            }
            return members;
          },
          [](const hir::ViewComputedValue& computed) {
            return computed.observes;
          }},
      name->meaning);

  std::vector<hir::SensitivityEntry> out;
  out.reserve(watched.size());
  for (const hir::PublishedMemberId id : watched) {
    const hir::PublishedMember member =
        unit_.external_unit_objects.Get(object).members.Get(id);
    const hir::RoutedRefId slot = MapOrGetRoutedRef(
        frame.Current(), hir::RoutedRefDecl{
                             .recipe = hir::RoutedPathRecipe{
                                 .head = route.head,
                                 .steps = route.steps,
                                 .leaf = hir::SignatureMemberLeaf{
                                     .object = object,
                                     .member = id,
                                     .storage = member.storage,
                                     .type = member.type}}});
    out.push_back(
        hir::SensitivityEntry{
            .ref = hir::ValueTarget{hir::ReferenceRoute{
                hir::RoutedRef{.id = slot}}},
            .footprint = std::nullopt});
  }
  return out;
}

auto UnitLowerer::TranslateSensitivityReads(
    const std::vector<SensitivityRead>& reads, const WalkFrame& frame)
    -> diag::Result<std::vector<hir::SensitivityEntry>> {
  std::vector<hir::SensitivityEntry> out;
  out.reserve(reads.size());
  for (const auto& read : reads) {
    const auto span = SourceMapper().PointSpanOf(read.symbol->location);
    auto resolved = ResolveReferent(*read.symbol, span);
    if (!resolved) return std::unexpected(std::move(resolved.error()));
    const slang::ast::ValueSymbol& target = *resolved->symbol;

    // A footprint is meaningful only for a signal the runtime bit-addresses: a
    // packed bit vector, which renders to one observable cell whose change set
    // is read per bit. For an enum, unpacked aggregate, string, or real the
    // runtime observes the whole signal on any change, so the read carries no
    // footprint regardless of the flat-bit view the DFA computed over its own
    // encoding.
    const auto observe = [&](hir::ValueTarget cell) {
      const slang::ast::Type& read_type = target.getType();
      out.push_back(
          hir::SensitivityEntry{
              .ref = std::move(cell),
              .footprint = read_type.isIntegral() && !read_type.isEnum()
                               ? read.footprint
                               : std::nullopt});
    };

    switch (resolved->kind) {
      // A name a view defined for itself stands for an expression the interface
      // evaluates (LRM 25.5.4), so it is no single declaration to wait on. What
      // waiting on it means is waiting on every member that expression reads,
      // which the interface publishes alongside the name.
      case Referent::kViewDefinedName: {
        auto entries = ObservedThroughModport(
            target.as<slang::ast::ModportPortSymbol>(), frame);
        if (!entries) return std::unexpected(std::move(entries.error()));
        out.insert(out.end(), entries->begin(), entries->end());
        break;
      }
      // A value fixed before simulation starts never changes, so a read of one
      // subscribes to nothing -- a parameter or an enumeration name (LRM 6.20,
      // 6.19), and a specparam, which LRM 6.20.4 makes a constant too however
      // little of it this compiler carries elsewhere.
      case Referent::kParameterConstant:
      case Referent::kEnumConstant:
      case Referent::kSpecparam:
        break;
      // LRM 9.2.2.2.1 excludes a variable the block itself declares, and the
      // surface a read set comes from has applied that already.
      case Referent::kPatternBinding:
        break;
      // The same clause excludes a reference to a class object, which a handle
      // to the invoking object is.
      case Referent::kThisHandle:
        break;
      // A static property is the one copy its class shares and is usable with
      // no object of that type (LRM 8.9), so it is a variable read within the
      // block like any other. An instance property is reached through an
      // object, which the clause above excludes.
      case Referent::kClassProperty: {
        const auto& prop = target.as<slang::ast::ClassPropertySymbol>();
        if (prop.lifetime != slang::ast::VariableLifetime::Static) break;
        auto property = ResolveStaticPropertyTarget(frame, prop, span);
        if (!property) return std::unexpected(std::move(property.error()));
        observe(hir::ValueTarget{*std::move(property)});
        break;
      }
      case Referent::kVariableStorage:
      case Referent::kNetStorage: {
        auto cell = ResolveValueTarget(frame, target, span);
        if (!cell) return std::unexpected(std::move(cell.error()));
        observe(*std::move(cell));
        break;
      }
      case Referent::kPrimitivePort:
      case Referent::kClockingSignal:
      case Referent::kAssertionLocal:
      case Referent::kStructureMember:
        return FailOnUnsupportedReferent(resolved->kind, span);
      case Referent::kNotAValue:
        throw InternalError(
            "TranslateSensitivityReads: a read resolved to a declaration that "
            "denotes no value");
    }
  }
  return out;
}

}  // namespace lyra::lowering::ast_to_hir
