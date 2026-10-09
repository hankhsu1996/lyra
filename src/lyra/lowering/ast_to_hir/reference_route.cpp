#include <algorithm>
#include <concepts>
#include <cstdint>
#include <expected>
#include <format>
#include <optional>
#include <span>
#include <string>
#include <unordered_set>
#include <utility>
#include <variant>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/HierarchicalReference.h>
#include <slang/ast/Scope.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/MemberSymbols.h>
#include <slang/ast/symbols/PortSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/ValueSymbol.h>
#include <slang/ast/types/Type.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/expr_builders.hpp"
#include "lyra/hir/published_modport.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/ast_to_hir/climb.hpp"
#include "lyra/lowering/ast_to_hir/connected_interface.hpp"
#include "lyra/lowering/ast_to_hir/expression/references.hpp"
#include "lyra/lowering/ast_to_hir/expression/view_names.hpp"
#include "lyra/lowering/ast_to_hir/generate_construct.hpp"
#include "lyra/lowering/ast_to_hir/instance_array_shape.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/sensitivity.hpp"
#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"
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

// The hop a name takes into `symbol` where this unit declares nothing about
// it, as the front end resolved the name: an instance or a set of them, a
// generate block, or a procedural scope. Nothing for anything else a path can
// pass, which no route yet walks.
auto NamedHopInto(const slang::ast::Symbol& symbol) -> std::optional<NamedHop> {
  const std::string name{symbol.name};
  if (symbol.kind == slang::ast::SymbolKind::Instance ||
      symbol.kind == slang::ast::SymbolKind::InstanceArray) {
    return InstanceHop{.name = name, .indices = {}};
  }
  if (const auto* block = symbol.as_if<slang::ast::GenerateBlockSymbol>()) {
    // A loop's block is named by its construct, and the value its index stood
    // at picks it out (LRM 27.4).
    if (block->getArrayIndex() != nullptr) {
      return LoopBlockHop{
          .loop = std::string{block->getParentScope()->asSymbol().name},
          .index = LoopIndexOf(*block)};
    }
    return LabeledBlockHop{.name = name};
  }
  if (IsProceduralScope(symbol)) {
    return ProceduralHop{.name = name};
  }
  return std::nullopt;
}

// How a name reaches `scope` from the scope standing above it where this unit
// declares nothing about it, and that scope. An instance is reached from where
// it was declared, together with the element of an array it is (LRM 23.3.2); a
// loop's block from above its construct, which is no scope a route steps
// through on its own (LRM 27.4).
struct NamedHopUp {
  std::optional<NamedHop> hop;
  const slang::ast::Scope* scope = nullptr;
};

auto NamedHopAbove(const slang::ast::Scope& scope) -> NamedHopUp {
  const slang::ast::Symbol& symbol = scope.asSymbol();
  if (const auto* body = symbol.as_if<slang::ast::InstanceBodySymbol>()) {
    const slang::ast::InstanceSymbol* inst = body->parentInstance;
    if (inst == nullptr) return NamedHopUp{};
    const slang::ast::Symbol& declared = OwnerOfInstance(*inst);
    return NamedHopUp{
        .hop =
            InstanceHop{
                .name = std::string{declared.name},
                .indices = {inst->arrayPath.begin(), inst->arrayPath.end()}},
        .scope = declared.getHierarchicalParent()};
  }
  const slang::ast::Scope* above = symbol.getHierarchicalParent();
  if (const auto* block = symbol.as_if<slang::ast::GenerateBlockSymbol>();
      block != nullptr && block->getArrayIndex() != nullptr) {
    above = above->asSymbol().getHierarchicalParent();
  }
  return NamedHopUp{.hop = NamedHopInto(symbol), .scope = above};
}

// What a step standing over `dims` leaves open once the name picked `picked`
// of them: a coordinate the name wrote settles the outermost dimension still
// open.
auto SettledDimensions(
    std::span<const hir::UnpackedRange> dims, std::size_t picked)
    -> std::vector<OpenDimension> {
  return WholeDimensions(dims.subspan(std::min(picked, dims.size())));
}

// The hops a name takes from `from` down to `to`, which stands in it, each
// resolved against what the scope above it published. Nothing where `to` does
// not stand in `from`, or the way down passes a scope no route walks.
auto NamedHopsDown(const slang::ast::Scope& from, const slang::ast::Scope& to)
    -> std::optional<std::vector<NamedHop>> {
  std::vector<NamedHop> hops;
  for (const slang::ast::Scope* scope = &to; scope != &from;) {
    if (scope == nullptr) return std::nullopt;
    auto above = NamedHopAbove(*scope);
    if (!above.hop.has_value()) return std::nullopt;
    hops.emplace_back(*std::move(above.hop));
    scope = above.scope;
  }
  std::ranges::reverse(hops);
  return hops;
}

// Adds a coordinate the name wrote to the hop it selects out of. False where
// that hop stands for no set of objects to select from.
auto AddCoordinate(DescentHop& hop, std::uint32_t position) -> bool {
  return std::visit(
      Overloaded{
          [&](DeclaredHop& declared) {
            declared.step.selects.push_back(position);
            return true;
          },
          [&](NamedHop& named) {
            return std::visit(
                Overloaded{
                    [&](InstanceHop& instance) {
                      instance.indices.push_back(position);
                      return true;
                    },
                    [](LoopBlockHop&) { return false; },
                    [](LabeledBlockHop&) { return false; },
                    [](ProceduralHop&) { return false; }},
                named);
          }},
      hop);
}

// Where a scope published the generate block a name selects: the construct,
// the select picking the block out of it, and the class the block was
// published as.
struct PublishedBlockAt {
  hir::PublishedGenerateId generate;
  std::vector<std::uint32_t> selects;
  std::string class_name;
};

// A loop's block is found by the value its index stood at, and selected by its
// position among the blocks the loop published (LRM 27.4). Nothing where the
// scope published no such block.
auto FindPublishedBlock(
    const base::Arena<hir::PublishedGenerate, hir::PublishedGenerateId>&
        generates,
    const LoopBlockHop& hop) -> std::optional<PublishedBlockAt> {
  for (const hir::PublishedGenerateId at : generates.Ids()) {
    auto found = std::visit(
        Overloaded{
            [&](const hir::PublishedLoop& loop)
                -> std::optional<PublishedBlockAt> {
              if (loop.name != hop.loop) return std::nullopt;
              for (std::uint32_t block = 0; block < loop.blocks.size();
                   ++block) {
                if (loop.blocks[block].index == hop.index) {
                  return PublishedBlockAt{
                      .generate = at,
                      .selects = {block},
                      .class_name = loop.blocks[block].class_name};
                }
              }
              return std::nullopt;
            },
            [](const hir::PublishedChoice&) -> std::optional<PublishedBlockAt> {
              return std::nullopt;
            }},
        generates.Get(at));
    if (found.has_value()) return found;
  }
  return std::nullopt;
}

// A block that stands alone or was chosen by a conditional is found by the
// name the source gave it, and is the one block its construct holds (LRM
// 27.5).
auto FindPublishedBlock(
    const base::Arena<hir::PublishedGenerate, hir::PublishedGenerateId>&
        generates,
    const LabeledBlockHop& hop) -> std::optional<PublishedBlockAt> {
  for (const hir::PublishedGenerateId at : generates.Ids()) {
    auto found = std::visit(
        Overloaded{
            [](const hir::PublishedLoop&) -> std::optional<PublishedBlockAt> {
              return std::nullopt;
            },
            [&](const hir::PublishedChoice& choice)
                -> std::optional<PublishedBlockAt> {
              for (const hir::PublishedAlternative& block : choice.blocks) {
                if (block.name == hop.name) {
                  return PublishedBlockAt{
                      .generate = at,
                      .selects = {},
                      .class_name = block.class_name};
                }
              }
              return std::nullopt;
            }},
        generates.Get(at));
    if (found.has_value()) return found;
  }
  return std::nullopt;
}

}  // namespace

auto UnitLowerer::RoutesOf(ScopeFrameId owner_frame) -> hir::ScopeRoutes& {
  return routes_by_frame_[owner_frame];
}

auto UnitLowerer::TakeRoutesForFrame(ScopeFrameId owner_frame)
    -> hir::ScopeRoutes {
  const auto it = routes_by_frame_.find(owner_frame);
  if (it == routes_by_frame_.end()) {
    return {};
  }
  auto out = std::move(it->second);
  routes_by_frame_.erase(it);
  return out;
}

namespace {

// The id of `walk` in `table`, reusing one already there. Two walks that
// navigate the same way to the same end are one; two that reach it differently
// are two, and the walk is what says which. An object reached through a port
// and the same object named hierarchically coincide only in the instance being
// lowered -- the port reaches whatever it was bound to, the name reaches what
// it names -- so sharing one between them would make the second follow the
// first's binding.
template <typename Walk, typename Id>
auto MapOrGetRoute(base::Arena<Walk, Id>& table, Walk walk) -> Id {
  for (const Id id : table.Ids()) {
    if (table.Get(id) == walk) return id;
  }
  return table.Add(std::move(walk));
}

}  // namespace

auto UnitLowerer::MakeRoutedMemberRef(
    ScopeFrameId owner_frame, hir::ValueRoute route, diag::SourceSpan span)
    -> hir::Expr {
  const hir::TypeId type = hir::CellOf(route.leaf).type;
  const hir::RoutedValueRefId id =
      MapOrGetRoute(RoutesOf(owner_frame).values, std::move(route));
  return hir::MakeRefExpr(hir::RoutedValueRef{.id = id}, type, span);
}

auto UnitLowerer::ExternalMemberLeafOf(
    hir::ExternalScopeClassId scope_class, hir::PublishedMemberId member) const
    -> hir::ExternalMemberLeaf {
  // The member's type is already in this unit's pool, because the record
  // brought it there.
  const hir::PublishedMember& published =
      unit_.external_scope_classes.Get(scope_class)
          .signature.members.Get(member);
  return hir::ExternalMemberLeaf{
      .scope_class = scope_class,
      .member = member,
      .storage = published.storage,
      .type = published.type};
}

auto UnitLowerer::ResolveRouteTarget(
    const slang::ast::ValueSymbol& value, const ScopeRoute& route)
    -> diag::Result<hir::DataLeaf> {
  const auto span = SourceMapper().PointSpanOf(value.location);
  const auto unsupported = [&] {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        std::format("reaching `{}` this way is not yet supported", value.name));
  };

  return std::visit(
      Overloaded{
          // A route that landed on a scope of another unit names what that
          // scope published, under the named blocks and subroutines the name
          // went on through, and the signature also states what storage it is
          // -- so nothing about it is read off the unit that declared it. Every
          // declaration a name may reach is published (LRM 23.6), so one that
          // is not is a form this compiler does not publish yet.
          [&](const InExternalScope& on) -> diag::Result<hir::DataLeaf> {
            const auto member =
                unit_.external_scope_classes.Get(on.scope_class)
                    .signature.FindMember(value.name, on.within);
            if (!member.has_value()) return unsupported();
            return ExternalMemberLeafOf(on.scope_class, *member);
          },
          // A route that stays in this unit's layout names this unit's own
          // declaration. For a static a name reaches through a procedural
          // scope (LRM 23.9 puts a block, a task and a function on that path
          // alike), that identity also says every such scope between the
          // static and its structural one describes where the storage sits
          // rather than a step the route takes.
          // A value is a member of one object, so a walk standing on several
          // has not reached one.
          [&](const OnSeveralObjects&) -> diag::Result<hir::DataLeaf> {
            return unsupported();
          },
          [&](const InOwnScope&) -> diag::Result<hir::DataLeaf> {
            auto type = InternType(value.getType(), span);
            if (!type) return std::unexpected(std::move(type.error()));
            if (const auto data_object =
                    LookupStructuralDataObjectBinding(value)) {
              return hir::StructuralDataObjectLeaf{
                  .object = data_object->var_id,
                  .storage = DeclarationStorage(value),
                  .type = *type};
            }
            if (const auto procedural_static = LookupProceduralStatic(value)) {
              return hir::ProceduralStaticLeaf{
                  .body = procedural_static->body,
                  .var = procedural_static->var,
                  .type = *type};
            }
            return unsupported();
          }},
      route.place);
}

auto UnitLowerer::MakeRoutedValueRef(
    const slang::ast::ValueSymbol& value, ScopeFrameId owner_frame,
    ScopeRoute route) -> diag::Result<hir::RoutedValueRef> {
  auto leaf = ResolveRouteTarget(value, route);
  if (!leaf) return std::unexpected(std::move(leaf.error()));
  const hir::RoutedValueRefId id = MapOrGetRoute(
      RoutesOf(owner_frame).values, hir::ValueRoute{
                                        .base = std::move(route.base),
                                        .steps = std::move(route.steps),
                                        .leaf = *std::move(leaf)});
  return hir::RoutedValueRef{.id = id};
}

auto UnitLowerer::MakeRoutedObjectRef(
    ScopeFrameId owner_frame, ScopeRoute route, hir::TypeId object_type)
    -> hir::RoutedObjectRef {
  const hir::RoutedObjectRefId id = MapOrGetRoute(
      RoutesOf(owner_frame).objects,
      hir::ObjectRoute{
          .base = std::move(route.base),
          .steps = std::move(route.steps),
          .leaf = hir::ScopeLeaf{.type = object_type}});
  return hir::RoutedObjectRef{.id = id};
}

auto UnitLowerer::DisableTargetOf(
    const WalkFrame& frame, const slang::ast::Symbol& target,
    const slang::ast::HierarchicalReference& reference, diag::SourceSpan span)
    -> diag::Result<hir::DisableTarget> {
  const auto refuse = [&] {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedStatementForm,
        "a disable of a block or task reached this way is not yet supported");
  };
  // A name leaving the instance reaches the target of another instance, which
  // only its publication says anything about, whatever this unit minted for
  // the scope it resolved to here; only a name staying inside the instance
  // reaches a scope this unit minted.
  RouteOrigin origin = StartOf(frame, reference);
  const auto minted = std::holds_alternative<FromReader>(origin)
                          ? LookupMintedProceduralScope(target)
                          : std::nullopt;
  // A scope's identity indexes its declaring scope's registry, so one the
  // body's own declaration scope declares is named outright.
  if (minted.has_value() && minted->owner == &frame.ProceduralScopeOwner()) {
    return hir::DisableTarget{hir::DirectDisableTarget{.scope = minted->scope}};
  }
  // Any other is reached by a route. One carrying an identity runs to the
  // scope that minted it, and the procedural scopes between are where the
  // target sits rather than steps of their own -- the same reading a static
  // declared in one of them takes. A target of another unit is reached the
  // same way: the route runs through the procedural scopes down to the target
  // itself, and that path is what the declaring scope published it by.
  const slang::ast::Scope* walk_to =
      minted.has_value() ? minted->owner : target.as_if<slang::ast::Scope>();
  if (walk_to == nullptr) {
    throw InternalError(
        "UnitLowerer::DisableTargetOf: a disable names a block or a task, each "
        "of which defines a scope");
  }
  auto route = RouteToScope(frame, *walk_to, std::move(origin));
  if (!route.has_value()) return refuse();
  std::optional<hir::DisableLeaf> leaf;
  if (minted.has_value()) {
    leaf = hir::DisableTargetLeaf{.scope = minted->scope};
  } else if (const auto* on = std::get_if<InExternalScope>(&route->place)) {
    const std::optional<hir::PublishedDisableTargetId> published =
        unit_.external_scope_classes.Get(on->scope_class)
            .signature.FindDisableTarget(on->within);
    if (published.has_value()) {
      leaf = hir::ExternalDisableTargetLeaf{
          .scope_class = on->scope_class, .target = *published};
    }
  }
  if (!leaf.has_value()) return refuse();
  const hir::RoutedDisableTargetRefId id = MapOrGetRoute(
      RoutesOf(frame.Current()).disable_targets,
      hir::DisableTargetRoute{
          .base = std::move(route->base),
          .steps = std::move(route->steps),
          .leaf = *std::move(leaf)});
  return hir::DisableTarget{hir::RoutedDisableTarget{
      .target = hir::RoutedDisableTargetRef{.id = id}}};
}

auto UnitLowerer::ReaderInstance() const -> std::optional<ReaderClimbs> {
  const auto* body =
      SourceScope().asSymbol().as_if<slang::ast::InstanceBodySymbol>();
  if (body == nullptr) return std::nullopt;
  return ReaderClimbs{
      .body = body,
      .climbs = Specialization().ClimbsOutOf(InstantiationOf(*body))};
}

auto UnitLowerer::StartOf(
    const WalkFrame& frame, const slang::ast::HierarchicalReference& reference)
    -> RouteOrigin {
  const std::optional<ReaderClimbs> reader = ReaderInstance();
  if (!reader.has_value()) return RouteOrigin{FromReader{}};

  // Through a port the name stands in whatever instance the port was bound to,
  // and a coordinate written after the port picks one where it stands for
  // several (LRM 25.3). A coordinate past those, on a loop generate inside the
  // instance, is the descent's own (LRM 27.4).
  if (reference.isViaIfacePort()) {
    const auto& port =
        reference.path.front().symbol->as<slang::ast::InterfacePortSymbol>();
    PortReach reach = ReachOfPort(frame, port);
    // The instance the name stands in is the one bound where its selects land:
    // a port standing for one is bound to one, and the front end names the
    // element a select picks out of one carrying a range.
    const auto connected = ConnectedInterfaceOf(port.getConnection()).instances;
    const slang::ast::InstanceSymbol* bound =
        reach.hop.dims.empty() && connected.size() == 1 ? connected[0]
                                                        : nullptr;
    for (std::size_t at = 1; at < reference.path.size(); ++at) {
      // The front end lists a loop generate by its name, with no selector,
      // before the block its select picks, so the port's own selects end
      // there.
      const auto* position =
          std::get_if<std::int32_t>(&reference.path[at].selector);
      if (position == nullptr) break;
      if (*position < 0) {
        throw InternalError(
            "UnitLowerer::StartOf: the front end resolves a coordinate on a "
            "port to a position in its range");
      }
      reach.hop.step.selects.push_back(static_cast<std::uint32_t>(*position));
      if (const auto* element =
              reference.path[at].symbol->as_if<slang::ast::InstanceSymbol>()) {
        bound = element;
      }
    }
    // A name continuing through the port names a member of one instance, so
    // it selects one in every dimension the port declares (LRM 23.6, 25.3).
    if (bound == nullptr ||
        reach.hop.step.selects.size() != reach.hop.dims.size()) {
      throw InternalError(
          "UnitLowerer::StartOf: a name through an interface port picks one of "
          "the instances it stands for -- please report this as a bug");
    }
    reach.hop.place = PlaceThroughPort(port, reach.hop.step.selects);
    return RouteOrigin{RouteStart{
        .below = &bound->body,
        .base = std::move(reach.base),
        .leading = std::move(reach.hop)}};
  }

  // Upward the name stands in the instance it landed in, or in the one
  // holding the generate block it landed in; the unit is told apart by which
  // block that was, so the walk down to it is the same in every instance of
  // the unit. A name that never leaves the instance starts at the reader.
  const std::optional<ClimbAnchor> climb = ClimbOutOf(reference, *reader->body);
  if (!climb.has_value()) return RouteOrigin{FromReader{}};
  return RouteOrigin{StartInEnclosing(*climb->instance)};
}

auto UnitLowerer::StartsOfNames(
    const WalkFrame& frame,
    std::span<const slang::ast::Expression* const> names)
    -> std::vector<RouteOrigin> {
  std::vector<RouteOrigin> origins;
  bool from_reader = names.empty();
  for (const slang::ast::Expression* name : names) {
    const auto* hierarchical =
        name->as_if<slang::ast::HierarchicalValueExpression>();
    if (hierarchical == nullptr) {
      from_reader = true;
      continue;
    }
    RouteOrigin origin = StartOf(frame, hierarchical->ref);
    if (std::holds_alternative<FromReader>(origin)) {
      from_reader = true;
    } else {
      origins.push_back(std::move(origin));
    }
  }
  if (from_reader) origins.emplace_back(FromReader{});
  return origins;
}

auto UnitLowerer::TranslateReferenceRoute(
    const WalkFrame& frame, const slang::ast::ValueSymbol& value,
    RouteOrigin origin) -> diag::Result<std::optional<hir::RoutedValueRef>> {
  // A name that left the reader's instance reaches whatever stands where it
  // starts, which is this unit's own declaration only in the instance being
  // lowered, so it is never read as one.
  if (std::holds_alternative<RouteStart>(origin)) {
    auto route =
        RouteToScope(frame, *value.getHierarchicalParent(), std::move(origin));
    if (!route.has_value()) return std::nullopt;
    auto reference = MakeRoutedValueRef(value, frame.Current(), *route);
    if (!reference) return std::unexpected(std::move(reference.error()));
    return *reference;
  }

  // This unit's own identity for the target, if it declares it.
  const auto data_object = LookupStructuralDataObjectBinding(value);
  const auto procedural_static = LookupProceduralStatic(value);
  const std::optional<hir::StructuralHops> data_object_hops =
      data_object ? frame.HopsTo(data_object->home_frame) : std::nullopt;

  const auto routed_ref = [&](ScopeFrameId owner_frame, ScopeRoute route)
      -> diag::Result<std::optional<hir::RoutedValueRef>> {
    auto reference = MakeRoutedValueRef(value, owner_frame, std::move(route));
    if (!reference) return std::unexpected(std::move(reference.error()));
    return *reference;
  };

  // The target's storage is the reader's own scope's or hangs under a scope
  // enclosing it in the same unit (LRM 23.9), so the whole route is a count of
  // parent edges, and zero of them for the reader's own scope.
  const auto in_unit_route = [&](hir::StructuralHops hops) {
    return routed_ref(frame.Current(), ScopeRoute::Enclosing(hops));
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

  auto route = RouteToScope(frame, *owner, FromReader{});
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
  // hop below take the classification every descent takes, so a hop the
  // interface published resolves against that publication exactly as it does
  // past a module instance.
  PortReach reach = ReachOfPort(frame, *port);
  ScopeRoute route{
      .base = std::move(reach.base),
      .steps = {},
      .place = InOwnScope{},
      .open = {}};
  std::vector<DescentHop> hops;
  hops.reserve(path.size());
  hops.emplace_back(std::move(reach.hop));

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
    // The front end lists a loop generate by its name, with no selector, before
    // the block its select picks. The loop is no scope a name stands in, and
    // the block states which value of the loop's index it stands at, so the
    // block is the hop (LRM 27.4).
    if (path[hop].symbol->kind == slang::ast::SymbolKind::GenerateBlockArray) {
      continue;
    }
    // A coordinate selects out of what the hop before it reached (LRM 25.3), so
    // it is part of that hop rather than one of its own.
    const auto* block =
        path[hop].symbol->as_if<slang::ast::GenerateBlockSymbol>();
    if (const auto* position = std::get_if<std::int32_t>(&path[hop].selector);
        position != nullptr && block == nullptr) {
      if (*position < 0 ||
          !AddCoordinate(hops.back(), static_cast<std::uint32_t>(*position))) {
        return std::nullopt;
      }
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
    auto named = NamedHopInto(*path[hop].symbol);
    if (!named.has_value()) return std::nullopt;
    hops.emplace_back(*std::move(named));
    if (path[hop].symbol == reference.target) {
      break;
    }
  }
  // What the port's hop stands on is settled by the selects the name wrote
  // for it: the instance they pick out, or the several they leave.
  auto& through_port = std::get<DeclaredHop>(hops.front());
  through_port.place = PlaceThroughPort(*port, through_port.step.selects);
  if (!ClassifyDescent(route, hops)) return std::nullopt;
  auto open = NarrowOutermost(std::move(route.open), part);
  if (!open.has_value()) return std::nullopt;
  route.open = *std::move(open);
  return route;
}

auto UnitLowerer::ReachOfPort(
    const WalkFrame& frame, const slang::ast::InterfacePortSymbol& port)
    -> PortReach {
  const auto binding = LookupInterfacePortBinding(port);
  if (!binding.has_value()) {
    throw InternalError(
        "UnitLowerer::ReachOfPort: the path starts at a port of this unit, "
        "which the unit's own walk declared");
  }
  const auto hops = frame.HopsTo(binding->home_frame);
  if (!hops.has_value()) {
    throw InternalError(
        "UnitLowerer::ReachOfPort: an interface port is a member of a scope "
        "enclosing every reader of it");
  }
  // The hop lands on the port itself, so every object it stands for is still
  // in play; a name that picks one out of them says so in a coordinate the
  // caller adds to it, and settles the place again once it has.
  return PortReach{
      .base = hir::InUnitBase{.hops = *hops},
      .hop = DeclaredHop{
          .step = hir::PathStep{.names = binding->port, .selects = {}},
          .place = PlaceThroughPort(port, {}),
          .dims = InterfacePortObjects(port).ranges}};
}

auto UnitLowerer::PlaceThroughPort(
    const slang::ast::InterfacePortSymbol& port,
    std::span<const std::uint32_t> selects) -> RoutePlace {
  const hir::UnitObjectsType objects = InterfacePortObjects(port);
  if (selects.size() != objects.ranges.size()) return OnSeveralObjects{};
  const hir::UnitObjectType& kind = objects.KindAt(selects);
  return InExternalScope{
      .scope_class = ExternalScopeClassOf(kind.unit_name, kind.class_name),
      .within = {}};
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

  auto route = RouteToScope(frame, target, FromReader{});
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
  const auto* in_unit = std::get_if<hir::InUnitBase>(&route->base);
  if (in_unit == nullptr) {
    return refuse(
        "a subroutine reached by a name anchored outside this module is not "
        "yet supported");
  }
  InUnitReach reach{.hops = in_unit->hops, .descent = {}};
  reach.descent.reserve(route->steps.size());
  for (const hir::PathStep& step : route->steps) {
    const auto* owned = std::get_if<hir::OwnedChildRef>(&step.names);
    if (owned == nullptr ||
        std::holds_alternative<hir::InstanceMemberId>(*owned)) {
      return refuse(
          "a subroutine reached through a scope this module does not lay out "
          "is not yet supported");
    }
    reach.descent.push_back(
        hir::OwnedChildStep{.names = *owned, .selects = step.selects});
  }
  return reach;
}

auto UnitLowerer::RouteToScopeOrRefuse(
    const WalkFrame& frame, const slang::ast::Scope& target, RouteOrigin origin,
    diag::SourceSpan span) -> diag::Result<ScopeRoute> {
  auto walked = RouteToScope(frame, target, std::move(origin));
  if (!walked.has_value()) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "a scope reached this way is not yet supported");
  }
  return *std::move(walked);
}

auto UnitLowerer::RouteToScope(
    const WalkFrame& frame, const slang::ast::Scope& target, RouteOrigin origin)
    -> std::optional<ScopeRoute> {
  // A name that left the reader's instance starts where it landed, which is a
  // scope of whatever instance stands there rather than of this unit's own
  // object -- even where that instance is of this unit -- so every hop below it
  // is resolved against what the scope above it published.
  if (auto* start = std::get_if<RouteStart>(&origin)) {
    auto below = NamedHopsDown(*start->below, target);
    if (!below.has_value()) return std::nullopt;
    std::vector<DescentHop> hops;
    hops.reserve(below->size() + 1);
    if (start->leading.has_value()) {
      hops.emplace_back(*std::move(start->leading));
    }
    for (NamedHop& hop : *below) {
      hops.emplace_back(std::move(hop));
    }
    ScopeRoute route{
        .base = std::move(start->base),
        .steps = {},
        .place = InOwnScope{},
        .open = {}};
    if (!ClassifyDescent(route, hops)) return std::nullopt;
    return route;
  }

  // The reader's elaborated ancestor scopes, across unit boundaries (slang's
  // `getHierarchicalParent` crosses the boundary at the instance-body
  // transition). The route meets the target at the deepest scope shared with
  // the reader; a target-side hop whose parent is one of these scopes is the
  // named child that shared ancestor exposes, which the route climbs to and
  // descends from.
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
  // A hop this unit does not declare is a step through what the scope above
  // it published (LRM 23.6, 25.10). Which scope stands above a hop is what
  // every hop before it decided, and this walk runs from the target upward, so
  // it records how the name reached each hop and leaves resolving it to a
  // forward pass over the whole descent.
  std::vector<DescentHop> descent;
  const slang::ast::Scope* scope = &target;
  while (scope != nullptr) {
    const slang::ast::Symbol* owned = &scope->asSymbol();
    // A route navigates the object tree, and a namespace unit -- a package (LRM
    // 26.2) or the `$unit` scope (LRM 3.12.1) -- has no instance and so no
    // object on it. Nothing declared there is reachable this way, whatever else
    // the walk would have found above it, so the walk answers with no route
    // rather than a base naming a scope the runtime never builds.
    if (owned->kind == slang::ast::SymbolKind::Package ||
        owned->kind == slang::ast::SymbolKind::CompilationUnit) {
      return std::nullopt;
    }
    // How the name reaches the hop where this unit declares nothing about it,
    // and the scope above it.
    auto [named, next] = NamedHopAbove(*scope);
    std::vector<std::uint32_t> indices;
    // The class of the unit an instance this one declares is built from, read
    // off the declaration -- this unit naming its own child. A hop it does not
    // declare is resolved against what the scope above it published instead.
    RoutePlace place = InOwnScope{};
    if (owned->kind == slang::ast::SymbolKind::InstanceBody) {
      const auto* inst =
          owned->as<slang::ast::InstanceBodySymbol>().parentInstance;
      if (inst == nullptr) return std::nullopt;
      place = InExternalScope{
          .scope_class = ScopeClassOfInstance(*inst), .within = {}};
      // The step is the member registered for the instance, which is the whole
      // array where it is an element of one.
      indices.assign(inst->arrayPath.begin(), inst->arrayPath.end());
      owned = &OwnerOfInstance(*inst);
    }

    // The hop onto a child this unit declares: the element its binding names,
    // with the element of an instance array this is selected after it.
    const auto owned_hop = [&](hir::OwnedChildStep step) {
      step.selects.insert(step.selects.end(), indices.begin(), indices.end());
      return DeclaredHop{
          .step = hir::AsPathStep(std::move(step)), .place = place, .dims = {}};
    };

    // The route's first step is onto the child of the deepest scope shared
    // with the reader: the first hop whose parent scope is a reader ancestor.
    // Everything already accumulated is the descent below it.
    if (next != nullptr && reader_ancestors.contains(next)) {
      // A child whose owning scope this unit emits stays inside this unit's
      // layout: the climb to that scope is typed, and the step onto the child
      // is the route's first typed step. A scope this unit does not lay out is
      // reached from where the route starts outside the reader's instance,
      // which the caller states.
      const auto obinding = LookupOwnedChildBinding(*owned);
      const auto hops = obinding.has_value()
                            ? frame.HopsTo(obinding->home_frame)
                            : std::nullopt;
      if (!hops.has_value()) return std::nullopt;
      descent.emplace_back(owned_hop(obinding->step));
      std::ranges::reverse(descent);
      ScopeRoute route{
          .base = hir::InUnitBase{.hops = *hops},
          .steps = {},
          .place = InOwnScope{},
          .open = {}};
      if (!ClassifyDescent(route, descent)) return std::nullopt;
      return route;
    }

    // A step this unit declares stays inside its layout and carries the
    // declaring scope's identity; one it does not is past the artifact
    // boundary, and is resolved against what the scope above it published.
    if (const auto obinding = LookupOwnedChildBinding(*owned)) {
      descent.emplace_back(owned_hop(obinding->step));
    } else {
      if (!named.has_value()) return std::nullopt;
      descent.emplace_back(*std::move(named));
    }
    scope = next;
  }
  return std::nullopt;
}

auto UnitLowerer::ClassifyDescent(ScopeRoute& route, std::span<DescentHop> hops)
    -> bool {
  // A route starting at the enclosing instance of another unit stands on that
  // unit's object from the start.
  RoutePlace standing = std::visit(
      Overloaded{
          [](const hir::InUnitBase&) -> RoutePlace { return InOwnScope{}; },
          [](const hir::EnclosingInstanceBase& base) -> RoutePlace {
            return InExternalScope{
                .scope_class = base.scope_class, .within = {}};
          }},
      route.base);
  std::optional<Descent> descended = DescendFrom(std::move(standing), hops);
  if (!descended.has_value()) return false;
  route.steps = std::move(descended->steps);
  route.place = std::move(descended->place);
  route.open = std::move(descended->open);
  return true;
}

auto UnitLowerer::DescendPublished(
    hir::ExternalScopeClassId from, const slang::ast::Scope& from_scope,
    const slang::ast::Scope& to) -> std::optional<PublishedDescent> {
  auto hops = NamedHopsDown(from_scope, to);
  if (!hops.has_value()) return std::nullopt;
  return DescendPublishedFrom(from, *hops);
}

auto UnitLowerer::DescendFrom(RoutePlace standing, std::span<DescentHop> hops)
    -> std::optional<Descent> {
  // The hops this unit declares come first, each landing where its own
  // declaration says; from the first it does not, the descent has left this
  // unit's layout and the rest is resolved against what each scope published.
  Descent descended{.steps = {}, .place = std::move(standing), .open = {}};
  std::size_t at = 0;
  for (; at < hops.size(); ++at) {
    auto* declared = std::get_if<DeclaredHop>(&hops[at]);
    if (declared == nullptr) break;
    descended.place = std::move(declared->place);
    descended.open =
        SettledDimensions(declared->dims, declared->step.selects.size());
    descended.steps.push_back(std::move(declared->step));
  }
  if (at == hops.size()) return descended;

  // What a scope published is what the rest is resolved against, so the walk
  // has to stand on one.
  const auto* on = std::get_if<InExternalScope>(&descended.place);
  if (on == nullptr) return std::nullopt;
  std::vector<NamedHop> named;
  named.reserve(hops.size() - at);
  for (; at < hops.size(); ++at) {
    auto* hop = std::get_if<NamedHop>(&hops[at]);
    if (hop == nullptr) return std::nullopt;
    named.push_back(std::move(*hop));
  }
  auto published = DescendPublishedFrom(on->scope_class, named);
  if (!published.has_value()) return std::nullopt;
  for (hir::ExternalStep& step : published->steps) {
    descended.steps.push_back(hir::AsPathStep(std::move(step)));
  }
  descended.place = std::move(published->place);
  descended.open = std::move(published->open);
  return descended;
}

auto UnitLowerer::DescendPublishedFrom(
    hir::ExternalScopeClassId standing, std::span<NamedHop> hops)
    -> std::optional<PublishedDescent> {
  PublishedDescent descended{
      .steps = {},
      .place = InExternalScope{.scope_class = standing, .within = {}},
      .open = {}};
  // A hop onto an instance, or a set of them, is a step onto the member the
  // scope published for it; that member's type says how many objects it
  // stands for and which kind stands at each position. Selects naming one
  // position land on the scope of the kind there; selects leaving a dimension
  // open stand on several objects, past which no name resolves (LRM 23.6), and
  // whoever picks one out of them reads its kind off the same set.
  const auto onto_instance = [&](const InExternalScope& on,
                                 InstanceHop& instance) -> bool {
    const hir::ScopeClassSignature& record =
        unit_.external_scope_classes.Get(on.scope_class).signature;
    const auto member = record.FindMember(instance.name);
    if (!member.has_value()) return false;
    const auto* objects = unit_.types.Get(record.members.Get(*member).type)
                              .As<hir::UnitObjectsType>();
    if (objects == nullptr) return false;
    descended.open =
        SettledDimensions(objects->ranges, instance.indices.size());
    // Copied out, since naming the kind's class below may add to the pool the
    // set was read from.
    const std::optional<hir::UnitObjectType> lands_on =
        instance.indices.size() == objects->ranges.size()
            ? std::optional{objects->KindAt(instance.indices)}
            : std::nullopt;
    descended.steps.push_back(
        hir::ExternalStep{
            .names =
                hir::ExternalMemberRef{
                    .scope_class = on.scope_class, .member = *member},
            .selects = std::move(instance.indices)});
    if (!lands_on.has_value()) {
      descended.place = OnSeveralObjects{};
      return true;
    }
    descended.place = InExternalScope{
        .scope_class =
            ExternalScopeClassOf(lands_on->unit_name, lands_on->class_name),
        .within = {}};
    return true;
  };

  // A hop into a generate block is a step through the construct the scope
  // published it under, onto the class that block was published as.
  const auto into_block = [&](const InExternalScope& on,
                              const auto& block) -> bool {
    const hir::ExternalScopeClass& record =
        unit_.external_scope_classes.Get(on.scope_class);
    auto found = FindPublishedBlock(record.signature.generates, block);
    if (!found.has_value()) return false;
    const std::string unit_name = record.unit_name;
    const hir::ExternalScopeClassId result_class =
        ExternalScopeClassOf(unit_name, found->class_name);
    descended.open.clear();
    descended.steps.push_back(
        hir::ExternalStep{
            .names =
                hir::ExternalGenerateRef{
                    .scope_class = on.scope_class,
                    .generate = found->generate,
                    .result_class = result_class},
            .selects = std::move(found->selects)});
    descended.place =
        InExternalScope{.scope_class = result_class, .within = {}};
    return true;
  };

  // A named block or subroutine is no object, so nothing below one is either
  // and no step follows it. Nothing resolves past several objects either.
  for (NamedHop& hop : hops) {
    auto* on = std::get_if<InExternalScope>(&descended.place);
    if (on == nullptr) return std::nullopt;
    const bool past_procedural = !on->within.empty();
    const bool resolved = std::visit(
        Overloaded{
            [&](InstanceHop& instance) {
              return !past_procedural && onto_instance(*on, instance);
            },
            [&](const LoopBlockHop& block) {
              return !past_procedural && into_block(*on, block);
            },
            [&](const LabeledBlockHop& block) {
              return !past_procedural && into_block(*on, block);
            },
            [&](ProceduralHop& procedural) {
              on->within.push_back(std::move(procedural.name));
              return true;
            }},
        hop);
    if (!resolved) return std::nullopt;
  }
  return descended;
}

auto UnitLowerer::NameFilledDuringElaboration(
    const slang::ast::Symbol& named) const -> const slang::ast::ValueSymbol* {
  if (named.kind != slang::ast::SymbolKind::Parameter &&
      named.kind != slang::ast::SymbolKind::Genvar) {
    return nullptr;
  }
  const auto* value = named.as_if<slang::ast::ValueSymbol>();
  if (value == nullptr) {
    return nullptr;
  }
  return LookupStructuralDataObjectBinding(*value).has_value() ? value
                                                               : nullptr;
}

auto UnitLowerer::ResolveValueTarget(
    const WalkFrame& frame, const slang::ast::ValueSymbol& value,
    RouteOrigin origin, diag::SourceSpan span)
    -> diag::Result<hir::ValueTarget> {
  // A namespace unit has no instance, so its cell is reached by name rather
  // than by a route out of the reader's own storage (LRM 26.2, 3.12.1). The
  // same by-name form serves a referrer in another unit and the owning unit's
  // own body, neither of which has a receiver to route through.
  if (const auto* unit = DeclaringUnitOfValue(value)) {
    auto value_type = InternType(value.getType(), span);
    if (!value_type) return std::unexpected(std::move(value_type.error()));
    return hir::ValueTarget{hir::ExternalUnitValueRef{
        .unit_name = CompilationUnitName(*unit, Specialization()),
        .variable_name = std::string{value.name},
        .value_type = *value_type}};
  }

  auto route = TranslateReferenceRoute(frame, value, std::move(origin));
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

auto UnitLowerer::StartInEnclosing(const slang::ast::InstanceBodySymbol& body)
    -> RouteStart {
  return RouteStart{
      .below = &body,
      .base =
          hir::EnclosingInstanceBase{
              .scope_class = ScopeClassOfInstance(InstantiationOf(body))},
      .leading = std::nullopt};
}

auto UnitLowerer::StartReaching(const slang::ast::Scope& target)
    -> std::optional<RouteOrigin> {
  const std::optional<ReaderClimbs> reader = ReaderInstance();
  if (!reader.has_value()) return std::nullopt;
  const auto start = StartOfReach(target, *reader->body, reader->climbs);
  if (!start.has_value()) return std::nullopt;
  return std::visit(
      Overloaded{
          [](const FromReader&) { return RouteOrigin{FromReader{}}; },
          [&](const FromInstance& from) {
            return RouteOrigin{StartInEnclosing(*from.body)};
          }},
      *start);
}

auto UnitLowerer::RouteToClassScope(
    const WalkFrame& frame, const slang::ast::ClassType& cls)
    -> std::optional<ScopeRoute> {
  const slang::ast::Scope& scope = ReplicatingScope(cls);
  auto origin = StartReaching(scope);
  if (!origin.has_value()) return std::nullopt;
  return RouteToScope(frame, scope, *std::move(origin));
}

auto UnitLowerer::DeclaringInstanceFrom(
    const slang::ast::ClassType& cls, const WalkFrame& frame,
    diag::SourceSpan span)
    -> diag::Result<std::optional<hir::DeclaringInstanceReach>> {
  auto takes = TakesDeclaringInstance(cls, span);
  if (!takes) return std::unexpected(std::move(takes.error()));
  if (!*takes) return std::nullopt;
  if (HeldByAnotherDesignElement(cls)) {
    auto route = RouteToClassScope(frame, cls);
    const auto* on = route.has_value()
                         ? std::get_if<InExternalScope>(&route->place)
                         : nullptr;
    if (on == nullptr) {
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedClassFeature,
          "reaching the instance a class another instance declares belongs "
          "to is not yet supported from here");
    }
    const hir::TypeId object_type = ScopeClassTypeOf(on->scope_class);
    return hir::DeclaringInstanceReach{
        MakeRoutedObjectRef(frame.Current(), *std::move(route), object_type)};
  }
  auto hops = DeclaringScopeHopsFrom(cls, frame, span);
  if (!hops) return std::unexpected(std::move(hops.error()));
  return hir::DeclaringInstanceReach{*hops};
}

auto UnitLowerer::ResolveStaticPropertyTarget(
    const WalkFrame& frame, const slang::ast::ClassPropertySymbol& prop,
    diag::SourceSpan span) -> diag::Result<hir::ValueTarget> {
  const auto& owner_class =
      prop.getParentScope()->asSymbol().as<slang::ast::ClassType>();
  auto owner_ref = ResolveClassRef(owner_class, span);
  if (!owner_ref) return std::unexpected(std::move(owner_ref.error()));

  // What the cell holds is read where the class states it: off the
  // declaration where this unit declares the class, and off the signature of
  // the unit that published it otherwise.
  const auto local_cell =
      [&](const hir::LocalClassRef& local) -> diag::Result<hir::ValueTarget> {
    auto value_type = InternType(prop.getType(), span);
    if (!value_type) return std::unexpected(std::move(value_type.error()));
    const hir::LocalStaticPropertyTarget property{
        .owner = local.class_id, .prop = LookupClassPropertyStaticId(prop)};
    if (!BelongsToAnInstance(owner_class)) {
      return hir::ValueTarget{hir::StaticPropertyRef{
          .target = property, .value_type = *value_type}};
    }
    auto hops = DeclaringScopeHopsFrom(owner_class, frame, span);
    if (!hops) return std::unexpected(std::move(hops.error()));
    return hir::ValueTarget{hir::StaticPropertyRef{
        .target =
            hir::InstanceStaticPropertyTarget{
                .property = property, .hops = *hops},
        .value_type = *value_type}};
  };

  // A class another design element declares inside one of its scopes is a
  // type of that scope's instance (LRM 6.22), so what the class keeps for
  // itself is a cell of the instance, which the scope published under the
  // name the class was published as. It is reached the way any declaration of
  // another instance is.
  const auto other_instance_cell =
      [&](const hir::ExternalClassRef& ext) -> diag::Result<hir::ValueTarget> {
    const auto refuse = [&] {
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedExpressionForm,
          std::format(
              "reaching the static property `{}` of a class another instance "
              "declares is not yet supported from here",
              prop.name));
    };
    auto route = RouteToClassScope(frame, owner_class);
    const auto* on = route.has_value()
                         ? std::get_if<InExternalScope>(&route->place)
                         : nullptr;
    if (on == nullptr) return refuse();
    const std::vector<std::string> within{ext.class_name};
    const auto member = unit_.external_scope_classes.Get(on->scope_class)
                            .signature.FindMember(prop.name, within);
    if (!member.has_value()) return refuse();
    return hir::ValueTarget{hir::RoutedValueRef{
        .id = MapOrGetRoute(
            RoutesOf(frame.Current()).values,
            hir::ValueRoute{
                .base = std::move(route->base),
                .steps = std::move(route->steps),
                .leaf = ExternalMemberLeafOf(on->scope_class, *member)})}};
  };

  // A class another namespace unit declares has one cell, named by the class.
  const auto namespace_cell =
      [&](const hir::ExternalClassRef& ext) -> diag::Result<hir::ValueTarget> {
    for (const hir::PublishedProperty& property :
         ExternalClassOf(ext.unit_name, ext.class_name).static_properties) {
      if (property.name == prop.name) {
        return hir::ValueTarget{hir::StaticPropertyRef{
            .target =
                hir::ExternalStaticPropertyTarget{
                    .unit_name = ext.unit_name,
                    .class_name = ext.class_name,
                    .property_name = std::string{prop.name}},
            .value_type = property.type}};
      }
    }
    throw InternalError(
        std::format(
            "UnitLowerer::ResolveStaticPropertyTarget: '{}::{}' publishes "
            "every static property another unit may name, and '{}' is not "
            "among them",
            ext.unit_name, ext.class_name, prop.name));
  };

  return std::visit(
      Overloaded{
          [&](const hir::LocalClassRef& local) { return local_cell(local); },
          [&](const hir::ExternalClassRef& ext) {
            return HeldByAnotherDesignElement(owner_class)
                       ? other_instance_cell(ext)
                       : namespace_cell(ext);
          }},
      *owner_ref);
}

auto UnitLowerer::ObservedThroughModport(
    const slang::ast::ModportPortSymbol& offered, const WalkFrame& frame,
    RouteOrigin origin) -> diag::Result<std::vector<hir::SensitivityEntry>> {
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

  // The interface instance is reached from where the name started (LRM 23.8,
  // 25.3), as any name on it is.
  auto reached = RouteToScope(
      frame, *view->asSymbol().getParentScope(), std::move(origin));
  const auto* on = reached.has_value()
                       ? std::get_if<InExternalScope>(&reached->place)
                       : nullptr;
  if (on == nullptr) {
    return refuse(
        "waiting on a name a view offers, reached by a path of this shape, is "
        "not yet supported");
  }
  const ScopeRoute& route = *reached;
  const hir::ExternalScopeClassId scope_class = on->scope_class;
  const std::vector<hir::PublishedMemberId> watched =
      hir::WatchedMembers(PublishedViewNameMeaning(
          *this, scope_class, view->asSymbol().name, offered.name));
  std::vector<hir::SensitivityEntry> out;
  out.reserve(watched.size());
  for (const hir::PublishedMemberId id : watched) {
    const hir::RoutedValueRefId reference = MapOrGetRoute(
        RoutesOf(frame.Current()).values,
        hir::ValueRoute{
            .base = route.base,
            .steps = route.steps,
            .leaf = ExternalMemberLeafOf(scope_class, id)});
    out.push_back(
        hir::SensitivityEntry{
            .cell = hir::RoutedValueRef{.id = reference},
            .part = hir::WatchedWhole{}});
  }
  return out;
}

template <typename PartsOf, typename DeclaredBy>
auto UnitLowerer::WatchedEntriesOf(
    const std::vector<AccessedPart>& reads, const WalkFrame& frame,
    PartsOf parts_of, DeclaredBy declared_by)
    -> diag::Result<std::vector<hir::SensitivityEntry>> {
  std::vector<hir::SensitivityEntry> out;
  out.reserve(reads.size());
  // A part is meaningful only for a signal the runtime bit-addresses: a packed
  // bit vector, which renders to one observable cell whose change set is read
  // per bit. For an enum, unpacked aggregate, string, or real the runtime
  // observes the whole signal on any change, so the read watches the whole of
  // it regardless of the flat-bit view the DFA computed over its own encoding.
  const auto observe =
      [&](const AccessedPart& read, const hir::WatchedStorage& cell,
          const slang::ast::Type& read_type) -> diag::Result<void> {
    if (!read_type.isIntegral() || read_type.isEnum()) {
      out.push_back(
          hir::SensitivityEntry{.cell = cell, .part = hir::WatchedWhole{}});
      return {};
    }
    auto parts = parts_of(read);
    if (!parts) return std::unexpected(std::move(parts.error()));
    for (hir::WatchedPart& part : *parts) {
      out.push_back(
          hir::SensitivityEntry{.cell = cell, .part = std::move(part)});
    }
    return {};
  };
  for (const auto& read : reads) {
    // Nothing writes these while a wait stands, so none is watched: a foreach
    // loop variable is read-only (LRM 12.7.3), an array method's iterator
    // exists only inside its method's expression (LRM 7.12), and a pattern's
    // binding is set by the match (LRM 12.6), the front end refusing any other
    // write to it.
    if (read.symbol->kind == slang::ast::SymbolKind::Iterator ||
        read.symbol->kind == slang::ast::SymbolKind::PatternVar) {
      continue;
    }

    // A variable the reading body declares is watched as that declaration,
    // the lexical binding winning as it does for a name read (LRM 6.21). A
    // `ref` formal's storage is its actual, which may be one element or member
    // of a larger variable, so the formal's bits are not that variable's and
    // the whole of it is watched; the change the wait tests is the formal's
    // own (LRM 9.4.2).
    if (const std::optional<hir::ProceduralVarId> var =
            declared_by(*read.symbol)) {
      const auto* formal =
          read.symbol->as_if<slang::ast::FormalArgumentSymbol>();
      const hir::ProceduralVarRef declared{.var = *var};
      if (formal != nullptr &&
          formal->direction == slang::ast::ArgumentDirection::Ref) {
        out.push_back(
            hir::SensitivityEntry{
                .cell = declared, .part = hir::WatchedWhole{}});
        continue;
      }
      if (auto observed = observe(read, declared, read.symbol->getType());
          !observed) {
        return std::unexpected(std::move(observed.error()));
      }
      continue;
    }

    const auto span = SourceMapper().PointSpanOf(read.symbol->location);
    auto resolved = ResolveReferent(*read.symbol, span);
    if (!resolved) return std::unexpected(std::move(resolved.error()));
    const slang::ast::ValueSymbol& target = *resolved->symbol;
    const auto observe_cell =
        [&](const hir::ValueTarget& cell) -> diag::Result<void> {
      return observe(read, cell, target.getType());
    };

    switch (resolved->kind) {
      // A name a view defined for itself stands for an expression the interface
      // evaluates (LRM 25.5.4), so it is no single declaration to wait on. What
      // waiting on it means is waiting on every member that expression reads,
      // which the interface publishes alongside the name.
      case Referent::kViewDefinedName: {
        // Each name that reached it is watched from where that name started,
        // the way a variable read is below.
        for (RouteOrigin& origin : StartsOfNames(frame, read.reached_by)) {
          auto entries = ObservedThroughModport(
              target.as<slang::ast::ModportPortSymbol>(), frame,
              std::move(origin));
          if (!entries) return std::unexpected(std::move(entries.error()));
          for (hir::SensitivityEntry& entry : *entries) {
            if (!std::ranges::contains(out, entry)) {
              out.push_back(std::move(entry));
            }
          }
        }
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
      case Referent::kPatternBinding:
        throw InternalError(
            "WatchedEntriesOf: a pattern's binding is never watched, and is "
            "passed over before its referent is resolved");
      // LRM 9.2.2.2.1 excludes a reference to a class object, which a handle to
      // the invoking object is.
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
        if (auto observed = observe_cell(*property); !observed) {
          return std::unexpected(std::move(observed.error()));
        }
        break;
      }
      // The same text watches, in each instance, what that instance's own names
      // reach. A name through an interface port reaches whatever the port is
      // bound to (LRM 25.3), so such a read is watched through the port; a name
      // leaving the instance reaches whatever stands where it lands (LRM
      // 23.8), so such a read is watched from there; any other name reaches the
      // same storage in every instance, which is where the symbol sits.
      case Referent::kVariableStorage:
      case Referent::kNetStorage: {
        std::vector<hir::ValueTarget> cells;
        for (RouteOrigin& origin : StartsOfNames(frame, read.reached_by)) {
          auto cell =
              ResolveValueTarget(frame, target, std::move(origin), span);
          if (!cell) return std::unexpected(std::move(cell.error()));
          if (!std::ranges::contains(cells, *cell)) cells.push_back(*cell);
        }
        for (const hir::ValueTarget& cell : cells) {
          if (auto observed = observe_cell(cell); !observed) {
            return std::unexpected(std::move(observed.error()));
          }
        }
        break;
      }
      case Referent::kPrimitivePort:
      case Referent::kClockingSignal:
      case Referent::kAssertionLocal:
      case Referent::kStructureMember:
        return FailOnUnsupportedReferent(resolved->kind, span);
      case Referent::kNotAValue:
        throw InternalError(
            "WatchedEntriesOf: a read resolved to a declaration that denotes "
            "no value");
    }
  }

  return out;
}

template <typename Lowerer>
auto UnitLowerer::SensitivityEntriesOf(
    Lowerer& lowerer, const std::vector<AccessedPart>& reads,
    const WalkFrame& frame)
    -> diag::Result<std::vector<hir::SensitivityEntry>> {
  return WatchedEntriesOf(
      reads, frame,
      [&](const AccessedPart& read)
          -> diag::Result<std::vector<hir::WatchedPart>> {
        return std::visit(
            Overloaded{
                [](const WholePart&)
                    -> diag::Result<std::vector<hir::WatchedPart>> {
                  return std::vector<hir::WatchedPart>{hir::WatchedWhole{}};
                },
                [&](const SelectedParts& named)
                    -> diag::Result<std::vector<hir::WatchedPart>> {
                  std::vector<hir::WatchedPart> parts;
                  parts.reserve(named.prefixes.size());
                  for (const slang::ast::Expression* prefix : named.prefixes) {
                    auto lowered = lowerer.LowerExpr(*prefix, frame);
                    if (!lowered) {
                      return std::unexpected(std::move(lowered.error()));
                    }
                    parts.emplace_back(
                        hir::WatchedSelect{
                            .prefix = frame.Exprs().Add(*std::move(lowered))});
                  }
                  return parts;
                },
                [](const UnselectedBits& bits)
                    -> diag::Result<std::vector<hir::WatchedPart>> {
                  return std::vector<hir::WatchedPart>{
                      hir::WatchedBits{.first = bits.first, .last = bits.last}};
                }},
            read.part);
      },
      [&](const slang::ast::ValueSymbol& symbol)
          -> std::optional<hir::ProceduralVarId> {
        if constexpr (std::same_as<Lowerer, ProcessLowerer>) {
          return lowerer.LookupProceduralVar(symbol);
        } else {
          return std::nullopt;
        }
      });
}

template auto UnitLowerer::SensitivityEntriesOf(
    ProcessLowerer&, const std::vector<AccessedPart>&, const WalkFrame&)
    -> diag::Result<std::vector<hir::SensitivityEntry>>;
template auto UnitLowerer::SensitivityEntriesOf(
    StructuralScopeLowerer&, const std::vector<AccessedPart>&, const WalkFrame&)
    -> diag::Result<std::vector<hir::SensitivityEntry>>;

auto UnitLowerer::CellsRead(
    const std::vector<AccessedPart>& reads, const WalkFrame& frame)
    -> diag::Result<std::vector<hir::ValueTarget>> {
  // A variable of a body answers for a sampled value with what it holds (LRM
  // 16.5.1) and is never armed, so every read here is resolved as a cell.
  auto entries = WatchedEntriesOf(
      reads, frame,
      [](const AccessedPart&) -> diag::Result<std::vector<hir::WatchedPart>> {
        return std::vector<hir::WatchedPart>{hir::WatchedWhole{}};
      },
      [](const slang::ast::ValueSymbol&)
          -> std::optional<hir::ProceduralVarId> { return std::nullopt; });
  if (!entries) return std::unexpected(std::move(entries.error()));
  std::vector<hir::ValueTarget> cells;
  cells.reserve(entries->size());
  for (hir::SensitivityEntry& entry : *entries) {
    cells.push_back(
        std::visit(
            Overloaded{
                [](hir::ValueTarget& cell) { return std::move(cell); },
                [](const hir::ProceduralVarRef&) -> hir::ValueTarget {
                  throw InternalError(
                      "CellsRead: a read resolved as a cell named a variable "
                      "of a body");
                }},
            entry.cell));
  }
  return cells;
}

}  // namespace lyra::lowering::ast_to_hir
