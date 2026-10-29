#include <algorithm>
#include <concepts>
#include <cstdint>
#include <expected>
#include <iterator>
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

#include "lyra/base/diag_message.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/frontend/slang_source_span.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/expr_builders.hpp"
#include "lyra/hir/published_clocking.hpp"
#include "lyra/hir/published_modport.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/ast_to_hir/climb.hpp"
#include "lyra/lowering/ast_to_hir/connected_interface.hpp"
#include "lyra/lowering/ast_to_hir/expression/references.hpp"
#include "lyra/lowering/ast_to_hir/expression/view_names.hpp"
#include "lyra/lowering/ast_to_hir/generate_construct.hpp"
#include "lyra/lowering/ast_to_hir/inferred_clocking.hpp"
#include "lyra/lowering/ast_to_hir/instance_array_shape.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/sensitivity.hpp"
#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/unit_identity.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"
#include "lyra/support/def_path.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// Whether a path the source wrote passes a block of a generate construct.
auto PassesAGenerateBlock(
    std::span<const slang::ast::HierarchicalReference::Element> written)
    -> bool {
  return std::ranges::any_of(
      written, [](const slang::ast::HierarchicalReference::Element& element) {
        return element.symbol->kind == slang::ast::SymbolKind::GenerateBlock;
      });
}

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

// The hop a name takes into `symbol` where this unit declares nothing about
// it, as the front end resolved the name: an instance or a set of them, a
// generate block, or a procedural scope. Nothing for anything else a path can
// pass, which no route yet walks, and for a loop's block no select is given
// for.
auto NamedHopInto(
    const slang::ast::Symbol& symbol, const LoopSelectOf& select_of)
    -> std::optional<NamedHop> {
  const std::string name{symbol.name};
  if (symbol.kind == slang::ast::SymbolKind::Instance ||
      symbol.kind == slang::ast::SymbolKind::InstanceArray) {
    return InstanceHop{.name = name, .indices = {}};
  }
  if (const auto* block = symbol.as_if<slang::ast::GenerateBlockSymbol>()) {
    // A loop's block is named by its construct, and the select the name wrote
    // picks it out (LRM 27.4).
    if (block->getArrayIndex() != nullptr) {
      const std::optional<hir::ExprId> position = select_of(*block);
      if (!position.has_value()) return std::nullopt;
      return LoopBlockHop{
          .loop = std::string{block->getParentScope()->asSymbol().name},
          .resolved_index = LoopIndexOf(*block),
          .position = *position};
    }
    return ChosenBlockHop{
        .construct = block->constructIndex,
        .position = AlternativePositionOf(*block)};
  }
  if (symbol.kind == slang::ast::SymbolKind::Subroutine ||
      symbol.kind == slang::ast::SymbolKind::StatementBlock) {
    return ProceduralHop{.step = ProceduralStepOf(symbol)};
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

auto NamedHopAbove(
    const slang::ast::Scope& scope, const LoopSelectOf& select_of)
    -> NamedHopUp {
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
  return NamedHopUp{.hop = NamedHopInto(symbol, select_of), .scope = above};
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
// not stand in `from`, or the way down passes a scope no route walks or a
// loop's block `select_of` gives no select for.
auto NamedHopsDown(
    const slang::ast::Scope& from, const slang::ast::Scope& to,
    const LoopSelectOf& select_of) -> std::optional<std::vector<NamedHop>> {
  std::vector<NamedHop> hops;
  for (const slang::ast::Scope* scope = &to; scope != &from;) {
    if (scope == nullptr) return std::nullopt;
    auto above = NamedHopAbove(*scope, select_of);
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
                    [](ChosenBlockHop&) { return false; },
                    [](ProceduralHop&) { return false; }},
                named);
          }},
      hop);
}

// Where a scope published the block of a loop a name reaches: the loop, and
// the class the block was published as.
struct PublishedLoopBlockAt {
  hir::PublishedLoopId loop;
  support::DefPath class_path;
};

// Where the scope published the block of a loop the name was resolved to: its
// class is the loop's one class or, where the loop has several, the one the
// block standing at the index the front end resolved the select to was
// published as (LRM 27.4). Nothing where the scope published no such block.
auto FindPublishedLoopBlock(
    const base::Arena<hir::PublishedLoop, hir::PublishedLoopId>& loops,
    const LoopBlockHop& hop) -> std::optional<PublishedLoopBlockAt> {
  for (const hir::PublishedLoopId at : loops.Ids()) {
    auto class_path = std::visit(
        Overloaded{
            [&](const hir::PublishedLoopOfOneClass& loop)
                -> std::optional<support::DefPath> {
              if (loop.name != hop.loop) return std::nullopt;
              return loop.class_path;
            },
            [&](const hir::PublishedLoopByIndex& counted)
                -> std::optional<support::DefPath> {
              if (counted.name != hop.loop) return std::nullopt;
              for (const hir::PublishedLoopBlock& block : counted.blocks) {
                if (block.index == hop.resolved_index) {
                  return counted.classes.at(block.class_at);
                }
              }
              return std::nullopt;
            }},
        loops.Get(at));
    if (class_path.has_value()) {
      return PublishedLoopBlockAt{
          .loop = at, .class_path = *std::move(class_path)};
    }
  }
  return std::nullopt;
}

// Where a scope published the one block a construct building at most one
// holds: the construct, and the class the block was published as.
struct PublishedChoiceAt {
  hir::PublishedChoiceId choice;
  support::DefPath class_path;
};

// A block that stands alone or was chosen by a conditional is found by where
// the source wrote it, and is the one block its construct holds in the scope
// the name stands in (LRM 27.5).
auto FindPublishedChoice(
    const base::Arena<hir::PublishedChoice, hir::PublishedChoiceId>& choices,
    const ChosenBlockHop& hop) -> std::optional<PublishedChoiceAt> {
  for (const hir::PublishedChoiceId at : choices.Ids()) {
    const hir::PublishedChoice& choice = choices.Get(at);
    if (choice.construct != hop.construct) continue;
    for (const hir::PublishedAlternative& block : choice.blocks) {
      if (block.position == hop.position) {
        return PublishedChoiceAt{.choice = at, .class_path = block.class_path};
      }
    }
  }
  return std::nullopt;
}

}  // namespace

auto UnitLowerer::RoutesOf(ScopeFrameId owner_frame) -> hir::ScopeRoutes& {
  return routes_by_frame_[owner_frame];
}

auto UnitLowerer::TakeRoutesForFrame(ScopeFrameId owner_frame)
    -> hir::ScopeRoutes {
  scopes_under_construction_.erase(owner_frame);
  const auto it = routes_by_frame_.find(owner_frame);
  if (it == routes_by_frame_.end()) {
    return {};
  }
  auto out = std::move(it->second);
  routes_by_frame_.erase(it);
  return out;
}

void UnitLowerer::BeginScope(
    ScopeFrameId frame, StructuralScopeLowerer& lowerer,
    hir::StructuralScope& scope) {
  scopes_under_construction_.insert_or_assign(
      frame, ScopeUnderConstruction{.lowerer = &lowerer, .scope = &scope});
}

auto UnitLowerer::RouteHolderOf(const WalkFrame& frame) const
    -> const ScopeUnderConstruction& {
  const auto it = scopes_under_construction_.find(frame.Current());
  if (it == scopes_under_construction_.end()) {
    throw InternalError(
        "UnitLowerer::RouteHolderOf: a name is read where no scope holding "
        "its routes is being built");
  }
  return it->second;
}

auto UnitLowerer::SelectsWritten(
    const WalkFrame& frame,
    std::span<const slang::ast::HierarchicalReference::Element> written) const
    -> diag::Result<LoopSelectOf> {
  // Each loop's block a route passes is the one its select picks in the
  // object the route is walked from, so the select goes where the route is
  // held, beside it, lowered by the scope holding it as it is read from
  // `frame`.
  std::vector<std::pair<const slang::ast::Symbol*, hir::ExprId>> lowered;
  for (const slang::ast::HierarchicalReference::Element& element : written) {
    const slang::ast::Expression* select = LoopBlockSelectOf(element);
    if (select == nullptr) continue;
    const ScopeUnderConstruction& holder = RouteHolderOf(frame);
    WalkFrame in_holder = frame;
    in_holder.current_exprs = &holder.scope->exprs;
    auto expr = holder.lowerer->LowerExpr(*select, in_holder);
    if (!expr) return std::unexpected(std::move(expr.error()));
    lowered.emplace_back(
        element.symbol, holder.scope->exprs.Add(*std::move(expr)));
  }
  return LoopSelectOf{
      [lowered =
           std::move(lowered)](const slang::ast::GenerateBlockSymbol& block)
          -> std::optional<hir::ExprId> {
        for (const auto& [picked, position] : lowered) {
          if (picked == &block) return position;
        }
        return std::nullopt;
      }};
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
  return hir::MakeRefExpr(
      RoutedMemberCell(owner_frame, std::move(route)), type, span);
}

auto UnitLowerer::RoutedMemberCell(
    ScopeFrameId owner_frame, hir::ValueRoute route) -> hir::RoutedValueRef {
  return hir::RoutedValueRef{
      .id = MapOrGetRoute(RoutesOf(owner_frame).values, std::move(route))};
}

auto UnitLowerer::PublishedMemberLeafOf(
    hir::ScopeClassId scope_class, hir::PublishedMemberId member) const
    -> hir::PublishedMemberLeaf {
  // The member's type is already in this unit's pool, because the record
  // brought it there.
  const hir::PublishedMember& published =
      unit_.scope_classes.Get(scope_class).members.Get(member);
  return hir::PublishedMemberLeaf{
      .scope_class = scope_class,
      .member = member,
      .storage = published.storage,
      .type = published.type};
}

auto UnitLowerer::ResolveRouteTarget(
    const slang::ast::ValueSymbol& value, const ScopeRoute& route)
    -> diag::Result<hir::DataLeaf> {
  const auto span = frontend::PointSpanOf(value.location);
  const auto unsupported = [&] {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "reaching '{}' this way is not yet supported", value.name);
  };

  return std::visit(
      Overloaded{
          // A route that landed on a scope of another unit names what that
          // scope published, as held by the scope the named blocks and
          // subroutines the name went on through lead to, and the signature
          // also states what storage it is -- so nothing about it is read off
          // the unit that declared it. Every declaration a name may reach is
          // published (LRM 23.6), so one that is not is a form this compiler
          // does not publish yet.
          [&](const InPublishedScope& on) -> diag::Result<hir::DataLeaf> {
            const hir::ScopeClassSignature& published =
                unit_.scope_classes.Get(on.scope_class);
            const auto member = published.FindMember(
                value.name, support::Extended(published.class_path, on.within));
            if (!member.has_value()) return unsupported();
            return PublishedMemberLeafOf(on.scope_class, *member);
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
  // A name leaving the instance or passing a generate block reaches a target
  // only its publication says anything about, whatever this unit minted for
  // the scope it resolved to here.
  RouteOrigin origin = StartOf(frame, target, reference);
  const auto minted = NamesOwnDeclaration(origin)
                          ? LookupMintedProceduralScope(target)
                          : std::nullopt;
  // A scope's identity indexes its declaring scope's registry, so one the
  // body's own declaration scope declares is named outright.
  if (minted.has_value() && minted->owner == &frame.ProceduralScopeOwner()) {
    return hir::DisableTarget{hir::DirectDisableTarget{.scope = minted->scope}};
  }
  // Any other is reached by a route. One the reader's own structural scope
  // minted is named by that identity over the route that climbs nothing, the
  // procedural scopes around it being where it sits rather than steps of their
  // own. One a scope enclosing the reader minted, or one of another unit, is
  // reached through what the scope declaring it published, so the route runs
  // down to the target itself, and that path is what it was published by.
  const auto minted_frame = minted.has_value()
                                ? scope_frames_.find(minted->owner)
                                : scope_frames_.end();
  const bool minted_in_own_scope = [&] {
    if (minted_frame == scope_frames_.end()) return false;
    const auto hops = frame.HopsTo(minted_frame->second);
    return hops.has_value() && hops->value == 0;
  }();
  std::optional<ScopeRoute> route;
  std::optional<hir::DisableLeaf> leaf;
  if (minted_in_own_scope) {
    route = ScopeRoute::Enclosing(hir::StructuralHops{0});
    leaf = hir::DisableTargetLeaf{.scope = minted->scope};
  } else {
    const auto* scope = target.as_if<slang::ast::Scope>();
    if (scope == nullptr) {
      throw InternalError(
          "UnitLowerer::DisableTargetOf: a disable names a block or a task, "
          "each of which defines a scope");
    }
    auto walked = RouteToScope(frame, *scope, origin);
    if (!walked) return std::unexpected(std::move(walked.error()));
    route = *std::move(walked);
    if (!route.has_value()) return refuse();
    if (const auto* on = std::get_if<InPublishedScope>(&route->place)) {
      const hir::ScopeClassSignature& signature =
          unit_.scope_classes.Get(on->scope_class);
      const std::optional<hir::PublishedDisableTargetId> published =
          signature.FindDisableTarget(
              support::Extended(signature.class_path, on->within));
      if (published.has_value()) {
        leaf = hir::PublishedDisableTargetLeaf{
            .scope_class = on->scope_class, .target = *published};
      }
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
      lowering_->asSymbol().as_if<slang::ast::InstanceBodySymbol>();
  if (body == nullptr) return std::nullopt;
  return ReaderClimbs{
      .body = body,
      .climbs = Naming().Keys().ContextOf(InstantiationOf(*body)).climbs};
}

auto NamesOwnDeclaration(const RouteOrigin& origin) -> bool {
  return std::holds_alternative<FromReader>(origin.start) &&
         !PassesAGenerateBlock(origin.written);
}

auto UnitLowerer::StartOf(
    const WalkFrame& frame, const slang::ast::Symbol& found,
    const slang::ast::HierarchicalReference& written) -> RouteOrigin {
  const std::optional<ReaderClimbs> reader = ReaderInstance();
  if (!reader.has_value()) {
    return RouteOrigin{.start = FromReader{}, .written = written.path};
  }

  // A name the body of a sequence or property writes binds in the scope
  // declaring it (LRM 16.8, 11.12), so where the instance reached that
  // declaration by a hierarchical name, a name declared beside it is reached
  // the way that name reached the declaration: where it led, and through the
  // blocks it selected on the way. The innermost such instance is the one whose
  // body holds the name.
  if (const ExpansionInstance* instance =
          ExpansionDeclaringBeside(frame, found)) {
    return StartOf(frame, *instance->declaration, *instance->reached_through);
  }

  // Through a port the name stands in whatever instance the port was bound to,
  // and a coordinate written after the port picks one where it stands for
  // several (LRM 25.3). A coordinate past those, on a loop generate inside the
  // instance, is the descent's own (LRM 27.4).
  if (written.isViaIfacePort()) {
    const auto& port =
        written.path.front().symbol->as<slang::ast::InterfacePortSymbol>();
    PortReach reach = ReachOfPort(frame, port);
    // The instance the name stands in is the one bound where its selects land:
    // a port standing for one is bound to one, and the front end names the
    // element a select picks out of one carrying a range.
    const auto connected = ConnectedInterfaceOf(port.getConnection()).instances;
    const slang::ast::InstanceSymbol* bound =
        reach.hop.dims.empty() && connected.size() == 1 ? connected[0]
                                                        : nullptr;
    for (std::size_t at = 1; at < written.path.size(); ++at) {
      // The front end lists a loop generate by its name, with no selector,
      // before the block its select picks, so the port's own selects end
      // there.
      const auto* position =
          std::get_if<std::int32_t>(&written.path[at].selector);
      if (position == nullptr) break;
      if (*position < 0) {
        throw InternalError(
            "UnitLowerer::StartOf: the front end resolves a coordinate on a "
            "port to a position in its range");
      }
      reach.hop.step.selects.push_back(static_cast<std::uint32_t>(*position));
      if (const auto* element =
              written.path[at].symbol->as_if<slang::ast::InstanceSymbol>()) {
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
    return RouteOrigin{
        .start =
            RouteStart{
                .below = &bound->body,
                .base = std::move(reach.base),
                .leading = std::move(reach.hop)},
        .written = written.path};
  }

  // Upward the name stands in the scope it landed in, an instance or a
  // generate block the reader stands in or a top-level instance; the unit is
  // told apart by the class of that scope, so every instance of the unit
  // starts at a scope of that class and the walk down is the one the name
  // writes. A name that never leaves the instance starts at the reader.
  const std::optional<ClimbAnchor> climb =
      ClimbOutOf(found, written, *reader->body);
  if (!climb.has_value()) {
    return RouteOrigin{.start = FromReader{}, .written = written.path};
  }
  return RouteOrigin{
      .start = StartInEnclosing(*climb->scope), .written = written.path};
}

auto UnitLowerer::StartsOfNames(
    const WalkFrame& frame,
    std::span<const slang::ast::Expression* const> names)
    -> std::vector<RouteOrigin> {
  std::vector<RouteOrigin> origins;
  bool from_reader = names.empty();
  for (const slang::ast::Expression* name : names) {
    RouteOrigin origin{.start = FromReader{}, .written = {}};
    if (const auto* hierarchical =
            name->as_if<slang::ast::HierarchicalValueExpression>()) {
      origin = StartOf(frame, hierarchical->symbol, hierarchical->ref);
    } else if (
        const auto* plain = name->as_if<slang::ast::NamedValueExpression>()) {
      origin = StartOf(frame, plain->symbol, {});
    }
    if (NamesOwnDeclaration(origin)) {
      from_reader = true;
    } else {
      origins.push_back(std::move(origin));
    }
  }
  if (from_reader) {
    origins.push_back(RouteOrigin{.start = FromReader{}, .written = {}});
  }
  return origins;
}

auto UnitLowerer::TranslateReferenceRoute(
    const WalkFrame& frame, const slang::ast::ValueSymbol& value,
    RouteOrigin origin) -> diag::Result<std::optional<hir::RoutedValueRef>> {
  // A name that left the reader's instance, or that passes a generate block,
  // is walked to whatever it reaches in each object.
  if (!NamesOwnDeclaration(origin)) {
    auto route =
        RouteToScope(frame, *value.getHierarchicalParent(), std::move(origin));
    if (!route) return std::unexpected(std::move(route.error()));
    if (!route->has_value()) return std::nullopt;
    auto reference = MakeRoutedValueRef(value, frame.Current(), **route);
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

  // The target's storage is the reader's own scope's (LRM 23.9), so the route
  // climbs nothing and the leaf names the declaration. One a scope enclosing
  // the reader declares is reached through what that scope published, below.
  const auto own_scope = [](const std::optional<hir::StructuralHops>& hops) {
    return hops.has_value() && hops->value == 0;
  };
  if (own_scope(data_object_hops) ||
      (procedural_static &&
       own_scope(frame.HopsTo(procedural_static->home_frame)))) {
    return routed_ref(
        frame.Current(), ScopeRoute::Enclosing(hir::StructuralHops{0}));
  }

  const slang::ast::Scope* owner = value.getHierarchicalParent();
  if (owner == nullptr) return std::nullopt;

  auto route = RouteToScope(frame, *owner, std::move(origin));
  if (!route) return std::unexpected(std::move(route.error()));
  if (!route->has_value()) return std::nullopt;
  return routed_ref(frame.Current(), **std::move(route));
}

auto UnitLowerer::ReachThroughInterfacePort(
    const WalkFrame& frame, const slang::ast::HierarchicalReference& reference)
    -> diag::Result<std::optional<ScopeRoute>> {
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

  auto selects = SelectsWritten(frame, path);
  if (!selects) return std::unexpected(std::move(selects.error()));
  const LoopSelectOf& select_of = *selects;
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
    auto named = NamedHopInto(*path[hop].symbol, select_of);
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
  return InPublishedScope{
      .scope_class = RecordedScopeClass(kind.scope_class), .within = {}};
}

auto UnitLowerer::IsOwnScope(
    const WalkFrame& frame, const slang::ast::Scope& target) const -> bool {
  // A name enabling a subroutine as this unit's own passes no generate block,
  // and the front end resolves such a name only to one a scope enclosing the
  // reader declares (LRM 23.9).
  const auto hops = frame.HopsTo(LookupScopeFrame(target));
  if (!hops.has_value()) {
    throw InternalError(
        "UnitLowerer::IsOwnScope: a subroutine called as this unit's own is "
        "declared by no scope enclosing the call");
  }
  return hops->value == 0;
}

auto UnitLowerer::RouteToScopeOrRefuse(
    const WalkFrame& frame, const slang::ast::Scope& target, RouteOrigin origin,
    diag::SourceSpan span) -> diag::Result<ScopeRoute> {
  auto walked = RouteToScope(frame, target, std::move(origin));
  if (!walked) return std::unexpected(std::move(walked.error()));
  if (!walked->has_value()) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "a scope reached this way is not yet supported");
  }
  return **std::move(walked);
}

auto UnitLowerer::RouteToScope(
    const WalkFrame& frame, const slang::ast::Scope& target, RouteOrigin origin)
    -> diag::Result<std::optional<ScopeRoute>> {
  auto selects = SelectsWritten(frame, origin.written);
  if (!selects) return std::unexpected(std::move(selects.error()));
  const LoopSelectOf& select_of = *selects;

  // A name that left the reader's instance starts where it landed, which is a
  // scope of whatever instance stands there rather than of this unit's own
  // object -- even where that instance is of this unit -- so every hop below it
  // is resolved against what the scope above it published.
  if (auto* start = std::get_if<RouteStart>(&origin.start)) {
    auto below = NamedHopsDown(*start->below, target, select_of);
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
  // across a unit boundary, with the element of an instance array attached.
  //
  // A hop this unit does not declare is a step through what the scope above
  // it published (LRM 23.6, 25.10). Which scope stands above a hop is what
  // every hop before it decided, and this walk runs from the target upward, so
  // it records how the name reached each hop and leaves resolving it to a
  // forward pass over the whole descent.
  //
  // Which body a generate block is, is its object's construction data -- a
  // loop's select picks a block in each object (LRM 27.4), a conditional
  // chooses one as each object is built (LRM 27.5) -- so the hop onto one is
  // resolved against what the scope holding its construct published, as a hop
  // of another unit is, and so is every hop below it: what the reader's code
  // states of a block is what it publishes, never which body it compiled to.
  struct Level {
    std::optional<DeclaredHop> declared;
    std::optional<NamedHop> named;
    const slang::ast::Scope* above = nullptr;
    bool onto_a_block = false;
  };
  std::vector<Level> levels;
  // A block a loop's select picks is the reader's ancestor only in the
  // elaboration being lowered, so the route meets the reader no lower than
  // above every such block the name passes.
  std::unordered_set<const slang::ast::Symbol*> selected_blocks;
  for (const slang::ast::HierarchicalReference::Element& element :
       origin.written) {
    if (LoopBlockSelectOf(element) != nullptr) {
      selected_blocks.insert(element.symbol);
    }
  }
  // A scope enclosing the reader is reached by climbing to it. Where it is not
  // the reader's own, what the reader states of it is what it published, as
  // of every scope a route meets the reader at above its own (below).
  if (selected_blocks.empty() && reader_ancestors.contains(&target)) {
    if (const auto it = scope_frames_.find(&target);
        it != scope_frames_.end()) {
      if (const auto hops = frame.HopsTo(it->second)) {
        return ScopeRoute{
            .base = hir::InUnitBase{.hops = *hops},
            .steps = {},
            .place = hops->value == 0
                         ? RoutePlace{InOwnScope{}}
                         : RoutePlace{InPublishedScope{
                               .scope_class = PublishedClassOf(target),
                               .within = {}}},
            .open = {}};
      }
    }
  }
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
    auto [named, next] = NamedHopAbove(*scope, select_of);
    std::vector<std::uint32_t> indices;
    // The class of the unit an instance this one declares is built from, read
    // off the declaration -- this unit naming its own child. A hop it does not
    // declare is resolved against what the scope above it published instead.
    RoutePlace place = InOwnScope{};
    if (owned->kind == slang::ast::SymbolKind::InstanceBody) {
      const auto* inst =
          owned->as<slang::ast::InstanceBodySymbol>().parentInstance;
      if (inst == nullptr) return std::nullopt;
      place = InPublishedScope{
          .scope_class = ScopeClassOfInstance(*inst), .within = {}};
      // The step is the member registered for the instance, which is the whole
      // array where it is an element of one.
      indices.assign(inst->arrayPath.begin(), inst->arrayPath.end());
      owned = &OwnerOfInstance(*inst);
    }

    // The hop onto a child this unit declares, with the element of an
    // instance array this is selected after it. Which body a generate block
    // is, is its object's, so the hop onto one is the one the name writes.
    const auto obinding = LookupOwnedChildBinding(*owned);
    Level level{
        .declared = std::nullopt,
        .named = std::move(named),
        .above = next,
        .onto_a_block = false};
    if (obinding.has_value()) {
      std::visit(
          Overloaded{
              [&](hir::InstanceMemberId id) {
                level.declared = DeclaredHop{
                    .step = hir::AsPathStep(
                        hir::OwnedChildStep{.names = id, .selects = indices}),
                    .place = place,
                    .dims = {}};
              },
              [&](const DeclaredGenerateBlock&) { level.onto_a_block = true; }},
          obinding->child);
    }
    if (!level.declared.has_value() && !level.named.has_value()) {
      return std::nullopt;
    }
    levels.push_back(std::move(level));
    selected_blocks.erase(owned);

    // The route's first step is onto the child of the deepest scope shared
    // with the reader: the first hop whose parent scope is a reader ancestor.
    // A child whose owning scope this unit emits stays inside this unit's
    // layout: the climb to that scope is typed, and the step onto the child is
    // the route's first typed step. A scope this unit does not lay out is
    // reached from where the route starts outside the reader's instance,
    // which the caller states.
    if (next != nullptr && reader_ancestors.contains(next) &&
        selected_blocks.empty()) {
      const auto met = scope_frames_.find(next);
      const auto hops =
          met != scope_frames_.end() ? frame.HopsTo(met->second) : std::nullopt;
      if (!hops.has_value()) return std::nullopt;
      // From the topmost hop onto a generate block down, every hop is the one
      // the scope above it published, and the walk stands on what the scope
      // holding that block's construct published to take the first of them.
      // A route meeting the reader above the reader's own scope reaches into
      // another body, whichever implementation the reader sits in, so it
      // takes every hop from what the scope it meets at published.
      std::optional<std::size_t> published_from;
      for (std::size_t at = 0; at < levels.size(); ++at) {
        if (levels[at].onto_a_block) published_from = at;
      }
      if (hops->value > 0) published_from = levels.size() - 1;
      std::vector<DescentHop> descent;
      descent.reserve(levels.size());
      for (std::size_t at = levels.size(); at-- > 0;) {
        Level& reached = levels[at];
        const bool published =
            published_from.has_value() && at <= *published_from;
        if (published || !reached.declared.has_value()) {
          if (!reached.named.has_value()) return std::nullopt;
          descent.emplace_back(*std::move(reached.named));
        } else {
          descent.emplace_back(*std::move(reached.declared));
        }
      }
      RoutePlace standing = InOwnScope{};
      if (published_from.has_value()) {
        const InPublishedScope holding = InPublishedScope{
            .scope_class = PublishedClassOf(*levels[*published_from].above),
            .within = {}};
        if (*published_from + 1 == levels.size()) {
          standing = holding;
        } else {
          auto* onto_holding = std::get_if<DeclaredHop>(
              &descent[levels.size() - 2 - *published_from]);
          if (onto_holding == nullptr) {
            throw InternalError(
                "UnitLowerer::RouteToScope: the scope holding a generate "
                "construct this unit declares is a scope this unit declares "
                "too");
          }
          onto_holding->place = holding;
        }
      }
      std::optional<Descent> descended =
          DescendFrom(std::move(standing), descent);
      if (!descended.has_value()) return std::nullopt;
      return ScopeRoute{
          .base = hir::InUnitBase{.hops = *hops},
          .steps = std::move(descended->steps),
          .place = std::move(descended->place),
          .open = std::move(descended->open)};
    }
    scope = next;
  }
  return std::nullopt;
}

auto UnitLowerer::ClassifyDescent(ScopeRoute& route, std::span<DescentHop> hops)
    -> bool {
  // A route starting at an enclosing scope stands on an object of that scope's
  // class from the start, which is read through what its unit published.
  RoutePlace standing = std::visit(
      Overloaded{
          [](const hir::InUnitBase&) -> RoutePlace { return InOwnScope{}; },
          [](const hir::EnclosingScopeBase& base) -> RoutePlace {
            return InPublishedScope{
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
    hir::ScopeClassId from, const slang::ast::Scope& from_scope,
    const slang::ast::Scope& to, const LoopSelectOf& select_of)
    -> std::optional<PublishedDescent> {
  auto hops = NamedHopsDown(from_scope, to, select_of);
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
  const auto* on = std::get_if<InPublishedScope>(&descended.place);
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
  for (hir::PublishedStep& step : published->steps) {
    descended.steps.push_back(hir::AsPathStep(std::move(step)));
  }
  descended.place = std::move(published->place);
  descended.open = std::move(published->open);
  return descended;
}

auto UnitLowerer::DescendPublishedFrom(
    hir::ScopeClassId standing, std::span<NamedHop> hops)
    -> std::optional<PublishedDescent> {
  PublishedDescent descended{
      .steps = {},
      .place = InPublishedScope{.scope_class = standing, .within = {}},
      .open = {}};
  // A hop onto an instance, or a set of them, is a step onto the member the
  // scope published for it; that member's type says how many objects it
  // stands for and which kind stands at each position. Selects naming one
  // position land on the scope of the kind there; selects leaving a dimension
  // open stand on several objects, past which no name resolves (LRM 23.6), and
  // whoever picks one out of them reads its kind off the same set.
  const auto onto_instance = [&](const InPublishedScope& on,
                                 InstanceHop& instance) -> bool {
    const hir::ScopeClassSignature& record =
        unit_.scope_classes.Get(on.scope_class);
    const auto member = record.FindMember(instance.name, record.class_path);
    if (!member.has_value()) return false;
    const auto* objects = unit_.types.Get(record.members.Get(*member).type)
                              .As<hir::UnitObjectsType>();
    if (objects == nullptr) return false;
    descended.open =
        SettledDimensions(objects->ranges, instance.indices.size());
    const std::optional<hir::UnitObjectType> lands_on =
        instance.indices.size() == objects->ranges.size()
            ? std::optional{objects->KindAt(instance.indices)}
            : std::nullopt;
    descended.steps.push_back(
        hir::PublishedStep{
            .names =
                hir::PublishedMemberRef{
                    .scope_class = on.scope_class, .member = *member},
            .selects = std::move(instance.indices)});
    if (!lands_on.has_value()) {
      descended.place = OnSeveralObjects{};
      return true;
    }
    descended.place = InPublishedScope{
        .scope_class = RecordedScopeClass(lands_on->scope_class), .within = {}};
    return true;
  };

  // A hop into a generate block lands on the class that block was published
  // as, which this unit records under the unit holding the block; the step
  // names the block as the hop says.
  const auto class_of_block = [&](const InPublishedScope& on,
                                  const support::DefPath& class_path) {
    const std::string unit_name =
        unit_.scope_classes.KeyOf(on.scope_class).unit_name;
    return ScopeClassOf(unit_name, class_path);
  };
  const auto step_into_block = [&](hir::PublishedScopeRef names,
                                   hir::ScopeClassId result_class) {
    descended.open.clear();
    descended.steps.push_back(
        hir::PublishedStep{.names = std::move(names), .selects = {}});
    descended.place =
        InPublishedScope{.scope_class = result_class, .within = {}};
    return true;
  };

  // A named block or subroutine is no object, so nothing below one is either
  // and no step follows it. Nothing resolves past several objects either.
  for (NamedHop& hop : hops) {
    auto* on = std::get_if<InPublishedScope>(&descended.place);
    if (on == nullptr) return std::nullopt;
    const bool past_procedural = !on->within.empty();
    const bool resolved = std::visit(
        Overloaded{
            [&](InstanceHop& instance) {
              return !past_procedural && onto_instance(*on, instance);
            },
            // A loop's block is named as the source names it, by the loop and
            // the select written for it, and the loop's member answers which
            // of the blocks the object built that is (LRM 27.4).
            [&](const LoopBlockHop& block) {
              if (past_procedural) return false;
              const auto found = FindPublishedLoopBlock(
                  unit_.scope_classes.Get(on->scope_class).loops, block);
              if (!found.has_value()) return false;
              const hir::ScopeClassId result_class =
                  class_of_block(*on, found->class_path);
              return step_into_block(
                  hir::PublishedGenerateLoopRef{
                      .scope_class = on->scope_class,
                      .loop = found->loop,
                      .position = block.position,
                      .result_class = result_class},
                  result_class);
            },
            // The one block a construct building at most one holds is the one
            // the member published for the construct holds (LRM 27.5).
            [&](const ChosenBlockHop& block) {
              if (past_procedural) return false;
              const auto found = FindPublishedChoice(
                  unit_.scope_classes.Get(on->scope_class).choices, block);
              if (!found.has_value()) return false;
              const hir::ScopeClassId result_class =
                  class_of_block(*on, found->class_path);
              return step_into_block(
                  hir::PublishedGenerateBlockRef{
                      .scope_class = on->scope_class,
                      .choice = found->choice,
                      .result_class = result_class},
                  result_class);
            },
            [&](ProceduralHop& procedural) {
              on->within.push_back(std::move(procedural.step));
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

auto UnitLowerer::ReadsParameterFromCell(
    const slang::ast::ParameterSymbol& param) const -> bool {
  if (NameFilledDuringElaboration(param) != nullptr) {
    return true;
  }
  const slang::ast::Scope* holder = param.getParentScope();
  return holder != nullptr && holder->getContainingInstance() != nullptr &&
         Naming().Keys().ValueSourceOf(InstanceHolding(*holder), param) !=
             ParameterValueSource::kSettledAtElaboration;
}

auto UnitLowerer::ResolveValueTarget(
    const WalkFrame& frame, const slang::ast::ValueSymbol& value,
    RouteOrigin origin, diag::SourceSpan span)
    -> diag::Result<hir::ValueTarget> {
  // A namespace unit has no instance, so its cell is reached by name rather
  // than by a route out of the reader's own storage (LRM 26.2, 3.12.1). The
  // same by-name form serves a referrer in another unit and the owning unit's
  // own body, neither of which has a receiver to route through.
  if (const auto* unit = DeclaringNamespaceUnit(value)) {
    auto value_type = InternType(value.getType(), span);
    if (!value_type) return std::unexpected(std::move(value_type.error()));
    return hir::ValueTarget{hir::NamespaceValueRef{
        .unit_name = CompilationUnitName(*unit, Naming()),
        .variable_name = std::string{value.name},
        .value_type = *value_type}};
  }

  auto route = TranslateReferenceRoute(frame, value, std::move(origin));
  if (!route) return std::unexpected(std::move(route.error()));
  if (!route->has_value()) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "reaching the storage '{}' names from here is not yet supported",
        value.name);
  }
  return hir::ValueTarget{*std::move(*route)};
}

auto UnitLowerer::PublishedClassOf(const slang::ast::Scope& scope)
    -> hir::ScopeClassId {
  // The one the unit of the instance holding `scope` published the scope as,
  // which this unit names the way that unit does -- this unit itself included.
  return ScopeClassOf(
      Naming().NameOf(InstanceHolding(scope)),
      DefPathOf(scope.asSymbol(), Naming()));
}

auto UnitLowerer::StartInEnclosing(const slang::ast::Scope& scope)
    -> RouteStart {
  return RouteStart{
      .below = &scope,
      .base = hir::EnclosingScopeBase{.scope_class = PublishedClassOf(scope)},
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
          [](const FromReader&) {
            return RouteOrigin{.start = FromReader{}, .written = {}};
          },
          [&](const FromScope& from) {
            return RouteOrigin{
                .start = StartInEnclosing(*from.scope), .written = {}};
          }},
      *start);
}

auto UnitLowerer::RouteToReplicatingScope(
    const WalkFrame& frame, const slang::ast::Symbol& type)
    -> diag::Result<std::optional<ScopeRoute>> {
  const slang::ast::Scope& scope = ReplicatingScope(type);
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
  return ReachDeclaringInstance(cls, frame, span);
}

auto UnitLowerer::ReachDeclaringInstance(
    const slang::ast::Symbol& type, const WalkFrame& frame,
    diag::SourceSpan span) -> diag::Result<hir::DeclaringInstanceReach> {
  if (HeldByAnotherDesignElement(type)) {
    auto walked = RouteToReplicatingScope(frame, type);
    if (!walked) return std::unexpected(std::move(walked.error()));
    std::optional<ScopeRoute> route = *std::move(walked);
    const auto* on = route.has_value()
                         ? std::get_if<InPublishedScope>(&route->place)
                         : nullptr;
    if (on == nullptr) {
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedClassFeature,
          "reaching, from here, the instance that holds a type another "
          "instance declares is not yet supported");
    }
    const hir::TypeId object_type = ScopeClassTypeOf(on->scope_class);
    return hir::DeclaringInstanceReach{
        MakeRoutedObjectRef(frame.Current(), *std::move(route), object_type)};
  }
  auto hops = DeclaringScopeHopsFrom(type, frame, span);
  if (!hops) return std::unexpected(std::move(hops.error()));
  return hir::DeclaringInstanceReach{*hops};
}

auto UnitLowerer::ResolveStaticPropertyTarget(
    const WalkFrame& frame, const slang::ast::ClassPropertySymbol& prop,
    diag::SourceSpan span) -> diag::Result<hir::ValueTarget> {
  const auto& owner_class =
      prop.getParentScope()->asSymbol().as<slang::ast::ClassType>();
  auto owner = ResolveClass(owner_class, span);
  if (!owner) return std::unexpected(std::move(owner.error()));

  // A class a namespace unit declares, whichever unit that is, keeps one cell
  // per static property, and a class one of this unit's structural scopes
  // declares keeps one per instance of that scope (LRM 6.22). Either way the
  // cell is named by the class and the property's position in its declaration.
  const auto class_cell = [&]() -> diag::Result<hir::ValueTarget> {
    const hir::ClassStaticPropertyTarget property{
        .owner = *owner, .prop = StaticPropertyPositionOf(*owner, prop.name)};
    if (!BelongsToAnInstance(owner_class)) {
      return hir::ValueTarget{hir::StaticPropertyRef{.target = property}};
    }
    auto hops = DeclaringScopeHopsFrom(owner_class, frame, span);
    if (!hops) return std::unexpected(std::move(hops.error()));
    return hir::ValueTarget{hir::StaticPropertyRef{
        .target = hir::InstanceStaticPropertyTarget{
            .property = property, .hops = *hops}}};
  };

  // A class another design element declares inside one of its scopes is a
  // type of that scope's instance (LRM 6.22), so what the class keeps for
  // itself is a cell of the instance, which the scope published as that
  // class's. It is reached the way any declaration of another instance is.
  const auto other_instance_cell = [&]() -> diag::Result<hir::ValueTarget> {
    const auto refuse = [&] {
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedExpressionForm,
          "reaching the static property '{}' of a class another instance "
          "declares is not yet supported from here",
          prop.name);
    };
    auto walked = RouteToReplicatingScope(frame, owner_class);
    if (!walked) return std::unexpected(std::move(walked.error()));
    std::optional<ScopeRoute> route = *std::move(walked);
    const auto* on = route.has_value()
                         ? std::get_if<InPublishedScope>(&route->place)
                         : nullptr;
    if (on == nullptr) return refuse();
    const auto member =
        unit_.scope_classes.Get(on->scope_class)
            .FindMember(prop.name, unit_.classes.PathOf(*owner));
    if (!member.has_value()) return refuse();
    return hir::ValueTarget{hir::RoutedValueRef{
        .id = MapOrGetRoute(
            RoutesOf(frame.Current()).values,
            hir::ValueRoute{
                .base = std::move(route->base),
                .steps = std::move(route->steps),
                .leaf = PublishedMemberLeafOf(on->scope_class, *member)})}};
  };

  return HeldByAnotherDesignElement(owner_class) ? other_instance_cell()
                                                 : class_cell();
}

auto UnitLowerer::ObservedThroughModport(
    const slang::ast::ModportPortSymbol& offered, const WalkFrame& frame,
    RouteOrigin origin) -> diag::Result<std::vector<hir::SensitivityEntry>> {
  const auto span = frontend::PointSpanOf(offered.location);
  const auto refuse = [&](base::DiagFormat<> message) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        base::DiagMessage(message));
  };
  const slang::ast::Scope* view = offered.getParentScope();
  if (view == nullptr) {
    throw InternalError(
        "UnitLowerer::ObservedThroughModport: a name a view offers is declared "
        "by the view");
  }

  // The interface instance is reached from where the name started (LRM 23.8,
  // 25.3), as any name on it is.
  auto walked = RouteToScope(
      frame, *view->asSymbol().getParentScope(), std::move(origin));
  if (!walked) return std::unexpected(std::move(walked.error()));
  std::optional<ScopeRoute> reached = *std::move(walked);
  const auto* on = reached.has_value()
                       ? std::get_if<InPublishedScope>(&reached->place)
                       : nullptr;
  if (on == nullptr) {
    return refuse(
        "waiting on a name a modport offers, reached by a path of this "
        "shape, is not yet supported");
  }
  const ScopeRoute& route = *reached;
  const hir::ScopeClassId scope_class = on->scope_class;
  return WholeMembersAlong(
      frame, route, scope_class,
      hir::WatchedMembers(PublishedViewNameMeaning(
          *this, scope_class, view->asSymbol().name, offered.name)));
}

auto UnitLowerer::WholeMembersAlong(
    const WalkFrame& frame, const ScopeRoute& route,
    hir::ScopeClassId scope_class,
    std::span<const hir::PublishedMemberId> members)
    -> std::vector<hir::SensitivityEntry> {
  std::vector<hir::SensitivityEntry> watched;
  watched.reserve(members.size());
  for (const hir::PublishedMemberId id : members) {
    watched.push_back(
        hir::SensitivityEntry{
            .cell = RoutedMemberCell(
                frame.Current(),
                hir::ValueRoute{
                    .base = route.base,
                    .steps = route.steps,
                    .leaf = PublishedMemberLeafOf(scope_class, id)}),
            .part = hir::WatchedWhole{}});
  }
  return watched;
}

auto UnitLowerer::PublishedClockingEntries(
    const WalkFrame& frame, const ClockOfAnotherInstance& clock,
    diag::SourceSpan span) -> diag::Result<std::vector<hir::EventEntry>> {
  // The instance declaring the block is reached the way a name in a body the
  // block declares reaches it (LRM 16.8).
  auto walked = RouteToScope(
      frame, *clock.block->getParentScope(),
      StartOf(frame, *clock.declaration, *clock.reached_through));
  if (!walked) return std::unexpected(std::move(walked.error()));
  std::optional<ScopeRoute> reached = *std::move(walked);
  const auto* on = reached.has_value()
                       ? std::get_if<InPublishedScope>(&reached->place)
                       : nullptr;
  if (on == nullptr) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedTimingControlKind,
        "a clocking block of another instance, reached by a path of this "
        "shape, is not yet supported");
  }
  const ScopeRoute& route = *reached;
  const hir::ScopeClassId scope_class = on->scope_class;
  const hir::ScopeClassSignature& declaring =
      unit_.scope_classes.Get(scope_class);
  const hir::PublishedClockingBlock* block =
      hir::FindClockingBlock(declaring.clocking_blocks, clock.block->name);
  if (block == nullptr) {
    throw InternalError(
        "UnitLowerer::PublishedClockingEntries: a scope publishes every named "
        "clocking block it declares");
  }

  // What the scope evaluates for the block, asked of the object the route
  // reaches.
  const auto evaluated = [&](hir::PublishedCallableId callable) {
    return frame.Exprs().Add(
        hir::Expr{
            .type = declaring.callables.Get(callable).result_type,
            .data =
                hir::CallExpr{
                    .callee =
                        hir::PublishedScopeMethodRef{
                            .receiver = MakeRoutedObjectRef(
                                frame.Current(), route,
                                ScopeClassTypeOf(scope_class)),
                            .scope_class = scope_class,
                            .callable = callable},
                    .arguments = {}},
            .span = span});
  };
  std::vector<hir::EventEntry> entries;
  entries.reserve(block->events.size());
  for (const hir::PublishedClockingEntry& event : block->events) {
    const hir::ExprId operand = evaluated(event.evaluate);
    auto occurrence = std::visit(
        Overloaded{
            [&](const hir::PublishedClockingChange& change)
                -> std::variant<hir::ValueChange, hir::NamedEventTrigger> {
              return hir::ValueChange{
                  .signal = operand,
                  .edge = change.edge,
                  .cells = WholeMembersAlong(
                      frame, route, scope_class, change.observes)};
            },
            [&](const hir::PublishedClockingTrigger&)
                -> std::variant<hir::ValueChange, hir::NamedEventTrigger> {
              return hir::NamedEventTrigger{.event = operand};
            }},
        event.occurrence);
    entries.push_back(
        hir::EventEntry{
            .occurrence = std::move(occurrence),
            .condition = event.condition.has_value()
                             ? std::optional{evaluated(*event.condition)}
                             : std::nullopt});
  }
  return entries;
}

template <typename PartsOf, typename DeclaredBy>
auto UnitLowerer::WatchedEntriesOf(
    const std::vector<AccessedPart>& reads, const WalkFrame& frame,
    PartsOf parts_of, DeclaredBy declared_by)
    -> diag::Result<std::vector<hir::SensitivityEntry>> {
  std::vector<hir::SensitivityEntry> out;
  out.reserve(reads.size());
  // The part was read off the symbol the analyzed text names, and what is
  // watched is the storage that name reaches, which for a name through a port
  // or a view may be of another type. Where that storage has no statable
  // parts, the whole of it is watched.
  const auto observe =
      [&](const AccessedPart& read, const hir::WatchedStorage& cell,
          const slang::ast::Type& read_type) -> diag::Result<void> {
    switch (PartAddressingOf(read_type)) {
      case PartAddressing::kWholeOnly:
        out.push_back(
            hir::SensitivityEntry{.cell = cell, .part = hir::WatchedWhole{}});
        return {};
      case PartAddressing::kBits:
      case PartAddressing::kSelects:
        break;
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

    const auto span = frontend::PointSpanOf(read.symbol->location);
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
        auto cells = CellsReachedBy(frame, target, read.reached_by, span);
        if (!cells) return std::unexpected(std::move(cells.error()));
        for (const hir::ValueTarget& cell : *cells) {
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

auto UnitLowerer::CellsReachedBy(
    const WalkFrame& frame, const slang::ast::ValueSymbol& target,
    std::span<const slang::ast::Expression* const> names, diag::SourceSpan span)
    -> diag::Result<std::vector<hir::ValueTarget>> {
  std::vector<hir::ValueTarget> cells;
  for (RouteOrigin& origin : StartsOfNames(frame, names)) {
    auto cell = ResolveValueTarget(frame, target, std::move(origin), span);
    if (!cell) return std::unexpected(std::move(cell.error()));
    if (!std::ranges::contains(cells, *cell)) cells.push_back(*cell);
  }
  return cells;
}

auto UnitLowerer::CellsRead(
    const std::vector<AccessedPart>& reads, const WalkFrame& frame)
    -> diag::Result<std::vector<hir::ValueTarget>> {
  // A wait watches no constant, since nothing changes one, so the cell a
  // constant is read from is resolved here rather than by the watch below; for
  // a sampled value that cell answers as a variable's does.
  std::vector<hir::ValueTarget> cells;
  std::vector<AccessedPart> watched;
  for (const AccessedPart& read : reads) {
    const auto* param = read.symbol->as_if<slang::ast::ParameterSymbol>();
    if (param == nullptr || !ReadsParameterFromCell(*param)) {
      watched.push_back(read);
      continue;
    }
    auto held = CellsReachedBy(
        frame, *param, read.reached_by,
        frontend::PointSpanOf(read.symbol->location));
    if (!held) return std::unexpected(std::move(held.error()));
    std::ranges::move(*held, std::back_inserter(cells));
  }
  // A variable of a body answers for a sampled value with what it holds (LRM
  // 16.5.1) and is never armed, so every read here is resolved as a cell.
  auto entries = WatchedEntriesOf(
      watched, frame,
      [](const AccessedPart&) -> diag::Result<std::vector<hir::WatchedPart>> {
        return std::vector<hir::WatchedPart>{hir::WatchedWhole{}};
      },
      [](const slang::ast::ValueSymbol&)
          -> std::optional<hir::ProceduralVarId> { return std::nullopt; });
  if (!entries) return std::unexpected(std::move(entries.error()));
  cells.reserve(cells.size() + entries->size());
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
