#pragma once

#include <compare>
#include <cstdint>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/base/pool_id.hpp"
#include "lyra/base/registry.hpp"
#include "lyra/base/time.hpp"
#include "lyra/hir/class_coordinate_id.hpp"
#include "lyra/hir/class_id.hpp"
#include "lyra/hir/continuous_assign.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/external_callee.hpp"
#include "lyra/hir/external_unit_object.hpp"
#include "lyra/hir/foreign_export.hpp"
#include "lyra/hir/owned_child_ref.hpp"
#include "lyra/hir/pattern.hpp"
#include "lyra/hir/port_direction.hpp"
#include "lyra/hir/procedural_scope.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/published_member.hpp"
#include "lyra/hir/sampled_history.hpp"
#include "lyra/hir/structural_data_object.hpp"
#include "lyra/hir/structural_hops.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/hir/value_ref.hpp"

namespace lyra::hir {

struct StructuralScope;

struct InterfacePortId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const InterfacePortId&) const
      -> std::strong_ordering = default;
};

// A declaration a unit published, named the way the scope that holds it names
// it. Which arena it lives in is what says how its storage is built: a data
// object owns a cell the scope installs, an instance member stands for an
// object the scope builds and owns, and an interface port stands for one the
// scope neither owns nor builds. The last two promise the same thing -- a
// borrowed pointer to another unit's object -- and differ only in what this
// scope does with it.
using PublishedDecl =
    std::variant<StructuralDataObjectId, InstanceMemberId, InterfacePortId>;

// One navigation step whose source and target objects are both declared by
// this compilation unit, so it realizes as typed member navigation.
// `indices` are the element coordinates within the named child, one per
// declared dimension: an instance array is a single child spanning every
// element (LRM 23.3.2), so the coordinates pick the element out of it. A
// generate loop instead elaborates each iteration into a child of its own
// (LRM 27.4), whose identity already fixes which iteration it is, so a
// generate step carries no coordinates.
struct OwnedChildStep {
  OwnedChildRef child;
  std::vector<std::uint32_t> indices;

  auto operator==(const OwnedChildStep&) const -> bool = default;
};

// One navigation step past this compilation unit's layout, into an object
// whose declaration another unit owns. The canonical hierarchical name is the
// only identity that crosses the boundary, so the step carries it verbatim for
// the runtime to resolve.
struct OpaqueStep {
  std::string name;
  std::vector<std::uint32_t> indices;

  auto operator==(const OpaqueStep&) const -> bool = default;
};

// One navigation step through an interface port of a scope on the path (LRM
// 25.3). The scope holds a borrowed reference the parent bound during
// elaboration, so the step is typed member navigation like an owned child's;
// what differs is that everything past it belongs to the unit the port names,
// which is why a leaf past this step is counted out of that unit's signature.
// `indices` are the element coordinates within the port, one per declared
// dimension: a port carrying a range is one member standing for every instance
// bound to it, so the coordinates pick one out of it. They are positions --
// the range the port declared is spent where the name resolves.
struct InterfacePortStep {
  InterfacePortId port;
  std::vector<std::uint32_t> indices;

  auto operator==(const InterfacePortStep&) const -> bool = default;
};

// One navigation step onto a member another unit published, whose type makes it
// an object of a third unit (LRM 25.3). It is the step form of the leaf that
// ends on a published member: the same record, the same position counted out of
// the same signature, reaching a pointer rather than a cell. `indices` pick one
// object out of a member standing for several. The position is what crosses,
// never the name -- a name identifies a step only where the route passes a
// signature, and this step lands on what one promised.
struct SignatureMemberStep {
  ExternalUnitObjectId object;
  PublishedMemberId member;
  std::vector<std::uint32_t> indices;

  auto operator==(const SignatureMemberStep&) const -> bool = default;
};

using PathStep = std::variant<
    OwnedChildStep, InterfacePortStep, SignatureMemberStep, OpaqueStep>;

// Where a route starts. `InUnitHead` anchors at a structural scope of this
// unit, `hops` typed parent edges out from the referrer (0 being the
// referrer's own scope); every step from there begins inside this unit's
// layout. `RootHead` anchors at the parent-less topmost scope named by
// `$root` (LRM 23.6). `VisibleChildHead` anchors at the scope an upward climb
// finds by name (LRM 23.8), which the referrer's unit does not declare and so
// cannot locate by a compile-time offset. Both climbing anchors leave the
// unit's layout, so everything past them is opaque.
struct InUnitHead {
  StructuralHops hops;

  auto operator==(const InUnitHead&) const -> bool = default;
};

struct RootHead {
  auto operator==(const RootHead&) const -> bool = default;
};

struct VisibleChildHead {
  std::string head_name;
  std::vector<std::uint32_t> head_indices;

  auto operator==(const VisibleChildHead&) const -> bool = default;
};

using RouteHead = std::variant<InUnitHead, RootHead, VisibleChildHead>;

// What a route ends at. A data object declared by the scope the steps land on,
// or a static-lifetime local of one of that scope's bodies, which a named block
// or a subroutine puts on the hierarchical path (LRM 23.9) -- every such scope
// between is part of where the storage sits, not a step of its own, so the leaf
// identity fixes the whole procedural descent. A leaf in another unit takes one
// of the forms below instead: against that unit's signature when it published
// the name, and against the runtime when it did not.
//
// Each leaf states everything the endpoint reaching it needs and nothing more.
// A leaf that ends at data states the storage its target holds and the data
// type behind it, because no consumer below can recover either: the declaration
// is in a scope the route walks to rather than one the reader can index, and
// past a signature there is no declaration at all. A leaf that ends at
// something other than data states neither.
struct StructuralDataObjectLeaf {
  StructuralDataObjectId object;
  PublishedStorage storage;
  TypeId type;

  auto operator==(const StructuralDataObjectLeaf&) const -> bool = default;
};

// The body a static-lifetime local was declared in. A static's identity is
// scoped to its body's declaration arena, so reaching one from elsewhere names
// the body alongside it.
using ProceduralBodyRef = std::variant<ProcessId, StructuralSubroutineId>;

struct ProceduralStaticLeaf {
  ProceduralBodyRef body;
  ProceduralVarId var;
  TypeId type;

  auto operator==(const ProceduralStaticLeaf&) const -> bool = default;
};

// The route ends at a member another unit published, at the position that
// unit's signature gave it. The name was resolved where this unit compiles, so
// a renamed member fails there rather than while the design elaborates.
struct SignatureMemberLeaf {
  ExternalUnitObjectId object;
  PublishedMemberId member;
  PublishedStorage storage;
  TypeId type;

  auto operator==(const SignatureMemberLeaf&) const -> bool = default;
};

// The route ends at the object the steps land on rather than at storage inside
// it. An interface port names a scope and not a value (LRM 25.3), so what a
// connection to one reaches is the instance itself.
struct ScopeLeaf {
  TypeId type;

  auto operator==(const ScopeLeaf&) const -> bool = default;
};

// The route ends past a signature, at a declaration no unit promised. Nothing
// was published to compile against, so the name is all that crosses and the
// runtime answers it while the design elaborates (LRM 23.6).
struct OpaqueLeaf {
  std::string name;
  PublishedStorage storage;
  TypeId type;

  auto operator==(const OpaqueLeaf&) const -> bool = default;
};

// The route ends past a signature too, at a subroutine no unit promised: a
// hierarchical name reaches a module's task or function (LRM 23.6, 23.8.1), and
// a module's signature is its parameters and ports. The name is all that
// crosses, and the scope answers it with an entry the way it answers one with a
// cell. `interface` is what the call passes and awaits, recomputed from the
// callee's declaration: nothing was published to shape the call, and the entry
// the scope publishes is generated from that same declaration, so the two
// cannot disagree.
struct OpaqueCallableLeaf {
  std::string name;
  ExternalCalleeInterface interface;

  auto operator==(const OpaqueCallableLeaf&) const -> bool = default;
};

// The route ends at what a `disable` naming a block or task terminates (LRM
// 9.6.2), where this artifact lays out the scope that declares it. The scope's
// identity indexes the registry of the structural scope the steps land on, so
// the procedural scopes between it and that scope are where the target sits
// rather than steps of their own -- the same reading a static declared in one
// of them takes.
struct DisableTargetLeaf {
  ProceduralScopeId scope;

  auto operator==(const DisableTargetLeaf&) const -> bool = default;
};

// The route ends at the same thing past a signature. No unit publishes what a
// `disable` terminates, so the steps reach the block's own node on the object
// tree and that node answers for the target it carries (LRM 23.9). It needs no
// name, because a scope has exactly one and the route already reached it.
struct OpaqueDisableTargetLeaf {
  auto operator==(const OpaqueDisableTargetLeaf&) const -> bool = default;
};

using RouteLeaf = std::variant<
    StructuralDataObjectLeaf, ProceduralStaticLeaf, SignatureMemberLeaf,
    ScopeLeaf, OpaqueLeaf, OpaqueCallableLeaf, DisableTargetLeaf,
    OpaqueDisableTargetLeaf>;

// A cell of the storage the declaring unit says its target is, holding a value
// of `type`.
struct EndpointCell {
  PublishedStorage storage;
  TypeId type;
};

// The object the route landed on, which is reached by a pointer to it with no
// cell in between.
struct EndpointObject {
  TypeId type;
};

// The entry a scope answered a callable's name with, which is a code address
// and so is already what a caller holds.
struct EndpointEntry {};

// What a `disable` naming the scope the route reached terminates (LRM 9.6.2).
// It is neither data nor an object of the design, so it has no data type: what
// a route ending here holds follows from the leaf alone.
struct EndpointDisableTarget {};

// What an endpoint reaching this leaf holds. Every consumer of a route asks
// this and nothing else about where it ends, so the answers are stated once
// here rather than re-derived from the leaf at each of them.
using Endpoint = std::variant<
    EndpointCell, EndpointObject, EndpointEntry, EndpointDisableTarget>;

[[nodiscard]] inline auto EndpointOf(const RouteLeaf& leaf) -> Endpoint {
  return std::visit(
      Overloaded{
          [](const StructuralDataObjectLeaf& l) -> Endpoint {
            return EndpointCell{.storage = l.storage, .type = l.type};
          },
          [](const SignatureMemberLeaf& l) -> Endpoint {
            return EndpointCell{.storage = l.storage, .type = l.type};
          },
          [](const OpaqueLeaf& l) -> Endpoint {
            return EndpointCell{.storage = l.storage, .type = l.type};
          },
          // A static-lifetime local is a variable wherever it sits (LRM 6.21),
          // so it needs no field to say so.
          [](const ProceduralStaticLeaf& l) -> Endpoint {
            return EndpointCell{.storage = VariableStorage{}, .type = l.type};
          },
          [](const ScopeLeaf& l) -> Endpoint {
            return EndpointObject{.type = l.type};
          },
          [](const OpaqueCallableLeaf&) -> Endpoint { return EndpointEntry{}; },
          [](const DisableTargetLeaf&) -> Endpoint {
            return EndpointDisableTarget{};
          },
          [](const OpaqueDisableTargetLeaf&) -> Endpoint {
            return EndpointDisableTarget{};
          }},
      leaf);
}

// How to navigate from a scope to a target elsewhere on the object tree:
// `head` is where navigation starts, `steps` carries the descent from there,
// and `leaf` is what it ends at. This is the route alone. Whether the route
// materializes a persistent endpoint slot (a value reference read on the hot
// path) or is resolved once for a one-shot bind (a `ref` port alias) is the
// consumer's endpoint-capability decision, not a property of the route.
struct RoutedPathRecipe {
  RouteHead head;
  std::vector<PathStep> steps;
  RouteLeaf leaf;

  auto operator==(const RoutedPathRecipe&) const -> bool = default;
};

// A routed reference that materializes a persistent endpoint slot, resolved
// once in the resolve phase after the object tree is fully built.
// The target's storage is stated by the unit declaring it, and with the
// recipe's leaf type it fixes the producer's actual cell, which the realized
// endpoint must match so a read reaches the right access protocol. The endpoint
// is read / written / observed through one stored direct reference.
struct RoutedRefDecl {
  RoutedPathRecipe recipe;
};

// A name on a class this artifact cannot name, and where that name lands on it.
// Such a class is nameable only inside the scope declaring it (LRM 23.9) and is
// a distinct type per instance of the element declaring it (LRM 6.22), so which
// class an access reaches is a fact of the instance and never of this artifact:
// one body serves every instance, and two of them may land on classes with
// different layouts.
//
// So the class is reached the way everything else past a signature is reached
// -- by walking to the scope and asking it by name -- and the answer, the
// position the member name lands on, is what the slot holds. `head` and `steps`
// are that walk; it ends at the scope rather than at anything the scope holds,
// which is why it carries no leaf.
//
// Which position the answer counts is the arena this sits in: storage among the
// declaring class's own properties, or an ordinal among the introducing class's
// own behaviors.
struct ClassNameDecl {
  RouteHead head;
  std::vector<PathStep> steps;
  std::string class_name;
  std::string name;
};

struct ConcurrentAssertionId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const ConcurrentAssertionId&) const
      -> std::strong_ordering = default;
};

// A concurrent assertion whose enabling condition is 1: an attempt begins at
// every tick of its clock, for the whole of the run, and nothing has to be true
// besides (LRM 16.14.5, and Annex F's top-level definition, where the enabling
// condition is what separates this from the same assertion written in a
// procedure). Having no condition to record, it has no position to record it
// at, which is why it is a declaration here rather than a statement.
//
// `action` is the body the statements an outcome selects live in, which such an
// assertion owns because no procedure encloses it.
//
// `standing_scope` is the scope the assertion itself stands in: the block a
// statement label named (LRM 9.3.5) where the source wrote one, and the action
// body's root where it did not. Every other scope in a body is named by the
// body that roots it or by the statement that opens it; an assertion is
// neither, so it names its own, and the name that scope carries is what a
// report calls the assertion.
struct ConcurrentAssertionDecl {
  diag::SourceSpan span;
  ConcurrentAssertion assertion;
  ProceduralBody action;
  ProceduralScopeId standing_scope;
};

// A child built from another compilation unit, standing on this unit's record
// of the object that unit's instances are. `array_dims` is empty for a scalar
// instance and holds one element count per dimension, outermost first, for an
// instance array (`Child c[2][3]` is `{2, 3}`).
struct InstanceMemberDecl {
  std::string instance_name;
  ExternalUnitObjectId object;
  std::vector<std::uint32_t> array_dims;
};

// An interface port's internal name (LRM 25.3). The scope names instances of
// another unit that it neither owns nor builds; the parent binds them during
// elaboration, the way it binds a `ref` port's internal name to the connected
// variable. `object` is this unit's record of what that unit published, so a
// name reached through the port is counted out of the order its signature
// states. `array_dims` is empty for a port standing for one instance and holds
// one element count per dimension, outermost first, for a port carrying a
// range: the port is one member however many instances it stands for, holding a
// handle on each.
struct InterfacePortDecl {
  std::string name;
  ExternalUnitObjectId object;
  std::vector<std::uint32_t> array_dims;
};

// How the child port is reached, by endpoint capability. An input or output
// port has its own cell, realized as a reactive edge over it (a variable cell
// written / read, a net cell driven / read), so it holds a persistent routed
// reference (`cell`, a `RoutedRef`) whose target capability (net versus
// variable) the reference itself carries. A `ref` port owns no cell: it is
// bound once in the resolve phase to the peer's cell, so it holds only the
// route to the child's reference member (a `RoutedPathRecipe`) -- no persistent
// slot, since a `ref` needs no simulation-time reach (LRM 23.3.3.2).
struct PortCellEndpoint {
  ExprId cell;
};
using PortEndpoint = std::variant<PortCellEndpoint, RoutedPathRecipe>;

struct PortConnectionId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const PortConnectionId&) const
      -> std::strong_ordering = default;
};

// A connection carrying data across the boundary (LRM 23.3.3). `endpoint`
// reaches the child's port member; `peer` is the parent-side connected
// expression; `sensitivity` is the read set the implied continuous assignment
// waits on (the peer's reads for an input port, the child port for an output
// port; empty for a `ref` port). HIR holds it verbatim and HIR-to-MIR realizes
// it: an input or output port as the implied continuous assignment between the
// two cells, a `ref` port as an alias bind of the child's reference member to
// the peer's cell, performed in the resolve phase (LRM 23.3.3.2).
struct DataPortConnection {
  PortDirection direction;
  PortEndpoint endpoint;
  ExprId peer;
  std::vector<SensitivityEntry> sensitivity;
};

// A connection binding a child's interface port to the interface instances it
// names (LRM 25.3). No value crosses in either direction, so there is nothing
// to drive and nothing to wait on: `endpoint` reaches the child's port member
// and each of `peers` reaches one instance bound there, all resolved once in
// the resolve phase, the way a `ref` port's alias is. A port carrying a range
// is bound to as many instances as it stands for, in the order its coordinates
// count them (LRM 23.3.3.5); a port standing for one has one peer, which is the
// no-dimension case of the same list rather than a shape of its own.
struct InterfacePortConnection {
  RoutedPathRecipe endpoint;
  std::vector<RoutedPathRecipe> peers;
};

struct PortConnection {
  diag::SourceSpan span;
  std::variant<DataPortConnection, InterfacePortConnection> kind;
};

// The lowered form of every generate construct (LRM 27): after frontend
// elaboration each construct is a set of blocks with an instantiated / not
// flag, so the lowering is one fully concrete scope per instantiated block,
// constructed unconditionally. Each scope is lowered from its own elaborated
// body -- its own selected arm, types, and slice widths, the genvar folded to
// a constant -- never borrowed from another block and never a runtime
// induction value or branch. A block's position here is its identity, so
// nothing restates which block a scope is.
struct Generate {
  base::Arena<StructuralScope, StructuralScopeId> child_scopes;
};

struct StructuralScope {
  // LRM source name of a generate child (label, or `genblk<n>` when unnamed,
  // LRM 27.6); empty for other scopes.
  std::string source_name;
  // The elaborated hierarchy index a generate loop iteration carries (LRM
  // 27.4); absent for an `if` / `case` arm, a bare block, and every scope no
  // generate produced. The index and the source label together are this
  // scope's whole hierarchy segment, which the scope carries itself rather
  // than leaving in a table its parent keeps about it.
  std::optional<std::int64_t> index;
  TimeResolution time_resolution;
  base::Arena<StructuralDataObjectDecl, StructuralDataObjectId>
      structural_data_objects;
  // The declarations this unit published, in the order its signature states
  // them -- which is where their storage sits, since a referrer counts a
  // published member's position out of that same order. Empty for a scope no
  // other unit names, which is every scope but the one a unit's instances are.
  std::vector<PublishedDecl> published_members;
  base::Arena<Expr, ExprId> exprs;
  base::Arena<Pattern, PatternId> patterns;
  base::Registry<Process, ProcessId> processes;
  base::Arena<ContinuousAssign, ContinuousAssignId> continuous_assigns;
  base::Registry<Generate, GenerateId> generates;
  base::Registry<InstanceMemberDecl, InstanceMemberId> instance_members;
  base::Arena<InterfacePortDecl, InterfacePortId> interface_ports;
  base::Arena<PortConnection, PortConnectionId> port_connections;
  base::Arena<RoutedRefDecl, RoutedRefId> routed_refs;
  base::Arena<ClassNameDecl, PropertyCoordinateId> property_coordinates;
  base::Arena<ClassNameDecl, BehaviorCoordinateId> behavior_coordinates;
  // The cells something in this scope reads a sampled value of (LRM 16.5.1),
  // each named the way an event control names what it watches -- so one reached
  // across an instance boundary is carried by its route like any other. A cell
  // answers for a sampled value only once armed, and arming installs the value
  // every read answers with until a later time slot first changes it, so it
  // happens once every variable initializer in the design has run.
  std::vector<SensitivityEntry> sampled_cells;
  // What something in this scope reads across the ticks of a clocking event
  // (LRM 16.9.3). A history's subject is an expression rather than a reference,
  // so it lives in this scope's own arena the way a continuous assignment's
  // does: nothing the user wrote evaluates it, and the process that does is
  // synthesized a layer down.
  base::Registry<SampledHistoryDecl, SampledHistoryId> sampled_histories;
  // The concurrent assertions this scope declares. They are an arena rather
  // than a registry because nothing names one before it exists: no reference
  // reaches an assertion, and what starts its attempts is the clocking event.
  base::Arena<ConcurrentAssertionDecl, ConcurrentAssertionId>
      concurrent_assertions;
  // Body-bearing SV subroutines only. A bodyless DPI-C import never enters this
  // arena; the unit owns it, because its foreign symbol is program-global and
  // belongs to no scope (LRM 35.4).
  base::Registry<SubroutineDecl, StructuralSubroutineId> structural_subroutines;
  // The classes this scope declares (LRM 23.9 lists a class among the elements
  // that define a scope). A class declared here is a type of this scope's
  // instance (LRM 6.22), so an object of it belongs to the one instance it was
  // created in and its bodies name that instance's declarations; stating the
  // relation on the scope is what lets the instance be supplied where it is
  // known. Every class a unit declares is named by exactly one scope, the
  // namespace unit's root scope included, so nothing is reached by elimination.
  std::vector<ClassId> declared_classes;
  std::vector<ForeignExportDecl> foreign_exports;
  // Every scope's identity is minted before any body is lowered, so a `disable`
  // naming one (LRM 9.6.2) -- possibly from another process lowered first --
  // carries a stable typed id rather than a name it would have to resolve
  // later. That is what the declare-then-define gap buys: the id exists up
  // front and the body pass fills the contents when it reaches the scope.
  base::Registry<ProceduralScopeDecl, ProceduralScopeId> procedural_scopes;
};

}  // namespace lyra::hir
