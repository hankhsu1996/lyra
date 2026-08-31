#pragma once

#include <compare>
#include <cstdint>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/base/registry.hpp"
#include "lyra/base/time.hpp"
#include "lyra/hir/continuous_assign.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/external_unit_object.hpp"
#include "lyra/hir/foreign_export.hpp"
#include "lyra/hir/pattern.hpp"
#include "lyra/hir/port_direction.hpp"
#include "lyra/hir/procedural_scope.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/published_member.hpp"
#include "lyra/hir/structural_data_object.hpp"
#include "lyra/hir/structural_hops.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/hir/value_ref.hpp"

namespace lyra::hir {

struct StructuralScope;

struct GenerateId {
  std::uint32_t value;

  auto operator<=>(const GenerateId&) const -> std::strong_ordering = default;
};

struct StructuralScopeId {
  std::uint32_t value;

  auto operator<=>(const StructuralScopeId&) const
      -> std::strong_ordering = default;
};

struct InstanceMemberId {
  std::uint32_t value;

  auto operator<=>(const InstanceMemberId&) const
      -> std::strong_ordering = default;
};

struct InterfacePortId {
  std::uint32_t value;

  auto operator<=>(const InterfacePortId&) const
      -> std::strong_ordering = default;
};

// A declaration a unit published, named the way the scope that holds it names
// it. Which arena it lives in is what says how its storage is built: a data
// object owns a cell the scope installs, an interface port stands for an object
// the scope neither owns nor builds.
using PublishedDecl = std::variant<StructuralDataObjectId, InterfacePortId>;

// A generate block (LRM 27) as a child of the scope that declares it: the
// generate construct it belongs to, plus which of that construct's elaborated
// blocks it is.
struct GenerateChildRef {
  GenerateId generate;
  StructuralScopeId scope;

  auto operator==(const GenerateChildRef&) const -> bool = default;
};

// A child object the referrer's compilation unit declares, named by the
// declaring scope's own identity for it.
using OwnedChildRef = std::variant<InstanceMemberId, GenerateChildRef>;

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
struct InterfacePortStep {
  InterfacePortId port;

  auto operator==(const InterfacePortStep&) const -> bool = default;
};

using PathStep = std::variant<OwnedChildStep, InterfacePortStep, OpaqueStep>;

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

// The storage a route ends at. A data object declared by the scope the steps
// land on, or a static-lifetime local of one of that scope's bodies, which a
// named block puts on the hierarchical path (LRM 23.9) -- the blocks between
// are part of where the storage sits, not steps of their own, so the leaf
// identity fixes the whole procedural descent. A leaf in another unit takes one
// of the forms below instead: against that unit's signature when it published
// the name, and against the runtime when it did not.
struct StructuralDataObjectLeaf {
  StructuralDataObjectId object;

  auto operator==(const StructuralDataObjectLeaf&) const -> bool = default;
};

// The body a static-lifetime local was declared in. A static's identity is
// scoped to its body's declaration arena, so reaching one from elsewhere names
// the body alongside it.
using ProceduralBodyRef = std::variant<ProcessId, StructuralSubroutineId>;

struct ProceduralStaticLeaf {
  ProceduralBodyRef body;
  ProceduralVarId var;

  auto operator==(const ProceduralStaticLeaf&) const -> bool = default;
};

// The route ends at a member another unit published, at the position that
// unit's signature gave it. The name was resolved where this unit compiles, so
// a renamed member fails there rather than while the design elaborates.
struct SignatureMemberLeaf {
  ExternalUnitObjectId object;
  PublishedMemberId member;

  auto operator==(const SignatureMemberLeaf&) const -> bool = default;
};

// The route ends at the object the steps land on rather than at storage inside
// it. An interface port names a scope and not a value (LRM 25.3), so what a
// connection to one reaches is the instance itself.
struct ScopeLeaf {
  auto operator==(const ScopeLeaf&) const -> bool = default;
};

// The route ends past a signature, at a declaration no unit promised. Nothing
// was published to compile against, so the name is all that crosses and the
// runtime answers it while the design elaborates (LRM 23.6).
struct OpaqueLeaf {
  std::string name;

  auto operator==(const OpaqueLeaf&) const -> bool = default;
};

using RouteLeaf = std::variant<
    StructuralDataObjectLeaf, ProceduralStaticLeaf, SignatureMemberLeaf,
    ScopeLeaf, OpaqueLeaf>;

// How to navigate from a scope to a target elsewhere on the object tree:
// `head` is where navigation starts, `steps` carries the descent from there,
// and `leaf` is the storage it ends at. `type` is the slang-resolved leaf data
// type. This is the route alone. Whether the route materializes a persistent
// endpoint slot (a value reference read on the hot path) or is resolved once
// for a one-shot bind (a `ref` port alias) is the consumer's
// endpoint-capability decision, not a property of the route.
struct RoutedPathRecipe {
  RouteHead head;
  std::vector<PathStep> steps;
  RouteLeaf leaf;
  TypeId type;

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
  PublishedStorage target_storage;
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

// An interface port's internal name (LRM 25.3). The scope names an instance of
// another unit that it neither owns nor builds; the parent binds it during
// elaboration, the way it binds a `ref` port's internal name to the connected
// variable. `object` is this unit's record of what that unit published, so a
// name reached through the port is counted out of the order its signature
// states.
struct InterfacePortDecl {
  std::string name;
  ExternalUnitObjectId object;
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
  std::uint32_t value;

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

// A connection binding a child's interface port to an interface instance
// (LRM 25.3). No value crosses in either direction, so there is nothing to
// drive and nothing to wait on: `endpoint` reaches the child's port member and
// `peer` reaches the instance bound there, both resolved once in the resolve
// phase, the way a `ref` port's alias is.
struct InterfacePortConnection {
  RoutedPathRecipe endpoint;
  RoutedPathRecipe peer;
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
  // Body-bearing SV subroutines only. A bodyless DPI-C import never enters this
  // arena; the unit owns it, because its foreign symbol is program-global and
  // belongs to no scope (LRM 35.4).
  base::Registry<SubroutineDecl, StructuralSubroutineId> structural_subroutines;
  std::vector<ForeignExportDecl> foreign_exports;
  // Every scope's identity is minted before any body is lowered, so a `disable`
  // naming one (LRM 9.6.2) -- possibly from another process lowered first --
  // carries a stable typed id rather than a name it would have to resolve
  // later. That is what the declare-then-define gap buys: the id exists up
  // front and the body pass fills the contents when it reaches the scope.
  base::Registry<ProceduralScopeDecl, ProceduralScopeId> procedural_scopes;
};

}  // namespace lyra::hir
