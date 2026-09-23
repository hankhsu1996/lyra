#pragma once

#include <compare>
#include <cstdint>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/base/internal_error.hpp"
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

  auto operator==(const RoutedRefDecl&) const -> bool = default;
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

  auto operator==(const ClassNameDecl&) const -> bool = default;
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

  auto operator==(const ConcurrentAssertionDecl&) const -> bool = default;
};

// A child built from another compilation unit, standing on this unit's record
// of the object that unit's instances are. `array_dims` is empty for a scalar
// instance and holds one element count per dimension, outermost first, for an
// instance array (`Child c[2][3]` is `{2, 3}`).
struct InstanceMemberDecl {
  std::string instance_name;
  ExternalUnitObjectId object;
  std::vector<std::uint32_t> array_dims;

  auto operator==(const InstanceMemberDecl&) const -> bool = default;
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

  auto operator==(const InterfacePortDecl&) const -> bool = default;
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

  auto operator==(const PortCellEndpoint&) const -> bool = default;
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
//
// A bidirectional port is not one of these. Nothing crosses it in either
// direction, so it has no source, no sink and nothing to wait on; what it
// states is a join.
struct DataPortConnection {
  PortDirection direction;
  PortEndpoint endpoint;
  ExprId peer;
  std::vector<SensitivityEntry> sensitivity;

  auto operator==(const DataPortConnection&) const -> bool = default;
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

  auto operator==(const InterfacePortConnection&) const -> bool = default;
};

struct PortConnection {
  diag::SourceSpan span;
  std::variant<DataPortConnection, InterfacePortConnection> kind;

  auto operator==(const PortConnection&) const -> bool = default;
};

// A run of one net's positions and an equally wide run of another's, which a
// construct states are the same physical net (LRM 23.3.3.7, 10.11). `here` and
// `there` name the two nets whole, and the offsets say where the shared run
// starts in each of their values, counted from the first position.
//
// What such a construct states is a position-wise overlay -- LRM 10.11 gives it
// the bit overlay rules of a packed union, and LRM 7.6 makes whole-value
// correspondence positional rather than range-relative -- so the declared range
// each side names its own positions by is read once, here, where the source
// wrote the select, and is not a fact any layer below needs. A construct naming
// one whole net on each side states the run that covers both, which is the
// case every design that joins whole nets is in rather than a shape of its own.
//
// Nothing is driven, read, or waited on: a bidirectional connection (LRM
// 23.3.3) and an `alias` (LRM 10.11) both state which positions resolve
// together and state no direction, so one construct spelling covers both.
struct NetJoin {
  diag::SourceSpan span;
  ExprId here;
  std::uint32_t here_offset{};
  ExprId there;
  std::uint32_t there_offset{};
  std::uint32_t width{};

  auto operator==(const NetJoin&) const -> bool = default;
};

// Each scope stands for one instantiated block and is built once: a bare
// block, and a loop whose blocks did not lower alike. Every scope is lowered
// from its own elaborated body -- its own types and slice widths -- and the
// index reaches it as a value its construction supplies here too, because that
// is what leaves two blocks differing in nothing else with nothing to differ
// in.
struct BlocksStandAlone {
  auto operator==(const BlocksStandAlone&) const -> bool = default;
};

// The one scope is built once at every index the loop counts out (LRM 27.4).
// What makes one scope enough is that the index reaches the block as a value
// construction supplies rather than as a constant folded into its body, so
// `index` is that block's own declaration of it. `variable` is the loop's
// index, declared by the scope holding the generate, and the three expressions
// are the loop's own: where the index starts, whether a block stands at it,
// and how it reaches the next one. The first two are values the loop reads;
// the step is written for its effect on the index and its own value is
// discarded, the same way a loop written among statements states its step.
struct BlocksRepeat {
  StructuralDataObjectId variable;
  ExprId initial;
  ExprId condition;
  ExprId step;
  StructuralDataObjectId index;

  auto operator==(const BlocksRepeat&) const -> bool = default;
};

struct SelectionChoiceId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const SelectionChoiceId&) const
      -> std::strong_ordering = default;
};

// Nothing stands on this side: an `if` the source gave no `else`, or a `case`
// it gave no `default`.
struct NothingStands {
  auto operator==(const NothingStands&) const -> bool = default;
};

// One of the construct's alternatives stands here, at the position the source
// wrote it.
struct AlternativeStands {
  std::uint32_t position = 0;

  auto operator==(const AlternativeStands&) const -> bool = default;
};

// What stands on one side of a choice. A conditional the source wrote inside
// another's selected side belongs to the outer construct (LRM 27.5), which is
// what a branch leading to a further choice is.
using SelectionBranch =
    std::variant<NothingStands, AlternativeStands, SelectionChoiceId>;

// An `if`: the condition, and what stands on each side of it. An `else` is not
// a condition of its own, so nothing here spells a negation and whatever tests
// it says how.
struct ChoiceOnCondition {
  ExprId condition;
  SelectionBranch holds;
  SelectionBranch fails;

  auto operator==(const ChoiceOnCondition&) const -> bool = default;
};

// One item of a `case`: the labels the source wrote for it, and what stands
// where the selector matches one of them. A comparison against a label
// succeeds only where every bit matches exactly, `x` and `z` included.
struct LabeledItem {
  std::vector<ExprId> labels;
  SelectionBranch stands;

  auto operator==(const LabeledItem&) const -> bool = default;
};

// A `case`: the expression it selects on, its items in the order the source
// wrote them, and what stands where no item matched. The order is the whole of
// what LRM 12.5 says about the search -- it stops at the first match, so an
// item is reached only once every item before it has failed and the `default`
// only once all of them have. Keeping the items in that order is what lets each
// state its own labels and nothing else.
struct ChoiceOnLabel {
  ExprId selector;
  std::vector<LabeledItem> items;
  SelectionBranch otherwise;

  auto operator==(const ChoiceOnLabel&) const -> bool = default;
};

// One conditional generate construct the source wrote, which selects at most
// one of the branches leading out of it.
using SelectionChoice = std::variant<ChoiceOnCondition, ChoiceOnLabel>;

// A conditional generate selects at most one block from a set of alternative
// blocks (LRM 27.5). What selects it is an expression, so where that
// expression reads a value the construction supplies -- a loop's index -- two
// indices select different alternatives of the same construct, and the
// construct states every alternative the source wrote rather than the one its
// own index selected.
//
// The choices are held as the source nested them, each condition stated once,
// because that is what an alternative standing under several of them costs
// nothing to say. `root` is the outermost, and a branch reaching a further
// choice is a conditional the source wrote inside the selected side of the one
// that reached it.
struct BlocksChoose {
  base::Arena<SelectionChoice, SelectionChoiceId> choices;
  SelectionChoiceId root;
  // One entry per alternative the source wrote, in that order: the scope its
  // block compiled to, or nothing where no elaboration of the construct
  // selected it. Such an alternative keeps its place because the positions
  // after it are the positions the source wrote, which is what a name
  // resolves against.
  std::vector<std::optional<StructuralScopeId>> alternatives;

  auto operator==(const BlocksChoose&) const -> bool = default;
};

// The lowered form of every generate construct (LRM 27). A block's position in
// `child_scopes` is its identity, so nothing restates which block a scope is,
// and how many objects a scope stands for is what `counting` says.
struct Generate {
  base::Arena<StructuralScope, StructuralScopeId> child_scopes;
  std::variant<BlocksStandAlone, BlocksRepeat, BlocksChoose> counting =
      BlocksStandAlone{};

  auto operator==(const Generate&) const -> bool = default;
};

// Which compiled scope a route naming one of a generate's blocks reaches.
// Every consumer of a route asks this and nothing else about a generate, so
// the answer is stated once here rather than re-derived at each of them.
//
// How a name identifies a block and how many scopes the construct compiled to
// are settled independently, so the two are paired here: a repeated structure
// compiles its indices to one scope, a conditional compiles each alternative
// it holds to one, and a construct that neither repeats nor chooses compiles
// its blocks one for one. A pairing the language does not have is a name that
// was resolved against a different construct than the one it reached.
[[nodiscard]] inline auto ChildScopeOf(
    const Generate& gen, const NamedBlock& block) -> StructuralScopeId {
  return std::visit(
      Overloaded{
          [&](const BlockAtIndex& at) {
            return std::visit(
                Overloaded{
                    [&](const BlocksStandAlone&) {
                      return StructuralScopeId{at.index};
                    },
                    [](const BlocksRepeat&) { return StructuralScopeId{0}; },
                    [](const BlocksChoose&) -> StructuralScopeId {
                      throw InternalError(
                          "hir::ChildScopeOf: a conditional's block is named "
                          "by an index");
                    },
                },
                gen.counting);
          },
          [&](const BlockAsAlternative& as) -> StructuralScopeId {
            const auto* chosen = std::get_if<BlocksChoose>(&gen.counting);
            if (chosen == nullptr ||
                as.position >= chosen->alternatives.size()) {
              throw InternalError(
                  "hir::ChildScopeOf: a block of a construct that chooses "
                  "nothing is named as one of its alternatives");
            }
            const std::optional<StructuralScopeId> block =
                chosen->alternatives[as.position];
            if (!block.has_value()) {
              throw InternalError(
                  "hir::ChildScopeOf: a name reached an alternative no "
                  "elaboration of the construct selected");
            }
            return *block;
          },
      },
      block);
}

// Which object of that scope the same name reaches, where the scope stands for
// more than one. Only a repeated structure does: its blocks are one scope built
// at every index, so the index the name carried survives as a coordinate on it.
// Every other form compiles a block to a scope of its own and leaves nothing to
// coordinate.
[[nodiscard]] inline auto ChildElementOf(
    const Generate& gen, const NamedBlock& block)
    -> std::optional<std::uint32_t> {
  const auto* at = std::get_if<BlockAtIndex>(&block);
  if (at == nullptr) return std::nullopt;
  return std::visit(
      Overloaded{
          [](const BlocksStandAlone&) {
            return std::optional<std::uint32_t>{};
          },
          [](const BlocksChoose&) { return std::optional<std::uint32_t>{}; },
          [&](const BlocksRepeat&) {
            return std::optional<std::uint32_t>{at->index};
          },
      },
      gen.counting);
}

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
  // them -- which is the order the promise states a behavior for each of them
  // in, since a referrer counts which behavior it wants out of that same order.
  // Empty for a scope no other unit names, which is every scope but the one a
  // unit's instances are.
  std::vector<PublishedDecl> published_members;
  // The subroutines this unit published, by the identifier its signature states
  // each under and in that same order, which continues the order above: a
  // promise states a behavior per published member and then one per published
  // subroutine. A name is what crosses because a name is what the signature
  // carries, and the scope answers it from its own subroutines -- an identifier
  // the signature minted for a view being one of those like any other. Empty on
  // the same scopes the list above is.
  std::vector<std::string> published_callables;
  base::Arena<Expr, ExprId> exprs;
  base::Arena<Pattern, PatternId> patterns;
  base::Registry<Process, ProcessId> processes;
  base::Arena<ContinuousAssign, ContinuousAssignId> continuous_assigns;
  base::Registry<Generate, GenerateId> generates;
  base::Registry<InstanceMemberDecl, InstanceMemberId> instance_members;
  base::Arena<InterfacePortDecl, InterfacePortId> interface_ports;
  base::Arena<PortConnection, PortConnectionId> port_connections;
  // The runs of nets this scope's constructs place in one resolution. A plain
  // list rather than an arena because nothing names one: no reference reaches a
  // join, and what it states is already complete when it is recorded.
  std::vector<NetJoin> net_joins;
  base::Arena<RoutedRefDecl, RoutedRefId> routed_refs;
  base::Arena<ClassNameDecl, PropertyCoordinateId> property_coordinates;
  base::Arena<ClassNameDecl, BehaviorCoordinateId> behavior_coordinates;
  // The bodies a name reaches on a class this scope's walk lands on, for a
  // method that answers no dispatch position -- what such a call runs is fixed
  // by the class the access names (LRM 8.14), so what is settled is the body
  // itself rather than a position something else answers.
  base::Arena<ClassNameDecl, BehaviorBodyId> behavior_bodies;
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

  // Two scopes are equal when everything they state is equal, `index`
  // included -- which is why a block carries none until whether it is one body
  // with its siblings has been settled. It is the one thing a loop's blocks
  // differ in by definition, so stamping it before the comparison would answer
  // "not the same" about every loop there is.
  //
  // Derived rather than written. A field added to any node below is compared
  // without anyone remembering to, and a field that cannot be compared breaks
  // the build rather than being silently left out -- which is the direction
  // this has to fail in, because a comparison that misses something answers
  // "the same" about two things that are not.
  auto operator==(const StructuralScope&) const -> bool = default;
};

}  // namespace lyra::hir
