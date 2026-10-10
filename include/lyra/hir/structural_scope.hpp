#pragma once

#include <compare>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/base/pool_id.hpp"
#include "lyra/base/registry.hpp"
#include "lyra/base/time.hpp"
#include "lyra/base/translation.hpp"
#include "lyra/hir/class_id.hpp"
#include "lyra/hir/class_ref.hpp"
#include "lyra/hir/continuous_assign.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/external_scope_class.hpp"
#include "lyra/hir/external_scope_ref.hpp"
#include "lyra/hir/foreign_export.hpp"
#include "lyra/hir/owned_child_ref.hpp"
#include "lyra/hir/pattern.hpp"
#include "lyra/hir/port_direction.hpp"
#include "lyra/hir/procedural_scope.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/published_callable.hpp"
#include "lyra/hir/published_member.hpp"
#include "lyra/hir/published_scope.hpp"
#include "lyra/hir/sampled_history.hpp"
#include "lyra/hir/structural_data_object.hpp"
#include "lyra/hir/structural_hops.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/value_ref.hpp"

namespace lyra::hir {

struct StructuralScope;

struct InterfacePortId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const InterfacePortId&) const
      -> std::strong_ordering = default;
};

// What one element of a route names: a child of this unit, an interface port
// of a scope on the path (LRM 25.3) -- a borrowed reference the parent bound
// during elaboration, past which everything belongs to the unit the port names
// -- or something a scope of another unit published.
using StepTarget =
    std::variant<OwnedChildRef, InterfacePortId, ExternalScopeRef>;

using PathStep = PathElement<StepTarget>;

// An element naming a child of this unit, or something another unit
// published, as an element of a route.
template <typename Names>
[[nodiscard]] auto AsPathStep(PathElement<Names> step) -> PathStep {
  return PathStep{
      .names = StepTarget{std::move(step.names)},
      .selects = std::move(step.selects)};
}

// Where a route starts. `InUnitBase` anchors at a structural scope of this
// unit, `hops` typed parent edges out from the referrer (0 being the
// referrer's own scope); every step from there begins inside this unit's
// layout.
struct InUnitBase {
  StructuralHops hops;

  auto operator==(const InUnitBase&) const -> bool = default;
};

// The route leaves the referrer's instance and starts at the enclosing scope
// of the class `scope_class` records: the nearest scope enclosing the referrer
// that is of that class, an instance or a generate block, or past the topmost
// of them a top-level instance of it -- the scope a name searched upward lands
// in (LRM 23.8), or the top-level instance a path from the top names (LRM
// 23.6). Which scope that is depends on where the referrer's instance stands,
// so the class is what the route states and the scope is found where it
// resolves.
struct EnclosingScopeBase {
  ExternalScopeClassId scope_class;

  auto operator==(const EnclosingScopeBase&) const -> bool = default;
};

using RouteBase = std::variant<InUnitBase, EnclosingScopeBase>;

// What a route ends at, grouped by the use the name is put to, since the use
// decides what the name may reach. Within a use, the end is named in one of
// two ways: by this unit's own declaration, or by the position another unit's
// signature gave it.
//
// A value ends at data: a data object declared by the scope the steps land on,
// or a static-lifetime local of one of that scope's bodies, which a named block
// or a subroutine puts on the hierarchical path (LRM 23.9) -- every such scope
// between is part of where the storage sits, not a step of its own, so the leaf
// identity fixes the whole procedural descent. A leaf in another unit is named
// against that unit's signature instead, which published every such
// declaration. Each states the storage its target holds and the data type
// behind it, because no consumer below can recover either: the declaration is
// in a scope the route walks to rather than one the reader can index.
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

// A static-lifetime local a scope published: the body declaring it, and its
// identity there.
struct PublishedStatic {
  ProceduralBodyRef body;
  ProceduralVarId var;

  auto operator==(const PublishedStatic&) const -> bool = default;
};

// A declaration a scope published, named the way the scope that holds it names
// it. Which arena it lives in is what says how its storage is built: a data
// object owns a cell the scope installs, a static-lifetime local owns one the
// scope holds for the body declaring it, a static property of a class the scope
// declares owns one the scope holds for that class (LRM 6.22, 8.9), an instance
// member stands for an object the scope builds and owns, and an interface port
// stands for one the scope neither owns nor builds. The last two publish the
// same thing -- a borrowed pointer to another unit's object -- and differ only
// in what this scope does with it.
using PublishedDecl = std::variant<
    StructuralDataObjectId, PublishedStatic, LocalStaticPropertyTarget,
    InstanceMemberId, InterfacePortId>;

// What a scope published: `signature` is the class the unit's signature states
// for it, in this unit's own types, which is what the scope's published class
// is laid out from on this side and every referrer's side alike; and each of
// its entries is the scope's own declaration, keyed by its position there. The
// subroutines are the class's methods.
//
// The signature's class path is the one a referrer reaches the class by.
// Several scopes of a unit may state one class: the block instances of one
// application of a generate block are objects of one class (LRM 27.3), and
// those of them that lowered apart are that class realized by a scope each.
struct ScopePublication {
  ScopeClassSignature signature;
  base::Translation<PublishedMemberId, PublishedDecl> members;
  base::Translation<PublishedGenerateId, GenerateId> generates;
  base::Translation<PublishedDisableTargetId, ProceduralScopeId>
      disable_targets;
  base::Translation<PublishedCallableId, StructuralSubroutineId> callables;

  auto operator==(const ScopePublication&) const -> bool = default;
};

// The route ends at a member another unit published, at the position that
// unit's signature gave it. The name was resolved where this unit compiles, so
// a renamed member fails there rather than while the design elaborates.
struct ExternalMemberLeaf {
  ExternalScopeClassId scope_class;
  PublishedMemberId member;
  PublishedStorage storage;
  TypeId type;

  auto operator==(const ExternalMemberLeaf&) const -> bool = default;
};

using DataLeaf = std::variant<
    StructuralDataObjectLeaf, ProceduralStaticLeaf, ExternalMemberLeaf>;

// A cell of the storage the declaring unit says a data end is, holding a value
// of `type`.
struct DataCell {
  PublishedStorage storage;
  TypeId type;
};

[[nodiscard]] inline auto CellOf(const DataLeaf& leaf) -> DataCell {
  return std::visit(
      Overloaded{
          [](const StructuralDataObjectLeaf& l) {
            return DataCell{.storage = l.storage, .type = l.type};
          },
          // A static-lifetime local is a variable wherever it sits (LRM 6.21),
          // so it needs no field to say so.
          [](const ProceduralStaticLeaf& l) {
            return DataCell{.storage = VariableStorage{}, .type = l.type};
          },
          [](const ExternalMemberLeaf& l) {
            return DataCell{.storage = l.storage, .type = l.type};
          }},
      leaf);
}

// The object a call is made on ends at the object the steps land on rather
// than at storage inside it, and so does a connection to an interface port,
// which names a scope and not a value (LRM 25.3).
struct ScopeLeaf {
  TypeId type;

  auto operator==(const ScopeLeaf&) const -> bool = default;
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

// The route ends at the same thing in a scope another unit published, at the
// position its signature gave that disable target among the scope's own.
struct ExternalDisableTargetLeaf {
  ExternalScopeClassId scope_class;
  PublishedDisableTargetId target;

  auto operator==(const ExternalDisableTargetLeaf&) const -> bool = default;
};

using DisableLeaf = std::variant<DisableTargetLeaf, ExternalDisableTargetLeaf>;

// How to navigate from a scope to what a name reaches: `base` is where
// navigation starts, `steps` carries the descent from there, and `leaf` is
// what it ends at, of the kinds the name's use allows. The path is one shape
// for every use; only the end differs. Whether the route is kept in a slot of
// its reader or walked where it is used, and when, is the lowering's decision
// and not a property of the route.
template <typename Leaf>
struct Route {
  RouteBase base;
  std::vector<PathStep> steps;
  Leaf leaf;

  auto operator==(const Route&) const -> bool = default;
};

using ValueRoute = Route<DataLeaf>;
using ObjectRoute = Route<ScopeLeaf>;
using DisableTargetRoute = Route<DisableLeaf>;

// Every walk one scope's names take, gathered while its bodies are lowered and
// handed to the scope whole, a table per use a name is put to.
struct ScopeRoutes {
  base::Arena<ValueRoute, RoutedValueRefId> values;
  base::Arena<ObjectRoute, RoutedObjectRefId> objects;
  base::Arena<DisableTargetRoute, RoutedDisableTargetRefId> disable_targets;

  auto operator==(const ScopeRoutes&) const -> bool = default;
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

// One way the objects of an instance declaration are built: the unit they are
// instances of, standing on this unit's record of the class that unit's
// instances are, and what each is passed at construction, one per parameter
// the unit takes there, in the order it declares them -- the expression this
// scope wrote (LRM 23.10.2), evaluated where the object is built, or the
// constant a value given elsewhere settled to (LRM 23.10.1, 33.4.3).
struct InstanceAlternative {
  ExternalScopeClassId scope_class;
  std::vector<ExprId> arguments;

  auto operator==(const InstanceAlternative&) const -> bool = default;
};

// A child built from another compilation unit, under the name a hierarchical
// name writes for it (LRM 23.6): the identifier as it stands, or escaped where
// such a name has to escape it. `array_dims` is empty for a scalar instance
// and holds the range each dimension declares, outermost first, for an
// instance array (`Child c[2][3:5]` is `{[0:1], [3:5]}`). The range is how
// many elements a dimension has and which index selects each, the element at
// a position being the one that many above the range's lowest index.
//
// Every element of an array takes what its instantiation wrote (LRM 23.3.2),
// so its elements differ only where something written elsewhere reached one of
// them. `alternatives` are the distinct ways its objects are built and `taken`
// says which each object is, one entry per object in row-major order: alike
// objects share one alternative, so instances handed different values by the
// instantiation are one unit and this scope states the same thing for each.
struct InstanceMemberDecl {
  std::string instance_name;
  std::vector<UnpackedRange> array_dims;
  std::vector<InstanceAlternative> alternatives;
  std::vector<std::uint32_t> taken;

  auto operator==(const InstanceMemberDecl&) const -> bool = default;
};

// An interface port's internal name (LRM 25.3). The scope names instances of
// another unit that it neither owns nor builds; the parent binds them during
// elaboration, the way it binds a `ref` port's internal name to the connected
// variable. `array_dims` is empty for a port standing for one instance and
// holds the range each dimension declares, outermost first, for a port
// carrying one: the port is one member however many instances it stands for,
// holding a handle on each. Which kind of instance is bound at each position
// is what the type this scope published for the port states.
struct InterfacePortDecl {
  std::string name;
  std::vector<UnpackedRange> array_dims;

  auto operator==(const InterfacePortDecl&) const -> bool = default;
};

// How the child port is reached, by endpoint capability. An input or output
// port has its own cell, realized as a reactive edge over it (a variable cell
// written / read, a net cell driven / read), so it holds a value reference
// (`cell`) whose target capability (net versus variable) the reference itself
// carries. A `ref` port owns no cell: it is bound once to the peer's cell, so
// it holds only the route to the child's reference member -- no reference of
// its own, since a `ref` needs no simulation-time reach (LRM 23.3.3.2).
struct PortCellEndpoint {
  ExprId cell;

  auto operator==(const PortCellEndpoint&) const -> bool = default;
};
using PortEndpoint = std::variant<PortCellEndpoint, ValueRoute>;

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
// the peer's cell, bound once (LRM 23.3.3.2).
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
// and each of `peers` reaches one instance bound there, each bound once, the
// way a `ref` port's alias is. A port carrying a range is bound to as many
// instances as it stands for, in the order its coordinates count them (LRM
// 23.3.3.5); a port standing for one has one peer, which is the no-dimension
// case of the same list rather than a shape of its own.
struct InterfacePortConnection {
  ValueRoute endpoint;
  std::vector<ObjectRoute> peers;

  auto operator==(const InterfacePortConnection&) const -> bool = default;
};

struct PortConnection {
  diag::SourceSpan span;
  std::variant<DataPortConnection, InterfacePortConnection> kind;

  auto operator==(const PortConnection&) const -> bool = default;
};

// Some of one net's positions, as one operand of a side names them. `part` is
// the part of a net the source wrote -- the net whole, or a constant select of
// it -- and `offset` and `width` say where among that part's positions they
// start, counted from the part's lowest, and how many there are. A select's
// index may be a value each construction is given, so the part is stated as
// the select rather than as the positions one construction settles it to.
//
// They are counted in positions, never in the `[msb:lsb]` the net was declared
// with: LRM 10.11 gives an overlay the bit overlay rules of a packed union, and
// LRM 7.6 makes whole-value correspondence positional rather than
// range-relative.
struct NetPositions {
  ExprId part;
  std::uint32_t offset{};
  std::uint32_t width{};

  auto operator==(const NetPositions&) const -> bool = default;
};

// One side of a join: the positions each of its operands names, most
// significant first. A side naming one whole net has one operand, and a
// concatenation names the positions of each of its operands (LRM 10.11
// `net_lvalue`).
using NetSide = std::vector<NetPositions>;

// The sides one construct states are the same physical nets (LRM 23.3.3.7,
// 10.11), each named once, in the order the source lists them. Every side
// covers the same number of positions, and the sides are laid over each other
// position-wise from the most significant end, so which positions of one net
// meet which of another follows from the sides.
//
// Nothing is driven, read, or waited on: a bidirectional connection (LRM
// 23.3.3), whose two sides are the actual and the port, and an `alias` (LRM
// 10.11), which lists two sides or more, both state which positions resolve
// together and state no direction, so one construct spelling covers both.
struct NetJoin {
  diag::SourceSpan span;
  std::vector<NetSide> sides;

  auto operator==(const NetJoin&) const -> bool = default;
};

// One block that no loop and no conditional produced (LRM 27.3), built once.
struct SingleBlock {
  auto operator==(const SingleBlock&) const -> bool = default;
};

// Each scope stands for one block of a loop that did not survive elaboration as
// a loop a construction can run, and is built once.
//
// `indices` are the values the index stood at, one per scope in the order the
// scopes are listed, which is the order the loop counted them out. The index
// and the source label together are each block's hierarchy segment (LRM 27.4).
struct BlocksStandAlone {
  std::vector<std::int64_t> indices;

  auto operator==(const BlocksStandAlone&) const -> bool = default;
};

// A body is built once at every index the loop counts out (LRM 27.4). What
// makes one body serve many indices is that the index reaches the block as a
// value construction supplies rather than as a constant folded into its body;
// blocks of different classes and blocks of one class that still lower apart
// are distinct bodies, and `taken` says which body the block at each index is,
// one entry per index in the order the loop counts them out. Blocks that all
// lower alike are one body every index takes.
//
// `variable` is the loop's index, declared by the scope holding the generate,
// and the three expressions are the loop's own: where the index starts,
// whether a block stands at it, and how it reaches the next one. The first two
// are values the loop reads; the step is written for its effect on the index
// and its own value is discarded, the same way a loop written among statements
// states its step.
struct BlocksRepeat {
  StructuralDataObjectId variable;
  ExprId initial;
  ExprId condition;
  ExprId step;
  std::vector<std::uint32_t> taken;

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

struct GenerateBlock;

// The lowered form of every generate construct (LRM 27). A block's position in
// `blocks` is its identity, so nothing restates which block a scope is, and
// how many objects a scope stands for is what `counting` says.
struct Generate {
  base::Arena<GenerateBlock, StructuralScopeId> blocks;
  std::variant<SingleBlock, BlocksStandAlone, BlocksRepeat, BlocksChoose>
      counting = SingleBlock{};

  auto operator==(const Generate&) const -> bool = default;
};

// Which compiled scope a path naming one of a generate's blocks reaches. Every
// consumer of a path asks this and nothing else about a generate, so the
// answer is stated once here rather than re-derived at each of them.
//
// How a path names a block and how many scopes the construct compiled to are
// settled independently, so the two are paired here. A loop a construction
// runs compiles the block at each position to the body that position takes,
// and one that did not survive elaboration compiles each block to its own, so
// `selects` -- the block the path picked -- names the position either way. A
// pairing the language does not have is a name that was resolved against a
// different construct than the one it reached.
[[nodiscard]] inline auto LoopBlockScopeOf(
    const Generate& gen, std::span<const std::uint32_t> selects)
    -> StructuralScopeId {
  const auto not_a_loop = [] -> StructuralScopeId {
    throw InternalError(
        "hir::LoopBlockScopeOf: a construct that counts out no blocks is "
        "named as a loop");
  };
  const auto position = [&] -> std::uint32_t {
    if (selects.size() != 1) {
      throw InternalError(
          "hir::LoopBlockScopeOf: a block of a loop is reached by one select");
    }
    return selects.front();
  };
  return std::visit(
      Overloaded{
          [&](const SingleBlock&) { return not_a_loop(); },
          [&](const BlocksChoose&) { return not_a_loop(); },
          [&](const BlocksStandAlone&) {
            return StructuralScopeId{position()};
          },
          [&](const BlocksRepeat& repeat) {
            return StructuralScopeId{repeat.taken.at(position())};
          },
      },
      gen.counting);
}

// The same for the one block a construct building at most one holds: a block
// standing on its own is its one scope, and a conditional compiles each
// alternative it holds to one.
[[nodiscard]] inline auto ChosenBlockScopeOf(
    const Generate& gen, std::uint32_t alternative) -> StructuralScopeId {
  const auto a_loop = [] -> StructuralScopeId {
    throw InternalError(
        "hir::ChosenBlockScopeOf: a loop's blocks are named by a select, not "
        "as an alternative");
  };
  return std::visit(
      Overloaded{
          [](const SingleBlock&) { return StructuralScopeId{0}; },
          [&](const BlocksStandAlone&) { return a_loop(); },
          [&](const BlocksRepeat&) { return a_loop(); },
          [&](const BlocksChoose& chosen) -> StructuralScopeId {
            if (alternative >= chosen.alternatives.size() ||
                !chosen.alternatives[alternative].has_value()) {
              throw InternalError(
                  "hir::ChosenBlockScopeOf: a name reached an alternative no "
                  "elaboration of the construct selected");
            }
            return *chosen.alternatives[alternative];
          },
      },
      gen.counting);
}

struct StructuralScope {
  // The label of a generate child as a hierarchical name writes it (LRM 23.6,
  // 27.6), escaped where such a name has to escape it; empty for a block the
  // source gave no label and for other scopes.
  std::string source_name;
  TimeResolution time_resolution;
  base::Registry<StructuralDataObjectDecl, StructuralDataObjectId>
      structural_data_objects;
  // What this scope published. Every scope a name can step into publishes --
  // a unit's instance and each generate block inside it (LRM 23.6) -- and a
  // namespace unit's root scope, which no name steps into, publishes nothing.
  ScopePublication published;
  base::Arena<Expr, ExprId> exprs;
  base::Arena<Pattern, PatternId> patterns;
  base::Registry<Process, ProcessId> processes;
  base::Arena<ContinuousAssign, ContinuousAssignId> continuous_assigns;
  base::Registry<Generate, GenerateId> generates;
  base::Registry<InstanceMemberDecl, InstanceMemberId> instance_members;
  base::Arena<InterfacePortDecl, InterfacePortId> interface_ports;
  base::Arena<PortConnection, PortConnectionId> port_connections;
  // The positions of nets this scope's constructs place in one resolution. A
  // plain list rather than an arena because nothing names one: no reference
  // reaches a join, and what it states is already complete when it is recorded.
  std::vector<NetJoin> net_joins;
  ScopeRoutes routes;
  // The cells something in this scope reads a sampled value of (LRM 16.5.1),
  // each named the way an event control names the cell it watches -- so one
  // reached across an instance boundary is carried by its route like any other.
  // A cell answers for a sampled value only once armed, and arming installs the
  // value every read answers with until a later time slot first changes it, so
  // it happens once every variable initializer in the design has run. What is
  // armed is the whole cell, whatever part of it a read selects.
  std::vector<ValueTarget> sampled_cells;
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
  // The classes that are types of this scope's instance: those it declares (LRM
  // 23.9 lists a class among the elements that define a scope, and LRM 6.22
  // makes one declared here a type of the instance), and the specializations of
  // a generic on such a type (LRM 8.25). An object of one belongs to the one
  // instance it was created in and its bodies reach that instance; stating the
  // relation on the scope is what lets the instance be supplied where it is
  // known. Every class a unit holds is named by exactly one scope, the
  // namespace unit's root scope included, so nothing is reached by elimination.
  std::vector<ClassId> replicated_classes;
  std::vector<ForeignExportDecl> foreign_exports;
  // Every scope's identity is minted before any body is lowered, so a `disable`
  // naming one (LRM 9.6.2) -- possibly from another process lowered first --
  // carries a stable typed id rather than a name it would have to resolve
  // later. That is what the declare-then-define gap buys: the id exists up
  // front and the body pass fills the contents when it reaches the scope.
  base::Registry<ProceduralScopeDecl, ProceduralScopeId> procedural_scopes;

  // Two scopes are equal when everything they state is equal. Derived rather
  // than written. A field added to any node below is compared
  // without anyone remembering to, and a field that cannot be compared breaks
  // the build rather than being silently left out -- which is the direction
  // this has to fail in, because a comparison that misses something answers
  // "the same" about two things that are not.
  auto operator==(const StructuralScope&) const -> bool = default;
};

// One block of a generate, and the arguments its construction passes the
// block's constructor: one expression per value the block receives, in the
// order it receives them. A loop's block receives its index (LRM 27.4); a block
// has no parameter ports (Syntax 27-1), so any other block receives nothing.
// The expressions belong to the scope holding the generate, because that
// scope's construction evaluates them, the way an instance's are.
struct GenerateBlock {
  StructuralScope scope;
  std::vector<ExprId> arguments;

  auto operator==(const GenerateBlock&) const -> bool = default;
};

}  // namespace lyra::hir
