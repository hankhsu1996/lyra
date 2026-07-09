#pragma once

#include <compare>
#include <cstdint>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/base/time.hpp"
#include "lyra/hir/continuous_assign.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/foreign_import.hpp"
#include "lyra/hir/foreign_import_id.hpp"
#include "lyra/hir/procedural_scope.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/structural_data_object.hpp"
#include "lyra/hir/structural_hops.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/hir/type_alias.hpp"
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

// One segment of a cross-unit reference's downward navigation past the head:
// a named member (`.name`) with the per-dimension indices selecting an array
// element on that name (`[i][j]`), if any. `m.l.x` is three segments each
// with empty indices; `c[1].x` is `[{c, [1]}, {x, []}]`. Indices are always
// attached to the segment they select on -- there is no orphan index step
// separable from its name.
struct PathSegment {
  std::string name;
  std::vector<std::uint32_t> indices;

  auto operator==(const PathSegment&) const -> bool = default;
};

// Where a cross-unit reference starts its navigation. A downward reference
// reaches into an owned child the referrer's own scope declares -- an
// instance/instance-array member, or a generate block (LRM 27) identified by
// its position in the scope. An upward reference (LRM 23.6) locates its
// starting scope at construction by climbing the enclosing chain. The child
// cannot know its depth at compile time, so it locates the scope by name (or
// as the `$root` anchor) rather than a baked-in offset.
struct GenerateChildRef {
  GenerateId generate;
  StructuralScopeId scope;
};
// A named procedural block head (LRM 9.3.5 / 23.9), identified by its SV
// label. A named block is a layout-visible structural child of its enclosing
// scope; the label is its stable declaration-time identity, not a by-name
// runtime lookup. The label is meaningful only relative to the `DownwardHead`
// owner reached by `hops` -- the enclosing structural scope directly
// containing the block, where sibling block labels are unique (LRM 23.9) -- and
// is not a unit-global name. HIR-to-MIR resolves it against that owner's
// materialized scopes to a typed companion handle, so the realization is a
// typed segment.
struct NamedBlockRef {
  std::string label;

  auto operator==(const NamedBlockRef&) const -> bool = default;
};
// A downward head can sit in the current scope (`hops == 0`) or in an
// enclosing scope (`hops > 0`). The enclosing case covers references that
// reach an owned child of an ancestor scope without leaving the compilation
// unit -- e.g. one generate block reading a signal in a sibling generate
// block of their common ancestor. `child` identifies the head as one of
// the owning scope's HIR-level identities; the install resolves it through
// the enclosing class's owned-child binding table. `head_indices` selects an
// element when the head is an instance / generate array; empty for a scalar
// head or a named-block head.
struct DownwardHead {
  StructuralHops hops = {};
  std::variant<InstanceMemberId, GenerateChildRef, NamedBlockRef> child;
  std::vector<std::uint32_t> head_indices = {};
};

// The head of a routed reference whose target is a data-object member of an
// enclosing scope of the reader, in the same compilation unit (`hops` typed
// parent edges up, then the leaf member). Reaching a same-unit ancestor member
// this way -- sealed once in the resolve phase like every other routed
// reference -- rather than re-walking the parent chain on each access is what
// keeps the hot path a single sealed-endpoint dereference. The leaf member is
// the route's single `PathSegment`; the climb is entirely typed.
struct EnclosingHead {
  StructuralHops hops;

  auto operator==(const EnclosingHead&) const -> bool = default;
};

// Where an upward cross-unit reference's navigation starts (LRM 23.8 / 23.9).
// `UpwardRootHead` denotes the parent-less topmost scope -- the `$root`
// source token identifies the climb target itself, so the descent suffix
// starts past it. `UpwardNamedHead` carries the canonical structural identity
// of the head -- its instance / generate-block name plus any per-dimension
// index. In both cases the descent suffix is strictly below the anchor.
struct UpwardRootHead {
  auto operator==(const UpwardRootHead&) const -> bool = default;
};

struct UpwardNamedHead {
  std::string head_name;
  std::vector<std::uint32_t> head_indices;

  auto operator==(const UpwardNamedHead&) const -> bool = default;
};

using RoutedRefHead =
    std::variant<EnclosingHead, DownwardHead, UpwardRootHead, UpwardNamedHead>;

// A routed reference resolved once, in the resolve phase after the object tree
// is fully built. `head` is where navigation starts -- an enclosing ancestor,
// an owned child, or an upward climb; `path` carries the navigation from the
// head down to the referenced leaf, one segment per named step (each with its
// own per-axis indices, if any); `type` is the slang-resolved leaf data type.
// `target_net_type` is the target's net type when the target is a net (LRM 6.7:
// the net type fixes how drivers resolve and the undriven value), or empty when
// the target is a variable. Together they determine the producer's actual cell
// -- a resolved net of that net type, or a variable's observable cell -- which
// the realized endpoint must match so a read reaches the right access protocol.
// The endpoint is read / written / observed through one stored direct
// reference.
struct RoutedRefDecl {
  RoutedRefHead head;
  std::vector<PathSegment> path;
  TypeId type;
  std::optional<NetType> target_net_type;
};

// `target_unit` is a cross-unit reference -- the name of the instantiated unit,
// resolved by name at link time, a distinct domain from the unit-local id
// kinds. `array_dims` is empty for a scalar instance and holds one element
// count per dimension, outermost first, for an instance array (`Child c[2][3]`
// is `{2, 3}`).
struct InstanceMemberDecl {
  std::string instance_name;
  std::string target_unit;
  std::vector<std::uint32_t> array_dims;
};

enum class PortDirection : std::uint8_t { kInput, kOutput, kRef };

// How the child port is reached, by realization. An input or output port has
// its own cell and is realized as a reactive edge over it (a variable cell is
// written / read, a net cell is driven / read), so it holds a persistent routed
// reference (`cell`, a `RoutedRef`) whose target capability (net versus
// variable) the reference itself carries. A `ref` port owns no cell; it is
// bound once in the resolve phase, so it holds only the by-name reach (`head` +
// `path`, with `type` the port value type) -- no persistent slot.
struct PortCellEndpoint {
  ExprId cell;
};
struct PortAliasEndpoint {
  RoutedRefHead head;
  std::vector<PathSegment> path;
  TypeId type;
};
using PortEndpoint = std::variant<PortCellEndpoint, PortAliasEndpoint>;

// A module port connection at an instantiation (LRM 23.3.3). `endpoint` reaches
// the child's port member; `peer` is the parent-side connected expression;
// `direction` is the port direction; `sensitivity` is the read set the implied
// continuous assignment waits on (the peer's reads for an input port, the child
// port for an output port; empty for a `ref` port). HIR holds the connection
// verbatim and HIR-to-MIR realizes it: an input or output port as the implied
// continuous assignment between the two cells (LRM 23.3.3), a `ref` port as an
// alias bind of the child's reference member to the peer's cell, performed in
// the resolve phase (LRM 23.3.3.2).
struct PortConnection {
  diag::SourceSpan span;
  PortDirection direction;
  PortEndpoint endpoint;
  ExprId peer;
  std::vector<SensitivityEntry> sensitivity;
};

// The lowered form of every generate construct (LRM 27): after frontend
// elaboration each construct is a set of blocks with an instantiated / not
// flag, so the lowering is one fully concrete scope per instantiated block,
// constructed unconditionally. A loop iteration carries its hierarchy index; an
// `if` / `case` arm or a bare block carries none. Each scope is lowered from
// its own elaborated body -- its own selected arm, types, and slice widths, the
// genvar folded to a constant -- never borrowed from another block and never a
// runtime induction value or branch.
struct ResolvedGenerateItem {
  std::optional<std::int64_t> index;
  StructuralScopeId scope{};
};
struct ResolvedGenerate {
  std::vector<ResolvedGenerateItem> items;
};

struct Generate {
  ResolvedGenerate data;
  base::Arena<StructuralScope, StructuralScopeId> child_scopes;
};

struct StructuralScope {
  StructuralScopeId id{};
  // LRM source name of a generate child (label, or `genblk<n>` when unnamed,
  // LRM 27.6); empty for other scopes.
  std::string source_name;
  TimeResolution time_resolution;
  base::Arena<StructuralDataObjectDecl, StructuralDataObjectId>
      structural_data_objects;
  base::Arena<Expr, ExprId> exprs;
  base::Arena<Process, ProcessId> processes;
  base::Arena<ContinuousAssign, ContinuousAssignId> continuous_assigns;
  base::Arena<Generate, GenerateId> generates;
  base::Arena<InstanceMemberDecl, InstanceMemberId> instance_members;
  std::vector<PortConnection> port_connections;
  base::Arena<RoutedRefDecl, RoutedRefId> routed_refs;
  // Body-bearing SV subroutines only. A bodyless DPI-C import never enters this
  // arena; it lives in `foreign_imports` (invariant relied on by the
  // per-subroutine body lowering, which assumes every entry here has a body).
  base::Arena<SubroutineDecl, StructuralSubroutineId> structural_subroutines;
  base::Arena<ForeignImportDecl, ForeignImportId> foreign_imports;
  base::Arena<ProceduralScopeDecl, ProceduralScopeId> procedural_scopes;
  std::vector<TypeAliasDecl> type_aliases;

  [[nodiscard]] auto NextGenerateId() const -> GenerateId {
    return GenerateId{static_cast<std::uint32_t>(generates.size())};
  }
  [[nodiscard]] auto NextStructuralSubroutineId() const
      -> StructuralSubroutineId {
    return StructuralSubroutineId{
        static_cast<std::uint32_t>(structural_subroutines.size())};
  }
  void AddTypeAlias(TypeAliasDecl decl) {
    type_aliases.push_back(std::move(decl));
  }
};

}  // namespace lyra::hir
