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
#include "lyra/hir/loop_var.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/structural_hops.hpp"
#include "lyra/hir/structural_var.hpp"
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

// One step of a cross-unit reference's downward navigation past the head: a
// named member (`->name`) or an instance-array index (`[index]`). A dotted path
// is a sequence of these; `m.l.x` is two member hops, `c[1].x` is an index hop
// then a member hop.
struct MemberHop {
  std::string name;
};
struct IndexHop {
  std::uint32_t index;
};
using PathStep = std::variant<MemberHop, IndexHop>;

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
// A downward head can sit in the current scope (`hops == 0`) or in an
// enclosing scope (`hops > 0`). The enclosing case covers references that
// reach an owned child of an ancestor scope without leaving the compilation
// unit -- e.g. one generate block reading a signal in a sibling generate
// block of their common ancestor. `child` identifies the head as one of
// the owning scope's HIR-level identities; the install resolves it through
// the enclosing class's owned-child binding table.
struct DownwardHead {
  StructuralHops hops = {};
  std::variant<InstanceMemberId, GenerateChildRef> child;
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

using CrossUnitRefHead =
    std::variant<DownwardHead, UpwardRootHead, UpwardNamedHead>;

// A cross-unit reference resolved once at construction. `head` is where
// navigation starts; `path` carries the shared navigation from the head down to
// the referenced leaf, by name across the unit boundary; `type` is the
// slang-resolved leaf type. The slot is read / written / observed through one
// stored direct reference.
struct CrossUnitRefDecl {
  CrossUnitRefHead head;
  std::vector<PathStep> path;
  TypeId type;
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
// its own cell and is realized as a reactive edge that reads it during
// simulation, so it holds a persistent cross-unit reference (`cell`, a
// `CrossUnitVarRef`). A `ref` port owns no cell; it is bound once in the
// resolve phase, so it holds only the by-name reach (`head` + `path`, with
// `type` the port value type) -- no persistent slot.
struct PortCellEndpoint {
  ExprId cell;
};
struct PortAliasEndpoint {
  CrossUnitRefHead head;
  std::vector<PathStep> path;
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

struct IfGenerate {
  ExprId condition;
  StructuralScopeId then_scope;
  std::optional<StructuralScopeId> else_scope;
};

struct CaseGenerateItem {
  std::vector<ExprId> labels;
  StructuralScopeId scope;
};

struct CaseGenerate {
  ExprId condition;
  std::vector<CaseGenerateItem> items;
  std::optional<StructuralScopeId> default_scope;
};

struct LoopGenerate {
  LoopVarDeclId loop_var;
  ExprId initial;
  ExprId stop;
  ExprId iter;
  StructuralScopeId scope;
};

using GenerateData = std::variant<IfGenerate, CaseGenerate, LoopGenerate>;

struct Generate {
  GenerateData data;
  base::Arena<StructuralScope, StructuralScopeId> child_scopes;
};

struct StructuralScope {
  StructuralScopeId id{};
  // LRM source name of a generate child (label, or `genblk<n>` when unnamed,
  // LRM 27.6); empty for other scopes.
  std::string source_name;
  TimeResolution time_resolution;
  base::Arena<StructuralVarDecl, StructuralVarId> structural_vars;
  base::Arena<LoopVarDecl, LoopVarDeclId> loop_var_decls;
  base::Arena<Expr, ExprId> exprs;
  base::Arena<Process, ProcessId> processes;
  base::Arena<ContinuousAssign, ContinuousAssignId> continuous_assigns;
  base::Arena<Generate, GenerateId> generates;
  base::Arena<InstanceMemberDecl, InstanceMemberId> instance_members;
  std::vector<PortConnection> port_connections;
  base::Arena<CrossUnitRefDecl, CrossUnitRefId> cross_unit_refs;
  base::Arena<StructuralSubroutineDecl, StructuralSubroutineId>
      structural_subroutines;
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
