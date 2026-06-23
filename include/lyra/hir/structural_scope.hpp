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
// its position in the scope. Both resolve to one owned-child companion var at
// construction, so the navigation past the head is identical. An upward
// reference (LRM 23.6) climbs the parent chain at construction to the nearest
// matching ancestor named `ancestor_name`; from there both directions share
// `path` to reach the leaf, by name across the unit boundary -- the referrer
// never names the ancestor's unit type, which it does not depend on
// (docs/architecture/emission_model.md). The child cannot know its depth at
// compile time, so it locates the ancestor by name rather than a baked-in
// offset.
struct GenerateChildRef {
  GenerateId generate;
  StructuralScopeId scope;
};
struct DownwardHead {
  std::variant<InstanceMemberId, GenerateChildRef> child;
};

// How an upward reference's head identifies its ancestor on the parent chain.
// A module-instance head matches the ancestor's module definition name (LRM
// 23.8): the key is class-level, so one artifact serves every depth, and an
// ancestor whose instance name merely equals it does not match. A named
// generate block or a `$root`-anchored path instead matches the ancestor
// scope's own name -- the generate-block label or `$root` -- because that name
// is itself the lookup key (LRM 23.6, 23.8). The two keys compare against
// different scopes at runtime, so the axis rides the head.
enum class UpwardMatch : std::uint8_t { kDefName, kScopeName };

struct UpwardHead {
  std::string ancestor_name;
  UpwardMatch match;
};
using CrossUnitRefHead = std::variant<DownwardHead, UpwardHead>;

// A cross-unit reference resolved once at construction (see
// reference_resolution.md). `head` is where navigation starts; `path` carries
// the shared navigation from the head down to the referenced leaf, by name
// across the unit boundary; `type` is the slang-resolved leaf type. The slot is
// read / written / observed through one stored direct reference.
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
