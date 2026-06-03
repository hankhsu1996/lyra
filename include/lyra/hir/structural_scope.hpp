#pragma once

#include <compare>
#include <cstdint>
#include <optional>
#include <string>
#include <variant>
#include <vector>

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

// A cross-unit reference resolved once at construction (see
// reference_resolution.md). `instance` names the owned child instance (or
// instance-array) member this unit reaches into -- the head of the downward
// path. `path` carries the navigation past the head down to the referenced
// leaf, referenced by name across the unit boundary. `type` is the
// slang-resolved type of the referenced leaf. The slot is read / written /
// observed through a stored direct reference the constructor materializes by
// chaining the path's hops.
struct CrossUnitRefDecl {
  InstanceMemberId instance;
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
  std::vector<StructuralScope> child_scopes;

  [[nodiscard]] auto GetChildScope(StructuralScopeId id) const
      -> const StructuralScope&;
};

struct StructuralScope {
  StructuralScopeId id{};
  TimeResolution time_resolution;
  std::vector<StructuralVarDecl> structural_vars;
  std::vector<LoopVarDecl> loop_var_decls;
  std::vector<Expr> exprs;
  std::vector<Process> processes;
  std::vector<ContinuousAssign> continuous_assigns;
  std::vector<Generate> generates;
  std::vector<InstanceMemberDecl> instance_members;
  std::vector<CrossUnitRefDecl> cross_unit_refs;
  std::vector<StructuralSubroutineDecl> structural_subroutines;
  std::vector<TypeAliasDecl> type_aliases;

  [[nodiscard]] auto GetStructuralVar(StructuralVarId id) const
      -> const StructuralVarDecl& {
    return structural_vars.at(id.value);
  }
  [[nodiscard]] auto GetLoopVarDecl(LoopVarDeclId id) const
      -> const LoopVarDecl& {
    return loop_var_decls.at(id.value);
  }
  [[nodiscard]] auto GetExpr(ExprId id) const -> const Expr& {
    return exprs.at(id.value);
  }
  [[nodiscard]] auto GetProcess(ProcessId id) const -> const Process& {
    return processes.at(id.value);
  }
  [[nodiscard]] auto GetContinuousAssign(ContinuousAssignId id) const
      -> const ContinuousAssign& {
    return continuous_assigns.at(id.value);
  }
  [[nodiscard]] auto GetGenerate(GenerateId id) const -> const Generate& {
    return generates.at(id.value);
  }
  [[nodiscard]] auto GetInstanceMember(InstanceMemberId id) const
      -> const InstanceMemberDecl& {
    return instance_members.at(id.value);
  }
  [[nodiscard]] auto GetCrossUnitRef(CrossUnitRefId id) const
      -> const CrossUnitRefDecl& {
    return cross_unit_refs.at(id.value);
  }
  [[nodiscard]] auto GetStructuralSubroutine(StructuralSubroutineId id) const
      -> const StructuralSubroutineDecl& {
    return structural_subroutines.at(id.value);
  }
};

inline auto Generate::GetChildScope(StructuralScopeId id) const
    -> const StructuralScope& {
  return child_scopes.at(id.value);
}

}  // namespace lyra::hir
