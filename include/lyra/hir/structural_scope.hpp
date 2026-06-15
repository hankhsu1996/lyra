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

// Where a cross-unit reference starts its navigation. A downward reference
// reaches into an owned child the referrer's own scope declares -- an
// instance/instance-array member, or a generate block (LRM 27) identified by
// its position in the scope. Both resolve to one owned-child companion var at
// construction, so the navigation past the head is identical. An upward
// reference (LRM 23.6) climbs the parent chain at construction to the nearest
// ancestor whose instance or module name is `ancestor_name`; from there both
// directions share `path` to reach the leaf, by name across the unit boundary
// -- the referrer never names the ancestor's unit type, which it does not
// depend on (docs/architecture/emission_model.md). The child cannot know its
// depth at compile time, so it locates the ancestor by name rather than a
// baked-in offset.
struct GenerateChildRef {
  GenerateId generate;
  StructuralScopeId scope;
};
struct DownwardHead {
  std::variant<InstanceMemberId, GenerateChildRef> child;
};
struct UpwardHead {
  std::string ancestor_name;
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
  std::vector<StructuralScope> child_scopes;

  [[nodiscard]] auto GetChildScope(StructuralScopeId id) const
      -> const StructuralScope&;
};

struct StructuralScope {
  StructuralScopeId id{};
  // LRM source name of a generate child (label, or `genblk<n>` when unnamed,
  // LRM 27.6); empty for other scopes.
  std::string source_name;
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

  auto AddStructuralVar(StructuralVarDecl decl) -> StructuralVarId {
    const StructuralVarId id{
        static_cast<std::uint32_t>(structural_vars.size())};
    structural_vars.push_back(std::move(decl));
    return id;
  }
  auto AddLoopVarDecl(LoopVarDecl decl) -> LoopVarDeclId {
    const LoopVarDeclId id{static_cast<std::uint32_t>(loop_var_decls.size())};
    loop_var_decls.push_back(std::move(decl));
    return id;
  }
  auto AddExpr(Expr expr) -> ExprId {
    const ExprId id{static_cast<std::uint32_t>(exprs.size())};
    exprs.push_back(std::move(expr));
    return id;
  }
  auto AddProcess(Process process) -> ProcessId {
    const ProcessId id{static_cast<std::uint32_t>(processes.size())};
    processes.push_back(std::move(process));
    return id;
  }
  auto AddContinuousAssign(ContinuousAssign ca) -> ContinuousAssignId {
    const ContinuousAssignId id{
        static_cast<std::uint32_t>(continuous_assigns.size())};
    continuous_assigns.push_back(std::move(ca));
    return id;
  }
  auto AddGenerate(Generate generate) -> GenerateId {
    const GenerateId id{static_cast<std::uint32_t>(generates.size())};
    generates.push_back(std::move(generate));
    return id;
  }
  [[nodiscard]] auto NextGenerateId() const -> GenerateId {
    return GenerateId{static_cast<std::uint32_t>(generates.size())};
  }
  auto AddInstanceMember(InstanceMemberDecl decl) -> InstanceMemberId {
    const InstanceMemberId id{
        static_cast<std::uint32_t>(instance_members.size())};
    instance_members.push_back(std::move(decl));
    return id;
  }
  auto AddCrossUnitRef(CrossUnitRefDecl decl) -> CrossUnitRefId {
    const CrossUnitRefId id{static_cast<std::uint32_t>(cross_unit_refs.size())};
    cross_unit_refs.push_back(std::move(decl));
    return id;
  }
  auto AddStructuralSubroutine(StructuralSubroutineDecl decl)
      -> StructuralSubroutineId {
    const StructuralSubroutineId id{
        static_cast<std::uint32_t>(structural_subroutines.size())};
    structural_subroutines.push_back(std::move(decl));
    return id;
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

inline auto Generate::GetChildScope(StructuralScopeId id) const
    -> const StructuralScope& {
  return child_scopes.at(id.value);
}

}  // namespace lyra::hir
