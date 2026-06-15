#pragma once

#include <string>
#include <vector>

#include "lyra/base/time.hpp"
#include "lyra/mir/process.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/structural_param.hpp"
#include "lyra/mir/structural_scope_id.hpp"
#include "lyra/mir/structural_subroutine.hpp"
#include "lyra/mir/structural_var.hpp"
#include "lyra/mir/type_alias.hpp"

namespace lyra::mir {

struct StructuralScope {
  std::string name;
  // The scope's resolved time unit and precision (LRM 3.14.2). The emitted
  // class exposes the precision so the engine can take the design-global
  // minimum (LRM 3.14.3) and so delays scale to it.
  TimeResolution time_resolution;
  std::vector<StructuralParamDecl> structural_params;
  std::vector<StructuralVarDecl> structural_vars;
  ProceduralScope constructor_scope;
  std::vector<Process> processes;
  std::vector<StructuralScope> child_structural_scopes;
  std::vector<StructuralSubroutineDecl> structural_subroutines;
  std::vector<TypeAliasDecl> type_aliases;

  [[nodiscard]] auto GetStructuralParam(StructuralParamId id) const
      -> const StructuralParamDecl& {
    return structural_params.at(id.value);
  }
  [[nodiscard]] auto GetStructuralVar(StructuralVarId id) const
      -> const StructuralVarDecl& {
    return structural_vars.at(id.value);
  }
  [[nodiscard]] auto GetProcess(ProcessId id) const -> const Process& {
    return processes.at(id.value);
  }
  [[nodiscard]] auto GetChildStructuralScope(StructuralScopeId id) const
      -> const StructuralScope& {
    return child_structural_scopes.at(id.value);
  }
  [[nodiscard]] auto GetStructuralSubroutine(StructuralSubroutineId id) const
      -> const StructuralSubroutineDecl& {
    return structural_subroutines.at(id.value);
  }

  auto AddStructuralParam(StructuralParamDecl decl) -> StructuralParamId {
    const StructuralParamId id{
        static_cast<std::uint32_t>(structural_params.size())};
    structural_params.push_back(std::move(decl));
    return id;
  }
  auto AddStructuralVar(StructuralVarDecl decl) -> StructuralVarId {
    const StructuralVarId id{
        static_cast<std::uint32_t>(structural_vars.size())};
    structural_vars.push_back(std::move(decl));
    return id;
  }
  auto AddProcess(Process process) -> ProcessId {
    const ProcessId id{static_cast<std::uint32_t>(processes.size())};
    processes.push_back(std::move(process));
    return id;
  }
  auto AddChildStructuralScope(StructuralScope child) -> StructuralScopeId {
    const StructuralScopeId id{
        static_cast<std::uint32_t>(child_structural_scopes.size())};
    child_structural_scopes.push_back(std::move(child));
    return id;
  }
  auto AddStructuralSubroutine(StructuralSubroutineDecl decl)
      -> StructuralSubroutineId {
    const StructuralSubroutineId id{
        static_cast<std::uint32_t>(structural_subroutines.size())};
    structural_subroutines.push_back(std::move(decl));
    return id;
  }
  void AddTypeAlias(TypeAliasDecl decl) {
    type_aliases.push_back(std::move(decl));
  }
};

}  // namespace lyra::mir
