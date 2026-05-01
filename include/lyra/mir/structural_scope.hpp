#pragma once

#include <string>
#include <vector>

#include "lyra/mir/process.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/structural_param.hpp"
#include "lyra/mir/structural_scope_id.hpp"
#include "lyra/mir/structural_subroutine.hpp"
#include "lyra/mir/structural_var.hpp"

namespace lyra::mir {

struct StructuralScope {
  std::string name;
  std::vector<StructuralParamDecl> structural_params;
  std::vector<StructuralVarDecl> structural_vars;
  ProceduralScope constructor_scope;
  std::vector<Process> processes;
  std::vector<StructuralScope> child_structural_scopes;
  std::vector<StructuralSubroutineDecl> structural_subroutines;

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
};

}  // namespace lyra::mir
