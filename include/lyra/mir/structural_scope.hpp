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
#include "lyra/mir/type_id.hpp"
#include "lyra/mir/value_ref.hpp"

namespace lyra::mir {

// A cross-unit reference resolved once at construction. `instance_var` is the
// structural var holding the owned child instance -- the head of the downward
// path; `member_path` is the chain of member names from the head's child down
// to the referenced leaf (`c.x` is `{"x"}`; `m.l.x` is `{"l", "x"}`); `type` is
// the referenced leaf's type. The backend stores a direct reference to
// `instance_var->...->leaf` by chaining `->` over the path.
struct CrossUnitRefDecl {
  StructuralVarId instance_var;
  std::vector<std::string> member_path;
  TypeId type;
};

struct StructuralScope {
  std::string name;
  // The scope's resolved time unit and precision (LRM 3.14.2). The emitted
  // class exposes the precision so the engine can take the design-global
  // minimum (LRM 3.14.3) and so delays scale to it.
  TimeResolution time_resolution;
  std::vector<StructuralParamDecl> structural_params;
  std::vector<StructuralVarDecl> structural_vars;
  std::vector<CrossUnitRefDecl> cross_unit_refs;
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
  [[nodiscard]] auto GetCrossUnitRef(CrossUnitRefId id) const
      -> const CrossUnitRefDecl& {
    return cross_unit_refs.at(id.value);
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
