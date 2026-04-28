#pragma once

#include <string>
#include <vector>

#include "lyra/mir/class_decl_id.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/member_var.hpp"
#include "lyra/mir/process.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::mir {

struct ClassDecl {
  std::string name;
  std::vector<MemberVar> member_vars;
  Body constructor;
  std::vector<Process> processes;
  std::vector<ClassDecl> classes;
  std::vector<UserSubroutineTarget> user_subroutine_targets;

  [[nodiscard]] auto GetMemberVar(MemberVarId id) const -> const MemberVar& {
    return member_vars.at(id.value);
  }
  [[nodiscard]] auto GetProcess(ProcessId id) const -> const Process& {
    return processes.at(id.value);
  }
  [[nodiscard]] auto GetClass(ClassDeclId id) const -> const ClassDecl& {
    return classes.at(id.value);
  }
  [[nodiscard]] auto GetUserSubroutineTarget(UserSubroutineTargetId id) const
      -> const UserSubroutineTarget& {
    return user_subroutine_targets.at(id.value);
  }
};

}  // namespace lyra::mir
