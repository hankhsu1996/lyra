#pragma once

#include <string>
#include <vector>

#include "lyra/mir/class_decl_id.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/member_var.hpp"
#include "lyra/mir/process.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::mir {

class ClassDecl {
 public:
  explicit ClassDecl(std::string name);
  ~ClassDecl();
  ClassDecl(ClassDecl&&) noexcept;
  auto operator=(ClassDecl&&) noexcept -> ClassDecl&;
  ClassDecl(const ClassDecl&) = delete;
  auto operator=(const ClassDecl&) -> ClassDecl& = delete;

  [[nodiscard]] auto Name() const -> const std::string&;

  [[nodiscard]] auto MemberVars() const -> const std::vector<MemberVar>&;
  [[nodiscard]] auto GetMemberVar(MemberVarId id) const -> const MemberVar&;
  auto AddMemberVar(MemberVar member) -> MemberVarId;

  [[nodiscard]] auto Constructor() const -> const Body&;
  auto Constructor() -> Body&;

  [[nodiscard]] auto Processes() const -> const std::vector<Process>&;
  [[nodiscard]] auto GetProcess(ProcessId id) const -> const Process&;
  auto AddProcess(Process process) -> ProcessId;

  [[nodiscard]] auto Classes() const -> const std::vector<ClassDecl>&;
  [[nodiscard]] auto GetClass(ClassDeclId id) const -> const ClassDecl&;
  auto AddClass(ClassDecl child) -> ClassDeclId;

  [[nodiscard]] auto UserSubroutineTargets() const
      -> const std::vector<UserSubroutineTarget>&;
  [[nodiscard]] auto GetUserSubroutineTarget(UserSubroutineTargetId id) const
      -> const UserSubroutineTarget&;
  auto AddUserSubroutineTarget(UserSubroutineTarget target)
      -> UserSubroutineTargetId;

 private:
  std::string name_;
  std::vector<MemberVar> member_vars_;
  Body constructor_;
  std::vector<Process> processes_;
  std::vector<ClassDecl> classes_;
  std::vector<UserSubroutineTarget> user_subroutine_targets_;
};

}  // namespace lyra::mir
