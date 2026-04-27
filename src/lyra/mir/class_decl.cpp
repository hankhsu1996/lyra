#include "lyra/mir/class_decl.hpp"

#include <cstdint>
#include <string>
#include <utility>
#include <vector>

#include "lyra/mir/member_var.hpp"
#include "lyra/mir/process.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::mir {

ClassDecl::ClassDecl(std::string name) : name_(std::move(name)) {
}

ClassDecl::~ClassDecl() = default;
ClassDecl::ClassDecl(ClassDecl&&) noexcept = default;
auto ClassDecl::operator=(ClassDecl&&) noexcept -> ClassDecl& = default;

auto ClassDecl::Name() const -> const std::string& {
  return name_;
}

auto ClassDecl::MemberVars() const -> const std::vector<MemberVar>& {
  return member_vars_;
}

auto ClassDecl::GetMemberVar(MemberVarId id) const -> const MemberVar& {
  return member_vars_.at(id.value);
}

auto ClassDecl::AddMemberVar(std::string name, MemberKind kind) -> MemberVarId {
  const MemberVarId id{static_cast<std::uint32_t>(member_vars_.size())};
  member_vars_.push_back(
      MemberVar{.name = std::move(name), .kind = std::move(kind)});
  return id;
}

auto ClassDecl::Constructor() const -> const Body& {
  return constructor_;
}

auto ClassDecl::Constructor() -> Body& {
  return constructor_;
}

auto ClassDecl::Processes() const -> const std::vector<Process>& {
  return processes_;
}

auto ClassDecl::GetProcess(ProcessId id) const -> const Process& {
  return processes_.at(id.value);
}

auto ClassDecl::AddProcess(Process process) -> ProcessId {
  const ProcessId id{static_cast<std::uint32_t>(processes_.size())};
  processes_.push_back(std::move(process));
  return id;
}

auto ClassDecl::Classes() const -> const std::vector<ClassDecl>& {
  return classes_;
}

auto ClassDecl::GetClass(ClassDeclId id) const -> const ClassDecl& {
  return classes_.at(id.value);
}

auto ClassDecl::AddClass(ClassDecl child) -> ClassDeclId {
  const ClassDeclId id{static_cast<std::uint32_t>(classes_.size())};
  classes_.push_back(std::move(child));
  return id;
}

auto ClassDecl::UserSubroutineTargets() const
    -> const std::vector<UserSubroutineTarget>& {
  return user_subroutine_targets_;
}

auto ClassDecl::GetUserSubroutineTarget(UserSubroutineTargetId id) const
    -> const UserSubroutineTarget& {
  return user_subroutine_targets_.at(id.value);
}

auto ClassDecl::AddUserSubroutineTarget(UserSubroutineTarget target)
    -> UserSubroutineTargetId {
  const UserSubroutineTargetId id{
      static_cast<std::uint32_t>(user_subroutine_targets_.size())};
  user_subroutine_targets_.push_back(std::move(target));
  return id;
}

}  // namespace lyra::mir
