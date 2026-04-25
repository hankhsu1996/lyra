#pragma once

#include <compare>
#include <cstdint>
#include <string>
#include <vector>

#include "lyra/mir/member_var.hpp"
#include "lyra/mir/process.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::mir {

struct ClassId {
  std::uint32_t value;

  auto operator<=>(const ClassId&) const -> std::strong_ordering = default;
};

class ClassDecl {
 public:
  explicit ClassDecl(std::string name);
  ~ClassDecl();
  ClassDecl(ClassDecl&&) noexcept;
  auto operator=(ClassDecl&&) noexcept -> ClassDecl&;
  ClassDecl(const ClassDecl&) = delete;
  auto operator=(const ClassDecl&) -> ClassDecl& = delete;

  [[nodiscard]] auto Name() const -> const std::string&;

  [[nodiscard]] auto Types() const -> const std::vector<Type>&;
  [[nodiscard]] auto GetType(TypeId id) const -> const Type&;
  auto AddType(TypeData data) -> TypeId;

  [[nodiscard]] auto MemberVars() const -> const std::vector<MemberVar>&;
  [[nodiscard]] auto GetMemberVar(MemberVarId id) const -> const MemberVar&;
  auto AddMemberVar(std::string name, TypeId type) -> MemberVarId;

  [[nodiscard]] auto Constructor() const -> const Body&;
  auto Constructor() -> Body&;

  [[nodiscard]] auto Processes() const -> const std::vector<Process>&;
  [[nodiscard]] auto GetProcess(ProcessId id) const -> const Process&;
  auto AddProcess(Process process) -> ProcessId;

 private:
  std::string name_;
  std::vector<Type> types_;
  std::vector<MemberVar> member_vars_;
  Body constructor_;
  std::vector<Process> processes_;
};

}  // namespace lyra::mir
