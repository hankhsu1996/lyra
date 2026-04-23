#pragma once

#include <cstdint>
#include <string>
#include <utility>
#include <vector>

#include "lyra/mir/member.hpp"
#include "lyra/mir/process.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::mir {

// A module specialization is a standalone compilation unit.
class ModuleUnit {
 public:
  explicit ModuleUnit(std::string name) : name_(std::move(name)) {
  }

  [[nodiscard]] auto Name() const -> const std::string& {
    return name_;
  }

  [[nodiscard]] auto Types() const -> const std::vector<Type>& {
    return types_;
  }

  [[nodiscard]] auto Members() const -> const std::vector<Member>& {
    return members_;
  }

  [[nodiscard]] auto Processes() const -> const std::vector<Process>& {
    return processes_;
  }

  [[nodiscard]] auto GetType(TypeId id) const -> const Type& {
    return types_.at(id.value);
  }

  [[nodiscard]] auto GetMember(MemberId id) const -> const Member& {
    return members_.at(id.value);
  }

  [[nodiscard]] auto GetProcess(ProcessId id) const -> const Process& {
    return processes_.at(id.value);
  }

  auto AddType(TypeData data) -> TypeId {
    const TypeId id{static_cast<std::uint32_t>(types_.size())};
    types_.push_back(Type{.data = std::move(data)});
    return id;
  }

  auto AddMember(std::string name, TypeId type) -> MemberId {
    const MemberId id{static_cast<std::uint32_t>(members_.size())};
    members_.push_back(Member{.name = std::move(name), .type = type});
    return id;
  }

  // Takes ownership of a fully-built process and produces its ProcessId.
  auto AddProcess(Process process) -> ProcessId {
    const ProcessId id{static_cast<std::uint32_t>(processes_.size())};
    processes_.push_back(std::move(process));
    return id;
  }

 private:
  std::string name_;
  std::vector<Type> types_;
  std::vector<Member> members_;
  std::vector<Process> processes_;
};

}  // namespace lyra::mir
