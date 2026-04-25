#include "lyra/mir/class_decl.hpp"

#include <cstdint>
#include <string>
#include <utility>
#include <vector>

#include "lyra/mir/member.hpp"
#include "lyra/mir/process.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::mir {

ClassDecl::ClassDecl(std::string name) : name_(std::move(name)) {
}

ClassDecl::~ClassDecl() = default;
ClassDecl::ClassDecl(ClassDecl&&) noexcept = default;
auto ClassDecl::operator=(ClassDecl&&) noexcept -> ClassDecl& = default;

auto ClassDecl::Name() const -> const std::string& {
  return name_;
}

auto ClassDecl::Types() const -> const std::vector<Type>& {
  return types_;
}

auto ClassDecl::GetType(TypeId id) const -> const Type& {
  return types_.at(id.value);
}

auto ClassDecl::AddType(TypeData data) -> TypeId {
  const TypeId id{static_cast<std::uint32_t>(types_.size())};
  types_.push_back(Type{.data = std::move(data)});
  return id;
}

auto ClassDecl::Members() const -> const std::vector<Member>& {
  return members_;
}

auto ClassDecl::GetMember(MemberId id) const -> const Member& {
  return members_.at(id.value);
}

auto ClassDecl::AddMember(std::string name, TypeId type) -> MemberId {
  const MemberId id{static_cast<std::uint32_t>(members_.size())};
  members_.push_back(Member{.name = std::move(name), .type = type});
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

}  // namespace lyra::mir
