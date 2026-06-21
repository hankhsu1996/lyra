#pragma once

#include <string>
#include <vector>

#include "lyra/base/time.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/member.hpp"
#include "lyra/mir/method.hpp"
#include "lyra/mir/param.hpp"
#include "lyra/mir/process.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_alias.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

struct Class {
  std::string name;
  TypeId self_pointer_type;
  // The class's resolved time unit and precision (LRM 3.14.2). The emitted
  // class exposes the precision so the engine can take the design-global
  // minimum (LRM 3.14.3) and so delays scale to it.
  TimeResolution time_resolution;
  std::vector<ParamDecl> params;
  std::vector<MemberDecl> members;
  Block constructor_block;
  std::vector<Process> processes;
  std::vector<Class> nested_classes;
  std::vector<MethodDecl> methods;
  std::vector<TypeAliasDecl> type_aliases;

  [[nodiscard]] auto GetParam(ParamId id) const -> const ParamDecl& {
    return params.at(id.value);
  }
  [[nodiscard]] auto GetMember(MemberId id) const -> const MemberDecl& {
    return members.at(id.value);
  }
  [[nodiscard]] auto GetProcess(ProcessId id) const -> const Process& {
    return processes.at(id.value);
  }
  [[nodiscard]] auto GetNestedClass(ClassId id) const -> const Class& {
    return nested_classes.at(id.value);
  }
  [[nodiscard]] auto GetMethod(MethodId id) const -> const MethodDecl& {
    return methods.at(id.value);
  }

  auto AddParam(ParamDecl decl) -> ParamId {
    const ParamId id{static_cast<std::uint32_t>(params.size())};
    params.push_back(std::move(decl));
    return id;
  }
  auto AddMember(MemberDecl decl) -> MemberId {
    const MemberId id{static_cast<std::uint32_t>(members.size())};
    members.push_back(std::move(decl));
    return id;
  }
  auto AddProcess(Process process) -> ProcessId {
    const ProcessId id{static_cast<std::uint32_t>(processes.size())};
    processes.push_back(std::move(process));
    return id;
  }
  auto AddNestedClass(Class child) -> ClassId {
    const ClassId id{static_cast<std::uint32_t>(nested_classes.size())};
    nested_classes.push_back(std::move(child));
    return id;
  }
  auto AddMethod(MethodDecl decl) -> MethodId {
    const MethodId id{static_cast<std::uint32_t>(methods.size())};
    methods.push_back(std::move(decl));
    return id;
  }
  void AddTypeAlias(TypeAliasDecl decl) {
    type_aliases.push_back(std::move(decl));
  }
};

}  // namespace lyra::mir
