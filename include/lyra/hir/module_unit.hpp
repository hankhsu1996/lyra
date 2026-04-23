#pragma once

#include <cstdint>
#include <string>
#include <utility>
#include <vector>

#include "lyra/hir/process.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/var_decl.hpp"

namespace lyra::hir {

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

  [[nodiscard]] auto VarDecls() const -> const std::vector<VarDecl>& {
    return var_decls_;
  }

  [[nodiscard]] auto Processes() const -> const std::vector<Process>& {
    return processes_;
  }

  [[nodiscard]] auto GetType(TypeId id) const -> const Type& {
    return types_.at(id.value);
  }

  [[nodiscard]] auto GetVarDecl(VarDeclId id) const -> const VarDecl& {
    return var_decls_.at(id.value);
  }

  [[nodiscard]] auto GetProcess(ProcessId id) const -> const Process& {
    return processes_.at(id.value);
  }

  auto AddType(TypeData data) -> TypeId {
    const TypeId id{static_cast<std::uint32_t>(types_.size())};
    types_.push_back(Type{.data = std::move(data)});
    return id;
  }

  auto AddVarDecl(std::string name, TypeId type) -> VarDeclId {
    const VarDeclId id{static_cast<std::uint32_t>(var_decls_.size())};
    var_decls_.push_back(VarDecl{.name = std::move(name), .type = type});
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
  std::vector<VarDecl> var_decls_;
  std::vector<Process> processes_;
};

}  // namespace lyra::hir
