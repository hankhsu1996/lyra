#pragma once

#include <span>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/runtime/runtime_scope_kind.hpp"

namespace lyra::runtime {

class RuntimeScope {
 public:
  RuntimeScope(RuntimeScope* parent, std::string name, RuntimeScopeKind kind);

  auto Parent() -> RuntimeScope*;
  [[nodiscard]] auto Name() const -> std::string_view;
  [[nodiscard]] auto Kind() const -> RuntimeScopeKind;

  void AddChild(RuntimeScope& child);
  [[nodiscard]] auto Children() const -> std::span<RuntimeScope* const>;

 private:
  RuntimeScope* parent_ = nullptr;
  std::string name_;
  RuntimeScopeKind kind_;
  std::vector<RuntimeScope*> children_;
};

}  // namespace lyra::runtime
