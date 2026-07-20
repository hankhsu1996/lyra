#include "lyra/runtime/dpi_scope_registry.hpp"

#include <string>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/scope.hpp"

namespace lyra::runtime {

DpiScopeRegistry::DpiScopeRegistry(Scope* root) {
  if (root != nullptr) {
    RegisterSubtree(*root);
  }
}

void DpiScopeRegistry::RegisterSubtree(Scope& scope) {
  // `$root` and synthetic anonymous begin/end scopes carry no source-visible
  // name, so they are not addressable svScope handles; their named descendants
  // still are, so the walk always recurses.
  if (scope.IsAddressable()) {
    std::string path{scope.HierarchicalPath().View()};
    auto [it, inserted] = scope_to_path_.try_emplace(&scope, std::move(path));
    if (!inserted) {
      throw InternalError("DPI scope registry: scope registered twice");
    }
    if (!path_to_scope_.try_emplace(it->second, &scope).second) {
      throw InternalError("DPI scope registry: duplicate hierarchical path");
    }
  }
  scope.ForEachChild([this](Scope& child) { RegisterSubtree(child); });
}

auto DpiScopeRegistry::IsValidHandle(const Scope* scope) const -> bool {
  return scope != nullptr && scope_to_path_.contains(scope);
}

auto DpiScopeRegistry::NameOf(const Scope* scope) const -> const char* {
  const auto it = scope_to_path_.find(scope);
  return it == scope_to_path_.end() ? nullptr : it->second.c_str();
}

auto DpiScopeRegistry::ScopeOfName(std::string_view name) const -> Scope* {
  const auto it = path_to_scope_.find(name);
  return it == path_to_scope_.end() ? nullptr : it->second;
}

auto DpiScopeRegistry::PutUserData(const Scope* scope, void* key, void* data)
    -> int {
  if (!IsValidHandle(scope) || key == nullptr) {
    return -1;
  }
  user_data_[scope][key] = data;
  return 0;
}

auto DpiScopeRegistry::GetUserData(const Scope* scope, void* key) const
    -> void* {
  if (!IsValidHandle(scope) || key == nullptr) {
    return nullptr;
  }
  const auto scope_it = user_data_.find(scope);
  if (scope_it == user_data_.end()) {
    return nullptr;
  }
  const auto key_it = scope_it->second.find(key);
  return key_it == scope_it->second.end() ? nullptr : key_it->second;
}

}  // namespace lyra::runtime
