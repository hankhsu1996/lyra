#include "lyra/runtime/dpi_scope_handles.hpp"

#include <optional>
#include <string>
#include <string_view>

#include "lyra/runtime/hierarchical_name.hpp"
#include "lyra/runtime/scope.hpp"

namespace lyra::runtime {

DpiScopeHandles::DpiScopeHandles(Scope& root) : root_(&root) {
}

auto DpiScopeHandles::Issue(Scope* scope) -> Scope* {
  if (scope != nullptr) {
    issued_.try_emplace(scope);
  }
  return scope;
}

auto DpiScopeHandles::IsIssued(const Scope* scope) const -> bool {
  return issued_.contains(scope);
}

auto DpiScopeHandles::NameOf(const Scope* scope) -> const char* {
  const auto it = issued_.find(scope);
  if (it == issued_.end()) {
    return nullptr;
  }
  std::optional<std::string>& name = it->second;
  if (!name.has_value()) {
    name.emplace(scope->HierarchicalPath().View());
  }
  return name->c_str();
}

// An instance scope is named from the tops down, so a name beginning at a
// package names none.
auto DpiScopeHandles::ScopeOfName(std::string_view name) const -> Scope* {
  const std::optional<HierarchicalName> read = ReadHierarchicalName(name);
  if (!read.has_value() || read->package.has_value()) return nullptr;
  const ScopeReached reached = LongestNamedPrefix(*root_, read->parts);
  return reached.parts == read->parts.size() ? reached.scope : nullptr;
}

auto DpiScopeHandles::PutUserData(const Scope* scope, void* key, void* data)
    -> int {
  if (!IsIssued(scope) || key == nullptr) {
    return -1;
  }
  user_data_[scope][key] = data;
  return 0;
}

auto DpiScopeHandles::GetUserData(const Scope* scope, void* key) const
    -> void* {
  const auto scope_it = user_data_.find(scope);
  if (scope_it == user_data_.end()) {
    return nullptr;
  }
  const auto key_it = scope_it->second.find(key);
  return key_it == scope_it->second.end() ? nullptr : key_it->second;
}

}  // namespace lyra::runtime
