#include "lyra/runtime/dpi_scope_handles.hpp"

#include <optional>
#include <string>
#include <string_view>

#include "lyra/runtime/scope.hpp"

namespace lyra::runtime {

namespace {

// The scope `name` names below `scope`, or null. A scope the source gave no
// name stands in no path (LRM 23.6), so it is looked through with the whole of
// `name` still to find; any other child is tried by its own part of a path,
// spelled as a path spells it, which either ends the name or is followed by the
// period before the rest.
auto FindBelow(Scope& scope, std::string_view name) -> Scope* {
  Scope* found = nullptr;
  scope.ForEachChild([&](Scope& child) {
    if (found != nullptr) {
      return;
    }
    if (!child.IsAddressable()) {
      found = FindBelow(child, name);
      return;
    }
    const std::string part = child.DisplaySegment();
    if (!name.starts_with(part)) {
      return;
    }
    const std::string_view rest = name.substr(part.size());
    if (rest.empty()) {
      found = &child;
    } else if (rest.front() == '.') {
      found = FindBelow(child, rest.substr(1));
    }
  });
  return found;
}

}  // namespace

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

auto DpiScopeHandles::ScopeOfName(std::string_view name) const -> Scope* {
  return FindBelow(*root_, name);
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
