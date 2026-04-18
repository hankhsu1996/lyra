#include <format>
#include <string_view>
#include <unordered_map>
#include <unordered_set>

#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/engine.hpp"
#include "lyra/runtime/runtime_instance.hpp"

namespace lyra::runtime {

namespace {

// Walk the runtime scope subtree rooted at `scope` and register every node
// (both instance and generate) into the DPI scope registry. The full
// scope tree is the authoritative membership set for svScope handles;
// any scope with a hierarchical path is a valid handle.
void RegisterScopeSubtree(
    const RuntimeScope* scope,
    std::unordered_set<const RuntimeScope*>& valid_scopes,
    std::unordered_map<std::string_view, const RuntimeScope*>& scope_path_map,
    std::unordered_map<const RuntimeScope*, const char*>& scope_path_by_ptr) {
  if (scope == nullptr) return;
  if (scope->path_c_str == nullptr) {
    throw common::InternalError(
        "BuildDpiScopeRegistry",
        "null path_c_str on runtime scope during subtree registration");
  }
  if (!valid_scopes.insert(scope).second) {
    throw common::InternalError(
        "BuildDpiScopeRegistry",
        std::format(
            "duplicate RuntimeScope pointer at path '{}'", scope->path_c_str));
  }
  if (!scope_path_map.emplace(std::string_view(scope->path_c_str), scope)
           .second) {
    throw common::InternalError(
        "BuildDpiScopeRegistry",
        std::format("duplicate scope path '{}'", scope->path_c_str));
  }
  if (!scope_path_by_ptr.emplace(scope, scope->path_c_str).second) {
    throw common::InternalError(
        "BuildDpiScopeRegistry",
        "duplicate RuntimeScope in DPI reverse path map");
  }
  for (const auto& edge : scope->children) {
    RegisterScopeSubtree(
        edge.child, valid_scopes, scope_path_map, scope_path_by_ptr);
  }
}

}  // namespace

void Engine::BuildDpiScopeRegistry() {
  valid_scopes_.clear();
  scope_path_map_.clear();
  scope_path_by_ptr_.clear();
  scope_user_data_.clear();
  if (instances_.empty()) {
    return;
  }
  // Walk the runtime scope tree from each root instance. Generate scopes
  // and nested instances are reached via scope.children edges. This is
  // the authoritative source of valid svScope handles.
  for (uint32_t i = 0; i < instances_.size(); ++i) {
    const RuntimeInstance* inst = instances_[i];
    if (inst == nullptr) {
      throw common::InternalError(
          "BuildDpiScopeRegistry",
          std::format("null RuntimeInstance at index {}", i));
    }
    if (inst->scope.parent != nullptr) continue;
    RegisterScopeSubtree(
        &inst->scope, valid_scopes_, scope_path_map_, scope_path_by_ptr_);
  }
}

auto Engine::ValidateScopeHandle(svScope scope) const -> const RuntimeScope* {
  if (scope == nullptr) {
    return nullptr;
  }
  const auto* rs = static_cast<const RuntimeScope*>(scope);
  if (!valid_scopes_.contains(rs)) {
    throw common::InternalError(
        "ValidateScopeHandle", "invalid svScope handle");
  }
  return rs;
}

auto Engine::IsScopeHandleValid(const RuntimeScope* scope) const -> bool {
  return valid_scopes_.contains(scope);
}

auto Engine::ResolveScopeByPath(std::string_view path) const
    -> const RuntimeScope* {
  auto it = scope_path_map_.find(path);
  if (it == scope_path_map_.end()) {
    return nullptr;
  }
  return it->second;
}

auto Engine::GetScopePath(const RuntimeScope* scope) const -> const char* {
  if (scope == nullptr) {
    return nullptr;
  }
  if (!valid_scopes_.contains(scope)) {
    throw common::InternalError(
        "GetScopePath", "scope is not registered in DPI scope registry");
  }
  auto it = scope_path_by_ptr_.find(scope);
  if (it == scope_path_by_ptr_.end()) {
    throw common::InternalError(
        "GetScopePath", "registered scope missing reverse path entry");
  }
  return it->second;
}

auto Engine::PutScopeUserData(const RuntimeScope* scope, void* key, void* data)
    -> int {
  if (scope == nullptr || key == nullptr) {
    return -1;
  }
  if (!valid_scopes_.contains(scope)) {
    throw common::InternalError(
        "PutScopeUserData", "scope is not registered in DPI scope registry");
  }
  scope_user_data_[scope][key] = data;
  return 0;
}

auto Engine::GetScopeUserData(const RuntimeScope* scope, void* key) const
    -> void* {
  if (scope == nullptr || key == nullptr) {
    return nullptr;
  }
  if (!valid_scopes_.contains(scope)) {
    throw common::InternalError(
        "GetScopeUserData", "scope is not registered in DPI scope registry");
  }
  auto scope_it = scope_user_data_.find(scope);
  if (scope_it == scope_user_data_.end()) {
    return nullptr;
  }
  auto value_it = scope_it->second.find(key);
  if (value_it == scope_it->second.end()) {
    return nullptr;
  }
  return value_it->second;
}

auto Engine::GetSimulationTimeSemantics() const -> SimulationTimeSemantics {
  return SimulationTimeSemantics{
      .unit_power = global_precision_power_,
      .precision_power = global_precision_power_,
  };
}

auto Engine::GetScopeTimeUnitPower(const RuntimeScope* scope) const -> int32_t {
  if (scope == nullptr) {
    return GetSimulationTimeSemantics().unit_power;
  }
  // Time metadata lives on the enclosing module instance. Walk up for
  // generate scopes. Per IEEE 1800, generate blocks inherit timescale
  // from their enclosing module.
  const RuntimeScope* cur = scope;
  while (cur != nullptr && cur->kind != RuntimeScopeKind::kInstance) {
    cur = cur->parent;
  }
  if (cur == nullptr) {
    return GetSimulationTimeSemantics().unit_power;
  }
  return ScopeAsInstanceChecked(cur)->scope_time_metadata.time_unit_power;
}

auto Engine::GetScopeTimePrecisionPower(const RuntimeScope* scope) const
    -> int32_t {
  if (scope == nullptr) {
    return GetSimulationTimeSemantics().precision_power;
  }
  const RuntimeScope* cur = scope;
  while (cur != nullptr && cur->kind != RuntimeScopeKind::kInstance) {
    cur = cur->parent;
  }
  if (cur == nullptr) {
    return GetSimulationTimeSemantics().precision_power;
  }
  return ScopeAsInstanceChecked(cur)->scope_time_metadata.time_precision_power;
}

}  // namespace lyra::runtime
