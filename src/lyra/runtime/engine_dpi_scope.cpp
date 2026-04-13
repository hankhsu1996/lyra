#include <format>

#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/engine.hpp"
#include "lyra/runtime/runtime_instance.hpp"

namespace lyra::runtime {

void Engine::BuildDpiScopeRegistry() {
  if (instance_paths_.size() != instances_.size()) {
    throw common::InternalError(
        "BuildDpiScopeRegistry",
        std::format(
            "instance_paths_ size {} != instances_ size {}",
            instance_paths_.size(), instances_.size()));
  }
  valid_scopes_.clear();
  scope_path_map_.clear();
  scope_inst_path_map_.clear();
  scope_user_data_.clear();
  if (instances_.empty()) {
    return;
  }
  for (uint32_t i = 0; i < instances_.size(); ++i) {
    const RuntimeInstance* inst = instances_[i];
    if (inst == nullptr) {
      throw common::InternalError(
          "BuildDpiScopeRegistry",
          std::format("null RuntimeInstance at index {}", i));
    }
    const char* path_cstr = instance_paths_[i].c_str();
    valid_scopes_.insert(inst);
    auto [path_it, path_inserted] =
        scope_path_map_.emplace(std::string_view(path_cstr), inst);
    if (!path_inserted) {
      throw common::InternalError(
          "BuildDpiScopeRegistry",
          std::format("duplicate instance path '{}'", instance_paths_[i]));
    }
    auto [inst_it, inst_inserted] =
        scope_inst_path_map_.emplace(inst, path_cstr);
    if (!inst_inserted) {
      throw common::InternalError(
          "BuildDpiScopeRegistry",
          "duplicate RuntimeInstance in DPI scope registry");
    }
  }
}

auto Engine::ValidateScopeHandle(svScope scope) const
    -> const RuntimeInstance* {
  if (scope == nullptr) {
    return nullptr;
  }
  const auto* inst = static_cast<const RuntimeInstance*>(scope);
  if (!valid_scopes_.contains(inst)) {
    throw common::InternalError(
        "ValidateScopeHandle", "invalid svScope handle");
  }
  return inst;
}

auto Engine::IsScopeHandleValid(const RuntimeInstance* inst) const -> bool {
  return valid_scopes_.contains(inst);
}

auto Engine::ResolveScopeByPath(std::string_view path) const
    -> const RuntimeInstance* {
  auto it = scope_path_map_.find(path);
  if (it == scope_path_map_.end()) {
    return nullptr;
  }
  return it->second;
}

auto Engine::GetScopePath(const RuntimeInstance* inst) const -> const char* {
  if (inst == nullptr) {
    return nullptr;
  }
  if (!valid_scopes_.contains(inst)) {
    throw common::InternalError(
        "GetScopePath", "scope is not registered in DPI scope registry");
  }
  auto it = scope_inst_path_map_.find(inst);
  if (it == scope_inst_path_map_.end()) {
    throw common::InternalError(
        "GetScopePath", "registered scope missing reverse path entry");
  }
  return it->second;
}

auto Engine::PutScopeUserData(
    const RuntimeInstance* inst, void* key, void* data) -> int {
  if (inst == nullptr || key == nullptr) {
    return -1;
  }
  if (!valid_scopes_.contains(inst)) {
    throw common::InternalError(
        "PutScopeUserData", "scope is not registered in DPI scope registry");
  }
  scope_user_data_[inst][key] = data;
  return 0;
}

auto Engine::GetScopeUserData(const RuntimeInstance* inst, void* key) const
    -> void* {
  if (inst == nullptr || key == nullptr) {
    return nullptr;
  }
  if (!valid_scopes_.contains(inst)) {
    throw common::InternalError(
        "GetScopeUserData", "scope is not registered in DPI scope registry");
  }
  auto scope_it = scope_user_data_.find(inst);
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

auto Engine::GetScopeTimeUnitPower(const RuntimeInstance* inst) const
    -> int32_t {
  if (inst == nullptr) {
    return GetSimulationTimeSemantics().unit_power;
  }
  return inst->scope_time_metadata.time_unit_power;
}

auto Engine::GetScopeTimePrecisionPower(const RuntimeInstance* inst) const
    -> int32_t {
  if (inst == nullptr) {
    return GetSimulationTimeSemantics().precision_power;
  }
  return inst->scope_time_metadata.time_precision_power;
}

}  // namespace lyra::runtime
