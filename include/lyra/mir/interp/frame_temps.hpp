#pragma once

#include <utility>
#include <vector>

#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/mir/interp/runtime_value.hpp"
#include "lyra/mir/operand.hpp"

namespace lyra::mir::interp {

// Forward declaration - defined in interp_rvalue.cpp, declared in
// interpreter.hpp
auto CreateDefaultValue(const TypeArena& types, TypeId type_id) -> RuntimeValue;

// Build temps and temp_types from canonical MIR metadata.
// Single source-of-truth for Frame temp initialization.
// Both CreateProcessState and RunFunction MUST use this helper.
inline auto BuildFrameTemps(
    const std::vector<TempMetadata>& temp_metadata, const TypeArena& types)
    -> std::pair<std::vector<RuntimeValue>, std::vector<TypeId>> {
  std::vector<RuntimeValue> temps;
  std::vector<TypeId> temp_types;
  temps.reserve(temp_metadata.size());
  temp_types.reserve(temp_metadata.size());

  for (const auto& meta : temp_metadata) {
    temps.push_back(
        meta.type ? CreateDefaultValue(types, meta.type) : RuntimeValue{});
    temp_types.push_back(meta.type);
  }
  return {std::move(temps), std::move(temp_types)};
}

}  // namespace lyra::mir::interp
