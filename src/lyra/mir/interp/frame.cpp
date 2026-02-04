#include "lyra/mir/interp/frame.hpp"

#include <cstddef>
#include <format>
#include <utility>
#include <vector>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/mir/interp/runtime_value.hpp"

namespace lyra::mir::interp {

Frame::Frame(
    std::vector<RuntimeValue> locals, std::vector<RuntimeValue> temps,
    std::vector<TypeId> temp_types)
    : locals_(std::move(locals)),
      temps_(std::move(temps)),
      temp_types_(std::move(temp_types)) {
  if (temps_.size() != temp_types_.size()) {
    throw common::InternalError(
        "Frame::Frame", std::format(
                            "temps/temp_types size mismatch: {} vs {}",
                            temps_.size(), temp_types_.size()));
  }
}

auto Frame::GetLocal(int id) -> RuntimeValue& {
  if (id < 0 || static_cast<size_t>(id) >= locals_.size()) {
    throw common::InternalError(
        "Frame::GetLocal",
        std::format(
            "local index {} out of bounds (size {})", id, locals_.size()));
  }
  return locals_[static_cast<size_t>(id)];
}

auto Frame::GetLocal(int id) const -> const RuntimeValue& {
  if (id < 0 || static_cast<size_t>(id) >= locals_.size()) {
    throw common::InternalError(
        "Frame::GetLocal",
        std::format(
            "local index {} out of bounds (size {})", id, locals_.size()));
  }
  return locals_[static_cast<size_t>(id)];
}

auto Frame::GetTemp(int id) -> RuntimeValue& {
  if (id < 0 || static_cast<size_t>(id) >= temps_.size()) {
    throw common::InternalError(
        "Frame::GetTemp",
        std::format(
            "temp index {} out of bounds (size {})", id, temps_.size()));
  }
  return temps_[static_cast<size_t>(id)];
}

auto Frame::GetTemp(int id) const -> const RuntimeValue& {
  if (id < 0 || static_cast<size_t>(id) >= temps_.size()) {
    throw common::InternalError(
        "Frame::GetTemp",
        std::format(
            "temp index {} out of bounds (size {})", id, temps_.size()));
  }
  return temps_[static_cast<size_t>(id)];
}

auto Frame::GetTempType(int id) const -> TypeId {
  if (id < 0 || static_cast<size_t>(id) >= temp_types_.size()) {
    throw common::InternalError(
        "Frame::GetTempType",
        std::format(
            "temp index {} out of bounds (size {})", id, temp_types_.size()));
  }
  return temp_types_[static_cast<size_t>(id)];
}

}  // namespace lyra::mir::interp
