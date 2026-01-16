#include "lyra/mir/interp/frame.hpp"

#include <cstddef>
#include <format>

#include "lyra/common/internal_error.hpp"
#include "lyra/mir/interp/runtime_value.hpp"
#include "lyra/mir/place.hpp"

namespace lyra::mir::interp {

Frame::Frame(size_t num_locals, size_t num_temps)
    : locals_(num_locals), temps_(num_temps) {
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

auto Frame::Resolve(const PlaceRoot& root) -> RuntimeValue& {
  switch (root.kind) {
    case PlaceRoot::Kind::kLocal:
      return GetLocal(root.id);
    case PlaceRoot::Kind::kTemp:
      return GetTemp(root.id);
    case PlaceRoot::Kind::kDesign:
      throw common::InternalError(
          "Frame::Resolve", "design storage not supported in interpreter");
  }
  throw common::InternalError("Frame::Resolve", "unknown PlaceRoot kind");
}

auto Frame::Resolve(const PlaceRoot& root) const -> const RuntimeValue& {
  switch (root.kind) {
    case PlaceRoot::Kind::kLocal:
      return GetLocal(root.id);
    case PlaceRoot::Kind::kTemp:
      return GetTemp(root.id);
    case PlaceRoot::Kind::kDesign:
      throw common::InternalError(
          "Frame::Resolve", "design storage not supported in interpreter");
  }
  throw common::InternalError("Frame::Resolve", "unknown PlaceRoot kind");
}

}  // namespace lyra::mir::interp
