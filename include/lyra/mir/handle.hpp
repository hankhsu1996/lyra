#pragma once

#include <cstdint>
#include <functional>
#include <utility>

namespace lyra::mir {

struct ProcessId {
  uint32_t value = UINT32_MAX;

  auto operator==(const ProcessId&) const -> bool = default;
  auto operator<=>(const ProcessId&) const = default;
  explicit operator bool() const {
    return value != UINT32_MAX;
  }

  template <typename H>
  friend auto AbslHashValue(H h, ProcessId id) -> H {
    return H::combine(std::move(h), id.value);
  }
};

constexpr ProcessId kInvalidProcessId{UINT32_MAX};

struct FunctionId {
  uint32_t value = UINT32_MAX;

  auto operator==(const FunctionId&) const -> bool = default;
  auto operator<=>(const FunctionId&) const = default;
  explicit operator bool() const {
    return value != UINT32_MAX;
  }

  template <typename H>
  friend auto AbslHashValue(H h, FunctionId id) -> H {
    return H::combine(std::move(h), id.value);
  }
};

constexpr FunctionId kInvalidFunctionId{UINT32_MAX};

// BasicBlockId is a function-local index into Function::blocks or
// Process::blocks, NOT a global arena index.
struct BasicBlockId {
  uint32_t value = UINT32_MAX;

  auto operator==(const BasicBlockId&) const -> bool = default;
  auto operator<=>(const BasicBlockId&) const = default;
  explicit operator bool() const {
    return value != UINT32_MAX;
  }

  template <typename H>
  friend auto AbslHashValue(H h, BasicBlockId id) -> H {
    return H::combine(std::move(h), id.value);
  }
};

constexpr BasicBlockId kInvalidBasicBlockId{UINT32_MAX};

struct PlaceId {
  uint32_t value = UINT32_MAX;

  auto operator==(const PlaceId&) const -> bool = default;
  auto operator<=>(const PlaceId&) const = default;
  explicit operator bool() const {
    return value != UINT32_MAX;
  }

  template <typename H>
  friend auto AbslHashValue(H h, PlaceId id) -> H {
    return H::combine(std::move(h), id.value);
  }
};

constexpr PlaceId kInvalidPlaceId{UINT32_MAX};

struct ModuleBodyId {
  uint32_t value = UINT32_MAX;

  auto operator==(const ModuleBodyId&) const -> bool = default;
  auto operator<=>(const ModuleBodyId&) const = default;
  explicit operator bool() const {
    return value != UINT32_MAX;
  }

  template <typename H>
  friend auto AbslHashValue(H h, ModuleBodyId id) -> H {
    return H::combine(std::move(h), id.value);
  }
};

constexpr ModuleBodyId kInvalidModuleBodyId{UINT32_MAX};

}  // namespace lyra::mir

// std::hash specialization for ModuleBodyId (enables std::unordered_set/map)
template <>
struct std::hash<lyra::mir::ModuleBodyId> {
  auto operator()(lyra::mir::ModuleBodyId id) const noexcept -> size_t {
    return std::hash<uint32_t>{}(id.value);
  }
};
