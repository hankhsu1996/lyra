#pragma once

#include <cstdint>
#include <utility>

namespace lyra::mir {

struct ProcessId {
  uint32_t value = 0;

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
  uint32_t value = 0;

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
  uint32_t value = 0;

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
  uint32_t value = 0;

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

struct SlotId {
  uint32_t value = 0;

  auto operator==(const SlotId&) const -> bool = default;
  auto operator<=>(const SlotId&) const = default;
  explicit operator bool() const {
    return value != UINT32_MAX;
  }

  template <typename H>
  friend auto AbslHashValue(H h, SlotId id) -> H {
    return H::combine(std::move(h), id.value);
  }
};

constexpr SlotId kInvalidSlotId{UINT32_MAX};

}  // namespace lyra::mir
