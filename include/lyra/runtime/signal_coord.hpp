#pragma once

#include <cstdint>
#include <format>
#include <variant>

#include "lyra/common/edge_kind.hpp"
#include "lyra/runtime/trigger_record.hpp"

namespace lyra::runtime {

struct RuntimeInstance;

// R5: Strong identity type for module instances.
// Stable semantic identity assigned during construction, never changes.
// Must not be used as a positional index into dense arrays -- use
// a validated resolver for that conversion.
struct InstanceId {
  uint32_t value;

  auto operator==(const InstanceId&) const -> bool = default;
  auto operator<=>(const InstanceId&) const = default;
};

}  // namespace lyra::runtime

// std::format support for InstanceId. Formats as the raw integer value.
template <>
struct std::formatter<lyra::runtime::InstanceId> : std::formatter<uint32_t> {
  auto format(lyra::runtime::InstanceId id, std::format_context& ctx) const {
    return std::formatter<uint32_t>::format(id.value, ctx);
  }
};

namespace lyra::runtime {

// Semantic identity for an instance-owned signal.
// Body-local dense slot ordinal: 0-based, stable across instances of the
// same body. This is the runtime-facing signal identity for instance-owned
// signals after R3.
struct LocalSignalId {
  uint32_t value;

  auto operator==(const LocalSignalId&) const -> bool = default;
};

// Semantic identity for a package/global signal.
// Design-global slot id for true package/global state.
struct GlobalSignalId {
  uint32_t value;

  auto operator==(const GlobalSignalId&) const -> bool = default;
};

// Object-scoped signal reference: identifies an instance-owned signal as
// "member N of this RuntimeInstance". This is the public coordination
// coordinate for instance-owned signals.
struct ObjectSignalRef {
  RuntimeInstance* instance;
  LocalSignalId local;
};

// R5: Instance-scoped signal reference for subscription and NBA boundaries.
// Carries instance_id (for container lookup) and LocalSignalId (for indexing).
// Used at the top-level subscription API; below that, domain-specific helpers
// take GlobalSignalId or LocalSignalRef directly.
struct LocalSignalRef {
  InstanceId instance_id;
  LocalSignalId signal;
};

// R5: Tagged union for the subscription/NBA top boundary.
// Dispatch once at entry, then use domain-specific helpers below.
using SignalRef = std::variant<GlobalSignalId, LocalSignalRef>;

// Semantic domain tag for trigger and mutation coordinates.
enum class SignalCoordKind : uint8_t {
  kLocalInstance,
  kGlobal,
};

// Typed trigger reference preserving semantic domain.
// Used at the runtime helper / engine API boundary and in constructor-fed
// coordination metadata. Not yet wired into runtime helper or
// constructor-fed coordination paths.
struct WaitTriggerRef {
  SignalCoordKind kind;
  uint32_t id;
  common::EdgeKind edge;
  TriggerInstallKind install_kind;
  uint8_t bit_index;
  uint8_t flags;
  uint32_t byte_offset;
  uint32_t byte_size;
  uint32_t container_elem_stride;
};

// Typed mutation target reference.
// Not yet wired into runtime helper or engine mutation paths.
struct MutationTargetRef {
  SignalCoordKind kind;
  uint32_t id;
};

}  // namespace lyra::runtime
