#pragma once

#include <cstdint>

#include "lyra/common/edge_kind.hpp"
#include "lyra/runtime/suspend_record.hpp"

namespace lyra::runtime {

struct RuntimeInstance;

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

// Engine-private dense coordination coordinate.
//
// INVARIANT: DenseSignalCoord must never cross the public runtime boundary.
// Generated code, runtime helpers, and constructor-fed metadata must not
// traffic in DenseSignalCoord directly. It is an engine-internal
// implementation detail of hot coordination structures. All public
// coordination paths use ObjectSignalRef or GlobalSignalId.
//
// Not yet wired into engine internals. Currently defined as a target type
// for the R3 migration. Actual engine-internal rekeying is a later step.
struct DenseSignalCoord {
  uint32_t value;

  auto operator==(const DenseSignalCoord&) const -> bool = default;
};

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
