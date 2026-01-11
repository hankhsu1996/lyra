#pragma once

#include <cstddef>
#include <functional>
#include <memory>

#include "lyra/interpreter/instance_context.hpp"
#include "lyra/lir/process.hpp"

namespace lyra::interpreter {

/// Composite key identifying a (process, instance) pair.
/// Used to uniquely identify a process execution context - the same process
/// definition can run in multiple module instances.
struct ProcessInstanceKey {
  std::shared_ptr<lir::Process> process;
  std::shared_ptr<InstanceContext> instance;

  auto operator==(const ProcessInstanceKey& other) const -> bool {
    return process.get() == other.process.get() &&
           instance.get() == other.instance.get();
  }
};

struct ProcessInstanceKeyHash {
  auto operator()(const ProcessInstanceKey& key) const -> std::size_t {
    auto h1 = std::hash<lir::Process*>{}(key.process.get());
    auto h2 = std::hash<InstanceContext*>{}(key.instance.get());
    return h1 ^ (h2 << 1);
  }
};

/// ProcessHandle is a non-owning reference to a ProcessFrame.
///
/// This mirrors C++ coroutines where `coroutine_handle<>` is a lightweight,
/// non-owning pointer to a coroutine frame. The actual frame is owned
/// centrally by SimulationRunner::process_frames_.
///
/// Key properties (matching coroutine_handle<>):
/// - Lightweight: just contains a key (like a pointer)
/// - Non-owning: doesn't manage frame lifetime
/// - Copyable: can be stored in multiple places (e.g., compound triggers)
///
/// This enables compound triggers like `@(a or b)` where the same handle
/// is registered for both triggers, ensuring the frame is preserved
/// regardless of which trigger fires first.
struct ProcessHandle {
  ProcessInstanceKey key;

  ProcessHandle() = default;
  explicit ProcessHandle(ProcessInstanceKey k) : key(std::move(k)) {
  }
};

}  // namespace lyra::interpreter
