#pragma once

#include <cstdint>
#include <variant>

namespace lyra::mir {

// A fixed runtime-library base class. These are object types the runtime
// library provides, not classes of any compilation unit: `kInstance` is a
// module / interface / program instance and `kGenScope` a named generate scope,
// both nodes in the runtime object tree.
enum class RuntimeClassKind : std::uint8_t {
  kInstance,
  kGenScope,
};

// A reference to a runtime-library base class.
struct RuntimeLibraryClassRef {
  RuntimeClassKind kind;

  auto operator==(const RuntimeLibraryClassRef&) const -> bool = default;
};

// A reference to the object type an object extends. A consumer resolves it
// through one path rather than switching a closed classification; a base is a
// runtime-library class.
using ClassRef = std::variant<RuntimeLibraryClassRef>;

// A virtual hook the runtime base declares and the engine invokes after
// construction: `kResolve` binds cross-instance references, `kInitialize` runs
// variable initializers, and `kActivate` registers processes (the Resolve /
// Initialize / Activate phases of the elaboration lifecycle).
enum class RuntimeMethod : std::uint8_t {
  kResolve,
  kInitialize,
  kActivate,
};

// A reference to a runtime-library method.
struct RuntimeLibraryMethodRef {
  RuntimeMethod method;

  auto operator==(const RuntimeLibraryMethodRef&) const -> bool = default;
};

// A reference to the base method an instance method overrides, resolved to a
// declaration rather than a textual name. A consumer reads the override target
// through one path; the only base method is a runtime-library lifecycle hook.
using OverriddenMethodRef = std::variant<RuntimeLibraryMethodRef>;

}  // namespace lyra::mir
