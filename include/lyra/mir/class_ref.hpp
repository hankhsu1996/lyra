#pragma once

#include <cstdint>
#include <variant>

#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

// A reference to a runtime-library base class, named by the type that renders
// it (`InstanceType` / `GenScopeType`). The type is the identity of the
// imported library declaration: a consumer renders the base through the one
// type-mapping dispatch and reads the base's contract from this type, never
// from a closed classification it switches on.
struct RuntimeLibraryClassRef {
  TypeId base_type;

  auto operator==(const RuntimeLibraryClassRef&) const -> bool = default;
};

// A reference to the object type an object extends, identity following the
// compilation-unit boundary. A consumer resolves it through one entry point;
// the boundary split lives in the resolver, not in each consumer. Today the
// only producer is a runtime-library base.
using ClassRef = std::variant<RuntimeLibraryClassRef>;

// A lifecycle hook the runtime base declares and a derived object may override.
// `kResolve` / `kInitialize` / `kActivate` are the post-construction phases the
// engine invokes (binding cross-instance references, running variable
// initializers, registering processes). Constant properties -- time precision
// (LRM 3.14.2) and def-name (LRM 23.8) -- are not hooks: they are immutable
// scope metadata read as data, not generated bodies.
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
// through one path; the only base method is a runtime-library virtual.
using OverriddenMethodRef = std::variant<RuntimeLibraryMethodRef>;

}  // namespace lyra::mir
