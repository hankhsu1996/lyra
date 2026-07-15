#pragma once

#include <cstdint>
#include <string>
#include <variant>

#include "lyra/mir/class_id.hpp"

namespace lyra::mir {

// A reference to a class in this compilation unit's own class registry, named
// by its canonical local identity. The referred class is defined in this unit;
// a consumer reads its declaration through the registry. Used when an SV
// class extends another class of the same unit (LRM 8.13).
struct IntraUnitClassRef {
  ClassId class_id;

  auto operator==(const IntraUnitClassRef&) const -> bool = default;
};

// A reference to a class declared outside this compilation unit, named by its
// final target-language qualified name (e.g. `lyra::runtime::Scope`). One arm
// covers every non-intra-unit class -- a runtime library base and a
// cross-unit SV class both flow through it, distinguished only by which
// qualified name the producer supplied. The backend renders it through the
// same type-mapping dispatch that renders `ExternalClassType`.
struct ExternalClassRef {
  std::string qualified_name;

  auto operator==(const ExternalClassRef&) const -> bool = default;
};

// A reference to the class an object extends. Either intra-unit (a class of
// this unit's registry) or external (any other class, named by its qualified
// target-language name). A consumer reads each arm directly.
using ClassRef = std::variant<IntraUnitClassRef, ExternalClassRef>;

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
