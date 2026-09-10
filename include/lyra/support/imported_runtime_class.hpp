#pragma once

#include <cstdint>
#include <string_view>

#include "lyra/support/builtin_fn.hpp"

namespace lyra::support {

// A nominal class the runtime library defines once and every unit imports by
// reference (LRM 9.7 `process` is the first). The identity names which library
// class; the members, methods, and handle realization live in the runtime, not
// in a per-unit declaration. Shared by HIR and MIR, like BuiltinFn.
enum class ImportedRuntimeClass : std::uint8_t {
  kProcess,
};

// The SystemVerilog source name of an imported runtime-library class.
auto ImportedRuntimeClassName(ImportedRuntimeClass klass) -> std::string_view;

// A method the runtime library provides for an imported class (LRM 9.7). The
// implementation is a runtime symbol reached by a mechanical call, so the
// identity is the flat closed namespace here, shared by HIR and MIR like
// BuiltinFn -- no per-unit method declaration exists to name it.
enum class ImportedRuntimeMethod : std::uint8_t {
  kProcessSelf,
  kProcessStatus,
  kProcessKill,
  kProcessAwait,
  kProcessSuspend,
  kProcessResume,
};

// The one declaration of a method the runtime library provides for an imported
// class -- the same shape every other runtime operation is declared in, because
// it is one: a free function the library declares, which takes the object it
// acts on as its leading argument. A backend spells the call from this and
// looks nothing up, and the ABI name a backend calling through C reaches is the
// entry's own name, so the two sides cannot drift apart.
[[nodiscard]] auto RuntimeEntryOf(ImportedRuntimeMethod method) -> RuntimeEntry;

// Whether the runtime symbol takes the runtime handle. Every method
// needs it except `status`, a pure read of the process node: `self` and
// `suspend` identify the calling process through it, while `kill`, `await`, and
// `resume` reach the scheduler to wake, park, or re-schedule. It is an ordinary
// argument wherever it is taken, whether or not the method dispatches on a
// handle.
auto ImportedRuntimeMethodTakesServices(ImportedRuntimeMethod method) -> bool;

// Whether calling the method suspends the caller until the target settles, so
// the statement lowering awaits it (LRM 9.7 `process::await` is the one
// blocking method). The others return without suspending.
auto ImportedRuntimeMethodSuspends(ImportedRuntimeMethod method) -> bool;

}  // namespace lyra::support
