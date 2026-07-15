#pragma once

#include <cstdint>
#include <string_view>

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
};

// The runtime-library function name a call to this method lowers to (the symbol
// under `lyra::runtime`). Kept in the support layer, not the backend, so the
// backend renders the call mechanically without a per-method branch.
auto ImportedRuntimeMethodSymbol(ImportedRuntimeMethod method)
    -> std::string_view;

// Whether the method is a static function reached without a receiver, so the
// call threads the engine services handle rather than a receiver as its
// leading argument (LRM 9.7 `process::self`).
auto ImportedRuntimeMethodIsStatic(ImportedRuntimeMethod method) -> bool;

}  // namespace lyra::support
