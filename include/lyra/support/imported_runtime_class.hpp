#pragma once

#include <cstdint>
#include <optional>
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

// The imported runtime-library class a source name denotes, or nothing where
// the library defines none by that name. A name resolves against the same
// spelling a name is printed from, so a class the library gains is spelled
// once and both directions follow.
auto ImportedRuntimeClassNamed(std::string_view name)
    -> std::optional<ImportedRuntimeClass>;

}  // namespace lyra::support
