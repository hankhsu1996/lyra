#pragma once

#include <optional>

#include <slang/ast/types/Type.h>

#include "lyra/hir/typespec.hpp"

namespace lyra::lowering::ast_to_hir {

// The type `declared` as the source declared it, none for a type no reader
// reaching the design by name describes -- an unpacked aggregate, a string, a
// handle, a container: its kind, its bits (LRM 6.24.3 `$bits`), the outermost
// packed range a vector declares, and an enumeration's base (LRM 6.19).
auto TypespecOf(const slang::ast::Type& declared)
    -> std::optional<hir::Typespec>;

}  // namespace lyra::lowering::ast_to_hir
