#pragma once

#include <optional>

#include <slang/ast/types/Type.h>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/typespec.hpp"

namespace lyra::lowering::ast_to_hir {

class UnitLowerer;

// The type `declared` as the source declared it, among `owner`'s unit's
// typespecs: what kind of type it is, its bits (LRM 6.24.3 `$bits`) and sign
// where it is packed, its range, and each dimension, member, element, index and
// base it is built from. None for a type no reader reaching the design by name
// describes -- a covergroup's, which the coverage API reaches instead (LRM 40).
auto TypespecOf(
    UnitLowerer& owner, const slang::ast::Type& declared, diag::SourceSpan span)
    -> diag::Result<std::optional<hir::TypespecId>>;

}  // namespace lyra::lowering::ast_to_hir
