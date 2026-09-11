#pragma once

#include <optional>
#include <utility>

#include <slang/ast/SemanticFacts.h>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/support/strength_level.hpp"

namespace lyra::lowering::ast_to_hir {

// The strength a driver's source states, as one level for the whole driver
// (LRM 28.11, Table 28-7). A source stating none drives at strong (LRM 10.3.1).
// A specification whose two halves differ is refused: where such a driver is
// unknown it occupies a range of levels rather than one, and a range of levels
// is not a strength.
auto TranslateDriveStrength(
    const std::pair<
        std::optional<slang::ast::DriveStrength>,
        std::optional<slang::ast::DriveStrength>>& strength,
    diag::SourceSpan span) -> diag::Result<support::StrengthLevel>;

// The strength a declaration states a stored value is held at (LRM 6.6.4),
// absent where the declaration states none.
auto TranslateChargeStrength(std::optional<slang::ast::ChargeStrength> charge)
    -> std::optional<support::StrengthLevel>;

}  // namespace lyra::lowering::ast_to_hir
