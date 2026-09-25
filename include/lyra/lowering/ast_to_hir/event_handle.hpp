#pragma once

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"

namespace slang::ast {
class Type;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// An event variable is also a handle to a synchronization object (LRM 15.5.5):
// it can be given another event's object or none, two can be compared, and one
// can be tested for naming any. Each use treats the variable as that handle,
// which is not yet supported, so each is refused where the source is read --
// one answer, whatever backend would have met the program later.

// Giving storage of `target`'s type a value, by assignment or as the
// initializer its declaration carries (LRM 15.5.5.1, 15.5.5.2).
auto RefuseGivingAnEventAValue(
    const slang::ast::Type& target, diag::SourceSpan span)
    -> diag::Result<void>;

// Reading a value of `operand`'s type as one: an operand of an operator, or a
// condition (LRM 15.5.5.3).
auto RefuseReadingAnEventAsAValue(
    const slang::ast::Type& operand, diag::SourceSpan span)
    -> diag::Result<void>;

}  // namespace lyra::lowering::ast_to_hir
