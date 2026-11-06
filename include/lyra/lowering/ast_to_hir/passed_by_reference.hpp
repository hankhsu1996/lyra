#pragma once

// What may be passed by reference (LRM 13.5.2), where the front end accepts
// more than the standard allows and the text has no meaning to carry out.

#include "lyra/diag/diagnostic.hpp"

namespace slang::ast {
class Expression;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// Refuses `actual`, bound to a formal or a port passed by reference, where it
// is a member of an unpacked union or reaches its part through one. LRM 13.5.2
// lists what may be passed by reference, and a union's member is not among
// them: a union is one storage its members are alternative readings of (LRM
// 7.3), so a reference held while another member writes that storage, or while
// a tagged union changes which member it holds (LRM 7.3.2, 11.9), would name
// storage of no fixed type. A part below a member -- a structure's member or an
// array's element -- has storage of its own only where every enclosing value
// does, so it is refused too.
[[nodiscard]] auto RefuseUnionPartByReference(
    const slang::ast::Expression& actual) -> diag::Result<void>;

}  // namespace lyra::lowering::ast_to_hir
