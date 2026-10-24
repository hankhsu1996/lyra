#pragma once

// A scope's hierarchical name as far as its text fixes it (LRM 21.2.1.5).
// The front end spells every symbol's name, so the text is read off it rather
// than composed here.

#include "lyra/hir/fixed_name.hpp"

namespace slang::ast {
class Scope;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// The name of `scope` -- a design element's body, a generate block, a
// package, the `$unit` scope, or a class -- below the innermost instance
// around it where there is one, and whole where there is none. A scope of the
// design hierarchy fixes nothing of its own, and neither does `$unit`, whose
// declarations the front end names alone.
[[nodiscard]] auto FixedNameOf(const slang::ast::Scope& scope)
    -> hir::FixedName;

}  // namespace lyra::lowering::ast_to_hir
