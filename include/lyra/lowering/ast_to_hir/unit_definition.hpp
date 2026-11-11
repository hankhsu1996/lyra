#pragma once

#include <slang/ast/SemanticFacts.h>
#include <slang/ast/Symbol.h>

#include "lyra/hir/unit_definition.hpp"

namespace lyra::lowering::ast_to_hir {

// The kind of design element a definition of `kind` is.
[[nodiscard]] auto DesignElementKindOf(slang::ast::DefinitionKind kind)
    -> hir::DesignElementKind;

// What the source states of the definition the unit at `home` is lowered
// from: a design element's body, a package, a `$unit` scope, or a
// specialization of a parameterized class lowered on its own, whose text lies
// in the scope `text_scope`.
[[nodiscard]] auto UnitDefinitionOf(
    const slang::ast::Symbol& home, const slang::ast::Symbol& text_scope)
    -> hir::UnitDefinition;

}  // namespace lyra::lowering::ast_to_hir
