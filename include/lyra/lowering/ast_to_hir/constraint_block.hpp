#pragma once

#include <string_view>

namespace slang::ast {
class ClassType;
class ConstraintBlockSymbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// The constraint block of the name `name` a class `cls` extends declares, which
// a block of that name in `cls` overrides (LRM 18.5.2): the nearest one, since
// a block overriding another is the one an extension reaches. A static block is
// overridden as any other is. Nothing where no class it extends declares one.
auto OverriddenConstraintBlock(
    const slang::ast::ClassType& cls, std::string_view name)
    -> const slang::ast::ConstraintBlockSymbol*;

// The block `block` overrides at the root of its lineage, which is the one
// whose class introduced the behavior and so the one a mode of the class is
// kept for (LRM 18.5.10).
auto IntroducingConstraintBlock(const slang::ast::ConstraintBlockSymbol& block)
    -> const slang::ast::ConstraintBlockSymbol&;

}  // namespace lyra::lowering::ast_to_hir
