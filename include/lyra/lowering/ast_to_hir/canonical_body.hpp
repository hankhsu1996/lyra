#pragma once

// Which elaborated body an instance is read through, and so which lowering
// serves it. The front end elaborates a body for every instance and leaves an
// instance it found alike an earlier one pointing at that one's body
// (`InstanceSymbol::getCanonicalBody`); that record is read as given, and a
// share it gets wrong is the front end's defect.

#include <string>

namespace slang::ast {
class InstanceBodySymbol;
class InstanceSymbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// The elaborated body `inst` is read through: its own, or the earlier
// instance's body the front end found it alike and left it pointing at, or,
// where `inst` stands below such an instance, the body at the same place under
// that earlier instance. One lowering serves every instance answering alike
// here.
[[nodiscard]] auto CanonicalBodyOf(const slang::ast::InstanceSymbol& inst)
    -> const slang::ast::InstanceBodySymbol&;

// The instance that body was elaborated for, which the lowering is lowered
// for.
[[nodiscard]] auto CanonicalInstanceOf(const slang::ast::InstanceSymbol& inst)
    -> const slang::ast::InstanceSymbol&;

// The name of the lowering serving `inst`: the hierarchical path of the
// instance its body was elaborated for (LRM 23.6).
[[nodiscard]] auto LoweringNameOf(const slang::ast::InstanceSymbol& inst)
    -> std::string;

}  // namespace lyra::lowering::ast_to_hir
