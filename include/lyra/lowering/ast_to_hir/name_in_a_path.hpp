#pragma once

// How a hierarchical name writes one of the names it is made of (LRM 23.6).
// The names are separated by periods, and an escaped identifier among them
// keeps its backslash and the white space that ends it, which is what lets the
// whole be read back one way only: no identifier holds white space, so no
// identifier can spell a separator.

#include <string>
#include <string_view>

namespace slang::ast {
class Compilation;
class Symbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// `identifier` as a hierarchical name of `compilation` writes it. A simple
// identifier stands as it is, including one the source happened to escape,
// since the backslash and the white space are no part of an identifier (LRM
// 5.6.1). Anything else is written escaped: an identifier holding a character
// a simple one cannot, and a keyword of the language version being compiled,
// which is an identifier only while it is escaped. A scope the source gave no
// name adds none to a path, so no name is written as no text.
[[nodiscard]] auto NameInAPath(
    std::string_view identifier, const slang::ast::Compilation& compilation)
    -> std::string;

// The same for the name `named` was declared under.
[[nodiscard]] auto NameInAPath(const slang::ast::Symbol& named) -> std::string;

}  // namespace lyra::lowering::ast_to_hir
