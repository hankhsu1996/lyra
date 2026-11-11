#pragma once

// A design element as a cell of a library (LRM 33.2.1). Every source file is
// mapped to exactly one library, the default one where no library declaration
// matches it (LRM 33.3.1), and a cell's name is unique inside its library, so
// the library and the name together say which design element is meant where
// the name alone does not.

#include <string>
#include <string_view>

namespace slang::ast {
class DefinitionSymbol;
class InstanceBodySymbol;
class Symbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// Whether `definition` is declared outside every other design element, which
// is what makes it a cell. One declared inside another (LRM 23.4) is named in
// that element's own name space and belongs to no library on its own.
[[nodiscard]] auto IsLibraryCell(const slang::ast::DefinitionSymbol& definition)
    -> bool;

// The body `nested` is declared in, for a design element that is no cell. The
// standard lets one be declared only among another's items (LRM 23.4), so there
// always is one.
[[nodiscard]] auto BodyDeclaring(const slang::ast::DefinitionSymbol& nested)
    -> const slang::ast::InstanceBodySymbol&;

// The cell as a person writes it: `cell` in the default library and
// `library.cell` in any other, which is how a top-level cell and a
// configuration's use clause are spelled (LRM 33.2.1).
[[nodiscard]] auto CellName(const slang::ast::DefinitionSymbol& cell)
    -> std::string;

// The library the text of `unit`, a design element's body, a package or a
// compilation-unit scope, was compiled into (LRM 33.3).
[[nodiscard]] auto LibraryOf(const slang::ast::Symbol& unit)
    -> std::string_view;

// The cell the text of `unit`, a design element's body, a package or a
// compilation-unit scope, lies in, which with its library is what LRM 33.7 has
// a `%l` print. A module declared inside another is no cell of its own, so its
// text is the enclosing cell's. A package is a cell too (LRM 33.2.1). The
// compilation-unit scope is none, and the standard gives its text no cell to
// print, so it is written `$unit`, the name LRM 3.12.1 gives that scope.
[[nodiscard]] auto CellOf(const slang::ast::Symbol& unit) -> std::string_view;

}  // namespace lyra::lowering::ast_to_hir
