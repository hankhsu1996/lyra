#pragma once

// A design element as a cell of a library (LRM 33.2.1). Every source file is
// mapped to exactly one library, the default one where no library declaration
// matches it (LRM 33.3.1), and a cell's name is unique inside its library, so
// the library and the name together say which design element is meant where
// the name alone does not.

#include <string>

namespace slang::ast {
class DefinitionSymbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// Whether `definition` is declared outside every other design element, which
// is what makes it a cell. One declared inside another (LRM 23.4) is named in
// that element's own name space and belongs to no library on its own.
[[nodiscard]] auto IsLibraryCell(const slang::ast::DefinitionSymbol& definition)
    -> bool;

// The cell as a person writes it: `cell` in the default library and
// `library.cell` in any other, which is how a top-level cell and a
// configuration's use clause are spelled (LRM 33.2.1).
[[nodiscard]] auto CellName(const slang::ast::DefinitionSymbol& cell)
    -> std::string;

}  // namespace lyra::lowering::ast_to_hir
