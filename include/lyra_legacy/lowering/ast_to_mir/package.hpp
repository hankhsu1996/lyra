#pragma once

#include <memory>

#include "lyra/mir/package.hpp"

namespace slang::ast {
class PackageSymbol;
}

namespace lyra::common {
class TypeArena;
}

namespace lyra::lowering::ast_to_mir {

class SymbolRegistrar;

// Lowers a slang PackageSymbol to an MIR Package.
// Currently extracts typedef and enum type declarations.
auto LowerPackage(
    const slang::ast::PackageSymbol& pkg_symbol, common::TypeArena& arena,
    SymbolRegistrar& registrar) -> std::unique_ptr<mir::Package>;

}  // namespace lyra::lowering::ast_to_mir
