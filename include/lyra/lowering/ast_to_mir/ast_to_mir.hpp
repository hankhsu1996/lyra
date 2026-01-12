#pragma once

#include <memory>
#include <string>
#include <vector>

#include "lyra/common/type_arena.hpp"
#include "lyra/mir/module.hpp"
#include "lyra/mir/package.hpp"

namespace slang::ast {
class Compilation;
}

namespace lyra::lowering::ast_to_mir {

/// Result of lowering AST to MIR.
/// TypeArena is declared first to ensure it is destroyed last (Types may
/// contain pointers to arena-allocated nested types).
struct LoweringResult {
  common::TypeArena type_arena;
  std::vector<std::unique_ptr<mir::Package>> packages;
  std::vector<std::unique_ptr<mir::Module>> modules;
};

// Lowers AST to MIR Modules and Packages.
//
// If `top` is specified: returns modules in hierarchy starting from top.
// If `top` is empty: returns all top-level instances (for dump command).
//
// Packages are always returned (all non-std packages in the compilation).
//
// Throws DiagnosticException if top module not found.
auto AstToMir(slang::ast::Compilation& compilation, const std::string& top)
    -> LoweringResult;

}  // namespace lyra::lowering::ast_to_mir
