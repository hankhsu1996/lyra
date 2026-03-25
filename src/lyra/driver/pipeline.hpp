#pragma once

#include <expected>

#include "compilation_error.hpp"
#include "lyra/lowering/ast_to_hir/lower.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"

namespace lyra::driver {

class CompilationOutput;

struct CompilationResult {
  lowering::ast_to_hir::LoweringResult hir;
  lowering::hir_to_mir::LoweringResult mir;
};

struct CompilationInput;

auto CompileToMir(const CompilationInput& input, CompilationOutput& output)
    -> std::expected<CompilationResult, CompilationError>;

}  // namespace lyra::driver
