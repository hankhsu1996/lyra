#pragma once

#include <expected>
#include <string>
#include <vector>

#include "lyra/lowering/ast_to_hir/lower.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"

namespace lyra::driver {

struct CompilationResult {
  lowering::ast_to_hir::LoweringResult hir;
  lowering::hir_to_mir::LoweringResult mir;
};

auto CompileToMir(const std::vector<std::string>& files)
    -> std::expected<CompilationResult, std::string>;

}  // namespace lyra::driver
