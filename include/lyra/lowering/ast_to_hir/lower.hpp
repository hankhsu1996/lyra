#pragma once

#include <vector>

#include <slang/ast/Compilation.h>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/slang_source_mapper.hpp"
#include "lyra/hir/module_unit.hpp"

namespace lyra::lowering::ast_to_hir {

class LowerCompilationFacts {
 public:
  LowerCompilationFacts(
      slang::ast::Compilation& compilation,
      const diag::SlangSourceMapper& source_mapper)
      : compilation_(&compilation), source_mapper_(&source_mapper) {
  }

  [[nodiscard]] auto Compilation() const -> slang::ast::Compilation& {
    return *compilation_;
  }
  [[nodiscard]] auto SourceMapper() const -> const diag::SlangSourceMapper& {
    return *source_mapper_;
  }

 private:
  slang::ast::Compilation* compilation_;
  const diag::SlangSourceMapper* source_mapper_;
};

auto LowerCompilation(const LowerCompilationFacts& facts)
    -> diag::Result<std::vector<hir::ModuleUnit>>;

}  // namespace lyra::lowering::ast_to_hir
