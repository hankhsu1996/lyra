#pragma once

#include <string>
#include <vector>

#include <slang/ast/Compilation.h>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/frontend/slang_source_mapper.hpp"
#include "lyra/hir/module_unit.hpp"
#include "lyra/lowering/ast_to_hir/sensitivity.hpp"

namespace slang::ast {
class InstanceBodySymbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// Driver-supplied facts threaded into AST-to-HIR lowering. `Compilation&` is
// the slang elaboration root; `SourceMapper&` translates slang source
// locations; `SensitivityAnalyzer&` is reused across modules so its read
// cache survives. `disable_assertions` is the lowering policy that elides
// assertion constructs instead of rejecting them.
class LowerCompilationFacts {
 public:
  LowerCompilationFacts(
      slang::ast::Compilation& compilation,
      const frontend::SlangSourceMapper& source_mapper,
      SensitivityAnalyzer& sensitivity_analyzer, bool disable_assertions)
      : compilation_(&compilation),
        source_mapper_(&source_mapper),
        sensitivity_analyzer_(&sensitivity_analyzer),
        disable_assertions_(disable_assertions) {
  }

  [[nodiscard]] auto Compilation() const -> slang::ast::Compilation& {
    return *compilation_;
  }
  [[nodiscard]] auto SourceMapper() const
      -> const frontend::SlangSourceMapper& {
    return *source_mapper_;
  }
  [[nodiscard]] auto Sensitivity() const -> SensitivityAnalyzer& {
    return *sensitivity_analyzer_;
  }
  [[nodiscard]] auto DisableAssertions() const -> bool {
    return disable_assertions_;
  }

 private:
  slang::ast::Compilation* compilation_;
  const frontend::SlangSourceMapper* source_mapper_;
  SensitivityAnalyzer* sensitivity_analyzer_;
  bool disable_assertions_;
};

// The distinct unit bodies reachable from the tops, deduped to one canonical
// body per specialization -- the work-list for per-unit lowering. A module
// instantiated many times appears once; the descent is transitive, so a unit
// reached only through instantiation is included though it is not a top.
auto CollectUnitBodies(const LowerCompilationFacts& facts)
    -> std::vector<const slang::ast::InstanceBodySymbol*>;

// Rejects a design that exports a subroutine over DPI-C (LRM 35.5): Lyra emits
// no C-export ABI, so the export contract cannot be honored. Runs once per
// compilation, before any unit lowers.
auto RejectDpiExports(const LowerCompilationFacts& facts) -> diag::Result<void>;

// Lowers one unit body to its HIR. Independent of every other unit: it reads
// only this body and the shared frontend, so units may be lowered in any order.
auto LowerUnit(
    const LowerCompilationFacts& facts,
    const slang::ast::InstanceBodySymbol& body)
    -> diag::Result<hir::ModuleUnit>;

// A top-level block is an auto-promoted, uninstantiated module. These names
// are a subset of the compiled units: a unit reached only through
// instantiation is compiled but is not a top.
auto TopLevelUnitNames(slang::ast::Compilation& compilation)
    -> std::vector<std::string>;

}  // namespace lyra::lowering::ast_to_hir
