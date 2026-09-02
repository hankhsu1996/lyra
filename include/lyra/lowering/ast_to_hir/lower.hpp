#pragma once

#include <string>
#include <vector>

#include <slang/ast/Compilation.h>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/frontend/slang_source_mapper.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/unit_signatures.hpp"
#include "lyra/lowering/ast_to_hir/sensitivity.hpp"
#include "lyra/support/assertion_policy.hpp"

namespace lyra::lowering::ast_to_hir {

// Driver-supplied facts threaded into AST-to-HIR lowering. `Compilation&` is
// the slang elaboration root; `SourceMapper&` translates slang source
// locations; `SensitivityAnalyzer&` is reused across modules so its read
// cache survives. The assertion policy is what decides whether an assertion
// construct is elided rather than refused.
class LowerCompilationFacts {
 public:
  LowerCompilationFacts(
      slang::ast::Compilation& compilation,
      const frontend::SlangSourceMapper& source_mapper,
      SensitivityAnalyzer& sensitivity_analyzer,
      support::AssertionPolicy assertion_policy)
      : compilation_(&compilation),
        source_mapper_(&source_mapper),
        sensitivity_analyzer_(&sensitivity_analyzer),
        assertion_policy_(assertion_policy) {
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
  [[nodiscard]] auto AssertionPolicy() const -> support::AssertionPolicy {
    return assertion_policy_;
  }

 private:
  slang::ast::Compilation* compilation_;
  const frontend::SlangSourceMapper* source_mapper_;
  SensitivityAnalyzer* sensitivity_analyzer_;
  support::AssertionPolicy assertion_policy_;
};

// The two artifacts the design's units yield: what each unit is, and what each
// publishes. They are separate because their readers are: a unit's own code is
// read by the stage that lowers it further, while its signature is read by the
// units that reference it, and by nothing else.
struct HirCompilation {
  std::vector<hir::CompilationUnit> units;
  hir::UnitSignatures signatures;
};

// Lowers the whole compilation to its HIR units: every namespace the design
// declares, then every distinct design-element body reachable from the tops,
// each tagged with whether its instances exist as objects. Each unit is lowered
// independently -- it reads
// only its own scope, the shared frontend, and the signatures of the units its
// own declarations name -- so the result is a flat set of self-contained units
// with no cross-unit HIR references.
auto LowerCompilationToHir(const LowerCompilationFacts& facts)
    -> diag::Result<HirCompilation>;

// A top-level block is an auto-promoted, uninstantiated module. These names
// are a subset of the compiled units: a unit reached only through
// instantiation is compiled but is not a top.
auto TopLevelUnitNames(slang::ast::Compilation& compilation)
    -> std::vector<std::string>;

}  // namespace lyra::lowering::ast_to_hir
