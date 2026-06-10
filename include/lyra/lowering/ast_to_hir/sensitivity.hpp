#pragma once

#include <cstdint>
#include <memory>
#include <unordered_map>
#include <utility>
#include <vector>

namespace slang::analysis {
class AnalysisContext;
class AnalysisManager;
}  // namespace slang::analysis

namespace slang::ast {
class Expression;
class Statement;
class Symbol;
class ValueSymbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// One leaf read produced by `SensitivityAnalyzer`.
struct SensitivityRead {
  const slang::ast::ValueSymbol* symbol;
  std::pair<std::uint64_t, std::uint64_t> bit_range;
};

// Reports every value read inside an arbitrary `slang::ast::Expression` or
// `slang::ast::Statement`. The analyzer is intentionally generic -- it does
// not know what feature is asking, and it does not filter out reads of
// locally-declared symbols. LRM-specific transformations on top of the raw
// read set (e.g. LRM 9.2.2.2.1 procedure-body sensitivity, which excludes
// locals) live at the layer that knows about the construct, not here.
//
// `containing_symbol` is a slang plumbing requirement: slang's flow analysis
// builds a name-lookup `EvalContext` from `symbol.getParentScope()`, so the
// analyzer needs any symbol whose parent scope covers the analyzed node.
// The choice of symbol does not change the returned read set; pass whatever
// symbol the caller already has on hand to wrap the node (a `ProceduralBlock`
// for waits inside a procedure, a `ContinuousAssignSymbol` for an `assign`
// rhs, and so on).
//
// Results are cached, so repeated queries on the same AST pointer are free.
class SensitivityAnalyzer {
 public:
  SensitivityAnalyzer();
  ~SensitivityAnalyzer();

  SensitivityAnalyzer(const SensitivityAnalyzer&) = delete;
  auto operator=(const SensitivityAnalyzer&) -> SensitivityAnalyzer& = delete;
  SensitivityAnalyzer(SensitivityAnalyzer&&) noexcept;
  auto operator=(SensitivityAnalyzer&&) noexcept -> SensitivityAnalyzer&;

  [[nodiscard]] auto AnalyzeReads(
      const slang::ast::Expression& expr,
      const slang::ast::Symbol& containing_symbol)
      -> const std::vector<SensitivityRead>&;

  [[nodiscard]] auto AnalyzeReads(
      const slang::ast::Statement& stmt,
      const slang::ast::Symbol& containing_symbol)
      -> const std::vector<SensitivityRead>&;

 private:
  std::unique_ptr<slang::analysis::AnalysisManager> manager_;
  std::unique_ptr<slang::analysis::AnalysisContext> context_;
  std::unordered_map<
      const slang::ast::Expression*, std::vector<SensitivityRead>>
      expression_cache_;
  std::unordered_map<const slang::ast::Statement*, std::vector<SensitivityRead>>
      statement_cache_;
};

}  // namespace lyra::lowering::ast_to_hir
