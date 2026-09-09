#pragma once

#include <cstdint>
#include <memory>
#include <optional>
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
class TimingControl;
class ValueSymbol;
class ProceduralBlockSymbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// One leaf read produced by `SensitivityAnalyzer`. `footprint` is the flat-bit
// range of the read within the symbol's encoding; an absent footprint means the
// whole signal is observed on any change, the form a non-bit-addressed
// observation (e.g. a port connection) produces.
struct SensitivityRead {
  const slang::ast::ValueSymbol* symbol = nullptr;
  std::optional<std::pair<std::uint64_t, std::uint64_t>> footprint;
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

  // The effective sensitivity of an `always_comb` / `always_latch` procedure
  // (LRM 9.2.2.2.1, 9.2.2.3): the reads that wake it, including reads inside
  // any function it calls, with locally-declared symbols and self-driven bit
  // ranges already excluded. This is the procedure-level surface, distinct from
  // the raw read set of a single node, which reflects only call arguments
  // across a function boundary (the `always @*` rule).
  [[nodiscard]] auto AnalyzeProcedureSensitivity(
      const slang::ast::ProceduralBlockSymbol& proc)
      -> const std::vector<SensitivityRead>&;

  // The clocking event a sampled value function written in this procedure
  // counts ticks of when it names none itself: the clock the procedure settles
  // (LRM 16.14.6), and otherwise the enclosing scope's default clocking (LRM
  // 14.12). Those are the two of LRM 16.9.3's five ordered rules that reach a
  // call outside an assertion, and the front end applies both -- so this
  // reports the answer rather than the inputs to it, and nothing below repeats
  // the rule. Absent where neither yields a clock, which is what makes a call
  // naming no event of its own the error the standard requires.
  //
  // The result points into the AST, so it outlives the analysis that found it.
  [[nodiscard]] auto AnalyzeProcedureClock(
      const slang::ast::ProceduralBlockSymbol& proc)
      -> const slang::ast::TimingControl*;

 private:
  std::unique_ptr<slang::analysis::AnalysisManager> manager_;
  std::unique_ptr<slang::analysis::AnalysisContext> context_;
  std::unordered_map<
      const slang::ast::Expression*, std::vector<SensitivityRead>>
      expression_cache_;
  std::unordered_map<const slang::ast::Statement*, std::vector<SensitivityRead>>
      statement_cache_;
  std::unordered_map<
      const slang::ast::ProceduralBlockSymbol*, std::vector<SensitivityRead>>
      procedure_cache_;
  std::unordered_map<
      const slang::ast::ProceduralBlockSymbol*,
      const slang::ast::TimingControl*>
      procedure_clock_cache_;
};

}  // namespace lyra::lowering::ast_to_hir
