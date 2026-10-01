#pragma once

#include <cstdint>
#include <memory>
#include <unordered_map>
#include <variant>
#include <vector>

namespace slang::analysis {
class AnalysisContext;
class AnalysisManager;
}  // namespace slang::analysis

namespace slang::ast {
class Expression;
class Statement;
class SubroutineSymbol;
class Symbol;
class TimingControl;
class ValueSymbol;
class ProceduralBlockSymbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// A read observes the whole signal on any change, the form a non-bit-addressed
// observation (e.g. a port connection) produces.
struct ReadOfWhole {};

// A read of the bits the source's own selects name: the longest static prefixes
// (LRM 11.5.3) of reads of the symbol in the analyzed node, which together make
// up exactly the bits the analysis reported. Where an index in one is a value
// each construction of the body is given, the prefix still says which bits the
// next construction reads, and the bits one elaboration settled do not.
struct ReadOfSelects {
  std::vector<const slang::ast::Expression*> prefixes;
};

// A read of a run of the symbol's flat-bit encoding, first and last bit, that
// no select in the analyzed node names -- a read inside a called function, or
// what is left of reads once a procedure's own writes are excluded.
struct ReadOfBits {
  std::uint64_t first = 0;
  std::uint64_t last = 0;
};

// One leaf read produced by `SensitivityAnalyzer`: which symbol, and which part
// of it.
struct SensitivityRead {
  const slang::ast::ValueSymbol* symbol = nullptr;
  std::variant<ReadOfWhole, ReadOfSelects, ReadOfBits> part;
};

// Reports the state an `slang::ast::Expression`, a `slang::ast::Statement`, or
// a subroutine's body reads from outside itself: what exists before the node
// runs, which is everything declared outside it and every static variable
// declared inside (LRM 6.21). What the node brings into being itself -- an
// automatic it declares, a subroutine's formals and result, an array method's
// iterator, a pattern's binding -- holds nothing until the node runs and
// nothing outside it can write, so a read of one is no read of state. Every
// caller asks this one question; none of them wants the reads of the node's
// own variables.
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

  // The state `subroutine`'s body reads from outside a call of it.
  [[nodiscard]] auto AnalyzeReads(
      const slang::ast::SubroutineSymbol& subroutine)
      -> const std::vector<SensitivityRead>&;

  // The effective sensitivity of an `always_comb` / `always_latch` procedure
  // (LRM 9.2.2.2.1, 9.2.2.3): the reads that wake it, including reads inside
  // any function it calls, with locally-declared symbols and self-driven bit
  // ranges already excluded. This is the procedure-level surface, distinct from
  // the reads of a single node, which reflect only call arguments across a
  // function boundary (the `always @*` rule).
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
      const slang::ast::SubroutineSymbol*, std::vector<SensitivityRead>>
      subroutine_cache_;
  std::unordered_map<
      const slang::ast::ProceduralBlockSymbol*, std::vector<SensitivityRead>>
      procedure_cache_;
  std::unordered_map<
      const slang::ast::ProceduralBlockSymbol*,
      const slang::ast::TimingControl*>
      procedure_clock_cache_;
};

// Whether `symbol` holds state before a call of `subroutine` runs, by the same
// rule the analyzer reads with.
[[nodiscard]] auto HoldsStateBeforeItRuns(
    const slang::ast::Symbol& symbol,
    const slang::ast::SubroutineSymbol& subroutine) -> bool;

}  // namespace lyra::lowering::ast_to_hir
