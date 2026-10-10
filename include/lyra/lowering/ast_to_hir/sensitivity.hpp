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
class ProceduralBlockSymbol;
class Statement;
class SubroutineSymbol;
class Symbol;
class TimingControl;
class ValueSymbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// All of the symbol, the form a non-bit-addressed observation (e.g. a port
// connection) produces.
struct WholePart {};

// The bits the source's own selects name: the longest static prefixes (LRM
// 11.5.3) of the symbol in the analyzed node, which together make up exactly
// the bits the analysis reported. Where an index in one is a value each
// construction of the body is given, the prefix still says which bits the next
// construction reaches, and the bits one elaboration settled do not.
struct SelectedParts {
  std::vector<const slang::ast::Expression*> prefixes;
};

// Bits of the symbol's flat-bit encoding, first and last bit, that no select in
// the analyzed node names.
struct UnselectedBits {
  std::uint64_t first = 0;
  std::uint64_t last = 0;
};

// One part of a declaration a text reads or writes: which symbol, which part
// of it, and the names the analyzed text reaches it by.
//
// The symbol says where the access landed in the instance that was analyzed,
// which is not always where the same text lands in another instance: a name
// through an interface port reaches whatever that port is bound to (LRM 25.3),
// and a name leaving the instance reaches whatever stands where it lands (LRM
// 23.8). So the names are kept beside it, as the expression each one starts
// at, for whoever states the access for every instance at once. Empty where
// the access was put together without a text to take them from.
struct AccessedPart {
  const slang::ast::ValueSymbol* symbol = nullptr;
  std::variant<WholePart, SelectedParts, UnselectedBits> part;
  std::vector<const slang::ast::Expression*> reached_by;
};

// What a node reads and what it writes, from outside itself, each stated the
// same way because what consumes a write is a comparison against a read.
struct NodeAccesses {
  std::vector<AccessedPart> reads;
  std::vector<AccessedPart> writes;
};

// Reports the state an `slang::ast::Expression`, a `slang::ast::Statement`, a
// subroutine's body, or a procedure's own text reads -- and, for the last two,
// writes -- from outside itself: what exists before the node runs, which is
// everything declared outside it and every static variable declared inside
// (LRM 6.21). What the node brings into being itself -- an automatic it
// declares, a subroutine's formals and result, an array method's iterator, a
// pattern's binding -- holds nothing until the node runs and nothing outside it
// can write, so an access to one is no access to state. Every caller asks this
// one question; none of them wants the node's own variables.
//
// A read counts wherever the text has it (LRM 9.2.2.2.1, 9.4.2.2). A condition
// a parameter or a generate index happens to settle rules none out, so every
// construction of one body reads alike and wakes on what the standard lists.
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
      -> const std::vector<AccessedPart>&;

  // What resumes the statement an `@*` governs: what it reads (LRM 9.4.2.2),
  // less the variables it uses only to control its `for` loops.
  //
  // The clause lists every identifier the statement reads, and such a variable
  // is read by its loop's condition. Two procedures under `@*` that each run a
  // loop over one variable declared outside both then resume each other on
  // every write of it and never stop, which RTL written before a loop could
  // declare its own variable commonly holds. Its value on entry reaches nothing
  // the statement computes, because the loop assigns it first, so leaving it
  // out changes no value a design that settles computes; it changes how often
  // the statement runs, and whether such a pair settles at all.
  [[nodiscard]] auto AnalyzeImplicitEventList(
      const slang::ast::Statement& stmt,
      const slang::ast::Symbol& containing_symbol)
      -> const std::vector<AccessedPart>&;

  // The state `subroutine`'s body reads and writes from outside a call of it.
  [[nodiscard]] auto AnalyzeAccesses(
      const slang::ast::SubroutineSymbol& subroutine) -> const NodeAccesses&;

  // What an `always_comb` / `always_latch` procedure's own text reads and
  // writes (LRM 9.2.2.2.1, 9.2.2.3), entering no function it calls, with
  // everything it declares left out, static variables too (9.2.2.2.1 a).
  [[nodiscard]] auto AnalyzeProcedureText(
      const slang::ast::ProceduralBlockSymbol& proc) -> const NodeAccesses&;

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
  std::unordered_map<const slang::ast::Expression*, std::vector<AccessedPart>>
      expression_cache_;
  std::unordered_map<const slang::ast::Statement*, std::vector<AccessedPart>>
      statement_cache_;
  std::unordered_map<const slang::ast::SubroutineSymbol*, NodeAccesses>
      subroutine_cache_;
  std::unordered_map<const slang::ast::ProceduralBlockSymbol*, NodeAccesses>
      procedure_text_cache_;
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

// The same, before `proc`'s body runs.
[[nodiscard]] auto HoldsStateBeforeItRuns(
    const slang::ast::Symbol& symbol,
    const slang::ast::ProceduralBlockSymbol& proc) -> bool;

}  // namespace lyra::lowering::ast_to_hir
