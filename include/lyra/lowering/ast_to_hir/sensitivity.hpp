#pragma once

#include <cstdint>
#include <unordered_map>
#include <utility>
#include <vector>

#include "lyra/hir/stmt.hpp"

namespace slang::ast {
class Compilation;
class Expression;
class Statement;
class ValueSymbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

class ScopeStack;
class UnitLoweringState;

// Mirrors slang's ReadRange. Multiple entries for the same symbol with
// disjoint bit ranges are kept as-is so downstream can preserve precision
// when the runtime supports bit-level subscription.
struct SensitivityRead {
  const slang::ast::ValueSymbol* symbol;
  std::pair<std::uint64_t, std::uint64_t> bit_range;
};

// Precomputed sensitivity reads, harvested by driving slang's flow analysis
// from `setCustomDFAProvider` + `addListener`. See
// `docs/decisions/read-set-inference.md` for the architecture rationale.
//
// Two keying dimensions matching slang's two analyzable AST hierarchies:
//   - `Statement` keys: the whole-statement read set produced by analyzing
//     a procedure body. always_comb / always_latch (LRM 9.2.2.2.1) and `@*`
//     regions (LRM 9.4.2.2) live here.
//   - `Expression` keys: the read set produced by analyzing one expression
//     subtree. `wait (cond)` (LRM 9.4.3), continuous-assignment
//     `AssignmentExpression` (LRM 10.3), future `wait_order` event-list
//     expressions, and future property / assertion expressions live here.
//
// The store routes internally via overloaded `Insert` / `Lookup` -- callers
// pass whatever AST handle they have. The two buckets exist because slang's
// `Statement` and `Expression` are disjoint hierarchies; a single map key
// type would require losing type safety.
class SensitivityReadStore {
 public:
  void Insert(
      const slang::ast::Statement& stmt, std::vector<SensitivityRead> reads) {
    by_statement_.emplace(&stmt, std::move(reads));
  }
  void Insert(
      const slang::ast::Expression& expr, std::vector<SensitivityRead> reads) {
    by_expression_.emplace(&expr, std::move(reads));
  }

  // Lookup returns nullptr if no reads have been recorded for the given key.
  // Empty result is legal -- the lowering side interprets it as "no
  // sensitivity", which materialises as a `SensitivityWaitStmt` with an
  // empty list (i.e. wait forever).
  [[nodiscard]] auto Lookup(const slang::ast::Statement& stmt) const
      -> const std::vector<SensitivityRead>* {
    const auto it = by_statement_.find(&stmt);
    return it == by_statement_.end() ? nullptr : &it->second;
  }
  [[nodiscard]] auto Lookup(const slang::ast::Expression& expr) const
      -> const std::vector<SensitivityRead>* {
    const auto it = by_expression_.find(&expr);
    return it == by_expression_.end() ? nullptr : &it->second;
  }

 private:
  std::unordered_map<const slang::ast::Statement*, std::vector<SensitivityRead>>
      by_statement_;
  std::unordered_map<
      const slang::ast::Expression*, std::vector<SensitivityRead>>
      by_expression_;
};

// Drives slang's flow analysis over `compilation` and produces the
// sensitivity store the AST -> HIR lowering will look up against. See
// `docs/decisions/read-set-inference.md`.
auto BuildSensitivityReadStore(slang::ast::Compilation& compilation)
    -> SensitivityReadStore;

// Translates the slang-side read vector (ValueSymbol* + bit_range) into
// HIR-side identity entries (StructuralVarRef + bit_range), resolving each
// symbol through the unit's structural binding table and the active scope
// stack. Reads that don't resolve to a visible structural variable are
// silently skipped -- slang may surface reads whose home scope was not
// elaborated into HIR (e.g. ports of an instance the caller is not in).
auto TranslateSensitivityReads(
    const std::vector<SensitivityRead>& reads,
    const UnitLoweringState& unit_state, const ScopeStack& stack)
    -> std::vector<hir::SensitivityEntry>;

}  // namespace lyra::lowering::ast_to_hir
