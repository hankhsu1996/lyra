#pragma once

// Lowering of assertion statements:
//   - Simple immediate assert / assume / cover (LRM 16.3)
//   - Concurrent assert / assume / cover (LRM 16.14)

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class ConcurrentAssertionStatement;
class ImmediateAssertionStatement;
class ProceduralBlockSymbol;
class StatementBlockSymbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// The assertion a procedure is, where its enabling condition is 1, together
// with the block a statement label put around it.
//
// `assertion` is null for every procedure that is not one. A concurrent
// assertion written as a module item is `always` around that assertion (LRM
// 16.14.5), and an `always` whose whole content is one concurrent assertion
// reaches it on every pass and under no condition, which is the same thing: the
// enabling condition is what separates the two placements and here there is
// none. The front end presents both spellings as a procedure, so the reading is
// of what the procedure contains rather than of which syntax produced it.
//
// `named_block` is the block a label named (LRM 9.3.5), and is null where the
// source wrote no label. Reaching the assertion means passing through whatever
// encloses it, and a named block is not scenery: its identity is minted before
// any body lowers, because a name is what something else can reach it by, so
// the walk that passes through one is what fills that identity in.
struct StaticConcurrentAssertion {
  const slang::ast::ConcurrentAssertionStatement* assertion = nullptr;
  const slang::ast::StatementBlockSymbol* named_block = nullptr;
};

[[nodiscard]] auto StaticConcurrentAssertionOf(
    const slang::ast::ProceduralBlockSymbol& proc) -> StaticConcurrentAssertion;

auto LowerImmediateAssertionStmt(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::ImmediateAssertionStatement& as, diag::SourceSpan span)
    -> diag::Result<hir::Stmt>;

// The assertion itself, for both the form a procedure runs and the form a scope
// declares. What differs between them is where the statements an outcome
// selects live, which is the caller's own body either way.
auto LowerConcurrentAssertion(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::ConcurrentAssertionStatement& as, diag::SourceSpan span)
    -> diag::Result<hir::ConcurrentAssertion>;

}  // namespace lyra::lowering::ast_to_hir
