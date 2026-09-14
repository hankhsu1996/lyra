#pragma once

#include <memory>
#include <optional>
#include <string>
#include <vector>

#include <slang/ast/Compilation.h>
#include <slang/driver/Driver.h>

#include "lyra/diag/sink.hpp"
#include "lyra/frontend/load.hpp"
#include "lyra/frontend/slang_source_mapper.hpp"
#include "lyra/lowering/ast_to_hir/lower.hpp"
#include "lyra/support/assertion_policy.hpp"

namespace lyra::compiler {

// What reading the elaborated AST produces: which units the design begins at,
// and the HIR of every unit. Neither member carries a front-end node, so the
// AST is read by one call and has to outlive nothing but it.
struct ElaboratedDesign {
  std::vector<lowering::ast_to_hir::TopLevelUnit> tops;
  lowering::ast_to_hir::HirCompilation hir;
};

// What the front end answered with, and its own account of answering. The
// elaboration is absent exactly when the front end refused the source, and the
// account says why.
//
// The account is worth having either way: a design that elaborates can still
// have produced warnings, and whether those reach the terminal is the caller's
// to decide.
struct FrontEndResult {
  std::optional<frontend::ParseResult> elaborated;
  std::string diagnostics;
};

// Choices about how to lower that the design does not make for itself.
struct LoweringPolicy {
  support::AssertionPolicy assertions = support::AssertionPolicy::kCheck;
};

// Reads everything the driver was pointed at and elaborates it, rendering
// every slang diagnostic once so that the engine's error count is the whole
// account of the front end.
//
// The driver arrives configured -- its options parsed, its sources named --
// and owns the text every resulting span points into, so it must outlive what
// comes back.
auto RunFrontEnd(slang::driver::Driver& driver) -> FrontEndResult;

// Lowers the whole elaborated compilation to a flat set of self-contained HIR
// units -- every package, then every module body -- each tagged with its kind.
// Every unit is attempted, so what the sink holds afterwards is the whole
// account of what this compilation cannot lower, and what comes back when any
// of them failed is nothing.
//
// The elaborated AST is taken rather than borrowed because its lifetime ends
// here: no IR carries a front-end node, and a diagnostic resolves its spans
// through the source manager instead. A whole design's worth of AST is among
// the largest things a run ever holds.
//
// Everything below HIR is per unit and belongs to whoever consumes it, so it
// is driven separately: a caller that wants MIR or an executable form asks for
// that over what comes back here, and takes each unit as it is produced.
auto LowerToHir(
    std::unique_ptr<slang::ast::Compilation> elaborated,
    const frontend::SlangSourceMapper& source_mapper, LoweringPolicy policy,
    diag::DiagnosticSink& sink) -> std::optional<ElaboratedDesign>;

}  // namespace lyra::compiler
