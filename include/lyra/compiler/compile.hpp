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

// The design once every unit has declared itself: which units the design
// begins at, and what each unit declared and published, together with the
// elaborated AST their bodies are lowered from.
struct ElaboratedDesign {
  std::vector<lowering::ast_to_hir::TopLevelUnit> tops;
  lowering::ast_to_hir::DeclaredDesign units;
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

// Declares every unit of the elaborated compilation -- every package, then
// every module body -- and settles which of them the design begins at. Every
// unit is attempted, so what the sink holds afterwards is the whole account of
// what this compilation cannot declare, and what comes back when any of them
// failed is nothing.
//
// The elaborated AST is taken because the units read it from here on, until
// the last of them has been lowered, and nothing else does. No IR carries a
// front-end node, and a diagnostic resolves its spans through the source
// manager instead.
//
// Lowering each unit's bodies, and everything below HIR, is per unit and
// belongs to whoever consumes it, so it is driven separately over what comes
// back here.
auto DeclareUnits(
    std::unique_ptr<slang::ast::Compilation> elaborated,
    const frontend::SlangSourceMapper& source_mapper, LoweringPolicy policy,
    diag::DiagnosticSink& sink) -> std::optional<ElaboratedDesign>;

}  // namespace lyra::compiler
