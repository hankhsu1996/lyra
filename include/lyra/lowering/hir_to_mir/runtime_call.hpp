#pragma once

#include <string>

#include "lyra/diag/source_manager.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/mir/expr.hpp"

namespace lyra::mir {
struct CompilationUnit;
struct Block;
}  // namespace lyra::mir

namespace lyra::lowering::hir_to_mir {

// Formats `span` as "basename:line:col" for the runtime diagnostic origin tag.
// SV simulators print diagnostics with the source file name, not its absolute
// path (LRM 20.10 examples and the Verilator / VCS / Modelsim convention), so
// the runtime origin uses the basename rather than
// `diag::FormatSourceLocation`'s full path. Empty when the span has no
// resolvable file.
[[nodiscard]] auto FormatRuntimeOriginString(
    diag::SourceSpan span, const diag::SourceManager& mgr) -> std::string;

// Builds the engine-handle expression `lyra::runtime::current_runtime()`:
// a zero-argument free-function call the owning Engine backs with a per-
// thread pointer. Returns the call node detached; the caller interns it.
// Every runtime-effect generic call -- `$finish`, the `$time` family,
// named-event trigger / triggered, `$display` and its diagnostic siblings --
// threads the result as the engine handle. Reachable from any body kind,
// so a package function, a class method on a plain SV class, and a class
// static method all use the same runtime-access path as a scope-method
// body.
[[nodiscard]] auto BuildCurrentRuntimeCallExpr(const UnitLowerer& unit_lowerer)
    -> mir::Expr;

// Builds the file-IO broker expression `current_runtime().Files()`: a
// `Files` method call on the engine handle, typed as the unit's `files`
// builtin. Returns the outer call detached; the caller interns it. The
// inner runtime call is interned into `block` as a child. LRM 21.3
// file-IO ops and LRM 21.2.1 / 21.3.1 sink writes thread the resulting
// handle as the receiver of their FileTable methods.
[[nodiscard]] auto BuildFilesCallExpr(
    const UnitLowerer& unit_lowerer, mir::Block& block) -> mir::Expr;

// Builds the diagnostic broker expression `current_runtime().Diagnostic()`:
// a `Diagnostic` method call on the engine handle, typed as the unit's
// `diagnostic` builtin. Returns the outer call detached; the caller
// interns it. The inner runtime call is interned into `block` as a child.
// LRM 20.10 `$info` / `$warning` / `$error` thread the resulting handle as
// the receiver of their severity-fixed Emit methods.
[[nodiscard]] auto BuildDiagnosticCallExpr(
    const UnitLowerer& unit_lowerer, mir::Block& block) -> mir::Expr;

// Builds the format-text expression `value::Format(items,
// runtime.TimeFormat())`: a value-layer free call over the print-item array
// that yields an SV `string`, taking the engine's `$timeformat` state as an
// explicit operand. The `TimeFormat` reader call is interned into `block` as a
// child; the outer Format call is returned detached for the caller to intern.
// The caller supplies `runtime_id` because a chained call already has one
// interned in the surrounding sequence. LRM 20.4.3 / 21.2.1: the `$display`,
// `$info`, and `$sformat` families and the deferred-check cascade all format
// through this one path.
[[nodiscard]] auto BuildFormatCallExpr(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId runtime_id,
    mir::ExprId items_array) -> mir::Expr;

}  // namespace lyra::lowering::hir_to_mir
