#pragma once

#include <string>

#include "lyra/diag/source_manager.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/lowering/hir_to_mir/module_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
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

// Builds the engine-handle expression `self.Services()`: a `Services`
// scope-method call whose receiver is the body's self read, interned as a
// child. Returns the call node detached; the caller interns it. Runtime-effect
// generic calls -- $finish, the $time family, named-event trigger / triggered
// -- thread it as the engine handle. It needs only the module and the walk
// frame, not the pass class, so it serves the process body and structural-scope
// (continuous-assign) call paths identically.
auto BuildServicesCallExpr(const ModuleLowerer& module, const WalkFrame& frame)
    -> mir::Expr;

// Builds the file-IO broker expression `services.Files()`: a `Files` method
// call on the engine handle, typed as the unit's `files` builtin. Returns the
// outer call detached; the caller interns it. The inner services call is
// interned into `frame.current_block` as a child. LRM 21.3 file-IO ops and
// LRM 21.2.1 / 21.3.1 sink writes thread the resulting handle as the receiver
// of their FileTable methods.
auto BuildFilesCallExpr(const ModuleLowerer& module, const WalkFrame& frame)
    -> mir::Expr;

// Builds the diagnostic broker expression `services.Diagnostic()`: a
// `Diagnostic` method call on the engine handle, typed as the unit's
// `diagnostic` builtin. Returns the outer call detached; the caller interns
// it. The inner services call is interned into `frame.current_block` as a
// child. LRM 20.10 `$info` / `$warning` / `$error` thread the resulting
// handle as the receiver of their severity-fixed Emit methods.
auto BuildDiagnosticCallExpr(
    const ModuleLowerer& module, const WalkFrame& frame) -> mir::Expr;

// Builds the format-text expression `value::Format(items,
// services.TimeFormat())`: a value-layer free call over the print-item array
// that yields an SV `string`, taking the engine's `$timeformat` state as an
// explicit operand. The `TimeFormat` reader call is interned into `block` as a
// child; the outer Format call is returned detached for the caller to intern.
// The caller supplies `services_id` because its hop depth varies by call site
// (a process body reads `self` directly, a deferred-check cascade body one hop
// up). LRM 20.4.3 / 21.2.1: the `$display`, `$info`, and `$sformat` families
// and the deferred-check cascade all format through this one path.
auto BuildFormatCallExpr(
    const mir::CompilationUnit& unit, mir::Block& block,
    mir::ExprId services_id, mir::ExprId items_array) -> mir::Expr;

}  // namespace lyra::lowering::hir_to_mir
