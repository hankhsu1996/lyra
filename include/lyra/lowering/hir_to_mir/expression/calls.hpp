#pragma once

// Lowering of call expressions (LRM 13): user-defined subroutine calls,
// builtin-method calls (enum / string / event / array / iterator), and
// system-subroutine calls. The system-subroutine arm fans out to the
// per-family handlers under `expression/system/*.hpp`; this header is the
// single dispatch surface every call site recurses through.

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/foreign_export.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/foreign_export_wrapper.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

class UnitLowerer;

// A call's meaning is independent of the enclosing scope, so one template over
// the pass class serves both the process body and the structural-scope
// (continuous-assign) contexts; explicit instantiations live in the
// implementation file. Value system subroutines on the structural path are the
// one form not yet wired.
template <ExprLowerer Lowerer>
auto LowerHirCallExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& c,
    diag::SourceSpan span, mir::TypeId result_type) -> diag::Result<mir::Expr>;

// Builds the foreign-linkage wrapper for a DPI-C export (LRM 35.5): a callable
// whose C-ABI parameters marshal to the exported subroutine's SV arguments,
// which it calls through its leading context argument, marshaling the result
// back. `target` is the call the body makes -- a class method takes a recovered
// receiver, any other target is a receiver-less package free function taking
// the run's services -- so it also fixes the leading argument. `context_frame`
// supplies the enclosing class for a receiver (a bare frame otherwise);
// `result_type` is the exported subroutine's result type the writeback
// destructures.
auto SynthesizeForeignExportWrapper(
    UnitLowerer& module, const WalkFrame& context_frame,
    mir::DirectTarget target, mir::TypeId result_type,
    const hir::ForeignExportDecl& export_decl) -> mir::ForeignExportWrapper;

}  // namespace lyra::lowering::hir_to_mir
