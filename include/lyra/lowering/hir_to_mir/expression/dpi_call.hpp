#pragma once

// DPI-C call lowering (LRM 35): the SystemVerilog / C foreign-language
// boundary. An `import "DPI-C"` call marshals each SV actual to its C ABI
// carrier, calls the external symbol, and marshals results back; an `export
// "DPI-C"` synthesizes the `extern "C"` wrapper foreign code calls to reach an
// SV subroutine. Both sides share the carrier-marshaling vocabulary, kept here
// so the ordinary-call dispatch in `calls.hpp` stays free of the DPI ABI
// surface.

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/foreign_export.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/foreign_export_wrapper.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

class UnitLowerer;

// LRM 35.4 import call: an ordinary call to the external static callable,
// wrapped in boundary marshaling. The carriers and directions are read from the
// callable's own declaration, resolved once at its declaration lowering. A task
// call is a coroutine the caller awaits, so it always sequences through a
// closure; a function call whose every actual is a by-value scalar input is a
// plain expression, and one with any actual that needs a boundary temp (an
// output / inout, or a canonical vector of any direction) sequences through a
// closure.
template <ExprLowerer Lowerer>
auto LowerForeignImportCall(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& c,
    const hir::ForeignImportRef& ref, mir::TypeId result_type)
    -> diag::Result<mir::Expr>;

// Builds the foreign-linkage wrapper for a DPI-C export (LRM 35.5): a callable
// whose C-ABI parameters marshal to the exported subroutine's SV arguments,
// which it calls through its leading context argument, marshaling the result
// back. `target` is the call the body makes -- a class method takes a recovered
// receiver, any other target is a receiver-less package free function taking
// the run's effects -- so it also fixes the leading argument. `context_frame`
// supplies the enclosing class for a receiver (a bare frame otherwise);
// `result_type` is the exported subroutine's result type the writeback
// destructures.
auto SynthesizeForeignExportWrapper(
    UnitLowerer& module, const WalkFrame& context_frame,
    mir::DirectTarget target, mir::TypeId result_type,
    const hir::ForeignExportDecl& export_decl) -> mir::ForeignExportWrapper;

}  // namespace lyra::lowering::hir_to_mir
