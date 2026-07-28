#pragma once

// DPI-C call lowering (LRM 35): the SystemVerilog / C foreign-language
// boundary. An `import "DPI-C"` call marshals each SV actual to its C ABI
// carrier, calls the foreign symbol, and marshals results back; an `export
// "DPI-C"` synthesizes the C entry point foreign code calls to reach an SV
// subroutine. Both sides share the carrier-marshaling vocabulary, kept here so
// the ordinary-call dispatch stays free of the DPI ABI surface.

#include <span>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/foreign_export.hpp"
#include "lyra/hir/foreign_import.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/dpi_abi.hpp"

namespace lyra::lowering::hir_to_mir {

class UnitLowerer;

// The signature a DPI-C declaration publishes to the C side (LRM 35.5.6): one
// binding per formal, typed as the value crosses the boundary, plus the type
// the entry point returns. It is the signature alone; a direction that defines
// the callable adds its body on top. Carrying the prototype as an ordinary
// signature is what lets an import's declaration and an export's definition
// render through one path, so the two can never disagree.
auto MakeForeignSignature(
    mir::CompilationUnit& unit, std::span<const hir::DpiParamAbi> params,
    support::DpiScalarAbi ret_abi, bool is_task) -> mir::CallableCode;

// The bodyless callable that publishes an import's foreign prototype (LRM
// 35.4). The unit owns it, because the DPI-C name space is program-global and
// contains no class; a backend emits it as the declaration the foreign call
// resolves against, and the user's C supplies the definition.
auto MakeForeignImportDecl(
    mir::CompilationUnit& unit, const hir::ForeignImportDecl& import)
    -> mir::CallableDecl;

// LRM 35.4 import call: a call to the foreign symbol, wrapped in boundary
// marshaling. The marshaling is built here, at the call, because an open-array
// formal leaves a dimension unsized (LRM 35.5.6.1) and the actual's own static
// type is what fixes it -- a fact only the call site holds. A boundary that
// needs body locals of its own lowers to a statement sequence; one that needs
// none is a plain expression.
template <ExprLowerer Lowerer>
auto LowerForeignImportCall(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& c,
    const hir::ForeignImportRef& ref, mir::TypeId result_type)
    -> diag::Result<mir::Expr>;

// Builds the C entry point of a DPI-C export (LRM 35.5): a receiver-less
// callable carrying foreign linkage, whose C-ABI parameters marshal to the
// exported subroutine's SV arguments, which it calls through its leading
// context argument, marshaling the result back. `target` is the call the body
// makes -- a class method takes a recovered receiver, any other target is a
// receiver-less package free function taking the run's effects -- so it also
// fixes the leading argument. `context_frame` supplies the enclosing class for
// a receiver (a bare frame otherwise); `result_type` is the exported
// subroutine's result type the writeback destructures.
auto SynthesizeForeignExportEntry(
    UnitLowerer& module, const WalkFrame& context_frame,
    mir::DirectTarget target, mir::TypeId result_type,
    const hir::ForeignExportDecl& export_decl) -> mir::CallableDecl;

}  // namespace lyra::lowering::hir_to_mir
