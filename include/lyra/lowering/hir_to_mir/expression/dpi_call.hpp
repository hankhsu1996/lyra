#pragma once

// DPI-C call lowering (LRM 35): the SystemVerilog / C foreign-language
// boundary. An `import "DPI-C"` call marshals each SV actual to its C ABI
// carrier, calls the foreign symbol, and marshals results back; an `export
// "DPI-C"` synthesizes the C entry point foreign code calls to reach an SV
// subroutine. Both sides share the carrier-marshaling vocabulary, kept here so
// the ordinary-call dispatch stays free of the DPI ABI surface.

#include <optional>
#include <span>
#include <string>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/foreign_export.hpp"
#include "lyra/hir/foreign_import.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/foreign_linkage.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/dpi_abi.hpp"

namespace lyra::lowering::hir_to_mir {

class UnitLowerer;
class ProcessLowerer;

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

// The one import call above that is not an expression: a function whose
// boundary needs body locals and whose foreign side returns nothing. Crossing
// the boundary is the whole of what it does, so it lowers to a run of
// statements with no value for an enclosing expression to have wanted. Returns
// nullopt for every other import call, which the expression form lowers.
auto LowerForeignImportCallStmtForm(
    ProcessLowerer& lowerer, WalkFrame frame,
    const std::optional<std::string>& label, const hir::CallExpr& c,
    const hir::ForeignImportRef& ref) -> std::optional<diag::Result<mir::Stmt>>;

// The C-ABI adaptation of one exported subroutine (LRM 35.5): the entry's own
// signature and body, the program-global name the foreign side reaches it by,
// and where that name's definition lives. Kept as those facts rather than a
// finished declaration because a subroutine of a scope is published as an entry
// the scope holds and a package's as a linked symbol of the package's own
// namespace, which is the caller's to place -- but which of the two it is
// follows the target it dispatches into, which is settled here.
struct ForeignExportEntry {
  mir::CallableCode code;
  mir::ForeignLinkage linkage;
  mir::ForeignDefinition definition;
};

// Builds that adaptation: C-ABI parameters marshal to the exported subroutine's
// SV arguments, the subroutine is called through the entry's leading context
// argument, and the result marshals back. `target` is the call the body makes
// -- a subroutine of a scope takes a `self` receiver, which the entry takes as
// its own first parameter; any other target is a receiver-less package free
// function taking the run's effects -- so it also fixes that leading argument.
// `context_frame` supplies the enclosing class for a receiver (a bare frame
// otherwise); `result_type` is the exported subroutine's result type the
// writeback destructures.
auto SynthesizeForeignExportEntry(
    UnitLowerer& module, const WalkFrame& context_frame,
    mir::DirectTarget target, mir::TypeId result_type,
    const hir::ForeignExportDecl& export_decl) -> ForeignExportEntry;

}  // namespace lyra::lowering::hir_to_mir
