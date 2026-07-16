#pragma once

// Lowering of call expressions (LRM 13): user-defined subroutine calls,
// builtin-method calls (enum / string / event / array / iterator), and
// system-subroutine calls. The system-subroutine arm fans out to the
// per-family handlers under `expression/system/*.hpp`; this header is the
// single dispatch surface every call site recurses through.

#include <string>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/foreign_export.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/foreign_export_wrapper.hpp"
#include "lyra/mir/method_id.hpp"
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

// Builds the foreign-linkage wrapper for a DPI-C export (LRM 35.5): a
// receiver-less callable whose C-ABI parameters marshal to the exported
// method's SV arguments, which it calls on the recovered receiver, marshaling
// the result back. `ctor_frame.current_class` is the class owning the method;
// `method_id` names it; `instance_name` names the design instance to recover
// the receiver from.
auto SynthesizeForeignExportWrapper(
    UnitLowerer& module, const WalkFrame& ctor_frame, mir::ClassId class_id,
    mir::MethodId method_id, const hir::ForeignExportDecl& export_decl,
    std::string instance_name) -> mir::ForeignExportWrapper;

}  // namespace lyra::lowering::hir_to_mir
