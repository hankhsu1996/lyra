#pragma once

#include <optional>
#include <string_view>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/class_decl.hpp"
#include "lyra/hir/foreign_export.hpp"
#include "lyra/hir/foreign_import.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class Expression;
class SubroutineSymbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

class UnitLowerer;

// Lowers a slang subroutine (LRM 13) into a hir::SubroutineDecl: its result
// type, its formals as body-local procedural vars carrying their direction, the
// implicit result variable of a non-void function (LRM 13.4.1), and its lowered
// body. The same shape serves a structural-scope subroutine and a class method;
// `frame` is the enclosing context the body is lowered against. The caller
// records the result where it belongs (a scope's subroutine arena, a class's
// method list).
auto LowerSubroutineDecl(
    UnitLowerer& unit_lowerer, const slang::ast::SubroutineSymbol& sym,
    WalkFrame frame) -> diag::Result<hir::SubroutineDecl>;

// Lowers a class constructor (LRM 8.7) into its callable and its optional
// base-construction call as one operation. The base-call is here rather than
// on the caller because its arguments evaluate in the constructor's own
// binding frame -- a formal parameter reference in `super.new(a)` must resolve
// to the same procedural var the body would read -- and only the subroutine
// lowerer holds that frame while the body lowers. `base_call_ast`, when
// present, is the slang `NewClassExpression` returned by
// `ClassType::getBaseConstructorCall()`; the caller retrieves it once and
// hands it in here.
struct ConstructorAndBaseCall {
  hir::SubroutineDecl constructor;
  std::optional<hir::BaseCall> base_call;
};

auto LowerConstructorDecl(
    UnitLowerer& unit_lowerer, const slang::ast::SubroutineSymbol& sym,
    WalkFrame frame, const slang::ast::Expression* base_call_ast)
    -> diag::Result<ConstructorAndBaseCall>;

// Lowers a slang `import "DPI-C"` subroutine (LRM 35.4) into a bodyless
// hir::ForeignImportDecl: the resolved foreign name, the pure property, and the
// ABI projection of its signature. Distinct from LowerSubroutineDecl because a
// DPI import has no SV body; the caller records the result in the scope's
// `foreign_imports` arena, never in its subroutine arena.
auto LowerForeignImport(
    UnitLowerer& unit_lowerer, const slang::ast::SubroutineSymbol& sym)
    -> diag::Result<hir::ForeignImportDecl>;

// Lowers an `export "DPI-C"` binding (LRM 35.5) into a hir::ForeignExportDecl:
// the C linkage name and the ABI projection of the exported subroutine's
// signature. The subroutine keeps its ordinary body and is lowered separately
// as a method; this record drives the foreign-linkage wrapper. `foreign_name`
// is the C identifier slang resolved for the export.
auto LowerForeignExport(
    UnitLowerer& unit_lowerer, const slang::ast::SubroutineSymbol& sym,
    std::string_view foreign_name) -> diag::Result<hir::ForeignExportDecl>;

}  // namespace lyra::lowering::ast_to_hir
