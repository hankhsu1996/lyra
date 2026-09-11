#pragma once

#include <optional>
#include <span>
#include <string>
#include <string_view>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/class_decl.hpp"
#include "lyra/hir/foreign_export.hpp"
#include "lyra/hir/foreign_import.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class Expression;
class FormalArgumentSymbol;
class MethodPrototypeSymbol;
class SubroutineSymbol;
class Symbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

class UnitLowerer;

// Classifies a slang formal's argument direction into its HIR ParamDirection
// (LRM 13.5). slang has no ConstRef direction (LRM 13.5.2): a `const ref`
// formal carries direction Ref with the Const variable flag, so const-ness is
// read off the formal rather than the direction enum alone. Shared by the
// subroutine-declaration lowering and the cross-unit call site, which recompute
// the same directions from the same formals.
auto ParamDirectionOf(const slang::ast::FormalArgumentSymbol& formal)
    -> hir::ParamDirection;

// True when a modport item names something the view itself defined. LRM 25.5.4
// makes an item written as a plain identifier serve twice, "as both a reference
// to an interface item and a port identifier", so it denotes that item and the
// view changes nothing about how it is reached; only an item the view wrote an
// expression for denotes something no member of the interface answers to. The
// promise, the bodies carrying it out, and every reference to it all ask this,
// so it is spelled once and the three cannot disagree about which names a view
// defines.
auto ViewDefinesTheName(const slang::ast::Symbol& item) -> bool;

// The subroutine an interface evaluates one name a view offers only for reading
// in (LRM 25.5.4), named from the view and the name. The declaring unit both
// promises it and builds it, so the two are spelled through one function and
// cannot describe different subroutines.
auto ModportReadName(std::string_view modport, std::string_view port)
    -> std::string;

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

// The arguments a base construction carries, read off whichever expression the
// front end resolved it into: a `super.new(...)` the source wrote is a
// new-class expression, while arguments written on the extends specifier
// (LRM 8.17) are bound as an ordinary call of the base constructor. Both are
// fully bound, so what this answers already carries whatever the call left to a
// default.
auto BaseCallArguments(const slang::ast::Expression& base_call)
    -> std::span<const slang::ast::Expression* const>;

// Lowers a class constructor (LRM 8.7) into its callable and its optional
// base-construction call as one operation. The base-call is here rather than
// on the caller because its arguments evaluate in the constructor's own
// binding frame -- a formal parameter reference in `super.new(a)` must resolve
// to the same procedural var the body would read -- and only the subroutine
// lowerer holds that frame while the body lowers. `base_call_ast`, when
// present, is what `ClassType::getBaseConstructorCall()` answered; the caller
// retrieves it once and hands it in here.
struct ConstructorAndBaseCall {
  hir::SubroutineDecl constructor;
  std::optional<hir::BaseCall> base_call;
};

auto LowerConstructorDecl(
    UnitLowerer& unit_lowerer, const slang::ast::SubroutineSymbol& sym,
    WalkFrame frame, const slang::ast::Expression* base_call_ast)
    -> diag::Result<ConstructorAndBaseCall>;

// Lowers a slang `pure virtual` class method prototype (LRM 8.21) into a
// hir::SubroutineDecl with `is_prototype = true` and `is_virtual = true`.
// The signature -- return type, parameter list, and the parameter binding
// arena -- is populated so a backend can emit the C++ pure declaration; the
// body arenas are left at construction default. The caller records the
// result in the owning class's method arena, exactly like an ordinary
// instance method, and sets `overrides` if slang identified the prototype
// as re-declaring an existing pure slot in an abstract descendant.
auto LowerMethodPrototypeDecl(
    UnitLowerer& unit_lowerer, const slang::ast::MethodPrototypeSymbol& proto,
    WalkFrame class_frame) -> diag::Result<hir::SubroutineDecl>;

// Lowers a slang `import "DPI-C"` subroutine (LRM 35.4) into a bodyless
// hir::ForeignImportDecl: the resolved foreign name, the pure property, and the
// ABI projection of its signature. Distinct from LowerSubroutineDecl because a
// DPI import has no SV body; the unit records the result among its foreign
// imports, never in a scope's subroutine arena. The result reads only the
// declaration, so every unit that lowers the same declaration gets the same
// record.
auto LowerForeignImport(
    UnitLowerer& unit_lowerer, const slang::ast::SubroutineSymbol& sym)
    -> diag::Result<hir::ForeignImportDecl>;

// Lowers an `export "DPI-C"` binding (LRM 35.5) into a hir::ForeignExportDecl:
// the C linkage name and the ABI projection of the exported subroutine's
// signature. The subroutine keeps its ordinary body and is lowered separately
// as a method; this record drives the foreign-linkage wrapper. `subroutine` is
// the identity the declaring scope gave that body, and `foreign_name` is the C
// identifier slang resolved for the export.
auto LowerForeignExport(
    UnitLowerer& unit_lowerer, const slang::ast::SubroutineSymbol& sym,
    hir::StructuralSubroutineId subroutine, std::string_view foreign_name)
    -> diag::Result<hir::ForeignExportDecl>;

}  // namespace lyra::lowering::ast_to_hir
