#pragma once

#include <cstddef>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/foreign_export.hpp"
#include "lyra/hir/foreign_import.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class CallExpression;
class ClassType;
class Expression;
class FormalArgumentSymbol;
class MethodPrototypeSymbol;
class ModportPortSymbol;
class SubroutineSymbol;
class Symbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

class UnitLowerer;

// The class declaring a subroutine, or none where no class declares it.
auto DeclaringClassOf(const slang::ast::SubroutineSymbol& sym)
    -> const slang::ast::ClassType*;

// The subroutine a class member stands for: a method itself, or the definition
// a prototype (LRM 8.21, 8.24) names; nothing for any other member, or for a
// prototype nothing defines.
auto SubroutineOf(const slang::ast::Symbol& member)
    -> const slang::ast::SubroutineSymbol*;

// Whether a call is made on a class object: through a handle, or an instance
// method named alone or through `super`, which is made on the object the
// calling method runs on (LRM 8.6, 8.15).
auto IsMadeOnAnObject(
    const slang::ast::CallExpression& call,
    const slang::ast::SubroutineSymbol& callee) -> bool;

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
// publication, the bodies carrying it out, and every reference to it all ask
// this, so it is spelled once and the three cannot disagree about which names a
// view defines.
auto ViewDefinesTheName(const slang::ast::Symbol& item) -> bool;

// The expression a view wrote for a name it defines, which is what the name
// stands for.
auto ViewExpressionOf(const slang::ast::ModportPortSymbol& port)
    -> const slang::ast::Expression&;

// The subroutines a unit makes up to evaluate an expression of its own for
// another unit: one name a view offers only for reading (LRM 25.5.4), one
// port's default (LRM 23.2.2.4), one formal's default (LRM 13.5.3), which a
// caller omitting the formal asks the subroutine's own scope or class for, and
// the operand and the `iff` qualifier of one event of a clocking block's
// clocking event, counted from 0 in the order its event list wrote them (LRM
// 14.3, 9.4.2.1). The declaring unit both publishes each and builds it, so the
// two are spelled through one function and cannot describe different
// subroutines. Each name holds a space, which no identifier can, escaped ones
// included (LRM 5.6.1), so it cannot be the name of anything the unit itself
// declares.
auto ModportReadName(std::string_view modport, std::string_view port)
    -> std::string;
auto PortDefaultName(std::string_view port) -> std::string;
auto ArgumentDefaultName(std::string_view subroutine, std::string_view formal)
    -> std::string;
auto ClockingEventName(std::string_view block, std::size_t entry)
    -> std::string;
auto ClockingConditionName(std::string_view block, std::size_t entry)
    -> std::string;

// Whether whatever declares `callee` evaluates the defaults of its formals in a
// function of its own, which a call omitting a formal calls (LRM 13.5.3). A
// subroutine of a scope or a namespace and a method of a class do. A DPI-C
// import has no body of Lyra's to hold one, a constructor's call has no object
// yet, a method the front end or the runtime supplies has no declaration of
// the design's, and an interface class's defaults are constant expressions, the
// same for every class implementing it (LRM 8.26.9); each of those has its
// default lowered as written where the call stands.
auto EvaluatesItsOwnDefaults(const slang::ast::SubroutineSymbol& callee)
    -> bool;

// Whether `call` omitted the argument at `position`, leaving it to the formal's
// default (LRM 13.5.3). The front end fills an omitted argument in by placing
// the default's own expression there, so an argument that is that expression
// is one the call omitted. Only where the callee evaluates its own defaults is
// one answered, and only for an input, the one direction whose value a default
// stands for. Nothing of such an argument is lowered or analyzed where the call
// stands; it is the callee's.
auto OmitsArgument(const slang::ast::CallExpression& call, std::size_t position)
    -> bool;

// The function evaluating the expression `expr` that `holder` holds, in the
// scope or class that wrote it, for a caller that asks for the value: the
// expression names declarations of that scope, which would mean nothing where
// the caller stands, so what crosses is the call and the expression is lowered
// where it was written. It is one return of the expression as `result_type`,
// and takes no formals.
auto LowerEvaluatorDecl(
    UnitLowerer& unit_lowerer, const slang::ast::Symbol& holder,
    std::string name, const slang::ast::Expression& expr,
    hir::TypeId result_type, WalkFrame frame)
    -> diag::Result<hir::SubroutineDecl>;

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

// Lowers a class constructor (LRM 8.7) into its callable and the arguments of
// its base construction as one operation. The arguments are here rather than
// on the caller because they evaluate in the constructor's own binding frame --
// a formal parameter reference in `super.new(a)` must resolve to the same
// procedural var the body would read -- and only the subroutine lowerer holds
// that frame while the body lowers. `base_call_ast`, when present, is what
// `ClassType::getBaseConstructorCall()` answered; the caller retrieves it once
// and hands it in here.
struct ConstructorAndBaseArguments {
  hir::SubroutineDecl constructor;
  std::optional<std::vector<hir::ExprId>> base_arguments;
};

auto LowerConstructorDecl(
    UnitLowerer& unit_lowerer, const slang::ast::SubroutineSymbol& sym,
    WalkFrame frame, const slang::ast::Expression* base_call_ast)
    -> diag::Result<ConstructorAndBaseArguments>;

// Lowers a slang `pure virtual` class method prototype (LRM 8.21) into a
// hir::SubroutineDecl with no statement to enter at. The signature -- return
// type, parameter list, and the parameter binding arena -- is populated so a
// backend can emit the pure declaration. The caller records the result in the
// owning class's method arena, exactly like an ordinary instance method.
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
