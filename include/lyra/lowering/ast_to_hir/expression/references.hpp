#pragma once

// Lowering of name resolution expressions: NamedValue (LRM 6.6 names) and
// HierarchicalValue (LRM 23.6 hierarchical references). Where a named value's
// cell is gets settled once, for every consumer of a name; this file turns
// that answer -- together with the forms that have no cell at all, a folded
// constant, a class property, a pattern binding, the object a subroutine was
// invoked on -- into an Expr. What a name denotes is the step before that, and
// it is settled here as well, because a read a process is sensitive to builds
// no Expr and asks the same question.

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class HierarchicalValueExpression;
class NamedValueExpression;
class Symbol;
class Type;
class ValueSymbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// What kind of declaration a name reaches, one value per kind the language
// admits, so that every consumer of a name says what it does about each. Each
// name states the construct and never what this compiler does about it: what a
// kind is good for differs by consumer, so a value fixed at elaboration is a
// folded operand to one and nothing to subscribe to for another, and a name
// carrying a verdict would be wrong for one of them.
enum class Referent {
  // Values fixed before simulation starts (LRM 6.20, 6.19, 6.20.4).
  kParameterConstant,
  kEnumConstant,
  kSpecparam,
  // Declarations with a cell.
  kVariableStorage,
  kNetStorage,
  kClassProperty,
  // The object a subroutine was invoked on (LRM 8.11), which is no cell.
  kThisHandle,
  // An identifier a pattern introduces (LRM 12.6), declared by the pattern.
  kPatternBinding,
  // A name a view computed for itself (LRM 25.5.4). The interface declares it
  // nowhere -- the view adds the name rather than narrowing one the interface
  // declared -- so what it means is the interface's own statement and not a
  // property of any storage this scope can reach.
  kViewDefinedName,
  // Declarations a value reference may denote whose own vocabulary lives in
  // another clause of the standard.
  kPrimitivePort,
  kClockingSignal,
  kAssertionLocal,
  kStructureMember,
  // Not something a value reference can denote at all. Only a value symbol
  // reaches this classification, so no program produces this answer.
  kNotAValue,
};

// What a name resolved to, and what kind of declaration that is. A port
// identifier lives in its modport's own name space and stands for the
// interface item the view named it after (LRM 25.5.4), so an identifier the
// view merely narrowed arrives here as the item; one the view computed arrives
// as the port, which is the only declaration there is for it.
struct NamedReferent {
  const slang::ast::ValueSymbol* symbol;
  Referent kind;
};

// What a name denotes, for every consumer of a name: a read a process is
// sensitive to builds no Expr and asks exactly this question.
//
// A reach this compiler cannot express is not one of these. That is a property
// of the compiler rather than of the name, so it is a refusal raised where the
// reach is attempted.
auto ResolveReferent(
    const slang::ast::ValueSymbol& value, diag::SourceSpan span)
    -> diag::Result<NamedReferent>;

// The refusal for a declaration a value reference may denote and that nothing
// here lowers. Every kind sent here is a construct, so every one names itself
// and cites the clause that defines it -- a reader who meets one can search for
// the construct and ask for it, which a shared sentence gives nobody.
auto FailOnUnsupportedReferent(Referent referent, diag::SourceSpan span)
    -> std::unexpected<diag::Diagnostic>;

// True when `expr` is the `this` keyword (LRM 8.11) -- the handle to the object
// the subroutine it appears in was invoked on. The front end spells it as an
// ordinary variable reference, so a consumer that treats every variable
// reference as storage reaches for a cell that does not exist.
auto NamesCurrentInstance(const slang::ast::Expression& expr) -> bool;

// A member of the current instance reached through `this` (LRM 8.11). The
// keyword qualifies what an unqualified name already reaches -- a property of
// the invoking object, or a value parameter of its specialization -- so the
// qualified spelling lowers to whatever the unqualified one lowers to and no
// handle is formed. A method is not reached here: a call states its receiver
// rather than evaluating one.
auto LowerCurrentInstanceMember(
    UnitLowerer& unit_lowerer, WalkFrame frame,
    const slang::ast::Symbol& member, const slang::ast::Type& type,
    diag::SourceSpan span) -> diag::Result<hir::Expr>;

auto LowerNamedValueProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::NamedValueExpression& named) -> diag::Result<hir::Expr>;

// A hierarchical reference has the same shape across procedural and
// structural contexts (it always resolves into a cross-unit member binding on
// the referrer's structural scope), so this entry is generic over the calling
// context rather than split into Proc / Structural variants.
auto LowerHierarchicalValue(
    UnitLowerer& unit_lowerer, WalkFrame frame,
    const slang::ast::HierarchicalValueExpression& hve)
    -> diag::Result<hir::Expr>;

auto LowerNamedValueStructural(
    UnitLowerer& unit_lowerer, WalkFrame frame,
    const slang::ast::NamedValueExpression& named) -> diag::Result<hir::Expr>;

}  // namespace lyra::lowering::ast_to_hir
