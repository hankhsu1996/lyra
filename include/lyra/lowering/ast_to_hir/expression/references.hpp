#pragma once

// Lowering of name resolution expressions: NamedValue (LRM 6.6 names) and
// HierarchicalValue (LRM 23.6 hierarchical references). Where a named value's
// cell is gets settled once, for every consumer of a name; this file turns
// that answer -- together with the forms that have no cell at all, a folded
// constant, a class property, a pattern binding, the object a subroutine was
// invoked on -- into an Expr. Which declaration a name reaches is the step
// before that, and it is settled here as well, because consumers that build no
// Expr -- a check on what may be written, a read a process is sensitive to --
// ask the same question.

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

// The declaration a name reaches: the named symbol itself, except through a
// modport, where a port identifier lives in the modport's own name space and
// stands for the interface item the modport named it after (LRM 25.5.4). What
// storage that name is, and whether it may be written, are questions about the
// item rather than about the identifier. A modport expression states a shape
// the interface declared rather than one of its items, so it has no declaration
// to answer with.
auto ResolveNamedDeclaration(
    const slang::ast::ValueSymbol& value, diag::SourceSpan span)
    -> diag::Result<const slang::ast::ValueSymbol*>;

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

// `expr` as a name offered by the modport an interface port selected, or
// nothing when it is anything else. Such a name stands for an expression the
// interface evaluates (LRM 25.5.4), so both reading and writing it are the
// interface carrying that out rather than storage the referrer reaches.
auto NameOfferedByModport(const slang::ast::Expression& expr)
    -> const slang::ast::HierarchicalValueExpression*;

// Writing such a name, as the call which performs it. `value` is the already
// lowered right side, which crosses as the call's one argument.
auto LowerModportPortWrite(
    UnitLowerer& unit_lowerer, WalkFrame frame,
    const slang::ast::HierarchicalValueExpression& target, hir::ExprId value,
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
