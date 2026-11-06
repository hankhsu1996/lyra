#pragma once

#include <optional>
#include <variant>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/ast_to_hir/expression/expr_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class ClockingBlockSymbol;
class ClockVarSymbol;
class Expression;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// The virtual interface `handle` holds, which a name reaches a clocking block
// of its instance through (LRM 25.9).
struct ThroughHandle {
  const slang::ast::Expression* handle = nullptr;
};

// How the text naming a clocking block, or one of its clockvars, reaches the
// block: through a virtual interface, or by a name whose route starts where a
// route origin says (LRM 23.6, 25.3). Either reaches every part of the block,
// which is how a use names what the block holds but the source never names --
// its event, an output's driving variable and its landing.
struct ClockingNaming {
  const slang::ast::ClockingBlockSymbol* block = nullptr;
  std::variant<ThroughHandle, RouteOrigin> reach;
};

// Whether `expr` is a name standing for a clocking block itself, as an event
// control's operand writes one (LRM 14.10), rather than for one of its
// clockvars.
[[nodiscard]] auto NamesClockingBlock(const slang::ast::Expression& expr)
    -> bool;

// How `naming` reaches its clocking block, where it stands for the block (an
// event control's operand, LRM 14.10) or for a clockvar, selected or not (a
// drive's target, LRM 14.16); nothing where it stands for neither.
[[nodiscard]] auto ClockingNamingOf(
    UnitLowerer& unit_lowerer, const WalkFrame& frame,
    const slang::ast::Expression& naming) -> std::optional<ClockingNaming>;

// What a use of a clocking block reaches that the text naming the block does
// not name itself, each a variable the block holds: the block's event (LRM
// 14.10), and the variable inside the block an output drives its signal from,
// which a drive of the output writes (LRM 14.16).
struct ClockingBlockEvent {};
struct ClockingDrivingVariable {
  const slang::ast::ClockVarSymbol* clockvar = nullptr;
};
using ClockingPart = std::variant<ClockingBlockEvent, ClockingDrivingVariable>;

// A reference to `part`, of type `type`, reached as `naming` reaches its block.
template <ExprLowerer Lowerer>
auto LowerClockingPart(
    Lowerer& lowerer, WalkFrame frame, const ClockingNaming& naming,
    const ClockingPart& part, hir::TypeId type, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;

// A reference to the event the block `naming` reaches stands for (LRM 14.10),
// which an event control waits on and a cycle delay counts (LRM 14.11).
template <ExprLowerer Lowerer>
auto LowerClockingBlockEvent(
    Lowerer& lowerer, WalkFrame frame, const ClockingNaming& naming,
    diag::SourceSpan span) -> diag::Result<hir::Expr>;

// What lands a drive of the output `clockvar` (LRM 14.16) on the instance of
// the scope declaring its block that `naming` reaches. `written` is the part of
// the drive's target that names the output's driving variable, as
// `LowerClockingPart` built it into `frame`'s arena: through a virtual
// interface the landing is made on the instance that access reaches.
auto LandingOf(
    UnitLowerer& unit_lowerer, const WalkFrame& frame,
    const ClockingNaming& naming, const slang::ast::ClockVarSymbol& clockvar,
    hir::ExprId written, diag::SourceSpan span)
    -> diag::Result<hir::ClockingLanding>;

}  // namespace lyra::lowering::ast_to_hir
