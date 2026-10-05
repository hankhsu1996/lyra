#pragma once

// Lowering of what is reached through a virtual interface (LRM 25.9): the
// variables of the instance it holds, the names its modport defines, the
// interfaces that instance instantiates, and the subroutines they declare. The
// virtual interface's type names the interface, so every position is counted
// out of what that interface published where this unit compiles, and only the
// instance is chosen when the access runs.

#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/interface_member_access.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class Scope;
class Symbol;
class VirtualInterfaceType;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// The scope declaring `scope`'s names, reached from the instance the virtual
// interface `handle` holds: that instance where `scope` is its body, or an
// interface it instantiates or a generate block inside it, a step onto each on
// the way down. `place` is where that leaves the descent: the scope of
// another unit it stands on and the named blocks and subroutines of it
// `scope` sits in, under which a name declared in `scope` is counted out of
// what that scope published.
struct HeldDescent {
  hir::InterfaceInstanceAccessExpr instance;
  InExternalScope place;
};

auto DescendThroughHandle(
    UnitLowerer& unit_lowerer, hir::ExprId handle,
    const slang::ast::VirtualInterfaceType& handle_type,
    const slang::ast::Scope& scope, diag::SourceSpan span)
    -> diag::Result<HeldDescent>;

// A member of the interface instance `handle` holds, `handle` being the already
// lowered virtual interface and `member` what the front end resolved the name
// to in the interface the handle's type names, or in the modport it selects.
// The position is counted out of what that interface published, so a name it
// did not publish has nothing to compile against.
auto LowerVirtualInterfaceMember(
    UnitLowerer& unit_lowerer, WalkFrame frame, hir::ExprId handle,
    const slang::ast::VirtualInterfaceType& handle_type,
    const slang::ast::Symbol& member, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;

// What a process waiting on `member` of the interface instance `handle` holds
// waits on (LRM 9.4.2): the variable itself, or, for a name a view defines,
// every member of the instance a change to that name is a change to -- the same
// members a wait on the name through an interface port watches. Which instance
// they sit in is known each time the wait evaluates the handle.
auto WatchedThroughHandle(
    UnitLowerer& unit_lowerer, hir::ExprId handle,
    const slang::ast::VirtualInterfaceType& handle_type,
    const slang::ast::Symbol& member, diag::SourceSpan span)
    -> diag::Result<std::vector<hir::InterfaceMemberAccessExpr>>;

}  // namespace lyra::lowering::ast_to_hir
