#pragma once

#include <optional>
#include <vector>

#include "lyra/base/component_index.hpp"
#include "lyra/hir/external_callee.hpp"
#include "lyra/hir/param_direction.hpp"
#include "lyra/hir/subroutine_kind.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::hir {
struct SubroutineDecl;
}  // namespace lyra::hir

namespace lyra::lowering::hir_to_mir {

class UnitLowerer;

// The type a call to a subroutine yields, which is how the call states its
// protocol: enabling a task produces a coroutine, because control returns to
// the enabler only once the task completes (LRM 13.3), while calling a function
// produces its result directly. Every call site types its call through here so
// that whether a callee suspends its caller is readable from the call's type
// rather than re-decided wherever the call is built.
auto SubroutineCallType(
    mir::CompilationUnit& unit, hir::SubroutineKind kind,
    mir::TypeId result_type) -> mir::TypeId;

// The type a call hands its caller: the product of the values the callee
// completes with -- its return value, if it has one, followed by each `output`
// / inout value in declaration order (LRM 13.5). It is a product at every
// count, including none, so that a completion is a value a caller can always
// bind and project from, and so that the count is read off the type rather than
// carried beside it.
auto CompletionPayloadType(
    mir::CompilationUnit& unit, const std::vector<mir::TypeId>& components)
    -> mir::TypeId;

// Reads one payload component out of a completion value bound to `completion`.
auto ProjectCompletionComponent(
    mir::Block& block, mir::LocalId completion, mir::TypeId payload_type,
    base::ComponentIndex index, mir::TypeId component_type) -> mir::ExprId;

// One formal as a completion is derived from it: how the call transfers it and
// what type it carries. Both HIR spellings of a callee's formals -- a
// declaration's own, and the interface a by-name reference records -- normalize
// to this, so one rule derives every completion.
struct CalleeFormal {
  hir::ParamDirection direction = hir::ParamDirection::kInput;
  mir::TypeId type{};
};

// The completion a callee hands back. `components` is the ordered product it
// completes with: the result, when it has one, then whatever each formal
// receives (LRM 13.5). Each formal states the component position it receives,
// absent when it receives none -- so a consumer reads a position rather than
// counting its way to one, and a completion of no components needs no one to
// ask whether there are any.
struct CompletionLayout {
  struct Formal {
    hir::ParamDirection direction = hir::ParamDirection::kInput;
    mir::TypeId type{};
    std::optional<base::ComponentIndex> component;
  };
  std::vector<Formal> formals;
  std::vector<mir::TypeId> components;
};

// Derives the completion from a callee's interface. This is the sole statement
// of which formals hand a value back: `output` and `inout` do at return, while
// `input` only passes one in and `ref` / `const ref` alias the caller's own
// storage, which the callee has already written.
auto BuildCompletionLayout(
    const std::vector<CalleeFormal>& formals,
    std::optional<mir::TypeId> result_type) -> CompletionLayout;

// The type a formal carries as a parameter of the callable, or nothing where it
// is no parameter at all. An `input` and an `inout` carry the formal's value
// type; a `ref` / `const ref` carries a reference to it, being a live alias of
// the caller's storage (LRM 13.5.2); an `output` carries nothing, being a body
// local whose final value rides the completion instead (LRM 13.5). This is the
// sole statement of which formals are parameters, so a definition and the
// prototype it implements cannot declare different parameter lists.
auto ParamTypeOf(
    UnitLowerer& unit, hir::TypeId value_type, hir::ParamDirection direction)
    -> std::optional<mir::TypeId>;

// A subroutine declaration's formals, read into the shape a completion derives
// from.
auto CalleeFormalsOf(UnitLowerer& unit, const hir::SubroutineDecl& decl)
    -> std::vector<CalleeFormal>;

// The same, for a callee this unit has no declaration of, read off the
// interface a call recomputed for it.
auto CalleeFormalsOf(
    UnitLowerer& unit, const hir::ExternalCalleeInterface& interface)
    -> std::vector<CalleeFormal>;

// The type a call to `decl` yields: its completion under the protocol its kind
// states. This is the one reading of a declaration every consumer of its
// interface goes through, so a definition, the prototype it implements, and
// every call site cannot state different interfaces.
auto SubroutineCallTypeOf(UnitLowerer& unit, const hir::SubroutineDecl& decl)
    -> mir::TypeId;

}  // namespace lyra::lowering::hir_to_mir
