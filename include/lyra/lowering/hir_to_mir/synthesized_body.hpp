#pragma once

#include <expected>
#include <optional>
#include <span>
#include <utility>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

// `for (;;) { each }`, appended to `frame`'s block.
template <typename Each>
auto AppendForever(const WalkFrame& frame, Each each) -> diag::Result<void> {
  mir::Block body;
  const WalkFrame body_frame = frame.WithBlock(&body);
  if (auto r = each(body_frame); !r) return r;
  mir::Builder block = frame.Builder();
  block.AppendStmt(
      mir::ForStmt{
          .init = {},
          .condition = std::nullopt,
          .step = {},
          .scope = block.AddScope(std::move(body))});
  return {};
}

// A body of the scope the source wrote none of, on its own object: a coroutine
// taking the object first and then `params`, completing with `payload`, whose
// body `write` appends, its return included, in the role `virtual_dispatch`
// names among the class's methods.
template <typename Write>
auto ScopeCoroutine(
    const StructuralScopeLowerer& lowerer, const WalkFrame& ctor_frame,
    diag::SourceSpan span, std::span<const mir::TypeId> params,
    mir::TypeId payload,
    std::optional<mir::VirtualDispatchRole> virtual_dispatch, Write write)
    -> diag::Result<mir::CallableDecl> {
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  mir::CallableCode code = mir::CallableCode::Defined(span);
  CallableBindings bindings(unit, code);
  const mir::LocalId self_id = bindings.Declare(
      ReceiverOrigin{}, ctor_frame.current_class.cls->self_pointer_type);
  std::vector<mir::LocalId> locals{self_id};
  for (const mir::TypeId param : params) {
    locals.push_back(bindings.Declare(
        SynthesizedOrigin{
            .owner_site = lowerer.Owner().NextSynthesizedSite(), .ordinal = 0},
        param));
  }
  const WalkFrame frame =
      ctor_frame.WithSpan(span).WithBindings(&bindings).WithBlock(&code.Body());
  if (auto written = write(frame, std::span{locals}.subspan(1)); !written) {
    return std::unexpected(std::move(written.error()));
  }
  code.params = std::move(locals);
  code.result_type =
      unit.types.Intern(mir::Type{mir::CoroutineType{.payload = payload}});
  return mir::CallableDecl{
      .code = std::move(code),
      .foreign = std::nullopt,
      .virtual_dispatch = virtual_dispatch,
      .defining_unit = std::nullopt};
}

// An execution of the scope the source wrote none of, which runs `each` for
// ever from its start.
template <typename Each>
auto ScopeExecution(
    const StructuralScopeLowerer& lowerer, const WalkFrame& ctor_frame,
    diag::SourceSpan span, Each each) -> diag::Result<mir::CallableDecl> {
  return ScopeCoroutine(
      lowerer, ctor_frame, span, {}, lowerer.Owner().Unit().builtins.void_type,
      std::nullopt,
      [&](const WalkFrame& frame,
          std::span<const mir::LocalId>) -> diag::Result<void> {
        if (auto r = AppendForever(frame, each); !r) return r;
        frame.Builder().AppendStmt(mir::ReturnStmt{.value = std::nullopt});
        return {};
      });
}

}  // namespace lyra::lowering::hir_to_mir
