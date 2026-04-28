#include "lyra/lowering/hir_to_mir/lower_process.hpp"

#include <cstddef>
#include <cstdint>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/local_var.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/lower_stmt.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/local_var.hpp"
#include "lyra/mir/process.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto LowerProcessKind(hir::ProcessKind kind) -> mir::ProcessKind {
  switch (kind) {
    case hir::ProcessKind::kInitial:
      return mir::ProcessKind::kInitial;
  }
  throw InternalError("LowerProcessKind: unknown HIR ProcessKind");
}

}  // namespace

auto LowerProcess(
    const UnitLoweringState& unit_state, const ClassLoweringState& class_state,
    const hir::Process& src) -> diag::Result<mir::Process> {
  ProcessLoweringState proc_state;
  BodyLoweringState body_state;

  const mir::LocalScopeId root_scope = body_state.RootScope();
  for (std::size_t i = 0; i < src.local_vars.size(); ++i) {
    const hir::LocalVarId hir_id{static_cast<std::uint32_t>(i)};
    const auto& hir_local = src.local_vars[i];
    const mir::LocalVarId local_id = body_state.AddLocal(
        root_scope, mir::LocalVar{
                        .name = hir_local.name,
                        .type = unit_state.TranslateType(hir_local.type)});
    proc_state.MapLocalVar(
        hir_id, mir::LocalVarRef{.scope = root_scope, .local = local_id});
  }

  const hir::Stmt& root = src.stmts.at(src.body.value);
  auto lowered_root_or =
      LowerStmt(unit_state, class_state, proc_state, body_state, src, root);
  if (!lowered_root_or) {
    return std::unexpected(std::move(lowered_root_or.error()));
  }
  body_state.AddRootStmt(body_state.AddStmt(*std::move(lowered_root_or)));

  return mir::Process{
      .kind = LowerProcessKind(src.kind), .body = body_state.Finish()};
}

}  // namespace lyra::lowering::hir_to_mir
