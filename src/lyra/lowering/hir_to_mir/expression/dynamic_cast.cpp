#include "lyra/lowering/hir_to_mir/expression/dynamic_cast.hpp"

#include <expected>
#include <optional>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/block_builder.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/expression/enum_method.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

template <ExprLowerer Lowerer>
auto LowerHirDynamicCastExpr(
    Lowerer& lowerer, const WalkFrame& frame, const hir::DynamicCastExpr& c,
    mir::TypeId result_type, diag::SourceSpan span) -> diag::Result<mir::Expr> {
  UnitLowerer& owner = lowerer.Owner();
  mir::CompilationUnit& unit = owner.Unit();

  BlockBuilder steps(frame);
  mir::Block& body = steps.Body();

  const hir::TypeId declared = lowerer.HirExprs().Get(c.destination).type;
  const mir::TypeId destination_type = owner.TranslateType(declared);

  auto source_or =
      lowerer.LowerExpr(lowerer.HirExprs().Get(c.source), steps.Frame());
  if (!source_or) return std::unexpected(std::move(source_or.error()));
  const mir::ExprId source = body.exprs.Add(*std::move(source_or));

  // LRM 6.24.2: called as a task the construct tells the design that the
  // assignment it asked for did not happen. It is the design's own error rather
  // than a limit of this compiler, and the standard leaves the destination
  // holding what it held, which is a state the run goes on from.
  const auto report_invalid = [&](mir::Block& into) {
    AppendToolReportStmt(
        owner, into, support::BuiltinFn::kEmitError,
        "$cast: the assignment is invalid, so the destination is left "
        "unchanged (LRM 6.24.2)",
        span);
  };

  // Where the two declared types allow no assignment between them, none is
  // written on any run and the standard defines no conversion to write one with
  // (LRM 6.22.4, 6.22.5). So there is nothing to state but the source, which
  // the source wrote, and the answer the types settled.
  if (std::holds_alternative<hir::NoAssignmentAllowed>(c.validity)) {
    body.AppendStmt(mir::ExprStmt{.expr = source});
    if (c.on_invalid == hir::InvalidAssignmentHandling::kReported) {
      report_invalid(body);
    }
    return steps.Build(BuildIntLiteral(unit, body, 0));
  }
  const hir::RunTimeCheck check =
      std::get<hir::AssignmentAllowed>(c.validity).check;

  // What the destination would take is settled once and then both asked about
  // and stored, so the question and the assignment cannot be about different
  // values. Toward a class the object may not be one of, forming the handle is
  // itself the question, and it refers to nothing where the answer is no.
  const mir::LocalId value_var =
      steps.Bindings().DeclareAnonymous(destination_type);
  const auto bind_value = [&](mir::ExprId converted) {
    body.AppendStmt(mir::LocalDeclStmt{.target = value_var, .init = converted});
  };
  // The value is read in two scopes -- where the check is made and where the
  // store happens -- and an expression belongs to the scope it is interned
  // into, so which one a read lands in is said at every read.
  const auto read_value = [&](mir::Block& into) {
    return into.exprs.Add(mir::MakeLocalRefExpr(value_var, destination_type));
  };

  const mir::ExprId answer = [&] {
    switch (check) {
      case hir::RunTimeCheck::kNone:
        bind_value(ConvertToType(unit, body, source, destination_type));
        return BuildIntLiteral(unit, body, 1);
      case hir::RunTimeCheck::kValueIsAMemberOfTheEnumeration:
        bind_value(ConvertToType(unit, body, source, destination_type));
        return body.exprs.Add(BuildEnumMembershipCallExpr(
            owner, body, read_value(body), declared));
      case hir::RunTimeCheck::kObjectIsOfTheDestinationClass:
        bind_value(body.exprs.Add(
            mir::Expr{
                .data = mir::DynamicCastExpr{.operand = source},
                .type = destination_type}));
        return read_value(body);
    }
    throw InternalError("LowerHirDynamicCastExpr: unknown run-time check");
  }();
  const mir::LocalId answer_var =
      steps.Bindings().DeclareAnonymous(result_type);
  body.AppendStmt(
      mir::LocalDeclStmt{
          .target = answer_var,
          .init = body.exprs.Add(
              mir::Expr{
                  .data =
                      mir::ConditionalExpr{
                          .condition = ReduceToCondition(unit, body, answer),
                          .then_value = BuildIntLiteral(unit, body, 1),
                          .else_value = BuildIntLiteral(unit, body, 0)},
                  .type = result_type})});
  const auto read_answer = [&] {
    return body.exprs.Add(mir::MakeLocalRefExpr(answer_var, result_type));
  };

  // The destination is reached inside the arm that writes it, because that is
  // the only run on which it is written: an invalid assignment leaves the
  // destination alone, and reaching it is part of writing it.
  mir::Block taken;
  auto target_or = lowerer.LowerLhsExpr(
      lowerer.HirExprs().Get(c.destination), steps.Frame().WithBlock(&taken));
  if (!target_or) return std::unexpected(std::move(target_or.error()));
  const AccessPath target = *std::move(target_or);
  taken.AppendStmt(
      mir::ExprStmt{
          .expr = taken.exprs.Add(
              BuildStoreExpr(unit, taken, target, read_value(taken)))});

  std::optional<mir::BlockId> invalid_scope;
  if (c.on_invalid == hir::InvalidAssignmentHandling::kReported) {
    mir::Block invalid;
    report_invalid(invalid);
    invalid_scope = body.child_scopes.Add(std::move(invalid));
  }

  body.AppendStmt(
      mir::IfStmt{
          .condition = ReduceToCondition(unit, body, read_answer()),
          .then_scope = body.child_scopes.Add(std::move(taken)),
          .else_scope = invalid_scope});

  return steps.Build(read_answer());
}

template auto LowerHirDynamicCastExpr(
    ProcessLowerer&, const WalkFrame&, const hir::DynamicCastExpr&, mir::TypeId,
    diag::SourceSpan) -> diag::Result<mir::Expr>;
template auto LowerHirDynamicCastExpr(
    const StructuralScopeLowerer&, const WalkFrame&,
    const hir::DynamicCastExpr&, mir::TypeId, diag::SourceSpan)
    -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
