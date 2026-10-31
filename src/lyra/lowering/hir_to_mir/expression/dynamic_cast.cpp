#include "lyra/lowering/hir_to_mir/expression/dynamic_cast.hpp"

#include <expected>
#include <optional>
#include <utility>

#include "lyra/base/diag_message.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/block_builder.hpp"
#include "lyra/lowering/hir_to_mir/callee_interface.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/class_definition.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/expression/enum_method.hpp"
#include "lyra/lowering/hir_to_mir/lvalue.hpp"
#include "lyra/lowering/hir_to_mir/predicate.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr_builders.hpp"
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
  mir::Builder body = steps.Body();

  const hir::TypeId declared = lowerer.HirExprs().Get(c.destination).type;
  const mir::TypeId destination_type = owner.TranslateType(declared);

  auto source_or =
      lowerer.LowerExpr(lowerer.HirExprs().Get(c.source), steps.Frame());
  if (!source_or) return std::unexpected(std::move(source_or.error()));
  const mir::ExprId source = body.Add(*std::move(source_or));

  // LRM 6.24.2: called as a task the construct tells the design that the
  // assignment it asked for did not happen. It is the design's own error rather
  // than a limit of this compiler, and the standard leaves the destination
  // holding what it held, which is a state the run goes on from.
  const auto report_invalid = [&](mir::Builder into) {
    AppendToolReportStmt(
        owner, into, support::BuiltinFn::kEmitError,
        base::DiagMessage(
            "the '$cast' assignment is invalid, so the destination is left "
            "unchanged"),
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
    return steps.Build(mir::BuildIntLiteral(unit, body, 0));
  }
  const hir::RunTimeCheck check =
      std::get<hir::AssignmentAllowed>(c.validity).check;

  // A destination of `dynamic_type` is of a type only the run knows (LRM
  // 15.4.9), so it is the destination that answers: it takes the value where
  // the two are of one identity, which asks for it as a reference.
  if (check == hir::RunTimeCheck::kValueIsOfAnEquivalentType) {
    auto destination_or = lowerer.LowerLhsExpr(
        lowerer.HirExprs().Get(c.destination), steps.Frame());
    if (!destination_or) {
      return std::unexpected(std::move(destination_or.error()));
    }
    const mir::ExprId reference = AtType(
        unit, body, PathReference(unit, body, *destination_or),
        *ParamTypeOf(unit, destination_type, hir::ParamDirection::kRef));
    const mir::ExprId received = body.Add(
        mir::MakeCallExpr(
            mir::Direct{.target = support::BuiltinFn::kReceiveAny},
            {reference, source}, unit.builtins.machine_bool));
    if (c.on_invalid == hir::InvalidAssignmentHandling::kReported) {
      mir::Block invalid;
      report_invalid(body.Into(invalid));
      body.AppendStmt(
          mir::IfStmt{
              .condition = BuildLogicalNot(unit, body, received),
              .then_scope = body.AddScope(std::move(invalid)),
              .else_scope = std::nullopt});
    }
    return steps.Build(body.Add(
        mir::Expr{
            .data =
                mir::ConditionalExpr{
                    .condition = received,
                    .then_value = mir::BuildIntLiteral(unit, body, 1),
                    .else_value = mir::BuildIntLiteral(unit, body, 0)},
            .type = result_type}));
  }

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
  const auto read_value = [&](mir::Builder into) {
    return into.ReadLocal(value_var, destination_type);
  };

  const mir::ExprId answer = [&] {
    switch (check) {
      case hir::RunTimeCheck::kNone:
        bind_value(ConvertToType(unit, body, source, destination_type));
        return mir::BuildIntLiteral(unit, body, 1);
      case hir::RunTimeCheck::kValueIsAMemberOfTheEnumeration:
        bind_value(ConvertToType(unit, body, source, destination_type));
        return body.Add(BuildEnumMembershipCallExpr(
            owner, body, read_value(body), declared));
      case hir::RunTimeCheck::kValueIsOfAnEquivalentType:
        throw InternalError(
            "LowerHirDynamicCastExpr: a destination of 'dynamic_type' is "
            "answered before any value is bound");
      case hir::RunTimeCheck::kObjectIsOfTheDestinationClass: {
        const mir::ClassId destination_class = mir::ClassOfObject(
            unit.types, unit.types.Get(destination_type)
                            .Get<mir::ManagedRefType>()
                            .pointee);
        bind_value(body.Add(
            mir::MakeCallExpr(
                mir::Direct{.target = support::BuiltinFn::kDynamicCast},
                {source, BuildDefinitionRead(unit, body, destination_class)},
                destination_type)));
        if (!c.destination_instance.has_value()) return read_value(body);
        // An object of the class is of this instance's type only where it
        // belongs to this instance (LRM 6.22): the instance its construction
        // was handed as the class's own, which the object keeps. A JVM class
        // is likewise its name together with the loader that defined it.
        const mir::ExprId is_of_class =
            ReduceToCondition(unit, body, read_value(body));
        mir::Block belongs;
        mir::Builder belongs_builder = body.Into(belongs);
        const mir::ExprId object = BuildObjectDeref(
            unit, belongs_builder,
            BuildObjectPointer(
                unit, belongs_builder, read_value(belongs_builder)));
        const mir::FieldId slot =
            owner.DeclaringInstanceFieldOf(destination_class);
        const mir::ExprId kept = belongs_builder.Read(belongs_builder.Add(
            mir::MakeFieldAccessExpr(
                object,
                mir::ClassFieldTarget{.owner = destination_class, .slot = slot},
                unit.builtins.scope_ptr)));
        const mir::ExprId expected = BuildImplicitInstanceArgument(
            lowerer, steps.Frame().WithBlock(&belongs),
            *c.destination_instance);
        const mir::ExprId same = belongs_builder.Add(
            mir::MakeBinary(
                unit, belongs_builder.Body(), mir::BinaryOp::kEquality, kept,
                expected, unit.builtins.machine_bool));
        const mir::ExprId belongs_answer = body.Add(
            mir::Expr{
                .data =
                    mir::BlockExpr{
                        .scope = body.AddScope(std::move(belongs)),
                        .value = same},
                .type = unit.builtins.machine_bool});
        return body.Add(
            mir::Expr{
                .data =
                    mir::ConditionalExpr{
                        .condition = is_of_class,
                        .then_value = belongs_answer,
                        .else_value = body.Add(
                            mir::Expr{
                                .data = mir::MachineBoolLiteral{.value = false},
                                .type = unit.builtins.machine_bool})},
                .type = unit.builtins.machine_bool});
      }
    }
    throw InternalError("LowerHirDynamicCastExpr: unknown run-time check");
  }();
  const mir::LocalId answer_var =
      steps.Bindings().DeclareAnonymous(result_type);
  body.AppendStmt(
      mir::LocalDeclStmt{
          .target = answer_var,
          .init = body.Add(
              mir::Expr{
                  .data =
                      mir::ConditionalExpr{
                          .condition = ReduceToCondition(unit, body, answer),
                          .then_value = mir::BuildIntLiteral(unit, body, 1),
                          .else_value = mir::BuildIntLiteral(unit, body, 0)},
                  .type = result_type})});
  const auto read_answer = [&] {
    return body.ReadLocal(answer_var, result_type);
  };

  // The destination is reached inside the arm that writes it, because that is
  // the only run on which it is written: an invalid assignment leaves the
  // destination alone, and reaching it is part of writing it.
  mir::Block taken;
  const WalkFrame taken_frame = steps.Frame().WithBlock(&taken);
  auto target_or =
      LowerLvalue(lowerer, lowerer.HirExprs().Get(c.destination), taken_frame);
  if (!target_or) return std::unexpected(std::move(target_or.error()));
  auto stored = AppendStores(
      lowerer.Owner(), taken_frame, *target_or,
      read_value(taken_frame.Builder()));
  if (!stored) return std::unexpected(std::move(stored.error()));

  std::optional<mir::BlockId> invalid_scope;
  if (c.on_invalid == hir::InvalidAssignmentHandling::kReported) {
    mir::Block invalid;
    mir::Builder invalid_builder(invalid, taken_frame.span);
    report_invalid(invalid_builder);
    invalid_scope = body.AddScope(std::move(invalid));
  }

  body.AppendStmt(
      mir::IfStmt{
          .condition = ReduceToCondition(unit, body, read_answer()),
          .then_scope = body.AddScope(std::move(taken)),
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
