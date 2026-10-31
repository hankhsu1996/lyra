#include "lyra/lowering/hir_to_mir/randomization.hpp"

#include <algorithm>
#include <cstdint>
#include <cstdlib>
#include <expected>
#include <format>
#include <functional>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/array_iterator_method.hpp"
#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/constraint.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/param_direction.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/type_definition.hpp"
#include "lyra/hir/unary_op.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/hir/with_clause_id.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/block_builder.hpp"
#include "lyra/lowering/hir_to_mir/callee_interface.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/expression/selects.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/statement/foreach.hpp"
#include "lyra/lowering/hir_to_mir/statement/loops.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/enum_table.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

using support::BuiltinFn;

auto Unsupported(diag::SourceSpan span, std::string_view what)
    -> std::unexpected<diag::Diagnostic> {
  return diag::Fail(
      span, diag::DiagCode::kUnsupportedExpressionForm,
      "{} in a constraint is not yet supported", what);
}

// What lowering one constraint needs at hand: the body it lowers into, the
// calls it makes on the problem the frame holds, and the loop variables of
// the `foreach` constraints around it, each a value fixed for one pass (LRM
// 18.5.7) rather than a variable of the problem.
class ConstraintWriter {
 public:
  ConstraintWriter(
      ProcessLowerer& lowerer, const WalkFrame& frame,
      std::vector<hir::ProceduralVarId> loop_vars = {})
      : lowerer_(&lowerer), frame_(frame), loop_vars_(std::move(loop_vars)) {
  }

  // The writer for the body of a `foreach` constraint, one pass of it
  // written into `frame`'s block, its loop variables `bound`.
  [[nodiscard]] auto Within(
      const WalkFrame& frame,
      std::span<const std::optional<hir::ProceduralVarId>> bound) const
      -> ConstraintWriter {
    std::vector<hir::ProceduralVarId> loop_vars = loop_vars_;
    for (const std::optional<hir::ProceduralVarId>& var : bound) {
      if (var.has_value()) {
        loop_vars.push_back(*var);
      }
    }
    return {*lowerer_, frame, std::move(loop_vars)};
  }

  [[nodiscard]] auto IsLoopVar(hir::ProceduralVarId var) const -> bool {
    return std::ranges::contains(loop_vars_, var);
  }

  // The terms an array method's `with` clause names one element by (LRM
  // 18.5.7.2): the element's, and its index's, each held in a local.
  struct Iteration {
    hir::WithClauseId clause;
    mir::LocalId element;
    mir::LocalId index;
  };
  // The writer for one element of a reduction, written into `frame`'s block,
  // `iteration` bound.
  [[nodiscard]] auto Iterating(
      const WalkFrame& frame, Iteration iteration) const -> ConstraintWriter {
    ConstraintWriter out(*lowerer_, frame, loop_vars_);
    out.iterations_ = iterations_;
    out.iterations_.push_back(iteration);
    return out;
  }
  [[nodiscard]] auto IterationTerm(const hir::IterationBindingRef& ref) const
      -> mir::ExprId {
    for (const Iteration& iteration : iterations_) {
      if (iteration.clause != ref.clause) {
        continue;
      }
      switch (ref.role) {
        case hir::IterationBindingRole::kElement:
          return ReadTerm(iteration.element);
        case hir::IterationBindingRole::kIndex:
          return ReadTerm(iteration.index);
      }
    }
    throw InternalError(
        "ConstraintWriter: a with clause names an element no reduction "
        "around it binds -- please report this as a bug");
  }

  [[nodiscard]] auto Unit() const -> mir::CompilationUnit& {
    return lowerer_->Owner().Unit();
  }
  [[nodiscard]] auto Block() const -> mir::Builder {
    return frame_.Builder();
  }
  [[nodiscard]] auto Frame() const -> const WalkFrame& {
    return frame_;
  }
  [[nodiscard]] auto Lowerer() const -> ProcessLowerer& {
    return *lowerer_;
  }

  [[nodiscard]] auto Problem() const -> mir::ExprId {
    if (!frame_.constraint_problem.has_value()) {
      throw InternalError(
          "ConstraintWriter: a constraint is lowered where no call of "
          "randomize() holds a problem -- please report this as a bug");
    }
    return Block().ReadLocal(
        *frame_.constraint_problem, Unit().builtins.constraint_problem);
  }

  [[nodiscard]] auto Call(
      BuiltinFn fn, std::vector<mir::ExprId> arguments, mir::TypeId type) const
      -> mir::ExprId {
    return Block().Add(
        mir::MakeCallExpr(
            mir::Direct{.target = fn}, std::move(arguments), type));
  }

  // A term the problem answers, a machine integer.
  [[nodiscard]] auto Term(BuiltinFn fn, std::vector<mir::ExprId> operands) const
      -> mir::ExprId {
    operands.insert(operands.begin(), Problem());
    return Call(fn, std::move(operands), Unit().builtins.machine_int64);
  }

  void Effect(BuiltinFn fn, std::vector<mir::ExprId> operands) const {
    operands.insert(operands.begin(), Problem());
    Block().AppendStmt(
        mir::ExprStmt{
            .expr = Call(fn, std::move(operands), Unit().builtins.void_type)});
  }

  [[nodiscard]] auto Bool(bool value) const -> mir::ExprId {
    return BuildMachineBool(Unit(), Block(), value);
  }
  [[nodiscard]] auto Machine(std::int64_t value) const -> mir::ExprId {
    return mir::BuildMachineIntLiteral(Unit(), Block(), value);
  }

  // `value` held in a local of the body, so the steps after it may name it more
  // than once.
  [[nodiscard]] auto Bind(mir::TypeId type, mir::ExprId value) const
      -> mir::LocalId {
    const mir::LocalId local = frame_.bindings->DeclareAnonymous(type);
    Block().AppendStmt(mir::LocalDeclStmt{.target = local, .init = value});
    return local;
  }

  [[nodiscard]] auto ReadTerm(mir::LocalId term) const -> mir::ExprId {
    return Block().ReadLocal(term, Unit().builtins.machine_int64);
  }

 private:
  ProcessLowerer* lowerer_;
  WalkFrame frame_;
  std::vector<hir::ProceduralVarId> loop_vars_;
  std::vector<Iteration> iterations_;
};

auto HirExpr(const ConstraintWriter& w, hir::ExprId id) -> const hir::Expr& {
  return w.Lowerer().HirExprs().Get(id);
}

auto MirTypeOf(const ConstraintWriter& w, hir::ExprId id) -> mir::TypeId {
  return w.Lowerer().Owner().TranslateType(HirExpr(w, id).type);
}

auto IsIntegral(const ConstraintWriter& w, hir::ExprId id) -> bool {
  return w.Unit().types.Get(MirTypeOf(w, id)).IsIntegral();
}

auto WidthOf(const ConstraintWriter& w, hir::ExprId id) -> std::int64_t {
  return static_cast<std::int64_t>(
      w.Unit().types.Get(MirTypeOf(w, id)).Integral().bit_width);
}

auto IsSigned(const ConstraintWriter& w, hir::ExprId id) -> bool {
  return w.Unit().types.Get(MirTypeOf(w, id)).Integral().signedness ==
         mir::Signedness::kSigned;
}

// Whether the HIR type `type` holds its parts as storage of their own, so a
// part of a value of it is a variable of its own rather than bits of one.
auto HoldsSeparateParts(const ConstraintWriter& w, hir::TypeId type) -> bool {
  const hir::Type& held = w.Lowerer().Owner().Hir().types.Get(type);
  return held.Is<hir::UnpackedArrayType>() ||
         held.Is<hir::DynamicArrayType>() || held.Is<hir::QueueType>() ||
         held.Is<hir::AssociativeArrayType>() ||
         held.Is<hir::UnpackedStructType>();
}

// Whether the expression names storage a variable of the problem is: a name,
// a property, or an element or member of a variable whose parts are its own.
// Bits of an integral value are not; they are that value's.
auto NamesVariable(const ConstraintWriter& w, hir::ExprId id) -> bool {
  const hir::Expr& expr = HirExpr(w, id);
  return std::visit(
      Overloaded{
          [&](const hir::PrimaryExpr& p) {
            return std::visit(
                Overloaded{
                    [](const hir::IntegerLiteral&) { return false; },
                    [](const hir::StringLiteral&) { return false; },
                    [](const hir::RealLiteral&) { return false; },
                    [](const hir::NullLiteral&) { return false; },
                    [](const hir::ThisHandle&) { return false; },
                    [](const hir::RandomizedObject&) { return false; },
                    [](const hir::QueueLastIndex&) { return false; },
                    [&](const hir::ProceduralVarRef& r) {
                      return !w.IsLoopVar(r.var);
                    },
                    [](const hir::ClassPropertyRef&) { return true; },
                    [](const hir::StaticPropertyRef&) { return true; },
                    [](const hir::RoutedValueRef&) { return true; },
                    [](const hir::RoutedObjectRef&) { return false; },
                    [](const hir::IterationBindingRef&) { return false; },
                    [](const hir::PatternVarRef&) { return false; },
                    [](const hir::NamespaceValueRef&) { return true; }},
                p.data);
          },
          [](const hir::UnaryExpr&) { return false; },
          [](const hir::BinaryExpr&) { return false; },
          [](const hir::ConditionalExpr&) { return false; },
          [](const hir::AssignExpr&) { return false; },
          [](const hir::IncDecExpr&) { return false; },
          [](const hir::CallExpr&) { return false; },
          [](const hir::CalleeDefaultExpr&) { return false; },
          [](const hir::ConversionExpr&) { return false; },
          [](const hir::ConstantExpr&) { return false; },
          [](const hir::ValueRangeExpr&) { return false; },
          [](const hir::InsideExpr&) { return false; },
          [&](const hir::ElementSelectExpr& e) {
            return HoldsSeparateParts(w, HirExpr(w, e.base_value).type) &&
                   NamesVariable(w, e.base_value);
          },
          [](const hir::RangeSelectExpr&) { return false; },
          [&](const hir::MemberAccessExpr& e) {
            return HoldsSeparateParts(w, HirExpr(w, e.base_value).type) &&
                   NamesVariable(w, e.base_value);
          },
          [](const hir::ClassPropertyAccessExpr&) { return true; },
          [](const hir::InterfaceMemberAccessExpr&) { return false; },
          [](const hir::InterfaceInstanceAccessExpr&) { return false; },
          [](const hir::ConcatExpr&) { return false; },
          [](const hir::StreamingConcatExpr&) { return false; },
          [](const hir::ReplicationExpr&) { return false; },
          [](const hir::AssignmentPatternExpr&) { return false; },
          [](const hir::AssignmentPatternReplicationExpr&) { return false; },
          [](const hir::DynamicArrayNewExpr&) { return false; },
          [](const hir::ClassNewExpr&) { return false; },
          [](const hir::AssociativeAssignmentPatternExpr&) { return false; },
          [](const hir::AssignmentPatternKeyedExpr&) { return false; },
          [](const hir::TaggedUnionExpr&) { return false; },
          [](const hir::DynamicCastExpr&) { return false; },
          [](const hir::CovergroupNewExpr&) { return false; },
          [](const hir::CovergroupMethodCallExpr&) { return false; },
          [](const hir::CovergroupOptionExpr&) { return false; },
          [](const hir::RandomizeExpr&) { return false; },
          [](const hir::RandModeSetExpr&) { return false; },
          [](const hir::RandModeGetExpr&) { return false; },
          [](const hir::ConstraintModeSetExpr&) { return false; },
          [](const hir::ConstraintModeGetExpr&) { return false; }},
      expr.data);
}

// The array `id` asks the size of, where it is `size()` of an array that is a
// variable of the problem (LRM 18.5.7.1): a size the call may choose.
auto SizedArray(const ConstraintWriter& w, hir::ExprId id)
    -> std::optional<hir::ExprId> {
  const auto* call = std::get_if<hir::CallExpr>(&HirExpr(w, id).data);
  if (call == nullptr) {
    return std::nullopt;
  }
  const auto* method = std::get_if<hir::BuiltinMethodRef>(&call->callee);
  if (method == nullptr || method->method != support::BuiltinFn::kSize ||
      !method->receiver.has_value() || !NamesVariable(w, *method->receiver)) {
    return std::nullopt;
  }
  return method->receiver;
}

// The call `id` is and the array it reduces, where it is `sum()`,
// `product()`, `and()`, `or()` or `xor()` of an array that is a variable of
// the problem (LRM 18.5.7.2): a value its elements decide.
struct Reduction {
  const hir::ArrayIteratorMethodRef* method;
  hir::ExprId array;
};

auto ReducedArray(const ConstraintWriter& w, hir::ExprId id)
    -> std::optional<Reduction> {
  const auto* call = std::get_if<hir::CallExpr>(&HirExpr(w, id).data);
  if (call == nullptr || call->arguments.empty() ||
      !call->arguments.front().has_value()) {
    return std::nullopt;
  }
  const auto* method = std::get_if<hir::ArrayIteratorMethodRef>(&call->callee);
  if (method == nullptr || !NamesVariable(w, *call->arguments.front())) {
    return std::nullopt;
  }
  switch (method->method) {
    case hir::ArrayIteratorMethod::kSum:
    case hir::ArrayIteratorMethod::kProduct:
    case hir::ArrayIteratorMethod::kAnd:
    case hir::ArrayIteratorMethod::kOr:
    case hir::ArrayIteratorMethod::kXor:
      return Reduction{.method = method, .array = *call->arguments.front()};
    case hir::ArrayIteratorMethod::kSort:
    case hir::ArrayIteratorMethod::kRsort:
    case hir::ArrayIteratorMethod::kFind:
    case hir::ArrayIteratorMethod::kFindIndex:
    case hir::ArrayIteratorMethod::kFindFirst:
    case hir::ArrayIteratorMethod::kFindFirstIndex:
    case hir::ArrayIteratorMethod::kFindLast:
    case hir::ArrayIteratorMethod::kFindLastIndex:
    case hir::ArrayIteratorMethod::kMin:
    case hir::ArrayIteratorMethod::kMax:
    case hir::ArrayIteratorMethod::kUnique:
    case hir::ArrayIteratorMethod::kUniqueIndex:
    case hir::ArrayIteratorMethod::kMap:
      return std::nullopt;
  }
  throw InternalError("ReducedArray: unknown array method");
}

// Whether anything under the expression is a variable of the problem, which
// is what makes it more than a value to evaluate where the constraint is.
auto MentionsVariable(const ConstraintWriter& w, hir::ExprId id) -> bool;

// The expressions directly under `id` a variable of the problem may stand in:
// the operands of an operator, a select's subject and index, and the
// arguments of a call.
auto Operands(const ConstraintWriter& w, hir::ExprId id)
    -> std::vector<hir::ExprId> {
  const hir::Expr& expr = HirExpr(w, id);
  return std::visit(
      Overloaded{
          [](const hir::PrimaryExpr&) -> std::vector<hir::ExprId> {
            return {};
          },
          [](const hir::UnaryExpr& e) -> std::vector<hir::ExprId> {
            return {e.operand};
          },
          [](const hir::BinaryExpr& e) -> std::vector<hir::ExprId> {
            return {e.lhs, e.rhs};
          },
          [](const hir::ConditionalExpr& e) -> std::vector<hir::ExprId> {
            std::vector<hir::ExprId> out;
            for (const hir::ConditionClause& clause : e.conditions) {
              out.push_back(clause.expr);
            }
            out.push_back(e.then_value);
            out.push_back(e.else_value);
            return out;
          },
          [](const hir::AssignExpr&) -> std::vector<hir::ExprId> { return {}; },
          [](const hir::IncDecExpr&) -> std::vector<hir::ExprId> { return {}; },
          [](const hir::CallExpr& e) -> std::vector<hir::ExprId> {
            std::vector<hir::ExprId> out;
            for (const std::optional<hir::ExprId>& argument : e.arguments) {
              if (argument.has_value()) out.push_back(*argument);
            }
            return out;
          },
          [](const hir::CalleeDefaultExpr&) -> std::vector<hir::ExprId> {
            return {};
          },
          [](const hir::ConversionExpr& e) -> std::vector<hir::ExprId> {
            return {e.operand};
          },
          [](const hir::ConstantExpr&) -> std::vector<hir::ExprId> {
            return {};
          },
          [](const hir::ValueRangeExpr& e) -> std::vector<hir::ExprId> {
            std::vector<hir::ExprId> out;
            if (e.lo.has_value()) out.push_back(*e.lo);
            if (e.hi.has_value()) out.push_back(*e.hi);
            return out;
          },
          [](const hir::InsideExpr& e) -> std::vector<hir::ExprId> {
            std::vector<hir::ExprId> out{e.lhs};
            out.insert(out.end(), e.items.begin(), e.items.end());
            return out;
          },
          [](const hir::ElementSelectExpr& e) -> std::vector<hir::ExprId> {
            return {e.base_value, e.index};
          },
          [](const hir::RangeSelectExpr& e) -> std::vector<hir::ExprId> {
            return {e.base_value};
          },
          [](const hir::MemberAccessExpr& e) -> std::vector<hir::ExprId> {
            return {e.base_value};
          },
          [](const hir::ClassPropertyAccessExpr&) -> std::vector<hir::ExprId> {
            return {};
          },
          [](const hir::InterfaceMemberAccessExpr&)
              -> std::vector<hir::ExprId> { return {}; },
          [](const hir::InterfaceInstanceAccessExpr&)
              -> std::vector<hir::ExprId> { return {}; },
          [](const hir::ConcatExpr& e) -> std::vector<hir::ExprId> {
            return e.operands;
          },
          [](const hir::StreamingConcatExpr&) -> std::vector<hir::ExprId> {
            return {};
          },
          [](const hir::ReplicationExpr& e) -> std::vector<hir::ExprId> {
            return {e.concat};
          },
          [](const hir::AssignmentPatternExpr&) -> std::vector<hir::ExprId> {
            return {};
          },
          [](const hir::AssignmentPatternReplicationExpr&)
              -> std::vector<hir::ExprId> { return {}; },
          [](const hir::DynamicArrayNewExpr&) -> std::vector<hir::ExprId> {
            return {};
          },
          [](const hir::ClassNewExpr&) -> std::vector<hir::ExprId> {
            return {};
          },
          [](const hir::AssociativeAssignmentPatternExpr&)
              -> std::vector<hir::ExprId> { return {}; },
          [](const hir::AssignmentPatternKeyedExpr&)
              -> std::vector<hir::ExprId> { return {}; },
          [](const hir::TaggedUnionExpr&) -> std::vector<hir::ExprId> {
            return {};
          },
          [](const hir::DynamicCastExpr&) -> std::vector<hir::ExprId> {
            return {};
          },
          [](const hir::CovergroupNewExpr&) -> std::vector<hir::ExprId> {
            return {};
          },
          [](const hir::CovergroupMethodCallExpr&) -> std::vector<hir::ExprId> {
            return {};
          },
          [](const hir::CovergroupOptionExpr&) -> std::vector<hir::ExprId> {
            return {};
          },
          [](const hir::RandomizeExpr&) -> std::vector<hir::ExprId> {
            return {};
          },
          [](const hir::RandModeSetExpr&) -> std::vector<hir::ExprId> {
            return {};
          },
          [](const hir::RandModeGetExpr&) -> std::vector<hir::ExprId> {
            return {};
          },
          [](const hir::ConstraintModeSetExpr&) -> std::vector<hir::ExprId> {
            return {};
          },
          [](const hir::ConstraintModeGetExpr&) -> std::vector<hir::ExprId> {
            return {};
          }},
      expr.data);
}

auto MentionsVariable(const ConstraintWriter& w, hir::ExprId id) -> bool {
  if (NamesVariable(w, id) || SizedArray(w, id).has_value() ||
      ReducedArray(w, id).has_value()) {
    return true;
  }
  if (const auto* primary = std::get_if<hir::PrimaryExpr>(&HirExpr(w, id).data);
      primary != nullptr &&
      std::holds_alternative<hir::IterationBindingRef>(primary->data)) {
    return true;
  }
  return std::ranges::any_of(Operands(w, id), [&](hir::ExprId operand) {
    return MentionsVariable(w, operand);
  });
}

auto Term(const ConstraintWriter& w, hir::ExprId id)
    -> diag::Result<mir::ExprId>;

auto ReductionTerm(
    const ConstraintWriter& w, hir::ExprId id, const Reduction& reduction)
    -> diag::Result<mir::ExprId>;

// A value the constraint reads that no variable stands in, evaluated where
// the constraint is.
auto ConstantTerm(const ConstraintWriter& w, hir::ExprId id)
    -> diag::Result<mir::ExprId> {
  const hir::Expr& expr = HirExpr(w, id);
  if (!IsIntegral(w, id)) {
    return Unsupported(expr.span, "a value that is not integral");
  }
  auto value = w.Lowerer().LowerExpr(expr, w.Frame());
  if (!value) return std::unexpected(std::move(value.error()));
  return w.Term(
      BuiltinFn::kConstraintConstant, {w.Block().Add(*std::move(value))});
}

// A constant term of `width` bits holding `value`, for the arithmetic a select
// does on a position.
auto NumberTerm(const ConstraintWriter& w, std::int64_t value) -> mir::ExprId {
  const mir::TypeId type = w.Unit().types.Intern(
      mir::Type{mir::IntegralType{
          .bit_width = 64,
          .signedness = mir::Signedness::kSigned,
          .state_kind = mir::IntegralStateKind::kTwoState}});
  const mir::ExprId literal = mir::BuildIntegralLiteral(
      w.Unit(), w.Block(), type,
      mir::IntegralConstant{
          .value_words = {static_cast<std::uint64_t>(value)},
          .state_words = {}});
  return w.Term(BuiltinFn::kConstraintConstant, {literal});
}

// The term of an expression read as a position: its value at 64 bits, signed,
// so the arithmetic of a select on it cannot wrap.
auto PositionTerm(const ConstraintWriter& w, hir::ExprId id)
    -> diag::Result<mir::ExprId> {
  auto term = Term(w, id);
  if (!term) return term;
  return w.Term(
      BuiltinFn::kConstraintResize,
      {*term, w.Machine(64), w.Bool(IsSigned(w, id))});
}

// A reference to the storage `id` names, and the value it holds now.
struct NamedVariable {
  mir::ExprId reference;
  mir::ExprId value;
};

auto NameVariable(const ConstraintWriter& w, hir::ExprId id)
    -> diag::Result<NamedVariable> {
  const hir::Expr& expr = HirExpr(w, id);
  auto path = w.Lowerer().LowerLhsExpr(expr, w.Frame());
  if (!path) return std::unexpected(std::move(path.error()));
  const mir::TypeId value_type = MirTypeOf(w, id);
  const mir::TypeId reference_type =
      *ParamTypeOf(w.Unit(), value_type, hir::ParamDirection::kRef);
  const mir::LocalId reference = w.Bind(
      reference_type,
      AtType(
          w.Unit(), w.Block(), PathReference(w.Unit(), w.Block(), *path),
          reference_type));
  const mir::ExprId value =
      w.Block().Read(w.Block().Add(w.Block().WrapperContents(
          w.Unit().types, w.Block().ReadLocal(reference, reference_type),
          value_type)));
  return NamedVariable{
      .reference = w.Block().ReadLocal(reference, reference_type),
      .value = value};
}

// What `rand_mode()` names (LRM 18.8): the random variable the class declares
// -- a property, or a member of a structure one is, reached by the steps that
// name storage of the property's own -- as a reference and the value it holds
// now, and a reference to the part the call names, the whole or an element of
// it at any depth. A mode is kept for the variable and the positions of the
// part, which an element keeps when an array holding it moves.
struct NamedModeVariable {
  mir::ExprId declared;
  mir::ExprId value;
  mir::ExprId part;
};

// Each random variable `id` names: the one it is, or of a structure each
// member, at any depth, since a call naming a structure without a member
// names every member (LRM 18.8).
auto NameModeVariables(const ConstraintWriter& w, hir::ExprId id)
    -> diag::Result<std::vector<NamedModeVariable>> {
  UnitLowerer& unit_lowerer = w.Lowerer().Owner();
  mir::CompilationUnit& unit = w.Unit();
  mir::Builder block = w.Block();
  auto path = w.Lowerer().LowerLhsExpr(HirExpr(w, id), w.Frame());
  if (!path) return std::unexpected(std::move(path.error()));
  const SettledPath settled =
      SettledForRead(unit_lowerer, w.Frame(), *std::move(path));
  const auto bind = [&](const AccessPath& at) {
    const mir::TypeId value_type = PathValueType(unit, block.Body(), at);
    const mir::TypeId reference_type =
        *ParamTypeOf(unit, value_type, hir::ParamDirection::kRef);
    return block.ReadLocal(
        w.Bind(
            reference_type,
            AtType(
                unit, block, PathReference(unit, block, at), reference_type)),
        reference_type);
  };
  // The variable the class declares that a part lies in is reached by the
  // steps that name storage of the property's own.
  const auto declared_of = [](AccessPath at) {
    at.descent.erase(
        std::ranges::find_if(
            at.descent,
            [](const DescentStep& step) {
              return !std::holds_alternative<StepToComponent>(step.to);
            }),
        at.descent.end());
    return at;
  };
  std::vector<NamedModeVariable> out;
  const auto each = [&](const auto& self, const std::vector<DescentStep>& more,
                        hir::TypeId type) -> void {
    const auto at = [&] {
      AccessPath named = NamedIn(settled, block);
      named.descent.insert(named.descent.end(), more.begin(), more.end());
      return named;
    };
    const hir::Type& held = unit_lowerer.Hir().types.Get(type);
    if (const auto* declared = held.As<hir::UnpackedStructType>()) {
      const hir::UnpackedStructDefinition& structure =
          unit_lowerer.Hir().DefinitionOf(*declared);
      for (std::uint32_t i = 0; i < structure.fields.size(); ++i) {
        const hir::TypeId member = structure.fields[i].type;
        std::vector<DescentStep> deeper = more;
        deeper.push_back(MemberStep(
            unit_lowerer, block, held, base::ComponentIndex{.value = i},
            unit_lowerer.TranslateType(member)));
        self(self, deeper, member);
      }
      return;
    }
    out.push_back(
        NamedModeVariable{
            .declared = bind(declared_of(at())),
            .value = PathValue(unit, block, declared_of(at())),
            .part = bind(at())});
  };
  each(each, {}, HirExpr(w, id).type);
  return out;
}

// A variable of the problem, through a reference to its storage, with the value
// it holds now.
auto VariableTerm(const ConstraintWriter& w, hir::ExprId id)
    -> diag::Result<mir::ExprId> {
  if (!IsIntegral(w, id)) {
    return Unsupported(
        HirExpr(w, id).span, "a random variable that is not integral");
  }
  auto named = NameVariable(w, id);
  if (!named) return std::unexpected(std::move(named.error()));
  return w.Term(
      BuiltinFn::kConstraintVariable, {named->reference, named->value});
}

// Each variable of the problem the operands of `id` name, wanted ahead of the
// constraints `id` enters (LRM 18.5.11).
auto WantVariables(const ConstraintWriter& w, hir::ExprId id)
    -> diag::Result<void> {
  for (const hir::ExprId operand : Operands(w, id)) {
    if (NamesVariable(w, operand)) {
      auto term = VariableTerm(w, operand);
      if (!term) return std::unexpected(std::move(term.error()));
      w.Effect(BuiltinFn::kConstraintWant, {*term});
    } else if (MentionsVariable(w, operand)) {
      if (auto r = WantVariables(w, operand); !r) return r;
    }
  }
  return {};
}

// The term of a variable `solve ... before` or `disable soft` names: a random
// variable, or the size of an array, which the standard lets either name (LRM
// 18.4).
auto OrderedTerm(const ConstraintWriter& w, hir::ExprId id)
    -> diag::Result<mir::ExprId> {
  if (const std::optional<hir::ExprId> array = SizedArray(w, id)) {
    auto named = NameVariable(w, *array);
    if (!named) return std::unexpected(std::move(named.error()));
    return w.Term(
        BuiltinFn::kConstraintArraySize, {named->reference, named->value});
  }
  return VariableTerm(w, id);
}

// The part of an integral value at `position` bits from its least significant
// end, `width` bits wide.
auto PartTerm(
    const ConstraintWriter& w, mir::ExprId whole, mir::ExprId position,
    std::int64_t width) -> mir::ExprId {
  const mir::ExprId shifted = w.Term(
      BuiltinFn::kConstraintShiftRight, {whole, position, w.Bool(false)});
  return w.Term(
      BuiltinFn::kConstraintResize, {shifted, w.Machine(width), w.Bool(false)});
}

// How a packed dimension numbers its elements: its range and how wide one is.
struct PackedDimension {
  std::int64_t left;
  std::int64_t right;
  std::int64_t element_width;
};

auto DimensionOfSelected(const ConstraintWriter& w, hir::ExprId base)
    -> PackedDimension {
  const hir::Expr& expr = HirExpr(w, base);
  const hir::Type& type = w.Lowerer().Owner().Hir().types.Get(expr.type);
  if (const auto* array = type.As<hir::PackedArrayType>()) {
    const auto element = static_cast<std::int64_t>(
        w.Unit()
            .types.Get(w.Lowerer().Owner().TranslateType(array->element_type))
            .Integral()
            .bit_width);
    return {
        .left = array->dim.left,
        .right = array->dim.right,
        .element_width = element};
  }
  return {.left = WidthOf(w, base) - 1, .right = 0, .element_width = 1};
}

// The position, in bits from the least significant end, of the index `index`
// of a dimension: a descending range counts up from its right bound, an
// ascending one down to it.
auto IndexPosition(
    const ConstraintWriter& w, const PackedDimension& dim, mir::ExprId index)
    -> mir::ExprId {
  const mir::ExprId right = NumberTerm(w, dim.right);
  const mir::ExprId offset =
      dim.left >= dim.right
          ? w.Term(BuiltinFn::kConstraintSubtract, {index, right})
          : w.Term(BuiltinFn::kConstraintSubtract, {right, index});
  return w.Term(
      BuiltinFn::kConstraintMultiply,
      {offset, NumberTerm(w, dim.element_width)});
}

auto SelectTerm(
    const ConstraintWriter& w, hir::ExprId id, const hir::RangeSelectExpr& sel)
    -> diag::Result<mir::ExprId> {
  auto whole = Term(w, sel.base_value);
  if (!whole) return whole;
  const PackedDimension dim = DimensionOfSelected(w, sel.base_value);
  const bool descending = dim.left >= dim.right;
  // The index of the selected part's least significant element.
  auto least = std::visit(
      Overloaded{
          [&](const hir::RangeConstantBounds& b) -> diag::Result<mir::ExprId> {
            return PositionTerm(w, b.right_bound);
          },
          [&](const hir::RangeIndexedUpBounds& b) -> diag::Result<mir::ExprId> {
            auto base = PositionTerm(w, b.base_index);
            if (!base || descending) return base;
            auto width = PositionTerm(w, b.width);
            if (!width) return width;
            return w.Term(
                BuiltinFn::kConstraintSubtract,
                {w.Term(BuiltinFn::kConstraintAdd, {*base, *width}),
                 NumberTerm(w, 1)});
          },
          [&](const hir::RangeIndexedDownBounds& b)
              -> diag::Result<mir::ExprId> {
            auto base = PositionTerm(w, b.base_index);
            if (!base || !descending) return base;
            auto width = PositionTerm(w, b.width);
            if (!width) return width;
            return w.Term(
                BuiltinFn::kConstraintAdd,
                {w.Term(BuiltinFn::kConstraintSubtract, {*base, *width}),
                 NumberTerm(w, 1)});
          }},
      sel.bounds);
  if (!least) return least;
  const mir::ExprId position = IndexPosition(w, dim, *least);
  const std::int64_t width = WidthOf(w, id);
  return PartTerm(w, *whole, position, width);
}

auto UnaryTerm(const ConstraintWriter& w, const hir::UnaryExpr& e)
    -> diag::Result<mir::ExprId> {
  auto a = Term(w, e.operand);
  if (!a) return a;
  switch (e.op) {
    case hir::UnaryOp::kPlus:
      return *a;
    case hir::UnaryOp::kMinus:
      return w.Term(BuiltinFn::kConstraintNegate, {*a});
    case hir::UnaryOp::kBitwiseNot:
      return w.Term(BuiltinFn::kConstraintBitwiseNot, {*a});
    case hir::UnaryOp::kLogicalNot:
      return w.Term(BuiltinFn::kConstraintLogicalNot, {*a});
    case hir::UnaryOp::kReductionAnd:
      return w.Term(BuiltinFn::kConstraintReduceAnd, {*a});
    case hir::UnaryOp::kReductionOr:
      return w.Term(BuiltinFn::kConstraintReduceOr, {*a});
    case hir::UnaryOp::kReductionXor:
      return w.Term(BuiltinFn::kConstraintReduceXor, {*a});
    case hir::UnaryOp::kReductionNand:
      return w.Term(
          BuiltinFn::kConstraintBitwiseNot,
          {w.Term(BuiltinFn::kConstraintReduceAnd, {*a})});
    case hir::UnaryOp::kReductionNor:
      return w.Term(
          BuiltinFn::kConstraintBitwiseNot,
          {w.Term(BuiltinFn::kConstraintReduceOr, {*a})});
    case hir::UnaryOp::kReductionXnor:
      return w.Term(
          BuiltinFn::kConstraintBitwiseNot,
          {w.Term(BuiltinFn::kConstraintReduceXor, {*a})});
  }
  throw InternalError("UnaryTerm: unknown unary operator");
}

// `base ** exponent` by squaring: each bit of the exponent multiplies in the
// square it stands for where it is set.
auto PowerTerm(
    const ConstraintWriter& w, mir::ExprId base, mir::ExprId exponent,
    std::int64_t width, std::int64_t exponent_width) -> mir::ExprId {
  const mir::LocalId result = w.Bind(
      w.Unit().builtins.machine_int64,
      w.Term(
          BuiltinFn::kConstraintResize,
          {NumberTerm(w, 1), w.Machine(width), w.Bool(false)}));
  mir::LocalId square = w.Bind(w.Unit().builtins.machine_int64, base);
  const mir::LocalId bits = w.Bind(w.Unit().builtins.machine_int64, exponent);
  for (std::int64_t k = 0; k < exponent_width; ++k) {
    const mir::ExprId bit = w.Term(
        BuiltinFn::kConstraintSlice,
        {w.ReadTerm(bits), w.Machine(k), w.Machine(1)});
    const mir::ExprId times = w.Term(
        BuiltinFn::kConstraintMultiply,
        {w.ReadTerm(result), w.ReadTerm(square)});
    const mir::ExprId target =
        w.Block().LocalPlace(result, w.Unit().builtins.machine_int64);
    const mir::ExprId selected =
        w.Term(BuiltinFn::kConstraintSelect, {bit, times, w.ReadTerm(result)});
    w.Block().AppendStmt(
        mir::ExprStmt{
            .expr = w.Block().Add(
                mir::MakeAssignExpr(w.Unit().builtins, target, selected))});
    square = w.Bind(
        w.Unit().builtins.machine_int64,
        w.Term(
            BuiltinFn::kConstraintMultiply,
            {w.ReadTerm(square), w.ReadTerm(square)}));
  }
  return w.ReadTerm(result);
}

auto BinaryTerm(
    const ConstraintWriter& w, hir::ExprId id, const hir::BinaryExpr& e)
    -> diag::Result<mir::ExprId> {
  auto a = Term(w, e.lhs);
  if (!a) return a;
  auto b = Term(w, e.rhs);
  if (!b) return b;
  const bool is_signed = IsSigned(w, e.lhs) && IsSigned(w, e.rhs);
  const auto two = [&](BuiltinFn fn, mir::ExprId x, mir::ExprId y) {
    return w.Term(fn, {x, y});
  };
  const auto signed_two = [&](BuiltinFn fn, mir::ExprId x, mir::ExprId y) {
    return w.Term(fn, {x, y, w.Bool(is_signed)});
  };
  const auto negate = [&](mir::ExprId x) {
    return w.Term(BuiltinFn::kConstraintLogicalNot, {x});
  };
  switch (e.op) {
    case hir::BinaryOp::kAdd:
      return two(BuiltinFn::kConstraintAdd, *a, *b);
    case hir::BinaryOp::kSub:
      return two(BuiltinFn::kConstraintSubtract, *a, *b);
    case hir::BinaryOp::kMul:
      return two(BuiltinFn::kConstraintMultiply, *a, *b);
    case hir::BinaryOp::kDiv:
      return signed_two(BuiltinFn::kConstraintDivide, *a, *b);
    case hir::BinaryOp::kMod:
      return signed_two(BuiltinFn::kConstraintRemainder, *a, *b);
    case hir::BinaryOp::kPower: {
      const std::int64_t width = WidthOf(w, id);
      const std::int64_t exponent_width = WidthOf(w, e.rhs);
      return PowerTerm(w, *a, *b, width, exponent_width);
    }
    case hir::BinaryOp::kBitwiseAnd:
      return two(BuiltinFn::kConstraintBitwiseAnd, *a, *b);
    case hir::BinaryOp::kBitwiseOr:
      return two(BuiltinFn::kConstraintBitwiseOr, *a, *b);
    case hir::BinaryOp::kBitwiseXor:
      return two(BuiltinFn::kConstraintBitwiseXor, *a, *b);
    case hir::BinaryOp::kBitwiseXnor:
      return w.Term(
          BuiltinFn::kConstraintBitwiseNot,
          {two(BuiltinFn::kConstraintBitwiseXor, *a, *b)});
    // Constraints are two-state (LRM 18.3), where the case and wildcard
    // equalities are the equality.
    case hir::BinaryOp::kEquality:
    case hir::BinaryOp::kCaseEquality:
    case hir::BinaryOp::kWildcardEquality:
      return two(BuiltinFn::kConstraintEqual, *a, *b);
    case hir::BinaryOp::kInequality:
    case hir::BinaryOp::kCaseInequality:
    case hir::BinaryOp::kWildcardInequality:
      return negate(two(BuiltinFn::kConstraintEqual, *a, *b));
    case hir::BinaryOp::kLessThan:
      return signed_two(BuiltinFn::kConstraintLess, *a, *b);
    case hir::BinaryOp::kGreaterThan:
      return signed_two(BuiltinFn::kConstraintLess, *b, *a);
    case hir::BinaryOp::kLessEqual:
      return negate(signed_two(BuiltinFn::kConstraintLess, *b, *a));
    case hir::BinaryOp::kGreaterEqual:
      return negate(signed_two(BuiltinFn::kConstraintLess, *a, *b));
    case hir::BinaryOp::kLogicalAnd:
      return two(BuiltinFn::kConstraintLogicalAnd, *a, *b);
    case hir::BinaryOp::kLogicalOr:
      return two(BuiltinFn::kConstraintLogicalOr, *a, *b);
    case hir::BinaryOp::kLogicalImplication:
      return two(BuiltinFn::kConstraintLogicalOr, negate(*a), *b);
    case hir::BinaryOp::kLogicalEquivalence: {
      const mir::ExprId lhs = w.Term(BuiltinFn::kConstraintReduceOr, {*a});
      const mir::ExprId rhs = w.Term(BuiltinFn::kConstraintReduceOr, {*b});
      return two(BuiltinFn::kConstraintEqual, lhs, rhs);
    }
    case hir::BinaryOp::kLogicalShiftLeft:
    case hir::BinaryOp::kArithmeticShiftLeft:
      return two(BuiltinFn::kConstraintShiftLeft, *a, *b);
    case hir::BinaryOp::kLogicalShiftRight:
      return w.Term(BuiltinFn::kConstraintShiftRight, {*a, *b, w.Bool(false)});
    case hir::BinaryOp::kArithmeticShiftRight:
      return w.Term(
          BuiltinFn::kConstraintShiftRight,
          {*a, *b, w.Bool(IsSigned(w, e.lhs))});
  }
  throw InternalError("BinaryTerm: unknown binary operator");
}

// The term of `id` at `width` bits, extended as its own type is signed.
auto AtWidth(const ConstraintWriter& w, hir::ExprId id, std::int64_t width)
    -> diag::Result<mir::ExprId> {
  auto term = Term(w, id);
  if (!term) return term;
  return w.Term(
      BuiltinFn::kConstraintResize,
      {*term, w.Machine(width), w.Bool(IsSigned(w, id))});
}

// The elements of the array `array_id` names, folded into one term from
// `start`: `fold` answers the term after one more element, given the term so
// far and the element, as a term, and its index in the coordinates the array
// was declared with.
struct FoldedElement {
  const ConstraintWriter* writer;
  const WalkFrame* frame;
  mir::ExprId so_far;
  mir::LocalId element;
  mir::LocalId index;
};
auto FoldElements(
    const ConstraintWriter& w, hir::ExprId array_id, mir::ExprId start,
    const std::function<diag::Result<mir::ExprId>(const FoldedElement&)>& fold)
    -> diag::Result<mir::ExprId>;

// Whether `id` is an unpacked array whose elements a constraint can walk.
auto IsArray(const ConstraintWriter& w, hir::ExprId id) -> bool {
  const hir::Type& type =
      w.Lowerer().Owner().Hir().types.Get(HirExpr(w, id).type);
  return type.Is<hir::UnpackedArrayType>() ||
         type.Is<hir::DynamicArrayType>() || type.Is<hir::QueueType>();
}

auto InsideTerm(const ConstraintWriter& w, const hir::InsideExpr& e)
    -> diag::Result<mir::ExprId> {
  auto subject = Term(w, e.lhs);
  if (!subject) return subject;
  const mir::LocalId held = w.Bind(w.Unit().builtins.machine_int64, *subject);
  const std::int64_t width = WidthOf(w, e.lhs);
  const bool is_signed = IsSigned(w, e.lhs);
  const auto at_width = [&](hir::ExprId id) { return AtWidth(w, id, width); };
  std::optional<mir::ExprId> any;
  for (const hir::ExprId item : e.items) {
    mir::ExprId matches{};
    if (const auto* range =
            std::get_if<hir::ValueRangeExpr>(&HirExpr(w, item).data)) {
      std::vector<mir::ExprId> holds;
      if (range->lo.has_value()) {
        auto lo = at_width(*range->lo);
        if (!lo) return lo;
        holds.push_back(w.Term(
            BuiltinFn::kConstraintLogicalNot,
            {w.Term(
                BuiltinFn::kConstraintLess,
                {w.ReadTerm(held), *lo, w.Bool(is_signed)})}));
      }
      if (range->hi.has_value()) {
        auto hi = at_width(*range->hi);
        if (!hi) return hi;
        holds.push_back(w.Term(
            BuiltinFn::kConstraintLogicalNot,
            {w.Term(
                BuiltinFn::kConstraintLess,
                {*hi, w.ReadTerm(held), w.Bool(is_signed)})}));
      }
      matches = holds.empty() ? NumberTerm(w, 1) : holds.front();
      for (std::size_t i = 1; i < holds.size(); ++i) {
        matches = w.Term(BuiltinFn::kConstraintLogicalAnd, {matches, holds[i]});
      }
    } else if (IsArray(w, item)) {
      // An unpacked array in the set stands for each of its elements (LRM
      // 11.4.13).
      const mir::Type& element =
          w.Unit().types.Get(w.Lowerer().Owner().TranslateType(ChosenValueType(
              w.Lowerer().Owner().Hir().types, HirExpr(w, item).type)));
      const bool element_signed =
          element.Integral().signedness == mir::Signedness::kSigned;
      const mir::ExprId none = NumberTerm(w, 0);
      auto found = FoldElements(
          w, item, none,
          [&](const FoldedElement& e) -> diag::Result<mir::ExprId> {
            const mir::ExprId value = e.writer->Term(
                BuiltinFn::kConstraintResize,
                {e.writer->ReadTerm(e.element), e.writer->Machine(width),
                 e.writer->Bool(element_signed)});
            return e.writer->Term(
                BuiltinFn::kConstraintLogicalOr,
                {e.so_far, e.writer->Term(
                               BuiltinFn::kConstraintEqual,
                               {e.writer->ReadTerm(held), value})});
          });
      if (!found) return found;
      matches = *found;
    } else {
      auto value = at_width(item);
      if (!value) return value;
      matches = w.Term(BuiltinFn::kConstraintEqual, {w.ReadTerm(held), *value});
    }
    any = any.has_value()
              ? w.Term(BuiltinFn::kConstraintLogicalOr, {*any, matches})
              : matches;
  }
  return any.has_value() ? *any
                         : w.Term(
                               BuiltinFn::kConstraintResize,
                               {NumberTerm(w, 0), w.Machine(1), w.Bool(false)});
}

auto Term(const ConstraintWriter& w, hir::ExprId id)
    -> diag::Result<mir::ExprId> {
  if (NamesVariable(w, id)) {
    return VariableTerm(w, id);
  }
  if (const auto* primary = std::get_if<hir::PrimaryExpr>(&HirExpr(w, id).data);
      primary != nullptr) {
    if (const auto* bound =
            std::get_if<hir::IterationBindingRef>(&primary->data)) {
      return w.IterationTerm(*bound);
    }
  }
  if (const std::optional<Reduction> reduction = ReducedArray(w, id)) {
    return ReductionTerm(w, id, *reduction);
  }
  if (const std::optional<hir::ExprId> array = SizedArray(w, id)) {
    auto named = NameVariable(w, *array);
    if (!named) return std::unexpected(std::move(named.error()));
    return w.Term(
        BuiltinFn::kConstraintArraySize, {named->reference, named->value});
  }
  if (!MentionsVariable(w, id)) {
    return ConstantTerm(w, id);
  }
  const hir::Expr& expr = HirExpr(w, id);
  return std::visit(
      Overloaded{
          [&](const hir::UnaryExpr& e) { return UnaryTerm(w, e); },
          [&](const hir::BinaryExpr& e) { return BinaryTerm(w, id, e); },
          [&](const hir::ConditionalExpr& e) -> diag::Result<mir::ExprId> {
            std::optional<mir::ExprId> condition;
            for (const hir::ConditionClause& clause : e.conditions) {
              if (clause.pattern.has_value()) {
                return Unsupported(expr.span, "a pattern-matching condition");
              }
              auto truth = Term(w, clause.expr);
              if (!truth) return truth;
              condition = condition.has_value()
                              ? w.Term(
                                    BuiltinFn::kConstraintLogicalAnd,
                                    {*condition, *truth})
                              : *truth;
            }
            auto then_term = Term(w, e.then_value);
            if (!then_term) return then_term;
            auto else_term = Term(w, e.else_value);
            if (!else_term) return else_term;
            return w.Term(
                BuiltinFn::kConstraintSelect,
                {*condition, *then_term, *else_term});
          },
          [&](const hir::ConversionExpr& e) -> diag::Result<mir::ExprId> {
            if (!IsIntegral(w, e.operand) || !IsIntegral(w, id)) {
              return Unsupported(
                  expr.span, "a conversion that is not integral");
            }
            auto operand = Term(w, e.operand);
            if (!operand) return operand;
            return w.Term(
                BuiltinFn::kConstraintResize,
                {*operand, w.Machine(WidthOf(w, id)),
                 w.Bool(IsSigned(w, e.operand))});
          },
          [&](const hir::InsideExpr& e) { return InsideTerm(w, e); },
          [&](const hir::ConcatExpr& e) -> diag::Result<mir::ExprId> {
            std::optional<mir::ExprId> joined;
            for (const hir::ExprId operand : e.operands) {
              auto part = Term(w, operand);
              if (!part) return part;
              joined =
                  joined.has_value()
                      ? w.Term(
                            BuiltinFn::kConstraintConcatenate, {*joined, *part})
                      : *part;
            }
            return *joined;
          },
          [&](const hir::ElementSelectExpr& e) -> diag::Result<mir::ExprId> {
            auto whole = Term(w, e.base_value);
            if (!whole) return whole;
            auto index = PositionTerm(w, e.index);
            if (!index) return index;
            const mir::ExprId position =
                IndexPosition(w, DimensionOfSelected(w, e.base_value), *index);
            const std::int64_t width = WidthOf(w, id);
            return PartTerm(w, *whole, position, width);
          },
          [&](const hir::RangeSelectExpr& e) { return SelectTerm(w, id, e); },
          // LRM 18.5.11: a function is called before the constraints its
          // answer enters are solved, and the answer is a value they read;
          // the variables it is handed are chosen first, so the call reads
          // them as chosen.
          [&](const hir::CallExpr&) -> diag::Result<mir::ExprId> {
            if (auto r = WantVariables(w, id); !r) {
              return std::unexpected(std::move(r.error()));
            }
            return ConstantTerm(w, id);
          },
          [&](const hir::MemberAccessExpr& e) -> diag::Result<mir::ExprId> {
            const hir::Type& type = w.Lowerer().Owner().Hir().types.Get(
                HirExpr(w, e.base_value).type);
            // A union's members overlap at its least significant bits (LRM
            // 7.3.1); a tagged one's are reached through a check of its tag
            // (LRM 11.9), which no constraint carries out yet. A structure's
            // members lie most significant first, so a member sits above every
            // member declared after it.
            std::int64_t offset = 0;
            if (const auto* united = type.As<hir::PackedUnionType>()) {
              if (united->tagged) {
                return Unsupported(expr.span, "a member of a tagged union");
              }
            } else {
              const auto& packed = type.Get<hir::PackedStructType>();
              for (std::size_t i = e.field_index.value + 1;
                   i < packed.fields.size(); ++i) {
                offset += static_cast<std::int64_t>(
                    w.Unit()
                        .types
                        .Get(w.Lowerer().Owner().TranslateType(
                            packed.fields[i].type))
                        .Integral()
                        .bit_width);
              }
            }
            auto whole = Term(w, e.base_value);
            if (!whole) return whole;
            const mir::ExprId position = NumberTerm(w, offset);
            const std::int64_t width = WidthOf(w, id);
            return PartTerm(w, *whole, position, width);
          },
          // A name, a constant and a property are either a variable of the
          // problem or a value the constraint reads, which are answered above.
          [](const hir::PrimaryExpr&) -> diag::Result<mir::ExprId> {
            throw InternalError(
                "Term: a leaf that is neither a variable nor a value -- please "
                "report this as a bug");
          },
          [&](const hir::AssignExpr&) -> diag::Result<mir::ExprId> {
            return Unsupported(expr.span, "an assignment");
          },
          [&](const hir::IncDecExpr&) -> diag::Result<mir::ExprId> {
            return Unsupported(expr.span, "an increment or a decrement");
          },
          [](const hir::CalleeDefaultExpr&) -> diag::Result<mir::ExprId> {
            throw InternalError(
                "Term: an omitted argument's default stands only in the call "
                "omitting it -- please report this as a bug");
          },
          [](const hir::ConstantExpr&) -> diag::Result<mir::ExprId> {
            throw InternalError(
                "Term: a leaf that is neither a variable nor a value -- please "
                "report this as a bug");
          },
          [&](const hir::ValueRangeExpr&) -> diag::Result<mir::ExprId> {
            return Unsupported(
                expr.span, "a value range outside a set membership");
          },
          [](const hir::ClassPropertyAccessExpr&) -> diag::Result<mir::ExprId> {
            throw InternalError(
                "Term: a leaf that is neither a variable nor a value -- please "
                "report this as a bug");
          },
          [&](const hir::InterfaceMemberAccessExpr&)
              -> diag::Result<mir::ExprId> {
            return Unsupported(expr.span, "a member of an interface");
          },
          [&](const hir::InterfaceInstanceAccessExpr&)
              -> diag::Result<mir::ExprId> {
            return Unsupported(expr.span, "an interface instance");
          },
          [&](const hir::StreamingConcatExpr&) -> diag::Result<mir::ExprId> {
            return Unsupported(expr.span, "a streaming concatenation");
          },
          [&](const hir::ReplicationExpr&) -> diag::Result<mir::ExprId> {
            return Unsupported(expr.span, "a replication");
          },
          [&](const hir::AssignmentPatternExpr&) -> diag::Result<mir::ExprId> {
            return Unsupported(expr.span, "an assignment pattern");
          },
          [&](const hir::AssignmentPatternReplicationExpr&)
              -> diag::Result<mir::ExprId> {
            return Unsupported(expr.span, "an assignment pattern");
          },
          [&](const hir::DynamicArrayNewExpr&) -> diag::Result<mir::ExprId> {
            return Unsupported(expr.span, "a new[] expression");
          },
          [&](const hir::ClassNewExpr&) -> diag::Result<mir::ExprId> {
            return Unsupported(expr.span, "a new expression");
          },
          [&](const hir::AssociativeAssignmentPatternExpr&)
              -> diag::Result<mir::ExprId> {
            return Unsupported(expr.span, "an assignment pattern");
          },
          [&](const hir::AssignmentPatternKeyedExpr&)
              -> diag::Result<mir::ExprId> {
            return Unsupported(expr.span, "an assignment pattern");
          },
          [&](const hir::TaggedUnionExpr&) -> diag::Result<mir::ExprId> {
            return Unsupported(expr.span, "a tagged union expression");
          },
          [&](const hir::DynamicCastExpr&) -> diag::Result<mir::ExprId> {
            return Unsupported(expr.span, "a dynamic cast");
          },
          [&](const hir::CovergroupNewExpr&) -> diag::Result<mir::ExprId> {
            return Unsupported(expr.span, "a covergroup construction");
          },
          [&](const hir::CovergroupMethodCallExpr&)
              -> diag::Result<mir::ExprId> {
            return Unsupported(expr.span, "a covergroup method call");
          },
          [&](const hir::CovergroupOptionExpr&) -> diag::Result<mir::ExprId> {
            return Unsupported(expr.span, "a covergroup option");
          },
          [&](const hir::RandomizeExpr&) -> diag::Result<mir::ExprId> {
            return Unsupported(expr.span, "a call of randomize()");
          },
          [&](const hir::RandModeSetExpr&) -> diag::Result<mir::ExprId> {
            return Unsupported(expr.span, "rand_mode()");
          },
          [&](const hir::RandModeGetExpr&) -> diag::Result<mir::ExprId> {
            return Unsupported(expr.span, "rand_mode()");
          },
          [&](const hir::ConstraintModeSetExpr&) -> diag::Result<mir::ExprId> {
            return Unsupported(expr.span, "constraint_mode()");
          },
          [&](const hir::ConstraintModeGetExpr&) -> diag::Result<mir::ExprId> {
            return Unsupported(expr.span, "constraint_mode()");
          }},
      expr.data);
}

// `array.sum()` and its kin (LRM 18.5.7.2): the operation folded over a term
// per element, starting from what it leaves a value as, each element's term
// the element itself or what the `with` clause makes of it, at the width of
// the call's own type. The elements are counted where the description is
// stated, so a size a constraint chose is the one counted (LRM 18.5.7.1).
auto FoldElements(
    const ConstraintWriter& w, hir::ExprId array_id, mir::ExprId start,
    const std::function<diag::Result<mir::ExprId>(const FoldedElement&)>& fold)
    -> diag::Result<mir::ExprId> {
  UnitLowerer& unit_lowerer = w.Lowerer().Owner();
  mir::CompilationUnit& unit = w.Unit();
  mir::Builder block = w.Block();
  const mir::LocalId acc = w.Bind(unit.builtins.machine_int64, start);

  // Which positions the array has, and the index each stands for in the
  // coordinates the array was declared with, which is what `item.index` is.
  const hir::Expr& array = HirExpr(w, array_id);
  const hir::Type& array_type = unit_lowerer.Hir().types.Get(array.type);
  std::optional<hir::TypeId> element;
  std::int64_t left = 0;
  std::int64_t step = 1;
  std::optional<std::int64_t> fixed;
  if (const auto* fixed_array = array_type.As<hir::UnpackedArrayType>()) {
    element = fixed_array->element_type;
    left = fixed_array->dim.left;
    step = fixed_array->dim.left <= fixed_array->dim.right ? 1 : -1;
    fixed = std::abs(fixed_array->dim.right - fixed_array->dim.left) + 1;
  } else if (const auto* dynamic = array_type.As<hir::DynamicArrayType>()) {
    element = dynamic->element_type;
  } else if (const auto* queue = array_type.As<hir::QueueType>()) {
    element = queue->element_type;
  } else {
    return Unsupported(array.span, "this kind of array");
  }
  auto path = w.Lowerer().LowerAccessPath(array, w.Frame());
  if (!path) return std::unexpected(std::move(path.error()));
  const SettledPath settled =
      SettledForRead(unit_lowerer, w.Frame(), *std::move(path));
  const mir::ExprId count =
      fixed.has_value()
          ? mir::BuildIntLiteral(unit, block, *fixed)
          : block.Add(
                mir::Expr{
                    .data =
                        mir::CallExpr{
                            .callee =
                                mir::Direct{
                                    .target = BuiltinFn::kSize,
                                    .receiver = PathValue(
                                        unit, block, NamedIn(settled, block))},
                            .arguments = {}},
                    .type = unit.builtins.int_type});
  const mir::LocalId position =
      w.Frame().bindings->DeclareAnonymous(unit.builtins.int_type);

  mir::Block pass;
  mir::Builder pass_builder = block.Into(pass);
  const WalkFrame at = w.Frame().WithBlock(&pass);
  const auto read_position = [&] {
    return pass_builder.ReadLocal(position, unit.builtins.int_type);
  };
  const mir::ExprId index =
      left == 0 && step == 1
          ? read_position()
          : pass_builder.Add(
                mir::Expr{
                    .data =
                        mir::BinaryExpr{
                            .op = mir::BinaryOp::kAdd,
                            .lhs =
                                mir::BuildIntLiteral(unit, pass_builder, left),
                            .rhs =
                                step == 1
                                    ? read_position()
                                    : pass_builder.Add(
                                          mir::Expr{
                                              .data =
                                                  mir::UnaryExpr{
                                                      .op =
                                                          mir::UnaryOp::kMinus,
                                                      .operand =
                                                          read_position()},
                                              .type = unit.builtins.int_type})},
                    .type = unit.builtins.int_type});
  const mir::LocalId index_local =
      at.bindings->DeclareAnonymous(unit.builtins.int_type);
  pass_builder.AppendStmt(
      mir::LocalDeclStmt{.target = index_local, .init = index});
  const mir::TypeId element_type = unit_lowerer.TranslateType(*element);
  const AccessPath array_path = NamedIn(settled, pass_builder);
  const DescentStep to_element = ElementStep(
      unit_lowerer, pass_builder, array.type,
      pass_builder.ReadLocal(index_local, unit.builtins.int_type),
      element_type);
  const AccessPath element_path = DescendInto(array_path, to_element);
  const ConstraintWriter in_pass(w.Lowerer(), at);
  const mir::TypeId reference_type =
      *ParamTypeOf(unit, element_type, hir::ParamDirection::kRef);
  const mir::LocalId reference = in_pass.Bind(
      reference_type,
      AtType(
          unit, pass_builder, PathReference(unit, pass_builder, element_path),
          reference_type));
  const mir::ExprId element_term = in_pass.Term(
      BuiltinFn::kConstraintVariable,
      {pass_builder.ReadLocal(reference, reference_type),
       pass_builder.Read(pass_builder.Add(pass_builder.WrapperContents(
           unit.types, pass_builder.ReadLocal(reference, reference_type),
           element_type)))});
  const mir::LocalId element_local =
      in_pass.Bind(unit.builtins.machine_int64, element_term);
  auto next = fold(
      FoldedElement{
          .writer = &in_pass,
          .frame = &at,
          .so_far = in_pass.ReadTerm(acc),
          .element = element_local,
          .index = index_local});
  if (!next) return next;
  pass_builder.AppendStmt(
      mir::ExprStmt{
          .expr = pass_builder.Add(
              mir::MakeAssignExpr(
                  unit.builtins,
                  pass_builder.LocalPlace(acc, unit.builtins.machine_int64),
                  *next))});
  const mir::BlockId body = block.AddScope(std::move(pass));
  block.AppendStmt(
      BuildCountingLoopStmt(unit, w.Frame(), block, count, position, body));
  return w.ReadTerm(acc);
}

auto ReductionTerm(
    const ConstraintWriter& w, hir::ExprId id, const Reduction& reduction)
    -> diag::Result<mir::ExprId> {
  const std::int64_t width = WidthOf(w, id);
  const auto at_width = [&](mir::ExprId term, bool is_signed) {
    return w.Term(
        BuiltinFn::kConstraintResize,
        {term, w.Machine(width), w.Bool(is_signed)});
  };
  const mir::ExprId zero = at_width(NumberTerm(w, 0), false);
  BuiltinFn fold{};
  mir::ExprId start{};
  switch (reduction.method->method) {
    case hir::ArrayIteratorMethod::kSum:
      fold = BuiltinFn::kConstraintAdd;
      start = zero;
      break;
    case hir::ArrayIteratorMethod::kProduct:
      fold = BuiltinFn::kConstraintMultiply;
      start = at_width(NumberTerm(w, 1), false);
      break;
    case hir::ArrayIteratorMethod::kAnd:
      fold = BuiltinFn::kConstraintBitwiseAnd;
      start = w.Term(BuiltinFn::kConstraintBitwiseNot, {zero});
      break;
    case hir::ArrayIteratorMethod::kOr:
      fold = BuiltinFn::kConstraintBitwiseOr;
      start = zero;
      break;
    case hir::ArrayIteratorMethod::kXor:
      fold = BuiltinFn::kConstraintBitwiseXor;
      start = zero;
      break;
    case hir::ArrayIteratorMethod::kSort:
    case hir::ArrayIteratorMethod::kRsort:
    case hir::ArrayIteratorMethod::kFind:
    case hir::ArrayIteratorMethod::kFindIndex:
    case hir::ArrayIteratorMethod::kFindFirst:
    case hir::ArrayIteratorMethod::kFindFirstIndex:
    case hir::ArrayIteratorMethod::kFindLast:
    case hir::ArrayIteratorMethod::kFindLastIndex:
    case hir::ArrayIteratorMethod::kMin:
    case hir::ArrayIteratorMethod::kMax:
    case hir::ArrayIteratorMethod::kUnique:
    case hir::ArrayIteratorMethod::kUniqueIndex:
    case hir::ArrayIteratorMethod::kMap:
      throw InternalError("ReductionTerm: a method that reduces nothing");
  }
  return FoldElements(
      w, reduction.array, start,
      [&](const FoldedElement& e) -> diag::Result<mir::ExprId> {
        const mir::TypeId int_type = e.writer->Unit().builtins.int_type;
        mir::ExprId item{};
        if (reduction.method->with_clause.has_value()) {
          const ConstraintWriter bound = w.Iterating(
              *e.frame,
              ConstraintWriter::Iteration{
                  .clause = reduction.method->with_clause->id,
                  .element = e.element,
                  .index = e.writer->Bind(
                      e.writer->Unit().builtins.machine_int64,
                      e.writer->Term(
                          BuiltinFn::kConstraintConstant,
                          {e.writer->Block().ReadLocal(e.index, int_type)}))});
          auto term = Term(bound, reduction.method->with_clause->expr);
          if (!term) return term;
          item = bound.Term(
              BuiltinFn::kConstraintResize,
              {*term, bound.Machine(width),
               bound.Bool(
                   IsSigned(bound, reduction.method->with_clause->expr))});
        } else {
          item = e.writer->Term(
              BuiltinFn::kConstraintResize,
              {e.writer->ReadTerm(e.element), e.writer->Machine(width),
               e.writer->Bool(false)});
        }
        return e.writer->Term(fold, {e.so_far, item});
      });
}

// A constraint (LRM 18.5, 18.7) is stated to the problem of the call of
// `randomize()` that reaches it, as the calls describing it: each operand that
// names storage is a variable of the problem, each operator one of its
// operations, and each constraint a requirement or a preference. A value the
// constraint reads that names no storage is evaluated where the constraint is
// and handed over as a constant. The constraints under `id` are appended to
// the writer's block, each holding where `guard` does where one is: a
// constraint under an implication or a condition holds where that condition
// does.
auto LowerConstraintInto(
    const ConstraintWriter& w, hir::ConstraintId id,
    std::optional<mir::LocalId> guard) -> diag::Result<void>;

// The same for `constraint`, written where `w` is, which is where the source
// wrote it.
auto LowerConstraintInto(
    const ConstraintWriter& w, const hir::Constraint& constraint,
    std::optional<mir::LocalId> guard) -> diag::Result<void>;

auto Guarded(
    const ConstraintWriter& w, std::optional<mir::LocalId> guard,
    mir::ExprId condition) -> mir::ExprId {
  if (!guard.has_value()) {
    return condition;
  }
  return w.Term(
      BuiltinFn::kConstraintLogicalOr,
      {w.Term(BuiltinFn::kConstraintLogicalNot, {w.ReadTerm(*guard)}),
       condition});
}

auto Within(
    const ConstraintWriter& w, std::optional<mir::LocalId> guard,
    mir::ExprId condition) -> mir::LocalId {
  const mir::ExprId truth = w.Term(BuiltinFn::kConstraintReduceOr, {condition});
  return w.Bind(
      w.Unit().builtins.machine_int64,
      guard.has_value()
          ? w.Term(
                BuiltinFn::kConstraintLogicalAnd, {w.ReadTerm(*guard), truth})
          : truth);
}

auto LowerConstraintInto(
    const ConstraintWriter& w, hir::ConstraintId id,
    std::optional<mir::LocalId> guard) -> diag::Result<void> {
  const hir::Constraint& constraint = w.Lowerer().HirBody().constraints.Get(id);
  return LowerConstraintInto(
      w.Within(w.Frame().WithSpan(constraint.span), {}), constraint, guard);
}

auto LowerConstraintInto(
    const ConstraintWriter& w, const hir::Constraint& constraint,
    std::optional<mir::LocalId> guard) -> diag::Result<void> {
  return std::visit(
      Overloaded{
          [&](const hir::ConstraintList& c) -> diag::Result<void> {
            for (const hir::ConstraintId item : c.items) {
              if (auto r = LowerConstraintInto(w, item, guard); !r) return r;
            }
            return {};
          },
          [&](const hir::ExpressionConstraint& c) -> diag::Result<void> {
            auto term = Term(w, c.condition);
            if (!term) return std::unexpected(std::move(term.error()));
            w.Effect(
                c.is_soft ? BuiltinFn::kConstraintPrefer
                          : BuiltinFn::kConstraintRequire,
                {Guarded(w, guard, *term)});
            return {};
          },
          // LRM 18.5.3: each item's bounds at the subject's width, compared
          // as the subject is, and its weight as the value it reads.
          [&](const hir::DistConstraint& c) -> diag::Result<void> {
            auto subject = Term(w, c.subject);
            if (!subject) return std::unexpected(std::move(subject.error()));
            const std::int64_t width = WidthOf(w, c.subject);
            const mir::LocalId distribution = w.Bind(
                w.Unit().builtins.machine_int64,
                w.Term(
                    BuiltinFn::kConstraintDistribution,
                    {*subject,
                     guard.has_value() ? w.ReadTerm(*guard) : NumberTerm(w, 1),
                     w.Bool(IsSigned(w, c.subject)), w.Bool(c.is_soft)}));
            const auto weight_of = [&](std::optional<hir::ExprId> weight)
                -> diag::Result<mir::ExprId> {
              return weight.has_value()
                         ? Term(w, *weight)
                         : diag::Result<mir::ExprId>{NumberTerm(w, 1)};
            };
            for (const hir::DistItem& item : c.items) {
              auto low = AtWidth(w, item.low, width);
              if (!low) return std::unexpected(std::move(low.error()));
              const mir::LocalId held_low =
                  w.Bind(w.Unit().builtins.machine_int64, *low);
              mir::ExprId high = w.ReadTerm(held_low);
              if (item.high.has_value()) {
                auto upper = AtWidth(w, *item.high, width);
                if (!upper) return std::unexpected(std::move(upper.error()));
                high = *upper;
              }
              auto weight = weight_of(item.weight);
              if (!weight) return std::unexpected(std::move(weight.error()));
              w.Effect(
                  BuiltinFn::kConstraintDistributionItem,
                  {w.ReadTerm(distribution), w.ReadTerm(held_low), high,
                   *weight, w.Bool(!item.weight_per_range)});
            }
            if (c.default_weight.has_value()) {
              auto weight = weight_of(c.default_weight);
              if (!weight) return std::unexpected(std::move(weight.error()));
              w.Effect(
                  BuiltinFn::kConstraintDistributionDefault,
                  {w.ReadTerm(distribution), *weight});
            }
            return {};
          },
          [&](const hir::ImplicationConstraint& c) -> diag::Result<void> {
            auto predicate = Term(w, c.predicate);
            if (!predicate)
              return std::unexpected(std::move(predicate.error()));
            return LowerConstraintInto(w, c.body, Within(w, guard, *predicate));
          },
          [&](const hir::ConditionalConstraint& c) -> diag::Result<void> {
            auto predicate = Term(w, c.predicate);
            if (!predicate)
              return std::unexpected(std::move(predicate.error()));
            const mir::LocalId held =
                w.Bind(w.Unit().builtins.machine_int64, *predicate);
            if (auto r = LowerConstraintInto(
                    w, c.then_body, Within(w, guard, w.ReadTerm(held)));
                !r) {
              return r;
            }
            if (!c.else_body.has_value()) {
              return {};
            }
            return LowerConstraintInto(
                w, *c.else_body,
                Within(
                    w, guard,
                    w.Term(
                        BuiltinFn::kConstraintLogicalNot, {w.ReadTerm(held)})));
          },
          // LRM 18.5.4: an array stands for each of its elements, which
          // only the running program counts.
          [&](const hir::UniquenessConstraint& c) -> diag::Result<void> {
            const mir::LocalId set = w.Bind(
                w.Unit().builtins.machine_int64,
                w.Term(BuiltinFn::kConstraintUniqueSet, {}));
            for (const hir::ExprId item : c.items) {
              if (IsIntegral(w, item)) {
                auto term = Term(w, item);
                if (!term) return std::unexpected(std::move(term.error()));
                w.Effect(
                    BuiltinFn::kConstraintUniqueMember,
                    {w.ReadTerm(set), *term});
                continue;
              }
              auto named = NameVariable(w, item);
              if (!named) return std::unexpected(std::move(named.error()));
              w.Effect(
                  BuiltinFn::kConstraintUniqueArray,
                  {w.ReadTerm(set), named->reference, named->value});
            }
            w.Effect(
                BuiltinFn::kConstraintUniqueRequire,
                {w.ReadTerm(set),
                 guard.has_value() ? w.ReadTerm(*guard) : NumberTerm(w, 1)});
            return {};
          },
          // LRM 18.5.7: the body once for each tuple of indices, in the loops
          // a `foreach` statement over the array has.
          [&](const hir::ForeachConstraint& c) -> diag::Result<void> {
            auto loops = LowerForeachLoops(
                w.Lowerer(), w.Frame(), std::nullopt, c.array, c.loop_vars,
                [&](const WalkFrame& at,
                    BreakTarget&) -> diag::Result<mir::Block> {
                  mir::Block pass;
                  if (auto r = LowerConstraintInto(
                          w.Within(at.WithBlock(&pass), c.loop_vars), c.body,
                          guard);
                      !r) {
                    return std::unexpected(std::move(r.error()));
                  }
                  return pass;
                });
            if (!loops) return std::unexpected(std::move(loops.error()));
            w.Block().AppendStmt(*std::move(loops));
            return {};
          },
          [&](const hir::SolveBeforeConstraint& c) -> diag::Result<void> {
            for (const hir::ExprId before : c.solve) {
              for (const hir::ExprId after : c.after) {
                auto b = OrderedTerm(w, before);
                if (!b) return std::unexpected(std::move(b.error()));
                auto a = OrderedTerm(w, after);
                if (!a) return std::unexpected(std::move(a.error()));
                w.Effect(BuiltinFn::kConstraintSolveBefore, {*b, *a});
              }
            }
            return {};
          },
          [&](const hir::DisableSoftConstraint& c) -> diag::Result<void> {
            auto target = OrderedTerm(w, c.target);
            if (!target) return std::unexpected(std::move(target.error()));
            w.Effect(BuiltinFn::kConstraintDisableSoft, {*target});
            return {};
          }},
      constraint.data);
}

auto ListVariables(
    const ConstraintWriter& w, std::span<const hir::ExprId> variables)
    -> diag::Result<void> {
  for (const hir::ExprId variable : variables) {
    // A listed array is each of its elements (LRM 18.11).
    UnitLowerer& unit_lowerer = w.Lowerer().Owner();
    const mir::TypeId chosen = unit_lowerer.TranslateType(
        ChosenValueType(unit_lowerer.Hir().types, HirExpr(w, variable).type));
    if (!w.Unit().types.Get(chosen).IsIntegral()) {
      return diag::Fail(
          HirExpr(w, variable).span, diag::DiagCode::kUnsupportedExpressionForm,
          "randomizing a listed variable that is not integral, or an array of "
          "integral values, is not yet supported");
    }
    auto named = NameVariable(w, variable);
    if (!named) return std::unexpected(std::move(named.error()));
    if (const std::optional<mir::ExprId> members = ChosenMembers(
            w.Lowerer().Owner(), w.Block(), HirExpr(w, variable).type)) {
      w.Effect(BuiltinFn::kConstraintMembers, {named->reference, *members});
    }
    w.Effect(BuiltinFn::kRandomizeList, {named->reference, named->value});
  }
  return {};
}

auto OnArgument(const ConstraintWriter& w, hir::ExprId on)
    -> diag::Result<mir::ExprId> {
  auto value = w.Lowerer().LowerExpr(HirExpr(w, on), w.Frame());
  if (!value) return std::unexpected(std::move(value.error()));
  return ReduceToCondition(
      w.Unit(), w.Block(), w.Block().Add(*std::move(value)));
}

// The object a call acts on, as the value reaching it, read where a variable
// holds it.
auto ObjectArgument(const ConstraintWriter& w, hir::ExprId object)
    -> diag::Result<mir::ExprId> {
  auto value = w.Lowerer().LowerExpr(HirExpr(w, object), w.Frame());
  if (!value) return std::unexpected(std::move(value.error()));
  return w.Block().AsValue(w.Unit().types, w.Block().Add(*std::move(value)));
}

auto StringArgument(const ConstraintWriter& w, const std::string& text)
    -> mir::ExprId {
  return BuildStringValueExpr(w.Unit(), w.Block(), text);
}

auto StaticKey(
    const ConstraintWriter& w, const hir::StaticConstraintBlockMode& mode)
    -> std::string {
  return StaticConstraintModeKey(
      w.Lowerer().Owner().Hir(), mode.introducer, mode.block);
}

}  // namespace

auto LowerConstraintStmt(
    ProcessLowerer& lowerer, const WalkFrame& frame,
    std::optional<std::string> label, const hir::ConstraintStmt& stmt)
    -> diag::Result<mir::Stmt> {
  mir::Block body;
  if (auto r = LowerConstraintInto(
          ConstraintWriter(lowerer, frame.WithBlock(&body)), stmt.constraints,
          std::nullopt);
      !r) {
    return std::unexpected(std::move(r.error()));
  }
  const mir::BlockId scope = frame.Builder().AddScope(std::move(body));
  return mir::Stmt{
      .label = std::move(label), .data = mir::BlockStmt{.scope = scope}};
}

auto LowerRandomizeExpr(
    ProcessLowerer& lowerer, const WalkFrame& frame,
    const hir::RandomizeExpr& call, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  BlockBuilder steps(frame);
  const ConstraintWriter outer(lowerer, steps.Frame());
  mir::CompilationUnit& unit = outer.Unit();
  std::optional<mir::LocalId> object;
  std::optional<mir::LocalId> problem;
  std::vector<hir::ExprId> listed;
  auto begun = std::visit(
      Overloaded{
          [&](const hir::RandomizedClassObject& target) -> diag::Result<void> {
            auto handle = ObjectArgument(outer, target.object);
            if (!handle) return std::unexpected(std::move(handle.error()));
            const mir::TypeId handle_type = outer.Block().Get(*handle).type;
            object = outer.Bind(handle_type, *handle);
            const BuiltinFn begin = std::visit(
                Overloaded{
                    [](const hir::DeclaredRandomVariables&) {
                      return BuiltinFn::kRandomizeObject;
                    },
                    [&](const hir::ListedRandomVariables& l) {
                      listed = l.variables;
                      return BuiltinFn::kRandomizeObjectListed;
                    }},
                target.variables);
            problem = outer.Bind(
                unit.builtins.constraint_problem,
                outer.Call(
                    begin,
                    {outer.Block().ReadLocal(*object, handle_type),
                     outer.Block().Add(
                         BuildCurrentRuntimeCallExpr(lowerer.Owner()))},
                    unit.builtins.constraint_problem));
            return {};
          },
          [&](const hir::RandomizedScopeVariables& target)
              -> diag::Result<void> {
            listed = target.variables;
            problem = outer.Bind(
                unit.builtins.constraint_problem,
                outer.Call(
                    BuiltinFn::kRandomizeScope,
                    {outer.Block().Add(
                        BuildCurrentRuntimeCallExpr(lowerer.Owner()))},
                    unit.builtins.constraint_problem));
            return {};
          }},
      call.target);
  if (!begun) return std::unexpected(std::move(begun.error()));

  WalkFrame within = steps.Frame().WithConstraintProblem(*problem);
  if (object.has_value()) {
    within = within.WithRandomizedObject(*object);
  }
  // The description is stated again once what is chosen first is settled --
  // the sizes a constraint chose (LRM 18.5.7.1), the variables a function is
  // handed (LRM 18.5.11) -- with those held; a call choosing neither states it
  // once.
  mir::Block pass;
  const ConstraintWriter w(lowerer, within.WithBlock(&pass));
  if (auto r = ListVariables(w, listed); !r) {
    return std::unexpected(std::move(r.error()));
  }
  w.Effect(BuiltinFn::kRandomizeConstrain, {});
  if (call.with.has_value()) {
    if (auto r = LowerConstraintInto(w, *call.with, std::nullopt); !r) {
      return std::unexpected(std::move(r.error()));
    }
  }
  const ConstraintWriter after(lowerer, within);
  after.Block().AppendStmt(
      mir::DoWhileStmt{
          .condition = after.Call(
              BuiltinFn::kRandomizeSettleFirst, {after.Problem()},
              unit.builtins.machine_bool),
          .scope = after.Block().AddScope(std::move(pass))});
  const mir::ExprId answer =
      after.Call(BuiltinFn::kRandomizeFinish, {after.Problem()}, result_type);
  return steps.Build(answer);
}

auto LowerRandModeSetExpr(
    ProcessLowerer& lowerer, const WalkFrame& frame,
    const hir::RandModeSetExpr& call) -> diag::Result<mir::Expr> {
  BlockBuilder steps(frame);
  const ConstraintWriter w(lowerer, steps.Frame());
  auto done = std::visit(
      Overloaded{
          [&](const hir::RandomVariableMode& m) -> diag::Result<mir::ExprId> {
            auto named = NameModeVariables(w, m.variable);
            if (!named) return std::unexpected(std::move(named.error()));
            auto on = OnArgument(w, call.on);
            if (!on) return on;
            const mir::TypeId on_type = w.Block().Get(*on).type;
            const mir::LocalId held = w.Bind(on_type, *on);
            // Each call but the last is a statement; the last is the value
            // the call expression answers.
            std::optional<mir::ExprId> set;
            for (const NamedModeVariable& variable : *named) {
              if (set.has_value()) {
                w.Block().AppendStmt(mir::ExprStmt{.expr = *set});
              }
              set = w.Call(
                  BuiltinFn::kRandModeSet,
                  {variable.declared, variable.value, variable.part,
                   w.Block().ReadLocal(held, on_type)},
                  w.Unit().builtins.void_type);
            }
            return *set;
          },
          [&](const hir::ObjectModes& m) -> diag::Result<mir::ExprId> {
            auto object = ObjectArgument(w, m.object);
            if (!object) return object;
            auto on = OnArgument(w, call.on);
            if (!on) return on;
            return w.Call(
                BuiltinFn::kObjectRandModeSet, {*object, *on},
                w.Unit().builtins.void_type);
          }},
      call.target);
  if (!done) return std::unexpected(std::move(done.error()));
  return steps.Build(*done);
}

auto LowerRandModeGetExpr(
    ProcessLowerer& lowerer, const WalkFrame& frame,
    const hir::RandModeGetExpr& call, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  BlockBuilder steps(frame);
  const ConstraintWriter w(lowerer, steps.Frame());
  auto named = NameModeVariables(w, call.target.variable);
  if (!named) return std::unexpected(std::move(named.error()));
  // The value form names one variable (LRM 18.8), which the front end checks.
  const NamedModeVariable& variable = named->front();
  return steps.Build(w.Call(
      BuiltinFn::kRandModeGet,
      {variable.declared, variable.value, variable.part}, result_type));
}

auto ChosenValueType(const hir::TypePool& types, hir::TypeId type)
    -> hir::TypeId {
  const hir::Type& held = types.Get(type);
  if (const auto* array = held.As<hir::UnpackedArrayType>()) {
    return ChosenValueType(types, array->element_type);
  }
  if (const auto* array = held.As<hir::DynamicArrayType>()) {
    return ChosenValueType(types, array->element_type);
  }
  if (const auto* queue = held.As<hir::QueueType>()) {
    return ChosenValueType(types, queue->element_type);
  }
  return type;
}

auto ChosenMembers(
    UnitLowerer& unit_lowerer, mir::Builder block, hir::TypeId type)
    -> std::optional<mir::ExprId> {
  const mir::TypeId value = unit_lowerer.TranslateType(
      ChosenValueType(unit_lowerer.Hir().types, type));
  if (!unit_lowerer.Unit().types.Get(value).Is<mir::EnumType>()) {
    return std::nullopt;
  }
  return mir::BuildEnumTableRef(unit_lowerer.Unit(), block, value);
}

auto StaticConstraintModeKey(
    const hir::CompilationUnit& unit, hir::ClassId introducer,
    std::string_view block) -> std::string {
  const hir::ClassKey& key = unit.classes.KeyOf(introducer);
  return std::format(
      "{}::{}::{}", key.unit_name, support::DisplayOf(key.class_path), block);
}

auto LowerConstraintModeSetExpr(
    ProcessLowerer& lowerer, const WalkFrame& frame,
    const hir::ConstraintModeSetExpr& call) -> diag::Result<mir::Expr> {
  BlockBuilder steps(frame);
  const ConstraintWriter w(lowerer, steps.Frame());
  const mir::TypeId void_type = w.Unit().builtins.void_type;
  auto done = std::visit(
      Overloaded{
          [&](const hir::ConstraintBlockMode& m) -> diag::Result<mir::ExprId> {
            auto object = ObjectArgument(w, m.object);
            if (!object) return object;
            auto on = OnArgument(w, call.on);
            if (!on) return on;
            return w.Call(
                BuiltinFn::kConstraintModeSet,
                {*object, StringArgument(w, m.name), *on}, void_type);
          },
          [&](const hir::StaticConstraintBlockMode& m)
              -> diag::Result<mir::ExprId> {
            auto on = OnArgument(w, call.on);
            if (!on) return on;
            return w.Call(
                BuiltinFn::kStaticConstraintModeSet,
                {StringArgument(w, StaticKey(w, m)), *on}, void_type);
          },
          [&](const hir::ObjectModes& m) -> diag::Result<mir::ExprId> {
            auto object = ObjectArgument(w, m.object);
            if (!object) return object;
            auto on = OnArgument(w, call.on);
            if (!on) return on;
            return w.Call(
                BuiltinFn::kObjectConstraintModeSet, {*object, *on}, void_type);
          }},
      call.target);
  if (!done) return std::unexpected(std::move(done.error()));
  return steps.Build(*done);
}

auto LowerConstraintModeGetExpr(
    ProcessLowerer& lowerer, const WalkFrame& frame,
    const hir::ConstraintModeGetExpr& call, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  BlockBuilder steps(frame);
  const ConstraintWriter w(lowerer, steps.Frame());
  auto done = std::visit(
      Overloaded{
          [&](const hir::ConstraintBlockMode& m) -> diag::Result<mir::ExprId> {
            auto object = ObjectArgument(w, m.object);
            if (!object) return object;
            return w.Call(
                BuiltinFn::kConstraintModeGet,
                {*object, StringArgument(w, m.name)}, result_type);
          },
          [&](const hir::StaticConstraintBlockMode& m)
              -> diag::Result<mir::ExprId> {
            return w.Call(
                BuiltinFn::kStaticConstraintModeGet,
                {StringArgument(w, StaticKey(w, m))}, result_type);
          }},
      call.target);
  if (!done) return std::unexpected(std::move(done.error()));
  return steps.Build(*done);
}

}  // namespace lyra::lowering::hir_to_mir
