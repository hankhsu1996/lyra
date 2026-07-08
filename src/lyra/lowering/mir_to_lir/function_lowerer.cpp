#include "lyra/lowering/mir_to_lir/function_lowerer.hpp"

#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/integral_constant.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/lowering/mir_to_lir/unit_lowerer.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::mir_to_lir {

namespace {

auto TranslateIntegralConstant(const mir::IntegralConstant& c)
    -> lir::IntegralConstant {
  return lir::IntegralConstant{
      .value_words = c.value_words,
      .state_words = c.state_words,
      .width = c.width,
      .signedness = c.signedness == mir::Signedness::kSigned
                        ? lir::Signedness::kSigned
                        : lir::Signedness::kUnsigned,
      .state_kind = c.state_kind == mir::IntegralStateKind::kFourState
                        ? lir::IntegralStateKind::kFourState
                        : lir::IntegralStateKind::kTwoState};
}

auto LowerCallTarget(
    const mir::Callee& callee, lir::ClassId current_class, lir::TypeId result)
    -> diag::Result<lir::CallTarget> {
  return std::visit(
      Overloaded{
          [&](const mir::Direct& d) -> diag::Result<lir::CallTarget> {
            if (d.qualification.has_value()) {
              return diag::Fail(
                  diag::DiagCode::kUnsupportedExpressionForm,
                  "mir_to_lir: qualified call is not yet lowerable to LIR");
            }
            return std::visit(
                Overloaded{
                    [&](const mir::MethodId& m)
                        -> diag::Result<lir::CallTarget> {
                      return lir::CallTarget{lir::MethodTarget{
                          .method = lir::MethodRef{
                              .class_id = current_class, .index = m.value}}};
                    },
                    [](const support::BuiltinFn& fn)
                        -> diag::Result<lir::CallTarget> {
                      return lir::CallTarget{lir::BuiltinTarget{.fn = fn}};
                    }},
                d.target);
          },
          [&](const mir::Construct&) -> diag::Result<lir::CallTarget> {
            return lir::CallTarget{lir::ConstructTarget{.result = result}};
          },
          [](const mir::Indirect&) -> diag::Result<lir::CallTarget> {
            return diag::Fail(
                diag::DiagCode::kUnsupportedExpressionForm,
                "mir_to_lir: indirect (closure) call is not yet lowerable to "
                "LIR");
          }},
      callee);
}

}  // namespace

FunctionLowerer::FunctionLowerer(
    UnitLowerer& unit, const mir::CallableCode& code, std::string name,
    lir::ClassId current_class)
    : unit_(&unit),
      code_(&code),
      name_(std::move(name)),
      current_class_(current_class),
      local_to_value_(code.locals.size(), std::nullopt) {
}

auto FunctionLowerer::Run() -> diag::Result<lir::Function> {
  fn_.name = std::move(name_);
  // A coroutine-bodied callable lowers to its step body, which returns void;
  // the coroutine value is a closure built where the callable is referenced.
  fn_.result_type = std::holds_alternative<mir::CoroutineType>(
                        unit_->Mir().types.Get(code_->result_type).data)
                        ? unit_->TranslateType(unit_->Mir().builtins.void_type)
                        : unit_->TranslateType(code_->result_type);

  for (const mir::LocalId param : code_->params) {
    const mir::LocalDecl& decl = code_->locals.Get(param);
    const lir::ValueId value = fn_.values.Add(
        lir::Local{
            .name = decl.name,
            .type = unit_->TranslateType(decl.type),
            .kind = lir::LocalKind::kParam});
    fn_.params.push_back(value);
    local_to_value_[param.value] = value;
  }

  for (const mir::StmtId sid : code_->body.root_stmts) {
    auto lowered = LowerStmtInto(code_->body.stmts.Get(sid));
    if (!lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
  }
  if (!terminated_) {
    block_.terminator = lir::Terminator{
        .data = lir::ReturnTerm{.value = std::nullopt, .is_coroutine = false}};
  }
  fn_.blocks.push_back(std::move(block_));
  return std::move(fn_);
}

auto FunctionLowerer::LowerStmtInto(const mir::Stmt& stmt)
    -> diag::Result<void> {
  return std::visit(
      Overloaded{
          [](const mir::EmptyStmt&) -> diag::Result<void> { return {}; },
          [&](const mir::ExprStmt& s) -> diag::Result<void> {
            auto lowered = LowerExpr(s.expr);
            if (!lowered) {
              return std::unexpected(std::move(lowered.error()));
            }
            return {};
          },
          [&](const mir::ReturnStmt& s) -> diag::Result<void> {
            std::optional<lir::Operand> value;
            if (s.value.has_value()) {
              auto lowered = LowerExpr(*s.value);
              if (!lowered) {
                return std::unexpected(std::move(lowered.error()));
              }
              value = *std::move(lowered);
            }
            block_.terminator = lir::Terminator{
                .data = lir::ReturnTerm{
                    .value = std::move(value),
                    .is_coroutine = s.is_coroutine_return}};
            terminated_ = true;
            return {};
          },
          [](const auto&) -> diag::Result<void> {
            return diag::Fail(
                diag::DiagCode::kUnsupportedStatementForm,
                "mir_to_lir: MIR statement form is not yet lowerable to LIR");
          }},
      stmt.data);
}

auto FunctionLowerer::LowerExpr(mir::ExprId id) -> diag::Result<lir::Operand> {
  const mir::Expr& expr = code_->body.exprs.Get(id);
  const mir::TypeId type = expr.type;
  return std::visit(
      Overloaded{
          [&](const mir::IntegerLiteral& lit) -> diag::Result<lir::Operand> {
            return lir::Operand{lir::IntConst{
                .value = TranslateIntegralConstant(lit.value),
                .type = unit_->TranslateType(type)}};
          },
          [&](const mir::StringLiteral& lit) -> diag::Result<lir::Operand> {
            return lir::Operand{lir::StrConst{
                .value = lit.value, .type = unit_->TranslateType(type)}};
          },
          [&](const mir::LocalRef& ref) -> diag::Result<lir::Operand> {
            const std::optional<lir::ValueId> value =
                local_to_value_[ref.var.value];
            if (!value.has_value()) {
              return diag::Fail(
                  diag::DiagCode::kUnsupportedExpressionForm,
                  "mir_to_lir: reference to an unlowered local");
            }
            return lir::Operand{lir::Use{.value = *value}};
          },
          [&](const mir::CallExpr& call) -> diag::Result<lir::Operand> {
            std::vector<lir::Operand> args;
            args.reserve(call.arguments.size());
            for (const mir::ExprId arg : call.arguments) {
              auto lowered = LowerExpr(arg);
              if (!lowered) {
                return std::unexpected(std::move(lowered.error()));
              }
              args.push_back(*std::move(lowered));
            }
            const lir::TypeId result_type = unit_->TranslateType(type);

            // A coroutine is a runtime value like any other: the runtime builds
            // it from an entry code reference and its environment (the
            // receiver), and it is reached as an opaque handle. It is
            // constructed through the same Construct path as any other runtime
            // value; the coroutine call protocol stays the result type.
            if (std::holds_alternative<mir::CoroutineType>(
                    unit_->Mir().types.Get(type).data)) {
              const auto* direct = std::get_if<mir::Direct>(&call.callee);
              const auto* method =
                  direct != nullptr
                      ? std::get_if<mir::MethodId>(&direct->target)
                      : nullptr;
              if (method == nullptr) {
                return diag::Fail(
                    diag::DiagCode::kUnsupportedExpressionForm,
                    "mir_to_lir: a coroutine value from a non-method callee is "
                    "not yet lowerable to LIR");
              }
              std::vector<lir::Operand> ctor_args;
              ctor_args.reserve(args.size() + 1);
              ctor_args.emplace_back(
                  lir::FuncRef{
                      .method = lir::MethodRef{
                          .class_id = current_class_, .index = method->value}});
              for (lir::Operand& arg : args) {
                ctor_args.emplace_back(std::move(arg));
              }
              return Emit(
                  result_type,
                  lir::CallInstr{
                      .target = lir::ConstructTarget{.result = result_type},
                      .args = std::move(ctor_args)});
            }

            auto target =
                LowerCallTarget(call.callee, current_class_, result_type);
            if (!target) {
              return std::unexpected(std::move(target.error()));
            }
            return Emit(
                result_type,
                lir::CallInstr{
                    .target = *std::move(target), .args = std::move(args)});
          },
          [&](const mir::ArrayLiteralExpr& lit) -> diag::Result<lir::Operand> {
            std::vector<lir::Operand> elements;
            elements.reserve(lit.elements.size());
            for (const mir::ExprId elem : lit.elements) {
              auto lowered = LowerExpr(elem);
              if (!lowered) {
                return std::unexpected(std::move(lowered.error()));
              }
              elements.push_back(*std::move(lowered));
            }
            return Emit(
                unit_->TranslateType(type),
                lir::AggregateInstr{.elements = std::move(elements)});
          },
          [&](const mir::PointerCastExpr& c) -> diag::Result<lir::Operand> {
            // A borrowed-pointer reinterpretation is identity in the
            // opaque-handle ABI: every pointer crosses as the same handle, so
            // the cast lowers to its operand unchanged.
            return LowerExpr(c.operand);
          },
          [](const auto&) -> diag::Result<lir::Operand> {
            return diag::Fail(
                diag::DiagCode::kUnsupportedExpressionForm,
                "mir_to_lir: MIR expression form is not yet lowerable to LIR");
          }},
      expr.data);
}

auto FunctionLowerer::Emit(lir::TypeId type, lir::InstrData data)
    -> lir::Operand {
  const lir::ValueId result = fn_.values.Add(
      lir::Local{.name = {}, .type = type, .kind = lir::LocalKind::kTemp});
  block_.instrs.push_back(
      lir::Instr{.result = result, .data = std::move(data)});
  return lir::Use{.value = result};
}

}  // namespace lyra::lowering::mir_to_lir
