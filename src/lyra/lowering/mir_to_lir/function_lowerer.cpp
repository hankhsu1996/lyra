#include "lyra/lowering/mir_to_lir/function_lowerer.hpp"

#include <cstddef>
#include <cstdint>
#include <optional>
#include <ranges>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/integral_constant.hpp"
#include "lyra/lir/operator.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/lir/type_query.hpp"
#include "lyra/lowering/mir_to_lir/unit_lowerer.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/inc_dec_op.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/unary_op.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::mir_to_lir {

namespace {

auto Unsupported(std::string message) -> std::unexpected<diag::Diagnostic> {
  return diag::Fail(
      diag::DiagCode::kUnsupportedExpressionForm, std::move(message));
}

// The place a place local names: its own storage, with nothing projected off
// it.
auto LocalPlace(lir::ValueId local) -> lir::Place {
  return lir::Place{.base = lir::Use{.value = local}, .chain = {}};
}

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

// The operators the executable IR realizes directly. Every other MIR operator
// is lifted to a library call before this point, so reaching one here is a
// lowering defect upstream, not an unsupported source form.
auto TranslateBinaryOp(mir::BinaryOp op) -> std::optional<lir::BinaryOp> {
  switch (op) {
    case mir::BinaryOp::kAdd:
      return lir::BinaryOp::kAdd;
    case mir::BinaryOp::kSub:
      return lir::BinaryOp::kSub;
    case mir::BinaryOp::kMul:
      return lir::BinaryOp::kMul;
    case mir::BinaryOp::kDiv:
      return lir::BinaryOp::kDiv;
    case mir::BinaryOp::kMod:
      return lir::BinaryOp::kMod;
    case mir::BinaryOp::kBitwiseAnd:
      return lir::BinaryOp::kBitwiseAnd;
    case mir::BinaryOp::kBitwiseOr:
      return lir::BinaryOp::kBitwiseOr;
    case mir::BinaryOp::kBitwiseXor:
      return lir::BinaryOp::kBitwiseXor;
    case mir::BinaryOp::kEquality:
      return lir::BinaryOp::kEquality;
    case mir::BinaryOp::kInequality:
      return lir::BinaryOp::kInequality;
    case mir::BinaryOp::kLessThan:
      return lir::BinaryOp::kLessThan;
    case mir::BinaryOp::kLessEqual:
      return lir::BinaryOp::kLessEqual;
    case mir::BinaryOp::kGreaterThan:
      return lir::BinaryOp::kGreaterThan;
    case mir::BinaryOp::kGreaterEqual:
      return lir::BinaryOp::kGreaterEqual;
    case mir::BinaryOp::kLogicalAnd:
      return lir::BinaryOp::kLogicalAnd;
    case mir::BinaryOp::kLogicalOr:
      return lir::BinaryOp::kLogicalOr;
    default:
      return std::nullopt;
  }
}

auto TranslateUnaryOp(mir::UnaryOp op) -> std::optional<lir::UnaryOp> {
  switch (op) {
    case mir::UnaryOp::kMinus:
      return lir::UnaryOp::kMinus;
    case mir::UnaryOp::kBitwiseNot:
      return lir::UnaryOp::kBitwiseNot;
    case mir::UnaryOp::kLogicalNot:
      return lir::UnaryOp::kLogicalNot;
    default:
      return std::nullopt;
  }
}

// The local a place-position expression names, if it names one directly.
auto PlacedLocal(const mir::Block& block, mir::ExprId id)
    -> std::optional<mir::LocalId> {
  const auto* ref = std::get_if<mir::LocalRef>(&block.exprs.Get(id).data);
  return ref != nullptr ? std::optional{ref->var} : std::nullopt;
}

// Marks every local the canonical lowering needs an address for: one that is
// assigned after its initialization, or has its address taken. Such a local is
// storage, so it must be a place local. A read never makes a local storage: a
// value read many times is still a value. The whole expression arena is
// scanned, not just the reachable statements, so a local written only by an
// unreachable expression is conservatively storage.
void CollectPlacedLocals(const mir::Block& block, std::vector<bool>& placed) {
  const auto mark = [&](std::optional<mir::LocalId> local) {
    if (local.has_value()) {
      placed[local->value] = true;
    }
  };
  for (std::size_t i = 0; i < block.exprs.size(); ++i) {
    const mir::Expr& expr =
        block.exprs.Get(mir::ExprId{static_cast<std::uint32_t>(i)});
    std::visit(
        Overloaded{
            [&](const mir::AssignExpr& e) {
              mark(PlacedLocal(block, e.target));
            },
            [&](const mir::IncDecExpr& e) {
              mark(PlacedLocal(block, e.target));
            },
            [&](const mir::AddressOfExpr& e) {
              mark(PlacedLocal(block, e.operand));
            },
            [](const auto&) {}},
        expr.data);
  }
  for (std::size_t i = 0; i < block.child_scopes.size(); ++i) {
    CollectPlacedLocals(
        block.child_scopes.Get(mir::BlockId{static_cast<std::uint32_t>(i)}),
        placed);
  }
}

auto LowerCallTarget(
    UnitLowerer& unit, const mir::Callee& callee, lir::ClassId current_class,
    lir::TypeId result) -> diag::Result<lir::CallTarget> {
  return std::visit(
      Overloaded{
          [&](const mir::Direct& d) -> diag::Result<lir::CallTarget> {
            std::optional<lir::TypeId> qualifier;
            if (d.qualification.has_value()) {
              qualifier = unit.TranslateType(
                  std::get<mir::TypeQualifier>(*d.qualification).type);
            }
            return std::visit(
                Overloaded{
                    [&](const mir::MethodId& m)
                        -> diag::Result<lir::CallTarget> {
                      if (qualifier.has_value()) {
                        return Unsupported(
                            "mir_to_lir: a qualified method call is not yet "
                            "lowerable to LIR");
                      }
                      return lir::CallTarget{lir::MethodTarget{
                          .method = lir::MethodRef{
                              .class_id = current_class, .index = m.value}}};
                    },
                    [&](const support::BuiltinFn& fn)
                        -> diag::Result<lir::CallTarget> {
                      return lir::CallTarget{
                          lir::BuiltinTarget{.fn = fn, .qualifier = qualifier}};
                    },
                    [&](const mir::StaticCallableTarget& s)
                        -> diag::Result<lir::CallTarget> {
                      // The target names its own owner, so the symbol is
                      // reached without a receiver to recover it from.
                      const mir::StaticCallableDecl& decl =
                          unit.Mir().GetClass(s.owner).static_callables.Get(
                              s.slot);
                      return lir::CallTarget{lir::ForeignTarget{
                          .symbol = decl.external.foreign_name}};
                    }},
                d.target);
          },
          [&](const mir::Construct&) -> diag::Result<lir::CallTarget> {
            return lir::CallTarget{lir::ConstructTarget{.result = result}};
          },
          [](const mir::Indirect&) -> diag::Result<lir::CallTarget> {
            return Unsupported(
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
      placed_(code.locals.size(), false),
      locals_(code.locals.size(), std::nullopt) {
}

auto FunctionLowerer::Run() -> diag::Result<lir::Function> {
  fn_.name = std::move(name_);
  // A coroutine-bodied callable keeps its coroutine result type: coroutine-ness
  // is the call protocol carried by the type, so a backend realizes suspension
  // and completion from the type, never from a separate flag.
  fn_.result_type = unit_->TranslateType(code_->result_type);
  const bool is_coroutine = unit_->Mir().types.IsCoroutine(code_->result_type);

  CollectPlacedLocals(code_->body, placed_);

  // A parameter is a declared local whose initial value is the incoming
  // argument. It arrives as a value in the signature and is bound like any
  // local: a place if the body assigns or addresses it, otherwise the argument
  // value itself. The entry block exists first so a spilled parameter's copy
  // into its place lands there, ahead of the body.
  SetCurrent(NewBlock());
  for (const mir::LocalId param : code_->params) {
    const mir::LocalDecl& decl = code_->locals.Get(param);
    const lir::TypeId type = unit_->TranslateType(decl.type);
    const lir::ValueId value = fn_.values.Add(
        lir::Local{
            .name = decl.name, .type = type, .kind = lir::LocalKind::kParam});
    fn_.params.push_back(value);
    BindLocal(param, type, lir::Use{.value = value});
  }

  auto lowered = LowerBlockInto(code_->body);
  if (!lowered) {
    return std::unexpected(std::move(lowered.error()));
  }

  // A block the lowering left open either falls off the end of a void or
  // coroutine body -- an implicit return -- or is a join control never reaches.
  // A value-returning body reaches its returns explicitly, so its open blocks
  // are the latter.
  const bool falls_through_to_return =
      is_coroutine ||
      fn_.result_type == unit_->TranslateType(unit_->Mir().builtins.void_type);
  for (std::size_t i = 0; i < fn_.blocks.size(); ++i) {
    if (terminated_[i]) {
      continue;
    }
    fn_.blocks[i].terminator = lir::Terminator{
        .data =
            falls_through_to_return
                ? lir::TerminatorData{lir::ReturnTerm{.value = std::nullopt}}
                : lir::TerminatorData{lir::UnreachableTerm{}}};
  }
  return std::move(fn_);
}

auto FunctionLowerer::NewBlock() -> lir::BlockId {
  fn_.blocks.emplace_back();
  terminated_.push_back(false);
  return lir::BlockId{static_cast<std::uint32_t>(fn_.blocks.size() - 1)};
}

void FunctionLowerer::SetCurrent(lir::BlockId id) {
  current_ = id;
}

void FunctionLowerer::Terminate(lir::TerminatorData data) {
  if (terminated_[current_.value]) {
    throw InternalError("mir_to_lir: block terminated twice");
  }
  fn_.blocks[current_.value].terminator = lir::Terminator{.data = data};
  terminated_[current_.value] = true;
}

auto FunctionLowerer::Terminated() const -> bool {
  return terminated_[current_.value];
}

auto FunctionLowerer::Emit(lir::TypeId type, lir::InstrData data)
    -> lir::Operand {
  const lir::ValueId result = fn_.values.Add(
      lir::Local{.name = {}, .type = type, .kind = lir::LocalKind::kTemp});
  fn_.blocks[current_.value].instrs.push_back(
      lir::Instr{.result = result, .data = std::move(data)});
  return lir::Use{.value = result};
}

auto FunctionLowerer::NewPlaceLocal(lir::TypeId type) -> lir::ValueId {
  return fn_.values.Add(
      lir::Local{.name = {}, .type = type, .kind = lir::LocalKind::kPlace});
}

// Introduces a declared local, holding its initial value. A local the lowering
// needs an address for -- assigned later, or addressed -- becomes frame storage
// the initial value is written into; one that is only ever read stays the
// initial value itself.
void FunctionLowerer::BindLocal(
    mir::LocalId local, lir::TypeId type, lir::Operand init) {
  if (!placed_[local.value]) {
    locals_[local.value] = LocalBinding{ValueBinding{.value = std::move(init)}};
    return;
  }
  const lir::ValueId slot = NewPlaceLocal(type);
  locals_[local.value] = LocalBinding{PlaceBinding{.slot = slot}};
  Store(LocalPlace(slot), std::move(init));
}

auto FunctionLowerer::Load(lir::Place place, lir::TypeId type) -> lir::Operand {
  return Emit(type, lir::LoadInstr{.place = std::move(place)});
}

void FunctionLowerer::Store(lir::Place place, lir::Operand value) {
  Emit(
      unit_->TranslateType(unit_->Mir().builtins.void_type),
      lir::StoreInstr{.place = std::move(place), .value = std::move(value)});
}

auto FunctionLowerer::LowerBlockInto(const mir::Block& block)
    -> diag::Result<void> {
  for (const mir::StmtId sid : block.root_stmts) {
    if (Terminated()) {
      // Control left this sequence -- a return, a break, a continue. What
      // follows in the same brace level is unreachable and has no lowering.
      break;
    }
    auto lowered = LowerStmtInto(block, block.stmts.Get(sid));
    if (!lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
  }
  return {};
}

auto FunctionLowerer::LowerStmtInto(
    const mir::Block& block, const mir::Stmt& stmt) -> diag::Result<void> {
  return std::visit(
      Overloaded{
          [](const mir::EmptyStmt&) -> diag::Result<void> { return {}; },
          [&](const mir::ExprStmt& s) -> diag::Result<void> {
            auto lowered = LowerExpr(block, s.expr);
            if (!lowered) {
              return std::unexpected(std::move(lowered.error()));
            }
            return {};
          },
          [&](const mir::BlockStmt& s) -> diag::Result<void> {
            return LowerBlockInto(block.child_scopes.Get(s.scope));
          },
          [&](const mir::LocalDeclStmt& s) -> diag::Result<void> {
            auto init = LowerExpr(block, s.init);
            if (!init) {
              return std::unexpected(std::move(init.error()));
            }
            BindLocal(
                s.target,
                unit_->TranslateType(code_->locals.Get(s.target).type),
                *std::move(init));
            return {};
          },
          [&](const mir::IfStmt& s) -> diag::Result<void> {
            return LowerIfInto(block, s);
          },
          [&](const mir::ForStmt& s) -> diag::Result<void> {
            return LowerForInto(block, s);
          },
          [&](const mir::WhileStmt& s) -> diag::Result<void> {
            return LowerWhileInto(block, s);
          },
          [&](const mir::DoWhileStmt& s) -> diag::Result<void> {
            return LowerDoWhileInto(block, s);
          },
          [&](const mir::BreakStmt& s) -> diag::Result<void> {
            return LowerBreakInto(s);
          },
          [&](const mir::ContinueStmt&) -> diag::Result<void> {
            return LowerContinueInto();
          },
          [&](const mir::ReturnStmt& s) -> diag::Result<void> {
            std::optional<lir::Operand> value;
            if (s.value.has_value()) {
              auto lowered = LowerExpr(block, *s.value);
              if (!lowered) {
                return std::unexpected(std::move(lowered.error()));
              }
              value = *std::move(lowered);
            }
            Terminate(lir::ReturnTerm{.value = std::move(value)});
            return {};
          },
          [](const mir::SensitivityWaitStmt&) -> diag::Result<void> {
            return diag::Fail(
                diag::DiagCode::kUnsupportedStatementForm,
                "mir_to_lir: a sensitivity wait is not yet lowerable to LIR");
          }},
      stmt.data);
}

auto FunctionLowerer::LowerIfInto(
    const mir::Block& block, const mir::IfStmt& stmt) -> diag::Result<void> {
  auto condition = LowerCondition(block, stmt.condition);
  if (!condition) {
    return std::unexpected(std::move(condition.error()));
  }
  const lir::BlockId then_id = NewBlock();
  const lir::BlockId else_id = NewBlock();
  const lir::BlockId merge_id = NewBlock();
  Terminate(
      lir::CondBranchTerm{
          .condition = *std::move(condition),
          .if_true = then_id,
          .if_false = else_id});

  SetCurrent(then_id);
  auto then_lowered = LowerBlockInto(block.child_scopes.Get(stmt.then_scope));
  if (!then_lowered) {
    return std::unexpected(std::move(then_lowered.error()));
  }
  if (!Terminated()) {
    Terminate(lir::BranchTerm{.target = merge_id});
  }

  SetCurrent(else_id);
  if (stmt.else_scope.has_value()) {
    auto else_lowered =
        LowerBlockInto(block.child_scopes.Get(*stmt.else_scope));
    if (!else_lowered) {
      return std::unexpected(std::move(else_lowered.error()));
    }
  }
  if (!Terminated()) {
    Terminate(lir::BranchTerm{.target = merge_id});
  }

  SetCurrent(merge_id);
  return {};
}

auto FunctionLowerer::LowerWhileInto(
    const mir::Block& block, const mir::WhileStmt& stmt) -> diag::Result<void> {
  const lir::BlockId header_id = NewBlock();
  const lir::BlockId body_id = NewBlock();
  const lir::BlockId exit_id = NewBlock();
  Terminate(lir::BranchTerm{.target = header_id});

  SetCurrent(header_id);
  auto condition = LowerCondition(block, stmt.condition);
  if (!condition) {
    return std::unexpected(std::move(condition.error()));
  }
  Terminate(
      lir::CondBranchTerm{
          .condition = *std::move(condition),
          .if_true = body_id,
          .if_false = exit_id});

  SetCurrent(body_id);
  loops_.push_back(
      LoopTargets{
          .label = std::nullopt,
          .continue_target = header_id,
          .break_target = exit_id});
  auto body = LowerBlockInto(block.child_scopes.Get(stmt.scope));
  loops_.pop_back();
  if (!body) {
    return std::unexpected(std::move(body.error()));
  }
  if (!Terminated()) {
    Terminate(lir::BranchTerm{.target = header_id});
  }

  SetCurrent(exit_id);
  return {};
}

auto FunctionLowerer::LowerDoWhileInto(
    const mir::Block& block, const mir::DoWhileStmt& stmt)
    -> diag::Result<void> {
  const lir::BlockId body_id = NewBlock();
  const lir::BlockId latch_id = NewBlock();
  const lir::BlockId exit_id = NewBlock();
  Terminate(lir::BranchTerm{.target = body_id});

  SetCurrent(body_id);
  loops_.push_back(
      LoopTargets{
          .label = std::nullopt,
          .continue_target = latch_id,
          .break_target = exit_id});
  auto body = LowerBlockInto(block.child_scopes.Get(stmt.scope));
  loops_.pop_back();
  if (!body) {
    return std::unexpected(std::move(body.error()));
  }
  if (!Terminated()) {
    Terminate(lir::BranchTerm{.target = latch_id});
  }

  SetCurrent(latch_id);
  auto condition = LowerCondition(block, stmt.condition);
  if (!condition) {
    return std::unexpected(std::move(condition.error()));
  }
  Terminate(
      lir::CondBranchTerm{
          .condition = *std::move(condition),
          .if_true = body_id,
          .if_false = exit_id});

  SetCurrent(exit_id);
  return {};
}

auto FunctionLowerer::LowerForInto(
    const mir::Block& block, const mir::ForStmt& stmt) -> diag::Result<void> {
  for (const mir::ForInit& init : stmt.init) {
    auto lowered = std::visit(
        Overloaded{
            [&](const mir::ForInitDecl& decl) -> diag::Result<void> {
              auto value = LowerExpr(block, decl.init);
              if (!value) {
                return std::unexpected(std::move(value.error()));
              }
              BindLocal(
                  decl.induction_var,
                  unit_->TranslateType(
                      code_->locals.Get(decl.induction_var).type),
                  *std::move(value));
              return {};
            },
            [&](const mir::ForInitExpr& expr) -> diag::Result<void> {
              auto value = LowerExpr(block, expr.expr);
              if (!value) {
                return std::unexpected(std::move(value.error()));
              }
              return {};
            }},
        init);
    if (!lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
  }

  const lir::BlockId header_id = NewBlock();
  const lir::BlockId body_id = NewBlock();
  const lir::BlockId step_id = NewBlock();
  const lir::BlockId exit_id = NewBlock();
  Terminate(lir::BranchTerm{.target = header_id});

  SetCurrent(header_id);
  if (stmt.condition.has_value()) {
    auto condition = LowerCondition(block, *stmt.condition);
    if (!condition) {
      return std::unexpected(std::move(condition.error()));
    }
    Terminate(
        lir::CondBranchTerm{
            .condition = *std::move(condition),
            .if_true = body_id,
            .if_false = exit_id});
  } else {
    Terminate(lir::BranchTerm{.target = body_id});
  }

  SetCurrent(body_id);
  loops_.push_back(
      LoopTargets{
          .label = stmt.break_label,
          .continue_target = step_id,
          .break_target = exit_id});
  auto body = LowerBlockInto(block.child_scopes.Get(stmt.scope));
  loops_.pop_back();
  if (!body) {
    return std::unexpected(std::move(body.error()));
  }
  if (!Terminated()) {
    Terminate(lir::BranchTerm{.target = step_id});
  }

  SetCurrent(step_id);
  for (const mir::ExprId step : stmt.step) {
    auto lowered = LowerExpr(block, step);
    if (!lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
  }
  Terminate(lir::BranchTerm{.target = header_id});

  SetCurrent(exit_id);
  return {};
}

auto FunctionLowerer::LowerBreakInto(const mir::BreakStmt& stmt)
    -> diag::Result<void> {
  // An unlabeled break leaves the innermost loop; a labeled one leaves the
  // loop that carries the label, however many loops it is nested inside.
  for (const LoopTargets& loop : std::views::reverse(loops_)) {
    if (!stmt.target.has_value() || loop.label == stmt.target) {
      Terminate(lir::BranchTerm{.target = loop.break_target});
      return {};
    }
  }
  throw InternalError("mir_to_lir: break outside of any matching loop");
}

auto FunctionLowerer::LowerContinueInto() -> diag::Result<void> {
  if (loops_.empty()) {
    throw InternalError("mir_to_lir: continue outside of any loop");
  }
  Terminate(lir::BranchTerm{.target = loops_.back().continue_target});
  return {};
}

auto FunctionLowerer::LowerCondition(const mir::Block& block, mir::ExprId id)
    -> diag::Result<lir::Operand> {
  // A condition arrives already reduced to a predicate at HIR-to-MIR, so it
  // lowers to the machine boolean a branch tests directly. The reduction is
  // stated upstream and lowered like any other value here, never re-derived
  // from the operand's type; a condition that did not arrive reduced is an
  // upstream defect.
  auto value = LowerExpr(block, id);
  if (!value) {
    return value;
  }
  if (lir::OperandType(fn_, *value) != unit_->MachineBoolType()) {
    throw InternalError(
        "mir_to_lir: a condition did not arrive as a reduced predicate");
  }
  return value;
}

auto FunctionLowerer::LowerPlace(const mir::Block& block, mir::ExprId id)
    -> diag::Result<lir::Place> {
  const mir::Expr& expr = block.exprs.Get(id);
  return std::visit(
      Overloaded{
          [&](const mir::LocalRef& ref) -> diag::Result<lir::Place> {
            const std::optional<LocalBinding>& binding = locals_[ref.var.value];
            const auto* place = binding.has_value()
                                    ? std::get_if<PlaceBinding>(&*binding)
                                    : nullptr;
            if (place == nullptr) {
              return Unsupported(
                  "mir_to_lir: local is not addressable storage");
            }
            return LocalPlace(place->slot);
          },
          [&](const mir::FieldAccessExpr& field) -> diag::Result<lir::Place> {
            auto receiver = LowerExpr(block, field.receiver);
            if (!receiver) {
              return std::unexpected(std::move(receiver.error()));
            }
            return lir::Place{
                .base = *std::move(receiver),
                .chain = {
                    lir::Projection{lir::DerefProjection{}},
                    lir::Projection{lir::MemberProjection{
                        .member = lir::MemberId{field.field.value}}}}};
          },
          [&](const mir::DerefExpr& deref) -> diag::Result<lir::Place> {
            auto pointer = LowerExpr(block, deref.pointer);
            if (!pointer) {
              return std::unexpected(std::move(pointer.error()));
            }
            return lir::Place{
                .base = *std::move(pointer),
                .chain = {lir::Projection{lir::DerefProjection{}}}};
          },
          [](const auto&) -> diag::Result<lir::Place> {
            return Unsupported("mir_to_lir: expression form names no place");
          }},
      expr.data);
}

auto FunctionLowerer::LowerArgument(const mir::Block& block, mir::ExprId id)
    -> diag::Result<lir::Operand> {
  // The parameter's type decides. A parameter of an address-only type asks for
  // the storage itself -- a cell, a scope -- so the argument is where it lives,
  // not a reading of it. This is a fact about what the callee takes, not about
  // the expression that produced the place.
  const lir::TypeId type = unit_->TranslateType(block.exprs.Get(id).type);
  if (!lir::IsAddressOnly(unit_->Types(), type)) {
    return LowerExpr(block, id);
  }
  auto place = LowerPlace(block, id);
  if (!place) {
    return std::unexpected(std::move(place.error()));
  }
  return Emit(
      unit_->BorrowedPointerTo(type),
      lir::AddrOfInstr{.place = *std::move(place)});
}

auto FunctionLowerer::LowerCall(
    const mir::Block& block, const mir::CallExpr& call, mir::TypeId type)
    -> diag::Result<lir::Operand> {
  std::vector<lir::Operand> args;
  args.reserve(call.arguments.size());
  for (const mir::ExprId arg : call.arguments) {
    auto lowered = LowerArgument(block, arg);
    if (!lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
    args.push_back(*std::move(lowered));
  }
  const lir::TypeId result_type = unit_->TranslateType(type);

  // A coroutine is a runtime value like any other: the runtime builds it from
  // an entry code reference and its environment (the receiver), and it is
  // reached as an opaque handle. It is constructed through the same Construct
  // path as any other runtime value; the coroutine call protocol stays the
  // result type.
  if (std::holds_alternative<mir::CoroutineType>(
          unit_->Mir().types.Get(type).data)) {
    const auto* direct = std::get_if<mir::Direct>(&call.callee);
    const auto* method = direct != nullptr
                             ? std::get_if<mir::MethodId>(&direct->target)
                             : nullptr;
    if (method == nullptr) {
      return Unsupported(
          "mir_to_lir: a coroutine value from a non-method callee is not yet "
          "lowerable to LIR");
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
        result_type, lir::CallInstr{
                         .target = lir::ConstructTarget{.result = result_type},
                         .args = std::move(ctor_args)});
  }

  auto target =
      LowerCallTarget(*unit_, call.callee, current_class_, result_type);
  if (!target) {
    return std::unexpected(std::move(target.error()));
  }
  return Emit(
      result_type,
      lir::CallInstr{.target = *std::move(target), .args = std::move(args)});
}

auto FunctionLowerer::LowerAssign(
    const mir::Block& block, const mir::AssignExpr& assign)
    -> diag::Result<lir::Operand> {
  const lir::TypeId type =
      unit_->TranslateType(block.exprs.Get(assign.target).type);
  auto place = LowerPlace(block, assign.target);
  if (!place) {
    return std::unexpected(std::move(place.error()));
  }
  auto value = LowerExpr(block, assign.value);
  if (!value) {
    return std::unexpected(std::move(value.error()));
  }

  lir::Operand written = *value;
  if (assign.compound_op.has_value()) {
    const std::optional<lir::BinaryOp> op =
        TranslateBinaryOp(*assign.compound_op);
    if (!op) {
      return Unsupported(
          "mir_to_lir: compound assignment operator has no direct realization");
    }
    lir::Operand old = Load(*place, type);
    written = Emit(
        type, lir::BinaryInstr{
                  .op = *op, .lhs = std::move(old), .rhs = *std::move(value)});
  }
  Store(*std::move(place), written);
  return written;
}

auto FunctionLowerer::LowerIncDec(
    const mir::Block& block, const mir::IncDecExpr& inc_dec)
    -> diag::Result<lir::Operand> {
  const lir::TypeId type =
      unit_->TranslateType(block.exprs.Get(inc_dec.target).type);
  auto place = LowerPlace(block, inc_dec.target);
  if (!place) {
    return std::unexpected(std::move(place.error()));
  }
  const bool is_increment = inc_dec.op == mir::IncDecOp::kPreInc ||
                            inc_dec.op == mir::IncDecOp::kPostInc;
  const bool is_prefix = inc_dec.op == mir::IncDecOp::kPreInc ||
                         inc_dec.op == mir::IncDecOp::kPreDec;

  lir::Operand old = Load(*place, type);
  lir::Operand updated = Emit(
      type, lir::UnaryInstr{
                .op = is_increment ? lir::UnaryOp::kIncrement
                                   : lir::UnaryOp::kDecrement,
                .operand = old});
  Store(*std::move(place), updated);
  return is_prefix ? updated : old;
}

auto FunctionLowerer::LowerConditional(
    const mir::Block& block, const mir::ConditionalExpr& cond, mir::TypeId type)
    -> diag::Result<lir::Operand> {
  auto condition = LowerCondition(block, cond.condition);
  if (!condition) {
    return std::unexpected(std::move(condition.error()));
  }
  // The arms are evaluated only on the path that selects them, so the result is
  // written through on two paths: it is storage, not a transient.
  const lir::TypeId result_type = unit_->TranslateType(type);
  const lir::ValueId slot = NewPlaceLocal(result_type);
  const lir::BlockId then_id = NewBlock();
  const lir::BlockId else_id = NewBlock();
  const lir::BlockId merge_id = NewBlock();
  Terminate(
      lir::CondBranchTerm{
          .condition = *std::move(condition),
          .if_true = then_id,
          .if_false = else_id});

  SetCurrent(then_id);
  auto then_value = LowerExpr(block, cond.then_value);
  if (!then_value) {
    return std::unexpected(std::move(then_value.error()));
  }
  Store(LocalPlace(slot), *std::move(then_value));
  Terminate(lir::BranchTerm{.target = merge_id});

  SetCurrent(else_id);
  auto else_value = LowerExpr(block, cond.else_value);
  if (!else_value) {
    return std::unexpected(std::move(else_value.error()));
  }
  Store(LocalPlace(slot), *std::move(else_value));
  Terminate(lir::BranchTerm{.target = merge_id});

  SetCurrent(merge_id);
  return Load(LocalPlace(slot), result_type);
}

auto FunctionLowerer::LowerExpr(const mir::Block& block, mir::ExprId id)
    -> diag::Result<lir::Operand> {
  const mir::Expr& expr = block.exprs.Get(id);
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
            const std::optional<LocalBinding>& binding = locals_[ref.var.value];
            if (!binding.has_value()) {
              return Unsupported("mir_to_lir: reference to an unlowered local");
            }
            return std::visit(
                Overloaded{
                    [&](const PlaceBinding& place) -> lir::Operand {
                      return Load(
                          LocalPlace(place.slot),
                          fn_.values.Get(place.slot).type);
                    },
                    [](const ValueBinding& value) -> lir::Operand {
                      return value.value;
                    }},
                *binding);
          },
          [&](const mir::CallExpr& call) -> diag::Result<lir::Operand> {
            return LowerCall(block, call, type);
          },
          [&](const mir::ArrayLiteralExpr& lit) -> diag::Result<lir::Operand> {
            std::vector<lir::Operand> elements;
            elements.reserve(lit.elements.size());
            for (const mir::ExprId elem : lit.elements) {
              auto lowered = LowerExpr(block, elem);
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
            auto operand = LowerExpr(block, c.operand);
            if (!operand) {
              return operand;
            }
            return Emit(
                unit_->TranslateType(type),
                lir::PointerCastInstr{.operand = *std::move(operand)});
          },
          [&](const mir::IntCastExpr& c) -> diag::Result<lir::Operand> {
            auto operand = LowerExpr(block, c.operand);
            if (!operand) {
              return operand;
            }
            return Emit(
                unit_->TranslateType(type),
                lir::IntCastInstr{.operand = *std::move(operand)});
          },
          [&](const mir::FieldAccessExpr&) -> diag::Result<lir::Operand> {
            const lir::TypeId field_type = unit_->TranslateType(type);
            if (lir::IsAddressOnly(unit_->Types(), field_type)) {
              return Unsupported(
                  "mir_to_lir: a storage cell has no value to read; it is "
                  "reached through its address");
            }
            auto place = LowerPlace(block, id);
            if (!place) {
              return std::unexpected(std::move(place.error()));
            }
            return Load(*std::move(place), field_type);
          },
          [&](const mir::DerefExpr&) -> diag::Result<lir::Operand> {
            auto place = LowerPlace(block, id);
            if (!place) {
              return std::unexpected(std::move(place.error()));
            }
            return Load(*std::move(place), unit_->TranslateType(type));
          },
          [&](const mir::AddressOfExpr& addr) -> diag::Result<lir::Operand> {
            auto place = LowerPlace(block, addr.operand);
            if (!place) {
              return std::unexpected(std::move(place.error()));
            }
            return Emit(
                unit_->TranslateType(type),
                lir::AddrOfInstr{.place = *std::move(place)});
          },
          [&](const mir::AssignExpr& assign) -> diag::Result<lir::Operand> {
            return LowerAssign(block, assign);
          },
          [&](const mir::IncDecExpr& inc_dec) -> diag::Result<lir::Operand> {
            return LowerIncDec(block, inc_dec);
          },
          [&](const mir::UnaryExpr& un) -> diag::Result<lir::Operand> {
            auto operand = LowerExpr(block, un.operand);
            if (!operand) {
              return operand;
            }
            // LRM 11.4.3: unary plus is an identity.
            if (un.op == mir::UnaryOp::kPlus) {
              return operand;
            }
            const std::optional<lir::UnaryOp> op = TranslateUnaryOp(un.op);
            if (!op) {
              return Unsupported(
                  "mir_to_lir: unary operator has no direct realization");
            }
            return Emit(
                unit_->TranslateType(type),
                lir::UnaryInstr{.op = *op, .operand = *std::move(operand)});
          },
          [&](const mir::BinaryExpr& bin) -> diag::Result<lir::Operand> {
            const std::optional<lir::BinaryOp> op = TranslateBinaryOp(bin.op);
            if (!op) {
              return Unsupported(
                  "mir_to_lir: binary operator has no direct realization");
            }
            auto lhs = LowerExpr(block, bin.lhs);
            if (!lhs) {
              return lhs;
            }
            auto rhs = LowerExpr(block, bin.rhs);
            if (!rhs) {
              return rhs;
            }
            return Emit(
                unit_->TranslateType(type),
                lir::BinaryInstr{
                    .op = *op, .lhs = *std::move(lhs), .rhs = *std::move(rhs)});
          },
          [&](const mir::BoolCastExpr& cast) -> diag::Result<lir::Operand> {
            auto operand = LowerExpr(block, cast.operand);
            if (!operand) {
              return operand;
            }
            return Emit(
                unit_->MachineBoolType(),
                lir::BoolCastInstr{.operand = *std::move(operand)});
          },
          [&](const mir::ConditionalExpr& cond) -> diag::Result<lir::Operand> {
            return LowerConditional(block, cond, type);
          },
          [&](const mir::MoveExpr& m) -> diag::Result<lir::Operand> {
            // A move is a last-use transfer marker placed at HIR-to-MIR; it
            // changes neither the value nor its type, so it unwraps to its
            // operand here. Whether the transfer is realized as a move or a
            // copy is decided below LIR, not at this layer.
            return LowerExpr(block, m.operand);
          },
          [&](const mir::AwaitExpr& await) -> diag::Result<lir::Operand> {
            // An await is two facts: the awaitable's effect -- registering the
            // wakeup source through an ordinary runtime call -- and the suspend
            // itself, a control edge back to the scheduler that resumes at the
            // next block. The registration runs first, so a delay, an event
            // control, and a level wait differ only in the awaitable's call.
            if (type != unit_->Mir().builtins.void_type) {
              return Unsupported(
                  "mir_to_lir: a value-carrying await is not yet lowerable to "
                  "LIR");
            }
            auto registration = LowerExpr(block, await.awaitable);
            if (!registration) {
              return registration;
            }
            const lir::BlockId resume = NewBlock();
            Terminate(lir::SuspendTerm{.resume = resume});
            SetCurrent(resume);
            return registration;
          },
          [](const auto&) -> diag::Result<lir::Operand> {
            return Unsupported(
                "mir_to_lir: MIR expression form is not yet lowerable to LIR");
          }},
      expr.data);
}

}  // namespace lyra::lowering::mir_to_lir
