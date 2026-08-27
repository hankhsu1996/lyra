#include "lyra/lowering/mir_to_lir/function_lowerer.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
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

// The builtin entry a call names directly, if any.
auto DirectBuiltinFn(const mir::CallExpr& call)
    -> std::optional<support::BuiltinFn> {
  const auto* direct = std::get_if<mir::Direct>(&call.callee);
  if (direct == nullptr) {
    return std::nullopt;
  }
  const auto* fn = std::get_if<support::BuiltinFn>(&direct->target);
  if (fn == nullptr) {
    return std::nullopt;
  }
  return *fn;
}

// The place a place local names: its own storage, with nothing projected off
// it.
auto LocalPlace(lir::ValueId local) -> lir::Place {
  return lir::Place{.base = lir::Use{.value = local}, .chain = {}};
}

auto FieldSlot(const mir::FieldAccessExpr& field) -> std::uint32_t {
  return std::visit(
      Overloaded{
          [](const mir::FieldTarget& t) { return t.slot.value; },
          [](const mir::FieldId& id) { return id.value; },
          [](const mir::ExternalFieldTarget&) -> std::uint32_t {
            throw InternalError(
                "mir_to_lir: cross-unit field access is not supported by the "
                "LIR path (LIR is not the target backend for cross-unit class "
                "references today)");
          }},
      field.field);
}

auto TranslateIntegralConstant(const mir::IntegralConstant& c)
    -> lir::IntegralConstant {
  return lir::IntegralConstant{
      .value_words = c.value_words, .state_words = c.state_words};
}

// Whether this call builds a reference over its one argument. A reference is
// the address of the storage it binds, so the argument's storage is what the
// value names -- which both the storage-topology pass and the lowering itself
// have to know, and must agree on.
auto BindsReference(
    const mir::TypeInterner& types, const mir::CallExpr& call,
    mir::TypeId result_type) -> bool {
  return std::holds_alternative<mir::Construct>(call.callee) &&
         std::holds_alternative<mir::RefType>(types.Get(result_type).data);
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

// A value type whose runtime realization is an opaque handle into transient
// storage the boundary releases at each suspension: a packed value (or the
// enumeration and packed struct/union projections that share its shape), a
// string, a real-family value (real / shortreal / realtime), an unpacked struct
// product value, or a dynamic array. A local of such a type that a suspending
// body reads across a suspension needs activation-stable storage, because its
// handle cannot outlive the stretch that produced it. A pointer, a reference,
// an object handle, or a machine scalar is not one of these -- it is stable
// across a suspension on its own -- so it is unaffected.
auto IsActivationValueType(const mir::Type& type) -> bool {
  const mir::TypeKind kind = type.Kind();
  return type.IsIntegralPacked() || kind == mir::TypeKind::kString ||
         kind == mir::TypeKind::kReal || kind == mir::TypeKind::kShortReal ||
         kind == mir::TypeKind::kRealTime || kind == mir::TypeKind::kTuple ||
         kind == mir::TypeKind::kDynamicArray;
}

// Marks every local the canonical lowering needs an address for: one that is
// assigned after its initialization, or has its address taken. Such a local is
// storage, so it must be a place local. A read never makes a local storage: a
// value read many times is still a value. The whole expression arena is
// scanned, not just the reachable statements, so a local written only by an
// unreachable expression is conservatively storage.
void CollectPlacedLocals(
    const mir::TypeInterner& types, const mir::Block& block,
    std::vector<bool>& placed) {
  const auto mark = [&](std::optional<mir::LocalId> local) {
    if (local.has_value()) {
      placed[local->value] = true;
    }
  };
  for (const mir::ExprId id : block.exprs.Ids()) {
    const mir::Expr& expr = block.exprs.Get(id);
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
            // Building a reference over a local binds that local's storage, so
            // the local needs an address for the same reason an explicit
            // address-of gives it one.
            [&](const mir::CallExpr& e) {
              if (BindsReference(types, e, expr.type)) {
                mark(PlacedLocal(block, e.arguments[0]));
              }
            },
            [&](const mir::MachineArrayDataExpr& e) {
              mark(PlacedLocal(block, e.array));
            },
            [](const auto&) {}},
        expr.data);
  }
  for (const mir::BlockId id : block.child_scopes.Ids()) {
    CollectPlacedLocals(types, block.child_scopes.Get(id), placed);
  }
}

auto LowerCallTarget(
    UnitLowerer& unit, const mir::Callee& callee, lir::TypeId result)
    -> diag::Result<lir::CallTarget> {
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
                    [&](const mir::CallableTarget& t)
                        -> diag::Result<lir::CallTarget> {
                      if (qualifier.has_value()) {
                        return Unsupported(
                            "mir_to_lir: a qualified method call is not yet "
                            "lowerable to LIR");
                      }
                      return lir::CallTarget{lir::FunctionTarget{
                          .function = unit.MethodFunction(t.owner, t.slot)}};
                    },
                    [&](const mir::ForeignSymbolTarget& f)
                        -> diag::Result<lir::CallTarget> {
                      // A name in the DPI-C name space is reached as an
                      // external-linkage symbol the execution session resolves
                      // (LRM 35.4); nothing about the call names a unit or a
                      // class.
                      return lir::CallTarget{
                          lir::ForeignTarget{.symbol = f.linkage_name}};
                    },
                    [&](const support::BuiltinFn& fn)
                        -> diag::Result<lir::CallTarget> {
                      // Every other builtin bottoms out on a value domain's
                      // library entry; a net's drivers act on a resolution node
                      // instead, which no such entry answers.
                      if (fn == support::BuiltinFn::kAttachDriver) {
                        return Unsupported(
                            "mir_to_lir: driving a net is not yet lowerable to "
                            "LIR");
                      }
                      return lir::CallTarget{
                          lir::BuiltinTarget{.fn = fn, .qualifier = qualifier}};
                    },
                    [&](const mir::ExternalUnitCallableTarget& t)
                        -> diag::Result<lir::CallTarget> {
                      // A callable of another unit's namespace is outside this
                      // unit and is reached by its symbol, which carries that
                      // unit because a namespace name is unique only inside it.
                      return lir::CallTarget{lir::ForeignTarget{
                          .symbol = std::format(
                              "{}.{}", t.unit_name, t.callable_name)}};
                    },
                    [&](const mir::ExternalUnitClassMethodTarget&)
                        -> diag::Result<lir::CallTarget> {
                      return Unsupported(
                          "mir_to_lir: a cross-unit class method call is not "
                          "yet lowerable to LIR");
                    },
                    [&](const mir::ImportedRuntimeCallTarget&)
                        -> diag::Result<lir::CallTarget> {
                      return Unsupported(
                          "mir_to_lir: an imported runtime-library method call "
                          "is not yet lowerable to LIR");
                    }},
                d.target);
          },
          [&](const mir::Construct&) -> diag::Result<lir::CallTarget> {
            return lir::CallTarget{lir::ConstructTarget{.result = result}};
          },
          [](const mir::Indirect&) -> diag::Result<lir::CallTarget> {
            throw InternalError(
                "mir_to_lir: a closure call carries its receiver, so it is "
                "resolved before target dispatch");
          },
          [](const mir::Virtual&) -> diag::Result<lir::CallTarget> {
            return Unsupported(
                "mir_to_lir: virtual method dispatch is not yet lowerable to "
                "LIR");
          }},
      callee);
}

}  // namespace

FunctionLowerer::FunctionLowerer(
    UnitLowerer& unit, const mir::CallableCode& code, std::string name)
    : unit_(&unit),
      code_(&code),
      closure_(nullptr),
      name_(std::move(name)),
      placed_(code.locals.size(), false),
      activation_value_local_(code.locals.size(), false),
      locals_(code.locals.size(), std::nullopt) {
}

FunctionLowerer::FunctionLowerer(
    UnitLowerer& unit, const mir::ClosureDecl& closure, std::string name)
    : unit_(&unit),
      code_(&closure.invoke),
      closure_(&closure),
      name_(std::move(name)),
      placed_(closure.invoke.locals.size(), false),
      activation_value_local_(closure.invoke.locals.size(), false),
      locals_(closure.invoke.locals.size(), std::nullopt) {
}

auto FunctionLowerer::CaptureRead(
    const mir::Block& block, const mir::FieldAccessExpr& field)
    -> std::optional<lir::Operand> {
  const std::optional<mir::LocalId> local = PlacedLocal(block, field.receiver);
  if (!local.has_value() || !locals_[local->value].has_value()) {
    return std::nullopt;
  }
  const auto* captures = std::get_if<CaptureBinding>(&*locals_[local->value]);
  if (captures == nullptr) {
    return std::nullopt;
  }
  const std::uint32_t slot = FieldSlot(field);
  if (slot >= captures->captures.size()) {
    throw InternalError("mir_to_lir: closure capture read is out of range");
  }
  return captures->captures[slot];
}

void FunctionLowerer::BindCaptureParams(
    const mir::ClosureDecl& closure, mir::LocalId receiver) {
  std::vector<lir::Operand> captures;
  captures.reserve(closure.fields.size());
  for (const mir::FieldId id : closure.fields.Ids()) {
    const mir::FieldDecl& field = closure.fields.Get(id);
    const lir::ValueId value = fn_.values.Add(
        lir::Local{
            .name = field.name,
            .type = unit_->TranslateType(field.type),
            .kind = lir::LocalKind::kParam});
    fn_.params.push_back(value);
    captures.emplace_back(lir::Use{.value = value});
  }
  locals_[receiver.value] =
      LocalBinding{CaptureBinding{.captures = std::move(captures)}};
}

auto FunctionLowerer::Run() -> diag::Result<lir::Function> {
  fn_.name = std::move(name_);
  // A coroutine-bodied callable keeps its coroutine result type: coroutine-ness
  // is the call protocol carried by the type, so a backend realizes suspension
  // and completion from the type, never from a separate flag.
  fn_.result_type = unit_->TranslateType(code_->result_type);
  const bool is_coroutine = unit_->Mir().types.IsCoroutine(code_->result_type);

  CollectPlacedLocals(unit_->Mir().types, code_->Body(), placed_);

  // In a suspending body every value-typed, non-managed local and parameter is
  // an activation-frame value, not a transient: a value's handle cannot safely
  // live across a suspension, so each such local needs activation-stable
  // storage the generated frame reaches by a handle. A suspension is a
  // statement boundary, so only named locals -- never sub-expression transients
  // -- can cross one, which is why marking locals is sufficient. A
  // non-suspending body keeps selective placement.
  if (is_coroutine) {
    for (const mir::LocalId local : code_->locals.Ids()) {
      if (IsActivationValueType(
              unit_->Mir().types.Get(code_->locals.Get(local).type))) {
        activation_value_local_[local.value] = true;
      }
    }
  }

  // A parameter is a declared local whose initial value is the incoming
  // argument. It arrives as a value in the signature and is bound like any
  // local: a place if the body assigns or addresses it, otherwise the argument
  // value itself. The entry block exists first so a spilled parameter's copy
  // into its place lands there, ahead of the body.
  SetCurrent(NewBlock());
  // A closure invoke's receiver is its environment, which the signature carries
  // as the capture parameters leading its per-invocation ones rather than as
  // one value.
  if (closure_ != nullptr) {
    BindCaptureParams(*closure_, mir::LocalId{0});
  }
  for (const mir::LocalId param : code_->params) {
    const mir::LocalDecl& decl = code_->locals.Get(param);
    const lir::TypeId type = unit_->TranslateType(decl.type);
    const lir::ValueId value = fn_.values.Add(
        lir::Local{
            .name = decl.name, .type = type, .kind = lir::LocalKind::kParam});
    fn_.params.push_back(value);
    // A cell parameter installs the cell's representation from the incoming
    // argument, its first store; every other parameter binds to the argument
    // value as a place or a plain value.
    if (activation_value_local_[param.value]) {
      const lir::Operand handle = AllocateActivationValue(type);
      locals_[param.value] =
          LocalBinding{ActivationValueBinding{.handle = handle}};
      StoreActivationValue(handle, lir::Use{.value = value});
    } else {
      BindLocal(param, type, lir::Use{.value = value});
    }
  }

  // Every other cell local's handle is allocated once here, at frame entry, so
  // it is reused across iterations rather than re-created per declaration; its
  // first store, installing the representation, is the declaration's
  // initializer reached during the body walk.
  for (const mir::LocalId id : code_->locals.Ids()) {
    if (activation_value_local_[id.value] && !locals_[id.value].has_value()) {
      const lir::TypeId type = unit_->TranslateType(code_->locals.Get(id).type);
      locals_[id.value] = LocalBinding{
          ActivationValueBinding{.handle = AllocateActivationValue(type)}};
    }
  }

  auto lowered = LowerBlockInto(code_->Body());
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
  fn_.blocks.reserve(blocks_.size());
  for (OpenBlock& block : blocks_) {
    if (!block.terminator.has_value()) {
      block.terminator = lir::Terminator{
          .data =
              falls_through_to_return
                  ? lir::TerminatorData{lir::ReturnTerm{.value = std::nullopt}}
                  : lir::TerminatorData{lir::UnreachableTerm{}}};
    }
    fn_.blocks.push_back(
        lir::BasicBlock{
            .instrs = std::move(block.instrs),
            .terminator = *std::move(block.terminator)});
  }
  return std::move(fn_);
}

auto FunctionLowerer::NewBlock() -> lir::BlockId {
  const lir::BlockId id{static_cast<std::uint32_t>(blocks_.size())};
  blocks_.emplace_back();
  return id;
}

void FunctionLowerer::SetCurrent(lir::BlockId id) {
  current_ = id;
}

void FunctionLowerer::Terminate(lir::TerminatorData data) {
  std::optional<lir::Terminator>& terminator =
      blocks_[current_.value].terminator;
  if (terminator.has_value()) {
    throw InternalError("mir_to_lir: block terminated twice");
  }
  terminator = lir::Terminator{.data = data};
}

auto FunctionLowerer::Terminated() const -> bool {
  return blocks_[current_.value].terminator.has_value();
}

auto FunctionLowerer::Emit(lir::TypeId type, lir::InstrData data)
    -> lir::Operand {
  const lir::ValueId result = fn_.values.Add(
      lir::Local{.name = {}, .type = type, .kind = lir::LocalKind::kTemp});
  blocks_[current_.value].instrs.push_back(
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
  // A cell local's handle was allocated at frame entry; its declaration's
  // initializer is the first store, which installs the cell's representation.
  if (activation_value_local_[local.value]) {
    StoreActivationValue(
        std::get<ActivationValueBinding>(*locals_[local.value]).handle,
        std::move(init));
    return;
  }
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

auto FunctionLowerer::Store(lir::Place place, lir::Operand value)
    -> lir::Operand {
  return Emit(
      unit_->TranslateType(unit_->Mir().builtins.void_type),
      lir::StoreInstr{.place = std::move(place), .value = std::move(value)});
}

auto FunctionLowerer::AllocateActivationValue(lir::TypeId value_type)
    -> lir::Operand {
  return Emit(
      value_type, lir::CallInstr{
                      .target =
                          lir::ActivationFrameTarget{
                              .op = lir::ActivationFrameTarget::Op::kAllocate},
                      .args = {}});
}

auto FunctionLowerer::LoadActivationValue(
    lir::Operand handle, lir::TypeId value_type) -> lir::Operand {
  return Emit(
      value_type, lir::CallInstr{
                      .target =
                          lir::ActivationFrameTarget{
                              .op = lir::ActivationFrameTarget::Op::kLoad},
                      .args = {std::move(handle)}});
}

auto FunctionLowerer::StoreActivationValue(
    lir::Operand handle, lir::Operand value) -> lir::Operand {
  return Emit(
      unit_->TranslateType(unit_->Mir().builtins.void_type),
      lir::CallInstr{
          .target =
              lir::ActivationFrameTarget{
                  .op = lir::ActivationFrameTarget::Op::kStore},
          .args = {std::move(handle), std::move(value)}});
}

auto FunctionLowerer::ActivationValueHandleForTarget(
    const mir::Block& block, mir::ExprId id) -> std::optional<lir::Operand> {
  const auto* ref = std::get_if<mir::LocalRef>(&block.exprs.Get(id).data);
  if (ref == nullptr || !locals_[ref->var.value].has_value()) {
    return std::nullopt;
  }
  const auto* cell =
      std::get_if<ActivationValueBinding>(&*locals_[ref->var.value]);
  return cell != nullptr ? std::optional{cell->handle} : std::nullopt;
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
          [](const mir::TryStmt&) -> diag::Result<void> {
            // A region that consumes a control effect needs an exceptional
            // edge out of its body, which this layer does not yet build.
            return Unsupported(
                "mir_to_lir: a region consuming a control effect is not yet "
                "lowerable to LIR");
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
            if (CaptureRead(block, field).has_value()) {
              return Unsupported(
                  "mir_to_lir: a closure capture is read-only and names no "
                  "storage");
            }
            auto receiver = LowerExpr(block, field.receiver);
            if (!receiver) {
              return std::unexpected(std::move(receiver.error()));
            }
            return lir::Place{
                .base = *std::move(receiver),
                .chain = {
                    lir::Projection{lir::DerefProjection{}},
                    lir::Projection{lir::MemberProjection{
                        .member = lir::MemberId{FieldSlot(field)}}}}};
          },
          [&](const mir::DerefExpr& deref) -> diag::Result<lir::Place> {
            const mir::TypeId operand_type =
                block.exprs.Get(deref.pointer).type;
            const mir::Type& operand_ty = unit_->Mir().types.Get(operand_type);
            // A driver's contribution folds into a net's resolution, which no
            // value-domain library entry answers.
            if (std::holds_alternative<mir::DriverType>(operand_ty.data)) {
              return Unsupported(
                  "mir_to_lir: driving a net is not yet lowerable to LIR");
            }
            // A wrapper that is itself storage -- an observable cell, a net's
            // resolved value -- is storage the chain has already reached, so
            // naming what it represents extends that chain by one step.
            // Everything else here refers to storage elsewhere: a pointer, a
            // handle, and a reference are values, and a value opens a chain
            // rather than continuing one.
            if (std::holds_alternative<mir::ObservableType>(operand_ty.data) ||
                std::holds_alternative<mir::ResolvedType>(operand_ty.data)) {
              auto wrapper = LowerPlace(block, deref.pointer);
              if (!wrapper) {
                return std::unexpected(std::move(wrapper.error()));
              }
              lir::Place place = *std::move(wrapper);
              place.chain.emplace_back(lir::DerefProjection{});
              return place;
            }
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

auto FunctionLowerer::LowerReferenceBind(
    const mir::Block& block, const mir::CallExpr& call, mir::TypeId type)
    -> diag::Result<lir::Operand> {
  // A reference is built over the one storage it binds, so the operand count is
  // the construction's own; a call that does not match it is a producer that
  // built the wrong shape.
  if (call.arguments.size() != 1) {
    throw InternalError(
        "mir_to_lir: a reference is built over exactly one referent");
  }
  const mir::ExprId referent = call.arguments[0];
  const lir::TypeId referent_type =
      unit_->TranslateType(block.exprs.Get(referent).type);
  // Storage that is only ever reached by its address is already a handle to a
  // runtime object; binding a reference to it would name the wrapper rather
  // than the value the reference is meant to alias.
  if (lir::IsAddressOnly(unit_->Types(), referent_type)) {
    return Unsupported(
        "mir_to_lir: a reference to a runtime cell is not yet lowerable to "
        "LIR");
  }
  auto place = LowerPlace(block, referent);
  if (!place) {
    return std::unexpected(std::move(place.error()));
  }
  return Emit(
      unit_->TranslateType(type), lir::AddrOfInstr{.place = *std::move(place)});
}

// Calling a closure is a direct call: the callee's type names one closure,
// whose invoke it is. Nothing is built to call through -- the captures are
// evaluated and passed ahead of the call's own arguments, in the order the
// construction lists them, and the receiver the body reads them through is
// those parameters.
auto FunctionLowerer::LowerClosureCall(
    const mir::Block& block, const mir::Indirect& callee,
    const std::vector<mir::ExprId>& arguments, mir::TypeId type)
    -> diag::Result<lir::Operand> {
  const auto* construct =
      std::get_if<mir::ClosureExpr>(&block.exprs.Get(callee.closure).data);
  if (construct == nullptr) {
    return Unsupported(
        "mir_to_lir: calling a closure that is not built at the call site is "
        "not yet lowerable to LIR");
  }
  if (unit_->Mir().types.IsCoroutine(type)) {
    return Unsupported(
        "mir_to_lir: a coroutine closure is not yet lowerable to LIR");
  }

  const mir::ClosureDecl& decl = unit_->Mir().GetClosure(construct->closure);
  if (construct->field_inits.size() != decl.fields.size()) {
    throw InternalError(
        "mir_to_lir: closure construction does not initialize every capture");
  }
  std::vector<lir::Operand> args(decl.fields.size());
  for (const mir::FieldInit& init : construct->field_inits) {
    auto value = LowerExpr(block, init.value);
    if (!value) {
      return std::unexpected(std::move(value.error()));
    }
    args[init.target.value] = *std::move(value);
  }
  for (const mir::ExprId arg : arguments) {
    auto lowered = LowerArgument(block, arg);
    if (!lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
    args.push_back(*std::move(lowered));
  }
  return Emit(
      unit_->TranslateType(type),
      lir::CallInstr{
          .target =
              lir::FunctionTarget{
                  .function = unit_->ClosureFunction(construct->closure)},
          .args = std::move(args)});
}

auto FunctionLowerer::LowerCall(
    const mir::Block& block, const mir::CallExpr& call, mir::TypeId type)
    -> diag::Result<lir::Operand> {
  // A receiver-mutating method on a value is not an in-place call: value
  // semantics forbid changing what a copy of the receiver would share, so it is
  // a functional operation whose result is stored back through the owner.
  if (const auto fn = DirectBuiltinFn(call);
      fn.has_value() && support::IsMutatingBuiltinFn(*fn) &&
      !call.arguments.empty()) {
    return LowerMutatingCall(block, call, *fn, type);
  }

  if (const auto* indirect = std::get_if<mir::Indirect>(&call.callee)) {
    return LowerClosureCall(block, *indirect, call.arguments, type);
  }

  // A reference is the address of the storage it binds, and reading or writing
  // through one reaches that storage directly. No runtime value stands between
  // the holder and the referent, so the three operations are the ordinary
  // address-of, load, and store over the referent's place.
  if (BindsReference(unit_->Mir().types, call, type)) {
    return LowerReferenceBind(block, call, type);
  }
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
    const auto* callable =
        direct != nullptr ? std::get_if<mir::CallableTarget>(&direct->target)
                          : nullptr;
    if (callable == nullptr) {
      return Unsupported(
          "mir_to_lir: a coroutine value from a non-method callee is not yet "
          "lowerable to LIR");
    }
    std::vector<lir::Operand> ctor_args;
    ctor_args.reserve(args.size() + 1);
    ctor_args.emplace_back(
        lir::FuncRef{
            .function =
                unit_->MethodFunction(callable->owner, callable->slot)});
    for (lir::Operand& arg : args) {
      ctor_args.emplace_back(std::move(arg));
    }
    return Emit(
        result_type, lir::CallInstr{
                         .target = lir::ConstructTarget{.result = result_type},
                         .args = std::move(ctor_args)});
  }

  auto target = LowerCallTarget(*unit_, call.callee, result_type);
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
  // A target that projects into a value aggregate -- a product component, a
  // value-container element, or any composition of them -- is not a place: the
  // aggregate is an opaque value, so the write folds into a functional
  // whole-value update stored back through the owner.
  if (std::holds_alternative<mir::ValueProjectionExpr>(
          block.exprs.Get(assign.target).data)) {
    return LowerProjectionAssign(block, assign);
  }

  // A whole-value store between two unpacked arrays of one size whose declared
  // ranges differ is position-wise (LRM 7.6), so both sides are the same value
  // below this layer -- the range names coordinates and reaches a select as its
  // own operand, never the payload. This layer still gives each declared range
  // its own type identity, so the two sides arrive as unequal types and the
  // store has no well-typed form here.
  const mir::TypeId target_type = block.exprs.Get(assign.target).type;
  const mir::TypeId value_type = block.exprs.Get(assign.value).type;
  if (const auto* target_array = std::get_if<mir::UnpackedArrayType>(
          &unit_->Mir().types.Get(target_type).data)) {
    const auto* value_array = std::get_if<mir::UnpackedArrayType>(
        &unit_->Mir().types.Get(value_type).data);
    if (value_array != nullptr && value_array->dim != target_array->dim) {
      return Unsupported(
          "mir_to_lir: a whole-value store between unpacked arrays of "
          "different declared ranges is not yet lowerable to LIR");
    }
  }

  const lir::TypeId type = unit_->TranslateType(target_type);

  // An activation-frame value local is written through its handle, not a place:
  // a compound assignment reads the old value out of the cell, combines, and
  // overwrites.
  if (auto handle = ActivationValueHandleForTarget(block, assign.target)) {
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
            "mir_to_lir: compound assignment operator has no direct "
            "realization");
      }
      written = Emit(
          type, lir::BinaryInstr{
                    .op = *op,
                    .lhs = LoadActivationValue(*handle, type),
                    .rhs = *std::move(value)});
    }
    StoreActivationValue(*handle, written);
    return written;
  }

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

auto FunctionLowerer::WriteWholeValue(
    const mir::Block& block, mir::ExprId id, lir::Operand value)
    -> diag::Result<lir::Operand> {
  if (const std::optional<lir::Operand> handle =
          ActivationValueHandleForTarget(block, id)) {
    return StoreActivationValue(*handle, std::move(value));
  }
  auto place = LowerPlace(block, id);
  if (!place) {
    return std::unexpected(std::move(place.error()));
  }
  return Store(*std::move(place), std::move(value));
}

// Read the owner's whole value, descend the path, let `make_leaf` produce the
// part's new value, rebuild the whole value outward, and store it back. The
// owner and every coordinate evaluate exactly once, whatever the path's depth,
// and the store back through the owner is a single one. `make_leaf` receives a
// reader that extracts the part's current value on demand, so a plain store
// never reads it and a compound or an increment reads it once.
auto FunctionLowerer::LowerProjectionUpdate(
    const mir::Block& block, mir::ExprId target, const LeafTransform& make_leaf)
    -> diag::Result<lir::Operand> {
  const mir::Expr& target_expr = block.exprs.Get(target);
  const auto& projection = std::get<mir::ValueProjectionExpr>(target_expr.data);
  const lir::TypeId designated_type = unit_->TranslateType(target_expr.type);
  const std::vector<mir::Selector>& path = projection.path;

  auto owner_value = LowerExpr(block, projection.owner);
  if (!owner_value) {
    return std::unexpected(std::move(owner_value.error()));
  }

  // A step's key expressions are evaluated once, shared by a compound read and
  // the write-back; a positional step has none.
  std::vector<std::vector<lir::Operand>> keys(path.size());
  for (std::size_t depth = 0; depth < path.size(); ++depth) {
    const std::vector<mir::ExprId>* operands = std::visit(
        Overloaded{
            [](const mir::ComponentSelector&) {
              return static_cast<const std::vector<mir::ExprId>*>(nullptr);
            },
            [](const mir::UnionMemberSelector&) {
              return static_cast<const std::vector<mir::ExprId>*>(nullptr);
            },
            [](const mir::ElementSelector& e) { return &e.operands; },
            [](const mir::SliceSelector& s) { return &s.operands; }},
        path[depth]);
    if (operands == nullptr) {
      continue;
    }
    for (const mir::ExprId operand : *operands) {
      auto key = LowerExpr(block, operand);
      if (!key) {
        return std::unexpected(std::move(key.error()));
      }
      keys[depth].push_back(*std::move(key));
    }
  }

  // The type of the value each step descends into, and of the part it reaches.
  const auto projected_type = [&](std::size_t depth) {
    return unit_->TranslateType(
        std::visit(
            [](const auto& selector) { return selector.projected_type; },
            path[depth]));
  };
  const auto container_type = [&](std::size_t depth) {
    return depth == 0
               ? unit_->TranslateType(block.exprs.Get(projection.owner).type)
               : projected_type(depth - 1);
  };

  // The step's selector, in LIR's own vocabulary. A positional step names a
  // component slot; a coordinate-bearing one carries the operands it was
  // evaluated with. Which runtime entry realizes a step follows from the
  // aggregate's type, below this layer.
  const auto selector = [&](std::size_t depth) -> lir::AggregateSelector {
    return std::visit(
        Overloaded{
            [&](const mir::ComponentSelector& c) -> lir::AggregateSelector {
              return lir::TupleElement{.index = c.index};
            },
            [&](const mir::UnionMemberSelector& m) -> lir::AggregateSelector {
              return lir::UnionMember{.index = m.index};
            },
            [&](const mir::ElementSelector&) -> lir::AggregateSelector {
              return lir::ContainerElement{.operands = keys[depth]};
            },
            [&](const mir::SliceSelector&) -> lir::AggregateSelector {
              return lir::ContainerSlice{.operands = keys[depth]};
            }},
        path[depth]);
  };
  const auto extract = [&](const lir::Operand& container,
                           std::size_t depth) -> lir::Operand {
    return Emit(
        projected_type(depth),
        lir::AggregateExtractInstr{
            .aggregate = container, .selector = selector(depth)});
  };
  const auto update = [&](const lir::Operand& container, std::size_t depth,
                          lir::Operand replacement) -> lir::Operand {
    return Emit(
        container_type(depth), lir::AggregateUpdateInstr{
                                   .aggregate = container,
                                   .selector = selector(depth),
                                   .replacement = std::move(replacement)});
  };

  // The whole value at each chain level, descending from the owner toward the
  // sub-value being written.
  std::vector<lir::Operand> containers;
  containers.reserve(path.size());
  containers.push_back(*std::move(owner_value));
  for (std::size_t depth = 1; depth < path.size(); ++depth) {
    containers.push_back(extract(containers[depth - 1], depth - 1));
  }

  // The last step reaches what the designator denotes, so the two statements of
  // that type must agree; a disagreement means the path and the node's type
  // came from different lowerings.
  const std::size_t leaf = path.size() - 1;
  if (projected_type(leaf) != designated_type) {
    throw InternalError(
        "mir_to_lir: a designator's last step reaches a different type than "
        "the designator states");
  }
  auto leaf_value = make_leaf(
      [&] { return extract(containers[leaf], leaf); }, designated_type);
  if (!leaf_value) {
    return std::unexpected(std::move(leaf_value.error()));
  }

  // Rebuild the whole value from the written sub-value outward.
  lir::Operand rebuilt = *std::move(leaf_value);
  for (std::size_t depth = path.size(); depth-- > 0;) {
    rebuilt = update(containers[depth], depth, std::move(rebuilt));
  }
  return WriteWholeValue(block, projection.owner, std::move(rebuilt));
}

auto FunctionLowerer::LowerProjectionAssign(
    const mir::Block& block, const mir::AssignExpr& assign)
    -> diag::Result<lir::Operand> {
  // What the update stores is the owner's whole value; the assignment's own
  // value is the part it wrote.
  std::optional<lir::Operand> assigned;
  auto written = LowerProjectionUpdate(
      block, assign.target,
      [&](const LeafReader& read_leaf,
          lir::TypeId leaf_type) -> diag::Result<lir::Operand> {
        auto rhs = LowerExpr(block, assign.value);
        if (!rhs) {
          return std::unexpected(std::move(rhs.error()));
        }
        if (!assign.compound_op.has_value()) {
          assigned = *std::move(rhs);
          return *assigned;
        }
        const std::optional<lir::BinaryOp> op =
            TranslateBinaryOp(*assign.compound_op);
        if (!op) {
          return Unsupported(
              "mir_to_lir: compound assignment operator has no direct "
              "realization");
        }
        assigned = Emit(
            leaf_type,
            lir::BinaryInstr{
                .op = *op, .lhs = read_leaf(), .rhs = *std::move(rhs)});
        return *assigned;
      });
  if (!written) {
    return std::unexpected(std::move(written.error()));
  }
  return *assigned;
}

auto FunctionLowerer::LowerMutatingCall(
    const mir::Block& block, const mir::CallExpr& call, support::BuiltinFn fn,
    mir::TypeId type) -> diag::Result<lir::Operand> {
  // The entry yields the receiver's updated whole value, which is the call's
  // one result, so a builtin that also states a result of its own -- a queue
  // pop yields the element it removed (LRM 7.10.2.4) -- would need the call to
  // produce two values.
  if (type != unit_->Mir().builtins.void_type) {
    return Unsupported(
        std::format(
            "mir_to_lir: the {} builtin updates its receiver and yields a "
            "result of its own, which is not yet lowerable to LIR",
            support::BuiltinFnName(fn)));
  }
  const mir::ExprId receiver = call.arguments[0];
  const lir::TypeId container_type =
      unit_->TranslateType(block.exprs.Get(receiver).type);

  auto value = LowerExpr(block, receiver);
  if (!value) {
    return std::unexpected(std::move(value.error()));
  }
  std::vector<lir::Operand> args;
  args.reserve(call.arguments.size());
  args.push_back(*std::move(value));
  for (std::size_t i = 1; i < call.arguments.size(); ++i) {
    auto arg = LowerArgument(block, call.arguments[i]);
    if (!arg) {
      return std::unexpected(std::move(arg.error()));
    }
    args.push_back(*std::move(arg));
  }

  lir::Operand updated = Emit(
      container_type,
      lir::CallInstr{
          .target = lir::BuiltinTarget{.fn = fn, .qualifier = std::nullopt},
          .args = std::move(args)});
  return WriteWholeValue(block, receiver, std::move(updated));
}

auto FunctionLowerer::LowerIncDec(
    const mir::Block& block, const mir::IncDecExpr& inc_dec)
    -> diag::Result<lir::Operand> {
  const lir::TypeId type =
      unit_->TranslateType(block.exprs.Get(inc_dec.target).type);
  const bool is_increment = inc_dec.op == mir::IncDecOp::kPreInc ||
                            inc_dec.op == mir::IncDecOp::kPostInc;
  const bool is_prefix = inc_dec.op == mir::IncDecOp::kPreInc ||
                         inc_dec.op == mir::IncDecOp::kPreDec;
  const lir::UnaryOp op =
      is_increment ? lir::UnaryOp::kIncrement : lir::UnaryOp::kDecrement;

  // A designated part increments through its owner: the part's current value is
  // read out of the owner's whole value, stepped, and folded back in.
  if (std::holds_alternative<mir::ValueProjectionExpr>(
          block.exprs.Get(inc_dec.target).data)) {
    std::optional<lir::Operand> old;
    std::optional<lir::Operand> stepped;
    auto written = LowerProjectionUpdate(
        block, inc_dec.target,
        [&](const LeafReader& read_leaf,
            lir::TypeId leaf_type) -> diag::Result<lir::Operand> {
          old = read_leaf();
          stepped = Emit(leaf_type, lir::UnaryInstr{.op = op, .operand = *old});
          return *stepped;
        });
    if (!written) {
      return std::unexpected(std::move(written.error()));
    }
    // What was stored is the owner's whole value; the statement's own value is
    // the part, before or after the step.
    return is_prefix ? *stepped : *old;
  }

  // An activation-frame value local increments through its handle: read the old
  // value out, apply the step, overwrite.
  if (auto handle = ActivationValueHandleForTarget(block, inc_dec.target)) {
    const lir::Operand old = LoadActivationValue(*handle, type);
    const lir::Operand updated =
        Emit(type, lir::UnaryInstr{.op = op, .operand = old});
    StoreActivationValue(*handle, updated);
    return is_prefix ? updated : old;
  }

  auto place = LowerPlace(block, inc_dec.target);
  if (!place) {
    return std::unexpected(std::move(place.error()));
  }
  const lir::Operand old = Load(*place, type);
  const lir::Operand updated =
      Emit(type, lir::UnaryInstr{.op = op, .operand = old});
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
          [&](const mir::RealLiteral& lit) -> diag::Result<lir::Operand> {
            return lir::Operand{lir::RealConst{
                .value = lit.value, .type = unit_->TranslateType(type)}};
          },
          [&](const mir::NullLiteral&) -> diag::Result<lir::Operand> {
            return lir::Operand{
                lir::NullConst{.type = unit_->TranslateType(type)}};
          },
          [&](const mir::MachineIntLiteral& lit) -> diag::Result<lir::Operand> {
            return lir::Operand{lir::IntConst{
                .value =
                    lir::IntegralConstant{
                        .value_words = {static_cast<std::uint64_t>(lit.value)},
                        .state_words = {}},
                .type = unit_->TranslateType(type)}};
          },
          [&](const mir::LocalRef& ref) -> diag::Result<lir::Operand> {
            const std::optional<LocalBinding>& binding = locals_[ref.var.value];
            if (!binding.has_value()) {
              return Unsupported("mir_to_lir: reference to an unlowered local");
            }
            return std::visit(
                Overloaded{
                    [&](const PlaceBinding& place)
                        -> diag::Result<lir::Operand> {
                      return Load(
                          LocalPlace(place.slot),
                          fn_.values.Get(place.slot).type);
                    },
                    [](const ValueBinding& value)
                        -> diag::Result<lir::Operand> { return value.value; },
                    [&](const ActivationValueBinding& cell)
                        -> diag::Result<lir::Operand> {
                      return LoadActivationValue(
                          cell.handle, unit_->TranslateType(type));
                    },
                    [](const CaptureBinding&) -> diag::Result<lir::Operand> {
                      // The captures were passed instead of a record, so there
                      // is nothing to hand on; a closure reaching a use other
                      // than a capture read is one that outlives its call.
                      return Unsupported(
                          "mir_to_lir: a closure used as a value is not yet "
                          "lowerable to LIR");
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
                lir::ArrayInstr{.elements = std::move(elements)});
          },
          [&](const mir::TupleExpr& tuple) -> diag::Result<lir::Operand> {
            std::vector<lir::Operand> components;
            components.reserve(tuple.components.size());
            for (const mir::ExprId component : tuple.components) {
              auto lowered = LowerExpr(block, component);
              if (!lowered) {
                return std::unexpected(std::move(lowered.error()));
              }
              components.push_back(*std::move(lowered));
            }
            return Emit(
                unit_->TranslateType(type),
                lir::ProductInstr{.components = std::move(components)});
          },
          [&](const mir::VectorExpr& vec) -> diag::Result<lir::Operand> {
            std::vector<lir::Operand> elements;
            elements.reserve(vec.elements.size());
            for (const mir::ExprId element : vec.elements) {
              auto lowered = LowerExpr(block, element);
              if (!lowered) {
                return std::unexpected(std::move(lowered.error()));
              }
              elements.push_back(*std::move(lowered));
            }
            return Emit(
                unit_->TranslateType(type),
                lir::ArrayInstr{.elements = std::move(elements)});
          },
          [&](const mir::VectorGetExpr& get) -> diag::Result<lir::Operand> {
            auto vector = LowerExpr(block, get.vector);
            if (!vector) {
              return std::unexpected(std::move(vector.error()));
            }
            auto index = LowerExpr(block, get.index);
            if (!index) {
              return std::unexpected(std::move(index.error()));
            }
            return Emit(
                unit_->TranslateType(type),
                lir::AggregateExtractInstr{
                    .aggregate = *std::move(vector),
                    .selector = lir::ContainerElement{
                        .operands = {*std::move(index)}}});
          },
          [&](const mir::TupleGetExpr& get) -> diag::Result<lir::Operand> {
            auto tuple = LowerExpr(block, get.tuple);
            if (!tuple) {
              return std::unexpected(std::move(tuple.error()));
            }
            return Emit(
                unit_->TranslateType(type),
                lir::AggregateExtractInstr{
                    .aggregate = *std::move(tuple),
                    .selector = lir::TupleElement{.index = get.index}});
          },
          [&](const mir::ClosureExpr& cl) -> diag::Result<lir::Operand> {
            // A closure that completes as a coroutine is that coroutine:
            // building it starts a frame the scheduler owns, which a record of
            // captures carrying no code identity cannot stand for.
            if (unit_->Mir().types.IsCoroutine(type)) {
              return Unsupported(
                  "mir_to_lir: a coroutine closure, the process a fork branch "
                  "or a task enable starts, is not yet lowerable to LIR");
            }
            // Constructing a closure builds its capture record, and nothing
            // else: which body a call runs is already fixed by the type, so no
            // code identity is stored alongside the captures. Initializers are
            // evaluated in the order they are listed -- their source-semantic
            // order -- and each lands at the capture it targets, since the two
            // orders need not agree.
            const mir::ClosureDecl& decl = unit_->Mir().GetClosure(cl.closure);
            if (cl.field_inits.size() != decl.fields.size()) {
              throw InternalError(
                  "mir_to_lir: closure construction does not initialize every "
                  "capture");
            }
            std::vector<lir::Operand> captures(decl.fields.size());
            for (const mir::FieldInit& init : cl.field_inits) {
              auto value = LowerExpr(block, init.value);
              if (!value) {
                return std::unexpected(std::move(value.error()));
              }
              captures[init.target.value] = *std::move(value);
            }
            return Emit(
                unit_->TranslateType(type),
                lir::ProductInstr{.components = std::move(captures)});
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
          [&](const mir::FieldAccessExpr& field) -> diag::Result<lir::Operand> {
            if (auto capture = CaptureRead(block, field)) {
              return *std::move(capture);
            }
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
          // A contiguous aggregate begins at its own address, so the pointer to
          // the first element is the array's address retyped by the result --
          // no interior step, which the place vocabulary does not have.
          [&](const mir::MachineArrayDataExpr& d)
              -> diag::Result<lir::Operand> {
            auto place = LowerPlace(block, d.array);
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
            // A logical-not over a machine boolean -- the reduced predicate a
            // real- or chandle-family `!` produces before `from_bool` widens it
            // back -- stays a machine boolean; its surface 1-bit type is
            // restored by the enclosing `from_bool`.
            const lir::TypeId result_type =
                (*op == lir::UnaryOp::kLogicalNot &&
                 lir::OperandType(fn_, *operand) == unit_->MachineBoolType())
                    ? unit_->MachineBoolType()
                    : unit_->TranslateType(type);
            return Emit(
                result_type,
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
            // A logical or equality operator composing machine booleans -- the
            // reduced predicates a real- or string-family `&&` / `||` / `<->`
            // builds before `from_bool` widens the result back -- stays a
            // machine boolean; its surface 1-bit type is restored by the
            // enclosing `from_bool`.
            const lir::TypeId result_type =
                lir::OperandType(fn_, *lhs) == unit_->MachineBoolType()
                    ? unit_->MachineBoolType()
                    : unit_->TranslateType(type);
            return Emit(
                result_type,
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
            //
            // An awaitable whose type is a coroutine is the other protocol: it
            // registers nothing, because its completion is the awaited body's
            // to signal, so a bare suspend edge would park the awaiting body
            // with nothing arranged to resume it.
            if (unit_->Mir().types.IsCoroutine(
                    block.exprs.Get(await.awaitable).type)) {
              return Unsupported(
                  "mir_to_lir: an await on a coroutine callee is not yet "
                  "lowerable to LIR");
            }
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
