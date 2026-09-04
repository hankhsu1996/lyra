#include "lyra/lowering/mir_to_lir/function_lowerer.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <format>
#include <optional>
#include <ranges>
#include <span>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/component_index.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/integral_constant.hpp"
#include "lyra/lir/operator.hpp"
#include "lyra/lir/type_builders.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/lowering/mir_to_lir/unit_lowerer.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/inc_dec_op.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/unary_op.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::mir_to_lir {

namespace {

// Where a mutating call's completion components sit when the method states a
// result of its own: the receiver as the operation left it, then that result.
constexpr base::ComponentIndex kUpdatedReceiver{0};
constexpr base::ComponentIndex kMutatingCallResult{1};

auto Unsupported(std::string message) -> std::unexpected<diag::Diagnostic> {
  return diag::Fail(
      diag::DiagCode::kUnsupportedExpressionForm, std::move(message));
}

// The place a place local names: its own storage, with nothing projected off
// it.
auto LocalPlace(lir::ValueId local) -> lir::Place {
  return lir::Place{.base = lir::Use{.value = local}, .chain = {}};
}

// Whether this call builds a reference over its one argument. A reference is
// the address of the storage it binds, so the argument's storage is what the
// value names -- which both the storage-topology pass and the lowering itself
// have to know, and must agree on.
auto BindsReference(
    const mir::TypePool& types, const mir::CallExpr& call,
    mir::TypeId result_type) -> bool {
  return std::holds_alternative<mir::Construct>(call.callee) &&
         types.Get(result_type).Is<mir::RefType>();
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
  return type.IsIntegralPacked() || type.Is<mir::StringType>() ||
         type.Is<mir::RealType>() || type.Is<mir::ShortRealType>() ||
         type.Is<mir::RealTimeType>() || type.Is<mir::TupleType>() ||
         type.Is<mir::DynamicArrayType>();
}

// Marks every local the canonical lowering needs an address for: one that is
// assigned after its initialization, or has its address taken. Such a local is
// storage, so it must be a place local. A read never makes a local storage: a
// value read many times is still a value. The whole expression arena is
// scanned, not just the reachable statements, so a local written only by an
// unreachable expression is conservatively storage.
void CollectStorageLocals(
    const mir::TypePool& types, const mir::Block& block,
    std::vector<bool>& placed, std::vector<bool>& lent) {
  const auto mark = [&](std::optional<mir::LocalId> local) {
    if (local.has_value()) {
      placed[local->value] = true;
    }
  };
  const auto mark_lent = [&](std::optional<mir::LocalId> local) {
    if (local.has_value()) {
      lent[local->value] = true;
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
                mark_lent(PlacedLocal(block, e.arguments[0]));
              }
            },
            [&](const mir::MachineArrayDataExpr& e) {
              mark(PlacedLocal(block, e.array));
            },
            [](const auto&) {}},
        expr.data);
  }
  for (const mir::BlockId id : block.child_scopes.Ids()) {
    CollectStorageLocals(types, block.child_scopes.Get(id), placed, lent);
  }
}

// A method of another unit's class is reached by the symbol that unit emits it
// under, which the referrer composes from the unit, the class, and the method
// its signature named -- the same three names, so the two agree with no table
// between them.
auto ExternalMethodSymbol(
    std::string_view unit_name, std::string_view class_name,
    std::string_view method_name) -> lir::ForeignTarget {
  return lir::ForeignTarget{
      .symbol = std::format("{}.{}.{}", unit_name, class_name, method_name)};
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
                    [&](const mir::ExternalUnitClassMethodTarget& t)
                        -> diag::Result<lir::CallTarget> {
                      return lir::CallTarget{ExternalMethodSymbol(
                          t.unit_name, t.class_name, t.method_name)};
                    },
                    [&](const mir::ExternalUnitStaticMethodTarget& t)
                        -> diag::Result<lir::CallTarget> {
                      return lir::CallTarget{ExternalMethodSymbol(
                          t.unit_name, t.class_name, t.method_name)};
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
            return Unsupported(
                "mir_to_lir: a call through a computed code address is not yet "
                "lowerable to LIR");
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
      description_(nullptr),
      name_(std::move(name)),
      placed_(code.locals.size(), false),
      activation_value_local_(code.locals.size(), false),
      cell_local_(code.locals.size(), false),
      locals_(code.locals.size(), std::nullopt) {
}

FunctionLowerer::FunctionLowerer(
    UnitLowerer& unit, const mir::ClosureDecl& closure, std::string name)
    : unit_(&unit),
      code_(&closure.invoke),
      closure_(&closure),
      description_(nullptr),
      name_(std::move(name)),
      placed_(closure.invoke.locals.size(), false),
      activation_value_local_(closure.invoke.locals.size(), false),
      cell_local_(closure.invoke.locals.size(), false),
      locals_(closure.invoke.locals.size(), std::nullopt) {
}

FunctionLowerer::FunctionLowerer(
    UnitLowerer& unit, const mir::PackedTypeDescription& description,
    std::string name)
    : unit_(&unit),
      code_(nullptr),
      closure_(nullptr),
      description_(&description),
      name_(std::move(name)) {
}

void FunctionLowerer::BindCaptureReceiver(mir::LocalId receiver) {
  const mir::LocalDecl& decl = code_->locals.Get(receiver);
  const lir::ValueId value = fn_.values.Add(
      lir::Local{
          .name = decl.name,
          .type = unit_->TranslateType(decl.type),
          .kind = lir::LocalKind::kParam});
  fn_.params.push_back(value);
  locals_[receiver.value] =
      LocalBinding{ValueBinding{.value = lir::Use{.value = value}}};
}

auto FunctionLowerer::LowerDescription(
    UnitLowerer& unit, const mir::PackedTypeDescription& description,
    std::string name) -> diag::Result<lir::Function> {
  return FunctionLowerer(unit, description, std::move(name)).RunDescription();
}

auto FunctionLowerer::RunDescription() -> diag::Result<lir::Function> {
  fn_.name = std::move(name_);
  fn_.result_type = unit_->TranslateType(unit_->Mir().builtins.packed_type);
  SetCurrent(NewBlock());
  auto value = LowerExpr(description_->body, description_->value);
  if (!value) {
    return std::unexpected(std::move(value.error()));
  }
  Terminate(lir::ReturnTerm{.value = *std::move(value)});
  for (OpenBlock& block : blocks_) {
    fn_.blocks.push_back(
        lir::BasicBlock{
            .instrs = std::move(block.instrs),
            .terminator = *std::move(block.terminator)});
  }
  return std::move(fn_);
}

auto FunctionLowerer::Run() -> diag::Result<lir::Function> {
  fn_.name = std::move(name_);
  // A coroutine-bodied callable keeps its coroutine result type: coroutine-ness
  // is the call protocol carried by the type, so a backend realizes suspension
  // and completion from the type, never from a separate flag.
  fn_.result_type = unit_->TranslateType(code_->result_type);
  const bool is_coroutine =
      unit_->Mir().types.Get(code_->result_type).Is<mir::CoroutineType>();

  std::vector<bool> lent(code_->locals.size(), false);
  CollectStorageLocals(unit_->Mir().types, code_->Body(), placed_, lent);

  // In a suspending body every value-typed, non-managed local and parameter is
  // an activation value, not a transient: a value's handle cannot safely live
  // across a suspension, so each such local needs a cell of the execution's own
  // store, which the generated frame reaches by a handle. A suspension is a
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

  // A local lent by reference lives in a cell a reference can name. Where its
  // value is already an activation value the placement above stands, and
  // lending it has no form here: such a cell is reached by its own calls rather
  // than by an address a reference could carry.
  for (const mir::LocalId local : code_->locals.Ids()) {
    cell_local_[local.value] =
        lent[local.value] && !activation_value_local_[local.value];
  }

  // A parameter is a declared local whose initial value is the incoming
  // argument. It arrives as a value in the signature and is bound like any
  // local: a place if the body assigns or addresses it, otherwise the argument
  // value itself. The entry block exists first so a spilled parameter's copy
  // into its place lands there, ahead of the body.
  SetCurrent(NewBlock());
  // A closure invoke's receiver names the storage its captures live in, and
  // leads the per-invocation parameters in the signature.
  if (closure_ != nullptr) {
    BindCaptureReceiver(mir::LocalId{0});
  }
  for (const mir::LocalId param : code_->params) {
    const mir::LocalDecl& decl = code_->locals.Get(param);
    const lir::TypeId type = unit_->TranslateType(decl.type);
    const lir::ValueId value = fn_.values.Add(
        lir::Local{
            .name = decl.name, .type = type, .kind = lir::LocalKind::kParam});
    fn_.params.push_back(value);
    // A parameter whose storage is a cell installs that cell's representation
    // from the incoming argument, its first write; every other parameter binds
    // to the argument value as a place or a plain value.
    if (activation_value_local_[param.value]) {
      const lir::Operand handle = AllocateActivationValue(type);
      locals_[param.value] =
          LocalBinding{ActivationValueBinding{.handle = handle}};
      StoreActivationValue(handle, lir::Use{.value = value}, type);
    } else if (cell_local_[param.value]) {
      const lir::Operand reference = AllocateCell(type);
      locals_[param.value] = LocalBinding{CellBinding{.reference = reference}};
      InitializeCell(reference, lir::Use{.value = value});
    } else {
      BindLocal(param, type, lir::Use{.value = value});
    }
  }

  // A body whose completion carries a value is handed the storage to complete
  // into, as its last parameter. The caller owns it, which is what makes it
  // readable after this body has stopped -- storage of this body's own would be
  // gone by then.
  if (const auto* coroutine =
          unit_->Mir().types.Get(code_->result_type).As<mir::CoroutineType>();
      coroutine != nullptr &&
      coroutine->payload != unit_->Mir().builtins.void_type) {
    const lir::TypeId payload = unit_->TranslateType(coroutine->payload);
    const lir::ValueId slot = fn_.values.Add(
        lir::Local{
            .name = "completion",
            .type = payload,
            .kind = lir::LocalKind::kParam});
    fn_.params.push_back(slot);
    completion_cell_ =
        CompletionCell{.cell = lir::Use{.value = slot}, .type = payload};
  }

  // Every other activation value's handle is allocated once here, at frame
  // entry, so it is reused across iterations rather than re-created per
  // declaration; its first store, installing the representation, is the
  // declaration's initializer reached during the body walk.
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

auto FunctionLowerer::AllocateCompletionFor(lir::TypeId payload)
    -> lir::Operand {
  const lir::ValueId result = fn_.values.Add(
      lir::Local{.name = {}, .type = payload, .kind = lir::LocalKind::kTemp});
  // Into the entry block rather than where the call stands, so a call inside a
  // loop writes into one place instead of leaving a fresh one behind on every
  // iteration.
  blocks_[0].instrs.push_back(
      lir::Instr{
          .result = result,
          .data = lir::CallInstr{
              .target =
                  lir::ValueCellTarget{
                      .op = lir::ValueCellTarget::Op::kAllocate,
                      .value = payload},
              .args = {}}});
  return lir::Use{.value = result};
}

auto FunctionLowerer::NewPlaceLocal(lir::TypeId type) -> lir::ValueId {
  return fn_.values.Add(
      lir::Local{.name = {}, .type = type, .kind = lir::LocalKind::kPlace});
}

// Introduces a declared local, holding its initial value. Where that value
// lives follows what the body does with the local: one whose storage is lent
// gets a cell, one the body assigns later or addresses becomes frame storage,
// and one that is only ever read stays the initial value itself.
void FunctionLowerer::BindLocal(
    mir::LocalId local, lir::TypeId type, lir::Operand init) {
  // An activation value's cell was allocated at frame entry; its declaration's
  // initializer is the first write, which installs its representation.
  if (activation_value_local_[local.value]) {
    StoreActivationValue(
        std::get<ActivationValueBinding>(*locals_[local.value]).handle,
        std::move(init), type);
    return;
  }
  // A lent local's cell is built where it is declared, so each entry to that
  // declaration is a fresh variable with its own storage, and installing the
  // cell's representation is that declaration's initializer.
  if (cell_local_[local.value]) {
    const lir::Operand reference = AllocateCell(type);
    locals_[local.value] = LocalBinding{CellBinding{.reference = reference}};
    InitializeCell(reference, std::move(init));
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
                          lir::ValueCellTarget{
                              .op = lir::ValueCellTarget::Op::kAllocate,
                              .value = value_type},
                      .args = {}});
}

auto FunctionLowerer::LoadActivationValue(
    lir::Operand handle, lir::TypeId value_type) -> lir::Operand {
  return Emit(
      value_type,
      lir::CallInstr{
          .target =
              lir::ValueCellTarget{
                  .op = lir::ValueCellTarget::Op::kLoad, .value = value_type},
          .args = {std::move(handle)}});
}

auto FunctionLowerer::StoreActivationValue(
    lir::Operand handle, lir::Operand value, lir::TypeId value_type)
    -> lir::Operand {
  return Emit(
      unit_->TranslateType(unit_->Mir().builtins.void_type),
      lir::CallInstr{
          .target =
              lir::ValueCellTarget{
                  .op = lir::ValueCellTarget::Op::kStore, .value = value_type},
          .args = {std::move(handle), std::move(value)}});
}

auto FunctionLowerer::AllocateCell(lir::TypeId value_type) -> lir::Operand {
  const lir::TypeId reference = lir::ReferenceToCellOf(
      unit_->Types(), value_type, lir::Mutability::kMutable);
  return Emit(
      reference,
      lir::CallInstr{
          .target = lir::ConstructTarget{.result = reference}, .args = {}});
}

auto FunctionLowerer::InitializeCell(lir::Operand reference, lir::Operand value)
    -> lir::Operand {
  return Emit(
      unit_->Types().Intern(lir::Type{lir::VoidType{}}),
      lir::CallInstr{
          .target =
              lir::BuiltinTarget{
                  .fn = support::BuiltinFn::kInitialize, .qualifier = {}},
          .args = {std::move(reference), std::move(value)}});
}

auto FunctionLowerer::ReferencedCell(lir::Operand reference) -> lir::Place {
  return lir::Place{
      .base = std::move(reference),
      .chain = {lir::Projection{lir::DerefProjection{}}}};
}

auto FunctionLowerer::ReferencedValue(lir::Operand reference) -> lir::Place {
  lir::Place place = ReferencedCell(std::move(reference));
  place.chain.emplace_back(lir::DerefProjection{});
  return place;
}

auto FunctionLowerer::ActivationValueHandleForTarget(
    const mir::Block& block, mir::ExprId id) -> std::optional<lir::Operand> {
  const auto* ref = std::get_if<mir::LocalRef>(&block.exprs.Get(id).data);
  if (ref == nullptr || !locals_[ref->var.value].has_value()) {
    return std::nullopt;
  }
  const auto* slot =
      std::get_if<ActivationValueBinding>(&*locals_[ref->var.value]);
  return slot != nullptr ? std::optional{slot->handle} : std::nullopt;
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
          [&](const mir::TryStmt& s) -> diag::Result<void> {
            return LowerTryInto(block, s);
          },
          [&](const mir::RaiseStmt& s) -> diag::Result<void> {
            return LowerRaiseInto(block, s);
          },
          [&](const mir::FinallyStmt& s) -> diag::Result<void> {
            return LowerFinallyInto(block, s);
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
            // An execution answers through its completion rather than to a
            // caller standing below it: nothing is on the stack to receive a
            // returned value, and what awaits it runs later.
            if (completion_cell_.has_value() && value.has_value()) {
              StoreActivationValue(
                  completion_cell_->cell, *std::move(value),
                  completion_cell_->type);
              value.reset();
            }
            // Returning leaves every guarded body between here and the frame's
            // edge, so each of their cleanups runs -- after the returned value
            // is settled, which a cleanup must not change.
            auto cleaned = RunCleanupsDownTo(0);
            if (!cleaned) {
              return std::unexpected(std::move(cleaned.error()));
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
          .break_target = exit_id,
          .cleanup_depth = cleanups_.size()});
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
          .break_target = exit_id,
          .cleanup_depth = cleanups_.size()});
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
          .break_target = exit_id,
          .cleanup_depth = cleanups_.size()});
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
      auto cleaned = RunCleanupsDownTo(loop.cleanup_depth);
      if (!cleaned) {
        return std::unexpected(std::move(cleaned.error()));
      }
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
  auto cleaned = RunCleanupsDownTo(loops_.back().cleanup_depth);
  if (!cleaned) {
    return std::unexpected(std::move(cleaned.error()));
  }
  Terminate(lir::BranchTerm{.target = loops_.back().continue_target});
  return {};
}

auto FunctionLowerer::CurrentRuntime() -> lir::Operand {
  return Emit(
      unit_->TranslateType(unit_->Mir().builtins.effects),
      lir::CallInstr{
          .target =
              lir::BuiltinTarget{
                  .fn = support::BuiltinFn::kCurrentRuntime,
                  .qualifier = std::nullopt},
          .args = {}});
}

auto FunctionLowerer::RunCleanupsDownTo(std::size_t depth)
    -> diag::Result<void> {
  for (std::size_t i = cleanups_.size(); i > depth; --i) {
    const PendingCleanup pending = cleanups_[i - 1];
    auto lowered =
        LowerBlockInto(pending.owner->child_scopes.Get(pending.cleanup));
    if (!lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
  }
  return {};
}

auto FunctionLowerer::LeaveCarrying(lir::Operand effect) -> diag::Result<void> {
  if (!regions_.empty()) {
    const RegionTargets region = regions_.back();
    auto cleaned = RunCleanupsDownTo(region.cleanup_depth);
    if (!cleaned) {
      return std::unexpected(std::move(cleaned.error()));
    }
    Store(
        lir::Place{.base = lir::Use{.value = region.caught}, .chain = {}},
        std::move(effect));
    Terminate(lir::BranchTerm{.target = region.handler});
    return {};
  }
  auto cleaned = RunCleanupsDownTo(0);
  if (!cleaned) {
    return std::unexpected(std::move(cleaned.error()));
  }
  Emit(
      unit_->TranslateType(unit_->Mir().builtins.void_type),
      lir::CallInstr{
          .target =
              lir::ControlEffectTarget{
                  .op = lir::ControlEffectTarget::Op::kSettleCancelled},
          .args = {std::move(effect)}});
  Terminate(lir::ReturnTerm{.value = std::nullopt});
  return {};
}

auto FunctionLowerer::CheckDisabledTarget() -> diag::Result<void> {
  const lir::Operand condition = Emit(
      unit_->MachineBoolType(),
      lir::CallInstr{
          .target =
              lir::ControlEffectTarget{
                  .op = lir::ControlEffectTarget::Op::kHasInvalidatedTarget},
          .args = {CurrentRuntime()}});
  const lir::BlockId leaving_id = NewBlock();
  const lir::BlockId continue_id = NewBlock();
  Terminate(
      lir::CondBranchTerm{
          .condition = condition,
          .if_true = leaving_id,
          .if_false = continue_id});

  SetCurrent(leaving_id);
  const lir::Operand effect = Emit(
      unit_->ControlEffectType(),
      lir::CallInstr{
          .target =
              lir::ControlEffectTarget{
                  .op = lir::ControlEffectTarget::Op::kInvalidatedTarget},
          .args = {CurrentRuntime()}});
  auto left = LeaveCarrying(effect);
  if (!left) {
    return std::unexpected(std::move(left.error()));
  }

  SetCurrent(continue_id);
  return {};
}

auto FunctionLowerer::LowerFinallyInto(
    const mir::Block& block, const mir::FinallyStmt& stmt)
    -> diag::Result<void> {
  cleanups_.push_back(PendingCleanup{.owner = &block, .cleanup = stmt.cleanup});
  auto body = LowerBlockInto(block.child_scopes.Get(stmt.body));
  cleanups_.pop_back();
  if (!body) {
    return std::unexpected(std::move(body.error()));
  }
  // Falling off the body's end is the one way out the body does not state
  // itself, so it is the one the region states here.
  if (Terminated()) {
    return {};
  }
  return LowerBlockInto(block.child_scopes.Get(stmt.cleanup));
}

auto FunctionLowerer::LowerTryInto(
    const mir::Block& block, const mir::TryStmt& stmt) -> diag::Result<void> {
  const lir::BlockId handler_id = NewBlock();
  const lir::BlockId merge_id = NewBlock();

  // The effect is written where control leaves the body and read where the
  // handler runs, which are different blocks, so it is frame storage rather
  // than a value in flight.
  const lir::ValueId caught =
      NewPlaceLocal(unit_->TranslateType(code_->locals.Get(stmt.caught).type));
  locals_[stmt.caught.value] = PlaceBinding{.slot = caught};

  regions_.push_back(
      RegionTargets{
          .handler = handler_id,
          .caught = caught,
          .cleanup_depth = cleanups_.size()});
  auto body = LowerBlockInto(block.child_scopes.Get(stmt.body));
  regions_.pop_back();
  if (!body) {
    return std::unexpected(std::move(body.error()));
  }
  if (!Terminated()) {
    Terminate(lir::BranchTerm{.target = merge_id});
  }

  SetCurrent(handler_id);
  auto handler = LowerBlockInto(block.child_scopes.Get(stmt.handler));
  if (!handler) {
    return std::unexpected(std::move(handler.error()));
  }
  // Reaching the handler's end is what claiming the effect amounts to:
  // execution resumes past the region (LRM 9.6.2).
  if (!Terminated()) {
    Terminate(lir::BranchTerm{.target = merge_id});
  }

  SetCurrent(merge_id);
  return {};
}

auto FunctionLowerer::LowerRaiseInto(
    const mir::Block& block, const mir::RaiseStmt& stmt) -> diag::Result<void> {
  auto effect = LowerExpr(block, stmt.effect);
  if (!effect) {
    return std::unexpected(std::move(effect.error()));
  }
  return LeaveCarrying(*std::move(effect));
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

auto FunctionLowerer::MemberRefOf(
    const mir::Block& block, const mir::FieldAccessExpr& field)
    -> diag::Result<lir::MemberRef> {
  return std::visit(
      Overloaded{
          [&](const mir::FieldTarget& t) -> diag::Result<lir::MemberRef> {
            return lir::MemberRef{
                .declared_by = unit_->ClassValueType(t.owner),
                .slot = lir::MemberSlot{t.slot.value}};
          },
          [&](const mir::FieldId& id) -> diag::Result<lir::MemberRef> {
            // The receiver of a bare field id declares the field itself -- a
            // struct, a closure, or another unit's object, none of which
            // inherits -- so the declaration is what the receiver points at.
            const lir::TypeId receiver =
                unit_->TranslateType(block.exprs.Get(field.receiver).type);
            const std::optional<lir::TypeId> pointee =
                unit_->Types().Get(receiver).DerefTarget();
            if (!pointee) {
              throw InternalError(
                  "mir_to_lir: a bare-field-id access expects a receiver that "
                  "refers to member-bearing storage");
            }
            return lir::MemberRef{
                .declared_by = *pointee, .slot = lir::MemberSlot{id.value}};
          },
          [](const mir::ExternalFieldTarget&) -> diag::Result<lir::MemberRef> {
            // A field of an SV class another unit declares is named rather than
            // numbered, and no unit publishes an SV class, so nothing here
            // states where it sits.
            return Unsupported(
                "mir_to_lir: a property of a class another compilation unit "
                "declares is not yet reachable on this backend");
          },
          [](const mir::ComponentTarget&) -> diag::Result<lir::MemberRef> {
            // A structural product's component is a position in a value rather
            // than a slot in member-bearing storage, so no member names it.
            return Unsupported(
                "mir_to_lir: binding part of a value rather than writing it is "
                "not yet lowerable to LIR");
          }},
      field.field);
}

// Whether an expression names a part of a value rather than storage. Every such
// expression is an access whose receiver holds the part it reaches, so the
// question is answered by that node alone -- reaching further would be asking
// where a write through it ultimately lands, a different question.
auto ReachesIntoValue(const mir::Block& block, mir::ExprId target) -> bool {
  const mir::ExprData& data = block.exprs.Get(target).data;
  if (const auto* field = std::get_if<mir::FieldAccessExpr>(&data)) {
    return std::holds_alternative<mir::ComponentTarget>(field->field);
  }
  const auto* call = std::get_if<mir::CallExpr>(&data);
  if (call == nullptr) {
    return false;
  }
  const std::optional<support::BuiltinFn> fn = mir::DirectBuiltinFn(*call);
  return fn == support::BuiltinFn::kElementRef ||
         fn == support::BuiltinFn::kSliceRef;
}

// The value a step reaches its part out of. Every such step names it first, so
// this is the operand a walk toward the owner continues through.
auto ValuePartReceiver(const mir::Block& block, mir::ExprId step)
    -> mir::ExprId {
  const mir::ExprData& data = block.exprs.Get(step).data;
  if (const auto* field = std::get_if<mir::FieldAccessExpr>(&data)) {
    return field->receiver;
  }
  return std::get<mir::CallExpr>(data).arguments.front();
}

// The wrapper a pointer opens, when the pointer is that opening rather than an
// ordinary one. Which storage a wrapper currently stands for is a fact about
// the wrapper, so asking for it is an operation on one: a target whose values
// carry interiors answers with a pointer into the storage, and one whose values
// do not answers with the chain step the wrapper's own storage already is.
auto OpenedWrapper(const mir::Block& block, mir::ExprId pointer)
    -> std::optional<mir::ExprId> {
  const auto* call = std::get_if<mir::CallExpr>(&block.exprs.Get(pointer).data);
  if (call == nullptr || call->arguments.empty() ||
      mir::DirectBuiltinFn(*call) != support::BuiltinFn::kOpenForWrite) {
    return std::nullopt;
  }
  return call->arguments.front();
}

auto FunctionLowerer::WrapperContentsPlace(
    const mir::Block& block, mir::ExprId wrapper) -> diag::Result<lir::Place> {
  const mir::Type& wrapper_ty =
      unit_->Mir().types.Get(block.exprs.Get(wrapper).type);
  // A wrapper that is itself storage -- an observable cell, a net's resolved
  // value -- is storage the chain has already reached, so naming what it
  // represents extends that chain by one step. Everything else here refers to
  // storage elsewhere: a pointer, a reference, and the driver handle a net
  // issued are values, and a value opens a chain rather than continuing one.
  if (wrapper_ty.Is<mir::ObservableType>() ||
      wrapper_ty.Is<mir::ResolvedType>()) {
    auto place = LowerPlace(block, wrapper);
    if (!place) {
      return std::unexpected(std::move(place.error()));
    }
    lir::Place contents = *std::move(place);
    contents.chain.emplace_back(lir::DerefProjection{});
    return contents;
  }
  auto pointer = LowerExpr(block, wrapper);
  if (!pointer) {
    return std::unexpected(std::move(pointer.error()));
  }
  // A reference points at a cell, so opening it lands on storage rather than on
  // a value: naming the value it stands for takes the cell's own step as well.
  if (wrapper_ty.Is<mir::RefType>()) {
    return ReferencedValue(*std::move(pointer));
  }
  return lir::Place{
      .base = *std::move(pointer),
      .chain = {lir::Projection{lir::DerefProjection{}}}};
}

auto FunctionLowerer::LowerPlace(const mir::Block& block, mir::ExprId id)
    -> diag::Result<lir::Place> {
  const mir::Expr& expr = block.exprs.Get(id);
  // A part of a value is a position in it rather than a slot in storage: the
  // value crosses to the generated side as a handle a copy may alias, so the
  // part has no storage of its own for anything to bind. A write through one
  // still has a realization -- read the whole, replace the part, store it back
  // -- because nothing there has to outlive the expression.
  if (ReachesIntoValue(block, id)) {
    return Unsupported(
        "mir_to_lir: binding part of a value rather than writing it is not yet "
        "lowerable to LIR");
  }
  return std::visit(
      Overloaded{
          [&](const mir::LocalRef& ref) -> diag::Result<lir::Place> {
            const std::optional<LocalBinding>& binding = locals_[ref.var.value];
            if (!binding.has_value()) {
              return Unsupported(
                  "mir_to_lir: local is not addressable storage");
            }
            // A local whose storage is a cell names its value through the
            // reference it holds; one that is a frame slot names the slot.
            if (const auto* cell = std::get_if<CellBinding>(&*binding)) {
              return ReferencedValue(cell->reference);
            }
            const auto* place = std::get_if<PlaceBinding>(&*binding);
            if (place == nullptr) {
              return Unsupported(
                  "mir_to_lir: local is not addressable storage");
            }
            return LocalPlace(place->slot);
          },
          [&](const mir::FieldAccessExpr& field) -> diag::Result<lir::Place> {
            auto member = MemberRefOf(block, field);
            if (!member) {
              return std::unexpected(std::move(member.error()));
            }
            auto receiver = LowerExpr(block, field.receiver);
            if (!receiver) {
              return std::unexpected(std::move(receiver.error()));
            }
            return lir::Place{
                .base = *std::move(receiver),
                .chain = {
                    lir::Projection{lir::DerefProjection{}},
                    lir::Projection{lir::MemberProjection{.member = *member}}}};
          },
          [&](const mir::DerefExpr& deref) -> diag::Result<lir::Place> {
            // Opening a wrapper for writing names the storage it stands for,
            // which is the same storage reading its contents names.
            if (const std::optional<mir::ExprId> wrapper =
                    OpenedWrapper(block, deref.pointer)) {
              return WrapperContentsPlace(block, *wrapper);
            }
            return WrapperContentsPlace(block, deref.pointer);
          },
          // A variable of a unit's namespace is one cell for the whole program
          // that no instance holds, so it is reached by the symbol it links
          // under rather than through a receiver -- by the unit that declares
          // it exactly as by any other, since a namespace has no instance. The
          // symbol names the cell's address, so the place opens there and
          // dereferences it, the same shape a member place has once its
          // receiver is resolved.
          [&](const mir::ExternalUnitVariableRef& ref)
              -> diag::Result<lir::Place> {
            return lir::Place{
                .base =
                    lir::StaticRef{
                        .symbol = StaticVariableSymbol(
                            ref.unit_name, ref.variable_name),
                        .type = unit_->Types().Intern(
                            lir::Type{lir::PointerType{
                                .pointee = unit_->TranslateType(expr.type),
                                .ownership = lir::PointerOwnership::kBorrowed,
                                .mutability = lir::Mutability::kMutable}})},
                .chain = {lir::Projection{lir::DerefProjection{}}}};
          },
          // Each of the following does name storage, reached by a name rather
          // than through a receiver, and what it lacks is the storage itself:
          // nothing yet builds a cell for a class's type-associated
          // declarations, so no symbol names one.
          [](const mir::StaticConstantRef&) -> diag::Result<lir::Place> {
            return Unsupported(
                "mir_to_lir: a class's static constant is not yet reachable on "
                "this backend");
          },
          [](const mir::StaticPropertyRef&) -> diag::Result<lir::Place> {
            return Unsupported(
                "mir_to_lir: a class's static property is not yet reachable on "
                "this backend");
          },
          [](const mir::ExternalStaticPropertyRef&)
              -> diag::Result<lir::Place> {
            return Unsupported(
                "mir_to_lir: a static property of a class another compilation "
                "unit declares is not yet reachable on this backend");
          },
          [](const auto&) -> diag::Result<lir::Place> {
            return Unsupported("mir_to_lir: expression form names no place");
          }},
      expr.data);
}

auto FunctionLowerer::ReadPlace(
    const mir::Block& block, mir::ExprId id, lir::TypeId type)
    -> diag::Result<lir::Operand> {
  if (unit_->Types().Get(type).IsAddressOnly()) {
    return Unsupported(
        "mir_to_lir: a storage cell has no value to read; it is reached "
        "through its address");
  }
  auto place = LowerPlace(block, id);
  if (!place) {
    return std::unexpected(std::move(place.error()));
  }
  return Load(*std::move(place), type);
}

auto FunctionLowerer::LowerArgument(const mir::Block& block, mir::ExprId id)
    -> diag::Result<lir::Operand> {
  // The parameter's type decides. A parameter of an address-only type asks for
  // the storage itself -- a cell, a scope -- so the argument is where it lives,
  // not a reading of it. This is a fact about what the callee takes, not about
  // the expression that produced the place.
  const lir::TypeId type = unit_->TranslateType(block.exprs.Get(id).type);
  if (!unit_->Types().Get(type).IsAddressOnly()) {
    return LowerExpr(block, id);
  }
  auto place = LowerPlace(block, id);
  if (!place) {
    return std::unexpected(std::move(place.error()));
  }
  return Emit(
      unit_->Types().Intern(
          lir::Type{lir::PointerType{
              .pointee = type,
              .ownership = lir::PointerOwnership::kBorrowed,
              .mutability = lir::Mutability::kMutable}}),
      lir::AddrOfInstr{.place = *std::move(place)});
}

auto FunctionLowerer::LowerArguments(
    const mir::Block& block, std::span<const mir::ExprId> arguments)
    -> diag::Result<std::vector<lir::Operand>> {
  std::vector<lir::Operand> args;
  args.reserve(arguments.size());
  for (const mir::ExprId argument : arguments) {
    auto lowered = LowerArgument(block, argument);
    if (!lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
    args.push_back(*std::move(lowered));
  }
  return args;
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
  auto cell = LowerCellPlace(block, call.arguments[0]);
  if (!cell) {
    return std::unexpected(std::move(cell.error()));
  }
  return Emit(
      unit_->TranslateType(type), lir::AddrOfInstr{.place = *std::move(cell)});
}

auto FunctionLowerer::LowerCellPlace(
    const mir::Block& block, mir::ExprId referent) -> diag::Result<lir::Place> {
  const mir::Expr& expr = block.exprs.Get(referent);
  if (unit_->Mir().types.Get(expr.type).Is<mir::ObservableType>()) {
    return LowerPlace(block, referent);
  }
  if (const auto* local = std::get_if<mir::LocalRef>(&expr.data);
      local != nullptr && locals_[local->var.value].has_value()) {
    if (const auto* cell =
            std::get_if<CellBinding>(&*locals_[local->var.value])) {
      return ReferencedCell(cell->reference);
    }
  }
  // Every other referent holds its value somewhere a reference cannot name. A
  // suspending body's local lives in a cell of the execution's own store,
  // reached by that cell's calls rather than by an address; a member that is
  // not a signal
  // owns its value rather than a cell holding it; and a part of a value
  // aggregate is no independent storage at all.
  return Unsupported(
      "mir_to_lir: storage of this kind is not yet lendable by reference");
}

auto FunctionLowerer::LowerCall(
    const mir::Block& block, const mir::CallExpr& call, mir::TypeId type)
    -> diag::Result<lir::Operand> {
  // A receiver-mutating method on a value is not an in-place call: value
  // semantics forbid changing what a copy of the receiver would share, so it is
  // a functional operation whose result is stored back through the owner.
  if (const auto fn = mir::DirectBuiltinFn(call);
      fn.has_value() && support::IsMutatingBuiltinFn(*fn) &&
      !call.arguments.empty()) {
    return LowerMutatingCall(block, call, *fn, type);
  }

  // Reading what a wrapper holds and replacing it are operations on the
  // wrapper, which this target realizes through the storage the wrapper stands
  // for rather than through an entry of its own -- the same storage a write
  // reaching one part of it descends into, so the two cannot disagree about
  // where the contents live.
  if (const auto fn = mir::DirectBuiltinFn(call);
      fn == support::BuiltinFn::kLoad || fn == support::BuiltinFn::kStore) {
    auto place = WrapperContentsPlace(block, call.arguments.front());
    if (!place) {
      return std::unexpected(std::move(place.error()));
    }
    if (fn == support::BuiltinFn::kLoad) {
      return Load(*std::move(place), unit_->TranslateType(type));
    }
    auto value = LowerExpr(block, call.arguments.back());
    if (!value) {
      return std::unexpected(std::move(value.error()));
    }
    return Store(*std::move(place), *std::move(value));
  }

  // A reference is the address of the storage it binds, and reading or writing
  // through one reaches that storage directly. No runtime value stands between
  // the holder and the referent, so the three operations are the ordinary
  // address-of, load, and store over the referent's place.
  if (BindsReference(unit_->Mir().types, call, type)) {
    return LowerReferenceBind(block, call, type);
  }
  // Reached where nothing awaits the execution -- a process handed to the
  // scheduler. Such a body finishes with no value, so there is nothing for it
  // to complete into; the awaiting form supplies that storage itself.
  if (unit_->Mir().types.Get(type).Is<mir::CoroutineType>()) {
    return EnterCoroutine(block, call, type, std::nullopt);
  }

  auto args = LowerArguments(block, call.arguments);
  if (!args) {
    return std::unexpected(std::move(args.error()));
  }
  return EmitCall(call, *std::move(args), unit_->TranslateType(type));
}

auto FunctionLowerer::EnterCoroutine(
    const mir::Block& block, const mir::CallExpr& call, mir::TypeId type,
    std::optional<lir::Operand> completion) -> diag::Result<lir::Operand> {
  auto args = LowerArguments(block, call.arguments);
  if (!args) {
    return std::unexpected(std::move(args.error()));
  }
  if (completion.has_value()) {
    args->push_back(*std::move(completion));
  }
  const lir::TypeId result_type = unit_->TranslateType(type);

  // Calling a body whose result type states the coroutine protocol builds its
  // frame rather than running it: the arguments are placed and the body stops
  // before its first statement, so no code of it has run when the call returns.
  // Making an execution out of that frame is the separate step, and it borrows
  // the environment the body reads -- a receiver, which outlives every
  // execution reaching its members.
  auto frame = EmitCall(call, *std::move(args), result_type);
  if (!frame) {
    return std::unexpected(std::move(frame.error()));
  }
  return Emit(
      result_type,
      lir::CallInstr{
          .target =
              lir::CoroutineTarget{
                  .op = lir::CoroutineTarget::Op::kEnterBorrowedEnvironment},
          .args = {*std::move(frame)}});
}

auto FunctionLowerer::EmitCall(
    const mir::CallExpr& call, std::vector<lir::Operand> args,
    lir::TypeId result_type) -> diag::Result<lir::Operand> {
  auto target = LowerCallTarget(*unit_, call.callee, result_type);
  if (!target) {
    return std::unexpected(std::move(target.error()));
  }
  const lir::Operand result = Emit(
      result_type,
      lir::CallInstr{.target = *std::move(target), .args = std::move(args)});

  // Disabling a target the disabling execution is itself inside leaves it (LRM
  // 9.6.2), and the statement is where that execution next has control, so it
  // is one of the points a region's body asks at.
  if (const auto fn = DirectBuiltinFn(call);
      fn.has_value() && *fn == support::BuiltinFn::kDisable) {
    auto checked = CheckDisabledTarget();
    if (!checked) {
      return std::unexpected(std::move(checked.error()));
    }
  }
  return result;
}

auto FunctionLowerer::LowerRegistration(
    const mir::Block& block, const mir::CallExpr& call)
    -> diag::Result<lir::Operand> {
  auto args = LowerArguments(block, call.arguments);
  if (!args) {
    return std::unexpected(std::move(args.error()));
  }
  return EmitCall(call, *std::move(args), unit_->MachineBoolType());
}

auto FunctionLowerer::LowerCoroutineAwait(
    const mir::Block& block, const mir::AwaitExpr& await, mir::TypeId type)
    -> diag::Result<lir::Operand> {
  // Where the awaited body finishes with a value, this body digs the place for
  // it and hands that place over as the call's last argument -- so what it
  // reads back afterwards is its own storage, which is still there.
  std::optional<lir::Operand> completion_slot;
  if (type != unit_->Mir().builtins.void_type) {
    completion_slot = AllocateCompletionFor(unit_->TranslateType(type));
  }

  const mir::Expr& awaitable = block.exprs.Get(await.awaitable);
  const auto* direct = std::get_if<mir::CallExpr>(&awaitable.data);
  if (direct == nullptr && completion_slot.has_value()) {
    return Unsupported(
        "mir_to_lir: an awaited execution that completes with a value must be "
        "a call, so that the place to complete into can be handed to it");
  }
  auto activation =
      direct != nullptr
          ? EnterCoroutine(block, *direct, awaitable.type, completion_slot)
          : LowerExpr(block, await.awaitable);
  if (!activation) {
    return activation;
  }
  // Handing the thread over runs the awaited body at once (LRM 13.3), so one
  // that consumes no time has already settled when control comes back and
  // there is nothing left to wait for; the answer says which of the two
  // happened.
  const lir::Operand park = Emit(
      unit_->MachineBoolType(),
      lir::CallInstr{
          .target =
              lir::CoroutineTarget{.op = lir::CoroutineTarget::Op::kAwait},
          .args = {CurrentRuntime(), *std::move(activation)}});
  const lir::BlockId parked = NewBlock();
  const lir::BlockId resume = NewBlock();
  Terminate(
      lir::CondBranchTerm{
          .condition = park, .if_true = parked, .if_false = resume});
  SetCurrent(parked);
  Terminate(lir::SuspendTerm{.resume = resume});
  SetCurrent(resume);

  std::optional<lir::Operand> completion;
  if (completion_slot.has_value()) {
    completion =
        LoadActivationValue(*completion_slot, unit_->TranslateType(type));
  }
  // Taking the thread back ends the awaited execution, before anything else
  // this one does: what runs next may await again, and a thread carries one
  // awaited execution at a time.
  Emit(
      unit_->TranslateType(unit_->Mir().builtins.void_type),
      lir::CallInstr{
          .target =
              lir::CoroutineTarget{.op = lir::CoroutineTarget::Op::kRelease},
          .args = {CurrentRuntime()}});

  // Having the thread back is a point where this execution regains control, so
  // a target it is inside may have been disabled while it was away.
  auto checked = CheckDisabledTarget();
  if (!checked) {
    return std::unexpected(std::move(checked.error()));
  }
  // A completion that carries nothing yields nothing, so what stands here is
  // never read.
  return completion.value_or(park);
}

auto FunctionLowerer::LowerCompoundOperator(
    mir::BinaryOp op, lir::Operand old_value, lir::Operand rhs,
    lir::TypeId type) -> diag::Result<lir::Operand> {
  if (const std::optional<lir::BinaryOp> binop = TranslateBinaryOp(op)) {
    return Emit(
        type,
        lir::BinaryInstr{
            .op = *binop, .lhs = std::move(old_value), .rhs = std::move(rhs)});
  }
  // A builtin operator's domain rides on its first operand, so the target
  // carries no qualifier and the entry resolves to the same call an expression
  // of the operator lowers to.
  if (const std::optional<support::BuiltinFn> fn =
          mir::BinaryOpAsBuiltinFn(op)) {
    return Emit(
        type,
        lir::CallInstr{
            .target = lir::BuiltinTarget{.fn = *fn, .qualifier = std::nullopt},
            .args = {std::move(old_value), std::move(rhs)}});
  }
  return Unsupported(
      "mir_to_lir: compound assignment operator has no direct realization");
}

auto FunctionLowerer::LowerAssign(
    const mir::Block& block, const mir::AssignExpr& assign)
    -> diag::Result<lir::Operand> {
  // A target that reaches into a value aggregate -- a product component, a
  // value-container element, or any composition of them -- is not a place here:
  // the aggregate crosses as an opaque handle a copy may alias, so the write is
  // a functional whole-value update stored back through whatever owns it. What
  // the update stores is the owner's whole value; the assignment's own value is
  // the part it wrote.
  if (ReachesIntoValue(block, assign.target)) {
    std::optional<lir::Operand> assigned;
    auto written = LowerValuePartUpdate(
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
          auto combined = LowerCompoundOperator(
              *assign.compound_op, read_leaf(), *std::move(rhs), leaf_type);
          if (!combined) {
            return std::unexpected(std::move(combined.error()));
          }
          assigned = *std::move(combined);
          return *assigned;
        });
    if (!written) {
      return std::unexpected(std::move(written.error()));
    }
    return *assigned;
  }

  const mir::TypeId target_type = block.exprs.Get(assign.target).type;
  const lir::TypeId type = unit_->TranslateType(target_type);

  // An activation value is written through its handle, not a place: a compound
  // assignment reads the old value out of the cell, combines, and overwrites.
  if (auto handle = ActivationValueHandleForTarget(block, assign.target)) {
    auto value = LowerExpr(block, assign.value);
    if (!value) {
      return std::unexpected(std::move(value.error()));
    }
    lir::Operand written = *value;
    if (assign.compound_op.has_value()) {
      auto combined = LowerCompoundOperator(
          *assign.compound_op, LoadActivationValue(*handle, type),
          *std::move(value), type);
      if (!combined) {
        return std::unexpected(std::move(combined.error()));
      }
      written = *std::move(combined);
    }
    StoreActivationValue(*handle, written, type);
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
    auto combined = LowerCompoundOperator(
        *assign.compound_op, Load(*place, type), *std::move(value), type);
    if (!combined) {
      return std::unexpected(std::move(combined.error()));
    }
    written = *std::move(combined);
  }
  Store(*std::move(place), written);
  return written;
}

auto FunctionLowerer::WriteWholeValue(
    const mir::Block& block, mir::ExprId id, lir::Operand value)
    -> diag::Result<lir::Operand> {
  if (const std::optional<lir::Operand> handle =
          ActivationValueHandleForTarget(block, id)) {
    return StoreActivationValue(
        *handle, std::move(value),
        unit_->TranslateType(block.exprs.Get(id).type));
  }
  auto place = LowerPlace(block, id);
  if (!place) {
    return std::unexpected(std::move(place.error()));
  }
  return Store(*std::move(place), std::move(value));
}

auto FunctionLowerer::LowerValuePartSelector(
    const mir::Block& block, mir::ExprId step)
    -> diag::Result<lir::AggregateSelector> {
  const mir::ExprData& data = block.exprs.Get(step).data;
  if (const auto* field = std::get_if<mir::FieldAccessExpr>(&data)) {
    return lir::AggregateSelector{lir::Component{
        .index = std::get<mir::ComponentTarget>(field->field).index}};
  }
  const auto& call = std::get<mir::CallExpr>(data);
  std::vector<lir::Operand> operands;
  operands.reserve(call.arguments.size() - 1);
  for (std::size_t i = 1; i < call.arguments.size(); ++i) {
    auto operand = LowerExpr(block, call.arguments[i]);
    if (!operand) {
      return std::unexpected(std::move(operand.error()));
    }
    operands.push_back(*std::move(operand));
  }
  if (mir::DirectBuiltinFn(call) == support::BuiltinFn::kSliceRef) {
    return lir::AggregateSelector{
        lir::ContainerSlice{.operands = std::move(operands)}};
  }
  return lir::AggregateSelector{
      lir::ContainerElement{.operands = std::move(operands)}};
}

auto FunctionLowerer::LowerValuePartUpdate(
    const mir::Block& block, mir::ExprId target, const LeafTransform& make_leaf)
    -> diag::Result<lir::Operand> {
  // The steps the write descends, outermost first, and the owner they bottom
  // out in. Composition is the operand chain, so the walk is the path.
  std::vector<mir::ExprId> steps;
  mir::ExprId owner = target;
  while (ReachesIntoValue(block, owner)) {
    steps.push_back(owner);
    owner = ValuePartReceiver(block, owner);
  }
  std::ranges::reverse(steps);

  auto owner_value = LowerExpr(block, owner);
  if (!owner_value) {
    return std::unexpected(std::move(owner_value.error()));
  }
  std::vector<lir::AggregateSelector> selectors;
  selectors.reserve(steps.size());
  for (const mir::ExprId step : steps) {
    auto selector = LowerValuePartSelector(block, step);
    if (!selector) {
      return std::unexpected(std::move(selector.error()));
    }
    selectors.push_back(*std::move(selector));
  }

  // The whole value at each level, descending from the owner toward the part.
  std::vector<lir::Operand> containers;
  containers.reserve(steps.size());
  containers.push_back(*std::move(owner_value));
  for (std::size_t depth = 1; depth < steps.size(); ++depth) {
    containers.push_back(Emit(
        unit_->TranslateType(block.exprs.Get(steps[depth - 1]).type),
        lir::AggregateExtractInstr{
            .aggregate = containers[depth - 1],
            .selector = selectors[depth - 1]}));
  }

  const std::size_t leaf = steps.size() - 1;
  const lir::TypeId leaf_type =
      unit_->TranslateType(block.exprs.Get(target).type);
  auto leaf_value = make_leaf(
      [&] {
        return Emit(
            leaf_type,
            lir::AggregateExtractInstr{
                .aggregate = containers[leaf], .selector = selectors[leaf]});
      },
      leaf_type);
  if (!leaf_value) {
    return std::unexpected(std::move(leaf_value.error()));
  }

  // Rebuild the whole value from the written part outward.
  lir::Operand rebuilt = *std::move(leaf_value);
  for (std::size_t depth = steps.size(); depth-- > 0;) {
    rebuilt = Emit(
        unit_->TranslateType(
            block.exprs.Get(ValuePartReceiver(block, steps[depth])).type),
        lir::AggregateUpdateInstr{
            .aggregate = containers[depth],
            .selector = selectors[depth],
            .replacement = std::move(rebuilt)});
  }
  return WriteWholeValue(block, owner, std::move(rebuilt));
}

// A value handle is immutable from the generated side, so a method that
// appears to mutate its receiver is realized as a functional operation whose
// result is stored back through the receiver's owner. Where the method also
// states a result of its own -- a queue pop yields the element it removed (LRM
// 7.10.2.4) -- the entry completes with both, the updated receiver first, and
// each is projected out of that product.
auto FunctionLowerer::LowerMutatingCall(
    const mir::Block& block, const mir::CallExpr& call, support::BuiltinFn fn,
    mir::TypeId type) -> diag::Result<lir::Operand> {
  const mir::ExprId receiver = call.arguments[0];
  const lir::TypeId container_type =
      unit_->TranslateType(block.exprs.Get(receiver).type);
  const bool yields_result = type != unit_->Mir().builtins.void_type;
  const lir::TypeId call_type =
      yields_result
          ? unit_->ProductOf({container_type, unit_->TranslateType(type)})
          : container_type;

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

  lir::Operand completion = Emit(
      call_type,
      lir::CallInstr{
          .target = lir::BuiltinTarget{.fn = fn, .qualifier = std::nullopt},
          .args = std::move(args)});
  if (!yields_result) {
    return WriteWholeValue(block, receiver, std::move(completion));
  }

  lir::Operand updated = Emit(
      container_type,
      lir::AggregateExtractInstr{
          .aggregate = completion,
          .selector = lir::Component{.index = kUpdatedReceiver}});
  auto stored = WriteWholeValue(block, receiver, std::move(updated));
  if (!stored) {
    return std::unexpected(std::move(stored.error()));
  }
  return Emit(
      unit_->TranslateType(type),
      lir::AggregateExtractInstr{
          .aggregate = std::move(completion),
          .selector = lir::Component{.index = kMutatingCallResult}});
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

  // Stepping part of a value aggregate reads the part out of the owner's whole
  // value, steps it, and folds it back in. What was stored is the owner's whole
  // value; the statement's own value is the part, before or after the step.
  if (ReachesIntoValue(block, inc_dec.target)) {
    std::optional<lir::Operand> old;
    std::optional<lir::Operand> stepped;
    auto written = LowerValuePartUpdate(
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
    return is_prefix ? *stepped : *old;
  }

  // An activation value increments through its handle: read the old value out,
  // apply the step, overwrite.
  if (auto handle = ActivationValueHandleForTarget(block, inc_dec.target)) {
    const lir::Operand old = LoadActivationValue(*handle, type);
    const lir::Operand updated =
        Emit(type, lir::UnaryInstr{.op = op, .operand = old});
    StoreActivationValue(*handle, updated, type);
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
          [&](const mir::StringLiteral& lit) -> diag::Result<lir::Operand> {
            return lir::Operand{lir::StrConst{
                .value = lit.value, .type = unit_->TranslateType(type)}};
          },
          [&](const mir::MachineFloatLiteral& lit)
              -> diag::Result<lir::Operand> {
            return lir::Operand{lir::RealConst{
                .value = lit.value, .type = unit_->TranslateType(type)}};
          },
          [&](const mir::NullLiteral&) -> diag::Result<lir::Operand> {
            return lir::Operand{
                lir::NullConst{.type = unit_->TranslateType(type)}};
          },
          [&](const mir::MachineBoolLiteral& lit)
              -> diag::Result<lir::Operand> {
            return lir::Operand{lir::BoolConst{
                .value = lit.value, .type = unit_->TranslateType(type)}};
          },
          [&](const mir::PackedTypeRef& ref) -> diag::Result<lir::Operand> {
            return lir::Operand{lir::PackedTypeRef{
                .integral = unit_->TranslateType(ref.integral),
                .type = unit_->TranslateType(type)}};
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
                    [&](const ActivationValueBinding& frame)
                        -> diag::Result<lir::Operand> {
                      return LoadActivationValue(
                          frame.handle, unit_->TranslateType(type));
                    },
                    [&](const CellBinding& cell) -> diag::Result<lir::Operand> {
                      return Load(
                          ReferencedValue(cell.reference),
                          unit_->TranslateType(type));
                    }},
                *binding);
          },
          [&](const mir::CallExpr& call) -> diag::Result<lir::Operand> {
            return LowerCall(block, call, type);
          },
          [&](const mir::ValueCastExpr& cast) -> diag::Result<lir::Operand> {
            auto operand = LowerExpr(block, cast.operand);
            if (!operand) {
              return std::unexpected(std::move(operand.error()));
            }
            return Emit(
                unit_->TranslateType(type),
                lir::ValueCastInstr{.operand = *std::move(operand)});
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
          [&](const mir::ClosureExpr& cl) -> diag::Result<lir::Operand> {
            // Constructing a closure builds the storage its captures live in,
            // and nothing else: which body a call runs is already fixed by the
            // type, so no code identity is stored alongside them. Initializers
            // are evaluated in the order they are listed -- their
            // source-semantic order -- and each lands at the capture it
            // targets, since the two orders need not agree.
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
            const lir::TypeId closure_type =
                unit_->ClosureValueType(cl.closure);
            const lir::Operand value = Emit(
                closure_type,
                lir::CallInstr{
                    .target = lir::ConstructTarget{.result = closure_type},
                    .args = std::move(captures)});
            if (!unit_->Mir().types.Get(type).Is<mir::CoroutineType>()) {
              return value;
            }
            // A closure whose invoke completes as a coroutine is entered
            // through that protocol, so what the expression yields is the
            // coroutine rather than the callable value. The captures stay the
            // environment it reads, and entering takes them, because they
            // outlive nothing on their own and the body runs after the stretch
            // that built them has returned (LRM 9.3.2).
            return Emit(
                unit_->TranslateType(type),
                lir::CallInstr{
                    .target =
                        lir::CoroutineTarget{
                            .op = lir::CoroutineTarget::Op::
                                kEnterOwnedEnvironment},
                    .args = {value}});
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
          // A field in a storage arena is read through its place. A structural
          // product's field is not addressable, so it is extracted from the
          // product's value instead -- the same read either way, over a
          // receiver that is storage in one case and a value in the other.
          [&](const mir::FieldAccessExpr& field) -> diag::Result<lir::Operand> {
            const auto* component =
                std::get_if<mir::ComponentTarget>(&field.field);
            if (component == nullptr) {
              return ReadPlace(block, id, unit_->TranslateType(type));
            }
            auto receiver = LowerExpr(block, field.receiver);
            if (!receiver) {
              return receiver;
            }
            return Emit(
                unit_->TranslateType(type),
                lir::AggregateExtractInstr{
                    .aggregate = *std::move(receiver),
                    .selector = lir::Component{.index = component->index}});
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
          [&](const mir::BlockExpr& be) -> diag::Result<lir::Operand> {
            // The steps run where they were written, so they lower into the
            // block being built, and the value the last one names is what the
            // expression yields.
            const mir::Block& scope = block.child_scopes.Get(be.scope);
            auto lowered = LowerBlockInto(scope);
            if (!lowered) {
              return std::unexpected(std::move(lowered.error()));
            }
            return LowerExpr(scope, be.value);
          },
          [&](const mir::MoveExpr& m) -> diag::Result<lir::Operand> {
            // A move is a last-use transfer marker placed at HIR-to-MIR; it
            // changes neither the value nor its type, so it unwraps to its
            // operand here. Whether the transfer is realized as a move or a
            // copy is decided below LIR, not at this layer.
            return LowerExpr(block, m.operand);
          },
          [&](const mir::AwaitExpr& await) -> diag::Result<lir::Operand> {
            // An awaitable arranges this execution's resumption through an
            // ordinary runtime call and answers whether it must park; where it
            // must, a control edge hands control back to the scheduler, which
            // resumes at the next block. A delay, an event control, and a join
            // differ only in that call.
            //
            // An awaitable whose type is a coroutine is the other protocol: it
            // registers nothing, because its completion is the awaited body's
            // to signal, so what it waits for is that body reaching its end.
            const mir::Expr& awaitable = block.exprs.Get(await.awaitable);
            if (unit_->Mir()
                    .types.Get(awaitable.type)
                    .Is<mir::CoroutineType>()) {
              return LowerCoroutineAwait(block, await, type);
            }
            if (type != unit_->Mir().builtins.void_type) {
              return Unsupported(
                  "mir_to_lir: a value-carrying await is not yet lowerable to "
                  "LIR");
            }
            const auto* registration =
                std::get_if<mir::CallExpr>(&awaitable.data);
            if (registration == nullptr) {
              return Unsupported(
                  "mir_to_lir: an awaitable that is not a registration call is "
                  "not yet lowerable to LIR");
            }
            auto park = LowerRegistration(block, *registration);
            if (!park) {
              return std::unexpected(std::move(park.error()));
            }
            const lir::BlockId parked = NewBlock();
            const lir::BlockId resume = NewBlock();
            Terminate(
                lir::CondBranchTerm{
                    .condition = *park, .if_true = parked, .if_false = resume});
            SetCurrent(parked);
            Terminate(lir::SuspendTerm{.resume = resume});
            SetCurrent(resume);
            // Resuming is a point where this execution regains control, so a
            // target it is inside may have been disabled while it was away.
            auto checked = CheckDisabledTarget();
            if (!checked) {
              return std::unexpected(std::move(checked.error()));
            }
            // An await of nothing yields nothing, so what stands here is never
            // read.
            return *park;
          },
          [&](const mir::UnionExpr& u) -> diag::Result<lir::Operand> {
            auto value = LowerExpr(block, u.value);
            if (!value) {
              return value;
            }
            return Emit(
                unit_->TranslateType(type),
                lir::UnionInstr{.index = u.index, .value = *std::move(value)});
          },
          [&](const mir::TaggedExpr& t) -> diag::Result<lir::Operand> {
            auto payload = LowerExpr(block, t.payload);
            if (!payload) {
              return payload;
            }
            return Emit(
                unit_->TranslateType(type),
                lir::UnionInstr{
                    .index = t.tag_index, .value = *std::move(payload)});
          },
          [&](const mir::TaggedIsExpr& g) -> diag::Result<lir::Operand> {
            // The non-throwing guard a pattern match tests: whether the value's
            // active tag is the one the pattern names (LRM 12.6). The runtime
            // holds the comparison, so this is one predicate, not a tag read
            // and a compare against a constant.
            auto union_value = LowerExpr(block, g.union_value);
            if (!union_value) {
              return union_value;
            }
            return Emit(
                unit_->MachineBoolType(),
                lir::TagTestInstr{
                    .aggregate = *std::move(union_value),
                    .index = g.tag_index});
          },
          [](const mir::FunctionCastExpr&) -> diag::Result<lir::Operand> {
            return Unsupported(
                "mir_to_lir: naming a code address as another function type is "
                "not yet lowerable to LIR");
          },
          [](const mir::FunctionRef&) -> diag::Result<lir::Operand> {
            return Unsupported(
                "mir_to_lir: a code address as a value is not yet lowerable to "
                "LIR");
          },
          [&](const mir::ExternalUnitVariableRef&)
              -> diag::Result<lir::Operand> {
            return ReadPlace(block, id, unit_->TranslateType(type));
          },
          // Each of the following names storage rather than reaching it
          // through a receiver, and nothing yet builds a cell for a class's
          // type-associated declarations, so there is no place to read one
          // from.
          [](const mir::StaticConstantRef&) -> diag::Result<lir::Operand> {
            return Unsupported(
                "mir_to_lir: a class's static constant is not yet reachable on "
                "this backend");
          },
          [](const mir::StaticPropertyRef&) -> diag::Result<lir::Operand> {
            return Unsupported(
                "mir_to_lir: a class's static property is not yet reachable on "
                "this backend");
          },
          [](const mir::ExternalStaticPropertyRef&)
              -> diag::Result<lir::Operand> {
            return Unsupported(
                "mir_to_lir: a static property of a class another compilation "
                "unit declares is not yet reachable on this backend");
          }},
      expr.data);
}

}  // namespace lyra::lowering::mir_to_lir
