#include "lyra/lowering/mir_to_lir/function_lowerer.hpp"

#include <cstddef>
#include <cstdint>
#include <optional>
#include <ranges>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/component_index.hpp"
#include "lyra/base/diag_message.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/failure_context.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/integral_constant.hpp"
#include "lyra/lir/operator.hpp"
#include "lyra/lir/place_query.hpp"
#include "lyra/lir/symbol_name.hpp"
#include "lyra/lir/type_builders.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/lir/value_step.hpp"
#include "lyra/lowering/mir_to_lir/unit_lowerer.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/storage_part.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_declaration_ref.hpp"
#include "lyra/mir/unary_op.hpp"
#include "lyra/profiling/time_trace.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::mir_to_lir {

namespace {

// Whether a call ending this way can be unwound out of, which is what obliges
// it to name the landing unwinding reaches.
auto MayUnwind(support::CallEnding ending) -> bool {
  switch (ending) {
    case support::CallEnding::kReturns:
      return false;
    case support::CallEnding::kReturnsOrUnwinds:
    case support::CallEnding::kUnwinds:
      return true;
  }
  throw InternalError("mir_to_lir: unknown call ending");
}

template <typename... Args>
auto Unsupported(base::DiagFormatFor<Args...> format, Args&&... args)
    -> std::unexpected<diag::Diagnostic> {
  return diag::Fail(
      diag::DiagCode::kUnsupportedExpressionForm, format,
      std::forward<Args>(args)...);
}

// Which build assembles a composite value follows from the storage its type
// names, which is what translating the type has just settled: a product's
// components each keep a type of their own, while an element list is one
// element type laid down a known number of times. A type reaching here that
// names neither is a producer that built a composite of something that is not
// composed from parts.
auto AssembledFrom(
    const lir::CompilationUnit& unit, lir::TypeId built,
    std::vector<lir::Operand> parts) -> lir::Rvalue {
  if (lir::ProductElements(unit, built).has_value()) {
    return lir::ProductInstr{.components = std::move(parts)};
  }
  if (unit.types.Get(built).Is<lir::MachineArrayType>()) {
    return lir::ArrayInstr{.elements = std::move(parts)};
  }
  throw InternalError(
      "mir_to_lir: a composite's type names storage that is not assembled "
      "from parts");
}

// The position a call names, where the part it names is a component of a
// product. Only a reference to a property names a property, and that is
// lowered as a step on its object
// before any entry is called, so one reaching here is a producer that named
// the wrong kind of part.
auto ComponentPartOf(const mir::Direct& direct)
    -> std::optional<base::ComponentIndex> {
  const auto names_a_property = []() -> base::ComponentIndex {
    throw InternalError(
        "mir_to_lir: an entry naming a property is a reference to it, taken as "
        "a step on its object -- please report this as a bug");
  };
  return direct.part.transform([&](const mir::CallPart& part) {
    return std::visit(
        Overloaded{
            [](base::ComponentIndex position) { return position; },
            [&](const mir::ClassFieldTarget&) { return names_a_property(); }},
        part);
  });
}

// The place a place local names: its own storage, with nothing projected off
// it.
auto LocalPlace(lir::ValueId local) -> lir::Place {
  return lir::Place{.base = lir::Use{.value = local}, .chain = {}};
}

// Whether this call opens a write into an object. The write is a library
// object the runtime builds over the object, so a construction of one is a
// call naming that object rather than an assembly of parts.
auto OpensObjectWrite(
    const mir::TypePool& types, const mir::CallExpr& call,
    mir::TypeId result_type) -> bool {
  return std::holds_alternative<mir::Construct>(call.callee) &&
         types.Get(result_type).Is<mir::ObjectWriteType>();
}

// The operators the executable IR realizes directly, which is every operator a
// MIR node carries: one a library performs is lifted to a call where the node
// would have been built, so none reaches here.
auto TranslateBinaryOp(mir::BinaryOp op) -> lir::BinaryOp {
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
  }
  throw InternalError("TranslateBinaryOp: unknown MIR BinaryOp");
}

auto TranslateUnaryOp(mir::UnaryOp op) -> lir::UnaryOp {
  switch (op) {
    case mir::UnaryOp::kMinus:
      return lir::UnaryOp::kMinus;
    case mir::UnaryOp::kBitwiseNot:
      return lir::UnaryOp::kBitwiseNot;
    case mir::UnaryOp::kLogicalNot:
      return lir::UnaryOp::kLogicalNot;
  }
  throw InternalError("TranslateUnaryOp: unknown MIR UnaryOp");
}

// What a dump of this function shows beside one of its values. A local the
// source declared shows the identifier the design wrote; one the lowering added
// shows nothing, and the value's own number is what a reader has. It is a
// label: nothing resolves a name to a value at this layer, and no symbol is
// composed from it.
auto LocalLabel(const mir::CallableCode& code, mir::LocalId local)
    -> std::string {
  const std::optional<std::string_view> name =
      mir::NameOf(code.named_locals, local);
  return name.has_value() ? std::string{*name} : std::string{};
}

// Whether a local is a cell as MIR declared it -- a variable the source
// declared, which reports its writes -- rather than a value the body keeps.
auto IsDeclaredCell(const mir::CompilationUnit& unit, mir::TypeId declared)
    -> bool {
  return unit.types.Get(declared).Is<mir::ObservableType>();
}

// The value a local's storage holds: what its cell holds where MIR declared
// one, and otherwise the local's own type.
auto StoredValueType(const mir::CompilationUnit& unit, mir::TypeId declared)
    -> mir::TypeId {
  if (const auto* cell = unit.types.Get(declared).As<mir::ObservableType>()) {
    return cell->value;
  }
  return declared;
}

}  // namespace

auto FunctionLowerer::LowerCallTarget(
    const mir::Block& block, const mir::Callee& callee)
    -> diag::Result<lir::CallTarget> {
  return std::visit(
      Overloaded{
          [&](const mir::Direct& d) -> diag::Result<lir::CallTarget> {
            return std::visit(
                Overloaded{
                    [&](const mir::CallableTarget& t)
                        -> diag::Result<lir::CallTarget> {
                      return unit_->CallableCallTarget(t.owner, t.slot);
                    },
                    [&](const mir::ClosureTarget& t)
                        -> diag::Result<lir::CallTarget> {
                      return lir::CallTarget{lir::FunctionTarget{
                          .function = unit_->ClosureFunction(t.closure)}};
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
                      return lir::CallTarget{lir::BuiltinTarget{
                          .fn = fn,
                          .position = ComponentPartOf(d),
                          .type_argument =
                              d.type_arguments.empty()
                                  ? std::nullopt
                                  : std::optional{unit_->TranslateType(
                                        d.type_arguments.front())}}};
                    },
                    [&](const mir::UnitCallableTarget& t)
                        -> diag::Result<lir::CallTarget> {
                      // A body of a unit's namespace is reached by the symbol
                      // the unit owning it emits it under.
                      return lir::CallTarget{lir::SymbolTarget{
                          .symbol = unit_->UnitCallableSymbol(t.slot)}};
                    },
                    // A base's constructor, entered on the object the call is
                    // made on, as a `new` enters the class's own.
                    [&](const mir::ConstructorTarget& t)
                        -> diag::Result<lir::CallTarget> {
                      return ConstructorOf(t.cls).callee;
                    },
                    // Reached by the symbol its declaring unit emits it under,
                    // from that unit and from any other alike.
                    [&](const mir::RecordMethodTarget& t)
                        -> diag::Result<lir::CallTarget> {
                      const mir::TypeDeclarationRef declaration =
                          mir::RecordDeclarationOf(unit_->Mir(), t.record);
                      return lir::CallTarget{lir::SymbolTarget{
                          .symbol = lir::RecordMethodSymbol(
                              declaration.unit_name, declaration.path,
                              t.answers)}};
                    },
                    // A body this unit states for the type, which no other
                    // unit names.
                    [&](const mir::TypeOperationTarget& t)
                        -> diag::Result<lir::CallTarget> {
                      return lir::CallTarget{lir::FunctionTarget{
                          .function =
                              unit_->TypeOperationFunction(t.type, t.answers)}};
                    },
                    [&](const mir::ExternalBringUpTarget& t)
                        -> diag::Result<lir::CallTarget> {
                      // It answers to no name, so its symbol is composed from
                      // the unit and which of them it is -- the same parts the
                      // unit that defines it composes.
                      return lir::CallTarget{lir::SymbolTarget{
                          .symbol = BringUpEntrySymbol(t.unit_name, t.entry)}};
                    }},
                d.target);
          },
          [](const mir::Construct&) -> diag::Result<lir::CallTarget> {
            return lir::CallTarget{lir::ConstructTarget{}};
          },
          [&](const mir::Indirect& i) -> diag::Result<lir::CallTarget> {
            auto code = LowerExpr(block, i.code);
            if (!code) {
              return std::unexpected(std::move(code.error()));
            }
            return lir::CallTarget{
                lir::IndirectTarget{.callee = *std::move(code)}};
          },
          // Which body runs is the receiving value's to decide, so what the
          // call states is where to look rather than what to call. The behavior
          // the layer above names becomes a coordinate here -- the declaration
          // that introduced it, and which of that declaration's introductions
          // it is (LRM 8.20) -- and where that lands in a value is settled with
          // the whole lineage in hand, which is nowhere near this pass. A
          // behavior an interface class introduced is found instead among the
          // tables of the interface classes the object's class implements (LRM
          // 8.26.5), as Swift's `witness_method` stands beside its
          // `class_method`. The receiver is already the call's first argument,
          // as it is for a method named outright.
          [&](const mir::Virtual& v) -> diag::Result<lir::CallTarget> {
            if (unit_->Mir().GetClass(v.slot.owner_class).is_interface_class) {
              return lir::CallTarget{lir::InterfaceDispatchTarget{
                  .requirement = unit_->SlotRef(v.slot)}};
            }
            return lir::CallTarget{
                lir::DispatchTarget{.method = unit_->SlotRef(v.slot)}};
          }},
      callee);
}

FunctionLowerer::FunctionLowerer(
    UnitLowerer& unit, const mir::CallableCode& code, std::string name)
    : unit_(&unit),
      code_(&code),
      closure_(nullptr),
      name_(std::move(name)),
      variable_slot_(code.locals.size(), std::nullopt),
      locals_(code.locals.size(), std::nullopt) {
}

FunctionLowerer::FunctionLowerer(
    UnitLowerer& unit, const mir::ClosureDecl& closure, std::string name)
    : unit_(&unit),
      code_(&closure.invoke),
      closure_(&closure),
      name_(std::move(name)),
      variable_slot_(closure.invoke.locals.size(), std::nullopt),
      locals_(closure.invoke.locals.size(), std::nullopt) {
}

auto FunctionLowerer::TakeParam(mir::LocalId param) -> lir::ValueId {
  const lir::TypeId type = unit_->TranslateType(code_->locals.Get(param).type);
  const lir::ValueId value = fn_.values.Add(
      lir::Local{
          .name = LocalLabel(*code_, param),
          .type = type,
          .kind = lir::LocalKind::kParam});
  fn_.params.push_back(value);
  // An argument with no copy is handed over rather than lent, so the body
  // owns it from here.
  if (unit_->Types().Get(type).IsMoveOnly()) {
    OpenScope(ValueEnd{.value = value});
  }
  return value;
}

void FunctionLowerer::BindParam(mir::LocalId param, lir::ValueId argument) {
  // A parameter is a declared local whose first write is the incoming
  // argument, so it binds exactly as any declaration does: a parameter the body
  // gave storage of its own begins there, and any other is a frame slot. An
  // argument the caller only lends is copied into a slot owning its value,
  // which the body may then write (LRM 13.5.1) and ends on its way out.
  if (variable_slot_[param.value].has_value()) {
    InitializeCell(BeginVariable(param), lir::Use{.value = argument});
    return;
  }
  const lir::TypeId type = fn_.values.Get(argument).type;
  const lir::ValueId slot = OpenLocalSlot(param, type);
  AssignFrom(LocalPlace(slot), lir::Use{.value = argument});
  if (unit_->Types().Get(type).IsOwnedValue()) {
    OpenScope(SlotEnd{.slot = slot});
  }
}

auto FunctionLowerer::EmitInitialSuspend() -> diag::Result<void> {
  const lir::BlockId first = NewBlock();
  const lir::BlockId abandoned = NewBlock();
  Terminate(lir::SuspendTerm{.resume = first, .abandoned = abandoned});
  SetCurrent(abandoned);
  auto ended = RunCleanupsDownTo(0);
  if (!ended) {
    return std::unexpected(std::move(ended.error()));
  }
  Terminate(lir::AbandonTerm{});
  SetCurrent(first);
  stretch_start_ = first;
  return {};
}

auto FunctionLowerer::Run() -> diag::Result<lir::Function> {
  fn_.name = std::move(name_);
  const auto in_function = diag::FailureContext::InFunction(fn_.name);
  const profiling::TimeTraceScope span(
      "lower function", [&] { return fn_.name; });
  // A coroutine-bodied callable keeps its coroutine result type: coroutine-ness
  // is the call protocol carried by the type, so a backend realizes suspension
  // and completion from the type, never from a separate flag.
  fn_.result_type = unit_->TranslateType(code_->result_type);
  const bool is_coroutine =
      unit_->Mir().types.Get(code_->result_type).Is<mir::CoroutineType>();
  const bool answers_nothing =
      fn_.result_type == unit_->TranslateType(unit_->Mir().builtins.void_type);
  if (!is_coroutine && !answers_nothing) {
    fn_.return_place = fn_.values.Add(
        lir::Local{
            .name = "return",
            .type = fn_.result_type,
            .kind = lir::LocalKind::kPlace});
  }

  // The body's variables, in declaration order. A declaration is what gives a
  // variable storage, so nothing about what the body does with one is read
  // here -- not whether anything writes it, not whether anything binds a
  // second name to it. A variable whose value is more than one quantity held
  // entire gets storage of its own there, which a reference can bind and
  // which survives a suspension, and so does a cell MIR declared, which is a
  // variable the source declared; any other stays a value of the body.
  for (const mir::LocalId local : code_->locals.Ids()) {
    const mir::TypeId declared = code_->locals.Get(local).type;
    if (!IsDeclaredCell(unit_->Mir(), declared) &&
        !unit_->Mir().types.Get(declared).IsRuntimeStoredValue()) {
      continue;
    }
    variable_slot_[local.value] =
        static_cast<std::uint32_t>(fn_.variables.size());
    fn_.variables.push_back(
        lir::CellOf(
            unit_->Types(),
            unit_->TranslateType(StoredValueType(unit_->Mir(), declared))));
  }

  // The entry block exists first, so the arguments are taken there, ahead of
  // the body. A closure invoke's receiver names the storage its captures live
  // in, and leads the per-invocation parameters in the signature; a body that
  // takes its closure over owns the box as it owns any argument handed over.
  stretch_start_ = NewBlock();
  SetCurrent(stretch_start_);
  std::vector<std::pair<mir::LocalId, lir::ValueId>> taken;
  if (closure_ != nullptr) {
    if (!code_->receiver.has_value()) {
      throw InternalError(
          "mir_to_lir: a closure's body reads its captures through the "
          "closure, yet states no receiver");
    }
    taken.emplace_back(*code_->receiver, TakeParam(*code_->receiver));
  }
  for (const mir::LocalId param : code_->params) {
    taken.emplace_back(param, TakeParam(param));
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
            .type = AddressType(payload),
            .kind = lir::LocalKind::kParam});
    fn_.params.push_back(slot);
    completion_cell_ =
        CompletionCell{.cell = lir::Use{.value = slot}, .type = payload};
  }

  // Everything from here runs as a stretch of the body: the storage its
  // variables live in belongs to whoever drives it, and so does each
  // parameter's binding.
  std::optional<RegionScope> top;
  if (is_coroutine) {
    auto suspended = EmitInitialSuspend();
    if (!suspended) {
      return std::unexpected(std::move(suspended.error()));
    }
    // The execution a coroutine's body is the top of takes whatever unwinds
    // out of the body and completes with it, so the whole body stands in a
    // region of its own, as a C++ coroutine's body stands in a `try` whose
    // handler hands what escaped to its promise.
    top = RegionScope{
        .handler = NewBlock(),
        .caught = NewPlaceLocal(unit_->ControlEffectType())};
    OpenScope(*top);
  }
  ReserveVariableStorage();
  for (const auto& [param, argument] : taken) {
    BindParam(param, argument);
  }

  auto lowered = LowerBlockInto(code_->Body());
  if (!lowered) {
    return std::unexpected(std::move(lowered.error()));
  }
  // What the frame itself owes -- the parameters' own values -- ends where
  // control falls off the body, as on every other way out.
  auto frame_closed = CloseExtent(0);
  if (!frame_closed) {
    return std::unexpected(std::move(frame_closed.error()));
  }
  if (top.has_value()) {
    auto completed = EmitCoroutineHandler(*top);
    if (!completed) {
      return std::unexpected(std::move(completed.error()));
    }
  }

  // A block the lowering left open either falls off the end of a void or
  // coroutine body -- an implicit return -- or is a join control never reaches.
  // A value-returning body reaches its returns explicitly, so its open blocks
  // are the latter.
  const bool falls_through_to_return = is_coroutine || answers_nothing;
  fn_.blocks.reserve(blocks_.size());
  for (OpenBlock& block : blocks_) {
    if (!block.terminator.has_value()) {
      block.terminator = lir::Terminator{
          .data = falls_through_to_return
                      ? lir::TerminatorData{lir::ReturnTerm{}}
                      : lir::TerminatorData{lir::UnreachableTerm{}}};
    }
    fn_.blocks.push_back(
        lir::BasicBlock{
            .instrs = std::move(block.instrs),
            .terminator = *std::move(block.terminator)});
  }
  return std::move(fn_);
}

auto FunctionLowerer::ConstructorOf(mir::ClassId cls) -> EnteredConstructor {
  return EnteredConstructor{
      .object_type = unit_->ClassValueType(cls),
      .callee = unit_->ConstructorCallTarget(cls)};
}

auto FunctionLowerer::UnwindingType() -> lir::TypeId {
  return AddressType(unit_->TranslateType(unit_->Mir().builtins.void_type));
}

auto FunctionLowerer::ExceptionSlot() -> lir::ValueId {
  if (!exception_slot_.has_value()) {
    exception_slot_ = NewPlaceLocal(UnwindingType());
  }
  return *exception_slot_;
}

auto FunctionLowerer::LandingHere(std::optional<lir::ValueId> unmade)
    -> diag::Result<lir::BlockId> {
  const diag::Result<lir::BlockId> continued =
      scopes_.empty() ? diag::Result<lir::BlockId>{FrameEdge()}
                      : UnwindEntryAt(scopes_.size() - 1);
  if (!continued) {
    return std::unexpected(continued.error());
  }
  // The way out stops at the innermost region open here, so this frame takes
  // the raise exactly where one is open at all.
  const bool taken = std::ranges::any_of(scopes_, [](const UnwindScope& scope) {
    return std::holds_alternative<RegionScope>(scope.kind);
  });
  const lir::BlockId landing = NewBlock();
  const lir::BlockId resumed = current_;
  SetCurrent(landing);
  const lir::Operand exception = Emit(
      UnwindingType(),
      lir::LandingInstr{
          .clause = taken ? lir::LandingInstr::Clause::kCatchAll
                          : lir::LandingInstr::Clause::kCleanup});
  Store(
      lir::Place{.base = lir::Use{.value = ExceptionSlot()}, .chain = {}},
      exception);
  if (unmade.has_value()) {
    MarkStorage(lir::StorageInstr::Op::kDead, *unmade);
  }
  Terminate(lir::BranchTerm{.target = *continued});
  SetCurrent(resumed);
  return landing;
}

// Walking outward from `index`, the walk stops at the first scope whose entry
// needs nothing outside it -- one already built, or a region, whose handler
// takes what arrives -- or at the outermost, which continues to the frame's
// edge. The scopes inside the stop are then built from the outside in, each
// continuing to the one around it, so unwinding from inside many scopes is one
// loop over them, as clang pops its cleanup stack.
auto FunctionLowerer::UnwindEntryAt(std::size_t index)
    -> diag::Result<lir::BlockId> {
  const auto stops = [&](std::size_t at) {
    return scopes_[at].unwind_entry.has_value() ||
           std::holds_alternative<RegionScope>(scopes_[at].kind);
  };
  std::size_t first = index;
  while (first > 0 && !stops(first)) {
    --first;
  }
  // What the scopes inside the stop continue to: the stop's own entry, which
  // for a region is its handler, and the frame's edge where the walk reached
  // the outermost scope without stopping, which is then built like the rest.
  lir::BlockId outward{};
  if (const auto built = scopes_[first].unwind_entry; built.has_value()) {
    outward = *built;
    ++first;
  } else if (const auto* region =
                 std::get_if<RegionScope>(&scopes_[first].kind);
             region != nullptr) {
    outward = region->handler;
    scopes_[first].unwind_entry = outward;
    ++first;
  } else {
    outward = FrameEdge();
  }
  for (std::size_t at = first; at <= index; ++at) {
    const diag::Result<lir::BlockId> entered = BuildUnwindEntry(at, outward);
    if (!entered) {
      return entered;
    }
    scopes_[at].unwind_entry = *entered;
    outward = *entered;
  }
  return outward;
}

auto FunctionLowerer::BuildUnwindEntry(std::size_t index, lir::BlockId outward)
    -> diag::Result<lir::BlockId> {
  const lir::BlockId resumed = current_;
  // What a scope owes, run once here and passed on to the scope it is nested
  // in, so its code appears once on the way out however many calls inside it
  // can be unwound out of.
  const auto owed_then_outward =
      [&](const auto& owe) -> diag::Result<lir::BlockId> {
    const lir::BlockId entry = NewBlock();
    SetCurrent(entry);
    auto owed = owe();
    if (!owed) {
      return std::unexpected(std::move(owed.error()));
    }
    Terminate(lir::BranchTerm{.target = outward});
    return entry;
  };
  // Taken by value: lowering a cleanup's code opens and closes scopes of its
  // own on the same stack.
  const ScopeKind kind = scopes_[index].kind;
  auto entered = std::visit(
      Overloaded{
          [&](const CleanupScope& cleanup) -> diag::Result<lir::BlockId> {
            return owed_then_outward([&] { return LowerCleanupInto(cleanup); });
          },
          [&](const ValueEnd& end) -> diag::Result<lir::BlockId> {
            return owed_then_outward([&]() -> diag::Result<void> {
              EmitScopeDrop(end);
              return {};
            });
          },
          [&](const SlotEnd& end) -> diag::Result<lir::BlockId> {
            return owed_then_outward([&]() -> diag::Result<void> {
              EmitScopeDrop(end);
              return {};
            });
          },
          [&](const VariableEnd& end) -> diag::Result<lir::BlockId> {
            return owed_then_outward([&]() -> diag::Result<void> {
              EmitScopeDrop(end);
              return {};
            });
          },
          // Nothing is owed here any more, so unwinding goes straight on.
          [&](const EndPassedOn&) -> diag::Result<lir::BlockId> {
            return outward;
          },
          [&](const StorageRelease& release) -> diag::Result<lir::BlockId> {
            return owed_then_outward([&]() -> diag::Result<void> {
              EmitScopeDrop(release);
              return {};
            });
          },
          [&](const HandlingScope&) -> diag::Result<lir::BlockId> {
            return owed_then_outward([&]() -> diag::Result<void> {
              EmitStatement(lir::EndHandlingInstr{});
              return {};
            });
          },
          [](const RegionScope&) -> diag::Result<lir::BlockId> {
            throw InternalError(
                "mir_to_lir: a region's unwind entry is built where the walk "
                "outward stops at it -- please report this as a bug");
          },
          [](const TerminateScope&) -> diag::Result<lir::BlockId> {
            throw InternalError(
                "mir_to_lir: a cleanup's code can be unwound out of, and "
                "unwinding starting inside a cleanup has nowhere to go -- "
                "please report this as a bug");
          }},
      kind);
  SetCurrent(resumed);
  return entered;
}

// Past every scope nothing here takes the raise and nothing more is owed, so
// unwinding goes on out of the body.
auto FunctionLowerer::FrameEdge() -> lir::BlockId {
  if (frame_edge_.has_value()) {
    return *frame_edge_;
  }
  const lir::BlockId edge = NewBlock();
  frame_edge_ = edge;
  const lir::BlockId resumed = current_;
  SetCurrent(edge);
  Terminate(
      lir::UnwindResumeTerm{
          .exception = Load(
              lir::Place{
                  .base = lir::Use{.value = ExceptionSlot()}, .chain = {}},
              UnwindingType())});
  SetCurrent(resumed);
  return edge;
}

auto FunctionLowerer::OpenHandler(const RegionScope& region)
    -> diag::Result<void> {
  SetCurrent(region.handler);
  EmitStatement(
      lir::BeginHandlingInstr{
          .exception = Load(
              lir::Place{
                  .base = lir::Use{.value = ExceptionSlot()}, .chain = {}},
              UnwindingType())});
  OpenScope(HandlingScope{});
  // What arrived is asked about inside the handling, because the asking is
  // itself a way out of it: what is not the design's goes on from there.
  return EmitInvoke(
      lir::BuiltinTarget{.fn = support::BuiltinFn::kReceiveDeparture}, {},
      lir::Place{.base = lir::Use{.value = region.caught}, .chain = {}});
}

auto FunctionLowerer::EmitCoroutineHandler(const RegionScope& top)
    -> diag::Result<void> {
  auto opened = OpenHandler(top);
  if (!opened) {
    return opened;
  }
  auto closed = CloseExtent(0);
  if (!closed) {
    return closed;
  }
  Terminate(
      lir::DepartTerm{
          .departure = Load(
              lir::Place{.base = lir::Use{.value = top.caught}, .chain = {}},
              unit_->ControlEffectType())});
  return {};
}

auto FunctionLowerer::EmitInvoke(
    lir::CallTarget target, std::vector<lir::Operand> args,
    lir::Destination destination) -> diag::Result<void> {
  const std::optional<lir::ValueId> owner =
      OwningTransient(destination, lir::CallMakesValue(target));
  // A call unwound out of makes no value, so the bytes the value would have
  // lain in go out of use where the unwinding lands.
  auto landing = LandingHere(owner);
  if (!landing) {
    return std::unexpected(std::move(landing.error()));
  }
  const lir::BlockId returned = NewBlock();
  Terminate(
      lir::InvokeTerm{
          .destination = std::move(destination),
          .target = std::move(target),
          .args = std::move(args),
          .returned = returned,
          .landing = *landing});
  SetCurrent(returned);
  // The value exists only where the call returned, so the landing, built
  // before it, owes nothing for it.
  if (owner.has_value()) {
    OpenScope(ValueEnd{.value = *owner});
  }
  return {};
}

auto FunctionLowerer::NewBlock() -> lir::BlockId {
  const lir::BlockId id{static_cast<std::uint32_t>(blocks_.size())};
  blocks_.emplace_back(OpenBlock{.id = id, .instrs = {}, .terminator = {}});
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

namespace {

// A callee that can be unwound out of is called as an invoke, so that whatever
// is owed between here and the frame's edge gets its turn.
void RequireReturns(const lir::Rvalue& value) {
  if (const auto* call = std::get_if<lir::CallInstr>(&value);
      call != nullptr && MayUnwind(lir::CallEndingOf(call->target))) {
    throw InternalError(
        "mir_to_lir: a callee that can be unwound out of is called as an "
        "instruction rather than as an invoke -- please report this as a "
        "bug");
  }
}

}  // namespace

auto FunctionLowerer::OwningTransient(
    const lir::Destination& destination, bool makes_value) const
    -> std::optional<lir::ValueId> {
  return std::visit(
      Overloaded{
          [&](const lir::Transient& defined) -> std::optional<lir::ValueId> {
            if (!makes_value || !unit_->Types()
                                     .Get(fn_.values.Get(defined.value).type)
                                     .IsOwnedValue()) {
              return std::nullopt;
            }
            return defined.value;
          },
          [](const lir::Place&) -> std::optional<lir::ValueId> {
            return std::nullopt;
          }},
      destination);
}

void FunctionLowerer::Assign(lir::Destination destination, lir::Rvalue value) {
  RequireReturns(value);
  const std::optional<lir::ValueId> owner =
      OwningTransient(destination, lir::MakesValue(value));
  blocks_[current_.value].instrs.push_back(
      lir::Instr{
          .data = lir::AssignInstr{
              .destination = std::move(destination),
              .value = std::move(value)}});
  if (owner.has_value()) {
    OpenScope(ValueEnd{.value = *owner});
  }
}

auto FunctionLowerer::NewTransient(lir::TypeId type) -> lir::ValueId {
  return fn_.values.Add(
      lir::Local{.name = {}, .type = type, .kind = lir::LocalKind::kTemp});
}

auto FunctionLowerer::Emit(lir::TypeId type, lir::Rvalue value)
    -> lir::Operand {
  const lir::ValueId result = NewTransient(type);
  Assign(lir::Transient{.value = result}, std::move(value));
  return lir::Use{.value = result};
}

auto FunctionLowerer::VoidValue() -> lir::Operand {
  return lir::VoidConst{
      .type = unit_->TranslateType(unit_->Mir().builtins.void_type)};
}

void FunctionLowerer::EmitStatement(lir::InstrData statement) {
  blocks_[current_.value].instrs.push_back(
      lir::Instr{.data = std::move(statement)});
}

void FunctionLowerer::EndValue(lir::Operand value) {
  Emit(
      unit_->TranslateType(unit_->Mir().builtins.void_type),
      lir::CallInstr{
          .target = lir::EndValueTarget{}, .args = {std::move(value)}});
}

void FunctionLowerer::EndSlotValue(lir::ValueId slot) {
  EndValue(Load(LocalPlace(slot), fn_.values.Get(slot).type));
}

void FunctionLowerer::MarkStorage(lir::StorageInstr::Op op, lir::ValueId of) {
  EmitStatement(lir::StorageInstr{.op = op, .of = of});
}

void FunctionLowerer::EmitScopeDrop(const ValueEnd& end) {
  EndValue(lir::Use{.value = end.value});
  MarkStorage(lir::StorageInstr::Op::kDead, end.value);
}

void FunctionLowerer::EmitScopeDrop(const SlotEnd& end) {
  EndSlotValue(end.slot);
  MarkStorage(lir::StorageInstr::Op::kDead, end.slot);
}

void FunctionLowerer::EmitScopeDrop(const VariableEnd& end) {
  EmitStatement(
      lir::EndVariableInstr{
          .variables = *variables_, .position = end.position});
}

void FunctionLowerer::EmitScopeDrop(const StorageRelease& release) {
  Emit(
      unit_->TranslateType(unit_->Mir().builtins.void_type),
      lir::CallInstr{
          .target =
              lir::DynamicStorageTarget{
                  .op = lir::DynamicStorageTarget::Op::kDeallocate},
          .args = {release.storage}});
}

auto FunctionLowerer::IsLentCopyable(const lir::Operand& value) const -> bool {
  const lir::TypeId type = lir::OperandType(fn_, value);
  return unit_->Types().Get(type).IsOwnedValue() &&
         !std::holds_alternative<lir::NoSuchStep>(
             lir::StepOf(unit_->Lir(), type, lir::ValueStep::kCopy)) &&
         !OwedEndAt(value).has_value();
}

auto FunctionLowerer::ConsumeByCopyOrMove(lir::Operand value) -> lir::Rvalue {
  if (IsLentCopyable(value)) {
    return lir::CallInstr{.target = lir::CopyValueTarget{}, .args = {value}};
  }
  return lir::UseInstr{.operand = HandOn(std::move(value))};
}

void FunctionLowerer::AssignFrom(
    lir::Destination destination, lir::Operand value) {
  std::visit(
      Overloaded{
          // Moved into a transient, a value is the one value still.
          [&](lir::Transient& defined) {
            Assign(defined, ConsumeByCopyOrMove(std::move(value)));
          },
          // Moved into storage, it leaves the bytes it was made in behind.
          [&](lir::Place& place) {
            const std::optional<std::size_t> owed = OwedEndAt(value);
            const std::optional<lir::ValueId> left_behind =
                owed.has_value()
                    ? std::optional{std::get<ValueEnd>(scopes_[*owed].kind)
                                        .value}
                    : std::nullopt;
            Assign(std::move(place), ConsumeByCopyOrMove(std::move(value)));
            if (left_behind.has_value()) {
              MarkStorage(lir::StorageInstr::Op::kDead, *left_behind);
            }
          }},
      destination);
}

void FunctionLowerer::OpenScope(ScopeKind kind) {
  scopes_.push_back(
      UnwindScope{.kind = std::move(kind), .unwind_entry = std::nullopt});
}

auto FunctionLowerer::OwedEndAt(const lir::Operand& value) const
    -> std::optional<std::size_t> {
  const auto* use = std::get_if<lir::Use>(&value);
  if (use == nullptr) {
    return std::nullopt;
  }
  for (std::size_t i = 0; i < scopes_.size(); ++i) {
    const auto* end = std::get_if<ValueEnd>(&scopes_[i].kind);
    if (end != nullptr && end->value == use->value) {
      return i;
    }
  }
  return std::nullopt;
}

auto FunctionLowerer::OwedEndOfLocal(const mir::Block& block, mir::ExprId place)
    const -> std::optional<std::size_t> {
  const std::optional<mir::LocalId> local =
      mir::ReferencedLocal(block.exprs.Get(place).data);
  if (!local.has_value() || !locals_[local->value].has_value()) {
    return std::nullopt;
  }
  const auto* binding = std::get_if<PlaceBinding>(&*locals_[local->value]);
  if (binding == nullptr) {
    return std::nullopt;
  }
  for (std::size_t i = 0; i < scopes_.size(); ++i) {
    const auto* end = std::get_if<SlotEnd>(&scopes_[i].kind);
    if (end != nullptr && end->slot == binding->slot) {
      return i;
    }
  }
  return std::nullopt;
}

auto FunctionLowerer::HandOn(lir::Operand value) -> lir::Operand {
  const lir::TypeId type = lir::OperandType(fn_, value);
  if (!unit_->Types().Get(type).IsOwnedValue()) {
    return value;
  }
  if (const std::optional<std::size_t> owed = OwedEndAt(value)) {
    PassEndAt(*owed);
    return value;
  }
  throw InternalError(
      "mir_to_lir: a value is handed on by a body that does not own it -- "
      "please report this as a bug");
}

void FunctionLowerer::PassEndAt(std::size_t index) {
  scopes_[index].kind = EndPassedOn{};
  // What was built while the end was owed goes on running it, which is right
  // for the calls made then; what is built from here on must not, so this
  // scope and every one inside it build theirs afresh.
  for (std::size_t j = index; j < scopes_.size(); ++j) {
    scopes_[j].unwind_entry = std::nullopt;
  }
}

auto FunctionLowerer::AllocateCompletionFor(lir::TypeId payload)
    -> lir::Operand {
  const lir::ValueId result = fn_.values.Add(
      lir::Local{
          .name = {},
          .type = AddressType(payload),
          .kind = lir::LocalKind::kTemp});
  // Into the block the body's stretches start in rather than where the call
  // stands, so a call inside a loop writes into one place instead of leaving a
  // fresh one behind on every iteration.
  blocks_[stretch_start_.value].instrs.push_back(
      lir::Instr{
          .data = lir::AssignInstr{
              .destination = lir::Transient{.value = result},
              .value = lir::CallInstr{
                  .target =
                      lir::ValueCellTarget{
                          .op = lir::ValueCellTarget::Op::kAllocate,
                          .value = payload},
                  .args = {}}}});
  return lir::Use{.value = result};
}

auto FunctionLowerer::NewPlaceLocal(lir::TypeId type) -> lir::ValueId {
  return fn_.values.Add(
      lir::Local{.name = {}, .type = type, .kind = lir::LocalKind::kPlace});
}

auto FunctionLowerer::NewLiveSlot(lir::TypeId type) -> lir::ValueId {
  const lir::ValueId slot = NewPlaceLocal(type);
  if (unit_->Types().Get(type).IsOwnedValue()) {
    MarkStorage(lir::StorageInstr::Op::kLive, slot);
  }
  return slot;
}

auto FunctionLowerer::OpenLocalSlot(mir::LocalId local, lir::TypeId type)
    -> lir::ValueId {
  const lir::ValueId slot = NewLiveSlot(type);
  locals_[local.value] = PlaceBinding{.slot = slot};
  return slot;
}

auto FunctionLowerer::DeclareLocal(
    const mir::Block& block, mir::LocalId local, mir::ExprId init)
    -> diag::Result<void> {
  // A local the body gave storage of its own begins where its declaration is
  // reached, so each entry to the declaration is a fresh variable in the one
  // storage. A cell MIR declared takes what it holds from the initialization
  // that follows; any other such variable from its initializer.
  const mir::TypeId declared = code_->locals.Get(local).type;
  if (IsDeclaredCell(unit_->Mir(), declared)) {
    BeginVariable(local);
    return {};
  }
  if (variable_slot_[local.value].has_value()) {
    const lir::Operand cell = BeginVariable(local);
    const std::size_t depth = scopes_.size();
    auto value = LowerExpr(block, init);
    if (!value) {
      return std::unexpected(std::move(value.error()));
    }
    InitializeCell(cell, *std::move(value));
    return CloseExtent(depth);
  }
  // The initial value is made where the local lies, as rustc's
  // `expr_into_dest` builds a `let`'s initializer into its place, so a value
  // that never moves -- a closure -- is a local like any other.
  const lir::TypeId type = unit_->TranslateType(declared);
  const std::size_t depth = scopes_.size();
  const lir::ValueId slot = OpenLocalSlot(local, type);
  auto made = LowerExprInto(block, init, LocalPlace(slot));
  if (!made) {
    return made;
  }
  auto closed = CloseExtent(depth);
  if (!closed) {
    return closed;
  }
  if (unit_->Types().Get(type).IsOwnedValue()) {
    OpenScope(SlotEnd{.slot = slot});
  }
  return {};
}

auto FunctionLowerer::Load(lir::Place place, lir::TypeId type) -> lir::Operand {
  return Emit(type, lir::LoadInstr{.place = std::move(place)});
}

auto FunctionLowerer::Store(lir::Place place, lir::Operand value)
    -> lir::Operand {
  // A frame slot of an owned type owns its value, so it ends the one it held
  // before taking the one it is given, and takes that one's end with it.
  if (lir::IsPlaceLocal(fn_, place.base) && place.chain.empty()) {
    const lir::TypeId type =
        fn_.values.Get(std::get<lir::Use>(place.base).value).type;
    if (unit_->Types().Get(type).IsOwnedValue()) {
      EndValue(Load(place, type));
      AssignFrom(std::move(place), std::move(value));
      return VoidValue();
    }
  }
  EmitStatement(
      lir::StoreInstr{.place = std::move(place), .value = std::move(value)});
  return VoidValue();
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

void FunctionLowerer::StoreActivationValue(
    lir::Operand handle, lir::Operand value, lir::TypeId value_type) {
  Emit(
      unit_->TranslateType(unit_->Mir().builtins.void_type),
      lir::CallInstr{
          .target =
              lir::ValueCellTarget{
                  .op = lir::ValueCellTarget::Op::kStore, .value = value_type},
          .args = {std::move(handle), std::move(value)}});
}

void FunctionLowerer::ReserveVariableStorage() {
  if (fn_.variables.empty()) {
    return;
  }
  // The storage is the body's own and the body never reads through it; it only
  // names it when reaching, beginning and ending a variable in it.
  const lir::TypeId storage =
      AddressType(unit_->Types().Intern(lir::Type{lir::VoidType{}}));
  variables_ = Emit(storage, lir::VariableStorageInstr{});
}

auto FunctionLowerer::BeginVariable(mir::LocalId local) -> lir::Operand {
  const lir::VariablePosition position{*variable_slot_[local.value]};
  const lir::TypeId value = unit_->TranslateType(
      StoredValueType(unit_->Mir(), code_->locals.Get(local).type));
  const lir::TypeId address = AddressType(lir::CellOf(unit_->Types(), value));
  const lir::Operand cell = Emit(
      address,
      lir::BeginVariableInstr{.variables = *variables_, .position = position});
  locals_[local.value] = CellBinding{.cell = cell};
  OpenScope(VariableEnd{.position = position});
  return cell;
}

void FunctionLowerer::InitializeCell(lir::Operand cell, lir::Operand value) {
  Emit(
      unit_->Types().Intern(lir::Type{lir::VoidType{}}),
      lir::CallInstr{
          .target = lir::BuiltinTarget{.fn = support::BuiltinFn::kInitialize},
          .args = {std::move(cell), std::move(value)}});
}

auto FunctionLowerer::StorageAt(lir::Operand address) -> lir::Place {
  return lir::Place{
      .base = std::move(address),
      .chain = {lir::Projection{lir::DerefProjection{}}}};
}

auto FunctionLowerer::ValueAt(lir::Operand address) -> lir::Place {
  lir::Place place = StorageAt(std::move(address));
  place.chain.emplace_back(lir::DerefProjection{});
  return place;
}

auto FunctionLowerer::LowerBlockInto(const mir::Block& block)
    -> diag::Result<void> {
  const std::size_t depth = scopes_.size();
  auto lowered = LowerStatementsInto(block);
  if (!lowered) {
    return lowered;
  }
  return CloseExtent(depth);
}

auto FunctionLowerer::LowerBlockExpr(const mir::Block& scope, mir::ExprId value)
    -> diag::Result<lir::Operand> {
  // With no steps, nothing ends with the block, which is its value.
  if (scope.root_stmts.empty()) {
    return LowerExpr(scope, value);
  }
  const std::size_t depth = scopes_.size();
  auto lowered = LowerStatementsInto(scope);
  if (!lowered) {
    return std::unexpected(std::move(lowered.error()));
  }
  auto yielded = LowerExpr(scope, value);
  if (!yielded) {
    return yielded;
  }
  lir::Operand carried = *std::move(yielded);
  // A value lent by storage it lies in is copied out before the block ends,
  // as a C++ `return` makes its result, whichever storage lent it.
  if (IsLentCopyable(carried)) {
    carried = Emit(
        lir::OperandType(fn_, carried),
        ConsumeByCopyOrMove(std::move(carried)));
  }
  auto closed = CloseExtentYielding(depth, carried);
  if (!closed) {
    return std::unexpected(std::move(closed.error()));
  }
  return carried;
}

auto FunctionLowerer::LowerBlockExprInto(
    const mir::Block& scope, mir::ExprId value,
    const lir::Destination& destination) -> diag::Result<void> {
  const std::size_t depth = scopes_.size();
  auto lowered = LowerStatementsInto(scope);
  if (!lowered) {
    return lowered;
  }
  auto made = LowerExprInto(scope, value, destination);
  if (!made) {
    return made;
  }
  // Storage the destination names was there before the block, and a value
  // made into a transient is owed from inside it.
  const auto* defined = std::get_if<lir::Transient>(&destination);
  if (defined == nullptr) {
    return CloseExtent(depth);
  }
  return CloseExtentYielding(depth, lir::Use{.value = defined->value});
}

auto FunctionLowerer::CloseExtentYielding(
    std::size_t depth, const lir::Operand& yielded) -> diag::Result<void> {
  const std::optional<std::size_t> owed = OwedEndAt(yielded);
  const bool carried_out = owed.has_value() && *owed >= depth;
  if (carried_out) {
    PassEndAt(*owed);
  }
  auto closed = CloseExtent(depth);
  if (!closed) {
    return closed;
  }
  if (carried_out) {
    OpenScope(ValueEnd{.value = std::get<lir::Use>(yielded).value});
  }
  return {};
}

auto FunctionLowerer::CloseExtent(std::size_t depth) -> diag::Result<void> {
  if (!Terminated()) {
    auto cleaned = RunCleanupsDownTo(depth);
    if (!cleaned) {
      return cleaned;
    }
  }
  scopes_.erase(
      scopes_.begin() + static_cast<std::ptrdiff_t>(depth), scopes_.end());
  return {};
}

auto FunctionLowerer::LowerDiscardedInto(
    const mir::Block& block, mir::ExprId id) -> diag::Result<void> {
  const std::size_t depth = scopes_.size();
  auto lowered = LowerExpr(block, id);
  if (!lowered) {
    return std::unexpected(std::move(lowered.error()));
  }
  return CloseExtent(depth);
}

auto FunctionLowerer::LowerStatementsInto(const mir::Block& block)
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
            return LowerDiscardedInto(block, s.expr);
          },
          [&](const mir::BlockStmt& s) -> diag::Result<void> {
            return LowerBlockStmtInto(block, s);
          },
          [&](const mir::TryStmt& s) -> diag::Result<void> {
            return LowerTryInto(block, s);
          },
          // A raise says the region holding this departure declines it, never
          // that a new effect starts here, so what carries on outward is the
          // one the statement names -- which is the departure the region
          // received, and not always what the unwinder brought it.
          [&](const mir::RaiseStmt& s) -> diag::Result<void> {
            auto effect = LowerExpr(block, s.effect);
            if (!effect) {
              return std::unexpected(std::move(effect.error()));
            }
            return LeaveCarrying(*std::move(effect));
          },
          [&](const mir::FinallyStmt& s) -> diag::Result<void> {
            return LowerFinallyInto(block, s);
          },
          [&](const mir::LocalDeclStmt& s) -> diag::Result<void> {
            return DeclareLocal(block, s.target, s.init);
          },
          [&](const mir::IfStmt& s) -> diag::Result<void> {
            return LowerIfInto(block, s);
          },
          [&](const mir::SwitchStmt& s) -> diag::Result<void> {
            return LowerSwitchInto(block, s);
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
            return LowerReturnInto(block, s);
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

// The discriminant is a full-expression, and the machine integer it settles
// owns nothing, so everything made while settling it ends before the
// dispatch. Every case, and the default, ends at one join.
auto FunctionLowerer::LowerSwitchInto(
    const mir::Block& block, const mir::SwitchStmt& stmt)
    -> diag::Result<void> {
  const std::size_t depth = scopes_.size();
  auto discriminant = LowerExpr(block, stmt.discriminant);
  if (!discriminant) {
    return std::unexpected(std::move(discriminant.error()));
  }
  if (auto closed = CloseExtent(depth); !closed) {
    return std::unexpected(std::move(closed.error()));
  }

  const lir::BlockId merge_id = NewBlock();
  const auto run = [&](mir::BlockId body) -> diag::Result<lir::BlockId> {
    const lir::BlockId entry = NewBlock();
    const lir::BlockId resume = current_;
    SetCurrent(entry);
    if (auto lowered = LowerBlockInto(block.child_scopes.Get(body)); !lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
    if (!Terminated()) {
      Terminate(lir::BranchTerm{.target = merge_id});
    }
    SetCurrent(resume);
    return entry;
  };

  std::vector<lir::SwitchTarget> targets;
  for (const mir::SwitchCase& selected : stmt.cases) {
    auto entry = run(selected.body);
    if (!entry) {
      return std::unexpected(std::move(entry.error()));
    }
    for (const std::int64_t value : selected.values) {
      targets.push_back(lir::SwitchTarget{.value = value, .target = *entry});
    }
  }
  lir::BlockId otherwise = merge_id;
  if (stmt.default_body.has_value()) {
    auto entry = run(*stmt.default_body);
    if (!entry) {
      return std::unexpected(std::move(entry.error()));
    }
    otherwise = *entry;
  }
  Terminate(
      lir::SwitchTerm{
          .discriminant = *std::move(discriminant),
          .targets = std::move(targets),
          .otherwise = otherwise});
  SetCurrent(merge_id);
  return {};
}

// A labeled block is the target of a break naming it, which goes on at the
// statement after the block; a block no break names is its statements.
auto FunctionLowerer::LowerBlockStmtInto(
    const mir::Block& block, const mir::BlockStmt& stmt) -> diag::Result<void> {
  if (!stmt.break_label.has_value()) {
    return LowerBlockInto(block.child_scopes.Get(stmt.scope));
  }
  const lir::BlockId exit_id = NewBlock();
  breakable_scopes_.push_back(
      BreakableScope{
          .label = stmt.break_label,
          .continue_target = std::nullopt,
          .break_target = exit_id,
          .scope_depth = scopes_.size()});
  auto body = LowerBlockInto(block.child_scopes.Get(stmt.scope));
  breakable_scopes_.pop_back();
  if (!body) {
    return std::unexpected(std::move(body.error()));
  }
  if (!Terminated()) {
    Terminate(lir::BranchTerm{.target = exit_id});
  }
  SetCurrent(exit_id);
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
  breakable_scopes_.push_back(
      BreakableScope{
          .label = stmt.break_label,
          .continue_target = header_id,
          .break_target = exit_id,
          .scope_depth = scopes_.size()});
  auto body = LowerBlockInto(block.child_scopes.Get(stmt.scope));
  breakable_scopes_.pop_back();
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
  breakable_scopes_.push_back(
      BreakableScope{
          .label = stmt.break_label,
          .continue_target = latch_id,
          .break_target = exit_id,
          .scope_depth = scopes_.size()});
  auto body = LowerBlockInto(block.child_scopes.Get(stmt.scope));
  breakable_scopes_.pop_back();
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
              return DeclareLocal(block, decl.induction_var, decl.init);
            },
            [&](const mir::ForInitExpr& expr) -> diag::Result<void> {
              return LowerDiscardedInto(block, expr.expr);
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
  breakable_scopes_.push_back(
      BreakableScope{
          .label = stmt.break_label,
          .continue_target = step_id,
          .break_target = exit_id,
          .scope_depth = scopes_.size()});
  auto body = LowerBlockInto(block.child_scopes.Get(stmt.scope));
  breakable_scopes_.pop_back();
  if (!body) {
    return std::unexpected(std::move(body.error()));
  }
  if (!Terminated()) {
    Terminate(lir::BranchTerm{.target = step_id});
  }

  SetCurrent(step_id);
  for (const mir::ExprId step : stmt.step) {
    auto lowered = LowerDiscardedInto(block, step);
    if (!lowered) {
      return lowered;
    }
  }
  Terminate(lir::BranchTerm{.target = header_id});

  SetCurrent(exit_id);
  return {};
}

auto FunctionLowerer::LowerBreakInto(const mir::BreakStmt& stmt)
    -> diag::Result<void> {
  // A break leaves the statement that carries the label it names, however
  // deep inside it the break stands.
  for (const BreakableScope& left : std::views::reverse(breakable_scopes_)) {
    if (left.label == stmt.target) {
      auto cleaned = RunCleanupsDownTo(left.scope_depth);
      if (!cleaned) {
        return std::unexpected(std::move(cleaned.error()));
      }
      Terminate(lir::BranchTerm{.target = left.break_target});
      return {};
    }
  }
  throw InternalError("mir_to_lir: break outside of any matching statement");
}

auto FunctionLowerer::LowerContinueInto() -> diag::Result<void> {
  for (const BreakableScope& loop : std::views::reverse(breakable_scopes_)) {
    if (loop.continue_target.has_value()) {
      auto cleaned = RunCleanupsDownTo(loop.scope_depth);
      if (!cleaned) {
        return std::unexpected(std::move(cleaned.error()));
      }
      Terminate(lir::BranchTerm{.target = *loop.continue_target});
      return {};
    }
  }
  throw InternalError("mir_to_lir: continue outside of any loop");
}

auto FunctionLowerer::CurrentRuntime() -> lir::Operand {
  return Emit(
      unit_->TranslateType(unit_->Mir().builtins.effects),
      lir::CallInstr{
          .target =
              lir::BuiltinTarget{.fn = support::BuiltinFn::kCurrentRuntime},
          .args = {}});
}

auto FunctionLowerer::RunCleanupsDownTo(std::size_t depth)
    -> diag::Result<void> {
  for (std::size_t i = scopes_.size(); i > depth; --i) {
    // Taken by value: lowering a cleanup's code opens and closes scopes of its
    // own on the same stack.
    const ScopeKind kind = scopes_[i - 1].kind;
    auto lowered = std::visit(
        Overloaded{
            [&](const CleanupScope& cleanup) -> diag::Result<void> {
              return LowerCleanupInto(cleanup);
            },
            [&](const ValueEnd& end) -> diag::Result<void> {
              EmitScopeDrop(end);
              return {};
            },
            [&](const SlotEnd& end) -> diag::Result<void> {
              EmitScopeDrop(end);
              return {};
            },
            [&](const VariableEnd& end) -> diag::Result<void> {
              EmitScopeDrop(end);
              return {};
            },
            [](const EndPassedOn&) -> diag::Result<void> { return {}; },
            [&](const StorageRelease& release) -> diag::Result<void> {
              EmitScopeDrop(release);
              return {};
            },
            [&](const HandlingScope&) -> diag::Result<void> {
              EmitStatement(lir::EndHandlingInstr{});
              return {};
            },
            // Leaving a region, or a cleanup's own code, by an ordinary way out
            // owes it nothing: a region's handler is only for what unwinds.
            [](const RegionScope&) -> diag::Result<void> { return {}; },
            [](const TerminateScope&) -> diag::Result<void> { return {}; }},
        kind);
    if (!lowered) {
      return lowered;
    }
  }
  return {};
}

auto FunctionLowerer::LowerCleanupInto(CleanupScope cleanup)
    -> diag::Result<void> {
  OpenScope(TerminateScope{});
  auto lowered =
      LowerBlockInto(cleanup.owner->child_scopes.Get(cleanup.cleanup));
  scopes_.pop_back();
  return lowered;
}

auto FunctionLowerer::SuspendIf(lir::Operand park) -> diag::Result<void> {
  const lir::BlockId parked = NewBlock();
  const lir::BlockId resume = NewBlock();
  Terminate(
      lir::CondBranchTerm{
          .condition = std::move(park), .if_true = parked, .if_false = resume});
  SetCurrent(parked);
  const lir::BlockId abandoned = NewBlock();
  Terminate(lir::SuspendTerm{.resume = resume, .abandoned = abandoned});
  SetCurrent(abandoned);
  auto cleaned = RunCleanupsDownTo(0);
  if (!cleaned) {
    return std::unexpected(std::move(cleaned.error()));
  }
  Terminate(lir::AbandonTerm{});
  SetCurrent(resume);
  return {};
}

auto FunctionLowerer::LeaveCarrying(lir::Operand effect) -> diag::Result<void> {
  // Declining puts the departure back on its way, and a region outside this
  // one is entitled to the same chance at it that this one just had, so the
  // decline is itself a point it can leave from.
  auto declined = EmitCallTo(
      lir::ControlEffectTarget{
          .op = lir::ControlEffectTarget::Op::kDeclineDeparture},
      {std::move(effect)},
      unit_->TranslateType(unit_->Mir().builtins.void_type));
  if (!declined) {
    return std::unexpected(std::move(declined.error()));
  }
  return {};
}

auto FunctionLowerer::TakeDepartureIfDue() -> diag::Result<void> {
  auto taken = EmitCallTo(
      lir::ControlEffectTarget{
          .op = lir::ControlEffectTarget::Op::kTakeDepartureIfDue},
      {CurrentRuntime()},
      unit_->TranslateType(unit_->Mir().builtins.void_type));
  if (!taken) {
    return std::unexpected(std::move(taken.error()));
  }
  return {};
}

auto FunctionLowerer::LowerFinallyInto(
    const mir::Block& block, const mir::FinallyStmt& stmt)
    -> diag::Result<void> {
  OpenScope(CleanupScope{.owner = &block, .cleanup = stmt.cleanup});
  auto body = LowerBlockInto(block.child_scopes.Get(stmt.body));
  scopes_.pop_back();
  if (!body) {
    return std::unexpected(std::move(body.error()));
  }
  // Falling off the body's end is the one way out the body does not state
  // itself, so it is the one the region states here.
  if (Terminated()) {
    return {};
  }
  return LowerCleanupInto(
      CleanupScope{.owner = &block, .cleanup = stmt.cleanup});
}

auto FunctionLowerer::LowerTryInto(
    const mir::Block& block, const mir::TryStmt& stmt) -> diag::Result<void> {
  const lir::BlockId handler_id = NewBlock();
  const lir::BlockId merge_id = NewBlock();

  // The effect is received where the handler begins and read by its
  // statements, which may stand in other blocks, so it is frame storage rather
  // than a value in flight.
  const lir::ValueId caught =
      NewPlaceLocal(unit_->TranslateType(code_->locals.Get(stmt.caught).type));
  locals_[stmt.caught.value] = PlaceBinding{.slot = caught};
  const RegionScope region{.handler = handler_id, .caught = caught};

  OpenScope(region);
  auto body = LowerBlockInto(block.child_scopes.Get(stmt.body));
  scopes_.pop_back();
  if (!body) {
    return std::unexpected(std::move(body.error()));
  }
  if (!Terminated()) {
    Terminate(lir::BranchTerm{.target = merge_id});
  }

  const std::size_t depth = scopes_.size();
  auto opened = OpenHandler(region);
  if (!opened) {
    return opened;
  }
  auto handler = LowerBlockInto(block.child_scopes.Get(stmt.handler));
  if (!handler) {
    return std::unexpected(std::move(handler.error()));
  }
  // Reaching the handler's end is what claiming the effect amounts to:
  // execution resumes past the region (LRM 9.6.2), once the handling of what
  // the region took has ended there.
  auto closed = CloseExtent(depth);
  if (!closed) {
    return closed;
  }
  if (!Terminated()) {
    Terminate(lir::BranchTerm{.target = merge_id});
  }

  SetCurrent(merge_id);
  return {};
}

auto FunctionLowerer::LowerCondition(const mir::Block& block, mir::ExprId id)
    -> diag::Result<lir::Operand> {
  // A condition arrives already reduced to a predicate at HIR-to-MIR, so it
  // lowers to the machine boolean a branch tests directly. The reduction is
  // stated upstream and lowered like any other value here, never re-derived
  // from the operand's type; a condition that did not arrive reduced is an
  // upstream defect. It is a full-expression, and the predicate it settles owns
  // nothing, so everything made while settling it ends before the branch.
  const std::size_t depth = scopes_.size();
  auto value = LowerExpr(block, id);
  if (!value) {
    return value;
  }
  if (lir::OperandType(fn_, *value) != unit_->MachineBoolType()) {
    throw InternalError(
        "mir_to_lir: a condition did not arrive as a reduced predicate");
  }
  auto closed = CloseExtent(depth);
  if (!closed) {
    return std::unexpected(std::move(closed.error()));
  }
  return value;
}

auto FunctionLowerer::FieldStep(const mir::FieldRef& field) -> lir::Projection {
  const auto member = [](lir::TypeId declared_by, auto slot) {
    return lir::Projection{lir::MemberProjection{
        .member = lir::StatedMemberRef{
            .declared_by = declared_by, .slot = lir::MemberSlot{slot.value}}}};
  };
  return std::visit(
      Overloaded{
          [&](const mir::ClassFieldTarget& t) {
            return member(unit_->ClassValueType(t.owner), t.slot);
          },
          [&](const mir::ClosureFieldTarget& t) {
            return member(unit_->ClosureValueType(t.owner), t.slot);
          },
          [](const mir::ComponentFieldTarget& t) {
            return lir::Projection{
                lir::ComponentProjection{.index = t.position}};
          }},
      field);
}

namespace {

// Whether a call names a library entry answering with a reference to what the
// capability wrapper it is taken on holds.
auto AnswersReferenceToContents(const mir::CallExpr& call) -> bool {
  const std::optional<support::BuiltinFn> fn = mir::DirectBuiltinFn(call);
  if (!fn.has_value()) {
    return false;
  }
  switch (support::RuntimeEntryOf(*fn).answer) {
    case support::EntryAnswer::kReferenceToContents:
      return true;
    case support::EntryAnswer::kNewValue:
    case support::EntryAnswer::kReferenceToSampledContents:
    case support::EntryAnswer::kReadOnlyReferenceIntoTheReceiver:
    case support::EntryAnswer::kMutableReferenceIntoTheReceiver:
    case support::EntryAnswer::kExecution:
      return false;
  }
  throw InternalError("mir_to_lir: unknown entry answer");
}

// Whether a call names a library entry answering with an execution it built
// itself, where a call of a body answering one builds the body's frame for the
// execution to be entered on.
auto EntryBuildsExecution(const mir::CallExpr& call) -> bool {
  const std::optional<support::BuiltinFn> fn = mir::DirectBuiltinFn(call);
  if (!fn.has_value()) {
    return false;
  }
  switch (support::RuntimeEntryOf(*fn).answer) {
    case support::EntryAnswer::kExecution:
      return true;
    case support::EntryAnswer::kNewValue:
    case support::EntryAnswer::kReadOnlyReferenceIntoTheReceiver:
    case support::EntryAnswer::kMutableReferenceIntoTheReceiver:
    case support::EntryAnswer::kReferenceToContents:
    case support::EntryAnswer::kReferenceToSampledContents:
      return false;
  }
  throw InternalError("mir_to_lir: unknown entry answer");
}

// The call a dereference opens, where `step` is the dereference of a call
// answering a reference into its receiver: an element's read or write form.
auto ReferencedPartCall(const mir::Block& block, mir::ExprId step)
    -> const mir::CallExpr* {
  const auto* opened = std::get_if<mir::DerefExpr>(&block.exprs.Get(step).data);
  if (opened == nullptr) {
    return nullptr;
  }
  const auto* call =
      std::get_if<mir::CallExpr>(&block.exprs.Get(opened->pointer).data);
  return call != nullptr && mir::AnswersReferenceIntoReceiver(*call) ? call
                                                                     : nullptr;
}

// The one position a step to bits names where they start by.
auto BitsStartOf(const mir::CallExpr& step) -> mir::ExprId {
  if (step.arguments.size() != 1) {
    throw InternalError(
        "mir_to_lir: a step to bits names where they start and nothing else "
        "-- please report this as a bug");
  }
  return step.arguments.front();
}

}  // namespace

auto FunctionLowerer::HolderReach(Reach part) -> Reach {
  switch (part) {
    case Reach::kRead:
      return Reach::kRead;
    case Reach::kWhole:
    case Reach::kInto:
      return Reach::kInto;
  }
  throw InternalError("mir_to_lir: unknown reach");
}

auto FunctionLowerer::BorrowReach(mir::Mutability mutability) -> Reach {
  switch (mutability) {
    case mir::Mutability::kReadOnly:
      return Reach::kRead;
    case mir::Mutability::kMutable:
      return Reach::kWhole;
  }
  throw InternalError("mir_to_lir: unknown mutability");
}

auto FunctionLowerer::WritesInto(Reach reach) -> bool {
  switch (reach) {
    case Reach::kRead:
    case Reach::kWhole:
      return false;
    case Reach::kInto:
      return true;
  }
  throw InternalError("mir_to_lir: unknown reach");
}

auto FunctionLowerer::ElementPlace(
    const mir::Block& block, const mir::CallExpr& element, Reach reach)
    -> diag::Result<lir::Place> {
  const std::optional<mir::ExprId> whole = mir::CalleeReceiver(element.callee);
  if (!whole.has_value()) {
    throw InternalError(
        "mir_to_lir: a reference into a receiver is taken on one, and this "
        "call names none -- please report this as a bug");
  }
  auto place = PlaceHolding(block, *whole, HolderReach(reach));
  if (!place) {
    return std::unexpected(std::move(place.error()));
  }
  auto step = PartStep(block, element);
  if (!step) {
    return std::unexpected(std::move(step.error()));
  }
  place->chain.push_back(*std::move(step));
  return place;
}

auto FunctionLowerer::PartStep(
    const mir::Block& block, const mir::CallExpr& part)
    -> diag::Result<lir::Projection> {
  const std::optional<support::PartSelection> selects = mir::SelectionOf(part);
  if (!selects.has_value()) {
    throw InternalError(
        "mir_to_lir: a step into a part is made by a call that reaches one -- "
        "please report this as a bug");
  }
  switch (*selects) {
    case support::PartSelection::kElement: {
      auto coordinates = LowerEachExpr(block, part.arguments);
      if (!coordinates) {
        return std::unexpected(std::move(coordinates.error()));
      }
      return lir::Projection{
          lir::ElementProjection{.coordinates = *std::move(coordinates)}};
    }
    case support::PartSelection::kComponent:
    case support::PartSelection::kSlice:
    case support::PartSelection::kBits:
      throw InternalError(
          "mir_to_lir: a component is a field step, a slice is several "
          "elements and bits lie inside a value, so none is a step a reference "
          "into the whole takes -- please report this as a bug");
  }
  throw InternalError("mir_to_lir: unknown part selection");
}

auto FunctionLowerer::PlaceHolding(
    const mir::Block& block, mir::ExprId id, Reach reach)
    -> diag::Result<lir::Place> {
  if (mir::IsPlace(block.exprs.Get(id).data)) {
    return LowerPlace(block, id, reach);
  }
  auto value = LowerExpr(block, id);
  if (!value) {
    return std::unexpected(std::move(value.error()));
  }
  return LocalPlace(HeldInSlot(
      *std::move(value), unit_->TranslateType(block.exprs.Get(id).type)));
}

auto FunctionLowerer::HeldInSlot(lir::Operand value, lir::TypeId type)
    -> lir::ValueId {
  const lir::ValueId slot = NewLiveSlot(type);
  AssignFrom(LocalPlace(slot), std::move(value));
  if (unit_->Types().Get(type).IsOwnedValue()) {
    OpenScope(SlotEnd{.slot = slot});
  }
  return slot;
}

auto FunctionLowerer::OpenWrite(lir::Operand handle, lir::TypeId value)
    -> diag::Result<lir::Operand> {
  auto write = EmitCallTo(
      lir::BuiltinTarget{.fn = support::BuiltinFn::kOpenForWrite},
      {std::move(handle)},
      unit_->Types().Intern(lir::Type{lir::OpenWriteType{.value = value}}));
  if (!write) {
    return std::unexpected(std::move(write.error()));
  }
  return EmitCallTo(
      lir::BuiltinTarget{.fn = support::BuiltinFn::kDesignateWhole},
      {*std::move(write)},
      unit_->Types().Intern(lir::Type{lir::DesignationType{.value = value}}));
}

auto FunctionLowerer::Land(lir::Operand designation, lir::TypeId value)
    -> diag::Result<lir::Place> {
  auto part = EmitCallTo(
      lir::OpenWriteTarget{
          .op = lir::OpenWriteTarget::Op::kLand, .value = value},
      {std::move(designation)}, AddressType(value));
  if (!part) {
    return std::unexpected(std::move(part.error()));
  }
  return StorageAt(*std::move(part));
}

auto FunctionLowerer::AddressType(lir::TypeId value) -> lir::TypeId {
  return unit_->Types().Intern(
      lir::Type{lir::PointerType{
          .pointee = value,
          .ownership = lir::PointerOwnership::kBorrowed,
          .mutability = lir::Mutability::kMutable}});
}

auto FunctionLowerer::WrapperOperand(
    const mir::Block& block, mir::ExprId wrapper)
    -> diag::Result<lir::Operand> {
  return mir::IsPlace(block.exprs.Get(wrapper).data)
             ? LowerRead(block, wrapper)
             : LowerExpr(block, wrapper);
}

auto FunctionLowerer::WrapperContentsPlace(
    const mir::Block& block, mir::ExprId wrapper) -> diag::Result<lir::Place> {
  const mir::Type& wrapper_ty =
      unit_->Mir().types.Get(block.exprs.Get(wrapper).type);
  // Dereferencing a place designated within a write lands the write there.
  if (const auto* designated = wrapper_ty.As<mir::DesignationType>()) {
    auto designation = WrapperOperand(block, wrapper);
    if (!designation) {
      return std::unexpected(std::move(designation.error()));
    }
    return Land(
        *std::move(designation), unit_->TranslateType(designated->value));
  }
  // A wrapper that is itself storage -- an observable cell, a net's resolved
  // value -- is storage the chain has already reached, so naming what it
  // represents extends that chain by one step. Everything else here refers to
  // storage elsewhere: a pointer, a reference, the driver handle a net issued
  // are values, and a value opens a chain rather than continuing one.
  if (wrapper_ty.Is<mir::ObservableType>() ||
      wrapper_ty.Is<mir::ResolvedType>()) {
    auto place = LowerPlace(block, wrapper, Reach::kWhole);
    if (!place) {
      return std::unexpected(std::move(place.error()));
    }
    lir::Place contents = *std::move(place);
    contents.chain.emplace_back(lir::DerefProjection{});
    return contents;
  }
  auto pointer = WrapperOperand(block, wrapper);
  if (!pointer) {
    return std::unexpected(std::move(pointer.error()));
  }
  // A write in progress into an object is dereferenced to that object, as a
  // guard is to what it guards; the library holds which object it is.
  if (const auto* write = wrapper_ty.As<mir::ObjectWriteType>()) {
    auto object = EmitCallTo(
        lir::BuiltinTarget{.fn = support::BuiltinFn::kWrittenObject},
        {*std::move(pointer)},
        AddressType(unit_->TranslateType(write->object)));
    if (!object) {
      return std::unexpected(std::move(object.error()));
    }
    return StorageAt(*std::move(object));
  }
  // Opening a reference lands on the value the storage it names holds, which
  // is what the reference states; which of the two forms that storage is, is
  // the reference's own to answer, so every access through this place is one
  // of its operations rather than a load or a store of an address.
  if (wrapper_ty.IsVariableRef()) {
    return StorageAt(*std::move(pointer));
  }
  return lir::Place{
      .base = *std::move(pointer),
      .chain = {lir::Projection{lir::DerefProjection{}}}};
}

auto FunctionLowerer::ReferenceValue(
    const mir::ReferenceTarget& target, mir::TypeId type)
    -> diag::Result<lir::Operand> {
  // What names storage is read by a read, which reads its place.
  const auto storage = []() -> diag::Result<lir::Operand> {
    throw InternalError(
        "mir_to_lir: a name of storage stands as a value outside a read -- "
        "please report this as a bug");
  };
  return std::visit(
      Overloaded{
          [&](const mir::LocalRef&) { return storage(); },
          [&](const mir::StaticVariableRef&) { return storage(); },
          [&](const mir::StaticPropertyRef&) { return storage(); },
          [&](const mir::EnumTableRef& ref) -> diag::Result<lir::Operand> {
            return lir::Operand{lir::EnumTableRef{
                .table = UnitLowerer::TranslateEnumTable(ref.table),
                .type = unit_->TranslateType(type)}};
          },
          [&](const mir::IntegralConstantRef& ref)
              -> diag::Result<lir::Operand> {
            return lir::Operand{lir::IntegralConstantRef{
                .constant = UnitLowerer::TranslateConstant(ref.constant),
                .type = unit_->TranslateType(type)}};
          },
          [&](const mir::DefinitionRef&) -> diag::Result<lir::Operand> {
            throw InternalError(
                "mir_to_lir: a class's definition is read whole, where only "
                "its address is ever taken -- please report this as a bug");
          },
          // Only a constant names another constant, and a constant is data
          // rather than a body.
          [&](const mir::ClassConstantRef&) -> diag::Result<lir::Operand> {
            throw InternalError(
                "mir_to_lir: a body reads a class's constant -- please report "
                "this as a bug");
          },
          // Read as a value, a body is its address.
          [&](const mir::FunctionRef& ref) -> diag::Result<lir::Operand> {
            return lir::Operand{lir::FunctionRef{
                .function =
                    unit_->MethodFunction(ref.body.owner, ref.body.slot),
                .type = unit_->TranslateType(type)}};
          },
          // Only a class's definition holds one, and a definition is data
          // rather than a body.
          [&](const mir::DispatchEntryRef&) -> diag::Result<lir::Operand> {
            throw InternalError(
                "mir_to_lir: a body reads the entry of a method -- please "
                "report this as a bug");
          },
          [&](const mir::DestructorRef&) -> diag::Result<lir::Operand> {
            throw InternalError(
                "mir_to_lir: a body reads what ends a class's objects -- "
                "please report this as a bug");
          },
          [&](const mir::DispatchTableRef&) -> diag::Result<lir::Operand> {
            throw InternalError(
                "mir_to_lir: a body reads a class's table -- please report "
                "this as a bug");
          },
          [&](const mir::InterfaceTableRef&) -> diag::Result<lir::Operand> {
            throw InternalError(
                "mir_to_lir: a body reads a class's table -- please report "
                "this as a bug");
          },
          [&](const mir::ExternalObjectEntryRef&)
              -> diag::Result<lir::Operand> {
            throw InternalError(
                "mir_to_lir: a body names another unit's entry as a value -- "
                "please report this as a bug");
          }},
      target);
}

auto FunctionLowerer::SymbolPlace(std::string symbol, mir::TypeId type)
    -> lir::Place {
  return lir::Place{
      .base =
          lir::StaticRef{
              .symbol = std::move(symbol),
              .type = AddressType(unit_->TranslateType(type))},
      .chain = {lir::Projection{lir::DerefProjection{}}}};
}

auto FunctionLowerer::ReferencePlace(
    const mir::ReferenceTarget& target, mir::TypeId type, Reach reach)
    -> diag::Result<lir::Place> {
  return std::visit(
      Overloaded{
          [&](const mir::LocalRef& ref) -> diag::Result<lir::Place> {
            const std::optional<LocalBinding>& binding = locals_[ref.var.value];
            if (!binding.has_value()) {
              return Unsupported(
                  "a reference to a local variable that has no storage at "
                  "this point");
            }
            return std::visit(
                Overloaded{
                    [&](const PlaceBinding& place) -> diag::Result<lir::Place> {
                      return LocalPlace(place.slot);
                    },
                    // The binding holds a reference to the cell the local
                    // lives in, so the local's storage is what that reference
                    // points at: one step to the cell, one more to its value.
                    // The cell reports its writes, which the source's plain
                    // variable does not say, so a write into part of it is
                    // opened on the cell here.
                    [&](const CellBinding& cell) -> diag::Result<lir::Place> {
                      // A cell MIR declared is named as the cell, whose own
                      // operations read and write it, as a design variable's
                      // cell is.
                      if (IsDeclaredCell(
                              unit_->Mir(), code_->locals.Get(ref.var).type)) {
                        return StorageAt(cell.cell);
                      }
                      if (!WritesInto(reach)) {
                        return ValueAt(cell.cell);
                      }
                      const lir::TypeId value =
                          unit_->TranslateType(code_->locals.Get(ref.var).type);
                      auto opened = OpenWrite(cell.cell, value);
                      if (!opened) {
                        return std::unexpected(std::move(opened.error()));
                      }
                      return Land(*std::move(opened), value);
                    }},
                *binding);
          },
          // A variable of a unit's namespace is one cell for the whole program
          // that no instance holds, so it is reached by the symbol it links
          // under rather than through a receiver -- by the unit that owns it
          // exactly as by any other, since a namespace has no instance.
          [&](const mir::StaticVariableRef& ref) -> diag::Result<lir::Place> {
            return SymbolPlace(
                unit_->StaticVariableSymbolOf(ref.variable), type);
          },
          // A cell a class owns rather than an object of it (LRM 8.9) is that
          // same one cell for the whole program, so it is reached the same way
          // and differs only in how far its name is qualified. The class the
          // reference names is the one that declares the cell, however the
          // source spelled it, so the symbol is settled without reading what
          // any other class holds.
          [&](const mir::StaticPropertyRef& ref) -> diag::Result<lir::Place> {
            return SymbolPlace(unit_->StaticPropertySymbolOf(ref), type);
          },
          // A class's definition is a constant the declaring unit emits, so
          // the place opens at its address.
          [&](const mir::DefinitionRef& r) -> diag::Result<lir::Place> {
            return lir::Place{
                .base =
                    lir::DefinitionRef{
                        .defined = unit_->ClassValueType(r.of),
                        .type = unit_->Types().Intern(
                            lir::Type{lir::PointerType{
                                .pointee = unit_->TranslateType(type),
                                .ownership = lir::PointerOwnership::kBorrowed,
                                .mutability = lir::Mutability::kReadOnly}})},
                .chain = {lir::Projection{lir::DerefProjection{}}}};
          },
          // A class's constant is data its unit emits under a symbol, so the
          // place opens at its address, through which it is only read.
          [&](const mir::ClassConstantRef& r) -> diag::Result<lir::Place> {
            return lir::Place{
                .base =
                    lir::StaticRef{
                        .symbol = unit_->ClassConstantSymbolOf(r),
                        .type = unit_->Types().Intern(
                            lir::Type{lir::PointerType{
                                .pointee = unit_->TranslateType(type),
                                .ownership = lir::PointerOwnership::kBorrowed,
                                .mutability = lir::Mutability::kReadOnly}})},
                .chain = {lir::Projection{lir::DerefProjection{}}}};
          },
          // A member table, a constant value and a function are values the
          // unit holds, not storage anything writes through.
          [](const mir::EnumTableRef&) -> diag::Result<lir::Place> {
            return Unsupported("an enumeration's member table names no place");
          },
          [](const mir::IntegralConstantRef&) -> diag::Result<lir::Place> {
            return Unsupported("a constant names no place");
          },
          [](const mir::FunctionRef&) -> diag::Result<lir::Place> {
            return Unsupported("a function names no place");
          },
          [](const mir::DispatchEntryRef&) -> diag::Result<lir::Place> {
            return Unsupported("a function names no place");
          },
          [](const mir::DestructorRef&) -> diag::Result<lir::Place> {
            return Unsupported("a function names no place");
          },
          [](const mir::DispatchTableRef&) -> diag::Result<lir::Place> {
            return Unsupported("a table names no place");
          },
          [](const mir::InterfaceTableRef&) -> diag::Result<lir::Place> {
            return Unsupported("a table names no place");
          },
          [](const mir::ExternalObjectEntryRef&) -> diag::Result<lir::Place> {
            return Unsupported("a function names no place");
          }},
      target);
}

auto FunctionLowerer::LowerPlace(
    const mir::Block& block, mir::ExprId id, Reach reach)
    -> diag::Result<lir::Place> {
  const mir::Expr& expr = block.exprs.Get(id);
  const auto names_no_place =
      [](std::string_view form) -> diag::Result<lir::Place> {
    return Unsupported("{} names no place", form);
  };
  return std::visit(
      Overloaded{
          [&](const mir::ReferenceExpr& reference) -> diag::Result<lir::Place> {
            return ReferencePlace(reference.target, expr.type, reach);
          },
          [&](const mir::FieldAccessExpr& field) -> diag::Result<lir::Place> {
            auto place =
                PlaceHolding(block, field.receiver, HolderReach(reach));
            if (!place) {
              return std::unexpected(std::move(place.error()));
            }
            place->chain.push_back(FieldStep(field.field));
            return place;
          },
          // An element a reference into its whole reaches is a step of the
          // place the whole lies in, so the chain names every step a load
          // reaches through, a member of an untagged union among them.
          [&](const mir::DerefExpr& deref) -> diag::Result<lir::Place> {
            if (const mir::CallExpr* element = ReferencedPartCall(block, id)) {
              return ElementPlace(block, *element, reach);
            }
            return WrapperContentsPlace(block, deref.pointer);
          },
          // Every other form answers with a value rather than naming where one
          // is kept, so binding one would need storage holding that value
          // first, which is a form the source did not write.
          [&](const mir::StringLiteral&) {
            return names_no_place("a literal");
          },
          [&](const mir::NullLiteral&) { return names_no_place("a literal"); },
          [&](const mir::MachineBoolLiteral&) {
            return names_no_place("a literal");
          },
          [&](const mir::MachineIntLiteral&) {
            return names_no_place("a literal");
          },
          [&](const mir::MachineFloatLiteral&) {
            return names_no_place("a literal");
          },
          [&](const mir::UnaryExpr&) {
            return names_no_place("the result of an operator");
          },
          [&](const mir::BinaryExpr&) {
            return names_no_place("the result of an operator");
          },
          [&](const mir::CastExpr&) {
            return names_no_place("a converted value");
          },
          [&](const mir::ConditionalExpr&) {
            return names_no_place("a value a condition chooses");
          },
          [&](const mir::BlockExpr&) {
            return names_no_place("the value a sequence of steps settles");
          },
          [&](const mir::AssignExpr&) { return names_no_place("a write"); },
          [&](const mir::CallExpr&) {
            return names_no_place("a call's result");
          },
          [&](const mir::NewExpr&) {
            return names_no_place("the owner of storage just allocated");
          },
          [&](const mir::AddressOfExpr&) {
            return names_no_place("an address of storage");
          },
          [&](const mir::BorrowExpr&) {
            return names_no_place("a reference to storage");
          },
          [&](const mir::MoveExpr&) {
            return names_no_place("a value moved out of where it was kept");
          },
          [&](const mir::CopyExpr&) {
            return names_no_place("a value read out of a place");
          },
          [&](const mir::ClosureExpr&) {
            return names_no_place("an anonymous function");
          },
          [&](const mir::CompositeExpr&) {
            return names_no_place("a value composed of its parts");
          },
          [&](const mir::UnionExpr&) {
            return names_no_place("a union built holding a member");
          },
          [&](const mir::AwaitExpr&) {
            return names_no_place("the value an await settles");
          },
          [&](const mir::WaitExpr&) {
            return names_no_place("a wait, which yields nothing");
          },
          [&](const mir::VectorGetExpr&) {
            return names_no_place("the handle a sequence answers with");
          },
          [&](const mir::DiscriminantExpr&) {
            return names_no_place("the member a tagged union holds");
          }},
      expr.data);
}

auto FunctionLowerer::LowerRead(const mir::Block& block, mir::ExprId place)
    -> diag::Result<lir::Operand> {
  const lir::TypeId type = unit_->TranslateType(block.exprs.Get(place).type);
  if (unit_->Types().Get(type).IsAddressOnly()) {
    return Unsupported(
        "a storage cell has no value to read; it is reached "
        "through its address");
  }
  auto named = LowerPlace(block, place, Reach::kRead);
  if (!named) {
    return std::unexpected(std::move(named.error()));
  }
  if (mir::KindOfPlace(unit_->Mir(), block, place) ==
      mir::PlaceKind::kOverlaid) {
    return Emit(type, lir::OverlaidLoadInstr{.place = *std::move(named)});
  }
  return Load(*std::move(named), type);
}

auto FunctionLowerer::LowerArgument(const mir::Block& block, mir::ExprId id)
    -> diag::Result<lir::Operand> {
  // The parameter's type decides. A parameter of an address-only type asks for
  // the storage itself -- a cell, a scope -- so the argument is where it lives,
  // not a reading of it. This is a fact about what the callee takes, not about
  // the expression that produced the place.
  const lir::TypeId type = unit_->TranslateType(block.exprs.Get(id).type);
  if (!unit_->Types().Get(type).IsAddressOnly()) {
    // A value with no copy is handed to the callee, which owns it from there;
    // every other value is lent for the call.
    auto value = LowerExpr(block, id);
    if (!value || !unit_->Types().Get(type).IsMoveOnly()) {
      return value;
    }
    return HandOn(*std::move(value));
  }
  auto place = LowerPlace(block, id, Reach::kWhole);
  if (!place) {
    return std::unexpected(std::move(place.error()));
  }
  return AddressOf(*std::move(place), type);
}

auto FunctionLowerer::AddressOf(lir::Place place, lir::TypeId type)
    -> lir::Operand {
  return Emit(AddressType(type), lir::AddrOfInstr{.place = std::move(place)});
}

auto FunctionLowerer::LowerEachExpr(
    const mir::Block& block, std::span<const mir::ExprId> ids)
    -> diag::Result<std::vector<lir::Operand>> {
  std::vector<lir::Operand> operands;
  operands.reserve(ids.size());
  for (const mir::ExprId id : ids) {
    auto lowered = LowerExpr(block, id);
    if (!lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
    operands.push_back(*std::move(lowered));
  }
  return operands;
}

auto FunctionLowerer::LowerCallOperands(
    const mir::Block& block, const mir::CallExpr& call)
    -> diag::Result<std::vector<lir::Operand>> {
  std::vector<lir::Operand> args;
  args.reserve(call.arguments.size() + 1);
  const auto lower_into = [&](mir::ExprId id) -> diag::Result<void> {
    auto lowered = LowerArgument(block, id);
    if (!lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
    args.push_back(*std::move(lowered));
    return {};
  };
  // MIR names the object a call is entered on where it lies, or as a value it
  // read. An entry takes an object holding a value by that value, so one named
  // where it lies is read there, and storage with no value to read is handed on
  // by its address.
  if (const std::optional<mir::ExprId> receiver =
          mir::CalleeReceiver(call.callee)) {
    auto lowered =
        mir::IsReadablePlace(unit_->Mir().types, block.exprs.Get(*receiver))
            ? LowerRead(block, *receiver)
            : LowerArgument(block, *receiver);
    if (!lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
    args.push_back(*std::move(lowered));
  }
  // An entry acting on an object is handed whatever reaches it, and takes the
  // object itself, so that operand is opened the way an access's receiver is.
  std::span<const mir::ExprId> rest = call.arguments;
  if (const auto fn = mir::DirectBuiltinFn(call);
      fn.has_value() && support::RuntimeEntryOf(*fn).reaches_an_object) {
    auto object = ObjectAddress(block, rest.front());
    if (!object) {
      return std::unexpected(std::move(object.error()));
    }
    args.push_back(*std::move(object));
    rest = rest.subspan(1);
  }
  for (const mir::ExprId argument : rest) {
    if (auto lowered = lower_into(argument); !lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
  }
  return args;
}

auto FunctionLowerer::ObjectAddress(
    const mir::Block& block, mir::ExprId reaches)
    -> diag::Result<lir::Operand> {
  auto part = WrapperContentsPlace(block, reaches);
  if (!part) {
    return std::unexpected(std::move(part.error()));
  }
  const lir::Operand reached = AddressOf(
      *std::move(part),
      unit_->TranslateType(
          mir::ObjectReachedThrough(
              unit_->Mir().types, block.exprs.Get(reaches).type)));
  // The library takes an object as the part every object starts with, which
  // lies where the object does whatever class it is reached as.
  return Emit(
      AddressType(unit_->Types().Intern(
          lir::Type{
              lir::RuntimeClassType{.which = support::RuntimeClass::kObject}})),
      lir::CastInstr{.operand = reached});
}

auto FunctionLowerer::LowerNew(
    const mir::Block& block, const mir::NewExpr& allocation, mir::TypeId owner)
    -> diag::Result<lir::Operand> {
  const mir::Expr& initializer = block.exprs.Get(allocation.initializer);
  if (unit_->Mir().types.Get(initializer.type).Is<mir::ObjectType>()) {
    return LowerObjectNew(block, initializer, owner);
  }
  if (unit_->Mir().types.Get(initializer.type).Is<mir::ObservableType>()) {
    return LowerCellNew(initializer, owner);
  }
  const lir::TypeId held = unit_->TranslateType(initializer.type);
  if (allocation.placement.has_value()) {
    return LowerPlacementNew(block, allocation, held);
  }
  // A value that has a value apart from where it lies is made in the storage
  // `operator new` allocates, as clang emits an initializer into it, so a way
  // out before it is made gives the storage back; only once it holds the value
  // is the owner made, as `std::unique_ptr` is given what a C++ `new` built.
  const lir::Operand storage = Emit(
      AddressType(held),
      lir::CallInstr{
          .target =
              lir::DynamicStorageTarget{
                  .op = lir::DynamicStorageTarget::Op::kAllocate},
          .args = {}});
  const std::size_t released_at = scopes_.size();
  OpenScope(StorageRelease{.storage = storage});
  if (auto made =
          LowerExprInto(block, allocation.initializer, StorageAt(storage));
      !made) {
    return std::unexpected(std::move(made.error()));
  }
  PassEndAt(released_at);
  return EmitCallTo(
      lir::ConstructTarget{}, {storage}, unit_->TranslateType(owner));
}

auto FunctionLowerer::LowerPlacementNew(
    const mir::Block& block, const mir::NewExpr& allocation, lir::TypeId held)
    -> diag::Result<lir::Operand> {
  // The placement's arguments, then the size and the alignment a value of
  // `held` is laid out at, as the entry declares them; clang hands a placement
  // `operator new` the same facts, then forms the initializer in what it
  // answers. The function owns that storage, so a way out before the
  // value is made gives nothing back: no deallocation function matches, as in
  // C++.
  std::vector<lir::Operand> arguments;
  for (const mir::ExprId argument : allocation.placement->arguments) {
    auto lowered = LowerExpr(block, argument);
    if (!lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
    arguments.push_back(*std::move(lowered));
  }
  const lir::TypeId number =
      unit_->TranslateType(unit_->Mir().builtins.machine_int64);
  for (const lir::LayoutConst::Measure measure :
       {lir::LayoutConst::Measure::kSize,
        lir::LayoutConst::Measure::kAlignment}) {
    arguments.emplace_back(
        lir::LayoutConst{.of = held, .measure = measure, .type = number});
  }
  auto storage = EmitCallTo(
      lir::BuiltinTarget{.fn = allocation.placement->allocate},
      std::move(arguments), AddressType(held));
  if (!storage) {
    return storage;
  }
  if (auto made =
          LowerExprInto(block, allocation.initializer, StorageAt(*storage));
      !made) {
    return std::unexpected(std::move(made.error()));
  }
  return storage;
}

auto FunctionLowerer::LowerCellNew(
    const mir::Expr& initializer, mir::TypeId owner)
    -> diag::Result<lir::Operand> {
  // A cell has no value apart from where it lies, as an object has none, so
  // it is built in the storage allocated for it rather than made and put in.
  const auto* construction = std::get_if<mir::CallExpr>(&initializer.data);
  if (construction == nullptr ||
      !std::holds_alternative<mir::Construct>(construction->callee) ||
      !construction->arguments.empty()) {
    return Unsupported("a cell is made only empty, as a variable is declared");
  }
  const lir::Operand storage = Emit(
      AddressType(unit_->TranslateType(initializer.type)),
      lir::CallInstr{
          .target =
              lir::DynamicStorageTarget{
                  .op = lir::DynamicStorageTarget::Op::kAllocate},
          .args = {}});
  Emit(
      unit_->TranslateType(unit_->Mir().builtins.void_type),
      lir::CallInstr{.target = lir::BuildCellTarget{}, .args = {storage}});
  return EmitCallTo(
      lir::ConstructTarget{}, {storage}, unit_->TranslateType(owner));
}

auto FunctionLowerer::LowerObjectNew(
    const mir::Block& block, const mir::Expr& initializer, mir::TypeId owner)
    -> diag::Result<lir::Operand> {
  // An object has no value apart from where it lies, so its initializer is its
  // construction. A construction carries every argument its constructor takes
  // -- the source wrote the call, so the front end bound it against the
  // declaration and filled in whatever it left to a default.
  const auto* construction = std::get_if<mir::CallExpr>(&initializer.data);
  if (construction == nullptr ||
      !std::holds_alternative<mir::Construct>(construction->callee)) {
    return Unsupported("an object is initialized only by its constructor");
  }
  // The arguments come first, so a way out while they are evaluated owes
  // nothing for an object.
  std::vector<lir::Operand> arguments;
  arguments.reserve(construction->arguments.size());
  for (const mir::ExprId argument : construction->arguments) {
    auto lowered = LowerArgument(block, argument);
    if (!lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
    arguments.push_back(*std::move(lowered));
  }
  // The object is then created whole -- every member of every class it is
  // built of, and its class -- which runs none of the design's
  // code, and its creator holds it from there. A construction left partway
  // ends that hold as any value this body owns is ended, and the object goes
  // on existing while anything else refers to it (LRM 8.29).
  const lir::TypeId object_type = unit_->TranslateType(initializer.type);
  const lir::Operand storage = Emit(
      AddressType(object_type),
      lir::CallInstr{
          .target = lir::CreationTarget{.cls = object_type}, .args = {}});
  auto held = EmitCallTo(
      lir::ConstructTarget{}, {storage}, unit_->TranslateType(owner));
  if (!held) {
    return held;
  }
  auto constructed = EnterConstructor(
      mir::ClassOfObject(unit_->Mir().types, initializer.type), storage,
      std::move(arguments));
  if (!constructed) {
    return std::unexpected(std::move(constructed.error()));
  }
  return held;
}

auto FunctionLowerer::EnterConstructor(
    mir::ClassId cls, const lir::Operand& object,
    std::vector<lir::Operand> arguments) -> diag::Result<void> {
  const EnteredConstructor constructor = ConstructorOf(cls);
  // The object leads its arguments the way a receiver leads any body's
  // parameters, and what built it names it rather than being it, so opening
  // that is what reaches the storage the body runs on.
  arguments.insert(
      arguments.begin(),
      Emit(
          AddressType(constructor.object_type),
          lir::AddrOfInstr{
              .place = lir::Place{
                  .base = object,
                  .chain = {lir::Projection{lir::DerefProjection{}}}}}));
  auto entered = EmitCallTo(
      constructor.callee, std::move(arguments),
      unit_->TranslateType(unit_->Mir().builtins.void_type));
  if (!entered) {
    return std::unexpected(std::move(entered.error()));
  }
  return {};
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

auto FunctionLowerer::LowerReferenceIntoReceiver(
    const mir::Block& block, const mir::CallExpr& call, mir::TypeId type)
    -> diag::Result<lir::Operand> {
  auto place = ElementPlace(
      block, call,
      BorrowReach(
          unit_->Mir().types.Get(type).Get<mir::ReferenceType>().mutability));
  if (!place) {
    return std::unexpected(std::move(place.error()));
  }
  return Emit(
      unit_->TranslateType(type), lir::AddrOfInstr{.place = *std::move(place)});
}

auto FunctionLowerer::LowerReferenceToContents(
    const mir::Block& block, const mir::CallExpr& call, mir::TypeId type)
    -> diag::Result<lir::Operand> {
  const std::optional<mir::ExprId> wrapper = mir::CalleeReceiver(call.callee);
  if (!wrapper.has_value()) {
    throw InternalError(
        "mir_to_lir: what a wrapper holds is asked of a wrapper, and this call "
        "names none -- please report this as a bug");
  }
  auto contents = WrapperContentsPlace(block, *wrapper);
  if (!contents) {
    return std::unexpected(std::move(contents.error()));
  }
  return Emit(
      unit_->TranslateType(type),
      lir::AddrOfInstr{.place = *std::move(contents)});
}

auto FunctionLowerer::LowerPropertyReference(
    const mir::Block& block, const mir::CallExpr& call, mir::TypeId type)
    -> diag::Result<lir::Operand> {
  const auto& direct = std::get<mir::Direct>(call.callee);
  if (call.arguments.size() != 1 || !direct.part.has_value()) {
    throw InternalError(
        "mir_to_lir: a reference to a property is a step taken on its object, "
        "naming the property -- please report this as a bug");
  }
  const mir::FieldRef property = std::visit(
      Overloaded{
          [](base::ComponentIndex) -> mir::FieldRef {
            throw InternalError(
                "mir_to_lir: a reference to a property names a property, and "
                "this one names a position -- please report this as a bug");
          },
          [](const mir::ClassFieldTarget& t) -> mir::FieldRef { return t; }},
      *direct.part);
  // The object is reached once, as every entry acting on an object reaches it,
  // and both what holds the storage and where the storage is are read off it,
  // the way a member access reaches its storage through its receiver.
  auto args = LowerCallOperands(block, call);
  if (!args) {
    return std::unexpected(std::move(args.error()));
  }
  const lir::TypeId value = unit_->TranslateType(
      unit_->Mir().types.Get(type).Get<mir::VariableRefType>().pointee);
  args->push_back(AddressOf(
      lir::Place{
          .base = args->front(),
          .chain =
              {lir::Projection{lir::DerefProjection{}}, FieldStep(property)}},
      value));
  return EmitCallTo(
      lir::BuiltinTarget{.fn = support::BuiltinFn::kReferProperty},
      *std::move(args), unit_->TranslateType(type));
}

auto FunctionLowerer::LowerObjectWriteOpening(
    const mir::Block& block, const mir::CallExpr& call, mir::TypeId type)
    -> diag::Result<lir::Operand> {
  if (call.arguments.size() != 1) {
    throw InternalError(
        "mir_to_lir: a write into an object is opened on the one object it "
        "writes -- please report this as a bug");
  }
  auto object = ObjectAddress(block, call.arguments.front());
  if (!object) {
    return object;
  }
  return EmitCallTo(
      lir::BuiltinTarget{.fn = support::BuiltinFn::kOpenObjectWrite},
      {*std::move(object)}, unit_->TranslateType(type));
}

auto FunctionLowerer::LowerCellPlace(
    const mir::Block& block, mir::ExprId referent) -> diag::Result<lir::Place> {
  const mir::Expr& expr = block.exprs.Get(referent);
  // A local the body gave storage of its own is named by the handle its
  // declaration took on that storage, which reaches the storage in one step
  // where reading the local reaches its value in two.
  if (const std::optional<mir::LocalId> local = mir::ReferencedLocal(expr.data);
      local.has_value() && locals_[local->value].has_value()) {
    if (const auto* cell = std::get_if<CellBinding>(&*locals_[local->value])) {
      return StorageAt(cell->cell);
    }
  }
  // Every other referent is named the way a write to it names it: what a
  // reference binds and what a store descends into are the same storage, so
  // one answer serves both, and a referent that names no storage meets its
  // refusal where naming is decided.
  return LowerPlace(block, referent, Reach::kWhole);
}

auto FunctionLowerer::LowerCallInto(
    const mir::Block& block, const mir::CallExpr& call, mir::TypeId type,
    const lir::Destination& destination) -> diag::Result<void> {
  using Lowered = diag::Result<void>;
  // The forms below answer with something other than the value a call makes,
  // which is then put where it goes.
  const auto settled = [&](diag::Result<lir::Operand> value) -> Lowered {
    if (!value) {
      return std::unexpected(std::move(value.error()));
    }
    AssignFrom(destination, *std::move(value));
    return {};
  };

  // A store through a reference to bits writes them where the reference was
  // taken (LRM 11.5.1).
  if (const std::optional<mir::ExprId> receiver =
          mir::CalleeReceiver(call.callee);
      receiver.has_value() && unit_->Mir()
                                  .types.Get(block.exprs.Get(*receiver).type)
                                  .Is<mir::BitsReferenceType>()) {
    return settled(LowerBitsStore(block, *receiver, call.arguments.front()));
  }
  // A method that changes the object it is applied to changes it where it
  // lies, which is a question of where the receiver is.
  if (const auto fn = mir::DirectBuiltinFn(call);
      fn.has_value() && support::RuntimeEntryOf(*fn).mutates_receiver) {
    return settled(LowerMutatingCall(block, call, *fn, type));
  }

  if (mir::AnswersReferenceIntoReceiver(call)) {
    return settled(LowerReferenceIntoReceiver(block, call, type));
  }
  if (AnswersReferenceToContents(call)) {
    return settled(LowerReferenceToContents(block, call, type));
  }

  // A reference is the address of the cell it binds, so building one is the
  // ordinary address-of over the referent's place. No runtime value stands
  // between the holder and that cell; reading and writing through the reference
  // are the cell's own access, reached the way every other call is.
  if (mir::BindsReference(unit_->Mir().types, call, type)) {
    return settled(LowerReferenceBind(block, call, type));
  }
  if (OpensObjectWrite(unit_->Mir().types, call, type)) {
    return settled(LowerObjectWriteOpening(block, call, type));
  }

  // Reached where nothing awaits the execution -- a process handed to the
  // scheduler. Such a body finishes with no value, so there is nothing for it
  // to complete into; the awaiting form supplies that storage itself. A library
  // entry building the execution is called as any entry is.
  if (unit_->Mir().types.Get(type).Is<mir::CoroutineType>() &&
      !EntryBuildsExecution(call)) {
    return settled(EnterCoroutine(block, call, type, std::nullopt));
  }

  if (const auto fn = mir::DirectBuiltinFn(call); fn.has_value()) {
    // A closure made one of no known type is the one value read as another
    // type: this target's closure opens with its definition, so the address
    // it lies at is the erased closure's too, as a C++ object's is its first
    // base's.
    if (*fn == support::BuiltinFn::kUnsize) {
      return settled(LowerReadAsAnother(block, call.arguments.front(), type));
    }
    if (*fn == support::BuiltinFn::kReferProperty) {
      return settled(LowerPropertyReference(block, call, type));
    }
    // A foreign caller cannot be parked, so the entry point it reached drives
    // the body to its end where it stands instead of waiting for it (LRM 35.8).
    if (*fn == support::BuiltinFn::kRunExportedTaskToCompletion) {
      return settled(LowerDriveToCompletion(block, call, type));
    }
    if (*fn == support::BuiltinFn::kTakeDepartureIfDue) {
      auto checked = TakeDepartureIfDue();
      if (!checked) {
        return std::unexpected(std::move(checked.error()));
      }
      // The gate answers nothing.
      return settled(VoidValue());
    }
  }

  auto args = LowerCallOperands(block, call);
  if (!args) {
    return std::unexpected(std::move(args.error()));
  }
  auto target = LowerCallTarget(block, call.callee);
  if (!target) {
    return std::unexpected(std::move(target.error()));
  }
  return CallInto(*std::move(target), *std::move(args), destination);
}

auto FunctionLowerer::LowerDriveToCompletion(
    const mir::Block& block, const mir::CallExpr& call, mir::TypeId type)
    -> diag::Result<lir::Operand> {
  std::optional<lir::Operand> completion_slot;
  if (type != unit_->Mir().builtins.void_type) {
    completion_slot = AllocateCompletionFor(unit_->TranslateType(type));
  }
  const mir::Expr& body = block.exprs.Get(call.arguments.front());
  const auto* frame = std::get_if<mir::CallExpr>(&body.data);
  if (frame == nullptr) {
    throw InternalError(
        "mir_to_lir: an execution driven to completion is entered from the "
        "call that builds its frame -- please report this as a bug");
  }
  auto activation = EnterCoroutine(block, *frame, body.type, completion_slot);
  if (!activation) {
    return activation;
  }
  const lir::Operand driven = *std::move(activation);
  auto drove = EmitCallTo(
      lir::BuiltinTarget{
          .fn = support::BuiltinFn::kRunExportedTaskToCompletion},
      {driven}, unit_->TranslateType(unit_->Mir().builtins.void_type));
  if (!drove) {
    return std::unexpected(std::move(drove.error()));
  }
  // Driving a body to its end is a point where this execution regains control,
  // and the body may have ended in a way that leaves nothing to read: a
  // run-time error of the design reported here rather than travelling, since
  // the frame above is foreign code.
  auto checked = TakeDepartureIfDue();
  if (!checked) {
    return std::unexpected(std::move(checked.error()));
  }
  if (completion_slot.has_value()) {
    return LoadActivationValue(*completion_slot, unit_->TranslateType(type));
  }
  return VoidValue();
}

auto FunctionLowerer::EnterCoroutine(
    const mir::Block& block, const mir::CallExpr& call, mir::TypeId type,
    std::optional<lir::Operand> completion) -> diag::Result<lir::Operand> {
  auto args = LowerCallOperands(block, call);
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
  auto frame = EmitCall(block, call, *std::move(args), result_type);
  if (!frame) {
    return std::unexpected(std::move(frame.error()));
  }
  return Emit(
      result_type,
      lir::CallInstr{
          .target =
              lir::CoroutineTarget{.op = lir::CoroutineTarget::Op::kEnter},
          .args = {*std::move(frame)}});
}

auto FunctionLowerer::EmitCall(
    const mir::Block& block, const mir::CallExpr& call,
    std::vector<lir::Operand> args, lir::TypeId result_type)
    -> diag::Result<lir::Operand> {
  auto target = LowerCallTarget(block, call.callee);
  if (!target) {
    return std::unexpected(std::move(target.error()));
  }
  return EmitCallTo(*std::move(target), std::move(args), result_type);
}

auto FunctionLowerer::EmitCallTo(
    lir::CallTarget target, std::vector<lir::Operand> args,
    lir::TypeId result_type) -> diag::Result<lir::Operand> {
  const lir::ValueId result = NewTransient(result_type);
  auto called = CallInto(
      std::move(target), std::move(args), lir::Transient{.value = result});
  if (!called) {
    return std::unexpected(std::move(called.error()));
  }
  return lir::Use{.value = result};
}

auto FunctionLowerer::CallInto(
    lir::CallTarget target, std::vector<lir::Operand> args,
    lir::Destination destination) -> diag::Result<void> {
  // A call answering with where a value lies rather than with a value it made
  // puts it in storage only as a value of the storage's own, copied in, as any
  // read does.
  if (const auto* place = std::get_if<lir::Place>(&destination);
      place != nullptr && !lir::CallMakesValue(target)) {
    const lir::ValueId answer = fn_.values.Add(
        lir::Local{
            .name = {},
            .type = lir::PlaceType(unit_->Lir(), fn_, *place),
            .kind = lir::LocalKind::kTemp});
    auto called = CallInto(
        std::move(target), std::move(args), lir::Transient{.value = answer});
    if (!called) {
      return called;
    }
    AssignFrom(std::move(destination), lir::Use{.value = answer});
    return {};
  }
  switch (lir::CallEndingOf(target)) {
    case support::CallEnding::kReturns:
      Assign(
          std::move(destination),
          lir::CallInstr{.target = std::move(target), .args = std::move(args)});
      return {};
    case support::CallEnding::kReturnsOrUnwinds:
      return EmitInvoke(
          std::move(target), std::move(args), std::move(destination));
    case support::CallEnding::kUnwinds: {
      auto unwound = EmitInvoke(
          std::move(target), std::move(args), std::move(destination));
      if (!unwound) {
        return unwound;
      }
      // Nothing comes back to the point the call returns to, so whatever the
      // source wrote after the call is never reached and has no lowering.
      Terminate(lir::UnreachableTerm{});
      return {};
    }
  }
  throw InternalError("mir_to_lir: unknown call ending");
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

  const mir::Expr& execution = block.exprs.Get(await.execution);
  const auto* direct = std::get_if<mir::CallExpr>(&execution.data);
  if (direct == nullptr && completion_slot.has_value()) {
    return Unsupported(
        "an awaited execution that completes with a value must be "
        "a call, so that the place to complete into can be handed to it");
  }
  auto activation =
      direct != nullptr
          ? EnterCoroutine(block, *direct, execution.type, completion_slot)
          : LowerExpr(block, await.execution);
  if (!activation) {
    return activation;
  }
  // Handing the thread over runs the awaited body at once (LRM 13.3), so one
  // that consumes no time has already settled when control comes back and
  // there is nothing left to wait for; the answer says which of the two
  // happened.
  auto awaited = EmitCallTo(
      lir::CoroutineTarget{.op = lir::CoroutineTarget::Op::kAwait},
      {CurrentRuntime(), *std::move(activation)}, unit_->MachineBoolType());
  if (!awaited) {
    return awaited;
  }
  auto suspended = SuspendIf(*std::move(awaited));
  if (!suspended) {
    return std::unexpected(std::move(suspended.error()));
  }

  std::optional<lir::Operand> completion;
  if (completion_slot.has_value()) {
    completion =
        LoadActivationValue(*completion_slot, unit_->TranslateType(type));
  }
  // Taking the thread back ends the awaited execution, before anything else
  // this one does: what runs next may await again, and a thread carries one
  // awaited execution at a time. What the awaited body raised is raised again
  // here, so this is a point the execution can leave from.
  auto released = EmitCallTo(
      lir::CoroutineTarget{.op = lir::CoroutineTarget::Op::kRelease},
      {CurrentRuntime()},
      unit_->TranslateType(unit_->Mir().builtins.void_type));
  if (!released) {
    return released;
  }

  // Having the thread back is a point where this execution regains control, so
  // a target it is inside may have been disabled while it was away.
  auto checked = TakeDepartureIfDue();
  if (!checked) {
    return std::unexpected(std::move(checked.error()));
  }
  return completion.value_or(VoidValue());
}

auto FunctionLowerer::LowerAssign(
    const mir::Block& block, const mir::AssignExpr& assign)
    -> diag::Result<lir::Operand> {
  auto place = LowerPlace(block, assign.target, Reach::kWhole);
  if (!place) {
    return std::unexpected(std::move(place.error()));
  }
  auto value = LowerExpr(block, assign.value);
  if (!value) {
    return std::unexpected(std::move(value.error()));
  }
  return Store(*std::move(place), *std::move(value));
}

auto FunctionLowerer::LowerBitsStore(
    const mir::Block& block, mir::ExprId reference, mir::ExprId value)
    -> diag::Result<lir::Operand> {
  // The `slice_mut` steps the reference was taken by, innermost first, back to
  // what the outermost was taken on.
  std::vector<const mir::CallExpr*> steps;
  mir::ExprId taken_on = reference;
  while (unit_->Mir()
             .types.Get(block.exprs.Get(taken_on).type)
             .Is<mir::BitsReferenceType>()) {
    const auto* step =
        std::get_if<mir::CallExpr>(&block.exprs.Get(taken_on).data);
    if (step == nullptr) {
      throw InternalError(
          "mir_to_lir: a reference to bits is taken on the value they lie in "
          "-- please report this as a bug");
    }
    steps.push_back(step);
    taken_on = *mir::CalleeReceiver(step->callee);
  }
  const lir::TypeId void_type =
      unit_->TranslateType(unit_->Mir().builtins.void_type);
  const mir::Type& taken_on_ty =
      unit_->Mir().types.Get(block.exprs.Get(taken_on).type);
  // Bits of the value a write in progress designates are written by one call
  // on the designation, which hears which bits moved.
  if (const auto* designated = taken_on_ty.As<mir::DesignationType>()) {
    auto designation = LowerExpr(block, taken_on);
    if (!designation) {
      return std::unexpected(std::move(designation.error()));
    }
    auto parts = LowerBitsParts(block, steps);
    if (!parts) {
      return std::unexpected(std::move(parts.error()));
    }
    auto bits = LowerExpr(block, value);
    if (!bits) {
      return std::unexpected(std::move(bits.error()));
    }
    return EmitCallTo(
        lir::DesignatedBitsTarget{
            .value = unit_->TranslateType(designated->value),
            .parts = *std::move(parts)},
        {*std::move(designation), *std::move(bits)}, void_type);
  }
  // Anywhere else the bits are written where they lie, by one statement naming
  // the place and the parts.
  auto place = LowerPlace(block, taken_on, Reach::kInto);
  if (!place) {
    return std::unexpected(std::move(place.error()));
  }
  auto parts = LowerBitsParts(block, steps);
  if (!parts) {
    return std::unexpected(std::move(parts.error()));
  }
  auto bits = LowerExpr(block, value);
  if (!bits) {
    return std::unexpected(std::move(bits.error()));
  }
  EmitStatement(
      lir::BitsStoreInstr{
          .place = *std::move(place),
          .parts = *std::move(parts),
          .bits = *std::move(bits)});
  return VoidValue();
}

auto FunctionLowerer::LowerBitsParts(
    const mir::Block& block, std::span<const mir::CallExpr* const> steps)
    -> diag::Result<std::vector<lir::BitsPart>> {
  std::vector<lir::BitsPart> parts;
  parts.reserve(steps.size());
  for (const mir::CallExpr* step : std::views::reverse(steps)) {
    auto start = LowerExpr(block, BitsStartOf(*step));
    if (!start) {
      return std::unexpected(std::move(start.error()));
    }
    parts.push_back(
        lir::BitsPart{
            .start = *std::move(start),
            .type = unit_->TranslateType(
                std::get<mir::Direct>(step->callee).type_arguments.front())});
  }
  return parts;
}

auto FunctionLowerer::LowerMutatingCall(
    const mir::Block& block, const mir::CallExpr& call, support::BuiltinFn fn,
    mir::TypeId type) -> diag::Result<lir::Operand> {
  const std::optional<mir::ExprId> receiver = mir::CalleeReceiver(call.callee);
  if (!receiver.has_value()) {
    throw InternalError(
        "mir_to_lir: a method that updates what it is applied to names that "
        "object, and this call names none -- please report this as a bug");
  }
  // The entry changes the object it is handed where it lies, and answers with
  // whatever result of its own the method states (LRM 7.10.2.4).
  const auto apply_as = [&](lir::CallTarget target,
                            lir::Operand object) -> diag::Result<lir::Operand> {
    std::vector<lir::Operand> args;
    args.reserve(call.arguments.size() + 1);
    args.push_back(std::move(object));
    for (const mir::ExprId argument : call.arguments) {
      auto arg = LowerArgument(block, argument);
      if (!arg) {
        return std::unexpected(std::move(arg.error()));
      }
      args.push_back(*std::move(arg));
    }
    return EmitCallTo(
        std::move(target), std::move(args), unit_->TranslateType(type));
  };
  const auto apply = [&](lir::Operand object) {
    return apply_as(lir::BuiltinTarget{.fn = fn}, std::move(object));
  };

  // A value a write in progress designates is changed by the write's own
  // operation, which hears what moved; the one a designation is handed is a
  // run of its elements written over (LRM 7.6), an array's or a queue's.
  if (const auto* designated = unit_->Mir()
                                   .types.Get(block.exprs.Get(*receiver).type)
                                   .As<mir::DesignationType>()) {
    const auto slice_write = [&]() -> lir::OpenWriteTarget::Op {
      if (fn == support::BuiltinFn::kAssignElementSlice) {
        return lir::OpenWriteTarget::Op::kAssignElementSlice;
      }
      if (fn == support::BuiltinFn::kAssignQueueSlice) {
        return lir::OpenWriteTarget::Op::kAssignQueueSlice;
      }
      throw InternalError(
          "mir_to_lir: what a write in progress designates is changed only by "
          "a run of its elements written over -- please report this as a bug");
    };
    auto designation = LowerExpr(block, *receiver);
    if (!designation) {
      return std::unexpected(std::move(designation.error()));
    }
    return apply_as(
        lir::OpenWriteTarget{
            .op = slice_write(),
            .value = unit_->TranslateType(designated->value)},
        *std::move(designation));
  }
  // The receiver is storage, changed where it lies.
  auto place = LowerPlace(block, *receiver, Reach::kInto);
  if (!place) {
    return std::unexpected(std::move(place.error()));
  }
  return apply(Load(
      *std::move(place),
      unit_->TranslateType(block.exprs.Get(*receiver).type)));
}

// A conditional read as a value is made in a slot of its own, which owns it
// from where the paths rejoin to the end of the full-expression the conditional
// stands in. A way out of an arm before it stores owes the slot nothing.
auto FunctionLowerer::LowerConditional(
    const mir::Block& block, const mir::ConditionalExpr& cond, mir::TypeId type)
    -> diag::Result<lir::Operand> {
  const lir::TypeId result_type = unit_->TranslateType(type);
  const lir::ValueId slot = NewLiveSlot(result_type);
  if (auto made = LowerConditionalInto(block, cond, LocalPlace(slot)); !made) {
    return std::unexpected(std::move(made.error()));
  }
  if (unit_->Types().Get(result_type).IsOwnedValue()) {
    OpenScope(SlotEnd{.slot = slot});
  }
  return Load(LocalPlace(slot), result_type);
}

// The arms are evaluated only on the path that selects them, so the result is
// written through on two paths: it is storage, not a transient. Each arm is
// made into that storage, as clang evaluates both arms of an aggregate
// conditional into its destination, and is an extent of its own -- what it
// made ends before the paths rejoin.
auto FunctionLowerer::LowerConditionalInto(
    const mir::Block& block, const mir::ConditionalExpr& cond,
    const lir::Place& destination) -> diag::Result<void> {
  auto condition = LowerCondition(block, cond.condition);
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

  const auto arm = [&](lir::BlockId id,
                       mir::ExprId value) -> diag::Result<void> {
    SetCurrent(id);
    const std::size_t depth = scopes_.size();
    if (auto made = LowerExprInto(block, value, destination); !made) {
      return made;
    }
    auto closed = CloseExtent(depth);
    if (!closed) {
      return closed;
    }
    Terminate(lir::BranchTerm{.target = merge_id});
    return {};
  };
  if (auto then_arm = arm(then_id, cond.then_value); !then_arm) {
    return then_arm;
  }
  if (auto else_arm = arm(else_id, cond.else_value); !else_arm) {
    return else_arm;
  }
  SetCurrent(merge_id);
  return {};
}

auto FunctionLowerer::LowerExpr(const mir::Block& block, mir::ExprId id)
    -> diag::Result<lir::Operand> {
  const mir::Expr& expr = block.exprs.Get(id);
  const mir::TypeId type = expr.type;
  using Lowered = diag::Result<lir::Operand>;
  // A form computed by one step of its own computes it into a transient.
  const auto computed = [&]() -> Lowered {
    auto value = LowerRvalue(block, id);
    if (!value) {
      return std::unexpected(std::move(value.error()));
    }
    return Emit(value->type, std::move(value->value));
  };
  // A place stands as a value only under a read, which is where it is read.
  const auto placed = [&]() -> Lowered {
    throw InternalError(
        "mir_to_lir: a place stands as a value outside a read -- please "
        "report this as a bug");
  };
  return std::visit(
      Overloaded{
          [&](const mir::StringLiteral& lit) -> Lowered {
            return lir::Operand{lir::StrConst{
                .value = lit.value, .type = unit_->TranslateType(type)}};
          },
          [&](const mir::MachineFloatLiteral& lit) -> Lowered {
            return lir::Operand{lir::RealConst{
                .value = lit.value, .type = unit_->TranslateType(type)}};
          },
          [&](const mir::NullLiteral&) -> Lowered {
            return lir::Operand{
                lir::NullConst{.type = unit_->TranslateType(type)}};
          },
          [&](const mir::MachineBoolLiteral& lit) -> Lowered {
            return lir::Operand{lir::BoolConst{
                .value = lit.value, .type = unit_->TranslateType(type)}};
          },
          [&](const mir::MachineIntLiteral& lit) -> Lowered {
            return lir::Operand{lir::IntConst{
                .value =
                    lir::IntegralConstant{
                        .value_words = {static_cast<std::uint64_t>(lit.value)},
                        .state_words = {}},
                .type = unit_->TranslateType(type)}};
          },
          [&](const mir::ReferenceExpr& reference) -> Lowered {
            if (mir::IsPlace(expr.data)) {
              return placed();
            }
            return ReferenceValue(reference.target, type);
          },
          [&](const mir::CallExpr& call) -> Lowered {
            const lir::ValueId result =
                NewTransient(unit_->TranslateType(type));
            auto called = LowerCallInto(
                block, call, type, lir::Transient{.value = result});
            if (!called) {
              return std::unexpected(std::move(called.error()));
            }
            return lir::Use{.value = result};
          },
          [&](const mir::NewExpr& allocation) -> Lowered {
            return LowerNew(block, allocation, type);
          },
          [&](const mir::FieldAccessExpr&) -> Lowered { return placed(); },
          [&](const mir::DerefExpr&) -> Lowered { return placed(); },
          [&](const mir::AssignExpr& assign) -> Lowered {
            return LowerAssign(block, assign);
          },
          [&](const mir::ConditionalExpr& cond) -> Lowered {
            return LowerConditional(block, cond, type);
          },
          [&](const mir::BlockExpr& be) -> Lowered {
            return LowerBlockExpr(block.child_scopes.Get(be.scope), be.value);
          },
          [&](const mir::MoveExpr& m) -> Lowered {
            // A move is a last use; it changes neither the value nor its type,
            // so it reads its operand as a copy does. An owned value moved out
            // of a local's slot takes the local's end with it, so whatever
            // takes the value owns it and the local, ending, releases nothing,
            // as rustc's drop elaboration leaves a place moved out of with
            // nothing to drop. A value only lent to this body owes no end
            // here, and what takes it copies it.
            auto value = LowerRead(block, m.operand);
            if (!value || !unit_->Types()
                               .Get(lir::OperandType(fn_, *value))
                               .IsOwnedValue()) {
              return value;
            }
            if (const std::optional<std::size_t> owed =
                    OwedEndOfLocal(block, m.operand)) {
              PassEndAt(*owed);
              OpenScope(ValueEnd{.value = std::get<lir::Use>(*value).value});
            }
            return value;
          },
          [&](const mir::CopyExpr& c) -> Lowered {
            return LowerRead(block, c.place);
          },
          [&](const mir::AwaitExpr& await) -> Lowered {
            return LowerCoroutineAwait(block, await, type);
          },
          [&](const mir::WaitExpr& wait) -> Lowered {
            // The call has already parked this execution and answered whether
            // it must give up control; where it must, a control edge hands
            // control back to the scheduler, which resumes at the next block.
            // Every wait differs only in the call that precedes it. That call
            // is lowered like any other -- what it answers is the machine
            // boolean MIR gave it, and the suspend edge is what this adds
            // around it.
            auto park = LowerExpr(block, wait.park);
            if (!park) {
              return std::unexpected(std::move(park.error()));
            }
            auto suspended = SuspendIf(*std::move(park));
            if (!suspended) {
              return std::unexpected(std::move(suspended.error()));
            }
            // Resuming is a point where this execution regains control, so a
            // target it is inside may have been disabled while it was away.
            auto checked = TakeDepartureIfDue();
            if (!checked) {
              return std::unexpected(std::move(checked.error()));
            }
            return VoidValue();
          },
          [&](const mir::BorrowExpr&) -> Lowered { return computed(); },
          [&](const mir::AddressOfExpr&) -> Lowered { return computed(); },
          [&](const mir::CastExpr& cast) -> Lowered {
            return LowerReadAsAnother(block, cast.operand, type);
          },
          [&](const mir::CompositeExpr&) -> Lowered { return computed(); },
          [&](const mir::UnionExpr&) -> Lowered { return computed(); },
          [&](const mir::VectorGetExpr&) -> Lowered { return computed(); },
          [&](const mir::DiscriminantExpr&) -> Lowered { return computed(); },
          [&](const mir::ClosureExpr&) -> Lowered { return computed(); },
          [&](const mir::UnaryExpr&) -> Lowered { return computed(); },
          [&](const mir::BinaryExpr&) -> Lowered { return computed(); },
      },
      expr.data);
}

auto FunctionLowerer::LowerReadAsAnother(
    const mir::Block& block, mir::ExprId operand, mir::TypeId type)
    -> diag::Result<lir::Operand> {
  const lir::TypeId read_as = unit_->TranslateType(type);
  auto value = LowerExpr(block, operand);
  if (!value) {
    return std::unexpected(std::move(value.error()));
  }
  if (!unit_->Types().Get(read_as).IsMoveOnly()) {
    return Emit(read_as, lir::CastInstr{.operand = *std::move(value)});
  }
  const lir::Operand moved =
      Emit(read_as, lir::CastInstr{.operand = HandOn(*std::move(value))});
  OpenScope(ValueEnd{.value = std::get<lir::Use>(moved).value});
  return moved;
}

auto FunctionLowerer::LowerRvalue(const mir::Block& block, mir::ExprId id)
    -> diag::Result<TypedRvalue> {
  const mir::Expr& expr = block.exprs.Get(id);
  const mir::TypeId type = expr.type;
  using Lowered = diag::Result<TypedRvalue>;
  const auto of_type = [&](lir::Rvalue value) -> Lowered {
    return TypedRvalue{
        .type = unit_->TranslateType(type), .value = std::move(value)};
  };
  // A form this answers by a value it reads or makes elsewhere is that value
  // moved, or copied where it is only lent, as rustc's `as_rvalue` answers
  // `Rvalue::Use` of an operand.
  const auto used = [&]() -> Lowered {
    auto value = LowerExpr(block, id);
    if (!value) {
      return std::unexpected(std::move(value.error()));
    }
    return of_type(ConsumeByCopyOrMove(*std::move(value)));
  };
  // A read of storage another member of an untagged union may have written
  // makes a value of its own, so it is computed where it goes rather than
  // first into a value of the whole read's size.
  const auto read = [&](mir::ExprId place) -> Lowered {
    if (mir::KindOfPlace(unit_->Mir(), block, place) !=
        mir::PlaceKind::kOverlaid) {
      return used();
    }
    auto named = LowerPlace(block, place, Reach::kRead);
    if (!named) {
      return std::unexpected(std::move(named.error()));
    }
    return of_type(lir::OverlaidLoadInstr{.place = *std::move(named)});
  };
  return std::visit(
      Overloaded{
          // A reference is the address of its referent: storage where it
          // lies, and a value in a slot that lasts to the end of the
          // full-expression, as clang binds a reference to a temporary.
          [&](const mir::BorrowExpr& borrow) -> Lowered {
            auto referent = PlaceHolding(
                block, borrow.operand,
                BorrowReach(unit_->Mir()
                                .types.Get(type)
                                .Get<mir::ReferenceType>()
                                .mutability));
            if (!referent) {
              return std::unexpected(std::move(referent.error()));
            }
            return of_type(lir::AddrOfInstr{.place = *std::move(referent)});
          },
          [&](const mir::AddressOfExpr& addr) -> Lowered {
            // A reference carries which form of storage it names rather than
            // stating it (LRM 13.5.2), and the two forms are different storage
            // with one type between them -- so an address taken through one
            // would name whichever form the type does not admit. Reading and
            // writing through a reference are its own operations, and so is
            // what a wait on it enrols on.
            if (const auto* opened = std::get_if<mir::DerefExpr>(
                    &block.exprs.Get(addr.operand).data);
                opened != nullptr &&
                unit_->Mir()
                    .types.Get(block.exprs.Get(opened->pointer).type)
                    .IsVariableRef()) {
              throw InternalError(
                  "mir_to_lir: an address was taken through a reference, where "
                  "the reference's own operations reach its storage");
            }
            auto place = LowerPlace(block, addr.operand, Reach::kWhole);
            if (!place) {
              return std::unexpected(std::move(place.error()));
            }
            return of_type(lir::AddrOfInstr{.place = *std::move(place)});
          },
          [&](const mir::CastExpr& cast) -> Lowered {
            // A value with no copy read as another type is the one value,
            // which reading it carries over with its end.
            if (unit_->Types().Get(unit_->TranslateType(type)).IsMoveOnly()) {
              return used();
            }
            auto operand = LowerExpr(block, cast.operand);
            if (!operand) {
              return std::unexpected(std::move(operand.error()));
            }
            return of_type(lir::CastInstr{.operand = *std::move(operand)});
          },
          [&](const mir::CompositeExpr& composite) -> Lowered {
            auto parts = LowerEachExpr(block, composite.parts);
            if (!parts) {
              return std::unexpected(std::move(parts.error()));
            }
            const lir::TypeId built = unit_->TranslateType(type);
            return of_type(
                AssembledFrom(unit_->Lir(), built, *std::move(parts)));
          },
          [&](const mir::UnionExpr& built) -> Lowered {
            auto value = LowerExpr(block, built.value);
            if (!value) {
              return std::unexpected(std::move(value.error()));
            }
            return of_type(
                lir::UnionInstr{
                    .member = built.member, .value = *std::move(value)});
          },
          // The tag is read where the union lies, as rustc reads an enum's
          // discriminant.
          [&](const mir::DiscriminantExpr& read) -> Lowered {
            auto place = PlaceHolding(block, read.tagged, Reach::kRead);
            if (!place) {
              return std::unexpected(std::move(place.error()));
            }
            return of_type(lir::DiscriminantInstr{.place = *std::move(place)});
          },
          [&](const mir::VectorGetExpr& get) -> Lowered {
            auto vector = LowerExpr(block, get.vector);
            if (!vector) {
              return std::unexpected(std::move(vector.error()));
            }
            auto index = LowerExpr(block, get.index);
            if (!index) {
              return std::unexpected(std::move(index.error()));
            }
            return of_type(
                lir::VectorGetInstr{
                    .vector = *std::move(vector), .index = *std::move(index)});
          },
          [&](const mir::ClosureExpr& cl) -> Lowered {
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
              // A capture with no copy is handed to the closure, which ends
              // it with its other captures.
              const bool moved = unit_->Types()
                                     .Get(lir::OperandType(fn_, *value))
                                     .IsMoveOnly();
              captures[init.target.value] =
                  moved ? HandOn(*std::move(value)) : *std::move(value);
            }
            return TypedRvalue{
                .type = unit_->ClosureValueType(cl.closure),
                .value = lir::ClosureInstr{.captures = std::move(captures)}};
          },
          [&](const mir::UnaryExpr& un) -> Lowered {
            auto operand = LowerExpr(block, un.operand);
            if (!operand) {
              return std::unexpected(std::move(operand.error()));
            }
            const lir::UnaryOp op = TranslateUnaryOp(un.op);
            // A logical-not over a machine boolean -- the reduced predicate a
            // real- or chandle-family `!` produces before `from_bool` widens it
            // back -- stays a machine boolean; its surface 1-bit type is
            // restored by the enclosing `from_bool`.
            const lir::TypeId result_type =
                (op == lir::UnaryOp::kLogicalNot &&
                 lir::OperandType(fn_, *operand) == unit_->MachineBoolType())
                    ? unit_->MachineBoolType()
                    : unit_->TranslateType(type);
            return TypedRvalue{
                .type = result_type,
                .value =
                    lir::UnaryInstr{.op = op, .operand = *std::move(operand)}};
          },
          [&](const mir::BinaryExpr& bin) -> Lowered {
            const lir::BinaryOp op = TranslateBinaryOp(bin.op);
            auto lhs = LowerExpr(block, bin.lhs);
            if (!lhs) {
              return std::unexpected(std::move(lhs.error()));
            }
            auto rhs = LowerExpr(block, bin.rhs);
            if (!rhs) {
              return std::unexpected(std::move(rhs.error()));
            }
            return of_type(
                lir::BinaryInstr{
                    .op = op, .lhs = *std::move(lhs), .rhs = *std::move(rhs)});
          },
          [&](const mir::StringLiteral&) { return used(); },
          [&](const mir::MachineFloatLiteral&) { return used(); },
          [&](const mir::NullLiteral&) { return used(); },
          [&](const mir::MachineBoolLiteral&) { return used(); },
          [&](const mir::MachineIntLiteral&) { return used(); },
          [&](const mir::ReferenceExpr&) { return used(); },
          [&](const mir::CallExpr&) { return used(); },
          [&](const mir::NewExpr&) { return used(); },
          [&](const mir::FieldAccessExpr&) { return used(); },
          [&](const mir::DerefExpr&) { return used(); },
          [&](const mir::AssignExpr&) { return used(); },
          [&](const mir::ConditionalExpr&) { return used(); },
          [&](const mir::BlockExpr&) { return used(); },
          [&](const mir::MoveExpr& move) { return read(move.operand); },
          [&](const mir::CopyExpr& copy) { return read(copy.place); },
          [&](const mir::AwaitExpr&) { return used(); },
          [&](const mir::WaitExpr&) { return used(); },
      },
      expr.data);
}

auto FunctionLowerer::LowerExprInto(
    const mir::Block& block, mir::ExprId id,
    const lir::Destination& destination) -> diag::Result<void> {
  const mir::Expr& expr = block.exprs.Get(id);
  using Lowered = diag::Result<void>;
  // Every other form computes its value by one step put where it goes, as
  // rustc's `expr_into_dest` assigns what `as_rvalue` answers. A step that
  // reads a value rather than making one puts it in storage only as a value of
  // the storage's own, copied or moved in.
  const auto assigned = [&]() -> Lowered {
    auto value = LowerRvalue(block, id);
    if (!value) {
      return std::unexpected(std::move(value.error()));
    }
    if (std::holds_alternative<lir::Place>(destination) &&
        !lir::MakesValue(value->value)) {
      AssignFrom(destination, Emit(value->type, std::move(value->value)));
      return {};
    }
    Assign(destination, std::move(value->value));
    return {};
  };
  return std::visit(
      Overloaded{
          [&](const mir::CallExpr& call) -> Lowered {
            return LowerCallInto(block, call, expr.type, destination);
          },
          [&](const mir::BlockExpr& be) -> Lowered {
            return LowerBlockExprInto(
                block.child_scopes.Get(be.scope), be.value, destination);
          },
          [&](const mir::MoveExpr&) { return assigned(); },
          [&](const mir::CopyExpr&) { return assigned(); },
          [&](const mir::StringLiteral&) { return assigned(); },
          [&](const mir::MachineFloatLiteral&) { return assigned(); },
          [&](const mir::NullLiteral&) { return assigned(); },
          [&](const mir::MachineBoolLiteral&) { return assigned(); },
          [&](const mir::MachineIntLiteral&) { return assigned(); },
          [&](const mir::ReferenceExpr&) { return assigned(); },
          [&](const mir::NewExpr&) { return assigned(); },
          [&](const mir::BorrowExpr&) { return assigned(); },
          [&](const mir::AddressOfExpr&) { return assigned(); },
          [&](const mir::CastExpr&) { return assigned(); },
          [&](const mir::CompositeExpr&) { return assigned(); },
          [&](const mir::UnionExpr&) { return assigned(); },
          [&](const mir::VectorGetExpr&) { return assigned(); },
          [&](const mir::DiscriminantExpr&) { return assigned(); },
          [&](const mir::ClosureExpr&) { return assigned(); },
          [&](const mir::FieldAccessExpr&) { return assigned(); },
          [&](const mir::DerefExpr&) { return assigned(); },
          [&](const mir::AssignExpr&) { return assigned(); },
          [&](const mir::UnaryExpr&) { return assigned(); },
          [&](const mir::BinaryExpr&) { return assigned(); },
          // A transient is defined once, so only storage takes an arm on each
          // path.
          [&](const mir::ConditionalExpr& cond) -> Lowered {
            const auto* place = std::get_if<lir::Place>(&destination);
            if (place == nullptr) {
              return assigned();
            }
            return LowerConditionalInto(block, cond, *place);
          },
          [&](const mir::AwaitExpr&) { return assigned(); },
          [&](const mir::WaitExpr&) { return assigned(); },
      },
      expr.data);
}

// An execution answers through its completion rather than to a caller standing
// below it: nothing is on the stack to receive a returned value, and what
// awaits it runs later. Any other body makes its answer in its return place, as
// rustc's `expr_into_dest` builds into `_0`, and the caller takes it there with
// its end.
auto FunctionLowerer::LowerReturnInto(
    const mir::Block& block, const mir::ReturnStmt& stmt)
    -> diag::Result<void> {
  if (stmt.value.has_value()) {
    if (completion_cell_.has_value()) {
      auto value = LowerExpr(block, *stmt.value);
      if (!value) {
        return std::unexpected(std::move(value.error()));
      }
      StoreActivationValue(
          completion_cell_->cell, *std::move(value), completion_cell_->type);
    } else {
      if (!fn_.return_place.has_value()) {
        throw InternalError(
            "mir_to_lir: a body answering nothing returns a value -- please "
            "report this as a bug");
      }
      auto made =
          LowerExprInto(block, *stmt.value, LocalPlace(*fn_.return_place));
      if (!made) {
        return made;
      }
    }
  }
  // Returning leaves every guarded body between here and the frame's edge, so
  // each of their cleanups runs -- after the returned value is settled, which
  // a cleanup must not change.
  auto cleaned = RunCleanupsDownTo(0);
  if (!cleaned) {
    return std::unexpected(std::move(cleaned.error()));
  }
  Terminate(lir::ReturnTerm{});
  return {};
}

}  // namespace lyra::lowering::mir_to_lir
