#include "lyra/lowering/mir_to_lir/function_lowerer.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <format>
#include <optional>
#include <ranges>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/component_index.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/failure_context.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/integral_constant.hpp"
#include "lyra/lir/operator.hpp"
#include "lyra/lir/place_query.hpp"
#include "lyra/lir/symbol_name.hpp"
#include "lyra/lir/type_builders.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/lowering/mir_to_lir/unit_lowerer.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/inc_dec_op.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/unary_op.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::mir_to_lir {

namespace {

// Whether a call ending this way can leave by a departure, which is what
// obliges it to name the landing the departure reaches.
auto MayDepart(support::CallEnding ending) -> bool {
  switch (ending) {
    case support::CallEnding::kReturns:
      return false;
    case support::CallEnding::kReturnsOrDeparts:
    case support::CallEnding::kDeparts:
      return true;
  }
  throw InternalError("mir_to_lir: unknown call ending");
}

auto Unsupported(std::string message) -> std::unexpected<diag::Diagnostic> {
  return diag::Fail(
      diag::DiagCode::kUnsupportedExpressionForm, std::move(message));
}

// Which build assembles a composite value follows from the storage its type
// names, which is what translating the type has just settled: a product's
// components each keep a type of their own, while an element list is one
// element type laid down a known number of times. A type reaching here that
// names neither is a producer that built a composite of something that is not
// composed from parts.
auto AssembledFrom(const lir::Type& built, std::vector<lir::Operand> parts)
    -> lir::InstrData {
  if (built.IsProduct()) {
    return lir::TupleInstr{.components = std::move(parts)};
  }
  if (built.Is<lir::MachineArrayType>()) {
    return lir::ArrayInstr{.elements = std::move(parts)};
  }
  throw InternalError(
      "mir_to_lir: a composite's type names storage that is not assembled "
      "from parts");
}

// The place a place local names: its own storage, with nothing projected off
// it.
auto LocalPlace(lir::ValueId local) -> lir::Place {
  return lir::Place{.base = lir::Use{.value = local}, .chain = {}};
}

// Whether this call builds a reference over its one argument. A construction
// whose result is a reference names storage rather than assembling a value, so
// it is lowered by naming that storage and never by the entries that build one.
auto BindsReference(
    const mir::TypePool& types, const mir::CallExpr& call,
    mir::TypeId result_type) -> bool {
  return std::holds_alternative<mir::Construct>(call.callee) &&
         types.Get(result_type).Is<mir::RefType>();
}

// Whether this call brings an object into existence. A construction names the
// type it builds and a managed reference is what a handle to an object is
// (LRM 8.3), so the two together are the whole of the question. Which class it
// builds is a second question, answered from the same type where the class is
// needed.
auto BuildsObject(
    const mir::TypePool& types, const mir::CallExpr& call,
    mir::TypeId result_type) -> bool {
  return std::holds_alternative<mir::Construct>(call.callee) &&
         types.Get(result_type).Is<mir::ManagedRefType>();
}

// Whether this call builds an instance of the design hierarchy. The owning
// pointer the object tree takes is built for nothing else, so a construction
// answering one is the whole of the question.
auto BuildsScope(
    const mir::TypePool& types, const mir::CallExpr& call,
    mir::TypeId result_type) -> bool {
  if (!std::holds_alternative<mir::Construct>(call.callee)) {
    return false;
  }
  const auto* pointer = types.Get(result_type).As<mir::PointerType>();
  return pointer != nullptr &&
         pointer->ownership == mir::PointerOwnership::kUnique;
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

// A method of another unit's class is reached by the symbol that unit emits it
// under, composed from the same three names that unit composed it from.
auto ExternalMethodSymbol(
    std::string_view unit_name, std::string_view class_name,
    std::string_view method_name) -> lir::SymbolTarget {
  return lir::SymbolTarget{
      .symbol = lir::ClassCallableSymbol(
          unit_name, lir::SymbolPart::Name(class_name),
          lir::SymbolPart::Name(method_name))};
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
                      return lir::CallTarget{lir::FunctionTarget{
                          .function = unit_->MethodFunction(t.owner, t.slot)}};
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
                          lir::BuiltinTarget{.fn = fn, .position = d.position}};
                    },
                    [&](const mir::UnitCallableTarget& t)
                        -> diag::Result<lir::CallTarget> {
                      // A body this unit's own namespace owns is reached by the
                      // symbol this unit emits it under, which it composes from
                      // the position the body sits at where nothing names it.
                      return lir::CallTarget{lir::SymbolTarget{
                          .symbol = unit_->UnitCallableSymbol(t.slot)}};
                    },
                    [&](const mir::ExternalUnitCallableTarget& t)
                        -> diag::Result<lir::CallTarget> {
                      // A callable of another unit's namespace is outside this
                      // unit and is reached by its symbol, which carries that
                      // unit because a namespace name is unique only inside it.
                      // Only a body that unit published can be named this way,
                      // so the name is the whole of the part.
                      return lir::CallTarget{lir::SymbolTarget{
                          .symbol = lir::NamespaceCallableSymbol(
                              t.unit_name,
                              lir::SymbolPart::Name(t.callable_name))}};
                    },
                    [&](const mir::ExternalUnitClassMethodTarget& t)
                        -> diag::Result<lir::CallTarget> {
                      return lir::CallTarget{ExternalMethodSymbol(
                          t.unit_name, t.class_name, t.method_name)};
                    },
                    // Reached by the symbol its declaring unit emits it under,
                    // from that unit and from any other alike.
                    [&](const mir::StructMethodTarget& t)
                        -> diag::Result<lir::CallTarget> {
                      return lir::CallTarget{lir::SymbolTarget{
                          .symbol = lir::StructMethodSymbol(
                              t.declaration.unit_name, t.declaration.name,
                              t.answers)}};
                    },
                    [&](const mir::ExternalUnitMintedEntryTarget& t)
                        -> diag::Result<lir::CallTarget> {
                      // It answers to no name, so its symbol is composed from
                      // the unit and which of them it is -- the same parts the
                      // unit that defines it composes.
                      return lir::CallTarget{lir::SymbolTarget{
                          .symbol = MintedEntrySymbol(t.unit_name, t.entry)}};
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
          // the whole lineage in hand, which is nowhere near this pass. The
          // receiver is already the call's first argument, as it is for a
          // method named outright, and is the introducing class's part of the
          // object.
          [&](const mir::Virtual& v) -> diag::Result<lir::CallTarget> {
            return lir::CallTarget{
                lir::DispatchTarget{.method = unit_->SlotRef(v.slot)}};
          }},
      callee);
}

FunctionLowerer::FunctionLowerer(
    UnitLowerer& unit, const mir::CallableCode& code, std::string name)
    : unit_(&unit),
      code_(&code),
      construction_(std::nullopt),
      closure_(nullptr),
      build_(nullptr),
      name_(std::move(name)),
      variable_slot_(code.locals.size(), std::nullopt),
      locals_(code.locals.size(), std::nullopt) {
}

FunctionLowerer::FunctionLowerer(
    UnitLowerer& unit, const mir::Class& cls,
    const mir::ConstructorDecl& constructor, std::string prologue,
    std::string name)
    : unit_(&unit),
      code_(&constructor.code),
      construction_(
          Construction{
              .cls = &cls,
              .constructor = &constructor,
              .prologue = std::move(prologue)}),
      closure_(nullptr),
      build_(nullptr),
      name_(std::move(name)),
      variable_slot_(constructor.code.locals.size(), std::nullopt),
      locals_(constructor.code.locals.size(), std::nullopt) {
}

FunctionLowerer::FunctionLowerer(
    UnitLowerer& unit, const mir::ClosureDecl& closure, std::string name)
    : unit_(&unit),
      code_(&closure.invoke),
      construction_(std::nullopt),
      closure_(&closure),
      build_(nullptr),
      name_(std::move(name)),
      variable_slot_(closure.invoke.locals.size(), std::nullopt),
      locals_(closure.invoke.locals.size(), std::nullopt) {
}

FunctionLowerer::FunctionLowerer(
    UnitLowerer& unit, const mir::ValueBuild& build, std::string name)
    : unit_(&unit),
      code_(nullptr),
      construction_(std::nullopt),
      closure_(nullptr),
      build_(&build),
      name_(std::move(name)) {
}

void FunctionLowerer::BindCaptureReceiver(mir::LocalId receiver) {
  const lir::TypeId type =
      unit_->TranslateType(code_->locals.Get(receiver).type);
  const lir::ValueId value = fn_.values.Add(
      lir::Local{
          .name = LocalLabel(*code_, receiver),
          .type = type,
          .kind = lir::LocalKind::kParam});
  fn_.params.push_back(value);
  BindLocal(receiver, type, lir::Use{.value = value});
}

auto FunctionLowerer::LowerValueBuild(
    UnitLowerer& unit, const mir::ValueBuild& build, std::string name)
    -> diag::Result<lir::Function> {
  return FunctionLowerer(unit, build, std::move(name)).RunValueBuild();
}

auto FunctionLowerer::RunValueBuild() -> diag::Result<lir::Function> {
  fn_.name = std::move(name_);
  const auto in_function = diag::FailureContext::InFunction(fn_.name);
  // The type is the built expression's own, so a description and a constant
  // reach this the same way and neither is named here.
  fn_.result_type =
      unit_->TranslateType(build_->body.exprs.Get(build_->value).type);
  SetCurrent(NewBlock());
  auto value = LowerExpr(build_->body, build_->value);
  if (!value) {
    return std::unexpected(std::move(value.error()));
  }
  const lir::Operand answer = HandOn(*std::move(value));
  auto cleaned = RunCleanupsDownTo(0);
  if (!cleaned) {
    return std::unexpected(std::move(cleaned.error()));
  }
  Terminate(lir::ReturnTerm{.value = answer});
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
  const auto in_function = diag::FailureContext::InFunction(fn_.name);
  // A coroutine-bodied callable keeps its coroutine result type: coroutine-ness
  // is the call protocol carried by the type, so a backend realizes suspension
  // and completion from the type, never from a separate flag.
  fn_.result_type = unit_->TranslateType(code_->result_type);
  const bool is_coroutine =
      unit_->Mir().types.Get(code_->result_type).Is<mir::CoroutineType>();

  // The body's variables, in declaration order. A declaration is what gives a
  // variable storage, so nothing about what the body does with one is read
  // here -- not whether anything writes it, not whether anything binds a
  // second name to it. A variable of a type whose values the runtime builds
  // gets storage of its own there, which a reference can bind and which
  // survives a suspension, and so does a cell MIR declared, which is a
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

  // The entry block exists first, so the storage the body's variables live in
  // and each parameter's binding land there, ahead of the body.
  SetCurrent(NewBlock());
  OpenVariables();
  // A closure invoke's receiver names the storage its captures live in, and
  // leads the per-invocation parameters in the signature.
  if (closure_ != nullptr) {
    if (!code_->receiver.has_value()) {
      throw InternalError(
          "mir_to_lir: a closure's body reads its captures through the "
          "closure, yet states no receiver");
    }
    BindCaptureReceiver(*code_->receiver);
  }
  for (const mir::LocalId param : code_->params) {
    const lir::TypeId type =
        unit_->TranslateType(code_->locals.Get(param).type);
    const lir::ValueId value = fn_.values.Add(
        lir::Local{
            .name = LocalLabel(*code_, param),
            .type = type,
            .kind = lir::LocalKind::kParam});
    fn_.params.push_back(value);
    // A parameter is a declared local whose first write is the incoming
    // argument, so it binds exactly as any declaration does. The caller only
    // lends the argument, so a slot owning its value takes a copy, which the
    // body may then write (LRM 13.5.1) and ends on its way out.
    if (const std::optional<lir::ValueId> owning =
            BindLocal(param, type, lir::Use{.value = value})) {
      OpenScope(SlotEnd{.slot = *owning});
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
            .type = CompletionCellType(payload),
            .kind = lir::LocalKind::kParam});
    fn_.params.push_back(slot);
    completion_cell_ =
        CompletionCell{.cell = lir::Use{.value = slot}, .type = payload};
  }

  // The base's arguments are one full-expression, evaluated before the body.
  const std::size_t base_depth = scopes_.size();
  auto based = ConstructBase();
  if (!based) {
    return std::unexpected(std::move(based.error()));
  }
  auto base_closed = CloseExtent(base_depth);
  if (!base_closed) {
    return std::unexpected(std::move(base_closed.error()));
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

  // A block the lowering left open either falls off the end of a void or
  // coroutine body -- an implicit return -- or is a join control never reaches.
  // A value-returning body reaches its returns explicitly, so its open blocks
  // are the latter.
  const bool falls_through_to_return =
      is_coroutine ||
      fn_.result_type == unit_->TranslateType(unit_->Mir().builtins.void_type);
  // Falling off the end is a way out like any other, and the one no statement
  // of the body spells, so what it owes is settled here rather than where the
  // statements are walked.
  if (falls_through_to_return) {
    // Reading the identity rather than the position, because emitting into one
    // block may open another and the set grows under the walk.
    std::vector<lir::BlockId> open;
    for (const OpenBlock& block : blocks_) {
      if (!block.terminator.has_value()) {
        open.push_back(block.id);
      }
    }
    for (const lir::BlockId id : open) {
      SetCurrent(id);
      CloseVariables();
    }
  }
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

auto FunctionLowerer::ConstructorOf(const mir::DeclaredClassRef& cls)
    -> EnteredConstructor {
  return std::visit(
      Overloaded{
          [&](const mir::IntraUnitClassRef& intra) {
            return EnteredConstructor{
                .object_type = unit_->ClassValueType(intra.class_id),
                .callee = lir::CallTarget{lir::FunctionTarget{
                    .function = unit_->ConstructorFunction(intra.class_id)}}};
          },
          [&](const mir::CrossUnitClassRef& ext) {
            return EnteredConstructor{
                .object_type = unit_->ExternalClassValueType(
                    ext.unit_name, ext.class_name),
                .callee = lir::CallTarget{lir::SymbolTarget{
                    .symbol = lir::ConstructorSymbol(
                        ext.unit_name,
                        lir::SymbolPart::Name(ext.class_name))}}};
          }},
      cls);
}

auto FunctionLowerer::ConstructBase() -> diag::Result<void> {
  if (!construction_.has_value() || !construction_->cls->base.has_value()) {
    return {};
  }
  lir::CallTarget callee = std::visit(
      Overloaded{
          [&](const mir::IntraUnitClassRef& intra) -> lir::CallTarget {
            return ConstructorOf(intra).callee;
          },
          [&](const mir::CrossUnitClassRef& cross) -> lir::CallTarget {
            return ConstructorOf(cross).callee;
          },
          [](const mir::ObjectTreeRootRef&) -> lir::CallTarget {
            return lir::LibraryConstructorTarget{
                .cls = support::RuntimeClass::kScope};
          },
          [](const mir::ManagedObjectRootRef&) -> lir::CallTarget {
            return lir::LibraryConstructorTarget{
                .cls = support::RuntimeClass::kObject};
          }},
      *construction_->cls->base);
  const lir::Operand object{lir::Use{.value = fn_.params.front()}};
  const lir::TypeId void_type =
      unit_->TranslateType(unit_->Mir().builtins.void_type);
  // What the base construction carries was settled where the class was read:
  // the arguments are complete however the source arrived at them, so there is
  // nothing to establish about them here.
  const std::vector<mir::ExprId>& stated =
      construction_->constructor->base_args;
  std::vector<lir::Operand> args;
  args.reserve(stated.size() + 1);
  // The base is entered on the object being constructed, which leads its
  // arguments the way a receiver leads any body's parameters.
  args.push_back(object);
  for (const mir::ExprId arg : stated) {
    auto lowered = LowerArgument(code_->Body(), arg);
    if (!lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
    args.push_back(*std::move(lowered));
  }
  auto entered = EmitCallTo(std::move(callee), std::move(args), void_type);
  if (!entered) {
    return std::unexpected(std::move(entered.error()));
  }
  // Once the base is built the object is a value of this class, which is what
  // the rest of a C++ constructor's prologue makes it.
  auto prologue = EmitCallTo(
      lir::SymbolTarget{.symbol = construction_->prologue}, {object},
      void_type);
  if (!prologue) {
    return std::unexpected(std::move(prologue.error()));
  }
  return {};
}

auto FunctionLowerer::DepartureSlot() -> lir::ValueId {
  if (!departure_slot_.has_value()) {
    departure_slot_ = NewPlaceLocal(unit_->ControlEffectType());
  }
  return *departure_slot_;
}

auto FunctionLowerer::LandingHere() -> diag::Result<lir::BlockId> {
  const diag::Result<lir::BlockId> continued =
      scopes_.empty() ? diag::Result<lir::BlockId>{FrameEdge()}
                      : UnwindEntryAt(scopes_.size() - 1);
  if (!continued) {
    return std::unexpected(continued.error());
  }
  const lir::BlockId landing = NewBlock();
  const lir::BlockId resumed = current_;
  SetCurrent(landing);
  const lir::Operand effect =
      Emit(unit_->ControlEffectType(), lir::ReceiveDepartureInstr{});
  Store(
      lir::Place{.base = lir::Use{.value = DepartureSlot()}, .chain = {}},
      effect);
  Terminate(lir::BranchTerm{.target = *continued});
  SetCurrent(resumed);
  return landing;
}

auto FunctionLowerer::UnwindEntryAt(std::size_t index)
    -> diag::Result<lir::BlockId> {
  if (const auto built = scopes_[index].unwind_entry; built.has_value()) {
    return *built;
  }
  const lir::Place slot{
      .base = lir::Use{.value = DepartureSlot()}, .chain = {}};
  const lir::BlockId resumed = current_;
  const auto outer = [&]() -> diag::Result<lir::BlockId> {
    return index == 0 ? diag::Result<lir::BlockId>{FrameEdge()}
                      : UnwindEntryAt(index - 1);
  };
  // What a scope owes, run once here and passed on to the scope it is nested
  // in, so its code appears once on the way out however many calls inside it
  // can leave.
  const auto owed_then_outward =
      [&](const auto& owe) -> diag::Result<lir::BlockId> {
    const diag::Result<lir::BlockId> next = outer();
    if (!next) {
      return next;
    }
    const lir::BlockId entry = NewBlock();
    SetCurrent(entry);
    auto owed = owe();
    if (!owed) {
      return std::unexpected(std::move(owed.error()));
    }
    Terminate(lir::BranchTerm{.target = *next});
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
              EndValue(lir::Use{.value = end.value});
              return {};
            });
          },
          [&](const SlotEnd& end) -> diag::Result<lir::BlockId> {
            return owed_then_outward([&]() -> diag::Result<void> {
              EndSlotValue(end.slot);
              return {};
            });
          },
          // Nothing is owed here any more, so the departure goes straight on.
          [&](const EndPassedOn&) -> diag::Result<lir::BlockId> {
            return outer();
          },
          // The region's own test decides whether it claims the departure, so
          // this only hands the departure to it.
          [&](const RegionScope& region) -> diag::Result<lir::BlockId> {
            const lir::BlockId entry = NewBlock();
            SetCurrent(entry);
            Store(
                lir::Place{
                    .base = lir::Use{.value = region.caught}, .chain = {}},
                Load(slot, unit_->ControlEffectType()));
            Terminate(lir::BranchTerm{.target = region.handler});
            return entry;
          },
          [](const TerminateScope&) -> diag::Result<lir::BlockId> {
            throw InternalError(
                "mir_to_lir: a cleanup's code can depart, and a departure "
                "starting inside a cleanup has nowhere to go -- please report "
                "this as a bug");
          }},
      kind);
  SetCurrent(resumed);
  if (!entered) {
    return entered;
  }
  scopes_[index].unwind_entry = *entered;
  return *entered;
}

// Past every scope nothing here claims the departure, so what is owed is what
// any other way out of the body owes -- the end of the storage its declared
// variables live in -- and then the body completes by departing.
auto FunctionLowerer::FrameEdge() -> lir::BlockId {
  if (frame_edge_.has_value()) {
    return *frame_edge_;
  }
  const lir::BlockId edge = NewBlock();
  frame_edge_ = edge;
  const lir::BlockId resumed = current_;
  SetCurrent(edge);
  CloseVariables();
  Terminate(
      lir::DepartTerm{
          .departure = Load(
              lir::Place{
                  .base = lir::Use{.value = DepartureSlot()}, .chain = {}},
              unit_->ControlEffectType())});
  SetCurrent(resumed);
  return edge;
}

auto FunctionLowerer::EmitDepartingCall(
    lir::CallTarget target, std::vector<lir::Operand> args,
    lir::TypeId result_type) -> diag::Result<lir::Operand> {
  auto landing = LandingHere();
  if (!landing) {
    return std::unexpected(std::move(landing.error()));
  }
  const lir::BlockId returned = NewBlock();
  const lir::ValueId result = fn_.values.Add(
      lir::Local{
          .name = {}, .type = result_type, .kind = lir::LocalKind::kTemp});
  const bool owes_its_end = lir::CallMakesValue(target) &&
                            unit_->Types().Get(result_type).IsOwnedValue();
  Terminate(
      lir::DepartingCallInstr{
          .result = result,
          .target = std::move(target),
          .args = std::move(args),
          .returned = returned,
          .landing = *landing});
  SetCurrent(returned);
  // The value exists only where the call returned, so the landing, built
  // before it, owes nothing for it.
  if (owes_its_end) {
    OpenScope(ValueEnd{.value = result});
  }
  return lir::Operand{lir::Use{.value = result}};
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

auto FunctionLowerer::Emit(lir::TypeId type, lir::InstrData data)
    -> lir::Operand {
  if (const auto* call = std::get_if<lir::CallInstr>(&data);
      call != nullptr && MayDepart(lir::CallEndingOf(call->target))) {
    throw InternalError(
        "FunctionLowerer::Emit: a callee that can leave without returning has "
        "to be stated as a departing call, so that whatever is owed between "
        "here and the frame's edge gets its turn; please report this as a bug");
  }
  return Append(type, std::move(data));
}

auto FunctionLowerer::Append(lir::TypeId type, lir::InstrData data)
    -> lir::Operand {
  const lir::ValueId result = fn_.values.Add(
      lir::Local{.name = {}, .type = type, .kind = lir::LocalKind::kTemp});
  const bool owes_its_end =
      lir::MakesValue(data) && unit_->Types().Get(type).IsOwnedValue();
  blocks_[current_.value].instrs.push_back(
      lir::Instr{.result = result, .data = std::move(data)});
  if (owes_its_end) {
    OpenScope(ValueEnd{.value = result});
  }
  return lir::Use{.value = result};
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

void FunctionLowerer::OpenScope(ScopeKind kind) {
  scopes_.push_back(
      UnwindScope{.kind = std::move(kind), .unwind_entry = std::nullopt});
}

auto FunctionLowerer::Kept(lir::Operand value) -> lir::Operand {
  const std::optional<lir::TypeId> type = lir::OperandType(fn_, value);
  if (!type.has_value() || !unit_->Types().Get(*type).IsOwnedValue() ||
      OwesItsEnd(value)) {
    return value;
  }
  return Emit(
      *type, lir::CallInstr{
                 .target = lir::CopyValueTarget{}, .args = {std::move(value)}});
}

auto FunctionLowerer::OwesItsEnd(const lir::Operand& value) const -> bool {
  const auto* use = std::get_if<lir::Use>(&value);
  if (use == nullptr) {
    return false;
  }
  return std::ranges::any_of(scopes_, [&](const UnwindScope& scope) {
    const auto* end = std::get_if<ValueEnd>(&scope.kind);
    return end != nullptr && end->value == use->value;
  });
}

auto FunctionLowerer::HandOn(lir::Operand value) -> lir::Operand {
  const lir::TypeId type = lir::OperandType(fn_, value);
  if (!unit_->Types().Get(type).IsOwnedValue()) {
    return value;
  }
  if (const auto* use = std::get_if<lir::Use>(&value)) {
    for (std::size_t i = 0; i < scopes_.size(); ++i) {
      const auto* end = std::get_if<ValueEnd>(&scopes_[i].kind);
      if (end == nullptr || end->value != use->value) {
        continue;
      }
      scopes_[i].kind = EndPassedOn{};
      // What was built while the value was owed goes on ending it, which is
      // right for the calls made then; what is built from here on must not,
      // so this scope and every one inside it build theirs afresh.
      for (std::size_t j = i; j < scopes_.size(); ++j) {
        scopes_[j].unwind_entry = std::nullopt;
      }
      return value;
    }
  }
  return HandOn(Emit(
      type, lir::CallInstr{
                .target = lir::CopyValueTarget{}, .args = {std::move(value)}}));
}

auto FunctionLowerer::CompletionCellType(lir::TypeId payload) -> lir::TypeId {
  return unit_->Types().Intern(
      lir::Type{lir::PointerType{
          .pointee = payload,
          .ownership = lir::PointerOwnership::kBorrowed,
          .mutability = lir::Mutability::kMutable}});
}

auto FunctionLowerer::AllocateCompletionFor(lir::TypeId payload)
    -> lir::Operand {
  const lir::ValueId result = fn_.values.Add(
      lir::Local{
          .name = {},
          .type = CompletionCellType(payload),
          .kind = lir::LocalKind::kTemp});
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

// Introduces a declared local, holding its initial value. A local the body
// gave storage of its own was given it when the body opened, so reaching the
// declaration is what installs the representation -- which is what makes each
// entry to a declaration a fresh variable in the one storage. Every other
// local is a frame slot the declaration writes, and a slot of an owned type
// owns what it holds: it takes the initial value along with its end.
auto FunctionLowerer::BindLocal(
    mir::LocalId local, lir::TypeId type, lir::Operand init)
    -> std::optional<lir::ValueId> {
  if (variable_slot_[local.value].has_value()) {
    InitializeCell(
        std::get<CellBinding>(*locals_[local.value]).cell, std::move(init));
    return std::nullopt;
  }
  const lir::ValueId slot = NewPlaceLocal(type);
  locals_[local.value] = LocalBinding{PlaceBinding{.slot = slot}};
  const bool owns = unit_->Types().Get(type).IsOwnedValue();
  Emit(
      unit_->TranslateType(unit_->Mir().builtins.void_type),
      lir::StoreInstr{
          .place = LocalPlace(slot),
          .value = owns ? HandOn(std::move(init)) : std::move(init)});
  if (!owns) {
    return std::nullopt;
  }
  return slot;
}

auto FunctionLowerer::DeclareLocal(
    const mir::Block& block, mir::LocalId local, mir::ExprId init)
    -> diag::Result<void> {
  // A cell MIR declared is built where the body opens its variables, so its
  // declaration names storage that already exists; what it holds is the
  // initialization that follows.
  if (IsDeclaredCell(unit_->Mir(), code_->locals.Get(local).type)) {
    return {};
  }
  const std::size_t depth = scopes_.size();
  auto value = LowerExpr(block, init);
  if (!value) {
    return std::unexpected(std::move(value.error()));
  }
  const std::optional<lir::ValueId> owning = BindLocal(
      local, unit_->TranslateType(code_->locals.Get(local).type),
      *std::move(value));
  auto closed = CloseExtent(depth);
  if (!closed) {
    return closed;
  }
  if (owning.has_value()) {
    OpenScope(SlotEnd{.slot = *owning});
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
      value = HandOn(std::move(value));
      EndValue(Load(place, type));
    }
  }
  return Emit(
      unit_->TranslateType(unit_->Mir().builtins.void_type),
      lir::StoreInstr{.place = std::move(place), .value = std::move(value)});
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

void FunctionLowerer::OpenVariables() {
  if (fn_.variables.empty()) {
    return;
  }
  // The storage is the body's own and the body never reads through it; it only
  // names it when reaching a variable in it and when closing it.
  const lir::TypeId opened = unit_->Types().Intern(
      lir::Type{lir::PointerType{
          .pointee = unit_->Types().Intern(lir::Type{lir::VoidType{}}),
          .ownership = lir::PointerOwnership::kBorrowed,
          .mutability = lir::Mutability::kMutable}});
  variables_ = Emit(opened, lir::OpenVariablesInstr{});
  for (const mir::LocalId local : code_->locals.Ids()) {
    if (!variable_slot_[local.value].has_value()) {
      continue;
    }
    const lir::TypeId value = unit_->TranslateType(
        StoredValueType(unit_->Mir(), code_->locals.Get(local).type));
    const lir::TypeId address = unit_->Types().Intern(
        lir::Type{lir::PointerType{
            .pointee = lir::CellOf(unit_->Types(), value),
            .ownership = lir::PointerOwnership::kBorrowed,
            .mutability = lir::Mutability::kMutable}});
    locals_[local.value] = LocalBinding{CellBinding{
        .cell = Emit(
            address, lir::VariableAddressInstr{
                         .variables = *variables_,
                         .position = lir::VariablePosition{
                             *variable_slot_[local.value]}})}};
  }
}

void FunctionLowerer::CloseVariables() {
  if (!variables_.has_value()) {
    return;
  }
  Emit(
      unit_->TranslateType(unit_->Mir().builtins.void_type),
      lir::CloseVariablesInstr{.variables = *variables_});
}

auto FunctionLowerer::InitializeCell(lir::Operand cell, lir::Operand value)
    -> lir::Operand {
  return Emit(
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
            return LowerBlockInto(block.child_scopes.Get(s.scope));
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
            // returned value, and what awaits it runs later. A caller that
            // does stand below takes the value, and its end with it.
            if (completion_cell_.has_value() && value.has_value()) {
              StoreActivationValue(
                  completion_cell_->cell, *std::move(value),
                  completion_cell_->type);
              value.reset();
            } else if (value.has_value()) {
              value = HandOn(*std::move(value));
            }
            // Returning leaves every guarded body between here and the frame's
            // edge, so each of their cleanups runs -- after the returned value
            // is settled, which a cleanup must not change.
            auto cleaned = RunCleanupsDownTo(0);
            if (!cleaned) {
              return std::unexpected(std::move(cleaned.error()));
            }
            CloseVariables();
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
          .scope_depth = scopes_.size()});
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
          .scope_depth = scopes_.size()});
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
  loops_.push_back(
      LoopTargets{
          .label = stmt.break_label,
          .continue_target = step_id,
          .break_target = exit_id,
          .scope_depth = scopes_.size()});
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
  // An unlabeled break leaves the innermost loop; a labeled one leaves the
  // loop that carries the label, however many loops it is nested inside.
  for (const LoopTargets& loop : std::views::reverse(loops_)) {
    if (!stmt.target.has_value() || loop.label == stmt.target) {
      auto cleaned = RunCleanupsDownTo(loop.scope_depth);
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
  auto cleaned = RunCleanupsDownTo(loops_.back().scope_depth);
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
              EndValue(lir::Use{.value = end.value});
              return {};
            },
            [&](const SlotEnd& end) -> diag::Result<void> {
              EndSlotValue(end.slot);
              return {};
            },
            [](const EndPassedOn&) -> diag::Result<void> { return {}; },
            // Leaving a region, or a cleanup's own code, by an ordinary way out
            // owes it nothing: a region's handler is only for a departure.
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

auto FunctionLowerer::SuspendResumingAt(lir::BlockId resume)
    -> diag::Result<void> {
  const lir::BlockId abandoned = NewBlock();
  Terminate(lir::SuspendTerm{.resume = resume, .abandoned = abandoned});
  SetCurrent(abandoned);
  auto cleaned = RunCleanupsDownTo(0);
  if (!cleaned) {
    return std::unexpected(std::move(cleaned.error()));
  }
  CloseVariables();
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

  // The effect is written where control leaves the body and read where the
  // handler runs, which are different blocks, so it is frame storage rather
  // than a value in flight.
  const lir::ValueId caught =
      NewPlaceLocal(unit_->TranslateType(code_->locals.Get(stmt.caught).type));
  locals_[stmt.caught.value] = PlaceBinding{.slot = caught};

  OpenScope(RegionScope{.handler = handler_id, .caught = caught});
  auto body = LowerBlockInto(block.child_scopes.Get(stmt.body));
  scopes_.pop_back();
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
  // execution resumes past the region (LRM 9.6.2), and the departure it was
  // holding is released here because nothing carries it any further.
  if (!Terminated()) {
    Emit(
        unit_->TranslateType(unit_->Mir().builtins.void_type),
        lir::CallInstr{
            .target =
                lir::ControlEffectTarget{
                    .op = lir::ControlEffectTarget::Op::kFinishDeparture},
            .args = {}});
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

auto FunctionLowerer::MemberRefOf(const mir::FieldRef& field)
    -> lir::StatedMemberRef {
  const auto at = [](lir::TypeId declared_by, auto slot) {
    return lir::StatedMemberRef{
        .declared_by = declared_by, .slot = lir::MemberSlot{slot.value}};
  };
  return std::visit(
      Overloaded{
          [&](const mir::ClassFieldTarget& t) {
            return at(unit_->ClassValueType(t.owner), t.slot);
          },
          [&](const mir::ClosureFieldTarget& t) {
            return at(unit_->ClosureValueType(t.owner), t.slot);
          },
          [&](const mir::CrossUnitClassFieldTarget& t) {
            return at(
                unit_->ExternalClassValueType(t.unit_name, t.class_name),
                t.slot);
          }},
      field);
}

namespace {

// The value a call reaches its part out of, and nothing where the call reaches
// no part. A step of a descent is a call whose entry answers with the part
// rather than with the part's value, so both questions -- whether this is one,
// and what it descends through -- are answered by the entry's own declaration
// and the call's receiver. Reaching further would be asking where a write
// through it ultimately lands, a different question.
auto PartReceiver(const mir::CallExpr& call) -> std::optional<mir::ExprId> {
  const std::optional<support::BuiltinFn> fn = mir::DirectBuiltinFn(call);
  if (!fn.has_value()) {
    return std::nullopt;
  }
  switch (support::RuntimeEntryOf(*fn).answer) {
    case support::EntryAnswer::kPartOfTheReceiver:
      return mir::CalleeReceiver(call.callee);
    case support::EntryAnswer::kNewValue:
    case support::EntryAnswer::kTheReceiver:
      return std::nullopt;
  }
  throw InternalError("mir_to_lir: unknown entry answer");
}

auto ValuePartReceiver(const mir::Block& block, mir::ExprId step)
    -> std::optional<mir::ExprId> {
  const auto* call = std::get_if<mir::CallExpr>(&block.exprs.Get(step).data);
  return call == nullptr ? std::nullopt : PartReceiver(*call);
}

// Whether an expression names a part of a value rather than storage.
auto ReachesIntoValue(const mir::Block& block, mir::ExprId target) -> bool {
  return ValuePartReceiver(block, target).has_value();
}

// The wrapper an expression reads what it holds of, where the expression is
// that read.
auto ReadWrapper(const mir::Expr& expr) -> std::optional<mir::ExprId> {
  const auto* call = std::get_if<mir::CallExpr>(&expr.data);
  if (call == nullptr ||
      mir::DirectBuiltinFn(*call) != support::BuiltinFn::kLoad) {
    return std::nullopt;
  }
  return mir::CalleeReceiver(call->callee);
}

// How a call names the part it reaches, and nothing for a call that reaches
// none. The entry's own declaration says, so reading and writing one part
// answer alike.
auto SelectionOf(const mir::CallExpr& call)
    -> std::optional<support::PartSelection> {
  const std::optional<support::BuiltinFn> fn = mir::DirectBuiltinFn(call);
  if (!fn.has_value()) {
    return std::nullopt;
  }
  return support::RuntimeEntryOf(*fn).selects;
}

// The position a call reaching a component by its position names. The position
// is fixed by the call rather than computed, so it rides on the callee.
auto PositionOf(const mir::CallExpr& call) -> base::ComponentIndex {
  const std::optional<base::ComponentIndex> position =
      std::get<mir::Direct>(call.callee).position;
  if (!position.has_value()) {
    throw InternalError(
        "mir_to_lir: an entry reaching a part by its position names that "
        "position, and this call names none -- please report this as a bug");
  }
  return *position;
}

}  // namespace

auto FunctionLowerer::StorageSelection(const mir::Block& block, mir::ExprId id)
    const -> std::optional<support::PartSelection> {
  const auto* call = std::get_if<mir::CallExpr>(&block.exprs.Get(id).data);
  if (call == nullptr) {
    return std::nullopt;
  }
  const std::optional<support::PartSelection> selects = SelectionOf(*call);
  const std::optional<mir::ExprId> receiver = mir::CalleeReceiver(call->callee);
  if (!selects.has_value() || !receiver.has_value() ||
      !unit_->Mir()
           .types.Get(block.exprs.Get(*receiver).type)
           .PartsAreStorage()) {
    return std::nullopt;
  }
  return selects;
}

auto FunctionLowerer::StoragePartAccess(
    const mir::Block& block, mir::ExprId id) const -> const mir::CallExpr* {
  const std::optional<support::PartSelection> selects =
      StorageSelection(block, id);
  if (!selects.has_value()) {
    return nullptr;
  }
  switch (*selects) {
    case support::PartSelection::kElement:
    case support::PartSelection::kComponent:
      return &std::get<mir::CallExpr>(block.exprs.Get(id).data);
    // A slice is several elements rather than one storage.
    case support::PartSelection::kSlice:
      return nullptr;
  }
  throw InternalError("mir_to_lir: unknown part selection");
}

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

auto FunctionLowerer::PartPlace(
    const mir::Block& block, const mir::CallExpr& part, Reach reach)
    -> diag::Result<lir::Place> {
  const std::optional<mir::ExprId> receiver = mir::CalleeReceiver(part.callee);
  if (!receiver.has_value()) {
    throw InternalError(
        "mir_to_lir: a part is reached out of a value, and this access names "
        "none -- please report this as a bug");
  }
  auto place = PlaceHolding(block, *receiver, HolderReach(reach));
  if (!place) {
    return std::unexpected(std::move(place.error()));
  }
  auto step = PartStep(block, part);
  if (!step) {
    return std::unexpected(std::move(step.error()));
  }
  place->chain.push_back(*std::move(step));
  return place;
}

auto FunctionLowerer::PartStep(
    const mir::Block& block, const mir::CallExpr& part)
    -> diag::Result<lir::Projection> {
  const std::optional<support::PartSelection> selects = SelectionOf(part);
  if (!selects.has_value()) {
    throw InternalError(
        "mir_to_lir: a step into a part is made by a call that reaches one -- "
        "please report this as a bug");
  }
  switch (*selects) {
    case support::PartSelection::kComponent:
      return lir::Projection{
          lir::ComponentProjection{.index = PositionOf(part)}};
    case support::PartSelection::kElement: {
      auto coordinates = LowerEachExpr(block, part.arguments);
      if (!coordinates) {
        return std::unexpected(std::move(coordinates.error()));
      }
      return lir::Projection{
          lir::ElementProjection{.coordinates = *std::move(coordinates)}};
    }
    case support::PartSelection::kSlice:
      throw InternalError(
          "mir_to_lir: a slice is several elements and never one step of a "
          "place -- please report this as a bug");
  }
  throw InternalError("mir_to_lir: unknown part selection");
}

auto FunctionLowerer::PlaceHolding(
    const mir::Block& block, mir::ExprId id, Reach reach)
    -> diag::Result<lir::Place> {
  if (NamesStorage(block, id)) {
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
  const lir::ValueId slot = NewPlaceLocal(type);
  Emit(
      unit_->TranslateType(unit_->Mir().builtins.void_type),
      lir::StoreInstr{
          .place = LocalPlace(slot), .value = HandOn(std::move(value))});
  if (unit_->Types().Get(type).IsOwnedValue()) {
    OpenScope(SlotEnd{.slot = slot});
  }
  return slot;
}

auto FunctionLowerer::NamesStorage(
    const mir::Block& block, mir::ExprId id) const -> bool {
  const mir::Expr& expr = block.exprs.Get(id);
  if (StoragePartAccess(block, id) != nullptr ||
      ReadWrapper(expr).has_value()) {
    return true;
  }
  if (const auto* reference = std::get_if<mir::ReferenceExpr>(&expr.data)) {
    return std::visit(
        Overloaded{
            [](const mir::LocalRef&) { return true; },
            [](const mir::StaticVariableRef&) { return true; },
            [](const mir::ExternalUnitVariableRef&) { return true; },
            [](const mir::StaticPropertyRef&) { return true; },
            [](const mir::ExternalStaticPropertyRef&) { return true; },
            // What the unit holds as a value, or reaches only to ask
            // something of, is not storage a part could be reached in.
            [](const mir::TypeDescriptorRef&) { return false; },
            [](const mir::IntegralConstantRef&) { return false; },
            [](const mir::DefinitionRef&) { return false; },
            [](const mir::ClassConstantRef&) { return false; },
            [](const mir::FunctionRef&) { return false; }},
        reference->target);
  }
  return std::holds_alternative<mir::FieldAccessExpr>(expr.data) ||
         std::holds_alternative<mir::DerefExpr>(expr.data);
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

auto FunctionLowerer::WrapperContentsPlace(
    const mir::Block& block, mir::ExprId wrapper) -> diag::Result<lir::Place> {
  const mir::Type& wrapper_ty =
      unit_->Mir().types.Get(block.exprs.Get(wrapper).type);
  // Dereferencing a place designated within a write lands the write there.
  if (const auto* designated = wrapper_ty.As<mir::DesignationType>()) {
    auto designation = LowerExpr(block, wrapper);
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
  auto pointer = LowerExpr(block, wrapper);
  if (!pointer) {
    return std::unexpected(std::move(pointer.error()));
  }
  // Opening a reference lands on the value the storage it names holds, which
  // is what the reference states; which of the two forms that storage is, is
  // the reference's own to answer, so every access through this place is one
  // of its operations rather than a load or a store of an address.
  if (wrapper_ty.Is<mir::RefType>()) {
    return StorageAt(*std::move(pointer));
  }
  return lir::Place{
      .base = *std::move(pointer),
      .chain = {lir::Projection{lir::DerefProjection{}}}};
}

auto FunctionLowerer::ReferenceValue(
    const mir::Block& block, mir::ExprId id, const mir::ReferenceTarget& target,
    mir::TypeId type) -> diag::Result<lir::Operand> {
  return std::visit(
      Overloaded{
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
                    // What comes out is what the storage holds, which is the
                    // type the declaration gave it -- not whatever the
                    // expression around the read is typed as, which may be a
                    // reading of that type rather than the type itself.
                    [&](const CellBinding& cell) -> diag::Result<lir::Operand> {
                      return Load(
                          ValueAt(cell.cell),
                          unit_->TranslateType(
                              code_->locals.Get(ref.var).type));
                    }},
                *binding);
          },
          [&](const mir::TypeDescriptorRef& ref) -> diag::Result<lir::Operand> {
            return lir::Operand{lir::TypeDescriptorRef{
                .descriptor = UnitLowerer::TranslateDescriptor(ref.descriptor),
                .type = unit_->TranslateType(type)}};
          },
          [&](const mir::IntegralConstantRef& ref)
              -> diag::Result<lir::Operand> {
            return lir::Operand{lir::IntegralConstantRef{
                .constant = UnitLowerer::TranslateConstant(ref.constant),
                .type = unit_->TranslateType(type)}};
          },
          [&](const mir::StaticVariableRef&) -> diag::Result<lir::Operand> {
            return ReadPlace(block, id, unit_->TranslateType(type));
          },
          [&](const mir::ExternalUnitVariableRef&)
              -> diag::Result<lir::Operand> {
            return ReadPlace(block, id, unit_->TranslateType(type));
          },
          [&](const mir::DefinitionRef&) -> diag::Result<lir::Operand> {
            throw InternalError(
                "mir_to_lir: a class's definition is read whole, where only "
                "its address is ever taken -- please report this as a bug");
          },
          [&](const mir::StaticPropertyRef&) -> diag::Result<lir::Operand> {
            return ReadPlace(block, id, unit_->TranslateType(type));
          },
          [&](const mir::ExternalStaticPropertyRef&)
              -> diag::Result<lir::Operand> {
            return ReadPlace(block, id, unit_->TranslateType(type));
          },
          // Only a constant names another constant or a body's address, and a
          // constant is data rather than a body.
          [&](const mir::ClassConstantRef&) -> diag::Result<lir::Operand> {
            throw InternalError(
                "mir_to_lir: a body reads a class's constant -- please report "
                "this as a bug");
          },
          [&](const mir::FunctionRef&) -> diag::Result<lir::Operand> {
            throw InternalError(
                "mir_to_lir: a body names a function as a value -- please "
                "report this as a bug");
          }},
      target);
}

auto FunctionLowerer::SymbolPlace(std::string symbol, mir::TypeId type)
    -> lir::Place {
  return lir::Place{
      .base =
          lir::StaticRef{
              .symbol = std::move(symbol),
              .type = unit_->Types().Intern(
                  lir::Type{lir::PointerType{
                      .pointee = unit_->TranslateType(type),
                      .ownership = lir::PointerOwnership::kBorrowed,
                      .mutability = lir::Mutability::kMutable}})},
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
              return Unsupported("mir_to_lir: reference to an unlowered local");
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
          // under rather than through a receiver -- by the unit that declares
          // it exactly as by any other, since a namespace has no instance.
          [&](const mir::StaticVariableRef& ref) -> diag::Result<lir::Place> {
            const mir::CompilationUnit& mir = unit_->Mir();
            return SymbolPlace(
                lir::NamespaceVariableSymbol(
                    mir.name,
                    lir::SymbolPartOf(
                        mir::NameOf(mir.named_static_variables, ref.variable),
                        ref.variable.value)),
                type);
          },
          [&](const mir::ExternalUnitVariableRef& ref)
              -> diag::Result<lir::Place> {
            return SymbolPlace(
                lir::NamespaceVariableSymbol(
                    ref.unit_name, lir::SymbolPart::Name(ref.variable_name)),
                type);
          },
          // A cell a class owns rather than an object of it (LRM 8.9) is that
          // same one cell for the whole program, so it is reached the same way
          // and differs only in how far its name is qualified. The class the
          // reference names is the one that declares the cell, however the
          // source spelled it, so the symbol is settled without reading what
          // any other class holds.
          [&](const mir::StaticPropertyRef& ref) -> diag::Result<lir::Place> {
            const mir::Class& cls = unit_->Mir().GetClass(ref.owner);
            return SymbolPlace(
                lir::StaticPropertySymbol(
                    unit_->Mir().name,
                    lir::SymbolPartOf(cls.name, ref.owner.value),
                    lir::SymbolPartOf(
                        mir::NameOf(cls.named_static_properties, ref.prop),
                        ref.prop.value)),
                type);
          },
          [&](const mir::ExternalStaticPropertyRef& ref)
              -> diag::Result<lir::Place> {
            return SymbolPlace(
                lir::StaticPropertySymbol(
                    ref.unit_name, lir::SymbolPart::Name(ref.class_name),
                    lir::SymbolPart::Name(ref.property_name)),
                type);
          },
          // A class's definition is a constant the declaring unit emits, so
          // the place opens at its address.
          [&](const mir::DefinitionRef& r) -> diag::Result<lir::Place> {
            return lir::Place{
                .base =
                    lir::DefinitionRef{
                        .defined = unit_->ClassRefValueType(r.of),
                        .type = unit_->Types().Intern(
                            lir::Type{lir::PointerType{
                                .pointee = unit_->TranslateType(type),
                                .ownership = lir::PointerOwnership::kBorrowed,
                                .mutability = lir::Mutability::kReadOnly}})},
                .chain = {lir::Projection{lir::DerefProjection{}}}};
          },
          // A descriptor, a constant and a function are values the unit holds,
          // not storage anything writes through.
          [](const mir::TypeDescriptorRef&) -> diag::Result<lir::Place> {
            return Unsupported(
                "mir_to_lir: a type's runtime descriptor names no place");
          },
          [](const mir::IntegralConstantRef&) -> diag::Result<lir::Place> {
            return Unsupported("mir_to_lir: a constant names no place");
          },
          [](const mir::ClassConstantRef&) -> diag::Result<lir::Place> {
            return Unsupported("mir_to_lir: a class's constant names no place");
          },
          [](const mir::FunctionRef&) -> diag::Result<lir::Place> {
            return Unsupported("mir_to_lir: a function names no place");
          }},
      target);
}

auto FunctionLowerer::LowerPlace(
    const mir::Block& block, mir::ExprId id, Reach reach)
    -> diag::Result<lir::Place> {
  const mir::Expr& expr = block.exprs.Get(id);
  // A part that is storage of its own is a step of the place holding the value
  // it is part of.
  if (const mir::CallExpr* part = StoragePartAccess(block, id)) {
    return PartPlace(block, *part, reach);
  }
  // A part that is a view of its whole is a position in it rather than
  // storage, so there is nothing for anything to bind. A write through one
  // still has a realization -- read the whole, replace the part, store it back
  // -- because nothing there has to outlive the expression.
  if (ReachesIntoValue(block, id)) {
    return Unsupported(
        "mir_to_lir: binding part of a value rather than writing it is not yet "
        "lowerable to LIR");
  }
  // Reading what a wrapper holds names the wrapper's contents, which is the
  // same storage a dereference of the wrapper names.
  if (const std::optional<mir::ExprId> wrapper = ReadWrapper(expr)) {
    return WrapperContentsPlace(block, *wrapper);
  }
  const auto names_no_place =
      [](std::string_view form) -> diag::Result<lir::Place> {
    return Unsupported(std::format("mir_to_lir: {} names no place", form));
  };
  return std::visit(
      Overloaded{
          [&](const mir::ReferenceExpr& reference) -> diag::Result<lir::Place> {
            return ReferencePlace(reference.target, expr.type, reach);
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
                        .member = MemberRefOf(field.field)}}}};
          },
          [&](const mir::DerefExpr& deref) -> diag::Result<lir::Place> {
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
          [&](const mir::DynamicCastExpr&) {
            return names_no_place("a converted value");
          },
          [&](const mir::ConditionalExpr&) {
            return names_no_place("a value a condition chooses");
          },
          [&](const mir::BlockExpr&) {
            return names_no_place("the value a sequence of steps settles");
          },
          [&](const mir::AssignExpr&) {
            return names_no_place("the value an assignment yields");
          },
          [&](const mir::IncDecExpr&) {
            return names_no_place("the value an increment yields");
          },
          [&](const mir::CallExpr&) {
            return names_no_place("a call's result");
          },
          [&](const mir::AddressOfExpr&) {
            return names_no_place("an address of storage");
          },
          [&](const mir::MoveExpr&) {
            return names_no_place("a value moved out of where it was kept");
          },
          [&](const mir::ClosureExpr&) { return names_no_place("a closure"); },
          [&](const mir::CompositeExpr&) {
            return names_no_place("a value composed of its parts");
          },
          [&](const mir::AwaitExpr&) {
            return names_no_place("the value an await settles");
          },
          [&](const mir::WaitExpr&) {
            return names_no_place("a wait, which yields nothing");
          },
          [&](const mir::VectorGetExpr&) {
            return names_no_place("the handle a sequence answers with");
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
  auto place = LowerPlace(block, id, Reach::kRead);
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
  if (const std::optional<mir::ExprId> receiver =
          mir::CalleeReceiver(call.callee)) {
    if (auto lowered = lower_into(*receiver); !lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
  }
  for (const mir::ExprId argument : call.arguments) {
    if (auto lowered = lower_into(argument); !lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
  }
  return args;
}

auto FunctionLowerer::LowerObjectConstruction(
    const mir::Block& block, const mir::CallExpr& call, mir::TypeId type)
    -> diag::Result<lir::Operand> {
  // A `new` allocates the object, runs its constructor on it, and only then
  // hands it to the handle that owns it from there, as `std::shared_ptr` is
  // given what a C++ `new` built.
  const lir::TypeId handle_type = unit_->TranslateType(type);
  const lir::TypeId object_type =
      unit_->Types().Get(handle_type).Get<lir::ManagedRefType>().pointee;
  auto allocated = EmitCallTo(
      lir::ConstructTarget{}, {},
      unit_->Types().Intern(
          lir::Type{lir::PointerType{
              .pointee = object_type,
              .ownership = lir::PointerOwnership::kUnique,
              .mutability = lir::Mutability::kMutable}}));
  if (!allocated) {
    return allocated;
  }
  const lir::Operand object = *std::move(allocated);

  // A construction carries every argument its constructor takes -- the source
  // wrote the call, so the front end bound it against the declaration and
  // filled in whatever it left to a default. So there is nothing to establish
  // about the arguments here, and nothing to read about the class beyond how
  // its constructor is named.
  std::vector<lir::Operand> args;
  args.reserve(call.arguments.size());
  for (const mir::ExprId argument : call.arguments) {
    auto lowered = LowerArgument(block, argument);
    if (!lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
    args.push_back(*std::move(lowered));
  }
  if (auto entered = EnterConstructor(
          mir::ClassOfObject(
              unit_->Mir().types,
              unit_->Mir().types.Get(type).Get<mir::ManagedRefType>().pointee),
          object, std::move(args));
      !entered) {
    return std::unexpected(std::move(entered.error()));
  }
  return EmitCallTo(lir::ConstructTarget{}, {object}, handle_type);
}

auto FunctionLowerer::EnterConstructor(
    const mir::DeclaredClassRef& cls, const lir::Operand& object,
    std::vector<lir::Operand> arguments) -> diag::Result<void> {
  const EnteredConstructor constructor = ConstructorOf(cls);
  // The object leads its arguments the way a receiver leads any body's
  // parameters, and what built it names it rather than being it, so opening
  // that is what reaches the storage the body runs on.
  arguments.insert(
      arguments.begin(),
      Emit(
          unit_->Types().Intern(
              lir::Type{lir::PointerType{
                  .pointee = constructor.object_type,
                  .ownership = lir::PointerOwnership::kBorrowed,
                  .mutability = lir::Mutability::kMutable}}),
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

auto FunctionLowerer::LowerScopeConstruction(
    const mir::Block& block, const mir::CallExpr& call, mir::TypeId type)
    -> diag::Result<lir::Operand> {
  std::vector<lir::Operand> arguments;
  arguments.reserve(call.arguments.size());
  for (const mir::ExprId argument : call.arguments) {
    auto lowered = LowerArgument(block, argument);
    if (!lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
    arguments.push_back(*std::move(lowered));
  }
  // The storage is allocated as for a value built with `new`, and the class's
  // constructor then runs on it with every argument, building each part from
  // the base up.
  auto begun =
      EmitCallTo(lir::ConstructTarget{}, {}, unit_->TranslateType(type));
  if (!begun) {
    return begun;
  }
  const lir::Operand scope = *std::move(begun);
  if (auto entered = EnterConstructor(
          mir::ClassOfObject(
              unit_->Mir().types,
              unit_->Mir().types.Get(type).Get<mir::PointerType>().pointee),
          scope, std::move(arguments));
      !entered) {
    return std::unexpected(std::move(entered.error()));
  }
  return scope;
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
  // A reference built over a reference denotes the storage at the end of the
  // chain rather than binding afresh (LRM 23.3.3.2), so what it carries is the
  // reference it was handed and there is no second address to take.
  if (unit_->Mir()
          .types.Get(block.exprs.Get(call.arguments[0]).type)
          .Is<mir::RefType>()) {
    return LowerExpr(block, call.arguments[0]);
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
  // A part is lent by a step taken on a reference to its whole, which is what
  // carries the variable the part belongs to; a reference built over the part's
  // own place would name the storage without it.
  if (StoragePartAccess(block, referent) != nullptr) {
    throw InternalError(
        "mir_to_lir: a reference is built over a whole, and a part is lent by "
        "a step taken on one");
  }
  // A local the body gave storage of its own is named by the handle the body
  // opened over that storage, which reaches the storage in one step where
  // reading the local reaches its value in two.
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

auto FunctionLowerer::LowerCall(
    const mir::Block& block, const mir::CallExpr& call, mir::TypeId type)
    -> diag::Result<lir::Operand> {
  // A method that changes the object it is applied to changes it where it
  // lies, which is a question of where the receiver is.
  if (const auto fn = mir::DirectBuiltinFn(call);
      fn.has_value() && support::RuntimeEntryOf(*fn).mutates_receiver) {
    return LowerMutatingCall(block, call, *fn, type);
  }

  // A call answering with a part that is a view of its whole has no storage to
  // reach, so in a value position it is the extraction, and in a target
  // position it is the rebuild the write does.
  if (const std::optional<mir::ExprId> owner = PartReceiver(call)) {
    auto whole = LowerExpr(block, *owner);
    if (!whole) {
      return std::unexpected(std::move(whole.error()));
    }
    auto selector = LowerValuePartSelector(block, call);
    if (!selector) {
      return std::unexpected(std::move(selector.error()));
    }
    return Emit(
        unit_->TranslateType(type),
        lir::AggregateExtractInstr{
            .aggregate = *std::move(whole), .selector = *std::move(selector)});
  }

  // A reference is the address of the cell it binds, so building one is the
  // ordinary address-of over the referent's place. No runtime value stands
  // between the holder and that cell; reading and writing through the reference
  // are the cell's own access, reached the way every other call is.
  if (BindsReference(unit_->Mir().types, call, type)) {
    return LowerReferenceBind(block, call, type);
  }

  // Bringing an object into existence and initializing it are two operations
  // over one heap the runtime owns: it answers an object whose properties hold
  // their storage's default, and the class's own constructor -- a body of this
  // program, reached like any other -- is what runs on it (LRM 8.7).
  if (BuildsObject(unit_->Mir().types, call, type)) {
    return LowerObjectConstruction(block, call, type);
  }
  if (BuildsScope(unit_->Mir().types, call, type)) {
    return LowerScopeConstruction(block, call, type);
  }
  // Reached where nothing awaits the execution -- a process handed to the
  // scheduler. Such a body finishes with no value, so there is nothing for it
  // to complete into; the awaiting form supplies that storage itself.
  if (unit_->Mir().types.Get(type).Is<mir::CoroutineType>()) {
    return EnterCoroutine(block, call, type, std::nullopt);
  }

  if (const auto fn = mir::DirectBuiltinFn(call); fn.has_value()) {
    // A foreign caller cannot be parked, so the entry point it reached drives
    // the body to its end where it stands instead of waiting for it (LRM 35.8).
    if (*fn == support::BuiltinFn::kRunExportedTaskToCompletion) {
      return LowerDriveToCompletion(block, call, type);
    }
    if (*fn == support::BuiltinFn::kTakeDepartureIfDue) {
      auto checked = TakeDepartureIfDue();
      if (!checked) {
        return std::unexpected(std::move(checked.error()));
      }
      // The gate answers nothing, so what stands here is never read.
      return lir::Operand{lir::IntConst{
          .value = lir::IntegralConstant{.value_words = {0}, .state_words = {}},
          .type = unit_->TranslateType(type)}};
    }
  }

  auto args = LowerCallOperands(block, call);
  if (!args) {
    return std::unexpected(std::move(args.error()));
  }
  return EmitCall(block, call, *std::move(args), unit_->TranslateType(type));
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
  // A body that completes with nothing leaves nothing to read, so what stands
  // here is never used.
  return driven;
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
              lir::CoroutineTarget{
                  .op = lir::CoroutineTarget::Op::kEnterBorrowedEnvironment},
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
  switch (lir::CallEndingOf(target)) {
    case support::CallEnding::kReturns:
      return Emit(
          result_type,
          lir::CallInstr{.target = std::move(target), .args = std::move(args)});
    case support::CallEnding::kReturnsOrDeparts:
      return EmitDepartingCall(std::move(target), std::move(args), result_type);
    case support::CallEnding::kDeparts: {
      auto departed =
          EmitDepartingCall(std::move(target), std::move(args), result_type);
      if (!departed) {
        return departed;
      }
      // Nothing comes back to the point the call returns to, so whatever the
      // source wrote after the call is never reached and has no lowering.
      Terminate(lir::UnreachableTerm{});
      return departed;
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
        "mir_to_lir: an awaited execution that completes with a value must be "
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
  const lir::Operand park = *std::move(awaited);
  const lir::BlockId parked = NewBlock();
  const lir::BlockId resume = NewBlock();
  Terminate(
      lir::CondBranchTerm{
          .condition = park, .if_true = parked, .if_false = resume});
  SetCurrent(parked);
  auto suspended = SuspendResumingAt(resume);
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
  // A completion that carries nothing yields nothing, so what stands here is
  // never read.
  return completion.value_or(park);
}

auto FunctionLowerer::LowerCompoundOperator(
    mir::BinaryOp op, lir::Operand old_value, lir::Operand rhs,
    lir::TypeId type) -> lir::Operand {
  return Emit(
      type, lir::BinaryInstr{
                .op = TranslateBinaryOp(op),
                .lhs = std::move(old_value),
                .rhs = std::move(rhs)});
}

auto FunctionLowerer::LowerAssign(
    const mir::Block& block, const mir::AssignExpr& assign)
    -> diag::Result<lir::Operand> {
  // The assignment's own value is what it wrote, which the target's update
  // yields nothing of -- a write completes with void -- so it is kept here as
  // the change runs.
  std::optional<lir::Operand> assigned;
  auto written = UpdateTarget(
      block, assign.target,
      [&](const ValueReader& read_old,
          lir::TypeId type) -> diag::Result<lir::Operand> {
        auto rhs = LowerExpr(block, assign.value);
        if (!rhs) {
          return std::unexpected(std::move(rhs.error()));
        }
        if (!assign.compound_op.has_value()) {
          assigned = *std::move(rhs);
          return *assigned;
        }
        auto old_value = read_old();
        if (!old_value) {
          return std::unexpected(std::move(old_value.error()));
        }
        assigned = LowerCompoundOperator(
            *assign.compound_op, *std::move(old_value), *std::move(rhs), type);
        return *assigned;
      });
  if (!written) {
    return std::unexpected(std::move(written.error()));
  }
  return *assigned;
}

auto FunctionLowerer::UpdateTarget(
    const mir::Block& block, mir::ExprId target, const ValueChange& change)
    -> diag::Result<lir::Operand> {
  // A slice of elements is several of them rather than storage of its own, so
  // what it is changed to is written into the elements already there.
  if (const mir::CallExpr* slice = StorageSlice(block, target)) {
    return LowerSliceUpdate(block, *slice, target, change);
  }
  // A target that reaches into a view of a value -- a bit or slice of a packed
  // value, a character of a string, a member of a union, or any composition of
  // them -- is not a place: what goes back is the whole the view is of, with
  // the part changed.
  if (ReachesViewedPart(block, target)) {
    return UpdateThroughView(block, target, change);
  }
  const lir::TypeId type = unit_->TranslateType(block.exprs.Get(target).type);
  auto place = LowerPlace(block, target, Reach::kWhole);
  if (!place) {
    return std::unexpected(std::move(place.error()));
  }
  auto changed = change(
      [&]() -> diag::Result<lir::Operand> { return Load(*place, type); }, type);
  if (!changed) {
    return std::unexpected(std::move(changed.error()));
  }
  return Store(*std::move(place), *std::move(changed));
}

auto FunctionLowerer::LowerValuePartSelector(
    const mir::Block& block, const mir::CallExpr& call)
    -> diag::Result<lir::AggregateSelector> {
  const std::optional<support::PartSelection> selects = SelectionOf(call);
  if (!selects.has_value()) {
    throw InternalError(
        "mir_to_lir: a part is selected by a call that reaches one -- please "
        "report this as a bug");
  }
  switch (*selects) {
    case support::PartSelection::kComponent:
      return lir::AggregateSelector{lir::Component{.index = PositionOf(call)}};
    case support::PartSelection::kElement: {
      auto operands = LowerEachExpr(block, call.arguments);
      if (!operands) {
        return std::unexpected(std::move(operands.error()));
      }
      return lir::AggregateSelector{
          lir::ContainerElement{.operands = *std::move(operands)}};
    }
    case support::PartSelection::kSlice: {
      auto operands = LowerEachExpr(block, call.arguments);
      if (!operands) {
        return std::unexpected(std::move(operands.error()));
      }
      return lir::AggregateSelector{
          lir::ContainerSlice{.operands = *std::move(operands)}};
    }
  }
  throw InternalError("mir_to_lir: unknown part selection");
}

auto FunctionLowerer::ReachesViewedPart(
    const mir::Block& block, mir::ExprId target) const -> bool {
  mir::ExprId reached = target;
  while (const std::optional<mir::ExprId> receiver =
             ValuePartReceiver(block, reached)) {
    if (StoragePartAccess(block, reached) == nullptr) {
      return true;
    }
    reached = *receiver;
  }
  return false;
}

auto FunctionLowerer::StorageSlice(
    const mir::Block& block, mir::ExprId target) const -> const mir::CallExpr* {
  // A slice designated within a write is where that write lands.
  if (const auto* landed =
          std::get_if<mir::DerefExpr>(&block.exprs.Get(target).data)) {
    const auto* call =
        std::get_if<mir::CallExpr>(&block.exprs.Get(landed->pointer).data);
    const std::optional<support::PartSelection> selects =
        call == nullptr ? std::nullopt : SelectionOf(*call);
    if (!selects.has_value()) {
      return nullptr;
    }
    switch (*selects) {
      case support::PartSelection::kSlice:
        return call;
      case support::PartSelection::kElement:
      case support::PartSelection::kComponent:
        return nullptr;
    }
    throw InternalError("mir_to_lir: unknown part selection");
  }
  const std::optional<support::PartSelection> selects =
      StorageSelection(block, target);
  if (!selects.has_value()) {
    return nullptr;
  }
  switch (*selects) {
    case support::PartSelection::kSlice:
      return &std::get<mir::CallExpr>(block.exprs.Get(target).data);
    case support::PartSelection::kElement:
    case support::PartSelection::kComponent:
      return nullptr;
  }
  throw InternalError("mir_to_lir: unknown part selection");
}

auto FunctionLowerer::LowerSliceUpdate(
    const mir::Block& block, const mir::CallExpr& slice, mir::ExprId target,
    const ValueChange& change) -> diag::Result<lir::Operand> {
  const mir::ExprId receiver = *mir::CalleeReceiver(slice.callee);
  const mir::Type& receiver_ty =
      unit_->Mir().types.Get(block.exprs.Get(receiver).type);
  const lir::TypeId slice_type =
      unit_->TranslateType(block.exprs.Get(target).type);
  // A slice of what a write in progress designates is written within that
  // write, which hears whether an element moved. Nothing reads it first: an
  // assignment operator applies to an integral or real operand (LRM 11.4.1),
  // and a slice is neither.
  if (const auto* designated = receiver_ty.As<mir::DesignationType>()) {
    auto designation = LowerExpr(block, receiver);
    if (!designation) {
      return std::unexpected(std::move(designation.error()));
    }
    return WriteSlice(
        block, slice, change, *std::move(designation), slice_type,
        lir::OpenWriteTarget{
            .op = lir::OpenWriteTarget::Op::kAssignSlice,
            .value = unit_->TranslateType(designated->value)},
        [](const std::vector<lir::Operand>&) -> diag::Result<lir::Operand> {
          throw InternalError(
              "mir_to_lir: a slice holds no value an assignment operator "
              "applies to (LRM 11.4.1), so nothing reads one it writes -- "
              "please report this as a bug");
        });
  }
  auto place = LowerPlace(block, receiver, Reach::kInto);
  if (!place) {
    return std::unexpected(std::move(place.error()));
  }
  const lir::Operand container = Load(
      *std::move(place), unit_->TranslateType(block.exprs.Get(receiver).type));
  return WriteSlice(
      block, slice, change, container, slice_type,
      lir::BuiltinTarget{.fn = support::BuiltinFn::kSliceRef},
      [&](const std::vector<lir::Operand>& bounds)
          -> diag::Result<lir::Operand> {
        std::vector<lir::Operand> args{container};
        args.insert(args.end(), bounds.begin(), bounds.end());
        return EmitCallTo(
            lir::BuiltinTarget{.fn = support::BuiltinFn::kSlice},
            std::move(args), slice_type);
      });
}

auto FunctionLowerer::WriteSlice(
    const mir::Block& block, const mir::CallExpr& slice,
    const ValueChange& change, const lir::Operand& container,
    lir::TypeId slice_type, lir::CallTarget writer, const SliceReader& read)
    -> diag::Result<lir::Operand> {
  auto bounds = LowerEachExpr(block, slice.arguments);
  if (!bounds) {
    return std::unexpected(std::move(bounds.error()));
  }
  auto changed = change([&] { return read(*bounds); }, slice_type);
  if (!changed) {
    return std::unexpected(std::move(changed.error()));
  }
  std::vector<lir::Operand> args{container};
  args.insert(
      args.end(), std::make_move_iterator(bounds->begin()),
      std::make_move_iterator(bounds->end()));
  args.push_back(*std::move(changed));
  return EmitCallTo(
      std::move(writer), std::move(args),
      unit_->TranslateType(unit_->Mir().builtins.void_type));
}

auto FunctionLowerer::UpdateThroughView(
    const mir::Block& block, mir::ExprId target, const ValueChange& change)
    -> diag::Result<lir::Operand> {
  const auto& part = std::get<mir::CallExpr>(block.exprs.Get(target).data);
  const mir::ExprId whole = *mir::CalleeReceiver(part.callee);
  const lir::TypeId part_type =
      unit_->TranslateType(block.exprs.Get(target).type);

  // A view: the part is extracted from the whole and the whole rebuilt around
  // what it is changed to.
  if (StoragePartAccess(block, target) == nullptr) {
    return UpdateTarget(
        block, whole,
        [&](const ValueReader& read_whole,
            lir::TypeId whole_type) -> diag::Result<lir::Operand> {
          auto read = read_whole();
          if (!read) {
            return std::unexpected(std::move(read.error()));
          }
          const lir::Operand value = *std::move(read);
          auto selector = LowerValuePartSelector(block, part);
          if (!selector) {
            return std::unexpected(std::move(selector.error()));
          }
          auto changed = change(
              [&]() -> diag::Result<lir::Operand> {
                return Emit(
                    part_type, lir::AggregateExtractInstr{
                                   .aggregate = value, .selector = *selector});
              },
              part_type);
          if (!changed) {
            return std::unexpected(std::move(changed.error()));
          }
          return Emit(
              whole_type, lir::AggregateUpdateInstr{
                              .aggregate = value,
                              .selector = *std::move(selector),
                              .replacement = *std::move(changed)});
        });
  }

  // A part that is storage of its own, inside a value some view reached: the
  // value is held in a slot, where the part is written as it lies, and what
  // the slot then holds is the whole the view is rebuilt from.
  return UpdateTarget(
      block, whole,
      [&](const ValueReader& read_whole,
          lir::TypeId whole_type) -> diag::Result<lir::Operand> {
        auto read = read_whole();
        if (!read) {
          return std::unexpected(std::move(read.error()));
        }
        const lir::ValueId slot = HeldInSlot(*std::move(read), whole_type);
        auto step = PartStep(block, part);
        if (!step) {
          return std::unexpected(std::move(step.error()));
        }
        lir::Place place = LocalPlace(slot);
        place.chain.push_back(*std::move(step));
        auto changed = change(
            [&]() -> diag::Result<lir::Operand> {
              return Load(place, part_type);
            },
            part_type);
        if (!changed) {
          return std::unexpected(std::move(changed.error()));
        }
        Store(std::move(place), *std::move(changed));
        return Load(LocalPlace(slot), whole_type);
      });
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
  const auto apply = [&](lir::Operand object) -> diag::Result<lir::Operand> {
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
        lir::BuiltinTarget{.fn = fn}, std::move(args),
        unit_->TranslateType(type));
  };

  // A receiver that is storage is changed where it lies.
  if (!ReachesViewedPart(block, *receiver)) {
    auto place = LowerPlace(block, *receiver, Reach::kInto);
    if (!place) {
      return std::unexpected(std::move(place.error()));
    }
    return apply(Load(
        *std::move(place),
        unit_->TranslateType(block.exprs.Get(*receiver).type)));
  }
  // A receiver that is a view of its whole is read out, changed, and written
  // back as any write to a view is.
  std::optional<lir::Operand> result;
  auto updated = UpdateTarget(
      block, *receiver,
      [&](const ValueReader& read_old,
          lir::TypeId) -> diag::Result<lir::Operand> {
        auto read = read_old();
        if (!read) {
          return std::unexpected(std::move(read.error()));
        }
        lir::Operand part = *std::move(read);
        auto answered = apply(part);
        if (!answered) {
          return std::unexpected(std::move(answered.error()));
        }
        result = *std::move(answered);
        return part;
      });
  if (!updated) {
    return std::unexpected(std::move(updated.error()));
  }
  return *result;
}

auto FunctionLowerer::LowerIncDec(
    const mir::Block& block, const mir::IncDecExpr& inc_dec)
    -> diag::Result<lir::Operand> {
  const bool is_increment = inc_dec.op == mir::IncDecOp::kPreInc ||
                            inc_dec.op == mir::IncDecOp::kPostInc;
  const bool is_prefix = inc_dec.op == mir::IncDecOp::kPreInc ||
                         inc_dec.op == mir::IncDecOp::kPreDec;
  const lir::UnaryOp op =
      is_increment ? lir::UnaryOp::kIncrement : lir::UnaryOp::kDecrement;

  // Which of the two the statement's own value is (LRM 11.4.2) is settled after
  // the step runs, so both are kept as it does.
  std::optional<lir::Operand> old;
  std::optional<lir::Operand> stepped;
  auto written = UpdateTarget(
      block, inc_dec.target,
      [&](const ValueReader& read_old,
          lir::TypeId type) -> diag::Result<lir::Operand> {
        // The old value is what a postfix step answers with after the write,
        // so it is kept rather than read where the write lands.
        auto read = read_old();
        if (!read) {
          return std::unexpected(std::move(read.error()));
        }
        old = Kept(*std::move(read));
        stepped = Emit(type, lir::UnaryInstr{.op = op, .operand = *old});
        return *stepped;
      });
  if (!written) {
    return std::unexpected(std::move(written.error()));
  }
  return is_prefix ? *stepped : *old;
}

auto FunctionLowerer::LowerConditional(
    const mir::Block& block, const mir::ConditionalExpr& cond, mir::TypeId type)
    -> diag::Result<lir::Operand> {
  auto condition = LowerCondition(block, cond.condition);
  if (!condition) {
    return std::unexpected(std::move(condition.error()));
  }
  // The arms are evaluated only on the path that selects them, so the result is
  // written through on two paths: it is storage, not a transient. Each arm is
  // an extent of its own -- what it made ends before the paths rejoin -- and
  // the value it settles passes into the slot, which owns it from there to the
  // end of the full-expression the conditional stands in.
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

  const auto arm = [&](lir::BlockId id,
                       mir::ExprId value) -> diag::Result<void> {
    SetCurrent(id);
    const std::size_t depth = scopes_.size();
    auto settled = LowerExpr(block, value);
    if (!settled) {
      return std::unexpected(std::move(settled.error()));
    }
    Emit(
        unit_->TranslateType(unit_->Mir().builtins.void_type),
        lir::StoreInstr{
            .place = LocalPlace(slot), .value = HandOn(*std::move(settled))});
    auto closed = CloseExtent(depth);
    if (!closed) {
      return closed;
    }
    Terminate(lir::BranchTerm{.target = merge_id});
    return {};
  };
  if (auto then_arm = arm(then_id, cond.then_value); !then_arm) {
    return std::unexpected(std::move(then_arm.error()));
  }
  if (auto else_arm = arm(else_id, cond.else_value); !else_arm) {
    return std::unexpected(std::move(else_arm.error()));
  }

  SetCurrent(merge_id);
  if (unit_->Types().Get(result_type).IsOwnedValue()) {
    OpenScope(SlotEnd{.slot = slot});
  }
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
          [&](const mir::ReferenceExpr& reference)
              -> diag::Result<lir::Operand> {
            return ReferenceValue(block, id, reference.target, type);
          },
          [&](const mir::MachineIntLiteral& lit) -> diag::Result<lir::Operand> {
            return lir::Operand{lir::IntConst{
                .value =
                    lir::IntegralConstant{
                        .value_words = {static_cast<std::uint64_t>(lit.value)},
                        .state_words = {}},
                .type = unit_->TranslateType(type)}};
          },
          [&](const mir::CallExpr& call) -> diag::Result<lir::Operand> {
            // Reading a part that is storage of its own, or what a wrapper
            // holds, reads a place: what comes out is the value where it lies.
            if (StoragePartAccess(block, id) != nullptr ||
                ReadWrapper(expr).has_value()) {
              return ReadPlace(block, id, unit_->TranslateType(type));
            }
            return LowerCall(block, call, type);
          },
          [&](const mir::CastExpr& cast) -> diag::Result<lir::Operand> {
            auto operand = LowerExpr(block, cast.operand);
            if (!operand) {
              return std::unexpected(std::move(operand.error()));
            }
            // A handle read as another class's handle is a new handle to the
            // same object; every other cast reads its operand again.
            if (unit_->Mir().types.Get(type).Is<mir::ManagedRefType>()) {
              return Emit(
                  unit_->TranslateType(type),
                  lir::HandleCastInstr{.operand = *std::move(operand)});
            }
            return Emit(
                unit_->TranslateType(type),
                lir::CastInstr{.operand = *std::move(operand)});
          },
          [&](const mir::DynamicCastExpr& cast) -> diag::Result<lir::Operand> {
            auto operand = LowerExpr(block, cast.operand);
            if (!operand) {
              return std::unexpected(std::move(operand.error()));
            }
            return Emit(
                unit_->TranslateType(type),
                lir::DynamicCastInstr{.operand = *std::move(operand)});
          },
          [&](const mir::CompositeExpr& composite)
              -> diag::Result<lir::Operand> {
            auto parts = LowerEachExpr(block, composite.parts);
            if (!parts) {
              return std::unexpected(std::move(parts.error()));
            }
            const lir::TypeId built = unit_->TranslateType(type);
            return Emit(
                built,
                AssembledFrom(unit_->Types().Get(built), *std::move(parts)));
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
            const lir::Operand value = Emit(
                unit_->ClosureValueType(cl.closure),
                lir::ClosureInstr{.captures = std::move(captures)});
            if (!unit_->Mir().types.Get(type).Is<mir::CoroutineType>()) {
              return value;
            }
            // A closure whose invoke completes as a coroutine is entered
            // through that protocol, so what the expression yields is the
            // coroutine rather than the callable value. The captures stay the
            // environment it reads, and entering takes them, because they
            // outlive nothing on their own and the body runs after the one
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
          [&](const mir::FieldAccessExpr&) -> diag::Result<lir::Operand> {
            return ReadPlace(block, id, unit_->TranslateType(type));
          },
          [&](const mir::DerefExpr&) -> diag::Result<lir::Operand> {
            auto place = LowerPlace(block, id, Reach::kRead);
            if (!place) {
              return std::unexpected(std::move(place.error()));
            }
            return Load(*std::move(place), unit_->TranslateType(type));
          },
          [&](const mir::AddressOfExpr& addr) -> diag::Result<lir::Operand> {
            // A reference carries which form of storage it names rather than
            // stating it (LRM 13.5.2), and the two forms are different storage
            // with one type between them -- so an address taken through one
            // would name whichever form the type does not admit. Reading and
            // writing through a reference are its own operations, and so is
            // what a wait on it registers on.
            if (const auto* opened = std::get_if<mir::DerefExpr>(
                    &block.exprs.Get(addr.operand).data);
                opened != nullptr &&
                unit_->Mir()
                    .types.Get(block.exprs.Get(opened->pointer).type)
                    .Is<mir::RefType>()) {
              throw InternalError(
                  "mir_to_lir: an address was taken through a reference, where "
                  "the reference's own operations reach its storage");
            }
            auto place = LowerPlace(block, addr.operand, Reach::kWhole);
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
            return Emit(
                result_type,
                lir::UnaryInstr{.op = op, .operand = *std::move(operand)});
          },
          [&](const mir::BinaryExpr& bin) -> diag::Result<lir::Operand> {
            const lir::BinaryOp op = TranslateBinaryOp(bin.op);
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
                    .op = op, .lhs = *std::move(lhs), .rhs = *std::move(rhs)});
          },
          [&](const mir::ConditionalExpr& cond) -> diag::Result<lir::Operand> {
            return LowerConditional(block, cond, type);
          },
          [&](const mir::BlockExpr& be) -> diag::Result<lir::Operand> {
            // The steps run where they were written, so they lower into the
            // block being built, and the value the last one names is what the
            // expression yields. That value may be what a step declared, so
            // what the steps bind ends with the enclosing full-expression.
            const mir::Block& scope = block.child_scopes.Get(be.scope);
            auto lowered = LowerStatementsInto(scope);
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
            return LowerCoroutineAwait(block, await, type);
          },
          [&](const mir::WaitExpr& wait) -> diag::Result<lir::Operand> {
            // The registration has already arranged this execution's
            // resumption and answered whether it must park; where it must, a
            // control edge hands control back to the scheduler, which resumes
            // at the next block. A delay, an event control and a join differ
            // only in the call that precedes it. That call is lowered like any
            // other -- what it answers is the machine boolean MIR gave it, and
            // the suspend edge is what this adds around it.
            auto park = LowerExpr(block, wait.registration);
            if (!park) {
              return std::unexpected(std::move(park.error()));
            }
            const lir::BlockId parked = NewBlock();
            const lir::BlockId resume = NewBlock();
            Terminate(
                lir::CondBranchTerm{
                    .condition = *park, .if_true = parked, .if_false = resume});
            SetCurrent(parked);
            auto suspended = SuspendResumingAt(resume);
            if (!suspended) {
              return std::unexpected(std::move(suspended.error()));
            }
            // Resuming is a point where this execution regains control, so a
            // target it is inside may have been disabled while it was away.
            auto checked = TakeDepartureIfDue();
            if (!checked) {
              return std::unexpected(std::move(checked.error()));
            }
            // A wait yields nothing, so what stands here is never read.
            return *park;
          },
      },
      expr.data);
}

}  // namespace lyra::lowering::mir_to_lir
