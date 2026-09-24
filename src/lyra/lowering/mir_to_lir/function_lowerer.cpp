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
#include "lyra/lir/function.hpp"
#include "lyra/lir/integral_constant.hpp"
#include "lyra/lir/operator.hpp"
#include "lyra/lir/symbol_name.hpp"
#include "lyra/lir/type_builders.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/lowering/mir_to_lir/unit_lowerer.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/external_class.hpp"
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

// How a call to a callee ends. The design's own code can depart, wherever it
// stands and whatever artifact holds it, because a `disable` anywhere inside it
// reaches every execution it encloses (LRM 9.6.2). A runtime entry answers for
// itself, since only the entry knows whether it runs the design's code or
// raises. Foreign code is the one callee a departure never comes out of: it
// stops at that frame and crosses as a value instead.
auto EndingOf(const lir::CallTarget& target) -> support::CallEnding {
  using support::CallEnding;
  return std::visit(
      Overloaded{
          [](const lir::FunctionTarget&) {
            return CallEnding::kReturnsOrDeparts;
          },
          [](const lir::DispatchTarget&) {
            return CallEnding::kReturnsOrDeparts;
          },
          [](const lir::IndirectTarget&) {
            return CallEnding::kReturnsOrDeparts;
          },
          [](const lir::SymbolTarget&) {
            return CallEnding::kReturnsOrDeparts;
          },
          [](const lir::ControlEffectTarget& effect) {
            switch (effect.op) {
              case lir::ControlEffectTarget::Op::kTakeDepartureIfDue:
                return CallEnding::kReturnsOrDeparts;
              case lir::ControlEffectTarget::Op::kFinishDeparture:
                return CallEnding::kReturns;
              case lir::ControlEffectTarget::Op::kDeclineDeparture:
                return CallEnding::kDeparts;
            }
            throw InternalError("mir_to_lir: unknown control-effect operation");
          },
          [](const lir::BuiltinTarget& builtin) {
            return support::RuntimeEntryOf(builtin.fn).ending;
          },
          // Building a coroutine's frame places its arguments and stops before
          // its first statement, so none of the body has run when it returns.
          [](const lir::CoroutineTarget&) { return CallEnding::kReturns; },
          [](const lir::ConstructTarget&) { return CallEnding::kReturns; },
          [](const lir::ForeignTarget&) { return CallEnding::kReturns; },
          [](const lir::ValueCellTarget&) { return CallEnding::kReturns; },
          [](const lir::OpenVariablesTarget&) { return CallEnding::kReturns; },
          [](const lir::VariableAddressTarget&) {
            return CallEnding::kReturns;
          },
          [](const lir::CloseVariablesTarget&) {
            return CallEnding::kReturns;
          }},
      target);
}

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
    return lir::ProductInstr{.components = std::move(parts)};
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

// The class a managed reference refers to an object of, named the way that
// class is named. Every other pointee is a producer that built a handle to
// something no class declares.
auto ObjectClassOf(const mir::TypePool& types, mir::TypeId handle)
    -> mir::ClassRef {
  const mir::TypeId object =
      types.Get(handle).Get<mir::ManagedRefType>().pointee;
  const auto refers_to_no_class = [](std::string_view what) -> mir::ClassRef {
    throw InternalError(
        std::format(
            "mir_to_lir: a managed reference over {} names no class -- please "
            "report this as a bug",
            what));
  };
  const auto not_an_object = [&]() -> mir::ClassRef {
    return refers_to_no_class("something that is not an object");
  };
  return types.Get(object).Visit(
      Overloaded{
          [](const mir::ObjectType& o) -> mir::ClassRef {
            return mir::IntraUnitClassRef{.class_id = o.class_id};
          },
          [](const mir::CrossUnitClassType& c) -> mir::ClassRef {
            return mir::CrossUnitClassRef{
                .unit_name = c.unit_name, .class_name = c.class_name};
          },
          // An object this unit carries no class identity for: one reached
          // past another unit's signature, one the runtime library defines,
          // and one another unit's design element declares. A handle to any of
          // them is built by a producer that had no class to name.
          [&](const mir::OpaqueObjectType&) {
            return refers_to_no_class("an object with no class to name");
          },
          [&](const mir::RuntimeClassType&) {
            return refers_to_no_class("an object of a runtime class");
          },
          [&](const mir::ExternalUnitObjectType&) {
            return refers_to_no_class("another unit's object");
          },

          // Not an object at all. A managed reference points at one, so a
          // pointee of any other type means the handle was built over
          // something no object model covers.
          [&](const mir::PackedArrayType&) { return not_an_object(); },
          [&](const mir::EnumType&) { return not_an_object(); },
          [&](const mir::PackedStructType&) { return not_an_object(); },
          [&](const mir::PackedUnionType&) { return not_an_object(); },
          [&](const mir::UnpackedArrayType&) { return not_an_object(); },
          [&](const mir::DynamicArrayType&) { return not_an_object(); },
          [&](const mir::QueueType&) { return not_an_object(); },
          [&](const mir::AssociativeArrayType&) { return not_an_object(); },
          [&](const mir::WildcardIndexType&) { return not_an_object(); },
          [&](const mir::StringType&) { return not_an_object(); },
          [&](const mir::MachineCStringType&) { return not_an_object(); },
          [&](const mir::MachineBoolType&) { return not_an_object(); },
          [&](const mir::MachineIntType&) { return not_an_object(); },
          [&](const mir::MachineFloatType&) { return not_an_object(); },
          [&](const mir::MachineArrayType&) { return not_an_object(); },
          [&](const mir::MachineFunctionType&) { return not_an_object(); },
          [&](const mir::EventType&) { return not_an_object(); },
          [&](const mir::RealType&) { return not_an_object(); },
          [&](const mir::ShortRealType&) { return not_an_object(); },
          [&](const mir::RealTimeType&) { return not_an_object(); },
          [&](const mir::ChandleType&) { return not_an_object(); },
          [&](const mir::VoidType&) { return not_an_object(); },
          [&](const mir::EmptyType&) { return not_an_object(); },
          [&](const mir::RuntimeEffectsType&) { return not_an_object(); },
          [&](const mir::FilesType&) { return not_an_object(); },
          [&](const mir::DiagnosticType&) { return not_an_object(); },
          [&](const mir::RuntimeLibraryType&) { return not_an_object(); },
          [&](const mir::CoroutineType&) { return not_an_object(); },
          [&](const mir::RefType&) { return not_an_object(); },
          [&](const mir::PointerType&) { return not_an_object(); },
          [&](const mir::ManagedRefType&) { return not_an_object(); },
          [&](const mir::VectorType&) { return not_an_object(); },
          [&](const mir::TupleType&) { return not_an_object(); },
          [&](const mir::UnpackedStructType&) { return not_an_object(); },
          [&](const mir::UnionType&) { return not_an_object(); },
          [&](const mir::TaggedUnionType&) { return not_an_object(); },
          [&](const mir::ObservableType&) { return not_an_object(); },
          [&](const mir::ResolvedType&) { return not_an_object(); },
          [&](const mir::DriverType&) { return not_an_object(); },
          [&](const mir::SampledHistoryType&) { return not_an_object(); },
          [&](const mir::EvaluationAttemptsType&) { return not_an_object(); },
          [&](const mir::StructType&) { return not_an_object(); },
          [&](const mir::ClosureType&) { return not_an_object(); }});
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
          // method named outright.
          [&](const mir::Virtual& v) -> diag::Result<lir::CallTarget> {
            // A behavior an interface class states belongs to no lineage: a
            // class commits to several interfaces whose declarations are
            // unrelated to each other and to its base, and two classes
            // committing to one need not order them alike (LRM 8.26). So it
            // has no position counted through a lineage, which is the only
            // coordinate this path carries -- and that holds wherever the
            // class was declared.
            const bool through_an_interface = std::visit(
                Overloaded{
                    [&](const mir::LocalVirtualSlot& slot) {
                      return unit_->Mir()
                          .GetClass(slot.owner_class)
                          .is_interface_class;
                    },
                    [&](const mir::ExternalVirtualSlot& slot) {
                      return unit_
                          ->PromisedClass(slot.unit_name, slot.class_name)
                          .is_interface_class;
                    }},
                v.slot);
            if (through_an_interface) {
              return Unsupported(
                  "mir_to_lir: dispatching on a behavior an interface class "
                  "states is not yet supported");
            }
            auto method = std::visit(
                Overloaded{
                    [&](const mir::LocalVirtualSlot& slot)
                        -> diag::Result<lir::StatedDispatchRef> {
                      return unit_->MethodRef(slot.owner_class, slot.slot);
                    },
                    [&](const mir::ExternalVirtualSlot& slot)
                        -> diag::Result<lir::StatedDispatchRef> {
                      return lir::StatedDispatchRef{
                          .introduced_by = unit_->ExternalClassValueType(
                              slot.unit_name, slot.class_name),
                          .ordinal = lir::DispatchOrdinal{slot.ordinal.value}};
                    }},
                v.slot);
            if (!method) {
              return std::unexpected(std::move(method.error()));
            }
            return lir::CallTarget{
                lir::DispatchTarget{.method = *std::move(method)}};
          }},
      callee);
}

FunctionLowerer::FunctionLowerer(
    UnitLowerer& unit, const mir::CallableCode& code, std::string name)
    : unit_(&unit),
      code_(&code),
      constructed_class_(nullptr),
      closure_(nullptr),
      build_(nullptr),
      name_(std::move(name)),
      variable_slot_(code.locals.size(), std::nullopt),
      locals_(code.locals.size(), std::nullopt) {
}

FunctionLowerer::FunctionLowerer(
    UnitLowerer& unit, const mir::Class& cls, std::string name)
    : unit_(&unit),
      code_(&cls.constructor.code),
      constructed_class_(&cls),
      closure_(nullptr),
      build_(nullptr),
      name_(std::move(name)),
      variable_slot_(cls.constructor.code.locals.size(), std::nullopt),
      locals_(cls.constructor.code.locals.size(), std::nullopt) {
}

FunctionLowerer::FunctionLowerer(
    UnitLowerer& unit, const mir::ClosureDecl& closure, std::string name)
    : unit_(&unit),
      code_(&closure.invoke),
      constructed_class_(nullptr),
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
      constructed_class_(nullptr),
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
  // The type is the built expression's own, so a description and a constant
  // reach this the same way and neither is named here.
  fn_.result_type =
      unit_->TranslateType(build_->body.exprs.Get(build_->value).type);
  SetCurrent(NewBlock());
  auto value = LowerExpr(build_->body, build_->value);
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

  // The body's variables, in declaration order. A declaration is what gives a
  // variable storage, so nothing about what the body does with one is read
  // here -- not whether anything writes it, not whether anything binds a
  // second name to it. What the type answers is whether the generated side
  // holds the whole of the value: where it does not, what it holds is a handle
  // into storage the boundary releases and the variable outlives that, so the
  // variable gets storage of its own; where it does, it stays a value of the
  // body.
  for (const mir::LocalId local : code_->locals.Ids()) {
    const mir::TypeId declared = code_->locals.Get(local).type;
    if (!unit_->Mir().types.Get(declared).IsRuntimeStoredValue()) {
      continue;
    }
    variable_slot_[local.value] =
        static_cast<std::uint32_t>(fn_.variables.size());
    fn_.variables.push_back(
        lir::CellOf(unit_->Types(), unit_->TranslateType(declared)));
  }

  // A parameter is a declared local whose initial value is the incoming
  // argument. It arrives as a value in the signature and is bound like any
  // local: a place if the body assigns or addresses it, otherwise the argument
  // value itself. The entry block exists first so a spilled parameter's copy
  // into its place lands there, ahead of the body.
  SetCurrent(NewBlock());
  OpenVariables();
  // A closure invoke's receiver names the storage its captures live in, and
  // leads the per-invocation parameters in the signature.
  if (closure_ != nullptr) {
    BindCaptureReceiver(mir::LocalId{0});
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
    // argument, so it binds exactly as any declaration does.
    BindLocal(param, type, lir::Use{.value = value});
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

  auto based = ConstructBase();
  if (!based) {
    return std::unexpected(std::move(based.error()));
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

auto FunctionLowerer::ConstructorOf(const mir::ClassRef& cls)
    -> std::optional<EnteredConstructor> {
  return std::visit(
      Overloaded{
          [&](const mir::IntraUnitClassRef& intra)
              -> std::optional<EnteredConstructor> {
            return EnteredConstructor{
                .object_type = unit_->ClassValueType(intra.class_id),
                .callee = lir::CallTarget{lir::FunctionTarget{
                    .function = unit_->ConstructorFunction(intra.class_id)}}};
          },
          [&](const mir::CrossUnitClassRef& ext)
              -> std::optional<EnteredConstructor> {
            return EnteredConstructor{
                .object_type = unit_->ExternalClassValueType(
                    ext.unit_name, ext.class_name),
                .callee = lir::CallTarget{lir::SymbolTarget{
                    .symbol = lir::ConstructorSymbol(
                        ext.unit_name,
                        lir::SymbolPart::Name(ext.class_name))}}};
          },
          [](const mir::RuntimeClassRef&) -> std::optional<EnteredConstructor> {
            return std::nullopt;
          }},
      cls);
}

auto FunctionLowerer::ObjectTypeOf(const mir::ClassRef& cls)
    -> diag::Result<lir::TypeId> {
  return std::visit(
      Overloaded{
          [&](const mir::IntraUnitClassRef& intra)
              -> diag::Result<lir::TypeId> {
            return unit_->ClassValueType(intra.class_id);
          },
          [&](const mir::CrossUnitClassRef& ext) -> diag::Result<lir::TypeId> {
            return unit_->ExternalClassValueType(ext.unit_name, ext.class_name);
          },
          // A class of the runtime library is laid out by the library rather
          // than by a unit, so it carries no record a unit could name.
          [](const mir::RuntimeClassRef&) -> diag::Result<lir::TypeId> {
            return Unsupported(
                "mir_to_lir: a class the runtime library defines states no "
                "record of its own");
          }},
      cls);
}

auto FunctionLowerer::ConstructBase() -> diag::Result<void> {
  if (constructed_class_ == nullptr || !constructed_class_->base.has_value()) {
    return {};
  }
  const std::optional<EnteredConstructor> base =
      ConstructorOf(*constructed_class_->base);
  if (!base.has_value()) {
    return {};
  }
  // What the base construction carries was settled where the class was read:
  // the arguments are complete however the source arrived at them, so there is
  // nothing to establish about them here.
  const std::vector<mir::ExprId>& stated =
      constructed_class_->constructor.base_args;
  std::vector<lir::Operand> args;
  args.reserve(stated.size() + 1);
  // The base is entered on the object being constructed, which leads its
  // arguments the way a receiver leads any body's parameters.
  args.emplace_back(lir::Use{.value = fn_.params.front()});
  for (const mir::ExprId arg : stated) {
    auto lowered = LowerArgument(code_->Body(), arg);
    if (!lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
    args.push_back(*std::move(lowered));
  }
  auto entered = EmitCallTo(
      base->callee, std::move(args),
      unit_->TranslateType(unit_->Mir().builtins.void_type));
  if (!entered) {
    return std::unexpected(std::move(entered.error()));
  }
  return {};
}

auto FunctionLowerer::BuildLanding() -> diag::Result<lir::BlockId> {
  const lir::BlockId landing = NewBlock();
  const lir::BlockId resumed = current_;
  SetCurrent(landing);

  const lir::Operand effect =
      Emit(unit_->ControlEffectType(), lir::ReceiveDepartureInstr{});
  if (regions_.empty()) {
    // Nothing here claims one, so what this landing owes is what any other way
    // out of the body owes: the cleanups it stands in front of, and the end of
    // the storage the body's declared variables live in.
    auto cleaned = RunCleanupsDownTo(0);
    if (!cleaned) {
      return std::unexpected(std::move(cleaned.error()));
    }
    CloseVariables();
    EmitCallLeavingTheFrame(
        unit_->TranslateType(unit_->Mir().builtins.void_type),
        lir::CallInstr{
            .target =
                lir::ControlEffectTarget{
                    .op = lir::ControlEffectTarget::Op::kDeclineDeparture},
            .args = {}});
    Terminate(lir::UnreachableTerm{});
    SetCurrent(resumed);
    return landing;
  }

  const RegionTargets region = regions_.back();
  auto cleaned = RunCleanupsDownTo(region.cleanup_depth);
  if (!cleaned) {
    return std::unexpected(std::move(cleaned.error()));
  }
  Store(
      lir::Place{.base = lir::Use{.value = region.caught}, .chain = {}},
      effect);
  Terminate(lir::BranchTerm{.target = region.handler});
  SetCurrent(resumed);
  return landing;
}

auto FunctionLowerer::EmitDepartingCall(
    lir::CallTarget target, std::vector<lir::Operand> args,
    lir::TypeId result_type) -> diag::Result<lir::Operand> {
  auto landing = BuildLanding();
  if (!landing) {
    return std::unexpected(std::move(landing.error()));
  }
  const lir::BlockId returned = NewBlock();
  const lir::ValueId result = fn_.values.Add(
      lir::Local{
          .name = {}, .type = result_type, .kind = lir::LocalKind::kTemp});
  Terminate(
      lir::DepartingCallInstr{
          .result = result,
          .target = std::move(target),
          .args = std::move(args),
          .returned = returned,
          .landing = *landing});
  SetCurrent(returned);
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
      call != nullptr && MayDepart(EndingOf(call->target))) {
    throw InternalError(
        "FunctionLowerer::Emit: a callee that can leave without returning has "
        "to be stated as a departing call, so that whatever is owed between "
        "here and the frame's edge gets its turn; please report this as a bug");
  }
  return Append(type, std::move(data));
}

// A call whose departure, if it makes one, leaves this frame: every region is
// already behind it and everything owed between here and the edge has been
// emitted, so there is nothing left here to give a turn to.
auto FunctionLowerer::EmitCallLeavingTheFrame(
    lir::TypeId type, lir::CallInstr call) -> lir::Operand {
  return Append(type, std::move(call));
}

auto FunctionLowerer::Append(lir::TypeId type, lir::InstrData data)
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

// Introduces a declared local, holding its initial value. A local the body
// gave storage of its own was given it when the body opened, so reaching the
// declaration is what installs the representation -- which is what makes each
// entry to a declaration a fresh variable in the one storage. Every other
// local is a frame slot the declaration writes.
void FunctionLowerer::BindLocal(
    mir::LocalId local, lir::TypeId type, lir::Operand init) {
  if (variable_slot_[local.value].has_value()) {
    InitializeCell(
        std::get<CellBinding>(*locals_[local.value]).cell, std::move(init));
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
  // The storage crosses as the runtime object it is: the body opens it, hands
  // it back at each operation over it, and ends it, and never reads through it.
  const lir::TypeId opened = unit_->Types().Intern(
      lir::Type{lir::PointerType{
          .pointee = unit_->Types().Intern(lir::Type{lir::VoidType{}}),
          .ownership = lir::PointerOwnership::kBorrowed,
          .mutability = lir::Mutability::kMutable}});
  variables_ = Emit(
      opened, lir::CallInstr{.target = lir::OpenVariablesTarget{}, .args = {}});
  const lir::TypeId index_type = unit_->Types().Intern(
      lir::Type{lir::MachineIntType{
          .width = lir::MachineIntWidth::k32,
          .signedness = lir::Signedness::kUnsigned}});
  for (const mir::LocalId local : code_->locals.Ids()) {
    if (!variable_slot_[local.value].has_value()) {
      continue;
    }
    const lir::TypeId value =
        unit_->TranslateType(code_->locals.Get(local).type);
    const lir::TypeId address = unit_->Types().Intern(
        lir::Type{lir::PointerType{
            .pointee = lir::CellOf(unit_->Types(), value),
            .ownership = lir::PointerOwnership::kBorrowed,
            .mutability = lir::Mutability::kMutable}});
    const lir::IntConst position{
        .value =
            lir::IntegralConstant{
                .value_words = {static_cast<std::uint64_t>(
                    *variable_slot_[local.value])},
                .state_words = {}},
        .type = index_type};
    locals_[local.value] = LocalBinding{CellBinding{
        .cell = Emit(
            address, lir::CallInstr{
                         .target = lir::VariableAddressTarget{},
                         .args = {*variables_, position}})}};
  }
}

void FunctionLowerer::CloseVariables() {
  if (!variables_.has_value()) {
    return;
  }
  Emit(
      unit_->TranslateType(unit_->Mir().builtins.void_type),
      lir::CallInstr{
          .target = lir::CloseVariablesTarget{}, .args = {*variables_}});
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
          // A raise says the region holding this departure declines it, never
          // that a new effect starts here, so what carries on outward is the
          // one already held and the statement's operand is read by nothing.
          [&](const mir::RaiseStmt&) -> diag::Result<void> {
            return LeaveCarrying();
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
              lir::BuiltinTarget{.fn = support::BuiltinFn::kCurrentRuntime},
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

auto FunctionLowerer::LeaveCarrying() -> diag::Result<void> {
  // Declining puts the departure back on its way, and a region outside this
  // one is entitled to the same chance at it that this one just had, so the
  // decline is itself a point it can leave from.
  auto declined = EmitCallTo(
      lir::ControlEffectTarget{
          .op = lir::ControlEffectTarget::Op::kDeclineDeparture},
      {}, unit_->TranslateType(unit_->Mir().builtins.void_type));
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
          [&](const mir::StructFieldTarget& t) {
            return at(unit_->StructValueType(t.owner), t.slot);
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

// The value a call reaches its part out of, and nothing where the call reaches
// no part. A step of a descent is a call whose entry answers with the part
// rather than with the part's value, so both questions -- whether this is one,
// and what it descends through -- are answered by the entry's own declaration
// and the call's receiver. Reaching further would be asking where a write
// through it ultimately lands, a different question.
auto PartReceiver(const mir::CallExpr& call) -> std::optional<mir::ExprId> {
  const std::optional<support::BuiltinFn> fn = mir::DirectBuiltinFn(call);
  if (!fn.has_value() || !support::RuntimeEntryOf(*fn).answers_with_the_part) {
    return std::nullopt;
  }
  return mir::CalleeReceiver(call.callee);
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

// The wrapper a pointer opens, when the pointer is that opening rather than an
// ordinary one. Which storage a wrapper currently stands for is a fact about
// the wrapper, so asking for it is an operation on one: a target whose values
// carry interiors answers with a pointer into the storage, and one whose values
// do not answers with the chain step the wrapper's own storage already is.
auto OpenedWrapper(const mir::Block& block, mir::ExprId pointer)
    -> std::optional<mir::ExprId> {
  const auto* call = std::get_if<mir::CallExpr>(&block.exprs.Get(pointer).data);
  if (call == nullptr ||
      mir::DirectBuiltinFn(*call) != support::BuiltinFn::kOpenForWrite) {
    return std::nullopt;
  }
  return mir::CalleeReceiver(call->callee);
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
          [](const mir::FunctionRef&) -> diag::Result<lir::Operand> {
            return Unsupported(
                "mir_to_lir: a code address as a value is not yet lowerable to "
                "LIR");
          },
          [&](const mir::StaticVariableRef&) -> diag::Result<lir::Operand> {
            return ReadPlace(block, id, unit_->TranslateType(type));
          },
          [&](const mir::ExternalUnitVariableRef&)
              -> diag::Result<lir::Operand> {
            return ReadPlace(block, id, unit_->TranslateType(type));
          },
          [&](const mir::StaticConstantRef&) -> diag::Result<lir::Operand> {
            return ReadPlace(block, id, unit_->TranslateType(type));
          },
          [&](const mir::ObjectRecordRef&) -> diag::Result<lir::Operand> {
            return ReadPlace(block, id, unit_->TranslateType(type));
          },
          [&](const mir::StaticPropertyRef&) -> diag::Result<lir::Operand> {
            return ReadPlace(block, id, unit_->TranslateType(type));
          },
          [&](const mir::ExternalStaticPropertyRef&)
              -> diag::Result<lir::Operand> {
            return ReadPlace(block, id, unit_->TranslateType(type));
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
    const mir::ReferenceTarget& target, mir::TypeId type)
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
                    [&](const CellBinding& cell) -> diag::Result<lir::Place> {
                      return ValueAt(cell.cell);
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
          // A class's static constant is a compile-time record the backend
          // consumes directly rather than storage a body reaches, the same way
          // the unit-definition record types are.
          [](const mir::StaticConstantRef&) -> diag::Result<lir::Place> {
            throw InternalError(
                "mir_to_lir: a class's static constant is a compile-time "
                "record consumed by the backend directly and names no place -- "
                "please report this as a bug");
          },
          // A class's record is not storage anything writes, but it has an
          // address and a body reaches it to ask the class a question. So it
          // opens the way storage named by a linkage symbol does: the operand
          // says which class, and what the target calls that class's record is
          // the target's own to know.
          [&](const mir::ObjectRecordRef& r) -> diag::Result<lir::Place> {
            auto object = ObjectTypeOf(r.of);
            if (!object) {
              return std::unexpected(std::move(object.error()));
            }
            return lir::Place{
                .base =
                    lir::ObjectRecordRef{
                        .object = *object,
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
          [](const mir::FunctionRef&) -> diag::Result<lir::Place> {
            return Unsupported("mir_to_lir: a function names no place");
          }},
      target);
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
  const auto names_no_place =
      [](std::string_view form) -> diag::Result<lir::Place> {
    return Unsupported(std::format("mir_to_lir: {} names no place", form));
  };
  return std::visit(
      Overloaded{
          [&](const mir::ReferenceExpr& reference) -> diag::Result<lir::Place> {
            return ReferencePlace(reference.target, expr.type);
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
            // Opening a wrapper for writing names the storage it stands for,
            // which a bare dereference of the wrapper names too, so the open
            // adds no step and the two forms reach one place.
            if (const std::optional<mir::ExprId> wrapper =
                    OpenedWrapper(block, deref.pointer)) {
              return WrapperContentsPlace(block, *wrapper);
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
          [&](const mir::MachineArrayDataExpr&) {
            return names_no_place("a run of machine data");
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
  const lir::TypeId handle_type = unit_->TranslateType(type);
  const lir::Operand handle = Emit(
      handle_type,
      lir::CallInstr{.target = lir::ConstructTarget{}, .args = {}});

  // A construction carries every argument its constructor takes -- the source
  // wrote the call, so the front end bound it against the declaration and
  // filled in whatever it left to a default. So there is nothing to establish
  // about the arguments here, and nothing to read about the class beyond how
  // its constructor is named.
  const std::optional<EnteredConstructor> constructor =
      ConstructorOf(ObjectClassOf(unit_->Mir().types, type));
  if (!constructor.has_value()) {
    throw InternalError(
        "mir_to_lir: a construction reached a class the runtime library "
        "defines, which no `new` expression names");
  }
  std::vector<lir::Operand> args;
  args.reserve(call.arguments.size() + 1);
  // The object leads its arguments the way a receiver leads any body's
  // parameters, and the handle names it rather than being it, so opening the
  // handle is what reaches the storage the body runs on.
  args.push_back(Emit(
      unit_->Types().Intern(
          lir::Type{lir::PointerType{
              .pointee = constructor->object_type,
              .ownership = lir::PointerOwnership::kBorrowed,
              .mutability = lir::Mutability::kMutable}}),
      lir::AddrOfInstr{
          .place = lir::Place{
              .base = handle,
              .chain = {lir::Projection{lir::DerefProjection{}}}}}));
  for (const mir::ExprId argument : call.arguments) {
    auto lowered = LowerArgument(block, argument);
    if (!lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
    args.push_back(*std::move(lowered));
  }
  auto entered = EmitCallTo(
      constructor->callee, std::move(args),
      unit_->TranslateType(unit_->Mir().builtins.void_type));
  if (!entered) {
    return std::unexpected(std::move(entered.error()));
  }
  return handle;
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
  return LowerPlace(block, referent);
}

auto FunctionLowerer::LowerCall(
    const mir::Block& block, const mir::CallExpr& call, mir::TypeId type)
    -> diag::Result<lir::Operand> {
  // A method that changes the object it is applied to answers with the changed
  // object: the generated side holds a value as a handle a copy may alias, so
  // there is nothing to change in place.
  if (const auto fn = mir::DirectBuiltinFn(call);
      fn.has_value() && support::RuntimeEntryOf(*fn).mutates_receiver) {
    return LowerMutatingCall(block, call, *fn, type);
  }

  // A call that answers with the part has nothing to reach here -- the value
  // holds no interior -- so in a value position it is the extraction, and in a
  // target position it is the rebuild the write does.
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
  switch (EndingOf(target)) {
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
  // awaited execution at a time.
  Emit(
      unit_->TranslateType(unit_->Mir().builtins.void_type),
      lir::CallInstr{
          .target =
              lir::CoroutineTarget{.op = lir::CoroutineTarget::Op::kRelease},
          .args = {CurrentRuntime()}});

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
        assigned =
            assign.compound_op.has_value()
                ? LowerCompoundOperator(
                      *assign.compound_op, read_old(), *std::move(rhs), type)
                : *std::move(rhs);
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
  // A target that reaches into a value aggregate -- a positional part, a
  // container element, or any composition of them -- is not a place here: the
  // aggregate crosses as an opaque handle a copy may alias, so what goes back
  // is the owner's whole value with the part changed.
  if (ReachesIntoValue(block, target)) {
    return LowerValuePartUpdate(block, target, change);
  }
  const lir::TypeId type = unit_->TranslateType(block.exprs.Get(target).type);
  auto place = LowerPlace(block, target);
  if (!place) {
    return std::unexpected(std::move(place.error()));
  }
  auto changed = change([&] { return Load(*place, type); }, type);
  if (!changed) {
    return std::unexpected(std::move(changed.error()));
  }
  return Store(*std::move(place), *std::move(changed));
}

auto FunctionLowerer::LowerValuePartSelector(
    const mir::Block& block, const mir::CallExpr& call)
    -> diag::Result<lir::AggregateSelector> {
  const auto& direct = std::get<mir::Direct>(call.callee);
  const support::BuiltinFn fn = std::get<support::BuiltinFn>(direct.target);
  if (fn == support::BuiltinFn::kPartRef) {
    if (!direct.position.has_value()) {
      throw InternalError(
          "mir_to_lir: an entry reaching a part by its position names that "
          "position, and this call names none -- please report this as a bug");
    }
    return lir::AggregateSelector{lir::Part{.index = *direct.position}};
  }
  std::vector<lir::Operand> operands;
  operands.reserve(call.arguments.size());
  for (const mir::ExprId argument : call.arguments) {
    auto operand = LowerExpr(block, argument);
    if (!operand) {
      return std::unexpected(std::move(operand.error()));
    }
    operands.push_back(*std::move(operand));
  }
  if (fn == support::BuiltinFn::kSliceRef) {
    return lir::AggregateSelector{
        lir::ContainerSlice{.operands = std::move(operands)}};
  }
  return lir::AggregateSelector{
      lir::ContainerElement{.operands = std::move(operands)}};
}

auto FunctionLowerer::LowerValuePartUpdate(
    const mir::Block& block, mir::ExprId target, const ValueChange& change)
    -> diag::Result<lir::Operand> {
  // The steps the write descends, outermost first, and the owner they bottom
  // out in. Composition is the receiver chain, so the walk is the path.
  std::vector<mir::ExprId> steps;
  mir::ExprId owner = target;
  while (const std::optional<mir::ExprId> receiver =
             ValuePartReceiver(block, owner)) {
    steps.push_back(owner);
    owner = *receiver;
  }
  std::ranges::reverse(steps);

  auto owner_value = LowerExpr(block, owner);
  if (!owner_value) {
    return std::unexpected(std::move(owner_value.error()));
  }
  std::vector<lir::AggregateSelector> selectors;
  selectors.reserve(steps.size());
  for (const mir::ExprId step : steps) {
    auto selector = LowerValuePartSelector(
        block, std::get<mir::CallExpr>(block.exprs.Get(step).data));
    if (!selector) {
      return std::unexpected(std::move(selector.error()));
    }
    selectors.push_back(*std::move(selector));
  }

  // The type of the whole value a level descends from: the owner's at the
  // outermost level, and the step above it at every deeper one. Both the
  // descent and the rebuild answer at it.
  const auto container_type = [&](std::size_t depth) -> lir::TypeId {
    return unit_->TranslateType(
        block.exprs.Get(depth == 0 ? owner : steps[depth - 1]).type);
  };

  // The whole value at each level, descending from the owner toward the part.
  std::vector<lir::Operand> containers;
  containers.reserve(steps.size());
  containers.push_back(*std::move(owner_value));
  for (std::size_t depth = 1; depth < steps.size(); ++depth) {
    containers.push_back(Emit(
        container_type(depth), lir::AggregateExtractInstr{
                                   .aggregate = containers[depth - 1],
                                   .selector = selectors[depth - 1]}));
  }

  const std::size_t leaf = steps.size() - 1;
  const lir::TypeId leaf_type =
      unit_->TranslateType(block.exprs.Get(target).type);
  auto leaf_value = change(
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

  // The whole value again, rebuilt outward from the part just changed, and put
  // back through the owner. The owner is where the descent bottomed out, so it
  // reaches into no value of its own and its update names its storage directly.
  lir::Operand rebuilt = *std::move(leaf_value);
  for (std::size_t depth = steps.size(); depth-- > 0;) {
    rebuilt = Emit(
        container_type(depth), lir::AggregateUpdateInstr{
                                   .aggregate = containers[depth],
                                   .selector = selectors[depth],
                                   .replacement = std::move(rebuilt)});
  }
  return UpdateTarget(
      block, owner,
      [&](const ValueReader&, lir::TypeId) -> diag::Result<lir::Operand> {
        return rebuilt;
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
  const bool yields_result = type != unit_->Mir().builtins.void_type;

  // Applying the entry to one value, answering with the value it updated. Where
  // the entry states a result of its own, that is projected out here and kept
  // for the call to answer with, because what the update stores is the value.
  std::optional<lir::Operand> result;
  const auto apply = [&](lir::Operand value,
                         lir::TypeId value_type) -> diag::Result<lir::Operand> {
    std::vector<lir::Operand> args;
    args.reserve(call.arguments.size() + 1);
    args.push_back(std::move(value));
    for (const mir::ExprId argument : call.arguments) {
      auto arg = LowerArgument(block, argument);
      if (!arg) {
        return std::unexpected(std::move(arg.error()));
      }
      args.push_back(*std::move(arg));
    }
    const lir::TypeId call_type =
        yields_result
            ? unit_->ProductOf({value_type, unit_->TranslateType(type)})
            : value_type;
    lir::Operand completion = Emit(
        call_type,
        lir::CallInstr{
            .target = lir::BuiltinTarget{.fn = fn}, .args = std::move(args)});
    if (!yields_result) {
      return completion;
    }
    result = Emit(
        unit_->TranslateType(type),
        lir::AggregateExtractInstr{
            .aggregate = completion,
            .selector = lir::Part{.index = kMutatingCallResult}});
    return Emit(
        value_type, lir::AggregateExtractInstr{
                        .aggregate = std::move(completion),
                        .selector = lir::Part{.index = kUpdatedReceiver}});
  };

  auto updated = UpdateTarget(
      block, *receiver,
      [&](const ValueReader& read_old, lir::TypeId value_type) {
        return apply(read_old(), value_type);
      });
  if (!updated) {
    return std::unexpected(std::move(updated.error()));
  }
  return yields_result ? *result : *updated;
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
        old = read_old();
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
            return LowerCall(block, call, type);
          },
          [&](const mir::CastExpr& cast) -> diag::Result<lir::Operand> {
            auto operand = LowerExpr(block, cast.operand);
            if (!operand) {
              return std::unexpected(std::move(operand.error()));
            }
            return Emit(
                unit_->TranslateType(type),
                lir::CastInstr{.operand = *std::move(operand)});
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
            const lir::TypeId closure_type =
                unit_->ClosureValueType(cl.closure);
            const lir::Operand value = Emit(
                closure_type, lir::CallInstr{
                                  .target = lir::ConstructTarget{},
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
          // A field in a storage arena is read through its place. A product's
          // component is not addressable, so it is extracted from the product's
          // value instead -- the same read either way, over a receiver that is
          // storage in one case and a value in the other.
          [&](const mir::FieldAccessExpr&) -> diag::Result<lir::Operand> {
            return ReadPlace(block, id, unit_->TranslateType(type));
          },
          [&](const mir::DerefExpr&) -> diag::Result<lir::Operand> {
            auto place = LowerPlace(block, id);
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
            // writing through a reference are its own operations and need no
            // address; reaching the storage itself, which is what registering
            // a wait on it takes, is not yet one of them.
            if (const auto* opened = std::get_if<mir::DerefExpr>(
                    &block.exprs.Get(addr.operand).data);
                opened != nullptr &&
                unit_->Mir()
                    .types.Get(block.exprs.Get(opened->pointer).type)
                    .Is<mir::RefType>()) {
              return Unsupported(
                  "mir_to_lir: reaching the storage a reference binds, rather "
                  "than reading or writing through it, is not yet an operation "
                  "on this backend");
            }
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
                    .op = op, .lhs = *std::move(lhs), .rhs = *std::move(rhs)});
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
            // What is being awaited says which of the two this is, and the two
            // are different operations rather than two readings of one.
            //
            // A registered wait has already arranged this execution's
            // resumption and answered whether it must park; where it must, a
            // control edge hands control back to the scheduler, which resumes
            // at the next block. A delay, an event control and a join differ
            // only in the call that precedes it.
            //
            // An execution registers nothing, because its completion is the
            // awaited body's to signal, so what this waits for is that body
            // reaching its end.
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
            if (!std::holds_alternative<mir::CallExpr>(awaitable.data)) {
              return Unsupported(
                  "mir_to_lir: an awaitable that is not a registration call is "
                  "not yet lowerable to LIR");
            }
            // The call is lowered like any other -- what it answers is the
            // machine boolean MIR gave it, and the suspend edge is what this
            // adds around it.
            auto park = LowerExpr(block, await.awaitable);
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
            // An await of nothing yields nothing, so what stands here is never
            // read.
            return *park;
          },
      },
      expr.data);
}

}  // namespace lyra::lowering::mir_to_lir
