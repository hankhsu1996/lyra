#include "lyra/backend/llvm/codegen_value_type.hpp"

#include <algorithm>
#include <array>
#include <bit>
#include <cstddef>
#include <cstdint>
#include <format>
#include <functional>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>

#include "lyra/backend/llvm/codegen_module.hpp"
#include "lyra/backend/llvm/constant_record.hpp"
#include "lyra/backend/llvm/integral_planes.hpp"
#include "lyra/backend/llvm/runtime_entry.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/record_id.hpp"
#include "lyra/lir/symbol_name.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/runtime_object.hpp"
#include "lyra/support/value_domain.hpp"
#include "lyra/support/value_operation.hpp"
#include "lyra/value/element_type.hpp"
#include "lyra/value/fixed_array_type.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_value_type.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/queue_type.hpp"
#include "lyra/value/tuple_type.hpp"

namespace lyra::backend::llvm_backend {

namespace {

using support::ValueDomain;

// The call on `op`, an entry standing between a type's own operations
// and the type the library asks them through, arranged as `abi`: `operands`
// crossing as it says, then `out` where the entry lays its answer out there.
// Such an entry is told only numbers of a type, so its arguments are always
// to be had.
auto CallStreamEntry(
    CodeGenModule& module, llvm::IRBuilderBase& b, RuntimeOp op,
    const FnAbi& abi, std::span<llvm::Value* const> operands,
    std::optional<llvm::Value*> out) -> llvm::Value* {
  auto args = module.BuildCallArgs(abi, operands);
  if (!args) {
    throw InternalError(
        "llvm codegen: an entry reading or writing a stream of bits is told "
        "of a type the library does not realize -- please report this as a "
        "bug");
  }
  if (out.has_value()) {
    args->push_back(*out);
  }
  return b.CreateCall(module.RuntimeFunction(RuntimeSymbol(op), abi), *args);
}

// The machine type a C++ parameter or answer of type `T` crosses as.
template <typename T>
constexpr auto MachineKindOf() -> MachineKind {
  if constexpr (std::is_void_v<T>) {
    return MachineKind::kVoid;
  } else if constexpr (std::is_pointer_v<T> || std::is_reference_v<T>) {
    return MachineKind::kAddress;
  } else if constexpr (std::is_same_v<T, bool>) {
    return MachineKind::kTruth;
  } else {
    static_assert(
        std::is_integral_v<T> || std::is_enum_v<T>,
        "a function of a type's table takes or answers a type no body of "
        "this module could");
    if constexpr (sizeof(T) == 1) {
      return MachineKind::kInt8;
    } else if constexpr (sizeof(T) == 4) {
      return MachineKind::kInt32;
    } else {
      static_assert(sizeof(T) == 8);
      return MachineKind::kInt64;
    }
  }
}

// The operations a container's algorithms ask of an element or of what a
// `with` clause answers -- an order, a truth, a reduction (LRM 7.8, 7.12, 12.4)
// -- none of which is asked of a structure, a union or a fixed-size array: none
// has a relational or arithmetic operator to order, test or fold by, and an
// associative array indexed by one is refused where it is declared.
struct NeverAsked {};

// What storage of the type holds before anything writes it.
struct BuiltAsItsDefault {};

// How an operation's body takes and answers its integral values. The type the
// library asks through answers an equality as its scalar and a case equality
// as whether it holds, writes a stream into an integral value it is handed
// and reads one out of one, and is handed a count's control bits as one too,
// while a type's own function takes and answers each as a value of
// the type its own code states.
struct TakenAsStated {};
struct AnswerScalar {};
struct AnswerWhetherItHolds {};
struct AnswerIntoStream {};
struct StreamOutOfValue {};
struct ControlOutOfValue {};
using IntegralCrossing = std::variant<
    TakenAsStated, AnswerScalar, AnswerWhetherItHolds, AnswerIntoStream,
    StreamOutOfValue, ControlOutOfValue>;

// An operation a type answers with a function of its own, and how that
// function takes and answers its integral values.
struct OwnOperationWitness {
  support::ValueOperation answers;
  IntegralCrossing crossing = TakenAsStated{};
};

using WitnessFilling = std::variant<
    LifecycleStep, OwnOperationWitness, NeverAsked, BuiltAsItsDefault>;

// One witness of a type's table: the storage's own lifecycle, the operations
// the language defines on the whole value (LRM 11.4.5, 20.6.2, 20.9, 6.24.3,
// 6.6.1, 28.12.1), which a record's declaration states as its methods and a
// unit states for an array, or one no such type answers. `offset` is where
// the table lays it out, and a body filling it takes and answers what its
// declaration does.
struct Witness {
  std::string_view name;
  std::size_t offset;
  WitnessSignature stands_in_for;
  WitnessFilling filled_by;
};

// What a function of the table takes after the type and what it answers,
// read off its declaration. One that does not throw is read the same way.
template <typename Function>
struct WitnessSignatureOf;
template <typename R, typename... Params>
struct WitnessSignatureOf<R (*)(const value::ValueType&, Params...)> {
  static auto Get() -> WitnessSignature {
    return {
        .returns = MachineKindOf<R>(), .takes = {MachineKindOf<Params>()...}};
  }
};
template <typename R, typename... Params>
struct WitnessSignatureOf<R (*)(const value::ValueType&, Params...) noexcept>
    : WitnessSignatureOf<R (*)(const value::ValueType&, Params...)> {};

// Where the table's declaration lays out its member `member`, read off one
// table laid out as it is.
template <typename Field>
auto OffsetInTable(Field value::ValueWitnessTable::* member) -> std::size_t {
  const value::ValueWitnessTable table{};
  return static_cast<std::size_t>(
      std::bit_cast<std::uintptr_t>(&(table.*member)) -
      std::bit_cast<std::uintptr_t>(&table));
}

// The witness the table's member `member` holds: where the declaration lays it
// out, and what a body filling it takes and answers.
template <typename Function>
auto WitnessOf(
    std::string_view name, Function value::ValueWitnessTable::* member,
    WitnessFilling filled_by) -> Witness {
  return {
      .name = name,
      .offset = OffsetInTable(member),
      .stands_in_for = WitnessSignatureOf<Function>::Get(),
      .filled_by = filled_by};
}

// Every witness of the table but its parts, which no body fills.
auto Witnesses() {
  using support::BuiltinFn;
  using value::ValueWitnessTable;
  return std::to_array<Witness>({
      WitnessOf("copy", &ValueWitnessTable::copy, LifecycleStep::kCopy),
      WitnessOf("move", &ValueWitnessTable::move, LifecycleStep::kMove),
      WitnessOf(
          "destroy", &ValueWitnessTable::destroy, LifecycleStep::kDestroy),
      WitnessOf("assign", &ValueWitnessTable::assign, LifecycleStep::kAssign),
      WitnessOf(
          "equal", &ValueWitnessTable::equal,
          OwnOperationWitness{
              .answers = support::ValueOperator::kEquality,
              .crossing = AnswerScalar{}}),
      WitnessOf(
          "case_equal", &ValueWitnessTable::case_equal,
          OwnOperationWitness{
              .answers = BuiltinFn::kCaseEqual,
              .crossing = AnswerWhetherItHolds{}}),
      WitnessOf(
          "bit_identical", &ValueWitnessTable::bit_identical,
          OwnOperationWitness{.answers = BuiltinFn::kBitIdentical}),
      WitnessOf(
          "has_unknown", &ValueWitnessTable::has_unknown,
          OwnOperationWitness{.answers = BuiltinFn::kHasUnknown}),
      WitnessOf(
          "build_default", &ValueWitnessTable::build_default,
          BuiltAsItsDefault{}),
      WitnessOf(
          "bitstream_width", &ValueWitnessTable::bitstream_width,
          OwnOperationWitness{.answers = BuiltinFn::kBitstreamWidth}),
      WitnessOf(
          "count_bits", &ValueWitnessTable::count_bits,
          OwnOperationWitness{
              .answers = BuiltinFn::kCountBits,
              .crossing = ControlOutOfValue{}}),
      WitnessOf(
          "write_to_stream", &ValueWitnessTable::write_to_stream,
          OwnOperationWitness{
              .answers = BuiltinFn::kToBitstream,
              .crossing = AnswerIntoStream{}}),
      WitnessOf(
          "read_from_stream", &ValueWitnessTable::read_from_stream,
          OwnOperationWitness{
              .answers = BuiltinFn::kFromBitstream,
              .crossing = StreamOutOfValue{}}),
      WitnessOf(
          "resolve_tri_state", &ValueWitnessTable::resolve_tri_state,
          OwnOperationWitness{.answers = BuiltinFn::kResolveTriState}),
      WitnessOf(
          "resolve_wired_and", &ValueWitnessTable::resolve_wired_and,
          OwnOperationWitness{.answers = BuiltinFn::kResolveWiredAnd}),
      WitnessOf(
          "resolve_wired_or", &ValueWitnessTable::resolve_wired_or,
          OwnOperationWitness{.answers = BuiltinFn::kResolveWiredOr}),
      WitnessOf(
          "dominating", &ValueWitnessTable::dominating,
          OwnOperationWitness{.answers = BuiltinFn::kDominating}),
      WitnessOf(
          "filled_like", &ValueWitnessTable::filled_like,
          OwnOperationWitness{.answers = BuiltinFn::kFilledLike}),
      WitnessOf("order_before", &ValueWitnessTable::order_before, NeverAsked{}),
      WitnessOf("is_true", &ValueWitnessTable::is_true, NeverAsked{}),
      WitnessOf("reduce", &ValueWitnessTable::reduce, NeverAsked{}),
  });
}

// The table is its witnesses, then its parts, one address each.
static_assert(
    sizeof(value::ValueWitnessTable) ==
    (std::tuple_size_v<decltype(Witnesses())> + 1) * sizeof(void*));

// At the start of `out`, the numbers every type states, where the library's
// declaration of a type lays them out: its table, the size and alignment of a
// value of it, and which record the type opens.
void PlaceValueType(
    ConstantRecord& out, llvm::Constant* witnesses,
    const support::ObjectLayout& storage, value::ValueTypeKind kind) {
  using value::ValueType;
  out.Place(ValueType::WitnessesStatedAt(), witnesses);
  out.PlaceInteger(ValueType::SizeStatedAt(), storage.layout.size);
  out.PlaceInteger(ValueType::AlignStatedAt(), storage.layout.align);
  out.PlaceInteger(ValueType::KindStatedAt(), std::to_underlying(kind));
}

auto StepName(LifecycleStep step) -> std::string_view {
  switch (step) {
    case LifecycleStep::kCopy:
      return "copy";
    case LifecycleStep::kMove:
      return "move";
    case LifecycleStep::kDestroy:
      return "destroy";
    case LifecycleStep::kAssign:
      return "assign";
  }
  throw InternalError("llvm codegen: unknown lifecycle step");
}

}  // namespace

CodeGenValueTypes::CodeGenValueTypes(
    CodeGenModule& module, CodeGenTypes& types,
    const lir::CompilationUnit& unit)
    : owner_(&module), types_(&types), unit_(&unit) {
}

auto CodeGenValueTypes::KeyOf(lir::TypeId type) -> const std::string& {
  if (const auto found = keys_.find(type); found != keys_.end()) {
    return found->second;
  }
  // A record is its declaration, which names it from any module; an array is
  // its count and element, and a tuple its components. The declaration is
  // spelled as a symbol's parts are, each stating its own extent, so no two
  // declarations share a key.
  if (const std::optional<lir::TypeDeclarationRef> declared =
          DeclarationOf(type)) {
    return keys_
        .emplace(
            type, std::format(
                      "record{{{}{}}}",
                      lir::SymbolPart::Name(declared->unit_name).encoded,
                      lir::SymbolPartOf(declared->path).encoded))
        .first->second;
  }

  const lir::Type& stated = unit_->types.Get(type);
  if (const auto* array = stated.As<lir::UnpackedArrayType>()) {
    return keys_
        .emplace(
            type,
            std::format(
                "[{}x{}]", array->size, ComponentKey(array->element_type)))
        .first->second;
  }
  const std::span<const lir::TypeId> parts =
      types_->LayoutOfProduct(type).components;
  std::string key = "{";
  for (std::size_t i = 0; i < parts.size(); ++i) {
    if (i != 0) {
      key += ",";
    }
    key += ComponentKey(parts[i]);
  }
  key += "}";
  return keys_.emplace(type, std::move(key)).first->second;
}

// What one component, member or element contributes to the key of the type
// holding it: everything about its type that makes two types holding it two
// types. An integral one's width, signedness and states are its type, and a
// type laid out by its type has a key of its own; a container's type is its
// domain and what it holds -- its element, an associative array's index, a
// queue's bound -- and any other value is an object of the library's, which is
// one type per domain.
auto CodeGenValueTypes::ComponentKey(lir::TypeId component) -> std::string {
  const lir::Type& stated = unit_->types.Get(component);
  constexpr const char* kNoDomain =
      "llvm codegen: a component is no value of a runtime domain";
  const std::optional<lir::Holding> held = stated.HeldAs();
  if (!held.has_value()) {
    throw InternalError(kNoDomain);
  }
  return std::visit(
      Overloaded{
          [&](const lir::IntegralBits&) -> std::string {
            const value::IntegralShape shape =
                types_->RequiredIntegralShapeOf(component);
            return std::format(
                "{}{}{}", shape.width,
                shape.signedness == value::Signedness::kSigned ? "s" : "u",
                shape.IsFourState() ? "4" : "2");
          },
          [&](ValueDomain domain) -> std::string {
            if (IsLaidOutByType(domain)) {
              return KeyOf(component);
            }
            const std::string_view named = support::ValueDomainName(domain);
            if (const auto* dynamic = stated.As<lir::DynamicArrayType>()) {
              return std::format(
                  "{}<{}>", named, ComponentKey(dynamic->element_type));
            }
            if (const auto* queue = stated.As<lir::QueueType>()) {
              return std::format(
                  "{}<{}:{}>", named, ComponentKey(queue->element_type),
                  queue->max_bound
                      .transform([](std::uint64_t bound) {
                        return std::to_string(bound);
                      })
                      .value_or("unbounded"));
            }
            if (const auto* associative =
                    stated.As<lir::AssociativeArrayType>()) {
              return std::format(
                  "{}<{},{}>", named, ComponentKey(associative->element_type),
                  ComponentKey(associative->key_type));
            }
            return std::string(named);
          },
          [&](support::LibraryObject) -> std::string {
            throw InternalError(kNoDomain);
          },
          [&](const lir::OwnedStorageAddress&) -> std::string {
            throw InternalError(kNoDomain);
          }},
      *held);
}

auto CodeGenValueTypes::Function(lir::TypeId type, LifecycleStep step)
    -> llvm::Function* {
  const std::string& key = KeyOf(type);
  if (const auto found = functions_.find({key, step});
      found != functions_.end()) {
    return found->second;
  }
  llvm::Module& module = owner_->Module();
  llvm::Type* ptr = types_->Ptr();
  llvm::Type* void_ty = llvm::Type::getVoidTy(module.getContext());
  // Ending a value takes it alone; every other step takes a second address,
  // the storage it builds in or the value it writes.
  llvm::FunctionType* signature = nullptr;
  switch (step) {
    case LifecycleStep::kDestroy:
      signature = llvm::FunctionType::get(void_ty, {ptr}, false);
      break;
    case LifecycleStep::kCopy:
    case LifecycleStep::kMove:
    case LifecycleStep::kAssign:
      signature = llvm::FunctionType::get(void_ty, {ptr, ptr}, false);
      break;
  }
  llvm::Function* fn = llvm::Function::Create(
      signature, llvm::GlobalValue::InternalLinkage,
      std::format("lyra.type{}.{}", key, StepName(step)), module);
  functions_.emplace(std::pair{key, step}, fn);
  Emit(type, step, fn);
  return fn;
}

auto CodeGenValueTypes::MachineTypeOf(MachineKind kind) const -> llvm::Type* {
  llvm::LLVMContext& ctx = owner_->Module().getContext();
  switch (kind) {
    case MachineKind::kVoid:
      return llvm::Type::getVoidTy(ctx);
    case MachineKind::kAddress:
      return types_->Ptr();
    case MachineKind::kTruth:
      return llvm::Type::getInt1Ty(ctx);
    case MachineKind::kInt8:
      return llvm::Type::getInt8Ty(ctx);
    case MachineKind::kInt32:
      return llvm::Type::getInt32Ty(ctx);
    case MachineKind::kInt64:
      return llvm::Type::getInt64Ty(ctx);
  }
  throw InternalError("llvm codegen: unknown machine kind");
}

auto CodeGenValueTypes::DeclareBody(
    lir::TypeId type, std::string_view witness,
    const WitnessSignature& stands_in_for) -> llvm::Function* {
  std::vector<llvm::Type*> params{types_->Ptr()};
  params.reserve(1 + stands_in_for.takes.size());
  for (const MachineKind taken : stands_in_for.takes) {
    params.push_back(MachineTypeOf(taken));
  }
  return llvm::Function::Create(
      llvm::FunctionType::get(
          MachineTypeOf(stands_in_for.returns), params, false),
      llvm::GlobalValue::InternalLinkage,
      std::format("lyra.type{}.type.{}", KeyOf(type), witness),
      owner_->Module());
}

auto CodeGenValueTypes::Body(
    lir::TypeId type, std::string_view witness,
    const WitnessSignature& stands_in_for, llvm::Function* filling)
    -> llvm::Constant* {
  llvm::Module& module = owner_->Module();
  if (filling == nullptr) {
    return llvm::cast<llvm::Constant>(
        owner_
            ->CreateRuntimeFunction(
                llvm::FunctionType::get(types_->Void(), false),
                kTypeLacksOperationSymbol)
            .getCallee());
  }
  // A body is entered with the type it belongs to ahead of the values it is
  // asked about, which it has no use for: everything the type is lies in the
  // function it hands them to.
  llvm::Function* body = DeclareBody(type, witness, stands_in_for);
  llvm::IRBuilder<> b(llvm::BasicBlock::Create(module.getContext(), "", body));
  std::vector<llvm::Value*> args;
  args.reserve(stands_in_for.takes.size());
  for (std::size_t i = 1; i < body->arg_size(); ++i) {
    args.push_back(body->getArg(static_cast<unsigned>(i)));
  }
  llvm::Value* answered = b.CreateCall(filling, args);
  // A function answering in storage it was handed answers with that storage
  // too, which the witness it fills does not.
  switch (stands_in_for.returns) {
    case MachineKind::kVoid:
      b.CreateRetVoid();
      break;
    case MachineKind::kAddress:
    case MachineKind::kTruth:
    case MachineKind::kInt8:
    case MachineKind::kInt32:
    case MachineKind::kInt64:
      b.CreateRet(answered);
      break;
  }
  return body;
}

// A thunk being built. The library asks a type for an operation
// through one signature for every type, and the function the design compiled
// for that operation takes or answers one integral value at its own type. The
// thunk stands between the two, and this is what building one needs.
struct CodeGenValueTypes::ThunkBody {
  // The thunk itself.
  llvm::Function* function;
  // The thunk's arguments after the first, which is the type.
  std::vector<llvm::Value*> args;
  // Storage for the one integral value that crosses at the design's type.
  llvm::Value* storage;
  // That value's type, and the same as the value library is told it.
  lir::TypeId value_type;
  value::IntegralShape shape;
};

auto CodeGenValueTypes::OpenThunkBody(
    lir::TypeId type, std::string_view witness,
    const WitnessSignature& stands_in_for, lir::TypeId stated) -> ThunkBody {
  const std::optional<value::IntegralShape> shape =
      types_->IntegralShapeOf(stated);
  if (!shape.has_value()) {
    throw InternalError(
        "llvm codegen: a type's own operation takes or answers no integral "
        "value where its type's table states one -- please report this as a "
        "bug");
  }
  llvm::Function* body = DeclareBody(type, witness, stands_in_for);
  llvm::IRBuilder<> b(
      llvm::BasicBlock::Create(owner_->Module().getContext(), "", body));
  // The value is a comparison's one-bit answer or a count's control bits,
  // which a design does not make wide.
  const support::ObjectLayout layout = types_->LayoutOf(stated);
  llvm::AllocaInst* const storage =
      b.CreateAlloca(llvm::ArrayType::get(b.getInt8Ty(), layout.layout.size));
  storage->setAlignment(llvm::Align(layout.layout.align));
  std::vector<llvm::Value*> args;
  args.reserve(stands_in_for.takes.size());
  for (std::size_t i = 1; i < body->arg_size(); ++i) {
    args.push_back(body->getArg(static_cast<unsigned>(i)));
  }
  return ThunkBody{
      .function = body,
      .args = std::move(args),
      .storage = storage,
      .value_type = stated,
      .shape = *shape};
}

auto CodeGenValueTypes::ReadOutOfStream(
    llvm::IRBuilderBase& b, llvm::Value* stream, llvm::Value* taken,
    llvm::Value* laid_out, lir::TypeId read) -> llvm::Value* {
  const std::array<StatedOperand, 2> stated{
      MachineOperand{.type = types_->Ptr()},
      MachineOperand{.type = b.getInt64Ty()}};
  const std::array<llvm::Value*, 2> operands{stream, taken};
  return CallStreamEntry(
      *owner_, b, RuntimeOp::kStreamRead,
      FnAbiOf(
          *owner_, RuntimeOp::kStreamRead, stated, std::nullopt, read,
          ReturnIndirect{.returned = b.getInt64Ty()}),
      operands, laid_out);
}

// The type of the value a type's own function takes its parameter `index` as
// a reference to, which is how it takes every value it is asked about.
auto CodeGenValueTypes::ParamReferent(
    lir::FunctionId method, std::size_t index) const -> lir::TypeId {
  const lir::Function& fn = unit_->functions.Get(method);
  return unit_->types.Get(fn.values.Get(fn.params.at(index)).type)
      .Get<lir::PointerType>()
      .pointee;
}

// The function answers a comparison as a value of the one-bit type its own
// code states, laid out in storage it is handed last, and the body answers the
// scalar that value holds.
auto CodeGenValueTypes::ScalarAnswerBody(
    lir::TypeId type, std::string_view witness,
    const WitnessSignature& stands_in_for, lir::FunctionId method,
    ComparisonAnswered answered) -> llvm::Constant* {
  const ThunkBody thunk = OpenThunkBody(
      type, witness, stands_in_for, unit_->functions.Get(method).result_type);
  llvm::IRBuilder<> b(&thunk.function->getEntryBlock());
  std::vector<llvm::Value*> args = thunk.args;
  args.push_back(thunk.storage);
  b.CreateCall(owner_->UnitFunction(method), args);
  llvm::Value* const scalar =
      LoadComparisonAnswer(b, thunk.storage, thunk.shape);
  switch (answered) {
    case ComparisonAnswered::kAsItsScalar:
      b.CreateRet(scalar);
      break;
    case ComparisonAnswered::kAsWhetherItHolds:
      b.CreateRet(b.CreateICmpEQ(
          scalar, b.getInt8(std::to_underlying(value::FourStateBit::kOne))));
      break;
  }
  return thunk.function;
}

// A type's own function answers its stream as a value of the type its
// code states, and reads one as such a value, while the type is handed a
// stream to write into or read out of. The library stands between: it is
// handed the function, holds the value for it, and writes it into the stream
// or reads it out first, so the body here holds nothing.
auto CodeGenValueTypes::StreamWriteBody(
    lir::TypeId type, std::string_view witness,
    const WitnessSignature& stands_in_for, lir::FunctionId method)
    -> llvm::Constant* {
  llvm::Function* const body = DeclareBody(type, witness, stands_in_for);
  llvm::IRBuilder<> b(
      llvm::BasicBlock::Create(owner_->Module().getContext(), "", body));
  const value::IntegralShape stream =
      types_->RequiredIntegralShapeOf(unit_->functions.Get(method).result_type);
  b.CreateRet(ForwardToEntry(
      b, RuntimeOp::kStreamWrite, b.getInt64Ty(),
      {owner_->UnitFunction(method), body->getArg(1), b.getInt64(stream.width),
       b.getInt1(stream.IsFourState()), body->getArg(2), body->getArg(3)}));
  return body;
}

auto CodeGenValueTypes::StreamReadBody(
    lir::TypeId type, std::string_view witness,
    const WitnessSignature& stands_in_for, lir::FunctionId method)
    -> llvm::Constant* {
  llvm::Function* const body = DeclareBody(type, witness, stands_in_for);
  llvm::IRBuilder<> b(
      llvm::BasicBlock::Create(owner_->Module().getContext(), "", body));
  const value::IntegralShape stream =
      types_->RequiredIntegralShapeOf(ParamReferent(method, 0));
  b.CreateRet(ForwardToEntry(
      b, RuntimeOp::kStreamBuild, b.getInt64Ty(),
      {owner_->UnitFunction(method), body->getArg(1), body->getArg(2),
       b.getInt64(stream.width), b.getInt1(stream.IsFourState()),
       body->getArg(3)}));
  return body;
}

auto CodeGenValueTypes::ForwardToEntry(
    llvm::IRBuilderBase& b, RuntimeOp op, llvm::Type* answers,
    llvm::ArrayRef<llvm::Value*> args) -> llvm::Value* {
  std::vector<llvm::Type*> takes;
  takes.reserve(args.size());
  for (llvm::Value* const arg : args) {
    takes.push_back(arg->getType());
  }
  llvm::FunctionCallee entry = owner_->CreateRuntimeFunction(
      llvm::FunctionType::get(answers, takes, false), RuntimeSymbol(op));
  // The entry reads a flag as a C++ `bool`, a whole byte.
  auto* const declared = llvm::cast<llvm::Function>(entry.getCallee());
  for (unsigned i = 0; i < args.size(); ++i) {
    if (args[i]->getType()->isIntegerTy(1)) {
      declared->addParamAttr(i, llvm::Attribute::ZExt);
    }
  }
  return b.CreateCall(entry, args);
}

// The function takes a count's control bits as a value of the type its own
// code states, which the value the type was handed is read into first.
auto CodeGenValueTypes::ControlReadBody(
    lir::TypeId type, std::string_view witness,
    const WitnessSignature& stands_in_for, lir::FunctionId method)
    -> llvm::Constant* {
  const ThunkBody thunk =
      OpenThunkBody(type, witness, stands_in_for, ParamReferent(method, 1));
  llvm::IRBuilder<> b(&thunk.function->getEntryBlock());
  ReadOutOfStream(
      b, thunk.args[1], b.getInt64(0), thunk.storage, thunk.value_type);
  b.CreateCall(
      owner_->UnitFunction(method),
      {thunk.args[0], thunk.storage, thunk.args[2]});
  b.CreateRetVoid();
  return thunk.function;
}

void CodeGenValueTypes::CallBuildDefault(
    llvm::IRBuilderBase& b, lir::TypeId component, llvm::Value* value) {
  auto component_type = GetAddrOfValueType(component);
  if (!component_type) {
    throw InternalError(
        "llvm codegen: a tuple component is no value the runtime realizes");
  }
  llvm::Type* const ptr = types_->Ptr();
  llvm::Value* const table = b.CreateLoad(
      ptr, b.CreateConstInBoundsGEP1_64(
               b.getInt8Ty(), *component_type,
               value::ValueType::WitnessesStatedAt()));
  llvm::Value* const function = b.CreateLoad(
      ptr, b.CreateConstInBoundsGEP1_64(
               b.getInt8Ty(), table,
               OffsetInTable(&value::ValueWitnessTable::build_default)));
  b.CreateCall(
      llvm::FunctionType::get(b.getVoidTy(), {ptr, ptr}, false), function,
      {*component_type, value});
}

// Each component's type builds its own part, so the body asks each in turn.
auto CodeGenValueTypes::ComponentWiseDefaultBody(
    lir::TypeId tuple, std::string_view witness,
    const WitnessSignature& stands_in_for) -> llvm::Constant* {
  llvm::Function* const body = DeclareBody(tuple, witness, stands_in_for);
  llvm::IRBuilder<> b(
      llvm::BasicBlock::Create(owner_->Module().getContext(), "", body));
  const ProductLayout& layout = types_->LayoutOfProduct(tuple);
  for (std::size_t i = 0; i < layout.components.size(); ++i) {
    CallBuildDefault(
        b, layout.components[i],
        b.CreateConstInBoundsGEP1_64(
            b.getInt8Ty(), body->getArg(1), layout.offsets[i]));
  }
  b.CreateRetVoid();
  return body;
}

void CodeGenValueTypes::EmitClearAboveWidths(
    llvm::IRBuilderBase& b, lir::TypeId type, llvm::Value* at) {
  if (!types_->KeepsAboveWidthsClear(type)) {
    return;
  }
  if (const std::optional<value::IntegralShape> shape =
          types_->IntegralShapeOf(type)) {
    ClearAboveWidth(b, at, *shape);
    return;
  }
  b.CreateCall(ClearFunction(type), {at});
}

// A tagged union keeps clear what the member it holds keeps clear, an array
// what each element does, and a product what each component does. An untagged
// union keeps none of its own, since its bits are whatever its members' are,
// so no function is made for one.
auto CodeGenValueTypes::ClearFunction(lir::TypeId type) -> llvm::Function* {
  const std::string& key = KeyOf(type);
  if (const auto found = clear_functions_.find(key);
      found != clear_functions_.end()) {
    return found->second;
  }
  llvm::Module& module = owner_->Module();
  llvm::Function* const fn = llvm::Function::Create(
      llvm::FunctionType::get(
          llvm::Type::getVoidTy(module.getContext()), {types_->Ptr()}, false),
      llvm::GlobalValue::InternalLinkage,
      std::format("lyra.type{}.clear_above_widths", key), module);
  clear_functions_.emplace(key, fn);
  llvm::IRBuilder<> b(llvm::BasicBlock::Create(module.getContext(), "", fn));
  llvm::Value* const value = fn->getArg(0);
  std::visit(
      Overloaded{
          [&](const ProductLayout* product) {
            for (std::size_t i = 0; i < product->components.size(); ++i) {
              EmitClearAboveWidths(
                  b, product->components[i],
                  b.CreateConstInBoundsGEP1_64(
                      b.getInt8Ty(), value, product->offsets[i]));
            }
          },
          [&](const ArrayLayout* array) {
            ForEachElement(
                b, type, value,
                [&](llvm::IRBuilderBase& at, llvm::Value* each) {
                  EmitClearAboveWidths(at, array->element, each);
                });
          },
          [&](const UnionLayout* overlay) {
            if (overlay->tag.has_value()) {
              ForHeldMember(
                  b, overlay->members, *overlay->tag, value,
                  [&](llvm::IRBuilderBase& at, lir::TypeId member) {
                    EmitClearAboveWidths(at, member, value);
                  });
            }
          }},
      types_->FieldsOf(type));
  b.CreateRetVoid();
  return fn;
}

void CodeGenValueTypes::ForEachElement(
    llvm::IRBuilderBase& b, lir::TypeId array, llvm::Value* value,
    const std::function<void(llvm::IRBuilderBase&, llvm::Value*)>& each) {
  const ArrayLayout& laid = types_->LayoutOfArray(array);
  llvm::Function* const fn = b.GetInsertBlock()->getParent();
  llvm::LLVMContext& ctx = fn->getContext();
  llvm::BasicBlock* const before = b.GetInsertBlock();
  llvm::BasicBlock* const loop = llvm::BasicBlock::Create(ctx, "element", fn);
  llvm::BasicBlock* const done =
      llvm::BasicBlock::Create(ctx, "elements_done", fn);
  // An unpacked array declares at least one element (LRM 7.4.2).
  b.CreateBr(loop);
  b.SetInsertPoint(loop);
  llvm::PHINode* const position = b.CreatePHI(b.getInt64Ty(), 2);
  position->addIncoming(b.getInt64(0), before);
  each(
      b, b.CreateInBoundsGEP(
             b.getInt8Ty(), value,
             b.CreateMul(position, b.getInt64(laid.stride))));
  llvm::Value* const next = b.CreateAdd(position, b.getInt64(1));
  position->addIncoming(next, b.GetInsertBlock());
  b.CreateCondBr(b.CreateICmpULT(next, b.getInt64(laid.count)), loop, done);
  b.SetInsertPoint(done);
}

void CodeGenValueTypes::ForHeldMember(
    llvm::IRBuilderBase& b, std::span<const lir::TypeId> members,
    const value::TagPlacement& placed, llvm::Value* value,
    const std::function<void(llvm::IRBuilderBase&, lir::TypeId)>& held) {
  llvm::IntegerType* const tag_ty = b.getIntNTy(8 * placed.bytes);
  llvm::Value* const tag = b.CreateLoad(
      tag_ty,
      b.CreateConstInBoundsGEP1_64(b.getInt8Ty(), value, placed.offset));
  llvm::Function* const fn = b.GetInsertBlock()->getParent();
  llvm::LLVMContext& ctx = fn->getContext();
  // A tag naming no member is bits another member of an untagged union
  // holding this one wrote, and a union held so owns nothing (LRM 7.3).
  llvm::BasicBlock* const done = llvm::BasicBlock::Create(ctx, "held", fn);
  llvm::SwitchInst* const choose =
      b.CreateSwitch(tag, done, static_cast<unsigned>(members.size()));
  for (std::size_t i = 0; i < members.size(); ++i) {
    llvm::BasicBlock* const member =
        llvm::BasicBlock::Create(ctx, "holds", fn, done);
    choose->addCase(llvm::ConstantInt::get(tag_ty, i), member);
    b.SetInsertPoint(member);
    held(b, members[i]);
    b.CreateBr(done);
  }
  b.SetInsertPoint(done);
}

auto CodeGenValueTypes::OwnOperations(lir::TypeId type) const
    -> std::vector<OwnOperation> {
  std::vector<OwnOperation> operations;
  if (const lir::RecordRef* record = lir::RecordOf(unit_->types.Get(type))) {
    for (const lir::RecordMethod& method :
         unit_->records.Get(std::get<lir::RecordId>(*record)).methods) {
      operations.push_back(
          OwnOperation{.answers = method.answers, .function = method.function});
    }
    return operations;
  }
  for (const lir::TypeOperation& stated : unit_->type_operations) {
    if (stated.type == type) {
      operations.push_back(
          OwnOperation{.answers = stated.answers, .function = stated.function});
    }
  }
  return operations;
}

auto CodeGenValueTypes::GetAddrOfLaidOutType(lir::TypeId type)
    -> llvm::GlobalVariable* {
  const std::string& key = KeyOf(type);
  if (const auto found = types_of_.find(key); found != types_of_.end()) {
    return found->second;
  }
  llvm::Module& module = owner_->Module();
  llvm::LLVMContext& ctx = module.getContext();
  auto* ptr = types_->Ptr();
  const lir::Type& stated = unit_->types.Get(type);
  const auto* array = stated.As<lir::UnpackedArrayType>();
  const auto global = [&](std::string name, llvm::Type* of) {
    auto* made =
        llvm::cast<llvm::GlobalVariable>(module.getOrInsertGlobal(name, of));
    made->setLinkage(llvm::GlobalValue::InternalLinkage);
    made->setConstant(true);
    return made;
  };
  // Named before it is filled, so that a body emitted while filling it that
  // names this type again names the one being filled; the definition then
  // takes the name and every use, as clang replaces a global it declared
  // before it knew the type of its definition.
  llvm::GlobalVariable* constant =
      global(std::format("lyra.type{}", key), llvm::Type::getInt8Ty(ctx));
  types_of_.emplace(key, constant);

  // An array is its count and its element wherever it is named, so every
  // module naming it states the same constant under the same name and the
  // linker keeps one, as it keeps one of a template's static data member: one
  // array type is one object in the program.
  if (array != nullptr) {
    constant->setLinkage(llvm::GlobalValue::LinkOnceODRLinkage);
  }
  // A struct or a union has one type in the program, the declaring unit's,
  // linked as its declaration is, since that unit is the one stating its
  // methods; another unit refers to it and fills nothing.
  if (const lir::RecordRef* record = lir::RecordOf(stated)) {
    const auto* own = std::get_if<lir::RecordId>(record);
    if (own == nullptr) {
      constant->setLinkage(llvm::GlobalValue::ExternalLinkage);
      return constant;
    }
    constant->setLinkage(LinkageOf(unit_->records.Get(*own).linkage));
  }
  const std::vector<OwnOperation> operations = OwnOperations(type);

  // The function answering `operation` with the crossing `crossing` names.
  const auto answered_by =
      [&](const Witness& witness, support::ValueOperation operation,
          const IntegralCrossing& crossing) -> llvm::Constant* {
    const auto method = std::ranges::find_if(
        operations,
        [&](const OwnOperation& own) { return own.answers == operation; });
    if (method == operations.end()) {
      return Body(type, witness.name, witness.stands_in_for, nullptr);
    }
    return std::visit(
        Overloaded{
            [&](TakenAsStated) -> llvm::Constant* {
              return Body(
                  type, witness.name, witness.stands_in_for,
                  owner_->UnitFunction(method->function));
            },
            [&](AnswerScalar) -> llvm::Constant* {
              return ScalarAnswerBody(
                  type, witness.name, witness.stands_in_for, method->function,
                  ComparisonAnswered::kAsItsScalar);
            },
            [&](AnswerWhetherItHolds) -> llvm::Constant* {
              return ScalarAnswerBody(
                  type, witness.name, witness.stands_in_for, method->function,
                  ComparisonAnswered::kAsWhetherItHolds);
            },
            [&](AnswerIntoStream) -> llvm::Constant* {
              return StreamWriteBody(
                  type, witness.name, witness.stands_in_for, method->function);
            },
            [&](StreamOutOfValue) -> llvm::Constant* {
              return StreamReadBody(
                  type, witness.name, witness.stands_in_for, method->function);
            },
            [&](ControlOutOfValue) -> llvm::Constant* {
              return ControlReadBody(
                  type, witness.name, witness.stands_in_for, method->function);
            }},
        crossing);
  };

  // The table, each witness where the library's declaration of it lays it
  // out, and for an array the library's answer for its elements by position,
  // which reads them off the type it is handed.
  ConstantRecord witnesses(ctx, sizeof(value::ValueWitnessTable));
  for (const Witness& witness : Witnesses()) {
    witnesses.Place(
        witness.offset,
        std::visit(
            Overloaded{
                [&](LifecycleStep step) -> llvm::Constant* {
                  return Body(
                      type, witness.name, witness.stands_in_for,
                      Function(type, step));
                },
                [&](const OwnOperationWitness& own) -> llvm::Constant* {
                  return answered_by(witness, own.answers, own.crossing);
                },
                [&](NeverAsked) -> llvm::Constant* {
                  return Body(
                      type, witness.name, witness.stands_in_for, nullptr);
                },
                // A type with a default of its own -- a record's declaration,
                // the body a unit states for an array -- builds it; a tuple
                // holds each component at what its own type builds.
                [&](BuiltAsItsDefault) -> llvm::Constant* {
                  const bool defaulted = std::ranges::any_of(
                      operations, [](const OwnOperation& own) {
                        return own.answers ==
                               support::ValueOperation{
                                   support::BuiltinFn::kDeclaredDefault};
                      });
                  if (defaulted) {
                    return answered_by(
                        witness, support::BuiltinFn::kDeclaredDefault,
                        TakenAsStated{});
                  }
                  return ComponentWiseDefaultBody(
                      type, witness.name, witness.stands_in_for);
                }},
            witness.filled_by));
  }
  llvm::Constant* parts = llvm::ConstantPointerNull::get(ptr);
  if (array != nullptr) {
    parts = owner_->LibraryData(kFixedArrayPartsWitnessTableSymbol);
  }
  witnesses.Place(OffsetInTable(&value::ValueWitnessTable::parts), parts);
  llvm::Constant* const table_data = std::move(witnesses).Build();
  llvm::GlobalVariable* const table =
      global(std::format("lyra.type{}.witnesses", key), table_data->getType());
  table->setInitializer(table_data);
  table->setAlignment(llvm::Align(alignof(value::ValueWitnessTable)));

  using value::FixedArrayType;
  using value::TupleComponent;
  using value::TupleType;
  return std::visit(
      Overloaded{
          [&](const UnionLayout* overlay) -> llvm::GlobalVariable* {
            ConstantRecord out(ctx, sizeof(value::ValueType));
            PlaceValueType(
                out, table, overlay->storage, value::ValueTypeKind::kOther);
            return Defined(
                key, constant, std::move(out).Build(),
                alignof(value::ValueType));
          },
          [&](const ArrayLayout* laid) -> llvm::GlobalVariable* {
            auto element = GetAddrOfElementType(laid->element);
            if (!element) {
              throw InternalError(
                  "llvm codegen: an array's element is no value the runtime "
                  "realizes");
            }
            ConstantRecord out(ctx, sizeof(FixedArrayType));
            PlaceValueType(
                out, table, laid->storage, value::ValueTypeKind::kFixedArray);
            out.PlaceInteger(FixedArrayType::CountStatedAt(), laid->count);
            out.Place(FixedArrayType::ElementStatedAt(), *element);
            return Defined(
                key, constant, std::move(out).Build(), alignof(FixedArrayType));
          },
          [&](const ProductLayout* product) -> llvm::GlobalVariable* {
            // One component as its definition lays it out: where it lies, and
            // its type.
            const auto component = [&](std::uint64_t offset,
                                       llvm::Constant* of) {
              ConstantRecord record(ctx, sizeof(TupleComponent));
              record.PlaceInteger(
                  {.offset = offsetof(TupleComponent, offset),
                   .bytes = sizeof(TupleComponent::offset)},
                  offset);
              record.Place(offsetof(TupleComponent, type), of);
              return std::move(record).Build();
            };
            llvm::Type* const component_ty =
                component(0, llvm::ConstantPointerNull::get(ptr))->getType();
            std::vector<llvm::Constant*> components;
            components.reserve(product->components.size());
            for (std::size_t i = 0; i < product->components.size(); ++i) {
              auto component_type = GetAddrOfValueType(product->components[i]);
              if (!component_type) {
                throw InternalError(
                    "llvm codegen: a tuple component is no value the runtime "
                    "realizes");
              }
              components.push_back(
                  component(product->offsets[i], *component_type));
            }
            llvm::Constant* const listed_data = llvm::ConstantArray::get(
                llvm::ArrayType::get(component_ty, components.size()),
                components);
            llvm::GlobalVariable* listed = global(
                std::format("lyra.type{}.components", key),
                listed_data->getType());
            listed->setInitializer(listed_data);
            listed->setAlignment(llvm::Align(alignof(TupleComponent)));
            ConstantRecord out(ctx, sizeof(TupleType));
            PlaceValueType(
                out, table, product->storage, value::ValueTypeKind::kOther);
            out.PlaceInteger(
                TupleType::CountStatedAt(), product->components.size());
            out.Place(TupleType::ComponentsStatedAt(), listed);
            return Defined(
                key, constant, std::move(out).Build(), alignof(TupleType));
          }},
      types_->FieldsOf(type));
}

auto CodeGenValueTypes::GetAddrOfIntegralType(lir::TypeId type)
    -> llvm::Constant* {
  const value::IntegralShape shape = types_->RequiredIntegralShapeOf(type);
  const std::string key = ComponentKey(type);
  if (const auto found = integral_types_.find(key);
      found != integral_types_.end()) {
    return found->second;
  }
  using value::IntegralShape;
  using value::IntegralValueType;
  const std::size_t shape_at = IntegralValueType::ShapeStatedAt();
  ConstantRecord out(owner_->Module().getContext(), sizeof(IntegralValueType));
  PlaceValueType(
      out, owner_->LibraryData(kIntegralValueWitnessTableSymbol),
      types_->LayoutOf(type), value::ValueTypeKind::kIntegral);
  out.PlaceInteger(
      {.offset = shape_at + offsetof(IntegralShape, width),
       .bytes = sizeof(IntegralShape::width)},
      shape.width);
  out.PlaceInteger(
      {.offset = shape_at + offsetof(IntegralShape, signedness),
       .bytes = sizeof(IntegralShape::signedness)},
      std::to_underlying(shape.signedness));
  out.PlaceInteger(
      {.offset = shape_at + offsetof(IntegralShape, domain),
       .bytes = sizeof(IntegralShape::domain)},
      std::to_underlying(shape.domain));
  // An integral type is its shape wherever it is named, so every module naming
  // it states the same constant under the same name and the linker keeps one,
  // as it keeps one array type.
  llvm::GlobalVariable* const defined = owner_->DefineConstant(
      std::format("lyra.integral.{}", key), std::move(out).Build(),
      llvm::GlobalValue::LinkOnceODRLinkage,
      llvm::Align(alignof(IntegralValueType)));
  integral_types_.emplace(key, defined);
  return defined;
}

auto CodeGenValueTypes::Defined(
    const std::string& key, llvm::GlobalVariable* named,
    llvm::Constant* content, std::size_t align) -> llvm::GlobalVariable* {
  const llvm::GlobalValue::LinkageTypes linkage = named->getLinkage();
  llvm::GlobalVariable* const defined = owner_->DefineConstant(
      named->getName().str(), content, linkage, llvm::Align(align));
  types_of_.insert_or_assign(key, defined);
  return defined;
}

auto CodeGenValueTypes::EmitDeclared() -> void {
  for (const lir::RecordId id : unit_->records.Ids()) {
    GetAddrOfLaidOutType(
        unit_->types.Intern(lir::Type{lir::RecordType{.declaration = id}}));
  }
}

auto CodeGenValueTypes::DeclarationOf(lir::TypeId type) const
    -> std::optional<lir::TypeDeclarationRef> {
  const lir::RecordRef* record = lir::RecordOf(unit_->types.Get(type));
  if (record == nullptr) {
    return std::nullopt;
  }
  return lir::RecordDeclarationOf(*unit_, *record);
}

auto CodeGenValueTypes::Emit(
    lir::TypeId type, LifecycleStep step, llvm::Function* fn) -> void {
  llvm::LLVMContext& ctx = owner_->Module().getContext();
  llvm::IRBuilder<> b(llvm::BasicBlock::Create(ctx, "", fn));
  // A step of `of`'s, which its type says how to carry out. It fails only
  // where the library realizes no type for it, which is what building the
  // enclosing type refuses a component or a member for.
  const auto step_of = [&](llvm::IRBuilderBase& at, lir::TypeId of,
                           LifecycleStep taken,
                           std::vector<llvm::Value*> args) {
    if (!owner_->EmitLifecycle(at, of, taken, args)) {
      throw InternalError(
          "llvm codegen: a component or a member is no value the runtime "
          "realizes");
    }
  };
  std::visit(
      Overloaded{
          // An untagged union's members own nothing, so its steps are its
          // bytes'; a tagged one's carry every byte over and then take the
          // step of the member the tag names, as that member's type takes it,
          // which owns what it holds.
          [&](const UnionLayout* overlay) {
            const llvm::Align align(overlay->storage.layout.align);
            const std::size_t size = overlay->storage.layout.size;
            const std::optional<value::TagPlacement>& tag = overlay->tag;
            switch (step) {
              case LifecycleStep::kCopy:
              case LifecycleStep::kMove: {
                llvm::Value* const from = fn->getArg(0);
                llvm::Value* const out = fn->getArg(1);
                b.CreateMemCpy(out, align, from, align, size);
                if (tag.has_value()) {
                  ForHeldMember(
                      b, overlay->members, *tag, from,
                      [&](llvm::IRBuilderBase& at, lir::TypeId member) {
                        step_of(at, member, step, {from, out});
                      });
                }
                break;
              }
              case LifecycleStep::kDestroy:
                if (tag.has_value()) {
                  llvm::Value* const value = fn->getArg(0);
                  ForHeldMember(
                      b, overlay->members, *tag, value,
                      [&](llvm::IRBuilderBase& at, lir::TypeId member) {
                        step_of(at, member, step, {value});
                      });
                }
                break;
              // Assigning ends what the storage held and builds a copy of the
              // value in its place, unless the two are one.
              case LifecycleStep::kAssign: {
                llvm::Value* const storage_at = fn->getArg(0);
                llvm::Value* const value = fn->getArg(1);
                if (!tag.has_value()) {
                  b.CreateMemMove(storage_at, align, value, align, size);
                  break;
                }
                llvm::BasicBlock* const replace =
                    llvm::BasicBlock::Create(ctx, "replace", fn);
                llvm::BasicBlock* const done =
                    llvm::BasicBlock::Create(ctx, "assigned", fn);
                b.CreateCondBr(
                    b.CreateICmpEQ(storage_at, value), done, replace);
                b.SetInsertPoint(replace);
                b.CreateCall(
                    Function(type, LifecycleStep::kDestroy), {storage_at});
                b.CreateCall(
                    Function(type, LifecycleStep::kCopy), {value, storage_at});
                b.CreateBr(done);
                b.SetInsertPoint(done);
                break;
              }
            }
          },
          // An array's steps are each element's, in position order.
          [&](const ArrayLayout* laid) {
            switch (step) {
              case LifecycleStep::kCopy:
              case LifecycleStep::kMove:
              case LifecycleStep::kAssign: {
                llvm::Value* const from = fn->getArg(0);
                llvm::Value* const to = fn->getArg(1);
                ForEachElement(
                    b, type, from,
                    [&](llvm::IRBuilderBase& at, llvm::Value* element) {
                      llvm::Value* const offset =
                          at.CreatePtrDiff(at.getInt8Ty(), element, from);
                      step_of(
                          at, laid->element, step,
                          {element,
                           at.CreateInBoundsGEP(at.getInt8Ty(), to, offset)});
                    });
                break;
              }
              case LifecycleStep::kDestroy:
                ForEachElement(
                    b, type, fn->getArg(0),
                    [&](llvm::IRBuilderBase& at, llvm::Value* element) {
                      step_of(at, laid->element, step, {element});
                    });
                break;
            }
          },
          // A product's steps are each component's, in position order.
          [&](const ProductLayout* product) {
            // Where component `i` lies in the product at `base`.
            const auto at = [&](llvm::Value* base,
                                std::size_t i) -> llvm::Value* {
              return b.CreateConstInBoundsGEP1_64(
                  b.getInt8Ty(), base, product->offsets[i]);
            };
            const std::size_t count = product->components.size();
            switch (step) {
              case LifecycleStep::kCopy:
              case LifecycleStep::kMove:
              case LifecycleStep::kAssign:
                for (std::size_t i = 0; i < count; ++i) {
                  step_of(
                      b, product->components[i], step,
                      {at(fn->getArg(0), i), at(fn->getArg(1), i)});
                }
                break;
              case LifecycleStep::kDestroy:
                for (std::size_t i = 0; i < count; ++i) {
                  step_of(
                      b, product->components[i], step, {at(fn->getArg(0), i)});
                }
                break;
            }
          }},
      types_->FieldsOf(type));
  b.CreateRetVoid();
}

auto CodeGenValueTypes::GetAddrOfValueType(lir::TypeId type)
    -> diag::Result<llvm::Constant*> {
  using Answer = diag::Result<llvm::Constant*>;
  // A value no domain realizes is no value the library is told a type of.
  const auto unrealized = [&]() -> Answer {
    return diag::Fail(
        diag::DiagCode::kUnsupportedTypeKind,
        std::format(
            "a value of type {} has no runtime library realization",
            unit_->types.Get(type).KindName()));
  };
  const std::optional<lir::Holding> held = unit_->types.Get(type).HeldAs();
  if (!held.has_value()) {
    return unrealized();
  }
  return std::visit(
      Overloaded{
          [&](const lir::IntegralBits&) -> Answer {
            return GetAddrOfIntegralType(type);
          },
          [&](ValueDomain domain) -> Answer {
            // A type laid out by the design, whose table the code generator
            // fills; any other domain's type is the library's own.
            if (IsLaidOutByType(domain)) {
              return GetAddrOfLaidOutType(type);
            }
            return owner_->LibraryData(ValueTypeSymbol(domain));
          },
          [&](support::LibraryObject) -> Answer { return unrealized(); },
          [&](const lir::OwnedStorageAddress&) -> Answer {
            return unrealized();
          }},
      *held);
}

auto CodeGenValueTypes::GetAddrOfQueueType(lir::TypeId queue)
    -> diag::Result<llvm::Constant*> {
  if (const auto found = queue_types_.find(queue);
      found != queue_types_.end()) {
    return found->second;
  }
  const auto* declared = unit_->types.Get(queue).As<lir::QueueType>();
  if (declared == nullptr) {
    throw InternalError(
        "llvm codegen: a queue type's constant is asked of a type that is no "
        "queue -- please report this as a bug");
  }
  // LRM 7.10.5: a queue declaring no bound holds any number of elements, which
  // the constant states as a bound below zero.
  const std::int64_t bound =
      declared->max_bound.has_value()
          ? static_cast<std::int64_t>(*declared->max_bound)
          : -1;
  auto element = GetAddrOfElementType(declared->element_type);
  if (!element) {
    return std::unexpected(std::move(element.error()));
  }
  using value::QueueType;
  ConstantRecord out(owner_->Context(), sizeof(QueueType));
  out.Place(QueueType::ElementStatedAt(), *element);
  out.PlaceInteger(
      QueueType::BoundStatedAt(), static_cast<std::uint64_t>(bound));
  llvm::GlobalVariable* defined = owner_->DefineConstant(
      std::format("lyra.queue.{}", queue.value), std::move(out).Build(),
      llvm::GlobalValue::PrivateLinkage, llvm::Align(alignof(QueueType)));
  queue_types_.emplace(queue, defined);
  return defined;
}

auto CodeGenValueTypes::GetAddrOfElementType(lir::TypeId element)
    -> diag::Result<llvm::Constant*> {
  if (const auto found = element_types_.find(element);
      found != element_types_.end()) {
    return found->second;
  }
  auto type = GetAddrOfValueType(element);
  if (!type) {
    return std::unexpected(std::move(type.error()));
  }
  const auto stated = std::ranges::find_if(
      unit_->type_operations, [&](const lir::TypeOperation& operation) {
        return operation.type == element &&
               operation.answers == support::ValueOperation{
                                        support::BuiltinFn::kDeclaredDefault};
      });
  if (stated == unit_->type_operations.end()) {
    throw InternalError(
        "llvm codegen: the unit holds an array of a type whose default it "
        "does not state -- please report this as a bug");
  }
  using value::ElementScratch;
  using value::ElementType;
  const std::string name = std::format("lyra.element.{}", element.value);
  // What the type keeps between calls, which a constant cannot hold: where its
  // default and the storage a discarded write lands in are, once built. It
  // lasts as long as the program, so neither is ever given back.
  auto* scratch_ty = llvm::ArrayType::get(
      llvm::Type::getInt8Ty(owner_->Context()), sizeof(ElementScratch));
  auto* scratch = llvm::cast<llvm::GlobalVariable>(
      owner_->Module().getOrInsertGlobal(name + ".scratch", scratch_ty));
  scratch->setLinkage(llvm::GlobalValue::PrivateLinkage);
  scratch->setInitializer(llvm::ConstantAggregateZero::get(scratch_ty));
  scratch->setAlignment(llvm::Align(alignof(ElementScratch)));
  ConstantRecord out(owner_->Context(), sizeof(ElementType));
  out.Place(ElementType::TypeStatedAt(), *type);
  out.Place(
      ElementType::BuildDefaultStatedAt(),
      owner_->UnitFunction(stated->function));
  out.Place(ElementType::ScratchStatedAt(), scratch);
  llvm::GlobalVariable* defined = owner_->DefineConstant(
      name, std::move(out).Build(), llvm::GlobalValue::PrivateLinkage,
      llvm::Align(alignof(ElementType)));
  element_types_.emplace(element, defined);
  return defined;
}

}  // namespace lyra::backend::llvm_backend
