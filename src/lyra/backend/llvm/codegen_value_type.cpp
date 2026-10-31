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
#include <utility>
#include <variant>
#include <vector>

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

#include "lyra/backend/llvm/codegen_module.hpp"
#include "lyra/backend/llvm/constant_record.hpp"
#include "lyra/backend/llvm/integral_planes.hpp"
#include "lyra/backend/llvm/machine_signature.hpp"
#include "lyra/backend/llvm/runtime_entry.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/record_id.hpp"
#include "lyra/lir/symbol_name.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/value_step.hpp"
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

// How an operation's body takes and answers its integral values. The type the
// library asks through answers an equality as its scalar and a case equality
// as whether it holds, writes a stream into an integral value it is handed
// and reads one out of one, and is handed a count's control bits as one too,
// while a type's own function takes and answers each as a value of
// the type its own code states -- a stream a run of bits, where the type's is
// as long as the value makes it.
struct TakenAsStated {};
struct AnswerScalar {};
struct AnswerWhetherItHolds {};
struct AnswerIntoStream {};
struct StreamOutOfValue {};
struct RunIntoStream {};
struct StreamOutOfRun {};
struct ControlOutOfValue {};
using IntegralCrossing = std::variant<
    TakenAsStated, AnswerScalar, AnswerWhetherItHolds, AnswerIntoStream,
    StreamOutOfValue, RunIntoStream, StreamOutOfRun, ControlOutOfValue>;

// An operation a type answers with a function of its own, and how that
// function takes and answers its integral values.
struct OwnOperationWitness {
  support::ValueOperation answers;
  IntegralCrossing crossing = TakenAsStated{};
};

// A stream a type writes or reads by whichever of two functions of its own it
// states: one at the width its type fixes, or one whose stream is a run of
// bits as long as the value makes it (LRM 6.24.3).
struct EitherStreamForm {
  OwnOperationWitness fixed;
  OwnOperationWitness run;
};

using WitnessFilling =
    std::variant<lir::ValueStep, OwnOperationWitness, EitherStreamForm>;

// One witness of a table a type is told to the library through: the storage's
// own lifecycle, the operations the language defines on the whole value (LRM
// 11.4.5, 20.6.2, 20.9, 6.24.3), how a net folds values of it (LRM 6.6.1,
// 28.12.1), which a record's declaration states as its methods and a unit
// states for an array, or one no such type answers. `offset` is where the table
// lays it out, and a body filling it takes and answers what its declaration
// does.
struct Witness {
  std::string_view name;
  std::size_t offset;
  support::MachineSignature stands_in_for;
  WitnessFilling filled_by;
};

// What a function of the table takes after the type and what it answers,
// read off its declaration. One that does not throw is read the same way.
template <typename Function>
struct WitnessSignatureOf;
template <typename R, typename... Params>
struct WitnessSignatureOf<R (*)(const value::ValueType&, Params...)> {
  static auto Get() -> support::MachineSignature {
    return support::MachineSignatureOf<R(Params...)>::Get();
  }
};
template <typename R, typename... Params>
struct WitnessSignatureOf<R (*)(const value::ValueType&, Params...) noexcept>
    : WitnessSignatureOf<R (*)(const value::ValueType&, Params...)> {};
// An operation's function is handed the operations it belongs to first.
template <typename R, typename... Params>
struct WitnessSignatureOf<R (*)(const value::ValueOperations&, Params...)> {
  static auto Get() -> support::MachineSignature {
    return support::MachineSignatureOf<R(Params...)>::Get();
  }
};

// Where a table's declaration lays out its member `member`, read off one
// table laid out as it is.
template <typename Table, typename Field>
auto OffsetInTable(Field Table::* member) -> std::size_t {
  const Table table{};
  return static_cast<std::size_t>(
      std::bit_cast<std::uintptr_t>(&(table.*member)) -
      std::bit_cast<std::uintptr_t>(&table));
}

// The witness a table's member `member` holds: where the declaration lays it
// out, and what a body filling it takes and answers.
template <typename Table, typename Function>
auto WitnessOf(
    std::string_view name, Function Table::* member, WitnessFilling filled_by)
    -> Witness {
  return {
      .name = name,
      .offset = OffsetInTable(member),
      .stands_in_for = WitnessSignatureOf<Function>::Get(),
      .filled_by = filled_by};
}

// Every witness of the type's table but its parts, which no body fills.
auto ValueWitnesses() {
  using support::BuiltinFn;
  using value::ValueWitnessTable;
  return std::to_array<Witness>({
      WitnessOf("copy", &ValueWitnessTable::copy, lir::ValueStep::kCopy),
      WitnessOf("move", &ValueWitnessTable::move, lir::ValueStep::kMove),
      WitnessOf("destroy", &ValueWitnessTable::destroy, lir::ValueStep::kEnd),
      WitnessOf("assign", &ValueWitnessTable::assign, lir::ValueStep::kAssign),
      WitnessOf(
          "bit_identical", &ValueWitnessTable::bit_identical,
          OwnOperationWitness{.answers = BuiltinFn::kBitIdentical}),
  });
}

// The table is its witnesses, then its parts, one address each.
static_assert(
    sizeof(value::ValueWitnessTable) ==
    (std::tuple_size_v<decltype(ValueWitnesses())> + 1) * sizeof(void*));

// Every function of the language's operations on a whole value of a type.
auto OperationWitnesses() {
  using support::BuiltinFn;
  using value::OperationFunctions;
  return std::to_array<Witness>({
      WitnessOf(
          "equal", &OperationFunctions::equal,
          OwnOperationWitness{
              .answers = support::ValueOperator::kEquality,
              .crossing = AnswerScalar{}}),
      WitnessOf(
          "case_equal", &OperationFunctions::case_equal,
          OwnOperationWitness{
              .answers = BuiltinFn::kCaseEqual,
              .crossing = AnswerWhetherItHolds{}}),
      WitnessOf(
          "has_unknown", &OperationFunctions::has_unknown,
          OwnOperationWitness{.answers = BuiltinFn::kHasUnknown}),
      WitnessOf(
          "bitstream_width", &OperationFunctions::bitstream_width,
          OwnOperationWitness{.answers = BuiltinFn::kBitstreamWidth}),
      WitnessOf(
          "count_bits", &OperationFunctions::count_bits,
          OwnOperationWitness{
              .answers = BuiltinFn::kCountBits,
              .crossing = ControlOutOfValue{}}),
      WitnessOf(
          "write_to_stream", &OperationFunctions::write_to_stream,
          EitherStreamForm{
              .fixed =
                  OwnOperationWitness{
                      .answers = BuiltinFn::kToBitstream,
                      .crossing = AnswerIntoStream{}},
              .run =
                  OwnOperationWitness{
                      .answers = BuiltinFn::kToBitVec,
                      .crossing = RunIntoStream{}}}),
      WitnessOf(
          "read_from_stream", &OperationFunctions::read_from_stream,
          EitherStreamForm{
              .fixed =
                  OwnOperationWitness{
                      .answers = BuiltinFn::kFromBitstream,
                      .crossing = StreamOutOfValue{}},
              .run =
                  OwnOperationWitness{
                      .answers = BuiltinFn::kFromBitVec,
                      .crossing = StreamOutOfRun{}}}),
  });
}

static_assert(
    sizeof(value::OperationFunctions) ==
    std::tuple_size_v<decltype(OperationWitnesses())> * sizeof(void*));

// Every witness of how a net folds values of a type, each the function of the
// type's own answering that fold.
auto NetFoldWitnesses() {
  using support::BuiltinFn;
  using value::NetFoldTable;
  return std::to_array<Witness>({
      WitnessOf(
          "resolve_tri_state", &NetFoldTable::tri_state,
          OwnOperationWitness{.answers = BuiltinFn::kResolveTriState}),
      WitnessOf(
          "resolve_wired_and", &NetFoldTable::wired_and,
          OwnOperationWitness{.answers = BuiltinFn::kResolveWiredAnd}),
      WitnessOf(
          "resolve_wired_or", &NetFoldTable::wired_or,
          OwnOperationWitness{.answers = BuiltinFn::kResolveWiredOr}),
      WitnessOf(
          "dominating", &NetFoldTable::dominating,
          OwnOperationWitness{.answers = BuiltinFn::kDominating}),
      WitnessOf(
          "filled_like", &NetFoldTable::filled_like,
          OwnOperationWitness{.answers = BuiltinFn::kFilledLike}),
  });
}

static_assert(
    sizeof(value::NetFoldTable) ==
    std::tuple_size_v<decltype(NetFoldWitnesses())> * sizeof(void*));

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
  return keys_.emplace(type, lir::SymbolPartOf(*unit_, type).encoded)
      .first->second;
}

auto CodeGenValueTypes::DeclareBody(
    lir::TypeId type, std::string_view witness,
    const support::MachineSignature& stands_in_for) -> llvm::Function* {
  llvm::LLVMContext& context = owner_->Module().getContext();
  std::vector<llvm::Type*> params{types_->Ptr()};
  params.reserve(1 + stands_in_for.Takes().size());
  for (const support::MachineKind taken : stands_in_for.Takes()) {
    params.push_back(ToLlvmType(context, taken));
  }
  llvm::Function* body = llvm::Function::Create(
      llvm::FunctionType::get(
          ToLlvmType(context, stands_in_for.Returns()), params, false),
      llvm::GlobalValue::InternalLinkage,
      std::format("lyra.type{}.type.{}", KeyOf(type), witness),
      owner_->Module());
  MarkBoolsAsCpp(body);
  return body;
}

auto CodeGenValueTypes::Body(
    lir::TypeId type, std::string_view witness,
    const support::MachineSignature& stands_in_for, llvm::Function* filling)
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
  args.reserve(stands_in_for.Takes().size());
  for (std::size_t i = 1; i < body->arg_size(); ++i) {
    args.push_back(body->getArg(static_cast<unsigned>(i)));
  }
  llvm::Value* answered = b.CreateCall(filling, args);
  // A function answering in storage it was handed answers with that storage
  // too, which the witness it fills does not.
  switch (stands_in_for.Returns()) {
    case support::MachineKind::kVoid:
      b.CreateRetVoid();
      break;
    case support::MachineKind::kAddress:
    case support::MachineKind::kTruth:
    case support::MachineKind::kInt8:
    case support::MachineKind::kInt32:
    case support::MachineKind::kInt64:
    case support::MachineKind::kFloat:
    case support::MachineKind::kDouble:
    case support::MachineKind::kAggregate:
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
    const support::MachineSignature& stands_in_for, lir::TypeId stated)
    -> ThunkBody {
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
  args.reserve(stands_in_for.Takes().size());
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
    const support::MachineSignature& stands_in_for, lir::FunctionId method,
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
    const support::MachineSignature& stands_in_for, lir::FunctionId method)
    -> llvm::Constant* {
  llvm::Function* const body = DeclareBody(type, witness, stands_in_for);
  llvm::IRBuilder<> b(
      llvm::BasicBlock::Create(owner_->Module().getContext(), "", body));
  const value::IntegralShape stream =
      types_->RequiredIntegralShapeOf(unit_->functions.Get(method).result_type);
  llvm::Type* const answers = b.getInt64Ty();
  b.CreateRet(ForwardToEntry(
      b, RuntimeOp::kStreamWrite, answers,
      {owner_->UnitFunction(method), body->getArg(1), b.getInt64(stream.width),
       b.getInt1(stream.IsFourState()), body->getArg(2), body->getArg(3)}));
  return body;
}

auto CodeGenValueTypes::StreamReadBody(
    lir::TypeId type, std::string_view witness,
    const support::MachineSignature& stands_in_for, lir::FunctionId method)
    -> llvm::Constant* {
  llvm::Function* const body = DeclareBody(type, witness, stands_in_for);
  llvm::IRBuilder<> b(
      llvm::BasicBlock::Create(owner_->Module().getContext(), "", body));
  const value::IntegralShape stream =
      types_->RequiredIntegralShapeOf(ParamReferent(method, 0));
  llvm::Type* const answers = b.getInt64Ty();
  b.CreateRet(ForwardToEntry(
      b, RuntimeOp::kStreamBuild, answers,
      {owner_->UnitFunction(method), body->getArg(1), body->getArg(2),
       b.getInt64(stream.width), b.getInt1(stream.IsFourState()),
       body->getArg(4)}));
  return body;
}

auto CodeGenValueTypes::RunWriteBody(
    lir::TypeId type, std::string_view witness,
    const support::MachineSignature& stands_in_for, lir::FunctionId method)
    -> llvm::Constant* {
  llvm::Function* const body = DeclareBody(type, witness, stands_in_for);
  llvm::IRBuilder<> b(
      llvm::BasicBlock::Create(owner_->Module().getContext(), "", body));
  llvm::Type* const answers = b.getInt64Ty();
  b.CreateRet(ForwardToEntry(
      b, RuntimeOp::kRunWrite, answers,
      {owner_->UnitFunction(method), body->getArg(1), body->getArg(2),
       body->getArg(3)}));
  return body;
}

auto CodeGenValueTypes::RunBuildBody(
    lir::TypeId type, std::string_view witness,
    const support::MachineSignature& stands_in_for, lir::FunctionId method)
    -> llvm::Constant* {
  llvm::Function* const body = DeclareBody(type, witness, stands_in_for);
  llvm::IRBuilder<> b(
      llvm::BasicBlock::Create(owner_->Module().getContext(), "", body));
  llvm::Type* const answers = b.getInt64Ty();
  b.CreateRet(ForwardToEntry(
      b, RuntimeOp::kRunBuild, answers,
      {owner_->UnitFunction(method), body->getArg(1), body->getArg(2),
       body->getArg(3), body->getArg(4)}));
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
  return b.CreateCall(
      owner_->CreateRuntimeFunction(
          llvm::FunctionType::get(answers, takes, false), RuntimeSymbol(op)),
      args);
}

// The function takes a count's control bits as a value of the type its own
// code states, which the value the type was handed is read into first.
auto CodeGenValueTypes::ControlReadBody(
    lir::TypeId type, std::string_view witness,
    const support::MachineSignature& stands_in_for, lir::FunctionId method)
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
  llvm::Type* const byte = b.getInt8Ty();
  llvm::Value* const offset = b.CreateMul(position, b.getInt64(laid.stride));
  llvm::Value* const element = b.CreateInBoundsGEP(byte, value, offset);
  each(b, element);
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
  if (const lir::RecordId* record = lir::RecordOf(unit_->types.Get(type))) {
    for (const lir::RecordMethod& method :
         unit_->records.Get(*record).methods) {
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

auto CodeGenValueTypes::OwnOperationOf(
    lir::TypeId type, support::ValueOperation operation) const
    -> std::optional<lir::FunctionId> {
  for (const OwnOperation& own : OwnOperations(type)) {
    if (own.answers == operation) {
      return own.function;
    }
  }
  return std::nullopt;
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

  // A struct or a union another unit declares is that unit's to state; this
  // one refers to it and fills nothing.
  const std::optional<llvm::GlobalValue::LinkageTypes> linkage =
      LinkageOfItsOperations(type);
  if (!linkage.has_value()) {
    constant->setLinkage(llvm::GlobalValue::ExternalLinkage);
    return constant;
  }
  constant->setLinkage(*linkage);
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
            [&](RunIntoStream) -> llvm::Constant* {
              return RunWriteBody(
                  type, witness.name, witness.stands_in_for, method->function);
            },
            [&](StreamOutOfRun) -> llvm::Constant* {
              return RunBuildBody(
                  type, witness.name, witness.stands_in_for, method->function);
            },
            [&](ControlOutOfValue) -> llvm::Constant* {
              return ControlReadBody(
                  type, witness.name, witness.stands_in_for, method->function);
            }},
        crossing);
  };

  // The body filling `witness`, each where its record's declaration lays it
  // out.
  const auto filled = [&](const Witness& witness) -> llvm::Constant* {
    return std::visit(
        Overloaded{
            [&](lir::ValueStep step) -> llvm::Constant* {
              return Body(
                  type, witness.name, witness.stands_in_for,
                  owner_->UnitFunction(
                      lir::StepFunctionOf(*unit_, type, step)));
            },
            [&](const OwnOperationWitness& own) -> llvm::Constant* {
              return answered_by(witness, own.answers, own.crossing);
            },
            [&](const EitherStreamForm& either) -> llvm::Constant* {
              const OwnOperationWitness& own =
                  std::ranges::any_of(
                      operations,
                      [&](const OwnOperation& stated) {
                        return stated.answers == either.run.answers;
                      })
                      ? either.run
                      : either.fixed;
              return answered_by(witness, own.answers, own.crossing);
            }},
        witness.filled_by);
  };
  // The functions of the language's operations on a value of the type, which
  // the type's operations name.
  ConstantRecord function_record(ctx, sizeof(value::OperationFunctions));
  for (const Witness& witness : OperationWitnesses()) {
    function_record.Place(witness.offset, filled(witness));
  }
  llvm::Constant* const functions_data = std::move(function_record).Build();
  llvm::GlobalVariable* const functions = global(
      std::format("lyra.type{}.operation_functions", key),
      functions_data->getType());
  functions->setInitializer(functions_data);
  functions->setAlignment(llvm::Align(alignof(value::OperationFunctions)));
  operation_functions_of_.insert_or_assign(key, functions);

  // The table, each witness where the library's declaration of it lays it
  // out, and for an array the library's answer for its elements by position,
  // which reads them off the type it is handed.
  ConstantRecord witnesses(ctx, sizeof(value::ValueWitnessTable));
  for (const Witness& witness : ValueWitnesses()) {
    witnesses.Place(witness.offset, filled(witness));
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

auto CodeGenValueTypes::LinkageOfItsOperations(lir::TypeId type) const
    -> std::optional<llvm::GlobalValue::LinkageTypes> {
  const lir::Type& stated = unit_->types.Get(type);
  if (const lir::RecordId* record = lir::RecordOf(stated)) {
    const lir::RecordDecl& declared = unit_->records.Get(*record);
    if (declared.defining_unit.has_value()) {
      return std::nullopt;
    }
    return LinkageOf(declared.linkage);
  }
  if (stated.Is<lir::UnpackedArrayType>()) {
    return llvm::GlobalValue::LinkOnceODRLinkage;
  }
  return llvm::GlobalValue::InternalLinkage;
}

auto CodeGenValueTypes::GetAddrOfNetFolds(lir::TypeId type) -> llvm::Constant* {
  const std::string& key = KeyOf(type);
  if (const auto found = net_folds_of_.find(key);
      found != net_folds_of_.end()) {
    return found->second;
  }
  llvm::Module& module = owner_->Module();
  const std::string name = std::format("lyra.type{}.net_folds", key);
  const std::optional<llvm::GlobalValue::LinkageTypes> linkage =
      LinkageOfItsOperations(type);
  if (!linkage.has_value()) {
    llvm::Constant* const elsewhere =
        module.getOrInsertGlobal(name, types_->Ptr());
    net_folds_of_.emplace(key, elsewhere);
    return elsewhere;
  }
  ConstantRecord folds(module.getContext(), sizeof(value::NetFoldTable));
  for (const Witness& witness : NetFoldWitnesses()) {
    const std::optional<lir::FunctionId> method = OwnOperationOf(
        type, std::get<OwnOperationWitness>(witness.filled_by).answers);
    folds.Place(
        witness.offset,
        Body(
            type, witness.name, witness.stands_in_for,
            method.has_value() ? owner_->UnitFunction(*method) : nullptr));
  }
  llvm::GlobalVariable* const defined = owner_->DefineConstant(
      name, std::move(folds).Build(), *linkage,
      llvm::Align(alignof(value::NetFoldTable)));
  net_folds_of_.emplace(key, defined);
  return defined;
}

auto CodeGenValueTypes::GetAddrOfTakePositions(lir::TypeId type)
    -> llvm::Constant* {
  const std::string& key = KeyOf(type);
  if (const auto found = take_positions_of_.find(key);
      found != take_positions_of_.end()) {
    return found->second;
  }
  const support::MachineSignature stands_in_for =
      WitnessSignatureOf<value::TakePositionsWitness>::Get();
  const std::optional<llvm::GlobalValue::LinkageTypes> linkage =
      LinkageOfItsOperations(type);
  if (!linkage.has_value()) {
    llvm::Function* const elsewhere =
        DeclareBody(type, "take_positions", stands_in_for);
    elsewhere->setLinkage(llvm::GlobalValue::ExternalLinkage);
    take_positions_of_.emplace(key, elsewhere);
    return elsewhere;
  }
  const std::optional<lir::FunctionId> method =
      OwnOperationOf(type, support::BuiltinFn::kTakePositions);
  if (!method.has_value()) {
    throw InternalError(
        "llvm codegen: a takeover begins on a value whose type states no way "
        "to take some of its positions -- please report this as a bug");
  }
  llvm::Constant* const body = Body(
      type, "take_positions", stands_in_for, owner_->UnitFunction(*method));
  llvm::cast<llvm::Function>(body)->setLinkage(*linkage);
  take_positions_of_.emplace(key, body);
  return body;
}

auto CodeGenValueTypes::GetAddrOfOperations(lir::TypeId type)
    -> diag::Result<llvm::Constant*> {
  const std::string& key = KeyOf(type);
  if (const auto found = operations_of_.find(key);
      found != operations_of_.end()) {
    return found->second;
  }
  llvm::Module& module = owner_->Module();
  const std::string name = std::format("lyra.type{}.operations", key);
  // The record, once its functions, the type and the element's operations are
  // known, under `linkage`.
  const auto define =
      [&](llvm::Constant* functions, llvm::Constant* of,
          llvm::Constant* element,
          llvm::GlobalValue::LinkageTypes linkage) -> llvm::Constant* {
    ConstantRecord out(module.getContext(), sizeof(value::ValueOperations));
    out.Place(offsetof(value::ValueOperations, functions), functions);
    out.Place(offsetof(value::ValueOperations, type), of);
    out.Place(offsetof(value::ValueOperations, element), element);
    llvm::GlobalVariable* const defined = owner_->DefineConstant(
        name, std::move(out).Build(), linkage,
        llvm::Align(alignof(value::ValueOperations)));
    operations_of_.emplace(key, defined);
    return defined;
  };
  llvm::Constant* const none = llvm::ConstantPointerNull::get(types_->Ptr());
  const lir::Type& stated = unit_->types.Get(type);
  if (lir::IsLaidOutByItsType(stated)) {
    const std::optional<llvm::GlobalValue::LinkageTypes> linkage =
        LinkageOfItsOperations(type);
    if (!linkage.has_value()) {
      llvm::Constant* const elsewhere =
          module.getOrInsertGlobal(name, types_->Ptr());
      operations_of_.emplace(key, elsewhere);
      return elsewhere;
    }
    llvm::GlobalVariable* const of = GetAddrOfLaidOutType(type);
    // A fixed-size unpacked array names its element's operations, as any
    // array does, for what the library does with a run of its elements.
    llvm::Constant* element = none;
    if (const std::optional<lir::TypeId> held = stated.ContainerElementType()) {
      auto of_element = GetAddrOfOperations(*held);
      if (!of_element) {
        return std::unexpected(std::move(of_element.error()));
      }
      element = *of_element;
    }
    return define(operation_functions_of_.at(key), of, element, *linkage);
  }
  if (types_->IntegralShapeOf(type).has_value()) {
    return define(
        owner_->LibraryData(kIntegralOperationFunctionsSymbol),
        GetAddrOfIntegralType(type), none,
        llvm::GlobalValue::LinkOnceODRLinkage);
  }
  auto domain = owner_->DomainOf(type);
  if (!domain) {
    return std::unexpected(std::move(domain.error()));
  }
  if (const std::optional<lir::TypeId> element =
          stated.ContainerElementType()) {
    auto of_element = GetAddrOfOperations(*element);
    if (!of_element) {
      return std::unexpected(std::move(of_element.error()));
    }
    return define(
        owner_->LibraryData(OperationFunctionsSymbol(*domain)),
        owner_->LibraryData(ValueTypeSymbol(*domain)), *of_element,
        llvm::GlobalValue::LinkOnceODRLinkage);
  }
  llvm::Constant* const library =
      owner_->LibraryData(ValueOperationsSymbol(*domain));
  operations_of_.emplace(key, library);
  return library;
}

auto CodeGenValueTypes::GetAddrOfIntegralType(lir::TypeId type)
    -> llvm::Constant* {
  const value::IntegralShape shape = types_->RequiredIntegralShapeOf(type);
  const std::string& key = KeyOf(type);
  if (const auto found = integral_types_.find(key);
      found != integral_types_.end()) {
    return found->second;
  }
  using value::IntegralShape;
  using value::IntegralValueType;
  const std::size_t shape_at = IntegralValueType::ShapeStatedAt();
  ConstantRecord out(owner_->Module().getContext(), sizeof(IntegralValueType));
  llvm::Constant* const witnesses =
      owner_->LibraryData(kIntegralValueWitnessTableSymbol);
  const support::ObjectLayout storage = types_->LayoutOf(type);
  PlaceValueType(out, witnesses, storage, value::ValueTypeKind::kIntegral);
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
    // A record another unit defines is laid out by that unit.
    const lir::RecordDecl& declared = unit_->records.Get(id);
    if (declared.defining_unit.has_value()) continue;
    const lir::TypeId type =
        unit_->types.Intern(lir::Type{lir::RecordType{.declaration = id}});
    GetAddrOfLaidOutType(type);
    // Everything another unit hands the library of the record is stated here,
    // where the record states the operations it is made of.
    if (OwnOperationOf(type, support::BuiltinFn::kResolveTriState)
            .has_value()) {
      GetAddrOfNetFolds(type);
    }
    if (OwnOperationOf(type, support::BuiltinFn::kTakePositions).has_value()) {
      GetAddrOfTakePositions(type);
    }
    if (!GetAddrOfOperations(type)) {
      throw InternalError(
          "llvm codegen: a record this unit declares has no operations the "
          "runtime realizes -- please report this as a bug");
    }
    if (!GetAddrOfElementType(type)) {
      throw InternalError(
          "llvm codegen: a record this unit declares is no value the runtime "
          "realizes -- please report this as a bug");
    }
  }
}

auto CodeGenValueTypes::GetAddrOfValueType(lir::TypeId type)
    -> diag::Result<llvm::Constant*> {
  using Answer = diag::Result<llvm::Constant*>;
  // A value no domain realizes is no value the library is told a type of.
  const auto unrealized = [&]() -> Answer {
    return diag::Fail(
        diag::DiagCode::kUnsupportedTypeKind,
        "a value of type '{}' is not yet supported with '--backend llvm'",
        unit_->types.Get(type).KindName());
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
            if (support::IsLaidOutByItsType(domain)) {
              return GetAddrOfLaidOutType(type);
            }
            return owner_->LibraryData(ValueTypeSymbol(domain));
          },
          [&](support::LibraryObject) -> Answer { return unrealized(); },
          [&](const lir::OwnedStorageAddress&) -> Answer {
            return unrealized();
          },
          [&](const lir::ClosureStorage&) -> Answer { return unrealized(); }},
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
  using value::ElementScratch;
  using value::ElementType;
  // A record's default is its declaration's, so the record stating it, with
  // the type, is its declaring unit's, linked as the record is; another unit
  // refers to it. Any other type's is the default the unit states for it.
  const lir::RecordId* const record = lir::RecordOf(unit_->types.Get(element));
  std::string name = std::format("lyra.element.{}", element.value);
  llvm::GlobalValue::LinkageTypes linkage = llvm::GlobalValue::PrivateLinkage;
  if (record != nullptr) {
    name = std::format("lyra.type{}.held", KeyOf(element));
    const std::optional<llvm::GlobalValue::LinkageTypes> own =
        LinkageOfItsOperations(element);
    if (!own.has_value()) {
      llvm::Constant* const elsewhere =
          owner_->Module().getOrInsertGlobal(name, types_->Ptr());
      element_types_.emplace(element, elsewhere);
      return elsewhere;
    }
    linkage = *own;
  }
  const std::optional<lir::FunctionId> builder =
      OwnOperationOf(element, support::BuiltinFn::kDeclaredDefault);
  if (!builder.has_value()) {
    throw InternalError(
        "llvm codegen: a value of a type whose default the unit does not "
        "state is held -- please report this as a bug");
  }
  auto type = GetAddrOfValueType(element);
  if (!type) {
    return std::unexpected(std::move(type.error()));
  }
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
      ElementType::BuildDefaultStatedAt(), owner_->UnitFunction(*builder));
  out.Place(ElementType::ScratchStatedAt(), scratch);
  llvm::GlobalVariable* defined = owner_->DefineConstant(
      name, std::move(out).Build(), linkage, llvm::Align(alignof(ElementType)));
  element_types_.emplace(element, defined);
  return defined;
}

}  // namespace lyra::backend::llvm_backend
