#include "lyra/backend/llvm/codegen_tuple.hpp"

#include <array>
#include <bit>
#include <cstddef>
#include <cstdint>
#include <format>
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
#include "lyra/backend/llvm/integral_one_word.hpp"
#include "lyra/backend/llvm/runtime_entry.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/struct_id.hpp"
#include "lyra/lir/symbol_name.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/runtime/object_layout.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/runtime_object.hpp"
#include "lyra/support/value_domain.hpp"
#include "lyra/support/value_operation.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/runtime_tuple.hpp"

namespace lyra::backend::llvm_backend {

namespace {

using support::ValueDomain;

// The call on `op`, an entry standing between a structure's own operations
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
        "a virtual function of the type class takes or answers a type no "
        "body standing in for it could");
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

// A tuple's type is read by the runtime through the one C++ class both sides
// compile against: the address of its table, then its size, its alignment and
// how many components it has, then where they are listed, at that address's
// alignment -- laid out as the C++ ABI lays that class out (C++ ABI 2.4). The
// assertions hold that class's size, and its record of one component, to what
// is written out for them below.
static_assert(
    sizeof(value::TupleType) ==
    sizeof(void*) + (4 * sizeof(std::uint32_t)) + sizeof(void*));
static_assert(
    sizeof(value::TupleComponent) ==
    sizeof(std::uint32_t) + sizeof(std::uint32_t) + sizeof(void*));
static_assert(offsetof(value::TupleComponent, type) == sizeof(void*));

// The operations a container's algorithms ask of an element or of what a
// `with` clause answers -- an order, a truth, a reduction (LRM 7.8, 7.12, 12.4)
// -- none of which is asked of a structure: it has no relational or arithmetic
// operator to order, test or fold by, and an associative array indexed by one
// is refused where it is declared.
struct NeverAskedOfAStructure {};

// What a type is asked that a structure answers with nothing: the walk over
// parts ordered by position, a structure's being components named by position
// in its type rather than a sequence of one type, and the type as an integral
// one.
struct AnsweredWithNothing {};

// How an operation's body takes and answers its integral values. The type the
// library asks through answers an equality as its scalar and a case equality
// as whether it holds, writes a stream into planes it is handed and reads one
// out of them, and is handed a count's control bits as planes too, while the
// function a structure states takes and answers each as a value of the type
// its own code states.
struct TakenAsStated {};
struct AnswerScalar {};
struct AnswerWhetherItHolds {};
struct AnswerIntoStream {};
struct StreamOutOfPlanes {};
struct ControlOutOfPlanes {};
using IntegralCrossing = std::variant<
    TakenAsStated, AnswerScalar, AnswerWhetherItHolds, AnswerIntoStream,
    StreamOutOfPlanes, ControlOutOfPlanes>;

using SlotFilling = std::variant<
    TupleLifecycle, support::ValueOperation, NeverAskedOfAStructure,
    AnsweredWithNothing>;

// Where the virtual function `member` lies in its class's table, counted in
// entries from the address point. A pointer to a virtual member function holds
// one plus that entry's byte offset (C++ ABI 2.3), so the class declaration is
// what states the order.
template <typename Member>
auto EntryOf(Member member) -> std::size_t {
  struct Representation {
    std::uintptr_t offset_plus_one;
    std::ptrdiff_t adjustment;
  };
  static_assert(sizeof(Member) == sizeof(Representation));
  const auto held = std::bit_cast<Representation>(member);
  return (held.offset_plus_one - 1) / sizeof(void*);
}

// One body of a tuple type's table: the storage's own lifecycle, the operations
// the language defines on the whole value (LRM 11.4.5, 20.6.2, 20.9, 6.24.3,
// 6.6.1, 28.12.1), which the type's declaration states as its methods, or one
// no structure answers. A body takes and answers what the virtual function it
// stands in for does.
struct TableSlot {
  std::string_view name;
  std::size_t entry;
  VirtualSignature stands_in_for;
  SlotFilling filled_by;
  IntegralCrossing crossing;
};

// The slot of the virtual function `member` names, which also states what a
// body standing in for it takes and answers. A member that does not throw
// converts to this form, so both are read here.
template <typename R, typename... Params>
auto Slot(
    std::string_view name, R (value::ValueType::*member)(Params...) const,
    SlotFilling filled_by, IntegralCrossing crossing = TakenAsStated{})
    -> TableSlot {
  return {
      .name = name,
      .entry = EntryOf(member),
      .stands_in_for =
          VirtualSignature{
              .returns = MachineKindOf<R>(),
              .takes = {MachineKindOf<Params>()...}},
      .filled_by = filled_by,
      .crossing = crossing};
}

auto TableSlots() -> std::vector<TableSlot> {
  using support::BuiltinFn;
  using value::ValueType;
  return {
      Slot("copy", &ValueType::Copy, TupleLifecycle::kCopy),
      Slot("move", &ValueType::Move, TupleLifecycle::kMove),
      Slot("destroy", &ValueType::Destroy, TupleLifecycle::kDestroy),
      Slot("assign", &ValueType::Assign, TupleLifecycle::kAssign),
      Slot(
          "equal", &ValueType::Equal, support::ValueOperator::kEquality,
          AnswerScalar{}),
      Slot(
          "case_equal", &ValueType::CaseEqual, BuiltinFn::kCaseEqual,
          AnswerWhetherItHolds{}),
      Slot("bit_identical", &ValueType::BitIdentical, BuiltinFn::kBitIdentical),
      Slot("has_unknown", &ValueType::HasUnknown, BuiltinFn::kHasUnknown),
      Slot(
          "bitstream_width", &ValueType::BitstreamWidth,
          BuiltinFn::kBitstreamWidth),
      Slot(
          "count_bits", &ValueType::CountBits, BuiltinFn::kCountBits,
          ControlOutOfPlanes{}),
      Slot(
          "write_to_stream", &ValueType::WriteToStream, BuiltinFn::kToBitstream,
          AnswerIntoStream{}),
      Slot(
          "read_from_stream", &ValueType::ReadFromStream,
          BuiltinFn::kFromBitstream, StreamOutOfPlanes{}),
      Slot(
          "resolve_tri_state", &ValueType::ResolveTriState,
          BuiltinFn::kResolveTriState),
      Slot(
          "resolve_wired_and", &ValueType::ResolveWiredAnd,
          BuiltinFn::kResolveWiredAnd),
      Slot(
          "resolve_wired_or", &ValueType::ResolveWiredOr,
          BuiltinFn::kResolveWiredOr),
      Slot("dominating", &ValueType::Dominating, BuiltinFn::kDominating),
      Slot("filled_like", &ValueType::FilledLike, BuiltinFn::kFilledLike),
      Slot("order_before", &ValueType::OrderBefore, NeverAskedOfAStructure{}),
      Slot("is_true", &ValueType::IsTrue, NeverAskedOfAStructure{}),
      Slot("reduce", &ValueType::Reduce, NeverAskedOfAStructure{}),
      Slot("parts", &ValueType::Parts, AnsweredWithNothing{}),
      Slot("as_integral", &ValueType::AsIntegral, AnsweredWithNothing{}),
  };
}

// The destructor's two entries open the table (C++ ABI 2.5.2). A type lasts as
// long as the program and nothing ends one, so neither is ever entered.
constexpr std::size_t kDestructorEntries = 2;

auto StepName(TupleLifecycle step) -> std::string_view {
  switch (step) {
    case TupleLifecycle::kCopy:
      return "copy";
    case TupleLifecycle::kMove:
      return "move";
    case TupleLifecycle::kDestroy:
      return "destroy";
    case TupleLifecycle::kAssign:
      return "assign";
  }
  throw InternalError("llvm codegen: unknown tuple lifecycle step");
}

// The method among `methods` answering `operation`, or none where the type has
// none of the operation.
auto MethodAnswering(
    std::span<const lir::StructMethod> methods,
    support::ValueOperation operation) -> std::optional<lir::FunctionId> {
  for (const lir::StructMethod& method : methods) {
    if (method.answers == operation) {
      return method.function;
    }
  }
  return std::nullopt;
}

}  // namespace

auto LifecycleOp(TupleLifecycle step) -> RuntimeOp {
  switch (step) {
    case TupleLifecycle::kCopy:
      return RuntimeOp::kCopy;
    case TupleLifecycle::kMove:
      return RuntimeOp::kMove;
    case TupleLifecycle::kDestroy:
      return RuntimeOp::kDestroy;
    case TupleLifecycle::kAssign:
      return RuntimeOp::kAssign;
  }
  throw InternalError("llvm codegen: unknown tuple lifecycle step");
}

CodeGenTuples::CodeGenTuples(
    CodeGenModule& module, CodeGenTypes& types,
    const lir::CompilationUnit& unit)
    : owner_(&module), types_(&types), unit_(&unit) {
}

auto CodeGenTuples::KeyOf(lir::TypeId tuple) -> const std::string& {
  if (const auto found = keys_.find(tuple); found != keys_.end()) {
    return found->second;
  }
  // A struct is its declaration, which names it from any module; a tuple is
  // its components. The declaration is spelled as a symbol's parts are, each
  // stating its own extent, so no two declarations share a key.
  if (const std::optional<lir::TypeDeclarationRef> declared =
          DeclarationOf(tuple)) {
    return keys_
        .emplace(
            tuple,
            std::format(
                "{{{}{}}}", lir::SymbolPart::Name(declared->unit_name).encoded,
                lir::SymbolPartOf(declared->path).encoded))
        .first->second;
  }
  std::string key = "{";
  const TupleLayout& layout = types_->LayoutOfTuple(tuple);
  for (std::size_t i = 0; i < layout.components.size(); ++i) {
    if (i != 0) {
      key += ",";
    }
    const lir::TypeId component = layout.components[i];
    if (unit_->types.Get(component).IsProduct()) {
      key += KeyOf(component);
      continue;
    }
    // An integral component's width, signedness and states are its type, so
    // two tuples differing in any of them are two types.
    if (const std::optional<value::IntegralShape> shape =
            types_->IntegralShapeOf(component)) {
      key += std::format(
          "{}{}{}", shape->width,
          shape->signedness == value::Signedness::kSigned ? "s" : "u",
          shape->IsFourState() ? "4" : "2");
      continue;
    }
    const std::optional<ValueDomain> domain = ValueDomainOf(*unit_, component);
    if (!domain) {
      throw InternalError(
          "llvm codegen: a tuple component is no value of a runtime domain");
    }
    key += support::ValueDomainName(*domain);
  }
  key += "}";
  return keys_.emplace(tuple, std::move(key)).first->second;
}

auto CodeGenTuples::Function(lir::TypeId tuple, TupleLifecycle step)
    -> llvm::Function* {
  const std::string& key = KeyOf(tuple);
  if (const auto found = functions_.find({key, step});
      found != functions_.end()) {
    return found->second;
  }
  llvm::Module& module = owner_->Module();
  llvm::Type* ptr = types_->Ptr();
  llvm::Type* void_ty = llvm::Type::getVoidTy(module.getContext());
  llvm::FunctionType* type =
      step == TupleLifecycle::kDestroy
          ? llvm::FunctionType::get(void_ty, {ptr}, false)
          : llvm::FunctionType::get(void_ty, {ptr, ptr}, false);
  llvm::Function* fn = llvm::Function::Create(
      type, llvm::GlobalValue::InternalLinkage,
      std::format("lyra.tuple{}.{}", key, StepName(step)), module);
  functions_.emplace(std::pair{key, step}, fn);
  Emit(tuple, step, fn);
  return fn;
}

auto CodeGenTuples::MachineTypeOf(MachineKind kind) const -> llvm::Type* {
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

auto CodeGenTuples::DeclareBody(
    lir::TypeId tuple, std::string_view slot,
    const VirtualSignature& stands_in_for) -> llvm::Function* {
  std::vector<llvm::Type*> params{types_->Ptr()};
  params.reserve(1 + stands_in_for.takes.size());
  for (const MachineKind taken : stands_in_for.takes) {
    params.push_back(MachineTypeOf(taken));
  }
  return llvm::Function::Create(
      llvm::FunctionType::get(
          MachineTypeOf(stands_in_for.returns), params, false),
      llvm::GlobalValue::InternalLinkage,
      std::format("lyra.tuple{}.type.{}", KeyOf(tuple), slot),
      owner_->Module());
}

auto CodeGenTuples::Body(
    lir::TypeId tuple, std::string_view slot,
    const VirtualSignature& stands_in_for, llvm::Function* filling)
    -> llvm::Constant* {
  llvm::Module& module = owner_->Module();
  if (filling == nullptr) {
    return llvm::cast<llvm::Constant>(
        module
            .getOrInsertFunction(
                kNoBody, llvm::FunctionType::get(types_->Void(), false))
            .getCallee());
  }
  // A body is entered with the type it belongs to ahead of the values it is
  // asked about, which it has no use for: everything the type is lies in the
  // function it hands them to.
  llvm::Function* body = DeclareBody(tuple, slot, stands_in_for);
  llvm::IRBuilder<> b(llvm::BasicBlock::Create(module.getContext(), "", body));
  std::vector<llvm::Value*> args;
  args.reserve(stands_in_for.takes.size());
  for (std::size_t i = 1; i < body->arg_size(); ++i) {
    args.push_back(body->getArg(static_cast<unsigned>(i)));
  }
  llvm::Value* answered = b.CreateCall(filling, args);
  // A function answering in storage it was handed answers with that storage
  // too, which the virtual function it fills the slot of does not.
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

// What an opened thunk holds for the slot filling it: the body, what it was
// handed after the type, storage for a value of the integral type the method
// states, and that type with its shape.
struct CodeGenTuples::ThunkBody {
  llvm::Function* function;
  std::vector<llvm::Value*> handed;
  llvm::Value* laid_out;
  lir::TypeId stated;
  value::IntegralShape shape;
};

auto CodeGenTuples::OpenThunkBody(
    lir::TypeId tuple, std::string_view slot,
    const VirtualSignature& stands_in_for, lir::TypeId stated) -> ThunkBody {
  const std::optional<value::IntegralShape> shape =
      types_->IntegralShapeOf(stated);
  if (!shape.has_value()) {
    throw InternalError(
        "llvm codegen: a structure's operation takes or answers no integral "
        "value where its type's table states one -- please report this as a "
        "bug");
  }
  llvm::Function* body = DeclareBody(tuple, slot, stands_in_for);
  llvm::IRBuilder<> b(
      llvm::BasicBlock::Create(owner_->Module().getContext(), "", body));
  const support::ObjectLayout storage = types_->StorageOf(stated);
  llvm::AllocaInst* laid_out =
      b.CreateAlloca(llvm::ArrayType::get(b.getInt8Ty(), storage.size));
  laid_out->setAlignment(llvm::Align(storage.align));
  std::vector<llvm::Value*> args;
  args.reserve(stands_in_for.takes.size());
  for (std::size_t i = 1; i < body->arg_size(); ++i) {
    args.push_back(body->getArg(static_cast<unsigned>(i)));
  }
  return ThunkBody{
      .function = body,
      .handed = std::move(args),
      .laid_out = laid_out,
      .stated = stated,
      .shape = *shape};
}

auto CodeGenTuples::ReadOutOfPlanes(
    llvm::IRBuilderBase& b, llvm::Value* planes, llvm::Value* planes_width,
    llvm::Value* taken, llvm::Value* laid_out, lir::TypeId read)
    -> llvm::Value* {
  const std::array<StatedOperand, 3> stated{
      MachineOperand{.type = types_->Ptr()},
      MachineOperand{.type = b.getInt64Ty()},
      MachineOperand{.type = b.getInt64Ty()}};
  const std::array<llvm::Value*, 3> operands{planes, planes_width, taken};
  return CallStreamEntry(
      *owner_, b, RuntimeOp::kStreamRead,
      FnAbiOf(
          *owner_, RuntimeOp::kStreamRead, stated, read,
          ReturnIndirect{.returned = b.getInt64Ty()}),
      operands, laid_out);
}

auto CodeGenTuples::WriteIntoStream(
    llvm::IRBuilderBase& b, llvm::Value* value, lir::TypeId written,
    llvm::Value* stream, llvm::Value* stream_width, llvm::Value* filled)
    -> llvm::Value* {
  const std::array<StatedOperand, 4> stated{
      ValueOperand{.type = written}, MachineOperand{.type = types_->Ptr()},
      MachineOperand{.type = b.getInt64Ty()},
      MachineOperand{.type = b.getInt64Ty()}};
  const std::array<llvm::Value*, 4> operands{
      value, stream, stream_width, filled};
  return CallStreamEntry(
      *owner_, b, RuntimeOp::kStreamWrite,
      FnAbiOf(
          *owner_, RuntimeOp::kStreamWrite, stated, std::nullopt,
          ReturnDirect{.type = b.getInt64Ty()}),
      operands, std::nullopt);
}

// The function answers a comparison as a value of the one-bit type its own
// code states, laid out in storage it is handed last, and the body answers the
// scalar that value holds.
auto CodeGenTuples::ScalarAnswerBody(
    lir::TypeId tuple, std::string_view slot,
    const VirtualSignature& stands_in_for, lir::FunctionId method,
    ComparisonAnswered answered) -> llvm::Constant* {
  const ThunkBody thunk = OpenThunkBody(
      tuple, slot, stands_in_for, unit_->functions.Get(method).result_type);
  llvm::IRBuilder<> b(&thunk.function->getEntryBlock());
  std::vector<llvm::Value*> args = thunk.handed;
  args.push_back(thunk.laid_out);
  b.CreateCall(owner_->UnitFunction(method), args);
  llvm::Value* const scalar =
      LoadComparisonAnswer(b, thunk.laid_out, thunk.shape);
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

// The function answers its stream as a value of the type its own code states,
// which is then written into the stream the type was handed.
auto CodeGenTuples::StreamWriteBody(
    lir::TypeId tuple, std::string_view slot,
    const VirtualSignature& stands_in_for, lir::FunctionId method)
    -> llvm::Constant* {
  const ThunkBody thunk = OpenThunkBody(
      tuple, slot, stands_in_for, unit_->functions.Get(method).result_type);
  llvm::IRBuilder<> b(&thunk.function->getEntryBlock());
  b.CreateCall(owner_->UnitFunction(method), {thunk.handed[0], thunk.laid_out});
  b.CreateRet(WriteIntoStream(
      b, thunk.laid_out, thunk.stated, thunk.handed[1], thunk.handed[2],
      thunk.handed[3]));
  return thunk.function;
}

// The function takes its stream as a value of the type its own code states,
// which the planes the type was handed are read into first.
auto CodeGenTuples::StreamReadBody(
    lir::TypeId tuple, std::string_view slot,
    const VirtualSignature& stands_in_for, lir::FunctionId method)
    -> llvm::Constant* {
  const lir::Function& fn = unit_->functions.Get(method);
  const ThunkBody thunk = OpenThunkBody(
      tuple, slot, stands_in_for, fn.values.Get(fn.params.at(0)).type);
  llvm::IRBuilder<> b(&thunk.function->getEntryBlock());
  llvm::Value* const after = ReadOutOfPlanes(
      b, thunk.handed[0], thunk.handed[1], thunk.handed[2], thunk.laid_out,
      thunk.stated);
  b.CreateCall(
      owner_->UnitFunction(method),
      {thunk.laid_out, thunk.handed[3], thunk.handed[4]});
  b.CreateRet(after);
  return thunk.function;
}

// The function takes a count's control bits as a value of the type its own
// code states, which the planes the type was handed are read into first.
auto CodeGenTuples::ControlReadBody(
    lir::TypeId tuple, std::string_view slot,
    const VirtualSignature& stands_in_for, lir::FunctionId method)
    -> llvm::Constant* {
  const lir::Function& fn = unit_->functions.Get(method);
  const ThunkBody thunk = OpenThunkBody(
      tuple, slot, stands_in_for, fn.values.Get(fn.params.at(1)).type);
  llvm::IRBuilder<> b(&thunk.function->getEntryBlock());
  ReadOutOfPlanes(
      b, thunk.handed[1], thunk.handed[2], b.getInt64(0), thunk.laid_out,
      thunk.stated);
  b.CreateCall(
      owner_->UnitFunction(method),
      {thunk.handed[0], thunk.laid_out, thunk.handed[3]});
  b.CreateRetVoid();
  return thunk.function;
}

auto CodeGenTuples::NullBody(
    lir::TypeId tuple, std::string_view slot,
    const VirtualSignature& stands_in_for) -> llvm::Constant* {
  llvm::Function* body = DeclareBody(tuple, slot, stands_in_for);
  llvm::IRBuilder<> b(
      llvm::BasicBlock::Create(owner_->Module().getContext(), "", body));
  b.CreateRet(llvm::Constant::getNullValue(body->getReturnType()));
  return body;
}

auto CodeGenTuples::TypeOf(lir::TypeId tuple) -> llvm::GlobalVariable* {
  const std::string& key = KeyOf(tuple);
  if (const auto found = types_of_.find(key); found != types_of_.end()) {
    return found->second;
  }
  llvm::Module& module = owner_->Module();
  llvm::LLVMContext& ctx = module.getContext();
  auto* ptr = types_->Ptr();
  llvm::Type* word = llvm::Type::getInt32Ty(ctx);
  llvm::StructType* type_ty =
      llvm::StructType::get(ctx, {ptr, word, word, word, ptr});
  const auto global = [&](std::string name, llvm::Type* type) {
    auto* made =
        llvm::cast<llvm::GlobalVariable>(module.getOrInsertGlobal(name, type));
    made->setLinkage(llvm::GlobalValue::InternalLinkage);
    made->setConstant(true);
    return made;
  };
  llvm::GlobalVariable* type =
      global(std::format("lyra.tuple{}", key), type_ty);
  type->setAlignment(llvm::Align(alignof(value::TupleType)));
  // Named before it is filled: filling it emits the lifecycle, and each step
  // that builds a tuple writes this type's address into it.
  types_of_.emplace(key, type);

  // A struct has one type in the program, the declaring unit's, since that
  // unit is the one stating its methods; another unit refers to it and fills
  // nothing. A tuple has no methods, and each module keeps its own type of
  // one.
  std::span<const lir::StructMethod> methods;
  if (const auto* structure = unit_->types.Get(tuple).As<lir::StructType>()) {
    type->setLinkage(llvm::GlobalValue::ExternalLinkage);
    const auto* own = std::get_if<lir::StructId>(&structure->declaration);
    if (own == nullptr) {
      return type;
    }
    methods = unit_->structs.Get(*own).methods;
  }

  const TupleLayout& layout = types_->LayoutOfTuple(tuple);
  llvm::StructType* component_ty = llvm::StructType::get(ctx, {word, ptr});
  std::vector<llvm::Constant*> components;
  components.reserve(layout.components.size());
  for (std::size_t i = 0; i < layout.components.size(); ++i) {
    auto component_type = owner_->ValueTypeOf(layout.components[i]);
    if (!component_type) {
      throw InternalError(
          "llvm codegen: a tuple component is no value the runtime realizes");
    }
    components.push_back(
        llvm::ConstantStruct::get(
            component_ty, {llvm::ConstantInt::get(word, layout.offsets[i]),
                           *component_type}));
  }
  auto* components_ty =
      llvm::ArrayType::get(component_ty, layout.components.size());
  llvm::GlobalVariable* listed =
      global(std::format("lyra.tuple{}.components", key), components_ty);
  listed->setInitializer(llvm::ConstantArray::get(components_ty, components));

  // The table a C++ class with these virtual functions has (C++ ABI 2.5.2):
  // the offset back to the value's start, which is none, and the class's
  // description, which nothing reads since no cast is ever made to or from a
  // type; then the bodies, whose first the type holds the address of.
  auto* i64 = llvm::Type::getInt64Ty(ctx);
  const std::vector<TableSlot> slots = TableSlots();
  constexpr std::size_t kAddressPoint = 2;
  std::vector<llvm::Constant*> entries(
      kAddressPoint + kDestructorEntries + slots.size(), nullptr);
  entries[0] =
      llvm::ConstantExpr::getIntToPtr(llvm::ConstantInt::get(i64, 0), ptr);
  entries[1] = llvm::ConstantPointerNull::get(ptr);
  for (std::size_t i = 0; i < kDestructorEntries; ++i) {
    entries[kAddressPoint + i] = Body(
        tuple, "destructor",
        VirtualSignature{.returns = MachineKind::kVoid, .takes = {}}, nullptr);
  }
  for (const TableSlot& slot : slots) {
    const std::size_t at = kAddressPoint + slot.entry;
    if (at >= entries.size() || entries[at] != nullptr) {
      throw InternalError(
          std::format(
              "llvm codegen: the tuple table's `{}` entry falls outside the "
              "operations listed for it, or on another's",
              slot.name));
    }
    const auto filled_by = [&](llvm::Function* filling) -> llvm::Constant* {
      return Body(tuple, slot.name, slot.stands_in_for, filling);
    };
    entries[at] = std::visit(
        Overloaded{
            [&](TupleLifecycle step) -> llvm::Constant* {
              return filled_by(Function(tuple, step));
            },
            [&](const support::ValueOperation& operation) -> llvm::Constant* {
              const std::optional<lir::FunctionId> method =
                  MethodAnswering(methods, operation);
              if (!method.has_value()) {
                return filled_by(nullptr);
              }
              return std::visit(
                  Overloaded{
                      [&](TakenAsStated) -> llvm::Constant* {
                        return filled_by(owner_->UnitFunction(*method));
                      },
                      [&](AnswerScalar) -> llvm::Constant* {
                        return ScalarAnswerBody(
                            tuple, slot.name, slot.stands_in_for, *method,
                            ComparisonAnswered::kAsItsScalar);
                      },
                      [&](AnswerWhetherItHolds) -> llvm::Constant* {
                        return ScalarAnswerBody(
                            tuple, slot.name, slot.stands_in_for, *method,
                            ComparisonAnswered::kAsWhetherItHolds);
                      },
                      [&](AnswerIntoStream) -> llvm::Constant* {
                        return StreamWriteBody(
                            tuple, slot.name, slot.stands_in_for, *method);
                      },
                      [&](StreamOutOfPlanes) -> llvm::Constant* {
                        return StreamReadBody(
                            tuple, slot.name, slot.stands_in_for, *method);
                      },
                      [&](ControlOutOfPlanes) -> llvm::Constant* {
                        return ControlReadBody(
                            tuple, slot.name, slot.stands_in_for, *method);
                      }},
                  slot.crossing);
            },
            [&](NeverAskedOfAStructure) -> llvm::Constant* {
              return filled_by(nullptr);
            },
            [&](AnsweredWithNothing) -> llvm::Constant* {
              return NullBody(tuple, slot.name, slot.stands_in_for);
            }},
        slot.filled_by);
  }
  auto* table_ty = llvm::ArrayType::get(ptr, entries.size());
  llvm::GlobalVariable* table =
      global(std::format("lyra.tuple{}.table", key), table_ty);
  table->setInitializer(llvm::ConstantArray::get(table_ty, entries));

  type->setInitializer(
      llvm::ConstantStruct::get(
          type_ty,
          {llvm::ConstantExpr::getInBoundsGetElementPtr(
               ptr, table, llvm::ConstantInt::get(i64, kAddressPoint)),
           llvm::ConstantInt::get(word, layout.storage.size),
           llvm::ConstantInt::get(word, layout.storage.align),
           llvm::ConstantInt::get(word, layout.components.size()), listed}));
  return type;
}

auto CodeGenTuples::EmitDeclared() -> void {
  for (const lir::StructId id : unit_->structs.Ids()) {
    TypeOf(unit_->types.Intern(lir::Type{lir::StructType{.declaration = id}}));
  }
}

auto CodeGenTuples::DeclarationOf(lir::TypeId tuple) const
    -> std::optional<lir::TypeDeclarationRef> {
  const auto* structure = unit_->types.Get(tuple).As<lir::StructType>();
  if (structure == nullptr) {
    return std::nullopt;
  }
  return lir::StructDeclarationOf(*unit_, *structure);
}

auto CodeGenTuples::Emit(
    lir::TypeId tuple, TupleLifecycle step, llvm::Function* fn) -> void {
  llvm::LLVMContext& ctx = owner_->Module().getContext();
  llvm::IRBuilder<> b(llvm::BasicBlock::Create(ctx, "", fn));
  const TupleLayout& layout = types_->LayoutOfTuple(tuple);

  // Where component `i` lies in the tuple at `base`.
  const auto at = [&](llvm::Value* base, std::size_t i) -> llvm::Value* {
    return b.CreateConstInBoundsGEP1_64(b.getInt8Ty(), base, layout.offsets[i]);
  };
  // Component `i`'s own step: the nested type's own function where the
  // component is a tuple; the copy of its bytes where it is an integral value,
  // which is the whole of each step that has anything to do; and its domain's
  // entry otherwise, which answers with the storage it built in where the
  // tuple's own step answers nothing.
  const auto component = [&](std::size_t i, std::vector<llvm::Value*> args) {
    const lir::TypeId type = layout.components[i];
    if (unit_->types.Get(type).IsProduct()) {
      b.CreateCall(Function(type, step), args);
      return;
    }
    if (types_->IntegralShapeOf(type).has_value()) {
      const support::ObjectLayout bytes = types_->StorageOf(type);
      const llvm::Align align(bytes.align);
      switch (step) {
        case TupleLifecycle::kCopy:
        case TupleLifecycle::kMove:
          b.CreateMemCpy(args.at(1), align, args.at(0), align, bytes.size);
          return;
        case TupleLifecycle::kAssign:
          b.CreateMemCpy(args.at(0), align, args.at(1), align, bytes.size);
          return;
        case TupleLifecycle::kDestroy:
          return;
      }
      throw InternalError("llvm codegen: unknown tuple lifecycle step");
    }
    const ValueDomain domain = *ValueDomainOf(*unit_, type);
    if (step == TupleLifecycle::kDestroy &&
        runtime::LayoutOf(domain).ends_with_nothing_to_do) {
      return;
    }
    b.CreateCall(
        owner_->LifecycleEntry(RuntimeSymbol(domain, LifecycleOp(step)), step),
        args);
  };

  const std::size_t count = layout.components.size();
  switch (step) {
    case TupleLifecycle::kCopy:
    case TupleLifecycle::kMove:
      b.CreateStore(TypeOf(tuple), fn->getArg(1));
      for (std::size_t i = 0; i < count; ++i) {
        component(i, {at(fn->getArg(0), i), at(fn->getArg(1), i)});
      }
      break;
    case TupleLifecycle::kDestroy:
      for (std::size_t i = 0; i < count; ++i) {
        component(i, {at(fn->getArg(0), i)});
      }
      break;
    case TupleLifecycle::kAssign:
      for (std::size_t i = 0; i < count; ++i) {
        component(i, {at(fn->getArg(0), i), at(fn->getArg(1), i)});
      }
      break;
  }
  b.CreateRetVoid();
}

}  // namespace lyra::backend::llvm_backend
