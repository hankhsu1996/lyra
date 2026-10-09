#include <array>
#include <cstddef>
#include <cstdint>
#include <format>
#include <iterator>
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
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Type.h>

#include "lyra/backend/llvm/codegen_function.hpp"
#include "lyra/backend/llvm/codegen_module.hpp"
#include "lyra/backend/llvm/runtime_entry.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/integral_constant.hpp"
#include "lyra/lir/place_query.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/runtime/object_layout.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/member_storage_kind.hpp"
#include "lyra/support/runtime_object.hpp"

namespace lyra::backend::llvm_backend {

namespace {

auto Unsupported(std::string message) -> std::unexpected<diag::Diagnostic> {
  return diag::Fail(
      diag::DiagCode::kUnsupportedExpressionForm, std::move(message));
}

// The declaration a member step names as the one that gave the member.
auto DeclarationOf(const lir::CompilationUnit& unit, lir::TypeId declared_by)
    -> lir::TypeDeclaration {
  std::optional<lir::TypeDeclaration> declaration =
      unit.types.Get(declared_by).Declaration();
  if (!declaration.has_value()) {
    throw InternalError(
        "llvm codegen: a member step names a type that declares nothing");
  }
  return *std::move(declaration);
}

// The signature a call is made under: the result the instruction defines, over
// the values actually crossing. A callee named by symbol and one reached
// through an address are the same call beneath it, differing only in where the
// address comes from.
auto CallSignature(llvm::Type* result, std::span<llvm::Value* const> args)
    -> llvm::FunctionType* {
  std::vector<llvm::Type*> params;
  params.reserve(args.size());
  for (llvm::Value* arg : args) {
    params.push_back(arg->getType());
  }
  return llvm::FunctionType::get(result, params, false);
}

// Which capability wrapper a type is, and the value that wrapper represents;
// nothing for a type that represents no storage. Classifying a wrapper in one
// place is what keeps an access reached through a place and an operation
// reached through an operand from disagreeing about what a wrapper is.
auto WrapperOf(const lir::Type& type)
    -> std::optional<std::pair<WrapperKind, lir::TypeId>> {
  if (const auto* observable = type.As<lir::ObservableType>()) {
    return std::pair{WrapperKind::kCell, observable->value};
  }
  if (const auto* net = type.As<lir::ResolvedType>()) {
    return std::pair{WrapperKind::kNet, net->value};
  }
  if (const auto* driver = type.As<lir::DriverType>()) {
    return std::pair{WrapperKind::kDriver, driver->value};
  }
  // A reference is a wrapper rather than a way of reaching one: it is an
  // address whose storage may be subscribable or not, and only the operation
  // performed through it can tell which (LRM 13.5.2). So it is classified
  // where it stands, before anything strips it to look at what it points at.
  if (const auto* reference = type.As<lir::RefType>()) {
    return std::pair{WrapperKind::kRef, reference->pointee};
  }
  return std::nullopt;
}

// The values a storage holds, where they are all of one representation;
// nothing for a type that is not such storage. A capability wrapper holds the
// value it represents, a history holds what each tick of one clocking event
// settled for one expression, and a place designated within a write reaches
// the value it designates -- one representation each way, which is what lets
// an entry reaching the storage be named once per representation rather than
// per call.
auto ValuesHeldBy(const lir::Type& type) -> std::optional<lir::TypeId> {
  if (const std::optional<std::pair<WrapperKind, lir::TypeId>> wrapper =
          WrapperOf(type)) {
    return wrapper->second;
  }
  if (const auto* history = type.As<lir::SampledHistoryType>()) {
    return history->value;
  }
  if (const auto* designation = type.As<lir::DesignationType>()) {
    return designation->value;
  }
  return std::nullopt;
}

// What a dynamic cast is told of where the wanted class sits relative to the
// part it starts from (C++ ABI 2.9.7 `src2dst_offset`): nothing, so the cast
// works it out from the descriptions.
constexpr std::int64_t kNoCastHint = -1;

}  // namespace

auto CodeGenFunction::LowerInstr(const lir::Instr& instr)
    -> diag::Result<llvm::Value*> {
  const lir::TypeId result_type = fn_->values.Get(instr.result).type;
  llvm::Value* const out =
      lir::MakesValue(instr.data) ? StorageFor(result_type) : nullptr;
  return std::visit(
      Overloaded{
          [&](const lir::CallInstr& call) -> diag::Result<llvm::Value*> {
            return LowerCall(call, result_type, out);
          },
          [&](const lir::ReceiveDepartureInstr&) -> diag::Result<llvm::Value*> {
            return LowerReceiveDeparture();
          },
          [&](const lir::TupleInstr& tuple) -> diag::Result<llvm::Value*> {
            return LowerTuple(tuple, result_type, out);
          },
          [&](const lir::ClosureInstr& built) -> diag::Result<llvm::Value*> {
            return LowerClosure(built, result_type, out);
          },
          [&](const lir::ArrayInstr& array) -> diag::Result<llvm::Value*> {
            return LowerArray(array, result_type);
          },
          [&](const lir::AggregateExtractInstr& extract)
              -> diag::Result<llvm::Value*> {
            return LowerAggregateExtract(extract, out);
          },
          [&](const lir::AggregateUpdateInstr& update)
              -> diag::Result<llvm::Value*> {
            return LowerAggregateUpdate(update, out);
          },
          [&](const lir::TagTestInstr& test) -> diag::Result<llvm::Value*> {
            return LowerTagTest(test, result_type);
          },
          [&](const lir::LoadInstr& load) -> diag::Result<llvm::Value*> {
            return LowerLoad(load, result_type);
          },
          [&](const lir::StoreInstr& store) -> diag::Result<llvm::Value*> {
            return LowerStore(store);
          },
          [&](const lir::AddrOfInstr& addr) -> diag::Result<llvm::Value*> {
            return LowerAddrOf(addr, result_type, out);
          },
          [&](const lir::BinaryInstr& binary) -> diag::Result<llvm::Value*> {
            return LowerBinary(binary, out);
          },
          [&](const lir::UnaryInstr& unary) -> diag::Result<llvm::Value*> {
            return LowerUnary(unary, out);
          },
          [&](const lir::CastInstr& cast) -> diag::Result<llvm::Value*> {
            return LowerCast(cast, result_type);
          },
          [&](const lir::HandleCastInstr& cast) -> diag::Result<llvm::Value*> {
            return LowerHandleCast(cast, result_type, out);
          },
          [&](const lir::DynamicCastInstr& cast) -> diag::Result<llvm::Value*> {
            return LowerDynamicCast(cast, result_type, out);
          },
          [&](const lir::OpenVariablesInstr&) -> diag::Result<llvm::Value*> {
            return LowerOpenVariables();
          },
          [&](const lir::VariableAddressInstr& reached)
              -> diag::Result<llvm::Value*> {
            return LowerVariableAddress(reached);
          },
          [&](const lir::CloseVariablesInstr& closed)
              -> diag::Result<llvm::Value*> {
            return LowerCloseVariables(closed);
          }},
      instr.data);
}

// Reading a place answers with the value it holds, where it lies, whatever
// storage that is: a cell's contents are asked of the cell, and anything else
// is its own address. A value of a type held as itself is read out of that
// address.
auto CodeGenFunction::LowerLoad(
    const lir::LoadInstr& load, lir::TypeId result_type)
    -> diag::Result<llvm::Value*> {
  llvm::Type* const result = module_->Types().Map(result_type);
  auto wrapper = WrapperPlaceOf(load.place);
  if (!wrapper) {
    return std::unexpected(std::move(wrapper.error()));
  }
  if (wrapper->has_value()) {
    const WrapperPlace& through = **wrapper;
    auto address = ResolvePlaceAddress(through.wrapper, Access::kRead);
    if (!address) {
      return std::unexpected(std::move(address.error()));
    }
    return ContentsOf(through.domain, through.kind, *address);
  }
  auto address = ResolvePlaceAddress(load.place, Access::kRead);
  if (!address) {
    return std::unexpected(std::move(address.error()));
  }
  if (const std::optional<support::ValueDomain> cell =
          PlaceValueCellDomain(load.place, result_type)) {
    return ValueCellContents(*cell, *address);
  }
  if (module_->Unit().types.Get(result_type).HeldObject().has_value()) {
    return *address;
  }
  return builder_.CreateLoad(result, *address);
}

auto CodeGenFunction::ContentsOf(
    support::ValueDomain domain, WrapperKind kind, llvm::Value* wrapper)
    -> llvm::Value* {
  const std::array<llvm::Value*, 1> args{wrapper};
  return builder_.CreateCall(
      Entry(
          RuntimeSymbol(domain, kind, support::BuiltinFn::kLoad),
          module_->Types().Ptr(), args),
      args);
}

auto CodeGenFunction::ValueCellContents(
    support::ValueDomain domain, llvm::Value* cell) -> llvm::Value* {
  const std::array<llvm::Value*, 1> args{cell};
  return builder_.CreateCall(
      Entry(
          RuntimeSymbol(domain, lir::ValueCellTarget::Op::kLoad),
          module_->Types().Ptr(), args),
      args);
}

// Writing a place mirrors reading one, and a write through a wrapper is what
// gives the write its meaning -- waking whoever subscribed to a cell, or moving
// one driver's contribution and re-resolving the net it feeds -- which is why
// it is the wrapper that performs the write rather than a store to an address.
auto CodeGenFunction::LowerStore(const lir::StoreInstr& store)
    -> diag::Result<llvm::Value*> {
  auto wrapper = WrapperPlaceOf(store.place);
  if (!wrapper) {
    return std::unexpected(std::move(wrapper.error()));
  }
  if (!wrapper->has_value()) {
    auto value = LowerOperand(store.value);
    if (!value) {
      return std::unexpected(std::move(value.error()));
    }
    auto address = ResolvePlaceAddress(store.place, Access::kWrite);
    if (!address) {
      return std::unexpected(std::move(address.error()));
    }
    const lir::TypeId stored = OperandType(store.value);
    if (const std::optional<support::ValueDomain> cell =
            PlaceValueCellDomain(store.place, stored)) {
      const std::array<llvm::Value*, 2> args{*address, *value};
      return builder_.CreateCall(
          Entry(
              RuntimeSymbol(*cell, lir::ValueCellTarget::Op::kStore),
              module_->Types().Void(), args),
          args);
    }
    if (module_->Unit().types.Get(stored).IsOwnedValue()) {
      // A frame slot of an owned type holds the value, and a store into one
      // hands it the value along with its end, so the value moves in and
      // nothing is left behind where it was built.
      if (lir::IsPlaceLocal(*fn_, store.place.base) &&
          store.place.chain.empty()) {
        RelocateValue(stored, *value, *address);
        return nullptr;
      }
      // Anything else a chain reaches is a value that is already there, and a
      // store writes into it: whatever names that storage goes on naming it.
      AssignValue(stored, *address, *value);
      return nullptr;
    }
    return builder_.CreateStore(*value, *address);
  }
  const WrapperPlace& through = **wrapper;
  auto address = ResolvePlaceAddress(through.wrapper, Access::kWrite);
  if (!address) {
    return std::unexpected(std::move(address.error()));
  }
  auto value = LowerOperand(store.value);
  if (!value) {
    return std::unexpected(std::move(value.error()));
  }
  const std::array<llvm::Value*, 2> args{*address, *value};
  return builder_.CreateCall(
      Entry(
          RuntimeSymbol(
              through.domain, through.kind, support::BuiltinFn::kStore),
          module_->Types().Void(), args),
      args);
}

// A place resolves to an address. A place local's storage is its frame slot;
// any other base is a reference value, whose referent the opening dereference
// names. Each further dereference reads the reference held in the storage
// reached so far -- or, where that storage is a wrapper, reaches its contents
// for a read -- a member step reaches the storage where the declaration
// declaring that member placed it, a value cell a later step continues into is
// opened through its own access, an element step asks the value reached so far
// for the one it names, the way the access needs it, and a component step lies
// at an offset in the product.
auto CodeGenFunction::ResolvePlaceAddress(
    const lir::Place& place, Access access) -> diag::Result<llvm::Value*> {
  auto step = place.chain.begin();
  llvm::Value* address = nullptr;

  const auto* use = std::get_if<lir::Use>(&place.base);
  if (lir::IsPlaceLocal(*fn_, place.base)) {
    address = values_.at(use->value);
  } else {
    // A value base refers to storage, so opening it is what the base value
    // already answers; the dereference is the step that says the storage is
    // what the place names. A prefix that stops before that step -- the wrapper
    // half of an access whose wrapper is itself a value, as a net driver's
    // handle is -- has no dereference left to consume and names that same
    // storage.
    if (step != place.chain.end() &&
        !std::holds_alternative<lir::DerefProjection>(*step)) {
      throw InternalError(
          "llvm codegen: a place over a value base must open with a "
          "dereference");
    }
    auto base = LowerOperand(place.base);
    if (!base) {
      return std::unexpected(std::move(base.error()));
    }
    address = OpenedReferent(*base, OperandType(place.base));
    if (step != place.chain.end()) {
      ++step;
    }
  }

  for (; step != place.chain.end(); ++step) {
    // What the chain holds where this step applies, which decides how a
    // dereference reaches what it names: crossing a class handle is an
    // operation rather than a load.
    const lir::TypeId reached =
        ReachedType(place, std::distance(place.chain.begin(), step));
    auto reached_storage = std::visit(
        Overloaded{
            [&](const lir::DerefProjection&) -> diag::Result<llvm::Value*> {
              // A wrapper's contents are the wrapper's to answer for. Read,
              // they are where it keeps them; written, they are reached through
              // the write the wrapper opened, which is what reports it.
              if (const std::optional<std::pair<WrapperKind, lir::TypeId>>
                      wrapper = WrapperOf(module_->Unit().types.Get(reached))) {
                switch (access) {
                  case Access::kRead:
                    break;
                  case Access::kWrite:
                    throw InternalError(
                        "llvm codegen: a write into what a wrapper holds is "
                        "made through the write the wrapper opened -- please "
                        "report this as a bug");
                }
                auto domain = DomainOf(wrapper->second);
                if (!domain) {
                  return std::unexpected(std::move(domain.error()));
                }
                return ContentsOf(*domain, wrapper->first, address);
              }
              return OpenedReferent(
                  builder_.CreateLoad(module_->Types().Ptr(), address),
                  reached);
            },
            [&](const lir::MemberProjection& projection)
                -> diag::Result<llvm::Value*> {
              return MemberStorage(address, projection.member);
            },
            [&](const lir::ElementProjection& element)
                -> diag::Result<llvm::Value*> {
              auto domain = DomainOf(reached);
              if (!domain) {
                return std::unexpected(std::move(domain.error()));
              }
              std::vector<llvm::Value*> args{address};
              auto filled = SelectorArgs(reached, element.coordinates, args);
              if (!filled) {
                return std::unexpected(std::move(filled.error()));
              }
              return builder_.CreateCall(
                  Entry(
                      RuntimeSymbol(
                          *domain, StepEntry(
                                       access, support::BuiltinFn::kElement,
                                       support::BuiltinFn::kElementRef)),
                      module_->Types().Ptr(), args),
                  args);
            },
            // A component step is taken only over a product, and a product's
            // type lays it out, so its component lies at an offset from it,
            // whoever holds the product.
            [&](const lir::ComponentProjection& component)
                -> diag::Result<llvm::Value*> {
              return ComponentAddress(reached, address, component.index.value);
            }},
        *step);
    if (!reached_storage) {
      return std::unexpected(std::move(reached_storage.error()));
    }
    address = *reached_storage;
    // A value cell is not where the value it holds lies for every domain -- a
    // tuple's cell holds a handle on bytes kept elsewhere -- so a step into the
    // value continues from what the cell's own access hands out.
    const std::ptrdiff_t reached_by =
        std::distance(place.chain.begin(), step) + 1;
    if (reached_by == std::ssize(place.chain)) {
      continue;
    }
    const lir::Place cell_prefix{
        .base = place.base,
        .chain = {place.chain.begin(), place.chain.begin() + reached_by}};
    if (const std::optional<support::ValueDomain> cell =
            PlaceValueCellDomain(cell_prefix, ReachedType(place, reached_by))) {
      address = ValueCellContents(*cell, address);
    }
  }
  return address;
}

auto CodeGenFunction::StepEntry(
    Access access, support::BuiltinFn read, support::BuiltinFn write)
    -> support::BuiltinFn {
  switch (access) {
    case Access::kRead:
      return read;
    case Access::kWrite:
      return write;
  }
  throw InternalError("llvm codegen: unknown access");
}

// A member sits where the declaration declaring it placed it, which is the
// same in every value of a declaration extending that one.
auto CodeGenFunction::MemberStorage(
    llvm::Value* owner, const lir::StatedMemberRef& member)
    -> diag::Result<llvm::Value*> {
  auto record = module_->RecordOf(member.declared_by);
  if (!record) {
    return std::unexpected(std::move(record.error()));
  }
  return builder_.CreateConstInBoundsGEP1_64(
      builder_.getInt8Ty(), owner, (*record)->offsets.at(member.slot.value));
}

// The type of the storage the chain has arrived at where step `index` applies,
// which is the type of the place that stops just before it.
auto CodeGenFunction::ReachedType(
    const lir::Place& place, std::ptrdiff_t index) const -> lir::TypeId {
  const lir::Place prefix{
      .base = place.base,
      .chain = {place.chain.begin(), place.chain.begin() + index}};
  return prefix.chain.empty() ? OperandType(prefix.base)
                              : lir::PlaceType(module_->Unit(), *fn_, prefix);
}

// The address of what `reference` refers to, given the type it is. Most
// references are the address already and opening one is nothing. Two are not:
// a class handle and a counted hold on a value each carry what they name as a
// fact they hold rather than are, so the runtime answers which storage is
// meant -- and for the handle that is also where one referring to no object is
// caught (LRM 8.3).
auto CodeGenFunction::OpenedReferent(llvm::Value* reference, lir::TypeId type)
    -> llvm::Value* {
  const lir::Type& referring = module_->Unit().types.Get(type);
  std::optional<std::string> opening;
  if (referring.Is<lir::ManagedRefType>()) {
    opening = RuntimeSymbol(support::BuiltinFn::kViewOf);
  } else if (const auto* pointer = referring.As<lir::PointerType>()) {
    switch (pointer->ownership) {
      case lir::PointerOwnership::kUnique:
      case lir::PointerOwnership::kBorrowed:
        break;
      case lir::PointerOwnership::kShared:
        opening = RuntimeSymbol(RuntimeOp::kSharedPointerDeref);
        break;
    }
  }
  if (!opening) {
    return reference;
  }
  const std::array<llvm::Value*, 1> args{reference};
  return builder_.CreateCall(
      Entry(*opening, module_->Types().Ptr(), args), args);
}

// Taking the address of a place. What that address is, is the place's own
// answer; what it becomes depends on what is being built. A reference names
// storage that may belong to a subscribable variable or not, and the body
// holding one is lowered once for every caller, so it cannot ask which it was
// lent (LRM 13.5.2) -- which is why the variable is recorded here, where the
// place it came from still answers. Every other address is the address itself.
// Either way what is addressed is the value where it lies, which a value cell
// hands out through its own access.
auto CodeGenFunction::LowerAddrOf(
    const lir::AddrOfInstr& addr, lir::TypeId result_type, llvm::Value* out)
    -> diag::Result<llvm::Value*> {
  auto address = ResolvePlaceAddress(addr.place, Access::kWrite);
  if (!address) {
    return std::unexpected(std::move(address.error()));
  }
  const lir::TypeId reached_type =
      ReachedType(addr.place, std::ssize(addr.place.chain));
  llvm::Value* storage = *address;
  if (const std::optional<support::ValueDomain> held =
          PlaceValueCellDomain(addr.place, reached_type)) {
    storage = ValueCellContents(*held, storage);
  }
  const lir::TypePool& types = module_->Unit().types;
  if (!types.Get(result_type).Is<lir::RefType>()) {
    return storage;
  }
  if (const auto* cell = types.Get(reached_type).As<lir::ObservableType>()) {
    auto domain = DomainOf(cell->value);
    if (!domain) {
      return std::unexpected(std::move(domain.error()));
    }
    return BuildInto(
        RuntimeSymbol(*domain, RuntimeOp::kCellRefer), {storage}, out);
  }
  return BuildInto(RuntimeSymbol(RuntimeOp::kReferStorage), {storage}, out);
}

auto CodeGenFunction::IsHandleSequence(lir::TypeId type) const -> bool {
  return module_->Unit().types.Get(type).Is<lir::VectorType>();
}

auto CodeGenFunction::LowerBinary(
    const lir::BinaryInstr& binary, llvm::Value* out)
    -> diag::Result<llvm::Value*> {
  const lir::TypeId operand_type = OperandType(binary.lhs);
  // A machine integer is a native value, not a value-domain handle: its
  // operator is a machine instruction, not a runtime-library call. What arrives
  // this way is the words a synthesized transition computes over, which stand
  // for no value of the design at all.
  if (const std::optional<lir::Signedness> signedness =
          module_->Unit().types.Get(operand_type).MachineIntegerSignedness()) {
    return LowerMachineBinary(binary, *signedness);
  }
  // An address compared with another is a machine comparison too, as it is in
  // what clang makes of the same comparison.
  if (module_->Unit().types.Get(operand_type).Is<lir::PointerType>()) {
    return LowerMachineBinary(binary, lir::Signedness::kUnsigned);
  }
  auto domain = DomainOf(operand_type);
  if (!domain) {
    return std::unexpected(std::move(domain.error()));
  }
  auto lhs = LowerOperand(binary.lhs);
  if (!lhs) {
    return std::unexpected(std::move(lhs.error()));
  }
  auto rhs = LowerOperand(binary.rhs);
  if (!rhs) {
    return std::unexpected(std::move(rhs.error()));
  }
  return BuildInto(RuntimeSymbol(*domain, binary.op), {*lhs, *rhs}, out);
}

auto CodeGenFunction::LowerMachineBinary(
    const lir::BinaryInstr& binary, lir::Signedness signedness)
    -> diag::Result<llvm::Value*> {
  auto lhs = LowerOperand(binary.lhs);
  if (!lhs) {
    return std::unexpected(std::move(lhs.error()));
  }
  auto rhs = LowerOperand(binary.rhs);
  if (!rhs) {
    return std::unexpected(std::move(rhs.error()));
  }
  // The operand's own signedness is what division, remainder and the ordering
  // comparisons need. A logical operator over machine values would have to
  // decide whether its second operand runs, which a selection states instead,
  // so none arrives here.
  const bool is_signed = signedness == lir::Signedness::kSigned;
  switch (binary.op) {
    case lir::BinaryOp::kAdd:
      return builder_.CreateAdd(*lhs, *rhs);
    case lir::BinaryOp::kSub:
      return builder_.CreateSub(*lhs, *rhs);
    case lir::BinaryOp::kMul:
      return builder_.CreateMul(*lhs, *rhs);
    case lir::BinaryOp::kDiv:
      return is_signed ? builder_.CreateSDiv(*lhs, *rhs)
                       : builder_.CreateUDiv(*lhs, *rhs);
    case lir::BinaryOp::kMod:
      return is_signed ? builder_.CreateSRem(*lhs, *rhs)
                       : builder_.CreateURem(*lhs, *rhs);
    case lir::BinaryOp::kBitwiseAnd:
      return builder_.CreateAnd(*lhs, *rhs);
    case lir::BinaryOp::kBitwiseOr:
      return builder_.CreateOr(*lhs, *rhs);
    case lir::BinaryOp::kLogicalAnd:
    case lir::BinaryOp::kLogicalOr:
      throw InternalError(
          "llvm codegen: a logical operator over machine values; a condition "
          "search is a selection");
    case lir::BinaryOp::kBitwiseXor:
      return builder_.CreateXor(*lhs, *rhs);
    case lir::BinaryOp::kEquality:
      return builder_.CreateICmpEQ(*lhs, *rhs);
    case lir::BinaryOp::kInequality:
      return builder_.CreateICmpNE(*lhs, *rhs);
    case lir::BinaryOp::kLessThan:
      return is_signed ? builder_.CreateICmpSLT(*lhs, *rhs)
                       : builder_.CreateICmpULT(*lhs, *rhs);
    case lir::BinaryOp::kLessEqual:
      return is_signed ? builder_.CreateICmpSLE(*lhs, *rhs)
                       : builder_.CreateICmpULE(*lhs, *rhs);
    case lir::BinaryOp::kGreaterThan:
      return is_signed ? builder_.CreateICmpSGT(*lhs, *rhs)
                       : builder_.CreateICmpUGT(*lhs, *rhs);
    case lir::BinaryOp::kGreaterEqual:
      return is_signed ? builder_.CreateICmpSGE(*lhs, *rhs)
                       : builder_.CreateICmpUGE(*lhs, *rhs);
  }
  throw InternalError("llvm codegen: unknown binary operator");
}

auto CodeGenFunction::LowerUnary(const lir::UnaryInstr& unary, llvm::Value* out)
    -> diag::Result<llvm::Value*> {
  const lir::TypeId operand_type = OperandType(unary.operand);
  // A machine integer is a native value, not a value-domain handle: its
  // operator is a machine instruction, not a runtime-library call. This is how
  // the reduced predicate a real- or chandle-family `!` produces is negated
  // before `from_bool` widens it back to a 1-bit packed.
  if (module_->Unit()
          .types.Get(operand_type)
          .MachineIntegerSignedness()
          .has_value()) {
    return LowerMachineUnary(unary);
  }
  auto domain = DomainOf(operand_type);
  if (!domain) {
    return std::unexpected(std::move(domain.error()));
  }
  auto operand = LowerOperand(unary.operand);
  if (!operand) {
    return std::unexpected(std::move(operand.error()));
  }
  return BuildInto(RuntimeSymbol(*domain, unary.op), {*operand}, out);
}

auto CodeGenFunction::LowerMachineUnary(const lir::UnaryInstr& unary)
    -> diag::Result<llvm::Value*> {
  auto operand = LowerOperand(unary.operand);
  if (!operand) {
    return std::unexpected(std::move(operand.error()));
  }
  // Signedness decides none of these: negation and complement are one
  // instruction whichever way the sign bit is read.
  llvm::Value* const zero = llvm::ConstantInt::get((*operand)->getType(), 0);
  switch (unary.op) {
    case lir::UnaryOp::kLogicalNot:
      return builder_.CreateICmpEQ(*operand, zero);
    case lir::UnaryOp::kMinus:
      return builder_.CreateSub(zero, *operand);
    case lir::UnaryOp::kBitwiseNot:
      return builder_.CreateNot(*operand);
  }
  throw InternalError("llvm codegen: unknown unary operator");
}

// A cast says only which type a value is read as, so the pair of types is the
// whole of what it states and the machine conversion follows from that pair.
// Two types mapping to one machine type convert by nothing at all; a machine
// boolean is what the value's own domain answers about it; and between two
// machine integers the value resizes, repeating the sign bit only when the
// *source* is signed, the destination's signedness saying how the result is
// later read rather than what the added high bits hold. A pair outside those is
// one this target does not carry, and is refused rather than passed through.
auto CodeGenFunction::LowerCast(
    const lir::CastInstr& cast, lir::TypeId result_type)
    -> diag::Result<llvm::Value*> {
  auto operand = LowerOperand(cast.operand);
  if (!operand) {
    return std::unexpected(std::move(operand.error()));
  }
  const lir::TypeId operand_type = OperandType(cast.operand);
  // A pointer to one part of an object read as a pointer to another is the
  // address of that part, which is the same address except where it is an
  // interface class's.
  const std::optional<lir::TypeId> from = ClassBehind(operand_type);
  const std::optional<lir::TypeId> to = ClassBehind(result_type);
  if (from.has_value() && to.has_value()) {
    return ConvertView(*operand, *from, *to);
  }
  llvm::Type* target = module_->Types().Map(result_type);
  if ((*operand)->getType() == target) {
    return *operand;
  }
  if (module_->Unit().types.Get(result_type).Is<lir::MachineBoolType>()) {
    // A machine integer is a native value rather than a value-domain handle,
    // so reducing one to a predicate is a machine comparison against zero and
    // not a library call. What arrives this way is a runtime entry's plain
    // answer, which stands for no value of the design and so belongs to no
    // domain that could name an entry.
    if (module_->Unit().types.Get(operand_type).MachineIntegerSignedness()) {
      return builder_.CreateICmpNE(
          *operand, llvm::ConstantInt::get((*operand)->getType(), 0));
    }
    auto domain = DomainOf(operand_type);
    if (!domain) {
      return std::unexpected(std::move(domain.error()));
    }
    const std::array<llvm::Value*, 1> args{*operand};
    return builder_.CreateCall(
        Entry(RuntimeSymbol(*domain, RuntimeOp::kToBool), result_type, args),
        args);
  }
  const std::optional<lir::Signedness> signedness =
      module_->Unit().types.Get(operand_type).MachineIntegerSignedness();
  if (!signedness) {
    return Unsupported(
        "llvm codegen: cast between two types this target has no conversion "
        "between");
  }
  return builder_.CreateIntCast(
      *operand, target, *signedness == lir::Signedness::kSigned);
}

auto CodeGenFunction::ClassBehind(lir::TypeId reference) const
    -> std::optional<lir::TypeId> {
  const lir::TypePool& types = module_->Unit().types;
  const lir::Type& referring = types.Get(reference);
  std::optional<lir::TypeId> pointee;
  if (const auto* pointer = referring.As<lir::PointerType>()) {
    pointee = pointer->pointee;
  } else if (const auto* handle = referring.As<lir::ManagedRefType>()) {
    pointee = handle->pointee;
  }
  if (!pointee.has_value() ||
      !(types.Get(*pointee).Is<lir::ObjectType>() ||
        types.Get(*pointee).Is<lir::CrossUnitClassType>())) {
    return std::nullopt;
  }
  return pointee;
}

// Every class part of an object starts where the object does, so reaching one
// from another is the same address. An interface class's part is wherever the
// object's own class placed it, so the table the part the conversion starts
// from holds at its start holds the offset to it (C++ ABI 2.5.2 virtual base
// offsets).
auto CodeGenFunction::ConvertView(
    llvm::Value* view, lir::TypeId from, lir::TypeId to)
    -> diag::Result<llvm::Value*> {
  if (from == to || !module_->IsInterfaceClass(to)) {
    return view;
  }
  llvm::Type* ptr_ty = module_->Types().Ptr();
  llvm::Type* byte_ty = builder_.getInt8Ty();
  llvm::Value* table = builder_.CreateLoad(ptr_ty, view);
  llvm::Value* offset = builder_.CreateLoad(
      builder_.getInt64Ty(),
      builder_.CreateGEP(
          ptr_ty, table,
          llvm::ConstantInt::getSigned(
              builder_.getInt64Ty(),
              -static_cast<std::int64_t>(
                  module_->InterfacePartOffset(from, to)))));
  return builder_.CreateGEP(byte_ty, view, offset);
}

auto CodeGenFunction::IfNotNull(
    llvm::Value* view,
    llvm::function_ref<diag::Result<llvm::Value*>(llvm::Value*)> reach)
    -> diag::Result<llvm::Value*> {
  llvm::BasicBlock* asked = builder_.GetInsertBlock();
  llvm::BasicBlock* present =
      llvm::BasicBlock::Create(module_->Context(), "", value_);
  llvm::BasicBlock* joined =
      llvm::BasicBlock::Create(module_->Context(), "", value_);
  builder_.CreateCondBr(builder_.CreateIsNull(view), joined, present);
  builder_.SetInsertPoint(present);
  auto reached = reach(view);
  if (!reached) {
    return reached;
  }
  llvm::BasicBlock* reached_from = builder_.GetInsertBlock();
  builder_.CreateBr(joined);
  builder_.SetInsertPoint(joined);
  llvm::PHINode* result = builder_.CreatePHI(module_->Types().Ptr(), 2);
  result->addIncoming(
      llvm::ConstantPointerNull::get(module_->Types().Ptr()), asked);
  result->addIncoming(*reached, reached_from);
  return result;
}

// The handle a conversion forms refers to the object its operand does, so only
// the part it reaches through is worked out here, and a handle referring to no
// object stays one.
auto CodeGenFunction::LowerHandleCast(
    const lir::HandleCastInstr& cast, lir::TypeId result_type, llvm::Value* out)
    -> diag::Result<llvm::Value*> {
  auto handle = LowerOperand(cast.operand);
  if (!handle) {
    return std::unexpected(std::move(handle.error()));
  }
  llvm::Value* view = HandleView(*handle);
  const std::optional<lir::TypeId> from =
      ClassBehind(OperandType(cast.operand));
  const std::optional<lir::TypeId> to = ClassBehind(result_type);
  if (from.has_value() && to.has_value()) {
    auto converted = IfNotNull(view, [&](llvm::Value* present) {
      return ConvertView(present, *from, *to);
    });
    if (!converted) {
      return converted;
    }
    view = *converted;
  }
  return HandleWithView(*handle, view, out);
}

// The host's own dynamic cast answers it (C++ ABI 2.9.7), reading the
// descriptions each class's unit emits: handed a part, which holds its table's
// address at its start, and the descriptions of the part's class and the wanted
// one, it answers the wanted class's part, or null.
auto CodeGenFunction::LowerDynamicCast(
    const lir::DynamicCastInstr& cast, lir::TypeId result_type,
    llvm::Value* out) -> diag::Result<llvm::Value*> {
  auto handle = LowerOperand(cast.operand);
  if (!handle) {
    return std::unexpected(std::move(handle.error()));
  }
  const std::optional<lir::TypeId> from =
      ClassBehind(OperandType(cast.operand));
  const std::optional<lir::TypeId> to = ClassBehind(result_type);
  if (!from.has_value() || !to.has_value()) {
    throw InternalError(
        "llvm codegen: a dynamic cast between handles not both of classes -- "
        "please report this as a bug");
  }
  llvm::Type* ptr_ty = module_->Types().Ptr();
  auto found = IfNotNull(
      HandleView(*handle),
      [&](llvm::Value* view) -> diag::Result<llvm::Value*> {
        const std::array<llvm::Value*, 4> args{
            view, module_->TypeInfoOf(*from), module_->TypeInfoOf(*to),
            llvm::ConstantInt::getSigned(builder_.getInt64Ty(), kNoCastHint)};
        return builder_.CreateCall(Entry("__dynamic_cast", ptr_ty, args), args);
      });
  if (!found) {
    return found;
  }
  return HandleWithView(*handle, *found, out);
}

auto CodeGenFunction::HandleView(llvm::Value* handle) -> llvm::Value* {
  const std::array<llvm::Value*, 1> args{handle};
  return builder_.CreateCall(
      Entry(
          RuntimeSymbol(RuntimeOp::kHandleView), module_->Types().Ptr(), args),
      args);
}

auto CodeGenFunction::HandleWithView(
    llvm::Value* handle, llvm::Value* view, llvm::Value* out) -> llvm::Value* {
  const std::array<llvm::Value*, 3> args{handle, view, out};
  builder_.CreateCall(
      Entry(
          RuntimeSymbol(RuntimeOp::kHandleWithView), module_->Types().Ptr(),
          args),
      args);
  return out;
}

auto CodeGenFunction::LowerReceiveDeparture() -> diag::Result<llvm::Value*> {
  // The referee is named on the function rather than at the landing, so it is
  // set the first time one is needed and is absent from a body that has none.
  if (!value_->hasPersonalityFn()) {
    value_->setPersonalityFn(
        llvm::cast<llvm::Constant>(
            module_->Module()
                .getOrInsertFunction(
                    "__gxx_personality_v0",
                    llvm::FunctionType::get(builder_.getInt32Ty(), true))
                .getCallee()));
  }
  // The pad takes whatever reaches it: which region may claim a departure is a
  // question about the target it names, which the body already tests, so
  // selecting here on anything finer answers it twice in two vocabularies.
  //
  // It says so with a clause, which is what makes this frame one the platform
  // stops at while it works out where a raise is going -- and a landing has to
  // be such a frame, because a landing that is only reached afterwards is
  // reached only when somewhere else already stopped it. The clause matches
  // anything, so it names no raised type; receiving what arrived is what turns
  // it into the departure the body tests, and what is not the design's is
  // carried on from there before the body sees it.
  llvm::Type* const pad_type =
      llvm::StructType::get(module_->Types().Ptr(), builder_.getInt32Ty());
  llvm::LandingPadInst* const pad = builder_.CreateLandingPad(pad_type, 1);
  pad->addClause(llvm::ConstantPointerNull::get(module_->Types().Ptr()));
  const std::array<llvm::Value*, 1> carried{
      builder_.CreateExtractValue(pad, 0)};
  return builder_.CreateCall(
      Entry(
          RuntimeSymbol(support::BuiltinFn::kReceiveDeparture),
          module_->Types().Ptr(), carried),
      carried);
}

auto CodeGenFunction::ResolveCall(
    const lir::CallInstr& call, lir::TypeId result_type, llvm::Value* out)
    -> diag::Result<ResolvedCall> {
  std::vector<llvm::Value*> operands;
  operands.reserve(call.args.size());
  for (const lir::Operand& arg : call.args) {
    auto lowered = LowerOperand(arg);
    if (!lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
    operands.push_back(*lowered);
  }
  // The entry is resolved against what it is actually handed, so this target's
  // own encoding of the call is already in the argument list by the time the
  // entry's signature is read off it.
  auto args = CallArgs(call, result_type, std::move(operands));
  if (!args) {
    return std::unexpected(std::move(args.error()));
  }
  // Storage for what the callee builds goes last, whichever kind of callee it
  // is: an entry of the library and a body of this program take it alike. The
  // library holds no tuple type of its own, so storage given for a tuple says
  // which tuple it is before anything is built there.
  if (out != nullptr) {
    if (module_->Unit().types.Get(result_type).IsProduct()) {
      builder_.CreateStore(module_->Tuples().TypeOf(result_type), out);
    }
    args->push_back(out);
  }
  auto callee = ResolveCallee(call, result_type, *args);
  if (!callee) {
    return std::unexpected(std::move(callee.error()));
  }
  return ResolvedCall{.callee = *callee, .args = *std::move(args)};
}

auto CodeGenFunction::LowerCall(
    const lir::CallInstr& call, lir::TypeId result_type, llvm::Value* out)
    -> diag::Result<llvm::Value*> {
  // Ending a value whose storage going away is the whole of its end is nothing
  // to emit.
  if (std::holds_alternative<lir::EndValueTarget>(call.target) &&
      module_->Types()
          .StorageOf(OperandType(call.args.at(0)))
          .ends_with_nothing_to_do) {
    return nullptr;
  }
  // Entering a body builds the execution it becomes. Nothing ends that here:
  // whoever drives it takes it out of the storage, the moment it is made.
  if (const auto* coroutine = std::get_if<lir::CoroutineTarget>(&call.target)) {
    switch (coroutine->op) {
      case lir::CoroutineTarget::Op::kEnterBorrowedEnvironment:
      case lir::CoroutineTarget::Op::kEnterOwnedEnvironment:
        out = ObjectStorage(support::LibraryObject::kExecution);
        break;
      case lir::CoroutineTarget::Op::kAwait:
      case lir::CoroutineTarget::Op::kRelease:
        break;
    }
  }
  auto resolved = ResolveCall(call, result_type, out);
  if (!resolved) {
    return std::unexpected(std::move(resolved.error()));
  }
  llvm::Value* called = builder_.CreateCall(resolved->callee, resolved->args);
  return out != nullptr ? out : called;
}

// An entry the runtime publishes, typed by what the call hands it: the values
// crossing are its parameters by construction, so an entry and its call cannot
// disagree about what is passed.
auto CodeGenFunction::Entry(
    std::string_view symbol, llvm::Type* result,
    std::span<llvm::Value* const> args) -> llvm::FunctionCallee {
  return module_->Module().getOrInsertFunction(
      symbol, CallSignature(result, args));
}

auto CodeGenFunction::Entry(
    std::string_view symbol, lir::TypeId result,
    std::span<llvm::Value* const> args) -> llvm::FunctionCallee {
  return Entry(symbol, module_->Types().Map(result), args);
}

auto CodeGenFunction::CallArgs(
    const lir::CallInstr& call, lir::TypeId result_type,
    std::vector<llvm::Value*> operands)
    -> diag::Result<std::vector<llvm::Value*>> {
  auto encoding = EncodingOf(call, result_type);
  if (!encoding) {
    return std::unexpected(std::move(encoding.error()));
  }
  // A position the callee names is not a value the program computed, so this
  // target writes it where its calls take values: right after the object whose
  // part it names, and first where the entry acts on no object -- which is what
  // its declaration says by being a factory on the type it builds.
  std::optional<std::size_t> position_at;
  llvm::Value* position = nullptr;
  if (const auto* builtin = std::get_if<lir::BuiltinTarget>(&call.target);
      builtin != nullptr && builtin->position.has_value()) {
    const bool acts_on_an_object =
        !std::holds_alternative<support::StaticFactory>(
            support::RuntimeEntryOf(builtin->fn).declaration);
    position_at = acts_on_an_object ? 1 : 0;
    position = llvm::ConstantInt::get(
        llvm::Type::getInt64Ty(module_->Context()), builtin->position->value);
  }
  // An erased operand crosses as itself followed by its type.
  llvm::Value* erased_type = nullptr;
  if (const std::optional<ErasedArgument>& argument = encoding->erased) {
    auto type_object = module_->ValueTypeOf(argument->type);
    if (!type_object) {
      return std::unexpected(std::move(type_object.error()));
    }
    erased_type = *type_object;
  }
  std::vector<llvm::Value*> args;
  args.reserve(operands.size() + 2);
  for (std::size_t i = 0; i <= operands.size(); ++i) {
    if (position_at == i) {
      args.push_back(position);
    }
    if (i == operands.size()) {
      break;
    }
    args.push_back(operands[i]);
    if (encoding->erased.has_value() && encoding->erased->position == i) {
      args.push_back(erased_type);
    }
  }
  return ArgsInForm(encoding->operand_form, args);
}

auto CodeGenFunction::ArgsInForm(
    const OperandForm& form, const std::vector<llvm::Value*>& operands)
    -> diag::Result<std::vector<llvm::Value*>> {
  return std::visit(
      Overloaded{
          [&](const OperandsAsStated&)
              -> diag::Result<std::vector<llvm::Value*>> { return operands; },
          [&](const OperandsAfterSize& f)
              -> diag::Result<std::vector<llvm::Value*>> {
            auto size = module_->CompleteObjectSize(f.of);
            if (!size) {
              return std::unexpected(std::move(size.error()));
            }
            std::vector<llvm::Value*> args{builder_.getInt64(*size)};
            args.insert(args.end(), operands.begin(), operands.end());
            return args;
          }},
      form);
}

// Every call is a symbol invoked with arguments; the target kinds differ only
// in how the symbol is resolved.
auto CodeGenFunction::ResolveCallee(
    const lir::CallInstr& call, lir::TypeId result_type,
    std::span<llvm::Value*> args) -> diag::Result<llvm::FunctionCallee> {
  return std::visit(
      Overloaded{
          [&](const lir::BuiltinTarget& t)
              -> diag::Result<llvm::FunctionCallee> {
            return BuiltinCallee(t, call, result_type, args);
          },
          [&](const lir::FunctionTarget& t)
              -> diag::Result<llvm::FunctionCallee> {
            return module_->UnitFunction(t.function);
          },
          [&](const lir::IndirectTarget& t)
              -> diag::Result<llvm::FunctionCallee> {
            auto callee = LowerOperand(t.callee);
            if (!callee) {
              return std::unexpected(std::move(callee.error()));
            }
            return llvm::FunctionCallee(
                CallSignature(module_->Types().Map(result_type), args),
                *callee);
          },
          // The value holds where its class's table is, and the table holds
          // the body answering the behavior where the class introducing it put
          // it, as a C++ virtual call reads one.
          [&](const lir::DispatchTarget& t)
              -> diag::Result<llvm::FunctionCallee> {
            if (args.empty()) {
              throw InternalError(
                  "llvm codegen: a dispatched call states no value to dispatch "
                  "on");
            }
            auto at = module_->DispatchSlotOf(t.method);
            if (!at) {
              return std::unexpected(std::move(at.error()));
            }
            llvm::Type* ptr_ty = module_->Types().Ptr();
            llvm::Type* byte_ty = builder_.getInt8Ty();
            const auto body_at = [&](llvm::Value* table, std::uint64_t slot) {
              return builder_.CreateLoad(
                  ptr_ty,
                  builder_.CreateConstInBoundsGEP1_64(ptr_ty, table, slot));
            };
            llvm::Value* body = std::visit(
                Overloaded{
                    [&](const LineageSlot& lineage) -> llvm::Value* {
                      return body_at(
                          builder_.CreateLoad(ptr_ty, args[0]), lineage.slot);
                    },
                    // The part's table holds how far back the value's start
                    // is.
                    [&](const InterfaceSlot& part) -> llvm::Value* {
                      llvm::Value* table = builder_.CreateLoad(ptr_ty, args[0]);
                      llvm::Value* to_top = builder_.CreateLoad(
                          builder_.getInt64Ty(),
                          builder_.CreateGEP(
                              ptr_ty, table,
                              llvm::ConstantInt::getSigned(
                                  builder_.getInt64Ty(),
                                  -static_cast<std::int64_t>(
                                      kOffsetToTopEntry))));
                      args[0] = builder_.CreateGEP(byte_ty, args[0], to_top);
                      return body_at(table, part.slot);
                    }},
                *at);
            return llvm::FunctionCallee(
                CallSignature(module_->Types().Map(result_type), args), body);
          },
          [&](const lir::ConstructTarget&)
              -> diag::Result<llvm::FunctionCallee> {
            auto construction = ConstructionOf(call, result_type);
            if (!construction) {
              return std::unexpected(std::move(construction.error()));
            }
            return Entry(construction->symbol, result_type, args);
          },
          [&](const lir::LibraryConstructorTarget& t)
              -> diag::Result<llvm::FunctionCallee> {
            return Entry(
                runtime::BaseObjectConstructorSymbolOf(t.cls), result_type,
                args);
          },
          // A body another artifact defines is declared here and resolved by
          // the host, and its operands are this program's own values.
          [&](const lir::SymbolTarget& t)
              -> diag::Result<llvm::FunctionCallee> {
            return Entry(t.symbol, result_type, args);
          },
          // A foreign symbol is declared, never defined: the host resolves it.
          // The boundary already marshaled its operands and result to the
          // carriers the foreign side declared (LRM 35.5.6), so what crosses is
          // what the entry takes.
          [&](const lir::ForeignTarget& t)
              -> diag::Result<llvm::FunctionCallee> {
            return Entry(t.symbol, result_type, args);
          },
          [&](const lir::ValueCellTarget& t)
              -> diag::Result<llvm::FunctionCallee> {
            auto domain = DomainOf(t.value);
            if (!domain) {
              return std::unexpected(std::move(domain.error()));
            }
            return Entry(RuntimeSymbol(*domain, t.op), result_type, args);
          },
          [&](const lir::OpenWriteTarget& t)
              -> diag::Result<llvm::FunctionCallee> {
            auto domain = DomainOf(t.value);
            if (!domain) {
              return std::unexpected(std::move(domain.error()));
            }
            return Entry(RuntimeSymbol(*domain, t.op), result_type, args);
          },
          [&](const lir::EndValueTarget&)
              -> diag::Result<llvm::FunctionCallee> {
            return OwnedCallee(
                OperandType(call.args.at(0)), TupleLifecycle::kDestroy,
                RuntimeOp::kDestroy, module_->Types().Map(result_type), args);
          },
          [&](const lir::CopyValueTarget&)
              -> diag::Result<llvm::FunctionCallee> {
            return OwnedCallee(
                result_type, TupleLifecycle::kCopy, RuntimeOp::kCopy,
                module_->Types().Map(result_type), args);
          },
          [&](const lir::ControlEffectTarget& t)
              -> diag::Result<llvm::FunctionCallee> {
            return Entry(RuntimeSymbol(t.op), result_type, args);
          },
          [&](const lir::CoroutineTarget& t)
              -> diag::Result<llvm::FunctionCallee> {
            return Entry(RuntimeSymbol(t.op), result_type, args);
          }},
      call.target);
}

// A {pointer, length} span over a scratch buffer this function fills with
// `values`. The element type is the caller's to state: the machine element a
// LIR type names where the span carries plain data, and an address where it
// carries a sequence of objects the library defines. Nothing here reads
// what the values mean, so nothing depends on which entry the span feeds.
auto CodeGenFunction::SpanOver(
    std::span<llvm::Value* const> values, llvm::Type* element) -> llvm::Value* {
  auto* storage_ty = llvm::ArrayType::get(element, values.size());
  llvm::Value* storage = FrameStorage(storage_ty);
  for (std::uint32_t i = 0; i < values.size(); ++i) {
    llvm::Value* slot =
        builder_.CreateConstInBoundsGEP2_64(storage_ty, storage, 0, i);
    builder_.CreateStore(values[i], slot);
  }
  llvm::Value* span = llvm::UndefValue::get(module_->Types().Span());
  span = builder_.CreateInsertValue(span, storage, {0});
  return builder_.CreateInsertValue(
      span,
      llvm::ConstantInt::get(
          llvm::Type::getInt64Ty(module_->Context()),
          static_cast<std::uint64_t>(values.size())),
      {1});
}

auto CodeGenFunction::LowerArray(
    const lir::ArrayInstr& array, lir::TypeId result_type)
    -> diag::Result<llvm::Value*> {
  const lir::TypeId element_type = module_->Unit()
                                       .types.Get(result_type)
                                       .Get<lir::MachineArrayType>()
                                       .element;
  std::vector<llvm::Value*> elements;
  elements.reserve(array.elements.size());
  for (const lir::Operand& element : array.elements) {
    auto lowered = LowerOperand(element);
    if (!lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
    elements.push_back(*lowered);
  }
  return SpanOver(elements, module_->Types().Map(element_type));
}

// A tuple value is laid out in place: it opens with its type's operation
// table, and each component is built where the type's layout puts it, from a
// value LIR ends on its own, so the tuple takes a copy.
auto CodeGenFunction::LowerTuple(
    const lir::TupleInstr& tuple, lir::TypeId result_type, llvm::Value* out)
    -> diag::Result<llvm::Value*> {
  const TupleLayout& layout = module_->Types().LayoutOfTuple(result_type);
  if (layout.components.size() != tuple.components.size()) {
    throw InternalError(
        "llvm codegen: a tuple's result type does not describe the components "
        "it is built from");
  }
  builder_.CreateStore(module_->Tuples().TypeOf(result_type), out);
  for (std::size_t i = 0; i < tuple.components.size(); ++i) {
    auto component = LowerOperand(tuple.components[i]);
    if (!component) {
      return std::unexpected(std::move(component.error()));
    }
    CopyValue(
        layout.components[i], *component,
        ComponentAddress(result_type, out, i));
  }
  return out;
}

auto CodeGenFunction::UnionMemberType(
    lir::TypeId union_type, std::uint32_t index) const -> lir::TypeId {
  const lir::Type& ty = module_->Unit().types.Get(union_type);
  if (!ty.IsUnion()) {
    throw InternalError(
        "llvm codegen: a union member selects into a non-union type");
  }
  const std::vector<lir::TypeId> members = ty.UnionMemberTypes();
  if (index >= members.size()) {
    throw InternalError("llvm codegen: a union member index is out of range");
  }
  return members[index];
}

auto CodeGenFunction::LowerTagTest(
    const lir::TagTestInstr& test, lir::TypeId result_type)
    -> diag::Result<llvm::Value*> {
  auto aggregate = LowerOperand(test.aggregate);
  if (!aggregate) {
    return std::unexpected(std::move(aggregate.error()));
  }
  auto domain = DomainOf(OperandType(test.aggregate));
  if (!domain) {
    return std::unexpected(std::move(domain.error()));
  }
  // The predicate is a machine boolean, the shape a value's `to_bool` yields;
  // the packed one-bit surface a pattern expression wears is restored by the
  // enclosing `from_bool`, not here.
  const std::array<llvm::Value*, 2> args{
      *aggregate,
      llvm::ConstantInt::get(
          llvm::Type::getInt64Ty(module_->Context()), test.index.value)};
  return builder_.CreateCall(
      Entry(RuntimeSymbol(*domain, RuntimeOp::kTagMatches), result_type, args),
      args);
}

auto CodeGenFunction::SelectorArgs(
    lir::TypeId container, const std::vector<lir::Operand>& operands,
    std::vector<llvm::Value*>& shape) -> diag::Result<void> {
  const bool stated = SelectsByStatedIndex(module_->Unit(), container);
  for (const lir::Operand& operand : operands) {
    auto lowered = LowerOperand(operand);
    if (!lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
    if (!stated) {
      shape.push_back(*lowered);
      continue;
    }
    auto type_object = module_->ValueTypeOf(OperandType(operand));
    if (!type_object) {
      return std::unexpected(std::move(type_object.error()));
    }
    shape.push_back(*lowered);
    shape.push_back(*type_object);
  }
  return {};
}

auto CodeGenFunction::LowerAggregateExtract(
    const lir::AggregateExtractInstr& extract, llvm::Value* out)
    -> diag::Result<llvm::Value*> {
  auto aggregate = LowerOperand(extract.aggregate);
  if (!aggregate) {
    return std::unexpected(std::move(aggregate.error()));
  }
  const lir::TypeId container = OperandType(extract.aggregate);
  // A sequence of handles belongs to no value domain, so which object a
  // coordinate names is answered by the entry that knows the sequence rather
  // than by a value's own element read.
  if (IsHandleSequence(container)) {
    const auto* element = std::get_if<lir::ContainerElement>(&extract.selector);
    if (element == nullptr || element->operands.size() != 1) {
      throw InternalError(
          "llvm codegen: a sequence of handles is reached by one coordinate, "
          "which is what the step naming an element carries");
    }
    auto index = LowerOperand(element->operands.front());
    if (!index) {
      return std::unexpected(std::move(index.error()));
    }
    const std::array<llvm::Value*, 2> args{*aggregate, *index};
    return builder_.CreateCall(
        Entry(
            RuntimeSymbol(RuntimeOp::kSequenceElement), module_->Types().Ptr(),
            args),
        args);
  }
  auto domain = DomainOf(container);
  if (!domain) {
    return std::unexpected(std::move(domain.error()));
  }
  const auto coordinates = [&](const std::vector<lir::Operand>& operands)
      -> diag::Result<std::vector<llvm::Value*>> {
    std::vector<llvm::Value*> shape{*aggregate};
    auto filled = SelectorArgs(container, operands, shape);
    if (!filled) {
      return std::unexpected(std::move(filled.error()));
    }
    return shape;
  };
  // A member of an active-member value is named by its index alone, and the
  // aggregate's own domain is what names the entry that answers.
  const auto positional = [&](base::ComponentIndex index) -> llvm::Value* {
    return BuildInto(
        RuntimeSymbol(*domain, support::BuiltinFn::kComponent),
        {*aggregate,
         llvm::ConstantInt::get(
             llvm::Type::getInt64Ty(module_->Context()), index.value)},
        out);
  };
  return std::visit(
      Overloaded{
          [&](const lir::Component& component) -> diag::Result<llvm::Value*> {
            return positional(component.index);
          },
          [&](const lir::ContainerElement& e) -> diag::Result<llvm::Value*> {
            auto shape = coordinates(e.operands);
            if (!shape) {
              return std::unexpected(std::move(shape.error()));
            }
            return BuildInto(
                RuntimeSymbol(*domain, support::BuiltinFn::kElement),
                *std::move(shape), out);
          },
          [&](const lir::ContainerSlice& s) -> diag::Result<llvm::Value*> {
            auto shape = coordinates(s.operands);
            if (!shape) {
              return std::unexpected(std::move(shape.error()));
            }
            return BuildInto(
                RuntimeSymbol(*domain, support::BuiltinFn::kSlice),
                *std::move(shape), out);
          }},
      extract.selector);
}

auto CodeGenFunction::LowerAggregateUpdate(
    const lir::AggregateUpdateInstr& update, llvm::Value* out)
    -> diag::Result<llvm::Value*> {
  auto aggregate = LowerOperand(update.aggregate);
  if (!aggregate) {
    return std::unexpected(std::move(aggregate.error()));
  }
  auto replacement = LowerOperand(update.replacement);
  if (!replacement) {
    return std::unexpected(std::move(replacement.error()));
  }
  const lir::TypeId container = OperandType(update.aggregate);
  auto domain = DomainOf(container);
  if (!domain) {
    return std::unexpected(std::move(domain.error()));
  }
  const auto coordinates = [&](const std::vector<lir::Operand>& operands)
      -> diag::Result<std::vector<llvm::Value*>> {
    std::vector<llvm::Value*> shape{*aggregate};
    auto filled = SelectorArgs(container, operands, shape);
    if (!filled) {
      return std::unexpected(std::move(filled.error()));
    }
    shape.push_back(*replacement);
    return shape;
  };
  return std::visit(
      Overloaded{
          // A member is replaced by naming its index and the value that takes
          // its place, and the aggregate's own domain names the entry that
          // performs it. The union's entries are compiled once for every
          // member type, so the replacement crosses with the type its member
          // is declared as. Whether the write then makes the member live or
          // faults a mismatched tag follows from the domain the entry is named
          // in.
          [&](const lir::Component& component) -> diag::Result<llvm::Value*> {
            auto member_type = module_->ValueTypeOf(
                UnionMemberType(container, component.index.value));
            if (!member_type) {
              return std::unexpected(std::move(member_type.error()));
            }
            return BuildInto(
                RuntimeSymbol(*domain, RuntimeOp::kWithComponent),
                {*aggregate,
                 llvm::ConstantInt::get(
                     llvm::Type::getInt64Ty(module_->Context()),
                     component.index.value),
                 *replacement, *member_type},
                out);
          },
          [&](const lir::ContainerElement& e) -> diag::Result<llvm::Value*> {
            auto shape = coordinates(e.operands);
            if (!shape) {
              return std::unexpected(std::move(shape.error()));
            }
            return BuildInto(
                RuntimeSymbol(*domain, RuntimeOp::kWithElement),
                *std::move(shape), out);
          },
          [&](const lir::ContainerSlice& s) -> diag::Result<llvm::Value*> {
            auto shape = coordinates(s.operands);
            if (!shape) {
              return std::unexpected(std::move(shape.error()));
            }
            return BuildInto(
                RuntimeSymbol(*domain, RuntimeOp::kWithSlice),
                *std::move(shape), out);
          }},
      update.selector);
}

auto CodeGenFunction::LowerOperand(const lir::Operand& operand)
    -> diag::Result<llvm::Value*> {
  return std::visit(
      Overloaded{
          [&](const lir::Use& use) -> diag::Result<llvm::Value*> {
            return values_.at(use.value);
          },
          [&](const lir::IntConst& c) -> diag::Result<llvm::Value*> {
            return LowerIntConst(c);
          },
          [&](const lir::StrConst& c) -> diag::Result<llvm::Value*> {
            return LowerStrConst(c);
          },
          [&](const lir::RealConst& c) -> diag::Result<llvm::Value*> {
            return LowerRealConst(c);
          },
          [&](const lir::NullConst& c) -> diag::Result<llvm::Value*> {
            return LowerNullConst(c);
          },
          [&](const lir::BoolConst& c) -> diag::Result<llvm::Value*> {
            return llvm::ConstantInt::get(
                llvm::Type::getInt1Ty(module_->Context()),
                static_cast<std::uint64_t>(c.value));
          },
          [&](const lir::TypeDescriptorRef& c) -> diag::Result<llvm::Value*> {
            return LowerTypeDescriptorRef(c);
          },
          [&](const lir::IntegralConstantRef& c) -> diag::Result<llvm::Value*> {
            return LowerIntegralConstantRef(c);
          },
          // The symbol is the storage itself, which the unit declaring it
          // defines and builds before the program starts, so a reference of
          // any unit names the same address.
          [&](const lir::StaticRef& s) -> diag::Result<llvm::Value*> {
            return module_->SharedStorage(s.symbol);
          },
          // The definition is the global itself, so its address is the
          // operand.
          [&](const lir::DefinitionRef& c) -> diag::Result<llvm::Value*> {
            auto definition = module_->DefinitionOf(c.defined);
            if (!definition) {
              return std::unexpected(std::move(definition.error()));
            }
            return *definition;
          }},
      operand);
}

// An integral constant is a machine integer, a native LLVM constant. A packed
// value has no native constant form in the opaque value model -- it is a
// runtime object -- so a constant of one reaches this backend as a call.
auto CodeGenFunction::LowerIntConst(const lir::IntConst& constant)
    -> diag::Result<llvm::Value*> {
  const auto* machine =
      module_->Unit().types.Get(constant.type).As<lir::MachineIntType>();
  if (machine == nullptr) {
    return Unsupported(
        std::format(
            "llvm codegen: a constant of type {} has no native form on this "
            "backend",
            module_->Unit().types.Get(constant.type).KindName()));
  }
  return llvm::ConstantInt::get(
      llvm::cast<llvm::IntegerType>(module_->Types().Map(constant.type)),
      constant.value.value_words.front(),
      machine->signedness == lir::Signedness::kSigned);
}

// A value whose contents are settled before the run is built by the first use
// that reaches it; every later use loads what that one left in the cell. It is
// built once because whatever settles it settles it once, and the cell is what
// gives it an address that outlives the call that built it.
auto CodeGenFunction::BuiltOnce(
    llvm::GlobalVariable* cell, const std::function<llvm::Value*()>& make)
    -> llvm::Value* {
  auto* ptr_ty = module_->Types().Ptr();
  llvm::Value* cached = builder_.CreateLoad(ptr_ty, cell);

  llvm::Function* fn = builder_.GetInsertBlock()->getParent();
  auto* build = llvm::BasicBlock::Create(module_->Context(), "", fn);
  auto* ready = llvm::BasicBlock::Create(module_->Context(), "", fn);
  llvm::BasicBlock* entry = builder_.GetInsertBlock();
  builder_.CreateCondBr(builder_.CreateIsNull(cached), build, ready);

  builder_.SetInsertPoint(build);
  llvm::Value* built = make();
  builder_.CreateStore(built, cell);
  builder_.CreateBr(ready);

  builder_.SetInsertPoint(ready);
  llvm::PHINode* value = builder_.CreatePHI(ptr_ty, 2);
  value->addIncoming(cached, entry);
  value->addIncoming(built, build);
  return value;
}

auto CodeGenFunction::LowerTypeDescriptorRef(const lir::TypeDescriptorRef& ref)
    -> diag::Result<llvm::Value*> {
  const lir::FunctionId initializer =
      module_->Unit().type_descriptor_initializers.Get(ref.descriptor);
  // A description comes back owned by the run already, so keeping its address
  // is the whole of what the cell does.
  return BuiltOnce(module_->TypeDescriptorCell(ref.descriptor), [&] {
    return builder_.CreateCall(module_->UnitFunction(initializer), {});
  });
}

// The initializer builds the value in storage of this body's frame, which ends
// with the body, so the run takes its own copy before the cell keeps the
// address.
auto CodeGenFunction::LowerIntegralConstantRef(
    const lir::IntegralConstantRef& ref) -> llvm::Value* {
  const lir::FunctionId initializer =
      module_->Unit().integral_constant_initializers.Get(ref.constant);
  const support::RuntimeObject object =
      ObjectOf(module_->Unit().functions.Get(initializer).result_type);
  return BuiltOnce(module_->IntegralConstantCell(ref.constant), [&] {
    llvm::Value* built = ObjectStorage(object);
    builder_.CreateCall(module_->UnitFunction(initializer), {built});
    const std::array<llvm::Value*, 1> args{built};
    llvm::Value* retained = builder_.CreateCall(
        Entry(
            RuntimeSymbol(RuntimeOp::kRetainConstant), module_->Types().Ptr(),
            args),
        args);
    EndObject(object, built);
    return retained;
  });
}

// A string literal materializes as its native constant bytes; the owning
// runtime String is built from them by a constructor, not at the use site.
auto CodeGenFunction::LowerStrConst(const lir::StrConst& constant)
    -> llvm::Value* {
  return builder_.CreateGlobalString(constant.value);
}

// A real constant is a machine float, a native LLVM constant. A real-family
// value is a runtime object, so a constant of one reaches this backend as a
// construction over a machine float.
auto CodeGenFunction::LowerRealConst(const lir::RealConst& constant)
    -> diag::Result<llvm::Value*> {
  const auto* machine =
      module_->Unit().types.Get(constant.type).As<lir::MachineFloatType>();
  if (machine == nullptr) {
    return Unsupported(
        std::format(
            "llvm codegen: a real constant of type {} has no native form on "
            "this backend",
            module_->Unit().types.Get(constant.type).KindName()));
  }
  return llvm::ConstantFP::get(
      module_->Types().Map(constant.type), constant.value);
}

auto CodeGenFunction::LowerNullConst(const lir::NullConst& constant)
    -> llvm::Value* {
  // Referring to nothing is a value of the type like any other, so where the
  // type's values are objects the null one is too, built in the frame like any
  // other. It holds nothing, so its storage going away is the whole of its
  // end. A pointer has no object behind it to build, so there the null is the
  // null pointer itself.
  const std::optional<support::ValueDomain> domain =
      ValueDomainOf(module_->Unit(), constant.type);
  if (domain.has_value()) {
    return BuildInto(
        RuntimeSymbol(*domain, RuntimeOp::kDefault), {},
        ObjectStorage(*domain));
  }
  return llvm::ConstantPointerNull::get(
      llvm::cast<llvm::PointerType>(module_->Types().Map(constant.type)));
}

// The entry behind a builtin. What names it is the operation, plus -- where the
// library realizes an operation once per value representation -- the
// representation of a value the call carries, which is one it is handed for an
// operation on a value and the one it answers with for a factory. Which of
// those the builtin takes is the builtin's own property, so it is read from its
// identity.
auto CodeGenFunction::BuiltinCallee(
    const lir::BuiltinTarget& target, const lir::CallInstr& call,
    lir::TypeId result_type, std::span<llvm::Value* const> args)
    -> diag::Result<llvm::FunctionCallee> {
  const auto over = [&](diag::Result<support::ValueDomain> domain)
      -> diag::Result<llvm::FunctionCallee> {
    if (!domain) {
      return std::unexpected(std::move(domain.error()));
    }
    return Entry(RuntimeSymbol(*domain, target.fn), result_type, args);
  };
  return std::visit(
      Overloaded{
          [&](const NamedAlone&) -> diag::Result<llvm::FunctionCallee> {
            return Entry(RuntimeSymbol(target.fn), result_type, args);
          },
          [&](const NamedByValue& named) -> diag::Result<llvm::FunctionCallee> {
            return over(DomainOf(OperandType(call.args.at(named.operand))));
          },
          [&](const NamedByResult&) -> diag::Result<llvm::FunctionCallee> {
            return over(DomainOf(result_type));
          },
          [&](const NamedByWrapper&) -> diag::Result<llvm::FunctionCallee> {
            auto wrapper = WrapperBehind(OperandType(call.args.at(0)));
            if (!wrapper) {
              return std::unexpected(std::move(wrapper.error()));
            }
            return Entry(
                RuntimeSymbol(wrapper->domain, wrapper->kind, target.fn),
                result_type, args);
          },
          [&](const NamedByStorageDomain&)
              -> diag::Result<llvm::FunctionCallee> {
            return over(StorageDomainBehind(OperandType(call.args.at(0))));
          },
          [&](const NamedByConversion&) -> diag::Result<llvm::FunctionCallee> {
            auto destination = DomainOf(result_type);
            if (!destination) {
              return std::unexpected(std::move(destination.error()));
            }
            auto source = DomainOf(OperandType(call.args.front()));
            if (!source) {
              return std::unexpected(std::move(source.error()));
            }
            return Entry(
                RuntimeSymbol(*destination, target.fn, *source), result_type,
                args);
          },
          [&](const NotRealized& unrealized)
              -> diag::Result<llvm::FunctionCallee> {
            return Unsupported(
                std::format(
                    "llvm codegen: the {} builtin {} and the library has no "
                    "entry of that shape",
                    support::RuntimeEntryOf(target.fn).name, unrealized.shape));
          }},
      EntryNamingOf(target.fn));
}

auto CodeGenFunction::WrapperPlaceOf(const lir::Place& place) const
    -> diag::Result<std::optional<CodeGenFunction::WrapperPlace>> {
  // The last step names the storage behind whatever the chain had reached. When
  // that is a capability wrapper, the storage is the wrapper's contents, which
  // have no address of their own -- the wrapper decides what reading and
  // writing them mean, so the prefix names the wrapper and the access goes
  // through it.
  if (place.chain.empty() ||
      !std::holds_alternative<lir::DerefProjection>(place.chain.back())) {
    return std::nullopt;
  }
  lir::Place wrapper{
      .base = place.base,
      .chain = {place.chain.begin(), std::prev(place.chain.end())}};
  const lir::Type& opened_type = module_->Unit().types.Get(
      ReachedType(place, std::ssize(place.chain) - 1));
  const std::optional<std::pair<WrapperKind, lir::TypeId>> reached =
      WrapperOf(opened_type);
  if (!reached.has_value()) {
    return std::nullopt;
  }
  auto domain = DomainOf(reached->second);
  if (!domain) {
    return std::unexpected(std::move(domain.error()));
  }
  return WrapperPlace{
      .domain = *domain, .kind = reached->first, .wrapper = std::move(wrapper)};
}

// A capture is a member of the closure value, so each operand is taken into it
// as a value is into any storage it initializes: an owned value as a copy, and
// anything else as itself.
auto CodeGenFunction::LowerClosure(
    const lir::ClosureInstr& built, lir::TypeId result_type, llvm::Value* out)
    -> diag::Result<llvm::Value*> {
  auto definition = module_->DefinitionOf(result_type);
  if (!definition) {
    return std::unexpected(std::move(definition.error()));
  }
  auto record = module_->RecordOf(result_type);
  if (!record) {
    return std::unexpected(std::move(record.error()));
  }
  const std::array<llvm::Value*, 2> made{*definition, out};
  llvm::Value* closure = builder_.CreateCall(
      Entry(
          RuntimeSymbol(RuntimeOp::kClosureMake), module_->Types().Ptr(), made),
      made);
  for (std::size_t i = 0; i < built.captures.size(); ++i) {
    auto value = LowerOperand(built.captures[i]);
    if (!value) {
      return std::unexpected(std::move(value.error()));
    }
    const lir::TypeId type = (*record)->types.at(i);
    llvm::Value* slot = builder_.CreateConstInBoundsGEP1_64(
        builder_.getInt8Ty(), closure, (*record)->offsets.at(i));
    if (module_->Unit().types.Get(type).IsOwnedValue()) {
      CopyValue(type, *value, slot);
    } else {
      builder_.CreateStore(*value, slot);
    }
  }
  return out;
}

auto CodeGenFunction::StorageReached(lir::TypeId operand) const
    -> const lir::Type& {
  const lir::TypePool& types = module_->Unit().types;
  const lir::Type& carried = types.Get(operand);
  // A reference is a handle naming storage someone else owns, so it reaches
  // that storage by being what it is rather than by pointing at it: what it
  // states is the values the storage holds, which is not the storage.
  if (carried.Is<lir::RefType>()) {
    return carried;
  }
  const std::optional<lir::TypeId> pointee = carried.Pointee();
  return types.Get(pointee.value_or(operand));
}

auto CodeGenFunction::WrapperBehind(lir::TypeId operand) const
    -> diag::Result<CodeGenFunction::WrapperBehindRef> {
  const std::optional<std::pair<WrapperKind, lir::TypeId>> reached =
      WrapperOf(StorageReached(operand));
  if (!reached.has_value()) {
    throw InternalError(
        "llvm codegen: an operation on a wrapper needs one to act on");
  }
  auto domain = DomainOf(reached->second);
  if (!domain) {
    return std::unexpected(std::move(domain.error()));
  }
  return WrapperBehindRef{.domain = *domain, .kind = reached->first};
}

auto CodeGenFunction::StorageDomainBehind(lir::TypeId operand) const
    -> diag::Result<support::ValueDomain> {
  const std::optional<lir::TypeId> held = ValuesHeldBy(StorageReached(operand));
  if (!held.has_value()) {
    throw InternalError(
        "llvm codegen: an entry named by the representation of what a storage "
        "holds needs a storage whose values are all of one");
  }
  return DomainOf(*held);
}

auto CodeGenFunction::PlaceValueCellDomain(
    const lir::Place& place, lir::TypeId value) const
    -> std::optional<support::ValueDomain> {
  // A value cell is storage the runtime built around a declaration's value and
  // owns the representation of, and only the member step naming it reaches it:
  // an address taken of one is of the value it holds, so what a dereference
  // arrives at is the value. An element or a component is the value itself,
  // where its container holds it; so is what a write opened on a wrapper
  // reaches, and a base local's own slot.
  if (place.chain.empty()) {
    return std::nullopt;
  }
  const std::optional<MemberSlotRole> role = std::visit(
      Overloaded{
          [&](const lir::MemberProjection& step)
              -> std::optional<MemberSlotRole> {
            return MemberSlotRoleOf(
                DeclarationOf(module_->Unit(), step.member.declared_by));
          },
          [](const lir::DerefProjection&) -> std::optional<MemberSlotRole> {
            return std::nullopt;
          },
          [](const lir::ElementProjection&) -> std::optional<MemberSlotRole> {
            return std::nullopt;
          },
          [](const lir::ComponentProjection&) -> std::optional<MemberSlotRole> {
            return std::nullopt;
          }},
      place.chain.back());
  if (!role.has_value() || MemberStorageKindOf(module_->Unit(), value, *role) !=
                               support::MemberStorageKind::kValueCell) {
    return std::nullopt;
  }
  return ValueDomainOf(module_->Unit(), value);
}

auto CodeGenFunction::ConstructionOf(
    const lir::CallInstr& call, lir::TypeId result) const
    -> diag::Result<Construction> {
  const auto entry = [](std::string symbol) -> Construction {
    return Construction{.symbol = std::move(symbol)};
  };
  // A container holds elements of a representation nothing else states, so
  // building one takes a prototype of the element it is to hold.
  const auto seeded = [](std::string symbol) -> Construction {
    return Construction{.symbol = std::move(symbol), .shape_operand = 0};
  };
  const auto no_construct = [&]() -> std::unexpected<diag::Diagnostic> {
    return Unsupported(
        std::format(
            "llvm codegen: a value of type {} has no construct on this backend",
            module_->Unit().types.Get(result).KindName()));
  };
  const auto no_real_from_host = [&]() -> std::unexpected<diag::Diagnostic> {
    return Unsupported(
        std::format(
            "llvm codegen: building a {} from a host scalar has no entry on "
            "this backend",
            module_->Unit().types.Get(result).KindName()));
  };
  // An entry named by the representation of the value the construction is built
  // over, which is the call's first operand.
  const auto over_operand = [&](RuntimeOp op) -> diag::Result<Construction> {
    auto domain = DomainOf(OperandType(call.args.at(0)));
    if (!domain) {
      return std::unexpected(std::move(domain.error()));
    }
    return entry(RuntimeSymbol(*domain, op));
  };
  const auto real_from_host = [&]() -> diag::Result<Construction> {
    const lir::Type& arg =
        module_->Unit().types.Get(OperandType(call.args.at(0)));
    if (!arg.Is<lir::MachineFloatType>()) {
      return no_real_from_host();
    }
    auto domain = DomainOf(result);
    if (!domain) {
      return std::unexpected(std::move(domain.error()));
    }
    return entry(RuntimeSymbol(*domain, RuntimeOp::kConst));
  };
  return module_->Unit().types.Get(result).Visit(
      Overloaded{
          [&](const lir::StringType&) -> diag::Result<Construction> {
            return entry(
                RuntimeSymbol(support::ValueDomain::kString, RuntimeOp::kMake));
          },
          // LRM 6.14: a chandle is a host pointer, so the domain carries its
          // value inline and what comes into existence is the pointer the
          // boundary handed back.
          [&](const lir::ChandleType&) -> diag::Result<Construction> {
            return entry(RuntimeSymbol(
                support::ValueDomain::kChandle, RuntimeOp::kMake));
          },
          [&](const lir::ClosureType&) -> diag::Result<Construction> {
            throw InternalError(
                "llvm codegen: a closure is built by an instruction of its "
                "own, "
                "never by a construction -- please report this as a bug");
          },
          // A container is built over an element list laid down a stated number
          // of times.
          [&](const lir::DynamicArrayType&) -> diag::Result<Construction> {
            return seeded(RuntimeSymbol(
                support::ValueDomain::kDynArray, RuntimeOp::kFromLiteral));
          },
          [&](const lir::UnpackedArrayType&) -> diag::Result<Construction> {
            return seeded(RuntimeSymbol(
                support::ValueDomain::kUnpackedArray, RuntimeOp::kFromLiteral));
          },
          // LRM 7.10.5: a bounded queue enforces a maximum index, which its own
          // type declares, so which of the two entries builds one follows from
          // the type being built.
          [&](const lir::QueueType& q) -> diag::Result<Construction> {
            return seeded(RuntimeSymbol(
                support::ValueDomain::kQueue,
                q.max_bound.has_value() ? RuntimeOp::kFromLiteralBounded
                                        : RuntimeOp::kFromLiteral));
          },
          // LRM 7.8: the index type imposes the order the entries are held in.
          // Every declared index type's order is its own, which an index's type
          // answers wherever the index crosses; a wildcard index (LRM 7.8.1)
          // is self-determined, treated as unsigned, and admits one value at
          // any width, so its order is absent from the indices and the array
          // is built holding it.
          [&](const lir::AssociativeArrayType& a)
              -> diag::Result<Construction> {
            const bool wildcard_index = module_->Unit()
                                            .types.Get(a.key_type)
                                            .Is<lir::WildcardIndexType>();
            return seeded(RuntimeSymbol(
                support::ValueDomain::kAssocArray,
                wildcard_index ? RuntimeOp::kFromEntriesDefaultWildcard
                               : RuntimeOp::kFromEntriesDefault));
          },
          // A sequence outlives the body that built it -- the owner keeps
          // its address, and a dimension above it keeps that address as an
          // ordinary element -- so the runtime takes the handles rather than
          // the element list standing as the value.
          [&](const lir::VectorType&) -> diag::Result<Construction> {
            return entry(RuntimeSymbol(RuntimeOp::kSequenceMake));
          },
          [&](const lir::RuntimeLibraryType& r) -> diag::Result<Construction> {
            switch (r.kind) {
              case lir::RuntimeLibraryKind::kPrintLiteralItem:
                return entry(RuntimeSymbol(RuntimeOp::kMakePrintLiteralItem));
              case lir::RuntimeLibraryKind::kHierarchySegment:
                return entry(RuntimeSymbol(RuntimeOp::kMakeSegment));
              case lir::RuntimeLibraryKind::kTrigger:
                return entry(RuntimeSymbol(RuntimeOp::kMakeTrigger));
              case lir::RuntimeLibraryKind::kFormatSpec:
                return entry(RuntimeSymbol(RuntimeOp::kMakeFormatSpec));
              case lir::RuntimeLibraryKind::kPackedRange:
                return entry(RuntimeSymbol(RuntimeOp::kMakePackedRange));
              case lir::RuntimeLibraryKind::kUnpackedRange:
                return entry(RuntimeSymbol(RuntimeOp::kMakeUnpackedRange));
              case lir::RuntimeLibraryKind::kPackedType:
                return entry(RuntimeSymbol(RuntimeOp::kMakePackedType));
              case lir::RuntimeLibraryKind::kEnumeration:
                return entry(RuntimeSymbol(RuntimeOp::kMakeEnumeration));
              // What a value formats as is the value's own answer, so both of
              // these are named by the representation of what they are built
              // over. Each borrows that value rather than copying it, which
              // holds because the value it borrows belongs to the same
              // full-expression as the print, and ends only after it.
              case lir::RuntimeLibraryKind::kPrintValueItem:
                return over_operand(RuntimeOp::kMakePrintValueItem);
              case lir::RuntimeLibraryKind::kFormatArg:
                return over_operand(RuntimeOp::kMakeFormatArg);
              // The DPI-C boundary temporaries (LRM 35.5.6.1, Annex H.7.7).
              // Each images one SV value in the canonical form the C side
              // reads, so the entry is one function over every value it can
              // image and the value crosses erased.
              case lir::RuntimeLibraryKind::kDpiBitBuffer:
                return entry(RuntimeSymbol(RuntimeOp::kMakeDpiBitBuffer));
              case lir::RuntimeLibraryKind::kDpiLogicBuffer:
                return entry(RuntimeSymbol(RuntimeOp::kMakeDpiLogicBuffer));
              case lir::RuntimeLibraryKind::kDpiOpenArray:
                return seeded(RuntimeSymbol(RuntimeOp::kMakeDpiOpenArray));
              // The rest come into existence some other way, so a construction
              // naming one would have nothing to call. A print item is built as
              // one of its two forms and never as their sum; a time format, an
              // open-array handle, a control effect and an observation are what
              // some other entry answers with, and so are a read report and a
              // held wait; a chunk is the element type a canonical buffer's
              // pointer addresses rather than a value; a cancellation target
              // and a channel's joint cancel state are storage the owner holds
              // and reaches by address; and a write into an object is opened
              // by the entry that opens it.
              case lir::RuntimeLibraryKind::kPrintItem:
              case lir::RuntimeLibraryKind::kTimeFormat:
              case lir::RuntimeLibraryKind::kDpiBitChunk:
              case lir::RuntimeLibraryKind::kDpiLogicChunk:
              case lir::RuntimeLibraryKind::kDpiOpenArrayHandle:
              case lir::RuntimeLibraryKind::kControlEffect:
              case lir::RuntimeLibraryKind::kObservation:
              case lir::RuntimeLibraryKind::kReadReport:
              case lir::RuntimeLibraryKind::kWait:
              case lir::RuntimeLibraryKind::kObjectWrite:
              case lir::RuntimeLibraryKind::kCancellationTarget:
              case lir::RuntimeLibraryKind::kChannelCancellation:
              // A class's definition, and what a definition is made of, are
              // constants the declaring unit emits, so a body names one and
              // never builds one.
              case lir::RuntimeLibraryKind::kObjectDefinition:
              case lir::RuntimeLibraryKind::kScopeInfo:
              case lir::RuntimeLibraryKind::kScopeCallable:
                return no_construct();
            }
            throw InternalError("llvm codegen: unknown runtime library kind");
          },
          // A wrapper that owns storage brings that storage into existence with
          // itself, and which entry does that is what the ownership says. A
          // sole owner is storage `operator new` allocates for a complete
          // object of the class, which the class's constructor then runs on, as
          // a `new` does. A shared owner instead answers with a hold, because
          // what it brings into existence outlives the scope that asked for it
          // and ends with the last holder rather than at any one exit (LRM
          // 6.21); a hold makes an empty variable's cell, so it takes nothing
          // and is named by the domain the cell holds. A borrowed pointer is
          // bound to storage that already exists, so nothing constructs one.
          [&](const lir::PointerType& p) -> diag::Result<Construction> {
            switch (p.ownership) {
              case lir::PointerOwnership::kUnique:
                return Construction{
                    .symbol = std::string(kOperatorNew),
                    .operand_form = OperandsAfterSize{.of = p.pointee}};
              case lir::PointerOwnership::kShared: {
                const auto* cell = module_->Unit()
                                       .types.Get(p.pointee)
                                       .As<lir::ObservableType>();
                if (cell == nullptr) {
                  throw InternalError(
                      "llvm codegen: a counted hold is made over a variable's "
                      "cell -- please report this as a bug");
                }
                auto domain = DomainOf(cell->value);
                if (!domain) {
                  return std::unexpected(std::move(domain.error()));
                }
                return entry(
                    RuntimeSymbol(*domain, RuntimeOp::kSharedCellMake));
              }
              case lir::PointerOwnership::kBorrowed:
                return no_construct();
            }
            throw InternalError("llvm codegen: unknown pointer ownership");
          },
          // A handle owning an object the program built (LRM 8.3), handed that
          // object once its construction has run.
          [&](const lir::ManagedRefType&) -> diag::Result<Construction> {
            return entry(RuntimeSymbol(RuntimeOp::kObjectAdopt));
          },
          // Landing a machine integer in a real and reshaping across precisions
          // are named conversions, so what reaches the real family here is a
          // build over a host scalar: a constant of the destination's own
          // precision. Anything else the boundary hands back has no entry.
          [&](const lir::RealType&) -> diag::Result<Construction> {
            return real_from_host();
          },
          [&](const lir::ShortRealType&) -> diag::Result<Construction> {
            return real_from_host();
          },
          // A value carrying no bits has exactly one value (a tagged union's
          // void member, LRM 7.3.2), so its construction takes nothing and
          // yields that value.
          [&](const lir::EmptyType&) -> diag::Result<Construction> {
            return entry(RuntimeSymbol(
                support::ValueDomain::kEmpty, RuntimeOp::kDefault));
          },

          // Nothing below has a construction entry on this backend. Each says
          // so on a line of its own, because "nothing brings one of these into
          // existence" is a claim about that type and a reader can only check
          // a claim that was made; a type added later lands in none of them
          // and fails to compile until someone places it.

          // A value that is a vector of bits, or a host scalar standing beside
          // one.
          [&](const lir::PackedArrayType&) -> diag::Result<Construction> {
            return no_construct();
          },
          [&](const lir::WildcardIndexType&) -> diag::Result<Construction> {
            return no_construct();
          },
          [&](const lir::MachineBoolType&) -> diag::Result<Construction> {
            return no_construct();
          },
          [&](const lir::MachineIntType&) -> diag::Result<Construction> {
            return no_construct();
          },
          [&](const lir::MachineFloatType&) -> diag::Result<Construction> {
            return no_construct();
          },
          [&](const lir::MachineCStringType&) -> diag::Result<Construction> {
            return no_construct();
          },
          [&](const lir::VoidType&) -> diag::Result<Construction> {
            return no_construct();
          },

          // Aggregates this layer lays out itself, and the code address of a
          // body.
          [&](const lir::MachineArrayType&) -> diag::Result<Construction> {
            return no_construct();
          },
          [&](const lir::TupleType&) -> diag::Result<Construction> {
            return no_construct();
          },
          [&](const lir::StructType&) -> diag::Result<Construction> {
            return no_construct();
          },

          // A union's value is made by the instruction that states which
          // member it holds, never by a construction.
          [&](const lir::UnionType&) -> diag::Result<Construction> {
            return no_construct();
          },
          [&](const lir::TaggedUnionType&) -> diag::Result<Construction> {
            return no_construct();
          },
          [&](const lir::MachineFunctionType&) -> diag::Result<Construction> {
            return no_construct();
          },
          [&](const lir::CoroutineType&) -> diag::Result<Construction> {
            return no_construct();
          },

          // A node of the object tree. What brings one into existence is the
          // construction of the unique owner whose pointee it is, so the node
          // type itself never names an entry.
          [&](const lir::ObjectType&) -> diag::Result<Construction> {
            return no_construct();
          },
          [&](const lir::CrossUnitClassType&) -> diag::Result<Construction> {
            return no_construct();
          },
          [&](const lir::RuntimeClassType&) -> diag::Result<Construction> {
            return no_construct();
          },

          // A reference is bound to storage that already exists, naming it
          // rather than bringing it about, so nothing constructs one.
          [&](const lir::RefType&) -> diag::Result<Construction> {
            return no_construct();
          },

          // Storage an owner holds, which comes into existence with the owner
          // and is reached by address.
          [&](const lir::ObservableType&) -> diag::Result<Construction> {
            return no_construct();
          },
          [&](const lir::ResolvedType&) -> diag::Result<Construction> {
            return no_construct();
          },
          [&](const lir::DriverType&) -> diag::Result<Construction> {
            return no_construct();
          },
          [&](const lir::SampledHistoryType&) -> diag::Result<Construction> {
            return no_construct();
          },
          [&](const lir::EvaluationAttemptsType&)
              -> diag::Result<Construction> { return no_construct(); },
          [&](const lir::EventType&) -> diag::Result<Construction> {
            return no_construct();
          },
          // A write is opened by the wrapper it writes through, which is an
          // operation on the wrapper rather than a value anything builds, and
          // a part designated within it is built by the step that reaches it.
          [&](const lir::OpenWriteType&) -> diag::Result<Construction> {
            return no_construct();
          },
          [&](const lir::DesignationType&) -> diag::Result<Construction> {
            return no_construct();
          },

          // A stable runtime facade, realized as a live reference rather than
          // as a value anything builds.
          [&](const lir::RuntimeEffectsType&) -> diag::Result<Construction> {
            return no_construct();
          },
          [&](const lir::FilesType&) -> diag::Result<Construction> {
            return no_construct();
          },
          [&](const lir::DiagnosticType&) -> diag::Result<Construction> {
            return no_construct();
          }});
}

auto CodeGenFunction::OfItsOwnType(
    const lir::CallInstr& call, std::size_t position) const -> ErasedArgument {
  if (position >= call.args.size()) {
    throw InternalError(
        "llvm codegen: an entry names an operand it was not handed -- please "
        "report this as a bug");
  }
  return ErasedArgument{
      .position = position, .type = OperandType(call.args.at(position))};
}

auto CodeGenFunction::BuiltinErasedOperand(
    const lir::BuiltinTarget& target, const lir::CallInstr& call,
    lir::TypeId result_type) const -> std::optional<ErasedArgument> {
  const support::RuntimeEntry entry = support::RuntimeEntryOf(target.fn);
  // The union a member goes into is what the call answers with, because an
  // entry that builds one takes no object to read it from.
  if (entry.member_operand.has_value()) {
    return ErasedArgument{
        .position = *entry.member_operand,
        .type = UnionMemberType(result_type, target.position->value)};
  }
  if (entry.result_prototype_operand.has_value()) {
    return OfItsOwnType(call, *entry.result_prototype_operand);
  }
  if (entry.spread_operand.has_value()) {
    return OfItsOwnType(call, *entry.spread_operand);
  }
  // A coordinate crosses erased only where the container it selects into holds
  // no prototype for one; where a container names its entries by ordinals, the
  // entry already knows what an index is and the coordinate crosses as itself.
  // The container is the value the call is handed, or the one a designation
  // it is handed designates.
  if (!entry.index_operand.has_value()) {
    return std::nullopt;
  }
  const lir::TypeId receiver = OperandType(call.args.front());
  if (!SelectsByStatedIndex(
          module_->Unit(), ValuesHeldBy(module_->Unit().types.Get(receiver))
                               .value_or(receiver))) {
    return std::nullopt;
  }
  return OfItsOwnType(call, *entry.index_operand);
}

auto CodeGenFunction::EncodingOf(
    const lir::CallInstr& call, lir::TypeId result_type) const
    -> diag::Result<CallEncoding> {
  using Encoded = diag::Result<CallEncoding>;
  // A target encodes something where its entry takes more than the call
  // states. A library entry says on its declaration which operand states a
  // representation, and a construction is named by the type it builds, which
  // says both that and the form its entry takes the rest in. Every other target
  // names code whose parameters are already typed, so what the call states is
  // what crosses.
  return std::visit(
      Overloaded{
          [&](const lir::BuiltinTarget& t) -> Encoded {
            return CallEncoding{
                .erased = BuiltinErasedOperand(t, call, result_type)};
          },
          [&](const lir::ConstructTarget&) -> Encoded {
            auto construction = ConstructionOf(call, result_type);
            if (!construction) {
              return std::unexpected(std::move(construction.error()));
            }
            if (!construction->shape_operand.has_value()) {
              return CallEncoding{.operand_form = construction->operand_form};
            }
            return CallEncoding{
                .erased = OfItsOwnType(call, *construction->shape_operand),
                .operand_form = construction->operand_form};
          },
          [](const lir::FunctionTarget&) -> Encoded { return CallEncoding{}; },
          [](const lir::DispatchTarget&) -> Encoded { return CallEncoding{}; },
          [](const lir::IndirectTarget&) -> Encoded { return CallEncoding{}; },
          [](const lir::LibraryConstructorTarget&) -> Encoded {
            return CallEncoding{};
          },
          [](const lir::SymbolTarget&) -> Encoded { return CallEncoding{}; },
          [](const lir::ForeignTarget&) -> Encoded { return CallEncoding{}; },
          [](const lir::ValueCellTarget&) -> Encoded { return CallEncoding{}; },
          [](const lir::OpenWriteTarget&) -> Encoded { return CallEncoding{}; },
          [](const lir::EndValueTarget&) -> Encoded { return CallEncoding{}; },
          [](const lir::CopyValueTarget&) -> Encoded { return CallEncoding{}; },
          [](const lir::ControlEffectTarget&) -> Encoded {
            return CallEncoding{};
          },
          [](const lir::CoroutineTarget&) -> Encoded {
            return CallEncoding{};
          }},
      call.target);
}

}  // namespace lyra::backend::llvm_backend
