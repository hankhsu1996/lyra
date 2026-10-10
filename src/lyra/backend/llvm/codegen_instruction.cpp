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
#include "lyra/backend/llvm/fn_abi.hpp"
#include "lyra/backend/llvm/integral_one_word.hpp"
#include "lyra/backend/llvm/runtime_entry.hpp"
#include "lyra/base/component_index.hpp"
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
#include "lyra/value/integral.hpp"

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

// The member storage a wrapper is where its owner lays it out, which is where
// generated code can reach into it: a variable's cell and a net. A driver is a
// handle on a contribution its net keeps, and a reference names storage a
// caller lent, so neither is storage this side laid out.
auto StorageLaidOutFor(WrapperKind kind)
    -> std::optional<support::MemberStorageKind> {
  switch (kind) {
    case WrapperKind::kCell:
      return support::MemberStorageKind::kObservableCell;
    case WrapperKind::kNet:
      return support::MemberStorageKind::kResolvedNet;
    case WrapperKind::kDriver:
    case WrapperKind::kRef:
      return std::nullopt;
  }
  throw InternalError("llvm codegen: unknown capability wrapper");
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
            return LowerAggregateExtract(extract, result_type, out);
          },
          [&](const lir::AggregateUpdateInstr& update)
              -> diag::Result<llvm::Value*> {
            return LowerAggregateUpdate(update, result_type, out);
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
            return LowerBinary(binary, result_type, out);
          },
          [&](const lir::UnaryInstr& unary) -> diag::Result<llvm::Value*> {
            return LowerUnary(unary, result_type, out);
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
  if (module_->Unit().types.Get(result_type).HeldAs().has_value()) {
    return *address;
  }
  return builder_.CreateLoad(result, *address);
}

auto CodeGenFunction::HeldAt(
    llvm::Value* storage, support::DeclaredMemberStorage held)
    -> std::optional<llvm::Value*> {
  return runtime::ContentsOffsetOf(held).transform(
      [&](std::uint64_t at) -> llvm::Value* {
        return builder_.CreateConstInBoundsGEP1_64(
            builder_.getInt8Ty(), storage, at);
      });
}

auto CodeGenFunction::ContentsOf(
    support::ValueDomain domain, WrapperKind kind, llvm::Value* wrapper)
    -> llvm::Value* {
  if (const std::optional<support::MemberStorageKind> laid_out =
          StorageLaidOutFor(kind)) {
    if (const std::optional<llvm::Value*> held =
            HeldAt(wrapper, {.kind = *laid_out, .domain = domain})) {
      return *held;
    }
  }
  const std::array<llvm::Value*, 1> args{wrapper};
  return CallEntry(
      RuntimeSymbol(domain, kind, support::BuiltinFn::kLoad),
      module_->Types().Ptr(), Addresses(1), args);
}

auto CodeGenFunction::ValueCellContents(
    support::ValueDomain domain, llvm::Value* cell) -> llvm::Value* {
  if (const std::optional<llvm::Value*> held = HeldAt(
          cell,
          {.kind = support::MemberStorageKind::kValueCell, .domain = domain})) {
    return *held;
  }
  const std::array<llvm::Value*, 1> args{cell};
  return CallEntry(
      RuntimeSymbol(domain, lir::ValueCellTarget::Op::kLoad),
      module_->Types().Ptr(), Addresses(1), args);
}

// Writing a place mirrors reading one, and a write through a wrapper is what
// gives the write its meaning -- waking whoever subscribed to a cell, or moving
// one driver's contribution and re-resolving the net it feeds -- which is why
// it is the wrapper that performs the write rather than a store to an address.
// What is stored crosses as a value the storage holds.
auto CodeGenFunction::LowerStore(const lir::StoreInstr& store)
    -> diag::Result<llvm::Value*> {
  auto wrapper = WrapperPlaceOf(store.place);
  if (!wrapper) {
    return std::unexpected(std::move(wrapper.error()));
  }
  const lir::TypeId stored = OperandType(store.value);
  // What a store is told is of the values its storage holds, which are of the
  // stored value's type.
  const ToldTypes told_types{.held = stored, .member = std::nullopt};
  // The store `symbol` names, made on the storage `args` holds as `arranged`
  // says it crosses, with the stored value crossing after it as a store whose
  // realization is told `told` reads the operand that follows the storage it
  // acts on.
  const auto stored_by =
      [&](const std::string& symbol, std::vector<ArgAbi> arranged,
          std::vector<llvm::Value*> args,
          const support::OperandReadings& readings, const Told& told,
          llvm::Value* value) -> diag::Result<llvm::Value*> {
    arranged.push_back(arranger_.ArgOf(readings, 1, stored, told, told_types));
    auto crossed = module_->BuildCallArg(arranged.back().mode, value, args);
    if (!crossed) {
      return std::unexpected(std::move(crossed.error()));
    }
    return CallEntry(
        symbol, module_->Types().Void(), std::move(arranged), args);
  };
  if (!wrapper->has_value()) {
    auto value = LowerOperand(store.value);
    if (!value) {
      return std::unexpected(std::move(value.error()));
    }
    auto address = ResolvePlaceAddress(store.place, Access::kWrite);
    if (!address) {
      return std::unexpected(std::move(address.error()));
    }
    if (const std::optional<support::ValueDomain> cell =
            PlaceValueCellDomain(store.place, stored)) {
      // A cell holding the value as its own bytes takes them where they lie.
      if (const std::optional<llvm::Value*> held = HeldAt(
              *address, {.kind = support::MemberStorageKind::kValueCell,
                         .domain = *cell})) {
        AssignValue(stored, *value, *held);
        return nullptr;
      }
      return stored_by(
          RuntimeSymbol(*cell, lir::ValueCellTarget::Op::kStore), Addresses(1),
          {*address}, OperandReadingsOf(lir::ValueCellTarget::Op::kStore),
          ToldOf(*cell, lir::ValueCellTarget::Op::kStore), *value);
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
      AssignValue(stored, *value, *address);
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
  // The wrapper and then the value, each as the wrapper's store reads it.
  const support::OperandReadings readings =
      support::RuntimeEntryOf(support::BuiltinFn::kStore).operands;
  const Told told =
      ToldOf(through.domain, through.kind, support::BuiltinFn::kStore);
  const lir::TypeId wrapper_type =
      ReachedType(store.place, std::ssize(store.place.chain) - 1);
  std::vector<ArgAbi> arranged{
      arranger_.ArgOf(readings, 0, wrapper_type, told, told_types)};
  std::vector<llvm::Value*> wrapper_args;
  auto wrapper_crossed =
      module_->BuildCallArg(arranged.front().mode, *address, wrapper_args);
  if (!wrapper_crossed) {
    return std::unexpected(std::move(wrapper_crossed.error()));
  }
  return stored_by(
      RuntimeSymbol(through.domain, through.kind, support::BuiltinFn::kStore),
      std::move(arranged), std::move(wrapper_args), readings, told, *value);
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

  // What a dereference of something of type `reached` at `holder` names. A
  // wrapper's contents are the wrapper's to answer for: read, they are where it
  // keeps them; written, they are reached through the write the wrapper
  // opened, which is what reports it. Anything else is the reference `value`
  // answers with, opened.
  const auto dereferenced =
      [&](lir::TypeId reached, llvm::Value* holder,
          const auto& value) -> diag::Result<llvm::Value*> {
    const std::optional<std::pair<WrapperKind, lir::TypeId>> wrapper =
        WrapperOf(module_->Unit().types.Get(reached));
    if (!wrapper.has_value()) {
      return OpenedReferent(value(), reached);
    }
    switch (access) {
      case Access::kRead:
        break;
      case Access::kWrite:
        throw InternalError(
            "llvm codegen: a write into what a wrapper holds is made through "
            "the write the wrapper opened -- please report this as a bug");
    }
    auto domain = DomainOf(wrapper->second);
    if (!domain) {
      return std::unexpected(std::move(domain.error()));
    }
    return ContentsOf(*domain, wrapper->first, holder);
  };

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
    const lir::TypeId base_type = OperandType(place.base);
    if (step == place.chain.end()) {
      address = OpenedReferent(*base, base_type);
    } else {
      auto named = dereferenced(
          base_type, *base, [&]() -> llvm::Value* { return *base; });
      if (!named) {
        return std::unexpected(std::move(named.error()));
      }
      address = *named;
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
            // Storage holding a reference holds it as an address to load.
            [&](const lir::DerefProjection&) -> diag::Result<llvm::Value*> {
              return dereferenced(reached, address, [&]() -> llvm::Value* {
                return builder_.CreateLoad(module_->Types().Ptr(), address);
              });
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
              const ElementEntries entries = ElementEntriesOf(*domain);
              const StepEntry& step = EntryFor(access, entries);
              std::vector<ArgAbi> arranged = Addresses(1);
              std::vector<llvm::Value*> args{address};
              auto filled = SelectorArgs(
                  support::RuntimeEntryOf(step.entry).operands,
                  element.coordinates, arranged, args);
              if (!filled) {
                return std::unexpected(std::move(filled.error()));
              }
              return CallEntry(
                  step.symbol, module_->Types().Ptr(), std::move(arranged),
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

auto CodeGenFunction::EntryFor(Access access, const ElementEntries& entries)
    -> const StepEntry& {
  switch (access) {
    case Access::kRead:
      return entries.read;
    case Access::kWrite:
      return entries.write;
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
  return CallEntry(*opening, module_->Types().Ptr(), Addresses(1), args);
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
        RuntimeSymbol(*domain, RuntimeOp::kCellRefer), Addresses(1), {storage},
        out);
  }
  return BuildInto(
      RuntimeSymbol(RuntimeOp::kReferStorage), Addresses(1), {storage}, out);
}

auto CodeGenFunction::IsHandleSequence(lir::TypeId type) const -> bool {
  return module_->Unit().types.Get(type).Is<lir::VectorType>();
}

auto CodeGenFunction::LowerBinary(
    const lir::BinaryInstr& binary, lir::TypeId result_type, llvm::Value* out)
    -> diag::Result<llvm::Value*> {
  const lir::TypeId operand_type = OperandType(binary.lhs);
  // A machine integer's operator is the machine instruction over one scalar.
  // What arrives this way is the words a synthesized transition computes over,
  // which stand for no value of the design at all.
  if (const std::optional<lir::Signedness> signedness =
          module_->Unit().types.Get(operand_type).MachineIntegerSignedness()) {
    return LowerMachineBinary(binary, *signedness);
  }
  // An address compared with another is a machine comparison too, as it is in
  // what clang makes of the same comparison.
  if (module_->Unit().types.Get(operand_type).Is<lir::PointerType>()) {
    return LowerMachineBinary(binary, lir::Signedness::kUnsigned);
  }
  auto lhs = LowerOperand(binary.lhs);
  if (!lhs) {
    return std::unexpected(std::move(lhs.error()));
  }
  auto rhs = LowerOperand(binary.rhs);
  if (!rhs) {
    return std::unexpected(std::move(rhs.error()));
  }
  if (module_->Types().IntegralShapeOf(operand_type).has_value()) {
    const std::array<OperandRef, 2> operands{
        OperandRef{.value = *lhs, .type = operand_type},
        OperandRef{.value = *rhs, .type = OperandType(binary.rhs)}};
    return LowerIntegral(IntegralOpOf(binary.op), operands, result_type, out);
  }
  auto domain = DomainOf(operand_type);
  if (!domain) {
    return std::unexpected(std::move(domain.error()));
  }
  const std::string entry = RuntimeSymbol(*domain, binary.op);
  switch (lir::BinaryAnswerOf(binary.op)) {
    case lir::BinaryAnswer::kOperandType:
      return BuildInto(entry, Addresses(2), {*lhs, *rhs}, out);
    // The entry answers the scalar, which is held here as a value of the type
    // the instruction states.
    case lir::BinaryAnswer::kOneBit: {
      const std::optional<value::IntegralShape> answer =
          module_->Types().IntegralShapeOf(result_type);
      if (!answer.has_value()) {
        throw InternalError(
            "llvm codegen: a comparison is stated at no integral type -- "
            "please report this as a bug");
      }
      const std::array<llvm::Value*, 2> args{*lhs, *rhs};
      StoreComparisonAnswer(
          builder_, CallEntry(entry, builder_.getInt8Ty(), Addresses(2), args),
          out, *answer);
      return out;
    }
  }
  throw InternalError("llvm codegen: unknown binary answer");
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

auto CodeGenFunction::LowerUnary(
    const lir::UnaryInstr& unary, lir::TypeId result_type, llvm::Value* out)
    -> diag::Result<llvm::Value*> {
  const lir::TypeId operand_type = OperandType(unary.operand);
  // A machine integer's operator is the machine instruction over one scalar.
  // This is how the reduced predicate a real- or chandle-family `!` produces
  // is negated before it is widened back to a one-bit integral value.
  if (module_->Unit()
          .types.Get(operand_type)
          .MachineIntegerSignedness()
          .has_value()) {
    return LowerMachineUnary(unary);
  }
  auto operand = LowerOperand(unary.operand);
  if (!operand) {
    return std::unexpected(std::move(operand.error()));
  }
  if (module_->Types().IntegralShapeOf(operand_type).has_value()) {
    const std::array<OperandRef, 1> operands{
        OperandRef{.value = *operand, .type = operand_type}};
    return LowerIntegral(IntegralOpOf(unary.op), operands, result_type, out);
  }
  auto domain = DomainOf(operand_type);
  if (!domain) {
    return std::unexpected(std::move(domain.error()));
  }
  return BuildInto(
      RuntimeSymbol(*domain, unary.op), Addresses(1), {*operand}, out);
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
  // One integral type read as another is the same bytes only where the two
  // lay a value out alike, which is what a cast between them states; a
  // difference in width or in states is a conversion, which is an operation.
  const std::optional<value::IntegralShape> read_from =
      module_->Types().IntegralShapeOf(operand_type);
  const std::optional<value::IntegralShape> read_as =
      module_->Types().IntegralShapeOf(result_type);
  if (read_from.has_value() && read_as.has_value() &&
      (read_from->width != read_as->width ||
       read_from->domain != read_as->domain)) {
    throw InternalError(
        "llvm codegen: a cast between integral types laid out differently -- "
        "please report this as a bug");
  }
  llvm::Type* target = module_->Types().Map(result_type);
  if ((*operand)->getType() == target) {
    return *operand;
  }
  if (module_->Unit().types.Get(result_type).Is<lir::MachineBoolType>()) {
    // Reducing a machine integer to a predicate is a machine comparison
    // against zero. What arrives this way is a runtime entry's plain answer,
    // which stands for no value of the design and so belongs to no domain that
    // could name an entry.
    if (module_->Unit().types.Get(operand_type).MachineIntegerSignedness()) {
      return builder_.CreateICmpNE(
          *operand, llvm::ConstantInt::get((*operand)->getType(), 0));
    }
    if (read_from.has_value()) {
      const std::array<OperandRef, 1> operands{
          OperandRef{.value = *operand, .type = operand_type}};
      return LowerIntegral(
          support::IntegralOp::kIsTrue, operands, result_type, nullptr);
    }
    auto domain = DomainOf(operand_type);
    if (!domain) {
      return std::unexpected(std::move(domain.error()));
    }
    const std::array<llvm::Value*, 1> args{*operand};
    return CallEntry(
        RuntimeSymbol(*domain, RuntimeOp::kToBool), target, Addresses(1), args);
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
  auto found = IfNotNull(
      HandleView(*handle),
      [&](llvm::Value* view) -> diag::Result<llvm::Value*> {
        const std::array<llvm::Value*, 4> args{
            view, module_->TypeInfoOf(*from), module_->TypeInfoOf(*to),
            llvm::ConstantInt::getSigned(builder_.getInt64Ty(), kNoCastHint)};
        return builder_.CreateCall(module_->DynamicCast(), args);
      });
  if (!found) {
    return found;
  }
  return HandleWithView(*handle, *found, out);
}

auto CodeGenFunction::HandleView(llvm::Value* handle) -> llvm::Value* {
  const std::array<llvm::Value*, 1> args{handle};
  return CallEntry(
      RuntimeSymbol(RuntimeOp::kHandleView), module_->Types().Ptr(),
      Addresses(1), args);
}

auto CodeGenFunction::HandleWithView(
    llvm::Value* handle, llvm::Value* view, llvm::Value* out) -> llvm::Value* {
  return BuildInto(
      RuntimeSymbol(RuntimeOp::kHandleWithView), Addresses(2), {handle, view},
      out);
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
  return CallEntry(
      RuntimeSymbol(support::BuiltinFn::kReceiveDeparture),
      module_->Types().Ptr(), Addresses(1), carried);
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
  auto abi = arranger_.FnAbiOf(call, result_type);
  if (!abi) {
    return std::unexpected(std::move(abi.error()));
  }
  auto args = module_->BuildCallArgs(*abi, operands);
  if (!args) {
    return std::unexpected(std::move(args.error()));
  }
  // Storage for what the callee builds goes last, whichever kind of callee it
  // is: an entry of the library and a body of this program take it alike. The
  // library holds no tuple type of its own, so storage given for a tuple says
  // which tuple it is before anything is built there.
  const bool builds_its_answer = std::visit(
      Overloaded{
          [](const ReturnDirect&) { return false; },
          [](const ReturnIndirect&) { return true; }},
      abi->ret);
  if (builds_its_answer != (out != nullptr)) {
    throw InternalError(
        "llvm codegen: a call is given storage for its answer exactly where "
        "it is arranged to build one -- please report this as a bug");
  }
  if (builds_its_answer) {
    if (module_->Unit().types.Get(result_type).IsProduct()) {
      builder_.CreateStore(module_->Tuples().TypeOf(result_type), out);
    }
    args->push_back(out);
  }
  auto callee = ResolveCallee(call, result_type, *abi, *args);
  if (!callee) {
    return std::unexpected(std::move(callee.error()));
  }
  return ResolvedCall{.callee = *callee, .args = *std::move(args)};
}

auto CodeGenFunction::LowerOperandRefs(std::span<const lir::Operand> operands)
    -> diag::Result<std::vector<OperandRef>> {
  std::vector<OperandRef> lowered;
  lowered.reserve(operands.size());
  for (const lir::Operand& operand : operands) {
    auto value = LowerOperand(operand);
    if (!value) {
      return std::unexpected(std::move(value.error()));
    }
    lowered.push_back(
        OperandRef{.value = *value, .type = OperandType(operand)});
  }
  return lowered;
}

auto CodeGenFunction::LowerIntegralCall(
    support::IntegralOp op, const lir::BuiltinTarget& target,
    const lir::CallInstr& call, lir::TypeId result_type, llvm::Value* out)
    -> diag::Result<llvm::Value*> {
  auto operands = LowerOperandRefs(call.args);
  if (!operands) {
    return std::unexpected(std::move(operands.error()));
  }
  // An operation generic over the type it answers at is called at that type,
  // and any other answers at the type its operands settle, which the call has.
  if (!support::RuntimeEntryOf(target.fn).takes_a_type_argument) {
    return LowerIntegral(op, *operands, result_type, out);
  }
  if (!target.type_argument.has_value()) {
    throw InternalError(
        "llvm codegen: an entry generic over the type it answers at is "
        "called at none -- please report this as a bug");
  }
  return LowerIntegral(op, *operands, *target.type_argument, out);
}

auto CodeGenFunction::EmitCallOrInvoke(
    llvm::FunctionCallee callee, std::span<llvm::Value* const> args)
    -> llvm::Value* {
  if (invoke_dest_.has_value()) {
    return builder_.CreateInvoke(
        callee, invoke_dest_->normal, invoke_dest_->unwind, args);
  }
  return builder_.CreateCall(callee, args);
}

auto CodeGenFunction::LowerCall(
    const lir::CallInstr& call, lir::TypeId result_type, llvm::Value* out)
    -> diag::Result<llvm::Value*> {
  using Lowered = diag::Result<llvm::Value*>;
  // The call made on what the target names, which builds what it answers with
  // in `into` where it is given storage.
  const auto called_into = [&](llvm::Value* into) -> Lowered {
    auto resolved = ResolveCall(call, result_type, into);
    if (!resolved) {
      return std::unexpected(std::move(resolved.error()));
    }
    llvm::Value* const answered =
        EmitCallOrInvoke(resolved->callee, resolved->args);
    return into != nullptr ? into : answered;
  };
  const auto called = [&](const auto&) -> Lowered { return called_into(out); };
  return std::visit(
      Overloaded{
          // An operation over integral values is the few instructions it is
          // on one word per plane, or a call on the library's entry.
          [&](const lir::BuiltinTarget& builtin) -> Lowered {
            const std::optional<support::IntegralOp> integral =
                support::RuntimeEntryOf(builtin.fn).integral;
            if (!integral.has_value()) {
              return called(builtin);
            }
            return LowerIntegralCall(
                *integral, builtin, call, result_type, out);
          },
          // Ending a value whose storage going away is the whole of its end.
          [&](const lir::EndValueTarget& end) -> Lowered {
            if (!module_->Types()
                     .StorageOf(OperandType(call.args.at(0)))
                     .ends_with_nothing_to_do) {
              return called(end);
            }
            return nullptr;
          },
          // A copy of an integral value is a copy of its bytes.
          [&](const lir::CopyValueTarget& copy) -> Lowered {
            if (!module_->Types().IntegralShapeOf(result_type).has_value()) {
              return called(copy);
            }
            auto copied = LowerOperand(call.args.at(0));
            if (!copied) {
              return std::unexpected(std::move(copied.error()));
            }
            return CopyValue(result_type, *copied, out);
          },
          // A cell holding a value as its own bytes is read and written where
          // they lie; only one the activation owns is asked of the library.
          [&](const lir::ValueCellTarget& cell) -> Lowered {
            auto held_in = DomainOf(cell.value);
            if (!held_in) {
              return std::unexpected(std::move(held_in.error()));
            }
            const support::ValueDomain domain = *held_in;
            if (!support::IsIntegralLayout(domain)) {
              return called(cell);
            }
            switch (cell.op) {
              case lir::ValueCellTarget::Op::kAllocate:
                return called(cell);
              case lir::ValueCellTarget::Op::kLoad: {
                auto handle = LowerOperand(call.args.at(0));
                if (!handle) {
                  return std::unexpected(std::move(handle.error()));
                }
                return ValueCellContents(domain, *handle);
              }
              case lir::ValueCellTarget::Op::kStore: {
                auto handle = LowerOperand(call.args.at(0));
                if (!handle) {
                  return std::unexpected(std::move(handle.error()));
                }
                auto stored = LowerOperand(call.args.at(1));
                if (!stored) {
                  return std::unexpected(std::move(stored.error()));
                }
                AssignValue(
                    cell.value, *stored, ValueCellContents(domain, *handle));
                return nullptr;
              }
            }
            throw InternalError("llvm codegen: unknown value cell operation");
          },
          // Bits of a designated integral value are read, and placed, by the
          // operation over integral values, applied to the value where the
          // designation says it lies (LRM 11.5.1).
          [&](const lir::DesignatedBitsTarget& bits) -> Lowered {
            const auto over_the_designated_value =
                [&](support::IntegralOp op) -> Lowered {
              auto operands = LowerOperandRefs(call.args);
              if (!operands) {
                return std::unexpected(std::move(operands.error()));
              }
              operands->front() = OperandRef{
                  .value = DesignatedPart(operands->front().value),
                  .type = bits.value};
              return LowerIntegral(op, *operands, result_type, out);
            };
            switch (bits.op) {
              case lir::DesignatedBitsTarget::Op::kRead:
                return over_the_designated_value(support::IntegralOp::kSlice);
              case lir::DesignatedBitsTarget::Op::kPlace:
                return over_the_designated_value(
                    support::IntegralOp::kWithSlice);
              case lir::DesignatedBitsTarget::Op::kReport:
                return called(bits);
            }
            throw InternalError(
                "llvm codegen: unknown designated-bits operation");
          },
          [&](const lir::OpenWriteTarget& t) { return called(t); },
          // Entering a body builds the execution it becomes. Nothing ends that
          // here: whoever drives it takes it out of the storage, the moment it
          // is made.
          [&](const lir::CoroutineTarget& coroutine) -> Lowered {
            switch (coroutine.op) {
              case lir::CoroutineTarget::Op::kEnterBorrowedEnvironment:
              case lir::CoroutineTarget::Op::kEnterOwnedEnvironment:
                return called_into(
                    ObjectStorage(support::LibraryObject::kExecution));
              case lir::CoroutineTarget::Op::kAwait:
              case lir::CoroutineTarget::Op::kRelease:
                return called(coroutine);
            }
            throw InternalError("llvm codegen: unknown coroutine operation");
          },
          [&](const lir::FunctionTarget& t) { return called(t); },
          [&](const lir::IndirectTarget& t) { return called(t); },
          [&](const lir::DispatchTarget& t) { return called(t); },
          [&](const lir::ConstructTarget& t) { return called(t); },
          [&](const lir::LibraryConstructorTarget& t) { return called(t); },
          [&](const lir::SymbolTarget& t) { return called(t); },
          [&](const lir::ForeignTarget& t) { return called(t); },
          [&](const lir::ControlEffectTarget& t) { return called(t); }},
      call.target);
}

auto CodeGenFunction::DesignatedPart(llvm::Value* designation) -> llvm::Value* {
  return builder_.CreateLoad(
      module_->Types().Ptr(),
      builder_.CreateConstInBoundsGEP1_64(
          builder_.getInt8Ty(), designation, runtime::DesignatedPartAt()));
}

// Every call is a symbol invoked with arguments; the target kinds differ only
// in how the symbol is resolved. A callee this module does not define is
// declared at the type the call's arrangement has.
auto CodeGenFunction::ResolveCallee(
    const lir::CallInstr& call, lir::TypeId result_type, const FnAbi& abi,
    std::span<llvm::Value*> args) -> diag::Result<llvm::FunctionCallee> {
  return std::visit(
      Overloaded{
          [&](const lir::BuiltinTarget& t)
              -> diag::Result<llvm::FunctionCallee> {
            auto entry = arranger_.EntryOf(t, call, result_type);
            if (!entry) {
              return std::unexpected(std::move(entry.error()));
            }
            return module_->RuntimeFunction(entry->symbol, abi);
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
                module_->Types().GetFunctionType(abi), *callee);
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
                module_->Types().GetFunctionType(abi), body);
          },
          [&](const lir::ConstructTarget&)
              -> diag::Result<llvm::FunctionCallee> {
            auto constructor = arranger_.ConstructorOf(call, result_type);
            if (!constructor) {
              return std::unexpected(std::move(constructor.error()));
            }
            return module_->RuntimeFunction(*constructor, abi);
          },
          [&](const lir::LibraryConstructorTarget& t)
              -> diag::Result<llvm::FunctionCallee> {
            return module_->RuntimeFunction(
                runtime::BaseObjectConstructorSymbolOf(t.cls), abi);
          },
          // A body another artifact defines is declared here and resolved by
          // the host, and its operands are this program's own values.
          [&](const lir::SymbolTarget& t)
              -> diag::Result<llvm::FunctionCallee> {
            return module_->RuntimeFunction(t.symbol, abi);
          },
          // A foreign symbol is declared, never defined: the host resolves it.
          // The boundary already marshaled its operands and result to the
          // carriers the foreign side declared (LRM 35.5.6), so what crosses is
          // what the entry takes.
          [&](const lir::ForeignTarget& t)
              -> diag::Result<llvm::FunctionCallee> {
            return module_->RuntimeFunction(t.symbol, abi);
          },
          [&](const lir::ValueCellTarget& t)
              -> diag::Result<llvm::FunctionCallee> {
            auto domain = DomainOf(t.value);
            if (!domain) {
              return std::unexpected(std::move(domain.error()));
            }
            return module_->RuntimeFunction(RuntimeSymbol(*domain, t.op), abi);
          },
          [&](const lir::OpenWriteTarget& t)
              -> diag::Result<llvm::FunctionCallee> {
            auto domain = DomainOf(t.value);
            if (!domain) {
              return std::unexpected(std::move(domain.error()));
            }
            return module_->RuntimeFunction(RuntimeSymbol(*domain, t.op), abi);
          },
          [&](const lir::DesignatedBitsTarget& t)
              -> diag::Result<llvm::FunctionCallee> {
            auto domain = DomainOf(t.value);
            if (!domain) {
              return std::unexpected(std::move(domain.error()));
            }
            return module_->RuntimeFunction(RuntimeSymbol(*domain, t.op), abi);
          },
          [&](const lir::EndValueTarget&)
              -> diag::Result<llvm::FunctionCallee> {
            return OwnedCallee(
                OperandType(call.args.at(0)), TupleLifecycle::kDestroy);
          },
          [&](const lir::CopyValueTarget&)
              -> diag::Result<llvm::FunctionCallee> {
            return OwnedCallee(result_type, TupleLifecycle::kCopy);
          },
          [&](const lir::ControlEffectTarget& t)
              -> diag::Result<llvm::FunctionCallee> {
            return module_->RuntimeFunction(RuntimeSymbol(t.op), abi);
          },
          [&](const lir::CoroutineTarget& t)
              -> diag::Result<llvm::FunctionCallee> {
            return module_->RuntimeFunction(RuntimeSymbol(t.op), abi);
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
    llvm::Value* const address = ComponentAddress(result_type, out, i);
    // A component standing where an index of a wildcard-indexed array goes
    // has no type of its own (LRM 7.8.1), so the integral value put there is
    // kept with the type it was written in, as the array keeps an index.
    const lir::TypeId stated = OperandType(tuple.components[i]);
    if (module_->Unit()
            .types.Get(layout.components[i])
            .Is<lir::WildcardIndexType>() &&
        module_->Types().IntegralShapeOf(stated).has_value()) {
      std::vector<ArgAbi> arranged{arranger_.ArgOf(
          OperandReadingsOf(RuntimeOp::kMakeWildcardIndex), 0, stated,
          ToldNothing{}, {})};
      std::vector<llvm::Value*> kept;
      auto crossed =
          module_->BuildCallArg(arranged.front().mode, *component, kept);
      if (!crossed) {
        return std::unexpected(std::move(crossed.error()));
      }
      BuildInto(
          RuntimeSymbol(RuntimeOp::kMakeWildcardIndex), std::move(arranged),
          std::move(kept), address);
      continue;
    }
    CopyValue(layout.components[i], *component, address);
  }
  return out;
}

auto CodeGenFunction::SelectorArgs(
    const support::OperandReadings& readings,
    std::span<const lir::Operand> operands, std::vector<ArgAbi>& arranged,
    std::vector<llvm::Value*>& args) -> diag::Result<void> {
  for (std::size_t i = 0; i < operands.size(); ++i) {
    auto lowered = LowerOperand(operands[i]);
    if (!lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
    arranged.push_back(arranger_.ArgOf(
        readings, i + 1, OperandType(operands[i]), ToldNothing{}, {}));
    auto crossed = module_->BuildCallArg(arranged.back().mode, *lowered, args);
    if (!crossed) {
      return std::unexpected(std::move(crossed.error()));
    }
  }
  return {};
}

auto CodeGenFunction::IntegralPosition(const lir::AggregateSelector& selector)
    -> diag::Result<OperandRef> {
  const auto* bits = std::get_if<lir::ContainerSlice>(&selector);
  if (bits == nullptr || bits->operands.size() != 1) {
    throw InternalError(
        "llvm codegen: a part of an integral value is the bits one position "
        "names -- please report this as a bug");
  }
  auto position = LowerOperand(bits->operands.front());
  if (!position) {
    return std::unexpected(std::move(position.error()));
  }
  return OperandRef{
      .value = *position, .type = OperandType(bits->operands.front())};
}

auto CodeGenFunction::LowerAggregateExtract(
    const lir::AggregateExtractInstr& extract, lir::TypeId result_type,
    llvm::Value* out) -> diag::Result<llvm::Value*> {
  auto aggregate = LowerOperand(extract.aggregate);
  if (!aggregate) {
    return std::unexpected(std::move(aggregate.error()));
  }
  const lir::TypeId container = OperandType(extract.aggregate);
  if (module_->Types().IntegralShapeOf(container).has_value()) {
    auto position = IntegralPosition(extract.selector);
    if (!position) {
      return std::unexpected(std::move(position.error()));
    }
    const std::array<OperandRef, 2> operands{
        OperandRef{.value = *aggregate, .type = container}, *position};
    return LowerIntegral(
        support::IntegralOp::kSlice, operands, result_type, out);
  }
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
    return CallEntry(
        RuntimeSymbol(RuntimeOp::kSequenceElement), module_->Types().Ptr(),
        {CallArranger::Direct(module_->Types().Ptr()),
         CallArranger::Direct(
             module_->Types().Map(OperandType(element->operands.front())))},
        args);
  }
  auto domain = DomainOf(container);
  if (!domain) {
    return std::unexpected(std::move(domain.error()));
  }
  // The part an entry reads out of the aggregate, built in `out`.
  const auto read_by = [&](const StepEntry& step,
                           const std::vector<lir::Operand>& operands)
      -> diag::Result<llvm::Value*> {
    std::vector<ArgAbi> arranged = Addresses(1);
    std::vector<llvm::Value*> shape{*aggregate};
    auto filled = SelectorArgs(
        support::RuntimeEntryOf(step.entry).operands, operands, arranged,
        shape);
    if (!filled) {
      return std::unexpected(std::move(filled.error()));
    }
    return BuildInto(step.symbol, std::move(arranged), std::move(shape), out);
  };
  // A member of an active-member value is named by its index alone, and the
  // aggregate's own domain is what names the entry that answers.
  const auto positional = [&](base::ComponentIndex index) -> llvm::Value* {
    return BuildInto(
        RuntimeSymbol(*domain, support::BuiltinFn::kComponent),
        {CallArranger::Direct(module_->Types().Ptr()),
         CallArranger::Direct(builder_.getInt64Ty())},
        {*aggregate, builder_.getInt64(index.value)}, out);
  };
  return std::visit(
      Overloaded{
          [&](const lir::Component& component) -> diag::Result<llvm::Value*> {
            return positional(component.index);
          },
          [&](const lir::ContainerElement& e) -> diag::Result<llvm::Value*> {
            return read_by(ElementEntriesOf(*domain).read, e.operands);
          },
          [&](const lir::ContainerSlice& s) -> diag::Result<llvm::Value*> {
            return read_by(
                StepEntry{
                    .entry = support::BuiltinFn::kElementSlice,
                    .symbol = RuntimeSymbol(
                        *domain, support::BuiltinFn::kElementSlice)},
                s.operands);
          }},
      extract.selector);
}

auto CodeGenFunction::LowerAggregateUpdate(
    const lir::AggregateUpdateInstr& update, lir::TypeId result_type,
    llvm::Value* out) -> diag::Result<llvm::Value*> {
  auto aggregate = LowerOperand(update.aggregate);
  if (!aggregate) {
    return std::unexpected(std::move(aggregate.error()));
  }
  auto replacement = LowerOperand(update.replacement);
  if (!replacement) {
    return std::unexpected(std::move(replacement.error()));
  }
  const lir::TypeId container = OperandType(update.aggregate);
  const lir::TypeId replacement_type = OperandType(update.replacement);
  if (module_->Types().IntegralShapeOf(container).has_value()) {
    auto position = IntegralPosition(update.selector);
    if (!position) {
      return std::unexpected(std::move(position.error()));
    }
    const std::array<OperandRef, 3> operands{
        OperandRef{.value = *aggregate, .type = container}, *position,
        OperandRef{.value = *replacement, .type = replacement_type}};
    return LowerIntegral(
        support::IntegralOp::kWithSlice, operands, result_type, out);
  }
  auto domain = DomainOf(container);
  if (!domain) {
    return std::unexpected(std::move(domain.error()));
  }
  // The aggregate with one part replaced, built in `out` by the entry `op`
  // names in the aggregate's domain. That entry reads the aggregate, what
  // names the part, and then what takes its place, which `args` holds all but
  // the last of as `named_by` operands, crossing as `arranged` says.
  const auto replaced =
      [&](RuntimeOp op, const ToldTypes& told_types,
          std::vector<ArgAbi> arranged, std::vector<llvm::Value*> args,
          std::size_t named_by) -> diag::Result<llvm::Value*> {
    arranged.push_back(arranger_.ArgOf(
        OperandReadingsOf(op), 1 + named_by, replacement_type,
        ToldOf(*domain, op), told_types));
    auto crossed =
        module_->BuildCallArg(arranged.back().mode, *replacement, args);
    if (!crossed) {
      return std::unexpected(std::move(crossed.error()));
    }
    return BuildInto(
        RuntimeSymbol(*domain, op), std::move(arranged), std::move(args), out);
  };
  // The same where the part is named by operands the program computes.
  const auto replaced_at = [&](RuntimeOp op,
                               const std::vector<lir::Operand>& operands)
      -> diag::Result<llvm::Value*> {
    std::vector<ArgAbi> arranged = Addresses(1);
    std::vector<llvm::Value*> args{*aggregate};
    auto filled = SelectorArgs(OperandReadingsOf(op), operands, arranged, args);
    if (!filled) {
      return std::unexpected(std::move(filled.error()));
    }
    return replaced(
        op, {}, std::move(arranged), std::move(args), operands.size());
  };
  return std::visit(
      Overloaded{
          // A member is replaced by naming its index and the value that takes
          // its place. Whether the write then makes the member live or faults
          // a mismatched tag follows from the domain the entry is named in.
          [&](const lir::Component& component) -> diag::Result<llvm::Value*> {
            return replaced(
                RuntimeOp::kWithComponent,
                ToldTypes{
                    .held = std::nullopt,
                    .member = UnionMemberType(
                        module_->Unit(), container, component.index.value)},
                {CallArranger::Direct(module_->Types().Ptr()),
                 CallArranger::Direct(builder_.getInt64Ty())},
                {*aggregate, builder_.getInt64(component.index.value)}, 1);
          },
          [&](const lir::ContainerElement& e) -> diag::Result<llvm::Value*> {
            return replaced_at(RuntimeOp::kWithElement, e.operands);
          },
          [&](const lir::ContainerSlice& s) -> diag::Result<llvm::Value*> {
            return replaced_at(RuntimeOp::kWithSlice, s.operands);
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
          // A member table is data of the module, named by its address.
          [&](const lir::EnumTableRef& c) -> diag::Result<llvm::Value*> {
            return module_->GetAddrOfEnumTable(c.table);
          },
          // A constant's bytes are data of the module, and a value is held by
          // where its bytes lie, so the constant is that data's address.
          [&](const lir::IntegralConstantRef& c) -> diag::Result<llvm::Value*> {
            return module_->GetAddrOfIntegralConstant(c.constant);
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

// A machine integer's constant is a native LLVM constant. A constant of an
// integral type of the design is data the unit states, reached by reference.
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
        RuntimeSymbol(*domain, RuntimeOp::kDefault), {}, {},
        ObjectStorage(*domain));
  }
  return llvm::ConstantPointerNull::get(
      llvm::cast<llvm::PointerType>(module_->Types().Map(constant.type)));
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
  // The entry builds what owns the closure in `out` and answers the closure
  // itself, which is where its captures are laid out.
  const std::array<llvm::Value*, 2> made{*definition, out};
  llvm::Value* closure = builder_.CreateCall(
      module_->RuntimeFunction(
          RuntimeSymbol(RuntimeOp::kClosureMake),
          CallArranger::Arrange(
              Addresses(1),
              ReturnIndirect{.returned = module_->Types().Ptr()})),
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

}  // namespace lyra::backend::llvm_backend
