#include <array>
#include <cstddef>
#include <cstdint>
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
#include "lyra/backend/llvm/integral_planes.hpp"
#include "lyra/backend/llvm/runtime_entry.hpp"
#include "lyra/base/component_index.hpp"
#include "lyra/base/diag_message.hpp"
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

template <typename... Args>
auto Unsupported(base::DiagFormatFor<Args...> format, Args&&... args)
    -> std::unexpected<diag::Diagnostic> {
  return diag::Fail(
      diag::DiagCode::kUnsupportedExpressionForm, format,
      std::forward<Args>(args)...);
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

}  // namespace

auto CodeGenFunction::LowerInstrInto(const lir::Instr& instr)
    -> diag::Result<void> {
  using Done = diag::Result<void>;
  // A statement's lowering answers no value, so only whether it failed is
  // kept.
  const auto done = [](diag::Result<llvm::Value*> lowered) -> Done {
    if (!lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
    return {};
  };
  return std::visit(
      Overloaded{
          [&](const lir::AssignInstr& assign) -> Done {
            return LowerAssignInto(assign);
          },
          [&](const lir::StoreInstr& store) -> Done {
            return done(LowerStore(store));
          },
          [&](const lir::BitsStoreInstr& store) -> Done {
            return done(LowerBitsStore(store));
          },
          // Bytes a frame keeps for a value come into use and go out of it
          // with nothing to do where they lie in the frame.
          [](const lir::StorageInstr&) -> Done { return {}; },
          [&](const lir::EndVariableInstr& ended) -> Done {
            return done(LowerEndVariable(ended));
          },
          [&](const lir::BeginHandlingInstr& begun) -> Done {
            return done(LowerBeginHandling(begun));
          },
          [&](const lir::EndHandlingInstr&) -> Done {
            return done(LowerEndHandling());
          }},
      instr.data);
}

auto CodeGenFunction::LowerAssignInto(const lir::AssignInstr& assign)
    -> diag::Result<void> {
  auto destination = ResolveDestination(assign.destination);
  if (!destination) {
    return std::unexpected(std::move(destination.error()));
  }
  auto made = LowerRvalue(assign.value, *destination);
  if (!made) {
    return std::unexpected(std::move(made.error()));
  }
  StoreInto(*destination, *made, builder_);
  return {};
}

auto CodeGenFunction::LowerRvalue(
    const lir::Rvalue& value, const DestinationRef& destination)
    -> diag::Result<llvm::Value*> {
  using Lowered = diag::Result<llvm::Value*>;
  const lir::TypeId result_type = TypeOf(destination);
  return std::visit(
      Overloaded{
          [&](const lir::CallInstr& call) -> Lowered {
            return LowerCall(call, destination);
          },
          [&](const lir::UseInstr& use) -> Lowered {
            return LowerUse(use, destination);
          },
          [&](const lir::LandingInstr& landing) -> Lowered {
            return LowerLanding(landing);
          },
          [&](const lir::ProductInstr& product) -> Lowered {
            return LowerProduct(
                product, result_type, EnsureDestination(destination));
          },
          [&](const lir::UnionInstr& built) -> Lowered {
            return LowerUnion(
                built, result_type, EnsureDestination(destination));
          },
          [&](const lir::ClosureInstr& built) -> Lowered {
            return LowerClosure(
                built, result_type, EnsureDestination(destination));
          },
          [&](const lir::ArrayInstr& array) -> Lowered {
            return LowerArray(array, destination);
          },
          [&](const lir::VectorGetInstr& get) -> Lowered {
            return LowerVectorGet(get);
          },
          [&](const lir::DiscriminantInstr& read) -> Lowered {
            return LowerDiscriminant(read, result_type);
          },
          [&](const lir::LoadInstr& load) -> Lowered {
            return LoadPlace(load.place, result_type);
          },
          [&](const lir::OverlaidLoadInstr& load) -> Lowered {
            return LowerOverlaidLoad(
                load, result_type, EnsureDestination(destination));
          },
          [&](const lir::AddrOfInstr& addr) -> Lowered {
            return LowerAddrOf(
                addr, result_type, EnsureDestination(destination));
          },
          [&](const lir::BinaryInstr& binary) -> Lowered {
            return LowerBinary(
                binary, result_type, EnsureDestination(destination));
          },
          [&](const lir::UnaryInstr& unary) -> Lowered {
            return LowerUnary(
                unary, result_type, EnsureDestination(destination));
          },
          [&](const lir::CastInstr& cast) -> Lowered {
            return LowerCast(cast, result_type);
          },
          [&](const lir::VariableStorageInstr&) -> Lowered {
            return LowerVariableStorage();
          },
          [&](const lir::BeginVariableInstr& begun) -> Lowered {
            return LowerBeginVariable(begun);
          }},
      value);
}

// Reading a place answers with the value it holds, where it lies, whatever
// storage that is: a cell's contents are asked of the cell, and anything else
// is its own address. A value of a type held as itself is read out of that
// address.
auto CodeGenFunction::LoadPlace(
    const lir::Place& place, lir::TypeId result_type)
    -> diag::Result<llvm::Value*> {
  llvm::Type* const result = module_->Types().Map(result_type);
  auto wrapper = WrapperPlaceOf(place);
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
  auto address = ResolvePlaceAddress(place, Access::kRead);
  if (!address) {
    return std::unexpected(std::move(address.error()));
  }
  if (const std::optional<support::ValueDomain> cell =
          PlaceValueCellDomain(place, result_type)) {
    return ValueCellContents(*cell, *address);
  }
  if (module_->Types().HasAggregateEvaluationKind(result_type)) {
    return *address;
  }
  if (module_->Types().IsReadAsView(result_type)) {
    return ViewOver(
        *address, module_->Unit()
                      .types.Get(result_type)
                      .Get<lir::MachineArrayType>()
                      .size);
  }
  return builder_.CreateLoad(result, *address);
}

// A value lies at an offset in what holds it, or is reached through the
// address what holds it keeps at one.
auto CodeGenFunction::HeldAt(
    llvm::Value* storage, support::DeclaredMemberStorage held)
    -> std::optional<llvm::Value*> {
  const auto at = [&](std::uint64_t offset) -> llvm::Value* {
    return builder_.CreateConstInBoundsGEP1_64(
        builder_.getInt8Ty(), storage, offset);
  };
  return runtime::HeldContentsOf(held).transform(
      [&](const runtime::HeldContents& contents) -> llvm::Value* {
        return std::visit(
            Overloaded{
                [&](const runtime::ContentsAt& lies) {
                  return at(lies.offset);
                },
                [&](const runtime::ContentsIndirectAt& kept) -> llvm::Value* {
                  return builder_.CreateLoad(
                      module_->Types().Ptr(), at(kept.offset));
                }},
            contents);
      });
}

// A cell is read where the library states its value lies, at an offset or
// through the address it keeps, and a net through the address it keeps of
// what it shows, since that is its own value or another object's. Only one
// keeping an object of the library's own hands its contents out through its
// own access.
auto CodeGenFunction::ContentsOf(
    support::ValueDomain domain, WrapperKind kind, llvm::Value* wrapper)
    -> llvm::Value* {
  switch (kind) {
    case WrapperKind::kCell:
      if (const std::optional<llvm::Value*> held = HeldAt(
              wrapper, {.kind = support::MemberStorageKind::kObservableCell,
                        .domain = domain})) {
        return *held;
      }
      break;
    case WrapperKind::kNet:
      if (const std::optional<llvm::Value*> held = HeldAt(
              wrapper, {.kind = support::MemberStorageKind::kResolvedNet,
                        .domain = domain})) {
        return *held;
      }
      break;
    case WrapperKind::kDriver:
    case WrapperKind::kRef:
      break;
  }
  const std::array<llvm::Value*, 1> args{wrapper};
  return CallEntry(
      RuntimeSymbol(domain, kind, support::BuiltinFn::kGet),
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
// gives the write its meaning -- waking whoever subscribed to a cell, or giving
// one driver's value to the net it feeds, which shows what its drivers then
// determine -- which is why it is the wrapper that performs the write rather
// than a store to an address.
// What is stored crosses as a value the storage holds.
auto CodeGenFunction::LowerStore(const lir::StoreInstr& store)
    -> diag::Result<llvm::Value*> {
  auto wrapper = WrapperPlaceOf(store.place);
  if (!wrapper) {
    return std::unexpected(std::move(wrapper.error()));
  }
  // What a store is told is of the values its storage holds, which are of the
  // stored value's type.
  const lir::TypeId stored = OperandType(store.value);
  // The store `symbol` names, made on `storage`, stated as `storage_operand`,
  // with the stored value crossing after it, each as `readings` reads it and
  // as the realization for the stored value's domain told `told` is told.
  const auto stored_by =
      [&](const std::string& symbol, const support::OperandReadings& readings,
          const Told& told, StatedOperand storage_operand, llvm::Value* storage,
          llvm::Value* value) -> diag::Result<llvm::Value*> {
    const std::array<StatedOperand, 2> operands{
        storage_operand, ValueOperand{.type = stored}};
    const FnAbi abi = ArrangeCall(
        *module_, readings, operands, Realization{.told = told, .of = stored},
        std::nullopt, {}, ReturnDirect{.type = module_->Types().Void()});
    const std::array<llvm::Value*, 2> values{storage, value};
    auto args = module_->BuildCallArgs(abi, values);
    if (!args) {
      return std::unexpected(std::move(args.error()));
    }
    return builder_.CreateCall(module_->RuntimeFunction(symbol, abi), *args);
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
        auto assigned = AssignValue(stored, *value, *held);
        if (!assigned) {
          return std::unexpected(std::move(assigned.error()));
        }
        return nullptr;
      }
      return stored_by(
          RuntimeSymbol(*cell, lir::ValueCellTarget::Op::kStore),
          OperandReadingsOf(lir::ValueCellTarget::Op::kStore),
          ToldOf(*cell, lir::ValueCellTarget::Op::kStore),
          MachineOperand{.type = module_->Types().Ptr()}, *address, *value);
    }
    // A store writes into a value that is already there, so whatever names
    // that storage goes on naming it.
    if (module_->Types().HasAggregateEvaluationKind(stored)) {
      auto written = AssignValue(stored, *value, *address);
      if (!written) {
        return std::unexpected(std::move(written.error()));
      }
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
  return stored_by(
      RuntimeSymbol(through.domain, through.kind, support::BuiltinFn::kSet),
      support::RuntimeEntryOf(support::BuiltinFn::kSet).operands,
      ToldOf(through.domain, through.kind, support::BuiltinFn::kSet),
      ValueOperand{
          .type = ReachedType(store.place, std::ssize(store.place.chain) - 1)},
      *address, *value);
}

// A value moved where an assignment puts it: into a transient it is the one
// value still, as rustc moves an operand into an SSA local; into storage, one
// lying in storage moves there and nothing is left behind where it was made,
// and any other is stored there as the assignment puts it.
auto CodeGenFunction::LowerUse(
    const lir::UseInstr& use, const DestinationRef& destination)
    -> diag::Result<llvm::Value*> {
  auto value = LowerOperand(use.operand);
  if (!value) {
    return value;
  }
  return std::visit(
      Overloaded{
          [&](const PendingOperand&) { return value; },
          [&](const PlaceRef& place) -> diag::Result<llvm::Value*> {
            if (module_->Types().IsReadAsView(place.type)) {
              throw InternalError(
                  "llvm codegen: a sequence of machine values is built where "
                  "it is kept, never moved into storage -- please report this "
                  "as a bug");
            }
            if (!module_->Types().HasAggregateEvaluationKind(place.type)) {
              return value;
            }
            auto moved = RelocateValue(place.type, *value, place.address);
            if (!moved) {
              return std::unexpected(std::move(moved.error()));
            }
            return place.address;
          }},
      destination);
}

// A place resolves to an address. A place local's storage is its frame slot;
// any other base is a reference value, whose referent the opening dereference
// names. Each further dereference reads the reference held in the storage
// reached so far -- or, where that storage is a wrapper, reaches its contents
// for a read -- a member step reaches the storage where the declaration
// declaring that member placed it, a value cell a later step continues into is
// opened through its own access, an element step into a fixed-size unpacked
// array is arithmetic on where the array lies and into any other container
// asks the value reached so far for the element it names, the way the access
// needs it, and a component step lies at an offset in the product, or at the
// union's own start for a member of one.
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
              if (module_->Unit()
                      .types.Get(reached)
                      .Is<lir::UnpackedArrayType>()) {
                auto position = LowerOperand(element.coordinates.front());
                if (!position) {
                  return std::unexpected(std::move(position.error()));
                }
                return ArrayElementAddress(
                    reached, address,
                    OperandRef{
                        .value = *position,
                        .type = OperandType(element.coordinates.front())},
                    access);
              }
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
            // A component step is taken over a product or a union, whose type
            // lays it out, so its component lies at an offset from it -- every
            // member of a union at none -- whoever holds it.
            [&](const lir::ComponentProjection& component)
                -> diag::Result<llvm::Value*> {
              return ComponentAddress(reached, address, component.index.value);
            }},
        *step);
    if (!reached_storage) {
      return std::unexpected(std::move(reached_storage.error()));
    }
    address = *reached_storage;
    // A value cell is not where the value it holds lies for every value -- a
    // cell of a value too large for a frame holds the address of bytes kept
    // apart -- so a step into the value continues from what the cell's own
    // access hands out.
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

auto CodeGenFunction::ArrayElementAddress(
    lir::TypeId array_type, llvm::Value* array, const OperandRef& position,
    Access access) -> diag::Result<llvm::Value*> {
  const ArrayLayout& layout = module_->Types().LayoutOfArray(array_type);
  llvm::Type* const ptr = module_->Types().Ptr();
  // A position names an element where none of its bits is unknown and its
  // number, read unsigned so that a negative one is past every count, counts
  // one.
  const Planes named = LoadPosition(position);
  llvm::Value* const names_one = builder_.CreateAnd(
      builder_.CreateIsNull(named.unknown),
      builder_.CreateICmpULT(
          named.value,
          llvm::ConstantInt::get(named.value->getType(), layout.count)));

  llvm::Function* const fn = builder_.GetInsertBlock()->getParent();
  llvm::LLVMContext& context = builder_.getContext();
  llvm::BasicBlock* const there =
      llvm::BasicBlock::Create(context, "element", fn);
  llvm::BasicBlock* const nowhere =
      llvm::BasicBlock::Create(context, "no_element", fn);
  llvm::BasicBlock* const joined =
      llvm::BasicBlock::Create(context, "element_reached", fn);
  builder_.CreateCondBr(names_one, there, nowhere);

  builder_.SetInsertPoint(there);
  llvm::Value* const lying = builder_.CreateInBoundsGEP(
      builder_.getInt8Ty(), array,
      builder_.CreateMul(
          builder_.CreateZExtOrTrunc(named.value, builder_.getInt64Ty()),
          builder_.getInt64(layout.stride)));
  builder_.CreateBr(joined);

  // What a position naming no element reaches is the element type's to answer,
  // so the entry is asked of that type alone.
  builder_.SetInsertPoint(nowhere);
  RuntimeOp answered_by = RuntimeOp::kElementDefault;
  switch (access) {
    case Access::kRead:
      break;
    case Access::kWrite:
      answered_by = RuntimeOp::kDiscardedWrite;
      break;
  }
  const FnAbi abi = FnAbiOf(
      *module_, answered_by, {}, std::nullopt, layout.element,
      ReturnDirect{.type = ptr});
  auto args = module_->BuildCallArgs(abi, {});
  if (!args) {
    return std::unexpected(std::move(args.error()));
  }
  llvm::Value* const answered = builder_.CreateCall(
      module_->RuntimeFunction(RuntimeSymbol(answered_by), abi), *args);
  builder_.CreateBr(joined);

  builder_.SetInsertPoint(joined);
  llvm::PHINode* const reached = builder_.CreatePHI(ptr, 2);
  reached->addIncoming(lying, there);
  reached->addIncoming(answered, nowhere);
  return reached;
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
  auto placed = module_->PlacedMembersOf(member.declared_by);
  if (!placed) {
    return std::unexpected(std::move(placed.error()));
  }
  return builder_.CreateConstInBoundsGEP1_64(
      builder_.getInt8Ty(), owner, (*placed)->offsets.at(member.slot.value));
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
    opening = RuntimeSymbol(support::BuiltinFn::kObjectOf);
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

auto CodeGenFunction::AccessGranted(lir::TypeId address_type) const -> Access {
  const auto* pointer =
      module_->Unit().types.Get(address_type).As<lir::PointerType>();
  if (pointer == nullptr) {
    return Access::kWrite;
  }
  switch (pointer->mutability) {
    case lir::Mutability::kReadOnly:
      return Access::kRead;
    case lir::Mutability::kMutable:
      return Access::kWrite;
  }
  throw InternalError("llvm codegen: unknown mutability");
}

// Taking the address of a place. What that address is, is the place's own
// answer; what it becomes depends on what is being built. A reference names
// storage that may belong to a subscribable variable or not, and the body
// holding one is lowered once for every caller, so it cannot ask which it was
// lent (LRM 13.5.2) -- which is why the variable is recorded here, where the
// place it came from still answers. Every other address is the address itself.
// Either way what is addressed is the value where it lies, which a value cell
// hands out through its own access. An address granting only reading reaches
// the storage the way a read does, which for a wrapper is where it keeps its
// contents.
auto CodeGenFunction::LowerAddrOf(
    const lir::AddrOfInstr& addr, lir::TypeId result_type, llvm::Value* out)
    -> diag::Result<llvm::Value*> {
  auto address = ResolvePlaceAddress(addr.place, AccessGranted(result_type));
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
  if (!types.Get(result_type).Is<lir::VariableRefType>()) {
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
  // An operator's entry takes each operand as the value it is, and is told what
  // the realization for its domain is told.
  const std::array<StatedOperand, 2> operands{
      ValueOperand{.type = operand_type},
      ValueOperand{.type = OperandType(binary.rhs)}};
  const std::array<llvm::Value*, 2> values{*lhs, *rhs};
  const Realization realization{
      .told = ToldOfAnOperator(*domain), .of = operand_type};
  const auto call = [&](ReturnInfo ret) -> diag::Result<llvm::CallInst*> {
    const FnAbi abi = ArrangeCall(
        *module_, support::OperandReadings{}, operands, realization,
        std::nullopt, {}, ret);
    auto args = module_->BuildCallArgs(abi, values);
    if (!args) {
      return std::unexpected(std::move(args.error()));
    }
    if (std::holds_alternative<ReturnIndirect>(ret)) {
      args->push_back(out);
    }
    return builder_.CreateCall(module_->RuntimeFunction(entry, abi), *args);
  };
  switch (lir::BinaryAnswerOf(binary.op)) {
    case lir::BinaryAnswer::kOperandType: {
      auto called = call(ReturnIndirect{.returned = module_->Types().Ptr()});
      if (!called) {
        return std::unexpected(std::move(called.error()));
      }
      return out;
    }
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
      auto called = call(ReturnDirect{.type = builder_.getInt8Ty()});
      if (!called) {
        return std::unexpected(std::move(called.error()));
      }
      StoreComparisonAnswer(builder_, *called, out, *answer);
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
// later read rather than what the added high bits hold. MIR's verification
// admits no other pair.
auto CodeGenFunction::LowerCast(
    const lir::CastInstr& cast, lir::TypeId result_type)
    -> diag::Result<llvm::Value*> {
  auto operand = LowerOperand(cast.operand);
  if (!operand) {
    return std::unexpected(std::move(operand.error()));
  }
  const lir::TypeId operand_type = OperandType(cast.operand);
  // Every part of an object starts where the object does, so a pointer to or
  // a handle of one class read as one of another, or as the part every object
  // starts with, is the same address and the same handle.
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
    throw InternalError(
        "llvm codegen: a cast between types no cast joins -- please report "
        "this as a bug");
  }
  return builder_.CreateIntCast(
      *operand, target, *signedness == lir::Signedness::kSigned);
}

auto CodeGenFunction::BodyAt(llvm::Value* table, std::uint64_t slot)
    -> llvm::Value* {
  llvm::Type* ptr_ty = module_->Types().Ptr();
  return builder_.CreateLoad(
      ptr_ty, builder_.CreateConstInBoundsGEP1_64(ptr_ty, table, slot));
}

auto CodeGenFunction::LowerLanding(const lir::LandingInstr& landing)
    -> llvm::Value* {
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
  // A frame that takes the raise matches anything, naming no raised type:
  // which region claims a departure is a question about the target it names,
  // which the handler tests once it has asked the runtime what arrived.
  llvm::LandingPadInst* const pad =
      builder_.CreateLandingPad(LandingPadType(), 1);
  switch (landing.clause) {
    case lir::LandingInstr::Clause::kCatchAll:
      pad->addClause(llvm::ConstantPointerNull::get(module_->Types().Ptr()));
      break;
    case lir::LandingInstr::Clause::kCleanup:
      pad->setCleanup(true);
      break;
  }
  return builder_.CreateExtractValue(pad, 0);
}

auto CodeGenFunction::LowerBeginHandling(const lir::BeginHandlingInstr& begun)
    -> diag::Result<llvm::Value*> {
  auto exception = LowerOperand(begun.exception);
  if (!exception) {
    return std::unexpected(std::move(exception.error()));
  }
  llvm::CallInst* const call = builder_.CreateCall(
      module_->Module().getOrInsertFunction(
          "__cxa_begin_catch",
          llvm::FunctionType::get(
              module_->Types().Ptr(), {module_->Types().Ptr()}, false)),
      {*exception});
  call->setDoesNotThrow();
  return call;
}

auto CodeGenFunction::LowerEndHandling() -> llvm::Value* {
  llvm::CallInst* const call =
      builder_.CreateCall(module_->Module().getOrInsertFunction(
          "__cxa_end_catch",
          llvm::FunctionType::get(builder_.getVoidTy(), false)));
  call->setDoesNotThrow();
  return call;
}

auto CodeGenFunction::LandingPadType() -> llvm::StructType* {
  return llvm::StructType::get(module_->Types().Ptr(), builder_.getInt32Ty());
}

auto CodeGenFunction::ResolveCall(
    const lir::CallInstr& call, lir::TypeId result_type,
    llvm::function_ref<llvm::Value*()> return_slot)
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
  // is: an entry of the library and a body of this program take it alike.
  llvm::Value* const slot = std::visit(
      Overloaded{
          [](const ReturnDirect&) -> llvm::Value* { return nullptr; },
          [&](const ReturnIndirect&) { return return_slot(); }},
      abi->ret);
  if (slot != nullptr) {
    args->push_back(slot);
  }
  auto callee = ResolveCallee(call, result_type, *abi, *args);
  if (!callee) {
    return std::unexpected(std::move(callee.error()));
  }
  return ResolvedCall{
      .callee = *callee, .args = *std::move(args), .return_slot = slot};
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
    const lir::CallInstr& call, const DestinationRef& destination)
    -> diag::Result<llvm::Value*> {
  using Lowered = diag::Result<llvm::Value*>;
  const lir::TypeId result_type = TypeOf(destination);
  // The call made on what the target names. An execution is built in storage
  // of its own. Nothing ends that here: whoever drives it takes it out of the
  // storage, the moment it is made.
  const auto called = [&](const auto&) -> Lowered {
    auto resolved = ResolveCall(call, result_type, [&] {
      return std::visit(
          Overloaded{
              // A transient answered in storage is given storage of its own,
              // as rustc gives an operand local storage for an indirect
              // return; an execution lies in the library's own object.
              [&](const PendingOperand&) {
                return lir::CallBuildsExecution(call.target)
                           ? ObjectStorage(support::LibraryObject::kExecution)
                           : EnsureDestination(destination);
              },
              [](const PlaceRef& place) { return place.address; }},
          destination);
    });
    if (!resolved) {
      return std::unexpected(std::move(resolved.error()));
    }
    llvm::Value* const answered =
        EmitCallOrInvoke(resolved->callee, resolved->args);
    return resolved->return_slot != nullptr ? resolved->return_slot : answered;
  };
  return std::visit(
      Overloaded{
          // An operation over integral values is the instructions it is on
          // planes, or a call on the library's entry.
          [&](const lir::BuiltinTarget& builtin) -> Lowered {
            const std::optional<support::IntegralOp> integral =
                support::RuntimeEntryOf(builtin.fn).integral;
            if (!integral.has_value()) {
              return called(builtin);
            }
            return LowerIntegralCall(
                *integral, builtin, call, result_type,
                EnsureDestination(destination));
          },
          // Ending a value and copying one are steps of its life, which its
          // type says how to carry out.
          [&](const lir::EndValueTarget&) -> Lowered {
            auto ended = LowerOperand(call.args.at(0));
            if (!ended) {
              return std::unexpected(std::move(ended.error()));
            }
            auto end = EndValue(OperandType(call.args.at(0)), *ended);
            if (!end) {
              return std::unexpected(std::move(end.error()));
            }
            return nullptr;
          },
          [&](const lir::CopyValueTarget&) -> Lowered {
            auto copied = LowerOperand(call.args.at(0));
            if (!copied) {
              return std::unexpected(std::move(copied.error()));
            }
            return CopyValue(
                result_type, *copied, EnsureDestination(destination));
          },
          // A cell whose value lies where the library states is read and
          // written there; only one keeping an object of the library's own is
          // asked of the library.
          [&](const lir::ValueCellTarget& cell) -> Lowered {
            auto held_in = DomainOf(cell.value);
            if (!held_in) {
              return std::unexpected(std::move(held_in.error()));
            }
            const support::ValueDomain domain = *held_in;
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
                const std::optional<llvm::Value*> held = HeldAt(
                    *handle, {.kind = support::MemberStorageKind::kValueCell,
                              .domain = domain});
                if (!held.has_value()) {
                  return called(cell);
                }
                auto stored = LowerOperand(call.args.at(1));
                if (!stored) {
                  return std::unexpected(std::move(stored.error()));
                }
                auto assigned = AssignValue(cell.value, *stored, *held);
                if (!assigned) {
                  return std::unexpected(std::move(assigned.error()));
                }
                return nullptr;
              }
            }
            throw InternalError("llvm codegen: unknown value cell operation");
          },
          [&](const lir::DesignatedBitsTarget& bits) -> Lowered {
            return LowerDesignatedBitsWrite(bits, call, result_type);
          },
          [&](const lir::OpenWriteTarget& t) { return called(t); },
          [&](const lir::CoroutineTarget& t) { return called(t); },
          [&](const lir::FunctionTarget& t) { return called(t); },
          [&](const lir::IndirectTarget& t) { return called(t); },
          [&](const lir::DispatchTarget& t) { return called(t); },
          [&](const lir::InterfaceDispatchTarget& t) { return called(t); },
          // A unique pointer is the address of the storage it takes over, as a
          // Rust `Box` is; every other construction is its type's entry.
          [&](const lir::ConstructTarget& t) -> Lowered {
            if (!module_->Unit().types.Get(result_type).IsMoveOnly()) {
              return called(t);
            }
            return LowerOperand(call.args.at(0));
          },
          // Storage of dynamic duration comes from the host's allocation
          // function and goes back to its sized deallocation function, each
          // told the size of a complete object of what the storage holds, as
          // a C++ new-expression asks them.
          [&](const lir::DynamicStorageTarget& storage) -> Lowered {
            switch (storage.op) {
              case lir::DynamicStorageTarget::Op::kAllocate: {
                auto layout = module_->AllocationLayoutOf(
                    *module_->Unit().types.Get(result_type).Pointee());
                if (!layout) {
                  return std::unexpected(std::move(layout.error()));
                }
                return builder_.CreateCall(
                    module_->OperatorNew(), {builder_.getInt64(layout->size)});
              }
              case lir::DynamicStorageTarget::Op::kDeallocate: {
                auto given = LowerOperand(call.args.at(0));
                if (!given) {
                  return std::unexpected(std::move(given.error()));
                }
                auto deleted = module_->EmitDeleteCall(
                    builder_,
                    *module_->Unit()
                         .types.Get(OperandType(call.args.at(0)))
                         .Pointee(),
                    *given);
                if (!deleted) {
                  return std::unexpected(std::move(deleted.error()));
                }
                return nullptr;
              }
            }
            throw InternalError(
                "llvm codegen: unknown dynamic-storage operation");
          },
          [&](const lir::CreationTarget& t) { return called(t); },
          [&](const lir::SymbolTarget& t) { return called(t); },
          [&](const lir::ForeignTarget& t) { return called(t); },
          [&](const lir::ControlEffectTarget& t) { return called(t); }},
      call.target);
}

auto CodeGenFunction::LowerBitsParts(std::span<const lir::BitsPart> parts)
    -> diag::Result<std::vector<BitsPartRef>> {
  std::vector<BitsPartRef> lowered;
  lowered.reserve(parts.size());
  for (const lir::BitsPart& part : parts) {
    // A part's start is a position, which is what the layer above states
    // every ordinal as.
    const lir::TypeId start_type = OperandType(part.start);
    const std::optional<value::IntegralShape> stated =
        module_->Types().IntegralShapeOf(start_type);
    if (!stated.has_value() || *stated != value::kShapeOf<value::Position>) {
      throw InternalError(
          "llvm codegen: a part of an integral value starts at something "
          "that is not stated as a position -- please report this as a bug");
    }
    auto start = LowerOperand(part.start);
    if (!start) {
      return std::unexpected(std::move(start.error()));
    }
    lowered.push_back(
        BitsPartRef{
            .start = OperandRef{.value = *start, .type = start_type},
            .type = part.type});
  }
  return lowered;
}

auto CodeGenFunction::OuterBitsParts(std::span<const BitsPartRef> parts)
    -> llvm::Value* {
  llvm::Type* const count = builder_.getInt64Ty();
  llvm::StructType* const crossing = module_->Types().BitsPartType();
  auto* const all = llvm::ArrayType::get(crossing, parts.size());
  llvm::Value* const storage = FrameStorage(all);
  for (std::size_t i = 0; i < parts.size(); ++i) {
    const BitsPartRef& part = parts[i];
    llvm::Value* const slot =
        builder_.CreateConstInBoundsGEP2_64(all, storage, 0, i);
    builder_.CreateStore(
        part.start.value, builder_.CreateStructGEP(crossing, slot, 0));
    builder_.CreateStore(
        llvm::ConstantInt::get(
            count, module_->Types().RequiredIntegralShapeOf(part.type).width),
        builder_.CreateStructGEP(crossing, slot, 1));
  }
  return ViewOver(storage, parts.size());
}

// The entry takes the designation, the parts the bits lie in, where the bits
// start, and the bits: every part but the one written crosses in the span, and
// the one written as where the bits start.
auto CodeGenFunction::LowerDesignatedBitsWrite(
    const lir::DesignatedBitsTarget& bits, const lir::CallInstr& call,
    lir::TypeId result_type) -> diag::Result<llvm::Value*> {
  auto designation = LowerOperand(call.args.front());
  if (!designation) {
    return std::unexpected(std::move(designation.error()));
  }
  auto parts = LowerBitsParts(bits.parts);
  if (!parts) {
    return std::unexpected(std::move(parts.error()));
  }
  auto written = LowerOperand(call.args.back());
  if (!written) {
    return std::unexpected(std::move(written.error()));
  }
  auto abi = arranger_.FnAbiOf(call, result_type);
  if (!abi) {
    return std::unexpected(std::move(abi.error()));
  }
  const std::array<llvm::Value*, 4> operands{
      *designation, OuterBitsParts(std::span(*parts).first(parts->size() - 1)),
      parts->back().start.value, *written};
  auto args = module_->BuildCallArgs(*abi, operands);
  if (!args) {
    return std::unexpected(std::move(args.error()));
  }
  auto callee = ResolveCallee(call, result_type, *abi, *args);
  if (!callee) {
    return std::unexpected(std::move(callee.error()));
  }
  return EmitCallOrInvoke(*callee, *args);
}

auto CodeGenFunction::LowerBitsStore(const lir::BitsStoreInstr& store)
    -> diag::Result<llvm::Value*> {
  auto address = ResolvePlaceAddress(store.place, Access::kWrite);
  if (!address) {
    return std::unexpected(std::move(address.error()));
  }
  const lir::TypeId whole =
      ReachedType(store.place, std::ssize(store.place.chain));
  llvm::Value* at = *address;
  if (const std::optional<support::ValueDomain> held =
          PlaceValueCellDomain(store.place, whole)) {
    at = ValueCellContents(*held, at);
  }
  auto parts = LowerBitsParts(store.parts);
  if (!parts) {
    return std::unexpected(std::move(parts.error()));
  }
  auto written = LowerOperand(store.bits);
  if (!written) {
    return std::unexpected(std::move(written.error()));
  }
  const lir::TypeId written_type = OperandType(store.bits);
  if (StoreBitsOnPlanes(
          at, whole, *parts,
          OperandRef{.value = *written, .type = written_type})) {
    return nullptr;
  }
  const std::array<StatedOperand, 4> stated{
      ValueOperand{.type = whole},
      MachineOperand{.type = module_->Types().Span()},
      ValueOperand{.type = parts->back().start.type},
      ValueOperand{.type = written_type}};
  const FnAbi abi = FnAbiOf(
      *module_, RuntimeOp::kInsertBits, stated, std::nullopt, std::nullopt,
      ReturnDirect{.type = module_->Types().Void()});
  const std::array<llvm::Value*, 4> operands{
      at, OuterBitsParts(std::span(*parts).first(parts->size() - 1)),
      parts->back().start.value, *written};
  auto args = module_->BuildCallArgs(abi, operands);
  if (!args) {
    return std::unexpected(std::move(args.error()));
  }
  return builder_.CreateCall(
      module_->RuntimeFunction(RuntimeSymbol(RuntimeOp::kInsertBits), abi),
      *args);
}

// Every call is a symbol invoked with arguments; the target kinds differ only
// in how the symbol is resolved. A callee this module does not define is
// declared at the type the call's arrangement has.
auto CodeGenFunction::ResolveCallee(
    const lir::CallInstr& call, lir::TypeId result_type, const FnAbi& abi,
    std::span<llvm::Value* const> args) -> diag::Result<llvm::FunctionCallee> {
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
          // The value holds its class's definition, the definition holds its
          // table, and the table holds the body answering the behavior where
          // the class introducing it put it (LRM 8.20).
          [&](const lir::DispatchTarget& t)
              -> diag::Result<llvm::FunctionCallee> {
            if (args.empty()) {
              throw InternalError(
                  "llvm codegen: a dispatched call states no value to dispatch "
                  "on");
            }
            llvm::Type* ptr_ty = module_->Types().Ptr();
            llvm::Value* definition = builder_.CreateLoad(ptr_ty, args[0]);
            llvm::Value* table = builder_.CreateLoad(
                ptr_ty, builder_.CreateConstInBoundsGEP1_64(
                            builder_.getInt8Ty(), definition,
                            runtime::DefinitionLayoutOf().methods));
            return llvm::FunctionCallee(
                module_->Types().GetFunctionType(abi),
                BodyAt(table, module_->DispatchSlotOf(t.method)));
          },
          // The table the value's class gives the interface class is found
          // among those its definition lists (LRM 8.26.5), and holds the body
          // answering the method where the interface class declares it.
          [&](const lir::InterfaceDispatchTarget& t)
              -> diag::Result<llvm::FunctionCallee> {
            if (args.empty()) {
              throw InternalError(
                  "llvm codegen: a dispatched call states no value to dispatch "
                  "on");
            }
            auto iface = module_->DefinitionOf(t.requirement.introduced_by);
            if (!iface) {
              return std::unexpected(std::move(iface.error()));
            }
            const std::array<llvm::Value*, 2> found{args[0], *iface};
            llvm::Value* table = CallEntry(
                RuntimeSymbol(RuntimeOp::kInterfaceTable),
                module_->Types().Ptr(), Addresses(2), found);
            return llvm::FunctionCallee(
                module_->Types().GetFunctionType(abi),
                BodyAt(table, t.requirement.ordinal.value));
          },
          [&](const lir::ConstructTarget&)
              -> diag::Result<llvm::FunctionCallee> {
            auto constructor = arranger_.ConstructorOf(call, result_type);
            if (!constructor) {
              return std::unexpected(std::move(constructor.error()));
            }
            return module_->RuntimeFunction(*constructor, abi);
          },
          [&](const lir::CreationTarget& t)
              -> diag::Result<llvm::FunctionCallee> {
            return module_->RuntimeFunction(
                module_->CreationSymbol(t.cls), abi);
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
            return module_->RuntimeFunction(
                RuntimeSymbol(*domain, RuntimeOp::kInsertDesignatedBits), abi);
          },
          [&](const lir::EndValueTarget&)
              -> diag::Result<llvm::FunctionCallee> {
            throw InternalError(
                "llvm codegen: ending a value is emitted where the call is "
                "lowered, and names no callee -- please report this as a bug");
          },
          [&](const lir::CopyValueTarget&)
              -> diag::Result<llvm::FunctionCallee> {
            throw InternalError(
                "llvm codegen: copying a value is emitted where the call is "
                "lowered, and names no callee -- please report this as a bug");
          },
          [&](const lir::DynamicStorageTarget&)
              -> diag::Result<llvm::FunctionCallee> {
            throw InternalError(
                "llvm codegen: storage of dynamic duration is allocated or "
                "given back where the call is lowered, and names no callee -- "
                "please report this as a bug");
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

// A {pointer, length} span over `storage`, which this fills with `values`. The
// element type is the caller's to state: the machine element a LIR type names
// where the span carries plain data, and an address where it carries a
// sequence of objects the library defines. Nothing here reads what the values
// mean, so nothing depends on which entry the span feeds.
auto CodeGenFunction::SpanOver(
    std::span<llvm::Value* const> values, llvm::Type* element,
    llvm::Value* storage) -> llvm::Value* {
  auto* storage_ty = llvm::ArrayType::get(element, values.size());
  for (std::uint32_t i = 0; i < values.size(); ++i) {
    llvm::Value* slot =
        builder_.CreateConstInBoundsGEP2_64(storage_ty, storage, 0, i);
    builder_.CreateStore(values[i], slot);
  }
  return ViewOver(storage, values.size());
}

auto CodeGenFunction::ViewOver(llvm::Value* storage, std::uint64_t count)
    -> llvm::Value* {
  llvm::Value* span = llvm::UndefValue::get(module_->Types().Span());
  span = builder_.CreateInsertValue(span, storage, {0});
  return builder_.CreateInsertValue(
      span,
      llvm::ConstantInt::get(llvm::Type::getInt64Ty(module_->Context()), count),
      {1});
}

// The values are built where the sequence is kept: in the frame for one the
// body holds as a transient, and in the storage that holds it otherwise -- a
// box made for a sequence too large for a frame.
auto CodeGenFunction::LowerArray(
    const lir::ArrayInstr& array, const DestinationRef& destination)
    -> diag::Result<llvm::Value*> {
  const lir::TypeId element_type = module_->Unit()
                                       .types.Get(TypeOf(destination))
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
  llvm::Type* const element = module_->Types().Map(element_type);
  llvm::Value* const storage = std::visit(
      Overloaded{
          [&](const PendingOperand&) {
            return FrameStorage(llvm::ArrayType::get(element, elements.size()));
          },
          [&](const PlaceRef& place) { return place.address; }},
      destination);
  return SpanOver(elements, element, storage);
}

// A product value is laid out in place: each component is built where the
// type's layout puts it, from a value LIR ends on its own, so the product takes
// a copy.
auto CodeGenFunction::LowerProduct(
    const lir::ProductInstr& product, lir::TypeId result_type, llvm::Value* out)
    -> diag::Result<llvm::Value*> {
  const ProductLayout& layout = module_->Types().LayoutOfProduct(result_type);
  if (layout.components.size() != product.components.size()) {
    throw InternalError(
        "llvm codegen: a product's result type does not describe the "
        "components it is built from");
  }
  for (std::size_t i = 0; i < product.components.size(); ++i) {
    auto component = LowerOperand(product.components[i]);
    if (!component) {
      return std::unexpected(std::move(component.error()));
    }
    llvm::Value* const address = ComponentAddress(result_type, out, i);
    // A component standing where an index of a wildcard-indexed array goes
    // has no type of its own (LRM 7.8.1), so the integral value put there is
    // kept with the type it was written in, as the array keeps an index.
    const lir::TypeId stated = OperandType(product.components[i]);
    if (module_->Unit()
            .types.Get(layout.components[i])
            .Is<lir::WildcardIndexType>() &&
        module_->Types().IntegralShapeOf(stated).has_value()) {
      std::vector<ArgAbi> arranged{arranger_.ArgOf(
          OperandReadingsOf(RuntimeOp::kMakeWildcardIndex), 0, stated)};
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
    auto copied = CopyValue(layout.components[i], *component, address);
    if (!copied) {
      return copied;
    }
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
    arranged.push_back(
        arranger_.ArgOf(readings, i + 1, OperandType(operands[i])));
    auto crossed = module_->BuildCallArg(arranged.back().mode, *lowered, args);
    if (!crossed) {
      return std::unexpected(std::move(crossed.error()));
    }
  }
  return {};
}

// A sequence of handles belongs to no value domain, so which object a position
// names is answered by the entry that knows the sequence rather than by a
// value's own element read.
auto CodeGenFunction::LowerVectorGet(const lir::VectorGetInstr& get)
    -> diag::Result<llvm::Value*> {
  auto vector = LowerOperand(get.vector);
  if (!vector) {
    return std::unexpected(std::move(vector.error()));
  }
  auto index = LowerOperand(get.index);
  if (!index) {
    return std::unexpected(std::move(index.error()));
  }
  const std::array<llvm::Value*, 2> args{*vector, *index};
  return CallEntry(
      RuntimeSymbol(RuntimeOp::kSequenceElement), module_->Types().Ptr(),
      {CallArranger::Direct(module_->Types().Ptr()),
       CallArranger::Direct(module_->Types().Map(OperandType(get.index)))},
      args);
}

auto CodeGenFunction::LowerUnion(
    const lir::UnionInstr& built, lir::TypeId result_type, llvm::Value* out)
    -> diag::Result<llvm::Value*> {
  const UnionLayout& layout = module_->Types().LayoutOfUnion(result_type);
  auto value = LowerOperand(built.value);
  if (!value) {
    return std::unexpected(std::move(value.error()));
  }
  builder_.CreateMemSet(
      out, builder_.getInt8(0), layout.storage.layout.size,
      llvm::Align(layout.storage.layout.align));
  auto copied = CopyValue(layout.members[built.member.value], *value, out);
  if (!copied) {
    return copied;
  }
  if (layout.tag.has_value()) {
    builder_.CreateStore(
        builder_.getIntN(8 * layout.tag->bytes, built.member.value),
        builder_.CreateConstInBoundsGEP1_64(
            builder_.getInt8Ty(), out, layout.tag->offset));
  }
  return out;
}

auto CodeGenFunction::LowerDiscriminant(
    const lir::DiscriminantInstr& read, lir::TypeId result_type)
    -> diag::Result<llvm::Value*> {
  const lir::TypeId tagged = lir::PlaceType(module_->Unit(), *fn_, read.place);
  // The union is read where it lies, which a load of a value laid out by its
  // type answers with.
  auto address = LoadPlace(read.place, tagged);
  if (!address) {
    return std::unexpected(std::move(address.error()));
  }
  const value::TagPlacement placed = module_->Types().TagOf(tagged);
  llvm::Value* const tag = builder_.CreateLoad(
      builder_.getIntNTy(8 * placed.bytes),
      builder_.CreateConstInBoundsGEP1_64(
          builder_.getInt8Ty(), *address, placed.offset));
  return builder_.CreateZExt(tag, module_->Types().Map(result_type));
}

auto CodeGenFunction::LowerOverlaidLoad(
    const lir::OverlaidLoadInstr& load, lir::TypeId result_type,
    llvm::Value* out) -> diag::Result<llvm::Value*> {
  auto address = LoadPlace(load.place, result_type);
  if (!address) {
    return std::unexpected(std::move(address.error()));
  }
  auto copied = CopyValue(result_type, *address, out);
  if (!copied) {
    return copied;
  }
  module_->EmitClearAboveWidths(builder_, result_type, out);
  return out;
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
          // The void value has no bits, so nothing stands for it.
          [](const lir::VoidConst&) -> diag::Result<llvm::Value*> {
            return nullptr;
          },
          // A type's size or alignment is a number of its layout, which this
          // layer lays the type out to answer.
          [&](const lir::LayoutConst& c) -> diag::Result<llvm::Value*> {
            auto layout = module_->AllocationLayoutOf(c.of);
            if (!layout) {
              return std::unexpected(std::move(layout.error()));
            }
            const auto measured = [&]() -> std::uint64_t {
              switch (c.measure) {
                case lir::LayoutConst::Measure::kSize:
                  return layout->size;
                case lir::LayoutConst::Measure::kAlignment:
                  return layout->align;
              }
              throw InternalError("llvm_backend: unknown layout measure");
            };
            return llvm::ConstantInt::get(
                llvm::Type::getInt64Ty(module_->Context()), measured());
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
          },
          // A function is its own address.
          [&](const lir::FunctionRef& f) -> diag::Result<llvm::Value*> {
            return module_->UnitFunction(f.function);
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
        "a constant of type '{}' is not yet supported with '--backend llvm'",
        module_->Unit().types.Get(constant.type).KindName());
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
        "a real constant of type '{}' is not yet supported with "
        "'--backend llvm'",
        module_->Unit().types.Get(constant.type).KindName());
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
  auto placed = module_->PlacedMembersOf(result_type);
  if (!placed) {
    return std::unexpected(std::move(placed.error()));
  }
  // The closure is made in `out`, where its value lies: the library's part
  // at its start is the address of its definition, stored as a constructor
  // stores a class's table address, and its captures lie after it.
  builder_.CreateStore(*definition, out);
  for (std::size_t i = 0; i < built.captures.size(); ++i) {
    auto value = LowerOperand(built.captures[i]);
    if (!value) {
      return std::unexpected(std::move(value.error()));
    }
    const lir::TypeId type = (*placed)->types.at(i);
    llvm::Value* slot = builder_.CreateConstInBoundsGEP1_64(
        builder_.getInt8Ty(), out, (*placed)->offsets.at(i));
    if (module_->Types().HasAggregateEvaluationKind(type)) {
      auto copied = CopyValue(type, *value, slot);
      if (!copied) {
        return copied;
      }
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
