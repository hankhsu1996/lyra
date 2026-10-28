#include "lyra/backend/llvm/codegen_module.hpp"

#include <algorithm>
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

#include <llvm/ADT/ArrayRef.h>
#include <llvm/IR/Attributes.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Transforms/Utils/ModuleUtils.h>

#include "lyra/backend/llvm/codegen_function.hpp"
#include "lyra/backend/llvm/constant_record.hpp"
#include "lyra/backend/llvm/fn_abi.hpp"
#include "lyra/backend/llvm/machine_signature.hpp"
#include "lyra/backend/llvm/runtime_entry.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/failure_context.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/symbol_name.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/profiling/time_trace.hpp"
#include "lyra/runtime/closure.hpp"
#include "lyra/runtime/object_layout.hpp"
#include "lyra/support/callable_bounds.hpp"
#include "lyra/support/runtime_class.hpp"
#include "lyra/support/runtime_object.hpp"
#include "lyra/support/value_domain.hpp"
#include "lyra/value/enumeration.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/layout.hpp"

namespace lyra::backend::llvm_backend {

CodeGenModule::CodeGenModule(const lir::CompilationUnit& unit)
    : context_(std::make_unique<llvm::LLVMContext>()),
      // A module carries the name of the body it holds, because the party
      // composing the program derives names of its own from it -- one that
      // collects what runs before the program starts gives its result a name
      // made from this one, so two modules sharing an identifier are two
      // artifacts that cannot be composed together, and two bodies of one
      // unit are two artifacts.
      module_(std::make_unique<llvm::Module>(unit.body_name, *context_)),
      unit_(&unit),
      types_(*this),
      value_types_(*this, types_, unit),
      functions_(unit.functions.size()) {
}

auto CodeGenModule::Run() -> diag::Result<EmittedModule> {
  // A body gives a closure it builds storage of the closure's size, so a
  // capture this backend cannot hold is refused before any body asks.
  for (const lir::ClosureId id : unit_->closures.Ids()) {
    auto placed = PlacedMembersOf(
        unit_->types.Intern(lir::Type{lir::ClosureType{.closure_id = id}}));
    if (!placed) {
      return std::unexpected(std::move(placed.error()));
    }
  }
  // Every function is declared before any body is generated, because a body may
  // call one whose own body is generated later, including itself.
  for (const lir::FunctionId id : unit_->functions.Ids()) {
    functions_.Append(DeclareCallable(id));
  }
  for (const lir::StaticStorage& storage : unit_->static_storage) {
    auto defined = DefineSharedStorage(storage);
    if (!defined) {
      return std::unexpected(std::move(defined.error()));
    }
  }
  for (const lir::FunctionId id : unit_->functions.Ids()) {
    const auto in_function =
        diag::FailureContext::InFunction(unit_->functions.Get(id).name);
    const profiling::TimeTraceScope span(
        "emit function", [&] { return unit_->functions.Get(id).name; });
    auto generated = CodeGenFunction(*this, id).Run();
    if (!generated) {
      return std::unexpected(std::move(generated.error()));
    }
  }
  value_types_.EmitDeclared();
  // Every class a value is built of builds and ends its own storage, which a
  // class extending it calls, in this unit or another, and states its tables.
  for (const lir::ClassId id : unit_->classes.Ids()) {
    const lir::Class& cls = unit_->classes.Get(id);
    // A class another unit defines is built and dispatched through there, and
    // an interface class holds no storage and no value is built of one.
    if (cls.defining_unit.has_value() || !cls.base.has_value()) continue;
    const lir::TypeId type =
        unit_->types.Intern(lir::Type{lir::ObjectType{.class_id = id}});
    const Declared declared = DeclaredClass(id);
    const lir::Linkage linkage = DeclaredLinkage(type);
    if (auto ended = EmitDestructors(declared, linkage); !ended) {
      return std::unexpected(std::move(ended.error()));
    }
    if (auto created = EmitCreations(type, declared, linkage); !created) {
      return std::unexpected(std::move(created.error()));
    }
    EmitTables(cls, declared);
  }
  for (const lir::GlobalConstant& constant : unit_->constants) {
    DefineData(constant);
  }
  for (const lir::ClosureId id : unit_->closures.Ids()) {
    if (auto defined = EmitClosureDefinition(id); !defined) {
      return std::unexpected(std::move(defined.error()));
    }
  }
  auto built = EmitSharedStorageConstruction();
  if (!built) {
    return std::unexpected(std::move(built.error()));
  }

  std::string error;
  llvm::raw_string_ostream os(error);
  if (llvm::verifyModule(*module_, &os)) {
    throw InternalError(
        std::format("llvm codegen: produced an invalid module: {}", os.str()));
  }
  return EmittedModule{std::move(context_), std::move(module_)};
}

// A definition several artifacts write is one the link keeps one of, which
// is what every target spells as a weak definition; one this artifact owns is
// written once and a repeat of it is a program that does not link.
auto LinkageOf(lir::Linkage linkage) -> llvm::GlobalValue::LinkageTypes {
  switch (linkage) {
    case lir::Linkage::kInternal:
      return llvm::GlobalValue::InternalLinkage;
    case lir::Linkage::kExternal:
      return llvm::GlobalValue::ExternalLinkage;
    case lir::Linkage::kShared:
      return llvm::GlobalValue::WeakODRLinkage;
  }
  throw InternalError("llvm codegen: unknown linkage");
}

auto CodeGenModule::FunctionTypeOf(
    std::span<const lir::TypeId> params, lir::TypeId result)
    -> llvm::FunctionType* {
  std::vector<ArgAbi> arranged;
  arranged.reserve(params.size());
  for (const lir::TypeId param : params) {
    arranged.push_back(CallArranger::Direct(types_.Map(param)));
  }
  // A body answers as every call of it is arranged to, the shape every
  // runtime entry making a value has.
  return types_.GetFunctionType(
      CallArranger::Arrange(
          std::move(arranged), CallArranger::ReturnOfValue(types_, result)));
}

auto CodeGenModule::ExternalFunction(
    const std::string& symbol, lir::TypeId prototype) -> llvm::Function* {
  const auto* function =
      unit_->types.Get(prototype).As<lir::MachineFunctionType>();
  if (function == nullptr) {
    throw InternalError(
        "llvm codegen: another module's function is declared at a type that "
        "is no function's -- please report this as a bug");
  }
  return llvm::cast<llvm::Function>(
      module_
          ->getOrInsertFunction(
              symbol, FunctionTypeOf(function->params, function->result))
          .getCallee());
}

auto CodeGenModule::DeclareCallable(lir::FunctionId id) -> llvm::Function* {
  const lir::Function& fn = unit_->functions.Get(id);
  std::vector<lir::TypeId> params;
  params.reserve(fn.params.size());
  for (const lir::ValueId param : fn.params) {
    params.push_back(fn.values.Get(param).type);
  }
  llvm::Function* declared = llvm::Function::Create(
      FunctionTypeOf(params, fn.result_type), LinkageOf(fn.linkage), fn.name,
      module_.get());
  MarkBoolsAsCpp(declared);
  return declared;
}

auto CodeGenModule::UnitFunction(lir::FunctionId function) -> llvm::Function* {
  return functions_.Get(function);
}

auto CodeGenModule::EmitSharedStorageConstruction() -> diag::Result<void> {
  auto* fn_ty =
      llvm::FunctionType::get(llvm::Type::getVoidTy(*context_), {}, false);
  // Nothing outside this module calls it: whoever composes the program finds
  // it in the module's own list of what runs before the program starts. So it
  // is linked under no name another artifact could reach, and each module
  // carrying one does not collide with any other.
  auto* fn = llvm::Function::Create(
      fn_ty, llvm::Function::InternalLinkage, "unit_shared_storage",
      module_.get());
  auto* entry = llvm::BasicBlock::Create(*context_, "", fn);
  llvm::IRBuilder<> builder(entry);
  for (const lir::StaticStorage& storage : unit_->static_storage) {
    auto stated = StateSharedStorage(builder, storage);
    if (!stated) {
      return std::unexpected(std::move(stated.error()));
    }
  }
  builder.CreateRetVoid();
  llvm::appendToGlobalCtors(*module_, fn, 0);
  return {};
}

auto CodeGenModule::CreateRuntimeFunction(
    llvm::FunctionType* type, std::string_view name) -> llvm::FunctionCallee {
  if (const llvm::Function* declared = module_->getFunction(name);
      declared != nullptr && declared->getFunctionType() != type) {
    throw InternalError(
        std::format(
            "llvm codegen: {} is declared at two types -- please report this "
            "as a bug",
            name));
  }
  llvm::FunctionCallee entry = module_->getOrInsertFunction(name, type);
  MarkBoolsAsCpp(llvm::cast<llvm::Function>(entry.getCallee()));
  return entry;
}

auto CodeGenModule::RuntimeFunction(std::string_view symbol, const FnAbi& abi)
    -> llvm::FunctionCallee {
  return CreateRuntimeFunction(types_.GetFunctionType(abi), symbol);
}

auto CodeGenModule::WidthArg(const value::IntegralShape& shape)
    -> llvm::Constant* {
  return llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context_), shape.width);
}

auto CodeGenModule::FourStateArg(const value::IntegralShape& shape)
    -> llvm::Constant* {
  return llvm::ConstantInt::getBool(*context_, shape.IsFourState());
}

auto CodeGenModule::BuildCallArgs(
    const FnAbi& abi, std::span<llvm::Value* const> operands)
    -> diag::Result<std::vector<llvm::Value*>> {
  if (operands.size() != abi.args.size()) {
    throw InternalError(
        "llvm codegen: a call hands over as many operands as it is arranged "
        "for -- please report this as a bug");
  }
  llvm::Type* const count = llvm::Type::getInt64Ty(*context_);
  // What the callee takes ahead of the operands leads, each operand crosses
  // as it is arranged with the part the callee names where the arrangement
  // puts it, and the types no operand is a value of follow them all.
  std::vector<llvm::Value*> args;
  auto led = std::visit(
      Overloaded{
          [](const NoImplicitArg&) -> diag::Result<void> { return {}; },
          [&](const WidthMetadataArg& held) -> diag::Result<void> {
            args.push_back(WidthArg(types_.RequiredIntegralShapeOf(held.of)));
            return {};
          },
          [&](const TypeMetadataArg& held) -> diag::Result<void> {
            auto type_object = value_types_.GetAddrOfValueType(held.of);
            if (!type_object) {
              return std::unexpected(std::move(type_object.error()));
            }
            args.push_back(*type_object);
            return {};
          }},
      abi.implicit_arg);
  if (!led) {
    return std::unexpected(std::move(led.error()));
  }
  for (std::size_t i = 0; i <= operands.size(); ++i) {
    if (abi.named_part.has_value() && abi.named_part->before == i) {
      args.push_back(llvm::ConstantInt::get(count, abi.named_part->position));
    }
    if (i == operands.size()) {
      break;
    }
    auto crossed = BuildCallArg(abi.args[i].mode, operands[i], args);
    if (!crossed) {
      return std::unexpected(std::move(crossed.error()));
    }
  }
  for (const TypeArg& told : abi.type_args) {
    auto crossed = std::visit(
        Overloaded{
            [&](const TypeConstantArg& of) -> diag::Result<void> {
              auto type_object = value_types_.GetAddrOfValueType(of.type);
              if (!type_object) {
                return std::unexpected(std::move(type_object.error()));
              }
              args.push_back(*type_object);
              return {};
            },
            [&](const TypeExtentArg& of) -> diag::Result<void> {
              const value::IntegralShape shape =
                  types_.RequiredIntegralShapeOf(of.type);
              args.push_back(WidthArg(shape));
              args.push_back(FourStateArg(shape));
              return {};
            },
            [&](const ElementTypeArg& of) -> diag::Result<void> {
              auto element_type = value_types_.GetAddrOfElementType(of.type);
              if (!element_type) {
                return std::unexpected(std::move(element_type.error()));
              }
              args.push_back(*element_type);
              return {};
            },
            [&](const QueueTypeArg& of) -> diag::Result<void> {
              auto queue_type = value_types_.GetAddrOfQueueType(of.type);
              if (!queue_type) {
                return std::unexpected(std::move(queue_type.error()));
              }
              args.push_back(*queue_type);
              return {};
            }},
        told);
    if (!crossed) {
      return std::unexpected(std::move(crossed.error()));
    }
  }
  return args;
}

auto CodeGenModule::BuildCallArg(
    const PassMode& mode, llvm::Value* value, std::vector<llvm::Value*>& args)
    -> diag::Result<void> {
  return std::visit(
      Overloaded{
          [&](const PassDirect&) -> diag::Result<void> {
            args.push_back(value);
            return {};
          },
          [&](const PassWithExtent& with) -> diag::Result<void> {
            const value::IntegralShape shape =
                types_.RequiredIntegralShapeOf(with.type);
            args.push_back(value);
            args.push_back(WidthArg(shape));
            args.push_back(FourStateArg(shape));
            return {};
          },
          [&](const PassWithShape& with) -> diag::Result<void> {
            const value::IntegralShape shape =
                types_.RequiredIntegralShapeOf(with.type);
            args.push_back(value);
            args.push_back(WidthArg(shape));
            args.push_back(
                llvm::ConstantInt::getBool(
                    *context_, shape.signedness == value::Signedness::kSigned));
            args.push_back(FourStateArg(shape));
            return {};
          },
          [&](const PassWithType& with) -> diag::Result<void> {
            auto type_object = value_types_.GetAddrOfValueType(with.type);
            if (!type_object) {
              return std::unexpected(std::move(type_object.error()));
            }
            args.push_back(value);
            args.push_back(*type_object);
            return {};
          },
          [&](const PassWithWidth& with) -> diag::Result<void> {
            args.push_back(value);
            args.push_back(WidthArg(types_.RequiredIntegralShapeOf(with.type)));
            return {};
          }},
      mode);
}

auto CodeGenModule::EndOwnedStorage(
    llvm::IRBuilderBase& builder, lir::TypeId owner, llvm::Value* storage)
    -> diag::Result<void> {
  // A box holds a value its type lays out, and an object of a class is ended by
  // its class.
  const lir::TypeId pointee = *unit_->types.Get(owner).Pointee();
  const lir::Type& pointed = unit_->types.Get(pointee);
  // A closure of no known type is ended, and its storage given back, through
  // the definition it opens with.
  if (pointed.Is<lir::ErasedCallableType>()) {
    builder.CreateCall(
        RuntimeFunction(
            RuntimeSymbol(RuntimeOp::kClosureBoxEnd),
            CallArranger::Arrange(
                {CallArranger::Direct(types_.Ptr())},
                ReturnDirect{.type = types_.Void()})),
        {storage});
    return {};
  }
  // An object, held as its own class or as a class of the library it
  // extends, is ended through its class's definition.
  const auto* library = pointed.As<lir::RuntimeClassType>();
  if (pointed.Is<lir::ObjectType>() ||
      (library != nullptr && support::IsObjectOfTheRoot(library->which))) {
    EndObject(builder, storage);
    return {};
  }
  // A variable's cell is ended as a body's variable is, by its storage.
  if (pointed.Is<lir::ObservableType>()) {
    const support::DeclaredMemberStorage cell = CellStorage(pointee);
    if (!MemberLayout(pointee, cell).ends_with_nothing_to_do) {
      builder.CreateCall(MemberStorageDestructor(cell), {storage});
    }
    return EmitDeleteCall(builder, pointee, storage);
  }
  if (!types_.LayoutOf(pointee).ends_with_nothing_to_do) {
    const std::array<llvm::Value*, 1> args{storage};
    if (auto ended =
            EmitLifecycle(builder, pointee, LifecycleStep::kDestroy, args);
        !ended) {
      return ended;
    }
  }
  return EmitDeleteCall(builder, pointee, storage);
}

auto CodeGenModule::EmitDeleteCall(
    llvm::IRBuilderBase& builder, lir::TypeId pointee, llvm::Value* storage)
    -> diag::Result<void> {
  auto layout = AllocationLayoutOf(pointee);
  if (!layout) {
    return std::unexpected(std::move(layout.error()));
  }
  builder.CreateCall(
      SizedOperatorDelete(), {storage, builder.getInt64(layout->size)});
  return {};
}

void CodeGenModule::EmitClearAboveWidths(
    llvm::IRBuilderBase& builder, lir::TypeId type, llvm::Value* at) {
  value_types_.EmitClearAboveWidths(builder, type, at);
}

auto CodeGenModule::EmitLifecycle(
    llvm::IRBuilderBase& builder, lir::TypeId type, LifecycleStep step,
    std::span<llvm::Value* const> args) -> diag::Result<void> {
  const lir::Type& ty = unit_->types.Get(type);
  const support::ObjectLayout storage = types_.LayoutOf(type);
  // A value laid out by its type that owns something takes each part's step: a
  // product each component's, an array each element's, and a union the step of
  // the member its tag names; each is compiled once per type.
  if (IsLaidOutByType(ty) && !storage.ends_with_nothing_to_do) {
    builder.CreateCall(value_types_.Function(type, step), args);
    return {};
  }
  // A pointer owning what it points at lies where it is held as that address,
  // and has no copy.
  if (ty.IsMoveOnly()) {
    switch (step) {
      case LifecycleStep::kDestroy:
        return EndOwnedStorage(
            builder, type, builder.CreateLoad(types_.Ptr(), args[0]));
      case LifecycleStep::kMove:
        builder.CreateStore(builder.CreateLoad(types_.Ptr(), args[0]), args[1]);
        return {};
      case LifecycleStep::kCopy:
      case LifecycleStep::kAssign:
        throw InternalError(
            "llvm codegen: a value with no copy is copied -- please report "
            "this as a bug");
    }
    throw InternalError("llvm codegen: unknown lifecycle step");
  }
  // An integral value, and a value laid out by its type that ends with nothing
  // to do, is its bytes where it is held, so every step that has anything to do
  // is a copy of them, as clang copies a trivially copyable aggregate.
  if (types_.IntegralShapeOf(type).has_value() || IsLaidOutByType(ty)) {
    const llvm::Align align(storage.layout.align);
    switch (step) {
      case LifecycleStep::kCopy:
      case LifecycleStep::kMove:
        builder.CreateMemCpy(
            args[1], align, args[0], align, storage.layout.size);
        return {};
      case LifecycleStep::kAssign:
        builder.CreateMemCpy(
            args[0], align, args[1], align, storage.layout.size);
        return {};
      case LifecycleStep::kDestroy:
        return {};
    }
    throw InternalError("llvm codegen: unknown lifecycle step");
  }
  // A closure is ended by the body its declaration states for ending its
  // captures, and never moves or is copied: a coroutine body reads the captures
  // where they lie.
  if (const auto* closure = ty.As<lir::ClosureType>()) {
    switch (step) {
      case LifecycleStep::kDestroy:
        builder.CreateCall(
            ValueFunction(
                lir::DestructorSymbol(
                    lir::DefinitionSymbol(*unit_, closure->closure_id),
                    lir::Destructor::kCompleteObject)),
            args);
        return {};
      case LifecycleStep::kCopy:
      case LifecycleStep::kMove:
      case LifecycleStep::kAssign:
        throw InternalError(
            "llvm codegen: a closure is moved or copied -- please report this "
            "as a bug");
    }
    throw InternalError("llvm codegen: unknown lifecycle step");
  }
  const std::optional<support::RuntimeObject> object = HeldObjectOf(ty);
  if (!object.has_value()) {
    throw InternalError(
        "llvm codegen: a step of a value's life is asked of a type whose "
        "values are not owned -- please report this as a bug");
  }
  // Every other value is carried through the step by the library's entry for
  // what the value is held as. Ending a value takes it; assigning takes the
  // one written into and then the one written; copying and moving take the
  // source and build in storage handed last, which they answer with.
  std::span<llvm::Value* const> operands = args;
  ReturnInfo ret = ReturnDirect{.type = types_.Void()};
  switch (step) {
    case LifecycleStep::kDestroy:
      if (storage.ends_with_nothing_to_do) {
        return {};
      }
      break;
    case LifecycleStep::kAssign:
      break;
    case LifecycleStep::kCopy:
    case LifecycleStep::kMove:
      operands = args.first(1);
      ret = ReturnIndirect{.returned = types_.Ptr()};
      break;
  }
  const RuntimeOp op = LifecycleOp(step);
  // A value of the design crosses as the realization for its domain is told it
  // does, and an object that is no value of the design as the address it is.
  const std::optional<Realization> realization = std::visit(
      Overloaded{
          [&](support::ValueDomain domain) -> std::optional<Realization> {
            return Realization{.told = ToldOf(domain, op), .of = type};
          },
          [](support::LibraryObject) -> std::optional<Realization> {
            return std::nullopt;
          }},
      *object);
  const std::vector<StatedOperand> stated(
      operands.size(), StatedOperand{ValueOperand{.type = type}});
  const FnAbi abi = FnAbiOf(*this, op, stated, realization, std::nullopt, ret);
  auto crossed = BuildCallArgs(abi, operands);
  if (!crossed) {
    return std::unexpected(std::move(crossed.error()));
  }
  const std::span<llvm::Value* const> built_in = args.subspan(operands.size());
  crossed->insert(crossed->end(), built_in.begin(), built_in.end());
  builder.CreateCall(
      RuntimeFunction(RuntimeSymbol(*object, op), abi), *crossed);
  return {};
}

auto CodeGenModule::MemberStorageDestructor(
    support::DeclaredMemberStorage storage) -> llvm::Function* {
  return ValueFunction(RuntimeSymbol(storage, RuntimeOp::kDestroy));
}

auto CodeGenModule::OperatorNew() -> llvm::FunctionCallee {
  return CreateRuntimeFunction(
      llvm::FunctionType::get(
          types_.Ptr(), {llvm::Type::getInt64Ty(*context_)}, false),
      kOperatorNew);
}

auto CodeGenModule::SizedOperatorDelete() -> llvm::FunctionCallee {
  return CreateRuntimeFunction(
      llvm::FunctionType::get(
          types_.Void(), {types_.Ptr(), llvm::Type::getInt64Ty(*context_)},
          false),
      "_ZdlPvm");
}

auto CodeGenModule::AtExit() -> llvm::FunctionCallee {
  auto* ptr_ty = types_.Ptr();
  return CreateRuntimeFunction(
      llvm::FunctionType::get(
          llvm::Type::getInt32Ty(*context_), {ptr_ty, ptr_ty, ptr_ty}, false),
      "__cxa_atexit");
}

auto CodeGenModule::Int(std::uint64_t value, std::size_t bytes)
    -> llvm::Constant* {
  return llvm::ConstantInt::get(
      llvm::Type::getIntNTy(*context_, static_cast<unsigned>(bytes * 8)),
      value);
}

auto CodeGenModule::NameConstant(std::string_view name) -> llvm::Constant* {
  llvm::IRBuilder<> strings(*context_);
  return strings.CreateGlobalString(name, "", 0, module_.get());
}

auto CodeGenModule::RecordData(const lir::ConstantRecord& record)
    -> llvm::Constant* {
  const LibraryRecordLayout layout = LayoutOfLibraryRecord(record.kind);
  if (layout.fields.size() != record.parts.size()) {
    throw InternalError(
        std::format(
            "llvm codegen: a constant states {} fields of a library structure "
            "that has {} -- please report this as a bug",
            record.parts.size(), layout.fields.size()));
  }
  // What a field's type is says which of the things a constant states belongs
  // in it, and anything else is a producer that stated the wrong one.
  const auto mismatch = [](std::string_view field) -> llvm::Constant* {
    throw InternalError(
        std::format(
            "llvm codegen: a constant states something else where the "
            "library's structure holds {} -- please report this as a bug",
            field));
  };
  const auto pointer_of = [&](const lir::Constant& part) -> llvm::Constant* {
    return std::visit(
        Overloaded{
            [&](const lir::ConstantNull&) -> llvm::Constant* {
              return llvm::ConstantPointerNull::get(types_.Ptr());
            },
            [&](const lir::ConstantString& c) -> llvm::Constant* {
              return NameConstant(c.text);
            },
            [&](const lir::ConstantFunction& c) -> llvm::Constant* {
              return UnitFunction(c.function);
            },
            [&](const lir::ConstantExternalFunction& c) -> llvm::Constant* {
              return ExternalFunction(c.symbol, c.prototype);
            },
            [&](const lir::ConstantAddress& c) -> llvm::Constant* {
              return DefinitionGlobal(c.symbol);
            },
            [&](const lir::ConstantDestructor& c) -> llvm::Constant* {
              return ValueFunction(
                  lir::DestructorSymbol(
                      *lir::DefinitionSymbol(*unit_, c.cls),
                      lir::Destructor::kDeleting));
            },
            [&](const lir::ConstantDispatchTable& c) -> llvm::Constant* {
              return DefinitionGlobal(
                  lir::DispatchTableSymbol(
                      *lir::DefinitionSymbol(*unit_, c.cls)));
            },
            [&](const lir::ConstantInterfaceTable& c) -> llvm::Constant* {
              return DefinitionGlobal(
                  lir::InterfaceTableSymbol(
                      *lir::DefinitionSymbol(*unit_, c.cls),
                      *lir::DefinitionSymbol(*unit_, c.iface)));
            },
            [&](const lir::ConstantInt&) { return mismatch("a pointer"); },
            [&](const lir::ConstantRecord&) { return mismatch("a pointer"); },
            [&](const lir::ConstantArray&) { return mismatch("a pointer"); }},
        part.value);
  };
  ConstantRecord out(*context_, layout.layout.size);
  for (std::size_t i = 0; i < layout.fields.size(); ++i) {
    const FieldLayout& field = layout.fields[i];
    const lir::Constant& part = record.parts[i];
    switch (field.kind) {
      case FieldKind::kInteger: {
        const auto* integer = std::get_if<lir::ConstantInt>(&part.value);
        if (integer == nullptr) {
          mismatch("an integer");
        }
        // The two's-complement bits of the value at the field's width.
        const std::uint64_t bits = field.size * 8;
        const std::uint64_t mask =
            bits == 64 ? ~std::uint64_t{0} : (std::uint64_t{1} << bits) - 1;
        out.Place(
            field.offset, Int(static_cast<std::uint64_t>(integer->value) & mask,
                              static_cast<unsigned>(field.size)));
        break;
      }
      case FieldKind::kPointer:
        out.Place(field.offset, pointer_of(part));
        break;
    }
  }
  return std::move(out).Build();
}

void CodeGenModule::DefineData(const lir::GlobalConstant& constant) {
  const std::string& symbol = constant.symbol;
  const auto not_whole = [](std::string_view stated) -> llvm::GlobalVariable* {
    throw InternalError(
        std::format(
            "llvm codegen: a constant is {} and no structure or array -- "
            "please report this as a bug",
            stated));
  };
  std::visit(
      Overloaded{
          [&](const lir::ConstantRecord& record) {
            return DefineConstant(
                symbol, RecordData(record), LinkageOf(constant.linkage),
                llvm::Align(LayoutOfLibraryRecord(record.kind).layout.align));
          },
          // Every element of an array is of one type, so the first says what
          // the array is of; an array of none holds nothing of any type.
          [&](const lir::ConstantArray& array) {
            std::vector<llvm::Constant*> elements;
            elements.reserve(array.elements.size());
            std::uint64_t align = 1;
            for (const lir::Constant& element : array.elements) {
              std::visit(
                  Overloaded{
                      [&](const lir::ConstantRecord& record) {
                        elements.push_back(RecordData(record));
                        align = LayoutOfLibraryRecord(record.kind).layout.align;
                      },
                      [&](const lir::ConstantFunction& function) {
                        elements.push_back(UnitFunction(function.function));
                        align = alignof(void*);
                      },
                      [&](const lir::ConstantInt&) { not_whole("an integer"); },
                      [&](const lir::ConstantNull&) { not_whole("a null"); },
                      [&](const lir::ConstantString&) {
                        not_whole("a string");
                      },
                      [&](const lir::ConstantExternalFunction&) {
                        not_whole("another unit's function");
                      },
                      [&](const lir::ConstantAddress& address) {
                        elements.push_back(DefinitionGlobal(address.symbol));
                        align = alignof(void*);
                      },
                      [&](const lir::ConstantDestructor&) {
                        not_whole("an address");
                      },
                      [&](const lir::ConstantDispatchTable&) {
                        not_whole("an address");
                      },
                      [&](const lir::ConstantInterfaceTable&) {
                        not_whole("an address");
                      },
                      [&](const lir::ConstantArray&) {
                        not_whole("an array of arrays");
                      }},
                  element.value);
            }
            llvm::Type* element_ty = elements.empty()
                                         ? llvm::Type::getInt8Ty(*context_)
                                         : elements.front()->getType();
            return DefineConstant(
                symbol,
                llvm::ConstantArray::get(
                    llvm::ArrayType::get(element_ty, elements.size()),
                    elements),
                LinkageOf(constant.linkage), llvm::Align(align));
          },
          [&](const lir::ConstantInt&) { return not_whole("an integer"); },
          [&](const lir::ConstantNull&) { return not_whole("a null"); },
          [&](const lir::ConstantString&) { return not_whole("a string"); },
          [&](const lir::ConstantFunction&) {
            return not_whole("a function's address");
          },
          [&](const lir::ConstantExternalFunction&) {
            return not_whole("a function's address");
          },
          [&](const lir::ConstantAddress&) { return not_whole("an address"); },
          [&](const lir::ConstantDestructor&) {
            return not_whole("an address");
          },
          [&](const lir::ConstantDispatchTable&) {
            return not_whole("an address");
          },
          [&](const lir::ConstantInterfaceTable&) {
            return not_whole("an address");
          }},
      constant.initializer.value);
}

auto CodeGenModule::DeclarationOf(lir::TypeId type) const -> Declared {
  const lir::Type& named = unit_->types.Get(type);
  if (const auto* object = named.As<lir::ObjectType>()) {
    return DeclaredClass(object->class_id);
  }
  throw InternalError(
      std::format(
          "llvm codegen: {} is read as a declaration a value holds part of -- "
          "please report this as a bug",
          named.KindName()));
}

auto CodeGenModule::DeclaredLinkage(lir::TypeId type) const -> lir::Linkage {
  const lir::Type& declared = unit_->types.Get(type);
  if (const auto* object = declared.As<lir::ObjectType>()) {
    return unit_->classes.Get(object->class_id).linkage;
  }
  if (const lir::RecordId* record = lir::RecordOf(declared)) {
    return unit_->records.Get(*record).linkage;
  }
  return lir::Linkage::kExternal;
}

auto CodeGenModule::DeclaredClass(lir::ClassId id) const -> Declared {
  const lir::Class& cls = unit_->classes.Get(id);
  return Declared{
      .definition = lir::DefinitionSymbol(*unit_, id),
      .base = cls.base,
      .members = cls.members,
      .dispatch = cls.dispatch};
}

void CodeGenModule::EndObject(
    llvm::IRBuilderBase& builder, llvm::Value* object) {
  auto* ptr_ty = types_.Ptr();
  llvm::Value* definition = builder.CreateLoad(ptr_ty, object);
  llvm::Value* end = builder.CreateLoad(
      ptr_ty,
      builder.CreateConstInBoundsGEP1_64(
          builder.getInt8Ty(), definition, runtime::DefinitionLayoutOf().end));
  builder.CreateCall(
      llvm::FunctionType::get(types_.Void(), {ptr_ty}, false), end, {object});
}

namespace {

// The part the library builds where a class extends one of its classes, which
// holds the object's class's definition at its start.
auto LibraryClassPart(support::RuntimeClass which) -> PlacedMembers {
  const support::ObjectLayout layout = runtime::LayoutOf(which);
  return PlacedMembers{
      .types = {},
      .storage = {},
      .offsets = {},
      .end = layout.layout.size,
      .layout = layout.layout};
}

}  // namespace

auto CodeGenModule::PlaceMembers(
    std::span<const lir::TypeId> members, MemberSlotRole role,
    const PlacedMembers& after, std::string_view what)
    -> diag::Result<PlacedMembers> {
  PlacedMembers placed{
      .types = {},
      .storage = {},
      .offsets = {},
      .end = after.end,
      .layout = {.size = 0, .align = after.layout.align}};
  placed.types.reserve(members.size());
  placed.storage.reserve(members.size());
  placed.offsets.reserve(members.size());
  for (const lir::TypeId type : members) {
    const std::optional<support::DeclaredMemberStorage> storage =
        DeclaredStorageOf(*unit_, type, role);
    if (!storage) {
      return diag::Fail(
          diag::DiagCode::kUnsupportedTypeKind,
          "{} of type '{}' is not yet supported with '--backend llvm'", what,
          unit_->types.Get(type).KindName());
    }
    const support::ObjectLayout layout = MemberLayout(type, *storage);
    const std::uint64_t offset =
        value::AlignTo(placed.end, layout.layout.align);
    placed.types.push_back(type);
    placed.storage.push_back(*storage);
    placed.offsets.push_back(offset);
    placed.end = offset + layout.layout.size;
    placed.layout.align = std::max(placed.layout.align, layout.layout.align);
  }
  placed.layout.size = value::AlignTo(placed.end, placed.layout.align);
  return placed;
}

auto CodeGenModule::LayOut(const Declared& declared)
    -> diag::Result<PlacedMembers> {
  if (!declared.base.has_value()) {
    throw InternalError(
        "llvm codegen: an interface class is laid out, which holds no storage "
        "-- please report this as a bug");
  }
  auto extended = PlacedMembersOf(*declared.base);
  if (!extended) {
    return std::unexpected(std::move(extended.error()));
  }
  const PlacedMembers& base = **extended;
  std::vector<lir::TypeId> types;
  types.reserve(declared.members.size());
  for (const lir::Member& member : declared.members) {
    types.push_back(member.type);
  }
  return PlaceMembers(types, MemberSlotRole::kVariable, base, "a member");
}

auto CodeGenModule::LayOutCaptures(lir::ClosureId id)
    -> diag::Result<PlacedMembers> {
  const lir::Closure& closure = unit_->closures.Get(id);
  std::vector<lir::TypeId> types;
  types.reserve(closure.captures.size());
  for (const lir::Member& capture : closure.captures) {
    types.push_back(capture.type);
  }
  const support::Layout part = runtime::ClosurePart();
  return PlaceMembers(
      types, MemberSlotRole::kSnapshot,
      PlacedMembers{
          .types = {},
          .storage = {},
          .offsets = {},
          .end = part.size,
          .layout = part},
      "a capture");
}

auto CodeGenModule::ClosureLayout(lir::TypeId closure) const
    -> support::Layout {
  const auto found = placed_members_.find(closure);
  if (found == placed_members_.end()) {
    throw InternalError(
        "llvm codegen: a closure is sized before its captures are laid out -- "
        "please report this as a bug");
  }
  return found->second.layout;
}

auto CodeGenModule::PlacedMembersOf(lir::TypeId type)
    -> diag::Result<const PlacedMembers*> {
  if (const auto found = placed_members_.find(type);
      found != placed_members_.end()) {
    return &found->second;
  }
  const lir::Type& named = unit_->types.Get(type);
  const std::optional<lir::TypeDeclaration> declaration = named.Declaration();
  const auto* library = named.As<lir::RuntimeClassType>();
  if (library == nullptr && !declaration.has_value()) {
    throw InternalError(
        "llvm codegen: members are placed for a type that declares no storage "
        "-- please report this as a bug");
  }
  auto laid =
      library != nullptr
          ? diag::Result<PlacedMembers>{LibraryClassPart(library->which)}
          : std::visit(
                Overloaded{
                    [&](const lir::ObjectType&) -> diag::Result<PlacedMembers> {
                      return LayOut(DeclarationOf(type));
                    },
                    [&](const lir::ClosureType& closure)
                        -> diag::Result<PlacedMembers> {
                      return LayOutCaptures(closure.closure_id);
                    }},
                *declaration);
  if (!laid) {
    return std::unexpected(std::move(laid.error()));
  }
  return &placed_members_.emplace(type, *std::move(laid)).first->second;
}

auto CodeGenModule::AllocationLayoutOf(lir::TypeId type)
    -> diag::Result<support::Layout> {
  // A variable's cell takes what a body's variable of it does, and a value no
  // declaration lays out what its type lays it out in.
  if (unit_->types.Get(type).Is<lir::ObservableType>()) {
    return MemberLayout(type, CellStorage(type)).layout;
  }
  const std::optional<lir::TypeDeclaration> declaration =
      unit_->types.Get(type).Declaration();
  if (!declaration.has_value()) {
    return types_.LayoutOf(type).layout;
  }
  const auto object = [&]() -> diag::Result<support::Layout> {
    auto placed = PlacedMembersOf(type);
    if (!placed) {
      return std::unexpected(std::move(placed.error()));
    }
    return (*placed)->layout;
  };
  return std::visit(
      Overloaded{
          [&](const lir::ObjectType&) { return object(); },
          // A closure is as large as its captures are laid out.
          [&](const lir::ClosureType&) -> diag::Result<support::Layout> {
            auto placed = PlacedMembersOf(type);
            if (!placed) {
              return std::unexpected(std::move(placed.error()));
            }
            return (*placed)->layout;
          }},
      *declaration);
}

auto CodeGenModule::HoldsItsBytesInline(
    lir::TypeId type, support::DeclaredMemberStorage storage) const -> bool {
  const lir::Type& held = unit_->types.Get(type);
  switch (storage.kind) {
    case support::MemberStorageKind::kInlineValue:
      return IsLaidOutByType(held) || held.Is<lir::MachineIntType>() ||
             held.IsMoveOnly() || types_.IntegralShapeOf(type).has_value();
    case support::MemberStorageKind::kObservableCell:
    case support::MemberStorageKind::kResolvedNet:
    case support::MemberStorageKind::kSampledHistory:
    case support::MemberStorageKind::kValueCell:
    case support::MemberStorageKind::kBorrowedHandle:
    case support::MemberStorageKind::kReference:
    case support::MemberStorageKind::kAnyReference:
    case support::MemberStorageKind::kWeakReference:
    case support::MemberStorageKind::kSharedPointer:
    case support::MemberStorageKind::kCancellationTarget:
    case support::MemberStorageKind::kChannelCancellation:
    case support::MemberStorageKind::kEvaluationAttempts:
    case support::MemberStorageKind::kTimer:
    case support::MemberStorageKind::kLoopBlocks:
      return false;
  }
  throw InternalError("llvm codegen: unknown member storage kind");
}

auto CodeGenModule::MemberLayout(
    lir::TypeId type, support::DeclaredMemberStorage storage)
    -> support::ObjectLayout {
  if (HoldsItsBytesInline(type, storage)) {
    return types_.LayoutOf(type);
  }
  return runtime::LayoutOf(
      storage, ValuesHeldByMember(type).transform(
                   [&](lir::TypeId held) { return types_.LayoutOf(held); }));
}

auto CodeGenModule::CellStorage(lir::TypeId cell) const
    -> support::DeclaredMemberStorage {
  const std::optional<support::DeclaredMemberStorage> storage =
      DeclaredStorageOf(*unit_, cell, MemberSlotRole::kVariable);
  if (!storage) {
    throw InternalError(
        "llvm codegen: a cell is asked of a type held as none -- please "
        "report this as a bug");
  }
  return *storage;
}

auto CodeGenModule::BuildCell(
    llvm::IRBuilderBase& builder, lir::TypeId cell, llvm::Value* storage)
    -> diag::Result<void> {
  return BuildMemberStorage(builder, cell, CellStorage(cell), storage);
}

// A member holding no value of the program's holds none.
auto CodeGenModule::ValuesHeldByMember(lir::TypeId type) const
    -> std::optional<lir::TypeId> {
  if (types_.HasAggregateEvaluationKind(type)) {
    return type;
  }
  return ValuesHeldBy(unit_->types.Get(type));
}

auto CodeGenModule::BeginMembers(
    llvm::IRBuilderBase& builder, llvm::Value* value,
    const PlacedMembers& placed) -> diag::Result<void> {
  for (std::size_t i = 0; i < placed.storage.size(); ++i) {
    auto built = BeginMember(builder, value, placed, i);
    if (!built) {
      return built;
    }
  }
  return {};
}

// A value held as its bytes has none until one is copied in, which is how the
// one value holding such members -- a closure's captures -- is filled, so
// nothing builds it here.
auto CodeGenModule::BeginMember(
    llvm::IRBuilderBase& builder, llvm::Value* value,
    const PlacedMembers& placed, std::size_t i) -> diag::Result<void> {
  if (HoldsItsBytesInline(placed.types[i], placed.storage[i])) {
    throw InternalError(
        "llvm codegen: a value held as its bytes is built by copying a value "
        "in, never default-built -- please report this as a bug");
  }
  return BuildMemberStorage(
      builder, placed.types[i], placed.storage[i],
      builder.CreateConstInBoundsGEP1_64(
          builder.getInt8Ty(), value, placed.offsets[i]));
}

// The entry building a member's storage takes where the storage is, after
// what it is told of the values held there.
auto CodeGenModule::BuildMemberStorage(
    llvm::IRBuilderBase& builder, lir::TypeId type,
    support::DeclaredMemberStorage storage, llvm::Value* at)
    -> diag::Result<void> {
  const std::array<StatedOperand, 1> stated{
      MachineOperand{.type = types_.Ptr()}};
  const FnAbi abi = ArrangeCall(
      *this, support::OperandReadings{support::OperandReading::kMachine},
      stated, ValuesHeldByMember(type).transform([&](lir::TypeId held) {
        return Realization{.told = ToldBuilding(storage), .of = held};
      }),
      std::nullopt, {}, ReturnDirect{.type = types_.Void()});
  const std::array<llvm::Value*, 1> operands{at};
  auto args = BuildCallArgs(abi, operands);
  if (!args) {
    return std::unexpected(std::move(args.error()));
  }
  builder.CreateCall(
      RuntimeFunction(RuntimeSymbol(storage, RuntimeOp::kConstruct), abi),
      *args);
  return {};
}

auto CodeGenModule::EndMembers(
    llvm::IRBuilderBase& builder, llvm::Value* value,
    const PlacedMembers& placed) -> diag::Result<void> {
  for (std::size_t i = placed.storage.size(); i-- > 0;) {
    auto ended = EndMember(builder, value, placed, i);
    if (!ended) {
      return ended;
    }
  }
  return {};
}

auto CodeGenModule::EndMember(
    llvm::IRBuilderBase& builder, llvm::Value* value,
    const PlacedMembers& placed, std::size_t i) -> diag::Result<void> {
  if (MemberLayout(placed.types[i], placed.storage[i])
          .ends_with_nothing_to_do) {
    return {};
  }
  const std::array<llvm::Value*, 1> args{builder.CreateConstInBoundsGEP1_64(
      builder.getInt8Ty(), value, placed.offsets[i])};
  if (HoldsItsBytesInline(placed.types[i], placed.storage[i])) {
    return EmitLifecycle(
        builder, placed.types[i], LifecycleStep::kDestroy, args);
  }
  builder.CreateCall(MemberStorageDestructor(placed.storage[i]), args);
  return {};
}

auto CodeGenModule::CreationSymbol(lir::TypeId cls) const -> std::string {
  return lir::CreationSymbol(
      *lir::DefinitionSymbol(*unit_, cls), lir::Creation::kAllocating);
}

auto CodeGenModule::ValueFunction(const std::string& symbol)
    -> llvm::Function* {
  return llvm::cast<llvm::Function>(
      module_
          ->getOrInsertFunction(
              symbol,
              llvm::FunctionType::get(types_.Void(), {types_.Ptr()}, false))
          .getCallee());
}

namespace {

// The library class `type` names, where it names one rather than a declared
// class.
auto LibraryClassOf(const lir::TypePool& types, lir::TypeId type)
    -> std::optional<support::RuntimeClass> {
  if (const auto* library = types.Get(type).As<lir::RuntimeClassType>()) {
    return library->which;
  }
  return std::nullopt;
}

}  // namespace

void CodeGenModule::BeginBase(
    llvm::IRBuilderBase& builder, lir::TypeId base, llvm::Value* value) {
  if (const std::optional<support::RuntimeClass> library =
          LibraryClassOf(unit_->types, base)) {
    builder.CreateCall(
        CreateRuntimeFunction(
            llvm::FunctionType::get(
                types_.Void(), {builder.getInt8Ty(), types_.Ptr()}, false),
            RuntimeSymbol(RuntimeOp::kBeginLibraryPart)),
        {builder.getInt8(static_cast<std::uint8_t>(*library)), value});
    return;
  }
  builder.CreateCall(
      ValueFunction(
          lir::CreationSymbol(
              *lir::DefinitionSymbol(*unit_, base),
              lir::Creation::kBaseObject)),
      {value});
}

void CodeGenModule::EndBase(
    llvm::IRBuilderBase& builder, lir::TypeId base, llvm::Value* value) {
  if (const std::optional<support::RuntimeClass> library =
          LibraryClassOf(unit_->types, base)) {
    builder.CreateCall(
        CreateRuntimeFunction(
            llvm::FunctionType::get(
                types_.Void(), {builder.getInt8Ty(), types_.Ptr()}, false),
            RuntimeSymbol(RuntimeOp::kEndLibraryPart)),
        {builder.getInt8(static_cast<std::uint8_t>(*library)), value});
    return;
  }
  builder.CreateCall(
      ValueFunction(
          lir::DestructorSymbol(
              *lir::DefinitionSymbol(*unit_, base),
              lir::Destructor::kBaseObject)),
      {value});
}

auto CodeGenModule::EmitCreations(
    lir::TypeId type, const Declared& declared, lir::Linkage linkage)
    -> diag::Result<void> {
  auto own = LayOut(declared);
  if (!own) {
    return std::unexpected(std::move(own.error()));
  }
  const auto define = [&](lir::Creation which) {
    llvm::Function* fn =
        ValueFunction(lir::CreationSymbol(declared.definition, which));
    fn->setLinkage(LinkageOf(linkage));
    return fn;
  };
  llvm::Function* base_object = define(lir::Creation::kBaseObject);
  {
    llvm::IRBuilder<> builder(
        llvm::BasicBlock::Create(*context_, "", base_object));
    llvm::Value* value = base_object->getArg(0);
    BeginBase(builder, *declared.base, value);
    if (auto begun = BeginMembers(builder, value, *own); !begun) {
      return begun;
    }
    builder.CreateRetVoid();
  }
  // How large an object of the class is, and so the storage it takes, is
  // known where the class is defined, so a `new` in another unit reads
  // nothing of its layout -- as Swift's allocating initializer is the
  // defining module's, for a class whose layout its clients may not know.
  auto* allocating = llvm::cast<llvm::Function>(
      module_
          ->getOrInsertFunction(
              lir::CreationSymbol(
                  declared.definition, lir::Creation::kAllocating),
              llvm::FunctionType::get(types_.Ptr(), {}, false))
          .getCallee());
  allocating->setLinkage(LinkageOf(linkage));
  llvm::IRBuilder<> builder(
      llvm::BasicBlock::Create(*context_, "", allocating));
  llvm::Value* value =
      builder.CreateCall(OperatorNew(), {builder.getInt64(own->layout.size)});
  builder.CreateCall(base_object, {value});
  // The object is of its class from here, before any constructor runs on it
  // (LRM 8.17 NOTE 2).
  auto definition = DefinitionOf(type);
  if (!definition) {
    return std::unexpected(std::move(definition.error()));
  }
  builder.CreateStore(*definition, value);
  builder.CreateRet(value);
  return {};
}

auto CodeGenModule::EmitDestructors(
    const Declared& declared, lir::Linkage linkage) -> diag::Result<void> {
  auto own = LayOut(declared);
  if (!own) {
    return std::unexpected(std::move(own.error()));
  }
  const auto define = [&](lir::Destructor which) {
    llvm::Function* fn =
        ValueFunction(lir::DestructorSymbol(declared.definition, which));
    fn->setLinkage(LinkageOf(linkage));
    return fn;
  };
  llvm::Function* base_object = define(lir::Destructor::kBaseObject);
  {
    llvm::IRBuilder<> builder(
        llvm::BasicBlock::Create(*context_, "", base_object));
    llvm::Value* value = base_object->getArg(0);
    if (auto ended = EndMembers(builder, value, *own); !ended) {
      return ended;
    }
    EndBase(builder, *declared.base, value);
    builder.CreateRetVoid();
  }
  llvm::Function* deleting = define(lir::Destructor::kDeleting);
  llvm::IRBuilder<> builder(llvm::BasicBlock::Create(*context_, "", deleting));
  llvm::Value* value = deleting->getArg(0);
  builder.CreateCall(base_object, {value});
  const std::array<llvm::Value*, 2> args{
      value, builder.getInt64(own->layout.size)};
  builder.CreateCall(SizedOperatorDelete(), args);
  builder.CreateRetVoid();
  return {};
}

auto CodeGenModule::EntryAddress(const std::optional<lir::TableBody>& body)
    -> llvm::Constant* {
  // A slot a class leaves abstract holds no body (LRM 8.21).
  if (!body.has_value()) {
    return llvm::ConstantPointerNull::get(types_.Ptr());
  }
  return std::visit(
      Overloaded{
          [&](const lir::ConstantFunction& c) -> llvm::Constant* {
            return UnitFunction(c.function);
          },
          [&](const lir::ConstantExternalFunction& c) -> llvm::Constant* {
            return ExternalFunction(c.symbol, c.prototype);
          }},
      *body);
}

void CodeGenModule::EmitTables(
    const lir::Class& cls, const Declared& declared) {
  auto* ptr_ty = types_.Ptr();
  const auto table = [&](const std::string& symbol,
                         std::vector<llvm::Constant*> entries) {
    DefineConstant(
        symbol,
        llvm::ConstantArray::get(
            llvm::ArrayType::get(ptr_ty, entries.size()), entries),
        LinkageOf(cls.linkage), llvm::Align(alignof(void*)));
  };
  // A table holds the bodies of what the class's lineage introduced in the
  // order the class states them, which lists each class's behaviors after
  // those of the class it extends, so a behavior lies where it does in its
  // introducer's table.
  std::vector<llvm::Constant*> own;
  own.reserve(cls.dispatch.size());
  for (const lir::DispatchedBehavior& dispatched : cls.dispatch) {
    own.push_back(EntryAddress(dispatched.answered_by));
  }
  table(lir::DispatchTableSymbol(declared.definition), std::move(own));
  for (const lir::InterfaceTable& implemented : cls.interface_tables) {
    std::vector<llvm::Constant*> entries;
    entries.reserve(implemented.answered_by.size());
    for (const std::optional<lir::TableBody>& body : implemented.answered_by) {
      entries.push_back(EntryAddress(body));
    }
    table(
        lir::InterfaceTableSymbol(
            declared.definition,
            *lir::DefinitionSymbol(*unit_, implemented.of)),
        std::move(entries));
  }
}

auto CodeGenModule::DispatchSlotOf(const lir::StatedDispatchRef& behavior)
    -> std::uint64_t {
  const Declared introducer = DeclarationOf(behavior.introduced_by);
  const auto found = std::ranges::find_if(
      introducer.dispatch, [&](const lir::DispatchedBehavior& dispatched) {
        return dispatched.behavior == behavior;
      });
  if (found == introducer.dispatch.end()) {
    throw InternalError(
        "llvm codegen: a dispatch names a behavior its introducer does not "
        "carry -- please report this as a bug");
  }
  return static_cast<std::uint64_t>(
      std::distance(introducer.dispatch.begin(), found));
}

auto CodeGenModule::DefineConstant(
    const std::string& symbol, llvm::Constant* value,
    llvm::GlobalValue::LinkageTypes linkage, llvm::Align align)
    -> llvm::GlobalVariable* {
  llvm::GlobalVariable* declared = module_->getNamedGlobal(symbol);
  if (declared != nullptr && declared->hasInitializer()) {
    throw InternalError(
        std::format(
            "llvm codegen: '{}' is defined twice -- please report this as a "
            "bug",
            symbol));
  }
  // What named it before it was defined -- a body, or the constant itself --
  // holds a declaration, which an address of any type stands for. The
  // declaration gives its name up to the definition and each reference then
  // names the definition instead, as clang replaces a global whose type it
  // learns late.
  if (declared != nullptr) {
    declared->setName("");
  }
  auto* defined = llvm::cast<llvm::GlobalVariable>(
      module_->getOrInsertGlobal(symbol, value->getType()));
  defined->setConstant(true);
  defined->setInitializer(value);
  defined->setLinkage(linkage);
  defined->setAlignment(align);
  // A definition every module states alike lies in a section of its own name,
  // as clang places one, so the linker keeps one section and drops the others
  // whole rather than keeping each module's copy unreferenced.
  if (linkage == llvm::GlobalValue::LinkOnceODRLinkage) {
    defined->setComdat(module_->getOrInsertComdat(symbol));
  }
  if (declared != nullptr) {
    declared->replaceAllUsesWith(defined);
    declared->eraseFromParent();
  }
  return defined;
}

auto CodeGenModule::EmitClosureDefinition(lir::ClosureId id)
    -> diag::Result<void> {
  using runtime::ClosureCommonEntries;
  const lir::Closure& closure = unit_->closures.Get(id);
  const std::string symbol = lir::DefinitionSymbol(*unit_, id);
  auto captures = PlacedMembersOf(
      unit_->types.Intern(lir::Type{lir::ClosureType{.closure_id = id}}));
  if (!captures) {
    return std::unexpected(std::move(captures.error()));
  }
  llvm::Function* end = ValueFunction(
      lir::DestructorSymbol(symbol, lir::Destructor::kCompleteObject));
  {
    llvm::IRBuilder<> ending(llvm::BasicBlock::Create(*context_, "", end));
    if (auto ended = EndMembers(ending, end->getArg(0), **captures); !ended) {
      return ended;
    }
    ending.CreateRetVoid();
  }

  // A body taking its closure over is started where it is made rather than
  // called, so its definition is the common entries alone. Any other is called
  // through the definition at the one signature it has; every call is a code
  // address wherever it lies, so the definition at one signature lays out
  // every other.
  const lir::Function& invoke = unit_->functions.Get(closure.invoke);
  const auto takes_closure_over = [&] {
    const auto* receiver =
        unit_->types.Get(invoke.values.Get(invoke.params.front()).type)
            .As<lir::PointerType>();
    if (receiver == nullptr) {
      return false;
    }
    switch (receiver->ownership) {
      case lir::PointerOwnership::kUnique:
        return true;
      case lir::PointerOwnership::kShared:
      case lir::PointerOwnership::kBorrowed:
        return false;
    }
    throw InternalError("llvm codegen: unknown PointerOwnership");
  }();
  using CalledDefinition = runtime::ClosureDefinition<support::EffectSignature>;
  const std::size_t size = takes_closure_over ? sizeof(ClosureCommonEntries)
                                              : sizeof(CalledDefinition);
  ConstantRecord out(*context_, size);
  out.Place(offsetof(ClosureCommonEntries, end), end);
  if (!takes_closure_over) {
    out.Place(offsetof(CalledDefinition, call), UnitFunction(closure.invoke));
  }
  // The definition and what ends the closure are named wherever the invoke
  // is, and nowhere else.
  end->setLinkage(LinkageOf(invoke.linkage));
  DefineConstant(
      symbol, std::move(out).Build(), LinkageOf(invoke.linkage),
      llvm::Align(alignof(CalledDefinition)));
  return {};
}

auto CodeGenModule::DomainOf(lir::TypeId type) const
    -> diag::Result<support::ValueDomain> {
  const std::optional<support::ValueDomain> domain =
      ValueDomainOf(*unit_, type);
  if (!domain) {
    return diag::Fail(
        diag::DiagCode::kUnsupportedTypeKind,
        "a value of type '{}' is not yet supported with '--backend llvm'",
        unit_->types.Get(type).KindName());
  }
  return *domain;
}

auto CodeGenModule::SharedStorageOf(const lir::StaticStorage& storage)
    -> diag::Result<support::DeclaredMemberStorage> {
  const std::optional<support::DeclaredMemberStorage> described =
      DeclaredStorageOf(*unit_, storage.type, MemberSlotRole::kVariable);
  if (!described) {
    return diag::Fail(
        diag::DiagCode::kUnsupportedTypeKind,
        "a shared variable of type '{}' is not yet supported with "
        "'--backend llvm'",
        unit_->types.Get(storage.type).KindName());
  }
  return *described;
}

auto CodeGenModule::DefineSharedStorage(const lir::StaticStorage& storage)
    -> diag::Result<void> {
  auto described = SharedStorageOf(storage);
  if (!described) {
    return std::unexpected(std::move(described.error()));
  }
  // The unit that declares the storage defines the symbol as the storage
  // itself, sized as the library states it; every other unit reaches the same
  // symbol as a declaration.
  const support::ObjectLayout layout = MemberLayout(storage.type, *described);
  auto* type = llvm::ArrayType::get(
      llvm::Type::getInt8Ty(*context_), layout.layout.size);
  auto* defined = llvm::cast<llvm::GlobalVariable>(
      module_->getOrInsertGlobal(storage.symbol, type));
  defined->setInitializer(llvm::ConstantAggregateZero::get(type));
  defined->setAlignment(llvm::Align(layout.layout.align));
  return {};
}

auto CodeGenModule::SharedStorage(const std::string& symbol)
    -> llvm::GlobalVariable* {
  return llvm::cast<llvm::GlobalVariable>(
      module_->getOrInsertGlobal(symbol, llvm::Type::getInt8Ty(*context_)));
}

// The storage is built before the program starts and lasts until it exits,
// ending after it as a C++ program ends a namespace-scope object: by handing
// the platform what ends it.
auto CodeGenModule::StateSharedStorage(
    llvm::IRBuilderBase& builder, const lir::StaticStorage& storage)
    -> diag::Result<void> {
  auto described = SharedStorageOf(storage);
  if (!described) {
    return std::unexpected(std::move(described.error()));
  }
  llvm::GlobalVariable* shared = SharedStorage(storage.symbol);
  if (auto built =
          BuildMemberStorage(builder, storage.type, *described, shared);
      !built) {
    return built;
  }
  if (MemberLayout(storage.type, *described).ends_with_nothing_to_do) {
    return {};
  }
  const std::array<llvm::Value*, 3> at_exit{
      MemberStorageDestructor(*described), shared,
      module_->getOrInsertGlobal(
          "__dso_handle", llvm::Type::getInt8Ty(*context_))};
  builder.CreateCall(AtExit(), at_exit);
  return {};
}

auto CodeGenModule::LibraryData(std::string_view symbol) -> llvm::Constant* {
  return DefinitionGlobal(std::string(symbol));
}

auto CodeGenModule::DefinitionGlobal(const std::string& symbol)
    -> llvm::GlobalVariable* {
  // Named before it is defined, or defined by another unit, it is a
  // declaration, which the definition replaces where this unit is the one
  // defining it.
  return llvm::cast<llvm::GlobalVariable>(
      module_->getOrInsertGlobal(symbol, llvm::Type::getInt8Ty(*context_)));
}

auto CodeGenModule::DefinitionOf(lir::TypeId type)
    -> diag::Result<llvm::Constant*> {
  const std::optional<std::string> symbol = lir::DefinitionSymbol(*unit_, type);
  if (!symbol.has_value()) {
    return diag::Fail(
        diag::DiagCode::kUnsupportedExpressionForm,
        "building a value of type '{}' is not yet supported with "
        "'--backend llvm'",
        unit_->types.Get(type).KindName());
  }
  return DefinitionGlobal(*symbol);
}

auto CodeGenModule::GetAddrOfEnumTable(lir::EnumTableId table)
    -> llvm::GlobalVariable* {
  if (const auto found = enum_tables_.find(table);
      found != enum_tables_.end()) {
    return found->second;
  }
  llvm::GlobalVariable* defined = DefineEnumTable(table);
  enum_tables_.emplace(table, defined);
  return defined;
}

auto CodeGenModule::GetAddrOfIntegralConstant(lir::IntegralConstantId constant)
    -> llvm::GlobalVariable* {
  if (const auto found = integral_constants_.find(constant);
      found != integral_constants_.end()) {
    return found->second;
  }
  llvm::GlobalVariable* defined = DefineIntegralConstant(constant);
  integral_constants_.emplace(constant, defined);
  return defined;
}

// The record the library reads a table through, pointing at the planes and the
// names, each an array of this module's own. No label here reaches a linker,
// so a table's identity is enough to tell its data from another's.
auto CodeGenModule::DefineEnumTable(lir::EnumTableId table)
    -> llvm::GlobalVariable* {
  using value::Enumeration;
  const lir::EnumTableDecl& decl = unit_->enum_tables.Get(table);
  const auto constant = [&](std::string_view part, llvm::Constant* data) {
    auto* defined = llvm::cast<llvm::GlobalVariable>(module_->getOrInsertGlobal(
        std::format("enum_table_{}{}", table.value, part), data->getType()));
    defined->setLinkage(llvm::GlobalValue::PrivateLinkage);
    defined->setConstant(true);
    defined->setInitializer(data);
    return defined;
  };
  const value::IntegralShape base = types_.RequiredIntegralShapeOf(decl.base);
  std::vector<value::ConstPlanes> member_planes;
  std::vector<llvm::Constant*> name_data;
  member_planes.reserve(decl.members.size());
  name_data.reserve(decl.members.size());
  for (const lir::EnumTableMember& member : decl.members) {
    member_planes.push_back(
        value::ConstPlanes{
            .value = member.value.value_words,
            .unknown = member.value.state_words});
    name_data.push_back(NameConstant(member.name));
  }
  llvm::GlobalVariable* planes = constant(
      "_planes",
      llvm::ConstantDataArray::get(
          *context_, llvm::ArrayRef<std::uint64_t>(value::EnumerationPlanes(
                         base.width, base.IsFourState(), member_planes))));
  planes->setAlignment(llvm::Align(alignof(std::uint64_t)));
  llvm::GlobalVariable* names = constant(
      "_names",
      llvm::ConstantArray::get(
          llvm::ArrayType::get(types_.Ptr(), name_data.size()), name_data));
  names->setAlignment(llvm::Align(alignof(const char*)));
  ConstantRecord out(*context_, sizeof(Enumeration));
  out.Place(offsetof(Enumeration, planes), planes);
  out.Place(offsetof(Enumeration, names), names);
  out.Place(
      offsetof(Enumeration, members),
      Int(decl.members.size(), sizeof(Enumeration::members)));
  out.Place(
      offsetof(Enumeration, width),
      Int(base.width, sizeof(Enumeration::width)));
  out.Place(
      offsetof(Enumeration, four_state),
      Int(base.IsFourState() ? 1 : 0, sizeof(Enumeration::four_state)));
  llvm::GlobalVariable* defined = constant("", std::move(out).Build());
  defined->setAlignment(llvm::Align(alignof(Enumeration)));
  return defined;
}

// The bytes are laid out by the value layer's own rule, so a constant here and
// a value the library builds of the same type are the same bytes. A constant
// whose type can hold x or z and that holds none states no unknown plane, which
// is then clear.
auto CodeGenModule::DefineIntegralConstant(lir::IntegralConstantId constant)
    -> llvm::GlobalVariable* {
  const lir::IntegralConstantDecl& decl =
      unit_->integral_constants.Get(constant);
  const std::optional<value::IntegralShape> shape =
      types_.IntegralShapeOf(decl.type);
  if (!shape.has_value()) {
    throw InternalError(
        "llvm codegen: an integral constant is of a type that is not integral "
        "-- please report this as a bug");
  }
  value::WrittenWords words(*shape);
  const value::IntegralView planes = words.MutableView();
  if (decl.value.value_words.size() != planes.value.size() ||
      decl.value.state_words.size() > planes.unknown.size()) {
    throw InternalError(
        "llvm codegen: an integral constant's planes do not span the width its "
        "type states -- please report this as a bug");
  }
  std::ranges::copy(decl.value.value_words, planes.value.begin());
  std::ranges::copy(decl.value.state_words, planes.unknown.begin());
  std::vector<std::uint8_t> bytes(shape->Bytes());
  words.StoreTo(bytes.data());
  llvm::Constant* laid_out = llvm::ConstantDataArray::get(*context_, bytes);
  auto* defined = llvm::cast<llvm::GlobalVariable>(module_->getOrInsertGlobal(
      std::format("constant_{}", constant.value), laid_out->getType()));
  defined->setLinkage(llvm::GlobalValue::PrivateLinkage);
  defined->setConstant(true);
  defined->setInitializer(laid_out);
  defined->setAlignment(llvm::Align(value::IntegralAlignFor(shape->width)));
  return defined;
}

}  // namespace lyra::backend::llvm_backend
