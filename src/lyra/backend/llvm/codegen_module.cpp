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
#include "lyra/backend/llvm/codegen_types.hpp"
#include "lyra/backend/llvm/constant_record.hpp"
#include "lyra/backend/llvm/entry_realizations.hpp"
#include "lyra/backend/llvm/llvm_fn_abi.hpp"
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
#include "lyra/support/library_decl.hpp"
#include "lyra/support/runtime_object.hpp"
#include "lyra/support/value_domain.hpp"
#include "lyra/value/constant_span.hpp"
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
  // Every class a value is built of states its tables.
  for (const lir::ClassId id : unit_->classes.Ids()) {
    const lir::Class& cls = unit_->classes.Get(id);
    // A class another unit defines is built and dispatched through there, and
    // an interface holds no storage and no value is built of one.
    if (cls.defining_unit.has_value() || !cls.base.has_value()) continue;
    EmitTables(cls, DeclaredClass(id));
  }
  for (const lir::GlobalConstant& definition : unit_->definitions) {
    DefineData(definition);
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
auto LinkageOf(support::Linkage linkage) -> llvm::GlobalValue::LinkageTypes {
  switch (linkage) {
    case support::Linkage::kInternal:
      return llvm::GlobalValue::InternalLinkage;
    case support::Linkage::kExternal:
      return llvm::GlobalValue::ExternalLinkage;
    case support::Linkage::kShared:
      return llvm::GlobalValue::WeakODRLinkage;
  }
  throw InternalError("llvm codegen: unknown linkage");
}

auto CodeGenModule::FunctionTypeOf(
    std::span<const lir::TypeId> params, lir::TypeId result)
    -> llvm::FunctionType* {
  std::vector<LlvmArgAbi> arranged;
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

auto CodeGenModule::RuntimeFunction(
    std::string_view symbol, const LlvmFnAbi& abi) -> llvm::FunctionCallee {
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
    llvm::IRBuilderBase& builder, const LlvmFnAbi& abi,
    std::span<llvm::Value* const> operands)
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
          [&](const TypeAndNetFoldsArg& held) -> diag::Result<void> {
            auto type_object = value_types_.GetAddrOfElementType(held.of);
            if (!type_object) {
              return std::unexpected(std::move(type_object.error()));
            }
            args.push_back(*type_object);
            args.push_back(value_types_.GetAddrOfNetFolds(held.of));
            return {};
          },
          [&](const TypeMetadataArg& held) -> diag::Result<void> {
            for (const TypeArg& told : held.args) {
              if (auto crossed = AppendTypeArg(told, args); !crossed) {
                return std::unexpected(std::move(crossed.error()));
              }
            }
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
    auto crossed = BuildCallArg(builder, abi.args[i].mode, operands[i], args);
    if (!crossed) {
      return std::unexpected(std::move(crossed.error()));
    }
  }
  for (const TypeArg& told : abi.type_args) {
    if (auto crossed = AppendTypeArg(told, args); !crossed) {
      return std::unexpected(std::move(crossed.error()));
    }
  }
  return args;
}

auto CodeGenModule::AppendTypeArg(
    const TypeArg& told, std::vector<llvm::Value*>& args)
    -> diag::Result<void> {
  return std::visit(
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
          },
          [&](const TakePositionsArg& of) -> diag::Result<void> {
            args.push_back(value_types_.GetAddrOfTakePositions(of.type));
            return {};
          },
          [&](const IndexOrderArg& of) -> diag::Result<void> {
            auto domain = DomainOf(of.key);
            if (!domain) {
              return std::unexpected(std::move(domain.error()));
            }
            args.push_back(LibraryData(IndexOrderSymbol(*domain)));
            return {};
          },
          [&](const OperationsArg& of) -> diag::Result<void> {
            auto operations = value_types_.GetAddrOfOperations(of.type);
            if (!operations) {
              return std::unexpected(std::move(operations.error()));
            }
            args.push_back(*operations);
            return {};
          }},
      told);
}

namespace {

// The arguments `value` is handed over as, appended to `args`: a structure's
// fields one after another, read off it where `builder` stands, and any other
// value as itself. A structure is expanded, as clang's `ABIArgInfo::Expand`
// expands one, so every argument a call hands over is a scalar or an address,
// which lands where its own kind says on every target; one handed whole lands
// where that target's rules for structures put it. The type a callee is
// declared at is expanded the same way.
void AppendExpandedArgs(
    llvm::IRBuilderBase& builder, llvm::Value* value,
    std::vector<llvm::Value*>& args) {
  auto* structure = llvm::dyn_cast<llvm::StructType>(value->getType());
  if (structure == nullptr) {
    args.push_back(value);
    return;
  }
  for (unsigned field = 0; field < structure->getNumElements(); ++field) {
    args.push_back(builder.CreateExtractValue(value, {field}));
  }
}

}  // namespace

auto CodeGenModule::BuildCallArg(
    llvm::IRBuilderBase& builder, const PassMode& mode, llvm::Value* value,
    std::vector<llvm::Value*>& args) -> diag::Result<void> {
  return std::visit(
      Overloaded{
          [&](const PassDirect&) -> diag::Result<void> {
            AppendExpandedArgs(builder, value, args);
            return {};
          },
          [&](const PassWithExtent& with) -> diag::Result<void> {
            const value::IntegralShape shape =
                types_.RequiredIntegralShapeOf(with.type);
            AppendExpandedArgs(builder, value, args);
            args.push_back(WidthArg(shape));
            args.push_back(FourStateArg(shape));
            return {};
          },
          [&](const PassWithShape& with) -> diag::Result<void> {
            const value::IntegralShape shape =
                types_.RequiredIntegralShapeOf(with.type);
            AppendExpandedArgs(builder, value, args);
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
            AppendExpandedArgs(builder, value, args);
            args.push_back(*type_object);
            return {};
          },
          [&](const PassWithWidth& with) -> diag::Result<void> {
            AppendExpandedArgs(builder, value, args);
            args.push_back(WidthArg(types_.RequiredIntegralShapeOf(with.type)));
            return {};
          }},
      mode);
}

auto CodeGenModule::EmitDeleteCall(
    llvm::IRBuilderBase& builder, lir::TypeId pointee, llvm::Value* storage)
    -> diag::Result<void> {
  auto layout = AllocationLayoutOf(pointee);
  if (!layout) {
    return std::unexpected(std::move(layout.error()));
  }
  const llvm::FunctionCallee release = SizedOperatorDelete();
  llvm::Value* const size = builder.getInt64(layout->size);
  builder.CreateCall(release, {storage, size});
  return {};
}

void CodeGenModule::EmitClearAboveWidths(
    llvm::IRBuilderBase& builder, lir::TypeId type, llvm::Value* at) {
  value_types_.EmitClearAboveWidths(builder, type, at);
}

auto CodeGenModule::EmitStep(
    llvm::IRBuilderBase& builder, lir::TypeId type, lir::ValueStep step,
    std::span<llvm::Value* const> args) -> diag::Result<void> {
  return std::visit(
      Overloaded{
          // A value that is its bytes is copied as them, as clang copies a
          // trivially copyable aggregate, and ends with nothing to do.
          [&](const lir::StepIsBytes&) -> diag::Result<void> {
            const support::Layout layout = types_.LayoutOf(type).layout;
            const llvm::Align align(layout.align);
            switch (step) {
              case lir::ValueStep::kCopy:
              case lir::ValueStep::kMove:
                builder.CreateMemCpy(
                    args[1], align, args[0], align, layout.size);
                return {};
              case lir::ValueStep::kAssign:
                builder.CreateMemCpy(
                    args[0], align, args[1], align, layout.size);
                return {};
              case lir::ValueStep::kEnd:
                return {};
            }
            throw InternalError("llvm codegen: unknown value step");
          },
          [&](const lir::StepByFunction&) -> diag::Result<void> {
            builder.CreateCall(
                UnitFunction(lir::StepFunctionOf(*unit_, type, step)), args);
            return {};
          },
          // What only the definition a value opens with knows how to end is
          // ended through it: an object as the class it was created as, a
          // closure of no known type by the entry that reads its definition.
          [&](const lir::StepThroughDefinition&) -> diag::Result<void> {
            if (step != lir::ValueStep::kEnd) {
              throw InternalError(
                  "llvm codegen: a value ended through its definition is "
                  "asked for another step -- please report this as a bug");
            }
            llvm::Value* const held = builder.CreateLoad(types_.Ptr(), args[0]);
            if (unit_->types.Get(*unit_->types.Get(type).Pointee())
                    .Is<lir::ErasedCallableType>()) {
              llvm::Type* const box = types_.Ptr();
              llvm::Type* const result = types_.Void();
              builder.CreateCall(
                  RuntimeFunction(
                      RuntimeSymbol(RuntimeOp::kClosureBoxEnd),
                      CallArranger::Arrange(
                          {CallArranger::Direct(box)},
                          ReturnDirect{.type = result})),
                  {held});
              return {};
            }
            EndObject(builder, held);
            return {};
          },
          // An owning pointer ends what it points at -- a variable's cell
          // with its storage, a value as its type ends one -- and gives the
          // storage back, as clang writes a `delete` out where it is.
          [&](const lir::StepThroughPointee&) -> diag::Result<void> {
            if (step != lir::ValueStep::kEnd) {
              throw InternalError(
                  "llvm codegen: an owning pointer is asked through what it "
                  "points at for a step other than its end -- please report "
                  "this as a bug");
            }
            llvm::Value* const held = builder.CreateLoad(types_.Ptr(), args[0]);
            const lir::TypeId pointee = *unit_->types.Get(type).Pointee();
            const std::array<llvm::Value*, 1> at{held};
            auto ended =
                unit_->types.Get(pointee).Is<lir::ObservableType>()
                    ? EndStorageAt(builder, pointee, CellStorage(pointee), held)
                    : EmitStep(builder, pointee, lir::ValueStep::kEnd, at);
            if (!ended) {
              return ended;
            }
            return EmitDeleteCall(builder, pointee, held);
          },
          [&](const lir::StepByLibrary& library) -> diag::Result<void> {
            return EmitLibraryStep(builder, type, library.object, step, args);
          },
          [&](const lir::NoSuchStep&) -> diag::Result<void> {
            throw InternalError(
                std::format(
                    "llvm codegen: a value with no {} step is asked for one -- "
                    "please report this as a bug",
                    lir::ValueStepName(step)));
          }},
      lir::StepOf(*unit_, type, step));
}

auto CodeGenModule::EmitLibraryStep(
    llvm::IRBuilderBase& builder, lir::TypeId type,
    const support::RuntimeObject& object, lir::ValueStep step,
    std::span<llvm::Value* const> args) -> diag::Result<void> {
  // Ending a value takes it; assigning takes the one written into and then
  // the one written; copying and moving take the source and build in storage
  // handed last, which they answer with.
  std::span<llvm::Value* const> operands = args;
  ReturnInfo ret = ReturnDirect{.type = types_.Void()};
  switch (step) {
    case lir::ValueStep::kEnd:
    case lir::ValueStep::kAssign:
      break;
    case lir::ValueStep::kCopy:
    case lir::ValueStep::kMove:
      operands = args.first(1);
      ret = ReturnIndirect{.returned = types_.Ptr()};
      break;
  }
  const RuntimeOp op = RuntimeOpOf(step);
  const LlvmFnAbi abi = AtMachineTypes(
      *this,
      ArrangeLibraryStep(unit_->types, type, object, op, operands.size()), ret);
  auto crossed = BuildCallArgs(builder, abi, operands);
  if (!crossed) {
    return std::unexpected(std::move(crossed.error()));
  }
  const std::span<llvm::Value* const> built_in = args.subspan(operands.size());
  crossed->insert(crossed->end(), built_in.begin(), built_in.end());
  builder.CreateCall(RuntimeFunction(RuntimeSymbol(object, op), abi), *crossed);
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
  llvm::Type* const result = types_.Void();
  llvm::Type* const storage = types_.Ptr();
  return CreateRuntimeFunction(
      llvm::FunctionType::get(
          result, {storage, llvm::Type::getInt64Ty(*context_)}, false),
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
  const std::uint64_t bits = bytes * 8;
  const std::uint64_t mask =
      bits >= 64 ? ~std::uint64_t{0} : (std::uint64_t{1} << bits) - 1;
  return llvm::ConstantInt::get(
      llvm::Type::getIntNTy(*context_, static_cast<unsigned>(bits)),
      value & mask);
}

auto CodeGenModule::NameConstant(std::string_view name) -> llvm::Constant* {
  llvm::IRBuilder<> strings(*context_);
  return strings.CreateGlobalString(name, "", 0, module_.get());
}

auto CodeGenModule::RecordData(const lir::ConstantRecord& record)
    -> llvm::Constant* {
  const LibraryRecordLayout layout = LayoutOfLibraryRecord(record.decl);
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
  ConstantRecord out(*context_, layout.layout.size);
  for (std::size_t i = 0; i < layout.fields.size(); ++i) {
    const FieldLayout& field = layout.fields[i];
    const lir::Constant& part = record.parts[i];
    std::visit(
        Overloaded{
            [&](const IntegerField& integer_field) {
              const auto* integer = std::get_if<lir::ConstantInt>(&part.value);
              if (integer == nullptr) {
                mismatch("an integer");
              }
              out.Place(
                  field.offset, Int(static_cast<std::uint64_t>(integer->value),
                                    integer_field.size));
            },
            [&](PointerField) { out.Place(field.offset, AddressData(part)); },
            [&](RecordField) {
              const auto* nested =
                  std::get_if<lir::ConstantRecord>(&part.value);
              if (nested == nullptr) {
                mismatch("a structure");
              }
              out.Place(field.offset, RecordData(*nested));
            },
            [&](SpanField) {
              const auto* span = std::get_if<lir::ConstantSpan>(&part.value);
              if (span == nullptr) {
                mismatch("a view of entries");
              }
              out.Place(field.offset, SpanData(*span));
            }},
        field.kind);
  }
  return std::move(out).Build();
}

// A view as the library lays its own out: where the first entry is, and how
// many there are, which the viewed array's type says. A view of no entries
// points nowhere, as the library's own empty view does.
auto CodeGenModule::SpanData(const lir::ConstantSpan& span) -> llvm::Constant* {
  using View = value::ConstantSpan<std::byte>;
  const std::uint64_t count =
      unit_->types.Get(unit_->constants.Get(span.array).type)
          .Get<lir::MachineArrayType>()
          .size;
  ConstantRecord out(*context_, sizeof(View));
  out.Place(
      offsetof(View, data),
      count == 0 ? llvm::ConstantPointerNull::get(types_.Ptr())
                 : static_cast<llvm::Constant*>(GetAddrOfConstant(span.array)));
  out.Place(offsetof(View, size), Int(count, sizeof(View::size)));
  return std::move(out).Build();
}

auto CodeGenModule::DefineData(const lir::GlobalConstant& constant)
    -> llvm::GlobalVariable* {
  return DefineConstant(
      constant.symbol, ValueData(constant.type, constant.initializer),
      LinkageOf(constant.linkage), llvm::Align(DataAlign(constant.type)));
}

// A structure of the library is laid out as the library's declaration of it
// says, and an array of values one after another.
auto CodeGenModule::ValueData(lir::TypeId type, const lir::Constant& value)
    -> llvm::Constant* {
  return std::visit(
      Overloaded{
          [&](const lir::ConstantInt& c) -> llvm::Constant* {
            return Int(
                static_cast<std::uint64_t>(c.value),
                types_.LayoutOf(type).layout.size);
          },
          // The bytes are laid out by the value layer's own rule, so a
          // constant here and a value the library builds of the same type are
          // the same bytes. A constant whose type can hold x or z and that
          // holds none states no unknown plane, which is then clear.
          [&](const lir::ConstantIntegral& c) -> llvm::Constant* {
            const value::IntegralShape shape =
                types_.RequiredIntegralShapeOf(type);
            value::WrittenWords words(shape);
            const value::IntegralView planes = words.MutableView();
            if (c.value.value_words.size() != planes.value.size() ||
                c.value.state_words.size() > planes.unknown.size()) {
              throw InternalError(
                  "llvm codegen: an integral constant's planes do not span the "
                  "width its type states -- please report this as a bug");
            }
            std::ranges::copy(c.value.value_words, planes.value.begin());
            std::ranges::copy(c.value.state_words, planes.unknown.begin());
            std::vector<std::uint8_t> bytes(shape.Bytes());
            words.StoreTo(bytes.data());
            return llvm::ConstantDataArray::get(*context_, bytes);
          },
          [&](const lir::ConstantRecord& c) { return RecordData(c); },
          [&](const lir::ConstantArray& c) -> llvm::Constant* {
            const lir::TypeId element_type =
                unit_->types.Get(type).Get<lir::MachineArrayType>().element;
            std::vector<llvm::Constant*> elements;
            elements.reserve(c.elements.size());
            for (const lir::Constant& element : c.elements) {
              elements.push_back(ValueData(element_type, element));
            }
            llvm::Type* element_ty = elements.empty()
                                         ? llvm::Type::getInt8Ty(*context_)
                                         : elements.front()->getType();
            return llvm::ConstantArray::get(
                llvm::ArrayType::get(element_ty, elements.size()), elements);
          },
          [&](const lir::ConstantSpan& c) { return SpanData(c); },
          [&](const lir::ConstantNull&) -> llvm::Constant* {
            return llvm::ConstantPointerNull::get(types_.Ptr());
          },
          [&](const lir::ConstantPointer& c) { return AddressOf(c.to); }},
      value.value);
}

auto CodeGenModule::AddressData(const lir::Constant& address)
    -> llvm::Constant* {
  const auto no_address = [](std::string_view stated) -> llvm::Constant* {
    throw InternalError(
        std::format(
            "llvm codegen: a constant states {} where the library's structure "
            "holds an address -- please report this as a bug",
            stated));
  };
  return std::visit(
      Overloaded{
          [&](const lir::ConstantNull&) -> llvm::Constant* {
            return llvm::ConstantPointerNull::get(types_.Ptr());
          },
          [&](const lir::ConstantPointer& c) { return AddressOf(c.to); },
          [&](const lir::ConstantInt&) { return no_address("an integer"); },
          [&](const lir::ConstantIntegral&) {
            return no_address("an integral value");
          },
          [&](const lir::ConstantRecord&) { return no_address("a structure"); },
          [&](const lir::ConstantArray&) { return no_address("an array"); },
          [&](const lir::ConstantSpan&) { return no_address("a view"); }},
      address.value);
}

auto CodeGenModule::AddressOf(const lir::Referent& referent)
    -> llvm::Constant* {
  return std::visit(
      Overloaded{
          [&](const lir::FunctionReferent& r) -> llvm::Constant* {
            return UnitFunction(r.function);
          },
          [&](const lir::ExternalFunctionReferent& r) -> llvm::Constant* {
            return ExternalFunction(r.symbol, r.prototype);
          },
          [&](const lir::ConstantReferent& r) -> llvm::Constant* {
            return GetAddrOfConstant(r.constant);
          },
          [&](const lir::DefinitionReferent& r) -> llvm::Constant* {
            return GlobalNamed(lir::DefinitionSymbol(*unit_, r.cls));
          },
          [&](const lir::SharedStorageReferent& r) -> llvm::Constant* {
            return GlobalNamed(r.symbol);
          },
          [&](const lir::DestructorReferent& r) -> llvm::Constant* {
            return ValueFunction(
                lir::DestructorSymbol(
                    lir::DefinitionSymbol(*unit_, r.cls),
                    lir::Destructor::kDeleting));
          },
          [&](const lir::DispatchTableReferent& r) -> llvm::Constant* {
            return GlobalNamed(
                lir::DispatchTableSymbol(lir::DefinitionSymbol(*unit_, r.cls)));
          },
          [&](const lir::InterfaceTableReferent& r) -> llvm::Constant* {
            return GlobalNamed(
                lir::InterfaceTableSymbol(
                    lir::DefinitionSymbol(*unit_, r.cls),
                    lir::DefinitionSymbol(*unit_, r.iface)));
          },
          [&](const lir::TextReferent& r) -> llvm::Constant* {
            return NameConstant(r.text);
          }},
      referent);
}

// A structure of the library is aligned as the library declares it, and so is
// an array of them; every other value as its type lays it out.
auto CodeGenModule::DataAlign(lir::TypeId type) -> std::uint64_t {
  const lir::Type& held = unit_->types.Get(type);
  const auto* array = held.As<lir::MachineArrayType>();
  const lir::Type& element =
      array != nullptr ? unit_->types.Get(array->element) : held;
  if (const auto* library = element.As<lir::LibraryType>()) {
    return LayoutOfLibraryRecord(library->decl).layout.align;
  }
  return types_.LayoutOf(type).layout.align;
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

// The part the library builds where a class extends the class of the
// runtime's `decl` states, which holds the object's class's definition at its
// start.
auto LibraryClassPart(support::LibraryDecl decl) -> PlacedMembers {
  const support::ObjectLayout layout = runtime::LibraryPartLayoutOf(decl);
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
        "llvm codegen: an interface is laid out, which holds no storage "
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
  const std::optional<support::LibraryDecl> library = named.RuntimeClassOf();
  if (!library.has_value() && !declaration.has_value()) {
    throw InternalError(
        "llvm codegen: members are placed for a type that declares no storage "
        "-- please report this as a bug");
  }
  auto laid =
      library.has_value()
          ? diag::Result<PlacedMembers>{LibraryClassPart(*library)}
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
  const auto held_elsewhere = [](const auto&) { return false; };
  return std::visit(
      Overloaded{
          // A value of the program held inline is its own bytes where this
          // target lays it out -- a scalar, or a value laid out by its type --
          // and an object of the library's otherwise.
          [&](const support::InlineValueMember&) {
            const lir::Type& held = unit_->types.Get(type);
            return lir::IsScalar(held) || lir::IsLaidOutByItsType(held);
          },
          [](const support::InlineBytesMember&) { return true; },
          [&](const support::ObservableCellMember& m) {
            return held_elsewhere(m);
          },
          [&](const support::ResolvedNetMember& m) {
            return held_elsewhere(m);
          },
          [&](const support::SampledHistoryMember& m) {
            return held_elsewhere(m);
          },
          [&](const support::ValueCellMember& m) { return held_elsewhere(m); },
          [&](const support::BorrowedHandleMember& m) {
            return held_elsewhere(m);
          },
          [&](const support::LibraryDeclMember& m) {
            return held_elsewhere(m);
          },
          [&](const support::LibraryObjectMember& m) {
            return held_elsewhere(m);
          }},
      storage);
}

auto CodeGenModule::MemberLayout(
    lir::TypeId type, support::DeclaredMemberStorage storage)
    -> support::ObjectLayout {
  if (HoldsItsBytesInline(type, storage)) {
    return types_.LayoutOf(type);
  }
  return runtime::LayoutOf(
      storage,
      ValuesHeldByMember(*unit_, type).transform([&](lir::TypeId held) {
        return types_.LayoutOf(held);
      }));
}

auto CodeGenModule::CellStorage(lir::TypeId cell) const
    -> support::DeclaredMemberStorage {
  const std::optional<support::DeclaredMemberStorage> storage =
      VariableStorageOf(*unit_, cell);
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

auto CodeGenModule::BeginMember(
    llvm::IRBuilderBase& builder, llvm::Value* value,
    const PlacedMembers& placed, std::size_t i) -> diag::Result<void> {
  return BeginStorageAt(
      builder, placed.types[i], placed.storage[i],
      builder.CreateConstInBoundsGEP1_64(
          builder.getInt8Ty(), value, placed.offsets[i]));
}

// Storage holding a value as its own bytes has no value until one is copied
// in, which is how the one value holding such members -- a closure's captures
// -- is filled, so nothing builds it; any other is the library's to build.
auto CodeGenModule::BeginStorageAt(
    llvm::IRBuilderBase& builder, lir::TypeId type,
    support::DeclaredMemberStorage storage, llvm::Value* at)
    -> diag::Result<void> {
  if (HoldsItsBytesInline(type, storage)) {
    throw InternalError(
        "llvm codegen: a value held as its bytes is built by copying a value "
        "in, never default-built -- please report this as a bug");
  }
  return BuildMemberStorage(builder, type, storage, at);
}

// The entry building a member's storage takes where the storage is, after
// what it is told of the values held there.
auto CodeGenModule::BuildMemberStorage(
    llvm::IRBuilderBase& builder, lir::TypeId type,
    support::DeclaredMemberStorage storage, llvm::Value* at)
    -> diag::Result<void> {
  llvm::Type* const result = types_.Void();
  const LlvmFnAbi abi = AtMachineTypes(
      *this, ArrangeMemberBuild(*unit_, type, storage),
      ReturnDirect{.type = result});
  const std::array<llvm::Value*, 1> operands{at};
  auto args = BuildCallArgs(builder, abi, operands);
  if (!args) {
    return std::unexpected(std::move(args.error()));
  }
  builder.CreateCall(
      RuntimeFunction(RuntimeSymbol(storage, RuntimeOp::kConstruct), abi),
      *args);
  return {};
}

auto CodeGenModule::EndMember(
    llvm::IRBuilderBase& builder, llvm::Value* value,
    const PlacedMembers& placed, std::size_t i) -> diag::Result<void> {
  return EndStorageAt(
      builder, placed.types[i], placed.storage[i],
      builder.CreateConstInBoundsGEP1_64(
          builder.getInt8Ty(), value, placed.offsets[i]));
}

// Storage holding a value as its own bytes ends as the value does; any other
// is the library's, which ends it where ending it does anything.
auto CodeGenModule::EndStorageAt(
    llvm::IRBuilderBase& builder, lir::TypeId type,
    support::DeclaredMemberStorage storage, llvm::Value* at)
    -> diag::Result<void> {
  const std::array<llvm::Value*, 1> args{at};
  if (HoldsItsBytesInline(type, storage)) {
    return EmitStep(builder, type, lir::ValueStep::kEnd, args);
  }
  if (!MemberLayout(type, storage).needs_end) {
    return {};
  }
  builder.CreateCall(MemberStorageDestructor(storage), args);
  return {};
}

auto CodeGenModule::CreationSymbol(lir::TypeId cls) const -> std::string {
  return lir::CreationSymbol(
      *lir::DefinitionSymbol(*unit_, cls), lir::Creation::kAllocating);
}

auto CodeGenModule::ValueFunction(const std::string& symbol)
    -> llvm::Function* {
  llvm::Type* const result = types_.Void();
  llvm::Type* const object = types_.Ptr();
  return llvm::cast<llvm::Function>(
      module_
          ->getOrInsertFunction(
              symbol, llvm::FunctionType::get(result, {object}, false))
          .getCallee());
}

void CodeGenModule::BeginParts(
    llvm::IRBuilderBase& builder, lir::TypeId cls, llvm::Value* value) {
  if (const std::optional<support::LibraryDecl> library =
          unit_->types.Get(cls).RuntimeClassOf()) {
    llvm::Type* const result = types_.Void();
    llvm::Type* const kind = builder.getInt8Ty();
    llvm::Type* const object = types_.Ptr();
    const llvm::FunctionCallee part = CreateRuntimeFunction(
        llvm::FunctionType::get(result, {kind, object}, false),
        RuntimeSymbol(RuntimeOp::kBeginLibraryPart));
    llvm::Value* const which =
        builder.getInt8(static_cast<std::uint8_t>(*library));
    builder.CreateCall(part, {which, value});
    return;
  }
  builder.CreateCall(
      ValueFunction(
          lir::CreationSymbol(
              *lir::DefinitionSymbol(*unit_, cls), lir::Creation::kBaseObject)),
      {value});
}

void CodeGenModule::EndParts(
    llvm::IRBuilderBase& builder, lir::TypeId cls, llvm::Value* value) {
  if (const std::optional<support::LibraryDecl> library =
          unit_->types.Get(cls).RuntimeClassOf()) {
    llvm::Type* const result = types_.Void();
    llvm::Type* const kind = builder.getInt8Ty();
    llvm::Type* const object = types_.Ptr();
    const llvm::FunctionCallee part = CreateRuntimeFunction(
        llvm::FunctionType::get(result, {kind, object}, false),
        RuntimeSymbol(RuntimeOp::kEndLibraryPart));
    llvm::Value* const which =
        builder.getInt8(static_cast<std::uint8_t>(*library));
    builder.CreateCall(part, {which, value});
    return;
  }
  builder.CreateCall(
      ValueFunction(
          lir::DestructorSymbol(
              *lir::DefinitionSymbol(*unit_, cls),
              lir::Destructor::kBaseObject)),
      {value});
}

auto CodeGenModule::EntryAddress(const std::optional<lir::TableBody>& body)
    -> llvm::Constant* {
  // A slot a class leaves abstract holds no body (LRM 8.21).
  if (!body.has_value()) {
    return llvm::ConstantPointerNull::get(types_.Ptr());
  }
  return AddressOf(lir::ReferentOf(*body));
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
  // What ends a value of the closure is the function the unit states for it.
  llvm::Function* end = UnitFunction(
      lir::StepFunctionOf(
          *unit_,
          unit_->types.Intern(lir::Type{lir::ClosureType{.closure_id = id}}),
          lir::ValueStep::kEnd));

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
  // The definition is named wherever the invoke is, and nowhere else.
  DefineConstant(
      symbol, std::move(out).Build(), LinkageOf(invoke.linkage),
      llvm::Align(alignof(CalledDefinition)));
  return {};
}

auto CodeGenModule::DomainOf(lir::TypeId type) const
    -> diag::Result<support::ValueDomain> {
  return RealizedDomainOf(*unit_, type);
}

auto CodeGenModule::SharedStorageOf(const lir::StaticStorage& storage)
    -> diag::Result<support::DeclaredMemberStorage> {
  const std::optional<support::DeclaredMemberStorage> described =
      VariableStorageOf(*unit_, storage.type);
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
  llvm::GlobalVariable* shared = GlobalNamed(storage.symbol);
  if (auto built =
          BuildMemberStorage(builder, storage.type, *described, shared);
      !built) {
    return built;
  }
  if (!MemberLayout(storage.type, *described).needs_end) {
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
  return GlobalNamed(std::string(symbol));
}

auto CodeGenModule::GlobalNamed(const std::string& symbol)
    -> llvm::GlobalVariable* {
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
  return GlobalNamed(*symbol);
}

auto CodeGenModule::GetAddrOfConstant(lir::ConstantId constant)
    -> llvm::GlobalVariable* {
  if (const auto found = constants_.find(constant); found != constants_.end()) {
    return found->second;
  }
  // Data of this artifact alone, which nothing outside it names, so it is
  // private, as clang defines an unnamed constant.
  const lir::ConstantDecl& decl = unit_->constants.Get(constant);
  llvm::Constant* const value = ValueData(decl.type, decl.value);
  auto* defined = new llvm::GlobalVariable(
      *module_, value->getType(), true, llvm::GlobalValue::PrivateLinkage,
      value, std::format("constant.{}", constant.value));
  defined->setAlignment(llvm::Align(DataAlign(decl.type)));
  constants_.emplace(constant, defined);
  return defined;
}

}  // namespace lyra::backend::llvm_backend
