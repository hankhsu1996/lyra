#include "lyra/backend/llvm/codegen_tuple.hpp"

#include <array>
#include <cstddef>
#include <cstdint>
#include <format>
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
#include <llvm/IR/Module.h>

#include "lyra/backend/llvm/codegen_module.hpp"
#include "lyra/backend/llvm/runtime_entry.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/struct_id.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/runtime/object_layout.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/runtime_object.hpp"
#include "lyra/support/tuple_operations.hpp"
#include "lyra/support/value_domain.hpp"
#include "lyra/support/value_operation.hpp"

namespace lyra::backend::llvm_backend {

namespace {

using support::ValueDomain;

// The table is read by the runtime through the one C++ definition both sides
// compile against, and built here field by field in that order; its size is
// what says no field was added on one side only.
constexpr std::size_t kLifecycleSlots = 4;
constexpr std::size_t kOperationSlots = 13;
static_assert(
    sizeof(support::TupleOperations) ==
    (4 * sizeof(std::uint32_t)) + sizeof(void*) +
        ((kLifecycleSlots + kOperationSlots) * sizeof(void*)));
static_assert(
    sizeof(support::TupleComponent) ==
    sizeof(std::uint32_t) + sizeof(std::uint32_t) + sizeof(void*));

constexpr std::array<TupleLifecycle, kLifecycleSlots> kLifecycle{
    TupleLifecycle::kCopy, TupleLifecycle::kMove, TupleLifecycle::kDestroy,
    TupleLifecycle::kAssign};

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

auto StepOp(TupleLifecycle step) -> RuntimeOp {
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

// The operation each of the table's operation slots holds, in the order it
// lists them.
const std::array<support::ValueOperation, kOperationSlots> kOperationSlotOrder{
    support::ValueOperator::kEquality,    support::BuiltinFn::kCaseEqual,
    support::BuiltinFn::kBitIdentical,    support::BuiltinFn::kHasUnknown,
    support::BuiltinFn::kBitstreamWidth,  support::BuiltinFn::kCountBits,
    support::BuiltinFn::kToBitstream,     support::BuiltinFn::kFromBitstream,
    support::BuiltinFn::kResolveTriState, support::BuiltinFn::kResolveWiredAnd,
    support::BuiltinFn::kResolveWiredOr,  support::BuiltinFn::kDominating,
    support::BuiltinFn::kFilledLike,
};

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
  // its components.
  if (const std::optional<lir::TypeDeclarationRef> declared =
          DeclarationOf(tuple)) {
    return keys_
        .emplace(
            tuple,
            std::format("{{{}::{}}}", declared->unit_name, declared->name))
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

auto CodeGenTuples::Slot(std::optional<lir::FunctionId> function)
    -> llvm::Constant* {
  if (!function.has_value()) {
    return llvm::ConstantPointerNull::get(types_->Ptr());
  }
  return owner_->UnitFunction(*function);
}

auto CodeGenTuples::Operations(lir::TypeId tuple) -> llvm::GlobalVariable* {
  const std::string& key = KeyOf(tuple);
  if (const auto found = tables_.find(key); found != tables_.end()) {
    return found->second;
  }
  llvm::Module& module = owner_->Module();
  llvm::LLVMContext& ctx = module.getContext();
  llvm::Type* ptr = types_->Ptr();
  llvm::Type* word = llvm::Type::getInt32Ty(ctx);
  std::vector<llvm::Type*> fields{word, word, word, ptr};
  fields.insert(fields.end(), kLifecycleSlots + kOperationSlots, ptr);
  llvm::StructType* table_ty = llvm::StructType::get(ctx, fields);
  const auto global = [&](std::string name, llvm::Type* type) {
    auto* made =
        llvm::cast<llvm::GlobalVariable>(module.getOrInsertGlobal(name, type));
    made->setLinkage(llvm::GlobalValue::InternalLinkage);
    made->setConstant(true);
    return made;
  };
  llvm::GlobalVariable* table =
      global(std::format("lyra.tuple{}", key), table_ty);
  table->setAlignment(llvm::Align(alignof(support::TupleOperations)));
  // Named before it is filled: filling it emits the lifecycle, and each step
  // that builds a tuple writes this table's address into it.
  tables_.emplace(key, table);

  // A struct has one table in the program, the declaring unit's, since that
  // unit is the one stating its methods; another unit refers to it and fills
  // nothing. A tuple has no methods, and each module keeps its own table of
  // one.
  std::span<const lir::StructMethod> methods;
  if (const auto* structure = unit_->types.Get(tuple).As<lir::StructType>()) {
    table->setLinkage(llvm::GlobalValue::ExternalLinkage);
    const auto* own = std::get_if<lir::StructId>(&structure->declaration);
    if (own == nullptr) {
      return table;
    }
    methods = unit_->structs.Get(*own).methods;
  }

  const TupleLayout& layout = types_->LayoutOfTuple(tuple);
  llvm::StructType* component_ty =
      llvm::StructType::get(ctx, {word, llvm::Type::getInt8Ty(ctx), ptr});
  std::vector<llvm::Constant*> components;
  components.reserve(layout.components.size());
  for (std::size_t i = 0; i < layout.components.size(); ++i) {
    const lir::TypeId component = layout.components[i];
    const bool nested = unit_->types.Get(component).IsProduct();
    const ValueDomain domain = *ValueDomainOf(*unit_, component);
    components.push_back(
        llvm::ConstantStruct::get(
            component_ty,
            {llvm::ConstantInt::get(word, layout.offsets[i]),
             llvm::ConstantInt::get(
                 llvm::Type::getInt8Ty(ctx),
                 static_cast<std::uint64_t>(domain)),
             nested ? llvm::cast<llvm::Constant>(Operations(component))
                    : llvm::ConstantPointerNull::get(
                          llvm::cast<llvm::PointerType>(ptr))}));
  }
  auto* components_ty =
      llvm::ArrayType::get(component_ty, layout.components.size());
  llvm::GlobalVariable* listed =
      global(std::format("lyra.tuple{}.components", key), components_ty);
  listed->setInitializer(llvm::ConstantArray::get(components_ty, components));

  std::vector<llvm::Constant*> values{
      llvm::ConstantInt::get(word, layout.storage.size),
      llvm::ConstantInt::get(word, layout.storage.align),
      llvm::ConstantInt::get(word, layout.components.size()), listed};
  for (const TupleLifecycle step : kLifecycle) {
    values.push_back(Function(tuple, step));
  }
  for (const support::ValueOperation& operation : kOperationSlotOrder) {
    values.push_back(Slot(MethodAnswering(methods, operation)));
  }
  table->setInitializer(llvm::ConstantStruct::get(table_ty, values));
  return table;
}

auto CodeGenTuples::EmitDeclared() -> void {
  for (const lir::StructId id : unit_->structs.Ids()) {
    Operations(
        unit_->types.Intern(lir::Type{lir::StructType{.declaration = id}}));
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
  llvm::Type* ptr = types_->Ptr();
  const TupleLayout& layout = types_->LayoutOfTuple(tuple);

  // Where component `i` lies in the tuple at `base`.
  const auto at = [&](llvm::Value* base, std::size_t i) -> llvm::Value* {
    return b.CreateConstInBoundsGEP1_64(b.getInt8Ty(), base, layout.offsets[i]);
  };
  // Component `i`'s own step: the nested type's own function where the
  // component is a tuple, and its domain's entry otherwise, which answers with
  // the storage it built in where the tuple's own step answers nothing.
  const auto component = [&](std::size_t i, std::vector<llvm::Value*> args) {
    const lir::TypeId type = layout.components[i];
    if (unit_->types.Get(type).IsProduct()) {
      b.CreateCall(Function(type, step), args);
      return;
    }
    const ValueDomain domain = *ValueDomainOf(*unit_, type);
    if (step == TupleLifecycle::kDestroy &&
        runtime::LayoutOf(domain).ends_with_nothing_to_do) {
      return;
    }
    const bool builds =
        step == TupleLifecycle::kCopy || step == TupleLifecycle::kMove;
    std::vector<llvm::Type*> params;
    params.reserve(args.size());
    for (llvm::Value* value : args) {
      params.push_back(value->getType());
    }
    b.CreateCall(
        owner_->Module().getOrInsertFunction(
            RuntimeSymbol(domain, StepOp(step)),
            llvm::FunctionType::get(
                builds ? ptr : b.getVoidTy(), params, false)),
        args);
  };

  const std::size_t count = layout.components.size();
  switch (step) {
    case TupleLifecycle::kCopy:
    case TupleLifecycle::kMove:
      b.CreateStore(Operations(tuple), fn->getArg(1));
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
