#include "lyra/backend/llvm/codegen_module.hpp"

#include <format>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Transforms/Utils/ModuleUtils.h>

#include "lyra/backend/llvm/codegen_function.hpp"
#include "lyra/backend/llvm/runtime_entry.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/symbol_name.hpp"
#include "lyra/lir/type.hpp"

namespace lyra::backend::llvm_backend {

CodeGenModule::CodeGenModule(
    const lir::CompilationUnit& unit, TimeResolution time)
    : context_(std::make_unique<llvm::LLVMContext>()),
      // A module carries the name of the unit it holds, because the party
      // composing the program derives names of its own from it -- one that
      // collects what runs before the program starts gives its result a name
      // made from this one, so two modules sharing an identifier are two
      // artifacts that cannot be composed together.
      module_(std::make_unique<llvm::Module>(unit.name, *context_)),
      unit_(&unit),
      time_(time),
      types_(*context_, unit),
      functions_(unit.functions.size()) {
  for (const lir::ClassId id : unit.classes.Ids()) {
    const lir::Class& cls = unit.classes.Get(id);
    // A class the runtime drives is the one a construction reaches holding a
    // definition and nothing else, so its construction is what answers to the
    // shared prototype. A class standing in the tree that supplies no way to
    // run is what a unit promised of its object: nothing constructs one through
    // a definition, and what extends it enters it by name with its own
    // arguments already in hand.
    if (cls.tree_program.has_value()) {
      scope_constructions_.insert(cls.constructor);
    }
  }
}

auto CodeGenModule::IsScopeConstruction(lir::FunctionId function) const
    -> bool {
  return scope_constructions_.contains(function);
}

auto CodeGenModule::Run() -> diag::Result<EmittedModule> {
  // Every function is declared before any body is generated, because a body may
  // call one whose own body is generated later, including itself.
  for (const lir::FunctionId id : unit_->functions.Ids()) {
    functions_.Append(DeclareCallable(id));
  }
  type_descriptor_cells_ =
      base::Translation<lir::TypeDescriptorId, llvm::GlobalVariable*>(
          unit_->type_descriptor_initializers.size());
  for (const lir::TypeDescriptorId id :
       unit_->type_descriptor_initializers.Ids()) {
    type_descriptor_cells_.Append(DeclareTypeDescriptorCell(id));
  }
  integral_constant_cells_ =
      base::Translation<lir::IntegralConstantId, llvm::GlobalVariable*>(
          unit_->integral_constant_initializers.size());
  for (const lir::IntegralConstantId id :
       unit_->integral_constant_initializers.Ids()) {
    integral_constant_cells_.Append(DeclareIntegralConstantCell(id));
  }
  variable_schema_cells_ =
      base::Translation<lir::FunctionId, llvm::GlobalVariable*>(
          unit_->functions.size());
  for (const lir::FunctionId id : unit_->functions.Ids()) {
    variable_schema_cells_.Append(DeclareVariableSchemaCell(id));
  }
  for (const lir::FunctionId id : unit_->functions.Ids()) {
    auto generated = CodeGenFunction(*this, id).Run();
    if (!generated) {
      return std::unexpected(std::move(generated.error()));
    }
  }
  auto declared = EmitUnitDeclaration();
  if (!declared) {
    return std::unexpected(std::move(declared.error()));
  }

  std::string error;
  llvm::raw_string_ostream os(error);
  if (llvm::verifyModule(*module_, &os)) {
    throw InternalError(
        std::format("llvm codegen: produced an invalid module: {}", os.str()));
  }
  return EmittedModule{std::move(context_), std::move(module_)};
}

namespace {

// What the linker is told to do where a second artifact defines the same name.
// A definition several of them write is one the session keeps one of, which is
// what every target spells as a weak definition; one this artifact owns is
// written once and a repeat of it is a program that does not link.
auto LinkageOf(lir::Definition definition) -> llvm::GlobalValue::LinkageTypes {
  switch (definition) {
    case lir::Definition::kOwned:
      return llvm::Function::ExternalLinkage;
    case lir::Definition::kShared:
      return llvm::Function::WeakODRLinkage;
  }
  throw InternalError("llvm codegen: unknown definition kind");
}

}  // namespace

auto CodeGenModule::DeclareCallable(lir::FunctionId id) -> llvm::Function* {
  const lir::Function& fn = unit_->functions.Get(id);
  std::vector<llvm::Type*> params;
  params.reserve(fn.params.size());
  for (const lir::ValueId param : fn.params) {
    params.push_back(types_.Map(fn.values.Get(param).type));
  }
  // A scope's construction answers to the one prototype every class's shares,
  // because what reaches it holds the class's definition and not its name: the
  // parameters the construction has in common with every other, and then the
  // values this class alone is parameterized by, collected into one span.
  if (IsScopeConstruction(id)) {
    params.resize(kScopeConstructSharedParams);
    params.push_back(types_.Span());
  }
  // A body answering a value its caller will own builds that value in storage
  // the caller gives, handed over last, and answers with it -- the shape every
  // runtime entry answering one has.
  if (unit_->types.Get(fn.result_type).IsOwnedValue()) {
    params.push_back(types_.Ptr());
  }
  auto* fn_ty =
      llvm::FunctionType::get(types_.Map(fn.result_type), params, false);
  return llvm::Function::Create(
      fn_ty, LinkageOf(fn.definition), fn.name, module_.get());
}

auto CodeGenModule::UnitFunction(lir::FunctionId function) -> llvm::Function* {
  return functions_.Get(function);
}

auto CodeGenModule::EmitUnitDeclaration() -> diag::Result<void> {
  auto* fn_ty =
      llvm::FunctionType::get(llvm::Type::getVoidTy(*context_), {}, false);
  // Nothing outside this module calls it: whoever composes the program finds
  // it in the module's own list of what runs before the program starts. So it
  // is linked under no name another artifact could reach, and each module
  // carrying one does not collide with any other.
  auto* fn = llvm::Function::Create(
      fn_ty, llvm::Function::InternalLinkage, "unit_declarations",
      module_.get());
  auto* entry = llvm::BasicBlock::Create(*context_, "", fn);
  llvm::IRBuilder<> builder(entry);
  for (const lir::FunctionId id : unit_->functions.Ids()) {
    auto stated = StateVariableSchema(builder, id);
    if (!stated) {
      return std::unexpected(std::move(stated.error()));
    }
  }
  for (const lir::StaticStorage& storage : unit_->static_storage) {
    auto stated = StateSharedStorage(builder, storage);
    if (!stated) {
      return std::unexpected(std::move(stated.error()));
    }
  }
  for (const lir::ClosureId id : unit_->closures.Ids()) {
    auto stated = StateClosure(builder, id);
    if (!stated) {
      return std::unexpected(std::move(stated.error()));
    }
  }
  for (const lir::ClassId id : unit_->classes.Ids()) {
    auto stated = StateClass(builder, id);
    if (!stated) {
      return std::unexpected(std::move(stated.error()));
    }
  }
  for (const lir::StructId id : unit_->structs.Ids()) {
    auto stated = StateStruct(builder, id);
    if (!stated) {
      return std::unexpected(std::move(stated.error()));
    }
  }
  builder.CreateRetVoid();
  llvm::appendToGlobalCtors(*module_, fn, 0);
  return {};
}

auto CodeGenModule::DescribedStorage(
    std::span<const lir::Member> members, MemberSlotRole role,
    std::string_view what) -> diag::Result<llvm::Constant*> {
  auto* byte_ty = llvm::Type::getInt8Ty(*context_);
  auto* described_ty = llvm::StructType::get(*context_, {byte_ty, byte_ty});
  std::vector<llvm::Constant*> described;
  described.reserve(members.size());
  for (const lir::Member& member : members) {
    const std::optional<support::DeclaredMemberStorage> storage =
        DeclaredStorageOf(*unit_, member.type, role);
    if (!storage) {
      return diag::Fail(
          diag::DiagCode::kUnsupportedTypeKind,
          std::format(
              "llvm codegen: {} of type {} has no storage realization on this "
              "backend",
              what, unit_->types.Get(member.type).KindName()));
    }
    described.push_back(
        llvm::ConstantStruct::get(
            described_ty,
            {llvm::ConstantInt::get(
                 byte_ty, static_cast<std::uint64_t>(storage->kind)),
             llvm::ConstantInt::get(
                 byte_ty, static_cast<std::uint64_t>(storage->domain))}));
  }
  auto* described_array_ty =
      llvm::ArrayType::get(described_ty, described.size());
  // The label reaches no linker, so which description this is among the
  // module's own is enough to tell one from another.
  auto* data = llvm::cast<llvm::GlobalVariable>(module_->getOrInsertGlobal(
      std::format("described_storage_{}", described_storage_count_++),
      described_array_ty));
  data->setLinkage(llvm::GlobalValue::PrivateLinkage);
  data->setConstant(true);
  data->setInitializer(llvm::ConstantArray::get(described_array_ty, described));
  return data;
}

auto CodeGenModule::StateVariableSchema(
    llvm::IRBuilderBase& builder, lir::FunctionId id) -> diag::Result<void> {
  llvm::GlobalVariable* cell = variable_schema_cells_.Get(id);
  if (cell == nullptr) {
    return {};
  }
  const lir::Function& body = unit_->functions.Get(id);
  std::vector<lir::Member> variables;
  variables.reserve(body.variables.size());
  for (const lir::TypeId type : body.variables) {
    variables.push_back(lir::Member{.type = type});
  }
  auto described =
      DescribedStorage(variables, MemberSlotRole::kVariable, "a variable");
  if (!described) {
    return std::unexpected(std::move(described.error()));
  }
  auto* ptr_ty = types_.Ptr();
  auto* count_ty = llvm::Type::getInt64Ty(*context_);
  llvm::FunctionCallee declare = module_->getOrInsertFunction(
      RuntimeSymbol(RuntimeOp::kVariableSchemaDeclare),
      llvm::FunctionType::get(ptr_ty, {ptr_ty, count_ty}, false));
  builder.CreateStore(
      builder.CreateCall(
          declare, {*described,
                    llvm::ConstantInt::get(count_ty, body.variables.size())}),
      cell);
  return {};
}

auto CodeGenModule::StateCall(
    llvm::IRBuilderBase& builder, RuntimeOp op,
    std::span<llvm::Value* const> args, llvm::Type* result) -> llvm::Value* {
  std::vector<llvm::Type*> params;
  params.reserve(args.size());
  for (llvm::Value* arg : args) {
    params.push_back(arg->getType());
  }
  llvm::FunctionCallee entry = module_->getOrInsertFunction(
      RuntimeSymbol(op), llvm::FunctionType::get(result, params, false));
  const std::vector<llvm::Value*> stated(args.begin(), args.end());
  return builder.CreateCall(entry, stated);
}

auto CodeGenModule::StatedName(std::string_view name)
    -> std::array<llvm::Value*, 2> {
  auto [entry, first] = stated_names_.try_emplace(std::string{name}, nullptr);
  if (first) {
    auto* bytes = llvm::ConstantDataArray::getString(*context_, name, false);
    // The label reaches no linker, so the order the module first states a
    // spelling in is enough to tell one from another -- never the spelling,
    // which admits every printable character.
    auto* data = llvm::cast<llvm::GlobalVariable>(module_->getOrInsertGlobal(
        std::format("stated_name_{}", stated_names_.size() - 1),
        bytes->getType()));
    data->setLinkage(llvm::GlobalValue::PrivateLinkage);
    data->setConstant(true);
    data->setInitializer(bytes);
    entry->second = data;
  }
  return {
      entry->second,
      llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context_), name.size())};
}

auto CodeGenModule::StateClass(llvm::IRBuilderBase& builder, lir::ClassId id)
    -> diag::Result<void> {
  const lir::Class& cls = unit_->classes.Get(id);
  const lir::SymbolPart part = lir::SymbolPartOf(cls.name, id.value);
  auto* ptr_ty = types_.Ptr();
  auto* void_ty = llvm::Type::getVoidTy(*context_);
  auto* byte_ty = llvm::Type::getInt8Ty(*context_);
  auto* position_ty = llvm::Type::getInt32Ty(*context_);
  auto* count_ty = llvm::Type::getInt64Ty(*context_);

  const lir::ObjectTreeProgram* driven_by = lir::TreeProgramOf(cls);
  const bool in_tree = lir::StandsInObjectTree(*unit_, cls.base);
  llvm::Value* declared = nullptr;
  if (in_tree) {
    const std::array<llvm::Value*, 2> stated{
        llvm::ConstantInt::getSigned(byte_ty, time_.unit_power),
        llvm::ConstantInt::getSigned(byte_ty, time_.precision_power)};
    declared =
        StateCall(builder, RuntimeOp::kScopeClassDeclare, stated, ptr_ty);
  } else {
    declared = StateCall(builder, RuntimeOp::kClassDeclare, {}, ptr_ty);
  }
  llvm::GlobalVariable* cell =
      DeclaredCell(lir::ClassDefinitionSymbol(unit_->name, part));
  cell->setInitializer(llvm::ConstantPointerNull::get(ptr_ty));
  builder.CreateStore(declared, cell);

  if (cls.base.has_value()) {
    const std::optional<lir::TypeId> base = lir::BaseType(*unit_, *cls.base);
    if (base.has_value()) {
      auto extended = DefinitionRef(*base);
      if (!extended) {
        return std::unexpected(std::move(extended.error()));
      }
      const std::array<llvm::Value*, 2> stated{declared, *extended};
      StateCall(builder, RuntimeOp::kClassDeclareBase, stated, void_ty);
    }
  }

  auto members =
      DescribedStorage(cls.members, MemberSlotRole::kVariable, "a property");
  if (!members) {
    return std::unexpected(std::move(members.error()));
  }
  const std::array<llvm::Value*, 3> held{
      declared, *members, llvm::ConstantInt::get(count_ty, cls.members.size())};
  StateCall(builder, RuntimeOp::kClassDeclareMembers, held, void_ty);

  // A behavior declared with no implementation is answered by nothing, which
  // no object of a constructible class ever reaches (LRM 8.21).
  for (const lir::Introduction& introduced : cls.introduces) {
    const std::array<llvm::Value*, 2> stated{
        declared, introduced.body.has_value()
                      ? llvm::cast<llvm::Value>(UnitFunction(*introduced.body))
                      : llvm::ConstantPointerNull::get(ptr_ty)};
    StateCall(builder, RuntimeOp::kClassDeclareIntroduction, stated, void_ty);
  }
  for (const lir::DispatchTakeover& taken : cls.takeovers) {
    auto introducer = DefinitionRef(taken.method.introduced_by);
    if (!introducer) {
      return std::unexpected(std::move(introducer.error()));
    }
    const std::array<llvm::Value*, 4> stated{
        declared, *introducer,
        llvm::ConstantInt::get(position_ty, taken.method.ordinal.value),
        UnitFunction(taken.body)};
    StateCall(builder, RuntimeOp::kClassDeclareTakeover, stated, void_ty);
  }
  for (const lir::NamedMember& named : cls.named_members) {
    const auto [bytes, length] = StatedName(named.name);
    const std::array<llvm::Value*, 4> stated{
        declared, bytes, length,
        llvm::ConstantInt::get(position_ty, named.position)};
    StateCall(builder, RuntimeOp::kClassDeclarePropertyName, stated, void_ty);
  }
  // A class gives its introductions positions in the order it introduces them,
  // and states no position of its own for one, so the order is the position.
  std::uint32_t behavior_position = 0;
  for (const lir::Introduction& introduced : cls.introduces) {
    const auto [bytes, length] = StatedName(introduced.name);
    const std::array<llvm::Value*, 4> stated{
        declared, bytes, length,
        llvm::ConstantInt::get(position_ty, behavior_position)};
    StateCall(builder, RuntimeOp::kClassDeclareBehaviorName, stated, void_ty);
    ++behavior_position;
  }
  for (const lir::DeclaredBody& declares : cls.bodies) {
    const auto [bytes, length] = StatedName(declares.name);
    const std::array<llvm::Value*, 4> stated{
        declared, bytes, length, UnitFunction(declares.body)};
    StateCall(builder, RuntimeOp::kClassDeclareBodyName, stated, void_ty);
  }

  if (!in_tree) {
    return {};
  }
  if (driven_by != nullptr) {
    const std::array<llvm::Value*, 5> stated{
        declared, UnitFunction(driven_by->resolve_state),
        UnitFunction(driven_by->initialize_state),
        UnitFunction(driven_by->create_processes),
        UnitFunction(cls.constructor)};
    StateCall(builder, RuntimeOp::kScopeDeclareProgram, stated, void_ty);
  }
  for (const lir::PublishedCallable& published : cls.subroutines) {
    const auto [bytes, length] = StatedName(published.name);
    const std::array<llvm::Value*, 4> stated{
        declared, bytes, length, UnitFunction(published.entry)};
    StateCall(builder, RuntimeOp::kScopeDeclareSubroutine, stated, void_ty);
  }
  for (const lir::PublishedCallable& published : cls.exports) {
    const auto [bytes, length] = StatedName(published.name);
    const std::array<llvm::Value*, 4> stated{
        declared, bytes, length, UnitFunction(published.entry)};
    StateCall(builder, RuntimeOp::kScopeDeclareExport, stated, void_ty);
  }
  for (const lir::DeclaredClass& answers : cls.declares) {
    const auto [bytes, length] = StatedName(answers.name);
    const std::array<llvm::Value*, 4> stated{
        declared, bytes, length,
        DeclaredCell(
            lir::ClassDefinitionSymbol(
                unit_->name, lir::SymbolPartOf(
                                 unit_->classes.Get(answers.declaration).name,
                                 answers.declaration.value)))};
    StateCall(builder, RuntimeOp::kScopeDeclareClass, stated, void_ty);
  }
  return {};
}

auto CodeGenModule::StateStruct(llvm::IRBuilderBase& builder, lir::StructId id)
    -> diag::Result<void> {
  const lir::Struct& record = unit_->structs.Get(id);
  const std::string symbol = lir::StructDefinitionSymbol(
      unit_->name, lir::SymbolPart::Ordinal(id.value));
  auto* ptr_ty = types_.Ptr();
  llvm::Value* declared =
      StateCall(builder, RuntimeOp::kClassDeclare, {}, ptr_ty);
  llvm::GlobalVariable* cell = DeclaredCell(symbol);
  cell->setInitializer(llvm::ConstantPointerNull::get(ptr_ty));
  builder.CreateStore(declared, cell);
  auto fields =
      DescribedStorage(record.fields, MemberSlotRole::kVariable, "a field");
  if (!fields) {
    return std::unexpected(std::move(fields.error()));
  }
  const std::array<llvm::Value*, 3> held{
      declared, *fields,
      llvm::ConstantInt::get(
          llvm::Type::getInt64Ty(*context_), record.fields.size())};
  StateCall(
      builder, RuntimeOp::kClassDeclareMembers, held,
      llvm::Type::getVoidTy(*context_));
  return {};
}

auto CodeGenModule::StateClosure(
    llvm::IRBuilderBase& builder, lir::ClosureId id) -> diag::Result<void> {
  const lir::Closure& closure = unit_->closures.Get(id);
  const std::string symbol = lir::ClosureDefinitionSymbol(
      unit_->name, lir::SymbolPart::Ordinal(id.value));
  auto described = DescribedStorage(
      closure.captures, MemberSlotRole::kSnapshot, "a capture");
  if (!described) {
    return std::unexpected(std::move(described.error()));
  }
  // Which protocol a body answers to follows from what it results in and what
  // it is handed: a coroutine yields the handle its caller drives, a body
  // resulting in nothing runs to completion, and one resulting in a value
  // states which representation that value comes back in -- taking an entry and
  // its position beyond the receiver is what separates the two that do.
  const lir::Function& invoke = unit_->functions.Get(closure.invoke);
  const lir::Type& result = unit_->types.Get(invoke.result_type);
  auto* ptr_ty = types_.Ptr();
  auto* byte_ty = llvm::Type::getInt8Ty(*context_);
  auto* count_ty = llvm::Type::getInt64Ty(*context_);
  std::vector<llvm::Type*> params{ptr_ty, count_ty, ptr_ty};
  std::vector<llvm::Value*> args{
      *described, llvm::ConstantInt::get(count_ty, closure.captures.size()),
      UnitFunction(closure.invoke)};
  RuntimeOp op = RuntimeOp::kClosureDeclareSynchronous;
  if (result.Is<lir::CoroutineType>()) {
    op = RuntimeOp::kClosureDeclareCoroutine;
  } else if (!result.Is<lir::VoidType>()) {
    const std::optional<support::ValueDomain> domain =
        ValueDomainOf(*unit_, invoke.result_type);
    if (!domain) {
      throw InternalError(
          "llvm codegen: a closure body answering a value settles a runtime "
          "value");
    }
    op = invoke.params.size() > 1 ? RuntimeOp::kClosureDeclarePerElement
                                  : RuntimeOp::kClosureDeclareValue;
    params.push_back(byte_ty);
    args.push_back(
        llvm::ConstantInt::get(byte_ty, static_cast<std::uint64_t>(*domain)));
  }
  llvm::FunctionCallee declare = module_->getOrInsertFunction(
      RuntimeSymbol(op), llvm::FunctionType::get(ptr_ty, params, false));
  llvm::GlobalVariable* cell = DeclaredCell(symbol);
  cell->setInitializer(llvm::ConstantPointerNull::get(ptr_ty));
  builder.CreateStore(builder.CreateCall(declare, args), cell);
  return {};
}

auto CodeGenModule::StateSharedStorage(
    llvm::IRBuilderBase& builder, const lir::StaticStorage& storage)
    -> diag::Result<void> {
  const std::optional<support::DeclaredMemberStorage> described =
      DeclaredStorageOf(*unit_, storage.type, MemberSlotRole::kVariable);
  if (!described) {
    return diag::Fail(
        diag::DiagCode::kUnsupportedTypeKind,
        std::format(
            "llvm codegen: a shared cell of type {} has no storage "
            "realization on this backend",
            unit_->types.Get(storage.type).KindName()));
  }
  auto* byte_ty = llvm::Type::getInt8Ty(*context_);
  auto* ptr_ty = types_.Ptr();
  llvm::FunctionCallee declare = module_->getOrInsertFunction(
      RuntimeSymbol(RuntimeOp::kSharedStorageDeclare),
      llvm::FunctionType::get(ptr_ty, {byte_ty, byte_ty}, false));
  llvm::GlobalVariable* cell = DeclaredCell(storage.symbol);
  // The unit that declares the cell is the one that defines the symbol; every
  // other unit reaches it as a declaration and never writes it.
  cell->setInitializer(llvm::ConstantPointerNull::get(ptr_ty));
  builder.CreateStore(
      builder.CreateCall(
          declare,
          {llvm::ConstantInt::get(
               byte_ty, static_cast<std::uint64_t>(described->kind)),
           llvm::ConstantInt::get(
               byte_ty, static_cast<std::uint64_t>(described->domain))}),
      cell);
  return {};
}

auto CodeGenModule::DeclaredCell(const std::string& symbol)
    -> llvm::GlobalVariable* {
  return llvm::cast<llvm::GlobalVariable>(
      module_->getOrInsertGlobal(symbol, types_.Ptr()));
}

auto CodeGenModule::DeclareVariableSchemaCell(lir::FunctionId id)
    -> llvm::GlobalVariable* {
  if (unit_->functions.Get(id).variables.empty()) {
    return nullptr;
  }
  llvm::PointerType* ptr_ty = types_.Ptr();
  // A body several units emit states its own description in each of them, so
  // the cell is this module's rather than one the program shares, and its
  // label reaches no linker: the body's own identity is enough.
  auto* cell = llvm::cast<llvm::GlobalVariable>(module_->getOrInsertGlobal(
      std::format("variable_schema_{}", id.value), ptr_ty));
  cell->setLinkage(llvm::GlobalValue::PrivateLinkage);
  cell->setInitializer(llvm::ConstantPointerNull::get(ptr_ty));
  return cell;
}

auto CodeGenModule::VariableSchemaCell(lir::FunctionId fn)
    -> llvm::GlobalVariable* {
  return variable_schema_cells_.Get(fn);
}

auto CodeGenModule::DefinitionRef(lir::TypeId type)
    -> diag::Result<llvm::GlobalVariable*> {
  const std::optional<std::string> symbol = lir::DefinitionSymbol(*unit_, type);
  if (!symbol.has_value()) {
    return diag::Fail(
        diag::DiagCode::kUnsupportedExpressionForm,
        std::format(
            "llvm codegen: a value of type {} has no definition the runtime "
            "builds values of",
            unit_->types.Get(type).KindName()));
  }
  return DeclaredCell(*symbol);
}

auto CodeGenModule::TypeDescriptorCell(lir::TypeDescriptorId descriptor)
    -> llvm::GlobalVariable* {
  return type_descriptor_cells_.Get(descriptor);
}

auto CodeGenModule::IntegralConstantCell(lir::IntegralConstantId constant)
    -> llvm::GlobalVariable* {
  return integral_constant_cells_.Get(constant);
}

// The module owns its globals, so what the list keeps is the module's cells
// rather than a second owner of them. The label reaches no linker, so a type's
// own identity is enough to tell one cell from another.
auto CodeGenModule::DeclareTypeDescriptorCell(lir::TypeDescriptorId descriptor)
    -> llvm::GlobalVariable* {
  llvm::PointerType* ptr_ty = types_.Ptr();
  auto* cell = llvm::cast<llvm::GlobalVariable>(module_->getOrInsertGlobal(
      std::format("type_descriptor_{}", descriptor.value), ptr_ty));
  cell->setLinkage(llvm::GlobalValue::PrivateLinkage);
  cell->setInitializer(llvm::ConstantPointerNull::get(ptr_ty));
  return cell;
}

auto CodeGenModule::DeclareIntegralConstantCell(
    lir::IntegralConstantId constant) -> llvm::GlobalVariable* {
  llvm::PointerType* ptr_ty = types_.Ptr();
  auto* cell = llvm::cast<llvm::GlobalVariable>(module_->getOrInsertGlobal(
      std::format("constant_{}", constant.value), ptr_ty));
  cell->setLinkage(llvm::GlobalValue::PrivateLinkage);
  cell->setInitializer(llvm::ConstantPointerNull::get(ptr_ty));
  return cell;
}

}  // namespace lyra::backend::llvm_backend
