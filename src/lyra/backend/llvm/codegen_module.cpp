#include "lyra/backend/llvm/codegen_module.hpp"

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
#include <format>
#include <iterator>
#include <optional>
#include <ranges>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalAlias.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Transforms/Utils/ModuleUtils.h>

#include "lyra/backend/llvm/codegen_function.hpp"
#include "lyra/backend/llvm/constant_record.hpp"
#include "lyra/backend/llvm/runtime_entry.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/failure_context.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/symbol_name.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/runtime/closure.hpp"
#include "lyra/runtime/object_layout.hpp"
#include "lyra/support/runtime_class.hpp"
#include "lyra/support/runtime_object.hpp"
#include "lyra/support/value_domain.hpp"

namespace lyra::backend::llvm_backend {

CodeGenModule::CodeGenModule(const lir::CompilationUnit& unit)
    : context_(std::make_unique<llvm::LLVMContext>()),
      // A module carries the name of the unit it holds, because the party
      // composing the program derives names of its own from it -- one that
      // collects what runs before the program starts gives its result a name
      // made from this one, so two modules sharing an identifier are two
      // artifacts that cannot be composed together.
      module_(std::make_unique<llvm::Module>(unit.name, *context_)),
      unit_(&unit),
      types_(*context_, unit),
      tuples_(*this, types_, unit),
      functions_(unit.functions.size()) {
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
  for (const lir::StaticStorage& storage : unit_->static_storage) {
    auto defined = DefineSharedStorage(storage);
    if (!defined) {
      return std::unexpected(std::move(defined.error()));
    }
  }
  for (const lir::FunctionId id : unit_->functions.Ids()) {
    const auto in_function =
        diag::FailureContext::InFunction(unit_->functions.Get(id).name);
    auto generated = CodeGenFunction(*this, id).Run();
    if (!generated) {
      return std::unexpected(std::move(generated.error()));
    }
  }
  tuples_.EmitDeclared();
  // Every declaration a value is built of builds and ends its own storage, and
  // a declaration extending it calls those, in this unit or another. Every one
  // extends the library's root, whose destructor is virtual, so each has a
  // table and is described where it is declared. The destructors come first,
  // so the table naming them names bodies already defined, and the prologue
  // storing the table's address comes after it.
  const auto emit_storage =
      [&](lir::TypeId type, const Declared& declared,
          std::span<const lir::ConformingBehavior> conforming)
      -> diag::Result<void> {
    if (auto ended = EmitDestructors(declared); !ended) {
      return ended;
    }
    if (auto table = EmitTable(type, declared, conforming); !table) {
      return table;
    }
    return EmitConstructorPrologue(declared);
  };
  for (const lir::ClassId id : unit_->classes.Ids()) {
    const lir::TypeId type =
        unit_->types.Intern(lir::Type{lir::ObjectType{.class_id = id}});
    // An interface class is described too, since a cast to one and a class
    // implementing it name its description; it extends nothing and no value
    // is built of one, so it has neither a table group nor storage of its own.
    TypeInfoOf(type);
    if (const Declared declared = DeclaredClass(id);
        declared.base.has_value()) {
      auto emitted =
          emit_storage(type, declared, unit_->classes.Get(id).conforming);
      if (!emitted) {
        return std::unexpected(std::move(emitted.error()));
      }
    }
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
  // A body answering a value its caller will own builds that value in storage
  // the caller gives, handed over last, and answers with it -- the shape every
  // runtime entry answering one has.
  if (unit_->types.Get(fn.result_type).IsOwnedValue()) {
    params.push_back(types_.Ptr());
  }
  auto* fn_ty =
      llvm::FunctionType::get(types_.Map(fn.result_type), params, false);
  llvm::Function* declared = llvm::Function::Create(
      fn_ty, LinkageOf(fn.definition), fn.name, module_.get());
  for (const std::string& alias : fn.aliases) {
    llvm::GlobalAlias::create(alias, declared);
  }
  // A body the runtime calls back through a tuple's table answers a
  // predicate as a C++ `bool`, which is read as a whole byte.
  if (unit_->types.Get(fn.result_type).Is<lir::MachineBoolType>()) {
    declared->addRetAttr(llvm::Attribute::ZExt);
  }
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

auto CodeGenModule::StateCall(
    llvm::IRBuilderBase& builder, std::string_view symbol,
    std::span<llvm::Value* const> args, llvm::Type* result) -> llvm::Value* {
  std::vector<llvm::Type*> params;
  params.reserve(args.size());
  for (llvm::Value* arg : args) {
    params.push_back(arg->getType());
  }
  llvm::FunctionCallee entry = module_->getOrInsertFunction(
      symbol, llvm::FunctionType::get(result, params, false));
  const std::vector<llvm::Value*> stated(args.begin(), args.end());
  return builder.CreateCall(entry, stated);
}

auto CodeGenModule::Int(std::uint64_t value, unsigned bytes)
    -> llvm::Constant* {
  return llvm::ConstantInt::get(
      llvm::Type::getIntNTy(*context_, bytes * 8), value);
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
            [&](const lir::ConstantAddress& c) -> llvm::Constant* {
              return DefinitionGlobal(c.symbol);
            },
            [&](const lir::ConstantInt&) { return mismatch("a pointer"); },
            [&](const lir::ConstantRecord&) { return mismatch("a pointer"); },
            [&](const lir::ConstantArray&) { return mismatch("a pointer"); }},
        part.value);
  };
  ConstantRecord out(*context_, layout.size);
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
  llvm::GlobalVariable* data = std::visit(
      Overloaded{
          [&](const lir::ConstantRecord& record) {
            llvm::GlobalVariable* defined =
                DefineConstant(symbol, RecordData(record));
            defined->setAlignment(
                llvm::Align(LayoutOfLibraryRecord(record.kind).alignment));
            return defined;
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
                        align = LayoutOfLibraryRecord(record.kind).alignment;
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
                      [&](const lir::ConstantAddress&) {
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
            llvm::GlobalVariable* defined = DefineConstant(
                symbol, llvm::ConstantArray::get(
                            llvm::ArrayType::get(element_ty, elements.size()),
                            elements));
            defined->setAlignment(llvm::Align(align));
            return defined;
          },
          [&](const lir::ConstantInt&) { return not_whole("an integer"); },
          [&](const lir::ConstantNull&) { return not_whole("a null"); },
          [&](const lir::ConstantString&) { return not_whole("a string"); },
          [&](const lir::ConstantFunction&) {
            return not_whole("a function's address");
          },
          [&](const lir::ConstantAddress&) { return not_whole("an address"); }},
      constant.initializer.value);
  switch (constant.linkage) {
    case lir::Linkage::kInternal:
      data->setLinkage(llvm::GlobalValue::InternalLinkage);
      break;
    case lir::Linkage::kExternal:
      data->setLinkage(llvm::GlobalValue::ExternalLinkage);
      break;
  }
}

auto CodeGenModule::ExtendedClassOf(const Declared& declared) const
    -> std::optional<lir::TypeId> {
  if (declared.base.has_value() &&
      lir::DefinitionSymbol(*unit_, *declared.base).has_value()) {
    return declared.base;
  }
  return std::nullopt;
}

auto CodeGenModule::DeclarationOf(lir::TypeId type) const -> Declared {
  const lir::Type& named = unit_->types.Get(type);
  if (const auto* object = named.As<lir::ObjectType>()) {
    return DeclaredClass(object->class_id);
  }
  if (const auto* cross = named.As<lir::CrossUnitClassType>()) {
    const lir::ExternalClass* published =
        lir::FindExternalClass(*unit_, cross->unit_name, cross->class_name);
    if (published == nullptr) {
      throw InternalError(
          "llvm codegen: a class of another unit is read that this unit "
          "consumed no signature of -- please report this as a bug");
    }
    return Declared{
        .definition = *lir::DefinitionSymbol(*unit_, type),
        .base = published->base,
        .members = published->members,
        .dispatch = &published->dispatch,
        .implements = published->implements};
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
      .dispatch = &cls.dispatch,
      .implements = cls.implements};
}

auto CodeGenModule::InterfacePartsOf(const Declared& declared) const
    -> std::vector<lir::TypeId> {
  std::vector<lir::TypeId> parts;
  if (const std::optional<lir::TypeId> extended = ExtendedClassOf(declared)) {
    parts = InterfacePartsOf(DeclarationOf(*extended));
  }
  for (const lir::TypeId named : declared.implements) {
    AppendInterfacePart(named, parts);
  }
  return parts;
}

void CodeGenModule::AppendInterfacePart(
    lir::TypeId iface, std::vector<lir::TypeId>& parts) const {
  if (std::ranges::find(parts, iface) != parts.end()) {
    return;
  }
  parts.push_back(iface);
  for (const lir::TypeId extended : DeclarationOf(iface).implements) {
    AppendInterfacePart(extended, parts);
  }
}

auto CodeGenModule::LineageStepOf(lir::TypeId type) const -> Declared {
  Declared step = DeclarationOf(type);
  if (!step.base.has_value()) {
    throw InternalError(
        "llvm codegen: an interface class is read as a step of a lineage, "
        "which holds no storage a class extends -- please report this as a "
        "bug");
  }
  return step;
}

auto CodeGenModule::IsInterfaceClass(lir::TypeId type) const -> bool {
  return !DeclarationOf(type).base.has_value();
}

namespace {

auto AlignUp(std::uint64_t offset, std::uint64_t align) -> std::uint64_t {
  return (offset + align - 1) / align * align;
}

// A table's address is a pointer of the host the program runs on, which is the
// host this compiler targets.
constexpr std::uint64_t kTableAddressSize = sizeof(void*);
constexpr std::uint64_t kTableAddressAlign = alignof(void*);

// What an Itanium table holds ahead of its bodies (C++ ABI 2.5.2): the offset
// from the part of the value holding the table's address to the value's start,
// and the description of the class a cast reads. The address a value holds is
// the first body's.
constexpr std::uint64_t kTableHeader = kOffsetToTopEntry;

// The C++ ABI's own descriptions of a class that extends nothing, of one that
// extends exactly one class at its own start, and of one with several bases or
// a base reached along several paths (C++ ABI 2.9.4); the host's C++ runtime
// defines their tables.
constexpr std::string_view kClassTypeInfoTable =
    "_ZTVN10__cxxabiv117__class_type_infoE";
constexpr std::string_view kSingleBaseTypeInfoTable =
    "_ZTVN10__cxxabiv120__si_class_type_infoE";
constexpr std::string_view kMultipleBaseTypeInfoTable =
    "_ZTVN10__cxxabiv121__vmi_class_type_infoE";

// What the ABI's description of a base says about it (C++ ABI 2.9.4
// `__base_class_type_info`): a virtual base, a public one, and how far the
// offset is shifted past those. An interface class is a virtual base (LRM
// 8.26.6.3), and every base of the source is public.
constexpr std::int64_t kVirtualBase = 0x1;
constexpr std::int64_t kPublicBase = 0x2;
constexpr std::int64_t kBaseOffsetShift = 8;

// The description's own flag that a base may be reached along several paths
// (`__diamond_shaped_mask`). It only makes a cast look further, so a class
// with a virtual base states it whether or not two paths meet.
constexpr std::uint32_t kDiamondShaped = 0x2;

// Where a table holds the offset from the part reading it to its first
// interface class part: just ahead of the header (C++ ABI 2.5.2, virtual base
// offsets in reverse order). The one for interface class `k` is `k` further.
constexpr std::uint64_t kFirstVirtualBaseOffset = kTableHeader + 1;

// The entries a virtual destructor takes in a table, the complete object
// destructor and then the deleting one (C++ ABI 2.5.2), at the place it is
// declared. It is the first virtual function of the library's root class, and
// the first an interface class declares, so it opens every table.
constexpr std::uint64_t kDestructorEntries = 2;
constexpr std::uint64_t kCompleteDestructorEntry = 0;
constexpr std::uint64_t kDeletingDestructorEntry = 1;

// The part the library builds where a class extends one of its classes. Its
// table's address is at its start, where every class extending it keeps its
// own.
auto LibraryRecord(support::RuntimeClass which) -> RecordLayout {
  const support::ObjectLayout layout = runtime::LayoutOf(which);
  return RecordLayout{
      .types = {},
      .storage = {},
      .offsets = {},
      .end = layout.size,
      .size = layout.size,
      .align = layout.align};
}

// The deallocation function a deleting destructor gives a value's storage back
// to, the one taking the size it was allocated with (C++ ABI mangling of
// `operator delete(void*, std::size_t)`).
constexpr std::string_view kSizedOperatorDelete = "_ZdlPvm";

// What a table holds where no class of the lineage gives a behavior a body: the
// host's C++ runtime ends the program if it is ever entered.
constexpr std::string_view kNoBody = "__cxa_pure_virtual";

}  // namespace

auto CodeGenModule::PlaceMembers(
    std::span<const lir::TypeId> members, MemberSlotRole role,
    const RecordLayout& after, std::string_view what)
    -> diag::Result<RecordLayout> {
  RecordLayout placed{
      .types = {},
      .storage = {},
      .offsets = {},
      .end = after.end,
      .size = 0,
      .align = after.align};
  placed.types.reserve(members.size());
  placed.storage.reserve(members.size());
  placed.offsets.reserve(members.size());
  for (const lir::TypeId type : members) {
    const std::optional<support::DeclaredMemberStorage> storage =
        DeclaredStorageOf(*unit_, type, role);
    if (!storage) {
      return diag::Fail(
          diag::DiagCode::kUnsupportedTypeKind,
          std::format(
              "llvm codegen: {} of type {} has no storage realization on this "
              "backend",
              what, unit_->types.Get(type).KindName()));
    }
    const support::ObjectLayout layout = HoldsProductInline(type, *storage)
                                             ? types_.StorageOf(type)
                                             : runtime::LayoutOf(*storage);
    const std::uint64_t offset = AlignUp(placed.end, layout.align);
    placed.types.push_back(type);
    placed.storage.push_back(*storage);
    placed.offsets.push_back(offset);
    placed.end = offset + layout.size;
    placed.align = std::max<std::uint64_t>(placed.align, layout.align);
  }
  placed.size = AlignUp(placed.end, placed.align);
  return placed;
}

auto CodeGenModule::LayOut(const Declared& declared)
    -> diag::Result<RecordLayout> {
  if (!declared.base.has_value()) {
    throw InternalError(
        "llvm codegen: an interface class is laid out, which holds no storage "
        "-- please report this as a bug");
  }
  auto extended = RecordOf(*declared.base);
  if (!extended) {
    return std::unexpected(std::move(extended.error()));
  }
  const RecordLayout& base = **extended;
  std::vector<lir::TypeId> types;
  types.reserve(declared.members.size());
  for (const lir::Member& member : declared.members) {
    types.push_back(member.type);
  }
  return PlaceMembers(types, MemberSlotRole::kVariable, base, "a member");
}

auto CodeGenModule::WholeLayOut(const Declared& declared)
    -> diag::Result<WholeRecord> {
  auto record = LayOut(declared);
  if (!record) {
    return std::unexpected(std::move(record.error()));
  }
  // Each interface class the value is also a value of is one part of it
  // however many ways it is arrived at, so only a whole value places them,
  // after everything its lineage places -- where C++ places a virtual base
  // (C++ ABI 2.4). A part holds where that interface class's table is and
  // nothing else.
  WholeRecord whole{
      .size = 0,
      .align = std::max(record->align, kTableAddressAlign),
      .interface_parts = {}};
  std::uint64_t end = record->end;
  const std::size_t part_count = InterfacePartsOf(declared).size();
  whole.interface_parts.reserve(part_count);
  for (std::size_t k = 0; k < part_count; ++k) {
    end = AlignUp(end, kTableAddressAlign);
    whole.interface_parts.push_back(end);
    end += kTableAddressSize;
  }
  whole.size = AlignUp(end, whole.align);
  return whole;
}

auto CodeGenModule::LayOutCaptures(lir::ClosureId id)
    -> diag::Result<RecordLayout> {
  const lir::Closure& closure = unit_->closures.Get(id);
  std::vector<lir::TypeId> types;
  types.reserve(closure.captures.size());
  for (const lir::Member& capture : closure.captures) {
    types.push_back(capture.type);
  }
  return PlaceMembers(
      types, MemberSlotRole::kSnapshot,
      RecordLayout{
          .types = {},
          .storage = {},
          .offsets = {},
          .end = runtime::ClosureCapturesAt(),
          .size = 0,
          .align = 1},
      "a capture");
}

auto CodeGenModule::RecordOf(lir::TypeId type)
    -> diag::Result<const RecordLayout*> {
  if (const auto found = records_.find(type); found != records_.end()) {
    return &found->second;
  }
  const lir::Type& named = unit_->types.Get(type);
  const std::optional<lir::TypeDeclaration> declaration = named.Declaration();
  const auto* library = named.As<lir::RuntimeClassType>();
  if (library == nullptr && !declaration.has_value()) {
    throw InternalError(
        "llvm codegen: a record is asked of a type that declares no storage "
        "-- please report this as a bug");
  }
  auto laid =
      library != nullptr
          ? diag::Result<RecordLayout>{LibraryRecord(library->which)}
          : std::visit(
                Overloaded{
                    [&](const lir::ObjectType&) -> diag::Result<RecordLayout> {
                      return LayOut(DeclarationOf(type));
                    },
                    [&](const lir::CrossUnitClassType&)
                        -> diag::Result<RecordLayout> {
                      return LayOut(DeclarationOf(type));
                    },
                    [&](const lir::ClosureType& closure)
                        -> diag::Result<RecordLayout> {
                      return LayOutCaptures(closure.closure_id);
                    }},
                *declaration);
  if (!laid) {
    return std::unexpected(std::move(laid.error()));
  }
  return &records_.emplace(type, *std::move(laid)).first->second;
}

auto CodeGenModule::CompleteObjectSize(lir::TypeId type)
    -> diag::Result<std::uint64_t> {
  auto whole = WholeLayOut(DeclarationOf(type));
  if (!whole) {
    return std::unexpected(std::move(whole.error()));
  }
  return whole->size;
}

auto CodeGenModule::HoldsProductInline(
    lir::TypeId type, support::DeclaredMemberStorage storage) const -> bool {
  return storage.kind == support::MemberStorageKind::kInlineValue &&
         unit_->types.Get(type).IsProduct();
}

// A product held inline has no value until one is copied in, which is how the
// one record holding such members -- a closure's captures -- is filled, so
// nothing builds it here.
void CodeGenModule::BeginMembers(
    llvm::IRBuilderBase& builder, llvm::Value* value,
    const RecordLayout& placed) {
  for (std::size_t i = 0; i < placed.storage.size(); ++i) {
    if (HoldsProductInline(placed.types[i], placed.storage[i])) {
      throw InternalError(
          "llvm codegen: a product held inline is built by copying a value in, "
          "never default-built -- please report this as a bug");
    }
    const std::array<llvm::Value*, 1> args{builder.CreateConstInBoundsGEP1_64(
        builder.getInt8Ty(), value, placed.offsets[i])};
    StateCall(
        builder, RuntimeSymbol(placed.storage[i], RuntimeOp::kConstruct), args,
        types_.Void());
  }
}

void CodeGenModule::EndMembers(
    llvm::IRBuilderBase& builder, llvm::Value* value,
    const RecordLayout& placed) {
  for (std::size_t i = placed.storage.size(); i-- > 0;) {
    const bool inline_product =
        HoldsProductInline(placed.types[i], placed.storage[i]);
    const support::ObjectLayout layout =
        inline_product ? types_.StorageOf(placed.types[i])
                       : runtime::LayoutOf(placed.storage[i]);
    if (layout.ends_with_nothing_to_do) {
      continue;
    }
    const std::array<llvm::Value*, 1> args{builder.CreateConstInBoundsGEP1_64(
        builder.getInt8Ty(), value, placed.offsets[i])};
    if (inline_product) {
      builder.CreateCall(
          tuples_.Function(placed.types[i], TupleLifecycle::kDestroy), args);
      continue;
    }
    StateCall(
        builder, RuntimeSymbol(placed.storage[i], RuntimeOp::kDestroy), args,
        types_.Void());
  }
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

auto CodeGenModule::EmitConstructorPrologue(const Declared& declared)
    -> diag::Result<void> {
  auto record = LayOut(declared);
  if (!record) {
    return std::unexpected(std::move(record.error()));
  }
  auto whole = WholeLayOut(declared);
  if (!whole) {
    return std::unexpected(std::move(whole.error()));
  }
  const TablePoints& points = table_points_.at(declared.definition);
  llvm::Function* fn =
      ValueFunction(lir::ConstructorPrologueSymbol(declared.definition));
  llvm::IRBuilder<> builder(llvm::BasicBlock::Create(*context_, "", fn));
  llvm::Value* value = fn->getArg(0);
  const auto table_at = [&](std::uint64_t offset, std::uint64_t point) {
    builder.CreateStore(
        TableAddressPoint(declared.definition, point),
        builder.CreateConstInBoundsGEP1_64(builder.getInt8Ty(), value, offset));
  };
  table_at(0, points.lineage);
  BeginMembers(builder, value, *record);
  // Each interface class part is placed where a complete object of this
  // declaration places it. Where the value is of a class extending this one,
  // that is storage the extending class's own members later take, and its
  // prologue then places the parts again where it does; until it does, a view
  // to an interface class is formed through this declaration's table and
  // dispatches as this declaration, which is what C++ gives a base being
  // constructed (C++ ABI 2.6, construction virtual tables).
  for (std::size_t k = 0; k < whole->interface_parts.size(); ++k) {
    table_at(whole->interface_parts[k], points.interfaces[k]);
  }
  builder.CreateRetVoid();
  return {};
}

auto CodeGenModule::EmitDestructors(const Declared& declared)
    -> diag::Result<void> {
  auto record = LayOut(declared);
  if (!record) {
    return std::unexpected(std::move(record.error()));
  }
  auto whole = WholeLayOut(declared);
  if (!whole) {
    return std::unexpected(std::move(whole.error()));
  }
  const auto define = [&](lir::Destructor which, const auto& write) {
    llvm::Function* fn =
        ValueFunction(lir::DestructorSymbol(declared.definition, which));
    llvm::IRBuilder<> builder(llvm::BasicBlock::Create(*context_, "", fn));
    write(builder, fn->getArg(0));
    builder.CreateRetVoid();
  };
  define(
      lir::Destructor::kBaseObject,
      [&](llvm::IRBuilderBase& builder, llvm::Value* value) {
        EndMembers(builder, value, *record);
        const std::optional<lir::TypeId> extended = ExtendedClassOf(declared);
        builder.CreateCall(
            ValueFunction(
                extended.has_value()
                    ? lir::DestructorSymbol(
                          *lir::DefinitionSymbol(*unit_, *extended),
                          lir::Destructor::kBaseObject)
                    : std::string(
                          runtime::BaseObjectDestructorSymbolOf(
                              unit_->types.Get(*declared.base)
                                  .Get<lir::RuntimeClassType>()
                                  .which))),
            {value});
      });
  define(
      lir::Destructor::kCompleteObject,
      [&](llvm::IRBuilderBase& builder, llvm::Value* value) {
        builder.CreateCall(
            ValueFunction(
                lir::DestructorSymbol(
                    declared.definition, lir::Destructor::kBaseObject)),
            {value});
      });
  define(
      lir::Destructor::kDeleting,
      [&](llvm::IRBuilderBase& builder, llvm::Value* value) {
        builder.CreateCall(
            ValueFunction(
                lir::DestructorSymbol(
                    declared.definition, lir::Destructor::kCompleteObject)),
            {value});
        const std::array<llvm::Value*, 2> args{
            value, builder.getInt64(whole->size)};
        StateCall(builder, kSizedOperatorDelete, args, types_.Void());
      });
  return {};
}

auto CodeGenModule::TableOf(lir::TypeId type)
    -> const std::vector<std::optional<std::string>>& {
  if (const auto found = tables_.find(type); found != tables_.end()) {
    return found->second;
  }
  const auto* library = unit_->types.Get(type).As<lir::RuntimeClassType>();
  std::vector<std::optional<std::string>> table =
      library != nullptr ? LibraryTableOf(library->which)
                         : TableOfStep(LineageStepOf(type));
  return tables_.emplace(type, std::move(table)).first->second;
}

auto CodeGenModule::LibraryTableOf(support::RuntimeClass which)
    -> std::vector<std::optional<std::string>> {
  // The root declares its destructor and nothing else virtual, and every
  // declaration extending it gives that destructor a body of its own.
  std::vector<std::optional<std::string>> table(kDestructorEntries);
  if (const std::optional<support::RuntimeClass> base =
          support::LibraryBaseOf(which)) {
    table = TableOf(
        unit_->types.Intern(lir::Type{lir::RuntimeClassType{.which = *base}}));
  }
  for (const support::LibraryVirtual function :
       support::VirtualsDeclaredBy(which)) {
    table.emplace_back(runtime::VirtualFunctionSymbolOf(function));
  }
  return table;
}

auto CodeGenModule::TableOfStep(const Declared& step)
    -> std::vector<std::optional<std::string>> {
  // The base's table, each body overriding one of its behaviors put where the
  // behavior sits, then what this class introduces -- the order C++ lays a
  // primary table out in (C++ ABI 2.5.2). The destructor is overridden by every
  // declaration, a generated record too.
  std::vector<std::optional<std::string>> table = TableOf(*step.base);
  table[kCompleteDestructorEntry] =
      lir::DestructorSymbol(step.definition, lir::Destructor::kCompleteObject);
  table[kDeletingDestructorEntry] =
      lir::DestructorSymbol(step.definition, lir::Destructor::kDeleting);
  if (step.dispatch == nullptr) {
    return table;
  }
  for (const lir::Override& overriding : step.dispatch->overrides) {
    const std::uint64_t slot = LineageIndexOf(overriding.behavior);
    if (slot >= table.size()) {
      throw InternalError(
          "llvm codegen: a class overrides a behavior its lineage does not "
          "carry -- please report this as a bug");
    }
    table[slot] = overriding.body;
  }
  table.insert(
      table.end(), step.dispatch->introduces.begin(),
      step.dispatch->introduces.end());
  return table;
}

auto CodeGenModule::DispatchSlotOf(const lir::StatedDispatchRef& behavior)
    -> diag::Result<DispatchSlot> {
  // A behavior an interface class introduced is reached through that class's
  // part, whose table holds its destructor and then that class's behaviors.
  if (IsInterfaceClass(behavior.introduced_by)) {
    return DispatchSlot{
        InterfaceSlot{.slot = kDestructorEntries + behavior.ordinal.value}};
  }
  return DispatchSlot{LineageSlot{.slot = LineageIndexOf(behavior)}};
}

auto CodeGenModule::LineageIndexOf(const lir::StatedDispatchRef& behavior)
    -> std::uint64_t {
  // What the introducer's own base carries comes first, so the introducer's
  // behaviors follow it in the order it introduces them, and a class of the
  // library's ones close its table the same way.
  if (const auto* library = unit_->types.Get(behavior.introduced_by)
                                .As<lir::RuntimeClassType>()) {
    return TableOf(behavior.introduced_by).size() -
           support::VirtualsDeclaredBy(library->which).size() +
           behavior.ordinal.value;
  }
  return TableOf(*LineageStepOf(behavior.introduced_by).base).size() +
         behavior.ordinal.value;
}

auto CodeGenModule::InterfacePartOffset(
    lir::TypeId view_class, lir::TypeId interface) -> std::uint64_t {
  const std::vector<lir::TypeId> parts =
      InterfacePartsOf(DeclarationOf(view_class));
  const auto found = std::ranges::find(parts, interface);
  if (found == parts.end()) {
    throw InternalError(
        "llvm codegen: a handle is converted to an interface class a value of "
        "its class holds no part for -- please report this as a bug");
  }
  return kFirstVirtualBaseOffset +
         static_cast<std::uint64_t>(std::distance(parts.begin(), found));
}

auto CodeGenModule::TableAddressPoint(
    std::string_view definition, std::uint64_t point) -> llvm::Constant* {
  auto* ptr_ty = types_.Ptr();
  return llvm::ConstantExpr::getInBoundsGetElementPtr(
      ptr_ty, module_->getNamedGlobal(lir::DispatchTableSymbol(definition)),
      llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context_), point));
}

auto CodeGenModule::DefineConstant(
    const std::string& symbol, llvm::Constant* value) -> llvm::GlobalVariable* {
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
  if (declared != nullptr) {
    declared->replaceAllUsesWith(defined);
    declared->eraseFromParent();
  }
  return defined;
}

auto CodeGenModule::TypeInfoOf(lir::TypeId type) -> llvm::Constant* {
  const std::string symbol =
      lir::TypeInfoSymbol(*lir::DefinitionSymbol(*unit_, type));
  if (llvm::GlobalVariable* named = module_->getNamedGlobal(symbol)) {
    return named;
  }
  // A class of another unit is described where it is declared.
  const lir::Type& described = unit_->types.Get(type);
  if (!described.Is<lir::ObjectType>() && !described.Is<lir::StructType>()) {
    return module_->getOrInsertGlobal(symbol, types_.Ptr());
  }
  auto* ptr_ty = types_.Ptr();
  auto* i32 = llvm::Type::getInt32Ty(*context_);
  auto* i64 = llvm::Type::getInt64Ty(*context_);
  const auto address_point = [&](std::string_view table) -> llvm::Constant* {
    return llvm::ConstantExpr::getInBoundsGetElementPtr(
        ptr_ty, module_->getOrInsertGlobal(table, ptr_ty),
        llvm::ConstantInt::get(i64, kTableHeader));
  };
  // What a cast reads of the class (C++ ABI 2.9.4): the table of the ABI's own
  // description, the class's name, and its bases. The name is the definition's
  // symbol, which no other class links under. The class it extends sits at its
  // own start -- a class of the library included, whose description the
  // library defines; each interface class it names is a virtual base, found
  // through the offset its table holds for it.
  const Declared declared = DeclarationOf(type);
  llvm::Constant* name = NameConstant(declared.definition);
  std::optional<llvm::Constant*> extended;
  if (const std::optional<lir::TypeId> declared_base =
          ExtendedClassOf(declared)) {
    extended = TypeInfoOf(*declared_base);
  } else if (declared.base.has_value()) {
    extended = module_->getOrInsertGlobal(
        runtime::TypeInfoSymbolOf(unit_->types.Get(*declared.base)
                                      .Get<lir::RuntimeClassType>()
                                      .which),
        ptr_ty);
  }
  if (declared.implements.empty()) {
    if (!extended.has_value()) {
      return DefineConstant(
          symbol, llvm::ConstantStruct::getAnon(
                      {address_point(kClassTypeInfoTable), name}));
    }
    return DefineConstant(
        symbol,
        llvm::ConstantStruct::getAnon(
            {address_point(kSingleBaseTypeInfoTable), name, *extended}));
  }
  std::vector<llvm::Constant*> bases;
  const auto base_entry = [&](llvm::Constant* base, std::int64_t offset_flags) {
    bases.push_back(
        llvm::ConstantStruct::getAnon(
            {base, llvm::ConstantInt::getSigned(i64, offset_flags)}));
  };
  if (extended.has_value()) {
    base_entry(*extended, kPublicBase);
  }
  for (const lir::TypeId iface : declared.implements) {
    const auto at = static_cast<std::int64_t>(
        InterfacePartOffset(type, iface) * kTableAddressSize);
    base_entry(
        TypeInfoOf(iface), (-at * (std::int64_t{1} << kBaseOffsetShift)) |
                               kVirtualBase | kPublicBase);
  }
  auto* base_ty = llvm::StructType::get(ptr_ty, i64);
  return DefineConstant(
      symbol, llvm::ConstantStruct::getAnon(
                  {address_point(kMultipleBaseTypeInfoTable), name,
                   llvm::ConstantInt::get(i32, kDiamondShaped),
                   llvm::ConstantInt::get(i32, bases.size()),
                   llvm::ConstantArray::get(
                       llvm::ArrayType::get(base_ty, bases.size()), bases)}));
}

auto CodeGenModule::EmitTable(
    lir::TypeId type, const Declared& declared,
    std::span<const lir::ConformingBehavior> conforming) -> diag::Result<void> {
  auto whole = WholeLayOut(declared);
  if (!whole) {
    return std::unexpected(std::move(whole.error()));
  }
  const std::vector<std::optional<std::string>> lineage = TableOfStep(declared);
  auto* ptr_ty = types_.Ptr();
  auto* i64 = llvm::Type::getInt64Ty(*context_);
  llvm::Constant* type_info = TypeInfoOf(type);
  const auto body =
      [&](const std::optional<std::string>& symbol) -> llvm::Constant* {
    const std::string_view named = symbol.has_value() ? *symbol : kNoBody;
    if (llvm::Function* defined = module_->getFunction(named)) {
      return defined;
    }
    return llvm::cast<llvm::Constant>(
        module_
            ->getOrInsertFunction(
                named, llvm::FunctionType::get(types_.Void(), false))
            .getCallee());
  };
  const auto offset = [&](std::int64_t bytes) {
    return llvm::ConstantExpr::getIntToPtr(
        llvm::ConstantInt::getSigned(i64, bytes), ptr_ty);
  };
  const std::vector<lir::TypeId> parts = InterfacePartsOf(declared);
  const auto part_of = [&](lir::TypeId iface) {
    const auto found = std::ranges::find(parts, iface);
    if (found == parts.end()) {
      throw InternalError(
          "llvm codegen: an interface class extends one the value holds no "
          "part for -- please report this as a bug");
    }
    return static_cast<std::int64_t>(
        whole->interface_parts[static_cast<std::size_t>(
            std::distance(parts.begin(), found))]);
  };
  // One group, C++ ABI 2.5.2's order: the table a value of the class holds at
  // its start, then one per interface class part. Each opens with the offsets
  // from the part holding it to every interface class part a handle of that
  // part's class converts to, last first, then the offset back to the value's
  // start and the class's description, then its bodies: the destructor first,
  // as the root and every interface class declare it first.
  std::vector<llvm::Constant*> entries;
  TablePoints points{.lineage = 0, .interfaces = {}};
  for (std::size_t k = parts.size(); k-- > 0;) {
    entries.push_back(
        offset(static_cast<std::int64_t>(whole->interface_parts[k])));
  }
  entries.push_back(offset(0));
  entries.push_back(type_info);
  points.lineage = entries.size();
  for (const std::optional<std::string>& symbol : lineage) {
    entries.push_back(body(symbol));
  }
  for (const lir::TypeId iface : parts) {
    const std::int64_t part = part_of(iface);
    const Declared reached = DeclarationOf(iface);
    const std::vector<lir::TypeId> extended = InterfacePartsOf(reached);
    for (std::size_t j = extended.size(); j-- > 0;) {
      entries.push_back(offset(part_of(extended[j]) - part));
    }
    entries.push_back(offset(-part));
    entries.push_back(type_info);
    points.interfaces.push_back(entries.size());
    entries.push_back(body(lineage[kCompleteDestructorEntry]));
    entries.push_back(body(lineage[kDeletingDestructorEntry]));
    // Each behavior the interface class introduces runs the body the value's
    // class answers it with: the one the table above holds for the behavior
    // of the lineage the class states answers it.
    for (const std::size_t ordinal : std::views::iota(
             std::size_t{0}, reached.dispatch->introduces.size())) {
      const auto answer = std::ranges::find_if(
          conforming, [&](const lir::ConformingBehavior& c) {
            return c.interface_behavior.introduced_by == iface &&
                   c.interface_behavior.ordinal.value == ordinal;
          });
      if (answer == conforming.end()) {
        throw InternalError(
            "llvm codegen: a class states nothing about a behavior of an "
            "interface class its values hold a part for -- please report this "
            "as a bug");
      }
      std::optional<std::string> answering;
      if (answer->answered_by.has_value()) {
        const std::uint64_t at = LineageIndexOf(*answer->answered_by);
        if (at >= lineage.size()) {
          throw InternalError(
              "llvm codegen: an interface class's behavior is answered by one "
              "outside the class's lineage -- please report this as a bug");
        }
        answering = lineage[at];
      }
      entries.push_back(body(answering));
    }
  }
  DefineConstant(
      lir::DispatchTableSymbol(declared.definition),
      llvm::ConstantArray::get(
          llvm::ArrayType::get(ptr_ty, entries.size()), entries));
  table_points_.emplace(declared.definition, std::move(points));
  return {};
}

auto CodeGenModule::EmitClosureDefinition(lir::ClosureId id)
    -> diag::Result<void> {
  using runtime::ClosureDefinition;
  const lir::Closure& closure = unit_->closures.Get(id);
  const std::string symbol = lir::DefinitionSymbol(*unit_, id);
  auto captures = LayOutCaptures(id);
  if (!captures) {
    return std::unexpected(std::move(captures.error()));
  }
  llvm::Function* end = ValueFunction(
      lir::DestructorSymbol(symbol, lir::Destructor::kCompleteObject));
  {
    llvm::IRBuilder<> ending(llvm::BasicBlock::Create(*context_, "", end));
    EndMembers(ending, end->getArg(0), *captures);
    ending.CreateRetVoid();
  }

  // Which protocol a body answers to follows from what it results in and what
  // it is handed: a coroutine yields the handle its caller drives, a body
  // resulting in nothing runs to completion, and one resulting in a value
  // states which representation that value comes back in, and for a tuple
  // which tuple -- taking an entry and its position beyond the receiver is
  // what separates the two that do.
  const lir::Function& invoke = unit_->functions.Get(closure.invoke);
  const lir::Type& result = unit_->types.Get(invoke.result_type);
  std::size_t entry = offsetof(ClosureDefinition, run);
  support::ValueDomain domain{};
  llvm::Constant* tuple = llvm::ConstantPointerNull::get(types_.Ptr());
  if (result.Is<lir::CoroutineType>()) {
    entry = offsetof(ClosureDefinition, start);
  } else if (!result.Is<lir::VoidType>()) {
    const std::optional<support::ValueDomain> settled =
        ValueDomainOf(*unit_, invoke.result_type);
    if (!settled) {
      throw InternalError(
          "llvm codegen: a closure body answering a value settles a runtime "
          "value");
    }
    domain = *settled;
    entry = invoke.params.size() > 1
                ? offsetof(ClosureDefinition, run_per_element)
                : offsetof(ClosureDefinition, run_value);
    if (result.IsProduct()) {
      tuple = tuples_.Operations(invoke.result_type);
    }
  }
  ConstantRecord out(*context_, sizeof(ClosureDefinition));
  // Each body entry the protocol does not use stays null.
  for (const std::size_t at :
       {offsetof(ClosureDefinition, run), offsetof(ClosureDefinition, start),
        offsetof(ClosureDefinition, run_per_element),
        offsetof(ClosureDefinition, run_value)}) {
    out.Place(
        at, at == entry
                ? llvm::cast<llvm::Constant>(UnitFunction(closure.invoke))
                : llvm::ConstantPointerNull::get(types_.Ptr()));
  }
  out.Place(
      offsetof(ClosureDefinition, result_domain),
      Int(static_cast<std::uint64_t>(domain),
          sizeof(ClosureDefinition::result_domain)));
  out.Place(offsetof(ClosureDefinition, result_tuple), tuple);
  out.Place(
      offsetof(ClosureDefinition, size),
      Int(captures->size, sizeof(ClosureDefinition::size)));
  out.Place(offsetof(ClosureDefinition, end_captures), end);
  DefineConstant(symbol, std::move(out).Build())
      ->setAlignment(llvm::Align(alignof(ClosureDefinition)));
  return {};
}

auto CodeGenModule::SharedStorageOf(const lir::StaticStorage& storage)
    -> diag::Result<support::DeclaredMemberStorage> {
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
  const support::ObjectLayout layout = runtime::LayoutOf(*described);
  auto* type =
      llvm::ArrayType::get(llvm::Type::getInt8Ty(*context_), layout.size);
  auto* defined = llvm::cast<llvm::GlobalVariable>(
      module_->getOrInsertGlobal(storage.symbol, type));
  defined->setInitializer(llvm::ConstantAggregateZero::get(type));
  defined->setAlignment(llvm::Align(layout.align));
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
  const std::array<llvm::Value*, 1> built{shared};
  StateCall(
      builder, RuntimeSymbol(*described, RuntimeOp::kConstruct), built,
      types_.Void());
  if (runtime::LayoutOf(*described).ends_with_nothing_to_do) {
    return {};
  }
  auto* ptr_ty = types_.Ptr();
  llvm::FunctionCallee end = module_->getOrInsertFunction(
      RuntimeSymbol(*described, RuntimeOp::kDestroy),
      llvm::FunctionType::get(types_.Void(), {ptr_ty}, false));
  const std::array<llvm::Value*, 3> at_exit{
      end.getCallee(), shared,
      module_->getOrInsertGlobal(
          "__dso_handle", llvm::Type::getInt8Ty(*context_))};
  StateCall(
      builder, "__cxa_atexit", at_exit, llvm::Type::getInt32Ty(*context_));
  return {};
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
        std::format(
            "llvm codegen: a value of type {} has no definition the runtime "
            "builds values of",
            unit_->types.Get(type).KindName()));
  }
  return DefinitionGlobal(*symbol);
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
