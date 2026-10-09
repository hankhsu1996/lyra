#include "lyra/backend/llvm/codegen_tuple.hpp"

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
#include "lyra/value/runtime_tuple.hpp"

namespace lyra::backend::llvm_backend {

namespace {

using support::ValueDomain;

// A tuple's type is read by the runtime through the one C++ class both sides
// compile against: the address of its table, then its size, its alignment and
// how many components it has, then where they are listed, at that address's
// alignment -- laid out as the C++ ABI lays that class out (C++ ABI 2.4). Its
// size is what says no field was added on one side only.
static_assert(
    sizeof(value::TupleType) ==
    sizeof(void*) + (4 * sizeof(std::uint32_t)) + sizeof(void*));
static_assert(
    sizeof(value::TupleComponent) ==
    sizeof(std::uint32_t) + sizeof(std::uint32_t) + sizeof(void*));
static_assert(offsetof(value::TupleComponent, type) == sizeof(void*));

// The operations a container's algorithms ask of an element or of what a
// `with` clause answers -- an order, a truth, a reduction (LRM 7.8, 7.12, 12.4)
// -- and what a walk asks of a value whose parts are ordered by position, none
// of which is asked of a structure: it has no relational or arithmetic
// operator to order, test or fold by, an associative array indexed by one is
// refused where it is declared, and its parts are components named by position
// in its type rather than a sequence of one type.
struct NoStructureAnswers {};

// Where the virtual function `member` lies in its class's table, counted in
// entries from the address point. A pointer to a virtual member function holds
// one plus that entry's byte offset (C++ ABI 2.3), so the class declaration is
// the one statement of the order and nothing here repeats it.
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

// What the virtual function a member pointer names returns.
template <typename Member>
struct ReturnOf;
template <typename R, typename... Params>
struct ReturnOf<R (value::ValueType::*)(Params...) const> {
  using Type = R;
};
template <typename R, typename... Params>
struct ReturnOf<R (value::ValueType::*)(Params...) const noexcept> {
  using Type = R;
};

// One body of a tuple type's table: the storage's own lifecycle, the operations
// the language defines on the whole value (LRM 11.4.5, 20.6.2, 20.9, 6.24.3,
// 6.6.1, 28.12.1), which the type's declaration states as its methods, or one
// no structure answers. A body whose function answers whether something holds
// returns that answer; every other one answers in storage it was handed.
struct TableSlot {
  std::string_view name;
  std::size_t entry;
  bool answers_truth;
  std::variant<TupleLifecycle, support::ValueOperation, NoStructureAnswers>
      filled_by;
};

template <typename Member>
auto Slot(
    std::string_view name, Member member,
    std::variant<TupleLifecycle, support::ValueOperation, NoStructureAnswers>
        filled_by) -> TableSlot {
  return {
      .name = name,
      .entry = EntryOf(member),
      .answers_truth = std::is_same_v<typename ReturnOf<Member>::Type, bool>,
      .filled_by = filled_by};
}

auto TableSlots() -> std::vector<TableSlot> {
  using support::BuiltinFn;
  using value::ValueType;
  return {
      Slot("copy", &ValueType::Copy, TupleLifecycle::kCopy),
      Slot("move", &ValueType::Move, TupleLifecycle::kMove),
      Slot("destroy", &ValueType::Destroy, TupleLifecycle::kDestroy),
      Slot("assign", &ValueType::Assign, TupleLifecycle::kAssign),
      Slot("equal", &ValueType::Equal, support::ValueOperator::kEquality),
      Slot("case_equal", &ValueType::CaseEqual, BuiltinFn::kCaseEqual),
      Slot("bit_identical", &ValueType::BitIdentical, BuiltinFn::kBitIdentical),
      Slot("has_unknown", &ValueType::HasUnknown, BuiltinFn::kHasUnknown),
      Slot(
          "bitstream_width", &ValueType::BitstreamWidth,
          BuiltinFn::kBitstreamWidth),
      Slot("count_bits", &ValueType::CountBits, BuiltinFn::kCountBits),
      Slot("to_bitstream", &ValueType::ToBitstream, BuiltinFn::kToBitstream),
      Slot(
          "from_bitstream", &ValueType::FromBitstream,
          BuiltinFn::kFromBitstream),
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
      Slot("order_before", &ValueType::OrderBefore, NoStructureAnswers{}),
      Slot("is_true", &ValueType::IsTrue, NoStructureAnswers{}),
      Slot("reduce", &ValueType::Reduce, NoStructureAnswers{}),
      Slot("part_count", &ValueType::PartCount, NoStructureAnswers{}),
      Slot("part_type", &ValueType::PartType, NoStructureAnswers{}),
      Slot("part_at", &ValueType::PartAt, NoStructureAnswers{}),
      Slot("part_ref_at", &ValueType::PartRefAt, NoStructureAnswers{}),
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

auto CodeGenTuples::Body(
    lir::TypeId tuple, std::string_view slot, llvm::Function* filling,
    bool answers_truth) -> llvm::Constant* {
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
  std::vector<llvm::Type*> params{types_->Ptr()};
  const llvm::ArrayRef<llvm::Type*> handed =
      filling->getFunctionType()->params();
  params.insert(params.end(), handed.begin(), handed.end());
  llvm::Type* answer =
      answers_truth ? filling->getReturnType() : types_->Void();
  llvm::Function* body = llvm::Function::Create(
      llvm::FunctionType::get(answer, params, false),
      llvm::GlobalValue::InternalLinkage,
      std::format("lyra.tuple{}.type.{}", KeyOf(tuple), slot), module);
  llvm::IRBuilder<> b(llvm::BasicBlock::Create(module.getContext(), "", body));
  std::vector<llvm::Value*> args;
  args.reserve(handed.size());
  for (std::size_t i = 1; i < body->arg_size(); ++i) {
    args.push_back(body->getArg(static_cast<unsigned>(i)));
  }
  llvm::Value* answered = b.CreateCall(filling, args);
  if (answers_truth) {
    b.CreateRet(answered);
  } else {
    b.CreateRetVoid();
  }
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
    entries[kAddressPoint + i] = Body(tuple, "destructor", nullptr, false);
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
    llvm::Function* filling = std::visit(
        Overloaded{
            [&](TupleLifecycle step) -> llvm::Function* {
              return Function(tuple, step);
            },
            [&](const support::ValueOperation& operation) -> llvm::Function* {
              const std::optional<lir::FunctionId> method =
                  MethodAnswering(methods, operation);
              return method.has_value() ? owner_->UnitFunction(*method)
                                        : nullptr;
            },
            [](NoStructureAnswers) -> llvm::Function* { return nullptr; }},
        slot.filled_by);
    entries[at] = Body(tuple, slot.name, filling, slot.answers_truth);
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
