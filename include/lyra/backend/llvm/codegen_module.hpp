#pragma once

#include <cstdint>
#include <memory>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

#include "lyra/backend/llvm/codegen_types.hpp"
#include "lyra/backend/llvm/codegen_value_type.hpp"
#include "lyra/backend/llvm/emit.hpp"
#include "lyra/backend/llvm/fn_abi.hpp"
#include "lyra/backend/llvm/runtime_entry.hpp"
#include "lyra/base/translation.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/lir/class_id.hpp"
#include "lyra/lir/closure_id.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/enum_table_id.hpp"
#include "lyra/lir/function_id.hpp"
#include "lyra/lir/integral_constant_id.hpp"
#include "lyra/lir/linkage.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/lir/value_step.hpp"
#include "lyra/support/layout.hpp"
#include "lyra/support/member_storage_kind.hpp"
#include "lyra/support/runtime_object.hpp"
#include "lyra/support/value_domain.hpp"
#include "lyra/value/integral.hpp"

namespace llvm {
class Constant;
class Function;
class FunctionCallee;
class FunctionType;
class GlobalVariable;
class IRBuilderBase;
class Value;
}  // namespace llvm

namespace lyra::lir {
struct Class;
struct CompilationUnit;
struct ConstantRecord;
struct DispatchedBehavior;
struct GlobalConstant;
struct Member;
struct StatedDispatchRef;
struct StaticStorage;
}  // namespace lyra::lir

namespace lyra::backend::llvm_backend {

// How a symbol linked as `linkage` says is linked here.
auto LinkageOf(lir::Linkage linkage) -> llvm::GlobalValue::LinkageTypes;

// Where the storage of one declaration's own members sits in a value of it,
// laid out after whatever the declaration extends: what each member holds and
// its offset from the value's start, where the last of them ends -- which is
// where a declaration extending this one places its first -- and what the
// whole value occupies. A value of a class holds its class's definition at its
// start, where the library's root class, which every class extends, places it.
//
// A member holding a product, a fixed-size unpacked array, an integral value
// or a machine integer inline holds its bytes, laid out by this backend as a
// product holds a component that is one, so that member is built and ended as
// its type says rather than as an object of the library is; which type each
// member holds is kept beside its storage for that.
struct PlacedMembers {
  std::vector<lir::TypeId> types;
  std::vector<support::DeclaredMemberStorage> storage;
  std::vector<std::uint64_t> offsets;
  std::uint64_t end = 0;
  support::Layout layout{.size = 0, .align = 1};
};

// The allocation function storage the program asks for comes from (C++ ABI
// mangling of `operator new(std::size_t)`).
inline constexpr std::string_view kOperatorNew = "_Znwm";

// Module-level code generation: owns the context and module, declares every
// callable's signature, drives per-function body generation, and yields the
// verified module. The narrow accessors hand the per-function generation the
// shared internals it needs without exposing the whole module emitter.
class CodeGenModule {
 public:
  explicit CodeGenModule(const lir::CompilationUnit& unit);

  auto Run() -> diag::Result<EmittedModule>;

  auto Context() -> llvm::LLVMContext& {
    return *context_;
  }
  auto Module() -> llvm::Module& {
    return *module_;
  }
  auto Types() -> CodeGenTypes& {
    return types_;
  }
  auto Unit() const -> const lir::CompilationUnit& {
    return *unit_;
  }

  // The LLVM function a unit function was emitted as, reached by the identity a
  // call carries. Identity is the function's own, never a reconstructed symbol
  // name.
  auto UnitFunction(lir::FunctionId function) -> llvm::Function*;

  // Data the library defines and generated code names by address, linked
  // under `symbol`.
  auto LibraryData(std::string_view symbol) -> llvm::Constant*;

  // The definition the library reads of a type whose values it builds -- a
  // class, a struct, or a closure: a constant the unit declaring the type
  // emits, which every unit reaches by the symbol it is linked under. A use
  // forwards its address without inspecting it, and a declaration of this unit
  // and one of another are named the same way.
  auto DefinitionOf(lir::TypeId type) -> diag::Result<llvm::Constant*>;

  // A function something outside this module defines -- an entry the runtime
  // publishes, a body of another artifact, a function of the host -- declared
  // under `name` at `type`, as clang's `CodeGenModule::CreateRuntimeFunction`
  // declares one. A name has one type, so declaring it again at another is
  // refused.
  auto CreateRuntimeFunction(llvm::FunctionType* type, std::string_view name)
      -> llvm::FunctionCallee;
  // The library entry `symbol`, declared at the type a call crossing to it as
  // `abi` arranges has.
  auto RuntimeFunction(std::string_view symbol, const FnAbi& abi)
      -> llvm::FunctionCallee;
  // What a callee a call crosses to as `abi` arranges is handed ahead of the
  // storage its answer is built in, given the operands the call states. Each
  // thing an entry is told of a type is a constant, so building the list
  // emits nothing.
  auto BuildCallArgs(const FnAbi& abi, std::span<llvm::Value* const> operands)
      -> diag::Result<std::vector<llvm::Value*>>;
  // The arguments the operand `value` crosses as, appended to `args`.
  auto BuildCallArg(
      const PassMode& mode, llvm::Value* value, std::vector<llvm::Value*>& args)
      -> diag::Result<void>;
  // One step of the life of a value of `type`, emitted where `builder`
  // stands, as the unit says it is carried out: a copy or a move takes where
  // the value lies and the storage it is built in, an assignment the storage
  // written into and where the value lies, an end where the value lies. A step
  // the library carries out fails where a type it has to be handed is one it
  // does not realize.
  auto EmitStep(
      llvm::IRBuilderBase& builder, lir::TypeId type, lir::ValueStep step,
      std::span<llvm::Value* const> args) -> diag::Result<void>;
  // Defines the constant `value` under `symbol`, linked as `linkage` says and
  // kept at `align`, which every reference made to a declaration of that
  // symbol before then names instead.
  auto DefineConstant(
      const std::string& symbol, llvm::Constant* value,
      llvm::GlobalValue::LinkageTypes linkage, llvm::Align align)
      -> llvm::GlobalVariable*;
  // Every position above an integral's width in the value of `type` lying at
  // `at` made clear, emitted where `builder` stands, as the C++ rendering's
  // clear is once a compiler inlines it.
  void EmitClearAboveWidths(
      llvm::IRBuilderBase& builder, lir::TypeId type, llvm::Value* at);
  // The host's allocation function, handed a size and answering with storage
  // of it, and its sized deallocation function, handed the storage and its
  // size (C++ ABI mangling of `operator delete(void*, std::size_t)`).
  auto OperatorNew() -> llvm::FunctionCallee;
  auto SizedOperatorDelete() -> llvm::FunctionCallee;

  // The library kind a value of `type` is, which names the entries acting on
  // it; a type the library realizes as no kind of its own is one this backend
  // does not carry.
  [[nodiscard]] auto DomainOf(lir::TypeId type) const
      -> diag::Result<support::ValueDomain>;

  // Storage the whole program shares, under the symbol it is linked by. The
  // symbol is the storage itself, so its address is what a reference names.
  auto SharedStorage(const std::string& symbol) -> llvm::GlobalVariable*;

  // One of the unit's enumeration member tables: constant data laid out as
  // the library's record of one, whose address is what a question about a
  // value is asked against. The module defines the ones its code names.
  auto GetAddrOfEnumTable(lir::EnumTableId table) -> llvm::GlobalVariable*;

  // One of the unit's integral constants: constant data holding the bytes a
  // value of its type is laid out in, whose address is where the value lies.
  // The module defines the ones its code names.
  auto GetAddrOfIntegralConstant(lir::IntegralConstantId constant)
      -> llvm::GlobalVariable*;

  // How a value of the declaration `type` names is laid out -- a class of this
  // unit or of another, the part the runtime library builds where a class
  // extends one of its classes, or a closure, its captures after the library's
  // part. Laid out once per declaration: its own members after the whole of
  // what it extends.
  auto PlacedMembersOf(lir::TypeId type) -> diag::Result<const PlacedMembers*>;
  // How large a value of the closure type `closure` is, from its captures as
  // they are laid out; every closure's are laid out before any body is
  // generated.
  [[nodiscard]] auto ClosureLayout(lir::TypeId closure) const
      -> support::Layout;

  // The layout an allocation of a complete object of `type` asks for -- the
  // whole layout of the declaration it names, or what any other value is laid
  // out in -- whose size is what `operator new` is asked for and sized
  // `operator delete` told, and what a type's size or alignment answers.
  auto AllocationLayoutOf(lir::TypeId type) -> diag::Result<support::Layout>;

  // Builds, at `storage`, a variable's cell of the type `cell`, as a body's
  // variable is built where its declaration is reached.
  auto BuildCell(
      llvm::IRBuilderBase& builder, lir::TypeId cell, llvm::Value* storage)
      -> diag::Result<void>;

  // `storage`, holding a complete object of `pointee` no longer, given back to
  // the host's sized deallocation function, as clang's `EmitDeleteCall` does.
  auto EmitDeleteCall(
      llvm::IRBuilderBase& builder, lir::TypeId pointee, llvm::Value* storage)
      -> diag::Result<void>;

  // Members of the types `members`, each in the slot `role` asks for, laid out
  // after `after`, naming each as `what` where it has no storage on this
  // backend.
  auto PlaceMembers(
      std::span<const lir::TypeId> members, MemberSlotRole role,
      const PlacedMembers& after, std::string_view what)
      -> diag::Result<PlacedMembers>;

  // Which of the bodies the table of a value's class holds answers `behavior`
  // (LRM 8.20): those of the introducer's lineage come first, so it lies where
  // it does in the introducer's own table, in every class extending that one.
  auto DispatchSlotOf(const lir::StatedDispatchRef& behavior) -> std::uint64_t;

  // Building the member at `i` that `placed` lays out, in the storage at
  // `value`, and ending it: a body's variable, begun where its declaration is
  // reached and ended where its block is left.
  auto BeginMember(
      llvm::IRBuilderBase& builder, llvm::Value* value,
      const PlacedMembers& placed, std::size_t i) -> diag::Result<void>;
  auto EndMember(
      llvm::IRBuilderBase& builder, llvm::Value* value,
      const PlacedMembers& placed, std::size_t i) -> diag::Result<void>;
  // Begins, and ends, the parts of the value at `value` that the class `cls`
  // places -- its own and those of what it extends: by that class's own base
  // object creation and destructor, or by the library where it is one of the
  // library's classes.
  void BeginParts(
      llvm::IRBuilderBase& builder, lir::TypeId cls, llvm::Value* value);
  void EndParts(
      llvm::IRBuilderBase& builder, lir::TypeId cls, llvm::Value* value);
  // Builds, at `at`, the storage a member of `type` is held as.
  auto BuildMemberStorage(
      llvm::IRBuilderBase& builder, lir::TypeId type,
      support::DeclaredMemberStorage storage, llvm::Value* at)
      -> diag::Result<void>;
  // Begins the storage at `at`, a member of `type` held as `storage`, holding
  // nothing yet.
  auto BeginStorageAt(
      llvm::IRBuilderBase& builder, lir::TypeId type,
      support::DeclaredMemberStorage storage, llvm::Value* at)
      -> diag::Result<void>;
  // Ends what the storage at `at`, a member of `type` held as `storage`,
  // holds, and the storage with it.
  auto EndStorageAt(
      llvm::IRBuilderBase& builder, lir::TypeId type,
      support::DeclaredMemberStorage storage, llvm::Value* at)
      -> diag::Result<void>;

  // The symbol of the allocating creation of `cls`, a class some unit
  // declares.
  [[nodiscard]] auto CreationSymbol(lir::TypeId cls) const -> std::string;

 private:
  // A step the library carries out on a value of `type` it holds as `object`,
  // taking where the values the step reads and writes lie, as every step does.
  auto EmitLibraryStep(
      llvm::IRBuilderBase& builder, lir::TypeId type,
      const support::RuntimeObject& object, lir::ValueStep step,
      std::span<llvm::Value* const> args) -> diag::Result<void>;
  // Ends the object at `object` through its class's definition, which ends it
  // as the class it was created as and gives its storage back.
  void EndObject(llvm::IRBuilderBase& builder, llvm::Value* object);
  // Whether a member of `type` held as `storage` is the bytes this backend lays
  // a value of the type out in -- a product's, a fixed-size unpacked array's,
  // an integral value's, a machine integer's -- rather than an object of the
  // library.
  [[nodiscard]] auto HoldsItsBytesInline(
      lir::TypeId type, support::DeclaredMemberStorage storage) const -> bool;
  // What a member of `type` held as `storage` occupies.
  [[nodiscard]] auto MemberLayout(
      lir::TypeId type, support::DeclaredMemberStorage storage)
      -> support::ObjectLayout;
  // The values a member of `type` holds: its own value, or what the storage
  // it is holds.
  [[nodiscard]] auto ValuesHeldByMember(lir::TypeId type) const
      -> std::optional<lir::TypeId>;
  // How a variable's cell of the type `cell` is held, which is how a body holds
  // one of its variables.
  [[nodiscard]] auto CellStorage(lir::TypeId cell) const
      -> support::DeclaredMemberStorage;
  // The arguments that tell an entry how wide an integral type is, and whether
  // it is four-state.
  auto WidthArg(const value::IntegralShape& shape) -> llvm::Constant*;
  auto FourStateArg(const value::IntegralShape& shape) -> llvm::Constant*;
  // What ends the storage of a member held as `storage`, handed where it is.
  auto MemberStorageDestructor(support::DeclaredMemberStorage storage)
      -> llvm::Function*;
  // What registers a function to run on an object when the program exits (C++
  // ABI 3.3.6.3): handed the function, the object and this module's handle.
  auto AtExit() -> llvm::FunctionCallee;

  auto DeclareCallable(lir::FunctionId id) -> llvm::Function*;
  // The type of a function taking `params` and answering `result`, as this
  // module declares a function of its own and one another module emits.
  auto FunctionTypeOf(std::span<const lir::TypeId> params, lir::TypeId result)
      -> llvm::FunctionType*;
  // The function another module emits under `symbol`, declared at
  // `prototype`, the type that module published it with.
  auto ExternalFunction(const std::string& symbol, lir::TypeId prototype)
      -> llvm::Function*;
  // Emits the body building the storage this unit shares, and asks the target
  // to run it before the program starts. Composing the program is what settles
  // when that is -- a linker collects it, a session runs it on the way up --
  // and the unit states the same thing either way.
  auto EmitSharedStorageConstruction() -> diag::Result<void>;
  // What one storage this unit shares holds; refused where its type has no
  // storage on this backend.
  auto SharedStorageOf(const lir::StaticStorage& storage)
      -> diag::Result<support::DeclaredMemberStorage>;
  // Defines the storage itself under its symbol, which is what every reference
  // to it names.
  auto DefineSharedStorage(const lir::StaticStorage& storage)
      -> diag::Result<void>;
  // Builds that storage before the program starts, and hands the platform what
  // ends it after the program exits.
  auto StateSharedStorage(
      llvm::IRBuilderBase& builder, const lir::StaticStorage& storage)
      -> diag::Result<void>;
  // Defines what the library reads of one closure this unit declares: what its
  // captures need, and the body a call runs in the protocol that body answers
  // to.
  auto EmitClosureDefinition(lir::ClosureId id) -> diag::Result<void>;
  // Defines the data `constant` states, under its symbol and linkage: a
  // structure of the library, or an array of them or of code addresses.
  void DefineData(const lir::GlobalConstant& constant);
  // A structure of the library as data, each member where the library's own
  // declaration of the structure puts it.
  auto RecordData(const lir::ConstantRecord& record) -> llvm::Constant*;
  // What code generation reads of a class a value holds part of, of this unit
  // or another: the symbol its definition is linked under; what it extends,
  // which is nothing for an interface class, of which no value is built, and
  // the part the library builds where it extends no declared class; its own
  // members; and the behaviors an object of it dispatches on.
  struct Declared {
    std::string definition;
    std::optional<lir::TypeId> base;
    std::span<const lir::Member> members;
    std::span<const lir::DispatchedBehavior> dispatch;
  };
  [[nodiscard]] auto DeclarationOf(lir::TypeId type) const -> Declared;
  // Emits the tables an object of the class `cls` of this unit dispatches
  // through: its own, the body of each behavior in the order the class states
  // them, and one for each interface class it implements.
  void EmitTables(const lir::Class& cls, const Declared& declared);
  // The address of what `body`, a code address a class states, names.
  auto EntryAddress(const std::optional<lir::TableBody>& body)
      -> llvm::Constant*;
  // How a value of `declared` is laid out.
  auto LayOut(const Declared& declared) -> diag::Result<PlacedMembers>;
  // How a closure's captures are laid out, after the library's part of it.
  auto LayOutCaptures(lir::ClosureId id) -> diag::Result<PlacedMembers>;
  // The definition linked under `symbol`, or a declaration of it where nothing
  // has defined it yet.
  auto DefinitionGlobal(const std::string& symbol) -> llvm::GlobalVariable*;
  // An integer of `bytes` bytes, as a field of a constant record.
  auto Int(std::uint64_t value, std::size_t bytes) -> llvm::Constant*;
  // The address of `name` as a NUL-terminated constant.
  auto NameConstant(std::string_view name) -> llvm::Constant*;
  // A body a declaration's values are built or ended by, this unit's or
  // another's, linked under `symbol`: it takes the value and answers nothing.
  auto ValueFunction(const std::string& symbol) -> llvm::Function*;
  auto DeclaredClass(lir::ClassId id) const -> Declared;
  auto DefineEnumTable(lir::EnumTableId table) -> llvm::GlobalVariable*;
  auto DefineIntegralConstant(lir::IntegralConstantId constant)
      -> llvm::GlobalVariable*;

  std::unique_ptr<llvm::LLVMContext> context_;
  std::unique_ptr<llvm::Module> module_;
  const lir::CompilationUnit* unit_;
  CodeGenTypes types_;
  CodeGenValueTypes value_types_;
  base::Translation<lir::FunctionId, llvm::Function*> functions_;
  std::unordered_map<lir::EnumTableId, llvm::GlobalVariable*> enum_tables_;
  std::unordered_map<lir::IntegralConstantId, llvm::GlobalVariable*>
      integral_constants_;
  std::unordered_map<lir::TypeId, PlacedMembers> placed_members_;
};

}  // namespace lyra::backend::llvm_backend
