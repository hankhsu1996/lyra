#pragma once

#include <cstdint>
#include <memory>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <unordered_map>
#include <variant>
#include <vector>

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

#include "lyra/backend/llvm/codegen_tuple.hpp"
#include "lyra/backend/llvm/codegen_types.hpp"
#include "lyra/backend/llvm/emit.hpp"
#include "lyra/backend/llvm/fn_abi.hpp"
#include "lyra/backend/llvm/runtime_entry.hpp"
#include "lyra/base/translation.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/lir/class_id.hpp"
#include "lyra/lir/closure_id.hpp"
#include "lyra/lir/enum_table_id.hpp"
#include "lyra/lir/function_id.hpp"
#include "lyra/lir/integral_constant_id.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/support/member_storage_kind.hpp"
#include "lyra/support/runtime_class.hpp"
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
struct ClassDispatch;
struct CompilationUnit;
struct ConformingBehavior;
struct ConstantRecord;
struct GlobalConstant;
struct Member;
struct StatedDispatchRef;
struct StaticStorage;
}  // namespace lyra::lir

namespace lyra::backend::llvm_backend {

// Where the storage of one declaration's own members sits in a value of it,
// laid out after whatever the declaration extends: what each member holds and
// its offset from the value's start, where the last of them ends -- which is
// where a declaration extending this one places its first -- and what the
// whole value occupies. A value of a class holds its table's address at its
// start, where the library's root class, which every class extends and which
// has a virtual destructor, places it (C++ ABI 2.4, the primary base).
//
// A member holding a product, an integral value or a machine integer inline
// holds its bytes, laid out by this backend as a tuple holds a component that
// is one, so that member is built and ended by what is compiled for its type
// rather than by the library; which type each member holds is kept beside its
// storage for that.
struct RecordLayout {
  std::vector<lir::TypeId> types;
  std::vector<support::DeclaredMemberStorage> storage;
  std::vector<std::uint64_t> offsets;
  std::uint64_t end = 0;
  std::uint64_t size = 0;
  std::uint64_t align = 1;
};

// What a whole value of a class occupies, and where each of its interface class
// parts sits, in the order those parts are placed. A class's own record is laid
// out without them, since a class extending it places them again after its own
// members.
struct WholeRecord {
  std::uint64_t size = 0;
  std::uint64_t align = 1;
  std::vector<std::uint64_t> interface_parts;
};

// A dispatch on a behavior of a class's lineage: which of the bodies of the
// table the value's start holds answers it.
struct LineageSlot {
  std::uint64_t slot = 0;
};

// A dispatch on a behavior an interface class introduced, made on that class's
// part: the part holds its table's address, `slot` is which of that table's
// bodies answers it, and a body is entered with the value's start, which is
// the part moved by the offset its table holds back to it (C++ ABI 2.5.2
// offset to top).
struct InterfaceSlot {
  std::uint64_t slot = 0;
};

using DispatchSlot = std::variant<LineageSlot, InterfaceSlot>;

// How many entries back from the address a part holds its table at the table
// holds the offset from that part to the value's table address (C++ ABI 2.5.2
// offset to top), the first of the two entries every table opens with.
inline constexpr std::uint64_t kOffsetToTopEntry = 2;

// What a table holds where no class gives a virtual function a body: the host's
// C++ runtime ends the program if it is ever entered.
inline constexpr std::string_view kNoBody = "__cxa_pure_virtual";

// The allocation function a value's storage comes from (C++ ABI mangling of
// `operator new(std::size_t)`).
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
  auto Tuples() -> CodeGenTuples& {
    return tuples_;
  }
  auto Unit() const -> const lir::CompilationUnit& {
    return *unit_;
  }

  // The LLVM function a unit function was emitted as, reached by the identity a
  // call carries. Identity is the function's own, never a reconstructed symbol
  // name.
  auto UnitFunction(lir::FunctionId function) -> llvm::Function*;

  // The definition the library reads of a type whose values it builds -- a
  // class, a struct, or a closure: a constant the unit declaring the type
  // emits, which every unit reaches by the symbol it is linked under. A use
  // forwards its address without inspecting it, and a declaration of this unit
  // and one of another are named the same way.
  auto DefinitionOf(lir::TypeId type) -> diag::Result<llvm::Constant*>;

  // The type the library asks of a value of `type`: a tuple's own, which this
  // unit emits with the tuple type, an integral type's, which it emits too, or
  // the library's for a value of one of its own kinds.
  auto ValueTypeOf(lir::TypeId type) -> diag::Result<llvm::Constant*>;

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
  // The library entry `symbol`, which carries out lifecycle step `step` on an
  // object of the library's own. Ending one takes it; assigning takes the one
  // written into and then the one written; copying and moving take the source
  // and build in storage handed last, which they answer with.
  auto LifecycleEntry(std::string_view symbol, TupleLifecycle step)
      -> llvm::FunctionCallee;
  // The host's dynamic cast (C++ ABI 2.9.7): handed a part of an object, the
  // descriptions of that part's class and of the wanted one, and a hint of
  // where the wanted part sits, it answers the wanted part or null.
  auto DynamicCast() -> llvm::FunctionCallee;

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
  // unit or of another, or the part the runtime library builds where a class
  // extends one of its classes. Laid out once per
  // declaration: its own members after the whole of what it extends.
  auto RecordOf(lir::TypeId type) -> diag::Result<const RecordLayout*>;

  // The size of a complete object of the declaration `type` names, which is
  // what `operator new` is asked for where one is built.
  auto CompleteObjectSize(lir::TypeId type) -> diag::Result<std::uint64_t>;

  // Members of the types `members`, each in the slot `role` asks for, laid out
  // after `after`, naming each as `what` where it has no storage on this
  // backend.
  auto PlaceMembers(
      std::span<const lir::TypeId> members, MemberSlotRole role,
      const RecordLayout& after, std::string_view what)
      -> diag::Result<RecordLayout>;

  // Where a dispatch on `behavior` finds the body a value answers it with.
  auto DispatchSlotOf(const lir::StatedDispatchRef& behavior)
      -> diag::Result<DispatchSlot>;

  // Whether `type` names an interface class (LRM 8.26), whose part of a value
  // is reached by an offset the value's table holds rather than by one the
  // lineage fixes.
  [[nodiscard]] auto IsInterfaceClass(lir::TypeId type) const -> bool;

  // How far back from the address a part of class `view_class` holds its table
  // at lies the offset to the part of `interface`, in table entries.
  auto InterfacePartOffset(lir::TypeId view_class, lir::TypeId interface)
      -> std::uint64_t;

  // The description of the class `type` names that a cast reads, emitted here
  // for a class of this unit the first time one is asked for.
  auto TypeInfoOf(lir::TypeId type) -> llvm::Constant*;

  // Building each member `placed` lays out, in the storage at `value`, and
  // ending each of them, last placed first as a C++ class ends its members.
  void BeginMembers(
      llvm::IRBuilderBase& builder, llvm::Value* value,
      const RecordLayout& placed);
  void EndMembers(
      llvm::IRBuilderBase& builder, llvm::Value* value,
      const RecordLayout& placed);

 private:
  // Whether a member of `type` held as `storage` is the bytes this backend lays
  // a value of the type out in -- a product's, an integral value's, a machine
  // integer's -- rather than an object of the library.
  [[nodiscard]] auto HoldsItsBytesInline(
      lir::TypeId type, support::DeclaredMemberStorage storage) const -> bool;
  // What a member of `type` held as `storage` occupies.
  [[nodiscard]] auto MemberLayout(
      lir::TypeId type, support::DeclaredMemberStorage storage)
      -> support::ObjectLayout;
  // Builds, at `at`, the storage a member of `type` is held as.
  void BuildMemberStorage(
      llvm::IRBuilderBase& builder, lir::TypeId type,
      support::DeclaredMemberStorage storage, llvm::Value* at);
  // The arguments that tell an entry how wide an integral type is, and whether
  // it is four-state.
  auto WidthArg(const value::IntegralShape& shape) -> llvm::Constant*;
  auto FourStateArg(const value::IntegralShape& shape) -> llvm::Constant*;
  // What builds the storage of a member held as `storage`, handed where that
  // storage is and, where it is built at the width of what it holds, that
  // width; and what ends it, handed where it is.
  auto MemberStorageConstructor(support::DeclaredMemberStorage storage)
      -> llvm::FunctionCallee;
  auto MemberStorageDestructor(support::DeclaredMemberStorage storage)
      -> llvm::Function*;
  // The host's sized deallocation function, handed the storage and its size
  // (C++ ABI mangling of `operator delete(void*, std::size_t)`).
  auto SizedOperatorDelete() -> llvm::FunctionCallee;
  // What registers a function to run on an object when the program exits (C++
  // ABI 3.3.6.3): handed the function, the object and this module's handle.
  auto AtExit() -> llvm::FunctionCallee;

  auto DeclareCallable(lir::FunctionId id) -> llvm::Function*;
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
  // members; what it adds to dispatch; and the interface classes it names.
  struct Declared {
    std::string definition;
    std::optional<lir::TypeId> base;
    std::span<const lir::Member> members;
    const lir::ClassDispatch* dispatch = nullptr;
    std::span<const lir::TypeId> implements;
  };
  [[nodiscard]] auto DeclarationOf(lir::TypeId type) const -> Declared;
  // The interface classes a value of `declared` holds a part for, in the order
  // the parts are placed. Those of the class it extends come first, so a part
  // keeps its place in every class extending this one; then each interface
  // class it names, followed by what that one extends. Each appears once
  // however many ways it is arrived at.
  [[nodiscard]] auto InterfacePartsOf(const Declared& declared) const
      -> std::vector<lir::TypeId>;
  // Appends `iface` and what it extends to `parts`, in that order, skipping
  // what is already there.
  void AppendInterfacePart(
      lir::TypeId iface, std::vector<lir::TypeId>& parts) const;
  // The same, for a class that is a step of a lineage: one that extends
  // something.
  [[nodiscard]] auto LineageStepOf(lir::TypeId type) const -> Declared;
  // The class `declared` extends, where some unit declares it rather than the
  // library.
  [[nodiscard]] auto ExtendedClassOf(const Declared& declared) const
      -> std::optional<lir::TypeId>;
  // Which of a lineage's bodies answers `behavior`, counted from the first.
  auto LineageIndexOf(const lir::StatedDispatchRef& behavior) -> std::uint64_t;
  // How a whole value of `declared` is laid out.
  auto WholeLayOut(const Declared& declared) -> diag::Result<WholeRecord>;
  // Where, in a class's table group, each table a value holds starts: the one
  // for its lineage and one per interface class part, in the order the parts
  // are placed.
  struct TablePoints {
    std::uint64_t lineage = 0;
    std::vector<std::uint64_t> interfaces;
  };
  // The body answering each behavior a value of `type` carries, where the
  // table holds it, named by its symbol; none where no class of the lineage
  // gives the behavior a body (LRM 8.21). A class of the library carries its
  // destructor, which every class extending it gives a body of its own.
  auto TableOf(lir::TypeId type)
      -> const std::vector<std::optional<std::string>>&;
  // The same, for the declaration `step` describes, and for a class of the
  // library, whose own bodies are its virtual functions' as the library
  // defines them.
  auto TableOfStep(const Declared& step)
      -> std::vector<std::optional<std::string>>;
  auto LibraryTableOf(support::RuntimeClass which)
      -> std::vector<std::optional<std::string>>;
  // Emits the table group a value of the declaration `type` names dispatches
  // through: the table for its lineage and one for each interface class part,
  // whose behaviors `conforming` states the answers to.
  auto EmitTable(
      lir::TypeId type, const Declared& declared,
      std::span<const lir::ConformingBehavior> conforming)
      -> diag::Result<void>;
  // Defines the constant `value` under `symbol`.
  auto DefineConstant(const std::string& symbol, llvm::Constant* value)
      -> llvm::GlobalVariable*;
  // The address, `point` entries into the table group of the class whose
  // definition is linked under `definition`, that a value holds as where one
  // of its tables is.
  auto TableAddressPoint(std::string_view definition, std::uint64_t point)
      -> llvm::Constant*;
  // How a value of `declared` is laid out.
  auto LayOut(const Declared& declared) -> diag::Result<RecordLayout>;
  // How the storage a closure's captures live in is laid out. A closure extends
  // nothing, so its first capture starts where that storage does.
  auto LayOutCaptures(lir::ClosureId id) -> diag::Result<RecordLayout>;
  // Emits the constructor prologue of `declared`, which its constructor runs
  // once its base is built: the value's table address, then its own members,
  // then the table address of each interface class part a complete object of
  // it holds.
  auto EmitConstructorPrologue(const Declared& declared) -> diag::Result<void>;
  // Emits the three destructors of `declared`, as clang emits them for a class
  // whose destructor is implicit: the base object destructor ends its own
  // members, last placed first, then what it extends; the complete object
  // destructor is the base object one, since an interface class part holds
  // nothing to end; the deleting destructor ends the value and gives its
  // storage back.
  auto EmitDestructors(const Declared& declared) -> diag::Result<void>;
  // The definition linked under `symbol`, or a declaration of it where nothing
  // has defined it yet.
  auto DefinitionGlobal(const std::string& symbol) -> llvm::GlobalVariable*;
  // An integer of `bytes` bytes, as a field of a constant record.
  auto Int(std::uint64_t value, unsigned bytes) -> llvm::Constant*;
  // The address of `name` as a NUL-terminated constant.
  auto NameConstant(std::string_view name) -> llvm::Constant*;
  // A body a declaration's values are built or ended by, this unit's or
  // another's, linked under `symbol`: it takes the value and answers nothing.
  auto ValueFunction(const std::string& symbol) -> llvm::Function*;
  auto DeclaredClass(lir::ClassId id) const -> Declared;
  auto DefineEnumTable(lir::EnumTableId table) -> llvm::GlobalVariable*;
  auto DefineIntegralConstant(lir::IntegralConstantId constant)
      -> llvm::GlobalVariable*;
  // What the library is handed as the type of a value of the integral type
  // `type`, where a value crosses with its type: constant data of the class
  // the library declares for an integral type, stating the width, signedness
  // and states that class's operations read. The module defines one per
  // integral type it hands over that way.
  auto GetAddrOfIntegralType(lir::TypeId type) -> llvm::Constant*;

  std::unique_ptr<llvm::LLVMContext> context_;
  std::unique_ptr<llvm::Module> module_;
  const lir::CompilationUnit* unit_;
  CodeGenTypes types_;
  CodeGenTuples tuples_;
  base::Translation<lir::FunctionId, llvm::Function*> functions_;
  std::unordered_map<lir::EnumTableId, llvm::GlobalVariable*> enum_tables_;
  std::unordered_map<lir::IntegralConstantId, llvm::GlobalVariable*>
      integral_constants_;
  std::unordered_map<lir::TypeId, llvm::GlobalVariable*> integral_types_;
  std::unordered_map<lir::TypeId, RecordLayout> records_;
  std::unordered_map<lir::TypeId, std::vector<std::optional<std::string>>>
      tables_;
  // Where each table group this unit emitted starts each table, by the
  // symbol its class's definition is linked under.
  std::unordered_map<std::string, TablePoints> table_points_;
};

}  // namespace lyra::backend::llvm_backend
