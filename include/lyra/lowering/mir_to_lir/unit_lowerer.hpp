#pragma once

#include <deque>
#include <map>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <vector>

#include "lyra/base/translation.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/enum_table_id.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/function_id.hpp"
#include "lyra/lir/integral_constant_id.hpp"
#include "lyra/lir/symbol_name.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/mir/bring_up_entry.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/closure_id.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/enum_table_id.hpp"
#include "lyra/mir/integral_constant_id.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/value_operation.hpp"

namespace lyra::lowering::mir_to_lir {

// The symbol a namespace unit's bring-up entry is emitted and linked under. It
// is composed from the unit and which of them it is, never from a name, so the
// unit that defines it and whoever calls it arrive at the same string with
// nothing shared between them.
[[nodiscard]] auto BringUpEntrySymbol(
    std::string_view unit_name, mir::BringUpEntry entry) -> std::string;

// Per-unit lowerer for the MIR-to-LIR pass. Reads the source MIR, owns the
// in-progress LIR unit, and memoizes type translation so each distinct MIR type
// mints one canonical LIR type. After `Run` returns, the produced LIR unit
// holds no reference to the MIR it was lowered from.
class UnitLowerer {
 public:
  // The unit being produced carries its source's name from the moment it
  // exists: it is the one thing about the output that is settled before any
  // lowering happens, and everything else here is minted as the pass runs.
  explicit UnitLowerer(const mir::CompilationUnit& mir) : mir_(&mir) {
    out_.name = mir.name;
    out_.body_name = mir.body_name;
  }

  auto Run() -> diag::Result<lir::CompilationUnit>;

  [[nodiscard]] auto Mir() const -> const mir::CompilationUnit& {
    return *mir_;
  }

  // Translates a MIR type to its LIR-owned identity, minting it on first use.
  // Mirrors the MIR type universe: a generic type maps mechanically to its LIR
  // counterpart.
  auto TranslateType(mir::TypeId id) -> lir::TypeId;
  auto TranslateTypes(std::span<const mir::TypeId> source)
      -> std::vector<lir::TypeId>;

  // Translates a MIR enumeration member table to its LIR-owned identity. Every
  // table the unit holds is lowered, in pool order, so the two pools run in
  // step and the position carries across.
  [[nodiscard]] static auto TranslateEnumTable(mir::EnumTableId id)
      -> lir::EnumTableId {
    return lir::EnumTableId{.value = id.value};
  }

  // Translates a MIR constant to its LIR-owned identity. Every constant the
  // unit holds is lowered, in pool order, so the two pools run in step and the
  // position carries across.
  [[nodiscard]] static auto TranslateConstant(mir::IntegralConstantId id)
      -> lir::IntegralConstantId {
    return lir::IntegralConstantId{.value = id.value};
  }

  [[nodiscard]] auto Types() const -> const lir::TypePool& {
    return out_.types;
  }

  // The LIR unit being built, whose records are settled before any body
  // lowers, so a body may ask what any of them is made of.
  [[nodiscard]] auto Lir() const -> const lir::CompilationUnit& {
    return out_;
  }

  // The scalar a conditional branch tests: MIR's own machine boolean
  // translated, so the two layers cannot disagree about what a predicate
  // reduces to.
  auto MachineBoolType() -> lir::TypeId;

  // A control effect crossing to or from the runtime. Its shape does not
  // depend on which region catches it -- an effect is the target it names --
  // so a point that asks the runtime for one names the type directly.
  [[nodiscard]] auto ControlEffectType() const -> lir::TypeId;

  // The type of the values one closure declaration builds. A closure whose
  // invoke completes as a coroutine states that protocol as its own type, so
  // its captures are still storage of this type but no MIR type names it; the
  // declaration is what does.
  auto ClosureValueType(mir::ClosureId closure) -> lir::TypeId;

  // The type of the values one class builds, whichever unit defines it. A
  // member step names the class that declares the member, which is not always
  // the class the receiver has: a class carries what its bases declare as well
  // as its own.
  auto ClassValueType(mir::ClassId cls) -> lir::TypeId;

  // What a direct call of the callable `callable` of `owner` enters: the
  // function this unit lowers it to, or the symbol the unit defining the class
  // emits it under.
  [[nodiscard]] auto CallableCallTarget(
      mir::ClassId owner, mir::CallableId callable) const -> lir::CallTarget;

  // What a construction of an object of `cls` enters, the same way.
  [[nodiscard]] auto ConstructorCallTarget(mir::ClassId cls) const
      -> lir::CallTarget;

  // The LIR function a class's callable lowers to. Throws if `callable` has no
  // body in `owner` -- a DPI-C import is reached as a foreign symbol and a pure
  // virtual has no implementation here, so neither is a function of this unit.
  [[nodiscard]] auto MethodFunction(
      mir::ClassId owner, mir::CallableId callable) const -> lir::FunctionId;
  // The LIR function a closure's body lowers to.
  [[nodiscard]] auto ClosureFunction(mir::ClosureId closure) const
      -> lir::FunctionId;

  // The symbol a variable of a unit's namespace is linked under, by the unit
  // whose namespace owns it.
  [[nodiscard]] auto StaticVariableSymbolOf(
      mir::StaticVariableId variable) const -> std::string;

  // The symbol a static property is linked under, by the unit defining its
  // class.
  [[nodiscard]] auto StaticPropertySymbolOf(
      const mir::StaticPropertyRef& ref) const -> std::string;

  // The symbol a constant of a class of this unit is linked under.
  [[nodiscard]] auto ClassConstantSymbolOf(
      const mir::ClassConstantRef& constant) const -> std::string;

  // The function of this unit answering `answers` at `type`.
  [[nodiscard]] auto TypeOperationFunction(
      mir::TypeId type, support::ValueOperation answers) -> lir::FunctionId;

  // The behavior a MIR dispatch slot names, as LIR names one: the declaration
  // that introduced it, and which of that declaration's introductions it is.
  // The slot is its own canonical identity, so this reads that one class and
  // nothing else -- where the behavior lands in a whole value is a layout
  // question, answered from the lineage below this pass.
  [[nodiscard]] auto SlotRef(const mir::VirtualSlot& slot)
      -> lir::StatedDispatchRef;

  // The address of the entry of `method`, a method of a class this unit
  // declares, whichever unit defines it, at `prototype`.
  [[nodiscard]] auto EntryConstant(
      const mir::CallableTarget& method, lir::TypeId prototype)
      -> lir::TableBody;

  // The LIR function a class's constructor lowers to. Every class an object is
  // built of defines its own construction, so every one of them has this
  // function, and it is named before any body is lowered -- which is what lets
  // a derived class enter its base's whichever order the two are lowered in.
  [[nodiscard]] auto ConstructorFunction(mir::ClassId cls) const
      -> lir::FunctionId;

  // The symbol a callable of this unit's namespace is emitted and linked under:
  // whatever reaches it where anything does, and the position it sits at where
  // nothing does, composed for the linker. A call written inside this unit
  // composes the same symbol from the same parts, so the two agree with no
  // table between them.
  [[nodiscard]] auto UnitCallableSymbol(mir::CallableId id) const
      -> std::string;

  // The symbol of the concrete form of the generic definition `definition` at
  // `arguments`, as rustc collects an instance per generic and its arguments:
  // the first call naming the pair makes it one of the unit's instances, which
  // are lowered once every other body has been, and every later call names the
  // same one.
  [[nodiscard]] auto GenericInstanceSymbol(
      mir::CallableId definition, std::vector<mir::TypeId> arguments)
      -> std::string;

 private:
  // The type of what a class extends: a class some unit declares, or the
  // library class either root is.
  [[nodiscard]] auto BaseType(const mir::ClassRef& base) -> lir::TypeId;

  // The declaration a closure's captures are members of.
  [[nodiscard]] auto ClosureDeclaration(mir::ClosureId closure) const
      -> lir::ClosureId;

  // The declaration a struct or a union of this unit is, which its members and
  // its methods are listed on.
  [[nodiscard]] auto RecordDeclaration(mir::RecordId record) const
      -> lir::RecordId;

  // What is settled about one MIR class before any body of it is lowered: its
  // own LIR identity, its constructor's function where it has a constructor,
  // one function identity per callable that has a body here, and which of the
  // class's introductions each callable is, counted in the order it introduces
  // them. A callable with no body here is no function of this unit and holds
  // none; one that introduces no behavior holds no ordinal, and the two are
  // independent.
  struct ClassIdentities {
    lir::ClassId lir_class{};
    std::optional<lir::FunctionId> constructor;
    base::Translation<mir::CallableId, std::optional<lir::FunctionId>> methods;
    base::Translation<mir::CallableId, std::optional<lir::DispatchOrdinal>>
        ordinals;
  };

  // The LIR identities taken on behalf of one MIR closure: the declaration its
  // captures are members of, and the function its invoke becomes.
  struct ClosureIdentities {
    lir::ClosureId declaration{};
    lir::FunctionId invoke{};
  };

  // Everything one class can be named by before it is built: its own LIR
  // identity and one function identity per body it will contribute. The
  // identities come from the LIR pools, which hold the reservation itself; what
  // this answers is which LIR identity stands for which MIR entity, since
  // neither pool's numbering determines the other's. Reads nothing but this
  // class, so classes take theirs in any order and none waits on another.
  [[nodiscard]] auto TakeClassIdentities(const mir::Class& cls)
      -> ClassIdentities;

  // The symbol one body of `cls` is emitted and linked under, as a body of the
  // class `class_part` names. Which body it is decides the rest: a body the
  // source declared is reached by its name, and one the compiler synthesized
  // by which body it is.
  [[nodiscard]] auto ClassBodySymbol(
      lir::SymbolPart class_part, const mir::Class& cls,
      mir::CallableId id) const -> std::string;

  auto TranslateType(const mir::Type& ty) -> lir::Type;
  static auto TranslateRuntimeLibrary(mir::RuntimeLibraryKind kind)
      -> lir::Type;
  auto LowerClass(mir::ClassId owner, const mir::Class& cls)
      -> diag::Result<lir::Class>;
  // The value `id` of `build` states, as the data it is. A constant is built
  // from literals, addresses and structures and arrays of them, so anything
  // else here is a producer that stated one out of something that runs.
  auto LowerConstant(const mir::ValueBuild& build, mir::ExprId id)
      -> lir::Constant;
  // The symbol the definition of the class `of` names is linked under,
  // whichever unit defines it.
  [[nodiscard]] auto DefinitionSymbolOf(mir::ClassId of) const -> std::string;

  // The part of a symbol that names the class `id`.
  [[nodiscard]] auto ClassPart(mir::ClassId id) const -> lir::SymbolPart;

  // The qualifier a symbol of the class `cls` is spelled under: the unit
  // defining it.
  [[nodiscard]] auto ClassQualifier(const mir::Class& cls) const
      -> std::string_view;

  // What this unit declares of a class another unit defines, as the record a
  // class extending it, or reaching into it, is laid out against.
  auto LowerDeclaredClass(const mir::Class& cls) -> lir::Class;

  // What an object of `cls` dispatches on and what answers each behavior of
  // every interface class it is an object of, as its unit stated them.
  auto LowerDispatch(const mir::Class& cls)
      -> std::vector<lir::DispatchedBehavior>;
  auto LowerInterfaceTables(const mir::Class& cls)
      -> std::vector<lir::InterfaceTable>;

  // What a body a table holds is declared at where another unit defines it: a
  // table holds its address as data, and nothing here calls through it.
  auto TableBodyPrototype() -> lir::TypeId;

  const mir::CompilationUnit* mir_;
  lir::CompilationUnit out_;
  std::unordered_map<mir::TypeId, lir::TypeId> type_memo_;
  base::Translation<mir::ClassId, ClassIdentities> class_identities_;
  base::Translation<mir::ClosureId, ClosureIdentities> closure_identities_;
  base::Translation<mir::RecordId, lir::RecordId> record_identities_;
  // The function answering each operation of each type the unit states its
  // operations for, by the type and the operation.
  std::map<std::pair<lir::TypeId, support::ValueOperation>, lir::FunctionId>
      type_operation_functions_;

  // One concrete form of a generic definition this unit's calls name: the
  // symbol it is emitted under, and its body with the types it is made at
  // substituted.
  struct GenericInstance {
    std::string symbol;
    mir::CallableCode code;
  };
  // Every instance named so far, in the order a call first named it; lowering
  // one may name more, which a deque adds without moving those before it.
  std::deque<GenericInstance> generic_instances_;
  // Where each instance lies among them, by the definition and the types it is
  // made at.
  std::map<std::pair<mir::CallableId, std::vector<mir::TypeId>>, std::size_t>
      generic_instance_at_;
};

}  // namespace lyra::lowering::mir_to_lir
