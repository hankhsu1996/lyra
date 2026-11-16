#pragma once

#include <cstddef>
#include <functional>
#include <optional>
#include <set>
#include <span>
#include <string>
#include <string_view>
#include <unordered_map>
#include <variant>
#include <vector>

#include "lyra/base/translation.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/constant_id.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/function_id.hpp"
#include "lyra/lir/referent.hpp"
#include "lyra/lir/symbol_name.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/lir/value_step.hpp"
#include "lyra/mir/bring_up_entry.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/closure_id.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/constant_id.hpp"
#include "lyra/mir/constant_value.hpp"
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

// Where the legalized body of an operation of a type comes from, none where the
// language gives the type no such operation. It is handed in, since building
// one is the work of the layer above and which are needed is learned below.
using OperationBodyProvider = std::function<std::optional<mir::CallableCode>(
    mir::TypeId, support::ValueOperation)>;

// Per-unit lowerer for the MIR-to-LIR pass. Reads the source MIR, owns the
// in-progress LIR unit, and memoizes type translation so each distinct MIR type
// mints one canonical LIR type. Every construct is rejected with a diagnostic
// rather than passed through where it cannot be lowered yet. Once `Finish`
// returns, the produced LIR unit holds no reference to the MIR it was lowered
// from.
//
// Whoever drives it lowers what the unit declares, then alternates asking for
// the operations of types no declaration states them for and stating those
// asked, each named by a function on the first ask, until nothing new is asked.
class UnitLowerer {
 public:
  // A lowerer holding every body of `mir` lowered, and what each class it
  // defines answers about its parts.
  [[nodiscard]] static auto LowerUnit(const mir::CompilationUnit& mir)
      -> diag::Result<UnitLowerer>;

  // Marks `operation` as used by the unit, which the language may give its type
  // no such thing as.
  void MarkOperationUsed(const lir::TypeOperation& operation);

  // Lowers each operation marked used since the last call, its body obtained
  // from `bodies`, and answers whether there was any. Two types MIR tells apart
  // and LIR does not are one representation with one function per operation,
  // which the first of them states: a type's bodies read only what LIR keeps of
  // it, so the two bodies are the same.
  auto StateTypeOperations(const OperationBodyProvider& bodies)
      -> diag::Result<bool>;

  // Translates what a body may add to and nothing lowers from, and hands over
  // the unit.
  auto Finish() && -> lir::CompilationUnit;

  // States, for every type named since the last call, a function for each step
  // whose answer is one, and for a value laid out by its type one for every
  // step it has, which the library holding one reaches through its table. A
  // body is what names a type, so it is called once lowered bodies have.
  void StateTypeSteps();

  [[nodiscard]] auto Mir() const -> const mir::CompilationUnit& {
    return *mir_;
  }

  // Translates a MIR type to its LIR-owned identity, minting it on first use.
  // Mirrors the MIR type universe: a generic type maps mechanically to its LIR
  // counterpart.
  auto TranslateType(mir::TypeId id) -> lir::TypeId;
  auto TranslateTypes(std::span<const mir::TypeId> source)
      -> std::vector<lir::TypeId>;

  // Translates a MIR constant to its LIR-owned identity. Every constant the
  // unit holds is lowered, in pool order, so the two pools run in step and the
  // position carries across.
  [[nodiscard]] static auto TranslateConstant(mir::ConstantId id)
      -> lir::ConstantId {
    return lir::ConstantId{.value = id.value};
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
  // What an address naming `target` names, where `named` is the type of what
  // it names: one translation for every place an address stands in LIR. A
  // local is a place and never such an address.
  auto ReferentOf(const mir::ReferenceTarget& target, mir::TypeId named)
      -> lir::Referent;

  // What a direct call of `callable` enters: the function this unit lowers a
  // body it defines to, whoever declares it; the symbol the unit defining it
  // emits it under for a callable of a class another unit defines, a subroutine
  // of another unit's namespace and a method of another unit's record, which
  // every unit reaching one spells alike; or the foreign symbol a function the
  // foreign program defines is (LRM 35.4).
  [[nodiscard]] auto CallTargetOf(mir::CallableId callable) const
      -> lir::CallTarget;

  // What a construction of an object of `cls` enters, the same way.
  [[nodiscard]] auto ConstructionCallTargetOf(mir::ClassId cls) const
      -> lir::CallTarget;

  // The symbol a variable of a unit's namespace is linked under, by the unit
  // whose namespace owns it.
  [[nodiscard]] auto StaticVariableSymbolOf(
      mir::StaticVariableId variable) const -> std::string;

  // The symbol a static property is linked under, by the unit defining its
  // class.
  [[nodiscard]] auto StaticPropertySymbolOf(
      const mir::StaticPropertyRef& ref) const -> std::string;

  // The function of this unit answering `answers` at `type`, which a call names
  // whether or not its body is lowered yet.
  [[nodiscard]] auto TypeOperationFunction(
      mir::TypeId type, support::ValueOperation answers) -> lir::FunctionId;

  // The behavior a MIR dispatch slot names, as LIR names one: the declaration
  // that introduced it, and which of that declaration's introductions it is.
  // The slot is its own canonical identity, so this reads that one class and
  // nothing else -- where the behavior lands in a whole value is a layout
  // question, answered from the lineage below this pass.
  [[nodiscard]] auto SlotRef(const mir::VirtualSlot& slot)
      -> lir::StatedDispatchRef;

 private:
  // How code naming a callable enters it, which its declaration's form alone
  // decides.
  using CallableEntered =
      std::variant<lir::FunctionTarget, lir::SymbolTarget, lir::ForeignTarget>;

  // How code naming `callable` enters it: the function this unit lowers it to
  // where this unit defines it, whoever declares it; the symbol the unit
  // defining it emits it under, which every unit reaching it spells alike; or
  // the foreign symbol a function the foreign program defines is (LRM 35.4). A
  // behavior its class leaves to what extends it is entered only by dispatch.
  [[nodiscard]] auto EnteredAs(mir::CallableId callable) const
      -> CallableEntered;

  // The symbol the unit defining `callable` emits it under.
  [[nodiscard]] auto SymbolOfElsewhere(mir::CallableId callable) const
      -> std::string;

  // The address of `callable`, at `prototype`, entered as `EnteredAs` says.
  [[nodiscard]] auto ReferentOfCallable(
      mir::CallableId callable, lir::TypeId prototype) const -> lir::TableBody;

  // The LIR function `callable` lowers to, whoever declares it. Throws where it
  // has no body here -- a DPI-C import is reached as a foreign symbol, a pure
  // virtual has no implementation, and another unit's body is that unit's, so
  // none is a function of this unit.
  [[nodiscard]] auto FunctionOf(mir::CallableId callable) const
      -> lir::FunctionId;

  // The address of the entry of `method`, a method of a class this unit
  // declares, whichever unit defines it, at `prototype`.
  [[nodiscard]] auto EntryBody(mir::CallableId method, lir::TypeId prototype)
      -> lir::TableBody;

  // The symbol a callable of this unit's namespace is emitted and linked under:
  // whatever reaches it where anything does, and the position it sits at where
  // nothing does, composed for the linker. A unit naming another's subroutine
  // composes the same symbol from the name that unit published it under.
  [[nodiscard]] auto NamespaceCallableSymbolOf(mir::CallableId id) const
      -> std::string;

  // The unit being produced carries its source's name from the moment it
  // exists: it is the one thing about the output that is settled before any
  // lowering happens, and everything else here is minted as the pass runs.
  explicit UnitLowerer(const mir::CompilationUnit& mir) : mir_(&mir) {
    out_.name = mir.name;
    out_.body_name = mir.body_name;
  }

  // Fills the unit being produced with what `mir_` declares.
  auto Populate() -> diag::Result<void>;

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

  // Which of the introductions of the class `cls` each callable introducing a
  // behavior is, counted in the order it introduces them, recorded against the
  // callable. One that introduces no behavior holds no ordinal. Reads nothing
  // but this class and its callables' declarations, so classes take theirs in
  // any order and none waits on another.
  void TakeDispatchOrdinals(
      mir::ClassId cls,
      std::vector<std::optional<lir::DispatchOrdinal>>& ordinals) const;

  // The symbol one body a class declares is emitted and linked under, as a body
  // of that class: a synthesized body is spelled by the method it enters, the
  // callback it answers, its name, or its place among the class's callables.
  [[nodiscard]] auto ClassBodySymbolOf(mir::CallableId id) const -> std::string;

  // The symbol the construction of `cls` is emitted and linked under, which
  // the class qualifies as it does each of its bodies.
  [[nodiscard]] auto ConstructorSymbolOf(mir::ClassId cls) const -> std::string;

  // The symbol a method of a record is emitted and linked under, composed from
  // the record and the operation it answers, which is how every unit reaching
  // it names it.
  [[nodiscard]] auto RecordMethodSymbolOf(mir::CallableId method) const
      -> std::string;

  auto TranslateType(const mir::Type& ty) -> lir::Type;
  auto LowerClass(mir::ClassId owner, const mir::Class& cls)
      -> diag::Result<lir::Class>;
  // `value`, a value of `type`, as the data it is.
  auto LowerConstant(mir::TypeId type, const mir::ConstantValue& value)
      -> lir::Constant;
  // The class of this unit's LIR that `cls` is.
  [[nodiscard]] auto LirClass(mir::ClassId cls) const -> lir::ClassId;

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
  auto LowerDeclaredClass(mir::ClassId id, const mir::Class& cls) -> lir::Class;

  // What an object of `cls` dispatches on and what answers each behavior of
  // every interface it is an object of, as its unit stated them.
  auto LowerDispatch(const mir::Class& cls)
      -> std::vector<lir::DispatchedBehavior>;
  auto LowerInterfaceTables(const mir::Class& cls)
      -> std::vector<lir::InterfaceTable>;

  // What a body a table holds is declared at where another unit defines it: a
  // table holds its address as data, and nothing here calls through it.
  auto TableBodyPrototype() const -> lir::TypeId;

  [[nodiscard]] auto NeedsStepFunction(
      lir::TypeId type, lir::ValueStep step) const -> bool;
  // The function carrying out `step` of a value of `type`, built from its
  // parts: it takes where the value lies, and where the value it builds or
  // writes lies.
  auto BuildStepFunction(lir::TypeId type, lir::ValueStep step)
      -> lir::Function;
  // States, for every class this unit defines that a value is built of, how
  // the parts it places begin and end and how an object of it is created and
  // ended whole.
  void StateClassSteps();

  const mir::CompilationUnit* mir_;
  lir::CompilationUnit out_;
  std::unordered_map<mir::TypeId, lir::TypeId> type_memo_;
  // The first MIR type each LIR type translates, which is what an operation
  // found to be used below MIR is built at.
  std::unordered_map<lir::TypeId, mir::TypeId> mir_type_of_;
  base::Translation<mir::ClassId, lir::ClassId> class_identities_;
  base::Translation<mir::ClosureId, lir::ClosureId> closure_identities_;
  base::Translation<mir::RecordId, lir::RecordId> record_identities_;
  // The function each callable of the unit's table this unit defines lowers
  // to, whoever declares it, and none for one it only declares; and which of
  // its class's introductions each callable introducing a behavior is. Both by
  // the callable's identity.
  base::Translation<mir::CallableId, std::optional<lir::FunctionId>> functions_;
  base::Translation<mir::CallableId, std::optional<lir::DispatchOrdinal>>
      ordinals_;
  // An operation marked used and not yet lowered, with the MIR type it is built
  // at.
  struct PendingOperation {
    lir::TypeOperation operation;
    mir::TypeId mir_type;
  };
  std::vector<PendingOperation> pending_;
  // Every operation ever handed to `pending_`, so each is lowered once.
  std::set<lir::TypeOperation> visited_;
  // How many types of the pool have had their steps stated, in pool order.
  std::size_t types_seen_ = 0;
};

}  // namespace lyra::lowering::mir_to_lir
