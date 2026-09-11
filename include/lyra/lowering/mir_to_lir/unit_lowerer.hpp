#pragma once

#include <map>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

#include "lyra/base/translation.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/function_id.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/closure_id.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/namespace_storage_phase.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::mir_to_lir {

// The symbol one of the two bodies bringing up a unit's namespace is emitted
// and linked under. It is composed from the unit and which of the two it is,
// never from a name, so the unit that defines it and the design root that calls
// it arrive at the same string with nothing shared between them.
[[nodiscard]] auto StorageEntrySymbol(
    std::string_view unit_name, mir::NamespaceStoragePhase phase)
    -> std::string;

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
  }

  auto Run() -> diag::Result<lir::CompilationUnit>;

  [[nodiscard]] auto Mir() const -> const mir::CompilationUnit& {
    return *mir_;
  }

  // Translates a MIR type to its LIR-owned identity, minting it on first use.
  // Mirrors the MIR type universe: a generic type maps mechanically to its LIR
  // counterpart. A type with no LIR mirror yet records an unsupported-type
  // error read at `Run`; it never silently mistranslates.
  auto TranslateType(mir::TypeId id) -> lir::TypeId;

  [[nodiscard]] auto Types() const -> const lir::TypePool& {
    return out_.types;
  }

  // The scalar a conditional branch tests: MIR's own machine boolean
  // translated, so the two layers cannot disagree about what a predicate
  // reduces to.
  auto MachineBoolType() -> lir::TypeId;

  // The product a call completes with where the operation answers with more
  // than one value, which no source-level type names.
  auto ProductOf(std::vector<lir::TypeId> components) -> lir::TypeId;

  // A control effect crossing to or from the runtime. Its shape does not
  // depend on which region catches it -- an effect is the target it names --
  // so a point that asks the runtime for one names the type directly.
  auto ControlEffectType() -> lir::TypeId;

  // The type of the values one closure declaration builds. A closure whose
  // invoke completes as a coroutine states that protocol as its own type, so
  // its captures are still storage of this type but no MIR type names it; the
  // declaration is what does.
  auto ClosureValueType(mir::ClosureId closure) -> lir::TypeId;

  // The type of the values one class declaration builds. A member step names
  // the class that declares the member, which is not always the class the
  // receiver has: a class carries what its bases declare as well as its own.
  auto ClassValueType(mir::ClassId cls) -> lir::TypeId;

  // The type of the values one generated-struct declaration builds.
  auto StructValueType(mir::StructId record) -> lir::TypeId;

  // The type of the values one object another unit published is an instance
  // of. Only the published prefix of its layout is named here, which is what a
  // member reached through it may name.
  auto ExternalUnitObjectValueType(mir::ExternalUnitObjectId object)
      -> lir::TypeId;

  // The type naming a class another unit declares. The pair is the whole
  // identity, which is what lets a property step and a dispatch on one name the
  // declaration without an id of this unit standing for it.
  [[nodiscard]] auto ExternalClassValueType(
      const std::string& unit_name, const std::string& class_name) const
      -> lir::TypeId;

  // What another unit promised about the class it declares under this pair.
  // Every reference that reached one consumed its promise where the reference
  // was lowered, so a pair naming no record is a producer that emitted a
  // reference it had nothing to compile against.
  [[nodiscard]] auto PromisedClass(
      const std::string& unit_name, const std::string& class_name) const
      -> const mir::ExternalClass&;

  // The LIR function a class's callable lowers to. Throws if `callable` has no
  // body in `owner` -- a DPI-C import is reached as a foreign symbol and a pure
  // virtual has no implementation here, so neither is a function of this unit.
  [[nodiscard]] auto MethodFunction(
      mir::ClassId owner, mir::CallableId callable) const -> lir::FunctionId;

  // The behavior a MIR dispatch slot names, as LIR names one: the declaration
  // that introduced it, and which of that declaration's introductions it is.
  // `owner` and `callable` are the slot's own canonical identity, so this reads
  // that one class and nothing else -- where the behavior lands in a whole
  // value is a layout question, answered from the lineage below this pass.
  [[nodiscard]] auto MethodRef(mir::ClassId owner, mir::CallableId callable)
      -> lir::StatedDispatchRef;

  // The LIR function a class's constructor lowers to. Every class defines its
  // own construction, so every one of them has this function, and it is named
  // before any body is lowered -- which is what lets a derived class enter its
  // base's whichever order the two are lowered in.
  [[nodiscard]] auto ConstructorFunction(mir::ClassId cls) const
      -> lir::FunctionId;

 private:
  // The declaration a closure's captures are members of, and the function its
  // invoke lowers to.
  [[nodiscard]] auto ClosureDeclaration(mir::ClosureId closure) const
      -> lir::ClosureId;
  [[nodiscard]] auto ClosureFunction(mir::ClosureId closure) const
      -> lir::FunctionId;

  // The declaration a struct's fields are members of. A struct carries no code,
  // so its identity is the declaration and nothing beside it.
  [[nodiscard]] auto StructDeclaration(mir::StructId record) const
      -> lir::StructId;

  // What is settled about one MIR class before any body of it is lowered: its
  // own LIR identity, its constructor's function, one function identity per
  // callable that has a body, the behaviors it introduces in the order it
  // introduces them, and which of those each callable is. A callable with no
  // body is no function of this unit and holds none; one that introduces no
  // behavior holds no ordinal, and the two are independent. An ordinal is read
  // off `introduces` as an entry is appended to it, so the position and the
  // list it indexes are one act.
  struct ClassIdentities {
    lir::ClassId lir_class{};
    lir::FunctionId constructor{};
    base::Translation<mir::CallableId, std::optional<lir::FunctionId>> methods;
    base::Translation<mir::CallableId, std::optional<lir::DispatchOrdinal>>
        ordinals;
    std::vector<lir::Introduction> introduces;
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

  // The behavior `callable` takes over from its lineage, if it takes one over
  // and answers it with a body. Absent otherwise, which covers introducing a
  // behavior, answering none, and taking one over with no body of its own.
  [[nodiscard]] auto TakenOver(
      const mir::CallableDecl& callable,
      const std::optional<lir::FunctionId>& body)
      -> std::optional<lir::DispatchTakeover>;

  // The symbol a callable of this unit's namespace is emitted and linked under,
  // which is whatever reaches it from outside the unit, composed for the
  // linker.
  [[nodiscard]] auto UnitCallableSymbol(mir::CallableId id) const
      -> std::string;

  // The symbol one body of `cls` is emitted and linked under. Which body it is
  // decides that: a body the source declared is reached by its name, and one
  // the compiler synthesized by which body it is.
  [[nodiscard]] auto ClassBodySymbol(
      const mir::Class& cls, mir::CallableId id) const -> std::string;

  auto TranslateType(const mir::Type& ty) -> lir::Type;
  // The LIR mirror of a runtime-library record type. MIR is written once for
  // every backend, so a record only the C++ backend realizes reaches here
  // whenever a program uses the construct behind it, and is recorded as an
  // unsupported type rather than read as a broken invariant.
  auto TranslateRuntimeLibrary(mir::RuntimeLibraryKind kind) -> lir::Type;
  // Records `what` (a human phrase like "a closure") as the unit's first
  // unmirrored-type error and returns a benign placeholder type; the unit fails
  // at `Run` before the placeholder is observed.
  auto RecordUnsupportedType(std::string_view what) -> lir::Type;
  auto LowerExternalUnitObject(const mir::ExternalUnitObject& object)
      -> lir::ExternalUnitObject;
  auto LowerClass(mir::ClassId owner, const mir::Class& cls)
      -> diag::Result<lir::Class>;
  auto LowerBase(mir::ClassId owner, const mir::ClassRef& base) const
      -> lir::Base;

  const mir::CompilationUnit* mir_;
  lir::CompilationUnit out_;
  std::unordered_map<mir::TypeId, lir::TypeId> type_memo_;
  base::Translation<mir::ClassId, ClassIdentities> class_identities_;
  base::Translation<mir::ExternalUnitObjectId, lir::ExternalUnitObjectId>
      external_unit_object_identities_;
  base::Translation<mir::ClosureId, ClosureIdentities> closure_identities_;
  base::Translation<mir::StructId, lir::StructId> struct_identities_;
  std::map<std::vector<lir::TypeId>, lir::TypeId> product_memo_;
  // Set the first time a MIR type with no LIR mirror is reached; surfaced as
  // the unit's failure at `Run`, so translation stays non-throwing and
  // total-shaped while an unmirrored type is still a clean diagnostic, not a
  // mistranslation.
  std::optional<diag::Diagnostic> type_error_;
};

}  // namespace lyra::lowering::mir_to_lir
