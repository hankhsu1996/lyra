#pragma once

#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

#include "lyra/base/translation.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/function_id.hpp"
#include "lyra/lir/integral_constant_id.hpp"
#include "lyra/lir/symbol_name.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/mir/behavior_ordinal.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/closure_id.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/integral_constant_id.hpp"
#include "lyra/mir/minted_entry.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::mir_to_lir {

// The symbol a body of a unit that answers to no name is emitted and linked
// under. It is composed from the unit and which of them it is, never from a
// name, so the unit that defines it and whoever calls it arrive at the same
// string with nothing shared between them.
[[nodiscard]] auto MintedEntrySymbol(
    std::string_view unit_name, mir::MintedEntry entry) -> std::string;

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
  // counterpart.
  auto TranslateType(mir::TypeId id) -> lir::TypeId;
  auto TranslateTypes(std::span<const mir::TypeId> source)
      -> std::vector<lir::TypeId>;
  // A declaration another unit names a type by, which crosses unchanged.
  [[nodiscard]] static auto TranslateDeclaration(
      const mir::TypeDeclarationRef& ref) -> lir::TypeDeclarationRef;

  // Translates a MIR description to its LIR-owned identity. Every description
  // the unit holds is lowered, in pool order, so the two pools run in step and
  // the position carries across.
  [[nodiscard]] static auto TranslateDescriptor(mir::TypeDescriptorId id)
      -> lir::TypeDescriptorId {
    return lir::TypeDescriptorId{.value = id.value};
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

  // The type of the values one class declaration builds. A member step names
  // the class that declares the member, which is not always the class the
  // receiver has: a class carries what its bases declare as well as its own.
  auto ClassValueType(mir::ClassId cls) -> lir::TypeId;

  // The type naming a class another unit declares. The pair is the whole
  // identity, which is what lets a property step and a dispatch on one name the
  // declaration without an id of this unit standing for it.
  [[nodiscard]] auto ExternalClassValueType(
      const std::string& unit_name, const std::string& class_name) const
      -> lir::TypeId;

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

  // The behavior a MIR slot names, whichever unit introduced it.
  [[nodiscard]] auto SlotRef(const mir::VirtualSlot& slot)
      -> lir::StatedDispatchRef;

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

  // The type of the values a class some unit declares builds, however the
  // reference names that class.
  [[nodiscard]] auto ClassRefValueType(const mir::DeclaredClassRef& of)
      -> lir::TypeId;

 private:
  // The behavior a class of another unit introduced, named by that class and
  // which of its introductions the behavior is.
  [[nodiscard]] auto ExternalMethodRef(
      const std::string& unit_name, const std::string& class_name,
      mir::BehaviorOrdinal ordinal) const -> lir::StatedDispatchRef;

  // The type of what a class extends: a class some unit declares, or the
  // library class either root is.
  [[nodiscard]] auto BaseType(const mir::ClassRef& base) -> lir::TypeId;

  // The declaration a closure's captures are members of, and the function its
  // invoke lowers to.
  [[nodiscard]] auto ClosureDeclaration(mir::ClosureId closure) const
      -> lir::ClosureId;
  [[nodiscard]] auto ClosureFunction(mir::ClosureId closure) const
      -> lir::FunctionId;

  // The declaration a struct of this unit is, which its components and its
  // methods are listed on.
  [[nodiscard]] auto StructDeclaration(mir::StructId record) const
      -> lir::StructId;

  // What is settled about one MIR class before any body of it is lowered: its
  // own LIR identity, its constructor's function where it has a constructor,
  // one function identity per callable that has a body, and which of the
  // class's introductions each callable is, counted in the order it introduces
  // them. A callable with no body is no function of this unit and holds none;
  // one that introduces no behavior holds no ordinal, and the two are
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
  // What `cls` adds to the dispatch its lineage carries, each body named by the
  // symbol it is emitted under.
  auto LowerDispatch(mir::ClassId owner, const mir::Class& cls)
      -> lir::ClassDispatch;
  // The value `id` of `build` states, as the data it is. A constant is built
  // from literals, addresses and structures and arrays of them, so anything
  // else here is a producer that stated one out of something that runs.
  auto LowerConstant(const mir::ValueBuild& build, mir::ExprId id)
      -> lir::Constant;
  // The symbol the definition of the class `of` names is linked under,
  // whichever unit declares it.
  [[nodiscard]] auto DefinitionSymbolOf(const mir::DeclaredClassRef& of) const
      -> std::string;

  const mir::CompilationUnit* mir_;
  lir::CompilationUnit out_;
  std::unordered_map<mir::TypeId, lir::TypeId> type_memo_;
  base::Translation<mir::ClassId, ClassIdentities> class_identities_;
  base::Translation<mir::ClosureId, ClosureIdentities> closure_identities_;
  base::Translation<mir::StructId, lir::StructId> struct_identities_;
};

}  // namespace lyra::lowering::mir_to_lir
