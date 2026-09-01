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
#include "lyra/lir/function_id.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/closure_id.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::mir_to_lir {

// The symbol a unit's namespace-level storage is linked under. A namespace
// name is unique only inside the unit that declares it while the whole program
// links into one name space, so the unit qualifies it. Both ends compose the
// string here: the unit that defines the storage from its own name, and a unit
// that reads it from the name its reference carries -- one rule, so the two
// agree with no shared table.
auto StaticVariableSymbol(
    std::string_view unit_name, std::string_view variable_name) -> std::string;

// Per-unit lowerer for the MIR-to-LIR pass. Reads the source MIR, owns the
// in-progress LIR unit, and memoizes type translation so each distinct MIR type
// mints one canonical LIR type. After `Run` returns, the produced LIR unit
// holds no reference to the MIR it was lowered from.
class UnitLowerer {
 public:
  explicit UnitLowerer(const mir::CompilationUnit& mir) : mir_(&mir) {
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


  // The LIR function a class's callable lowers to. Throws if `callable` has no
  // body in `owner` -- a DPI-C import is reached as a foreign symbol and a pure
  // virtual has no implementation here, so neither is a function of this unit.
  [[nodiscard]] auto MethodFunction(
      mir::ClassId owner, mir::CallableId callable) const -> lir::FunctionId;

 private:
  // The LIR function a closure's invoke lowers to, and the declaration its
  // captures are members of.
  [[nodiscard]] auto ClosureFunction(mir::ClosureId closure) const
      -> lir::FunctionId;
  [[nodiscard]] auto ClosureDeclaration(mir::ClosureId closure) const
      -> lir::ClosureId;

  // The LIR identities taken on behalf of one MIR class: the class itself, its
  // constructor's function, and one per callable that has a body. A callable
  // with no body is no function of this unit and holds none. `lir::Class` holds
  // the class's content, under these same identities.
  struct ClassIdentities {
    lir::ClassId lir_class{};
    lir::FunctionId constructor{};
    base::Translation<mir::CallableId, std::optional<lir::FunctionId>> methods;
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

  // The symbol a callable of this unit's namespace is emitted and linked under.
  [[nodiscard]] auto UnitCallableSymbol(const mir::CallableDecl& callable) const
      -> std::string;

  // The symbol a class of this unit is emitted and linked under, and the
  // qualifier its bodies take.
  [[nodiscard]] auto ClassSymbol(const mir::Class& cls) const -> std::string;

  // The symbol a closure of this unit is emitted and linked under, and the
  // qualifier its invoke takes.
  [[nodiscard]] auto ClosureSymbol(mir::ClosureId closure) const -> std::string;

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
  auto LowerBase(const mir::ClassRef& base) const -> lir::Base;

  const mir::CompilationUnit* mir_;
  lir::CompilationUnit out_;
  std::unordered_map<mir::TypeId, lir::TypeId> type_memo_;
  base::Translation<mir::ClassId, ClassIdentities> class_identities_;
  base::Translation<mir::ExternalUnitObjectId, lir::ExternalUnitObjectId>
      external_unit_object_identities_;
  base::Translation<mir::ClosureId, ClosureIdentities> closure_identities_;
  std::map<std::vector<lir::TypeId>, lir::TypeId> product_memo_;
  // Set the first time a MIR type with no LIR mirror is reached; surfaced as
  // the unit's failure at `Run`, so translation stays non-throwing and
  // total-shaped while an unmirrored type is still a clean diagnostic, not a
  // mistranslation.
  std::optional<diag::Diagnostic> type_error_;
};

}  // namespace lyra::lowering::mir_to_lir
