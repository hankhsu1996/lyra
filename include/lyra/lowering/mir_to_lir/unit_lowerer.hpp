#pragma once

#include <optional>
#include <string_view>
#include <unordered_map>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/function_id.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/lir/type_query.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/closure_id.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::mir_to_lir {

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

  [[nodiscard]] auto Types() const -> const lir::TypeArena& {
    return out_.types;
  }

  // LIR types the lowering needs that no MIR type maps to: the reference an
  // address-of yields, and the machine boolean a conditional branch tests. Each
  // is minted once and reused.
  auto BorrowedPointerTo(lir::TypeId pointee) -> lir::TypeId;
  auto MachineBoolType() -> lir::TypeId;
  auto VoidType() -> lir::TypeId;

  // The LIR function a class's callable lowers to. Throws if `callable` has no
  // body in `owner` -- a DPI-C import is reached as a foreign symbol and a pure
  // virtual has no implementation here, so neither is a function of this unit.
  [[nodiscard]] auto MethodFunction(
      mir::ClassId owner, mir::CallableId callable) const -> lir::FunctionId;

  // The LIR function a closure's invoke lowers to.
  [[nodiscard]] auto ClosureFunction(mir::ClosureId closure) const
      -> lir::FunctionId;

 private:
  // The function identities of one class, in the order the unit lowers them:
  // the constructor, then each callable that has a body. A callable with no
  // body is not a function of this unit and holds none.
  struct ClassFunctions {
    lir::FunctionId constructor{};
    std::vector<std::optional<lir::FunctionId>> methods;
  };

  // Assigns every function identity the unit will hold, before any body is
  // lowered, because a body may call a function whose own body is lowered later
  // -- including itself. Assignment walks in the same order the lowering
  // appends, so an append always lands on the identity planned for it.
  void PlanFunctions();

  auto TranslateTypeData(const mir::Type& ty) -> lir::TypeData;
  // Records `what` (a human phrase like "a closure") as the unit's first
  // unmirrored-type error and returns a benign placeholder type; the unit fails
  // at `Run` before the placeholder is observed.
  auto RecordUnsupportedType(std::string_view what) -> lir::TypeData;
  auto LowerClass(mir::ClassId owner, const mir::Class& cls)
      -> diag::Result<lir::Class>;
  static auto LowerBase(const mir::ClassRef& base) -> lir::Base;

  const mir::CompilationUnit* mir_;
  lir::CompilationUnit out_;
  std::unordered_map<mir::TypeId, lir::TypeId> type_memo_;
  std::unordered_map<lir::TypeId, lir::TypeId> pointer_memo_;
  std::vector<ClassFunctions> class_functions_;
  std::vector<lir::FunctionId> closure_functions_;
  std::optional<lir::TypeId> machine_bool_type_;
  std::optional<lir::TypeId> void_type_;
  // Set the first time a MIR type with no LIR mirror is reached; surfaced as
  // the unit's failure at `Run`, so translation stays non-throwing and
  // total-shaped while an unmirrored type is still a clean diagnostic, not a
  // mistranslation.
  std::optional<diag::Diagnostic> type_error_;
};

}  // namespace lyra::lowering::mir_to_lir
