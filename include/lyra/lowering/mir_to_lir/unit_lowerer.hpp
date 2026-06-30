#pragma once

#include <cstdint>
#include <unordered_map>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/lir/class_id.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_ref.hpp"
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
  // Total over the MIR type universe -- the translation is a closed mapping, so
  // it cannot fail; a MIR type added upstream is a compile error here until it
  // is mirrored.
  auto TranslateType(mir::TypeId id) -> lir::TypeId;

 private:
  auto TranslateTypeData(const mir::Type& ty) -> lir::TypeData;
  auto LowerClass(lir::ClassId class_id, const mir::Class& cls)
      -> diag::Result<lir::Class>;
  auto LowerBase(const mir::ClassRef& base) -> lir::Base;

  const mir::CompilationUnit* mir_;
  lir::CompilationUnit out_;
  std::unordered_map<std::uint32_t, lir::TypeId> type_memo_;
};

}  // namespace lyra::lowering::mir_to_lir
