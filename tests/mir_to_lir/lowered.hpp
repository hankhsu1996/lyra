#pragma once

#include <expected>
#include <optional>
#include <string>
#include <utility>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lowering/mir_to_lir/unit_lowerer.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/phase.hpp"

namespace lyra::lowering::mir_to_lir {

// `unit` lowered with no target asking anything of it, which is all a test of
// this pass reads.
inline auto Lowered(const mir::CompilationUnit& unit)
    -> diag::Result<lir::CompilationUnit> {
  auto lowerer = UnitLowerer::LowerUnit(unit);
  if (!lowerer) {
    return std::unexpected(std::move(lowerer.error()));
  }
  lowerer->StateTypeSteps();
  return std::move(*lowerer).Finish();
}

// A body of `unit`'s namespace that does nothing.
inline auto AddEmptyBody(mir::CompilationUnit& unit) -> mir::CallableId {
  mir::CallableCode code = mir::CallableCode::Defined(diag::SourceSpan{});
  code.result_type = unit.builtins.void_type;
  return unit.AddNamespaceCallable(std::move(code));
}

// A legalized namespace unit `U` that brings itself up doing nothing, which a
// test gives the body it reads.
inline auto NamespaceUnit() -> mir::CompilationUnit {
  mir::CompilationUnit unit;
  unit.name = "U";
  unit.phase = mir::Phase::kLegalized;
  unit.body_name = "U";
  unit.content = mir::BroughtUpNamespace{
      .install_storage = AddEmptyBody(unit),
      .initialize_storage = AddEmptyBody(unit),
      .scope_declarations = std::nullopt};
  return unit;
}

// `code` as the body of `unit`'s namespace named `name`.
inline void AddNamedBody(
    mir::CompilationUnit& unit, std::string name, mir::CallableCode code) {
  const mir::CallableId id = unit.AddNamespaceCallable(std::move(code));
  unit.named_callables.push_back(
      mir::NamedCallable{.name = std::move(name), .body = id});
}

}  // namespace lyra::lowering::mir_to_lir
