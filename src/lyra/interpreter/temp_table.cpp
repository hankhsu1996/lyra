#include "lyra/interpreter/temp_table.hpp"

#include <fmt/core.h>

#include "lyra/common/diagnostic.hpp"
#include "lyra/interpreter/runtime_value.hpp"
#include "lyra/lir/context.hpp"

namespace lyra::interpreter {

void TempTable::Write(const lir::TempRef& temp, const RuntimeValue& value) {
  registers_[temp] = value;
}

auto TempTable::Read(const lir::TempRef& temp) const -> RuntimeValue {
  auto it = registers_.find(temp);
  if (it == registers_.end()) {
    throw DiagnosticException(
        Diagnostic::Error(
            {}, fmt::format("cannot read from temp table: {}", temp->name)));
  }
  return it->second;
}

}  // namespace lyra::interpreter
