#include "lowering/ast_to_mir/variable.hpp"

#include <optional>

#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <spdlog/spdlog.h>

#include "mir/variable.hpp"

namespace lyra::lowering {

auto LowerVariable(const slang::ast::Symbol& symbol)
    -> std::optional<mir::Variable> {
  using SymbolKind = slang::ast::SymbolKind;

  if (symbol.kind != SymbolKind::Variable) {
    return std::nullopt;
  }

  const auto& variable_symbol = symbol.as<slang::ast::VariableSymbol>();

  mir::Variable variable;
  variable.name = std::string(variable_symbol.name);

  // Temporary: always treat as int type
  variable.type = mir::Type::kInt;

  return variable;
}

}  // namespace lyra::lowering
