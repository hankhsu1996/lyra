#include "lyra/lowering/ast_to_mir/variable.hpp"

#include <optional>

#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/Type.h>
#include <spdlog/spdlog.h>

#include "lyra/common/variable.hpp"

namespace lyra::lowering {

auto LowerVariable(const slang::ast::Symbol& symbol)
    -> std::optional<common::Variable> {
  using SymbolKind = slang::ast::SymbolKind;

  if (symbol.kind != SymbolKind::Variable) {
    return std::nullopt;
  }

  const auto& variable_symbol = symbol.as<slang::ast::VariableSymbol>();

  return common::Variable::FromSlang(variable_symbol);
}

}  // namespace lyra::lowering
