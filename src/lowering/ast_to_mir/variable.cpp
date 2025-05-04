#include "lowering/ast_to_mir/variable.hpp"

#include <optional>

#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/Type.h>
#include <spdlog/spdlog.h>

#include "common/type.hpp"
#include "common/variable.hpp"

namespace lyra::lowering {

auto LowerVariable(const slang::ast::Symbol& symbol)
    -> std::optional<common::Variable> {
  using SymbolKind = slang::ast::SymbolKind;

  if (symbol.kind != SymbolKind::Variable) {
    return std::nullopt;
  }

  const auto& variable_symbol = symbol.as<slang::ast::VariableSymbol>();

  common::Variable variable;
  variable.name = std::string(variable_symbol.name);

  const auto& slang_type = variable_symbol.getType();
  if (slang_type.isString()) {
    variable.type = common::Type::String();
  } else if (slang_type.isIntegral()) {
    if (slang_type.isSigned()) {
      variable.type = common::Type::TwoState(slang_type.getBitWidth(), true);
    } else {
      variable.type = common::Type::TwoState(slang_type.getBitWidth(), false);
    }
  } else {
    throw std::runtime_error(
        fmt::format("Unsupported type: {}", slang_type.toString()));
  }

  return variable;
}

}  // namespace lyra::lowering
