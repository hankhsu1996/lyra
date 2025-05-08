#include "lyra/lowering/ast_to_mir/module.hpp"

#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <spdlog/spdlog.h>

#include "lyra/lowering/ast_to_mir/process.hpp"
#include "lyra/mir/module.hpp"

namespace lyra::lowering {

auto LowerModule(const slang::ast::InstanceSymbol& instance_symbol)
    -> std::unique_ptr<mir::Module> {
  auto module = std::make_unique<mir::Module>();
  module->name = std::string(instance_symbol.name);

  const auto& body = instance_symbol.body;

  for (const auto& symbol : body.members()) {
    // Lower variables
    if (symbol.kind == slang::ast::SymbolKind::Variable) {
      const auto& variable_symbol = symbol.as<slang::ast::VariableSymbol>();
      auto variable = common::Variable::FromSlang(variable_symbol);
      module->variables.push_back(std::move(variable));
      continue;
    }

    // Lower procedural blocks (initial, always, etc.)
    if (symbol.kind == slang::ast::SymbolKind::ProceduralBlock) {
      const auto& procedural_block =
          symbol.as<slang::ast::ProceduralBlockSymbol>();
      auto process = LowerProcess(procedural_block);
      module->processes.push_back(std::move(process));
      continue;
    }
  }

  return module;
}

}  // namespace lyra::lowering
