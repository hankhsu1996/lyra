#include "lyra/lowering/ast_to_mir/module.hpp"

#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <spdlog/spdlog.h>

#include "lyra/lowering/ast_to_mir/expression.hpp"
#include "lyra/lowering/ast_to_mir/process.hpp"
#include "lyra/mir/module.hpp"

namespace lyra::lowering::ast_to_mir {

using SymbolRef = slang::ast::Symbol*;

auto LowerModule(const slang::ast::InstanceSymbol& instance_symbol)
    -> std::unique_ptr<mir::Module> {
  auto module = std::make_unique<mir::Module>();
  module->name = std::string(instance_symbol.name);

  const auto& body = instance_symbol.body;

  for (const auto& symbol : body.members()) {
    // Lower variables
    if (symbol.kind == slang::ast::SymbolKind::Variable) {
      const auto& variable_symbol = symbol.as<slang::ast::VariableSymbol>();
      auto variable = common::Variable::FromSlang(&variable_symbol);

      // Extract initializer if present
      std::unique_ptr<mir::Expression> init_expr = nullptr;
      if (const auto* initializer = variable_symbol.getInitializer()) {
        init_expr = LowerExpression(*initializer);
      }

      module->variables.push_back(
          mir::ModuleVariable{std::move(variable), std::move(init_expr)});
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

}  // namespace lyra::lowering::ast_to_mir
