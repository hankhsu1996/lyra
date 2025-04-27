#include "lowering/ast_to_mir/module.hpp"

#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <spdlog/spdlog.h>

#include "lowering/ast_to_mir/process.hpp"
#include "lowering/ast_to_mir/variable.hpp"
#include "mir/module.hpp"

namespace lyra::lowering {

auto LowerModule(const slang::ast::InstanceSymbol& instance_symbol)
    -> std::shared_ptr<mir::Module> {
  auto module = std::make_shared<mir::Module>();
  module->name = std::string(instance_symbol.name);

  const auto& body = instance_symbol.body;

  for (const auto& symbol : body.members()) {
    // First attempt to lower as a variable
    if (symbol.kind == slang::ast::SymbolKind::Variable) {
      if (auto variable = LowerVariable(symbol)) {
        module->variables.push_back(std::move(*variable));
        continue;
      }
    }

    // Process procedural blocks (initial, always, etc.)
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
