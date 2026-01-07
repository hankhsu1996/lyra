#include "lyra/lowering/ast_to_mir/module.hpp"

#include <memory>
#include <utility>

#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <spdlog/spdlog.h>

#include "lyra/common/diagnostic.hpp"
#include "lyra/common/timescale.hpp"
#include "lyra/common/variable.hpp"
#include "lyra/lowering/ast_to_mir/expression.hpp"
#include "lyra/lowering/ast_to_mir/process.hpp"
#include "lyra/lowering/ast_to_mir/type.hpp"
#include "lyra/mir/module.hpp"

namespace lyra::lowering::ast_to_mir {

using SymbolRef = slang::ast::Symbol*;

auto LowerModule(const slang::ast::InstanceSymbol& instance_symbol)
    -> std::unique_ptr<mir::Module> {
  auto module = std::make_unique<mir::Module>();
  module->name = std::string(instance_symbol.name);

  // Extract timescale from the instance body
  const auto& body = instance_symbol.body;
  if (auto ts = body.getTimeScale()) {
    module->timescale = common::TimeScale::FromSlang(*ts);
  }

  for (const auto& symbol : body.members()) {
    // Lower variables
    if (symbol.kind == slang::ast::SymbolKind::Variable) {
      const auto& variable_symbol = symbol.as<slang::ast::VariableSymbol>();

      // Create source range from symbol location
      slang::SourceRange source_range(
          variable_symbol.location, variable_symbol.location);

      auto type_result = LowerType(variable_symbol.getType(), source_range);
      if (!type_result) {
        throw DiagnosticException(std::move(type_result.error()));
      }

      common::Variable variable{
          .symbol = &variable_symbol,
          .type = *type_result,
      };

      // Extract initializer if present
      std::unique_ptr<mir::Expression> init_expr = nullptr;
      if (const auto* initializer = variable_symbol.getInitializer()) {
        init_expr = LowerExpression(*initializer);
      }

      module->variables.push_back(
          mir::ModuleVariable{
              .variable = std::move(variable),
              .initializer = std::move(init_expr)});
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
