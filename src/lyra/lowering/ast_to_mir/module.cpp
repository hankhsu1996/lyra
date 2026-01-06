#include "lyra/lowering/ast_to_mir/module.hpp"

#include <memory>
#include <unordered_set>
#include <utility>

#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/PortSymbols.h>
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

namespace {

auto MapPortDirection(slang::ast::ArgumentDirection direction)
    -> mir::PortDirection {
  switch (direction) {
    case slang::ast::ArgumentDirection::In:
      return mir::PortDirection::kInput;
    case slang::ast::ArgumentDirection::Out:
      return mir::PortDirection::kOutput;
    case slang::ast::ArgumentDirection::InOut:
    case slang::ast::ArgumentDirection::Ref:
      return mir::PortDirection::kInout;
  }
  // Should not reach here
  return mir::PortDirection::kInout;
}

}  // namespace

auto LowerModule(const slang::ast::InstanceSymbol& instance_symbol)
    -> std::unique_ptr<mir::Module> {
  auto module = std::make_unique<mir::Module>();
  module->name = std::string(instance_symbol.name);

  // Extract timescale from the instance body
  const auto& body = instance_symbol.body;
  if (auto ts = body.getTimeScale()) {
    module->timescale = common::TimeScale::FromSlang(*ts);
  }

  // Track port internal symbols to exclude from variables list
  std::unordered_set<const slang::ast::Symbol*> port_symbols;

  // Extract ports from the module definition
  for (const auto* port_symbol : body.getPortList()) {
    // Skip non-port symbols (e.g., multi-ports, interface ports)
    if (port_symbol->kind != slang::ast::SymbolKind::Port) {
      continue;
    }

    const auto& port = port_symbol->as<slang::ast::PortSymbol>();

    // Use the internal symbol for the variable (it's the actual signal inside)
    const auto* internal = port.internalSymbol;
    if (internal == nullptr) {
      // Null ports (not connected to internal signal) - skip for now
      continue;
    }

    // Get type from the port
    const auto& port_type = port.getType();
    slang::SourceRange source_range(port.location, port.location);

    auto type_result = LowerType(port_type, source_range);
    if (!type_result) {
      throw DiagnosticException(std::move(type_result.error()));
    }

    // Use the internal symbol as the variable symbol
    common::Variable variable{
        .symbol = internal,
        .type = *type_result,
    };

    module->ports.push_back(
        mir::Port{
            .variable = std::move(variable),
            .direction = MapPortDirection(port.direction),
        });

    // Track this symbol to exclude from variables
    port_symbols.insert(internal);
  }

  for (const auto& symbol : body.members()) {
    // Lower variables (skip port variables)
    if (symbol.kind == slang::ast::SymbolKind::Variable) {
      // Skip if this variable is a port
      if (port_symbols.contains(&symbol)) {
        continue;
      }

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
