#include "lyra/lowering/ast_to_mir/module.hpp"

#include <cstddef>
#include <memory>
#include <unordered_set>
#include <utility>
#include <vector>

#include <fmt/format.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/PortSymbols.h>
#include <spdlog/spdlog.h>

#include "lyra/common/diagnostic.hpp"
#include "lyra/common/literal.hpp"
#include "lyra/common/timescale.hpp"
#include "lyra/common/trigger.hpp"
#include "lyra/common/variable.hpp"
#include "lyra/lowering/ast_to_mir/collect_sensitivity.hpp"
#include "lyra/lowering/ast_to_mir/expression.hpp"
#include "lyra/lowering/ast_to_mir/process.hpp"
#include "lyra/lowering/ast_to_mir/type.hpp"
#include "lyra/mir/module.hpp"
#include "lyra/mir/process.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::lowering::ast_to_mir {

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

// Creates an always_comb-like process for a driver statement.
// Pattern: while (true) { driver_stmt; @(sensitivity_list); }
// Used for implicit port driving: Child c(.a(x + y)) generates c.a = x + y
auto CreateImplicitAlwaysComb(
    std::unique_ptr<mir::Statement> driver_stmt, std::size_t& process_counter)
    -> std::unique_ptr<mir::Process> {
  auto process = std::make_unique<mir::Process>();
  process->name = fmt::format("_port_driver_{}", process_counter++);

  // Collect sensitivity list from the driver statement
  auto variables = CollectSensitivityList(*driver_stmt);

  std::vector<common::Trigger> triggers;
  triggers.reserve(variables.size());
  for (const auto& variable : variables) {
    triggers.emplace_back(common::Trigger::AnyChange(variable));
  }

  // Build loop: driver first, then wait for triggers
  // This achieves always_comb semantics:
  // - Body executes at time 0 (first iteration before wait)
  // - Body re-executes on any input change (after wait)
  auto loop_block = std::make_unique<mir::BlockStatement>();
  loop_block->statements.push_back(std::move(driver_stmt));
  loop_block->statements.push_back(
      std::make_unique<mir::WaitEventStatement>(std::move(triggers)));

  // while (true) { driver; wait_event; }
  auto condition =
      std::make_unique<mir::LiteralExpression>(common::Literal::Bool(true));
  process->body = std::make_unique<mir::WhileStatement>(
      std::move(condition), std::move(loop_block));

  return process;
}

}  // namespace

auto LowerModule(const slang::ast::InstanceSymbol& instance_symbol)
    -> std::unique_ptr<mir::Module> {
  auto module = std::make_unique<mir::Module>();
  // Use module type name (definition name), not instance name
  module->name = std::string(instance_symbol.body.name);

  // Extract timescale from the instance body
  const auto& body = instance_symbol.body;
  if (auto ts = body.getTimeScale()) {
    module->timescale = common::TimeScale::FromSlang(*ts);
  }

  // Track port internal symbols to exclude from variables list
  std::unordered_set<const slang::ast::Symbol*> port_symbols;

  // Counter for generating unique port driver process names within this module
  std::size_t port_driver_counter = 0;

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

    // Lower submodule instances
    if (symbol.kind == slang::ast::SymbolKind::Instance) {
      const auto& child = symbol.as<slang::ast::InstanceSymbol>();

      mir::SubmoduleInstance submod;
      submod.instance_name = std::string(child.name);
      submod.module_type = std::string(child.getDefinition().name);

      for (const auto* conn : child.getPortConnections()) {
        const auto* expr = conn->getExpression();
        // Skip empty/implicit port connections
        if (expr == nullptr ||
            expr->kind == slang::ast::ExpressionKind::EmptyArgument) {
          continue;
        }

        mir::PortConnection port_conn;
        port_conn.port_name = std::string(conn->port.name);
        const auto& port = conn->port.as<slang::ast::PortSymbol>();
        port_conn.direction = MapPortDirection(port.direction);

        // For output ports, slang wraps the connection in an Assignment
        // expression (external = internal). Extract just the LHS signal.
        if (expr->kind == slang::ast::ExpressionKind::Assignment) {
          const auto& assignment = expr->as<slang::ast::AssignmentExpression>();
          port_conn.signal = LowerExpression(assignment.left());
          submod.connections.push_back(std::move(port_conn));
        } else {
          // Input port connection (simple `.a(x)` or expression `.a(x + y)`)
          // Create driver process using hierarchical assignment.
          // child.port = expr becomes AssignStatement with hierarchical target.
          //
          // Design: Input ports are value members in child, parent drives them.
          // - Codegen: Driver process writes to child_.port
          // - Interpreter (simple only): Uses binding model for optimization
          auto signal_expr = LowerExpression(*expr);

          // Create port driver process using hierarchical assignment
          mir::AssignmentTarget target(
              {submod.instance_name, port_conn.port_name});
          auto driver_stmt = std::make_unique<mir::AssignStatement>(
              std::move(target), std::move(signal_expr));
          auto process = CreateImplicitAlwaysComb(
              std::move(driver_stmt), port_driver_counter);
          module->processes.push_back(std::move(process));

          // For simple ports, also store binding for interpreter optimization
          // Interpreter uses port binding directly (more efficient than
          // process)
          if (expr->kind == slang::ast::ExpressionKind::NamedValue) {
            port_conn.signal = LowerExpression(*expr);
            submod.connections.push_back(std::move(port_conn));
          }
          // For expression ports, no binding - driver process handles it
        }
      }

      module->submodules.push_back(std::move(submod));
      continue;
    }
  }

  return module;
}

}  // namespace lyra::lowering::ast_to_mir
