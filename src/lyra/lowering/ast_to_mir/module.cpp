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
#include <slang/ast/symbols/MemberSymbols.h>
#include <slang/ast/symbols/PortSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/types/AllTypes.h>
#include <spdlog/spdlog.h>

#include "lyra/common/diagnostic.hpp"
#include "lyra/common/literal.hpp"
#include "lyra/common/timescale.hpp"
#include "lyra/common/trigger.hpp"
#include "lyra/common/variable.hpp"
#include "lyra/lowering/ast_to_mir/collect_sensitivity.hpp"
#include "lyra/lowering/ast_to_mir/expression.hpp"
#include "lyra/lowering/ast_to_mir/function.hpp"
#include "lyra/lowering/ast_to_mir/process.hpp"
#include "lyra/lowering/ast_to_mir/type.hpp"
#include "lyra/mir/expression.hpp"
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
  auto sensitivity_items = CollectSensitivityList(*driver_stmt);

  std::vector<common::Trigger> triggers;
  triggers.reserve(sensitivity_items.size());
  for (const auto& item : sensitivity_items) {
    triggers.emplace_back(
        common::Trigger::AnyChange(item.symbol, item.instance_path));
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

  // Track port internal symbols to exclude from variables list.
  // Port symbols appear BEFORE their internal Variable in body.members(),
  // so we can build this set as we go and check it when handling Variable.
  std::unordered_set<const slang::ast::Symbol*> port_symbols;

  // Counter for generating unique port driver process names within this module
  std::size_t port_driver_counter = 0;

  for (const auto& symbol : body.members()) {
    using SK = slang::ast::SymbolKind;

    switch (symbol.kind) {
      case SK::Port: {
        const auto& port = symbol.as<slang::ast::PortSymbol>();

        // Null ports (not connected to internal signal) - skip for now
        const auto* internal = port.internalSymbol;
        if (internal == nullptr) {
          break;
        }

        slang::SourceRange source_range(port.location, port.location);
        auto type_result = LowerType(port.getType(), source_range);
        if (!type_result) {
          throw DiagnosticException(std::move(type_result.error()));
        }

        common::Variable variable{
            .symbol = internal,
            .type = *type_result,
        };

        module->ports.push_back(
            mir::Port{
                .variable = std::move(variable),
                .direction = MapPortDirection(port.direction),
            });

        // Track so Variable case knows to skip this internal symbol
        port_symbols.insert(internal);
        break;
      }

      case SK::Variable: {
        // Skip if this is a port's internal symbol (already handled above)
        if (port_symbols.contains(&symbol)) {
          break;
        }

        const auto& variable_symbol = symbol.as<slang::ast::VariableSymbol>();

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

        std::unique_ptr<mir::Expression> init_expr = nullptr;
        if (const auto* initializer = variable_symbol.getInitializer()) {
          init_expr = LowerExpression(*initializer);
        }

        module->variables.push_back(
            mir::ModuleVariable{
                .variable = std::move(variable),
                .initializer = std::move(init_expr)});
        break;
      }

      case SK::ProceduralBlock: {
        const auto& procedural_block =
            symbol.as<slang::ast::ProceduralBlockSymbol>();
        auto process = LowerProcess(procedural_block);
        module->processes.push_back(std::move(process));
        break;
      }

      case SK::Subroutine: {
        const auto& subroutine = symbol.as<slang::ast::SubroutineSymbol>();

        if (subroutine.subroutineKind == slang::ast::SubroutineKind::Task) {
          throw DiagnosticException(
              Diagnostic::Error(
                  slang::SourceRange(subroutine.location, subroutine.location),
                  fmt::format(
                      "task '{}' is not yet supported", subroutine.name)));
        }

        auto func = LowerFunction(subroutine);
        module->functions.push_back(std::move(func));
        break;
      }

      case SK::Instance: {
        const auto& child = symbol.as<slang::ast::InstanceSymbol>();

        mir::SubmoduleInstance submod;
        submod.instance_symbol = &child;
        submod.instance_name = std::string(child.name);
        submod.module_type = std::string(child.getDefinition().name);

        for (const auto* conn : child.getPortConnections()) {
          const auto* expr = conn->getExpression();
          if (expr == nullptr ||
              expr->kind == slang::ast::ExpressionKind::EmptyArgument) {
            continue;
          }

          if (expr->kind == slang::ast::ExpressionKind::Assignment) {
            const auto& assignment =
                expr->as<slang::ast::AssignmentExpression>();
            mir::OutputBinding binding;
            binding.port_name = std::string(conn->port.name);
            binding.signal = LowerExpression(assignment.left());
            submod.output_bindings.push_back(std::move(binding));
          } else {
            auto signal_expr = LowerExpression(*expr);
            const auto& port_sym = conn->port.as<slang::ast::PortSymbol>();
            mir::AssignmentTarget target(
                port_sym.internalSymbol, {submod.instance_symbol});
            auto driver_stmt = std::make_unique<mir::AssignStatement>(
                std::move(target), std::move(signal_expr));
            auto process = CreateImplicitAlwaysComb(
                std::move(driver_stmt), port_driver_counter);
            module->processes.push_back(std::move(process));
          }
        }

        module->submodules.push_back(std::move(submod));
        break;
      }

      case SK::WildcardImport: {
        const auto& import = symbol.as<slang::ast::WildcardImportSymbol>();
        module->wildcard_imports.emplace_back(import.packageName);
        break;
      }

      case SK::ExplicitImport: {
        const auto& import = symbol.as<slang::ast::ExplicitImportSymbol>();
        module->explicit_imports.emplace_back(
            std::string(import.packageName), std::string(import.importName));
        break;
      }

      // Compile-time resolved - no runtime representation needed
      case SK::TypeAlias:
        // Type aliases are unwrapped to canonical types by LowerType()
        // Example: typedef logic [7:0] byte_t; → byte_t becomes logic[7:0]
        break;

      case SK::Parameter:
        // Compile-time constants - slang inlines values at use sites
        // Example: parameter int WIDTH = 8; → WIDTH becomes literal 8
        break;

      // Scoped symbols - slang exposes for name resolution, handled in blocks
      case SK::StatementBlock:
        // Contains local variables in begin...end blocks
        // These are lowered when processing the ProceduralBlock body
        break;

      case SK::TransparentMember:
        // Provides access to enum members at module scope
        // Example: enum {A, B} makes A, B visible - resolved at reference
        break;

      // Unsupported features - explicit errors
      case SK::ContinuousAssign:
        throw DiagnosticException(
            Diagnostic::Error(
                slang::SourceRange{symbol.location, symbol.location},
                "continuous assignments (assign) are not yet supported"));

      case SK::GenerateBlock:
      case SK::GenerateBlockArray:
        throw DiagnosticException(
            Diagnostic::Error(
                slang::SourceRange{symbol.location, symbol.location},
                "generate blocks are not yet supported"));

      // Unknown - force investigation
      default:
        throw DiagnosticException(
            Diagnostic::Error(
                slang::SourceRange{symbol.location, symbol.location},
                fmt::format(
                    "unhandled module member kind: {}",
                    toString(symbol.kind))));
    }
  }

  return module;
}

}  // namespace lyra::lowering::ast_to_mir
