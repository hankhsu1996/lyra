#include "lyra/lowering/ast_to_mir/module.hpp"

#include <cstddef>
#include <memory>
#include <optional>
#include <span>
#include <string>
#include <unordered_set>
#include <utility>
#include <vector>

#include <fmt/format.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/MemberSymbols.h>
#include <slang/ast/symbols/ParameterSymbols.h>
#include <slang/ast/symbols/PortSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/types/AllTypes.h>
#include <spdlog/spdlog.h>

#include "lyra/common/diagnostic.hpp"
#include "lyra/common/literal.hpp"
#include "lyra/common/symbol.hpp"
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

auto ComputeModuleSignature(const slang::ast::InstanceSymbol& instance)
    -> std::string {
  std::string sig(instance.body.name);

  // Append parameter values for parameterized modules
  bool first_param = true;
  for (const auto* param_base : instance.body.getParameters()) {
    if (!param_base->isPortParam()) {
      continue;
    }
    const auto& sym = param_base->symbol;
    if (sym.kind != slang::ast::SymbolKind::Parameter) {
      continue;
    }
    const auto& param = sym.as<slang::ast::ParameterSymbol>();

    // Build signature like "Counter<8>" or "Counter<8,16>"
    const auto& value = param.getValue();
    if (first_param) {
      sig += "<";
      first_param = false;
    } else {
      sig += ",";
    }
    sig += value.toString();
  }
  if (!first_param) {
    sig += ">";
  }

  return sig;
}

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

// Context for module lowering - groups parameters threaded through all
// functions.
struct ModuleLoweringContext {
  mir::Module* module;
  std::unordered_set<const slang::ast::Symbol*>* port_symbols;
  std::size_t* port_driver_counter;
  std::size_t* cont_assign_counter;
  ProcessCounters* process_counters;
};

// Forward declaration for mutual recursion with ProcessModuleMember
void ProcessModuleMember(
    const slang::ast::Symbol& symbol, ModuleLoweringContext& ctx);

// Process non-structural members (processes, functions) from all generate array
// entries. Variables and nested generates are handled by LowerGenerateScope.
void ProcessGenerateArrayMembers(
    std::span<const slang::ast::GenerateBlockSymbol* const> entries,
    ModuleLoweringContext& ctx);

// Lower a generate block scope to MIR, collecting variables and nested scopes.
// Processes (initial/always blocks) are added to the module, not returned here.
auto LowerGenerateScope(
    const slang::ast::Scope& scope, const std::string& name,
    common::SymbolRef symbol, std::optional<size_t> array_size,
    ModuleLoweringContext& ctx) -> mir::GenerateScope {
  using SK = slang::ast::SymbolKind;

  mir::GenerateScope result;
  result.name = name;
  result.symbol = symbol;
  result.array_size = array_size;

  for (const auto& member : scope.members()) {
    switch (member.kind) {
      case SK::Variable: {
        const auto& var = member.as<slang::ast::VariableSymbol>();
        slang::SourceRange source_range(var.location, var.location);
        auto type_result = LowerType(var.getType(), source_range);
        if (!type_result) {
          throw DiagnosticException(std::move(type_result.error()));
        }
        mir::ModuleVariable mod_var{
            .variable = common::Variable{.symbol = &var, .type = *type_result},
            .initializer = nullptr};
        if (const auto* init = var.getInitializer()) {
          mod_var.initializer = LowerExpression(*init);
        }
        result.variables.push_back(std::move(mod_var));
        break;
      }

      case SK::GenerateBlock: {
        const auto& nested_sym = member.as<slang::ast::GenerateBlockSymbol>();
        if (nested_sym.isUninstantiated) {
          break;
        }
        if (!nested_sym.name.empty()) {
          // Named nested generate block - add to nested_scopes
          result.nested_scopes.push_back(LowerGenerateScope(
              nested_sym, std::string(nested_sym.name), &nested_sym,
              std::nullopt, ctx));
        } else {
          // Unnamed nested block - flatten contents into this scope
          // Variables go into our variables, nested scopes get merged
          auto inner = LowerGenerateScope(
              nested_sym, "", &nested_sym, std::nullopt, ctx);
          for (auto& var : inner.variables) {
            result.variables.push_back(std::move(var));
          }
          for (auto& nested : inner.nested_scopes) {
            result.nested_scopes.push_back(std::move(nested));
          }
        }
        break;
      }

      case SK::GenerateBlockArray: {
        const auto& nested_array =
            member.as<slang::ast::GenerateBlockArraySymbol>();
        if (!nested_array.entries.empty()) {
          // Use first entry for structure (all entries have same structure)
          const auto* first_entry = nested_array.entries[0];
          result.nested_scopes.push_back(LowerGenerateScope(
              *first_entry, std::string(nested_array.name), &nested_array,
              nested_array.entries.size(), ctx));

          // Process non-variable/non-generate members from ALL entries
          ProcessGenerateArrayMembers(nested_array.entries, ctx);
        }
        break;
      }

      default:
        // Non-generate, non-variable members (processes, functions) go to
        // module
        ProcessModuleMember(member, ctx);
        break;
    }
  }

  return result;
}

// Process non-structural members (processes, functions) from all generate array
// entries. Variables and nested generates are handled by LowerGenerateScope.
void ProcessGenerateArrayMembers(
    std::span<const slang::ast::GenerateBlockSymbol* const> entries,
    ModuleLoweringContext& ctx) {
  using SK = slang::ast::SymbolKind;
  for (const auto* entry : entries) {
    for (const auto& member : entry->members()) {
      if (member.kind != SK::Variable && member.kind != SK::GenerateBlock &&
          member.kind != SK::GenerateBlockArray) {
        ProcessModuleMember(member, ctx);
      }
    }
  }
}

// Process a single module member symbol and add it to the MIR module.
// Called for direct module members and recursively for generate block contents.
void ProcessModuleMember(
    const slang::ast::Symbol& symbol, ModuleLoweringContext& ctx) {
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

      ctx.module->ports.push_back(
          mir::Port{
              .variable = std::move(variable),
              .direction = MapPortDirection(port.direction),
          });

      // Track so Variable case knows to skip this internal symbol
      ctx.port_symbols->insert(internal);
      break;
    }

    case SK::Variable: {
      // Skip if this is a port's internal symbol (already handled above)
      if (ctx.port_symbols->contains(&symbol)) {
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

      ctx.module->variables.push_back(
          mir::ModuleVariable{
              .variable = std::move(variable),
              .initializer = std::move(init_expr)});
      break;
    }

    case SK::ProceduralBlock: {
      const auto& procedural_block =
          symbol.as<slang::ast::ProceduralBlockSymbol>();
      auto process = LowerProcess(procedural_block, *ctx.process_counters);
      ctx.module->processes.push_back(std::move(process));
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
      ctx.module->functions.push_back(std::move(func));
      break;
    }

    case SK::Instance: {
      const auto& child = symbol.as<slang::ast::InstanceSymbol>();

      mir::SubmoduleInstance submod;
      submod.instance_symbol = &child;
      submod.instance_name = std::string(child.name);
      submod.module_type = std::string(child.getDefinition().name);
      submod.module_signature = ComputeModuleSignature(child);

      // Extract parameter overrides (evaluated constant values for now)
      // These become template arguments in C++ codegen: Counter<8>
      for (const auto* param_base : child.body.getParameters()) {
        if (!param_base->isPortParam()) {
          continue;
        }
        const auto& sym = param_base->symbol;
        if (sym.kind != slang::ast::SymbolKind::Parameter) {
          continue;
        }
        const auto& param = sym.as<slang::ast::ParameterSymbol>();
        // For now, store the evaluated constant value as a literal expression
        // Future: preserve original expression for cases like Counter<SIZE*2>
        const auto* init = param.getInitializer();
        if (init == nullptr) {
          continue;  // Skip parameters without initializers
        }
        auto value_expr = LowerExpression(*init);
        submod.parameter_overrides.push_back(
            mir::ParameterOverride{
                .parameter_name = std::string(param.name),
                .value = std::move(value_expr),
            });
      }

      for (const auto* conn : child.getPortConnections()) {
        const auto* expr = conn->getExpression();
        if (expr == nullptr ||
            expr->kind == slang::ast::ExpressionKind::EmptyArgument) {
          continue;
        }

        if (expr->kind == slang::ast::ExpressionKind::Assignment) {
          const auto& assignment = expr->as<slang::ast::AssignmentExpression>();
          mir::OutputBinding binding;
          binding.port_name = std::string(conn->port.name);
          binding.signal = LowerExpression(assignment.left());
          submod.output_bindings.push_back(std::move(binding));
        } else {
          auto signal_expr = LowerExpression(*expr);
          const auto& port_sym = conn->port.as<slang::ast::PortSymbol>();
          mir::AssignmentTarget target(
              port_sym.internalSymbol,
              {mir::HierarchicalPathElement{submod.instance_symbol}});
          auto driver_stmt = std::make_unique<mir::AssignStatement>(
              std::move(target), std::move(signal_expr));
          auto process = CreateImplicitAlwaysComb(
              std::move(driver_stmt), *ctx.port_driver_counter);
          ctx.module->processes.push_back(std::move(process));
        }
      }

      ctx.module->submodules.push_back(std::move(submod));
      break;
    }

    case SK::WildcardImport: {
      const auto& import = symbol.as<slang::ast::WildcardImportSymbol>();
      ctx.module->wildcard_imports.emplace_back(import.packageName);
      break;
    }

    case SK::ExplicitImport: {
      const auto& import = symbol.as<slang::ast::ExplicitImportSymbol>();
      ctx.module->explicit_imports.emplace_back(
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

    case SK::Genvar:
      // Generate loop index - exists only at elaboration time
      // Values are resolved to implicit localparams within generate blocks
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

    case SK::ContinuousAssign: {
      const auto& cont_assign = symbol.as<slang::ast::ContinuousAssignSymbol>();

      // Reject delays (not yet supported)
      if (cont_assign.getDelay() != nullptr) {
        throw DiagnosticException(
            Diagnostic::Error(
                slang::SourceRange{symbol.location, symbol.location},
                "continuous assignment delays are not yet supported"));
      }

      // Get the assignment expression
      const auto& assign_expr = cont_assign.getAssignment();

      // Must be an assignment expression
      if (assign_expr.kind != slang::ast::ExpressionKind::Assignment) {
        throw DiagnosticException(
            Diagnostic::Error(
                slang::SourceRange{symbol.location, symbol.location},
                "unexpected expression in continuous assignment"));
      }

      const auto& slang_assign =
          assign_expr.as<slang::ast::AssignmentExpression>();

      // Lower LHS to get target (MVP: simple variables only)
      const auto& left = slang_assign.left();
      if (left.kind != slang::ast::ExpressionKind::NamedValue) {
        throw DiagnosticException(
            Diagnostic::Error(
                left.sourceRange,
                "only simple variable targets supported in continuous "
                "assignments"));
      }
      const auto& target_sym =
          left.as<slang::ast::NamedValueExpression>().symbol;
      mir::AssignmentTarget target(&target_sym);

      // Lower RHS expression
      auto value = LowerExpression(slang_assign.right());

      // Create driver statement
      auto driver_stmt = std::make_unique<mir::AssignStatement>(
          std::move(target), std::move(value));

      // Create always_comb-like process
      auto process = CreateImplicitAlwaysComb(
          std::move(driver_stmt), *ctx.cont_assign_counter);
      process->name =
          fmt::format("_cont_assign_{}", (*ctx.cont_assign_counter)++);

      ctx.module->processes.push_back(std::move(process));
      break;
    }

    case SK::GenerateBlock: {
      const auto& gen_block_sym = symbol.as<slang::ast::GenerateBlockSymbol>();
      // Skip uninstantiated blocks (slang marks these when condition is false)
      if (gen_block_sym.isUninstantiated) {
        break;
      }

      // Named generate blocks (if-generate/case-generate) create a scope
      // that can be referenced hierarchically (e.g., enabled.value)
      if (!gen_block_sym.name.empty()) {
        ctx.module->generate_scopes.push_back(LowerGenerateScope(
            gen_block_sym, std::string(gen_block_sym.name), &gen_block_sym,
            std::nullopt, ctx));
      } else {
        // Unnamed generate block - flatten into module
        // Use LowerGenerateScope to process nested structures, then merge
        auto scope = LowerGenerateScope(
            gen_block_sym, "", &gen_block_sym, std::nullopt, ctx);
        // Flatten variables into module (not typical, but handle gracefully)
        for (auto& var : scope.variables) {
          ctx.module->variables.push_back(std::move(var));
        }
        // Merge nested scopes into module's generate_scopes
        for (auto& nested : scope.nested_scopes) {
          ctx.module->generate_scopes.push_back(std::move(nested));
        }
      }
      break;
    }

    case SK::GenerateBlockArray: {
      const auto& gen_array = symbol.as<slang::ast::GenerateBlockArraySymbol>();

      if (!gen_array.entries.empty()) {
        // Use first entry for structure (all entries have same structure)
        const auto* first_entry = gen_array.entries[0];
        ctx.module->generate_scopes.push_back(LowerGenerateScope(
            *first_entry, std::string(gen_array.name), &gen_array,
            gen_array.entries.size(), ctx));

        // Process non-variable/non-generate members from ALL entries
        ProcessGenerateArrayMembers(gen_array.entries, ctx);
      }
      break;
    }

    // Unknown - force investigation
    default:
      throw DiagnosticException(
          Diagnostic::Error(
              slang::SourceRange{symbol.location, symbol.location},
              fmt::format(
                  "unhandled module member kind: {}", toString(symbol.kind))));
  }
}

}  // namespace

auto LowerModule(const slang::ast::InstanceSymbol& instance_symbol)
    -> std::unique_ptr<mir::Module> {
  auto module = std::make_unique<mir::Module>();
  // Use module type name (definition name), not instance name
  module->name = std::string(instance_symbol.body.name);

  // Compute signature for linking (e.g., "Counter<8>" for parameterized
  // modules)
  module->signature = ComputeModuleSignature(instance_symbol);
  module->instance_symbol = &instance_symbol;
  const auto& body = instance_symbol.body;

  // Extract timescale from the instance body
  if (auto ts = body.getTimeScale()) {
    module->timescale = common::TimeScale::FromSlang(*ts);
  }

  // Extract module parameters (for C++ template generation)
  // Each unique body has fully-evaluated parameter values
  for (const auto* param_base : body.getParameters()) {
    // Only include port parameters (template params), not body localparams
    if (!param_base->isPortParam()) {
      continue;
    }

    // Check if this is a value parameter (not a type parameter)
    const auto& sym = param_base->symbol;
    if (sym.kind != slang::ast::SymbolKind::Parameter) {
      continue;  // Skip type parameters for now
    }

    const auto& param = sym.as<slang::ast::ParameterSymbol>();
    slang::SourceRange source_range(param.location, param.location);
    auto type_result = LowerType(param.getType(), source_range);
    if (!type_result) {
      throw DiagnosticException(std::move(type_result.error()));
    }

    // Get the evaluated constant value as the "default"
    // For body-based dedup, this is the actual value for this specialization
    std::unique_ptr<mir::Expression> default_expr;
    if (const auto* init = param.getInitializer()) {
      default_expr = LowerExpression(*init);
    }

    module->parameters.push_back(
        mir::ModuleParameter{
            .name = std::string(param.name),
            .type = *type_result,
            .default_value = std::move(default_expr),
            .symbol = &param,
        });
  }

  // Track port internal symbols to exclude from variables list.
  // Port symbols appear BEFORE their internal Variable in body.members(),
  // so we can build this set as we go and check it when handling Variable.
  std::unordered_set<const slang::ast::Symbol*> port_symbols;

  // Counter for generating unique port driver process names within this module
  std::size_t port_driver_counter = 0;

  // Counter for generating unique continuous assignment process names
  std::size_t cont_assign_counter = 0;

  // Counters for generating unique process names (initial, always, etc.)
  ProcessCounters process_counters;

  ModuleLoweringContext ctx{
      .module = module.get(),
      .port_symbols = &port_symbols,
      .port_driver_counter = &port_driver_counter,
      .cont_assign_counter = &cont_assign_counter,
      .process_counters = &process_counters,
  };

  for (const auto& symbol : body.members()) {
    ProcessModuleMember(symbol, ctx);
  }

  return module;
}

}  // namespace lyra::lowering::ast_to_mir
