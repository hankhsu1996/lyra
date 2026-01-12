#include "lyra/lowering/ast_to_mir/function.hpp"

#include <memory>
#include <utility>
#include <vector>

#include <fmt/format.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>

#include "lyra/common/diagnostic.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/common/variable.hpp"
#include "lyra/lowering/ast_to_mir/statement.hpp"
#include "lyra/lowering/ast_to_mir/type.hpp"
#include "lyra/mir/module.hpp"

namespace lyra::lowering::ast_to_mir {

auto LowerFunction(
    const slang::ast::SubroutineSymbol& subroutine, common::TypeArena& arena)
    -> mir::FunctionDefinition {
  mir::FunctionDefinition func;
  func.name = std::string(subroutine.name);

  // Lower return type
  slang::SourceRange source_range(subroutine.location, subroutine.location);
  auto return_type_result =
      LowerType(subroutine.getReturnType(), source_range, arena);
  if (!return_type_result) {
    throw DiagnosticException(std::move(return_type_result.error()));
  }
  func.return_type = *return_type_result;

  // Lower parameters (input arguments only for MVP)
  for (const auto* arg : subroutine.getArguments()) {
    // Check argument direction - only input supported for MVP
    if (arg->direction != slang::ast::ArgumentDirection::In) {
      throw DiagnosticException(
          Diagnostic::Error(
              slang::SourceRange(arg->location, arg->location),
              fmt::format(
                  "function '{}': output/inout/ref arguments not yet supported",
                  subroutine.name)));
    }

    auto type_result = LowerType(arg->getType(), source_range, arena);
    if (!type_result) {
      throw DiagnosticException(std::move(type_result.error()));
    }

    common::Variable param_var{
        .symbol = arg,
        .type = *type_result,
    };
    func.parameters.push_back(
        mir::FunctionParameter{.variable = std::move(param_var)});
  }

  // Collect local variables from function body
  // Note: slang hoists variable declarations as scope members
  for (const auto& member : subroutine.members()) {
    if (member.kind == slang::ast::SymbolKind::Variable) {
      const auto& var_symbol = member.as<slang::ast::VariableSymbol>();

      // Skip the implicit return value variable (same name as function)
      if (var_symbol.name == subroutine.name) {
        continue;
      }

      auto type_result = LowerType(var_symbol.getType(), source_range, arena);
      if (!type_result) {
        throw DiagnosticException(std::move(type_result.error()));
      }

      common::Variable local_var{
          .symbol = &var_symbol,
          .type = *type_result,
      };
      func.local_variables.push_back(std::move(local_var));
    }
  }

  // Lower the function body
  func.body = LowerStatement(subroutine.getBody(), arena);

  return func;
}

}  // namespace lyra::lowering::ast_to_mir
