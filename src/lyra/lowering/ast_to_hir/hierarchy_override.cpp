#include "lyra/lowering/ast_to_hir/hierarchy_override.hpp"

#include <slang/ast/Compilation.h>
#include <slang/ast/Expression.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/ParameterSymbols.h>
#include <slang/syntax/SyntaxNode.h>

namespace lyra::lowering::ast_to_hir {

auto ValueSetElsewhere(
    const slang::ast::InstanceBodySymbol& body,
    const slang::ast::ParameterSymbol& param) -> bool {
  if (param.isFromConfig()) return true;
  if (body.hierarchyOverrideNode == nullptr) return false;
  const slang::syntax::SyntaxNode* syntax = param.getSyntax();
  return syntax != nullptr &&
         body.hierarchyOverrideNode->paramOverrides.contains(syntax);
}

auto GivenAValue(
    const slang::ast::InstanceBodySymbol& body,
    const slang::ast::ParameterSymbol& param) -> bool {
  return param.isOverridden() || ValueSetElsewhere(body, param);
}

auto ValueWrittenAtInstantiation(
    const slang::ast::InstanceSymbol& inst,
    const slang::ast::ParameterSymbol& param) -> const slang::ast::Expression* {
  if (!param.isOverridden() || ValueSetElsewhere(inst.body, param)) {
    return nullptr;
  }
  return param.getInitializer();
}

}  // namespace lyra::lowering::ast_to_hir
