#include "lyra/lowering/ast_to_hir/hierarchy_override.hpp"

#include <cstddef>
#include <format>
#include <string>
#include <vector>

#include <slang/ast/Compilation.h>
#include <slang/ast/Scope.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/ParameterSymbols.h>
#include <slang/syntax/AllSyntax.h>
#include <slang/text/SourceManager.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/lowering/ast_to_hir/library_cell.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// The bind directive that inserted `bound`, found from the instantiation the
// directive holds, which is where the front end built the instance from.
auto BindDirectiveIdentity(const slang::ast::InstanceSymbol& bound)
    -> std::string {
  using slang::syntax::SyntaxKind;
  const slang::syntax::SyntaxNode* directive = bound.getSyntax();
  while (directive != nullptr && directive->kind != SyntaxKind::BindDirective) {
    directive = directive->parent;
  }
  const slang::syntax::SyntaxNode* holder =
      directive == nullptr ? nullptr : directive->parent;
  while (holder != nullptr &&
         !slang::syntax::ModuleDeclarationSyntax::isKind(holder->kind) &&
         holder->kind != SyntaxKind::CompilationUnit) {
    holder = holder->parent;
  }
  if (holder == nullptr) {
    throw InternalError(
        "BindDirectiveIdentity: an instance a bind inserted is written in a "
        "bind directive, which a module, an interface or a compilation unit "
        "holds");
  }
  const auto position_among =
      [&](const slang::syntax::SyntaxList<slang::syntax::MemberSyntax>&
              members) {
        std::size_t before = 0;
        for (const slang::syntax::MemberSyntax* member : members) {
          if (member == directive) break;
          if (member->kind == SyntaxKind::BindDirective) ++before;
        }
        return before;
      };
  // A compilation-unit scope has no name, and the source input it belongs to
  // is what tells two of them apart (LRM 3.12.1).
  if (holder->kind == SyntaxKind::CompilationUnit) {
    const slang::SourceManager* sources =
        bound.getParentScope()->getCompilation().getSourceManager();
    if (sources == nullptr) {
      throw InternalError(
          "BindDirectiveIdentity: compilation has no source manager");
    }
    return std::format(
        "$unit {}#{}",
        sources->getFullPath(directive->getFirstToken().location().buffer())
            .string(),
        position_among(
            holder->as<slang::syntax::CompilationUnitSyntax>().members));
  }
  // The declaration is named as the design element it is, which its name
  // alone does not say: the outermost one enclosing the directive is a library
  // cell, and each one inside it is named in the one holding it (LRM 23.4).
  std::vector<const slang::syntax::ModuleDeclarationSyntax*> enclosing;
  for (const slang::syntax::SyntaxNode* node = holder; node != nullptr;
       node = node->parent) {
    if (slang::syntax::ModuleDeclarationSyntax::isKind(node->kind)) {
      enclosing.push_back(&node->as<slang::syntax::ModuleDeclarationSyntax>());
    }
  }
  const slang::ast::Scope& target = *bound.getParentScope();
  const slang::ast::DefinitionSymbol* cell =
      target.getCompilation().getDefinition(target, *enclosing.back());
  if (cell == nullptr) {
    throw InternalError(
        "BindDirectiveIdentity: the declaration holding a bind directive is a "
        "design element the front end declared");
  }
  std::string name = CellName(*cell);
  for (std::size_t inner = enclosing.size() - 1; inner-- > 0;) {
    name += "::";
    name += enclosing[inner]->header->name.valueText();
  }
  return std::format("{}#{}", name, position_among(enclosing.front()->members));
}

// The configuration resolution of the instance whose body holds `inst`, or
// nothing for a top-level instance.
auto ResolutionAround(const slang::ast::InstanceSymbol& inst)
    -> const slang::ast::ResolvedConfig* {
  for (const slang::ast::Scope* scope = inst.getParentScope(); scope != nullptr;
       scope = scope->asSymbol().getParentScope()) {
    if (const auto* body =
            scope->asSymbol().as_if<slang::ast::InstanceBodySymbol>()) {
      return body->parentInstance == nullptr
                 ? nullptr
                 : body->parentInstance->resolvedConfig;
    }
  }
  return nullptr;
}

}  // namespace

auto OverridesOn(const slang::ast::InstanceSymbol& inst)
    -> std::vector<OverrideEffect> {
  std::vector<OverrideEffect> out;
  if (inst.body.flags.has(slang::ast::InstanceFlags::FromBind)) {
    out.emplace_back(InsertedByBind{.directive = BindDirectiveIdentity(inst)});
  }
  // A module declared inside another is no library cell (LRM 23.4, 33.2.1),
  // so no configuration chooses it.
  if (inst.resolvedConfig != nullptr && IsLibraryCell(inst.getDefinition())) {
    out.emplace_back(CellChosenByConfiguration{.cell = &inst.getDefinition()});
  }
  // A configuration hands an instance a resolution of its own only where one
  // of its rules selected that instance; every other instance under it holds
  // its parent's.
  const bool selected_by_rule = inst.resolvedConfig != nullptr &&
                                inst.resolvedConfig != ResolutionAround(inst) &&
                                inst.resolvedConfig->configRule != nullptr;
  for (const auto* param : inst.body.getParameters()) {
    const auto* value = param->symbol.as_if<slang::ast::ParameterSymbol>();
    const bool given_elsewhere =
        value != nullptr
            ? ValueSetElsewhere(inst.body, *value)
            : selected_by_rule &&
                  param->symbol.as<slang::ast::TypeParameterSymbol>()
                      .isOverridden();
    if (given_elsewhere) {
      out.emplace_back(ParameterGivenElsewhere{.parameter = &param->symbol});
    }
  }
  return out;
}

auto ValueSetElsewhere(
    const slang::ast::InstanceBodySymbol& body,
    const slang::ast::ParameterSymbol& param) -> bool {
  if (param.isFromConfig()) return true;
  if (body.hierarchyOverrideNode == nullptr) return false;
  const slang::syntax::SyntaxNode* syntax = param.getSyntax();
  return syntax != nullptr &&
         body.hierarchyOverrideNode->paramOverrides.contains(syntax);
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
