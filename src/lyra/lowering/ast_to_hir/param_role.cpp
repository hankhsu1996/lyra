#include "lyra/lowering/ast_to_hir/param_role.hpp"

#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include <slang/ast/ASTVisitor.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/ParameterSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/ValueSymbol.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/DeclaredType.h>
#include <slang/syntax/AllSyntax.h>
#include <slang/syntax/SyntaxNode.h>

namespace lyra::lowering::ast_to_hir {

auto ParamRoleTable::Lookup(const slang::ast::ParameterSymbol& p) const
    -> ParamRole {
  return value_only_params_.contains(&p) ? ParamRole::kValueOnly
                                         : ParamRole::kShape;
}

namespace {

// Visitor that collects parameter symbols referenced outside procedural
// contexts. Any parameter found outside a procedural body is conservatively
// classified as shape-affecting.
struct ShapeParamCollector
    : public slang::ast::ASTVisitor<ShapeParamCollector, true, true> {
  std::unordered_set<const slang::ast::ParameterSymbol*> shape_params;
  int procedural_depth = 0;

  // Procedural contexts: entering these sets in_procedural = true.
  void handle(const slang::ast::ProceduralBlockSymbol& proc) {
    ++procedural_depth;
    visitDefault(proc);
    --procedural_depth;
  }

  void handle(const slang::ast::SubroutineSymbol& sub) {
    ++procedural_depth;
    visitDefault(sub);
    --procedural_depth;
  }

  // Continuous assignments are runtime-only (RHS is procedural context).
  void handle(const slang::ast::ContinuousAssignSymbol& ca) {
    ++procedural_depth;
    visitDefault(ca);
    --procedural_depth;
  }

  // Named value expressions -- check if referencing a parameter.
  void handle(const slang::ast::NamedValueExpression& expr) {
    if (procedural_depth == 0) {
      if (expr.symbol.kind == slang::ast::SymbolKind::Parameter) {
        shape_params.insert(&expr.symbol.as<slang::ast::ParameterSymbol>());
      }
    }
    visitDefault(expr);
  }
};

// Recursively collect identifier tokens from a syntax tree.
void CollectIdentifiersFromSyntax(
    const slang::syntax::SyntaxNode* node,
    std::unordered_set<std::string_view>& identifiers) {
  if (node == nullptr) return;
  auto count = node->getChildCount();
  for (size_t i = 0; i < count; ++i) {
    if (const auto* child = node->childNode(i)) {
      CollectIdentifiersFromSyntax(child, identifiers);
    } else {
      auto token = node->childToken(i);
      if (token.kind == slang::parsing::TokenKind::Identifier) {
        identifiers.insert(token.valueText());
      }
    }
  }
}

// Trace a symbol's constant-expression dependency chain back to root
// (non-local) parameters. For localparams, recursively follows initializer
// expression references.
void CollectParamsFromDependencyChain(
    const slang::ast::Symbol& sym,
    std::unordered_set<const slang::ast::ParameterSymbol*>& out,
    std::unordered_set<const slang::ast::Symbol*>& visited) {
  if (!visited.insert(&sym).second) return;
  if (sym.kind != slang::ast::SymbolKind::Parameter) return;

  const auto& param = sym.as<slang::ast::ParameterSymbol>();

  if (!param.isLocalParam()) {
    out.insert(&param);
    return;
  }

  // Local parameter: trace initializer for further dependencies.
  const auto* init = param.getInitializer();
  if (init == nullptr) return;

  struct RefCollector
      : public slang::ast::ASTVisitor<RefCollector, true, true> {
    std::vector<const slang::ast::Symbol*> refs;
    void handle(const slang::ast::NamedValueExpression& expr) {
      if (expr.symbol.kind == slang::ast::SymbolKind::Parameter) {
        refs.push_back(&expr.symbol);
      }
    }
  };

  RefCollector collector;
  init->visit(collector);

  for (const auto* ref : collector.refs) {
    CollectParamsFromDependencyChain(*ref, out, visited);
  }
}

// Collect parameters that affect packed structural shape of body declarations.
// Analyzes one instance body at a time by inspecting type syntax of
// declarations with packed/integral types and tracing parameter dependencies.
auto CollectStructuralShapeParams(const slang::ast::InstanceBodySymbol& body)
    -> std::unordered_set<const slang::ast::ParameterSymbol*> {
  std::unordered_set<const slang::ast::ParameterSymbol*> result;

  for (const auto& member : body.members()) {
    bool is_value_sym =
        (member.kind == slang::ast::SymbolKind::Variable ||
         member.kind == slang::ast::SymbolKind::Net);
    if (!is_value_sym) continue;

    const auto& val_sym = member.as<slang::ast::ValueSymbol>();
    if (!val_sym.getType().isIntegral()) continue;

    const auto* type_syntax = val_sym.getDeclaredType()->getTypeSyntax();
    if (type_syntax == nullptr) continue;

    // Walk type syntax to find identifier references in packed dimensions.
    std::unordered_set<std::string_view> identifiers;
    CollectIdentifiersFromSyntax(type_syntax, identifiers);

    // Resolve each identifier and trace parameter dependencies.
    std::unordered_set<const slang::ast::Symbol*> visited;
    for (const auto& name : identifiers) {
      const auto* sym = body.find(name);
      if (sym == nullptr) continue;
      if (sym->kind == slang::ast::SymbolKind::Parameter) {
        CollectParamsFromDependencyChain(*sym, result, visited);
      }
    }
  }

  return result;
}

}  // namespace

auto ClassifyParamRoles(
    const std::vector<const slang::ast::InstanceSymbol*>& all_instances)
    -> ParamRoleTable {
  ParamRoleTable table;

  // Group instances by definition to classify per-definition.
  std::unordered_map<
      const slang::ast::DefinitionSymbol*,
      std::vector<const slang::ast::InstanceSymbol*>>
      by_def;
  for (const auto* inst : all_instances) {
    by_def[&inst->body.getDefinition()].push_back(inst);
  }

  for (const auto& [def, instances] : by_def) {
    // Use the first instance's body to walk for shape analysis.
    // All instances of the same definition share the same AST structure.
    const auto& body = instances[0]->body;

    // Collect all parameters in this definition.
    std::vector<const slang::ast::ParameterSymbol*> all_params;
    for (const auto& member : body.members()) {
      if (member.kind == slang::ast::SymbolKind::Parameter) {
        all_params.push_back(&member.as<slang::ast::ParameterSymbol>());
      }
    }
    if (all_params.empty()) continue;

    // Pass 1: syntax-visible shape collection.
    ShapeParamCollector collector;
    body.visit(collector);

    // Pass 2: provisionally classify remaining params as kValueOnly.
    for (const auto* param : all_params) {
      if (collector.shape_params.contains(param)) continue;

      // Scope guard: only promote packed integral / enum types.
      const auto& param_type = param->getType();
      if (!param_type.isIntegral()) continue;

      // Check cross-instance value variation (no benefit if all same).
      if (instances.size() < 2) continue;

      bool has_variation = false;
      const auto& ref_value = param->getValue();
      for (size_t i = 1; i < instances.size(); ++i) {
        const auto* other_sym = instances[i]->body.find(param->name);
        if (other_sym == nullptr) continue;
        if (other_sym->kind != slang::ast::SymbolKind::Parameter) continue;
        const auto& other_param = other_sym->as<slang::ast::ParameterSymbol>();
        if (other_param.getValue() != ref_value) {
          has_variation = true;
          break;
        }
      }
      if (!has_variation) continue;

      // Mark as ValueOnly in ALL instances of this definition.
      table.MarkValueOnly(param);
      for (size_t i = 1; i < instances.size(); ++i) {
        const auto* other_sym = instances[i]->body.find(param->name);
        if (other_sym == nullptr) continue;
        if (other_sym->kind != slang::ast::SymbolKind::Parameter) continue;
        table.MarkValueOnly(&other_sym->as<slang::ast::ParameterSymbol>());
      }
    }

    // Pass 3: structural shape promotion from resolved packed types.
    // Run on each instance body and promote discovered shape params.
    for (const auto* inst : instances) {
      auto structural_shape = CollectStructuralShapeParams(inst->body);
      for (const auto* param : structural_shape) {
        table.MarkShape(param);
      }
    }
  }

  return table;
}

}  // namespace lyra::lowering::ast_to_hir
