#include "lyra/lowering/ast_to_hir/param_role.hpp"

#include <unordered_map>
#include <unordered_set>
#include <vector>

#include <slang/ast/ASTVisitor.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/ParameterSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>

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

    // Walk the body to find shape-affecting references.
    ShapeParamCollector collector;
    body.visit(collector);

    // Classify: anything NOT in shape_params is a ValueOnly candidate.
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
  }

  return table;
}

}  // namespace lyra::lowering::ast_to_hir
