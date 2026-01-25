#include "lyra/lowering/ast_to_hir/design.hpp"

#include <algorithm>
#include <cstddef>
#include <format>
#include <type_traits>
#include <utility>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/PortSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>

#include "lyra/hir/design.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"
#include "lyra/lowering/ast_to_hir/expression.hpp"
#include "lyra/lowering/ast_to_hir/generate.hpp"
#include "lyra/lowering/ast_to_hir/module.hpp"
#include "lyra/lowering/ast_to_hir/package.hpp"
#include "lyra/lowering/ast_to_hir/port_binding.hpp"
#include "lyra/lowering/ast_to_hir/source_utils.hpp"
#include "lyra/lowering/ast_to_hir/symbol_registrar.hpp"
#include "lyra/lowering/ast_to_hir/type.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// Phase 0: Register module-level symbols for one instance.
// Establishes SymbolIds before any lowering runs. Scope is currently
// unused downstream but assigned for future use.
void RegisterModuleDeclarations(
    const slang::ast::InstanceSymbol& instance, SymbolRegistrar& registrar,
    Context* ctx) {
  const slang::ast::InstanceBodySymbol& body = instance.body;
  SourceSpan span = ctx->SpanOf(GetSourceRange(instance));

  registrar.Register(instance, SymbolKind::kInstance, kInvalidTypeId);

  ScopeGuard scope_guard(registrar, ScopeKind::kModule);

  CollectedMembers members;
  CollectScopeMembers(body, registrar, members);

  for (const auto* var : members.variables) {
    TypeId type = LowerType(var->getType(), span, ctx);
    if (type) {
      registrar.Register(*var, SymbolKind::kVariable, type);
    }
  }

  for (const auto* sub : members.functions) {
    const auto& ret_type = sub->getReturnType();
    if (!ret_type.isIntegral() && !ret_type.isVoid()) {
      continue;
    }
    TypeId return_type = LowerType(ret_type, span, ctx);
    if (return_type) {
      registrar.Register(*sub, SymbolKind::kFunction, return_type);
    }
  }
}

// Port direction for pending bindings
enum class PortDirection { kIn, kOut, kInOut };

// Validate that an HIR expression only references design-level symbols.
// Returns true if valid, false and emits diagnostic if invalid.
// Design-level: kDesignStorage (module vars, ports) or kConstOnly (params).
// Rejects: kLocalStorage (function locals, block locals).
auto ValidateDesignLevelExpression(
    hir::ExpressionId expr_id, const hir::Arena& arena,
    const SymbolTable& symbol_table, SourceSpan span, DiagnosticSink* sink)
    -> bool {
  // Walk expression tree looking for symbol references
  std::vector<hir::ExpressionId> worklist;
  worklist.push_back(expr_id);

  while (!worklist.empty()) {
    hir::ExpressionId current = worklist.back();
    worklist.pop_back();

    const hir::Expression& expr = arena[current];

    // Check symbol references
    if (expr.kind == hir::ExpressionKind::kNameRef) {
      const auto& data = std::get<hir::NameRefExpressionData>(expr.data);
      const Symbol& sym = symbol_table[data.symbol];
      if (sym.storage_class == StorageClass::kLocalStorage) {
        sink->Error(
            span,
            std::format(
                "port binding cannot reference local variable '{}'", sym.name));
        return false;
      }
    } else if (expr.kind == hir::ExpressionKind::kHierarchicalRef) {
      const auto& data =
          std::get<hir::HierarchicalRefExpressionData>(expr.data);
      const Symbol& sym = symbol_table[data.target];
      if (sym.storage_class == StorageClass::kLocalStorage) {
        sink->Error(
            span,
            std::format(
                "port binding cannot reference local variable '{}'", sym.name));
        return false;
      }
    }

    // Add child expressions to worklist
    std::visit(
        [&worklist](const auto& data) {
          using T = std::decay_t<decltype(data)>;
          if constexpr (std::is_same_v<T, hir::UnaryExpressionData>) {
            worklist.push_back(data.operand);
          } else if constexpr (std::is_same_v<T, hir::BinaryExpressionData>) {
            worklist.push_back(data.lhs);
            worklist.push_back(data.rhs);
          } else if constexpr (std::is_same_v<T, hir::CastExpressionData>) {
            worklist.push_back(data.operand);
          } else if constexpr (std::is_same_v<T, hir::BitCastExpressionData>) {
            worklist.push_back(data.operand);
          } else if constexpr (std::is_same_v<
                                   T, hir::ConditionalExpressionData>) {
            worklist.push_back(data.condition);
            worklist.push_back(data.then_expr);
            worklist.push_back(data.else_expr);
          } else if constexpr (std::is_same_v<
                                   T, hir::ElementAccessExpressionData>) {
            worklist.push_back(data.base);
            worklist.push_back(data.index);
          } else if constexpr (std::is_same_v<
                                   T, hir::MemberAccessExpressionData>) {
            worklist.push_back(data.base);
          } else if constexpr (std::is_same_v<
                                   T, hir::PackedElementSelectExpressionData>) {
            worklist.push_back(data.base);
            worklist.push_back(data.index);
          } else if constexpr (std::is_same_v<
                                   T, hir::PackedFieldAccessExpressionData>) {
            worklist.push_back(data.base);
          } else if constexpr (std::is_same_v<
                                   T, hir::BitSelectExpressionData>) {
            worklist.push_back(data.base);
            worklist.push_back(data.index);
          } else if constexpr (std::is_same_v<
                                   T, hir::RangeSelectExpressionData>) {
            worklist.push_back(data.base);
          } else if constexpr (std::is_same_v<
                                   T, hir::IndexedPartSelectExpressionData>) {
            worklist.push_back(data.base);
            worklist.push_back(data.index);
          }
          // Other expression types (Constant, Call, etc.) don't contain symbol
          // refs or are not valid in port binding context anyway
        },
        expr.data);
  }

  return true;
}

// Transient struct to track port binding info during BFS traversal.
// Holds slang pointers that are only valid during LowerDesign.
struct PendingPortBinding {
  const slang::ast::InstanceSymbol* parent_instance;
  const slang::ast::VariableSymbol* child_port_var;
  const slang::ast::Expression* value_expr;
  slang::SourceRange source_range;
  PortDirection direction;
};

// Collect port bindings for a parent->child instance edge.
// Returns pending bindings that will be lowered to HIR after Phase 0.
void CollectPendingPortBindings(
    const slang::ast::InstanceSymbol& parent,
    const slang::ast::InstanceSymbol& child, Context* ctx,
    std::vector<PendingPortBinding>& pending) {
  for (const auto* port_sym : child.body.getPortList()) {
    if (port_sym->kind != slang::ast::SymbolKind::Port) {
      continue;
    }

    const auto& port = port_sym->as<slang::ast::PortSymbol>();
    slang::SourceRange port_range{port.location, port.location};

    // Determine port direction
    PortDirection dir = PortDirection::kIn;
    switch (port.direction) {
      case slang::ast::ArgumentDirection::In:
        dir = PortDirection::kIn;
        break;
      case slang::ast::ArgumentDirection::Out:
        dir = PortDirection::kOut;
        break;
      case slang::ast::ArgumentDirection::InOut:
        dir = PortDirection::kInOut;
        break;
      default:
        // ref direction not supported
        continue;
    }

    if (port.internalSymbol == nullptr) {
      ctx->sink->Error(
          ctx->SpanOf(port_range),
          std::format(
              "port '{}' has no backing variable (null port)", port.name));
      continue;
    }

    if (port.internalSymbol->kind != slang::ast::SymbolKind::Variable) {
      ctx->sink->Error(
          ctx->SpanOf(port_range),
          std::format(
              "port '{}' backed by {} not supported (only variable ports)",
              port.name, toString(port.internalSymbol->kind)));
      continue;
    }

    const auto* conn = child.getPortConnection(port);
    if (conn == nullptr) {
      ctx->sink->Error(
          ctx->SpanOf(port_range),
          std::format(
              "unconnected port '{}' without default value", port.name));
      continue;
    }

    const auto* expr = conn->getExpression();
    if (expr == nullptr) {
      ctx->sink->Error(
          ctx->SpanOf(port_range),
          std::format(
              "port '{}': implicit named connections not yet supported",
              port.name));
      continue;
    }

    // For output/inout ports, slang may wrap the actual in an Assignment where:
    // - left is the parent's variable (the alias target)
    // - right is EmptyArgument
    // Unwrap this if present (compatibility with slang's representation).
    if (dir != PortDirection::kIn &&
        expr->kind == slang::ast::ExpressionKind::Assignment) {
      const auto& assign = expr->as<slang::ast::AssignmentExpression>();
      expr = &assign.left();
    }

    pending.push_back(
        PendingPortBinding{
            .parent_instance = &parent,
            .child_port_var =
                &port.internalSymbol->as<slang::ast::VariableSymbol>(),
            .value_expr = expr,
            .source_range = expr->sourceRange,
            .direction = dir,
        });
  }
}

// Lower pending port bindings to HIR expressions.
// Called after Phase 0 registration so all symbols are available.
auto LowerPortBindings(
    const std::vector<PendingPortBinding>& pending, SymbolRegistrar& registrar,
    Context* ctx) -> DesignBindingPlan {
  DesignBindingPlan plan;

  for (const auto& pb : pending) {
    SourceSpan binding_span = ctx->SpanOf(pb.source_range);

    // Look up the child's port variable symbol
    SymbolId child_port_sym = registrar.Lookup(*pb.child_port_var);
    if (!child_port_sym) {
      ctx->sink->Error(
          binding_span, std::format(
                            "port binding target '{}' not registered",
                            pb.child_port_var->name));
      continue;
    }

    // Lower the connection expression to HIR
    hir::ExpressionId expr_id = LowerExpression(*pb.value_expr, registrar, ctx);
    if (!expr_id) {
      continue;
    }

    // Validate that the expression only references design-level symbols.
    // Rejects local variables which don't have persistent storage.
    if (!ValidateDesignLevelExpression(
            expr_id, *ctx->hir_arena, *ctx->symbol_table, binding_span,
            ctx->sink)) {
      continue;
    }

    if (pb.direction == PortDirection::kIn) {
      // Input port: create DriveBinding (rvalue drives child variable)
      SymbolId parent_instance_sym = registrar.Lookup(*pb.parent_instance);
      if (!parent_instance_sym) {
        ctx->sink->Error(
            binding_span, std::format(
                              "parent instance '{}' not registered",
                              pb.parent_instance->name));
        continue;
      }

      plan.drives.push_back(
          DriveBinding{
              .child_port_sym = child_port_sym,
              .rvalue = expr_id,
              .span = binding_span,
              .parent_instance_sym = parent_instance_sym,
          });
    } else {
      // Output/inout port: create AliasBinding (child aliases parent's place)
      // Validate that the expression is an lvalue (can be used as alias target)
      const hir::Expression& hir_expr = (*ctx->hir_arena)[expr_id];
      if (!hir::IsPlaceExpressionKind(hir_expr.kind)) {
        ctx->sink->Error(
            binding_span,
            std::format(
                "output/inout port requires lvalue actual, got {}",
                static_cast<int>(hir_expr.kind)));
        continue;
      }

      plan.aliases.push_back(
          AliasBinding{
              .child_port_sym = child_port_sym,
              .lvalue = expr_id,
              .span = binding_span,
          });
    }
  }

  return plan;
}

}  // namespace

auto LowerDesign(
    slang::ast::Compilation& compilation, SymbolRegistrar& registrar,
    Context* ctx) -> DesignLoweringResult {
  const slang::ast::RootSymbol& root = compilation.getRoot();

  std::vector<hir::DesignElement> elements;

  // BFS traversal to collect all instances.
  // Also collect pending port bindings (slang pointers, valid during this
  // call).
  std::vector<const slang::ast::InstanceSymbol*> all_instances;
  std::vector<PendingPortBinding> pending_bindings;

  for (const auto* inst : root.topInstances) {
    all_instances.push_back(inst);
  }
  for (size_t i = 0; i < all_instances.size(); ++i) {
    const auto* parent = all_instances[i];

    for (const auto& child :
         parent->body.membersOfType<slang::ast::InstanceSymbol>()) {
      CollectPendingPortBindings(*parent, child, ctx, pending_bindings);
      all_instances.push_back(&child);
    }
  }

  // Stable sort for deterministic slot ordering.
  std::ranges::sort(all_instances, [](const auto* a, const auto* b) {
    return a->getHierarchicalPath() < b->getHierarchicalPath();
  });

  // Phase 0: Register all module declarations (creation allowed)
  for (const auto* instance : all_instances) {
    RegisterModuleDeclarations(*instance, registrar, ctx);
  }

  // Lower port bindings to HIR (after Phase 0, symbols are registered)
  DesignBindingPlan binding_plan =
      LowerPortBindings(pending_bindings, registrar, ctx);

  // Lower packages (independent of instances)
  for (const slang::ast::PackageSymbol* pkg : compilation.getPackages()) {
    if (pkg->name == "std") {
      continue;
    }
    elements.emplace_back(LowerPackage(*pkg, registrar, ctx));
  }

  // Phase 1: Lower module bodies.
  // Port bindings are now handled at HIR->MIR level, not here.
  for (const auto* instance : all_instances) {
    elements.emplace_back(LowerModule(*instance, registrar, ctx));
  }

  return DesignLoweringResult{
      .design = hir::Design{.elements = std::move(elements)},
      .binding_plan = std::move(binding_plan),
  };
}

}  // namespace lyra::lowering::ast_to_hir
