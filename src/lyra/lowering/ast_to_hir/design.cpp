#include "lyra/lowering/ast_to_hir/design.hpp"

#include <algorithm>
#include <cstddef>
#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/ParameterSymbols.h>
#include <slang/ast/symbols/PortSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/ValueSymbol.h>
#include <slang/ast/symbols/VariableSymbols.h>

#include "lyra/common/source_span.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/common/type.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/design.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/hir/fwd.hpp"
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

  // Register nets (GATE: reject net initializers)
  for (const auto* net : members.nets) {
    if (net->getInitializer() != nullptr) {
      ctx->sink->Error(
          span,
          std::format(
              "net declaration assignment not supported: '{}'", net->name));
      continue;
    }
    TypeId type = LowerType(net->getType(), span, ctx);
    if (type) {
      registrar.Register(*net, SymbolKind::kNet, type);
    }
  }

  // Register parameters (kConstOnly storage - no runtime storage)
  for (const auto* param : members.parameters) {
    TypeId type = LowerType(param->getType(), span, ctx);
    if (type) {
      registrar.Register(
          *param, SymbolKind::kParameter, type, StorageClass::kConstOnly);
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

// Classify port binding kind based on port direction and net/variable type.
// Returns nullopt for unsupported directions (inout, ref).
auto ClassifyPortBinding(const slang::ast::PortSymbol& port)
    -> std::optional<PortBinding::Kind> {
  using Kind = PortBinding::Kind;

  switch (port.direction) {
    case slang::ast::ArgumentDirection::In:
      return Kind::kDriveParentToChild;

    case slang::ast::ArgumentDirection::Out:
      // Use slang's authoritative API for net vs variable classification
      return port.isNetPort() ? Kind::kAlias : Kind::kDriveChildToParent;

    case slang::ast::ArgumentDirection::InOut:
      // GATE: reject all inout for MVP (requires tri-state semantics)
      return std::nullopt;

    default:
      return std::nullopt;  // ref direction not supported
  }
}

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

// Explicit type constraint: only Variable or Net allowed as port backing.
// Using variant forces exhaustive handling and prevents accidental widening.
using PortBackingSymbol = std::variant<
    const slang::ast::VariableSymbol*, const slang::ast::NetSymbol*>;

// Transient struct to track port binding info during BFS traversal.
// Holds slang pointers that are only valid during LowerDesign.
struct PendingPortBinding {
  const slang::ast::InstanceSymbol* parent_instance;
  PortBackingSymbol child_port_backing;
  const slang::ast::Expression* value_expr;
  slang::SourceRange source_range;
  PortBinding::Kind kind;
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

    // Classify port binding kind (single classification point)
    auto kind_opt = ClassifyPortBinding(port);
    if (!kind_opt) {
      // Emit explicit error for inout (common case), silent skip for ref
      if (port.direction == slang::ast::ArgumentDirection::InOut) {
        ctx->sink->Error(
            ctx->SpanOf(port_range),
            std::format(
                "inout port '{}' not supported (requires tri-state semantics)",
                port.name));
      }
      continue;
    }
    PortBinding::Kind kind = *kind_opt;

    if (port.internalSymbol == nullptr) {
      ctx->sink->Error(
          ctx->SpanOf(port_range),
          std::format(
              "port '{}' has no backing symbol (null port)", port.name));
      continue;
    }

    // Accept Variable or Net as port backing (variant forces exhaustiveness)
    PortBackingSymbol backing;
    auto sym_kind = port.internalSymbol->kind;
    if (sym_kind == slang::ast::SymbolKind::Variable) {
      backing = &port.internalSymbol->as<slang::ast::VariableSymbol>();
    } else if (sym_kind == slang::ast::SymbolKind::Net) {
      backing = &port.internalSymbol->as<slang::ast::NetSymbol>();
    } else {
      ctx->sink->Error(
          ctx->SpanOf(port_range), std::format(
                                       "port '{}' backed by {} not supported",
                                       port.name, toString(sym_kind)));
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
      // Unconnected port - skip binding, port uses default value (e.g., 0)
      continue;
    }

    // For output/inout ports, slang may wrap the actual in an Assignment where:
    // - left is the parent's variable (the alias target)
    // - right is EmptyArgument
    // Unwrap this if present (compatibility with slang's representation).
    if (kind != PortBinding::Kind::kDriveParentToChild &&
        expr->kind == slang::ast::ExpressionKind::Assignment) {
      const auto& assign = expr->as<slang::ast::AssignmentExpression>();
      expr = &assign.left();
    }

    pending.push_back(
        PendingPortBinding{
            .parent_instance = &parent,
            .child_port_backing = backing,
            .value_expr = expr,
            .source_range = expr->sourceRange,
            .kind = kind,
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

    // Look up the child's port symbol (handles both Variable and Net)
    SymbolId child_port_sym = std::visit(
        [&registrar](auto* sym) { return registrar.Lookup(*sym); },
        pb.child_port_backing);
    if (!child_port_sym) {
      std::string_view port_name = std::visit(
          [](auto* sym) { return sym->name; }, pb.child_port_backing);
      ctx->sink->Error(
          binding_span,
          std::format("port binding target '{}' not registered", port_name));
      continue;
    }

    // Look up the parent instance symbol (needed for all binding kinds)
    SymbolId parent_instance_sym = registrar.Lookup(*pb.parent_instance);
    if (!parent_instance_sym) {
      ctx->sink->Error(
          binding_span,
          std::format(
              "parent instance '{}' not registered", pb.parent_instance->name));
      continue;
    }

    // Lower the connection expression to HIR (design-level, no timescale)
    hir::ExpressionId expr_id =
        LowerDesignLevelExpression(*pb.value_expr, *ctx, registrar);
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

    // Create binding with explicit rvalue/lvalue fields based on kind
    PortBinding binding{
        .kind = pb.kind,
        .child_port_sym = child_port_sym,
        .parent_instance_sym = parent_instance_sym,
        .span = binding_span,
        .parent_rvalue = {},
        .parent_lvalue = {},
    };

    if (pb.kind == PortBinding::Kind::kDriveParentToChild) {
      // Input port: parent rvalue expression
      binding.parent_rvalue = expr_id;
    } else {
      // Output port: parent lvalue expression (alias target)
      // Validate that the expression is an lvalue (can be used as target)
      const hir::Expression& hir_expr = (*ctx->hir_arena)[expr_id];
      if (!hir::IsPlaceExpressionKind(hir_expr.kind)) {
        ctx->sink->Error(
            binding_span, std::format(
                              "output port requires lvalue actual, got {}",
                              static_cast<int>(hir_expr.kind)));
        continue;
      }
      binding.parent_lvalue = expr_id;
    }

    plan.bindings.push_back(std::move(binding));
  }

  return plan;
}

// Recursively collect instances from a scope, walking into generate blocks.
// The `parent` is the instance whose body we're scanning - used for port
// bindings. The `instances` vector is the BFS queue; caller uses index-based
// iteration.
void CollectInstancesFromScope(
    const slang::ast::Scope& scope, const slang::ast::InstanceSymbol& parent,
    Context* ctx, std::vector<PendingPortBinding>& pending_bindings,
    std::vector<const slang::ast::InstanceSymbol*>& instances) {
  for (const auto& member : scope.members()) {
    if (member.kind == slang::ast::SymbolKind::Instance) {
      const auto& child = member.as<slang::ast::InstanceSymbol>();
      CollectPendingPortBindings(parent, child, ctx, pending_bindings);
      instances.push_back(&child);
    } else if (member.kind == slang::ast::SymbolKind::GenerateBlock) {
      const auto& block = member.as<slang::ast::GenerateBlockSymbol>();
      if (!block.isUninstantiated) {
        CollectInstancesFromScope(
            block, parent, ctx, pending_bindings, instances);
      }
    } else if (member.kind == slang::ast::SymbolKind::GenerateBlockArray) {
      const auto& array = member.as<slang::ast::GenerateBlockArraySymbol>();
      for (const auto* entry : array.entries) {
        if (!entry->isUninstantiated) {
          CollectInstancesFromScope(
              *entry, parent, ctx, pending_bindings, instances);
        }
      }
    }
  }
}

}  // namespace

auto LowerDesign(
    slang::ast::Compilation& compilation, SymbolRegistrar& registrar,
    Context* ctx) -> DesignLoweringResult {
  // Initialize canonical builtin types (tick_type for $time, etc.)
  ctx->builtin_types = InternBuiltinTypes(*ctx->type_arena);

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
    CollectInstancesFromScope(
        parent->body, *parent, ctx, pending_bindings, all_instances);
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

  // Build instance table for %m support.
  // Index order matches all_instances (sorted by hierarchical path).
  mir::InstanceTable instance_table;
  instance_table.entries.reserve(all_instances.size());
  for (const auto* instance : all_instances) {
    instance_table.entries.push_back(
        mir::InstanceEntry{
            .full_path = std::string(instance->getHierarchicalPath()),
            .instance_sym = registrar.Lookup(*instance),
        });
  }

  return DesignLoweringResult{
      .design = hir::Design{.elements = std::move(elements)},
      .binding_plan = std::move(binding_plan),
      .instance_table = std::move(instance_table),
  };
}

}  // namespace lyra::lowering::ast_to_hir
