#include "lyra/lowering/ast_to_hir/design.hpp"

#include <algorithm>
#include <cstddef>
#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <type_traits>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

#include <slang/ast/Compilation.h>
#include <slang/ast/Expression.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/ParameterSymbols.h>
#include <slang/ast/symbols/PortSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/ValueSymbol.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/syntax/AllSyntax.h>

#include "lyra/common/child_coord_map.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/module_identity.hpp"
#include "lyra/common/source_span.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/common/type.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/design.hpp"
#include "lyra/hir/dpi.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/lowering/ast_to_hir/callable_registration.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"
#include "lyra/lowering/ast_to_hir/expression.hpp"
#include "lyra/lowering/ast_to_hir/generate.hpp"
#include "lyra/lowering/ast_to_hir/module.hpp"
#include "lyra/lowering/ast_to_hir/package.hpp"
#include "lyra/lowering/ast_to_hir/param_transmission.hpp"
#include "lyra/lowering/ast_to_hir/port_binding.hpp"
#include "lyra/lowering/ast_to_hir/repertoire_descriptor.hpp"
#include "lyra/lowering/ast_to_hir/routine.hpp"
#include "lyra/lowering/ast_to_hir/source_utils.hpp"
#include "lyra/lowering/ast_to_hir/specialization.hpp"
#include "lyra/lowering/ast_to_hir/symbol_registrar.hpp"
#include "lyra/lowering/ast_to_hir/task_analysis.hpp"
#include "lyra/lowering/ast_to_hir/timescale.hpp"
#include "lyra/lowering/ast_to_hir/type.hpp"
#include "lyra/lowering/ast_to_hir/type_seeder.hpp"
#include "lyra/mir/instance.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// Classify a DPI export subroutine's signature using the canonical
// slang-type classifier (shared with imports). Returns nullopt and
// emits user diagnostics if any parameter or return type is unsupported.
auto ClassifyExportSignature(
    const slang::ast::SubroutineSymbol& sub, SourceSpan span,
    SymbolRegistrar& registrar, Context* ctx)
    -> std::optional<hir::DpiExportSignature> {
  const auto& ret_type = sub.getReturnType();
  auto return_class = ClassifyDpiAbiType(ret_type);
  if (!return_class) {
    ctx->sink->Unsupported(
        span,
        std::format(
            "DPI-C export return type '{}' not yet supported",
            ret_type.toString()),
        UnsupportedCategory::kType);
    return std::nullopt;
  }
  TypeId return_type_id = LowerType(ret_type, span, ctx);
  if (!return_type_id) {
    return std::nullopt;
  }

  std::vector<hir::DpiParam> params;
  bool has_error = false;
  for (const slang::ast::FormalArgumentSymbol* arg : sub.getArguments()) {
    SourceSpan arg_span = ctx->SpanOf(GetSourceRange(*arg));
    if (arg->direction == slang::ast::ArgumentDirection::Ref) {
      ctx->sink->Unsupported(
          arg_span, "DPI-C export ref parameters not yet supported",
          UnsupportedCategory::kFeature);
      has_error = true;
      continue;
    }
    auto arg_class = ClassifyDpiAbiType(arg->getType());
    if (!arg_class || !hir::IsValidDpiParamType(*arg_class)) {
      ctx->sink->Unsupported(
          arg_span,
          std::format(
              "DPI-C export argument type '{}' not yet supported",
              arg->getType().toString()),
          UnsupportedCategory::kType);
      has_error = true;
      continue;
    }
    TypeId arg_type_id = LowerType(arg->getType(), arg_span, ctx);
    if (!arg_type_id) {
      has_error = true;
      continue;
    }
    SymbolId arg_sym = registrar.Lookup(*arg);
    if (!arg_sym) {
      has_error = true;
      continue;
    }
    ParameterDirection dir = ParameterDirection::kInput;
    switch (arg->direction) {
      case slang::ast::ArgumentDirection::In:
        dir = ParameterDirection::kInput;
        break;
      case slang::ast::ArgumentDirection::Out:
        dir = ParameterDirection::kOutput;
        break;
      case slang::ast::ArgumentDirection::InOut:
        dir = ParameterDirection::kInOut;
        break;
      default:
        throw common::InternalError(
            "ClassifyExportSignature",
            "unexpected argument direction after ref rejection");
    }
    params.push_back({
        .symbol = arg_sym,
        .span = arg_span,
        .type_id = arg_type_id,
        .dpi_type = *arg_class,
        .direction = dir,
    });
  }
  if (has_error) {
    return std::nullopt;
  }

  return hir::DpiExportSignature{
      .return_type_id = return_type_id,
      .return_dpi_type = *return_class,
      .params = std::move(params),
  };
}

// Get the source span for a DPI export directive. Uses the export
// declaration syntax when available, falling back to the function.
auto GetDpiExportSpan(
    const slang::ast::Compilation::DPIExport& dpi_export,
    const slang::ast::SubroutineSymbol& sub, Context* ctx) -> SourceSpan {
  if (dpi_export.syntax != nullptr) {
    return ctx->SpanOf(dpi_export.syntax->sourceRange());
  }
  return ctx->SpanOf(GetSourceRange(sub));
}

// Walk up through transparent scopes and return the first non-transparent
// ancestor. Generate blocks are transparent -- declarations inside them
// belong to the enclosing module body or package, not to the generate
// block itself.
//
// This is a general AST-to-HIR ownership rule. The transparent set
// (GenerateBlock, GenerateBlockArray) follows from Lyra's compilation
// model where generate scopes do not create ownership boundaries.
//
// The helper does not validate whether the returned ancestor is a valid
// terminal for a particular declaration kind. Callers must check the
// returned symbol's kind against their own terminal set.
auto GetFirstNonTransparentAncestor(const slang::ast::Symbol& start)
    -> const slang::ast::Symbol& {
  const slang::ast::Symbol* current = &start;
  while (true) {
    switch (current->kind) {
      case slang::ast::SymbolKind::GenerateBlock:
      case slang::ast::SymbolKind::GenerateBlockArray:
        current = &current->getParentScope()->asSymbol();
        continue;
      default:
        return *current;
    }
  }
}

// Attach a DpiExportDecl to its owning package or module body container.
void AttachDpiExportDecl(
    const slang::ast::Symbol& owner, hir::DpiExportDecl decl,
    std::unordered_map<const slang::ast::Symbol*, size_t>& pkg_to_element_idx,
    std::unordered_map<const slang::ast::Symbol*, size_t>& scope_to_body_idx,
    std::vector<hir::DesignElement>& elements,
    std::vector<hir::ModuleBody>& module_bodies) {
  if (auto it = pkg_to_element_idx.find(&owner);
      it != pkg_to_element_idx.end()) {
    auto& pkg = std::get<hir::Package>(elements[it->second]);
    pkg.dpi_exports.push_back(std::move(decl));
    return;
  }
  if (auto it = scope_to_body_idx.find(&owner); it != scope_to_body_idx.end()) {
    module_bodies[it->second].dpi_exports.push_back(std::move(decl));
    return;
  }
  throw common::InternalError(
      "AttachDpiExportDecl",
      std::format(
          "DPI export owner scope not mapped to HIR container: '{}'",
          std::string(owner.name)));
}

}  // namespace

namespace {

// Phase 0: Register module-level symbols for one instance.
// Establishes SymbolIds before any lowering runs. Scope is currently
// unused downstream but assigned for future use.
void RegisterModuleDeclarations(
    const slang::ast::InstanceSymbol& instance, SymbolRegistrar& registrar,
    const ParamTransmissionTable& transmission, Context* ctx) {
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

  // Register parameters.
  // Storage assignment is derived from within-group parameter variance,
  // not from the upstream param-role classifier.
  for (const auto* param : members.parameters) {
    TypeId type = LowerType(param->getType(), span, ctx);
    if (type) {
      StorageClass sc =
          transmission.Lookup(*param) == ParamDisposition::kTransmitted
              ? StorageClass::kDesignStorage
              : StorageClass::kConstOnly;
      registrar.Register(*param, SymbolKind::kParameter, type, sc);
    }
  }

  for (const auto* sub : members.functions) {
    RegisterCallableSymbol(*sub, SymbolKind::kFunction, *ctx, registrar, span);
  }

  for (const auto* sub : members.tasks) {
    RegisterCallableSymbol(*sub, SymbolKind::kTask, *ctx, registrar, span);
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
      // Both net and variable output ports use connection propagation.
      // Child owns its port storage; parent propagation is connection work.
      return Kind::kDriveChildToParent;

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
  const slang::ast::InstanceSymbol* child_instance;
  PortBackingSymbol child_port_backing;
  const slang::ast::Expression* value_expr;
  slang::SourceRange source_range;
  PortBinding::Kind kind;
  uint32_t child_port_ordinal = UINT32_MAX;
};

// Collect port bindings for a parent->child instance edge.
// Returns pending bindings that will be lowered to HIR after Phase 0.
void CollectPendingPortBindings(
    const slang::ast::InstanceSymbol& parent,
    const slang::ast::InstanceSymbol& child, Context* ctx,
    std::vector<PendingPortBinding>& pending) {
  const auto& port_list = child.body.getPortList();
  for (uint32_t port_ordinal = 0;
       port_ordinal < static_cast<uint32_t>(port_list.size()); ++port_ordinal) {
    const auto* port_sym = port_list[port_ordinal];
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
            .child_instance = &child,
            .child_port_backing = backing,
            .value_expr = expr,
            .source_range = expr->sourceRange,
            .kind = kind,
            .child_port_ordinal = port_ordinal,
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

    // Validate child port ordinal was captured during collection.
    if (pb.child_port_ordinal == UINT32_MAX) {
      throw common::InternalError(
          "LowerPortBindings", "child_port_ordinal not set");
    }

    // Look up child instance symbol (topology locator).
    SymbolId child_instance_sym = registrar.Lookup(*pb.child_instance);
    if (!child_instance_sym) {
      ctx->sink->Error(
          binding_span,
          std::format(
              "child instance '{}' not registered", pb.child_instance->name));
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

    // Classify parent-side source: compile-time constant or runtime
    // expression. Constant classification uses semantic evaluation,
    // not HIR expression kind inspection.
    ConstId parent_constant_id{UINT32_MAX};
    hir::ExpressionId expr_id;
    if (pb.kind == PortBinding::Kind::kDriveParentToChild) {
      if (auto const_id = TryEvaluatePortBindingConstant(
              *pb.value_expr, pb.parent_instance->body, binding_span, ctx)) {
        parent_constant_id = *const_id;
      } else {
        expr_id = LowerDesignLevelExpression(*pb.value_expr, *ctx, registrar);
        if (!expr_id) continue;
        if (!ValidateDesignLevelExpression(
                expr_id, *ctx->hir_arena, *ctx->symbol_table, binding_span,
                ctx->sink)) {
          continue;
        }
      }
    } else {
      expr_id = LowerDesignLevelExpression(*pb.value_expr, *ctx, registrar);
      if (!expr_id) continue;
      if (!ValidateDesignLevelExpression(
              expr_id, *ctx->hir_arena, *ctx->symbol_table, binding_span,
              ctx->sink)) {
        continue;
      }
    }

    // Create binding with source split based on classification.
    PortBinding binding{
        .kind = pb.kind,
        .child_instance_sym = child_instance_sym,
        .child_port_ordinal = pb.child_port_ordinal,
        .parent_instance_sym = parent_instance_sym,
        .span = binding_span,
        .parent_constant_id = parent_constant_id,
        .parent_rvalue = {},
        .parent_lvalue = {},
    };

    if (pb.kind == PortBinding::Kind::kDriveParentToChild) {
      // Input port: constant source or runtime expression.
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

// Build the hierarchy-node table from slang AST, emitting both generate
// scope nodes and instance nodes. The instance list (all_instances) must
// already be populated and sorted. This function produces a parallel
// table of hierarchy nodes in strict parent-before-child order.
// Emit hierarchy nodes in structural (declaration) order. This is the
// authoritative child ordering for the runtime scope tree. Instance
// children carry instance_index as a payload lookup key only -- it
// does NOT determine child position. The tree order is structural.

// Derive parent-relative label from authoritative full hierarchical
// paths. One strict policy for all scope kinds (instances and generate
// scopes): strip the parent's full_path + '.' prefix. Root nodes have
// empty parent_full and their label equals their full_path. Non-root
// nodes whose full_path does not start with parent_full + '.' violate
// the hierarchy invariant and fail loudly -- no silent fallback.
auto DeriveRelativeLabel(
    std::string_view full_path, std::string_view parent_full) -> std::string {
  if (parent_full.empty()) return std::string(full_path);
  if (!full_path.starts_with(parent_full) ||
      full_path.size() <= parent_full.size() + 1 ||
      full_path[parent_full.size()] != '.') {
    throw common::InternalError(
        "DeriveRelativeLabel",
        std::format(
            "full_path '{}' is not under parent '{}'", full_path, parent_full));
  }
  return std::string(full_path.substr(parent_full.size() + 1));
}

void BuildHierarchyNodes(
    const slang::ast::Scope& scope, uint32_t parent_node_index,
    const std::vector<const slang::ast::InstanceSymbol*>& all_instances,
    const std::unordered_map<const slang::ast::InstanceSymbol*, uint32_t>&
        instance_to_index,
    std::vector<common::HierarchyNode>& nodes) {
  // Copy parent full_path by value: nodes may reallocate during push_back
  // below, which would invalidate any string_view into nodes[parent].
  std::string parent_full = parent_node_index != UINT32_MAX
                                ? nodes[parent_node_index].full_path
                                : std::string{};
  for (const auto& member : scope.members()) {
    if (member.kind == slang::ast::SymbolKind::Instance) {
      const auto& child = member.as<slang::ast::InstanceSymbol>();
      auto it = instance_to_index.find(&child);
      if (it == instance_to_index.end()) continue;
      auto child_idx = static_cast<uint32_t>(nodes.size());
      std::string full_path = std::string(child.getHierarchicalPath());
      std::string label = DeriveRelativeLabel(full_path, parent_full);
      nodes.push_back(
          common::HierarchyNode{
              .kind = common::HierarchyNode::kInstance,
              .parent_node_index = parent_node_index,
              .label = std::move(label),
              .full_path = full_path,
              .instance_index = it->second,
          });
      BuildHierarchyNodes(
          child.body, child_idx, all_instances, instance_to_index, nodes);
    } else if (member.kind == slang::ast::SymbolKind::GenerateBlock) {
      const auto& block = member.as<slang::ast::GenerateBlockSymbol>();
      if (block.isUninstantiated) continue;
      auto scope_idx = static_cast<uint32_t>(nodes.size());
      std::string full_path = std::string(block.getHierarchicalPath());
      std::string label = DeriveRelativeLabel(full_path, parent_full);
      nodes.push_back(
          common::HierarchyNode{
              .kind = common::HierarchyNode::kGenerate,
              .parent_node_index = parent_node_index,
              .label = std::move(label),
              .full_path = full_path,
          });
      BuildHierarchyNodes(
          block, scope_idx, all_instances, instance_to_index, nodes);
    } else if (member.kind == slang::ast::SymbolKind::GenerateBlockArray) {
      const auto& array = member.as<slang::ast::GenerateBlockArraySymbol>();
      for (const auto* entry : array.entries) {
        if (entry->isUninstantiated) continue;
        auto scope_idx = static_cast<uint32_t>(nodes.size());
        std::string full_path = std::string(entry->getHierarchicalPath());
        std::string label = DeriveRelativeLabel(full_path, parent_full);
        nodes.push_back(
            common::HierarchyNode{
                .kind = common::HierarchyNode::kGenerate,
                .parent_node_index = parent_node_index,
                .label = std::move(label),
                .full_path = full_path,
            });
        BuildHierarchyNodes(
            *entry, scope_idx, all_instances, instance_to_index, nodes);
      }
    }
  }
}

// Prepare ownership-shaped lowering inputs from structural discovery.
// One CollectScopeMembers walk per instance, then immediate partitioning
// into behavioral (body) and registration (instance) inputs.
// CollectedMembers is internal to this function -- never crosses the
// LowerModuleBody / CollectModuleInstance boundary.
struct PreparedModuleInputs {
  std::vector<BodyLoweringInput> body_inputs;
  std::vector<InstanceRegistrationInput> instance_inputs;
};

auto PrepareModuleLoweringInputs(
    const std::vector<const slang::ast::InstanceSymbol*>& all_instances,
    const common::SpecializationMap& spec_map, SymbolRegistrar& registrar)
    -> PreparedModuleInputs {
  // Identify representatives so we can build body inputs during the walk.
  std::vector<int> rep_group_index(all_instances.size(), -1);
  for (size_t g = 0; g < spec_map.groups.size(); ++g) {
    rep_group_index[spec_map.groups[g].instance_indices[0]] =
        static_cast<int>(g);
  }

  PreparedModuleInputs result;
  result.instance_inputs.resize(all_instances.size());
  result.body_inputs.resize(spec_map.groups.size());

  for (size_t i = 0; i < all_instances.size(); ++i) {
    CollectedMembers members;
    {
      ScopeGuard scope_guard(registrar, ScopeKind::kModule);
      CollectScopeMembers(all_instances[i]->body, registrar, members);
    }

    // For representatives, build body lowering input from behavioral members.
    if (rep_group_index[i] >= 0) {
      auto g = static_cast<size_t>(rep_group_index[i]);
      result.body_inputs[g].processes = std::move(members.processes);
      result.body_inputs[g].continuous_assigns =
          std::move(members.continuous_assigns);
      result.body_inputs[g].functions = std::move(members.functions);
      result.body_inputs[g].tasks = std::move(members.tasks);

      // Prepare var_inits from representative's variables.
      // Iterate before moving variables into instance_inputs.
      for (const auto* var : members.variables) {
        SymbolId sym = registrar.Lookup(*var);
        if (sym) {
          if (const auto* init = var->getInitializer()) {
            result.body_inputs[g].var_inits.push_back(
                BodyLoweringInput::VarInit{.target = sym, .initializer = init});
          }
        }
      }
    }

    // Always build instance registration input (per-instance).
    result.instance_inputs[i] = InstanceRegistrationInput{
        .variables = std::move(members.variables),
        .nets = std::move(members.nets),
        .parameters = std::move(members.parameters),
    };
  }

  return result;
}

}  // namespace

auto LowerDesign(
    slang::ast::Compilation& compilation, SymbolRegistrar& registrar,
    Context* ctx) -> DesignLoweringOutput {
  // Initialize the builtin semantic type catalog (Phase 0).
  ctx->builtin_types.Init(*ctx->type_arena);

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

  // Build instance-to-sorted-index map for hierarchy node building.
  std::unordered_map<const slang::ast::InstanceSymbol*, uint32_t>
      instance_to_sorted_index;
  for (uint32_t i = 0; i < all_instances.size(); ++i) {
    instance_to_sorted_index[all_instances[i]] = i;
  }

  // Build hierarchy-node table from slang AST. This captures both
  // generate scope nodes and instance nodes in parent-before-child order.
  // Root instance labels equal their full_path (parent is empty).
  std::vector<common::HierarchyNode> hierarchy_nodes;
  for (const auto* inst : root.topInstances) {
    auto top_idx = static_cast<uint32_t>(hierarchy_nodes.size());
    std::string full_path = std::string(inst->getHierarchicalPath());
    std::string label = DeriveRelativeLabel(full_path, {});
    hierarchy_nodes.push_back(
        common::HierarchyNode{
            .kind = common::HierarchyNode::kInstance,
            .parent_node_index = UINT32_MAX,
            .label = std::move(label),
            .full_path = full_path,
            .instance_index = instance_to_sorted_index.at(inst),
        });
    BuildHierarchyNodes(
        inst->body, top_idx, all_instances, instance_to_sorted_index,
        hierarchy_nodes);
  }

  // Build specialization groups from compile-owned body facts.
  // No parameter classification -- grouping is self-contained.
  common::SpecializationMap spec_map = BuildSpecializationMap(all_instances);

  // Build per-definition child-coord map from repertoire descriptors.
  // One entry per unique definition. Each entry maps child instance names
  // to their durable repertoire coordinates within the parent definition.
  common::ChildCoordMap child_coord_map;
  {
    std::unordered_map<const slang::ast::DefinitionSymbol*, common::ModuleDefId>
        seen_defs;
    for (size_t i = 0; i < all_instances.size(); ++i) {
      const auto& def = all_instances[i]->body.getDefinition();
      auto def_id = spec_map.spec_id_by_instance[i].def_id;
      auto [it, inserted] = seen_defs.try_emplace(&def, def_id);
      if (!inserted) continue;
      auto desc = BuildDefinitionRepertoireDesc(all_instances[i]->body);
      std::vector<common::ChildCoordEntry> entries;
      for (const auto& artifact : desc.artifacts) {
        if (artifact.kind != RepertoireArtifactKind::kChildInstance) continue;
        const auto& child_desc =
            std::get<ChildInstanceArtifactDesc>(artifact.payload);
        entries.push_back(
            common::ChildCoordEntry{
                .inst_name = child_desc.inst_name, .coord = artifact.coord});
      }
      if (!entries.empty()) {
        child_coord_map[def_id] = std::move(entries);
      }
    }
  }

  // Derive per-instance parameter transmission from within-group variance.
  ParamTransmissionTable transmission =
      DeriveParamTransmission(spec_map, all_instances);

  // Phase 0: Register all module declarations (creation allowed).
  // Storage class is derived from transmission, not from param roles.
  for (const auto* instance : all_instances) {
    RegisterModuleDeclarations(*instance, registrar, transmission, ctx);
  }

  // Lower port bindings to HIR (after Phase 0, symbols are registered)
  DesignBindingPlan binding_plan =
      LowerPortBindings(pending_bindings, registrar, ctx);

  // Lower packages (independent of instances).
  // Track package pointer -> elements index for DPI export attachment.
  std::unordered_map<const slang::ast::Symbol*, size_t> pkg_to_element_idx;
  for (const slang::ast::PackageSymbol* pkg : compilation.getPackages()) {
    if (pkg->name == "std") {
      continue;
    }
    size_t idx = elements.size();
    elements.emplace_back(LowerPackage(*pkg, registrar, ctx));
    pkg_to_element_idx[pkg] = idx;
  }

  auto [body_inputs, instance_inputs] =
      PrepareModuleLoweringInputs(all_instances, spec_map, registrar);

  // Phase 0b: seed all body-reachable and deterministic synthetic body types.
  for (size_t g = 0; g < spec_map.groups.size(); ++g) {
    SeedBodyTypes(body_inputs[g], ctx);
  }

  // Seed decision observation types (i8, i16) before freeze.
  // These are not SV types but are needed by MIR decision lowering.
  ctx->type_arena->Intern(
      TypeKind::kIntegral,
      IntegralInfo{.bit_width = 8, .is_signed = false, .is_four_state = false});
  ctx->type_arena->Intern(
      TypeKind::kIntegral,
      IntegralInfo{
          .bit_width = 16, .is_signed = false, .is_four_state = false});

  // TypeArena is now a shared read-only artifact for Phase 1.
  ctx->type_arena->Freeze();

  // Phase 1: Lower one shared body per specialization group.
  // Each call returns an isolated BodyLoweringResult with body-local
  // diagnostics. No body-lowering path writes to the pipeline sink.
  std::vector<BodyLoweringResult> body_results;
  body_results.reserve(spec_map.groups.size());

  for (size_t g = 0; g < spec_map.groups.size(); ++g) {
    uint32_t rep_idx = spec_map.groups[g].instance_indices[0];
    body_results.push_back(LowerModuleBody(
        *all_instances[rep_idx], body_inputs[g], registrar, ctx));
  }

  // Phase 2: Merge diagnostics and assemble module bodies in stable
  // group order. This is the deterministic assembly step.
  std::vector<hir::ModuleBody> module_bodies;
  module_bodies.reserve(body_results.size());
  std::vector<common::BodyTimeScale> body_timescale_table;
  body_timescale_table.reserve(body_results.size());

  std::vector<hir::ModuleBodyId> body_id_by_instance(all_instances.size());
  // Track representative instance body scope -> body index for DPI export.
  std::unordered_map<const slang::ast::Symbol*, size_t> scope_to_body_idx;

  for (size_t g = 0; g < body_results.size(); ++g) {
    for (auto& diag : body_results[g].diagnostics) {
      ctx->sink->Report(std::move(diag));
    }

    hir::ModuleBodyId body_id{static_cast<uint32_t>(module_bodies.size())};
    module_bodies.push_back(std::move(body_results[g].body));

    // Extract timescale from the representative instance's scope.
    uint32_t rep_idx = spec_map.groups[g].instance_indices[0];
    auto resolved =
        ResolveScopeTimeScale(all_instances[rep_idx]->body.getTimeScale());
    body_timescale_table.push_back(
        common::BodyTimeScale{
            .body_id = body_id.value,
            .unit_power = static_cast<int8_t>(resolved.unit_power),
            .precision_power = static_cast<int8_t>(resolved.precision_power),
        });

    // Map all instance body scopes in this group to the same body index.
    for (uint32_t idx : spec_map.groups[g].instance_indices) {
      body_id_by_instance[idx] = body_id;
      scope_to_body_idx[&all_instances[idx]->body] = body_id.value;
    }
  }

  // Phase 1b: Collect per-instance data (lightweight instance records).
  for (size_t i = 0; i < all_instances.size(); ++i) {
    auto mod = CollectModuleInstance(
        *all_instances[i], instance_inputs[i], registrar, ctx,
        spec_map.spec_id_by_instance[i].def_id, body_id_by_instance[i]);
    elements.emplace_back(std::move(mod));
  }

  // Build instance table for %m support.
  // Index order matches all_instances (sorted by hierarchical path).
  mir::InstanceTable instance_table;
  instance_table.entries.reserve(all_instances.size());
  for (const auto* instance : all_instances) {
    instance_table.entries.push_back(
        mir::InstanceEntry{
            .full_path = std::string(instance->getHierarchicalPath()),
            .inst_name = std::string(instance->name),
            .instance_sym = registrar.Lookup(*instance),
        });
  }

  // Collect DPI export declarations from slang's resolved export list.
  // Slang provides a named struct with subroutine, cIdentifier, and
  // syntax provenance -- no manual resolution needed.
  // ABI type classification is produced as a separate artifact
  // (DpiExportSignatureCache) for MIR consumption.
  hir::DpiExportSignatureCache export_sig_cache;

  for (const auto& dpi_export : compilation.getDPIExports()) {
    if (dpi_export.subroutine == nullptr) {
      throw common::InternalError(
          "LowerDesign",
          "slang returned DPI export entry with null subroutine");
    }

    const auto& sub = *dpi_export.subroutine;
    const std::string& c_name = dpi_export.cIdentifier;
    SourceSpan span = GetDpiExportSpan(dpi_export, sub, ctx);

    bool is_task = sub.subroutineKind == slang::ast::SubroutineKind::Task;
    bool is_context = sub.flags.has(slang::ast::MethodFlags::DPIContext);

    const auto& owner =
        GetFirstNonTransparentAncestor(sub.getParentScope()->asSymbol());
    if (owner.kind != slang::ast::SymbolKind::InstanceBody &&
        owner.kind != slang::ast::SymbolKind::Package) {
      throw common::InternalError(
          "LowerDesign", std::format(
                             "unexpected DPI export owner scope '{}' for '{}'",
                             std::string(owner.name), std::string(sub.name)));
    }
    bool is_module_scoped = owner.kind == slang::ast::SymbolKind::InstanceBody;

    // Scope/kind gating: reject unsupported forms with diagnostics.
    if (is_task) {
      if (!is_module_scoped) {
        ctx->sink->Unsupported(
            span,
            "DPI-C export task not yet supported: "
            "package-scoped export tasks",
            UnsupportedCategory::kFeature);
        continue;
      }
      auto check = CheckNonSuspendingTask(sub, *ctx->source_mapper);
      if (!check.ok) {
        ctx->sink->Unsupported(
            check.offending_span.value_or(span),
            std::format(
                "DPI-C export task not yet supported: {}", check.reason),
            UnsupportedCategory::kFeature);
        continue;
      }
    }
    if (is_context) {
      ctx->sink->Unsupported(
          span, "DPI-C context export functions not yet supported",
          UnsupportedCategory::kFeature);
      continue;
    }

    SymbolId symbol = registrar.Lookup(sub);
    if (!symbol) {
      continue;
    }

    hir::DpiExportDecl decl{
        .symbol = symbol,
        .span = span,
        .c_name = c_name,
        .is_task = is_task,
        .is_context = is_context,
        .is_module_scoped = is_module_scoped,
    };

    auto sig = ClassifyExportSignature(sub, span, registrar, ctx);
    if (!sig) {
      continue;
    }
    auto [cache_it, cache_inserted] =
        export_sig_cache.emplace(symbol, std::move(*sig));
    if (!cache_inserted) {
      throw common::InternalError(
          "LowerDesign",
          std::format(
              "duplicate DPI export signature cache entry for symbol {}",
              symbol.value));
    }

    AttachDpiExportDecl(
        owner, std::move(decl), pkg_to_element_idx, scope_to_body_idx, elements,
        module_bodies);
  }

  return DesignLoweringOutput{
      .hir =
          DesignLoweringResult{
              .design =
                  hir::Design{
                      .elements = std::move(elements),
                      .module_bodies = std::move(module_bodies),
                      .callable_signatures = {},
                  },
          },
      .composition =
          DesignCompositionMetadata{
              .binding_plan = std::move(binding_plan),
              .specialization_map = std::move(spec_map),
              .instance_table = std::move(instance_table),
              .body_timescales = std::move(body_timescale_table),
              .child_coord_map = std::move(child_coord_map),
              .hierarchy_nodes = std::move(hierarchy_nodes),
              .dpi_export_signatures = std::move(export_sig_cache),
          },
  };
}

}  // namespace lyra::lowering::ast_to_hir
