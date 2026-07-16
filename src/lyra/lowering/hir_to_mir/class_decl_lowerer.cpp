#include "lyra/lowering/hir_to_mir/class_decl_lowerer.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/callable_storage_plan.hpp"
#include "lyra/lowering/hir_to_mir/declaration_initializer.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

auto ClassDeclLowerer::Run() -> diag::Result<mir::Class> {
  UnitLowerer& unit_lowerer = *owner_;
  const hir::ClassDecl& hir_class = *hir_class_;

  const mir::TypeId self_pointer_type = unit_lowerer.Unit().types.PointerTo(
      object_type_, mir::PointerOwnership::kBorrowed);

  // An SV class extends another SV class of this unit; the base identity is
  // its intra-unit class id, resolved through the unit's class registry.
  std::optional<mir::ClassRef> base_ref;
  if (hir_class.base.has_value()) {
    base_ref = mir::ClassRef{mir::IntraUnitClassRef{
        .class_id = unit_lowerer.TranslateClass(*hir_class.base)}};
  }

  mir::Class mir_class{
      .name = hir_class.name,
      .base = base_ref,
      .is_scope_tree_node = false,
      .is_final = false,
      .self_pointer_type = self_pointer_type,
      .time_resolution = {},
      .fields = {},
      .constructor = {},
      .contained = {},
      .structs = {},
      .methods = {},
      .static_constants = {},
      .static_callables = {},
      .type_aliases = {}};

  // The constructor's callable code is built in a local first, then handed
  // to the class's method storage last so its id sits after every
  // declaration-ordered SV method -- each SV method keeps its natural
  // declaration index.
  mir::CallableCode ctor_code;
  CallableBindings ctor_bindings(unit_lowerer.Unit(), ctor_code);
  const mir::LocalId self_id = ctor_bindings.Declare(
      BindingOriginId::Receiver(),
      mir::LocalDecl{.name = "self", .type = self_pointer_type});
  mir::Block& ctor_block = ctor_code.body;
  ScopeChainNode scope_link{};
  const WalkFrame frame = WalkFrame{}
                              .WithClass(&mir_class, class_id_, scope_link)
                              .WithBlock(&ctor_block)
                              .WithBindings(&ctor_bindings);

  // Declare each class property (LRM 8.4) as a value-typed member -- a property
  // owns its storage directly and is not an observable cell. Declaration order
  // is preserved so a receiver-relative property reference in an initializer or
  // a method body indexes the same member.
  std::vector<mir::FieldId> field_ids;
  std::vector<mir::TypeId> field_types;
  field_ids.reserve(hir_class.fields.size());
  field_types.reserve(hir_class.fields.size());
  for (const auto& field : hir_class.fields) {
    const mir::TypeId field_type = unit_lowerer.TranslateType(field.type);
    field_ids.push_back(mir_class.fields.Add(
        mir::FieldDecl{.name = field.name, .type = field_type}));
    field_types.push_back(field_type);
  }

  // Pre-declare a callable's static-lifetime body locals as members on this
  // class before any body lowers. A static local has per-instance storage that
  // outlives every activation of its body (LRM 13.3.1); an SV class has no
  // named-procedural-block hierarchy, so every static lives directly on the
  // enclosing SV class. The mangled name (`<callable>__<source>_<hir_id>`)
  // keeps sibling callables that reuse a source identifier from colliding on
  // the field arena; the `hir_id` suffix distinguishes nested-block reuses too.
  // These trail the property members, so property indices stay stable.
  //
  // The materialization table is empty because there are no procedural-storage
  // scopes inside class bodies; the per-callable plans still need a reference
  // for the API.
  ProceduralScopeMaterializationTable class_scopes;
  const auto plan_static_storage = [&](std::string_view callable_name,
                                       const hir::ProceduralBody& body) {
    std::vector<std::optional<StaticStoragePlacement>> placements(
        body.procedural_vars.size());
    for (std::size_t j = 0; j < body.procedural_vars.size(); ++j) {
      const hir::ProceduralVarId var_id{static_cast<std::uint32_t>(j)};
      const auto& v = body.procedural_vars.Get(var_id);
      if (v.lifetime != hir::VariableLifetime::kStatic) {
        continue;
      }
      const std::string mangled =
          std::format("{}__{}_{}", callable_name, v.name, j);
      const mir::TypeId type = unit_lowerer.TranslateType(v.type);
      const mir::FieldId mid =
          mir_class.fields.Add(mir::FieldDecl{.name = mangled, .type = type});
      placements[j] = StaticStoragePlacement{
          .owner = StorageOwner{EnclosingClass{}}, .field = mid};
    }
    return placements;
  };

  std::vector<CallableStoragePlan> method_plans;
  method_plans.reserve(hir_class.methods.size());
  for (const auto& method : hir_class.methods) {
    method_plans.emplace_back(
        class_scopes, plan_static_storage(method.name, method.body));
  }

  const hir::SubroutineDecl& ctor = hir_class.constructor;
  const CallableStoragePlan ctor_plan(
      class_scopes, plan_static_storage("<ctor>", ctor.body));
  ProcessLowerer ctor_lowerer(
      unit_lowerer, nullptr, mir_class.time_resolution, ctor.body, "<ctor>",
      mir::MethodVisibility::kInternal, frame, ctor_plan);

  // Initialize each property in declaration order before the constructor body
  // runs (LRM 8.7): a property with an explicit initializer takes that value --
  // lowered through the constructor lowerer so a property read resolves against
  // the receiver -- and one without takes its type's Table 7-1 default. The
  // ordering is the single declaration-order pass because an initializer may
  // read an earlier property whose own initialization has already run.
  for (std::size_t i = 0; i < hir_class.fields.size(); ++i) {
    const hir::ClassField& field = hir_class.fields[i];
    mir::ExprId value_id{};
    if (field.initializer.has_value()) {
      auto value_or = ctor_lowerer.LowerExpr(
          ctor.body.exprs.Get(*field.initializer), frame);
      if (!value_or) return std::unexpected(std::move(value_or.error()));
      value_id = ctor_block.exprs.Add(*std::move(value_or));
    } else {
      value_id = ctor_block.exprs.Add(
          BuildDefaultValueFromHir(unit_lowerer, frame, field.type));
    }
    const mir::ExprId self_ref =
        ctor_block.exprs.Add(MakeSelfRefExpr(frame, self_pointer_type));
    const mir::ExprId target = ctor_block.exprs.Add(
        mir::MakeFieldAccessExpr(
            self_ref,
            mir::FieldTarget{.owner = class_id_, .slot = field_ids[i]},
            field_types[i]));
    const mir::ExprId assign = ctor_block.exprs.Add(
        mir::MakeAssignExpr(target, value_id, field_types[i]));
    ctor_block.AppendStmt(mir::ExprStmt{.expr = assign});
  }

  // Each instance method (LRM 8.6) is lowered as a callable this class owns: it
  // resolves the body's `self` to the managed handle, and the method's
  // declaration-order position becomes its `MethodId`, so a call site naming
  // the index reaches the same method. SV classes do not have a separate
  // Initialize lifecycle phase, so a method's pending static initializers
  // integrate into this class's constructor block (matching the per-instance
  // storage shape used for class-method statics).
  for (std::size_t i = 0; i < hir_class.methods.size(); ++i) {
    const auto& method = hir_class.methods[i];
    ScopeChainNode method_link{};
    const WalkFrame method_owner_frame =
        WalkFrame{}.WithClass(&mir_class, class_id_, method_link);
    ProcessLowerer method_lowerer(
        unit_lowerer, nullptr, mir_class.time_resolution, method.body,
        method.name, mir::MethodVisibility::kPublic, method_owner_frame,
        method_plans[i]);
    auto method_or = method_lowerer.Run(method);
    if (!method_or) return std::unexpected(std::move(method_or.error()));
    mir_class.methods.Add(*std::move(method_or));
    for (const auto& pending : method_lowerer.TakePendingStaticInitializers()) {
      auto integ = IntegratePendingStaticInitializer(
          method_lowerer, method.body, frame, pending);
      if (!integ) return std::unexpected(std::move(integ.error()));
    }
  }

  // The constructor body (LRM 8.7) runs after each property is initialized, so
  // its statements follow the field-init prologue already in the constructor
  // body. Its input formals extend the constructor signature past the receiver;
  // a synthesized default constructor contributes an empty body and no formals.
  std::vector<mir::LocalId> ctor_params{self_id};
  auto ctor_body_or =
      ctor_lowerer.LowerConstructorBodyInto(ctor, frame, ctor_params);
  if (!ctor_body_or) {
    return std::unexpected(std::move(ctor_body_or.error()));
  }
  for (const auto& pending : ctor_lowerer.TakePendingStaticInitializers()) {
    auto integ = IntegratePendingStaticInitializer(
        ctor_lowerer, ctor.body, frame, pending);
    if (!integ) return std::unexpected(std::move(integ.error()));
  }

  ctor_code.params = std::move(ctor_params);
  ctor_code.result_type = unit_lowerer.Unit().builtins.void_type;
  const mir::MethodId ctor_method_id = mir_class.methods.Add(
      mir::MethodDecl{
          .name = "<ctor>",
          .code = std::move(ctor_code),
          .overrides = std::nullopt,
          .visibility = mir::MethodVisibility::kInternal});
  mir_class.constructor = mir::ConstructorDecl{
      .method = ctor_method_id, .base_init = std::nullopt, .member_inits = {}};

  return mir_class;
}

}  // namespace lyra::lowering::hir_to_mir
