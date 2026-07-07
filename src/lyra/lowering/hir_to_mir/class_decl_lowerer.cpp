#include "lyra/lowering/hir_to_mir/class_decl_lowerer.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <format>
#include <optional>
#include <string>
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
  ModuleLowerer& module = *module_;
  const hir::ClassDecl& hir_class = *hir_class_;

  const mir::TypeId self_pointer_type = module.Unit().types.PointerTo(
      object_type_, mir::PointerOwnership::kBorrowed);

  mir::Class mir_class{
      .name = hir_class.name,
      .base = std::nullopt,
      .self_pointer_type = self_pointer_type,
      .time_resolution = {},
      .ctor_prefix_params = {},
      .params = {},
      .fields = {},
      .constructor = {},
      .contained = {},
      .structs = {},
      .methods = {},
      .type_aliases = {}};

  mir::CallableCode ctor_code;
  CallableBindings ctor_bindings(module.Unit(), ctor_code);
  const mir::LocalId self_id = ctor_bindings.Declare(
      BindingOriginId::Receiver(),
      mir::LocalDecl{.name = "self", .type = self_pointer_type});
  mir::Block& ctor_block = ctor_code.body;
  ScopeChainNode scope_link{};
  const WalkFrame frame = WalkFrame{}
                              .WithClass(&mir_class, scope_link)
                              .WithBlock(&ctor_block)
                              .WithBindings(&ctor_bindings);

  // A class property owns its storage directly -- it is not an observable cell,
  // so it is a plain value-typed member and its construction-time default is a
  // plain assignment through `self`, not an observable `Set`.
  for (const auto& field : hir_class.fields) {
    const mir::TypeId field_type = module.TranslateType(field.type);
    const mir::FieldId field_id = mir_class.fields.Add(
        mir::FieldDecl{.name = field.name, .type = field_type});
    const mir::ExprId default_id = ctor_block.exprs.Add(
        BuildDefaultValueFromHir(module, frame, field.type));
    const mir::ExprId self_ref =
        ctor_block.exprs.Add(MakeSelfRefExpr(frame, self_pointer_type));
    const mir::ExprId target = ctor_block.exprs.Add(
        mir::MakeFieldAccessExpr(self_ref, field_id, field_type));
    const mir::ExprId assign = ctor_block.exprs.Add(
        mir::MakeAssignExpr(target, default_id, field_type));
    ctor_block.AppendStmt(mir::ExprStmt{.expr = assign});
  }

  // Pre-declare each method's static-lifetime body locals as members on
  // this class before any method body lowers. A static local has per-
  // instance storage that outlives every activation of its body (LRM
  // 13.3.1); an SV class has no named-procedural-block hierarchy, so every
  // static lives directly on the enclosing SV class. The mangled name
  // (`<method>__<source>_<hir_id>`) keeps sibling methods that reuse a
  // source identifier from colliding on the field arena; the `hir_id`
  // suffix distinguishes nested-block reuses too.
  //
  // The materialization table is empty because there are no procedural-
  // storage scopes inside class methods; the per-method plans still need a
  // reference for the API.
  ProceduralScopeMaterializationTable method_scopes;
  std::vector<CallableStoragePlan> method_plans;
  method_plans.reserve(hir_class.methods.size());
  for (const auto& method : hir_class.methods) {
    std::vector<std::optional<StaticStoragePlacement>> placements(
        method.body.procedural_vars.size());
    for (std::size_t j = 0; j < method.body.procedural_vars.size(); ++j) {
      const hir::ProceduralVarId var_id{static_cast<std::uint32_t>(j)};
      const auto& v = method.body.procedural_vars.Get(var_id);
      if (v.lifetime != hir::VariableLifetime::kStatic) {
        continue;
      }
      const std::string mangled =
          std::format("{}__{}_{}", method.name, v.name, j);
      const mir::TypeId type = module.TranslateType(v.type);
      const mir::FieldId mid =
          mir_class.fields.Add(mir::FieldDecl{.name = mangled, .type = type});
      placements[j] = StaticStoragePlacement{
          .owner = StorageOwner{EnclosingClass{}}, .field = mid};
    }
    method_plans.emplace_back(method_scopes, std::move(placements));
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
        WalkFrame{}.WithClass(&mir_class, method_link);
    ProcessLowerer method_lowerer(
        module, nullptr, mir_class.time_resolution, method.body, method.name,
        mir::MethodVisibility::kPublic, method_owner_frame, method_plans[i]);
    auto method_or = method_lowerer.Run(method);
    if (!method_or) return std::unexpected(std::move(method_or.error()));
    mir_class.methods.Add(*std::move(method_or));
    for (const auto& pending : method_lowerer.TakePendingStaticInitializers()) {
      auto integ = IntegratePendingStaticInitializer(
          method_lowerer, method.body, frame, pending);
      if (!integ) return std::unexpected(std::move(integ.error()));
    }
  }

  ctor_code.params = {self_id};
  ctor_code.result_type = module.Unit().builtins.void_type;
  mir_class.constructor = std::move(ctor_code);

  return mir_class;
}

}  // namespace lyra::lowering::hir_to_mir
