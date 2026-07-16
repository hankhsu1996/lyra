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

#include "lyra/base/overloaded.hpp"
#include "lyra/hir/class_decl.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/callable_storage_plan.hpp"
#include "lyra/lowering/hir_to_mir/declaration_initializer.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/method_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// Walks the HIR override chain starting at `method` and returns its
// participation in the class's dispatch table (LRM 8.20). Reads HIR only --
// HIR is complete before any class shape lowers, so this is order-independent
// with respect to sibling classes. The walk terminates at either an
// introducer (`overrides == nullopt && is_virtual`) or a non-participating
// method (both facts absent). Depth is bounded by inheritance depth; the
// frontend guarantees the chain is acyclic (LRM 8.13).
auto CanonicalizeVirtualDispatch(
    const UnitLowerer& unit_lowerer, const hir::SubroutineDecl& method)
    -> std::optional<mir::VirtualDispatchRole> {
  if (method.overrides.has_value()) {
    const hir::MethodRef& base_ref = *method.overrides;
    const hir::SubroutineDecl& base_method = unit_lowerer.Hir()
                                                 .classes.Get(base_ref.class_id)
                                                 .methods.Get(base_ref.method);
    const auto base_role =
        CanonicalizeVirtualDispatch(unit_lowerer, base_method);
    if (!base_role.has_value()) {
      throw InternalError(
          "CanonicalizeVirtualDispatch: HIR override references a base "
          "method the frontend did not classify as virtual");
    }
    const mir::ClassId base_mir_class =
        unit_lowerer.TranslateClass(base_ref.class_id);
    const mir::MethodId base_mir_slot{base_ref.method.value};
    return std::visit(
        Overloaded{
            [&](const mir::IntroducesVirtualSlot&) -> mir::VirtualDispatchRole {
              return mir::OverridesIntraUnitSlot{
                  .slot_owner = base_mir_class, .slot_id = base_mir_slot};
            },
            [](const mir::OverridesIntraUnitSlot& s)
                -> mir::VirtualDispatchRole { return s; }},
        *base_role);
  }
  if (method.is_virtual) {
    return mir::VirtualDispatchRole{mir::IntroducesVirtualSlot{}};
  }
  return std::nullopt;
}

// Appends a mangled static-lifetime field per static procedural var to
// `fields`, returning the placements a callable body reads to route each
// static write to its persistent slot. Static locals of a class body
// (LRM 13.3.1) live on the enclosing class since a class carries no
// procedural-scope hierarchy of its own.
auto PlanStaticStorage(
    const UnitLowerer& unit_lowerer, std::string_view callable_name,
    const hir::ProceduralBody& body,
    base::Arena<mir::FieldDecl, mir::FieldId>& fields)
    -> std::vector<std::optional<StaticStoragePlacement>> {
  std::vector<std::optional<StaticStoragePlacement>> placements(
      body.procedural_vars.size());
  for (std::size_t j = 0; j < body.procedural_vars.size(); ++j) {
    const hir::ProceduralVarId var_id{static_cast<std::uint32_t>(j)};
    const auto& v = body.procedural_vars.Get(var_id);
    if (v.lifetime != hir::VariableLifetime::kStatic) continue;
    const std::string mangled =
        std::format("{}__{}_{}", callable_name, v.name, j);
    const mir::TypeId type = unit_lowerer.TranslateType(v.type);
    const mir::FieldId mid =
        fields.Add(mir::FieldDecl{.name = mangled, .type = type});
    placements[j] = StaticStoragePlacement{
        .owner = StorageOwner{EnclosingClass{}}, .field = mid};
  }
  return placements;
}

}  // namespace

auto ClassDeclLowerer::DeclareShape() -> diag::Result<void> {
  UnitLowerer& unit_lowerer = *owner_;
  const hir::ClassDecl& hir_class = *hir_class_;

  const mir::TypeId self_pointer_type = unit_lowerer.Unit().types.PointerTo(
      object_type_, mir::PointerOwnership::kBorrowed);

  std::optional<mir::ClassRef> base_ref;
  if (hir_class.base.has_value()) {
    base_ref = mir::ClassRef{mir::IntraUnitClassRef{
        .class_id = unit_lowerer.TranslateClass(*hir_class.base)}};
  }

  mir::ClassShape shape{
      .name = hir_class.name,
      .base = base_ref,
      .self_pointer_type = self_pointer_type,
      .time_resolution = {},
      .ctor_prefix_params = {},
      .fields = {},
      .method_signatures = {},
      .contained = {},
      .type_aliases = {},
      .is_scope_tree_node = false,
      .is_final = false};

  // Property fields come first in declaration order (LRM 8.4), so an SV
  // reference to a property by its declaration index reaches the same shape
  // slot regardless of what static-lifetime storage the class also owns.
  field_ids_.reserve(hir_class.fields.size());
  for (const auto& field : hir_class.fields) {
    const mir::TypeId field_type = unit_lowerer.TranslateType(field.type);
    field_ids_.push_back(shape.fields.Add(
        mir::FieldDecl{.name = field.name, .type = field_type}));
  }

  // Each SV method's static-lifetime locals get their per-instance slot on
  // this same shape, appended after the properties, so a static write from a
  // body routes to the exact same field slot the shape declares.
  method_plans_.reserve(hir_class.methods.size());
  for (const auto& method : hir_class.methods) {
    method_plans_.emplace_back(
        class_scopes_,
        PlanStaticStorage(
            unit_lowerer, method.name, method.body, shape.fields));
  }
  ctor_plan_.emplace(
      class_scopes_,
      PlanStaticStorage(
          unit_lowerer, "<ctor>", hir_class.constructor.body, shape.fields));

  // Method signatures publish each method's canonical dispatch role. Peer
  // body lowering that names one of this class's methods reads this to pick
  // between direct and virtual invocation, with no cross-class MIR read.
  for (const auto& method : hir_class.methods) {
    shape.method_signatures.Add(
        mir::MethodSignature{
            .virtual_dispatch =
                CanonicalizeVirtualDispatch(unit_lowerer, method)});
  }

  unit_lowerer.DefineClassShape(class_id_, std::move(shape));
  return {};
}

auto ClassDeclLowerer::PopulateBodies() -> diag::Result<void> {
  UnitLowerer& unit_lowerer = *owner_;
  const hir::ClassDecl& hir_class = *hir_class_;
  const mir::ClassShape& shape = unit_lowerer.GetClassShape(class_id_);

  mir::Class mir_class{
      .name = shape.name,
      .base = shape.base,
      .is_scope_tree_node = shape.is_scope_tree_node,
      .is_final = shape.is_final,
      .self_pointer_type = shape.self_pointer_type,
      .time_resolution = shape.time_resolution,
      .fields = shape.fields,
      .constructor = {},
      .contained = shape.contained,
      .structs = {},
      .methods = {},
      .abi_adapters = {},
      .static_constants = {},
      .static_callables = {},
      .type_aliases = shape.type_aliases};

  // The constructor's callable code is built in a local first, then handed
  // to the class's method storage last so its id sits after every
  // declaration-ordered SV method -- each SV method keeps its natural
  // declaration index.
  mir::CallableCode ctor_code;
  CallableBindings ctor_bindings(unit_lowerer.Unit(), ctor_code);
  const mir::LocalId self_id = ctor_bindings.Declare(
      BindingOriginId::Receiver(),
      mir::LocalDecl{.name = "self", .type = shape.self_pointer_type});
  mir::Block& ctor_block = ctor_code.body;
  ScopeChainNode scope_link{};
  const WalkFrame frame = WalkFrame{}
                              .WithClass(&mir_class, class_id_, scope_link)
                              .WithBlock(&ctor_block)
                              .WithBindings(&ctor_bindings);

  const hir::SubroutineDecl& ctor = hir_class.constructor;
  ProcessLowerer ctor_lowerer(
      unit_lowerer, nullptr, mir_class.time_resolution, ctor.body, "<ctor>",
      mir::MethodVisibility::kInternal, frame, *ctor_plan_);

  // Register the ctor formals early so a base-constructor arg (LRM 8.7) can
  // reference them: `super.new(a * 2)` in the derived ctor reads its own `a`
  // formal, and that lookup resolves through the same procedural-var
  // registry the ctor body uses. Formals land as MIR locals appended after
  // the receiver; the base-call arg exprs and every field initializer below
  // read them through that registry.
  std::vector<mir::LocalId> ctor_params{self_id};
  auto formals_or =
      ctor_lowerer.RegisterConstructorFormals(ctor, frame, ctor_params);
  if (!formals_or) return std::unexpected(std::move(formals_or.error()));

  // Base construction (LRM 8.7): a derived class always forwards to its base
  // -- explicit `super.new(args)` when the source wrote one, an implicit
  // no-arg `super.new()` otherwise. Both cases publish stated args on the
  // constructor's base-init so the backend never falls back to its target
  // language's default-construction convention; base-constructor ordering
  // is a MIR-stated fact, not a backend convention. A class with no base
  // carries no base-init.
  std::optional<mir::BaseInit> base_init;
  if (hir_class.base_call.has_value()) {
    std::vector<mir::ExprId> lowered;
    lowered.reserve(hir_class.base_call->arguments.size());
    for (const hir::ExprId arg : hir_class.base_call->arguments) {
      auto arg_or = ctor_lowerer.LowerExpr(ctor.body.exprs.Get(arg), frame);
      if (!arg_or) return std::unexpected(std::move(arg_or.error()));
      lowered.push_back(ctor_block.exprs.Add(*std::move(arg_or)));
    }
    base_init = mir::BaseInit{.args = std::move(lowered)};
  } else if (hir_class.base.has_value()) {
    base_init = mir::BaseInit{.args = {}};
  }

  // Initialize each property in declaration order before the constructor body
  // runs (LRM 8.7): a property with an explicit initializer takes that value --
  // lowered through the constructor lowerer so a property read resolves against
  // the receiver -- and one without takes its type's Table 7-1 default. The
  // ordering is the single declaration-order pass because an initializer may
  // read an earlier property whose own initialization has already run.
  for (std::size_t i = 0; i < hir_class.fields.size(); ++i) {
    const hir::ClassField& field = hir_class.fields[i];
    const mir::TypeId field_type = mir_class.fields.Get(field_ids_[i]).type;
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
        ctor_block.exprs.Add(MakeSelfRefExpr(frame, shape.self_pointer_type));
    const mir::ExprId target = ctor_block.exprs.Add(
        mir::MakeFieldAccessExpr(
            self_ref,
            mir::FieldTarget{.owner = class_id_, .slot = field_ids_[i]},
            field_type));
    const mir::ExprId assign =
        ctor_block.exprs.Add(mir::MakeAssignExpr(target, value_id, field_type));
    ctor_block.AppendStmt(mir::ExprStmt{.expr = assign});
  }

  // Each instance method (LRM 8.6) is lowered as a callable this class owns:
  // it resolves the body's `self` to the managed handle, and the method's
  // declaration-order position becomes its `MethodId`, so a call site naming
  // the index reaches the same method. SV classes do not have a separate
  // Initialize lifecycle phase, so a method's pending static initializers
  // integrate into this class's constructor block (matching the per-instance
  // storage shape used for class-method statics).
  for (std::size_t i = 0; i < hir_class.methods.size(); ++i) {
    const hir::MethodId method_id{static_cast<std::uint32_t>(i)};
    const auto& method = hir_class.methods.Get(method_id);
    ScopeChainNode method_link{};
    const WalkFrame method_owner_frame =
        WalkFrame{}.WithClass(&mir_class, class_id_, method_link);
    ProcessLowerer method_lowerer(
        unit_lowerer, nullptr, mir_class.time_resolution, method.body,
        method.name, mir::MethodVisibility::kPublic, method_owner_frame,
        method_plans_[i]);
    auto method_or = method_lowerer.Run(method);
    if (!method_or) return std::unexpected(std::move(method_or.error()));
    method_or->virtual_dispatch =
        shape.method_signatures.Get(mir::MethodId{method_id.value})
            .virtual_dispatch;
    mir_class.methods.Add(*std::move(method_or));
    for (const auto& pending : method_lowerer.TakePendingStaticInitializers()) {
      auto integ = IntegratePendingStaticInitializer(
          method_lowerer, method.body, frame, pending);
      if (!integ) return std::unexpected(std::move(integ.error()));
    }
  }

  // The constructor body statements (LRM 8.7) run after base construction
  // and property initialization, so they follow both the field-init prologue
  // and the base-call arg evaluation already emitted into the ctor block.
  auto body_or = ctor_lowerer.LowerConstructorBodyInto(frame);
  if (!body_or) return std::unexpected(std::move(body_or.error()));
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
          .virtual_dispatch = std::nullopt,
          .visibility = mir::MethodVisibility::kInternal});
  mir_class.constructor = mir::ConstructorDecl{
      .method = ctor_method_id,
      .base_init = std::move(base_init),
      .member_inits = {}};

  unit_lowerer.Unit().DefineClass(class_id_, std::move(mir_class));
  return {};
}

}  // namespace lyra::lowering::hir_to_mir
