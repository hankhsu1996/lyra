#include "lyra/lowering/hir_to_mir/class_decl_lowerer.hpp"

#include <expected>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <vector>

#include "lyra/base/overloaded.hpp"
#include "lyra/hir/class_decl.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/callee_interface.hpp"
#include "lyra/lowering/hir_to_mir/class_shape.hpp"
#include "lyra/lowering/hir_to_mir/declaration_initializer.hpp"
#include "lyra/lowering/hir_to_mir/declared_scope.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/static_var_binding.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// Walks the HIR override chain starting at `method` and returns its
// participation in the class's dispatch table (LRM 8.20). The walk terminates
// at either an introducer (`overrides == nullopt && is_virtual`) or a
// non-participating method (both facts absent). Depth is bounded by
// inheritance depth; the frontend guarantees the chain is acyclic (LRM 8.13).
//
// An intra-unit override names the slot by the identity the declaring class
// handed it, which is why a class is declared only after the classes it
// extends: the answer is read off the base's own declaration rather than
// worked out again from the shape of the source.
auto CanonicalizeVirtualDispatch(
    UnitLowerer& unit_lowerer, const hir::SubroutineDecl& method)
    -> std::optional<mir::VirtualDispatchRole> {
  if (method.overrides.has_value()) {
    if (const auto* ext =
            std::get_if<hir::ExternalClassMethodTarget>(&*method.overrides)) {
      // A slot introduced in another unit is canonically owned there; this
      // unit records the override by the (unit, class, method) name triple
      // and reaches the base's dispatch machinery through the link-time
      // include of the declaring unit.
      return mir::VirtualDispatchRole{
          unit_lowerer.MakeExternalMethodOverride(*ext)};
    }
    const auto& local_ref =
        std::get<hir::LocalClassMethodTarget>(*method.overrides);
    const hir::SubroutineDecl& base_method = unit_lowerer.Hir()
                                                 .classes.Get(local_ref.owner)
                                                 .methods.Get(local_ref.method);
    const auto base_role =
        CanonicalizeVirtualDispatch(unit_lowerer, base_method);
    if (!base_role.has_value()) {
      throw InternalError(
          "CanonicalizeVirtualDispatch: HIR override references a base "
          "method the frontend did not classify as virtual");
    }
    const mir::ClassId base_mir_class =
        unit_lowerer.TranslateClass(local_ref.owner);
    const mir::CallableId base_mir_slot =
        unit_lowerer.TranslateMethod(local_ref.owner, local_ref.method);
    return std::visit(
        Overloaded{
            [&](const mir::IntroducesVirtualSlot&) -> mir::VirtualDispatchRole {
              return mir::OverridesIntraUnitSlot{
                  .slot_owner = base_mir_class, .slot_id = base_mir_slot};
            },
            [](const mir::OverridesIntraUnitSlot& s)
                -> mir::VirtualDispatchRole { return s; },
            [](const mir::OverridesExternalSlot& e)
                -> mir::VirtualDispatchRole { return e; }},
        *base_role);
  }
  if (method.is_virtual) {
    return mir::VirtualDispatchRole{mir::IntroducesVirtualSlot{}};
  }
  return std::nullopt;
}

// One body of the class and the static-lifetime locals it declared, paired so
// the design-init body can apply each initializer in the arena the expression
// was written in.
struct BodyStatics {
  const hir::ProceduralBody* body;
  std::string_view name;
  std::span<const StaticVarBinding> statics;
};

// Lowers the class's design-init body (LRM 8.9 / 10.5): a receiver-less,
// formal-less callable code the runtime invokes once at program startup,
// before any initial or always procedure runs. Everything the class owns for
// itself is brought up here. Each source-written static property initializer
// lowers to an `AssignExpr(StaticPropertyRef, value)` statement in declaration
// order; a static property without a source initializer takes its type's Table
// 7-1 default and gets no statement here. A static-lifetime local of a body
// follows, its cell being the class's for the same reason.
auto LowerStaticInit(
    UnitLowerer& unit_lowerer, const hir::ClassDecl& hir_class,
    const ClassShape& shape, mir::Class& mir_class, mir::ClassId class_id,
    const DeclaredScopes& scopes, std::span<const BodyStatics> body_statics)
    -> diag::Result<mir::CallableCode> {
  mir::CallableCode code = mir::CallableCode::Defined();
  CallableBindings bindings(unit_lowerer.Unit(), code);
  code.params = {};
  code.result_type = unit_lowerer.Unit().builtins.void_type;
  mir::Block& block = code.Body();
  ScopeChainNode link{};
  const WalkFrame frame = WalkFrame{}
                              .WithClass(&mir_class, class_id, link)
                              .WithBlock(&block)
                              .WithBindings(&bindings);
  ProcessLowerer lowerer(
      unit_lowerer, nullptr, mir_class.time_resolution, hir_class.static_init,
      "<static_init>", frame, scopes, {});

  for (const hir::StaticPropertyInit& init : hir_class.static_property_inits) {
    const hir::Expr& hir_value = hir_class.static_init.exprs.Get(init.value);
    auto value_or = lowerer.LowerExpr(hir_value, frame);
    if (!value_or) return std::unexpected(std::move(value_or.error()));
    const mir::ExprId value_id = block.exprs.Add(*std::move(value_or));

    const mir::StaticPropertyId mir_prop_id =
        shape.static_property_translation.Get(init.target);
    const mir::TypeId prop_type =
        mir_class.static_properties.Get(mir_prop_id).type;
    const mir::ExprId target = block.exprs.Add(
        mir::Expr{
            .data =
                mir::StaticPropertyRef{.owner = class_id, .prop = mir_prop_id},
            .type = prop_type});
    const mir::ExprId assign =
        block.exprs.Add(mir::MakeAssignExpr(target, value_id, prop_type));
    block.AppendStmt(mir::ExprStmt{.expr = assign});
  }

  // LRM 6.21 applies such an initializer once before any process starts, rather
  // than on each entry to the body that declares it. Each body is lowered
  // against its own statics, since a declaration's identity is scoped to the
  // body's arena.
  for (const BodyStatics& body : body_statics) {
    ProcessLowerer body_lowerer(
        unit_lowerer, nullptr, mir_class.time_resolution, *body.body,
        std::string{body.name}, frame, scopes, body.statics);
    for (const StaticVarBinding& binding : body.statics) {
      auto integ = IntegrateStaticInitializer(
          body_lowerer, *body.body, frame, frame, binding);
      if (!integ) return std::unexpected(std::move(integ.error()));
    }
  }
  return code;
}

}  // namespace

auto ClassDeclLowerer::DeclareShape() -> diag::Result<void> {
  UnitLowerer& unit_lowerer = *owner_;
  const hir::ClassDecl& hir_class = *hir_class_;

  const mir::TypeId self_pointer_type = unit_lowerer.Unit().types.Intern(
      mir::Type{mir::PointerType{
          .pointee = object_type_,
          .ownership = mir::PointerOwnership::kBorrowed,
          .mutability = mir::Mutability::kMutable}});

  std::optional<mir::ClassRef> base_ref;
  if (hir_class.base.has_value()) {
    base_ref = unit_lowerer.TranslateClassRef(*hir_class.base);
  }
  std::vector<mir::ClassRef> implements;
  implements.reserve(hir_class.implements.size());
  for (const hir::ClassRef& iface : hir_class.implements) {
    implements.push_back(unit_lowerer.TranslateClassRef(iface));
  }

  ClassShape shape{
      .name = hir_class.name,
      .base = base_ref,
      .implements = std::move(implements),
      .self_pointer_type = self_pointer_type,
      .time_resolution = {},
      .ctor_prefix_params = {},
      .fields = {},
      .static_properties = {},
      .callable_signatures = {},
      .field_translation = {},
      .static_property_translation = {},
      .contained = {},
      .is_final = false,
      .is_interface_class = hir_class.is_interface_class};

  // A property (LRM 8.4) becomes one field of the class, so where a property
  // lands is a fact only this loop knows. It is recorded as the loop goes;
  // nothing downstream recomputes it.
  shape.field_translation =
      base::Translation<hir::FieldId, mir::FieldId>{hir_class.fields.size()};
  for (const auto& field : hir_class.fields) {
    const mir::TypeId field_type = unit_lowerer.TranslateType(field.type);
    shape.field_translation.Append(shape.fields.Add(
        mir::FieldDecl{.name = field.name, .type = field_type}));
  }

  // Static properties (LRM 8.9) enter the shape's type-associated arena in
  // declaration order, recorded the same way and for the same reason. That
  // arena also takes what the class's bodies keep for the whole class, so a
  // property's position in it is not its position in the source.
  shape.static_property_translation =
      base::Translation<hir::StaticPropertyId, mir::StaticPropertyId>{
          hir_class.static_properties.size()};
  for (const auto& sp : hir_class.static_properties) {
    const mir::TypeId sp_type = unit_lowerer.TranslateType(sp.type);
    shape.static_property_translation.Append(shape.static_properties.Add(
        mir::StaticPropertyDecl{.name = sp.name, .type = sp_type}));
  }

  const auto bind_statics = [&](const hir::SubroutineDecl& decl,
                                std::string_view callable_name) {
    return BindBodyStatics(
        unit_lowerer, hir_class.procedural_scopes,
        ClassStorage{.properties = &shape.static_properties}, decl.body,
        SignatureBoundVars(decl), callable_name);
  };

  // Everything a peer may need about a method before its body exists is
  // settled here, in one pass over the methods. Its callable identity comes
  // from the shape's own pool, so a call resolves whatever order the two
  // bodies reach (LRM 13.7), and the signature published alongside it is the
  // method's canonical dispatch role, which a peer reads to pick between
  // direct and virtual invocation with no cross-class MIR read. Its
  // static-lifetime locals take their cells on this same shape, so a static
  // write from a body routes to the exact cell the shape declares.
  std::vector<DeclaredCallable> declared_methods;
  declared_methods.reserve(hir_class.methods.size());
  std::vector<CallableSignature> signatures;
  signatures.reserve(hir_class.methods.size());
  for (const hir::MethodId id : hir_class.methods.Ids()) {
    const hir::SubroutineDecl& method = hir_class.methods.Get(id);
    signatures.push_back(
        CallableSignature{
            .virtual_dispatch =
                CanonicalizeVirtualDispatch(unit_lowerer, method)});
    declared_methods.push_back(
        DeclaredCallable{
            .callable = unit_lowerer.TranslateMethod(hir_class_id_, id),
            .statics = bind_statics(method, method.name)});
  }
  shape.callable_signatures = {hir_class.methods.size(), std::move(signatures)};
  declared_methods_ = {hir_class.methods.size(), std::move(declared_methods)};
  ctor_static_bindings_ = bind_statics(hir_class.constructor, "<ctor>");

  // No lexical scope of a method body answers for a name: a class object is
  // reached by member select rather than by scope name (LRM 23.7), so no
  // hierarchical path names a block inside a method. A `disable` written inside
  // one still names it, and what such a target belongs to is the class: a
  // method is automatic (LRM 8.6), and LRM 9.6.2 disables a block inside an
  // automatic task for every concurrent execution of it, so one cell serves
  // every object of the class.
  scopes_ = ScopesOwningDisableTargets(
      hir_class.procedural_scopes,
      ClassStorage{.properties = &shape.static_properties},
      unit_lowerer.Unit().types.Intern(
          mir::Type{mir::RuntimeLibraryType{
              .kind = mir::RuntimeLibraryKind::kCancellationTarget}}));

  unit_lowerer.DefineClassShape(class_id_, std::move(shape));
  return {};
}

auto ClassDeclLowerer::PopulateBodies() -> diag::Result<void> {
  UnitLowerer& unit_lowerer = *owner_;
  const hir::ClassDecl& hir_class = *hir_class_;
  const ClassShape& shape = unit_lowerer.GetClassShape(class_id_);

  mir::Class mir_class = shape.OpenClass();

  mir::CallableCode ctor_code = mir::CallableCode::Defined();
  CallableBindings ctor_bindings(unit_lowerer.Unit(), ctor_code);
  const mir::LocalId self_id = ctor_bindings.Declare(
      BindingOriginId::Receiver(),
      mir::LocalDecl{.name = "self", .type = shape.self_pointer_type});
  mir::Block& ctor_block = ctor_code.Body();
  ScopeChainNode scope_link{};
  const WalkFrame frame = WalkFrame{}
                              .WithClass(&mir_class, class_id_, scope_link)
                              .WithBlock(&ctor_block)
                              .WithBindings(&ctor_bindings);

  const hir::SubroutineDecl& ctor = hir_class.constructor;
  ProcessLowerer ctor_lowerer(
      unit_lowerer, nullptr, mir_class.time_resolution, ctor.body, "<ctor>",
      frame, scopes_, ctor_static_bindings_);

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
  // `super.new()` otherwise. Publishing the base-init is what makes
  // base-constructor ordering a stated fact rather than a backend convention.
  // A class with no base carries no base-init.
  //
  // The implicit case states no arguments, which is only right where the base
  // constructor declares none: a formal carrying a default value never reaches
  // the call, so the target language is left to decide it.
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
  // Index the source-declared initializers by their target so the per-field
  // loop below reads each one in O(1): the source names only the fields it
  // wrote, and the pass visits every field.
  std::unordered_map<hir::FieldId, hir::ExprId> initializer_of;
  initializer_of.reserve(hir_class.field_inits.size());
  for (const hir::FieldInit& init : hir_class.field_inits) {
    initializer_of.emplace(init.target, init.value);
  }
  for (const hir::FieldId hir_field_id : hir_class.fields.Ids()) {
    const hir::ClassField& field = hir_class.fields.Get(hir_field_id);
    const mir::FieldId mir_field_id = shape.field_translation.Get(hir_field_id);
    const mir::TypeId field_type = mir_class.fields.Get(mir_field_id).type;
    mir::ExprId value_id{};
    if (const auto it = initializer_of.find(hir_field_id);
        it != initializer_of.end()) {
      auto value_or =
          ctor_lowerer.LowerExpr(ctor.body.exprs.Get(it->second), frame);
      if (!value_or) return std::unexpected(std::move(value_or.error()));
      value_id = ctor_block.exprs.Add(*std::move(value_or));
    } else {
      value_id = ctor_block.exprs.Add(
          BuildDefaultValueFromHir(unit_lowerer, ctor_block, field.type));
    }
    const mir::ExprId self_ref =
        ctor_block.exprs.Add(MakeSelfRefExpr(frame, shape.self_pointer_type));
    const mir::ExprId target = ctor_block.exprs.Add(
        mir::MakeFieldAccessExpr(
            self_ref,
            mir::FieldTarget{.owner = class_id_, .slot = mir_field_id},
            field_type));
    const mir::ExprId assign =
        ctor_block.exprs.Add(mir::MakeAssignExpr(target, value_id, field_type));
    ctor_block.AppendStmt(mir::ExprStmt{.expr = assign});
  }

  // Each instance method (LRM 8.6) is lowered as a callable this class owns: it
  // resolves the body's `self` to the managed handle, and the method's
  // callable identity is the one the shape handed out, so a call site that
  // resolved before this body lowered reaches this method.
  //
  // A pure virtual prototype (LRM 8.21) has no source-defined body to walk;
  // its MIR record still carries the signature -- receiver, named parameters,
  // and result type -- so the backend can emit the class's declaration, but
  // no body is produced, and the absence of one is what states the shape.
  for (const hir::MethodId method_id : hir_class.methods.Ids()) {
    const auto& method = hir_class.methods.Get(method_id);
    const DeclaredCallable& declared = declared_methods_.Get(method_id);
    const auto method_dispatch =
        shape.callable_signatures.Get(declared.callable).virtual_dispatch;
    if (method.is_prototype) {
      mir::CallableCode proto_code;
      CallableBindings proto_bindings(unit_lowerer.Unit(), proto_code);
      const mir::LocalId proto_self_id = proto_bindings.Declare(
          BindingOriginId::Receiver(),
          mir::LocalDecl{.name = "self", .type = shape.self_pointer_type});
      std::vector<mir::LocalId> proto_params{proto_self_id};
      proto_params.reserve(method.params.size() + 1);
      for (const auto& hir_param : method.params) {
        const auto& hir_var = method.body.procedural_vars.Get(hir_param.var);
        const std::optional<mir::TypeId> param_type =
            ParamTypeOf(unit_lowerer, hir_var.type, hir_param.direction);
        if (!param_type.has_value()) {
          continue;
        }
        const mir::LocalId param_id = proto_bindings.Declare(
            BindingOriginId::Procedural(hir_param.var),
            mir::LocalDecl{.name = hir_var.name, .type = *param_type});
        proto_params.push_back(param_id);
      }
      proto_code.params = std::move(proto_params);
      // A prototype declares the interface its definitions implement, so it
      // reads that interface from the same declaration they do.
      proto_code.result_type = SubroutineCallTypeOf(unit_lowerer, method);
      // `proto_code.body` stays absent: this declaration does not define the
      // method, and the deriving class supplies it.
      mir_class.callables.Define(
          declared.callable, mir::CallableDecl{
                                 .name = method.name,
                                 .code = std::move(proto_code),
                                 .foreign = std::nullopt,
                                 .virtual_dispatch = method_dispatch});
      continue;
    }
    ScopeChainNode method_link{};
    const WalkFrame method_owner_frame =
        WalkFrame{}.WithClass(&mir_class, class_id_, method_link);
    ProcessLowerer method_lowerer(
        unit_lowerer, nullptr, mir_class.time_resolution, method.body,
        method.name, method_owner_frame, scopes_, declared.statics);
    auto method_code_or = method_lowerer.Run(method);
    if (!method_code_or) {
      return std::unexpected(std::move(method_code_or.error()));
    }
    mir_class.callables.Define(
        declared.callable, mir::CallableDecl{
                               .name = method.name,
                               .code = *std::move(method_code_or),
                               .foreign = std::nullopt,
                               .virtual_dispatch = method_dispatch});
  }

  // The constructor body statements (LRM 8.7) run after base construction
  // and property initialization, so they follow both the field-init prologue
  // and the base-call arg evaluation already emitted into the ctor block.
  auto body_or = ctor_lowerer.LowerConstructorBodyInto(frame);
  if (!body_or) return std::unexpected(std::move(body_or.error()));

  ctor_code.params = std::move(ctor_params);
  ctor_code.result_type = unit_lowerer.Unit().builtins.void_type;
  mir_class.constructor = mir::ConstructorDecl{
      .code = std::move(ctor_code), .base_init = std::move(base_init)};

  std::vector<BodyStatics> body_statics;
  body_statics.reserve(hir_class.methods.size() + 1);
  for (const hir::MethodId method_id : hir_class.methods.Ids()) {
    const auto& method = hir_class.methods.Get(method_id);
    body_statics.push_back(
        BodyStatics{
            .body = &method.body,
            .name = method.name,
            .statics = declared_methods_.Get(method_id).statics});
  }
  body_statics.push_back(
      BodyStatics{
          .body = &ctor.body,
          .name = "<ctor>",
          .statics = ctor_static_bindings_});

  auto static_init_or = LowerStaticInit(
      unit_lowerer, hir_class, shape, mir_class, class_id_, scopes_,
      body_statics);
  if (!static_init_or)
    return std::unexpected(std::move(static_init_or.error()));
  mir_class.static_init = *std::move(static_init_or);

  unit_lowerer.Unit().DefineClass(class_id_, std::move(mir_class));
  return {};
}

}  // namespace lyra::lowering::hir_to_mir
