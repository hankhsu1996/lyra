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
            std::get_if<hir::ExternalDispatchSlot>(&*method.overrides)) {
      // A behavior introduced in another unit is canonically owned there, so
      // this unit records the takeover by the coordinate that unit published
      // and states nothing about where it lands.
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
// lowers to an assignment to that property, in declaration order; a static
// property without a source initializer takes its type's Table 7-1 default and
// gets no statement here. A static-lifetime local of a body follows, its cell
// being the class's for the same reason.
auto LowerStaticInitInto(
    UnitLowerer& unit_lowerer, const StructuralScopeLowerer* declaring_scope,
    const hir::ClassDecl& hir_class, const ClassShape& shape,
    const mir::Class& mir_class, const DeclaredScopes& scopes,
    std::span<const BodyStatics> body_statics, const WalkFrame& frame)
    -> diag::Result<void> {
  mir::Block& block = *frame.current_block;
  ProcessLowerer lowerer(
      unit_lowerer, declaring_scope, mir_class.time_resolution,
      hir_class.static_init, std::nullopt, "<static_init>", frame, scopes, {});

  for (const hir::StaticPropertyInit& init : hir_class.static_property_inits) {
    const hir::Expr& hir_value = hir_class.static_init.exprs.Get(init.value);
    auto value_or = lowerer.LowerExpr(hir_value, frame);
    if (!value_or) return std::unexpected(std::move(value_or.error()));
    const mir::ExprId value_id = block.exprs.Add(*std::move(value_or));

    const StaticStorageHome& home =
        shape.static_property_translation.Get(init.target);
    const mir::TypeId prop_type = unit_lowerer.TranslateType(
        hir_class.static_properties.Get(init.target).type);
    const mir::ExprId target = block.exprs.Add(BuildStaticStorageAccess(
        unit_lowerer.Unit(), frame, home, prop_type, mir::EnclosingHops{}));
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
        unit_lowerer, declaring_scope, mir_class.time_resolution, *body.body,
        std::nullopt, std::string{body.name}, frame, scopes, body.statics);
    for (const StaticVarBinding& binding : body.statics) {
      auto integ = IntegrateStaticInitializer(
          body_lowerer, *body.body, frame, frame, binding);
      if (!integ) return std::unexpected(std::move(integ.error()));
    }
  }
  return {};
}

// As many of this class's own construction-prefix locals as its base declares
// parameters for. A base in another compilation unit takes none: what crosses a
// unit boundary is that unit's signature, which carries no instance of a scope
// inside it, so such a base belongs to no instance of this one's.
auto BaseCtorPrefixLocals(
    const UnitLowerer& unit_lowerer, const hir::ClassRef& base,
    std::span<const mir::LocalId> own_prefix) -> std::span<const mir::LocalId> {
  const auto* local = std::get_if<hir::LocalClassRef>(&base);
  if (local == nullptr) return {};
  const std::size_t wanted =
      unit_lowerer.GetClassShape(unit_lowerer.TranslateClass(local->class_id))
          .ctor_prefix_params.size();
  return own_prefix.first(std::min(wanted, own_prefix.size()));
}

}  // namespace

auto ClassDeclLowerer::DeclareShape(ClassShape* declaring_shape)
    -> diag::Result<void> {
  UnitLowerer& unit_lowerer = *owner_;
  const hir::ClassDecl& hir_class = *hir_class_;

  const mir::TypeId self_pointer_type = unit_lowerer.Unit().types.Intern(
      mir::Type{mir::PointerType{
          .pointee = object_type_,
          .ownership = mir::PointerOwnership::kBorrowed,
          .mutability = mir::Mutability::kMutable}});

  std::optional<mir::ClassRef> base_ref;
  if (hir_class.base.has_value()) {
    base_ref = unit_lowerer.TranslateBaseClassRef(*hir_class.base);
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

  // The instance this class's objects belong to (LRM 6.22), recorded ahead of
  // every source property so a property's position never moves with it. It is
  // a borrow: the instance is built during elaboration and outlives every
  // object of the class, so nothing here owns it. Construction is where it
  // arrives, which is why it is also the constructor's leading parameter.
  if (declaring_shape != nullptr) {
    const mir::TypeId declaring_ptr = declaring_shape->self_pointer_type;
    declaring_scope_field_ = shape.fields.Add(
        mir::FieldDecl{.name = "declaring_scope", .type = declaring_ptr});
    shape.ctor_prefix_params.Add(
        mir::ParamDecl{.name = "declaring_scope", .type = declaring_ptr});
  }

  // A property (LRM 8.4) becomes one field of the class, so where a property
  // lands is a fact only this loop knows. It is recorded as the loop goes;
  // nothing downstream recomputes it.
  //
  // A property another unit may name sits in a fixed prefix, ahead of every one
  // the class keeps to itself (LRM 8.18), so a unit reading the promise counts
  // the same slot out of it and a property the class never promised can move
  // none of them. The source order is what the promise states and what a
  // declaration initializer runs in, so it is kept within each group.
  shape.field_translation =
      base::Translation<hir::FieldId, mir::FieldId>{hir_class.fields.size()};
  std::vector<mir::FieldId> placed(hir_class.fields.size());
  const auto place = [&](bool published) {
    for (const hir::FieldId id : hir_class.fields.Ids()) {
      const hir::ClassField& field = hir_class.fields.Get(id);
      if (field.is_published != published) {
        continue;
      }
      placed[id.value] = shape.fields.Add(
          mir::FieldDecl{
              .name = field.name,
              .type = unit_lowerer.TranslateType(field.type)});
    }
  };
  place(true);
  place(false);
  for (const mir::FieldId id : placed) {
    shape.field_translation.Append(id);
  }

  // Everything the class keeps for itself rather than per object goes to one
  // pool, chosen by what replicates the class declaration (LRM 6.22, 8.9): a
  // class a namespace unit declares owns its cells, and one a structural scope
  // declares is replicated with that scope, so its cells are fields of the
  // instance and each instance has its own. Static properties, the
  // static-lifetime locals of its bodies, and what a `disable` naming one of
  // its blocks invalidates all take the same answer, because the question is
  // the same one.
  const StaticStorageOwner class_storage =
      declaring_shape != nullptr ? StaticStorageOwner{InstanceStorage{
                                       .fields = &declaring_shape->fields}}
                                 : StaticStorageOwner{ClassStorage{
                                       .properties = &shape.static_properties}};

  // A cell's name has to be unique in the pool that holds it. A callable name
  // and a declaration id are unique within one class; a structural scope's pool
  // is shared by every class it declares, so the class name joins them there.
  const std::string cell_prefix =
      declaring_shape != nullptr ? hir_class.name + "__" : std::string{};

  // Static properties (LRM 8.9) take their cells in declaration order, recorded
  // as the loop goes. That pool also takes what the class's bodies keep, so a
  // property's position in it is not its position in the source.
  shape.static_property_translation =
      base::Translation<hir::StaticPropertyId, StaticStorageHome>{
          hir_class.static_properties.size()};
  for (const auto& sp : hir_class.static_properties) {
    const mir::TypeId sp_type = unit_lowerer.TranslateType(sp.type);
    shape.static_property_translation.Append(
        DeclareStaticCell(class_storage, cell_prefix + sp.name, sp_type));
  }

  const auto bind_statics = [&](const hir::SubroutineDecl& decl,
                                std::string_view callable_name) {
    return BindBodyStatics(
        unit_lowerer, hir_class.procedural_scopes, class_storage, decl.body,
        SignatureBoundVars(decl), cell_prefix + std::string{callable_name});
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
      hir_class.procedural_scopes, class_storage, cell_prefix,
      unit_lowerer.Unit().types.Intern(
          mir::Type{mir::RuntimeLibraryType{
              .kind = mir::RuntimeLibraryKind::kCancellationTarget}}));

  unit_lowerer.DefineClassShape(class_id_, std::move(shape));
  return {};
}

auto ClassDeclLowerer::BodyFrame(
    const WalkFrame& declaring_frame, mir::Class& mir_class,
    ScopeChainNode& link) const -> WalkFrame {
  const WalkFrame frame =
      declaring_frame.WithClass(&mir_class, class_id_, link);
  if (!declaring_scope_field_.has_value()) return frame;
  return frame.WithStructuralBase(
      ScopeThroughMember{.member = *declaring_scope_field_});
}

auto ClassDeclLowerer::PopulateBodies(
    WalkFrame declaring_frame, WalkFrame declaring_init_frame)
    -> diag::Result<void> {
  UnitLowerer& unit_lowerer = *owner_;
  const hir::ClassDecl& hir_class = *hir_class_;
  const ClassShape& shape = unit_lowerer.GetClassShape(class_id_);

  mir::Class mir_class = shape.OpenClass();

  mir::CallableCode ctor_code = mir::CallableCode::Defined();
  CallableBindings ctor_bindings(unit_lowerer.Unit(), ctor_code);
  const mir::LocalId self_id = ctor_bindings.Declare(
      BindingOriginId::Receiver(),
      mir::LocalDecl{.name = "self", .type = shape.self_pointer_type});
  // The instance the object belongs to lands as an ordinary local after
  // `self`, the way every construction prefix does, and is written into the
  // member the object records it in before anything the body can observe.
  std::vector<mir::LocalId> ctor_prefix_local_ids;
  ctor_prefix_local_ids.reserve(shape.ctor_prefix_params.size());
  for (const mir::ParamId param : shape.ctor_prefix_params.Ids()) {
    const auto& p = shape.ctor_prefix_params.Get(param);
    ctor_prefix_local_ids.push_back(ctor_bindings.DeclareAnonymous(
        mir::LocalDecl{.name = p.name, .type = p.type}));
  }
  mir::Block& ctor_block = ctor_code.Body();
  ScopeChainNode scope_link{};
  const WalkFrame frame = BodyFrame(declaring_frame, mir_class, scope_link)
                              .WithBlock(&ctor_block)
                              .WithBindings(&ctor_bindings);

  const hir::SubroutineDecl& ctor = hir_class.constructor;
  ProcessLowerer ctor_lowerer(
      unit_lowerer, declaring_scope_, mir_class.time_resolution, ctor.body,
      ctor.root_stmt, "<ctor>", frame, scopes_, ctor_static_bindings_);

  // Register the ctor formals early so a base-constructor arg (LRM 8.7) can
  // reference them: `super.new(a * 2)` in the derived ctor reads its own `a`
  // formal, and that lookup resolves through the same procedural-var
  // registry the ctor body uses. Formals land as MIR locals appended after
  // the receiver; the base-call arg exprs and every field initializer below
  // read them through that registry.
  std::vector<mir::LocalId> ctor_params{self_id};
  ctor_params.insert(
      ctor_params.end(), ctor_prefix_local_ids.begin(),
      ctor_prefix_local_ids.end());
  if (declaring_scope_field_.has_value()) {
    const mir::TypeId declaring_ptr =
        shape.fields.Get(*declaring_scope_field_).type;
    const mir::ExprId value = ctor_block.exprs.Add(
        mir::Expr{
            .data =
                mir::ReferenceExpr{
                    .target =
                        mir::LocalRef{.var = ctor_prefix_local_ids.front()}},
            .type = declaring_ptr});
    const mir::ExprId target = ctor_block.exprs.Add(
        mir::MakeFieldAccessExpr(
            ctor_block.exprs.Add(
                MakeSelfRefExpr(frame, shape.self_pointer_type)),
            mir::FieldTarget{
                .owner = class_id_, .slot = *declaring_scope_field_},
            declaring_ptr));
    ctor_block.AppendStmt(
        mir::ExprStmt{
            .expr = ctor_block.exprs.Add(
                mir::MakeAssignExpr(target, value, declaring_ptr))});
  }
  auto formals_or =
      ctor_lowerer.RegisterConstructorFormals(ctor, frame, ctor_params);
  if (!formals_or) return std::unexpected(std::move(formals_or.error()));

  // Base construction (LRM 8.7): a derived class always forwards to its base,
  // and what that forward carries came with the base itself. Publishing it here
  // is what makes base-constructor ordering a stated fact rather than a backend
  // convention.
  std::vector<mir::ExprId> base_args;
  if (hir_class.base.has_value()) {
    // The base's own construction prefix leads its arguments: a base that
    // belongs to the same instance this class does is handed that instance,
    // which only this constructor holds.
    for (const mir::LocalId prefix : BaseCtorPrefixLocals(
             unit_lowerer, *hir_class.base, ctor_prefix_local_ids)) {
      base_args.push_back(ctor_block.exprs.Add(
          mir::Expr{
              .data =
                  mir::ReferenceExpr{.target = mir::LocalRef{.var = prefix}},
              .type = ctor_code.locals.Get(prefix).type}));
    }
    for (const hir::ExprId arg : hir_class.base_call.arguments) {
      auto arg_or = ctor_lowerer.LowerExpr(ctor.body.exprs.Get(arg), frame);
      if (!arg_or) return std::unexpected(std::move(arg_or.error()));
      base_args.push_back(ctor_block.exprs.Add(*std::move(arg_or)));
    }
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
        BodyFrame(declaring_frame, mir_class, method_link);
    ProcessLowerer method_lowerer(
        unit_lowerer, declaring_scope_, mir_class.time_resolution, method.body,
        method.root_stmt, method.name, method_owner_frame, scopes_,
        declared.statics);
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
      .code = std::move(ctor_code), .base_args = std::move(base_args)};

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

  // The cells these initializers write go wherever what replicates the class
  // declaration puts them, so the statements go wherever that owner is brought
  // up: a class a namespace unit declares owns its cells and brings them up in
  // its own design-init body (LRM 10.5), and one a structural scope declares
  // has them as fields of the instance, brought up where that instance brings
  // up its own -- once per instance, still before any process runs.
  if (declaring_scope_ != nullptr) {
    if (auto r = LowerStaticInitInto(
            unit_lowerer, declaring_scope_, hir_class, shape, mir_class,
            scopes_, body_statics, declaring_init_frame);
        !r) {
      return std::unexpected(std::move(r.error()));
    }
  } else {
    mir::CallableCode static_init = mir::CallableCode::Defined();
    CallableBindings static_bindings(unit_lowerer.Unit(), static_init);
    static_init.params = {};
    static_init.result_type = unit_lowerer.Unit().builtins.void_type;
    ScopeChainNode static_link{};
    const WalkFrame static_frame =
        WalkFrame{}
            .WithClass(&mir_class, class_id_, static_link)
            .WithBlock(&static_init.Body())
            .WithBindings(&static_bindings);
    if (auto r = LowerStaticInitInto(
            unit_lowerer, nullptr, hir_class, shape, mir_class, scopes_,
            body_statics, static_frame);
        !r) {
      return std::unexpected(std::move(r.error()));
    }
    mir_class.static_init = std::move(static_init);
  }

  unit_lowerer.Unit().DefineClass(class_id_, std::move(mir_class));
  return {};
}

}  // namespace lyra::lowering::hir_to_mir
