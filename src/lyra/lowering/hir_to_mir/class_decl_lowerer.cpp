#include "lyra/lowering/hir_to_mir/class_decl_lowerer.hpp"

#include <algorithm>
#include <expected>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/component_index.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/class_decl.hpp"
#include "lyra/hir/class_ref.hpp"
#include "lyra/hir/param_direction.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/random_qualifier.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/hir/subroutine_kind.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/callee_interface.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/class_shape.hpp"
#include "lyra/lowering/hir_to_mir/covergroup_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/declaration_initializer.hpp"
#include "lyra/lowering/hir_to_mir/declared_initial_value.hpp"
#include "lyra/lowering/hir_to_mir/declared_scope.hpp"
#include "lyra/lowering/hir_to_mir/expression/selects.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/randomization.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"
#include "lyra/lowering/hir_to_mir/static_var_binding.hpp"
#include "lyra/lowering/hir_to_mir/subroutine_call.hpp"
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
#include "lyra/mir/type_builders.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/runtime_class.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// Whether a property of type `type` holds class handles, itself or as the
// elements of an array of them, so what is random of it is the objects they
// reach (LRM 18.4) rather than a value of its own.
auto HoldsHandles(const hir::TypePool& types, hir::TypeId type) -> bool {
  return types.Get(ChosenValueType(types, type)).Is<hir::ClassHandleType>();
}

// One of what `randomize()` asks of a class, as its body is written: handed
// the object, and then the call's problem, as a reference to it, where
// `problem_type` is that reference's type.
class HookBody {
 public:
  template <typename MakeFrame>
  HookBody(
      UnitLowerer& unit_lowerer, const ClassShape& shape,
      const MakeFrame& make_frame, std::optional<mir::TypeId> problem_type)
      : unit_lowerer_(&unit_lowerer),
        code_(mir::CallableCode::Defined()),
        bindings_(unit_lowerer.Unit(), code_),
        bound_(BindImplicitParameters(
            unit_lowerer,
            make_frame(link_)
                .WithBlock(&code_.Body())
                .WithBindings(&bindings_)
                .WithBodyCanWait(false),
            shape, CallableForm::kInstanceMember)),
        problem_(problem_type.transform([&](mir::TypeId type) {
          return Handed{
              .local = bindings_.DeclareAnonymous(type), .type = type};
        })) {
    if (problem_.has_value()) {
      bound_.params.push_back(problem_->local);
    }
  }
  HookBody(const HookBody&) = delete;
  auto operator=(const HookBody&) -> HookBody& = delete;
  HookBody(HookBody&&) = delete;
  auto operator=(HookBody&&) -> HookBody& = delete;
  ~HookBody() = default;

  [[nodiscard]] auto Unit() const -> mir::CompilationUnit& {
    return unit_lowerer_->Unit();
  }
  [[nodiscard]] auto Block() -> mir::Block& {
    return code_.Body();
  }
  [[nodiscard]] auto Frame() const -> const WalkFrame& {
    return bound_.frame;
  }

  // The object the hook runs on, as the pointer its body holds.
  [[nodiscard]] auto Self() -> mir::ExprId {
    return Block().exprs.Add(MakeSelfRefExpr(Frame()));
  }
  // The problem, as the reference the hook was handed, and as itself.
  [[nodiscard]] auto ProblemReference() -> mir::ExprId {
    return Block().ReadLocal(problem_->local, problem_->type);
  }
  [[nodiscard]] auto ProblemLocal() const -> mir::LocalId {
    return problem_->local;
  }
  [[nodiscard]] auto HandedType() const -> mir::TypeId {
    return problem_->type;
  }
  [[nodiscard]] auto Problem() -> mir::ExprId {
    return Block().Read(
        Block().exprs.Add(
            mir::MakeDerefExpr(
                ProblemReference(), Unit().builtins.constraint_problem)));
  }

  [[nodiscard]] auto Call(
      mir::Callee callee, std::vector<mir::ExprId> arguments, mir::TypeId type)
      -> mir::ExprId {
    return Block().exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee = std::move(callee),
                    .arguments = std::move(arguments)},
            .type = type});
  }
  void Run(mir::Callee callee, std::vector<mir::ExprId> arguments) {
    Block().AppendStmt(
        mir::ExprStmt{
            .expr = Call(
                std::move(callee), std::move(arguments),
                Unit().builtins.void_type)});
  }

  // A reference to the part `path` reaches, and the value of type `type` it
  // holds now.
  struct Named {
    mir::ExprId reference;
    mir::ExprId value;
  };
  [[nodiscard]] auto Name(const AccessPath& path, mir::TypeId type) -> Named {
    mir::Block& block = Block();
    const mir::TypeId reference_type =
        *ParamTypeOf(Unit(), type, hir::ParamDirection::kRef);
    const mir::LocalId reference = bindings_.DeclareAnonymous(reference_type);
    block.AppendStmt(
        mir::LocalDeclStmt{
            .target = reference,
            .init = AtType(
                Unit(), block, PathReference(Unit(), block, path),
                reference_type)});
    return Named{
        .reference = block.ReadLocal(reference, reference_type),
        .value = block.Read(block.exprs.Add(block.WrapperContents(
            Unit().types, block.ReadLocal(reference, reference_type), type)))};
  }

  // The finished body, which the library calls on an object of the class.
  [[nodiscard]] auto Finish() -> mir::CallableDecl {
    code_.params = std::move(bound_.params);
    code_.result_type = Unit().builtins.void_type;
    return mir::CallableDecl{
        .code = std::move(code_),
        .foreign = std::nullopt,
        .virtual_dispatch = std::nullopt,
        .type_parameters = {},
        .defining_unit = std::nullopt};
  }

 private:
  UnitLowerer* unit_lowerer_;
  ScopeChainNode link_{};
  mir::CallableCode code_;
  CallableBindings bindings_;
  BoundImplicitParameters bound_;
  // The parameter the problem is handed in, and its type.
  struct Handed {
    mir::LocalId local;
    mir::TypeId type;
  };
  std::optional<Handed> problem_;
};

// The body the class `base` answers `callback` with, one of the callbacks
// every class answers for `randomize()`, which a class extending it calls
// directly to do what its base does first.
auto BaseHookTarget(
    UnitLowerer& unit_lowerer, hir::ClassId base,
    support::LibraryCallback callback) -> mir::DirectTarget {
  if (unit_lowerer.Hir().classes.IsOwn(base)) {
    return mir::CallableTarget{
        .owner = unit_lowerer.ClassIdentity(base),
        .slot = unit_lowerer.RandomizationHooksOf(base)->BodyOf(callback)};
  }
  const mir::ClassId declared = unit_lowerer.ClassIdentity(base);
  const std::optional<mir::CallableId> body =
      unit_lowerer.Unit().GetClass(declared).CallbackBody(callback);
  if (!body.has_value()) {
    throw InternalError(
        "BaseHookTarget: a class of another unit gives no body to a callback "
        "every class answers for randomize() -- please report this as a bug");
  }
  return mir::CallableTarget{.owner = declared, .slot = *body};
}

// One body of the class and the static-lifetime variables it declared, paired
// so the bring-up can apply each initializer in the arena the expression was
// written in.
struct BodyStatics {
  const hir::ProceduralBody* body;
  std::string_view name;
  std::span<const StaticVarBinding> statics;
};

// Brings up the cells this class owns rather than an object of it (LRM 8.9),
// into the bodies its owner brings its own storage up in (LRM 10.5). Which
// owner that is was settled where the cells were declared; this only writes
// where it was told.
//
// Every property's cell is installed with its declared representation and
// default in the install phase, because the storage arrives with none. A
// source-written value then stores through the cell in the value phase, and the
// default case needs no statement there, being fully expressed by the
// installation already. The values are written in the order the source declares
// them, because an initializer may read a property declared before it. A
// static-lifetime variable of one of the class's bodies follows, its cell being
// the class's for the same reason.
auto LowerStaticStorageInto(
    UnitLowerer& unit_lowerer, const StructuralScopeLowerer* declaring_scope,
    const hir::ClassDecl& hir_class, const hir::ClassDeclaration& declared,
    const ClassShape& shape, const mir::Class& mir_class,
    const DeclaredScopes& scopes, std::span<const BodyStatics> body_statics,
    const StorageBringUp& bring_up) -> diag::Result<void> {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  const WalkFrame& frame = bring_up.value;
  mir::Block& block = *frame.current_block;
  mir::Block& install_block = *bring_up.install.current_block;
  ProcessLowerer lowerer(
      unit_lowerer, declaring_scope, mir_class.time_resolution,
      hir_class.static_init, std::nullopt, frame, scopes, {});

  const auto cell_type_of = [&](hir::StaticPropertyId property) {
    return mir::ObservableCellOf(
        unit.types, unit_lowerer.TranslateType(
                        declared.static_properties.Get(property).type));
  };
  for (const hir::StaticPropertyId prop_id : declared.static_properties.Ids()) {
    const mir::ExprId installed =
        install_block.exprs.Add(BuildStaticStorageAccess(
            unit, bring_up.install,
            shape.static_property_translation.Get(prop_id),
            cell_type_of(prop_id), mir::EnclosingHops{}));
    const mir::ExprId prototype = BuildDeclaredInitialValue(
        unit_lowerer, bring_up.install,
        declared.static_properties.Get(prop_id).type);
    install_block.AppendStmt(
        mir::ExprStmt{
            .expr = install_block.exprs.Add(
                mir::MakeCapabilityInstallCallExpr(
                    installed, prototype, support::BuiltinFn::kInitialize,
                    unit.builtins.void_type))});
  }
  for (const hir::StaticPropertyInit& init : hir_class.static_property_inits) {
    // The initializer is written in the class, whose name a construct in it
    // reports (LRM 21.2.1.5).
    auto value_or = lowerer.LowerExpr(
        hir_class.static_init.exprs.Get(init.value),
        frame.WithScopeName(hir_class.fixed_name));
    if (!value_or) return std::unexpected(std::move(value_or.error()));
    const mir::ExprId value_id = block.exprs.Add(*std::move(value_or));
    const mir::ExprId target = block.exprs.Add(BuildStaticStorageAccess(
        unit, frame, shape.static_property_translation.Get(init.target),
        cell_type_of(init.target), mir::EnclosingHops{}));
    block.AppendStmt(
        mir::ExprStmt{
            .expr = block.exprs.Add(BuildStoreExpr(
                unit, block, AccessPath{.owner = target, .descent = {}},
                value_id))});
  }

  // LRM 6.21 applies such an initializer once before any process starts, rather
  // than on each entry to the body that declares it. Each body is lowered
  // against its own statics, since a declaration's identity is scoped to the
  // body's arena.
  for (const BodyStatics& body : body_statics) {
    ProcessLowerer body_lowerer(
        unit_lowerer, declaring_scope, mir_class.time_resolution, *body.body,
        std::nullopt, frame, scopes, body.statics);
    for (const StaticVarBinding& binding : body.statics) {
      auto integ = IntegrateStaticInitializer(
          body_lowerer, *body.body, bring_up, binding);
      if (!integ) return std::unexpected(std::move(integ.error()));
    }
  }
  return {};
}

// A static method (LRM 8.10) belongs to the class and runs on no object; every
// other method runs on the one it was called through.
auto CallableFormOf(const hir::MethodDispatch& dispatch) -> CallableForm {
  return std::visit(
      Overloaded{
          [](const hir::TypeAssociated&) {
            return CallableForm::kTypeAssociated;
          },
          [](const hir::NotVirtual&) { return CallableForm::kInstanceMember; },
          [](const hir::IntroducesVirtual&) {
            return CallableForm::kInstanceMember;
          },
          [](const hir::OverridesVirtual&) {
            return CallableForm::kInstanceMember;
          }},
      dispatch);
}

}  // namespace

auto ClassDeclLowerer::Declared() const -> const hir::ClassDeclaration& {
  return owner_->Hir().classes.Declaration(hir_class_id_);
}

auto ClassDeclLowerer::DeclareShape(
    ClassShape* declaring_shape,
    std::span<const PlacedProperty> published_statics) -> diag::Result<void> {
  UnitLowerer& unit_lowerer = *owner_;
  const hir::ClassDecl& hir_class = *hir_class_;
  const hir::ClassDeclaration& declared = Declared();

  const mir::TypeId self_pointer_type = unit_lowerer.Unit().types.Intern(
      mir::Type{mir::PointerType{
          .pointee = object_type_,
          .ownership = mir::PointerOwnership::kBorrowed,
          .mutability = mir::Mutability::kMutable}});

  // A class extending nothing the source wrote still extends the root every
  // object does; an interface class has no object and extends nothing at all
  // (LRM 8.26).
  std::optional<mir::ClassRef> base_ref;
  if (declared.base.has_value()) {
    base_ref = mir::ClassRef(unit_lowerer.ClassIdentity(*declared.base));
  } else if (!declared.is_interface_class) {
    base_ref = mir::ClassRef{mir::ManagedObjectRootRef{}};
  }
  std::vector<mir::ClassId> implements;
  implements.reserve(declared.implements.size());
  for (const hir::ClassId iface : declared.implements) {
    implements.push_back(unit_lowerer.ClassIdentity(iface));
  }

  ClassShape shape{
      .identity = unit_lowerer.Hir().classes.PathOf(hir_class_id_),
      .base = base_ref,
      .implements = std::move(implements),
      .self_pointer_type = self_pointer_type,
      .declaring_instance = std::nullopt,
      .time_resolution = {},
      .execution = {},
      .dumped_as = {},
      .ctor_prefix_params = {},
      .fields = {},
      .named_fields = {},
      .static_properties = {},
      .named_static_properties = {},
      .callable_signatures = {},
      .library_callbacks = {},
      .static_property_translation = {},
      .contained = {},
      .is_final = false,
      .is_interface_class = declared.is_interface_class};

  // A property (LRM 8.4) becomes the field at its position, laid out as every
  // unit declaring the class lays it out. The instance the class's objects
  // belong to (LRM 6.22) is a borrow: it is built during elaboration and
  // outlives every object of the class, so nothing here owns it. Construction
  // is where it arrives, which is why the constructor takes it as a parameter.
  ClassFields fields = unit_lowerer.ClassFieldsOf(declared);
  for (PropertyField& property : fields.properties) {
    shape.AddNamedField(std::move(property.name), property.type);
  }
  if (fields.declaring_instance.has_value()) {
    shape.declaring_instance = DeclaringInstance{
        .type = *fields.declaring_instance,
        .member = shape.AddField(*fields.declaring_instance)};
  }

  // Everything the class keeps for itself rather than per object goes to one
  // pool, chosen by what replicates the class declaration (LRM 6.22, 8.9): a
  // class a namespace unit declares owns its cells, and one a structural scope
  // declares is replicated with that scope, so its cells are fields of the
  // instance and each instance has its own. Static properties, the
  // static-lifetime variables of its bodies, and what a `disable` naming one of
  // its blocks invalidates all take the same answer, because the question is
  // the same one.
  const StaticStorageOwner class_storage =
      declaring_shape != nullptr ? StaticStorageOwner{InstanceStorage{
                                       .owner = declaring_scope_->ClassId(),
                                       .shape = declaring_shape,
                                       .placed = {}}}
                                 : StaticStorageOwner{ClassStorage{
                                       .owner = class_id_, .shape = &shape}};

  // Static properties (LRM 8.9) take their cells in declaration order, recorded
  // as the loop goes. That pool also takes what the class's bodies keep, so a
  // property's position in it is not its position in the source.
  //
  // Each is an observable cell. A static property needs no object, so a name
  // outside every body of the class reaches it and a continuous assignment may
  // take it as an operand and must re-evaluate when it changes (LRM 8.9,
  // 10.3.2) -- the same test that makes a namespace variable a cell and leaves
  // an instance property, reachable only through a receiver, a plain value.
  shape.static_property_translation =
      base::Translation<hir::StaticPropertyId, StaticStorageHome>{
          declared.static_properties.size()};
  for (const hir::StaticPropertyId id : declared.static_properties.Ids()) {
    const hir::DeclaredProperty& sp = declared.static_properties.Get(id);
    const mir::TypeId cell_type = mir::ObservableCellOf(
        unit_lowerer.Unit().types, unit_lowerer.TranslateType(sp.type));
    const auto published =
        std::ranges::find(published_statics, id, &PlacedProperty::property);
    const StaticStorageHome home =
        published != published_statics.end()
            ? StaticStorageHome{InstanceFieldHome{.field = published->field}}
            : DeclareStaticCell(class_storage, cell_type);
    // The class answers the identifier the source declared only where the class
    // owns the cell. Where a structural scope replicates the class the cell is
    // storage of that scope's instance, reached as the field it is, and the
    // class-qualified name a source reference carries is resolved before this
    // layer rather than spelled on the storage.
    std::visit(
        Overloaded{
            [&](const ClassCellHome& cell) {
              shape.named_static_properties.push_back(
                  mir::NamedStaticProperty{
                      .name = sp.name, .slot = cell.property});
            },
            [](const InstanceFieldHome&) {},
            [](const UnitCellHome&) {
              throw InternalError(
                  "DeclareStaticProperties: a class property took a cell of a "
                  "unit's namespace");
            }},
        home);
    shape.static_property_translation.Append(home);
  }

  const auto bind_statics = [&](const hir::SubroutineDecl& decl) {
    return BindBodyStatics(
        unit_lowerer, hir_class.procedural_scopes, class_storage, decl.body);
  };

  // Everything a peer may need about a method before its body exists is
  // settled here, in one pass over what the class declares. Each method is the
  // callable at its position, so a call resolves whatever order the two bodies
  // reach (LRM 13.7). Its static-lifetime variables take their cells on this
  // same shape, so a static write from a body routes to the exact cell the
  // shape declares. The method making a covergroup the class embeds (LRM
  // 19.4.1) is virtual, so a class extending this one makes its extension of
  // the covergroup through it.
  std::vector<CallableSignature> signatures;
  signatures.reserve(declared.methods.size());
  for (const hir::MethodId id : declared.methods.Ids()) {
    signatures.push_back(
        CallableSignature{
            .virtual_dispatch = unit_lowerer.DispatchRoleOf(
                hir::ClassMethodTarget{.owner = hir_class_id_, .method = id})});
  }
  std::vector<StaticVarBindings> method_statics;
  method_statics.reserve(hir_class.methods.size());
  for (const hir::MethodId id : hir_class.methods.Ids()) {
    method_statics.push_back(bind_statics(hir_class.methods.Get(id)));
  }
  // What `randomize()` asks of the class (LRM 18.6) is answered by bodies the
  // library calls, each at the identity the class took for it.
  std::size_t callable_count = signatures.size();
  if (const std::optional<RandomizationHooks>& hooks =
          unit_lowerer.RandomizationHooksOf(hir_class_id_)) {
    const auto callbacks = hooks->Callbacks();
    callable_count += callbacks.size();
    signatures.resize(callable_count);
    for (const auto& [id, callback] : callbacks) {
      shape.library_callbacks.push_back(
          mir::LibraryCallbackBody{.callback = callback, .body = id});
    }
  }
  shape.callable_signatures = {callable_count, std::move(signatures)};
  method_statics_ = {hir_class.methods.size(), std::move(method_statics)};
  ctor_static_bindings_ = bind_statics(hir_class.constructor);

  // No lexical scope of a method body is a place a path reaches: a class
  // object is reached by member select rather than by scope name (LRM 23.7),
  // so no hierarchical path names a block inside a method. A `disable` written
  // inside one still names it, and what such a target belongs to is the class:
  // a method is automatic (LRM 8.6), and LRM 9.6.2 disables a block inside an
  // automatic task for every concurrent execution of it, so one cell serves
  // every object of the class.
  //
  // A block of a method a covergroup the class embeds is sampled at triggers
  // the event of the record of each instance the object holds (LRM 19.3).
  BlockEventTriggers block_events;
  AddEmbeddedBlockEvents(unit_lowerer, hir_class_id_, block_events);
  scopes_ = ScopesOwningDisableTargets(
      hir_class.procedural_scopes, hir_class.fixed_name, class_storage,
      unit_lowerer.Unit().types.Intern(
          mir::Type{mir::RuntimeLibraryType{
              .kind = mir::RuntimeLibraryKind::kCancellationTarget}}),
      block_events);

  unit_lowerer.DefineClassShape(class_id_, std::move(shape));
  return {};
}

auto ClassDeclLowerer::BodyFrame(
    const WalkFrame& declaring_frame, mir::Class& mir_class,
    ScopeChainNode& link) const -> WalkFrame {
  const WalkFrame frame = declaring_frame
                              .WithClass(
                                  {.cls = &mir_class,
                                   .id = class_id_,
                                   .enclosing = std::nullopt,
                                   .published = std::nullopt},
                                  link)
                              .WithScopeName(hir_class_->fixed_name);
  const std::optional<DeclaringInstance>& instance =
      owner_->GetClassShape(class_id_).declaring_instance;
  if (!instance.has_value()) return frame.WithStructuralBase(NoScope{});
  return frame.WithStructuralBase(
      ScopeThroughMember{.member = instance->member});
}

// Each hook starts with what the class it extends does -- a source class by
// the body that class gave it, which it calls directly, and the part every
// object starts with by nothing at all -- and then adds what the class
// declares itself.
void ClassDeclLowerer::DefineRandomizationHooks(
    const WalkFrame& declaring_frame, mir::Class& mir_class,
    const RandomizationHooks& hooks) const {
  UnitLowerer& unit_lowerer = *owner_;
  const hir::ClassDecl& hir_class = *hir_class_;
  const hir::ClassDeclaration& declared = Declared();
  const ClassShape& shape = unit_lowerer.GetClassShape(class_id_);
  const auto make_frame = [&](ScopeChainNode& link) {
    return BodyFrame(declaring_frame, mir_class, link);
  };
  // A hook is handed the problem as each constraint block it calls is.
  const std::optional<mir::TypeId> handed = ConstraintProblemParamTypeOf(
      unit_lowerer.Unit(), hir::SubroutineKind::kConstraintBlock);
  const auto finish = [&](HookBody& body, support::LibraryCallback callback) {
    mir_class.callables.Define(hooks.BodyOf(callback), body.Finish());
  };
  const auto begin = [&](HookBody& body, support::LibraryCallback callback) {
    if (!declared.base.has_value()) {
      return;
    }
    body.Run(
        mir::Direct{
            .target = BaseHookTarget(unit_lowerer, *declared.base, callback),
            .receiver =
                BuildObjectDeref(body.Unit(), body.Block(), body.Self())},
        {body.ProblemReference()});
  };
  // Each random variable the class declares (LRM 18.4): each property
  // declared random, an instance one through the object and a static one
  // through its cell, and of a structure declared random each member its type
  // declares random, at any depth. Each is visited with the part of its type
  // the source wrote, which says whether it holds handles, whose objects are
  // what is random of it.
  const auto each_random_variable = [&](HookBody& body, const auto& visit) {
    mir::CompilationUnit& unit = body.Unit();
    const hir::TypePool& hir_types = unit_lowerer.Hir().types;
    const auto walk = [&](const auto& self, AccessPath path,
                          hir::TypeId hir_type, mir::TypeId type,
                          hir::RandomQualifier random) -> void {
      const hir::Type& declared = hir_types.Get(hir_type);
      const auto* structure = declared.As<hir::UnpackedStructType>();
      if (structure == nullptr) {
        visit(path, hir_type, type, random);
        return;
      }
      for (std::uint32_t i = 0; i < structure->fields.size(); ++i) {
        const hir::UnpackedAggregateField& member = structure->fields[i];
        if (!hir::IsRandom(member.random)) {
          continue;
        }
        const mir::TypeId member_type = unit_lowerer.TranslateType(member.type);
        self(
            self,
            DescendInto(
                path, MemberStep(
                          unit_lowerer, body.Block(), declared,
                          base::ComponentIndex{.value = i}, member_type)),
            member.type, member_type, member.random);
      }
    };
    for (const hir::FieldId id : declared.properties.Ids()) {
      const hir::DeclaredProperty& field = declared.properties.Get(id);
      if (!hir::IsRandom(field.random)) {
        continue;
      }
      const mir::FieldId slot{id.value};
      const mir::TypeId type = mir_class.fields.Get(slot).type;
      walk(
          walk,
          AccessPath{
              .owner =
                  ObjectProperty{
                      .object = body.Self(),
                      .property =
                          mir::ClassFieldTarget{
                              .owner = class_id_, .slot = slot},
                      .type = type},
              .descent = {}},
          field.type, type, field.random);
    }
    for (const hir::StaticPropertyId id : declared.static_properties.Ids()) {
      const hir::DeclaredProperty& property =
          declared.static_properties.Get(id);
      if (!hir::IsRandom(property.random)) {
        continue;
      }
      const mir::TypeId type = unit_lowerer.TranslateType(property.type);
      walk(
          walk,
          AccessPath{
              .owner = body.Block().exprs.Add(BuildStaticStorageAccess(
                  unit, body.Frame(), shape.static_property_translation.Get(id),
                  mir::ObservableCellOf(unit.types, type),
                  mir::EnclosingHops{})),
              .descent = {}},
          property.type, type, property.random);
    }
  };

  {
    HookBody body(unit_lowerer, shape, make_frame, handed);
    begin(body, support::LibraryCallback::kClassRandVariables);
    each_random_variable(
        body, [&](const AccessPath& path, hir::TypeId hir_type,
                  mir::TypeId type, hir::RandomQualifier random) {
          if (HoldsHandles(unit_lowerer.Hir().types, hir_type)) {
            return;
          }
          const HookBody::Named named = body.Name(path, type);
          if (const std::optional<mir::ExprId> members =
                  ChosenMembers(unit_lowerer, body.Block(), hir_type)) {
            body.Run(
                mir::Direct{.target = support::BuiltinFn::kConstraintMembers},
                {body.Problem(), named.reference, *members});
          }
          body.Run(
              mir::Direct{
                  .target = random == hir::RandomQualifier::kRandc
                                ? support::BuiltinFn::kConstraintRandc
                                : support::BuiltinFn::kConstraintRand},
              {body.Problem(), named.reference, named.value});
        });
    finish(body, support::LibraryCallback::kClassRandVariables);
  }

  {
    HookBody body(unit_lowerer, shape, make_frame, handed);
    begin(body, support::LibraryCallback::kClassConstrain);
    for (const hir::MethodId id : hir_class.methods.Ids()) {
      const hir::SubroutineDecl& method = hir_class.methods.Get(id);
      const mir::CallableId callable{id.value};
      const std::optional<mir::VirtualDispatchRole>& role =
          shape.callable_signatures.Get(callable).virtual_dispatch;
      // A block overriding one of the class it extends is reached by that
      // class's call, which dispatches to it.
      if (!role.has_value() ||
          !std::holds_alternative<mir::IntroducesVirtualSlot>(*role)) {
        continue;
      }
      mir::Block& block = body.Block();
      mir::CompilationUnit& unit = body.Unit();
      const auto name = [&](std::string text) {
        return BuildStringValueExpr(unit, block, std::move(text));
      };
      mir::ExprId enabled{};
      switch (method.kind) {
        case hir::SubroutineKind::kTask:
        case hir::SubroutineKind::kFunction:
          continue;
        case hir::SubroutineKind::kConstraintBlock:
          enabled = body.Call(
              mir::Direct{
                  .target = support::BuiltinFn::kConstraintBlockEnabled},
              {block.AsValue(unit.types, body.Self()), name(method.name)},
              unit.builtins.machine_bool);
          break;
        case hir::SubroutineKind::kStaticConstraintBlock:
          enabled = body.Call(
              mir::Direct{
                  .target = support::BuiltinFn::kStaticConstraintBlockEnabled},
              {name(StaticConstraintModeKey(
                  unit_lowerer.Hir(), hir_class_id_, method.name))},
              unit.builtins.machine_bool);
          break;
      }
      const mir::VirtualSlot slot =
          CanonicalVirtualSlot(class_id_, callable, *role);
      mir::Block then_block;
      then_block.AppendStmt(
          mir::ExprStmt{
              .expr = then_block.exprs.Add(
                  mir::Expr{
                      .data =
                          mir::CallExpr{
                              .callee =
                                  mir::Virtual{
                                      .receiver = BuildObjectDeref(
                                          unit, then_block,
                                          AsIntroducer(
                                              unit.types, then_block,
                                              then_block.exprs.Add(
                                                  MakeSelfRefExpr(
                                                      body.Frame())),
                                              slot)),
                                      .slot = slot},
                              .arguments = {then_block.ReadLocal(
                                  body.ProblemLocal(), body.HandedType())}},
                      .type = unit.builtins.void_type})});
      block.AppendStmt(
          mir::IfStmt{
              .condition = enabled,
              .then_scope = block.child_scopes.Add(std::move(then_block)),
              .else_scope = std::nullopt});
    }
    finish(body, support::LibraryCallback::kClassConstrain);
  }

  {
    HookBody body(unit_lowerer, shape, make_frame, handed);
    begin(body, support::LibraryCallback::kClassRandObjects);
    each_random_variable(
        body, [&](const AccessPath& path, hir::TypeId hir_type,
                  mir::TypeId type, hir::RandomQualifier) {
          if (!HoldsHandles(unit_lowerer.Hir().types, hir_type)) {
            return;
          }
          const HookBody::Named named = body.Name(path, type);
          body.Run(
              mir::Direct{.target = support::BuiltinFn::kRandomizeNested},
              {body.Problem(), named.reference, named.value});
        });
    finish(body, support::LibraryCallback::kClassRandObjects);
  }

  // The `pre_randomize` or `post_randomize` the class declares, called on the
  // object as a call written in the class names it: directly, since neither is
  // virtual, and handing on no report, since no evaluation makes the call.
  const auto calls_own =
      [&](const std::optional<RandomizationHooks::CallsDeclared>& hook) {
        if (!hook.has_value()) {
          return;
        }
        const hir::MethodId method = hook->method;
        HookBody body(unit_lowerer, shape, make_frame, std::nullopt);
        std::vector<mir::ExprId> arguments;
        if (ReportParamTypeOf(body.Unit(), hir_class.methods.Get(method).kind)
                .has_value()) {
          arguments.push_back(BuildReportHandedOn(body.Unit(), body.Frame()));
        }
        const mir::ExprId called = body.Call(
            mir::Direct{
                .target =
                    mir::CallableTarget{
                        .owner = class_id_,
                        .slot = mir::CallableId{method.value}},
                .receiver =
                    BuildObjectDeref(body.Unit(), body.Block(), body.Self())},
            std::move(arguments),
            SubroutineCallTypeOf(unit_lowerer, hir_class.methods.Get(method)));
        body.Block().AppendStmt(mir::ExprStmt{.expr = called});
        mir_class.callables.Define(hook->body, body.Finish());
      };
  calls_own(hooks.pre_randomize);
  calls_own(hooks.post_randomize);
}

auto ClassDeclLowerer::PopulateBodies(
    WalkFrame declaring_frame, StorageBringUp owner_bring_up)
    -> diag::Result<void> {
  UnitLowerer& unit_lowerer = *owner_;
  const hir::ClassDecl& hir_class = *hir_class_;
  const hir::ClassDeclaration& declared = Declared();
  const ClassShape& shape = unit_lowerer.GetClassShape(class_id_);

  mir::Class mir_class = shape.OpenClass();
  for (const hir::ConformingBehavior& answered : hir_class.conforming) {
    mir_class.conforming.push_back(
        mir::ConformingBehavior{
            .interface_behavior =
                unit_lowerer.VirtualSlotOf(answered.interface_behavior),
            .answered_by = answered.answered_by.transform(
                [&](const hir::ClassMethodTarget& behavior) {
                  return unit_lowerer.VirtualSlotOf(behavior);
                })});
  }

  mir::CallableCode ctor_code = mir::CallableCode::Defined();
  CallableBindings ctor_bindings(unit_lowerer.Unit(), ctor_code);
  mir::Block& ctor_block = ctor_code.Body();
  ScopeChainNode scope_link{};
  // The object, and then the instance it belongs to where the class belongs to
  // one, which is written into the member the object records it in for the
  // object's later bodies to read.
  BoundImplicitParameters bound = BindImplicitParameters(
      unit_lowerer,
      BodyFrame(declaring_frame, mir_class, scope_link)
          .WithBlock(&ctor_block)
          .WithBindings(&ctor_bindings)
          .WithBodyCanWait(false),
      shape, CallableForm::kConstructor);
  const WalkFrame& frame = bound.frame;
  std::vector<mir::LocalId> ctor_params = std::move(bound.params);

  const hir::SubroutineDecl& ctor = hir_class.constructor;
  ProcessLowerer ctor_lowerer(
      unit_lowerer, declaring_scope_, mir_class.time_resolution, ctor.body,
      ctor.root_stmt, frame, scopes_, ctor_static_bindings_);

  if (const std::optional<DeclaringInstance>& instance =
          shape.declaring_instance) {
    const mir::ExprId value = ctor_block.exprs.Add(
        mir::Expr{
            .data =
                mir::CastExpr{
                    .operand = BuildEnclosingScopeReceiver(
                        frame, unit_lowerer.Unit(), mir::EnclosingHops{})},
            .type = instance->type});
    const mir::ExprId target = ctor_block.exprs.Add(
        mir::MakeFieldAccessExpr(
            BuildObjectDeref(
                unit_lowerer.Unit(), ctor_block,
                ctor_block.exprs.Add(MakeSelfRefExpr(frame))),
            mir::ClassFieldTarget{.owner = class_id_, .slot = instance->member},
            instance->type));
    ctor_block.AppendStmt(
        mir::ExprStmt{
            .expr = ctor_block.exprs.Add(
                mir::MakeAssignExpr(
                    unit_lowerer.Unit().builtins, target, value))});
  }
  // Register the ctor formals early so a base-constructor arg (LRM 8.7) can
  // reference them: `super.new(a * 2)` in the derived ctor reads its own `a`
  // formal, and that lookup resolves through the same procedural-var
  // registry the ctor body uses. Formals land as MIR locals after the leading
  // parameters; the base-call arg exprs and every field initializer below read
  // them through that registry.
  ctor_lowerer.RegisterConstructorFormals(ctor, frame, ctor_params);
  // A construction is handed where to report what it reads (LRM 9.4.2), after
  // its formals, as a function is, and everything it evaluates -- its base's
  // arguments, its property initializers, its body -- states there what it
  // reaches through a handle as it reaches it.
  const mir::LocalId report = ctor_bindings.DeclareAnonymous(
      unit_lowerer.Unit().builtins.read_report_ptr);
  ctor_params.push_back(report);
  auto reported_or =
      ctor_lowerer.BuildConstructionReport(frame, ctor.reads, report);
  if (!reported_or) return std::unexpected(std::move(reported_or.error()));
  const WalkFrame reporting = frame.WithReportingReachedTo(report);

  // Base construction (LRM 8.7): a derived class always constructs its base,
  // with the arguments the class states for it, as a statement of this
  // constructor after what the object records and before any property is
  // initialized. The object is already of the class it was created as (LRM
  // 8.17 NOTE 2), so the base's constructor runs on it as it is.
  if (declared.base.has_value()) {
    std::vector<mir::ExprId> base_args;
    // A base belonging to an instance leads its arguments with that instance,
    // which encloses the one this class belongs to.
    if (hir_class.base_call.declaring_instance.has_value()) {
      base_args.push_back(BuildImplicitInstanceArgument(
          ctor_lowerer, frame, *hir_class.base_call.declaring_instance));
    }
    auto actuals = EmitConstructionActuals(
        ctor_lowerer, reporting, *declared.base, hir_class.base_call.arguments);
    if (!actuals) return std::unexpected(std::move(actuals.error()));
    base_args.insert(base_args.end(), actuals->begin(), actuals->end());
    // The base's construction is part of this one, so it reports where this
    // one does.
    base_args.push_back(ctor_block.ReadLocal(
        report, unit_lowerer.Unit().builtins.read_report_ptr));
    const mir::ExprId object = BuildObjectDeref(
        unit_lowerer.Unit(), ctor_block,
        ctor_block.exprs.Add(MakeSelfRefExpr(frame)));
    ctor_block.AppendStmt(
        mir::ExprStmt{
            .expr = ctor_block.exprs.Add(
                mir::MakeCallExpr(
                    mir::Direct{
                        .target =
                            mir::ConstructorTarget{
                                .cls =
                                    unit_lowerer.ClassIdentity(*declared.base)},
                        .receiver = object},
                    std::move(base_args),
                    unit_lowerer.Unit().builtins.void_type))});
  }

  // Every property starts as a declaration that writes none does (LRM Table
  // 6-7), and then each one the source wrote an initializer for is assigned
  // that value, in the order the source declares them (LRM 8.7), before the
  // constructor body runs -- lowered through the constructor lowerer so a
  // property read resolves against the receiver, and in that order because an
  // initializer may read a property declared before it (LRM 10.5).
  const auto store = [&](hir::FieldId property, mir::ExprId value_id) {
    const mir::FieldId slot{property.value};
    const mir::ExprId self_ref = ctor_block.exprs.Add(MakeSelfRefExpr(frame));
    const mir::ExprId target = ctor_block.exprs.Add(
        mir::MakeFieldAccessExpr(
            BuildObjectDeref(unit_lowerer.Unit(), ctor_block, self_ref),
            mir::ClassFieldTarget{.owner = class_id_, .slot = slot},
            mir_class.fields.Get(slot).type));
    ctor_block.AppendStmt(
        mir::ExprStmt{
            .expr = ctor_block.exprs.Add(
                mir::MakeAssignExpr(
                    unit_lowerer.Unit().builtins, target, value_id))});
  };
  for (const hir::FieldId property : declared.properties.Ids()) {
    store(
        property,
        BuildDeclaredInitialValue(
            unit_lowerer, frame, declared.properties.Get(property).type));
  }
  for (const hir::FieldInit& init : hir_class.field_inits) {
    auto value_or =
        ctor_lowerer.LowerExpr(ctor.body.exprs.Get(init.value), reporting);
    if (!value_or) return std::unexpected(std::move(value_or.error()));
    store(init.target, ctor_block.exprs.Add(*std::move(value_or)));
  }

  // Each method (LRM 8.6) is lowered as a callable this class owns, at its
  // position, where a call site that resolved before this body lowered already
  // reaches it; its body resolves `self` to the managed handle. A method is
  // what a name reaches, here and from another unit, so the class records the
  // name against the body.
  //
  // A pure virtual prototype (LRM 8.21) has no source-defined body to walk;
  // its MIR record still carries the signature -- receiver, named parameters,
  // and result type -- so the backend can emit the class's declaration, but
  // no body is produced, and the absence of one is what states the shape.
  const auto lower_method =
      [&](const hir::SubroutineDecl& method, CallableForm form,
          mir::CallableId callable,
          const StaticVarBindings& statics) -> diag::Result<void> {
    mir_class.named_callables.push_back(
        mir::NamedCallable{.name = method.name, .body = callable});
    const auto method_dispatch =
        shape.callable_signatures.Get(callable).virtual_dispatch;
    if (!method.root_stmt.has_value()) {
      // A prototype declares the interface its definitions implement, so it
      // reads that interface from the same declaration they do.
      mir_class.callables.Define(
          callable, mir::CallableDecl{
                        .code = PrototypeOf(
                            unit_lowerer, method, shape.self_pointer_type),
                        .foreign = std::nullopt,
                        .virtual_dispatch = method_dispatch,
                        .type_parameters = {},
                        .defining_unit = std::nullopt});
      return {};
    }
    ScopeChainNode method_link{};
    const WalkFrame method_owner_frame =
        BodyFrame(declaring_frame, mir_class, method_link);
    ProcessLowerer method_lowerer(
        unit_lowerer, declaring_scope_, mir_class.time_resolution, method.body,
        method.root_stmt, method_owner_frame, scopes_, statics);
    auto method_code_or = method_lowerer.Run(method, form);
    if (!method_code_or) {
      return std::unexpected(std::move(method_code_or.error()));
    }
    mir_class.callables.Define(
        callable, mir::CallableDecl{
                      .code = *std::move(method_code_or),
                      .foreign = std::nullopt,
                      .virtual_dispatch = method_dispatch,
                      .type_parameters = {},
                      .defining_unit = std::nullopt});
    return {};
  };
  for (const hir::MethodId method_id : hir_class.methods.Ids()) {
    if (auto lowered = lower_method(
            hir_class.methods.Get(method_id),
            CallableFormOf(declared.methods.Get(method_id).dispatch),
            mir::CallableId{method_id.value}, method_statics_.Get(method_id));
        !lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
  }

  // The method making each covergroup the class embeds (LRM 19.4.1), named by
  // the covergroup's name, as the class's signature publishes it.
  for (const hir::MethodId method_id : declared.methods.Ids()) {
    const std::optional<hir::CovergroupId>& made =
        declared.methods.Get(method_id).makes;
    if (!made.has_value()) {
      continue;
    }
    const hir::CovergroupId covergroup = *made;
    const mir::CallableId callable{method_id.value};
    ScopeChainNode factory_link{};
    auto factory = LowerCovergroupFactory(
        unit_lowerer, BodyFrame(declaring_frame, mir_class, factory_link),
        shape, covergroup);
    if (!factory) return std::unexpected(std::move(factory.error()));
    mir_class.named_callables.push_back(
        mir::NamedCallable{
            .name = hir::CovergroupFactoryName(
                unit_lowerer.Hir().covergroups.PathOf(covergroup)),
            .body = callable});
    mir_class.callables.Define(
        callable,
        mir::CallableDecl{
            .code = *std::move(factory),
            .foreign = std::nullopt,
            .virtual_dispatch =
                shape.callable_signatures.Get(callable).virtual_dispatch,
            .type_parameters = {},
            .defining_unit = std::nullopt});
  }
  if (const std::optional<RandomizationHooks>& hooks =
          unit_lowerer.RandomizationHooksOf(hir_class_id_)) {
    DefineRandomizationHooks(declaring_frame, mir_class, *hooks);
  }

  // The constructor body statements (LRM 8.7) run after base construction
  // and property initialization, so they follow both, already appended to the
  // ctor block. They are the body of `new`, a function the source named (LRM
  // 8.7), so a name reported inside them is that function's.
  auto body_or = ctor_lowerer.LowerConstructorBodyInto(
      ctor_lowerer.InRootScope(reporting));
  if (!body_or) return std::unexpected(std::move(body_or.error()));

  ctor_code.params = std::move(ctor_params);
  ctor_code.result_type = unit_lowerer.Unit().builtins.void_type;
  if (!declared.is_interface_class) {
    mir_class.constructor = mir::ConstructorDecl{.code = std::move(ctor_code)};
  }

  std::vector<BodyStatics> body_statics;
  body_statics.reserve(hir_class.methods.size() + 1);
  for (const hir::MethodId method_id : hir_class.methods.Ids()) {
    const auto& method = hir_class.methods.Get(method_id);
    body_statics.push_back(
        BodyStatics{
            .body = &method.body,
            .name = method.name,
            .statics = method_statics_.Get(method_id)});
  }
  body_statics.push_back(
      BodyStatics{
          .body = &ctor.body,
          .name = "<ctor>",
          .statics = ctor_static_bindings_});

  // The cells these initializers write go wherever what replicates the class
  // declaration puts them, so the statements go wherever that owner is brought
  // up: a class a structural scope declares has them as fields of the instance,
  // brought up where that instance brings up its own, and one a namespace unit
  // declares has them as its own, brought up where that namespace brings up the
  // rest of what it owns. Either way once, and before any process runs (LRM
  // 10.5).
  if (auto r = LowerStaticStorageInto(
          unit_lowerer, declaring_scope_, hir_class, declared, shape, mir_class,
          scopes_, body_statics, owner_bring_up);
      !r) {
    return std::unexpected(std::move(r.error()));
  }

  unit_lowerer.Unit().DefineClass(class_id_, std::move(mir_class));
  return {};
}

}  // namespace lyra::lowering::hir_to_mir
