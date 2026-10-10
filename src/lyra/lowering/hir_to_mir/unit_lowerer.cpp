#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"

#include <algorithm>
#include <cstddef>
#include <expected>
#include <format>
#include <memory>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <unordered_set>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/id_allocator.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/base/translation.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/class_decl.hpp"
#include "lyra/hir/class_ref.hpp"
#include "lyra/hir/external_scope_class.hpp"
#include "lyra/hir/published_scope.hpp"
#include "lyra/hir/structural_data_object.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/class_decl_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/declaration_initializer.hpp"
#include "lyra/lowering/hir_to_mir/declared_scope.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/design_namespaces.hpp"
#include "lyra/lowering/hir_to_mir/expression/dpi_call.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/pattern_rendering.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/static_var_binding.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/external_class.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/mir/verify.hpp"
#include "lyra/support/def_path.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The handle a member holds on each object it stands for. A handle reaches one
// object, so where the member stands for several the multiplicity stays outside
// it: a sequence of handles, never one handle on the sequence.
auto BorrowedObjectHandles(const mir::TypePool& types, mir::TypeId value_type)
    -> mir::TypeId {
  if (const auto* sequence = types.Get(value_type).As<mir::VectorType>()) {
    return types.Intern(
        mir::Type{mir::VectorType{
            .element = BorrowedObjectHandles(types, sequence->element)}});
  }
  return types.Intern(
      mir::Type{mir::PointerType{
          .pointee = value_type,
          .ownership = mir::PointerOwnership::kBorrowed,
          .mutability = mir::Mutability::kMutable}});
}

// A body is a tree of blocks -- a predicate that declares identifiers (LRM
// 12.6.3) puts the arms it guards in one of its own -- so what the body reads
// is the union over the whole tree, gathered here as the walk descends.
void GatherUnitsReadBy(
    const mir::Block& body, std::string_view own_unit,
    std::unordered_set<std::string>& units) {
  const auto note = [&](std::string_view unit_name) {
    if (unit_name != own_unit) {
      units.emplace(unit_name);
    }
  };
  for (const mir::ExprId id : body.exprs.Ids()) {
    const auto* reference =
        std::get_if<mir::ReferenceExpr>(&body.exprs.Get(id).data);
    if (reference == nullptr) {
      continue;
    }
    if (const auto* ref =
            std::get_if<mir::ExternalUnitVariableRef>(&reference->target)) {
      note(ref->unit_name);
    }
    if (const auto* ref =
            std::get_if<mir::ExternalStaticPropertyRef>(&reference->target)) {
      note(ref->unit_name);
    }
  }
  for (const mir::BlockId id : body.child_scopes.Ids()) {
    GatherUnitsReadBy(body.child_scopes.Get(id), own_unit, units);
  }
}

// The other units whose namespace-owned cells `body` reads -- a variable of the
// namespace itself, or one of its classes' type-associated cells, the two being
// one kind of storage differing in how far the name is qualified. Each named
// once and in name order, because what is done with the answer is emitting a
// call per unit and a design compiles to the same program each time.
auto UnitsReadBy(const mir::Block& body, std::string_view own_unit)
    -> std::vector<std::string> {
  std::unordered_set<std::string> gathered;
  GatherUnitsReadBy(body, own_unit, gathered);
  std::vector<std::string> units(gathered.begin(), gathered.end());
  std::ranges::sort(units);
  return units;
}

// Brings the cells the unit's namespace owns directly up in the two bodies the
// design root runs at time zero: `install_frame` receives each cell's declared
// representation and default, and `value_frame` each LRM 10.5 value
// initializer. What this covers is the namespace's own -- its variables (LRM
// 26.2) and the static-lifetime locals its subroutines declare, which are the
// same one-program-global-cell storage; the cells the unit's classes own join
// the same two bodies from the class lowering. The design root installs every
// unit before initializing any, so a value initializer always reaches installed
// storage. Neither this unit's cells nor another's sit on an instance, so an
// initializer here lowers with no enclosing scope and no receiver: a sibling
// cell is reached by its position in this arena, and another unit's by the
// identifier that unit published (`unit::name`).
auto PopulateNamespaceOwnStorage(
    UnitLowerer& unit_lowerer, const hir::StructuralScope& scope,
    const base::Translation<hir::StructuralSubroutineId, StaticVarBindings>&
        subroutine_statics,
    const DeclaredScopes& scope_nodes, const WalkFrame& install_frame,
    const WalkFrame& value_frame) -> diag::Result<void> {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& install_block = *install_frame.current_block;
  mir::Block& value_block = *value_frame.current_block;

  // The package root scope is an ExprLowerer over its own expressions: a
  // variable initializer's operands are literals, operators, and by-name
  // package symbols, none of which reach a class or a `self`.
  const StructuralScopeLowerer expr_lowerer(unit_lowerer, nullptr, scope);

  const auto make_cell = [&](mir::Block& block, mir::StaticVariableId variable,
                             mir::TypeId cell_type) -> mir::ExprId {
    return block.exprs.Add(
        mir::Expr{
            .data =
                mir::ReferenceExpr{
                    .target = mir::StaticVariableRef{.variable = variable}},
            .type = cell_type});
  };

  for (const hir::StructuralDataObjectId hir_id :
       scope.structural_data_objects.Ids()) {
    const hir::StructuralDataObjectDecl& d =
        scope.structural_data_objects.Get(hir_id);
    // A package declares no ports, so nothing here stands for storage another
    // scope owns; a net is the only other kind a package-scope declaration
    // reaches this as.
    const auto* var = std::get_if<hir::StructuralVariableDecl>(&d.kind);
    if (var == nullptr) {
      return diag::Fail(
          diag::DiagCode::kUnsupportedExpressionForm,
          "a net declared in a package is not supported");
    }
    const mir::TypeId value_type = unit_lowerer.TranslateType(d.type);
    const mir::TypeId cell_type = mir::ObservableCellOf(unit.types, value_type);
    if (!unit.types.Get(cell_type).IsCapabilityWrapper()) {
      return diag::Fail(
          diag::DiagCode::kUnsupportedExpressionForm,
          "a package variable of this type is not yet supported");
    }
    const mir::StaticVariableId variable =
        unit.static_variables.Add(mir::StaticVariableDecl{.type = cell_type});
    unit.named_static_variables.push_back(
        mir::NamedStaticVariable{.name = d.name, .variable = variable});

    // The install body gives the cell its declared representation and default.
    const mir::ExprId prototype = install_block.exprs.Add(
        BuildDefaultValueFromHir(unit_lowerer, install_block, d.type));
    install_block.AppendStmt(
        mir::ExprStmt{
            .expr = install_block.exprs.Add(
                mir::MakeCapabilityInstallCallExpr(
                    make_cell(install_block, variable, cell_type), prototype,
                    support::BuiltinFn::kInitialize,
                    unit.builtins.void_type))});

    // The value body runs a user initializer (LRM 10.5), which writes through
    // the cell.
    if (var->initializer.has_value()) {
      auto value_or = expr_lowerer.LowerExpr(
          scope.exprs.Get(*var->initializer), value_frame);
      if (!value_or) return std::unexpected(std::move(value_or.error()));
      const mir::ExprId value_id = value_block.exprs.Add(*std::move(value_or));
      value_block.AppendStmt(
          mir::ExprStmt{
              .expr = value_block.exprs.Add(BuildStoreExpr(
                  unit, value_block,
                  AccessPath{
                      .owner = make_cell(value_block, variable, cell_type),
                      .descent = {}},
                  value_id))});
    }
  }

  // A static-lifetime local of a package subroutine is one program-global cell
  // like the package's own variables (LRM 6.21, 26.2), so it comes up in these
  // same two bodies -- once before any process starts, rather than on each
  // entry to the subroutine that declares it.
  for (const hir::StructuralSubroutineId id :
       scope.structural_subroutines.Ids()) {
    const hir::SubroutineDecl& src = scope.structural_subroutines.Get(id);
    const StaticVarBindings& statics = subroutine_statics.Get(id);
    ProcessLowerer body_lowerer(
        unit_lowerer, nullptr, scope.time_resolution, src.body, std::nullopt,
        WalkFrame{}, scope_nodes, statics);
    for (const StaticVarBinding& binding : statics) {
      auto integ = IntegrateStaticInitializer(
          body_lowerer, src.body,
          StorageBringUp{.install = install_frame, .value = value_frame},
          binding);
      if (!integ) return std::unexpected(std::move(integ.error()));
    }
  }

  return {};
}

// A namespace's variable declaration assignments run before any procedure
// starts (LRM 26.2), so a randomization call inside one draws from the
// namespace's own initialization RNG rather than from any process's (LRM
// 18.14.1). A namespace is not instantiated, so nothing is named here: the
// entry starts the generator the standard gives every package.
void WrapInNamespaceStaticInitExtent(
    const UnitLowerer& unit_lowerer, mir::CallableCode& code) {
  mir::Block extent;
  AppendRuntimeEffectStmt(
      unit_lowerer, extent, support::BuiltinFn::kEnterNamespaceStaticInit, {});

  mir::Block cleanup;
  AppendRuntimeEffectStmt(
      unit_lowerer, cleanup, support::BuiltinFn::kLeaveStaticInit, {});

  extent.AppendFinally(std::move(code.Body()), std::move(cleanup));
  code.Body() = std::move(extent);
}

// A namespace's initializers run exactly once, and whichever call reaches them
// first is the one that runs them: the design's bring-up reaches every
// namespace, and a namespace whose own initializers read another's cells
// reaches that one ahead of its own (LRM 26.2 / 10.5). Taking the claim before
// descending is what makes running once true and what ends a cycle among them.
// The order LRM 26.2 leaves open is therefore these calls executed.
//
// A read reaches another unit's cell, so that unit is already one this one
// references; the call it adds here names no declaration the target did not
// publish.
void GuardNamespaceInitialization(
    const UnitLowerer& unit_lowerer, mir::CompilationUnit& unit,
    mir::CallableCode& code, std::span<const std::string> reads) {
  mir::Block outer;
  const mir::ExprId runtime =
      outer.exprs.Add(BuildCurrentRuntimeCallExpr(unit_lowerer));
  const mir::ExprId name = outer.exprs.Add(
      mir::Expr{
          .data = mir::StringLiteral{.value = unit.name},
          .type = unit.types.Intern(mir::Type{mir::MachineCStringType{}})});
  const mir::ExprId claimed = outer.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target =
                              support::BuiltinFn::kClaimNamespaceInitialize},
                  .arguments = {runtime, name}},
          .type = unit.builtins.machine_int64});
  const mir::ExprId took_it = outer.exprs.Add(MakeBinary(
      unit, outer, mir::BinaryOp::kInequality, claimed,
      BuildMachineIntLiteral(unit, outer, 0), unit.builtins.machine_bool));

  mir::Block bring_up;
  for (const std::string& read : reads) {
    unit.ConsumeNamespaceOf(read);
    bring_up.AppendStmt(
        mir::ExprStmt{
            .expr = bring_up.exprs.Add(
                mir::Expr{
                    .data =
                        mir::CallExpr{
                            .callee =
                                mir::Direct{
                                    .target =
                                        mir::ExternalUnitMintedEntryTarget{
                                            .unit_name = read,
                                            .entry = mir::MintedEntry::
                                                kInitializeStorage}},
                            .arguments = {}},
                    .type = unit.builtins.void_type})});
  }
  const mir::BlockId initializers =
      bring_up.child_scopes.Add(std::move(code.Body()));
  bring_up.AppendStmt(mir::BlockStmt{.scope = initializers});

  const mir::BlockId bring_up_id = outer.child_scopes.Add(std::move(bring_up));
  outer.AppendStmt(
      mir::IfStmt{
          .condition = took_it,
          .then_scope = bring_up_id,
          .else_scope = std::nullopt});
  code.Body() = std::move(outer);
}

// Publishes the two bodies the design root calls. Every namespace unit
// publishes both entries. One that owns no cell publishes a body that installs
// none and a body that initializes none -- zero declarations is a count, not
// another kind of unit -- so the design root calls both without first finding
// out what this one supplied. What the value body reads of other units is read
// off the initializers before the extent is wrapped around them, and becomes
// calls that body makes.
void PublishNamespaceStorageBringUp(
    const UnitLowerer& unit_lowerer, mir::CompilationUnit& unit,
    mir::CallableCode install_code, mir::CallableCode value_code) {
  const std::vector<std::string> reads =
      UnitsReadBy(value_code.Body(), unit.name);
  WrapInNamespaceStaticInitExtent(unit_lowerer, value_code);
  GuardNamespaceInitialization(unit_lowerer, unit, value_code, reads);

  unit.content = mir::BroughtUpNamespace{
      .install_storage = unit.callables.Add(
          mir::CallableDecl{
              .code = std::move(install_code),
              .foreign = std::nullopt,
              .virtual_dispatch = std::nullopt}),
      .initialize_storage = unit.callables.Add(
          mir::CallableDecl{
              .code = std::move(value_code),
              .foreign = std::nullopt,
              .virtual_dispatch = std::nullopt})};
}

// Publishes the one body that makes an object of this unit. A unit that
// instantiates this one read what this one published, which states what may
// be reached and never how much storage an object takes -- so it asks here
// instead of making one, and the construction it would otherwise have written
// sits on the side that knows the answer. The party that builds the design's
// tops asks the same way, having no more than any other referrer.
//
// Beside where the object hangs, it takes the values an instance is handed at
// construction and passes them on.
auto PublishObjectEntry(
    mir::CompilationUnit& unit, mir::ClassId root, mir::ClassId published,
    std::span<const ConstructionValue> handed) -> mir::CallableId {
  const auto owning_pointer = [&](mir::ClassId cls) {
    return unit.types.Intern(
        mir::Type{mir::PointerType{
            .pointee = unit.types.Intern(
                mir::Type{mir::ObjectType{.of = mir::IntraUnitClassRef{cls}}}),
            .ownership = mir::PointerOwnership::kUnique}});
  };
  // What it makes is the class that realizes the object; what it answers with
  // is what the unit published of it, that being all a referrer has.
  const mir::TypeId made = owning_pointer(root);
  const mir::TypeId owning = owning_pointer(published);

  mir::CallableCode code = mir::CallableCode::Defined();
  const mir::LocalId parent = code.AddLocal(unit.builtins.scope_ptr);
  const mir::LocalId segment = code.AddLocal(unit.builtins.hierarchy_segment);
  code.params = {parent, segment};
  code.result_type = owning;

  mir::Block& body = code.Body();
  std::vector<mir::ExprId> arguments{
      body.exprs.Add(mir::MakeLocalRefExpr(parent, unit.builtins.scope_ptr)),
      body.exprs.Add(
          mir::MakeLocalRefExpr(segment, unit.builtins.hierarchy_segment))};
  for (const ConstructionValue& value : handed) {
    const mir::LocalId param = code.AddLocal(value.type);
    code.params.push_back(param);
    arguments.push_back(
        body.exprs.Add(mir::MakeLocalRefExpr(param, value.type)));
  }
  const mir::ExprId built = body.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Construct{},
                  .arguments = std::move(arguments)},
          .type = made});
  body.AppendStmt(mir::ReturnStmt{.value = ObjectAs(body, built, owning)});

  return unit.callables.Add(
      mir::CallableDecl{
          .code = std::move(code),
          .foreign = std::nullopt,
          .virtual_dispatch = std::nullopt});
}

// What a signature names of another unit's behavior, in MIR's terms. Reading a
// signature records no dependency, so neither does this.
auto PublishedSlot(const hir::ExternalDispatchSlot& slot)
    -> mir::OverridesExternalSlot {
  return mir::OverridesExternalSlot{
      .unit_name = slot.unit_name,
      .class_path = slot.class_path,
      .ordinal = mir::BehaviorOrdinal{slot.behavior.value}};
}

}  // namespace

auto UnitLowerer::MemberCellType(
    mir::TypeId value_type, const hir::PublishedStorage& storage) const
    -> mir::TypeId {
  return std::visit(
      Overloaded{
          [&](const hir::VariableStorage&) {
            return mir::ObservableCellOf(unit_.types, value_type);
          },
          [&](const hir::NetStorage&) {
            return unit_.types.Intern(
                mir::Type{mir::ResolvedType{.value = value_type}});
          },
          [&](const hir::ReferenceStorage& reference) {
            return unit_.types.Intern(
                mir::Type{mir::RefType{
                    .pointee = value_type,
                    .mutability =
                        reference.binding == hir::ReferenceBinding::kConstRef
                            ? mir::Mutability::kReadOnly
                            : mir::Mutability::kMutable}});
          },
          [&](const hir::BorrowedObjectStorage&) {
            return BorrowedObjectHandles(unit_.types, value_type);
          }},
      storage);
}

auto UnitLowerer::ScopeClassIdentity(hir::ExternalScopeClassId hir_id) const
    -> mir::DeclaredClassRef {
  const hir::ExternalScopeClass& scope_class =
      hir_->external_scope_classes.Get(hir_id);
  return ClassIdentityOf(
      scope_class.unit_name, scope_class.signature.class_path);
}

auto UnitLowerer::UnitObjectType(hir::ExternalScopeClassId hir_id) const
    -> mir::TypeId {
  return unit_.types.Intern(
      mir::Type{mir::ObjectType{.of = ScopeClassIdentity(hir_id)}});
}

auto UnitLowerer::ClassIdentityOf(
    const std::string& unit_name, const support::DefPath& class_path) const
    -> mir::DeclaredClassRef {
  if (unit_name == unit_.name) {
    return mir::IntraUnitClassRef{
        .class_id = PublishedScopeClassAt(class_path)};
  }
  return mir::CrossUnitClassRef{
      .unit_name = unit_name, .class_path = class_path};
}

auto UnitLowerer::PublishedScopeClassAt(
    const support::DefPath& class_path) const -> mir::ClassId {
  const auto it = published_scope_classes_.find(class_path);
  if (it == published_scope_classes_.end()) {
    throw InternalError(
        std::format(
            "UnitLowerer::PublishedScopeClassAt: this unit published no scope "
            "as '{}' -- please report this as a bug",
            support::DisplayOf(class_path)));
  }
  return it->second;
}

auto UnitLowerer::TakePublishedScopeClass(
    const hir::ScopePublication& published) -> mir::ClassId {
  const auto [known, first] = published_scope_classes_.try_emplace(
      published.signature.class_path, mir::ClassId{});
  if (first) known->second = unit_.DeclareClass();
  return known->second;
}

auto UnitLowerer::SettlePublishedScope(
    mir::ClassId published, const hir::StructuralScope& scope)
    -> const PublishedScopeLayout& {
  if (const auto settled = published_scopes_.find(published);
      settled != published_scopes_.end()) {
    return settled->second.layout.published;
  }
  const hir::ScopeClassSignature& signature = scope.published.signature;
  ClassShape shape;
  shape.path = signature.class_path;
  shape.base = mir::ClassRef{mir::ObjectTreeRootRef{}};
  shape.self_pointer_type = unit_.types.Intern(
      mir::Type{mir::PointerType{
          .pointee = unit_.types.Intern(
              mir::Type{
                  mir::ObjectType{.of = mir::IntraUnitClassRef{published}}}),
          .ownership = mir::PointerOwnership::kBorrowed}});
  // It is the unit's object as other units see it, so it is the same time
  // scope (LRM 3.14.2.2) as the classes extending it.
  shape.time_resolution = scope.time_resolution;
  // Each published subroutine is a method of it, its identity reserved here at
  // the position the signature gave the subroutine, so a body reaching another
  // instance of this unit calls it by position before its body is built.
  shape.callable_signatures = {
      scope.published.callables.size(),
      std::vector<CallableSignature>(
          scope.published.callables.size(),
          CallableSignature{.virtual_dispatch = std::nullopt})};
  std::vector<std::string> subroutine_names;
  subroutine_names.reserve(scope.published.callables.size());
  for (const hir::StructuralSubroutineId id : scope.published.callables) {
    subroutine_names.push_back(scope.structural_subroutines.Get(id).name);
  }

  std::vector<mir::FieldId> slots;
  std::vector<mir::TypeId> field_types;
  for (PublishedField& field : PublishedFieldsOf(signature)) {
    field_types.push_back(field.type);
    slots.push_back(
        field.name.has_value()
            ? shape.AddNamedField(*std::move(field.name), field.type)
            : shape.AddField(field.type));
  }
  DefineClassShape(published, std::move(shape));
  const auto settled = published_scopes_.emplace(
      published,
      PublishedScope{
          .layout =
              ScopeClassLayout{
                  .cls = mir::IntraUnitClassRef{.class_id = published},
                  .field_types = std::move(field_types),
                  .published = PublishedLayoutAt(signature, slots)},
          .subroutine_names = std::move(subroutine_names),
          .realizations = {}});
  return settled.first->second.layout.published;
}

void UnitLowerer::AddRealization(
    mir::ClassId published, ScopeRealization realization) {
  const auto settled = published_scopes_.find(published);
  if (settled == published_scopes_.end()) {
    throw InternalError(
        "UnitLowerer::AddRealization: a scope realizes a class the unit "
        "published, which was laid out when the scope's shape was declared");
  }
  settled->second.realizations.push_back(std::move(realization));
}

auto UnitLowerer::TakeClassIdentities(const hir::ClassDecl& decl)
    -> ClassTranslation {
  const mir::ClassId id = unit_.DeclareClass();
  base::IdAllocator<mir::CallableId> callables;
  std::vector<mir::CallableId> methods;
  methods.reserve(decl.methods.size());
  for (std::size_t m = 0; m < decl.methods.size(); ++m) {
    methods.push_back(callables.Take());
  }
  return ClassTranslation{
      .id = id,
      .object_type = unit_.types.Intern(
          mir::Type{mir::ObjectType{.of = mir::IntraUnitClassRef{id}}}),
      .methods = {decl.methods.size(), std::move(methods)}};
}

auto UnitLowerer::PublishUnitDeclarations() -> diag::Result<void> {
  unit_.name = hir_->name;
  unit_.source_name = hir_->source_name;

  // Every identity another declaration can name is taken before any
  // declaration settles: a class handle type resolves to the pointee that names
  // it, and an overriding method states its dispatch role against the slot its
  // base was given. Nothing here reads a declaration, so no declaration has to
  // settle before another.
  std::vector<ClassTranslation> classes;
  classes.reserve(hir_->classes.size());
  for (const hir::ClassId hir_id : hir_->classes.Ids()) {
    classes.push_back(TakeClassIdentities(hir_->classes.Get(hir_id)));
    declared_classes_.emplace(hir_->classes.PathOf(hir_id), classes.back().id);
  }
  class_translations_ = {hir_->classes.size(), std::move(classes)};

  // Every HIR type is MIR-representable: AST-to-HIR rejects the forms MIR has
  // no shape for, so this projection never fails.
  // A composite type's translation reads the translations of its components,
  // which HIR minted before it, so the answers land one at a time and each one
  // can see the ones before it.
  type_translations_ =
      base::Translation<hir::TypeId, mir::TypeId>{hir_->types.size()};
  for (const hir::TypeId hir_id : hir_->types.Ids()) {
    const hir::Type& type = hir_->types.Get(hir_id);
    const mir::TypeId translated = unit_.types.Intern(TranslateType(type));
    type_translations_.Append(translated);
    // A struct this unit declares took an identity with no declaration yet,
    // which its type now names.
    const auto* structure = unit_.types.Get(translated).As<mir::StructType>();
    if (structure == nullptr) {
      continue;
    }
    if (const auto* id = std::get_if<mir::StructId>(&structure->declaration)) {
      DefineOwnStruct(type.Get<hir::UnpackedStructType>(), *id, translated);
    }
  }

  // Every record this unit read of what another unit published about its
  // class, taken whole and before anything names one. A place reaching a member
  // of a class an ancestor declares walks the lineage over these records, so a
  // class the walk only passes through is as much needed as the one that
  // declares the member -- which is not something the reference asking for the
  // member can know.
  //
  // Holding a record is not depending on the class. A signature is read here
  // as soon as the elaborating design asks anything of it, including of a class
  // this unit turns out to name nowhere, so what this unit depends on is
  // recorded where a reference takes the name rather than here.
  for (const hir::ExternalClass& published : hir_->external_classes) {
    TakePublishedClass(published);
  }

  // What each unit this one references published of its scope classes, in
  // this unit's terms, taken before any body lowers so a body reaching a
  // child's member reads the record rather than building one.
  for (const hir::ExternalScopeClassId id :
       hir_->external_scope_classes.Ids()) {
    RecordPublishedScopeClass(id, hir_->external_scope_classes.Get(id));
  }

  // The prototype of every DPI-C import this unit takes part in (LRM 35.4),
  // published as a bodyless callable of the unit: the DPI-C name space contains
  // no class, so no class owns one. Every call to the import resolves against
  // this one declaration, whichever unit wrote the source declaration.
  for (const hir::ForeignImportDecl& import : hir_->foreign_imports) {
    unit_.callables.Add(MakeForeignImportDecl(unit_, import));
  }

  PublishAssignmentPatternTexts();

  // Every class this unit holds is a type of one of its structural scopes'
  // instance (LRM 23.9, 6.22), which settles that class's shape and lowers its
  // bodies -- so a class body stands where the scope stands and reaches what it
  // reaches. A
  // class identity is minted on first reference, which a scope's own shape may
  // be, so nothing is minted ahead of the walk.
  return {};
}

void UnitLowerer::PublishAssignmentPatternTexts() {
  const auto names_its_text = [&](hir::TypeId type) {
    return TypeOwnsItsText(PatternReadingOf(*hir_, type));
  };

  // Every identity first, because one text's body reaches the texts of the
  // types it names and a type is free to name one interned after it.
  for (const hir::TypeId type : hir_->types.Ids()) {
    if (names_its_text(type)) {
      assignment_pattern_texts_.emplace(type, unit_.callables.Declare());
    }
  }

  for (const hir::TypeId type : hir_->types.Ids()) {
    if (names_its_text(type)) {
      unit_.callables.Define(
          assignment_pattern_texts_.at(type),
          mir::CallableDecl{
              .code = BuildAssignmentPatternTextCode(*this, type),
              .foreign = std::nullopt,
              .virtual_dispatch = std::nullopt});
    }
  }
}

auto UnitLowerer::Finish() -> mir::CompilationUnit {
  mir::Verify(unit_);
  return std::move(unit_);
}

auto UnitLowerer::RunObjectRoot() -> diag::Result<mir::CompilationUnit> {
  if (auto root = PopulateModuleRoot({}); !root) {
    return std::unexpected(std::move(root.error()));
  }
  return Finish();
}

auto UnitLowerer::RunDesignRoot(DesignNamespaces namespaces)
    -> diag::Result<mir::CompilationUnit> {
  if (auto root = PopulateModuleRoot(std::move(namespaces)); !root) {
    return std::unexpected(std::move(root.error()));
  }
  return Finish();
}

auto UnitLowerer::PopulateModuleRoot(DesignNamespaces namespaces)
    -> diag::Result<void> {
  WalkFrame root_frame;
  // The tree of scope lowerers is built first, and building it takes the
  // identity of every class the unit published of its scopes: a type a record
  // of another unit states may name one -- wherever a name leaves an instance
  // of this unit and reaches another -- so each exists before any type
  // translates. The namespaces the design root brings up ride on the root
  // scope's lowering and are empty for a source module.
  const std::unique_ptr<StructuralScopeLowerer> root =
      StructuralScopeLowerer::ForScope(
          *this, nullptr, hir_->root_scope, std::move(namespaces));
  if (auto prologue = PublishUnitDeclarations(); !prologue) {
    return std::unexpected(std::move(prologue.error()));
  }

  // Two-sweep structural lowering: the first sweep settles every class's
  // declaration; the second lowers every body and commits the composed class
  // to the unit.
  auto top_r = root->DeclareShape();
  if (!top_r) return std::unexpected(std::move(top_r.error()));
  auto body_r = root->PopulateBodies(root_frame);
  if (!body_r) return std::unexpected(std::move(body_r.error()));
  StructuralScopeLowerer::DefinePublishedClasses(*this);

  unit_.content = mir::RootedTree{
      .root = *top_r,
      .object_entry = PublishObjectEntry(
          unit_, *top_r, root->PublishedClassId(), root->ConstructionValues())};
  return {};
}

auto UnitLowerer::RunNamespace() -> diag::Result<mir::CompilationUnit> {
  if (auto prologue = PublishUnitDeclarations(); !prologue) {
    return std::unexpected(std::move(prologue.error()));
  }

  // A package's root scope holds no processes and no instances -- only its
  // variables, functions, and tasks (LRM 26.2). Each function and task lowers
  // to a receiver-less callable, and each variable to unit-level static
  // storage, so a package produces no root class and never enters the
  // structural-scope body machinery. A body's own static-lifetime locals take
  // the same unit-level storage its variables do, being one program-global cell
  // each. The frame has no owner class, so the produced body carries no `self`
  // -- and with no object to hang one under, none of its scopes owns a name
  // node.
  const hir::StructuralScope& scope = hir_->root_scope;
  const DeclaredScopes package_scope_nodes = ScopesOwningDisableTargets(
      scope.procedural_scopes,
      UnitStorage{.variables = &unit_.static_variables},
      unit_.types.Intern(
          mir::Type{mir::RuntimeLibraryType{
              .kind = mir::RuntimeLibraryKind::kCancellationTarget}}));

  base::Translation<hir::StructuralSubroutineId, StaticVarBindings>
      subroutine_statics{scope.structural_subroutines.size()};
  for (const hir::SubroutineDecl& src : scope.structural_subroutines) {
    subroutine_statics.Append(BindBodyStatics(
        *this, scope.procedural_scopes,
        UnitStorage{.variables = &unit_.static_variables}, src.body,
        SignatureBoundVars(src)));
  }

  // The callable each package subroutine lowered to, so an export below names
  // its own by identity. Every identity and every name is published before any
  // body this namespace owns is lowered -- a variable's initializer, a body of
  // a class it declares, and its own subroutines alike -- because any of them
  // may call a subroutine the source declared after it, or itself, and what
  // such a call names is the position, which has to exist and to answer to the
  // identifier the source spelled before the body that spells it is walked
  // (LRM 13.7, 26.2).
  base::Translation<hir::StructuralSubroutineId, mir::CallableId>
      subroutine_callables{scope.structural_subroutines.size()};
  for (const hir::StructuralSubroutineId id :
       scope.structural_subroutines.Ids()) {
    const mir::CallableId body = unit_.callables.Declare();
    // A package subroutine is what another unit spells (LRM 26.3), so the
    // unit's namespace records the name against the body it reaches.
    unit_.named_callables.push_back(
        mir::NamedCallable{
            .name = scope.structural_subroutines.Get(id).name, .body = body});
    subroutine_callables.Append(body);
  }

  // The two bodies the design root calls at time zero. They exist before
  // anything that fills them, because everything the namespace owns comes up in
  // them -- its own variables and the type-associated cells of the classes it
  // declares -- and the class lowering writes into them as it goes.
  mir::CallableCode install_code = mir::CallableCode::Defined();
  install_code.result_type = unit_.builtins.void_type;
  const WalkFrame install_frame = WalkFrame{}.WithBlock(&install_code.Body());

  mir::CallableCode value_code = mir::CallableCode::Defined();
  value_code.result_type = unit_.builtins.void_type;
  CallableBindings value_bindings(unit_, value_code);
  const WalkFrame value_frame =
      WalkFrame{}.WithBlock(&value_code.Body()).WithBindings(&value_bindings);

  if (auto own = PopulateNamespaceOwnStorage(
          *this, scope, subroutine_statics, package_scope_nodes, install_frame,
          value_frame);
      !own) {
    return std::unexpected(std::move(own.error()));
  }

  // The classes this unit holds. A namespace replicates nothing, so an object
  // of one belongs to no instance and its bodies name no scope's declarations
  // -- which is the same relation a module's scope carries, with the instance
  // absent rather than a second arrangement. Nothing replicates the class, so
  // its type-associated cells are its own and come up where the namespace
  // brings up the rest of what it owns (LRM 8.9, 10.5).
  std::vector<ClassDeclLowerer> class_lowerers;
  class_lowerers.reserve(scope.replicated_classes.size());
  for (const hir::ClassId hir_class : scope.replicated_classes) {
    class_lowerers.emplace_back(
        *this, hir_class, TranslateClass(hir_class), ClassObjectType(hir_class),
        hir_->classes.Get(hir_class), nullptr);
  }
  for (ClassDeclLowerer& class_lowerer : class_lowerers) {
    if (auto r = class_lowerer.DeclareShape(nullptr, {}); !r) {
      return std::unexpected(std::move(r.error()));
    }
  }
  for (ClassDeclLowerer& class_lowerer : class_lowerers) {
    if (auto r = class_lowerer.PopulateBodies(
            WalkFrame{},
            StorageBringUp{.install = install_frame, .value = value_frame});
        !r) {
      return std::unexpected(std::move(r.error()));
    }
  }

  for (const hir::StructuralSubroutineId id :
       scope.structural_subroutines.Ids()) {
    const hir::SubroutineDecl& src = scope.structural_subroutines.Get(id);
    ProcessLowerer subroutine_lowerer(
        *this, nullptr, scope.time_resolution, src.body, src.root_stmt,
        WalkFrame{}, package_scope_nodes, subroutine_statics.Get(id));
    auto code_or = subroutine_lowerer.Run(src);
    if (!code_or) return std::unexpected(std::move(code_or.error()));
    unit_.callables.Define(
        subroutine_callables.Get(id), mir::CallableDecl{
                                          .code = *std::move(code_or),
                                          .foreign = std::nullopt,
                                          .virtual_dispatch = std::nullopt});
  }

  // Each exported package subroutine (LRM 26.3, 35.7) is receiver-less: its
  // C entry point has no calling instance to recover, and enters the body this
  // unit's namespace already holds.
  for (const hir::ForeignExportDecl& export_decl : scope.foreign_exports) {
    const mir::CallableId callable_id =
        subroutine_callables.Get(export_decl.subroutine);
    const mir::TypeId result_type =
        unit_.callables.Get(callable_id).code.result_type;
    // A package subroutine has no receiver and a package has one form, so the
    // entry calls the body this unit's own namespace holds, naming the position
    // it already has in hand.
    ForeignExportEntry entry = SynthesizeForeignExportEntry(
        *this, WalkFrame{}, mir::UnitCallableTarget{.slot = callable_id},
        result_type, export_decl);
    // A name a namespace owns needs no entry beside its callable: that callable
    // is the program-global symbol and carries the prototype it publishes.
    unit_.callables.Add(
        mir::CallableDecl{
            .code = std::move(entry.code),
            .foreign = std::move(entry.linkage),
            .virtual_dispatch = std::nullopt});
  }

  PublishNamespaceStorageBringUp(
      *this, unit_, std::move(install_code), std::move(value_code));

  return Finish();
}

auto UnitLowerer::MakeExternalClassPointee(const hir::ExternalClassRef& ref)
    -> mir::TypeId {
  return unit_.types.Intern(
      mir::Type{mir::ObjectType{.of = MakeExternalClassRef(ref)}});
}

auto UnitLowerer::PublishedClass(const hir::ExternalClassRef& ref) const
    -> mir::DeclaredClassRef {
  return DeclaredClassIdentityOf(ref.unit_name, ref.class_path);
}

auto UnitLowerer::DeclaredClassIdentityOf(
    const std::string& unit_name, const support::DefPath& class_path) const
    -> mir::DeclaredClassRef {
  if (unit_name != unit_.name) {
    return mir::CrossUnitClassRef{
        .unit_name = unit_name, .class_path = class_path};
  }
  const auto it = declared_classes_.find(class_path);
  if (it == declared_classes_.end()) {
    throw InternalError(
        std::format(
            "UnitLowerer::DeclaredClassIdentityOf: this unit declares no class "
            "'{}' -- please report this as a bug",
            support::DisplayOf(class_path)));
  }
  return mir::IntraUnitClassRef{.class_id = it->second};
}

auto UnitLowerer::MakeExternalClassRef(const hir::ExternalClassRef& ref)
    -> mir::DeclaredClassRef {
  if (ref.unit_name != unit_.name) {
    unit_.ConsumeClassOf(ref.unit_name, ref.class_path);
  }
  return DeclaredClassIdentityOf(ref.unit_name, ref.class_path);
}

auto UnitLowerer::TranslateClassRef(const hir::ClassRef& ref)
    -> mir::DeclaredClassRef {
  return std::visit(
      Overloaded{
          [&](const hir::LocalClassRef& local) -> mir::DeclaredClassRef {
            return mir::IntraUnitClassRef{
                .class_id = TranslateClass(local.class_id)};
          },
          [&](const hir::ExternalClassRef& external) -> mir::DeclaredClassRef {
            return MakeExternalClassRef(external);
          }},
      ref);
}

auto UnitLowerer::MakeCrossUnitClassFieldTarget(
    const hir::ExternalClassPropertyTarget& target) -> mir::ClassFieldTarget {
  // The properties a class publishes are a prefix of its own storage, so the
  // position counted out of the signature is the slot that class gave.
  return mir::ClassFieldTarget{
      .owner = MakeExternalClassRef(
          hir::ExternalClassRef{
              .unit_name = target.unit_name, .class_path = target.class_path}),
      .slot = mir::FieldId{target.property.value}};
}

auto UnitLowerer::TakePublishedClass(const hir::ExternalClass& published)
    -> void {
  // A class extending nothing the source wrote extends the root every built
  // object does, as a class of this unit would. An interface class holds no
  // storage and no value is built of one, so nothing is placed after it (LRM
  // 8.26).
  std::optional<mir::ClassRef> base;
  if (!published.is_interface_class) {
    base = published.base.has_value()
               ? mir::AsClassRef(PublishedClass(*published.base))
               : mir::ClassRef{mir::ManagedObjectRootRef{}};
  }
  mir::ExternalClass record{
      .unit_name = published.unit_name,
      .class_path = published.class_path,
      .base = std::move(base),
      .is_interface_class = published.is_interface_class,
      .implements = {},
      .fields = {},
      .named_fields = {},
      .private_field_types = {},
      .behaviors = {},
      .overrides = {}};
  for (const hir::PublishedPropertyId id : published.properties.Ids()) {
    const hir::PublishedProperty& property = published.properties.Get(id);
    record.named_fields.push_back(
        mir::NamedField{
            .name = property.name,
            .slot = record.fields.Add(
                mir::FieldDecl{.type = TranslateType(property.type)})});
  }
  for (const hir::TypeId type : published.local_property_types) {
    record.private_field_types.push_back(TranslateType(type));
  }
  // A class that is a type of the instance declaring it (LRM 6.22) holds that
  // instance after its properties, which an object built from here has to have
  // room for like any other.
  if (published.takes_declaring_instance) {
    record.private_field_types.push_back(unit_.builtins.scope_ptr);
  }
  for (const hir::ExternalClassRef& iface : published.implements) {
    record.implements.push_back(PublishedClass(iface));
  }
  // What a class extending this one lays its table out from is the virtual
  // methods this one introduces, in the order it declares them.
  for (const hir::PublishedMethod& method : published.methods) {
    if (const auto* introduced =
            std::get_if<hir::IntroducesVirtual>(&method.dispatch)) {
      record.behaviors.push_back(
          mir::PublishedBehavior{
              .name = method.prototype.name, .is_pure = introduced->is_pure});
    }
  }
  for (const hir::PublishedOverride& overriding : published.overrides) {
    record.overrides.push_back(
        mir::PublishedOverride{
            .method = overriding.method,
            .behavior = PublishedSlot(overriding.behavior)});
  }
  unit_.external_classes.push_back(std::move(record));
}

auto UnitLowerer::RecordPublishedScopeClass(
    hir::ExternalScopeClassId hir_id, const hir::ExternalScopeClass& scope)
    -> void {
  const support::DefPath& class_path = scope.signature.class_path;
  // A class this unit published is its own: what is known of it is the
  // declaration this unit states, laid out where its scope's shape is, and it
  // is no dependency.
  if (std::holds_alternative<mir::IntraUnitClassRef>(
          ScopeClassIdentity(hir_id))) {
    return;
  }
  unit_.ConsumeClassOf(scope.unit_name, class_path);
  // The class of a scope of another unit -- one of its instances, or a
  // generate block inside one -- is a class of that unit, reached the way any
  // other is. It is a scope of the design hierarchy, so it extends the
  // library's root of that tree, and what the scope published is its first
  // fields. What the unit adds while lowering its bodies is placed after them
  // and named by nothing, so the record states none of it; no other unit
  // extends the class, and its own unit makes every object of it, so nothing
  // here needs its size. A published subroutine is one of its methods, called
  // directly.
  mir::ExternalClass record{
      .unit_name = scope.unit_name,
      .class_path = class_path,
      .base = mir::ClassRef{mir::ObjectTreeRootRef{}},
      .is_interface_class = false,
      .implements = {},
      .fields = {},
      .named_fields = {},
      .private_field_types = {},
      .behaviors = {},
      .overrides = {}};
  std::vector<mir::FieldId> slots;
  std::vector<mir::TypeId> field_types;
  for (PublishedField& field : PublishedFieldsOf(scope.signature)) {
    const mir::FieldId slot =
        record.fields.Add(mir::FieldDecl{.type = field.type});
    if (field.name.has_value()) {
      record.named_fields.push_back(
          mir::NamedField{.name = *std::move(field.name), .slot = slot});
    }
    slots.push_back(slot);
    field_types.push_back(field.type);
  }
  external_scope_layouts_.emplace(
      hir_id,
      ScopeClassLayout{
          .cls =
              mir::CrossUnitClassRef{
                  .unit_name = scope.unit_name, .class_path = class_path},
          .field_types = std::move(field_types),
          .published = PublishedLayoutAt(scope.signature, slots)});
  if (mir::FindExternalClass(
          unit_.external_classes, scope.unit_name, class_path) == nullptr) {
    unit_.external_classes.push_back(std::move(record));
  }
}

auto UnitLowerer::ScopeClassLayoutOf(hir::ExternalScopeClassId hir_id) const
    -> const ScopeClassLayout& {
  // What is known of a class is one question with two answers: the layout this
  // unit gave a class of its own, and the record of what another unit
  // published.
  return std::visit(
      Overloaded{
          [&](const mir::IntraUnitClassRef& own) -> const ScopeClassLayout& {
            const auto it = published_scopes_.find(own.class_id);
            if (it == published_scopes_.end()) {
              throw InternalError(
                  "UnitLowerer::ScopeClassLayoutOf: every scope class of this "
                  "unit is laid out before any body lowers");
            }
            return it->second.layout;
          },
          [&](const mir::CrossUnitClassRef&) -> const ScopeClassLayout& {
            const auto it = external_scope_layouts_.find(hir_id);
            if (it == external_scope_layouts_.end()) {
              throw InternalError(
                  "UnitLowerer::ScopeClassLayoutOf: every scope class of "
                  "another unit this unit records is laid out before any body "
                  "lowers");
            }
            return it->second;
          }},
      ScopeClassIdentity(hir_id));
}

namespace {

// The member holding what a generate construct built: one handle for a
// construct that builds at most one block, and a sequence of them for a loop.
// Each block is published under a class of its own, and which of them are one
// body is decided by lowering the bodies, which a unit's publication does not
// depend on -- an edit to a body never changes what a referrer compiles
// against. So the handle holds the base every block extends, as a C++ header
// keeps a pointer to a base whose derived classes its implementation defines.
auto BlockHandles(const mir::CompilationUnit& unit, bool one_per_block)
    -> mir::TypeId {
  return one_per_block ? unit.types.Intern(
                             mir::Type{mir::VectorType{
                                 .element = unit.builtins.scope_ptr}})
                       : unit.builtins.scope_ptr;
}

}  // namespace

auto UnitLowerer::PublishedFieldsOf(const hir::ScopeClassSignature& signature)
    const -> std::vector<PublishedField> {
  std::vector<PublishedField> fields;
  fields.reserve(
      signature.members.size() + signature.generates.size() +
      signature.disable_targets.size());
  for (const hir::PublishedMember& member : signature.members) {
    fields.push_back(
        PublishedField{
            .name = member.name,
            .type =
                MemberCellType(TranslateType(member.type), member.storage)});
  }
  for (const hir::PublishedGenerate& generate : signature.generates) {
    const bool loop = std::visit(
        Overloaded{
            [](const hir::PublishedLoop&) { return true; },
            [](const hir::PublishedChoice&) { return false; }},
        generate);
    fields.push_back(
        PublishedField{
            .name = std::nullopt, .type = BlockHandles(unit_, loop)});
  }
  const mir::TypeId cancellation_target = unit_.types.Intern(
      mir::Type{mir::RuntimeLibraryType{
          .kind = mir::RuntimeLibraryKind::kCancellationTarget}});
  for (std::size_t i = 0; i < signature.disable_targets.size(); ++i) {
    fields.push_back(
        PublishedField{.name = std::nullopt, .type = cancellation_target});
  }
  return fields;
}

auto UnitLowerer::PublishedLayoutAt(
    const hir::ScopeClassSignature& signature,
    std::span<const mir::FieldId> slots) -> PublishedScopeLayout {
  const std::size_t members = signature.members.size();
  const std::size_t generates = signature.generates.size();
  const std::size_t disable_targets = signature.disable_targets.size();
  if (slots.size() != members + generates + disable_targets) {
    throw InternalError(
        "UnitLowerer::PublishedLayoutAt: a published class holds one field "
        "per thing its scope published");
  }
  const auto part = [&](std::size_t first, std::size_t count) {
    return std::vector<mir::FieldId>(
        slots.begin() + static_cast<std::ptrdiff_t>(first),
        slots.begin() + static_cast<std::ptrdiff_t>(first + count));
  };
  return PublishedScopeLayout{
      .members = {members, part(0, members)},
      .generates = {generates, part(members, generates)},
      .disable_targets = {
          disable_targets, part(members + generates, disable_targets)}};
}

auto UnitLowerer::TranslateClassPropertyTarget(
    const hir::LocalClassPropertyTarget& local) const -> mir::ClassFieldTarget {
  const mir::ClassId owner = TranslateClass(local.owner);
  return mir::ClassFieldTarget{
      .owner = mir::IntraUnitClassRef{owner},
      .slot = GetClassShape(owner).field_translation.Get(local.field)};
}

auto UnitLowerer::MakeExternalStaticPropertyRef(
    const hir::ExternalStaticPropertyTarget& target)
    -> mir::ExternalStaticPropertyRef {
  unit_.ConsumeClassOf(target.unit_name, target.class_path);
  return mir::ExternalStaticPropertyRef{
      .unit_name = target.unit_name,
      .class_path = target.class_path,
      .property_name = target.property_name};
}

auto UnitLowerer::MakeExternalMethodTarget(
    const hir::ExternalClassMethodTarget& target)
    -> mir::ExternalUnitClassMethodTarget {
  unit_.ConsumeClassOf(target.unit_name, target.class_path);
  return mir::ExternalUnitClassMethodTarget{
      .unit_name = target.unit_name,
      .class_path = target.class_path,
      .method_name = target.method_name};
}

auto UnitLowerer::MakeExternalMethodOverride(
    const hir::ExternalDispatchSlot& slot) -> mir::OverridesExternalSlot {
  unit_.ConsumeClassOf(slot.unit_name, slot.class_path);
  return PublishedSlot(slot);
}

auto UnitLowerer::MakeExternalVirtualSlot(const hir::ExternalDispatchSlot& slot)
    -> mir::ExternalVirtualSlot {
  unit_.ConsumeClassOf(slot.unit_name, slot.class_path);
  return mir::ExternalVirtualSlot{
      .unit_name = slot.unit_name,
      .class_path = slot.class_path,
      .ordinal = mir::BehaviorOrdinal{slot.behavior.value}};
}

auto UnitLowerer::LocalVirtualSlotOf(const hir::LocalClassMethodTarget& method)
    const -> std::optional<mir::VirtualSlot> {
  const mir::ClassId owner = TranslateClass(method.owner);
  const mir::CallableId callable = TranslateMethod(method.owner, method.method);
  // A method's dispatch role is read from the class's declaration rather than
  // its body: a peer body may name the method before the class is lowered.
  return GetClassShape(owner)
      .callable_signatures.Get(callable)
      .virtual_dispatch.transform([&](const mir::VirtualDispatchRole& role) {
        return CanonicalVirtualSlot(owner, callable, role);
      });
}

auto UnitLowerer::MakeNamespaceCallableTarget(
    const hir::ExternalUnitSubroutineRef& ref) -> mir::DirectTarget {
  // A call into this unit's own namespace has the arena the body lives in, so
  // it names the position; one into another unit has only the identifier that
  // unit published, and consuming that signature is what makes the unit a
  // dependency whose header and link edge the backend then emits.
  if (ref.unit_name == unit_.name) {
    const std::optional<mir::CallableId> body =
        mir::CallableNamed(unit_.named_callables, ref.subroutine_name);
    if (!body.has_value()) {
      throw InternalError(
          "MakeNamespaceCallableTarget: this unit's namespace publishes no "
          "subroutine under the identifier a call inside it spells");
    }
    return mir::UnitCallableTarget{.slot = *body};
  }
  unit_.ConsumeNamespaceOf(ref.unit_name);
  return mir::ExternalUnitCallableTarget{
      .unit_name = ref.unit_name, .callable_name = ref.subroutine_name};
}

auto UnitLowerer::MakeExternalUnitMethodTarget(
    hir::ExternalScopeClassId scope_class,
    hir::PublishedCallableId callable) const -> mir::DirectTarget {
  const hir::ExternalScopeClass& scope =
      Hir().external_scope_classes.Get(scope_class);
  return std::visit(
      Overloaded{
          [&](const mir::IntraUnitClassRef& own) -> mir::DirectTarget {
            return mir::CallableTarget{
                .owner = own.class_id, .slot = PublishedMethodOf(callable)};
          },
          [&](const mir::CrossUnitClassRef& other) -> mir::DirectTarget {
            return mir::ExternalUnitClassMethodTarget{
                .unit_name = other.unit_name,
                .class_path = other.class_path,
                .method_name = scope.signature.callables.Get(callable).name};
          }},
      ScopeClassIdentity(scope_class));
}

}  // namespace lyra::lowering::hir_to_mir
