#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <deque>
#include <expected>
#include <map>
#include <memory>
#include <optional>
#include <set>
#include <span>
#include <string>
#include <unordered_set>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/id_allocator.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/base/translation.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/class_declaration.hpp"
#include "lyra/hir/class_ref.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/published_scope.hpp"
#include "lyra/hir/scope_class.hpp"
#include "lyra/hir/structural_data_object.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/hir/subroutine_prototype.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/callee_interface.hpp"
#include "lyra/lowering/hir_to_mir/class_code.hpp"
#include "lyra/lowering/hir_to_mir/class_decl_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/class_definition.hpp"
#include "lyra/lowering/hir_to_mir/covergroup_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/declaration_initializer.hpp"
#include "lyra/lowering/hir_to_mir/declared_initial_value.hpp"
#include "lyra/lowering/hir_to_mir/declared_scope.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/design_root_inputs.hpp"
#include "lyra/lowering/hir_to_mir/expression/dpi_call.hpp"
#include "lyra/lowering/hir_to_mir/pattern_rendering.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/static_var_binding.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/value_operations.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/block_body_ordinal.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/support/def_path.hpp"
#include "lyra/support/runtime_class.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// Whether `signature` is what a generate block published: a block keeps the
// scope that built it (LRM 27.3), which a unit's own scope does not.
auto IsPublishedBlock(const hir::ScopeClassSignature& signature) -> bool {
  return !signature.class_path.data.empty() &&
         std::holds_alternative<support::GenerateBlockStep>(
             signature.class_path.data.back());
}

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
    const mir::CompilationUnit& unit, const mir::Block& body,
    std::unordered_set<std::string>& units) {
  for (const mir::ExprId id : body.exprs.Ids()) {
    const auto* reference =
        std::get_if<mir::ReferenceExpr>(&body.exprs.Get(id).data);
    if (reference == nullptr) {
      continue;
    }
    if (const auto* ref =
            std::get_if<mir::StaticVariableRef>(&reference->target)) {
      if (const std::optional<std::string>& defining =
              unit.static_variables.Get(ref->variable).defining_unit) {
        units.emplace(*defining);
      }
    }
    if (const auto* ref =
            std::get_if<mir::StaticPropertyRef>(&reference->target)) {
      if (const std::optional<std::string>& defining =
              unit.GetClass(ref->owner).defining_unit) {
        units.emplace(*defining);
      }
    }
  }
  for (const mir::BlockId id : body.child_scopes.Ids()) {
    GatherUnitsReadBy(unit, body.child_scopes.Get(id), units);
  }
}

// The other units whose namespace-owned cells `body` reads -- a variable of the
// namespace itself, or one of its classes' type-associated cells, the two being
// one kind of storage differing in how far the name is qualified. Each named
// once and in name order, because what is done with the answer is emitting a
// call per unit and a design compiles to the same program each time.
auto UnitsReadBy(const mir::CompilationUnit& unit, const mir::Block& body)
    -> std::vector<std::string> {
  std::unordered_set<std::string> gathered;
  GatherUnitsReadBy(unit, body, gathered);
  std::vector<std::string> units(gathered.begin(), gathered.end());
  std::ranges::sort(units);
  return units;
}

// Brings the cells the unit's namespace owns directly up in the two bodies the
// design root runs at time zero: `install_frame` receives each cell's declared
// representation and default, and `value_frame` each LRM 10.5 value
// initializer. What this covers is the namespace's own -- its variables (LRM
// 26.2) and the static-lifetime variables its subroutines declare, which are
// the same one-program-global-cell storage; the cells the unit's classes own
// join the same two bodies from the class lowering. The design root installs
// every unit before initializing any, so a value initializer always reaches
// installed storage. Neither this unit's cells nor another's sit on an
// instance, so an initializer here lowers with no enclosing scope and no
// receiver: a sibling cell is reached by its position in this arena, and
// another unit's by the identifier that unit published (`unit::name`).
auto PopulateNamespaceOwnStorage(
    UnitLowerer& unit_lowerer, const hir::StructuralScope& scope,
    const base::Translation<hir::StructuralSubroutineId, StaticVarBindings>&
        subroutine_statics,
    const DeclaredScopes& scopes, const WalkFrame& install_frame,
    const WalkFrame& value_frame) -> diag::Result<void> {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& install_block = *install_frame.current_block;
  mir::Block& value_block = *value_frame.current_block;

  // The package root scope is an ExprLowerer over its own expressions: a
  // variable initializer's operands are literals, operators, and by-name
  // package symbols, none of which reach a class or a `self`.
  const StructuralScopeLowerer expr_lowerer(unit_lowerer, scope);

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
          "a net declared in a package is not yet supported");
    }
    const mir::TypeId value_type = unit_lowerer.TranslateType(d.type);
    const mir::TypeId cell_type = mir::ObservableCellOf(unit.types, value_type);
    if (!unit.types.Get(cell_type).IsCapabilityWrapper()) {
      return diag::Fail(
          diag::DiagCode::kUnsupportedExpressionForm,
          "a package variable of this type is not yet supported");
    }
    const mir::StaticVariableId variable = unit.static_variables.Add(
        mir::StaticVariableDecl{
            .type = cell_type, .defining_unit = std::nullopt});
    unit.named_static_variables.push_back(
        mir::NamedStaticVariable{.name = d.name, .variable = variable});

    // The install body gives the cell its declared representation and default.
    const mir::ExprId prototype =
        BuildDeclaredInitialValue(unit_lowerer, install_frame, d.type);
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

  // A static-lifetime variable of a package subroutine is one program-global
  // cell like the package's own variables (LRM 6.21, 26.2), so it comes up in
  // these same two bodies -- once before any process starts, rather than on
  // each entry to the subroutine that declares it.
  for (const hir::StructuralSubroutineId id :
       scope.structural_subroutines.Ids()) {
    const hir::SubroutineDecl& src = scope.structural_subroutines.Get(id);
    const StaticVarBindings& statics = subroutine_statics.Get(id);
    ProcessLowerer body_lowerer(
        unit_lowerer, nullptr, scope.time_resolution, src.body, std::nullopt,
        WalkFrame{}.WithScopeName(unit_lowerer.Hir().fixed_name), scopes,
        statics);
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
// 18.14.1). A namespace is not instantiated, so it is named by its name, which
// is how the run holds that RNG.
void WrapInNamespaceStaticInitExtent(
    const UnitLowerer& unit_lowerer, mir::CompilationUnit& unit,
    mir::CallableCode& code) {
  mir::Block extent;
  const mir::ExprId name = extent.exprs.Add(
      mir::MakeStringLiteral(unit.builtins.machine_c_string, unit.name));
  AppendRuntimeEffectStmt(
      unit_lowerer, extent, support::BuiltinFn::kEnterNamespaceStaticInit,
      {name});

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
      mir::MakeStringLiteral(unit.builtins.machine_c_string, unit.name));
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
  const mir::ExprId took_it = outer.exprs.Add(
      mir::MakeBinary(
          unit, outer, mir::BinaryOp::kInequality, claimed,
          mir::BuildMachineIntLiteral(unit, outer, 0),
          unit.builtins.machine_bool));

  mir::Block bring_up;
  for (const std::string& read : reads) {
    bring_up.AppendStmt(
        mir::ExprStmt{
            .expr = bring_up.exprs.Add(
                mir::Expr{
                    .data =
                        mir::CallExpr{
                            .callee =
                                mir::Direct{
                                    .target =
                                        mir::ExternalBringUpTarget{
                                            .unit_name = read,
                                            .entry = mir::BringUpEntry::
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
  const std::vector<std::string> reads = UnitsReadBy(unit, value_code.Body());
  WrapInNamespaceStaticInitExtent(unit_lowerer, unit, value_code);
  GuardNamespaceInitialization(unit_lowerer, unit, value_code, reads);

  unit.content = mir::BroughtUpNamespace{
      .install_storage = unit.callables.Add(
          mir::CallableDecl{
              .code = std::move(install_code),
              .foreign = std::nullopt,
              .virtual_dispatch = std::nullopt,
              .defining_unit = std::nullopt}),
      .initialize_storage = unit.callables.Add(
          mir::CallableDecl{
              .code = std::move(value_code),
              .foreign = std::nullopt,
              .virtual_dispatch = std::nullopt,
              .defining_unit = std::nullopt})};
}

// Publishes the one body that makes an object of this unit. A unit that
// instantiates this one read what this one published, which states what may
// be reached and never how much storage an object takes -- so it asks here
// instead of making one, and the construction it would otherwise have written
// sits on the side that knows the answer. The party that builds the design's
// tops asks the same way, having no more than any other referrer.
//
// Beside where the object hangs, it takes the construction table the object
// is built with -- handed in by whoever asks, unless `table` says the unit
// states its own -- and the values an instance is handed at construction, and
// passes them on.
auto PublishObjectEntry(
    mir::CompilationUnit& unit, mir::ClassId root, mir::ClassId published,
    std::span<const ConstructionValue> handed,
    const ConstructionTableSource& table) -> mir::CallableId {
  const auto object_of = [&](mir::ClassId cls) {
    return unit.types.Intern(mir::Type{mir::ObjectType{.of = cls}});
  };
  const auto owning_pointer = [&](mir::ClassId cls) {
    return mir::BoxOf(unit.types, object_of(cls));
  };
  // What it makes is the class that realizes the object; what it answers with
  // is what the unit published of it, that being all a referrer has.
  const mir::TypeId made = owning_pointer(root);
  const mir::TypeId owning = owning_pointer(published);

  mir::CallableCode code = mir::CallableCode::Defined();
  const mir::TypeId table_type = mir::ConstructionTablePointer(unit.types);
  const mir::LocalId parent = code.AddLocal(unit.builtins.scope_ptr);
  const mir::LocalId segment = code.AddLocal(unit.builtins.hierarchy_segment);
  code.params = {parent, segment};
  code.result_type = owning;

  mir::Block& body = code.Body();
  std::vector<mir::ExprId> arguments{
      body.ReadLocal(parent, unit.builtins.scope_ptr),
      body.ReadLocal(segment, unit.builtins.hierarchy_segment),
      std::visit(
          Overloaded{
              [&](const ConstructionTableSupplied&) {
                const mir::LocalId handed_in = code.AddLocal(table_type);
                code.params.push_back(handed_in);
                return body.ReadLocal(handed_in, table_type);
              },
              [&](const mir::ClassConstantRef& own) {
                return body.exprs.Add(
                    mir::MakeAddressOfExpr(
                        body.exprs.Add(
                            mir::Expr{
                                .data = mir::ReferenceExpr{.target = own},
                                .type =
                                    mir::ConstructionTableType(unit.types)}),
                        table_type));
              }},
          table)};
  for (const ConstructionValue& value : handed) {
    const mir::LocalId param = code.AddLocal(value.type);
    code.params.push_back(param);
    arguments.push_back(body.ReadLocal(param, value.type));
  }
  const mir::ExprId built = body.exprs.Add(
      mir::MakeNewExpr(
          body.exprs.Add(
              mir::MakeObjectConstructionExpr(
                  object_of(root), std::move(arguments))),
          made));
  body.AppendStmt(mir::ReturnStmt{.value = ObjectAs(body, built, owning)});

  return unit.callables.Add(
      mir::CallableDecl{
          .code = std::move(code),
          .foreign = std::nullopt,
          .virtual_dispatch = std::nullopt,
          .defining_unit = std::nullopt});
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
            // A port's member is bound by its connection after the object is
            // built (LRM 23.3.3), and an input port's again by a force on it
            // (LRM 10.6.2). An input's is mutable because a force re-points
            // it; nothing writes a value through it, the language admitting no
            // assignment to an input (LRM 23.3.3.2).
            const auto mutability = [&] {
              switch (reference.binding) {
                case hir::ReferenceBinding::kRef:
                case hir::ReferenceBinding::kInput:
                  return mir::Mutability::kMutable;
                case hir::ReferenceBinding::kConstRef:
                  return mir::Mutability::kReadOnly;
              }
              throw InternalError("MemberCellType: unknown reference binding");
            }();
            return unit_.types.Intern(
                mir::Type{mir::ReferenceWrapperType{
                    .pointee = value_type, .mutability = mutability}});
          },
          [&](const hir::BorrowedObjectStorage&) {
            return BorrowedObjectHandles(unit_.types, value_type);
          }},
      storage);
}

auto UnitLowerer::ScopeClassIdentity(hir::ScopeClassId hir_id) const
    -> mir::ClassId {
  const auto it = scope_class_translations_.find(hir_id);
  if (it == scope_class_translations_.end()) {
    throw InternalError(
        "UnitLowerer::ScopeClassIdentity: a scope class this unit names took "
        "no identity before types translated -- please report this as a bug");
  }
  return it->second;
}

auto UnitLowerer::UnitObjectType(hir::ScopeClassId hir_id) const
    -> mir::TypeId {
  return unit_.types.Intern(
      mir::Type{mir::ObjectType{.of = ScopeClassIdentity(hir_id)}});
}

auto UnitLowerer::SettlePublishedScope(
    mir::ClassId published, const hir::ScopeClassSignature& signature)
    -> const PublishedScopeLayout& {
  if (const auto settled = published_scopes_.find(published);
      settled != published_scopes_.end()) {
    return settled->second.layout.published;
  }
  ClassShape shape;
  shape.identity = signature.class_path;
  shape.base = mir::ClassRef{mir::ObjectTreeRootRef{}};
  shape.self_pointer_type = SelfPointerTo(published);
  // It is the unit's object as other units see it, so it is the same time
  // scope (LRM 3.14.2.2) as the classes extending it, which is the unit's.
  shape.time_resolution = hir_->root_scope.time_resolution;
  shape.execution = ScopeExecution();
  // Each published subroutine is a virtual method it introduces, its identity
  // reserved here at the position the signature gave the subroutine, so a body
  // reaching another instance of this unit calls it by position before any
  // class realizing the scope is built. Which body runs is the answer of the
  // class the object is, which it carries.
  std::vector<CallableSignature> signatures(
      signature.callables.size(),
      CallableSignature{
          .virtual_dispatch = mir::IntroducesVirtualSlot{.is_pure = true}});
  base::IdAllocator<mir::CallableId> methods;
  base::Translation<hir::PublishedCallableId, PublishedSubroutine> subroutines{
      signature.callables.size()};
  for (const hir::SubroutinePrototype& callable : signature.callables) {
    methods.Take();
    subroutines.Append(
        PublishedSubroutine{
            .name = callable.name,
            .prototype =
                PrototypeOf(*this, callable, shape.self_pointer_type)});
  }

  // Then one virtual method per operation of each class the unit declares in
  // the scope, which each body realizing the scope answers with the code it
  // compiled for it: the constructor, every method the declaration does not
  // leave pure (LRM 8.21), and what `randomize()` asks of the class (LRM
  // 18.6). Which there are and what each takes are read off the declarations,
  // so a body whose construction built none of the scope states them alike.
  std::vector<ClassCodeSlot> class_code;
  const auto introduce = [&](mir::ClassId cls, const support::DefPath& path,
                             ClassCodePoint point, std::string_view operation,
                             const mir::CallableCode& code) {
    class_code.push_back(
        ClassCodeSlot{
            .cls = cls,
            .point = point,
            .slot = methods.Take(),
            .name = std::format("{} {}", support::DisplayOf(path), operation),
            .operation = code});
    signatures.push_back(
        CallableSignature{
            .virtual_dispatch = mir::IntroducesVirtualSlot{.is_pure = true}});
  };
  for (const hir::ClassId hir_class : InstanceClassesOf(signature.class_path)) {
    const hir::ClassDeclaration& declared =
        hir_->classes.Declaration(hir_class);
    const mir::ClassId cls = ClassIdentity(hir_class);
    const support::DefPath& path = hir_->classes.PathOf(hir_class);
    introduce(
        cls, path, ClassConstructorCode{}, "new",
        ClassConstructorPrototype(*this, hir_class));
    for (const hir::MethodId method : declared.methods.Ids()) {
      const hir::DeclaredMethod& m = declared.methods.Get(method);
      if (hir::IsLeftAbstract(m.dispatch)) continue;
      introduce(
          cls, path,
          ClassCallableCode{.callable = mir::CallableId{method.value}},
          m.prototype.name, ClassMethodPrototype(*this, hir_class, method));
    }
    if (const std::optional<RandomizationHooks>& hooks =
            RandomizationHooksOf(hir_class)) {
      for (const auto& [body, callback] : hooks->Callbacks()) {
        introduce(
            cls, path, ClassCallableCode{.callable = body},
            std::format("hook {}", support::LibraryCallbackName(callback)),
            ClassHookPrototype(*this, hir_class, callback));
      }
    }
    for (const auto& [method, form] : ReportingFormsOf(hir_class)) {
      introduce(
          cls, path, ClassCallableCode{.callable = form},
          std::format(
              "{} reporting", declared.methods.Get(method).prototype.name),
          ClassMethodPrototype(*this, hir_class, method));
    }
  }
  // A covergroup's class has four operations besides its constructor, each
  // the code of the scope declaring it.
  for (const hir::CovergroupId covergroup :
       InstanceCovergroupsOf(signature.class_path)) {
    const mir::ClassId cls = CovergroupClassIdentity(covergroup);
    const support::DefPath& path = hir_->covergroups.PathOf(covergroup);
    const auto operation = [&](ClassCodePoint point, std::string_view name) {
      introduce(
          cls, path, point, name,
          CovergroupOperationPrototype(*this, covergroup, point));
    };
    operation(ClassConstructorCode{}, "new");
    for (const auto& [callable, name] : CovergroupOperations()) {
      operation(ClassCallableCode{.callable = callable}, name);
    }
  }
  shape.callable_signatures = {signatures.size(), std::move(signatures)};

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
      published, PublishedScope{
                     .layout =
                         ScopeClassLayout{
                             .cls = published,
                             .field_types = std::move(field_types),
                             .published = PublishedLayoutAt(signature, slots)},
                     .subroutines = std::move(subroutines),
                     .class_code = std::move(class_code)});
  return settled.first->second.layout.published;
}

auto UnitLowerer::PublishedClassOf(const support::DefPath& scope) const
    -> mir::ClassId {
  for (const hir::ScopeClassId id : hir_->scope_classes.Ids()) {
    if (hir_->scope_classes.IsOwn(id) &&
        hir_->scope_classes.KeyOf(id).class_path == scope) {
      return ScopeClassIdentity(id);
    }
  }
  throw InternalError(
      std::format(
          "UnitLowerer::PublishedClassOf: the unit published no class of its "
          "scope '{}' -- please report this as a bug",
          support::DisplayOf(scope)));
}

auto UnitLowerer::InstanceScopeOf(hir::CovergroupId covergroup) const
    -> std::optional<support::DefPath> {
  const hir::PublishedCovergroup& published =
      hir_->covergroups.Published(covergroup);
  if (published.embedding.has_value()) {
    return hir_->classes.Declaration(*published.embedding).instance_scope;
  }
  return support::DeclaringScopeClassOf(published.path);
}

auto UnitLowerer::InstanceCovergroupsOf(const support::DefPath& scope) const
    -> std::vector<hir::CovergroupId> {
  std::vector<hir::CovergroupId> covergroups;
  for (const hir::CovergroupId covergroup : hir_->covergroups.OwnIds()) {
    if (InstanceScopeOf(covergroup) == scope) {
      covergroups.push_back(covergroup);
    }
  }
  std::ranges::sort(
      covergroups, {}, [&](hir::CovergroupId covergroup) -> const auto& {
        return hir_->covergroups.PathOf(covergroup);
      });
  return covergroups;
}

auto UnitLowerer::InstanceClassesOf(const support::DefPath& scope) const
    -> std::vector<hir::ClassId> {
  std::vector<hir::ClassId> classes;
  for (const hir::ClassId cls : hir_->classes.OwnIds()) {
    if (hir_->classes.Declaration(cls).instance_scope == scope) {
      classes.push_back(cls);
    }
  }
  std::ranges::sort(classes, {}, [&](hir::ClassId cls) -> const auto& {
    return hir_->classes.PathOf(cls);
  });
  return classes;
}

auto UnitLowerer::ClassCodeSlotOf(
    mir::ClassId published, mir::ClassId cls, ClassCodePoint point) const
    -> mir::CallableId {
  for (const ClassCodeSlot& code : published_scopes_.at(published).class_code) {
    if (code.cls == cls && code.point == point) return code.slot;
  }
  throw InternalError(
      std::format(
          "UnitLowerer::ClassCodeSlotOf: the class {} published of a scope "
          "introduces no method for {} of the class {} the scope declares "
          "-- please report this as a bug",
          published.value,
          std::visit(
              Overloaded{
                  [](const ClassConstructorCode&) {
                    return std::string{"the constructor"};
                  },
                  [](const ClassCallableCode& code) {
                    return std::format("callable {}", code.callable.value);
                  }},
              point),
          cls.value));
}

auto UnitLowerer::ClassCodePrototypeOf(
    mir::ClassId published, mir::ClassId cls, ClassCodePoint point) const
    -> const mir::CallableCode& {
  for (const ClassCodeSlot& code : published_scopes_.at(published).class_code) {
    if (code.cls == cls && code.point == point) return code.operation;
  }
  throw InternalError(
      "UnitLowerer::ClassCodePrototypeOf: a scope's published class "
      "introduces a method for every operation of each class the scope "
      "declares -- please report this as a bug");
}

auto UnitLowerer::TakeRandomizationHooks(hir::ClassId cls)
    -> std::optional<RandomizationHooks> {
  const hir::ClassDeclaration& declared = hir_->classes.Declaration(cls);
  // Each method the class declares is the callable at its position, so a
  // method is named alike in this unit and in every unit reading the
  // declaration.
  base::IdAllocator<mir::CallableId> callables;
  for (std::size_t m = 0; m < declared.methods.size(); ++m) {
    callables.Take();
  }
  const auto calls_declared = [&](hir::MethodId method) {
    return RandomizationHooks::CallsDeclared{
        .body = callables.Take(), .method = method};
  };
  if (declared.is_interface_class) {
    return std::nullopt;
  }
  return RandomizationHooks{
      .rand_variables = callables.Take(),
      .constrain = callables.Take(),
      .rand_objects = callables.Take(),
      .pre_randomize =
          hir::MethodReplacing(
              declared.methods, hir::RandomizationHook::kPreRandomize)
              .transform(calls_declared),
      .post_randomize =
          hir::MethodReplacing(
              declared.methods, hir::RandomizationHook::kPostRandomize)
              .transform(calls_declared)};
}

auto UnitLowerer::TakeReportingForms(hir::ClassId cls)
    -> std::vector<std::pair<hir::MethodId, mir::CallableId>> {
  const hir::ClassDeclaration& declared = hir_->classes.Declaration(cls);
  base::IdAllocator<mir::CallableId> callables;
  for (std::size_t m = 0; m < declared.methods.size(); ++m) {
    callables.Take();
  }
  if (const std::optional<RandomizationHooks>& hooks =
          RandomizationHooksOf(cls)) {
    for (std::size_t h = 0; h < hooks->Callbacks().size(); ++h) {
      callables.Take();
    }
  }
  // A pure method has no body (LRM 8.21), and the method making an embedded
  // covergroup is written whole rather than compiled from a body.
  std::vector<std::pair<hir::MethodId, mir::CallableId>> forms;
  for (const hir::MethodId method : declared.methods.Ids()) {
    const hir::DeclaredMethod& m = declared.methods.Get(method);
    if (hir::IsLeftAbstract(m.dispatch) || m.makes.has_value() ||
        !ReportParamTypeOf(unit_, m.prototype.interface.kind).has_value()) {
      continue;
    }
    forms.emplace_back(method, callables.Take());
  }
  return forms;
}

auto UnitLowerer::PublishUnitDeclarations() -> diag::Result<void> {
  unit_.name = hir_->name;
  unit_.body_name = hir_->body_name;
  unit_.source_name = hir_->source_name;

  // Every class the unit names takes its identity before any declaration
  // settles, whichever unit declares it, and so does the class each covergroup
  // is written as (LRM 19.3): a type names one, and so does a declaration
  // naming the class it extends or whose behavior it overrides. Nothing here
  // reads a declaration, so no declaration has to settle before another.
  class_identities_ =
      base::Translation<hir::ClassId, mir::ClassId>{hir_->classes.size()};
  for (std::size_t i = 0; i < hir_->classes.size(); ++i) {
    class_identities_.Append(unit_.DeclareClass());
  }
  covergroup_identities_ = base::Translation<hir::CovergroupId, mir::ClassId>{
      hir_->covergroups.size()};
  for (std::size_t i = 0; i < hir_->covergroups.size(); ++i) {
    covergroup_identities_.Append(unit_.DeclareClass());
  }
  for (const hir::ClassId hir_id : hir_->classes.OwnIds()) {
    randomization_hooks_.emplace(hir_id, TakeRandomizationHooks(hir_id));
    reporting_forms_.emplace(hir_id, TakeReportingForms(hir_id));
  }

  // Every HIR type is MIR-representable: AST-to-HIR rejects the forms MIR has
  // no shape for, so this projection never fails.
  // A composite type's translation reads the translations of its components,
  // which HIR minted before it, so the answers land one at a time and each one
  // can see the ones before it.
  type_translations_ =
      base::Translation<hir::TypeId, mir::TypeId>{hir_->types.size()};
  for (const hir::TypeId hir_id : hir_->types.Ids()) {
    type_translations_.Append(
        unit_.types.Intern(TranslateType(hir_->types.Get(hir_id))));
  }
  // A struct or a union is named before what it holds is settled, since its
  // members may be of types minted after it; each is defined once every type
  // has its translation. The methods of one this unit declares, which take
  // values of its type and read what its members hold, follow once every
  // record is defined.
  for (const hir::TypeId hir_id : hir_->types.Ids()) {
    const mir::RecordId* id =
        mir::RecordOf(unit_.types.Get(type_translations_.Get(hir_id)));
    if (id == nullptr) continue;
    DefineRecord(hir_->types.Get(hir_id), *id);
  }
  for (const hir::TypeId hir_id : hir_->types.Ids()) {
    const mir::TypeId translated = type_translations_.Get(hir_id);
    const mir::RecordId* id = mir::RecordOf(unit_.types.Get(translated));
    if (id == nullptr) continue;
    DefineRecordMethods(hir_->types.Get(hir_id), *id, translated);
  }

  for (const hir::ClassId published : hir_->classes.ImportedIds()) {
    DeclarePublishedClass(published);
  }
  // The class another unit writes for each of its covergroups this one names,
  // laid out from what it published (LRM 26.3).
  for (const hir::CovergroupId published : hir_->covergroups.ImportedIds()) {
    const mir::ClassId id = CovergroupClassIdentity(published);
    if (hir_->covergroups.IsPublished(published)) {
      unit_.DefineClass(id, PublishedCovergroupClass(*this, published));
      continue;
    }
    const hir::CovergroupKey& key = hir_->covergroups.KeyOf(published);
    DeclareIncompleteClass(id, key.unit_name, key.path);
  }

  // Each scope class of another unit this unit reaches is declared before any
  // body lowers, so a body reaching a child's member reads that declaration
  // rather than building one. A scope class only named here -- the class of
  // an object a type names, never reached into -- is declared with nothing of
  // what it holds.
  for (const hir::ScopeClassId id : hir_->scope_classes.Ids()) {
    if (hir_->scope_classes.IsRecorded(id)) {
      DeclarePublishedScopeClass(id, hir_->scope_classes.Get(id));
    } else if (!hir_->scope_classes.IsOwn(id)) {
      const hir::ScopeClassKey& key = hir_->scope_classes.KeyOf(id);
      DeclareIncompleteClass(
          ScopeClassIdentity(id), key.unit_name, key.class_path);
    }
  }

  // A class or a covergroup the unit declares in a block this body's
  // construction did not build (LRM 27.5) has no code here, and is still the
  // unit's, which every body states alike.
  std::set<hir::ClassId> compiled_classes;
  std::set<hir::CovergroupId> compiled_covergroups;
  const auto compiled_by = [&](const hir::StructuralScope& scope) {
    for (const hir::ReplicatedClass& replicated : scope.replicated_classes) {
      compiled_classes.insert(replicated.id);
    }
    for (const hir::ReplicatedCovergroup& replicated :
         scope.replicated_covergroups) {
      compiled_covergroups.insert(replicated.id);
    }
  };
  compiled_by(hir_->root_scope);
  for (const hir::BlockBodyId id : hir_->block_bodies.Ids()) {
    compiled_by(hir_->block_bodies.Get(id).scope);
  }
  for (const hir::ClassId own : hir_->classes.OwnIds()) {
    if (!compiled_classes.contains(own)) {
      unit_.DefineClass(
          ClassIdentity(own), StateClassFromDeclaration(*this, own));
    }
  }
  for (const hir::CovergroupId own : hir_->covergroups.OwnIds()) {
    if (!compiled_covergroups.contains(own)) {
      unit_.DefineClass(
          CovergroupClassIdentity(own),
          StateCovergroupClassFromDeclaration(*this, own));
    }
  }

  // The prototype of every DPI-C import this unit takes part in (LRM 35.4),
  // published as a bodyless callable of the unit: the DPI-C name space contains
  // no class, so no class owns one. Every call to the import resolves against
  // this one declaration, whichever unit wrote the source declaration.
  for (const hir::ForeignImportDecl& import : hir_->foreign_imports) {
    unit_.callables.Add(MakeForeignImportDecl(unit_, import));
  }

  PublishAssignmentPatternTexts();
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
              .virtual_dispatch = std::nullopt,
              .defining_unit = std::nullopt});
    }
  }
}

void UnitLowerer::PublishTypeOperations() {
  // Run once every body of the unit is lowered, since a body may name an array
  // no declaration does -- the keys a sort gathers -- and a call names an
  // operation by its type rather than by what is stated here. A body built
  // here interns types, and a type interned that way states its operations
  // too, so the pool is walked by position and its size read again at every
  // step.
  for (std::uint32_t i = 0; i < unit_.types.size(); ++i) {
    const mir::TypeId type{.value = i};
    if (!mir::OperationsAreStatedPerUnit(unit_.types.Get(type))) {
      continue;
    }
    for (mir::TypeOperation& stated : ArrayOperationsOf(*this, type)) {
      unit_.type_operations.push_back(std::move(stated));
    }
  }
  // An element type whose operations the unit states has its default among
  // them; every other is given one here.
  const support::ValueOperation default_value =
      support::BuiltinFn::kDeclaredDefault;
  for (const mir::TypeId element : mir::ArrayElementTypes(unit_.types)) {
    if (mir::FindTypeOperation(unit_, element, default_value) != nullptr) {
      continue;
    }
    mir::CallableCode code = mir::CallableCode::Defined();
    code.result_type = element;
    mir::Block& block = code.Body();
    block.AppendStmt(
        mir::ReturnStmt{
            .value =
                block.exprs.Add(BuildDefaultValueExpr(unit_, block, element))});
    unit_.type_operations.push_back(
        mir::TypeOperation{
            .type = element,
            .answers = default_value,
            .code = std::move(code)});
  }
}

auto UnitLowerer::ScopeExecution() const -> support::ScopeExecution {
  switch (hir_->role) {
    case hir::UnitRole::kModuleOrInterface:
      return support::ScopeExecution::kDesign;
    case hir::UnitRole::kProgram:
      return support::ScopeExecution::kProgram;
    case hir::UnitRole::kNamespace:
      throw InternalError(
          "UnitLowerer::ScopeExecution: a namespace has no instance to start "
          "anything in");
  }
  throw InternalError("UnitLowerer::ScopeExecution: a unit of no role");
}

auto UnitLowerer::RunObjectRoot() -> diag::Result<mir::CompilationUnit> {
  if (auto root = PopulateModuleRoot(std::nullopt); !root) {
    return std::unexpected(std::move(root.error()));
  }
  return std::move(unit_);
}

auto UnitLowerer::RunDesignRoot(DesignRootInputs design_root)
    -> diag::Result<mir::CompilationUnit> {
  if (auto root = PopulateModuleRoot(std::move(design_root)); !root) {
    return std::unexpected(std::move(root.error()));
  }
  return std::move(unit_);
}

auto UnitLowerer::PopulateModuleRoot(
    std::optional<DesignRootInputs> design_root) -> diag::Result<void> {
  // The scope lowerers are built first, each naming the class it is published
  // as. What the design root brings up and states ride on the root scope's
  // lowering, and a source module has neither.
  const StructuralScopeLowerer::Tree tree = StructuralScopeLowerer::ForRoot(
      *this, hir_->root_scope, std::move(design_root));
  StructuralScopeLowerer& root = *tree.root;
  if (auto prologue = PublishUnitDeclarations(); !prologue) {
    return std::unexpected(std::move(prologue.error()));
  }

  // Two-sweep structural lowering: the first sweep settles every class's
  // declaration; the second lowers every body and commits the composed class
  // to the unit.
  auto top_r = root.DeclareShape();
  if (!top_r) return std::unexpected(std::move(top_r.error()));
  for (const std::unique_ptr<StructuralScopeLowerer>& block : tree.blocks) {
    if (auto r = block->DeclareShape(); !r) {
      return std::unexpected(std::move(r.error()));
    }
  }
  if (auto r = root.PopulateBodies(); !r) {
    return std::unexpected(std::move(r.error()));
  }
  for (const std::unique_ptr<StructuralScopeLowerer>& block : tree.blocks) {
    if (auto r = block->PopulateBodies(); !r) {
      return std::unexpected(std::move(r.error()));
    }
  }
  StructuralScopeLowerer::DefinePublishedClasses(*this);

  std::map<mir::CallableId, mir::BlockBodyOrdinal> block_entries;
  for (const hir::BlockBodyId id : hir_->block_bodies.Ids()) {
    block_entries.emplace(
        tree.blocks[id.value]->PublishBlockEntry(), BlockBodyOrdinalOf(id));
  }
  unit_.content = mir::RootedTree{
      .root = *top_r,
      .object_entry = PublishObjectEntry(
          unit_, *top_r, root.PublishedClassId(), root.ConstructionValues(),
          root.ConstructionTable()),
      .block_entries = std::move(block_entries)};
  PublishTypeOperations();
  StateObjectDefinitions(unit_);
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
  // structural-scope body machinery. A body's own static-lifetime variables
  // take the same unit-level storage its variables do, being one program-global
  // cell each. The frame has no owner class, so the produced body carries no
  // `self`. Every body the namespace owns starts from one frame, which says
  // what a name reported in it begins with.
  const hir::StructuralScope& scope = hir_->root_scope;
  const WalkFrame namespace_frame = WalkFrame{}.WithScopeName(hir_->fixed_name);

  // The covergroups the namespace declares, each the class it is written as
  // (LRM 19.3). Nothing replicates one, so one sampled where a block of the
  // namespace begins or ends keeps the event that block triggers for its type
  // itself, and its shape is settled before those blocks are given what they
  // trigger.
  std::deque<CovergroupLowerer> covergroup_lowerers;
  BlockEventTriggers block_events;
  for (const hir::ReplicatedCovergroup& covergroup :
       scope.replicated_covergroups) {
    CovergroupLowerer& lowerer =
        covergroup_lowerers.emplace_back(*this, covergroup, nullptr);
    lowerer.DeclareShape(nullptr, std::nullopt);
    lowerer.AddTypeBlockEvents(block_events);
  }
  const DeclaredScopes package_scopes = ScopesOwningDisableTargets(
      scope.procedural_scopes, hir_->fixed_name,
      UnitStorage{.variables = &unit_.static_variables},
      unit_.types.Intern(
          mir::Type{mir::RuntimeLibraryType{
              .kind = mir::RuntimeLibraryKind::kCancellationTarget}}),
      block_events);

  base::Translation<hir::StructuralSubroutineId, StaticVarBindings>
      subroutine_statics{scope.structural_subroutines.size()};
  for (const hir::SubroutineDecl& src : scope.structural_subroutines) {
    subroutine_statics.Append(BindBodyStatics(
        *this, scope.procedural_scopes,
        UnitStorage{.variables = &unit_.static_variables}, src.body));
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
  CallableBindings install_bindings(unit_, install_code);
  const WalkFrame install_frame =
      namespace_frame.WithBlock(&install_code.Body())
          .WithBindings(&install_bindings);

  mir::CallableCode value_code = mir::CallableCode::Defined();
  value_code.result_type = unit_.builtins.void_type;
  CallableBindings value_bindings(unit_, value_code);
  const WalkFrame value_frame = namespace_frame.WithBlock(&value_code.Body())
                                    .WithBindings(&value_bindings);

  if (auto own = PopulateNamespaceOwnStorage(
          *this, scope, subroutine_statics, package_scopes, install_frame,
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
  for (const hir::ReplicatedClass& replicated : scope.replicated_classes) {
    class_lowerers.emplace_back(
        *this, replicated, ClassIdentity(replicated.id),
        ClassPointee(replicated.id), scope, nullptr);
  }
  for (ClassDeclLowerer& class_lowerer : class_lowerers) {
    if (auto r = class_lowerer.DeclareShape(nullptr, {}); !r) {
      return std::unexpected(std::move(r.error()));
    }
  }
  for (ClassDeclLowerer& class_lowerer : class_lowerers) {
    if (auto r = class_lowerer.PopulateBodies(
            namespace_frame,
            StorageBringUp{.install = install_frame, .value = value_frame});
        !r) {
      return std::unexpected(std::move(r.error()));
    }
  }
  for (CovergroupLowerer& covergroup_lowerer : covergroup_lowerers) {
    if (auto r = covergroup_lowerer.PopulateBodies(
            namespace_frame,
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
        namespace_frame, package_scopes, subroutine_statics.Get(id));
    auto code_or = subroutine_lowerer.Run(src, std::nullopt, std::nullopt);
    if (!code_or) return std::unexpected(std::move(code_or.error()));
    unit_.callables.Define(
        subroutine_callables.Get(id), mir::CallableDecl{
                                          .code = *std::move(code_or),
                                          .foreign = std::nullopt,
                                          .virtual_dispatch = std::nullopt,
                                          .defining_unit = std::nullopt});
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
        *this, namespace_frame, mir::UnitCallableTarget{.slot = callable_id},
        result_type, export_decl);
    // A name a namespace owns needs no entry beside its callable: that callable
    // is the program-global symbol and carries the prototype it publishes.
    unit_.callables.Add(
        mir::CallableDecl{
            .code = std::move(entry.code),
            .foreign = std::move(entry.linkage),
            .virtual_dispatch = std::nullopt,
            .defining_unit = std::nullopt});
  }

  PublishNamespaceStorageBringUp(
      *this, unit_, std::move(install_code), std::move(value_code));
  PublishTypeOperations();
  StateObjectDefinitions(unit_);

  return std::move(unit_);
}

auto UnitLowerer::ClassPointee(hir::ClassId cls) const -> mir::TypeId {
  return unit_.types.Intern(
      mir::Type{mir::ObjectType{.of = ClassIdentity(cls)}});
}

auto UnitLowerer::CovergroupObjectType(hir::CovergroupId covergroup) const
    -> mir::TypeId {
  return unit_.types.Intern(
      mir::Type{mir::ObjectType{.of = CovergroupClassIdentity(covergroup)}});
}

auto UnitLowerer::MethodRunBy(const hir::InstanceMethodCallee& callee) const
    -> std::optional<hir::ClassMethodTarget> {
  const auto* builtin = std::get_if<hir::BuiltinHookCallee>(&callee);
  if (builtin == nullptr) {
    return std::get<hir::ClassMethodTarget>(callee);
  }
  if (hir_->classes.IsInterfaceClass(builtin->cls)) {
    return std::nullopt;
  }
  for (std::optional<hir::ClassId> at = builtin->cls; at.has_value();
       at = hir_->classes.BaseOf(*at)) {
    if (const std::optional<hir::MethodId> declared = hir::MethodReplacing(
            hir_->classes.Declaration(*at).methods, builtin->hook)) {
      return hir::ClassMethodTarget{.owner = *at, .method = *declared};
    }
  }
  return std::nullopt;
}

auto UnitLowerer::SelfPointerTo(mir::ClassId cls) const -> mir::TypeId {
  return unit_.types.Intern(
      mir::Type{mir::PointerType{
          .pointee = unit_.types.Intern(mir::Type{mir::ObjectType{.of = cls}}),
          .ownership = mir::PointerOwnership::kBorrowed,
          .mutability = mir::Mutability::kMutable}});
}

auto UnitLowerer::DeclareIncompleteClass(
    mir::ClassId id, std::string unit_name, support::DefPath class_path)
    -> void {
  unit_.DefineClass(
      id, IncompleteClass(id, std::move(unit_name), std::move(class_path)));
}

auto UnitLowerer::IncompleteClass(
    mir::ClassId id, std::string unit_name, support::DefPath class_path) const
    -> mir::Class {
  mir::Class declared{};
  declared.defining_unit = std::move(unit_name);
  declared.identity = std::move(class_path);
  declared.self_pointer_type = SelfPointerTo(id);
  return declared;
}

auto UnitLowerer::DeclarePublishedClass(hir::ClassId cls) -> void {
  const hir::ClassKey& key = hir_->classes.KeyOf(cls);
  const mir::ClassId id = ClassIdentity(cls);
  // A class only named here -- a handle's type, never reached into -- is
  // declared with nothing of what it declares, as an incomplete type is.
  if (!hir_->classes.HasDeclaration(cls)) {
    DeclareIncompleteClass(id, key.unit_name, key.class_path);
    return;
  }
  mir::Class declared = IncompleteClass(id, key.unit_name, key.class_path);
  const hir::ClassDeclaration& published = hir_->classes.Declaration(cls);
  if (const std::optional<DeclaringInstance> instance =
          StateDeclaredLayout(cls, declared)) {
    RecordDeclaringInstanceField(id, instance->member);
  }
  for (const hir::StaticPropertyId prop : published.static_properties.Ids()) {
    const hir::DeclaredProperty& property =
        published.static_properties.Get(prop);
    declared.named_static_properties.push_back(
        mir::NamedStaticProperty{
            .name = property.name,
            .slot = declared.static_properties.Add(
                mir::StaticPropertyDecl{
                    .type = TranslateType(property.type)})});
  }
  // Every method it published, at the position it published it, with the part
  // it takes in dispatch.
  for (const hir::MethodId method : published.methods.Ids()) {
    const hir::DeclaredMethod& declared_method = published.methods.Get(method);
    const mir::CallableId callable = declared.callables.Add(
        mir::CallableDecl{
            .code = {},
            .foreign = std::nullopt,
            .virtual_dispatch = DispatchRoleOf(
                hir::ClassMethodTarget{.owner = cls, .method = method}),
            .defining_unit = std::nullopt});
    declared.named_callables.push_back(
        mir::NamedCallable{
            .name = declared_method.prototype.name, .body = callable});
  }
  // Every class but an interface class answers with a body of its own each
  // callback `randomize()` makes on every class, the hooks among them where it
  // declares a method taking a hook's place (LRM 18.6.2).
  if (!published.is_interface_class) {
    for (const support::LibraryCallback function : RandomizationCallbacks(
             hir::MethodReplacing(
                 published.methods, hir::RandomizationHook::kPreRandomize)
                 .has_value(),
             hir::MethodReplacing(
                 published.methods, hir::RandomizationHook::kPostRandomize)
                 .has_value())) {
      const mir::CallableId body = declared.callables.Add(
          mir::CallableDecl{
              .code = {},
              .foreign = std::nullopt,
              .virtual_dispatch = std::nullopt,
              .defining_unit = std::nullopt});
      declared.library_callbacks.push_back(
          mir::LibraryCallbackBody{.callback = function, .body = body});
    }
  }
  unit_.DefineClass(id, std::move(declared));
}

auto UnitLowerer::DeclarePublishedScopeClass(
    hir::ScopeClassId hir_id, const hir::ScopeClassSignature& scope) -> void {
  const hir::ScopeClassKey& key = hir_->scope_classes.KeyOf(hir_id);
  // A class this unit published is its own: every body of the unit states it
  // alike from what the unit published, whichever of its scopes this body's
  // instances built.
  const mir::ClassId id = ScopeClassIdentity(hir_id);
  if (hir_->scope_classes.IsOwn(hir_id)) {
    SettlePublishedScope(id, scope);
    return;
  }
  // The class of a scope of another unit -- one of its instances, or a
  // generate block inside one -- is a class of that unit, reached the way any
  // other is. It is a scope of the design hierarchy, so it extends the
  // library's root of that tree, and what the scope published is its first
  // fields. What the unit adds while lowering its bodies is placed after them
  // and named by nothing, so the declaration states none of it; no other unit
  // extends the class, and its own unit makes every object of it, so nothing
  // here needs its size. A published subroutine is a virtual method it
  // introduces and leaves to the classes realizing the scope, in the order the
  // signature gives them.
  mir::Class declared = IncompleteClass(id, key.unit_name, key.class_path);
  declared.base = mir::ClassRef{mir::ObjectTreeRootRef{}};
  std::vector<mir::FieldId> slots;
  std::vector<mir::TypeId> field_types;
  for (PublishedField& field : PublishedFieldsOf(scope)) {
    const mir::FieldId slot =
        declared.fields.Add(mir::FieldDecl{.type = field.type});
    if (field.name.has_value()) {
      declared.named_fields.push_back(
          mir::NamedField{.name = *std::move(field.name), .slot = slot});
    }
    slots.push_back(slot);
    field_types.push_back(field.type);
  }
  for (const hir::SubroutinePrototype& callable : scope.callables) {
    declared.named_callables.push_back(
        mir::NamedCallable{
            .name = callable.name,
            .body = declared.callables.Add(
                mir::CallableDecl{
                    .code = {},
                    .foreign = std::nullopt,
                    .virtual_dispatch =
                        mir::IntroducesVirtualSlot{.is_pure = true},
                    .defining_unit = std::nullopt})});
  }
  unit_.DefineClass(id, std::move(declared));
  scope_layouts_.emplace(
      hir_id, ScopeClassLayout{
                  .cls = id,
                  .field_types = std::move(field_types),
                  .published = PublishedLayoutAt(scope, slots)});
}

auto UnitLowerer::ScopeClassLayoutOf(hir::ScopeClassId hir_id) const
    -> const ScopeClassLayout& {
  // What is known of a class is one question with two answers: the layout this
  // unit gave a class of its own, and the one it gave its declaration of
  // another unit's.
  if (hir_->scope_classes.IsOwn(hir_id)) {
    const auto it = published_scopes_.find(ScopeClassIdentity(hir_id));
    if (it == published_scopes_.end()) {
      throw InternalError(
          "UnitLowerer::ScopeClassLayoutOf: every scope class of this unit is "
          "laid out before any body lowers");
    }
    return it->second.layout;
  }
  const auto it = scope_layouts_.find(hir_id);
  if (it == scope_layouts_.end()) {
    throw InternalError(
        "UnitLowerer::ScopeClassLayoutOf: every scope class of another unit "
        "this unit reaches is declared before any body lowers");
  }
  return it->second;
}

void UnitLowerer::RecordDeclaringInstanceField(
    mir::ClassId cls, mir::FieldId field) {
  declaring_instance_fields_.emplace(cls, field);
}

auto UnitLowerer::DeclaringInstanceFieldOf(mir::ClassId cls) const
    -> mir::FieldId {
  const auto it = declaring_instance_fields_.find(cls);
  if (it == declaring_instance_fields_.end()) {
    throw InternalError(
        "UnitLowerer::DeclaringInstanceFieldOf: only a class a structural "
        "scope declares keeps the instance it belongs to, in the field laid "
        "out for it");
  }
  return it->second;
}

auto UnitLowerer::ClassFieldsOf(const hir::ClassDeclaration& declared) const
    -> ClassFields {
  ClassFields fields{
      .properties = {},
      .declaring_instance = declared.instance_scope.has_value()
                                ? std::optional{unit_.builtins.scope_ptr}
                                : std::nullopt};
  for (const hir::DeclaredProperty& property : declared.properties) {
    fields.properties.push_back(
        PropertyField{
            .name = property.name, .type = TranslateType(property.type)});
  }
  return fields;
}

auto UnitLowerer::StateDeclaredLayout(hir::ClassId cls, mir::Class& stated)
    const -> std::optional<DeclaringInstance> {
  const hir::ClassDeclaration& declared = hir_->classes.Declaration(cls);
  stated.is_interface_class = declared.is_interface_class;
  // A class extending nothing the source wrote extends the root every built
  // object does. An interface class holds no storage and no value is built of
  // one, so nothing is placed after it (LRM 8.26).
  if (!declared.is_interface_class) {
    stated.base = declared.base.has_value()
                      ? mir::ClassRef{ClassIdentity(*declared.base)}
                      : mir::ClassRef{mir::ManagedObjectRootRef{}};
  }
  for (const hir::ClassId iface : declared.implements) {
    stated.implements.push_back(ClassIdentity(iface));
  }
  ClassFields fields = ClassFieldsOf(declared);
  for (PropertyField& property : fields.properties) {
    stated.named_fields.push_back(
        mir::NamedField{
            .name = std::move(property.name),
            .slot = stated.fields.Add(mir::FieldDecl{.type = property.type})});
  }
  return fields.declaring_instance.transform([&](mir::TypeId type) {
    return DeclaringInstance{
        .type = type,
        .member = stated.fields.Add(mir::FieldDecl{.type = type})};
  });
}

auto UnitLowerer::PublishedFieldsOf(const hir::ScopeClassSignature& signature)
    const -> std::vector<PublishedField> {
  std::vector<PublishedField> fields;
  fields.reserve(
      signature.members.size() + signature.loops.size() +
      signature.choices.size() + signature.disable_targets.size());
  for (const hir::PublishedMember& member : signature.members) {
    fields.push_back(
        PublishedField{
            .name = member.name,
            .type =
                MemberCellType(TranslateType(member.type), member.storage)});
  }
  // The member holding the blocks a loop built, each under the value its
  // index stood at, which is what a name reaching one asks for.
  const mir::TypeId loop_blocks =
      unit_.types.Intern(mir::Type{mir::LoopBlocksType{}});
  for (std::size_t i = 0; i < signature.loops.size(); ++i) {
    fields.push_back(PublishedField{.name = std::nullopt, .type = loop_blocks});
  }
  // The member holding the one block a construct building at most one built.
  // Each alternative is published under a class of its own, and which of them
  // an object built is settled as it is constructed, so the handle holds the
  // base every alternative extends, as a C++ header keeps a pointer to a base
  // whose derived classes its implementation defines; a name reaching the
  // block views it as the class it knows the block to be.
  for (std::size_t i = 0; i < signature.choices.size(); ++i) {
    fields.push_back(
        PublishedField{.name = std::nullopt, .type = unit_.builtins.scope_ptr});
  }
  const mir::TypeId cancellation_target = unit_.types.Intern(
      mir::Type{mir::RuntimeLibraryType{
          .kind = mir::RuntimeLibraryKind::kCancellationTarget}});
  for (std::size_t i = 0; i < signature.disable_targets.size(); ++i) {
    fields.push_back(
        PublishedField{.name = std::nullopt, .type = cancellation_target});
  }
  if (IsPublishedBlock(signature)) {
    fields.push_back(
        PublishedField{.name = std::nullopt, .type = unit_.builtins.scope_ptr});
  }
  return fields;
}

auto UnitLowerer::PublishedLayoutAt(
    const hir::ScopeClassSignature& signature,
    std::span<const mir::FieldId> slots) -> PublishedScopeLayout {
  const std::size_t members = signature.members.size();
  const std::size_t loops = signature.loops.size();
  const std::size_t choices = signature.choices.size();
  const std::size_t disable_targets = signature.disable_targets.size();
  const std::size_t enclosing = IsPublishedBlock(signature) ? 1 : 0;
  if (slots.size() != members + loops + choices + disable_targets + enclosing) {
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
      .loops = {loops, part(members, loops)},
      .choices = {choices, part(members + loops, choices)},
      .disable_targets =
          {disable_targets, part(members + loops + choices, disable_targets)},
      .enclosing = enclosing == 0 ? std::nullopt : std::optional{slots.back()}};
}

// A member is at the position its class's declaration gives it, in this unit
// and in every unit reading the declaration alike, so the MIR member is that
// position of the class's MIR identity.
auto UnitLowerer::TranslateClassPropertyTarget(
    const hir::ClassPropertyTarget& target) const -> mir::ClassFieldTarget {
  return mir::ClassFieldTarget{
      .owner = ClassIdentity(target.owner),
      .slot = mir::FieldId{target.field.value}};
}

auto UnitLowerer::TranslateStaticPropertyTarget(
    const hir::ClassStaticPropertyTarget& target) -> StaticStorageHome {
  if (hir_->classes.IsOwn(target.owner)) {
    return GetClassShape(ClassIdentity(target.owner))
        .static_property_translation.Get(target.prop);
  }
  return ClassCellHome{
      .owner = ClassIdentity(target.owner),
      .property = mir::StaticPropertyId{target.prop.value}};
}

auto UnitLowerer::TranslateMethodTarget(
    const hir::ClassMethodTarget& target) const -> mir::CallableTarget {
  return mir::CallableTarget{
      .owner = ClassIdentity(target.owner),
      .slot = mir::CallableId{target.method.value}};
}

auto UnitLowerer::DispatchRoleOf(const hir::ClassMethodTarget& method) const
    -> std::optional<mir::VirtualDispatchRole> {
  const hir::DeclaredMethod& declared =
      hir_->classes.Declaration(method.owner).methods.Get(method.method);
  return std::visit(
      Overloaded{
          [](const hir::TypeAssociated&)
              -> std::optional<mir::VirtualDispatchRole> {
            return std::nullopt;
          },
          [](const hir::NotVirtual&)
              -> std::optional<mir::VirtualDispatchRole> {
            return std::nullopt;
          },
          [](const hir::IntroducesVirtual& introduces)
              -> std::optional<mir::VirtualDispatchRole> {
            return mir::VirtualDispatchRole{
                mir::IntroducesVirtualSlot{.is_pure = introduces.is_pure}};
          },
          [&](const hir::OverridesVirtual& overriding)
              -> std::optional<mir::VirtualDispatchRole> {
            return mir::VirtualDispatchRole{mir::OverridesVirtualSlot{
                .overridden = VirtualSlotOf(
                    hir_->classes.IntroducedBy(overriding.overridden)),
                .is_pure = overriding.is_pure}};
          }},
      declared.dispatch);
}

// A behavior is named by the method introducing it, at its position.
auto UnitLowerer::VirtualSlotOf(const hir::ClassMethodTarget& behavior) const
    -> mir::VirtualSlot {
  return mir::VirtualSlot{
      .owner_class = ClassIdentity(behavior.owner),
      .slot = mir::CallableId{behavior.method.value}};
}

auto UnitLowerer::ConformanceOf(hir::ClassId cls) const
    -> std::vector<mir::ConformingBehavior> {
  const auto slot_of = [&](const hir::NamedBehavior& behavior) {
    return VirtualSlotOf(hir_->classes.IntroducedBy(behavior));
  };
  std::vector<mir::ConformingBehavior> conforming;
  for (const hir::ConformingBehavior& answered :
       hir_->classes.Declaration(cls).conforming) {
    conforming.push_back(
        mir::ConformingBehavior{
            .interface_behavior = slot_of(answered.interface_behavior),
            .answered_by = answered.answered_by.transform(slot_of)});
  }
  return conforming;
}

auto UnitLowerer::MakeNamespaceCallableTarget(
    const hir::NamespaceSubroutineRef& ref) -> mir::DirectTarget {
  const auto key = std::pair{ref.unit_name, ref.subroutine_name};
  if (const auto known = namespace_callables_.find(key);
      known != namespace_callables_.end()) {
    return mir::UnitCallableTarget{.slot = known->second};
  }
  // A subroutine of this unit's own namespace is the body declared under the
  // identifier; one of another unit's is a body declared here for the
  // identifier that unit published.
  mir::CallableId callable{};
  if (hir_->IsItself(ref.unit_name)) {
    const std::optional<mir::CallableId> body =
        mir::CallableNamed(unit_.named_callables, ref.subroutine_name);
    if (!body.has_value()) {
      throw InternalError(
          "MakeNamespaceCallableTarget: this unit's namespace publishes no "
          "subroutine under the identifier a call inside it spells");
    }
    callable = *body;
  } else {
    callable = unit_.callables.Add(
        mir::CallableDecl{
            .code = {},
            .foreign = std::nullopt,
            .virtual_dispatch = std::nullopt,
            .defining_unit = ref.unit_name});
    unit_.named_callables.push_back(
        mir::NamedCallable{.name = ref.subroutine_name, .body = callable});
  }
  namespace_callables_.emplace(key, callable);
  return mir::UnitCallableTarget{.slot = callable};
}

auto UnitLowerer::NamespaceVariable(
    const hir::NamespaceValueRef& ref, mir::TypeId cell_type)
    -> mir::StaticVariableId {
  const auto key = std::pair{ref.unit_name, ref.variable_name};
  if (const auto known = namespace_variables_.find(key);
      known != namespace_variables_.end()) {
    return known->second;
  }
  // The cell a variable of this unit's own namespace holds; for one of another
  // unit's, a cell declared here for the identifier that unit published.
  mir::StaticVariableId variable{};
  if (hir_->IsItself(ref.unit_name)) {
    const std::optional<mir::StaticVariableId> own = mir::StaticVariableNamed(
        unit_.named_static_variables, ref.variable_name);
    if (!own.has_value()) {
      throw InternalError(
          "UnitLowerer::NamespaceVariable: this unit's namespace publishes no "
          "variable under the identifier a reference inside it spells");
    }
    variable = *own;
  } else {
    variable = unit_.static_variables.Add(
        mir::StaticVariableDecl{
            .type = cell_type, .defining_unit = ref.unit_name});
    unit_.named_static_variables.push_back(
        mir::NamedStaticVariable{
            .name = ref.variable_name, .variable = variable});
  }
  namespace_variables_.emplace(key, variable);
  return variable;
}

auto UnitLowerer::PublishedSubroutineSlot(
    hir::ScopeClassId scope_class, hir::PublishedCallableId callable) const
    -> mir::VirtualSlot {
  return mir::VirtualSlot{
      .owner_class = ScopeClassIdentity(scope_class),
      .slot = PublishedMethodOf(callable)};
}

}  // namespace lyra::lowering::hir_to_mir
