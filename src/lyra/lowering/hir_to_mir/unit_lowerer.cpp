#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"

#include <algorithm>
#include <cstddef>
#include <expected>
#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_set>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/id_allocator.hpp"
#include "lyra/base/translation.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/class_decl.hpp"
#include "lyra/hir/class_ref.hpp"
#include "lyra/hir/structural_data_object.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/class_decl_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/declared_scope.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/expression/dpi_call.hpp"
#include "lyra/lowering/hir_to_mir/lhs_store.hpp"
#include "lyra/lowering/hir_to_mir/package_initialization.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The other units whose variables `body` reads. A body is a tree of blocks --
// a predicate that declares identifiers (LRM 12.6.3) puts the arms it guards in
// one of its own -- so the answer is the union over the whole tree.
auto UnitsReadBy(const mir::Block& body, std::string_view own_unit)
    -> std::unordered_set<std::string> {
  std::unordered_set<std::string> units;
  for (const mir::ExprId id : body.exprs.Ids()) {
    if (const auto* ref =
            std::get_if<mir::ExternalUnitVariableRef>(&body.exprs.Get(id).data);
        ref != nullptr && ref->unit_name != own_unit) {
      units.insert(ref->unit_name);
    }
  }
  for (const mir::BlockId id : body.child_scopes.Ids()) {
    units.merge(UnitsReadBy(body.child_scopes.Get(id), own_unit));
  }
  return units;
}

// Lowers a package's variables (LRM 26.2) into unit-level static storage and
// synthesizes the two receiver-less callables that bring them up at time zero:
// `Install` installs every cell's declared representation and default, and
// `Initialize` runs each LRM 10.5 value initializer through its cell. Neither
// takes a parameter: a package has no `self`, and neither operation needs one.
// The design root installs every package before initializing any,
// so a value initializer always reaches installed storage. A package variable
// is reached by name (`unit::name`), so a variable initializer's references
// to sibling or other-package variables lower through the same by-name path
// with no enclosing scope or receiver; the other-package reads are recorded
// as the unit's initializer dependency.
auto PopulatePackageStaticVariables(
    UnitLowerer& unit_lowerer, const hir::StructuralScope& scope)
    -> diag::Result<void> {
  mir::CompilationUnit& unit = unit_lowerer.Unit();

  mir::CallableCode install_code = mir::CallableCode::Defined();
  install_code.result_type = unit.builtins.void_type;
  mir::Block& install_block = install_code.Body();
  const WalkFrame install_frame = WalkFrame{}.WithBlock(&install_block);

  mir::CallableCode value_code = mir::CallableCode::Defined();
  value_code.result_type = unit.builtins.void_type;
  CallableBindings value_bindings(unit, value_code);
  mir::Block& value_block = value_code.Body();
  const WalkFrame value_frame =
      WalkFrame{}.WithBlock(&value_block).WithBindings(&value_bindings);

  // The package root scope is an ExprLowerer over its own expressions: a
  // variable initializer's operands are literals, operators, and by-name
  // package symbols, none of which reach a class or a `self`.
  const StructuralScopeLowerer expr_lowerer(
      unit_lowerer, nullptr, unit.name, scope);

  const auto make_cell = [&](mir::Block& block, const std::string& name,
                             mir::TypeId cell_type) -> mir::ExprId {
    return block.exprs.Add(
        mir::Expr{
            .data =
                mir::ExternalUnitVariableRef{
                    .unit_name = unit.name, .variable_name = name},
            .type = cell_type});
  };

  for (const hir::StructuralDataObjectId hir_id :
       scope.structural_data_objects.Ids()) {
    const hir::StructuralDataObjectDecl& d =
        scope.structural_data_objects.Get(hir_id);
    const auto* var = std::get_if<hir::StructuralVariableDecl>(&d.kind);
    if (var == nullptr) {
      return diag::Fail(
          diag::DiagCode::kUnsupportedExpressionForm,
          "a net declared in a package is not supported");
    }
    const mir::TypeId value_type = unit_lowerer.TranslateType(d.type);
    const mir::TypeId cell_type = unit.types.ObservableCellOf(value_type);
    if (!unit.types.Get(cell_type).IsCapabilityWrapper()) {
      return diag::Fail(
          diag::DiagCode::kUnsupportedExpressionForm,
          "a package variable of this type is not yet supported");
    }
    unit.static_variables.Add(
        mir::StaticVariableDecl{.name = d.name, .type = cell_type});

    // Phase 1: install the cell's declared representation and default.
    const mir::ExprId prototype = install_block.exprs.Add(
        BuildDefaultValueFromHir(unit_lowerer, install_frame, d.type));
    install_block.AppendStmt(
        mir::ExprStmt{
            .expr = install_block.exprs.Add(
                mir::MakeCapabilityInitializeCallExpr(
                    make_cell(install_block, d.name, cell_type), prototype,
                    unit.builtins.void_type))});

    // Phase 2: a user initializer (LRM 10.5) writes the value through the cell.
    if (var->initializer.has_value()) {
      auto value_or = expr_lowerer.LowerExpr(
          scope.exprs.Get(*var->initializer), value_frame);
      if (!value_or) return std::unexpected(std::move(value_or.error()));
      const mir::ExprId value_id = value_block.exprs.Add(*std::move(value_or));
      value_block.AppendStmt(
          mir::ExprStmt{
              .expr = value_block.exprs.Add(BuildStoreExpr(
                  unit, value_block, make_cell(value_block, d.name, cell_type),
                  value_id, std::nullopt, value_type))});
    }
  }

  // Both phases exist for every package. A package that declares nothing gets
  // an empty body for each, because zero declarations is a count and not a
  // different shape -- the design root then calls both without first finding
  // out whether this package supplied them.
  //
  // The initializer's direct other-package variable reads are the by-name
  // dependency the design root orders on.
  const std::unordered_set<std::string> reads =
      UnitsReadBy(value_block, unit.name);
  unit.direct_initializer_package_reads.assign(reads.begin(), reads.end());
  std::ranges::sort(unit.direct_initializer_package_reads);

  unit.callables.Add(
      mir::CallableDecl{
          .name = std::string{kPackageInstallCallableName},
          .code = std::move(install_code),
          .foreign = std::nullopt,
          .virtual_dispatch = std::nullopt});
  unit.callables.Add(
      mir::CallableDecl{
          .name = std::string{kPackageInitializeCallableName},
          .code = std::move(value_code),
          .foreign = std::nullopt,
          .virtual_dispatch = std::nullopt});
  return {};
}

}  // namespace

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
      .object_type = unit_.types.Intern(mir::ObjectType{.class_id = id}),
      .methods = {decl.methods.size(), std::move(methods)}};
}

auto UnitLowerer::PublishUnitDeclarations() -> diag::Result<void> {
  unit_.name = hir_->name;

  // Every identity another declaration can name is taken before any
  // declaration settles: a class handle type resolves to the pointee that names
  // it, and an overriding method states its dispatch role against the slot its
  // base was given. Nothing here reads a declaration, so no declaration has to
  // settle before another.
  std::vector<ClassTranslation> classes;
  classes.reserve(hir_->classes.size());
  for (const hir::ClassId hir_id : hir_->classes.Ids()) {
    classes.push_back(TakeClassIdentities(hir_->classes.Get(hir_id)));
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
    type_translations_.Append(
        unit_.types.Intern(TranslateTypeData(hir_->types.Get(hir_id).data)));
  }

  // The prototype of every DPI-C import this unit takes part in (LRM 35.4),
  // published as a bodyless callable of the unit: the DPI-C name space contains
  // no class, so no class owns one. Every call to the import resolves against
  // this one declaration, whichever unit wrote the source declaration.
  for (const hir::ForeignImportDecl& import : hir_->foreign_imports) {
    unit_.callables.Add(MakeForeignImportDecl(unit_, import));
  }

  // The remaining two stages. The first settles every class's declaration so a
  // peer body reads any cross-class fact from the unit's declarations rather
  // than from a sibling lowerer's in-progress state. The second composes each
  // class's executable form and commits it to the unit. Neither stage observes
  // any order: everything a declaration names of another was taken above.
  std::vector<ClassDeclLowerer> class_lowerers;
  class_lowerers.reserve(hir_->classes.size());
  for (const hir::ClassId hir_id : hir_->classes.Ids()) {
    class_lowerers.emplace_back(
        *this, hir_id, TranslateClass(hir_id), ClassObjectType(hir_id),
        hir_->classes.Get(hir_id));
  }
  for (ClassDeclLowerer& class_lowerer : class_lowerers) {
    auto r = class_lowerer.DeclareShape();
    if (!r) return std::unexpected(std::move(r.error()));
  }
  for (ClassDeclLowerer& class_lowerer : class_lowerers) {
    auto r = class_lowerer.PopulateBodies();
    if (!r) return std::unexpected(std::move(r.error()));
  }
  return {};
}

auto UnitLowerer::RunModule() -> diag::Result<mir::CompilationUnit> {
  return LowerModuleUnit({});
}

auto UnitLowerer::RunDesignRoot(PackageInitializationPlan package_init_plan)
    -> diag::Result<mir::CompilationUnit> {
  return LowerModuleUnit(std::move(package_init_plan));
}

auto UnitLowerer::LowerModuleUnit(PackageInitializationPlan package_init_plan)
    -> diag::Result<mir::CompilationUnit> {
  WalkFrame root_frame;
  if (auto prologue = PublishUnitDeclarations(); !prologue) {
    return std::unexpected(std::move(prologue.error()));
  }

  // Two-sweep structural lowering: the first sweep mints every class identity
  // and settles its declaration; the second lowers every body and commits the
  // composed class to the unit. The design root's package initialization plan
  // rides on the root scope's lowering and is empty for a source module.
  StructuralScopeLowerer root(
      *this, nullptr, hir_->name, hir_->root_scope,
      std::move(package_init_plan));
  auto top_r = root.DeclareShape();
  if (!top_r) return std::unexpected(std::move(top_r.error()));
  auto body_r = root.PopulateBodies(root_frame);
  if (!body_r) return std::unexpected(std::move(body_r.error()));

  unit_.root = *top_r;
  return std::move(unit_);
}

auto UnitLowerer::RunPackage() -> diag::Result<mir::CompilationUnit> {
  if (auto prologue = PublishUnitDeclarations(); !prologue) {
    return std::unexpected(std::move(prologue.error()));
  }

  // A package's root scope holds no processes and no instances -- only its
  // variables, functions, and tasks (LRM 26.2). Each function and task lowers
  // to a receiver-less callable, and each variable to unit-level static
  // storage, so a package produces no root class and never enters the
  // structural-scope body machinery. A package function reaches no static
  // storage and no enclosing scope, so it lowers against no enclosing-scope
  // lowerer and is given nothing. The frame has no owner class, so the produced
  // body carries no `self` -- and with no object to hang one under, none of its
  // scopes owns a name node or a `disable` target either.
  const hir::StructuralScope& scope = hir_->root_scope;
  const DeclaredScopes package_scope_nodes =
      ScopesOwningNothing(scope.procedural_scopes.size());

  // The callable each package subroutine lowered to, recorded where it is
  // created so an export below names its own by identity.
  base::Translation<hir::StructuralSubroutineId, mir::CallableId>
      subroutine_callables{scope.structural_subroutines.size()};
  for (const hir::SubroutineDecl& src : scope.structural_subroutines) {
    ProcessLowerer subroutine_lowerer(
        *this, nullptr, scope.time_resolution, src.body, src.name, WalkFrame{},
        package_scope_nodes, {});
    auto code_or = subroutine_lowerer.Run(src);
    if (!code_or) return std::unexpected(std::move(code_or.error()));
    subroutine_callables.Append(unit_.callables.Add(
        mir::CallableDecl{
            .name = src.name,
            .code = *std::move(code_or),
            .foreign = std::nullopt,
            .virtual_dispatch = std::nullopt}));
  }

  // Each exported package subroutine (LRM 26.3, 35.7) is receiver-less: its
  // C entry point recovers the run's services instead of a calling
  // instance and calls the package's own free function by name.
  for (const hir::ForeignExportDecl& export_decl : scope.foreign_exports) {
    const mir::CallableId callable_id =
        subroutine_callables.Get(export_decl.subroutine);
    const mir::TypeId result_type =
        unit_.callables.Get(callable_id).code.result_type;
    // A package subroutine has no receiver and a package has one form, so its
    // entry is reached by a plain linked name: the unit's own namespace defines
    // the program-global symbol directly.
    ForeignExportEntry entry = SynthesizeForeignExportEntry(
        *this, WalkFrame{},
        mir::ExternalUnitCallableTarget{
            .unit_name = unit_.name,
            .callable_name =
                scope.structural_subroutines.Get(export_decl.subroutine).name},
        result_type, export_decl);
    unit_.foreign_surface.push_back(
        mir::ForeignSymbol{
            .linkage = entry.linkage,
            .signature = entry.signature,
            .definition = mir::ForeignDefinition::kUnitSymbol});
    unit_.callables.Add(
        mir::CallableDecl{
            .name = entry.linkage.foreign_name,
            .code = std::move(entry.code),
            .foreign = std::move(entry.linkage),
            .virtual_dispatch = std::nullopt});
  }

  if (auto vars = PopulatePackageStaticVariables(*this, scope); !vars) {
    return std::unexpected(std::move(vars.error()));
  }

  unit_.root = std::nullopt;
  return std::move(unit_);
}

auto UnitLowerer::NextGenerateScopeName(std::string_view arm_tag)
    -> std::string {
  return std::format("gen{}_{}", next_generate_scope_name_++, arm_tag);
}

auto UnitLowerer::MakeExternalClassPointee(const hir::ExternalClassRef& ref)
    -> mir::TypeId {
  unit_.AddExternalClassUnit(ref.unit_name);
  return unit_.types.Intern(
      mir::ExternalClassType{
          .qualified_name =
              std::format("{}::{}", ref.unit_name, ref.class_name)});
}

auto UnitLowerer::MakeExternalClassRef(const hir::ExternalClassRef& ref)
    -> mir::ClassRef {
  unit_.AddExternalClassUnit(ref.unit_name);
  return mir::ClassRef{mir::ExternalClassRef{
      .qualified_name = std::format("{}::{}", ref.unit_name, ref.class_name)}};
}

auto UnitLowerer::TranslateClassRef(const hir::ClassRef& ref) -> mir::ClassRef {
  if (const auto* local = std::get_if<hir::LocalClassRef>(&ref)) {
    return mir::ClassRef{
        mir::IntraUnitClassRef{.class_id = TranslateClass(local->class_id)}};
  }
  return MakeExternalClassRef(std::get<hir::ExternalClassRef>(ref));
}

auto UnitLowerer::MakeExternalFieldTarget(
    const hir::ExternalClassPropertyTarget& target)
    -> mir::ExternalFieldTarget {
  unit_.AddExternalClassUnit(target.unit_name);
  return mir::ExternalFieldTarget{
      .unit_name = target.unit_name,
      .class_name = target.class_name,
      .field_name = target.property_name};
}

auto UnitLowerer::TranslateClassPropertyTarget(
    const hir::ClassPropertyTarget& target) -> mir::FieldRef {
  if (const auto* local = std::get_if<hir::LocalClassPropertyTarget>(&target)) {
    const mir::ClassId owner = TranslateClass(local->owner);
    return mir::FieldRef{mir::FieldTarget{
        .owner = owner,
        .slot = GetClassShape(owner).field_translation.Get(local->field)}};
  }
  return mir::FieldRef{MakeExternalFieldTarget(
      std::get<hir::ExternalClassPropertyTarget>(target))};
}

auto UnitLowerer::MakeExternalStaticPropertyRef(
    const hir::ExternalStaticPropertyTarget& target)
    -> mir::ExternalStaticPropertyRef {
  unit_.AddExternalClassUnit(target.unit_name);
  return mir::ExternalStaticPropertyRef{
      .unit_name = target.unit_name,
      .class_name = target.class_name,
      .property_name = target.property_name};
}

auto UnitLowerer::MakeExternalMethodTarget(
    const hir::ExternalClassMethodTarget& target)
    -> mir::ExternalUnitClassMethodTarget {
  unit_.AddExternalClassUnit(target.unit_name);
  return mir::ExternalUnitClassMethodTarget{
      .unit_name = target.unit_name,
      .class_name = target.class_name,
      .method_name = target.method_name};
}

auto UnitLowerer::MakeExternalMethodOverride(
    const hir::ExternalClassMethodTarget& target)
    -> mir::OverridesExternalSlot {
  unit_.AddExternalClassUnit(target.unit_name);
  return mir::OverridesExternalSlot{
      .unit_name = target.unit_name,
      .class_name = target.class_name,
      .method_name = target.method_name};
}

auto UnitLowerer::MakeExternalVirtualSlot(
    const hir::ExternalClassMethodTarget& target) -> mir::ExternalVirtualSlot {
  unit_.AddExternalClassUnit(target.unit_name);
  return mir::ExternalVirtualSlot{
      .unit_name = target.unit_name,
      .class_name = target.class_name,
      .method_name = target.method_name};
}

auto UnitLowerer::MakeExternalCallableTarget(
    const hir::ExternalUnitSubroutineRef& ref)
    -> mir::ExternalUnitCallableTarget {
  unit_.AddExternalReferencedUnit(ref.unit_name);
  return mir::ExternalUnitCallableTarget{
      .unit_name = ref.unit_name, .callable_name = ref.subroutine_name};
}

}  // namespace lyra::lowering::hir_to_mir
