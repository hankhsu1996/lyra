#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <expected>
#include <format>
#include <string>
#include <string_view>
#include <unordered_set>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/structural_data_object.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/callable_storage_plan.hpp"
#include "lyra/lowering/hir_to_mir/class_decl_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/package_initialization.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// Lowers a package's variables (LRM 26.2) into unit-level static storage and
// synthesizes the two receiver-less callables that bring them up at time zero:
// `Install` installs every cell's declared representation and default (no
// runtime handle -- nothing to fire before any process), and `Initialize`
// runs each LRM 10.5 value initializer through its cell (taking a runtime
// handle as its one parameter, since a package has no `self` to reach it
// through). The design root installs every package before initializing any,
// so a value initializer always reaches installed storage. A package variable
// is reached by name (`unit::name`), so a variable initializer's references
// to sibling or other-package variables lower through the same by-name path
// with no enclosing scope or receiver; the other-package reads are recorded
// as the unit's initializer dependency.
auto PopulatePackageStaticVariables(
    UnitLowerer& unit_lowerer, const hir::StructuralScope& scope)
    -> diag::Result<void> {
  mir::CompilationUnit& unit = unit_lowerer.Unit();

  mir::CallableCode install_code;
  install_code.result_type = unit.builtins.void_type;
  mir::Block& install_block = install_code.body;
  const WalkFrame install_frame = WalkFrame{}.WithBlock(&install_block);

  mir::CallableCode value_code;
  value_code.result_type = unit.builtins.void_type;
  CallableBindings value_bindings(unit, value_code);
  const mir::LocalId runtime_local = value_bindings.Declare(
      BindingOriginId::Runtime(),
      mir::LocalDecl{.name = "runtime", .type = unit.builtins.effects});
  value_code.params.push_back(runtime_local);
  mir::Block& value_block = value_code.body;
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

  for (std::size_t i = 0; i < scope.structural_data_objects.size(); ++i) {
    const hir::StructuralDataObjectId hir_id{static_cast<std::uint32_t>(i)};
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
    if (!mir::IsObservableCellType(unit.types.Get(cell_type))) {
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
                mir::MakeObservableInitializeCallExpr(
                    make_cell(install_block, d.name, cell_type), prototype,
                    unit.builtins.void_type))});

    // Phase 2: a user initializer (LRM 10.5) writes the value through the
    // cell; the runtime handle comes from the callable's parameter, since
    // there is no `self`.
    if (var->initializer.has_value()) {
      auto value_or = expr_lowerer.LowerExpr(
          scope.exprs.Get(*var->initializer), value_frame);
      if (!value_or) return std::unexpected(std::move(value_or.error()));
      const mir::ExprId value_id = value_block.exprs.Add(*std::move(value_or));
      const mir::ExprId runtime_ref = value_block.exprs.Add(
          mir::MakeLocalRefExpr(runtime_local, unit.builtins.effects));
      value_block.AppendStmt(
          mir::ExprStmt{
              .expr = value_block.exprs.Add(
                  mir::MakeObservableSetCallExpr(
                      make_cell(value_block, d.name, cell_type), runtime_ref,
                      value_id, unit.builtins.void_type))});
    }
  }

  // A package with variables always installs them.
  if (!install_block.root_stmts.empty()) {
    unit.callables.Add(
        mir::CallableDecl{
            .name = std::string{kPackageInstallCallableName},
            .impl = mir::InternalCallable{.code = std::move(install_code)},
            .virtual_dispatch = std::nullopt,
            .visibility = mir::CallableVisibility::kInternal});
  }
  // Only a package with a value initializer needs the initialize callable. Its
  // direct other-package variable reads are the by-name dependency the design
  // root orders on: scan the initializer body for cross-package cell
  // references.
  if (!value_block.root_stmts.empty()) {
    std::unordered_set<std::string> reads;
    for (std::size_t i = 0; i < value_block.exprs.size(); ++i) {
      const auto& data =
          value_block.exprs.Get(mir::ExprId{static_cast<std::uint32_t>(i)})
              .data;
      if (const auto* ref = std::get_if<mir::ExternalUnitVariableRef>(&data);
          ref != nullptr && ref->unit_name != unit.name) {
        reads.insert(ref->unit_name);
      }
    }
    unit.direct_initializer_package_reads.assign(reads.begin(), reads.end());
    std::ranges::sort(unit.direct_initializer_package_reads);
    unit.callables.Add(
        mir::CallableDecl{
            .name = std::string{kPackageInitializeCallableName},
            .impl = mir::InternalCallable{.code = std::move(value_code)},
            .virtual_dispatch = std::nullopt,
            .visibility = mir::CallableVisibility::kInternal});
  }
  return {};
}

}  // namespace

auto UnitLowerer::PopulateTypesAndClasses() -> diag::Result<void> {
  unit_.name = hir_->name;

  // Every class identity is minted before any type is translated, so a class
  // handle type resolves to the managed-reference pointee that names it while
  // the class bodies are still being built.
  for (std::size_t i = 0; i < hir_->classes.size(); ++i) {
    const hir::ClassId hir_id{static_cast<std::uint32_t>(i)};
    const mir::ClassId mir_id = unit_.DeclareClass();
    const mir::TypeId object_type =
        unit_.types.Intern(mir::ObjectType{.class_id = mir_id});
    MapClass(hir_id, mir_id, object_type);
  }

  // Every HIR type is MIR-representable: AST-to-HIR rejects the forms MIR has
  // no shape for, so this projection never fails.
  for (std::size_t i = 0; i < hir_->types.size(); ++i) {
    const hir::TypeId hir_id{static_cast<std::uint32_t>(i)};
    const hir::Type& hir_type = hir_->types.Get(hir_id);
    const mir::TypeId mir_id =
        unit_.types.Intern(TranslateTypeData(hir_type.data));
    MapType(hir_id, mir_id);
  }

  // Two-stage class lowering. Stage 1 publishes every class's shape so a
  // peer body reads any cross-class fact from the shape store, not from a
  // sibling lowerer's in-progress state. Stage 2 composes each class's
  // executable form and commits it to the unit. Neither stage observes the
  // other's iteration order.
  std::vector<ClassDeclLowerer> class_lowerers;
  class_lowerers.reserve(hir_->classes.size());
  for (std::size_t i = 0; i < hir_->classes.size(); ++i) {
    const hir::ClassId hir_id{static_cast<std::uint32_t>(i)};
    class_lowerers.emplace_back(
        *this, TranslateClass(hir_id), ClassObjectType(hir_id),
        hir_->classes.Get(hir_id));
  }
  for (auto& class_lowerer : class_lowerers) {
    auto r = class_lowerer.DeclareShape();
    if (!r) return std::unexpected(std::move(r.error()));
  }
  for (auto& class_lowerer : class_lowerers) {
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
  if (auto prologue = PopulateTypesAndClasses(); !prologue) {
    return std::unexpected(std::move(prologue.error()));
  }

  // Two-sweep structural lowering: the first sweep mints every class identity
  // and publishes its shape; the second lowers every body and commits the
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
  if (auto prologue = PopulateTypesAndClasses(); !prologue) {
    return std::unexpected(std::move(prologue.error()));
  }

  // A package's root scope holds no processes and no instances -- only its
  // variables, functions, and tasks (LRM 26.2). Each function and task lowers
  // to a receiver-less callable, and each variable to unit-level static
  // storage, so a package produces no root class and never enters the
  // structural-scope body machinery. A package function reaches no static
  // storage and no enclosing scope, so it lowers against an empty storage plan
  // and no enclosing-scope lowerer. The frame has no owner class, so the
  // produced body carries no `self`.
  const CallableStoragePlan empty_storage_plan;
  const hir::StructuralScope& scope = hir_->root_scope;

  // An enum the package declares carries no name of its own, so the typedef
  // naming it records that name at the unit for the backend to resolve against;
  // a typedef of an already-named type records nothing (see the same rule in
  // the structural path).
  for (const hir::TypeAliasDecl& alias : scope.type_aliases) {
    const mir::TypeId target = TranslateType(alias.target);
    if (std::holds_alternative<mir::EnumType>(unit_.types.Get(target).data)) {
      unit_.nominal_type_names.try_emplace(target, alias.name);
    }
  }
  // A DPI-C import declared in a package (LRM 35.4) has no lowering here yet;
  // it would otherwise drop silently, since only the subroutines below are
  // walked.
  if (!scope.foreign_imports.empty()) {
    return diag::Fail(
        diag::DiagCode::kUnsupportedExpressionForm,
        "a DPI-C import declared in a package is not yet supported");
  }
  for (std::size_t i = 0; i < scope.structural_subroutines.size(); ++i) {
    const hir::SubroutineDecl& src = scope.structural_subroutines.Get(
        hir::StructuralSubroutineId{static_cast<std::uint32_t>(i)});
    ProcessLowerer subroutine_lowerer(
        *this, nullptr, scope.time_resolution, src.body, src.name, WalkFrame{},
        empty_storage_plan);
    auto code_or = subroutine_lowerer.Run(src);
    if (!code_or) return std::unexpected(std::move(code_or.error()));
    unit_.callables.Add(
        mir::CallableDecl{
            .name = src.name,
            .impl = mir::InternalCallable{.code = *std::move(code_or)},
            .virtual_dispatch = std::nullopt,
            .visibility = mir::CallableVisibility::kInternal});
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
    return mir::FieldRef{mir::FieldTarget{
        .owner = TranslateClass(local->owner),
        .slot = TranslateField(local->field)}};
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
