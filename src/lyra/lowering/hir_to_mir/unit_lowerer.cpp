#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <format>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/lowering/hir_to_mir/callable_storage_plan.hpp"
#include "lyra/lowering/hir_to_mir/class_decl_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

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
  WalkFrame root_frame;
  if (auto prologue = PopulateTypesAndClasses(); !prologue) {
    return std::unexpected(std::move(prologue.error()));
  }

  // Two-sweep structural lowering: the first sweep mints every class identity
  // and publishes its shape; the second lowers every body and commits the
  // composed class to the unit.
  StructuralScopeLowerer root(*this, nullptr, hir_->name, hir_->root_scope);
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

  // A package's root scope holds no processes, no structural data, and no
  // instances -- only its functions and tasks (LRM 26.2). Each lowers to a
  // receiver-less callable owned by the unit's namespace, so a package produces
  // no root class and never enters the structural-scope body machinery.
  // A package function reaches no static storage and no enclosing scope, so it
  // lowers against an empty storage plan and no enclosing-scope lowerer. The
  // frame has no owner class, so the produced body carries no `self`.
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

  unit_.root = std::nullopt;
  return std::move(unit_);
}

auto UnitLowerer::NextGenerateScopeName(std::string_view arm_tag)
    -> std::string {
  return std::format("gen{}_{}", next_generate_scope_name_++, arm_tag);
}

}  // namespace lyra::lowering::hir_to_mir
