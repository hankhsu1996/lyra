#include "lyra/compiler/design_root.hpp"

#include <algorithm>
#include <cstdint>
#include <expected>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#include "lyra/compiler/unit_metadata.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lir/verify.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/package_initialization.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/mir_to_lir/lower.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::compiler {

namespace {

// The design-root unit is a module whose only members are the top-level units,
// instantiated as its owned children. Its constructor then elaborates the
// design through the same owned-child construction any parent uses for a
// submodule, so no code path is special-cased for the top level. Its HIR
// carries only this source-faithful structure; running the packages'
// initializers is a whole-design composition step handled at lowering, not HIR
// content.
auto BuildDesignRootHir(std::span<const std::string> top_names)
    -> hir::CompilationUnit {
  hir::CompilationUnit root{std::string{kDesignRootUnitName}};
  for (const auto& name : top_names) {
    root.root_scope.instance_members.Add(
        hir::InstanceMemberDecl{
            .instance_name = name, .target_unit = name, .array_dims = {}});
  }
  return root;
}

// Whether the unit owns a callable of this name -- used to tell a package that
// has a value initializer (and so a synthesized initialize callable) from one
// whose variables all take their default.
auto HasCallableNamed(const mir::CompilationUnit& unit, std::string_view name)
    -> bool {
  for (std::uint32_t i = 0; i < unit.callables.size(); ++i) {
    if (unit.callables.Get(mir::CallableId{i}).name == name) {
      return true;
    }
  }
  return false;
}

// Post-order DFS emitting `name` after the packages its initializer reads (LRM
// 26.2 / 10.5), so a dependency precedes its dependent. The LRM leaves the
// relative order of initializers unspecified; this is a stable, best-effort
// preference, not a correctness input -- every cell is installed with its
// default before any initializer runs, so a missed or cyclic dependency only
// means a read observes a default, never an uninstalled cell. Visiting `name`
// before descending makes a cyclic dependency terminate with a deterministic
// order rather than recur; `deps` is walked in the caller's stable
// (name-sorted) order, so the whole result is reproducible for a given design.
void OrderPackageInit(
    const std::string& name,
    const std::unordered_map<std::string, std::vector<std::string>>&
        package_deps,
    const std::unordered_set<std::string>& init_set,
    std::unordered_set<std::string>& placed,
    std::vector<std::string>& ordered) {
  if (!placed.insert(name).second) {
    return;
  }
  if (const auto it = package_deps.find(name); it != package_deps.end()) {
    for (const std::string& dep : it->second) {
      if (init_set.contains(dep)) {
        OrderPackageInit(dep, package_deps, init_set, placed, ordered);
      }
    }
  }
  ordered.push_back(name);
}

// Resolves the whole-design package initialization plan from the compiled
// units. A package unit is the one with no root class (`root == nullopt`); the
// install pass covers every package that declares a variable, and the
// value-initialize pass every package that has a value initializer, ordered so
// a package a given initializer reads directly comes first where that is
// possible. Both passes are name-sorted first so the result is deterministic
// for a given design.
auto BuildPackageInitializationPlan(std::span<const mir::CompilationUnit> units)
    -> lowering::hir_to_mir::PackageInitializationPlan {
  std::vector<std::string> packages_with_value_init;
  std::unordered_map<std::string, std::vector<std::string>> package_init_deps;
  lowering::hir_to_mir::PackageInitializationPlan plan;
  for (const auto& unit : units) {
    if (unit.root.has_value()) {
      continue;
    }
    if (!unit.static_variables.empty()) {
      plan.install_order.push_back(unit.name);
    }
    if (HasCallableNamed(
            unit, lowering::hir_to_mir::kPackageInitializeCallableName)) {
      packages_with_value_init.push_back(unit.name);
      package_init_deps.emplace(
          unit.name, unit.direct_initializer_package_reads);
    }
  }
  std::ranges::sort(plan.install_order);
  std::ranges::sort(packages_with_value_init);
  const std::unordered_set<std::string> init_set(
      packages_with_value_init.begin(), packages_with_value_init.end());
  std::unordered_set<std::string> placed;
  for (const std::string& name : packages_with_value_init) {
    OrderPackageInit(
        name, package_init_deps, init_set, placed, plan.value_initialize_order);
  }
  return plan;
}

// Re-interns a foreign boundary type into the design root's own type pool. A
// type identity belongs to the unit that interned it, so a signature read from
// a unit means nothing in the root until every type it names is interned there
// too. The set is the closed one a foreign signature can name (LRM 35.5.6,
// Annex H): machine scalars, a borrowed pointer to one, and the canonical
// vector and open-array handles.
auto ReinternForeignType(
    mir::CompilationUnit& root, const mir::CompilationUnit& unit,
    mir::TypeId id) -> mir::TypeId {
  const mir::TypeData& data = unit.types.Get(id).data;
  if (const auto* pointer = std::get_if<mir::PointerType>(&data)) {
    return root.types.PointerTo(
        ReinternForeignType(root, unit, pointer->pointee), pointer->ownership,
        pointer->mutability);
  }
  if (std::holds_alternative<mir::VoidType>(data) ||
      std::holds_alternative<mir::MachineIntType>(data) ||
      std::holds_alternative<mir::MachineFloatType>(data) ||
      std::holds_alternative<mir::MachineCStringType>(data) ||
      std::holds_alternative<mir::RuntimeLibraryType>(data)) {
    return root.types.Intern(data);
  }
  throw InternalError(
      "ReinternForeignType: this type does not cross a foreign boundary");
}

// Defines, in the design root, the program-global symbol a foreign source calls
// for one exported name whose subroutine is reached through a scope (LRM
// 35.5.3). The subroutine is compiled once per specialization of its declaring
// scope, so the symbol cannot call any one of them: it resolves the entry
// against the scope the foreign call chain established, restores it to the
// prototype its definition was generated with, and calls it.
//
// A name is program-global, so no unit can own the symbol -- two scopes may
// export the same one. The design root is where the whole design is read, so it
// is where the one definition is built.
void DefineExportSymbol(
    mir::CompilationUnit& root, const mir::ForeignLinkage& linkage,
    const mir::MachineFunctionType& signature) {
  mir::CallableCode code = mir::CallableCode::Defined();
  code.body.emplace();
  lowering::hir_to_mir::CallableBindings bindings(root, code);
  mir::Block& body = code.Body();

  std::vector<mir::TypeId> entry_params{root.builtins.scope_ptr};
  for (std::size_t i = 0; i < signature.params.size(); ++i) {
    const mir::LocalId param = bindings.DeclareAnonymous(
        mir::LocalDecl{
            .name = "arg" + std::to_string(i), .type = signature.params[i]});
    code.params.push_back(param);
    entry_params.push_back(signature.params[i]);
  }
  code.result_type = signature.result;

  const mir::LocalId scope = bindings.DeclareAnonymous(
      mir::LocalDecl{.name = "scope", .type = root.builtins.scope_ptr});
  body.AppendStmt(
      mir::LocalDeclStmt{
          .target = scope,
          .init = body.exprs.Add(
              mir::Expr{
                  .data =
                      mir::CallExpr{
                          .callee =
                              mir::Direct{
                                  .target =
                                      support::BuiltinFn::kCurrentExportScope},
                          .arguments = {}},
                  .type = root.builtins.scope_ptr})});

  const mir::ExprId scope_ref =
      body.exprs.Add(mir::MakeLocalRefExpr(scope, root.builtins.scope_ptr));
  const mir::ExprId name = body.exprs.Add(
      mir::Expr{
          .data = mir::StringLiteral{.value = linkage.foreign_name},
          .type = root.types.Intern(mir::MachineCStringType{})});
  const mir::ExprId entry = body.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kFindExportEntry},
                  .arguments = {scope_ref, name}},
          .type = root.types.ErasedFunction()});

  const mir::ExprId restored = body.exprs.Add(
      mir::Expr{
          .data = mir::FunctionCastExpr{.operand = entry},
          .type = root.types.Intern(
              mir::MachineFunctionType{
                  .params = std::move(entry_params),
                  .result = signature.result})});
  std::vector<mir::ExprId> call_args;
  call_args.reserve(code.params.size() + 1);
  call_args.push_back(
      body.exprs.Add(mir::MakeLocalRefExpr(scope, root.builtins.scope_ptr)));
  for (const mir::LocalId param : code.params) {
    call_args.push_back(body.exprs.Add(
        mir::MakeLocalRefExpr(param, code.locals.Get(param).type)));
  }
  const mir::ExprId call = body.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Indirect{.closure = restored},
                  .arguments = std::move(call_args)},
          .type = signature.result});
  // A void entry is called for its effect and returns nothing; any other hands
  // its result straight back.
  if (std::holds_alternative<mir::VoidType>(
          root.types.Get(signature.result).data)) {
    body.AppendStmt(mir::ExprStmt{.expr = call});
    body.AppendStmt(mir::ReturnStmt{.value = std::nullopt});
  } else {
    body.AppendStmt(mir::ReturnStmt{.value = call});
  }

  root.callables.Add(
      mir::CallableDecl{
          .name = linkage.foreign_name,
          .code = std::move(code),
          .foreign = linkage,
          .virtual_dispatch = std::nullopt});
}

// Builds the design's way in: a nullary callable that constructs the root
// object -- parentless, at the hierarchy's origin -- and returns it as the
// generic scope. Construction is what a design does at its own root, so it is
// stated here as ordinary construction rather than left for a host artifact to
// hand-compose; everything else about starting a run is the same for every
// design and is the host's.
void DefineRootFactory(mir::CompilationUnit& root) {
  if (!root.root.has_value()) {
    throw InternalError("DefineRootFactory: the design root has no root class");
  }
  const mir::Class& root_class = root.GetClass(*root.root);
  const mir::TypeId owned_scope = root.types.PointerTo(
      root.types.Intern(
          mir::ExternalClassType{.qualified_name = "lyra::runtime::Scope"}),
      mir::PointerOwnership::kUnique);

  mir::CallableCode code = mir::CallableCode::Defined();
  code.body.emplace();
  code.result_type = owned_scope;
  mir::Block& body = code.Body();

  const mir::ExprId label = body.exprs.Add(
      mir::Expr{
          .data = mir::StringLiteral{.value = root_class.name},
          .type = root.builtins.string});
  const mir::ExprId indices = body.exprs.Add(
      mir::Expr{
          .data = mir::ArrayLiteralExpr{.elements = {}},
          .type = root.types.MachineArrayOf(root.builtins.int_type, 0)});
  const mir::ExprId segment = body.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Construct{}, .arguments = {label, indices}},
          .type = root.builtins.hierarchy_segment});
  const mir::ExprId no_parent = body.exprs.Add(
      mir::Expr{.data = mir::NullLiteral{}, .type = root.builtins.scope_ptr});
  const mir::ExprId built = body.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Construct{},
                  .arguments = {no_parent, segment}},
          .type = root.types.PointerTo(
              root.types.Intern(mir::ObjectType{.class_id = *root.root}),
              mir::PointerOwnership::kUnique)});
  body.AppendStmt(
      mir::ReturnStmt{
          .value = body.exprs.Add(
              mir::Expr{
                  .data = mir::PointerCastExpr{.operand = built},
                  .type = owned_scope})});

  root.root_factory = root.callables.Add(
      mir::CallableDecl{
          .name = "BuildRoot",
          .code = std::move(code),
          .foreign = std::nullopt,
          .virtual_dispatch = std::nullopt});
}

// Every exported name the design must define over per-scope entries, defined
// once. Several scopes may export one C name (LRM 35.4), and each publishes its
// own entry, but the name is one symbol; LRM 35.5.4 requires their prototypes
// to agree, which the frontend has already checked.
void DefineExportSymbols(
    mir::CompilationUnit& root, std::span<const mir::CompilationUnit> units) {
  std::unordered_set<std::string> defined;
  for (const mir::CompilationUnit& unit : units) {
    for (const mir::ForeignSymbol& symbol : unit.foreign_surface) {
      if (symbol.definition != mir::ForeignDefinition::kPerScopeEntry ||
          !defined.insert(symbol.linkage.foreign_name).second) {
        continue;
      }
      const auto& signature = std::get<mir::MachineFunctionType>(
          unit.types.Get(symbol.signature).data);
      std::vector<mir::TypeId> params;
      params.reserve(signature.params.size());
      for (const mir::TypeId param : signature.params) {
        params.push_back(ReinternForeignType(root, unit, param));
      }
      const mir::MachineFunctionType local{
          .params = std::move(params),
          .result = ReinternForeignType(root, unit, signature.result)};
      DefineExportSymbol(root, symbol.linkage, local);
    }
  }
}

}  // namespace

auto LinkDesign(
    std::span<const mir::CompilationUnit> units,
    std::span<const std::string> top_names, StopAfter stop_after,
    const diag::SourceManager& source_manager)
    -> diag::Result<DesignRootArtifacts> {
  const hir::CompilationUnit root_hir = BuildDesignRootHir(top_names);
  lowering::hir_to_mir::UnitLowerer root_lowerer(root_hir, source_manager);
  auto root_mir =
      root_lowerer.RunDesignRoot(BuildPackageInitializationPlan(units));
  if (!root_mir) {
    return std::unexpected(std::move(root_mir.error()));
  }
  DefineRootFactory(*root_mir);
  DefineExportSymbols(*root_mir, units);
  DesignRootArtifacts artifacts{
      .mir = *std::move(root_mir),
      .lir = std::nullopt,
      .metadata = std::nullopt};

  if (stop_after < StopAfter::kLir) {
    return artifacts;
  }
  auto root_lir = lowering::mir_to_lir::LowerUnit(artifacts.mir);
  if (!root_lir) {
    return std::unexpected(std::move(root_lir.error()));
  }
  artifacts.lir = *std::move(root_lir);
  lir::Verify(*artifacts.lir);
  artifacts.metadata = BuildUnitMetadata(artifacts.mir);
  return artifacts;
}

}  // namespace lyra::compiler
