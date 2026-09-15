#include "lyra/compiler/design_root.hpp"

#include <expected>
#include <optional>
#include <span>
#include <string>
#include <utility>

#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/external_unit_object.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/unit_signature.hpp"
#include "lyra/hir/unit_signatures.hpp"
#include "lyra/lowering/hir_to_mir/design_namespaces.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::compiler {

namespace {

// The design-root unit is a module whose only members are the top-level units,
// instantiated as its owned children. Its constructor then elaborates the
// design through the same owned-child construction any parent uses for a
// submodule, so no code path is special-cased for the top level. Its HIR
// carries only this source-faithful structure; reaching the packages' bring-up
// entries is settled at lowering, not HIR content.
auto BuildDesignRootHir(
    std::span<const lowering::ast_to_hir::TopLevelUnit> tops,
    const hir::UnitSignatures& signatures) -> hir::CompilationUnit {
  hir::CompilationUnit root{std::string{kDesignRootUnitName}};
  for (const lowering::ast_to_hir::TopLevelUnit& top : tops) {
    // The root reaches a top the way any parent reaches a child it builds:
    // through its own record of the object that unit's signature promised.
    const hir::ExternalUnitObjectId object = root.external_unit_objects.Add(
        hir::ImportExternalUnitObject(
            signatures.Instantiated(top.unit_name), root.types));
    root.root_scope.instance_members.Define(
        root.root_scope.instance_members.Declare(),
        hir::InstanceMemberDecl{
            .instance_name = top.instance_name,
            .object = object,
            .array_dims = {}});
  }
  return root;
}

}  // namespace

auto SynthesizeDesignRoot(
    std::span<const lowering::ast_to_hir::TopLevelUnit> tops,
    const hir::UnitSignatures& signatures,
    const diag::SourceManager& source_manager)
    -> diag::Result<mir::CompilationUnit> {
  const hir::CompilationUnit root_hir = BuildDesignRootHir(tops, signatures);
  lowering::hir_to_mir::UnitLowerer root_lowerer(root_hir, source_manager);
  auto root_mir = root_lowerer.RunDesignRoot(
      lowering::hir_to_mir::DesignNamespaces{
          .units = signatures.NamespaceUnitNames()});
  if (!root_mir) {
    return std::unexpected(std::move(root_mir.error()));
  }
  return *std::move(root_mir);
}

}  // namespace lyra::compiler
