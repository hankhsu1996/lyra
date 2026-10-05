#include "lyra/compiler/design_root.hpp"

#include <array>
#include <expected>
#include <span>
#include <string>
#include <utility>
#include <vector>

#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/external_scope_class.hpp"
#include "lyra/hir/published_member.hpp"
#include "lyra/hir/published_scope.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/type.hpp"
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
  // Like any module, its object is published under the unit's class name, and
  // what it publishes is every instance it builds (LRM 23.6).
  hir::ScopeClassSignature published{
      .class_name = hir::InstanceClassName(kDesignRootUnitName),
      .members = {},
      .callables = {},
      .generates = {},
      .disable_targets = {},
      .modports = {}};
  std::vector<hir::PublishedDecl> instances;
  for (const lowering::ast_to_hir::TopLevelUnit& top : tops) {
    // The root reaches a top the way any parent reaches a child it builds:
    // through its own record of the class that unit's signature published. A
    // top is handed nothing, since nothing instantiates it.
    const hir::UnitSignature& signature =
        signatures.Instantiated(top.unit_name);
    const hir::ScopeClassSignature& instance_class =
        hir::DesignElementOf(signature).instance_class;
    const hir::ExternalScopeClassId scope_class =
        root.external_scope_classes.Add(
            hir::ImportExternalScopeClass(
                signature, instance_class, root.types));
    const hir::InstanceMemberId instance =
        root.root_scope.instance_members.Declare();
    root.root_scope.instance_members.Define(
        instance, hir::InstanceMemberDecl{
                      .instance_name = top.instance_name,
                      .array_dims = {},
                      .alternatives = {hir::InstanceAlternative{
                          .scope_class = scope_class, .arguments = {}}},
                      .taken = {0}});
    const std::array kind{hir::UnitObjectType{
        .unit_name = top.unit_name, .class_name = instance_class.class_name}};
    published.members.Add(
        hir::PublishedMember{
            .name = top.instance_name,
            .within = {},
            .type = root.types.Intern(hir::Type{hir::ObjectsOf({}, kind)}),
            .storage = hir::BorrowedObjectStorage{}});
    instances.emplace_back(instance);
  }
  root.root_scope.published = hir::ScopePublication{
      .signature = std::move(published),
      .aliases = {},
      .members = {instances.size(), std::move(instances)},
      .generates = {},
      .disable_targets = {},
      .callables = {}};
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
