#include "lyra/compiler/design_root.hpp"

#include <array>
#include <expected>
#include <functional>
#include <map>
#include <span>
#include <string>
#include <utility>
#include <vector>

#include "lyra/compiler/unit_pipeline.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/published_member.hpp"
#include "lyra/hir/published_scope.hpp"
#include "lyra/hir/scope_class.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/type_import.hpp"
#include "lyra/hir/unit_body_ref.hpp"
#include "lyra/hir/unit_signature.hpp"
#include "lyra/hir/unit_signatures.hpp"
#include "lyra/lowering/hir_to_mir/design_instantiations.hpp"
#include "lyra/lowering/hir_to_mir/design_namespaces.hpp"
#include "lyra/lowering/hir_to_mir/design_root_inputs.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir_transform/transform.hpp"

namespace lyra::compiler {

namespace {

// Nothing instantiates a top, so every input of one is left unconnected and
// holds its data type's default initial value (LRM 23.3.3.2). An input whose
// member owns no cell is given that the way any instantiating scope gives it:
// by a connection that names no source.
void LeaveInputsUnconnected(
    hir::CompilationUnit& root, const hir::UnitSignature& signature,
    hir::ScopeClassId scope_class, hir::InstanceMemberId instance) {
  const hir::ScopeClassSignature& imported =
      root.scope_classes.Get(scope_class);
  for (const hir::PortDecl& port : hir::DesignElementOf(signature).ports) {
    for (const hir::PortPart& part : port.parts) {
      const auto* data = std::get_if<hir::DataPortPart>(&part);
      if (data == nullptr || data->direction != hir::PortDirection::kInput) {
        continue;
      }
      const auto* projection =
          std::get_if<hir::MemberProjection>(&data->target);
      if (projection == nullptr) continue;
      const hir::PublishedMember& member =
          imported.members.Get(projection->member);
      const auto* reference =
          std::get_if<hir::ReferenceStorage>(&member.storage);
      if (reference == nullptr ||
          reference->binding != hir::ReferenceBinding::kInput) {
        continue;
      }
      const hir::OwnedChildStep step{
          .names = hir::OwnedChildRef{instance}, .selects = {}};
      root.root_scope.port_connections.Add(
          hir::PortConnection{
              .span = {},
              .instance = step,
              .kind = hir::DataPortConnection{
                  .direction = hir::PortDirection::kInput,
                  .endpoint =
                      hir::ValueRoute{
                          .base = hir::InUnitBase{.hops = {}},
                          .steps = {hir::AsPathStep(step)},
                          .leaf =
                              hir::PublishedMemberLeaf{
                                  .scope_class = scope_class,
                                  .member = projection->member,
                                  .storage = member.storage,
                                  .type = member.type}},
                  .peer = std::nullopt,
                  .sensitivity = {}}});
    }
  }
}

// The design-root unit is a module whose only members are the top-level units,
// instantiated as its owned children. Its constructor then elaborates the
// design through the same owned-child construction any parent uses for a
// submodule, so no code path is special-cased for the top level. Its HIR
// carries only this source-faithful structure; reaching the packages' bring-up
// entries is settled at lowering, not HIR content.
auto BuildDesignRootHir(
    std::span<const lowering::ast_to_hir::TopLevelUnit> tops,
    const hir::UnitSignatures& signatures) -> hir::CompilationUnit {
  hir::CompilationUnit root{
      std::string{kDesignRootUnitName}, std::string{kDesignRootUnitName}};
  // Like any module, its object is published as the class an instance of the
  // unit is, and what it publishes is every instance it builds (LRM 23.6).
  hir::ScopeClassSignature published{
      .class_path = {},
      .members = {},
      .callables = {},
      .loops = {},
      .choices = {},
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
    const hir::ScopeClassId scope_class = root.scope_classes.Import(
        hir::ScopeClassKey{
            .unit_name = top.unit_name,
            .class_path = instance_class.class_path});
    if (!root.scope_classes.IsRecorded(scope_class)) {
      hir::TypeImportMemo memo;
      hir::TypeImporter importer = hir::ReaderImporter(
          signature, root.types, root.classes, root.covergroups,
          root.scope_classes, memo);
      root.scope_classes.Record(
          scope_class, hir::ImportScopeClass(importer, instance_class));
    }
    const hir::InstanceMemberId instance =
        root.root_scope.instance_members.Declare();
    root.root_scope.instance_members.Define(
        instance, hir::InstanceMemberDecl{
                      .instance_name = top.name_in_a_path,
                      .array_dims = {},
                      .alternatives = {hir::InstanceAlternative{
                          .scope_class = scope_class,
                          .body = top.unit_name,
                          .assignments = {}}},
                      .taken = {0}});
    LeaveInputsUnconnected(root, signature, scope_class, instance);
    const std::array kind{hir::UnitObjectType{.scope_class = scope_class}};
    published.members.Add(
        hir::PublishedMember{
            .name = top.instance_name,
            .holder = published.class_path,
            .type = root.types.Intern(hir::Type{hir::ObjectsOf({}, kind)}),
            .storage = hir::BorrowedObjectStorage{}});
    instances.emplace_back(instance);
  }
  const hir::ScopeClassId root_class =
      root.scope_classes.DeclareOwn(published.class_path);
  root.root_scope.published = hir::ScopePublication{
      .scope_class = root_class,
      .signature = std::move(published),
      .members = {instances.size(), std::move(instances)},
      .loops = {},
      .choices = {},
      .disable_targets = {},
      .callables = {}};
  return root;
}

// The object entry of every unit the root's tables make, as each unit
// published it, its types taken into the root's: the tops, which the root's
// own table makes, and every body any table lists.
auto PublishedObjectEntries(
    std::span<const lowering::ast_to_hir::TopLevelUnit> tops,
    const hir::UnitSignatures& signatures,
    const lowering::hir_to_mir::DesignInstantiations& instantiations,
    hir::TypePool& into, hir::ClassRegistry& classes,
    hir::CovergroupRegistry& covergroups,
    hir::ScopeClassRegistry& scope_classes)
    -> std::map<
        std::string, lowering::hir_to_mir::PublishedObjectEntry, std::less<>> {
  std::map<std::string, lowering::hir_to_mir::PublishedObjectEntry, std::less<>>
      entries;
  const auto take = [&](const std::string& unit) {
    if (entries.contains(unit)) return;
    const hir::UnitSignature& signature = signatures.Instantiated(unit);
    const hir::PublishedDesignElement& element =
        hir::DesignElementOf(signature);
    // The entry answers an owning pointer to the class, which names it and
    // reads nothing of what it holds, so the root depends on none of that.
    const hir::ScopeClassId published_class = scope_classes.Import(
        hir::ScopeClassKey{
            .unit_name = unit,
            .class_path = element.instance_class.class_path});
    hir::TypeImportMemo memo;
    hir::TypeImporter importer = hir::ReaderImporter(
        signature, into, classes, covergroups, scope_classes, memo);
    entries.emplace(
        unit, lowering::hir_to_mir::PublishedObjectEntry{
                  .published_class = published_class,
                  .handed = hir::ImportTypes(importer, element.handed)});
  };
  for (const lowering::ast_to_hir::TopLevelUnit& top : tops) {
    take(top.unit_name);
  }
  for (const auto& [_, made] : instantiations.by_body) {
    for (const hir::UnitBodyRef& body : made) {
      take(body.unit);
    }
  }
  return entries;
}

}  // namespace

auto SynthesizeDesignRoot(
    std::span<const lowering::ast_to_hir::TopLevelUnit> tops,
    const hir::UnitSignatures& signatures,
    lowering::hir_to_mir::DesignInstantiations instantiations,
    const diag::SourceManager& source_manager,
    const mir_transform::PassOptions& passes)
    -> diag::Result<mir::CompilationUnit> {
  hir::CompilationUnit root_hir = BuildDesignRootHir(tops, signatures);
  auto object_entries = PublishedObjectEntries(
      tops, signatures, instantiations, root_hir.types, root_hir.classes,
      root_hir.covergroups, root_hir.scope_classes);
  lowering::hir_to_mir::UnitLowerer root_lowerer(root_hir, source_manager);
  auto root_mir = root_lowerer.RunDesignRoot(
      lowering::hir_to_mir::DesignRootInputs{
          .namespaces =
              lowering::hir_to_mir::DesignNamespaces{
                  .units = signatures.NamespaceUnitNames()},
          .instantiations = std::move(instantiations),
          .object_entries = std::move(object_entries)});
  if (!root_mir) {
    return std::unexpected(std::move(root_mir.error()));
  }
  mir::CompilationUnit transformed =
      mir_transform::Transform(*std::move(root_mir), passes);
  RecordSemanticForm(transformed);
  return transformed;
}

}  // namespace lyra::compiler
