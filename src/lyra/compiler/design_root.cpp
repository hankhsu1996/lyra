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

// Nothing instantiates a top, so every input of one is left unconnected and
// holds its data type's default initial value (LRM 23.3.3.2). An input whose
// member owns no cell is given that the way any instantiating scope gives it:
// by a connection that names no source.
void LeaveInputsUnconnected(
    hir::CompilationUnit& root, const hir::UnitSignature& signature,
    hir::ExternalScopeClassId scope_class, hir::InstanceMemberId instance) {
  const hir::ScopeClassSignature& imported =
      root.external_scope_classes.Get(scope_class).signature;
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
      root.root_scope.port_connections.Add(
          hir::PortConnection{
              .span = {},
              .kind = hir::DataPortConnection{
                  .direction = hir::PortDirection::kInput,
                  .endpoint =
                      hir::ValueRoute{
                          .base = hir::InUnitBase{.hops = {}},
                          .steps = {hir::AsPathStep(
                              hir::OwnedChildStep{
                                  .names = hir::OwnedChildRef{instance},
                                  .selects = {}})},
                          .leaf =
                              hir::ExternalMemberLeaf{
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
  hir::CompilationUnit root{std::string{kDesignRootUnitName}};
  // Like any module, its object is published as the class an instance of the
  // unit is, and what it publishes is every instance it builds (LRM 23.6).
  hir::ScopeClassSignature published{
      .class_path = {},
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
                      .instance_name = top.name_in_a_path,
                      .array_dims = {},
                      .alternatives = {hir::InstanceAlternative{
                          .scope_class = scope_class, .arguments = {}}},
                      .taken = {0}});
    LeaveInputsUnconnected(root, signature, scope_class, instance);
    const std::array kind{hir::UnitObjectType{
        .unit_name = top.unit_name, .class_path = instance_class.class_path}};
    published.members.Add(
        hir::PublishedMember{
            .name = top.instance_name,
            .holder = published.class_path,
            .type = root.types.Intern(hir::Type{hir::ObjectsOf({}, kind)}),
            .storage = hir::BorrowedObjectStorage{}});
    instances.emplace_back(instance);
  }
  root.root_scope.published = hir::ScopePublication{
      .signature = std::move(published),
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
