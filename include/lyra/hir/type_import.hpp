#pragma once

#include <functional>
#include <optional>
#include <vector>

#include "lyra/hir/class_id.hpp"
#include "lyra/hir/covergroup_ref.hpp"
#include "lyra/hir/published_covergroup.hpp"
#include "lyra/hir/scope_class_id.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/type_id.hpp"

namespace lyra::hir {

// The identity a class of the source pool's context has in the destination's.
// A pool names a class by the identity its own context gives it -- a unit's
// registry, a signature's class list -- so moving a type between the two maps
// each class it names, as rustc maps each crate number of the metadata it
// decodes.
using ClassIdMap = std::function<ClassId(ClassId)>;

// The same for a covergroup the source pool names.
using CovergroupIdMap = std::function<CovergroupId(CovergroupId)>;

// The same for the class of a scope of the design hierarchy.
using ScopeClassIdMap = std::function<ScopeClassId(ScopeClassId)>;

// What one source pool's identities became in a destination pool. Held outside
// the importer so a consumer taking types out of the same source over the
// course of its lowering keeps taking each one once, which is what makes the
// sharing the source had survive the move.
using TypeImportMemo = std::vector<std::optional<TypeId>>;

// Takes types out of one pool and into another. A type identity indexes the
// pool that minted it and means nothing in any other, so a type reaching a new
// pool arrives as its whole subgraph, interned there. A type the destination
// already holds is not copied: the destination's identity is structural, so the
// import lands on the entry it already had.
class TypeImporter {
 public:
  TypeImporter(
      const TypePool& source, TypePool& destination, TypeImportMemo& memo,
      ClassIdMap classes, CovergroupIdMap covergroups,
      ScopeClassIdMap scope_classes);

  auto Import(TypeId id) -> TypeId;

  // A covergroup published by the source's unit, its types taken into the
  // destination and every covergroup and class it names renamed as a type
  // naming one is.
  auto Import(const PublishedCovergroup& published) -> PublishedCovergroup;

  // The identity the class `id` of the source's context has in the
  // destination's.
  [[nodiscard]] auto ImportClass(ClassId id) const -> ClassId;

  // The identity the covergroup `id` of the source's context has in the
  // destination's.
  [[nodiscard]] auto ImportCovergroup(CovergroupId id) const -> CovergroupId;

  // The identity the scope class `id` of the source's context has in the
  // destination's.
  [[nodiscard]] auto ImportScopeClass(ScopeClassId id) const -> ScopeClassId;

 private:
  auto Import(const Type& type) -> Type;

  const TypePool* source_;
  TypePool* destination_;
  TypeImportMemo* memo_;
  ClassIdMap classes_;
  CovergroupIdMap covergroups_;
  ScopeClassIdMap scope_classes_;
};

}  // namespace lyra::hir
