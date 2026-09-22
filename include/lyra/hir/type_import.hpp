#pragma once

#include <optional>
#include <string_view>
#include <vector>

#include "lyra/hir/class_ref.hpp"
#include "lyra/hir/class_registry.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/type_id.hpp"

namespace lyra::hir {

// The unit a source pool still belongs to, for a pool whose class references
// are that unit's own ids. A pool that has left its unit -- a
// signature's -- names every class by declaring unit and class name instead, so
// it has no owner, and a local id appearing in one is a compiler-bug invariant
// rather than a case to translate.
struct TypePoolOwner {
  std::string_view unit_name;
  const ClassRegistry* classes;
  // Whether a unit reading these types may name the classes this one declares.
  // A design element's are types of its instance (LRM 6.22) and no signature
  // carries them, so a handle to one leaves this pool as the handle that names
  // no class -- which is the whole of what a reader can hold, since everything
  // it could ask of such a class it asks the declaring scope by name.
  bool classes_are_nameable = true;
};

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
      const TypePool& source, std::optional<TypePoolOwner> source_owner,
      TypePool& destination, TypeImportMemo& memo);

  auto Import(TypeId id) -> TypeId;

 private:
  auto Import(const Type& type) -> Type;

  // The class a reference names, as the destination reaches it. A class the
  // source's own unit declares is renamed to that unit and the class's name,
  // which is what identifies it from anywhere; a class already named that way
  // needs nothing.
  [[nodiscard]] auto ImportClassRef(const ClassRef& ref) const -> ClassRef;

  const TypePool* source_;
  std::optional<TypePoolOwner> source_owner_;
  TypePool* destination_;
  TypeImportMemo* memo_;
};

}  // namespace lyra::hir
