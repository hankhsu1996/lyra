#pragma once

#include <cstddef>
#include <unordered_map>

#include "lyra/base/arena.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

// Interning equality over MIR types: the rule that decides which `TypeData`
// requests collapse to one canonical id. A nominal type compares by its
// declaration identity; a structural type by its constructor, the canonical
// `TypeId`s of its children, and its semantic modifiers. A packed array's slang
// syntactic-origin marker carries no semantic content and is excluded, so `int`
// and `bit signed [31:0]` canonicalize together. These functors are the
// definition of that equality; everything else follows from them.
struct SemanticTypeHash {
  auto operator()(const TypeData& data) const -> std::size_t;
};

struct SemanticTypeEqual {
  auto operator()(const TypeData& a, const TypeData& b) const -> bool;
};

// The MIR type pool. Interning canonicalizes: within one compilation unit a
// semantic type maps to one canonical `TypeId`, so equal types share an id and
// a `TypeId` comparison is a semantic-type comparison.
//
// Storage and canonicalization are separate concerns: a plain append-only arena
// holds the canonical representatives and mints the dense `TypeId`; a key index
// keyed by semantic identity decides which requests collapse to one id. The
// `TypeId` is the local handle; the equality functor is the definition of
// interning equality.
//
// Only complete type requests are interned -- there is no reserve-then-fill or
// placeholder. A recursive type graph is broken by a nominal type, whose
// declaration identity exists before the body, so no incomplete request arises.
//
// `Get` keeps the arena's transient-view contract: the `TypeId` is durable, a
// reference it returns must not be held across a later `Intern`.
class TypeInterner {
 public:
  auto Intern(TypeData data) -> TypeId;

  [[nodiscard]] auto Get(TypeId id) const -> const Type& {
    return storage_.Get(id);
  }

  [[nodiscard]] auto size() const -> std::size_t {
    return storage_.size();
  }

  [[nodiscard]] auto begin() const {
    return storage_.begin();
  }

  [[nodiscard]] auto end() const {
    return storage_.end();
  }

  // Typed constructors for the composite types lowering materializes
  // repeatedly. Each interns its constructor, so a repeated request returns the
  // one canonical id rather than a duplicate.
  auto CoroutineOf(TypeId payload) -> TypeId {
    return Intern(CoroutineType{.payload = payload});
  }

  // Whether a callable's result type states the coroutine call protocol: the
  // body may suspend and completes as a coroutine, rather than running to a
  // value in one call. The protocol is the type -- nothing else records it --
  // so every consumer that realizes suspension or completion asks this of the
  // result type.
  [[nodiscard]] auto IsCoroutine(TypeId id) const -> bool {
    return std::holds_alternative<CoroutineType>(Get(id).data);
  }

  auto PointerTo(
      TypeId pointee, PointerOwnership ownership,
      Mutability mutability = Mutability::kMutable) -> TypeId {
    return Intern(
        PointerType{
            .pointee = pointee,
            .ownership = ownership,
            .mutability = mutability});
  }

 private:
  base::Arena<Type, TypeId> storage_;
  std::unordered_map<TypeData, TypeId, SemanticTypeHash, SemanticTypeEqual>
      index_;
};

}  // namespace lyra::mir
