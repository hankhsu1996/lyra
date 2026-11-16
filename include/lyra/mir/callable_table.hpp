#pragma once

#include <cstddef>
#include <map>
#include <optional>
#include <utility>
#include <vector>

#include "lyra/base/id_range.hpp"
#include "lyra/base/registry.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/callable_id.hpp"

namespace lyra::mir {

struct CompilationUnit;

// What a class declares among the unit's callables: every one but its
// construction, in the order it declares them -- instance methods (LRM 8.6),
// process and lifecycle bodies, and the receiver-less static methods (LRM
// 8.10) -- and its construction.
//
// A foreign function is never among them: its name is program-global and
// belongs to no class (LRM 35.4), so the unit's namespace owns it. An instance
// method carries `self` as its first parameter and a static callable omits it,
// but both are one `CallableDecl` reached by one `CallableId` -- the receiver
// is a property of the signature, not a separate declaration space. The order
// is what the behaviors the class introduces are read in, and a body nothing
// names is spelled from where it stands in it.
//
// The construction is entered on an object already created whole (LRM 8.7):
// every member of every class of its lineage exists and the object is of its
// own class before any constructor runs. Every step of construction is a
// statement of it, in order: what the compiler records for the object, the
// base's construction, each property's start value and initializer, then the
// source's body. A class this unit defines defines its construction, and an
// empty body is a class that constructs nothing; a class another unit defines
// states only what its construction takes, as it states each method's
// prototype. It never dispatches, so it is none of `callables`. Absent for a
// class no object is ever built of: an interface (LRM 8.26) holds no storage
// and is never constructed, so it has no construction to state.
struct DeclaredCallables {
  std::vector<CallableId> callables;
  std::optional<CallableId> constructor;
};

// The callables of every owner: each class's and the namespace's in ordinal
// order, each closure's body, and each record's methods in the order they
// were taken.
struct CallablesByOwner {
  std::vector<DeclaredCallables> of_classes;
  std::vector<CallableId> of_namespace;
  std::vector<std::optional<CallableId>> of_closures;
  std::vector<std::vector<CallableId>> of_records;
};

// The unit's one table of callable declarations, as a translation unit holds
// one declaration per function whatever declares it. Identity is minted
// separately from content, because a callable is named before it exists: a
// subroutine may call a sibling the source declared after it, or itself, and a
// class extending another names a behavior the other introduces whichever is
// lowered first (LRM 8.20); what such a call names is the identity.
//
// Only the unit's take functions write it. Each stamps the owner, and appends
// the identity to what that owner declares, as clang's `DeclContext::addDecl`
// keeps a context's list and the declaration's context together; an ordinal is
// how many the owner has taken before it, as rustc hands out each
// disambiguator under its parent. So nothing outside can state an owner the
// take did not, and what an owner declares never disagrees with the owners.
class CallableTable {
 public:
  [[nodiscard]] auto Get(CallableId id) const -> const CallableDecl& {
    return decls_.Get(id);
  }

  // Whether `id`, which is taken, has had its code stated. A question only a
  // lowering running mid-construction has.
  [[nodiscard]] auto IsStated(CallableId id) const -> bool {
    return decls_.IsDefined(id);
  }

  [[nodiscard]] auto Ids() const -> base::IdRange<CallableId> {
    return decls_.Ids();
  }

  [[nodiscard]] auto size() const -> std::size_t {
    return decls_.size();
  }

  // One identity taken whose code is never stated, where there is one; a
  // finished unit has none.
  [[nodiscard]] auto AnyUnstated() const -> std::optional<CallableId> {
    if (unstated_.empty()) {
      return std::nullopt;
    }
    return unstated_.begin()->first;
  }

  // Every stated declaration's code passed through `rewrite`, given its
  // identity, so each stays at the identity every reference to it names.
  template <typename Rewrite>
  void RewriteEachCode(const Rewrite& rewrite) {
    decls_ = std::move(decls_).TransformValues(
        [&](CallableId id, CallableDecl decl) {
          rewrite(id, decl.code);
          return decl;
        });
  }

 private:
  friend struct CompilationUnit;

  base::Registry<CallableDecl, CallableId> decls_;
  // The owner each identity was taken with, until its code is stated.
  std::map<CallableId, CallableOwner> unstated_;
  CallablesByOwner by_owner_;
};

}  // namespace lyra::mir
