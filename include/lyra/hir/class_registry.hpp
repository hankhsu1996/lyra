#pragma once

#include <cstddef>
#include <string>
#include <utility>
#include <vector>

#include "lyra/base/id_range.hpp"
#include "lyra/base/registry.hpp"
#include "lyra/hir/class_decl.hpp"
#include "lyra/hir/class_id.hpp"

namespace lyra::hir {

// The classes one compilation unit declares.
//
// A class variable may be declared before the class itself is, which is how two
// classes each hold a handle to the other and how one holds a handle to itself
// (LRM 8.27). So a class is nameable from the moment the source names it and
// complete only once its declaration is settled, and the two are answered
// separately: the name arrives with the identity, what the class declares
// arrives with the declaration. Asking what a class is called therefore never
// waits on anything, which is what a unit's published surface needs -- another
// unit names a class by the declaring unit and that name, and nothing else
// (LRM 26.3).
class ClassRegistry {
 public:
  auto Declare(std::string name) -> ClassId {
    names_.push_back(std::move(name));
    return decls_.Declare();
  }

  void Define(ClassId id, ClassDecl decl) {
    decls_.Define(id, std::move(decl));
  }

  [[nodiscard]] auto NameOf(ClassId id) const -> const std::string& {
    return names_.at(id.value);
  }

  [[nodiscard]] auto Get(ClassId id) const -> const ClassDecl& {
    return decls_.Get(id);
  }

  [[nodiscard]] auto IsDefined(ClassId id) const -> bool {
    return decls_.IsDefined(id);
  }

  [[nodiscard]] auto Ids() const -> base::IdRange<ClassId> {
    return decls_.Ids();
  }

  [[nodiscard]] auto size() const -> std::size_t {
    return decls_.size();
  }

 private:
  base::Registry<ClassDecl, ClassId> decls_;
  std::vector<std::string> names_;
};

}  // namespace lyra::hir
