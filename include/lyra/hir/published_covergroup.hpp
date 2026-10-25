#pragma once

#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/hir/class_ref.hpp"
#include "lyra/hir/covergroup_ref.hpp"
#include "lyra/hir/param_direction.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/support/def_path.hpp"

namespace lyra::hir {

// What another unit needs of a covergroup it names (LRM 26.3): its formals as
// declared, the formals of its `sample`, its coverpoints with the classes that
// count them, its crosses, the class embedding it, and the covergroup it
// extends. Everything the step to MIR makes of a covergroup is a function of
// these, so a unit naming it makes what the declaring unit makes.
struct PublishedCovergroupFormal {
  std::string name;
  TypeId type;
  ParamDirection direction = ParamDirection::kInput;

  auto operator==(const PublishedCovergroupFormal&) const -> bool = default;
};

struct PublishedCoverpoint {
  std::string name;
  TypeId value_type;
  ClassId library_class;

  auto operator==(const PublishedCoverpoint&) const -> bool = default;
};

struct PublishedCovergroup {
  support::DefPath path;
  std::vector<PublishedCovergroupFormal> formals;
  std::vector<PublishedCovergroupFormal> sample_formals;
  std::optional<ClassId> embedding;
  std::vector<PublishedCoverpoint> coverpoints;
  std::vector<std::string> crosses;
  std::optional<CovergroupId> base;

  auto operator==(const PublishedCovergroup&) const -> bool = default;
};

// The method of a class embedding a covergroup that makes the covergroup's
// instance (LRM 19.4.1): virtual, so the `new` a class writes for its embedded
// covergroup makes the extension a class extending it declares. It is a method
// of the class's signature, named by the covergroup's name, which an
// extension shares, and by a space no identifier holds (LRM 5.6.1).
[[nodiscard]] inline auto CovergroupFactoryName(
    const support::DefPath& covergroup) -> std::string {
  return std::string{"make "} +
         std::get<support::ClassStep>(covergroup.data.back()).name;
}

}  // namespace lyra::hir
