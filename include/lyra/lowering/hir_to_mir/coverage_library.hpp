#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include "lyra/hir/class_id.hpp"
#include "lyra/hir/class_ref.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/coverage_library.hpp"
#include "lyra/hir/covergroup.hpp"
#include "lyra/hir/published_covergroup.hpp"
#include "lyra/hir/subroutine_prototype.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/hir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// The coverage library every covergroup's class is written over (LRM 19), as
// one unit reaches it: each member the unit resolved where it named the
// library, with what the unit holds of the class declaring it.
class CoverageLibrary {
 public:
  explicit CoverageLibrary(const hir::CompilationUnit& unit);

  [[nodiscard]] auto Members() const -> const hir::CoverageLibraryMembers& {
    return *members_;
  }

  // The members of `counted_by`, the class counting one of the unit's
  // coverpoints, and those of them only an integral coverpoint's class has.
  [[nodiscard]] auto CoverpointClass(hir::ClassId counted_by) const
      -> const hir::CoverpointClassMembers&;
  [[nodiscard]] auto IntegralCoverpointClass(hir::ClassId counted_by) const
      -> const hir::IntegralCoverpointClassMembers&;

  // A method of the library, as a call names it.
  struct Method {
    hir::ClassMethodTarget callee;
    hir::SubroutinePrototype prototype;
  };

  [[nodiscard]] auto MethodOf(const hir::ClassMethodTarget& target) const
      -> Method;

  // A property of the library, and the type it holds.
  struct Property {
    hir::ClassPropertyTarget target;
    hir::TypeId type;
  };

  [[nodiscard]] auto PropertyOf(const hir::ClassPropertyTarget& target) const
      -> Property;

  // The type of each formal of `cls`'s constructor.
  [[nodiscard]] auto ConstructorParamTypes(hir::ClassId cls) const
      -> std::vector<hir::TypeId>;

  // Whether a construction of `cls` is handed the instance it belongs to (LRM
  // 6.22).
  [[nodiscard]] auto TakesDeclaringInstance(hir::ClassId cls) const -> bool;

 private:
  const hir::CompilationUnit* unit_;
  const hir::CoverageLibraryMembers* members_;
};

// What one property of a covergroup's class holds: the object embedding it,
// an argument, the instance's record, or an item's library object. A class
// holds only what its own declaration adds (LRM 19.4.1), so the arguments and
// the record are the first class's of a lineage.
struct EmbeddingProperty {};

struct FormalProperty {
  std::uint32_t formal;
};

struct RecordProperty {};

struct CoverpointProperty {
  std::uint32_t coverpoint;
};

struct CrossProperty {
  std::uint32_t cross;
};

using CovergroupPropertyRole = std::variant<
    EmbeddingProperty, FormalProperty, RecordProperty, CoverpointProperty,
    CrossProperty>;

struct CovergroupProperty {
  std::string name;
  CovergroupPropertyRole role;
};

// The properties of the class a covergroup is written as, in the order the
// class places them, with where each role landed. It is a function of what the
// covergroup's unit publishes of it alone, so the unit declaring the
// covergroup and every unit naming it place each property alike (LRM 26.3).
struct CovergroupLayout {
  std::vector<CovergroupProperty> properties;
  std::optional<std::uint32_t> embedding;
  std::vector<std::uint32_t> formals;
  std::optional<std::uint32_t> record;
  std::vector<std::uint32_t> coverpoints;
  std::vector<std::uint32_t> crosses;
};

[[nodiscard]] auto CovergroupLayoutOf(const hir::PublishedCovergroup& published)
    -> CovergroupLayout;

// The methods of a covergroup's class another class or unit names: `sample`
// (LRM 19.8) and the record of the object's own type, both virtual and
// introduced by the first class of a lineage in that order, and the type's
// record and what the declaration states of its type, both of the class
// itself.
inline constexpr std::string_view kCovergroupSample = "sample";
inline constexpr std::string_view kCovergroupMakeRecord = "__make_record";
inline constexpr std::string_view kCovergroupTypeRecord = "__type";
inline constexpr std::string_view kCovergroupDeclareType = "__declare_type";

// The library's enumerator for what a bin construct counts toward, for how many
// bins it makes, and for how a transition step repeats.
[[nodiscard]] auto LibraryKindOf(
    const hir::CoverageEnumerators& enumerators, hir::BinKind kind)
    -> const hir::LibraryEnumerator&;
[[nodiscard]] auto LibraryArrayOf(
    const hir::CoverageEnumerators& enumerators, const hir::BinArray& array)
    -> const hir::LibraryEnumerator&;
[[nodiscard]] auto LibraryRepetitionOf(
    const hir::CoverageEnumerators& enumerators, hir::StepRepetition repetition)
    -> const hir::LibraryEnumerator&;

}  // namespace lyra::lowering::hir_to_mir
