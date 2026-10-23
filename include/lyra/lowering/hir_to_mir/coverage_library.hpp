#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include "lyra/hir/class_ref.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/covergroup.hpp"
#include "lyra/hir/external_class.hpp"
#include "lyra/hir/integral_constant.hpp"
#include "lyra/hir/published_covergroup.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/hir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// The coverage library every covergroup's class is written over (LRM 19), as
// one unit reaches it: the records of its classes this unit read, and the
// declarations of those the unit holds itself -- a coverpoint's class
// specialized on a type of this unit's (LRM 8.25).
class CoverageLibrary {
 public:
  explicit CoverageLibrary(const hir::CompilationUnit& unit);

  [[nodiscard]] auto Refs() const -> const hir::CoverageLibraryRefs& {
    return *refs_;
  }

  // A method of `cls` or of a class it extends, as a call names it.
  struct Method {
    hir::MethodCallee callee;
    hir::PublishedCallable prototype;
  };

  [[nodiscard]] auto MethodOf(
      const hir::ClassRef& cls, std::string_view name) const -> Method;

  // A property of `cls` or of a class it extends.
  struct Property {
    hir::ClassPropertyTarget target;
    hir::TypeId type;
  };

  [[nodiscard]] auto PropertyOf(
      const hir::ClassRef& cls, std::string_view name) const -> Property;

  // The type of each formal of `cls`'s constructor.
  [[nodiscard]] auto ConstructorParamTypes(const hir::ClassRef& cls) const
      -> std::vector<hir::TypeId>;

  // Whether a construction of `cls` is handed the instance it belongs to: a
  // class of this unit specialized on a type of an instance is (LRM 6.22).
  [[nodiscard]] auto TakesDeclaringInstance(const hir::ClassRef& cls) const
      -> bool;

  // The value the enumerator `name` of the library's enumeration `type`
  // stands for.
  [[nodiscard]] auto EnumeratorOf(hir::TypeId type, std::string_view name) const
      -> const hir::IntegralConstant&;

 private:
  [[nodiscard]] auto Record(const hir::ExternalClassRef& ref) const
      -> const hir::ExternalClass&;
  [[nodiscard]] auto SlotOf(hir::ExternalClassRef from, std::string_view name)
      const -> hir::ExternalDispatchSlot;

  const hir::CompilationUnit* unit_;
  const hir::CoverageLibraryRefs* refs_;
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

// What a bin construct counts toward, as the library's `coverage_bin_kind`
// names it.
[[nodiscard]] auto LibraryKindOf(hir::BinKind kind) -> std::string_view;

// How many bins one construct makes, as the library's `coverage_bin_array`
// names it.
[[nodiscard]] auto LibraryArrayOf(const hir::BinArray& array)
    -> std::string_view;

// How a transition step repeats, as the library's `coverage_repetition` names
// it.
[[nodiscard]] auto LibraryRepetitionOf(hir::StepRepetition repetition)
    -> std::string_view;

}  // namespace lyra::lowering::hir_to_mir
