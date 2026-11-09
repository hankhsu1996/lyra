#include "lyra/lowering/hir_to_mir/coverage_library.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/class_declaration.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/published_covergroup.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/type_definition.hpp"

namespace lyra::lowering::hir_to_mir {

CoverageLibrary::CoverageLibrary(const hir::CompilationUnit& unit)
    : unit_(&unit), members_(&*unit.coverage_library) {
}

auto CoverageLibrary::CoverpointClass(hir::ClassId counted_by) const
    -> const hir::CoverpointClassMembers& {
  const auto found = members_->coverpoint_classes.find(counted_by);
  if (found == members_->coverpoint_classes.end()) {
    throw InternalError(
        "the unit resolved no members of a class counting one of its "
        "coverpoints -- please report this as a bug");
  }
  return found->second;
}

auto CoverageLibrary::IntegralCoverpointClass(hir::ClassId counted_by) const
    -> const hir::IntegralCoverpointClassMembers& {
  const std::optional<hir::IntegralCoverpointClassMembers>& integral =
      CoverpointClass(counted_by).integral;
  if (!integral.has_value()) {
    throw InternalError(
        "a real coverpoint was given what only an integral one takes, which "
        "the front end refuses -- please report this as a bug");
  }
  return *integral;
}

auto CoverageLibrary::MethodOf(const hir::ClassMethodTarget& target) const
    -> Method {
  return Method{
      .callee = target,
      .prototype = unit_->classes.Declaration(target.owner)
                       .methods.Get(target.method)
                       .prototype};
}

auto CoverageLibrary::PropertyOf(const hir::ClassPropertyTarget& target) const
    -> Property {
  return Property{
      .target = target,
      .type = unit_->classes.Declaration(target.owner)
                  .properties.Get(target.field)
                  .type};
}

auto CoverageLibrary::ConstructorParamTypes(hir::ClassId cls) const
    -> std::vector<hir::TypeId> {
  std::vector<hir::TypeId> types;
  const hir::ClassDeclaration& record = unit_->classes.Declaration(cls);
  if (!record.constructor.has_value()) {
    throw InternalError(
        "the coverage library published no constructor, which every "
        "covergroup's class is written over -- please report this as a bug");
  }
  for (const hir::CalleeParam& param : record.constructor->interface.params) {
    types.push_back(param.type);
  }
  return types;
}

auto CoverageLibrary::TakesDeclaringInstance(hir::ClassId cls) const -> bool {
  return unit_->classes.Declaration(cls).instance_scope.has_value();
}

auto CovergroupLayoutOf(const hir::PublishedCovergroup& published)
    -> CovergroupLayout {
  CovergroupLayout layout;
  const auto add = [&](std::string name, CovergroupPropertyRole role) {
    layout.properties.push_back(
        CovergroupProperty{.name = std::move(name), .role = role});
    return static_cast<std::uint32_t>(layout.properties.size() - 1);
  };
  if (published.embedding.has_value()) {
    layout.embedding = add("__embedding", EmbeddingProperty{});
  }
  if (!published.base.has_value()) {
    for (std::size_t i = 0; i < published.formals.size(); ++i) {
      layout.formals.push_back(
          add(std::format("__formal_{}", i),
              FormalProperty{.formal = static_cast<std::uint32_t>(i)}));
    }
    layout.record = add("__record", RecordProperty{});
  }
  for (std::size_t i = 0; i < published.coverpoints.size(); ++i) {
    layout.coverpoints.push_back(
        add(std::format("__coverpoint_{}", i),
            CoverpointProperty{.coverpoint = static_cast<std::uint32_t>(i)}));
  }
  for (std::size_t i = 0; i < published.cross_count; ++i) {
    layout.crosses.push_back(
        add(std::format("__cross_{}", i),
            CrossProperty{.cross = static_cast<std::uint32_t>(i)}));
  }
  return layout;
}

auto LibraryKindOf(
    const hir::CoverageEnumerators& enumerators, hir::BinKind kind)
    -> const hir::LibraryEnumerator& {
  switch (kind) {
    case hir::BinKind::kBins:
      return enumerators.bins;
    case hir::BinKind::kIgnoreBins:
      return enumerators.ignore_bins;
    case hir::BinKind::kIllegalBins:
      return enumerators.illegal_bins;
  }
  throw InternalError("LibraryKindOf: unknown kind of bins");
}

auto LibraryArrayOf(
    const hir::CoverageEnumerators& enumerators, const hir::BinArray& array)
    -> const hir::LibraryEnumerator& {
  return std::visit(
      Overloaded{
          [&](const hir::OneBin&) -> const hir::LibraryEnumerator& {
            return enumerators.one_bin;
          },
          [&](const hir::BinPerValue&) -> const hir::LibraryEnumerator& {
            return enumerators.bin_per_value;
          },
          [&](const hir::FixedBins&) -> const hir::LibraryEnumerator& {
            return enumerators.fixed_bins;
          }},
      array);
}

auto LibraryRepetitionOf(
    const hir::CoverageEnumerators& enumerators, hir::StepRepetition repetition)
    -> const hir::LibraryEnumerator& {
  switch (repetition) {
    case hir::StepRepetition::kOnce:
      return enumerators.once;
    case hir::StepRepetition::kConsecutive:
      return enumerators.consecutive;
    case hir::StepRepetition::kGoTo:
      return enumerators.go_to;
    case hir::StepRepetition::kNonconsecutive:
      return enumerators.nonconsecutive;
  }
  throw InternalError("LibraryRepetitionOf: unknown kind of repetition");
}

}  // namespace lyra::lowering::hir_to_mir
