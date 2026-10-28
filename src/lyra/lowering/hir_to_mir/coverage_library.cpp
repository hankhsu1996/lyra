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

namespace {

[[noreturn]] void Unpublished(std::string_view what) {
  throw InternalError(
      std::format(
          "the coverage library published no {}, which every covergroup's "
          "class is written over -- please report this as a bug",
          what));
}

}  // namespace

CoverageLibrary::CoverageLibrary(const hir::CompilationUnit& unit)
    : unit_(&unit), refs_(&*unit.coverage_library) {
}

auto CoverageLibrary::MethodOf(hir::ClassId cls, std::string_view name) const
    -> Method {
  for (std::optional<hir::ClassId> at = cls; at.has_value();
       at = unit_->classes.BaseOf(*at)) {
    const hir::DeclaredMethods& methods =
        unit_->classes.Declaration(*at).methods;
    if (const std::optional<hir::MethodId> method =
            hir::FindMethod(methods, name)) {
      return Method{
          .callee = hir::ClassMethodTarget{.owner = *at, .method = *method},
          .prototype = methods.Get(*method).prototype};
    }
  }
  Unpublished(std::format("method '{}'", name));
}

auto CoverageLibrary::PropertyOf(hir::ClassId cls, std::string_view name) const
    -> Property {
  for (std::optional<hir::ClassId> at = cls; at.has_value();
       at = unit_->classes.BaseOf(*at)) {
    const hir::DeclaredProperties& properties =
        unit_->classes.Declaration(*at).properties;
    if (const std::optional<hir::FieldId> field =
            hir::FindProperty(properties, name)) {
      return Property{
          .target = hir::ClassPropertyTarget{.owner = *at, .field = *field},
          .type = properties.Get(*field).type};
    }
  }
  Unpublished(std::format("property '{}'", name));
}

auto CoverageLibrary::ConstructorParamTypes(hir::ClassId cls) const
    -> std::vector<hir::TypeId> {
  std::vector<hir::TypeId> types;
  const hir::ClassDeclaration& record = unit_->classes.Declaration(cls);
  if (!record.constructor.has_value()) Unpublished("constructor");
  for (const hir::CalleeParam& param : record.constructor->params) {
    types.push_back(param.type);
  }
  return types;
}

auto CoverageLibrary::TakesDeclaringInstance(hir::ClassId cls) const -> bool {
  return unit_->classes.Declaration(cls).takes_declaring_instance;
}

auto CoverageLibrary::EnumeratorOf(hir::TypeId type, std::string_view name)
    const -> const hir::IntegralConstant& {
  const auto* enumeration = unit_->types.Get(type).As<hir::EnumType>();
  if (enumeration == nullptr) {
    Unpublished(std::format("enumeration holding '{}'", name));
  }
  for (const hir::EnumMember& member :
       unit_->DefinitionOf(*enumeration).members) {
    if (member.name == name) return member.value;
  }
  Unpublished(std::format("enumerator '{}'", name));
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
  for (std::size_t i = 0; i < published.crosses.size(); ++i) {
    layout.crosses.push_back(
        add(std::format("__cross_{}", i),
            CrossProperty{.cross = static_cast<std::uint32_t>(i)}));
  }
  return layout;
}

auto LibraryKindOf(hir::BinKind kind) -> std::string_view {
  switch (kind) {
    case hir::BinKind::kBins:
      return "COVERAGE_BINS";
    case hir::BinKind::kIgnoreBins:
      return "COVERAGE_IGNORE_BINS";
    case hir::BinKind::kIllegalBins:
      return "COVERAGE_ILLEGAL_BINS";
  }
  throw InternalError("LibraryKindOf: unknown kind of bins");
}

auto LibraryArrayOf(const hir::BinArray& array) -> std::string_view {
  return std::visit(
      Overloaded{
          [](const hir::OneBin&) { return "COVERAGE_ONE_BIN"; },
          [](const hir::BinPerValue&) { return "COVERAGE_BIN_PER_VALUE"; },
          [](const hir::FixedBins&) { return "COVERAGE_FIXED_BINS"; }},
      array);
}

auto LibraryRepetitionOf(hir::StepRepetition repetition) -> std::string_view {
  switch (repetition) {
    case hir::StepRepetition::kOnce:
      return "COVERAGE_ONCE";
    case hir::StepRepetition::kConsecutive:
      return "COVERAGE_CONSECUTIVE";
    case hir::StepRepetition::kGoTo:
      return "COVERAGE_GOTO";
    case hir::StepRepetition::kNonconsecutive:
      return "COVERAGE_NONCONSECUTIVE";
  }
  throw InternalError("LibraryRepetitionOf: unknown kind of repetition");
}

}  // namespace lyra::lowering::hir_to_mir
