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
#include "lyra/hir/class_decl.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/external_class.hpp"
#include "lyra/hir/published_covergroup.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/support/def_path.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

[[noreturn]] void Unpublished(std::string_view what) {
  throw InternalError(
      std::format(
          "the coverage library published no {}, which every covergroup's "
          "class is written over -- please report this as a bug",
          what));
}

// The prototype a call of a method of this unit's own is made through.
auto PrototypeOf(const hir::SubroutineDecl& method) -> hir::PublishedCallable {
  hir::PublishedCallable prototype{
      .name = method.name,
      .interface =
          hir::ExternalCalleeInterface{.kind = method.kind, .params = {}},
      .result_type = method.result_type};
  for (const hir::SubroutineParam& param : method.params) {
    prototype.interface.params.push_back(
        hir::ExternalCalleeParam{
            .direction = param.direction,
            .type = method.body.procedural_vars.Get(param.var).type});
  }
  return prototype;
}

}  // namespace

CoverageLibrary::CoverageLibrary(const hir::CompilationUnit& unit)
    : unit_(&unit), refs_(&*unit.coverage_library) {
}

auto CoverageLibrary::MethodOf(
    const hir::ClassRef& cls, std::string_view name) const -> Method {
  for (std::optional<hir::ClassRef> at = cls; at.has_value();) {
    if (const auto* local = std::get_if<hir::LocalClassRef>(&*at)) {
      const hir::ClassDecl& decl = unit_->classes.Get(local->class_id);
      for (const hir::MethodId id : decl.methods.Ids()) {
        const hir::SubroutineDecl& method = decl.methods.Get(id);
        if (method.name != name) continue;
        return Method{
            .callee =
                hir::LocalClassMethodTarget{
                    .owner = local->class_id, .method = id},
            .prototype = PrototypeOf(method)};
      }
      at = decl.base;
      continue;
    }
    const auto& external = std::get<hir::ExternalClassRef>(*at);
    const hir::ExternalClass& record = Record(external);
    if (const hir::PublishedMethod* method =
            hir::FindMethod(record.methods, name)) {
      const bool is_virtual = std::visit(
          Overloaded{
              [](const hir::TypeAssociated&) { return false; },
              [](const hir::NotVirtual&) { return false; },
              [](const hir::IntroducesVirtual&) { return true; },
              [](const hir::OverridesVirtual&) { return true; }},
          method->dispatch);
      return Method{
          .callee =
              hir::ExternalMethodCallee{
                  .target =
                      hir::ExternalClassMethodTarget{
                          .unit_name = external.unit_name,
                          .class_path = external.class_path,
                          .method_name = std::string{name}},
                  .behavior = is_virtual
                                  ? std::optional<hir::VirtualBehavior>{SlotOf(
                                        external, name)}
                                  : std::nullopt,
                  .interface = method->prototype.interface},
          .prototype = method->prototype};
    }
    at = record.base.transform(
        [](const hir::ExternalClassRef& base) { return hir::ClassRef{base}; });
  }
  Unpublished(std::format("method '{}'", name));
}

auto CoverageLibrary::PropertyOf(
    const hir::ClassRef& cls, std::string_view name) const -> Property {
  for (std::optional<hir::ClassRef> at = cls; at.has_value();) {
    if (const auto* local = std::get_if<hir::LocalClassRef>(&*at)) {
      const hir::ClassDecl& decl = unit_->classes.Get(local->class_id);
      for (const hir::FieldId id : decl.fields.Ids()) {
        if (decl.fields.Get(id).name != name) continue;
        return Property{
            .target =
                hir::LocalClassPropertyTarget{
                    .owner = local->class_id, .field = id},
            .type = decl.fields.Get(id).type};
      }
      at = decl.base;
      continue;
    }
    const auto& external = std::get<hir::ExternalClassRef>(*at);
    const hir::ExternalClass& record = Record(external);
    if (const auto id = hir::FindProperty(record.properties, name)) {
      return Property{
          .target =
              hir::ExternalClassPropertyTarget{
                  .unit_name = external.unit_name,
                  .class_path = external.class_path,
                  .property = *id},
          .type = record.properties.Get(*id).type};
    }
    at = record.base.transform(
        [](const hir::ExternalClassRef& base) { return hir::ClassRef{base}; });
  }
  Unpublished(std::format("property '{}'", name));
}

auto CoverageLibrary::ConstructorParamTypes(const hir::ClassRef& cls) const
    -> std::vector<hir::TypeId> {
  std::vector<hir::TypeId> types;
  if (const auto* local = std::get_if<hir::LocalClassRef>(&cls)) {
    const hir::SubroutineDecl& constructor =
        unit_->classes.Get(local->class_id).constructor;
    for (const hir::SubroutineParam& param : constructor.params) {
      types.push_back(constructor.body.procedural_vars.Get(param.var).type);
    }
    return types;
  }
  const hir::ExternalClass& record =
      Record(std::get<hir::ExternalClassRef>(cls));
  if (!record.constructor.has_value()) Unpublished("constructor");
  for (const hir::ExternalCalleeParam& param : record.constructor->params) {
    types.push_back(param.type);
  }
  return types;
}

auto CoverageLibrary::TakesDeclaringInstance(const hir::ClassRef& cls) const
    -> bool {
  const auto* local = std::get_if<hir::LocalClassRef>(&cls);
  return local != nullptr &&
         unit_->classes.Get(local->class_id).takes_declaring_instance;
}

auto CoverageLibrary::EnumeratorOf(hir::TypeId type, std::string_view name)
    const -> const hir::IntegralConstant& {
  const auto* enumeration = unit_->types.Get(type).As<hir::EnumType>();
  if (enumeration == nullptr) {
    Unpublished(std::format("enumeration holding '{}'", name));
  }
  for (const hir::EnumMember& member : enumeration->members) {
    if (member.name == name) return member.value;
  }
  Unpublished(std::format("enumerator '{}'", name));
}

auto CoverageLibrary::Record(const hir::ExternalClassRef& ref) const
    -> const hir::ExternalClass& {
  const hir::ExternalClass* record = hir::FindExternalClass(
      unit_->external_classes, ref.unit_name, ref.class_path);
  if (record == nullptr) {
    Unpublished(
        std::format("record of '{}'", support::DisplayOf(ref.class_path)));
  }
  return *record;
}

// The behavior a virtual method answers, named by the class that introduced it
// (LRM 8.20).
auto CoverageLibrary::SlotOf(hir::ExternalClassRef from, std::string_view name)
    const -> hir::ExternalDispatchSlot {
  for (std::optional<hir::ExternalClassRef> at = from; at.has_value();) {
    const hir::ExternalClass& record = Record(*at);
    if (const auto behavior =
            hir::FindIntroducedVirtual(record.methods, name)) {
      return hir::ExternalDispatchSlot{
          .unit_name = at->unit_name,
          .class_path = at->class_path,
          .behavior = *behavior};
    }
    at = record.base;
  }
  Unpublished(std::format("introduction of virtual method '{}'", name));
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
