#include "lyra/backend/cpp/render_type.hpp"

#include <cstddef>
#include <format>
#include <string>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::backend::cpp {

auto RenderPackedArrayCtorArgs(const mir::PackedArrayType& pa) -> std::string {
  const char* signed_lit =
      pa.signedness == mir::Signedness::kSigned ? "true" : "false";
  const char* four_state_lit = pa.atom != mir::BitAtom::kBit ? "true" : "false";

  // 1D shorthand: emit `bit_width, signed, four_state` -- uses the
  // PackedArray 3-arg constructor. The dim list ctor would produce the same
  // shape but is less readable for the common case.
  if (pa.dims.size() == 1) {
    return std::format("{}, {}, {}", pa.BitWidth(), signed_lit, four_state_lit);
  }

  // Multi-dim: emit `{{l0, r0}, {l1, r1}, ...}, signed, four_state` for the
  // PackedArray initializer-list constructor. The runtime keeps the dim stack
  // so operator[] / Slice dispatch on outer-element bit width.
  std::string dim_list = "{";
  for (std::size_t i = 0; i < pa.dims.size(); ++i) {
    if (i != 0) dim_list += ", ";
    dim_list +=
        std::format("{{ {}LL, {}LL }}", pa.dims[i].left, pa.dims[i].right);
  }
  dim_list += "}";
  return std::format("{}, {}, {}", dim_list, signed_lit, four_state_lit);
}

auto RenderEnumClassName(const mir::Class& owner_class, mir::TypeId id)
    -> std::string {
  // First TypeAlias targeting `id` supplies the canonical class name. SV
  // identifiers are valid C++ identifiers, so the alias name flows directly.
  for (const auto& alias : owner_class.type_aliases) {
    if (alias.target == id) {
      return alias.name;
    }
  }
  return std::format("lyra_anon_enum_{}", id.value);
}

auto RenderTypeAsCpp(
    const mir::CompilationUnit& unit, const mir::Class& owner_class,
    mir::TypeId type_id) -> std::string {
  return std::visit(
      Overloaded{
          [](const mir::PackedArrayType&) -> std::string {
            return std::string{"lyra::value::PackedArray"};
          },
          [&](const mir::EnumType&) -> std::string {
            return RenderEnumClassName(owner_class, type_id);
          },
          [](const mir::StringType&) -> std::string {
            return std::string{"lyra::value::String"};
          },
          [](const mir::EventType&) -> std::string {
            return std::string{"lyra::runtime::NamedEvent"};
          },
          [](const mir::RealType&) -> std::string {
            return std::string{"lyra::value::Real"};
          },
          [](const mir::ShortRealType&) -> std::string {
            return std::string{"lyra::value::ShortReal"};
          },
          [](const mir::RealTimeType&) -> std::string {
            return std::string{"lyra::value::Real"};
          },
          [&](const mir::UnpackedArrayType& ua) -> std::string {
            return std::format(
                "lyra::value::UnpackedArray<{}>",
                RenderTypeAsCpp(unit, owner_class, ua.element_type));
          },
          [&](const mir::DynamicArrayType& da) -> std::string {
            return std::format(
                "lyra::value::DynamicArray<{}>",
                RenderTypeAsCpp(unit, owner_class, da.element_type));
          },
          [&](const mir::QueueType& q) -> std::string {
            return std::format(
                "lyra::value::Queue<{}>",
                RenderTypeAsCpp(unit, owner_class, q.element_type));
          },
          [&](const mir::AssociativeArrayType& a) -> std::string {
            return std::format(
                "lyra::value::AssociativeArray<{}, {}>",
                RenderTypeAsCpp(unit, owner_class, a.key_type),
                RenderTypeAsCpp(unit, owner_class, a.element_type));
          },
          [](const mir::WildcardIndexType&) -> std::string {
            return "lyra::value::WildcardKey";
          },
          [](const mir::ObjectType& o) -> std::string { return o.name; },
          [](const mir::ExternalUnitObjectType& e) -> std::string {
            return e.unit_name;
          },
          [](const mir::ScopeType&) -> std::string {
            return std::string{"lyra::runtime::Scope"};
          },
          [](const mir::ServicesType&) -> std::string {
            return std::string{"lyra::runtime::RuntimeServices&"};
          },
          [](const mir::FilesType&) -> std::string {
            return std::string{"lyra::runtime::FileTable&"};
          },
          [](const mir::DiagnosticType&) -> std::string {
            return std::string{"lyra::runtime::DiagnosticDispatcher&"};
          },
          [](const mir::RuntimeLibraryType& r) -> std::string {
            switch (r.kind) {
              case mir::RuntimeLibraryKind::kPrintItem:
                return std::string{"lyra::value::PrintItem"};
              case mir::RuntimeLibraryKind::kPrintLiteralItem:
                return std::string{"lyra::value::PrintLiteralItem"};
              case mir::RuntimeLibraryKind::kPrintValueItem:
                return std::string{"lyra::value::PrintValueItem"};
              case mir::RuntimeLibraryKind::kFormatSpec:
                return std::string{"lyra::value::FormatSpec"};
              case mir::RuntimeLibraryKind::kChannelCancellation:
                return std::string{"lyra::runtime::ChannelCancellation"};
              case mir::RuntimeLibraryKind::kTimeFormat:
                return std::string{"lyra::value::TimeFormat"};
              case mir::RuntimeLibraryKind::kHierarchySegment:
                return std::string{"lyra::runtime::HierarchySegment"};
            }
            throw InternalError("RenderTypeAsCpp: unknown RuntimeLibraryKind");
          },
          [&](const mir::CoroutineType& c) -> std::string {
            return std::format(
                "lyra::runtime::Coroutine<{}>",
                RenderTypeAsCpp(unit, owner_class, c.payload));
          },
          [&](const mir::RefType& r) -> std::string {
            std::string ref = std::format(
                "lyra::runtime::Ref<{}>",
                RenderTypeAsCpp(unit, owner_class, r.pointee));
            return r.is_const ? std::format("const {}", ref) : ref;
          },
          [](const mir::VoidType&) -> std::string {
            return std::string{"void"};
          },
          [&](const mir::PointerType& p) -> std::string {
            std::string inner = RenderTypeAsCpp(unit, owner_class, p.pointee);
            switch (p.ownership) {
              case mir::PointerOwnership::kUnique:
                return std::format("std::unique_ptr<{}>", inner);
              case mir::PointerOwnership::kShared:
                return std::format("std::shared_ptr<{}>", inner);
              case mir::PointerOwnership::kBorrowed:
                // A borrowed pointer refers to the pointee's storage cell --
                // a `Var<T>` if the pointee is an observable wrapper, the
                // bare type otherwise -- so the slot mirrors what it points
                // at by recursing.
                return std::format("{}*", inner);
            }
            throw InternalError("RenderTypeAsCpp: unknown PointerOwnership");
          },
          [&](const mir::VectorType& v) -> std::string {
            return std::format(
                "std::vector<{}>",
                RenderTypeAsCpp(unit, owner_class, v.element));
          },
          [&](const mir::TupleType& t) -> std::string {
            std::string inners;
            for (std::size_t i = 0; i < t.elements.size(); ++i) {
              if (i != 0) inners += ", ";
              inners += RenderTypeAsCpp(unit, owner_class, t.elements[i]);
            }
            return std::format("lyra::value::Tuple<{}>", inners);
          },
          [&](const mir::ExternalRefType& e) -> std::string {
            return std::format(
                "lyra::runtime::ExternUp<{}>",
                RenderTypeAsCpp(unit, owner_class, e.element));
          },
          [&](const mir::ObservableType& o) -> std::string {
            return std::format(
                "lyra::runtime::Var<{}>",
                RenderTypeAsCpp(unit, owner_class, o.value));
          },
          [](const auto&) -> std::string {
            throw InternalError(
                "RenderTypeAsCpp: unsupported MIR type for current C++ render "
                "cut");
          },
      },
      unit.GetType(type_id).data);
}

}  // namespace lyra::backend::cpp
