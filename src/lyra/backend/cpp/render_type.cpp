#include "lyra/backend/cpp/render_type.hpp"

#include <cstddef>
#include <format>
#include <string>
#include <utility>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/structural_scope.hpp"
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
    return std::to_string(pa.BitWidth()) + ", " + signed_lit + ", " +
           four_state_lit;
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
  return dim_list + ", " + signed_lit + ", " + four_state_lit;
}

auto IsObservableScalarType(const mir::Type& ty) -> bool {
  // An ExternalRef member is a Var-like observable: it forwards reads, writes,
  // and subscription to its resolved cell, so a write routes through Set.
  return ty.IsIntegralPacked() ||
         std::holds_alternative<mir::ExternalRefType>(ty.data);
}

auto RenderEnumClassName(
    const mir::StructuralScope& owner_scope, mir::TypeId id) -> std::string {
  // First TypeAlias targeting `id` supplies the canonical class name. SV
  // identifiers are valid C++ identifiers, so the alias name flows directly.
  for (const auto& alias : owner_scope.type_aliases) {
    if (alias.target == id) {
      return alias.name;
    }
  }
  return std::format("lyra_anon_enum_{}", id.value);
}

auto RenderTypeAsCpp(
    const mir::CompilationUnit& unit, const mir::StructuralScope& owner_scope,
    mir::TypeId type_id) -> diag::Result<std::string> {
  return std::visit(
      Overloaded{
          [](const mir::PackedArrayType&) -> diag::Result<std::string> {
            return std::string{"lyra::value::PackedArray"};
          },
          [&](const mir::EnumType&) -> diag::Result<std::string> {
            return RenderEnumClassName(owner_scope, type_id);
          },
          [](const mir::StringType&) -> diag::Result<std::string> {
            return std::string{"lyra::value::String"};
          },
          [](const mir::EventType&) -> diag::Result<std::string> {
            return std::string{"lyra::runtime::NamedEvent"};
          },
          [](const mir::RealType&) -> diag::Result<std::string> {
            return std::string{"double"};
          },
          [](const mir::ShortRealType&) -> diag::Result<std::string> {
            return std::string{"float"};
          },
          [](const mir::RealTimeType&) -> diag::Result<std::string> {
            return std::string{"double"};
          },
          [&](const mir::UnpackedArrayType& ua) -> diag::Result<std::string> {
            auto inner_or = RenderTypeAsCpp(unit, owner_scope, ua.element_type);
            if (!inner_or) return std::unexpected(std::move(inner_or.error()));
            return "lyra::value::UnpackedArray<" + *inner_or + ">";
          },
          [&](const mir::DynamicArrayType& da) -> diag::Result<std::string> {
            auto inner_or = RenderTypeAsCpp(unit, owner_scope, da.element_type);
            if (!inner_or) return std::unexpected(std::move(inner_or.error()));
            return "lyra::value::DynamicArray<" + *inner_or + ">";
          },
          [&](const mir::QueueType& q) -> diag::Result<std::string> {
            auto inner_or = RenderTypeAsCpp(unit, owner_scope, q.element_type);
            if (!inner_or) return std::unexpected(std::move(inner_or.error()));
            return "lyra::value::Queue<" + *inner_or + ">";
          },
          [&](const mir::AssociativeArrayType& a) -> diag::Result<std::string> {
            if (!a.key_type.has_value()) {
              throw InternalError(
                  "RenderTypeAsCpp: associative array with a wildcard index "
                  "type reached the backend; AST -> HIR rejects it");
            }
            auto key_or = RenderTypeAsCpp(unit, owner_scope, *a.key_type);
            if (!key_or) return std::unexpected(std::move(key_or.error()));
            auto elem_or = RenderTypeAsCpp(unit, owner_scope, a.element_type);
            if (!elem_or) return std::unexpected(std::move(elem_or.error()));
            return "lyra::value::AssociativeArray<" + *key_or + ", " +
                   *elem_or + ">";
          },
          [&](const mir::ObjectType& o) -> diag::Result<std::string> {
            return std::string{
                owner_scope.GetChildStructuralScope(o.target).name};
          },
          [](const mir::ExternalUnitObjectType& e)
              -> diag::Result<std::string> { return e.unit_name; },
          [](const mir::ScopeType&) -> diag::Result<std::string> {
            return std::string{"lyra::runtime::Scope"};
          },
          [&](const mir::SelfType&) -> diag::Result<std::string> {
            return std::string{owner_scope.name};
          },
          [&](const mir::PointerType& p) -> diag::Result<std::string> {
            auto inner_or = RenderTypeAsCpp(unit, owner_scope, p.pointee);
            if (!inner_or) return std::unexpected(std::move(inner_or.error()));
            switch (p.ownership) {
              case mir::PointerOwnership::kUnique:
                return "std::unique_ptr<" + *inner_or + ">";
              case mir::PointerOwnership::kShared:
                return "std::shared_ptr<" + *inner_or + ">";
              case mir::PointerOwnership::kBorrowed: {
                // A borrowed pointer refers to the pointee's storage cell: a
                // `Var<T>` for an observable scalar, the bare type otherwise
                // (e.g. a `Scope`), so the slot mirrors what it points at.
                const std::string storage =
                    IsObservableScalarType(unit.GetType(p.pointee))
                        ? "lyra::runtime::Var<" + *inner_or + ">"
                        : *inner_or;
                return storage + "*";
              }
            }
            throw InternalError("RenderTypeAsCpp: unknown PointerOwnership");
          },
          [&](const mir::VectorType& v) -> diag::Result<std::string> {
            auto inner_or = RenderTypeAsCpp(unit, owner_scope, v.element);
            if (!inner_or) return std::unexpected(std::move(inner_or.error()));
            return "std::vector<" + *inner_or + ">";
          },
          [&](const mir::ExternalRefType& e) -> diag::Result<std::string> {
            auto inner_or = RenderTypeAsCpp(unit, owner_scope, e.element);
            if (!inner_or) return std::unexpected(std::move(inner_or.error()));
            return "lyra::runtime::ExternUp<" + *inner_or + ">";
          },
          [](const auto&) -> diag::Result<std::string> {
            throw InternalError(
                "RenderTypeAsCpp: unsupported MIR type for current C++ render "
                "cut");
          },
      },
      unit.GetType(type_id).data);
}

}  // namespace lyra::backend::cpp
