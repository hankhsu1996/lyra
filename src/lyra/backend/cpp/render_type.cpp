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

auto RenderPackedType(const mir::PackedArrayType& pa) -> std::string {
  const char* signed_lit =
      pa.signedness == mir::Signedness::kSigned ? "true" : "false";
  const char* four_state_lit = pa.atom != mir::BitAtom::kBit ? "true" : "false";

  // The shape is one `PackedType` descriptor: the dimension stack plus
  // signedness and state domain. A one-dimensional type is a one-element stack,
  // not a scalar special case; the runtime keeps the stack so operator[] /
  // Slice dispatch on outer-element bit width at any rank.
  std::string dim_list = "{";
  for (std::size_t i = 0; i < pa.dims.size(); ++i) {
    if (i != 0) dim_list += ", ";
    dim_list +=
        std::format("{{ {}LL, {}LL }}", pa.dims[i].left, pa.dims[i].right);
  }
  dim_list += "}";
  return std::format(
      "lyra::value::PackedType{{{}, {}, {}}}", dim_list, signed_lit,
      four_state_lit);
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
          [](const mir::StringViewType&) -> std::string {
            return std::string{"std::string_view"};
          },
          [](const mir::MachineIntType& m) -> std::string {
            const std::string_view sign =
                m.signedness == mir::Signedness::kSigned ? "" : "u";
            switch (m.bit_width) {
              case 8:
              case 16:
              case 32:
              case 64:
                return std::format("std::{}int{}_t", sign, m.bit_width);
              default:
                throw InternalError(
                    "RenderTypeAsCpp: unsupported MachineIntType width");
            }
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
          [&unit](const mir::ObjectType& o) -> std::string {
            return unit.GetClass(o.class_id).name;
          },
          [&unit](const mir::StructType& s) -> std::string {
            return unit.GetStruct(s.struct_id).name;
          },
          [](const mir::ExternalUnitObjectType& e) -> std::string {
            return e.unit_name;
          },
          [](const mir::ScopeType&) -> std::string {
            return std::string{"lyra::runtime::Scope"};
          },
          [](const mir::InstanceType&) -> std::string {
            return std::string{"lyra::runtime::Instance"};
          },
          [](const mir::GenScopeType&) -> std::string {
            return std::string{"lyra::runtime::GenScope"};
          },
          [](const mir::ProceduralStorageScopeType&) -> std::string {
            return std::string{"lyra::runtime::ProceduralStorageScope"};
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
              case mir::RuntimeLibraryKind::kScopeProgram:
                return std::string{"lyra::runtime::ScopeProgram"};
              case mir::RuntimeLibraryKind::kUnitDefinition:
                return std::string{"lyra::runtime::UnitDefinition"};
              case mir::RuntimeLibraryKind::kScopeMetadata:
                return std::string{"lyra::runtime::ScopeMetadata"};
              case mir::RuntimeLibraryKind::kAbiStringRef:
                return std::string{"lyra::runtime::AbiStringRef"};
              case mir::RuntimeLibraryKind::kScopeEntry:
                return std::string{"lyra::runtime::ScopeEntry"};
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
            return r.mutability == mir::Mutability::kReadOnly
                       ? std::format("const {}", ref)
                       : ref;
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
                // at by recursing. A read-only borrow grants no write
                // capability (`const T*`), the immutable-receiver case.
                return std::format(
                    "{}{}*",
                    p.mutability == mir::Mutability::kReadOnly ? "const " : "",
                    inner);
            }
            throw InternalError("RenderTypeAsCpp: unknown PointerOwnership");
          },
          [&](const mir::ManagedRefType& m) -> std::string {
            return std::format(
                "lyra::runtime::GcRef<{}>",
                RenderTypeAsCpp(unit, owner_class, m.pointee));
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
          [&](const mir::UnionType& u) -> std::string {
            std::string inners;
            for (std::size_t i = 0; i < u.elements.size(); ++i) {
              if (i != 0) inners += ", ";
              inners += RenderTypeAsCpp(unit, owner_class, u.elements[i]);
            }
            return std::format("lyra::value::Union<{}>", inners);
          },
          [&](const mir::ObservableType& o) -> std::string {
            return std::format(
                "lyra::runtime::Var<{}>",
                RenderTypeAsCpp(unit, owner_class, o.value));
          },
          [&](const mir::ResolvedType& r) -> std::string {
            return std::format(
                "lyra::runtime::ResolvedNet<{}, lyra::runtime::WireResolver>",
                RenderTypeAsCpp(unit, owner_class, r.value));
          },
          [&](const mir::DriverType& d) -> std::string {
            return std::format(
                "lyra::runtime::Driver<{}, lyra::runtime::WireResolver>",
                RenderTypeAsCpp(unit, owner_class, d.value));
          },
          [](const auto&) -> std::string {
            throw InternalError(
                "RenderTypeAsCpp: MIR type not yet supported in C++ render "
                "cut");
          },
      },
      unit.types.Get(type_id).data);
}

}  // namespace lyra::backend::cpp
