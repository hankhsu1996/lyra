#include "lyra/backend/cpp/render_type.hpp"

#include <cstddef>
#include <format>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include "lyra/backend/cpp/formatting.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::backend::cpp {

// The runtime resolver realizing one net resolution (LRM 6.6). The policy is a
// type parameter of the runtime's net and driver, so each resolution names its
// own; the tri-state fold is the one `wire` and `tri` share.
auto NetResolverCppName(mir::NetResolution resolution) -> std::string_view {
  switch (resolution) {
    case mir::NetResolution::kTriState:
      return "lyra::runtime::WireResolver";
  }
  throw InternalError("NetResolverCppName: unknown NetResolution");
}

auto RenderTypeAsCpp(const mir::CompilationUnit& unit, mir::TypeId type_id)
    -> std::string {
  return std::visit(
      Overloaded{
          [](const mir::PackedArrayType&) -> std::string {
            return std::string{"lyra::value::PackedArray"};
          },
          [](const mir::EnumType&) -> std::string {
            // An enum value is its base integral -- a `PackedArray`. The enum's
            // nominal content is consumed at HIR-to-MIR, never emitted as a
            // type.
            return std::string{"lyra::value::PackedArray"};
          },
          [](const mir::StringType&) -> std::string {
            return std::string{"lyra::value::String"};
          },
          [](const mir::MachineCStringType&) -> std::string {
            return std::string{"const char*"};
          },
          [](const mir::MachineBoolType&) -> std::string {
            return std::string{"bool"};
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
          [](const mir::MachineFloatType& m) -> std::string {
            switch (m.bit_width) {
              case 32:
                return std::string{"float"};
              case 64:
                return std::string{"double"};
              default:
                throw InternalError(
                    "RenderTypeAsCpp: unsupported MachineFloatType width");
            }
          },
          [&](const mir::MachineArrayType& m) -> std::string {
            return std::format(
                "std::array<{}, {}>", RenderTypeAsCpp(unit, m.element), m.size);
          },
          [&](const mir::MachineFunctionType& m) -> std::string {
            std::string params;
            for (std::size_t i = 0; i < m.params.size(); ++i) {
              if (i != 0) params += ", ";
              params += RenderTypeAsCpp(unit, m.params[i]);
            }
            return std::format(
                "{} (*)({})", RenderTypeAsCpp(unit, m.result), params);
          },
          [](const mir::ChandleType&) -> std::string {
            return std::string{"lyra::value::Chandle"};
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
                RenderTypeAsCpp(unit, ua.element_type));
          },
          [&](const mir::DynamicArrayType& da) -> std::string {
            return std::format(
                "lyra::value::DynamicArray<{}>",
                RenderTypeAsCpp(unit, da.element_type));
          },
          [&](const mir::QueueType& q) -> std::string {
            return std::format(
                "lyra::value::Queue<{}>",
                RenderTypeAsCpp(unit, q.element_type));
          },
          [&](const mir::AssociativeArrayType& a) -> std::string {
            return std::format(
                "lyra::value::AssociativeArray<{}, {}>",
                RenderTypeAsCpp(unit, a.key_type),
                RenderTypeAsCpp(unit, a.element_type));
          },
          [](const mir::WildcardIndexType&) -> std::string {
            return "lyra::value::WildcardKey";
          },
          [&unit](const mir::ObjectType& o) -> std::string {
            return ToCppName(unit.GetClass(o.class_id).name);
          },
          [&unit](const mir::StructType& s) -> std::string {
            return unit.GetStruct(s.struct_id).name;
          },
          [&unit](const mir::ExternalUnitObjectType& e) -> std::string {
            // A unit's emitted peer is a namespace, and the class it publishes
            // its instances as sits inside it.
            const mir::ExternalUnitObject& object =
                unit.external_unit_objects.Get(e.object);
            return std::format(
                "{}::{}", ToCppName(object.unit_name),
                ToCppName(object.class_name));
          },
          [](const mir::CrossUnitClassType& e) -> std::string {
            // A unit's emitted peer is a namespace, and the class it declares
            // sits inside it.
            return std::format(
                "{}::{}", ToCppName(e.unit_name), ToCppName(e.class_name));
          },
          [](const mir::RuntimeClassType& e) -> std::string {
            return e.symbol;
          },
          [](const mir::RuntimeEffectsType&) -> std::string {
            return std::string{"lyra::runtime::RuntimeEffects&"};
          },
          [](const mir::FilesType&) -> std::string {
            return std::string{"lyra::runtime::FileTable&"};
          },
          [](const mir::DiagnosticType&) -> std::string {
            return std::string{"lyra::runtime::DiagnosticDispatcher&"};
          },
          [](const mir::RuntimeLibraryType& r) -> std::string {
            switch (r.kind) {
              case mir::RuntimeLibraryKind::kPackedType:
                return std::string{"lyra::value::PackedType"};
              case mir::RuntimeLibraryKind::kPackedRange:
                return std::string{"lyra::value::PackedRange"};
              case mir::RuntimeLibraryKind::kPrintItem:
                return std::string{"lyra::value::PrintItem"};
              case mir::RuntimeLibraryKind::kPrintLiteralItem:
                return std::string{"lyra::value::PrintLiteralItem"};
              case mir::RuntimeLibraryKind::kPrintValueItem:
                return std::string{"lyra::value::PrintValueItem"};
              case mir::RuntimeLibraryKind::kCancellationTarget:
                return std::string{"lyra::runtime::CancellationTarget"};
              case mir::RuntimeLibraryKind::kControlEffect:
                return std::string{"lyra::runtime::ControlEffect"};
              case mir::RuntimeLibraryKind::kFormatSpec:
                return std::string{"lyra::value::FormatSpec"};
              case mir::RuntimeLibraryKind::kFormatArg:
                return std::string{"lyra::value::FormatArg"};
              case mir::RuntimeLibraryKind::kChannelCancellation:
                return std::string{"lyra::runtime::ChannelCancellation"};
              case mir::RuntimeLibraryKind::kTimeFormat:
                return std::string{"lyra::value::TimeFormat"};
              case mir::RuntimeLibraryKind::kHierarchySegment:
                return std::string{"lyra::runtime::HierarchySegment"};
              case mir::RuntimeLibraryKind::kTrigger:
                return std::string{"lyra::runtime::Trigger"};
              case mir::RuntimeLibraryKind::kScopeProgram:
                return std::string{"lyra::runtime::ScopeProgram"};
              case mir::RuntimeLibraryKind::kScopeExport:
                return std::string{"lyra::runtime::ScopeExport"};
              case mir::RuntimeLibraryKind::kScopeExportTable:
                return std::string{"lyra::runtime::ScopeExportTable"};
              case mir::RuntimeLibraryKind::kScopeDefinition:
                return std::string{"lyra::runtime::ScopeDefinition"};
              case mir::RuntimeLibraryKind::kScopeMetadata:
                return std::string{"lyra::runtime::ScopeMetadata"};
              case mir::RuntimeLibraryKind::kAbiStringRef:
                return std::string{"lyra::runtime::AbiStringRef"};
              case mir::RuntimeLibraryKind::kDpiBitBuffer:
                return std::string{"lyra::value::DpiBitBuffer"};
              case mir::RuntimeLibraryKind::kDpiLogicBuffer:
                return std::string{"lyra::value::DpiLogicBuffer"};
              case mir::RuntimeLibraryKind::kDpiBitChunk:
                return std::string{"svBitVecVal"};
              case mir::RuntimeLibraryKind::kDpiLogicChunk:
                return std::string{"svLogicVecVal"};
              case mir::RuntimeLibraryKind::kDpiOpenArray:
                return std::string{"lyra::value::DpiOpenArray"};
              case mir::RuntimeLibraryKind::kDpiOpenArrayHandle:
                return std::string{"const svOpenArrayHandle"};
              case mir::RuntimeLibraryKind::kDpiScopeGuard:
                return std::string{"lyra::runtime::DpiScopeGuard"};
              case mir::RuntimeLibraryKind::kForeignTaskAwaitable:
                return std::string{"lyra::runtime::ForeignTaskAwaitable"};
            }
            throw InternalError("RenderTypeAsCpp: unknown RuntimeLibraryKind");
          },
          [&](const mir::CoroutineType& c) -> std::string {
            return std::format(
                "lyra::runtime::Coroutine<{}>",
                RenderTypeAsCpp(unit, c.payload));
          },
          [&](const mir::RefType& r) -> std::string {
            std::string ref = std::format(
                "lyra::runtime::Ref<{}>", RenderTypeAsCpp(unit, r.pointee));
            return r.mutability == mir::Mutability::kReadOnly
                       ? std::format("const {}", ref)
                       : ref;
          },
          [](const mir::VoidType&) -> std::string {
            return std::string{"void"};
          },
          [&](const mir::PointerType& p) -> std::string {
            std::string inner = RenderTypeAsCpp(unit, p.pointee);
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
                "lyra::runtime::GcRef<{}>", RenderTypeAsCpp(unit, m.pointee));
          },
          [&](const mir::VectorType& v) -> std::string {
            return std::format(
                "std::vector<{}>", RenderTypeAsCpp(unit, v.element));
          },
          [&](const mir::TupleType& t) -> std::string {
            std::string inners;
            for (std::size_t i = 0; i < t.elements.size(); ++i) {
              if (i != 0) inners += ", ";
              inners += RenderTypeAsCpp(unit, t.elements[i]);
            }
            return std::format("lyra::value::Tuple<{}>", inners);
          },
          [&](const mir::UnionType& u) -> std::string {
            std::string inners;
            for (std::size_t i = 0; i < u.elements.size(); ++i) {
              if (i != 0) inners += ", ";
              inners += RenderTypeAsCpp(unit, u.elements[i]);
            }
            return std::format("lyra::value::Union<{}>", inners);
          },
          [](const mir::EmptyType&) -> std::string {
            return std::string{"lyra::value::Empty"};
          },
          [&](const mir::TaggedUnionType& u) -> std::string {
            std::string inners;
            for (std::size_t i = 0; i < u.elements.size(); ++i) {
              if (i != 0) inners += ", ";
              inners += RenderTypeAsCpp(unit, u.elements[i]);
            }
            return std::format("lyra::value::TaggedUnion<{}>", inners);
          },
          [&](const mir::ObservableType& o) -> std::string {
            return std::format(
                "lyra::runtime::Var<{}>", RenderTypeAsCpp(unit, o.value));
          },
          [&](const mir::ResolvedType& r) -> std::string {
            return std::format(
                "lyra::runtime::ResolvedNet<{}, {}>",
                RenderTypeAsCpp(unit, r.value),
                NetResolverCppName(r.resolution));
          },
          [&](const mir::DriverType& d) -> std::string {
            return std::format(
                "lyra::runtime::Driver<{}, {}>", RenderTypeAsCpp(unit, d.value),
                NetResolverCppName(d.resolution));
          },
          [](const auto&) -> std::string {
            throw InternalError(
                "RenderTypeAsCpp: MIR type not yet supported in the C++ "
                "backend");
          },
      },
      unit.types.Get(type_id).data);
}

auto RenderTypeConstructionAsCpp(
    const mir::CompilationUnit& unit, mir::TypeId type_id) -> std::string {
  return std::visit(
      Overloaded{
          // A wrapper that owns what it points at brings the pointee into
          // existence along with itself, so what names its construction is the
          // entry that allocates and constructs together rather than the
          // wrapper's own spelling. A borrowed pointer owns nothing and is
          // bound to storage that already exists, so nothing constructs one.
          [&](const mir::PointerType& p) -> std::string {
            const std::string inner = RenderTypeAsCpp(unit, p.pointee);
            switch (p.ownership) {
              case mir::PointerOwnership::kUnique:
                return std::format("std::make_unique<{}>", inner);
              case mir::PointerOwnership::kShared:
                return std::format("std::make_shared<{}>", inner);
              case mir::PointerOwnership::kBorrowed:
                throw InternalError(
                    "RenderTypeConstructionAsCpp: a borrowed pointer is bound "
                    "to storage that already exists, so none is constructed");
            }
            throw InternalError(
                "RenderTypeConstructionAsCpp: unknown PointerOwnership");
          },
          [&](const mir::ManagedRefType& m) -> std::string {
            return std::format(
                "lyra::runtime::GcNew<{}>", RenderTypeAsCpp(unit, m.pointee));
          },
          // Every other type is built by naming itself.
          [&](const auto&) -> std::string {
            return RenderTypeAsCpp(unit, type_id);
          }},
      unit.types.Get(type_id).data);
}

auto RenderClassRefAsCpp(
    const mir::CompilationUnit& unit, const mir::ClassRef& ref) -> std::string {
  return std::visit(
      Overloaded{
          [&unit](const mir::IntraUnitClassRef& i) -> std::string {
            return ToCppName(unit.GetClass(i.class_id).name);
          },
          [](const mir::CrossUnitClassRef& e) -> std::string {
            return std::format(
                "{}::{}", ToCppName(e.unit_name), ToCppName(e.class_name));
          },
          [](const mir::RuntimeClassRef& e) -> std::string {
            return e.symbol;
          }},
      ref);
}

}  // namespace lyra::backend::cpp
