#include "lyra/mir/frame_size.hpp"

#include <algorithm>
#include <cstdint>
#include <ranges>
#include <span>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/runtime_object.hpp"
#include "lyra/value/chandle.hpp"
#include "lyra/value/dpi_canonical.hpp"
#include "lyra/value/dpi_open_array.hpp"
#include "lyra/value/empty.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/frame_bound.hpp"
#include "lyra/value/index_order.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/layout.hpp"
#include "lyra/value/object_ref.hpp"
#include "lyra/value/real.hpp"
#include "lyra/value/runtime_associative_array.hpp"
#include "lyra/value/runtime_dynamic_array.hpp"
#include "lyra/value/runtime_queue.hpp"
#include "lyra/value/string.hpp"

namespace lyra::mir {

namespace {

template <typename T>
constexpr auto LayoutOfClass() -> value::Layout {
  return value::Layout{.size = sizeof(T), .align = alignof(T)};
}

constexpr value::Layout kAddress = LayoutOfClass<void*>();

auto LayoutOfIntegral(const IntegralType& integral) -> value::Layout {
  return value::Layout{
      .size = value::IntegralBytesFor(
          integral.bit_width, StateDomainOf(integral.state_kind)),
      .align = value::IntegralAlignFor(integral.bit_width)};
}

auto LayoutStated(const support::ObjectLayout& stated) -> value::Layout {
  return value::Layout{.size = stated.size, .align = stated.align};
}

// An object of the library's: the class it is, where that class is the value
// library's, and the storage stated for it where the runtime defines it and a
// frame holds one by value. Every other class of the runtime's is reached by
// address, and is counted as the word a frame names one by.
auto LayoutOfLibraryObject(RuntimeLibraryKind kind) -> value::Layout {
  switch (kind) {
    case RuntimeLibraryKind::kPrintItem:
      return LayoutOfClass<value::PrintItem>();
    case RuntimeLibraryKind::kPrintLiteralItem:
      return LayoutOfClass<value::PrintLiteralItem>();
    case RuntimeLibraryKind::kPrintValueItem:
      return LayoutOfClass<value::PrintValueItem>();
    case RuntimeLibraryKind::kFormatSpec:
      return LayoutOfClass<value::FormatSpec>();
    case RuntimeLibraryKind::kFormatArg:
      return LayoutOfClass<value::FormatArg>();
    case RuntimeLibraryKind::kTimeFormat:
      return LayoutOfClass<value::TimeFormat>();
    case RuntimeLibraryKind::kDpiBitBuffer:
      return LayoutOfClass<value::DpiBitBuffer>();
    case RuntimeLibraryKind::kDpiLogicBuffer:
      return LayoutOfClass<value::DpiLogicBuffer>();
    case RuntimeLibraryKind::kDpiOpenArray:
      return LayoutOfClass<value::DpiOpenArray>();
    // A table the unit states once and every use names.
    case RuntimeLibraryKind::kEnumeration:
      return kAddress;
    case RuntimeLibraryKind::kTrigger:
      return LayoutStated(support::kTriggerStorage);
    case RuntimeLibraryKind::kObservation:
      return LayoutStated(support::kObservationStorage);
    case RuntimeLibraryKind::kReadReport:
      return LayoutStated(support::kReadReportStorage);
    case RuntimeLibraryKind::kWait:
      return LayoutStated(support::kWaitStorage);
    case RuntimeLibraryKind::kHierarchySegment:
      return LayoutStated(support::kHierarchySegmentStorage);
    case RuntimeLibraryKind::kChannelCancellation:
      return LayoutStated(support::kChannelCancellationStorage);
    case RuntimeLibraryKind::kDpiBitChunk:
    case RuntimeLibraryKind::kDpiLogicChunk:
    case RuntimeLibraryKind::kDpiOpenArrayHandle:
    case RuntimeLibraryKind::kCancellationTarget:
    case RuntimeLibraryKind::kControlEffect:
    case RuntimeLibraryKind::kObjectDefinition:
    case RuntimeLibraryKind::kScopeInfo:
    case RuntimeLibraryKind::kScopeCallable:
      return kAddress;
  }
  throw InternalError("mir: unknown runtime library kind");
}

auto LayoutOf(const CompilationUnit& unit, TypeId type) -> value::Layout;

// Values one after another, each at its alignment.
auto Sequence(const CompilationUnit& unit, TypeId element, std::uint64_t count)
    -> value::Layout {
  return value::SequenceLayoutOf(LayoutOf(unit, element), count).whole;
}

// Values side by side, as a C++ struct lays its members out.
auto Product(const CompilationUnit& unit, std::span<const TypeId> parts)
    -> value::Layout {
  return value::ProductLayoutOf(parts | std::views::transform([&](TypeId part) {
                                  return LayoutOf(unit, part);
                                }))
      .whole;
}

// Values sharing one storage, each at its start.
auto Overlay(const CompilationUnit& unit, std::span<const TypeId> members)
    -> value::Layout {
  return value::OverlayLayoutOf(
      members | std::views::transform(
                    [&](TypeId member) { return LayoutOf(unit, member); }));
}

// A value laid out by what it holds is that; a value of the library's is the
// class the library keeps it as; a machine value is what a register holding it
// holds. Storage the library keeps -- a cell, a write in progress, an object of
// the hierarchy -- is never a part of a value, and keeps a value too large for
// a frame in storage of its own, as every holder does, so it is never too large
// for one either; it is counted as the handle a frame names it by.
auto LayoutOf(const CompilationUnit& unit, TypeId type) -> value::Layout {
  const auto handle = [](const auto&) { return kAddress; };
  return unit.types.Get(type).Visit(
      Overloaded{
          [&](const IntegralType& t) { return LayoutOfIntegral(t); },
          [&](const EnumType& t) { return LayoutOfIntegral(t.base); },
          [&](const UnpackedArrayType& t) {
            return Sequence(unit, t.element_type, t.Size());
          },
          [&](const MachineArrayType& t) {
            return Sequence(unit, t.element, t.size);
          },
          [&](const TupleType& t) { return Product(unit, t.elements); },
          [&](const StructType& t) {
            return Product(unit, StructElements(unit, t));
          },
          [&](const ClosureType& t) {
            std::vector<TypeId> captures;
            for (const FieldDecl& field :
                 unit.GetClosure(t.closure_id).fields) {
              captures.push_back(field.type);
            }
            return Product(unit, captures);
          },
          [&](const UnionType& t) { return Overlay(unit, t.members); },
          [&](const TaggedUnionType& t) {
            return value::TaggedUnionLayoutOf(
                       Overlay(unit, t.members), t.members.size())
                .whole;
          },
          [](const DynamicArrayType&) {
            return LayoutOfClass<value::RuntimeDynamicArray>();
          },
          [](const QueueType&) { return LayoutOfClass<value::RuntimeQueue>(); },
          [](const AssociativeArrayType&) {
            return LayoutOfClass<value::RuntimeAssociativeArray>();
          },
          [](const WildcardIndexType&) {
            return LayoutOfClass<value::WildcardKey>();
          },
          [](const StringType&) { return LayoutOfClass<value::String>(); },
          [](const RealType&) { return LayoutOfClass<value::Real>(); },
          [](const ShortRealType&) {
            return LayoutOfClass<value::ShortReal>();
          },
          [](const ChandleType&) { return LayoutOfClass<value::Chandle>(); },
          [](const ManagedRefType&) {
            return LayoutOfClass<value::ObjectRef>();
          },
          [](const EmptyType&) { return LayoutOfClass<value::Empty>(); },
          [](const VoidType&) { return value::Layout{.size = 0, .align = 1}; },
          [](const MachineBoolType&) { return LayoutOfClass<bool>(); },
          [](const MachineIntType& t) {
            const std::uint64_t bytes = BitsOf(t.width) / 8;
            return value::Layout{.size = bytes, .align = bytes};
          },
          [](const MachineFloatType& t) {
            const std::uint64_t bytes = BitsOf(t.width) / 8;
            return value::Layout{.size = bytes, .align = bytes};
          },
          [&](const MachineCStringType& t) { return handle(t); },
          [&](const MachineFunctionType& t) { return handle(t); },
          [&](const PointerType& t) { return handle(t); },
          [&](const ReferenceType& t) { return handle(t); },
          [&](const VariableRefType& t) { return handle(t); },
          [&](const VectorType& t) { return handle(t); },
          [&](const CoroutineType& t) { return handle(t); },
          [&](const DriverType& t) { return handle(t); },
          [&](const EventType& t) { return handle(t); },
          [&](const ObjectType& t) { return handle(t); },
          [&](const RuntimeClassType& t) { return handle(t); },
          [&](const RuntimeEffectsType& t) { return handle(t); },
          [&](const FilesType& t) { return handle(t); },
          [&](const DiagnosticType& t) { return handle(t); },
          [](const RuntimeLibraryType& t) {
            return LayoutOfLibraryObject(t.kind);
          },
          [&](const ObservableType& t) { return handle(t); },
          [&](const ResolvedType& t) { return handle(t); },
          [&](const OpenWriteType& t) { return handle(t); },
          [&](const DesignationType& t) { return handle(t); },
          [&](const BitsReferenceType& t) { return handle(t); },
          [&](const ObjectWriteType& t) { return handle(t); },
          [&](const SampledHistoryType& t) { return handle(t); },
          [&](const EvaluationAttemptsType& t) { return handle(t); }});
}

}  // namespace

auto IsTooLargeForAFrame(const CompilationUnit& unit, TypeId type) -> bool {
  return LayoutOf(unit, type).size > value::kMaxInlineBytes;
}

}  // namespace lyra::mir
