#include "lyra/mir/frame_size.hpp"

#include <algorithm>
#include <cstdint>
#include <span>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/value/chandle.hpp"
#include "lyra/value/dpi_canonical.hpp"
#include "lyra/value/dpi_open_array.hpp"
#include "lyra/value/empty.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/frame_bound.hpp"
#include "lyra/value/index_order.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/object_ref.hpp"
#include "lyra/value/real.hpp"
#include "lyra/value/runtime_associative_array.hpp"
#include "lyra/value/runtime_dynamic_array.hpp"
#include "lyra/value/runtime_queue.hpp"
#include "lyra/value/string.hpp"

namespace lyra::mir {

namespace {

// How many bytes a value of a type takes and to what it is aligned.
struct Layout {
  std::uint64_t size;
  std::uint64_t align;
};

template <typename T>
constexpr auto LayoutOfClass() -> Layout {
  return Layout{.size = sizeof(T), .align = alignof(T)};
}

constexpr Layout kAddress = LayoutOfClass<void*>();

auto LayoutOfIntegral(const IntegralType& integral) -> Layout {
  return Layout{
      .size = value::IntegralBytesFor(
          integral.bit_width, StateDomainOf(integral.state_kind)),
      .align = value::IntegralAlignFor(integral.bit_width)};
}

// An object of the library's: the class it is, where that class is the value
// library's. A class of the runtime's is one MIR is compiled without, and is
// counted as the word a frame names one by.
auto LayoutOfLibraryObject(RuntimeLibraryKind kind) -> Layout {
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
    case RuntimeLibraryKind::kDpiBitChunk:
    case RuntimeLibraryKind::kDpiLogicChunk:
    case RuntimeLibraryKind::kDpiOpenArrayHandle:
    case RuntimeLibraryKind::kCancellationTarget:
    case RuntimeLibraryKind::kControlEffect:
    case RuntimeLibraryKind::kChannelCancellation:
    case RuntimeLibraryKind::kHierarchySegment:
    case RuntimeLibraryKind::kTrigger:
    case RuntimeLibraryKind::kObservation:
    case RuntimeLibraryKind::kReadReport:
    case RuntimeLibraryKind::kWait:
    case RuntimeLibraryKind::kObjectDefinition:
    case RuntimeLibraryKind::kScopeInfo:
    case RuntimeLibraryKind::kScopeCallable:
      return kAddress;
  }
  throw InternalError("mir: unknown runtime library kind");
}

auto LayoutOf(const CompilationUnit& unit, TypeId type) -> Layout;

// Values one after another, each at its alignment.
auto Sequence(const CompilationUnit& unit, TypeId element, std::uint64_t count)
    -> Layout {
  const Layout one = LayoutOf(unit, element);
  const std::uint64_t stride =
      (one.size + one.align - 1) / one.align * one.align;
  return Layout{.size = stride * count, .align = one.align};
}

// Values side by side, as a C++ struct lays its members out.
auto Product(const CompilationUnit& unit, std::span<const TypeId> parts)
    -> Layout {
  Layout whole{.size = 0, .align = 1};
  for (const TypeId component : parts) {
    const Layout part = LayoutOf(unit, component);
    whole.size =
        (whole.size + part.align - 1) / part.align * part.align + part.size;
    whole.align = std::max(whole.align, part.align);
  }
  return whole;
}

// Values sharing one storage, which is as large as the largest.
auto Overlay(const CompilationUnit& unit, std::span<const TypeId> members)
    -> Layout {
  Layout whole{.size = 0, .align = 1};
  for (const TypeId member : members) {
    const Layout part = LayoutOf(unit, member);
    whole.size = std::max(whole.size, part.size);
    whole.align = std::max(whole.align, part.align);
  }
  return whole;
}

// A tagged union's members overlaid, then its tag at the tag's alignment: the
// smallest unsigned integer that counts the members.
auto TaggedOverlay(const CompilationUnit& unit, std::span<const TypeId> members)
    -> Layout {
  const Layout overlay = Overlay(unit, members);
  std::uint64_t tag = 1;
  while (tag < sizeof(std::uint64_t) &&
         members.size() > (std::uint64_t{1} << (tag * 8))) {
    tag *= 2;
  }
  return Layout{
      .size = ((overlay.size + tag - 1) / tag * tag) + tag,
      .align = std::max(overlay.align, tag)};
}

// A value laid out by what it holds is that; a value of the library's is the
// class the library keeps it as; a machine value is what a register holding it
// holds. Storage the library keeps -- a cell, a write in progress, an object of
// the hierarchy -- is never a part of a value, and keeps a value too large for
// a frame in storage of its own, as every holder does, so it is never too large
// for one either; it is counted as the handle a frame names it by.
auto LayoutOf(const CompilationUnit& unit, TypeId type) -> Layout {
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
            return TaggedOverlay(unit, t.members);
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
          [](const VoidType&) { return Layout{.size = 0, .align = 1}; },
          [](const MachineBoolType&) { return LayoutOfClass<bool>(); },
          [](const MachineIntType& t) {
            const std::uint64_t bytes = BitsOf(t.width) / 8;
            return Layout{.size = bytes, .align = bytes};
          },
          [](const MachineFloatType& t) {
            const std::uint64_t bytes = BitsOf(t.width) / 8;
            return Layout{.size = bytes, .align = bytes};
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
