#include "lyra/mir/frame_size.hpp"

#include <algorithm>
#include <cstdint>
#include <ranges>
#include <span>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/builtin_fn.hpp"
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
constexpr auto LayoutOfClass() -> support::Layout {
  return support::Layout{.size = sizeof(T), .align = alignof(T)};
}

constexpr support::Layout kAddress = LayoutOfClass<void*>();

auto LayoutOfIntegral(const IntegralType& integral) -> support::Layout {
  return support::Layout{
      .size = value::IntegralBytesFor(
          integral.bit_width, StateDomainOf(integral.state_kind)),
      .align = value::IntegralAlignFor(integral.bit_width)};
}

// An object of the library's: the class it is, where that class is the value
// library's, and the storage stated for it where the runtime defines it and a
// frame holds one by value. Every other class of the runtime's is reached by
// address, and is counted as the word a frame names one by.
auto LayoutOfLibraryObject(RuntimeLibraryKind kind) -> support::Layout {
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
      return support::kTriggerStorage.layout;
    case RuntimeLibraryKind::kObservation:
      return support::kObservationStorage.layout;
    case RuntimeLibraryKind::kReadReport:
      return support::kReadReportStorage.layout;
    case RuntimeLibraryKind::kWait:
      return support::kWaitStorage.layout;
    case RuntimeLibraryKind::kHierarchySegment:
      return support::kHierarchySegmentStorage.layout;
    case RuntimeLibraryKind::kChannelCancellation:
      return support::kChannelCancellationStorage.layout;
    case RuntimeLibraryKind::kDpiBitChunk:
    case RuntimeLibraryKind::kDpiLogicChunk:
    case RuntimeLibraryKind::kDpiOpenArrayHandle:
    case RuntimeLibraryKind::kCancellationTarget:
    case RuntimeLibraryKind::kControlEffect:
    case RuntimeLibraryKind::kObjectDefinition:
    case RuntimeLibraryKind::kScopeInfo:
    case RuntimeLibraryKind::kScopeCallable:
    case RuntimeLibraryKind::kConstructionTable:
    case RuntimeLibraryKind::kInstantiation:
    case RuntimeLibraryKind::kTimer:
    case RuntimeLibraryKind::kDumpEntry:
    case RuntimeLibraryKind::kDumpBlock:
      return kAddress;
  }
  throw InternalError("mir: unknown runtime library kind");
}

auto LayoutOf(const CompilationUnit& unit, TypeId type) -> support::Layout;

// Values one after another, each at its alignment.
auto Sequence(const CompilationUnit& unit, TypeId element, std::uint64_t count)
    -> support::Layout {
  return value::SequenceLayoutOf(LayoutOf(unit, element), count).whole;
}

// Values side by side, as a C++ struct lays its members out.
auto Product(const CompilationUnit& unit, std::span<const TypeId> parts)
    -> support::Layout {
  return value::ProductLayoutOf(parts | std::views::transform([&](TypeId part) {
                                  return LayoutOf(unit, part);
                                }))
      .whole;
}

// Values sharing one storage, each at its start.
auto Overlay(const CompilationUnit& unit, std::span<const TypeId> members)
    -> support::Layout {
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
auto LayoutOf(const CompilationUnit& unit, TypeId type) -> support::Layout {
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
          // A structure's members side by side, a union's each at the start
          // of one storage, and a tagged union's tag after them.
          [&](const RecordType& t) {
            const std::span<const TypeId> members =
                RecordElements(unit, t.declaration);
            switch (RecordKindOf(unit, t.declaration)) {
              case RecordKind::kStruct:
                return Product(unit, members);
              case RecordKind::kUnion:
                return Overlay(unit, members);
              case RecordKind::kTaggedUnion:
                return value::TaggedUnionLayoutOf(
                           Overlay(unit, members), members.size())
                    .whole;
            }
            throw InternalError("mir: unknown record kind");
          },
          // A closure opens with the address of the definition the library
          // runs and ends it through, and its captures lie after it.
          [&](const ClosureType& t) {
            std::vector<support::Layout> parts{kAddress};
            for (const FieldDecl& field :
                 unit.GetClosure(t.closure_id).fields) {
              parts.push_back(LayoutOf(unit, field.type));
            }
            return value::ProductLayoutOf(parts).whole;
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
          [](const VoidType&) {
            return support::Layout{.size = 0, .align = 1};
          },
          [](const MachineBoolType&) { return LayoutOfClass<bool>(); },
          [](const MachineIntType& t) {
            const std::uint64_t bytes = BitsOf(t.width) / 8;
            return support::Layout{.size = bytes, .align = bytes};
          },
          [](const MachineFloatType& t) {
            const std::uint64_t bytes = BitsOf(t.width) / 8;
            return support::Layout{.size = bytes, .align = bytes};
          },
          [&](const MachineCStringType& t) { return handle(t); },
          [&](const MachineFunctionType& t) { return handle(t); },
          [](const ErasedCallableType&) -> support::Layout {
            throw InternalError(
                "mir: an erased callable is laid out only behind a box or a "
                "borrow -- please report this as a bug");
          },
          [&](const PointerType& t) { return handle(t); },
          [&](const ReferenceType& t) { return handle(t); },
          [&](const VariableRefType& t) { return handle(t); },
          [&](const ReferenceWrapperType& t) { return handle(t); },
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
          [&](const EvaluationAttemptsType& t) { return handle(t); },
          [&](const LoopBlocksType& t) { return handle(t); }});
}

}  // namespace

auto IsTooLargeForAFrame(const CompilationUnit& unit, TypeId type) -> bool {
  return LayoutOf(unit, type).size > value::kMaxInlineBytes;
}

auto TakesAValueTooLargeForAFrame(const CompilationUnit& unit, TypeId type)
    -> bool {
  const auto* code = unit.types.Get(type).As<MachineFunctionType>();
  return code != nullptr &&
         std::ranges::any_of(code->params, [&](TypeId param) {
           return IsTooLargeForAFrame(unit, param);
         });
}

auto EntersACompiledBody(
    const CompilationUnit& unit, const CallExpr& call, TypeId result) -> bool {
  return std::visit(
      Overloaded{
          [](const Direct& direct) {
            return !std::holds_alternative<support::BuiltinFn>(direct.target) &&
                   !std::holds_alternative<ForeignSymbolTarget>(
                       direct.target) &&
                   !std::holds_alternative<RecordMethodTarget>(direct.target) &&
                   !std::holds_alternative<TypeOperationTarget>(direct.target);
          },
          [](const Indirect&) { return true; },
          [](const Virtual&) { return true; },
          [&](const Construct&) {
            return unit.types.Get(result).Is<ObjectType>();
          }},
      call.callee);
}

}  // namespace lyra::mir
