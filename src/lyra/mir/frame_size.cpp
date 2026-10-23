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
#include "lyra/value/any.hpp"
#include "lyra/value/bit_vec.hpp"
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
    case RuntimeLibraryKind::kBitVec:
      return LayoutOfClass<value::BitVec>();
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

// Whether a value laid out as `layout` is too large for a frame, which is a
// stack no design chose the size of.
auto TooLargeForAFrame(const support::Layout& layout) -> bool {
  return layout.size > value::kMaxInlineBytes;
}

// Values one after another, each at its alignment.
auto Sequence(const auto& part, TypeId element, std::uint64_t count)
    -> support::Layout {
  return value::SequenceLayoutOf(part(element), count).whole;
}

// Values side by side, as a C++ struct lays its members out.
auto Product(const auto& part, std::span<const TypeId> parts)
    -> support::Layout {
  return value::ProductLayoutOf(parts | std::views::transform(part)).whole;
}

// Values sharing one storage, each at its start.
auto Overlay(const auto& part, std::span<const TypeId> members)
    -> support::Layout {
  return value::OverlayLayoutOf(members | std::views::transform(part));
}

// A value laid out by what it holds is that; a value of the library's is the
// class the library keeps it as; a machine value is what a register holding it
// holds. Storage the library keeps -- a cell, a write in progress, an object of
// the hierarchy -- is never a part of a value, and keeps a value too large for
// a frame in storage of its own, as every holder does, so it is never too large
// for one either; it is counted as the handle a frame names it by. `part`
// answers the layout of each type `type` is made of.
auto LaidOut(const CompilationUnit& unit, TypeId type, const auto& part)
    -> support::Layout {
  const auto handle = [](const auto&) { return kAddress; };
  return unit.types.Get(type).Visit(
      Overloaded{
          [&](const IntegralType& t) { return LayoutOfIntegral(t); },
          [&](const EnumType& t) { return LayoutOfIntegral(t.base); },
          [&](const UnpackedArrayType& t) {
            return Sequence(part, t.element_type, t.Size());
          },
          [&](const MachineArrayType& t) {
            return Sequence(part, t.element, t.size);
          },
          [&](const TupleType& t) { return Product(part, t.elements); },
          // A structure's members side by side, a union's each at the start
          // of one storage, and a tagged union's tag after them.
          [&](const RecordType& t) {
            const std::span<const TypeId> members =
                RecordElements(unit, t.declaration);
            switch (RecordKindOf(unit, t.declaration)) {
              case RecordKind::kStruct:
                return Product(part, members);
              case RecordKind::kUnion:
                return Overlay(part, members);
              case RecordKind::kTaggedUnion:
                return value::TaggedUnionLayoutOf(
                           Overlay(part, members), members.size())
                    .whole;
            }
            throw InternalError("mir: unknown record kind");
          },
          // A closure opens with the address of the definition the library
          // runs and ends it through, and its captures lie after it, each as
          // it is held: one too large for a frame is held through a box,
          // whether its declaration names the value or that box.
          [&](const ClosureType& t) {
            std::vector<support::Layout> parts{kAddress};
            for (const FieldDecl& field :
                 unit.GetClosure(t.closure_id).fields) {
              const support::Layout captured = part(field.type);
              parts.push_back(
                  TooLargeForAFrame(captured) ? kAddress : captured);
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
          [](const AnyType&) { return LayoutOfClass<value::Any>(); },
          [](const ErasedValueType&) -> support::Layout {
            throw InternalError(
                "mir: a value of a type known only where the program runs is "
                "laid out only behind a reference -- please report this as a "
                "bug");
          },
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

// `type`'s layout, worked out the first time it is asked of `unit` and kept on
// it from then on.
auto LayoutOf(const CompilationUnit& unit, TypeId type) -> support::Layout {
  if (const auto known = unit.layouts.find(type); known != unit.layouts.end()) {
    return known->second;
  }
  const support::Layout layout =
      LaidOut(unit, type, [&](TypeId part) { return LayoutOf(unit, part); });
  unit.layouts.emplace(type, layout);
  return layout;
}

}  // namespace

auto IsTooLargeForAFrame(const CompilationUnit& unit, TypeId type) -> bool {
  return TooLargeForAFrame(LayoutOf(unit, type));
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
