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
#include "lyra/mir/operand_use.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/storage_part.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/mir/verify.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/layout.hpp"
#include "lyra/support/runtime_object.hpp"
#include "lyra/value/frame_bound.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/layout.hpp"

namespace lyra::mir {

namespace {

constexpr support::Layout kAddress{
    .size = sizeof(void*), .align = alignof(void*)};

auto LayoutOfIntegral(const IntegralType& integral) -> support::Layout {
  return support::Layout{
      .size = value::IntegralBytesFor(
          integral.bit_width, StateDomainOf(integral.state_kind)),
      .align = value::IntegralAlignFor(integral.bit_width)};
}

// An object of the library's a frame holds by value: the storage stated for
// it. Every other class of the runtime's is reached by address, and is counted
// as the word a frame names one by.
auto LayoutOfLibraryObject(RuntimeLibraryKind kind) -> support::Layout {
  switch (kind) {
    case RuntimeLibraryKind::kBitVec:
      return support::kBitVecStorage.layout;
    case RuntimeLibraryKind::kPrintItem:
      return support::kPrintItemStorage.layout;
    case RuntimeLibraryKind::kPrintLiteralItem:
      return support::kPrintLiteralItemStorage.layout;
    case RuntimeLibraryKind::kPrintValueItem:
      return support::kPrintValueItemStorage.layout;
    case RuntimeLibraryKind::kFormatSpec:
      return support::kFormatSpecStorage.layout;
    case RuntimeLibraryKind::kFormatArg:
      return support::kFormatArgStorage.layout;
    case RuntimeLibraryKind::kTimeFormat:
      return support::kTimeFormatStorage.layout;
    case RuntimeLibraryKind::kDpiBitBuffer:
      return support::kDpiBitBufferStorage.layout;
    case RuntimeLibraryKind::kDpiLogicBuffer:
      return support::kDpiLogicBufferStorage.layout;
    case RuntimeLibraryKind::kDpiOpenArray:
      return support::kDpiOpenArrayStorage.layout;
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
    case RuntimeLibraryKind::kLeasedReference:
      return support::kLeasedReferenceStorage.layout;
    case RuntimeLibraryKind::kHierarchySegment:
      return support::kHierarchySegmentStorage.layout;
    case RuntimeLibraryKind::kChannelCancellation:
      return support::kChannelCancellationStorage.layout;
    case RuntimeLibraryKind::kDpiBitChunk:
    case RuntimeLibraryKind::kDpiLogicChunk:
    case RuntimeLibraryKind::kDpiOpenArrayHandle:
    case RuntimeLibraryKind::kWatchedPlace:
    case RuntimeLibraryKind::kCancellationTarget:
    case RuntimeLibraryKind::kControlEffect:
    case RuntimeLibraryKind::kObjectDefinition:
    case RuntimeLibraryKind::kScopeInfo:
    case RuntimeLibraryKind::kScopeCallable:
    case RuntimeLibraryKind::kClassObjectInfo:
    case RuntimeLibraryKind::kInterfaceRow:
    case RuntimeLibraryKind::kConstructionTable:
    case RuntimeLibraryKind::kInstantiation:
    case RuntimeLibraryKind::kGeneratePlaces:
    case RuntimeLibraryKind::kTimer:
    case RuntimeLibraryKind::kDeferredEffects:
    case RuntimeLibraryKind::kViolationReportKeeper:
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
// holds; an object of the library's a frame holds while it works -- a
// reference, a write in progress -- is the storage stated for it. Storage the
// library keeps -- a cell, an object of the hierarchy -- is never a part of a
// value, and keeps a value too large for a frame in storage of its own, as
// every holder does, so it is never too large for one either; it is counted as
// the handle a frame names it by. `part` answers the layout of each type
// `type` is made of.
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
            return support::kDynamicArrayStorage.layout;
          },
          [](const QueueType&) { return support::kQueueStorage.layout; },
          [](const AssociativeArrayType&) {
            return support::kAssociativeArrayStorage.layout;
          },
          [](const WildcardIndexType&) {
            return support::kWildcardKeyStorage.layout;
          },
          [](const StringType&) { return support::kStringStorage.layout; },
          [](const AnyType&) { return support::kAnyStorage.layout; },
          [](const ErasedValueType&) -> support::Layout {
            throw InternalError(
                "mir: a value of a type known only where the program runs is "
                "laid out only behind a reference -- please report this as a "
                "bug");
          },
          [](const RealType&) { return support::kRealStorage.layout; },
          [](const ShortRealType&) {
            return support::kShortRealStorage.layout;
          },
          [](const ChandleType&) { return support::kChandleStorage.layout; },
          [](const ManagedRefType&) {
            return support::kObjectRefStorage.layout;
          },
          [](const WeakManagedRefType&) {
            return support::kWeakReferenceStorage.layout;
          },
          [](const EmptyType&) { return support::kEmptyStorage.layout; },
          [](const VoidType&) {
            return support::Layout{.size = 0, .align = 1};
          },
          [](const MachineBoolType&) {
            return support::Layout{
                .size = sizeof(bool), .align = alignof(bool)};
          },
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
          // A shared pointer is the library's object counting its holds; any
          // other is the address it holds.
          [&](const PointerType& t) {
            switch (t.ownership) {
              case PointerOwnership::kShared:
                return support::kSharedPointerStorage.layout;
              case PointerOwnership::kUnique:
              case PointerOwnership::kBorrowed:
                return kAddress;
            }
            throw InternalError("mir: unknown pointer ownership");
          },
          [&](const ReferenceType& t) { return handle(t); },
          // A variable's reference is the library's object naming where its
          // referent lies and whatever is told of a write through it.
          [&](const VariableRefType& t) {
            return unit.types.Get(t.pointee).Is<ErasedValueType>()
                       ? support::kAnyReferenceStorage.layout
                       : support::kReferenceStorage.layout;
          },
          [](const ReferenceWrapperType&) {
            return support::kReferenceStorage.layout;
          },
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
          // A write in progress and a place designated within one are the
          // library's objects the writer holds for as long as the write lasts.
          [](const OpenWriteType&) {
            return support::kOpenWriteStorage.layout;
          },
          [](const DesignationType&) {
            return support::kDesignationStorage.layout;
          },
          [&](const BitsReferenceType& t) { return handle(t); },
          [](const ObjectWriteType&) {
            return support::kObjectWriteStorage.layout;
          },
          [&](const SampledHistoryType& t) { return handle(t); },
          [&](const EvaluationAttemptsType& t) { return handle(t); },
          [&](const LoopBlocksType& t) { return handle(t); }});
}

// Calls `discard` with each expression `data` evaluates and discards the value
// of. A declaration's initializer is made in the variable it declares, a
// return's answer where the caller or the completion takes it, and every other
// expression a statement evaluates is read by it.
void ForEachDiscarded(const StmtData& data, const auto& discard) {
  std::visit(
      Overloaded{
          [](const EmptyStmt&) {}, [](const LocalDeclStmt&) {},
          [&](const ExprStmt& s) { discard(s.expr); }, [](const BlockStmt&) {},
          [](const TryStmt&) {}, [](const RaiseStmt&) {},
          [](const FinallyStmt&) {}, [](const IfStmt&) {},
          [](const SwitchStmt&) {},
          [&](const ForStmt& s) {
            for (const ForInit& init : s.init) {
              std::visit(
                  Overloaded{
                      [](const ForInitDecl&) {},
                      [&](const ForInitExpr& expr) { discard(expr.expr); }},
                  init);
            }
            std::ranges::for_each(s.step, discard);
          },
          [](const WhileStmt&) {}, [](const DoWhileStmt&) {},
          [](const BreakStmt&) {}, [](const ContinueStmt&) {},
          [](const ReturnStmt&) {}},
      data);
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

auto FrameBytesOf(const CompilationUnit& unit, TypeId type) -> std::size_t {
  const Type& held = unit.types.Get(type);
  // A variable's cell keeps its value inline where the one rule every holder
  // reads lays it there, which is what holding the cell apart spares the frame.
  if (const auto* cell = held.As<ObservableType>()) {
    return value::LiesInline(LayoutOf(unit, cell->value))
               ? FrameBytesOf(unit, cell->value)
               : 0;
  }
  if (!held.IsIntegral() && !held.IsRuntimeStoredValue()) {
    return 0;
  }
  const std::size_t bytes = LayoutOf(unit, type).size;
  return bytes > kAddress.size ? bytes : 0;
}

auto ValuesAFrameKeeps(const CompilationUnit& unit, const Block& block)
    -> std::vector<ExprId> {
  std::vector<bool> made(block.exprs.size(), false);
  for (const Expr& expr : block.exprs) {
    ForEachOperand(expr.data, [&](ExprId operand, OperandUse) {
      switch (WhereOperandIsMade(expr.data, operand)) {
        case OperandMadeIn::kWhereTaken:
          made.at(operand.value) = true;
          break;
        case OperandMadeIn::kTheOperationsValue:
        case OperandMadeIn::kTheOperationsAllocation:
        case OperandMadeIn::kTheRuntime:
          break;
      }
    });
  }
  for (const Stmt& stmt : block.stmts) {
    ForEachDiscarded(stmt.data, [&](ExprId id) { made.at(id.value) = true; });
  }
  std::vector<ExprId> kept;
  for (const ExprId id : block.exprs.Ids()) {
    const TypeId type = block.exprs.Get(id).type;
    if (made.at(id.value) && !ValueLiesInStorage(unit, block, id) &&
        (IsTooLargeForAFrame(unit, type) ||
         (!EvaluatesNothing(block, id) && FrameBytesOf(unit, type) > 0))) {
      kept.push_back(id);
    }
  }
  return kept;
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
