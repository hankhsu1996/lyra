#include "lyra/lowering/hir_to_mir/container_bodies.hpp"

#include <array>
#include <cstddef>
#include <cstdint>
#include <expected>
#include <functional>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/base/diag_message.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/binary_op.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/bitstream.hpp"
#include "lyra/lowering/hir_to_mir/block_builder.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/concatenation.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/expression/operators.hpp"
#include "lyra/lowering/hir_to_mir/operation_bodies.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/select_position.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/value_operations.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/inherent_methods.hpp"
#include "lyra/mir/library_record_builder.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/storage_part.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/mir/type_interface.hpp"
#include "lyra/mir/value_parts.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/library_decl.hpp"
#include "lyra/support/value_operation.hpp"
#include "lyra/support/value_step.hpp"
#include "lyra/value/container_core.hpp"
#include "lyra/value/container_lease.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto ElementCount(
    const mir::CompilationUnit& unit, mir::Builder block, mir::TypeId type,
    mir::ExprId array) -> mir::ExprId;

// The element at the machine index `index` of the array `array` lies in, an
// array of elements of `element`, reached at `mutability`, as Rust's
// `<[T]>::get_unchecked` and `get_unchecked_mut` reach one: the index counts
// below that array's own count, so nothing about it is checked. A
// run-time-sized array's element lies where its type's own body says, and a
// fixed-size array's where its type's layout places it.
auto UncheckedElement(
    const mir::CompilationUnit& unit, mir::Builder block, mir::ExprId array,
    mir::ExprId index, mir::TypeId element, mir::Mutability mutability)
    -> mir::ExprId {
  const support::ElementAccessItem item = [&] {
    switch (mutability) {
      case mir::Mutability::kReadOnly:
        return support::ElementAccessItem::kGetUnchecked;
      case mir::Mutability::kMutable:
        return support::ElementAccessItem::kGetUncheckedMut;
    }
    throw InternalError("hir_to_mir: unknown mutability");
  }();
  const support::BuiltinFn fn = support::LibraryEntryOf(item);
  const mir::TypeId reference = unit.types.Intern(
      mir::Type{
          mir::ReferenceType{.referent = element, .mutability = mutability}});
  std::optional<mir::Expr> typed =
      ContainerOperationCall(unit, block, item, array, {index}, reference);
  const mir::ExprId reached = block.Add(
      typed.has_value()
          ? *std::move(typed)
          : mir::Expr{
                .data =
                    mir::CallExpr{
                        .callee = mir::Direct{.target = fn, .receiver = array},
                        .arguments = {index}},
                .type = reference});
  return block.Add(mir::MakeDerefExpr(reached, element));
}

// What an array of each kind is to the bodies built over it. A fixed-size
// array (LRM 7.4) holds as many elements as its type says, where it lies. A
// dynamic array's core (LRM 7.5) keeps its elements one after another, in one
// generation at a time. A queue's core (LRM 7.10) keeps each element in
// storage of its own, which stays where it is while the queue changes around
// it.
struct FixedSizeKind {
  std::int64_t size;
};
struct DynamicArrayKind {};
struct QueueKind {};
using ArrayKind = std::variant<FixedSizeKind, DynamicArrayKind, QueueKind>;

// The end of a queue an element is put at or taken from (LRM 7.10.2).
enum class QueueSide : std::uint8_t { kFront, kBack };

auto KindOf(const mir::CompilationUnit& unit, mir::TypeId type) -> ArrayKind {
  const mir::Type& stated = unit.types.Get(type);
  if (const auto* fixed = stated.As<mir::UnpackedArrayType>()) {
    return FixedSizeKind{.size = static_cast<std::int64_t>(fixed->Size())};
  }
  if (stated.IsDeclaredBy(support::LibraryDecl::kDynamicArray)) {
    return DynamicArrayKind{};
  }
  if (stated.IsDeclaredBy(support::LibraryDecl::kQueue)) {
    return QueueKind{};
  }
  throw InternalError(
      "hir_to_mir: a body over the elements of an array is built for a type "
      "that is no array of a kind it knows -- please report this as a bug");
}

// The entry of the core an array of `kind` keeps that builds an array of no
// elements, told the record of the element type, and the one giving the
// array's storage back once its body has let go of its elements. A fixed-size
// array keeps no core, so nothing building a body for one asks.
auto CoreConstructOf(const ArrayKind& kind) -> support::BuiltinFn {
  return std::visit(
      Overloaded{
          [](const FixedSizeKind&) -> support::BuiltinFn {
            throw InternalError(
                "hir_to_mir: the core of a fixed-size array is asked for, "
                "and it keeps none -- please report this as a bug");
          },
          [](const DynamicArrayKind&) {
            return support::BuiltinFn::kDynamicArrayConstruct;
          },
          [](const QueueKind&) { return support::BuiltinFn::kQueueConstruct; }},
      kind);
}
auto CoreEndOf(const ArrayKind& kind) -> support::BuiltinFn {
  return std::visit(
      Overloaded{
          [](const FixedSizeKind&) -> support::BuiltinFn {
            throw InternalError(
                "hir_to_mir: the core of a fixed-size array is asked for, "
                "and it keeps none -- please report this as a bug");
          },
          [](const DynamicArrayKind&) {
            return support::BuiltinFn::kDynamicArrayFree;
          },
          [](const QueueKind&) { return support::BuiltinFn::kQueueFreeRing; }},
      kind);
}

// `value` built in storage the queue `queue` lies in allocates for an element,
// answering where it lies, a pointer of `element_pointer`.
auto MadeInQueueStorage(
    const mir::CompilationUnit& unit, mir::Builder block, mir::ExprId queue,
    mir::ExprId value, mir::TypeId element_pointer) -> mir::ExprId {
  return block.Add(
      mir::MakePlacementNewExpr(
          value,
          mir::PlacementAllocation{
              .allocate = support::BuiltinFn::kDefaultPlacementNew,
              .arguments = {block.Add(*ContainerOperationCall(
                  unit, block, support::BuiltinFn::kQueueAllocate, queue, {},
                  element_pointer))}},
          element_pointer));
}

// The bodies built over the elements of one array type: an operation every
// kind has is one program over the elements, asking the array how many it
// holds -- its type's count, or its core's -- as Rust's arrays, vectors and
// deques compare as the slices of their elements. Where kinds differ, each
// body says what it does for each kind.
class ArrayBodies : public OperationBodies {
  // Answers each trait's items with this type's own bodies.
  friend class OperationBodies;

 public:
  ArrayBodies(
      const mir::CompilationUnit& unit, mir::TypeId array, mir::TypeId element)
      : OperationBodies(
            unit, array, {element},
            [array](support::ValueOperation answers) -> mir::DirectTarget {
              return mir::TypeOperationTarget{
                  .type = array, .answers = answers};
            }),
        kind_(KindOf(unit, array)) {
  }

  // Every operation an array of this type answers with a body, each its
  // type's interface declares.
  auto Build(const mir::TypeOperationSource& source) -> mir::CallableCode {
    const support::ValueOperation answers = source.answers;
    return std::visit(
        Overloaded{
            [&](support::ValueStep step) -> mir::CallableCode {
              return Step(step);
            },
            [&](support::EqualityItem item) {
              return AnswerEquality(*this, item);
            },
            [&](support::LibraryAnsweredItem trait_item) {
              return std::visit(
                  Overloaded{
                      [&](support::ChangeTestItem item) {
                        return AnswerChangeTest(*this, item);
                      },
                      [&](support::UnknownItem item) -> mir::CallableCode {
                        switch (item) {
                          case support::UnknownItem::kHasUnknown:
                            return HasUnknown();
                          case support::UnknownItem::kMergeConditional:
                            return MergeConditional();
                        }
                        throw InternalError(
                            "hir_to_mir: unknown unknown-bit item");
                      },
                      // A fixed-size array's stream is read back by its own
                      // count, and a run-time-sized one's by the count the run
                      // holds.
                      [&](support::StreamItem item) -> mir::CallableCode {
                        switch (item) {
                          case support::StreamItem::kBitstreamWidth:
                            return BitWidth();
                          case support::StreamItem::kCountBits:
                            return CountBits();
                          case support::StreamItem::kToBitstream:
                            return ToBitstream();
                          case support::StreamItem::kFromBitstream:
                            return FromBitstream();
                          case support::StreamItem::kToBitVec:
                            return ToBitVec();
                          case support::StreamItem::kFromBitVec:
                            return IsFixedSize() ? FixedFromBitVec()
                                                 : FromBitVec();
                        }
                        throw InternalError("hir_to_mir: unknown stream item");
                      },
                      [&](support::PositionsItem item) {
                        return AnswerPositions(*this, item);
                      },
                      // A fixed-size array's default is each element's; a
                      // run-time-sized one's is the empty array its core
                      // answers.
                      [&](support::DefaultItem item) -> mir::CallableCode {
                        if (IsFixedSize()) {
                          return AnswerDeclaredDefault(*this, item);
                        }
                        return Default(item);
                      },
                      [&](support::ElementAccessItem item)
                          -> mir::CallableCode {
                        switch (item) {
                          case support::ElementAccessItem::kGetUnchecked:
                            return GetUncheckedBody(
                                item, mir::Mutability::kReadOnly);
                          case support::ElementAccessItem::kGetUncheckedMut:
                            return GetUncheckedBody(
                                item, mir::Mutability::kMutable);
                          case support::ElementAccessItem::kElementPlace:
                            return ElementPlace();
                        }
                        throw InternalError(
                            "hir_to_mir: unknown element-access item");
                      },
                      [&](support::ResizeItem item) -> mir::CallableCode {
                        switch (item) {
                          case support::ResizeItem::kResized:
                            return Resized();
                        }
                        throw InternalError("hir_to_mir: unknown resize item");
                      }},
                  trait_item);
            },
            [](support::BoundFreeItem) -> mir::CallableCode {
              NoBoundFreeBody();
            },
            // One of this kind's own methods, built as its row says.
            [&](support::BuiltinFn fn) -> mir::CallableCode {
              return MethodBodyOf(fn).build(*this, source);
            }},
        answers);
  }

  // LRM 7.6: an array of this type -- a dynamic array or a queue -- taking
  // the elements of an array of `source`, which holds elements of this
  // type's, in order; a queue then holds them to its bound (LRM 7.10.5).
  auto FromArray(support::ValueOperation answers, mir::TypeId source)
      -> mir::CallableCode {
    return Defined(
        Declaration(answers, source),
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          const auto from = [&](mir::Builder at) {
            return Referent(at, p[0], source);
          };
          const mir::LocalId shape =
              Declared(code, block, WholeType(), Empty(block));
          const mir::ExprId made = Built(
              code, block, LocalArray(shape),
              ElementCount(Unit(), block, source, from(block)),
              [&](mir::Builder body, mir::LocalId index) {
                return body.AsValue(
                    Unit().types, GetUnchecked(body, from(body), index));
              });
          block.AppendStmt(
              mir::ReturnStmt{.value = HeldToItsBound(code, block, made)});
        });
  }

  // A fixed-size array of `destination`, of this type's element type, taking
  // the elements of an array of this type, which holds as many.
  auto IntoFixedArray(mir::TypeId destination) -> mir::CallableCode {
    const std::uint64_t count =
        Unit().types.Get(destination).Get<mir::UnpackedArrayType>().Size();
    return Defined(
        Declaration(
            mir::TypeOperationSource{
                .type = destination,
                .answers = support::BuiltinFn::kUnpackedArrayFromArray,
                .arguments = {WholeType()}}),
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          mir::AppendRunTimeCheck(
              Unit(), block,
              mir::BuildMachineIntComparison(
                  Unit(), block, mir::BinaryOp::kEquality,
                  ElementCount(Unit(), block, WholeType(), Read(block, p[0])),
                  CountLiteral(block, count)),
              base::DiagMessage(
                  "a fixed-size array is assigned an array holding another "
                  "number of elements")
                  .TakeText());
          const mir::LocalId built = Declared(
              code, block, destination,
              block.Add(BuildDefaultValueExpr(Unit(), block, destination)));
          ForEachIndex(
              code, block, CountLiteral(block, count),
              [&](mir::Builder body, mir::LocalId index) {
                StoredUnchecked(
                    body, body.LocalPlace(built, destination), index,
                    body.AsValue(
                        Unit().types,
                        GetUnchecked(body, Read(body, p[0]), index)));
              });
          block.AppendStmt(
              mir::ReturnStmt{.value = block.ReadLocal(built, destination)});
        });
  }

  // LRM 7.4.5 / 7.4.6: the `run` elements of an array of this type from a
  // start, a position, read as a fixed-size array of them, each the element
  // default where it lies outside the array. The read step answers the
  // default outside, so the array's count is not asked.
  auto ElementSlice(mir::TypeId run) -> mir::CallableCode {
    const mir::TypeId position_type = mir::PositionType(Unit().types);
    const std::uint64_t count =
        Unit().types.Get(run).Get<mir::UnpackedArrayType>().Size();
    return Defined(
        Declaration(support::BuiltinFn::kElementSlice, run),
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          const mir::LocalId built = Declared(
              code, block, run,
              block.Add(BuildDefaultValueExpr(Unit(), block, run)));
          ForEachIndex(
              code, block, CountLiteral(block, count),
              [&](mir::Builder body, mir::LocalId index) {
                const mir::ExprId from = body.Add(
                    mir::MakeBinary(
                        Unit(), body.Body(), mir::BinaryOp::kAdd,
                        body.AsValue(
                            Unit().types, Referent(body, p[1], position_type)),
                        PositionOf(body, index), position_type));
                StoredUnchecked(
                    body, body.LocalPlace(built, run), index,
                    body.AsValue(
                        Unit().types, Element(body, Read(body, p[0]), from)));
              });
          block.AppendStmt(
              mir::ReturnStmt{.value = block.ReadLocal(built, run)});
        });
  }

  // LRM 7.6: the elements of an array of this type from a start, a position,
  // each taking the one at the same place of `run`, a fixed-size array of as
  // many, answering the positions from the first that took a different value
  // to one past the last (LRM 4.3). An element the run would reach outside the
  // array is skipped, and a start that names no position names none.
  auto AssignElementSlice(mir::TypeId run) -> mir::CallableCode {
    const mir::TypeId position_type = mir::PositionType(Unit().types);
    const std::uint64_t count =
        Unit().types.Get(run).Get<mir::UnpackedArrayType>().Size();
    return Defined(
        Declaration(support::BuiltinFn::kAssignElementSlice, run),
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          const ChangedRange changed = NoneChanged(code, block);
          const mir::LocalId held = Declared(
              code, block, position_type, Count(block, Written(block, p[0])));
          ForEachIndex(
              code, block, CountLiteral(block, count),
              [&](mir::Builder body, mir::LocalId index) {
                const mir::LocalId target = Declared(
                    code, body, position_type,
                    PositionSum(
                        body,
                        body.AsValue(
                            Unit().types, Referent(body, p[1], position_type)),
                        PositionOf(body, index)));
                mir::Block within;
                mir::Builder within_builder = body.Into(within);
                mir::Block below;
                mir::Builder below_builder = within_builder.Into(below);
                WrittenWhereItDiffers(
                    code, below_builder, p[0], target,
                    [&](mir::Builder in) {
                      return GetUnchecked(in, Referent(in, p[2], run), index);
                    },
                    changed);
                within_builder.AppendIfThen(
                    PositionCompare(
                        within_builder, mir::BinaryOp::kLessThan,
                        within_builder.ReadLocal(target, position_type),
                        within_builder.ReadLocal(held, position_type)),
                    std::move(below));
                body.AppendIfThen(
                    PositionCompare(
                        body, mir::BinaryOp::kGreaterEqual,
                        body.ReadLocal(target, position_type),
                        BuildConstantPosition(Unit(), body, 0)),
                    std::move(within));
              });
          block.AppendStmt(
              mir::ReturnStmt{.value = ChangedAnswer(block, changed)});
        });
  }

  // The positions a run write changed, the first to take a different value
  // and the one past the last, both 0 while none has (LRM 4.3). The elements
  // are written in order, so the first to change is the one found while
  // nothing has yet, and each one found moves the end past it.
  struct ChangedRange {
    mir::LocalId begin;
    mir::LocalId end;
  };
  auto NoneChanged(mir::CallableCode& code, mir::Builder block)
      -> ChangedRange {
    const mir::TypeId position_type = mir::PositionType(Unit().types);
    return {
        .begin = Declared(
            code, block, position_type,
            BuildConstantPosition(Unit(), block, 0)),
        .end = Declared(
            code, block, position_type,
            BuildConstantPosition(Unit(), block, 0))};
  }
  auto ChangedAnswer(mir::Builder block, const ChangedRange& changed)
      -> mir::ExprId {
    const mir::TypeId position_type = mir::PositionType(Unit().types);
    return block.Add(
        mir::Expr{
            .data =
                mir::CompositeExpr{
                    .parts =
                        {block.ReadLocal(changed.begin, position_type),
                         block.ReadLocal(changed.end, position_type)}},
            .type = mir::ChangedRangeType(Unit().types)});
  }

  // The element at `target` of the array the parameter `array` refers to
  // written with what `replacement(in)` reads, where the two differ, the
  // write then counted in `changed`. The target names an element the array
  // holds, so it is read and written there unchecked.
  template <typename Replacement>
  void WrittenWhereItDiffers(
      mir::CallableCode& code, mir::Builder body, mir::LocalId array,
      mir::LocalId target, Replacement replacement,
      const ChangedRange& changed) {
    const mir::TypeId position_type = mir::PositionType(Unit().types);
    const auto set = [&](mir::Builder in, mir::LocalId local,
                         mir::ExprId value) {
      Statement(
          in, in.Add(
                  mir::MakeAssignExpr(
                      Unit().builtins, in.LocalPlace(local, position_type),
                      value)));
    };
    const mir::LocalId at = DeclaredCount(
        code, body,
        BuildToInt64Call(Unit(), body, body.ReadLocal(target, position_type)));
    mir::Block differs;
    mir::Builder differs_builder = body.Into(differs);
    StoredUnchecked(
        differs_builder, Written(differs_builder, array), at,
        differs_builder.AsValue(Unit().types, replacement(differs_builder)));
    mir::Block first;
    mir::Builder first_builder = differs_builder.Into(first);
    set(first_builder, changed.begin,
        first_builder.ReadLocal(target, position_type));
    differs_builder.AppendIfThen(
        PositionCompare(
            differs_builder, mir::BinaryOp::kEquality,
            differs_builder.ReadLocal(changed.end, position_type),
            BuildConstantPosition(Unit(), differs_builder, 0)),
        std::move(first));
    set(differs_builder, changed.end,
        PositionSum(
            differs_builder, differs_builder.ReadLocal(target, position_type),
            BuildConstantPosition(Unit(), differs_builder, 1)));
    const mir::ExprId same = BuildBitIdentity(
        Unit(), body, GetUnchecked(body, Written(body, array), at),
        replacement(body));
    body.AppendIfThen(
        body.Add(
            mir::MakeUnary(
                Unit(), body.Body(), mir::UnaryOp::kLogicalNot, same,
                Unit().builtins.machine_bool)),
        std::move(differs));
  }

  // A string holding `text`, and one holding the number the machine count
  // `local` holds in decimal, for a message the design is told.
  auto Text(mir::Builder block, std::string_view text) -> mir::ExprId {
    return BuildStringValueExpr(Unit(), block, std::string(text));
  }
  auto Decimal(mir::CallableCode& code, mir::Builder block, mir::LocalId local)
      -> mir::ExprId {
    const mir::TypeId text = Unit().builtins.string;
    const mir::LocalId written = Declared(
        code, block, text,
        block.Add(BuildDefaultValueExpr(Unit(), block, text)));
    Statement(
        block,
        block.Add(
            mir::MakeBuiltinCall(
                Unit(), block.Body(), support::BuiltinFn::kItoa,
                block.LocalPlace(written, text),
                {CountAsIntegral(
                    block,
                    block.ReadLocal(local, Unit().builtins.machine_int64),
                    Unit().builtins.int_type)},
                Unit().builtins.void_type)));
    return block.ReadLocal(written, text);
  }

  // LRM 7.10.1: the window a queue's slice between two positions names: from
  // the first held to 0 up to the last held to the queue's last, none where
  // either holds x or z or the first lies past the last.
  struct QueueWindow {
    mir::LocalId first;
    mir::LocalId count;
  };
  auto WindowOf(
      mir::CallableCode& code, mir::Builder block, mir::LocalId queue,
      mir::Mutability taken_as, mir::LocalId lowest, mir::LocalId highest)
      -> QueueWindow {
    const mir::TypeId position_type = mir::PositionType(Unit().types);
    const auto bound = [&](mir::LocalId reference) {
      return block.AsValue(
          Unit().types, Referent(block, reference, position_type));
    };
    const mir::LocalId lo = Declared(code, block, position_type, bound(lowest));
    const mir::LocalId hi =
        Declared(code, block, position_type, bound(highest));
    const auto read = [&](mir::LocalId local) {
      return block.ReadLocal(local, position_type);
    };
    const mir::ExprId zero = BuildConstantPosition(Unit(), block, 0);
    const mir::LocalId last = Declared(
        code, block, position_type,
        block.Add(
            mir::MakeBinary(
                Unit(), block.Body(), mir::BinaryOp::kSub,
                Count(block, Array(block, queue, taken_as)),
                BuildConstantPosition(Unit(), block, 1), position_type)));
    const mir::LocalId first = Declared(
        code, block, position_type,
        PositionChoice(
            block,
            PositionCompare(block, mir::BinaryOp::kLessThan, read(lo), zero),
            BuildConstantPosition(Unit(), block, 0), read(lo)));
    const mir::LocalId to = Declared(
        code, block, position_type,
        PositionChoice(
            block,
            PositionCompare(
                block, mir::BinaryOp::kGreaterThan, read(hi), read(last)),
            read(last), read(hi)));
    const std::array named{
        Known(block, read(lo)), Known(block, read(hi)),
        PositionCompare(
            block, mir::BinaryOp::kLessEqual, read(first), read(to))};
    const mir::LocalId count = Declared(
        code, block, position_type,
        PositionChoice(
            block, AllHold(Unit(), block, named),
            PositionSum(
                block,
                block.Add(
                    mir::MakeBinary(
                        Unit(), block.Body(), mir::BinaryOp::kSub, read(to),
                        read(first), position_type)),
                BuildConstantPosition(Unit(), block, 1)),
            BuildConstantPosition(Unit(), block, 0)));
    return {.first = first, .count = count};
  }

  // LRM 7.10.1: the elements of a queue of this type between two positions,
  // as a queue declaring no bound.
  auto QueueSlice() -> mir::CallableCode {
    const mir::TypeId position_type = mir::PositionType(Unit().types);
    const mir::TypeId slice =
        mir::UnboundedQueueOf(Unit().types, ElementType());
    return Defined(
        Declaration(support::BuiltinFn::kQueueSlice),
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          const QueueWindow window = WindowOf(
              code, block, p[0], mir::Mutability::kReadOnly, p[1], p[2]);
          const mir::LocalId made = QueueFilled(
              code, block, slice,
              BuildToInt64Call(
                  Unit(), block, block.ReadLocal(window.count, position_type)),
              [&](mir::Builder body, mir::LocalId index) {
                const mir::LocalId at = Declared(
                    code, body, Unit().builtins.machine_int64,
                    mir::BuildMachineIntBinary(
                        Unit(), body, mir::BinaryOp::kAdd,
                        BuildToInt64Call(
                            Unit(), body,
                            body.ReadLocal(window.first, position_type)),
                        IndexNow(body, index)));
                return body.AsValue(
                    Unit().types, GetUnchecked(body, Read(body, p[0]), at));
              });
          block.AppendStmt(
              mir::ReturnStmt{.value = TakenOutOf(block, made, slice)});
        });
  }

  // LRM 7.6, 7.10.1: the elements of a queue of this type between two
  // positions, the ones its clamping names, each taking the one at the same
  // place of a queue of as many, answering the positions it changed as an
  // array's run write does (LRM 4.3). A value of another count is the
  // design's own error, and nothing is written.
  auto AssignQueueSlice() -> mir::CallableCode {
    const mir::TypeId position_type = mir::PositionType(Unit().types);
    const mir::TypeId values =
        mir::UnboundedQueueOf(Unit().types, ElementType());
    return Defined(
        Declaration(support::BuiltinFn::kAssignQueueSlice),
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          const QueueWindow window = WindowOf(
              code, block, p[0], mir::Mutability::kMutable, p[1], p[2]);
          const mir::TypeId machine = Unit().builtins.machine_int64;
          const mir::LocalId named = Declared(
              code, block, machine,
              BuildToInt64Call(
                  Unit(), block, block.ReadLocal(window.count, position_type)));
          const mir::LocalId given = Declared(
              code, block, machine,
              ElementCount(
                  Unit(), block, values, Referent(block, p[3], values)));
          mir::Block unequal;
          mir::Builder unequal_builder = block.Into(unequal);
          const std::array message{
              Text(unequal_builder, "a slice of a queue holding "),
              Decimal(code, unequal_builder, named),
              Text(
                  unequal_builder, " elements cannot be assigned an array of "),
              Decimal(code, unequal_builder, given)};
          const mir::TypeId text = Unit().builtins.string;
          const mir::LocalId joined = Declared(
              code, unequal_builder, text,
              BuildStringJoin(Unit(), unequal_builder, message));
          Statement(
              unequal_builder,
              unequal_builder.Add(
                  mir::MakeAssertExpr(
                      BuildMachineBool(Unit(), unequal_builder, false),
                      unequal_builder.Add(
                          mir::Expr{
                              .data =
                                  mir::CallExpr{
                                      .callee =
                                          mir::Direct{
                                              .target = support::BuiltinFn::
                                                  kStringCStr,
                                              .receiver =
                                                  unequal_builder.LocalPlace(
                                                      joined, text)},
                                      .arguments = {}},
                              .type = Unit().builtins.machine_c_string}),
                      Unit().builtins.void_type)));
          block.AppendIfThen(
              mir::BuildMachineIntComparison(
                  Unit(), block, mir::BinaryOp::kInequality,
                  block.ReadLocal(named, machine),
                  block.ReadLocal(given, machine)),
              std::move(unequal));
          const ChangedRange changed = NoneChanged(code, block);
          ForEachIndex(
              code, block, block.ReadLocal(named, machine),
              [&](mir::Builder body, mir::LocalId index) {
                const mir::LocalId target = Declared(
                    code, body, position_type,
                    PositionSum(
                        body, body.ReadLocal(window.first, position_type),
                        PositionOf(body, index)));
                WrittenWhereItDiffers(
                    code, body, p[0], target,
                    [&](mir::Builder in) {
                      return GetUnchecked(
                          in, Referent(in, p[3], values), index);
                    },
                    changed);
              });
          block.AppendStmt(
              mir::ReturnStmt{.value = ChangedAnswer(block, changed)});
        });
  }

  // LRM 11.4.14.4: the stream of a window of an array of this type, the
  // elements from a start, a position, as many as a machine count says, laid
  // end to end, the one at the start most significant and each the element
  // default where it lies outside the array.
  auto WindowToBitVec() -> mir::CallableCode {
    const mir::TypeId run = Unit().builtins.bit_vec;
    const mir::TypeId machine = Unit().builtins.machine_int64;
    const mir::TypeId position_type = mir::PositionType(Unit().types);
    return Defined(
        Declaration(support::BuiltinFn::kWindowToBitVec),
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          const auto taken = [&](mir::Builder at) {
            return at.AsValue(Unit().types, Referent(at, p[2], machine));
          };
          const auto windowed = [&](mir::Builder at, mir::LocalId index) {
            return Element(
                at, Read(at, p[0]),
                at.Add(
                    mir::MakeBinary(
                        Unit(), at.Body(), mir::BinaryOp::kAdd,
                        at.AsValue(
                            Unit().types, Referent(at, p[1], position_type)),
                        PositionOf(at, index), position_type)));
          };
          const mir::LocalId width = Declared(
              code, block, machine,
              mir::BuildMachineIntLiteral(Unit(), block, 0));
          ForEachIndex(
              code, block, taken(block),
              [&](mir::Builder body, mir::LocalId index) {
                Statement(
                    body,
                    body.Add(
                        mir::MakeAssignExpr(
                            Unit().builtins, body.LocalPlace(width, machine),
                            mir::BuildMachineIntBinary(
                                Unit(), body, mir::BinaryOp::kAdd,
                                body.ReadLocal(width, machine),
                                BuildToInt64Call(
                                    Unit(), body,
                                    BuildBitWidth(
                                        Unit(), body,
                                        windowed(body, index)))))));
              });
          const mir::LocalId bits = Declared(
              code, block, run,
              BuildRunOfZeros(Unit(), block, block.ReadLocal(width, machine)));
          const mir::LocalId at = Declared(
              code, block, machine,
              mir::BuildMachineIntLiteral(Unit(), block, 0));
          ForEachIndex(
              code, block, taken(block),
              [&](mir::Builder body, mir::LocalId index) {
                auto element =
                    BuildToBitstream(Unit(), body, windowed(body, index), {});
                if (!element) {
                  throw InternalError(
                      "hir_to_mir: an element of an array holding a stream "
                      "has none -- please report this as a bug");
                }
                const mir::LocalId part = Declared(
                    code, body, run,
                    BuildRunOf(
                        Unit(), body, body.AsValue(Unit().types, *element)));
                InsertIntoRun(
                    Unit(), body, bits, body.ReadLocal(at, machine),
                    body.ReadLocal(part, run));
                Statement(
                    body, body.Add(
                              mir::MakeAssignExpr(
                                  Unit().builtins, body.LocalPlace(at, machine),
                                  mir::BuildMachineIntBinary(
                                      Unit(), body, mir::BinaryOp::kAdd,
                                      body.ReadLocal(at, machine),
                                      BuildRunLength(
                                          Unit(), body,
                                          body.ReadLocal(part, run))))));
              });
          block.AppendStmt(
              mir::ReturnStmt{.value = block.ReadLocal(bits, run)});
        });
  }

  // LRM 11.4.14.4: an array of this type with a window of its elements, from
  // a start, a position, as many as a machine count says, taking the values a
  // dynamic array of the element type holds, in order. A fixed-size array
  // holds every position it has, so a window reaching outside it is the
  // design's own error; a dynamic array or a queue is first grown to reach
  // the window, each element it grows by the element default, and a queue is
  // then held to its bound (LRM 7.10.5).
  auto WithWindow() -> mir::CallableCode {
    const mir::TypeId machine = Unit().builtins.machine_int64;
    const mir::TypeId position_type = mir::PositionType(Unit().types);
    const mir::TypeId values = mir::DynamicArrayOf(Unit().types, ElementType());
    return Defined(
        Declaration(support::BuiltinFn::kWithWindow),
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          const auto start = [&](mir::Builder at) {
            return at.AsValue(Unit().types, Referent(at, p[1], position_type));
          };
          const auto taken = [&](mir::Builder at) {
            return CountAsIntegral(
                at, at.AsValue(Unit().types, Referent(at, p[2], machine)),
                position_type);
          };
          const auto compared = [&](mir::Builder at, mir::BinaryOp op,
                                    mir::ExprId lhs, mir::ExprId rhs) {
            return ReduceToCondition(
                Unit(), at,
                at.Add(
                    mir::MakeBinary(
                        Unit(), at.Body(), op, lhs, rhs,
                        mir::OneBitAnswerType(
                            Unit(), std::array{position_type}))));
          };
          const auto reached = [&](mir::Builder at) {
            return at.Add(
                mir::MakeBinary(
                    Unit(), at.Body(), mir::BinaryOp::kAdd, start(at),
                    taken(at), position_type));
          };
          const auto checked = [&](const FixedSizeKind& fixed) {
            const mir::ExprId held =
                BuildConstantPosition(Unit(), block, fixed.size);
            const std::array within{
                compared(
                    block, mir::BinaryOp::kGreaterEqual, start(block),
                    BuildConstantPosition(Unit(), block, 0)),
                compared(
                    block, mir::BinaryOp::kLessEqual, reached(block), held)};
            const std::array allowed{
                compared(
                    block, mir::BinaryOp::kLessEqual, taken(block),
                    BuildConstantPosition(Unit(), block, 0)),
                AllHold(Unit(), block, within)};
            mir::AppendRunTimeCheck(
                Unit(), block, AnyHolds(Unit(), block, allowed),
                base::DiagMessage(
                    "the range a 'with' names reaches outside the array it "
                    "is unpacked into")
                    .TakeText());
            return Declared(
                code, block, WholeType(),
                block.AsValue(Unit().types, Read(block, p[0])));
          };
          const auto grown = [&] {
            const mir::LocalId length = Declared(
                code, block, position_type, Count(block, Read(block, p[0])));
            mir::Block grows;
            mir::Builder grows_builder = block.Into(grows);
            Statement(
                grows_builder,
                grows_builder.Add(
                    mir::MakeAssignExpr(
                        Unit().builtins,
                        grows_builder.LocalPlace(length, position_type),
                        reached(grows_builder))));
            // A window of no elements names none, wherever it starts, so it
            // grows nothing.
            const std::array reaches_past{
                compared(
                    block, mir::BinaryOp::kGreaterThan, taken(block),
                    BuildConstantPosition(Unit(), block, 0)),
                compared(
                    block, mir::BinaryOp::kGreaterEqual, start(block),
                    BuildConstantPosition(Unit(), block, 0)),
                compared(
                    block, mir::BinaryOp::kGreaterThan, reached(block),
                    block.ReadLocal(length, position_type))};
            block.AppendIfThen(
                AllHold(Unit(), block, reaches_past), std::move(grows));
            return Declared(
                code, block, WholeType(),
                KeptAndGrown(
                    code, block, p[0],
                    BuildToInt64Call(
                        Unit(), block,
                        block.ReadLocal(length, position_type))));
          };
          const mir::LocalId out = std::visit(
              Overloaded{
                  checked, [&](const DynamicArrayKind&) { return grown(); },
                  [&](const QueueKind&) { return grown(); }},
              kind_);
          ForEachIndex(
              code, block,
              block.AsValue(Unit().types, Referent(block, p[2], machine)),
              [&](mir::Builder body, mir::LocalId index) {
                mir::Block named;
                mir::Builder named_builder = body.Into(named);
                Statement(
                    named_builder,
                    named_builder.Add(BuildStoreExpr(
                        Unit(), named_builder,
                        AccessPath{
                            .owner = named_builder.LocalPlace(out, WholeType()),
                            .descent = {DescentStep{
                                .to =
                                    StepToElement{
                                        .position = named_builder.Add(
                                            mir::MakeBinary(
                                                Unit(), named_builder.Body(),
                                                mir::BinaryOp::kAdd,
                                                start(named_builder),
                                                PositionOf(
                                                    named_builder, index),
                                                position_type))},
                                .part_type = ElementType()}}},
                        named_builder.AsValue(
                            Unit().types,
                            Element(
                                named_builder,
                                Referent(named_builder, p[3], values),
                                PositionOf(named_builder, index))))));
                body.AppendIfThen(
                    compared(
                        body, mir::BinaryOp::kGreaterEqual, start(body),
                        BuildConstantPosition(Unit(), body, 0)),
                    std::move(named));
              });
          block.AppendStmt(
              mir::ReturnStmt{.value = Answered(code, block, out)});
        });
  }

  // LRM 7.12.2: the elements of an array of this type put in an order, the
  // one at each position becoming the one that was at the position a dynamic
  // array of positions, a sort's answer, holds there. Every element is read
  // before any is written: a dynamic array builds a new generation of them,
  // and a fixed-size one reads them out of a copy of itself. A queue's core
  // reorders where its elements lie, so none moves.
  auto ApplyOrder() -> mir::CallableCode {
    const mir::TypeId position_type = mir::PositionType(Unit().types);
    const mir::TypeId order = mir::SortOrderType(Unit().types);
    return Defined(
        Declaration(support::BuiltinFn::kApplyOrder),
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          // The position the order holds at `position`, which is of its own
          // element type, read as a position.
          const mir::TypeId held_position =
              Unit().types.Get(order).ContainerElementType().value();
          const auto from = [&](mir::Builder at, mir::ExprId position) {
            return at.Add(
                mir::MakeIntegralConversion(
                    Unit(), at.Body(),
                    at.AsValue(
                        Unit().types,
                        at.Add(StepForRead(
                            Unit(), at,
                            DescentStep{
                                .to = StepToElement{.position = position},
                                .part_type = held_position},
                            Referent(at, p[1], order)))),
                    position_type));
          };
          const auto reordered = [&](const QueueKind&) {
            Statement(
                block,
                Core(
                    block, support::BuiltinFn::kQueueApplyOrder,
                    Written(block, p[0]),
                    {mir::Borrow(Unit(), block, Referent(block, p[1], order))},
                    Unit().builtins.void_type));
          };
          const auto regenerated = [&](const DynamicArrayKind&) {
            Regenerate(
                code, block, ParameterArray(p[0], mir::Mutability::kMutable),
                ElementCount(Unit(), block, WholeType(), Written(block, p[0])),
                [&](mir::Builder body, mir::LocalId index) {
                  return body.AsValue(
                      Unit().types, Element(
                                        body, Written(body, p[0]),
                                        from(body, PositionOf(body, index))));
                });
          };
          const auto permuted = [&](const FixedSizeKind& fixed) {
            const mir::LocalId held = Declared(
                code, block, WholeType(),
                block.AsValue(Unit().types, Written(block, p[0])));
            ForEachIndex(
                code, block,
                CountLiteral(block, static_cast<std::uint64_t>(fixed.size)),
                [&](mir::Builder body, mir::LocalId index) {
                  StoredUnchecked(
                      body, Written(body, p[0]), index,
                      body.AsValue(
                          Unit().types,
                          Element(
                              body, body.LocalPlace(held, WholeType()),
                              from(body, PositionOf(body, index)))));
                });
          };
          std::visit(Overloaded{permuted, regenerated, reordered}, kind_);
          block.AppendStmt(mir::ReturnStmt{.value = std::nullopt});
        });
  }

  // LRM 7.12.2 `reverse`: the elements of an array of this type in the
  // opposite order, each pair from the two ends exchanged where it lies; a
  // queue's core reverses where its elements lie, so none moves.
  auto Reverse() -> mir::CallableCode {
    const mir::TypeId machine = Unit().builtins.machine_int64;
    return Defined(
        Declaration(support::BuiltinFn::kReverse),
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          const auto reversed_in_place = [&](const QueueKind&) {
            Statement(
                block,
                Core(
                    block, support::BuiltinFn::kQueueReverse,
                    Written(block, p[0]), {}, Unit().builtins.void_type));
          };
          const auto exchanged = [&] {
            const mir::LocalId last = Declared(
                code, block, machine,
                mir::BuildMachineIntBinary(
                    Unit(), block, mir::BinaryOp::kSub,
                    ElementCount(
                        Unit(), block, WholeType(), Written(block, p[0])),
                    mir::BuildMachineIntLiteral(Unit(), block, 1)));
            const mir::ExprId pairs = mir::BuildMachineIntBinary(
                Unit(), block, mir::BinaryOp::kDiv,
                mir::BuildMachineIntBinary(
                    Unit(), block, mir::BinaryOp::kAdd,
                    block.ReadLocal(last, machine),
                    mir::BuildMachineIntLiteral(Unit(), block, 1)),
                mir::BuildMachineIntLiteral(Unit(), block, 2));
            ForEachIndex(
                code, block, pairs, [&](mir::Builder body, mir::LocalId low) {
                  const mir::LocalId high = Declared(
                      code, body, machine,
                      mir::BuildMachineIntBinary(
                          Unit(), body, mir::BinaryOp::kSub,
                          body.ReadLocal(last, machine), IndexNow(body, low)));
                  const auto array = [&] { return Written(body, p[0]); };
                  const mir::LocalId held = Declared(
                      code, body, ElementType(),
                      body.AsValue(
                          Unit().types, GetUnchecked(body, array(), low)));
                  StoredUnchecked(
                      body, array(), low,
                      body.AsValue(
                          Unit().types, GetUnchecked(body, array(), high)));
                  StoredUnchecked(
                      body, array(), high, body.ReadLocal(held, ElementType()));
                });
          };
          std::visit(
              Overloaded{
                  [&](const FixedSizeKind&) { exchanged(); },
                  [&](const DynamicArrayKind&) { exchanged(); },
                  reversed_in_place},
              kind_);
          block.AppendStmt(mir::ReturnStmt{.value = std::nullopt});
        });
  }

 private:
  [[nodiscard]] auto ElementType() const -> mir::TypeId {
    return Members().front();
  }

  // How many elements an array of this type holds, where its type fixes it,
  // which only a fixed-size array's own operations ask (LRM 7.4).
  [[nodiscard]] auto FixedCount() const -> std::uint64_t {
    const auto* fixed = std::get_if<FixedSizeKind>(&kind_);
    if (fixed == nullptr) {
      throw InternalError(
          "hir_to_mir: the count a fixed-size array's type fixes is asked of "
          "an array of another kind -- please report this as a bug");
    }
    return static_cast<std::uint64_t>(fixed->size);
  }

  // The fixed-size array whose element at every machine index `each(body,
  // index)` builds, `index` the local holding it, written into a local
  // starting at the array's default.
  template <typename Each>
  auto Assembled(mir::CallableCode& code, mir::Builder block, Each each)
      -> mir::ExprId {
    const mir::LocalId built = Declared(
        code, block, WholeType(),
        block.Add(BuildDefaultValueExpr(Unit(), block, WholeType())));
    ForEachIndex(
        code, block, CountLiteral(block, FixedCount()),
        [&](mir::Builder body, mir::LocalId index) {
          const mir::ExprId element = each(body, index);
          StoredUnchecked(
              body, body.LocalPlace(built, WholeType()), index, element);
        });
    return block.ReadLocal(built, WholeType());
  }

  // The stream each element makes, and where the one at the machine index the
  // local `index` holds starts in the array's stream: the element at index 0
  // is the most significant.
  [[nodiscard]] auto ElementStream() const -> mir::StreamShape {
    return *mir::FixedStreamShapeOf(Unit(), ElementType());
  }
  [[nodiscard]] auto Stream() const -> mir::TypeId {
    const mir::StreamShape shape =
        *mir::FixedStreamShapeOf(Unit(), WholeType());
    return mir::PackedVectorOf(Unit().types, shape.width, shape.state_kind);
  }
  auto ElementStart(mir::Builder block, mir::LocalId index) -> mir::ExprId {
    const auto width = static_cast<std::int64_t>(ElementStream().width);
    const mir::ExprId offset = mir::BuildMachineIntBinary(
        Unit(), block, mir::BinaryOp::kMul, IndexNow(block, index),
        mir::BuildMachineIntLiteral(Unit(), block, width));
    return CountAsIntegral(
        block,
        mir::BuildMachineIntBinary(
            Unit(), block, mir::BinaryOp::kSub,
            mir::BuildMachineIntLiteral(
                Unit(), block,
                (static_cast<std::int64_t>(FixedCount()) - 1) * width),
            offset),
        mir::PositionType(Unit().types));
  }
  // The bits of the stream one element takes.
  [[nodiscard]] auto ElementBits() const -> mir::TypeId {
    const mir::StreamShape element = ElementStream();
    return mir::PackedVectorOf(Unit().types, element.width, element.state_kind);
  }

  // LRM 6.24.3: a fixed-size array's elements' own streams laid end to end,
  // the one at position 0 most significant.
  auto ToBitstream() -> mir::CallableCode {
    const mir::TypeId stream = Stream();
    return BodyOf(
        support::StreamItem::kToBitstream,
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          const mir::LocalId bits = Declared(
              code, block, stream,
              mir::BuildIntegralLiteral(
                  Unit(), block, stream, mir::IntegralConstant{}));
          ForEachIndex(
              code, block, CountLiteral(block, FixedCount()),
              [&](mir::Builder body, mir::LocalId index) {
                auto element = BuildToBitstream(
                    Unit(), body, GetUnchecked(body, Read(body, p[0]), index),
                    {});
                if (!element) {
                  throw InternalError(
                      "hir_to_mir: an element of an array whose stream is "
                      "fixed has none -- please report this as a bug");
                }
                const mir::ExprId owner = body.LocalPlace(bits, stream);
                const mir::ExprId start = ElementStart(body, index);
                const mir::ExprId value = body.AsValue(Unit().types, *element);
                Statement(
                    body, body.Add(BuildStoreExpr(
                              Unit(), body,
                              AccessPath{
                                  .owner = owner,
                                  .descent = {DescentStep{
                                      .to = StepToBits{.start = start},
                                      .part_type = ElementBits()}}},
                              value)));
              });
          return block.ReadLocal(bits, stream);
        });
  }

  // The inverse: each element reads its own width off the stream, the first
  // from the most significant end (LRM 11.4.14.3).
  auto FromBitstream() -> mir::CallableCode {
    const mir::TypeId stream = Stream();
    return BodyOf(
        support::StreamItem::kFromBitstream,
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          return Assembled(
              code, block, [&](mir::Builder body, mir::LocalId index) {
                const mir::ExprId start = ElementStart(body, index);
                const mir::ExprId source = Referent(body, p[0], stream);
                const mir::ExprId bits = body.Add(StepRead(
                    Unit(), body,
                    DescentStep{
                        .to = StepToBits{.start = start},
                        .part_type = ElementBits()},
                    source));
                return BuildFromBitstream(Unit(), body, bits, ElementType());
              });
        });
  }

  // The inverse of a fixed-size array's run of bits, out of a run holding
  // exactly the array's bits: each element takes what the elements after it
  // leave where they are empty, so the first takes the rest (LRM 11.4.14.4).
  auto FixedFromBitVec() -> mir::CallableCode {
    const mir::TypeId run = Unit().builtins.bit_vec;
    const std::uint64_t empty = StreamFillOf(Unit(), ElementType()).empty_width;
    return BodyOf(
        support::StreamItem::kFromBitVec,
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          const mir::LocalId length = DeclaredCount(
              code, block,
              BuildRunLength(Unit(), block, Referent(block, p[0], run)));
          const mir::LocalId at =
              DeclaredCount(code, block, CountLiteral(block, 0));
          const mir::LocalId after = DeclaredCount(
              code, block, CountLiteral(block, (FixedCount() - 1) * empty));
          return Assembled(code, block, [&](mir::Builder body, mir::LocalId) {
            const mir::LocalId allotted =
                PartAllotment(code, body, length, at, CountNow(body, after));
            const mir::ExprId source = Referent(body, p[0], run);
            const mir::ExprId start = CountNow(body, at);
            const mir::ExprId width = CountNow(body, allotted);
            const mir::ExprId element = Held(
                code, body, ElementType(),
                BuildFromBitVec(
                    Unit(), body,
                    BuildRunExtract(Unit(), body, source, start, width),
                    ElementType()));
            const mir::ExprId at_now = CountNow(body, at);
            const mir::ExprId taken = CountNow(body, allotted);
            SetCount(
                body, at,
                mir::BuildMachineIntBinary(
                    Unit(), body, mir::BinaryOp::kAdd, at_now, taken));
            const mir::ExprId after_now = CountNow(body, after);
            const mir::ExprId element_empty = CountLiteral(body, empty);
            SetCount(
                body, after,
                mir::BuildMachineIntBinary(
                    Unit(), body, mir::BinaryOp::kSub, after_now,
                    element_empty));
            return element;
          });
        });
  }

  // Where the positions of the element at the machine index the local `index`
  // holds start among the fixed-size array's, as a position: the element at
  // index 0 first.
  auto ElementPositionsStart(mir::Builder block, mir::LocalId index)
      -> mir::ExprId {
    return CountAsIntegral(
        block,
        mir::BuildMachineIntBinary(
            Unit(), block, mir::BinaryOp::kMul, IndexNow(block, index),
            CountLiteral(block, mir::PositionCountOf(Unit(), ElementType()))),
        mir::PositionType(Unit().types));
  }

  // A fixed-size array's positions are its elements', the one at position 0
  // first.
  auto ToPositions() -> mir::CallableCode {
    const mir::TypeId positions_type =
        mir::PositionsTypeOf(Unit(), WholeType());
    return BodyOf(
        support::PositionsItem::kToPositions,
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          const mir::LocalId positions = Declared(
              code, block, positions_type,
              block.Add(BuildDefaultValueExpr(Unit(), block, positions_type)));
          ForEachIndex(
              code, block, CountLiteral(block, FixedCount()),
              [&](mir::Builder body, mir::LocalId index) {
                LayPositions(
                    body, positions, positions_type,
                    ElementPositionsStart(body, index),
                    PartPositions(
                        body, ElementType(),
                        GetUnchecked(body, Read(body, p[0]), index)),
                    mir::PositionsTypeOf(Unit(), ElementType()));
              });
          return block.ReadLocal(positions, positions_type);
        });
  }

  // The fixed-size array whose elements' positions an integral value of its
  // positions holds, the one at position 0 first.
  auto FromPositions() -> mir::CallableCode {
    const mir::TypeId positions_type =
        mir::PositionsTypeOf(Unit(), WholeType());
    return BodyOf(
        support::PositionsItem::kFromPositions,
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          return Assembled(
              code, block, [&](mir::Builder body, mir::LocalId index) {
                const mir::ExprId bits = body.Add(StepRead(
                    Unit(), body,
                    DescentStep{
                        .to =
                            StepToBits{
                                .start = ElementPositionsStart(body, index)},
                        .part_type =
                            mir::PositionsTypeOf(Unit(), ElementType())},
                    Referent(body, p[0], positions_type)));
                return body.Add(
                    BuildFromPositions(Unit(), body, bits, ElementType()));
              });
        });
  }

  // Only the elements the view reaches are written: the loop starts at the
  // element holding the first position the view reaches, found by dividing,
  // and ends after the one holding its last, so a view over one element costs
  // that element whatever the array's size.
  auto WritePositions() -> mir::CallableCode {
    return BodyOf(
        support::PositionsItem::kWritePositions,
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          const PositionsWrite write = HoldPositionsWrite(code, block, p);
          const std::uint64_t per_element =
              mir::PositionCountOf(Unit(), ElementType());
          const mir::TypeId position_type = mir::PositionType(Unit().types);
          const mir::ExprId reached_from = mir::BuildMachineIntMax(
              Unit(), block, CountNow(block, write.start),
              CountLiteral(block, 0));
          const mir::ExprId first_element = mir::BuildMachineIntBinary(
              Unit(), block, mir::BinaryOp::kDiv, reached_from,
              CountLiteral(block, per_element));
          const mir::ExprId view_end = mir::BuildMachineIntBinary(
              Unit(), block, mir::BinaryOp::kAdd, CountNow(block, write.start),
              CountNow(block, write.count));
          const mir::LocalId past_last_element = DeclaredCount(
              code, block,
              mir::BuildMachineIntBinary(
                  Unit(), block, mir::BinaryOp::kDiv,
                  mir::BuildMachineIntBinary(
                      Unit(), block, mir::BinaryOp::kAdd, view_end,
                      CountLiteral(block, per_element - 1)),
                  CountLiteral(block, per_element)));
          ForEachIndexBetween(
              code, block, first_element,
              mir::BuildMachineIntMin(
                  Unit(), block, CountNow(block, past_last_element),
                  CountLiteral(block, FixedCount())),
              [&](mir::Builder body, mir::LocalId index) {
                // The element's position, held so that the read of the
                // element and the write back both reach it without computing
                // it twice.
                const mir::LocalId position = Declared(
                    code, body, position_type, PositionOf(body, index));
                PartWritten(
                    code, body, ElementType(),
                    AccessPath{
                        .owner = Written(body, write.written),
                        .descent = {DescentStep{
                            .to =
                                StepToElement{
                                    .position = body.ReadLocal(
                                        position, position_type)},
                            .part_type = ElementType()}}},
                    mir::BuildMachineIntBinary(
                        Unit(), body, mir::BinaryOp::kMul,
                        IndexNow(body, index), CountLiteral(body, per_element)),
                    write);
              });
          return block.ReadLocal(write.changed, Unit().builtins.machine_bool);
        });
  }

  // A fixed-size array's positions are its elements', the one at position 0
  // first; a run-time-sized array is one position, held whole, so a run naming
  // any of it takes all of it.
  auto TakePositions() -> mir::CallableCode {
    return BodyOf(
        support::PositionsItem::kTakePositions,
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          if (!IsFixedSize()) {
            return block.AsValue(Unit().types, Read(block, p[1]));
          }
          const HeldRun run = HoldRun(code, block, p[2], p[3]);
          const std::uint64_t per_element =
              mir::PositionCountOf(Unit(), ElementType());
          return Assembled(
              code, block, [&](mir::Builder body, mir::LocalId index) {
                const mir::ExprId into =
                    GetUnchecked(body, Read(body, p[0]), index);
                const mir::ExprId from =
                    GetUnchecked(body, Read(body, p[1]), index);
                const mir::ExprId stride = CountLiteral(body, per_element);
                const mir::ExprId start = mir::BuildMachineIntBinary(
                    Unit(), body, mir::BinaryOp::kMul, IndexNow(body, index),
                    stride);
                return PartTaken(
                    code, body, ElementType(), into, from, start, run);
              });
        });
  }

  auto Step(support::ValueStep step) -> mir::CallableCode {
    switch (step) {
      case support::ValueStep::kCopy:
        return Copy();
      // A relocation is no operation of a type's: it moves a value's bytes
      // whatever the type.
      case support::ValueStep::kRelocate:
        throw InternalError(
            "hir_to_mir: a run-time-sized array is asked a relocation as an "
            "operation of its type -- please report this as a bug");
      case support::ValueStep::kAssign:
        return Assign();
      case support::ValueStep::kEnd:
        return End();
    }
    throw InternalError("hir_to_mir: unknown value step");
  }

  // Whether this is a fixed-size array, whose count its type fixes, rather
  // than a run-time-sized one, whose core holds its count (LRM 7.4, 7.5,
  // 7.10): a default, and a value read out of a run of bits, are each kind's
  // own.
  [[nodiscard]] auto IsFixedSize() const -> bool {
    return std::holds_alternative<FixedSizeKind>(kind_);
  }

  // One of a kind's own methods and how its body is built, as a row of the
  // kind's inherent impl: each kind's table names exactly the methods MIR
  // declares for the kind, which a check at compile time holds it to.
  struct MethodBody {
    support::BuiltinFn name;
    mir::CallableCode (*build)(
        ArrayBodies& bodies, const mir::TypeOperationSource& source);
  };

  // The row of this kind's table building the method `name`, which this
  // kind's interface declares.
  [[nodiscard]] auto MethodBodyOf(support::BuiltinFn name) const
      -> const MethodBody&;

  // What the parameter `local` refers to, an array of this type, as the place
  // it is.
  auto Array(mir::Builder block, mir::LocalId local, mir::Mutability mutability)
      -> mir::ExprId {
    const mir::TypeId reference = Unit().types.Intern(
        mir::Type{mir::ReferenceType{
            .referent = WholeType(), .mutability = mutability}});
    return block.Deref(
        Unit().types, block.LocalPlace(local, reference), WholeType());
  }
  auto Read(mir::Builder block, mir::LocalId local) -> mir::ExprId {
    return Array(block, local, mir::Mutability::kReadOnly);
  }
  auto Written(mir::Builder block, mir::LocalId local) -> mir::ExprId {
    return Array(block, local, mir::Mutability::kMutable);
  }

  // A call of the core's member `fn` on the array `array` lies in.
  static auto Core(
      mir::Builder block, support::BuiltinFn fn, mir::ExprId array,
      std::vector<mir::ExprId> arguments, mir::TypeId result) -> mir::ExprId {
    return block.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee = mir::Direct{.target = fn, .receiver = array},
                    .arguments = std::move(arguments)},
            .type = result});
  }
  static void Statement(mir::Builder block, mir::ExprId expr) {
    block.AppendStmt(mir::ExprStmt{.expr = expr});
  }

  // Whether the array's stream is one this lowering builds. An element holding
  // a class handle streams the object the handle refers to (LRM 6.24.3), which
  // is not yet supported, so each question about such an array's stream is a
  // body telling the design so where it is asked, answering `answer`.
  [[nodiscard]] auto StreamsItsElements() const -> bool {
    return !mir::HoldsClassHandle(Unit(), ElementType());
  }
  void RefuseTheStream(mir::Builder block, mir::ExprId answer) {
    mir::AppendRunTimeCheck(
        Unit(), block, BuildMachineBool(Unit(), block, false),
        "streaming the object a class handle refers to is not yet supported");
    block.AppendStmt(mir::ReturnStmt{.value = answer});
  }

  // An array of no elements of this type.
  auto Empty(mir::Builder block) -> mir::ExprId {
    return Constructed(block, WholeType());
  }
  // An empty value of `type`, a run-time-sized array: a construction of the
  // type, which is its type's default.
  static auto Constructed(mir::Builder block, mir::TypeId type) -> mir::ExprId {
    return block.Add(
        mir::Expr{
            .data = mir::CallExpr{.callee = mir::Construct{}, .arguments = {}},
            .type = type});
  }

  // A comparison of two positions, as a condition; their sum; and one of two
  // as a condition chooses.
  auto PositionCompare(
      mir::Builder block, mir::BinaryOp op, mir::ExprId lhs, mir::ExprId rhs)
      -> mir::ExprId {
    return ReduceToCondition(
        Unit(), block,
        block.Add(
            mir::MakeBinary(
                Unit(), block.Body(), op, lhs, rhs,
                mir::OneBitAnswerType(
                    Unit(), std::array{mir::PositionType(Unit().types)}))));
  }
  auto PositionSum(mir::Builder block, mir::ExprId lhs, mir::ExprId rhs)
      -> mir::ExprId {
    return block.Add(
        mir::MakeBinary(
            Unit(), block.Body(), mir::BinaryOp::kAdd, lhs, rhs,
            mir::PositionType(Unit().types)));
  }
  auto PositionChoice(
      mir::Builder block, mir::ExprId condition, mir::ExprId chosen,
      mir::ExprId otherwise) -> mir::ExprId {
    return block.Add(
        mir::Expr{
            .data =
                mir::ConditionalExpr{
                    .condition = condition,
                    .then_value = chosen,
                    .else_value = otherwise},
            .type = mir::PositionType(Unit().types)});
  }
  // Whether a position holds no x or z, so names a position at all.
  auto Known(mir::Builder block, mir::ExprId position) -> mir::ExprId {
    return block.Add(
        mir::MakeUnary(
            Unit(), block.Body(), mir::UnaryOp::kLogicalNot,
            BuildValueOperation(
                Unit(), block, support::UnknownItem::kHasUnknown, position, {},
                Unit().builtins.machine_bool),
            Unit().builtins.machine_bool));
  }

  // A queue of `type` holding `count` elements, a machine count, the one at
  // each machine index built as `value(body, index)` makes it, `index` the
  // local holding it, in storage its core hands out, kept in a local of the
  // body; none of them is held to the bound yet.
  template <typename Value>
  auto QueueFilled(
      mir::CallableCode& code, mir::Builder block, mir::TypeId type,
      mir::ExprId count, Value value) -> mir::LocalId {
    const mir::TypeId nothing = Unit().builtins.void_type;
    const mir::LocalId queue =
        Declared(code, block, type, Constructed(block, type));
    const mir::LocalId held_count = DeclaredCount(code, block, count);
    Statement(
        block, Core(
                   block, support::BuiltinFn::kQueueReserve,
                   block.LocalPlace(queue, type), {CountNow(block, held_count)},
                   nothing));
    ForEachIndex(
        code, block, CountNow(block, held_count),
        [&](mir::Builder body, mir::LocalId index) {
          Statement(
              body, Core(
                        body, support::BuiltinFn::kQueueCommit,
                        body.LocalPlace(queue, type),
                        {IndexNow(body, index),
                         MadeInQueueStorage(
                             Unit(), body, body.LocalPlace(queue, type),
                             value(body, index), ElementPointer())},
                        nothing));
        });
    return queue;
  }

  // The queue's own allocation of storage for an element.
  auto Allocate() -> mir::CallableCode {
    return Defined(
        Declaration(support::BuiltinFn::kQueueAllocate),
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          const mir::LocalId storage =
              AllocatedStorage(code, block, ParameterQueue(p[0]));
          block.AppendStmt(
              mir::ReturnStmt{
                  .value = CastTo(
                      block, block.ReadLocal(storage, ElementStoragePointer()),
                      ElementPointer())});
        });
  }

  // Where an array's own code finds the array it acts on, a place made afresh
  // for each use in a block: what a parameter refers to, at a mutability, or a
  // local of the body.
  using PlaceAt = std::function<mir::ExprId(mir::Builder)>;
  auto ParameterArray(mir::LocalId parameter, mir::Mutability mutability)
      -> PlaceAt {
    return [this, parameter, mutability](mir::Builder at) {
      return Array(at, parameter, mutability);
    };
  }
  auto ParameterQueue(mir::LocalId parameter) -> PlaceAt {
    return ParameterArray(parameter, mir::Mutability::kMutable);
  }
  auto LocalArray(mir::LocalId local) -> PlaceAt {
    return [this, local](mir::Builder at) {
      return at.LocalPlace(local, WholeType());
    };
  }

  // What a queue's own code reaches of the record its core is (LRM 7.10): a
  // member of it, as a place; one of its counts as a machine integer, and one
  // set to such a number; and the element storage at the ring's entry `entry`,
  // an entry of the ring's length. Each element lies in storage of its own,
  // its lease count after it (LRM 13.5.2), as the core lays one out.
  template <typename Member>
  auto CoreMember(
      mir::Builder block, const PlaceAt& queue,
      Member value::QueueCore::* member) -> mir::ExprId {
    return mir::LibraryRecordMemberPlace(Unit(), block, queue(block), member);
  }
  auto CoreCount(
      mir::Builder block, const PlaceAt& queue,
      std::uint64_t value::QueueCore::* member) -> mir::ExprId {
    return AsMachine(block, block.Read(CoreMember(block, queue, member)));
  }
  void SetCoreCount(
      mir::Builder block, const PlaceAt& queue,
      std::uint64_t value::QueueCore::* member, mir::ExprId number) {
    const mir::ExprId place = CoreMember(block, queue, member);
    Statement(
        block, block.Add(
                   mir::MakeAssignExpr(
                       Unit().builtins, place,
                       CastTo(block, number, block.Get(place).type))));
  }
  auto RingEntry(mir::Builder block, const PlaceAt& queue, mir::ExprId entry)
      -> mir::ExprId {
    const mir::TypeId ring =
        mir::LibraryRecordMemberType(Unit(), &value::QueueCore::ring);
    const mir::ExprId at = block.Add(
        mir::MakeBinary(
            Unit(), block.Body(), mir::BinaryOp::kAdd,
            block.Read(CoreMember(block, queue, &value::QueueCore::ring)),
            entry, ring));
    return block.Deref(Unit().types, at, Address());
  }
  // The ring's entry the element at `position` lies in.
  auto EntryOf(mir::Builder block, const PlaceAt& queue, mir::ExprId position)
      -> mir::ExprId {
    return mir::BuildMachineIntBinary(
        Unit(), block, mir::BinaryOp::kBitwiseAnd,
        mir::BuildMachineIntBinary(
            Unit(), block, mir::BinaryOp::kAdd,
            CoreCount(block, queue, &value::QueueCore::head), position),
        CoreCount(block, queue, &value::QueueCore::mask));
  }

  // An untyped address, the storage of one of this queue's elements with its
  // lease count, and a pointer to that storage; a value read as another type
  // of the same machine kind.
  [[nodiscard]] auto Address() const -> mir::TypeId {
    return mir::LibraryRecordMemberType(Unit(), &value::Recycler::free_list);
  }
  [[nodiscard]] auto ElementStorageType() const -> mir::TypeId {
    return Unit().types.Intern(
        mir::Type{mir::TupleType{
            .elements = {
                ElementType(),
                mir::LibraryTypeOf(
                    Unit().types, support::LibraryDecl::kLeaseCount)}}});
  }
  [[nodiscard]] auto ElementStoragePointer() const -> mir::TypeId {
    return mir::BorrowedPointerTo(
        Unit().types, ElementStorageType(), mir::Mutability::kMutable);
  }
  static auto CastTo(mir::Builder block, mir::ExprId value, mir::TypeId type)
      -> mir::ExprId {
    return block.Add(
        mir::Expr{.data = mir::CastExpr{.operand = value}, .type = type});
  }
  auto AsMachine(mir::Builder block, mir::ExprId value) -> mir::ExprId {
    return CastTo(block, value, Unit().builtins.machine_int64);
  }
  // Whether the address `address` is no address.
  auto IsNull(mir::Builder block, mir::ExprId address) -> mir::ExprId {
    return block.Add(
        mir::Expr{
            .data =
                mir::BinaryExpr{
                    .op = mir::BinaryOp::kEquality,
                    .lhs = address,
                    .rhs = block.Add(
                        mir::Expr{
                            .data = mir::NullLiteral{},
                            .type = block.Get(address).type})},
            .type = Unit().builtins.machine_bool});
  }

  // The element and the lease count of the storage `storage` points at, as
  // places: how many calls lease it, and whether the queue has let it go.
  auto ElementIn(mir::Builder block, mir::ExprId storage) -> mir::ExprId {
    return block.Deref(
        Unit().types, CastTo(block, storage, ElementPointer()), ElementType());
  }
  template <typename Member>
  auto LeaseIn(
      mir::Builder block, mir::ExprId storage,
      Member value::LeaseCount::* member) -> mir::ExprId {
    const mir::TypeId storage_type = ElementStorageType();
    return mir::LibraryRecordMemberPlace(
        Unit(), block,
        block.Add(
            mir::MakeFieldAccessExpr(
                block.Deref(Unit().types, storage, storage_type),
                mir::ComponentFieldTarget{.position = base::ComponentIndex{1}},
                Unit()
                    .types.Get(storage_type)
                    .Get<mir::TupleType>()
                    .elements[1])),
        member);
  }

  // Where the recycler the queue `queue` names keeps the first piece of
  // storage given back, as a place; and the type of the link a free piece
  // holds in its first word, the address of the next.
  auto FreeList(mir::Builder block, const PlaceAt& queue) -> mir::ExprId {
    const mir::TypeId recycler_type =
        mir::LibraryRecordMemberType(Unit(), &value::QueueCore::recycler);
    return mir::LibraryRecordMemberPlace(
        Unit(), block,
        block.Deref(
            Unit().types,
            block.Read(CoreMember(block, queue, &value::QueueCore::recycler)),
            Unit().types.Get(recycler_type).Get<mir::PointerType>().pointee),
        &value::Recycler::free_list);
  }
  [[nodiscard]] auto Link() const -> mir::TypeId {
    return mir::BorrowedPointerTo(
        Unit().types, Address(), mir::Mutability::kMutable);
  }

  // Storage for an element, kept in a local of the body, holding no value and
  // leased by no call, from the recycler of the queue `queue` names: what was
  // given back where there is any, and fresh storage from the run otherwise, as
  // LLVM's `Recycler::Allocate` pops its free list or asks its allocator. The
  // link a free piece holds lies over whatever it holds in use, its lease count
  // included, so the count is started where the storage is taken; and storage
  // holding no value given back to that recycler, as `Recycler::Deallocate`
  // pushes it.
  auto AllocatedStorage(
      mir::CallableCode& code, mir::Builder block, const PlaceAt& queue)
      -> mir::LocalId {
    const mir::TypeId boolean = Unit().builtins.machine_bool;
    const mir::LocalId none_given_back = Declared(
        code, block, boolean,
        IsNull(block, block.Read(FreeList(block, queue))));
    const mir::ExprId fresh = CastTo(
        block,
        block.Add(
            mir::MakeCallExpr(
                mir::Direct{.target = support::BuiltinFn::kRecyclerAllocate},
                {block.Read(
                    CoreMember(block, queue, &value::QueueCore::recycler))},
                Address())),
        ElementStoragePointer());
    const mir::ExprId given_back = CastTo(
        block, block.Read(FreeList(block, queue)), ElementStoragePointer());
    const mir::LocalId storage = Declared(
        code, block, ElementStoragePointer(),
        block.Add(
            mir::Expr{
                .data =
                    mir::ConditionalExpr{
                        .condition = block.ReadLocal(none_given_back, boolean),
                        .then_value = fresh,
                        .else_value = given_back},
                .type = ElementStoragePointer()}));
    mir::Block popped;
    mir::Builder popped_builder = block.Into(popped);
    Statement(
        popped_builder,
        popped_builder.Add(
            mir::MakeAssignExpr(
                Unit().builtins, FreeList(popped_builder, queue),
                popped_builder.Read(popped_builder.Deref(
                    Unit().types,
                    CastTo(
                        popped_builder,
                        popped_builder.ReadLocal(
                            storage, ElementStoragePointer()),
                        Link()),
                    Address())))));
    block.AppendIfThen(
        block.Add(
            mir::MakeUnary(
                Unit(), block.Body(), mir::UnaryOp::kLogicalNot,
                block.ReadLocal(none_given_back, boolean), boolean)),
        std::move(popped));
    SetLease(block, storage, &value::LeaseCount::held, 0);
    SetLease(block, storage, &value::LeaseCount::detached, 0);
    return storage;
  }
  void Deallocate(
      mir::Builder block, const PlaceAt& queue, mir::LocalId storage) {
    Statement(
        block,
        block.Add(
            mir::MakeAssignExpr(
                Unit().builtins,
                block.Deref(
                    Unit().types,
                    CastTo(
                        block,
                        block.ReadLocal(storage, ElementStoragePointer()),
                        Link()),
                    Address()),
                block.Read(FreeList(block, queue)))));
    Statement(
        block,
        block.Add(
            mir::MakeAssignExpr(
                Unit().builtins, FreeList(block, queue),
                CastTo(
                    block, block.ReadLocal(storage, ElementStoragePointer()),
                    Address()))));
  }

  // Whether a call leases the element in the storage the local `storage`
  // points at.
  auto Leased(mir::Builder block, mir::LocalId storage) -> mir::ExprId {
    return mir::BuildMachineIntComparison(
        Unit(), block, mir::BinaryOp::kInequality,
        AsMachine(
            block, block.Read(LeaseIn(
                       block, block.ReadLocal(storage, ElementStoragePointer()),
                       &value::LeaseCount::held))),
        mir::BuildMachineIntLiteral(Unit(), block, 0));
  }

  // The member `member` of the lease count of the storage the local `storage`
  // points at set to `value`; and the storage marked as one its queue let go of
  // while a call leases it, which the last lease then ends (LRM 13.5.2).
  template <typename Member>
  void SetLease(
      mir::Builder block, mir::LocalId storage,
      Member value::LeaseCount::* member, std::int64_t value) {
    const mir::ExprId place = LeaseIn(
        block, block.ReadLocal(storage, ElementStoragePointer()), member);
    Statement(
        block,
        block.Add(
            mir::MakeAssignExpr(
                Unit().builtins, place,
                CastTo(
                    block, mir::BuildMachineIntLiteral(Unit(), block, value),
                    block.Get(place).type))));
  }
  void Detach(mir::Builder block, mir::LocalId storage) {
    SetLease(block, storage, &value::LeaseCount::detached, 1);
  }

  // A loop over the machine indexes below the machine count `count`, read
  // once ahead of the loop, after what `block` holds, whose body
  // `each(body, index)` states, `index` the local holding each in turn. It
  // counts a machine integer, as Rust's `for idx in 0..len` over a slice
  // counts a `usize`, so no step of the loop is a four-state one.
  template <typename Each>
  void ForEachIndex(
      mir::CallableCode& code, mir::Builder block, mir::ExprId count,
      Each each) {
    ForEachIndexBetween(code, block, CountLiteral(block, 0), count, each);
  }
  // The same over the machine indexes from `from` up to `below`.
  template <typename Each>
  void ForEachIndexBetween(
      mir::CallableCode& code, mir::Builder block, mir::ExprId from,
      mir::ExprId below, Each each) {
    StepThrough(
        code, block, from, below, 1,
        [&](mir::Builder body, mir::LocalId at) { each(body, at); });
  }
  // A loop over the runs of `kPairsPerRun` machine indexes below the count
  // the local `count` holds, `each(body, from, to)` stating the body of one,
  // `from` and `to` the locals holding where it starts and where it ends,
  // the last run ending at the count.
  template <typename Each>
  void ForEachRun(
      mir::CallableCode& code, mir::Builder block, mir::LocalId count,
      Each each) {
    StepThrough(
        code, block, CountLiteral(block, 0), CountNow(block, count),
        kPairsPerRun, [&](mir::Builder body, mir::LocalId from) {
          const mir::ExprId run_end = mir::BuildMachineIntBinary(
              Unit(), body, mir::BinaryOp::kAdd, CountNow(body, from),
              mir::BuildMachineIntLiteral(Unit(), body, kPairsPerRun));
          const mir::LocalId end = DeclaredCount(code, body, run_end);
          const mir::LocalId to = DeclaredCount(
              code, body,
              Chosen(
                  body,
                  mir::BuildMachineIntComparison(
                      Unit(), body, mir::BinaryOp::kLessThan,
                      CountNow(body, end), CountNow(body, count)),
                  CountNow(body, end), CountNow(body, count),
                  Unit().builtins.machine_int64));
          each(body, from, to);
        });
  }
  // A loop counting a machine integer from `from` up to `below`, read once
  // ahead of the loop, by `stride`, after what `block` holds, whose body
  // `each(body, at)` states, `at` the local holding each value in turn.
  template <typename Each>
  void StepThrough(
      mir::CallableCode& code, mir::Builder block, mir::ExprId from,
      mir::ExprId below, std::int64_t stride, Each each) {
    const mir::TypeId machine = Unit().builtins.machine_int64;
    const mir::LocalId limit = DeclaredCount(code, block, below);
    const mir::LocalId at = code.AddLocal(machine);
    mir::Block body;
    mir::Builder body_builder = block.Into(body);
    each(body_builder, at);
    const mir::BlockId scope = block.AddScope(std::move(body));
    const mir::ExprId within = mir::BuildMachineIntComparison(
        Unit(), block, mir::BinaryOp::kLessThan, block.ReadLocal(at, machine),
        block.ReadLocal(limit, machine));
    const mir::ExprId next = mir::BuildMachineIntBinary(
        Unit(), block, mir::BinaryOp::kAdd, block.ReadLocal(at, machine),
        mir::BuildMachineIntLiteral(Unit(), block, stride));
    std::vector<mir::ForInit> init;
    init.emplace_back(mir::ForInitDecl{.induction_var = at, .init = from});
    block.AppendStmt(
        mir::ForStmt{
            .init = std::move(init),
            .condition = within,
            .step = {block.Add(
                mir::MakeAssignExpr(
                    Unit().builtins, block.LocalPlace(at, machine), next))},
            .scope = scope});
  }

  // The storage `storage` let go of by the queue `queue` names:
  // its element ended and the storage given back, unless a call leases it,
  // which then keeps it until the last lease lets go (LRM 13.5.2).
  void Release(mir::Builder block, const PlaceAt& queue, mir::LocalId storage) {
    mir::Block leased;
    mir::Builder leased_builder = block.Into(leased);
    Detach(leased_builder, storage);
    mir::Block held_by_none;
    mir::Builder none_builder = block.Into(held_by_none);
    Statement(
        none_builder,
        none_builder.Add(
            mir::MakeCallExpr(
                mir::Direct{.target = support::BuiltinFn::kPtrRead},
                {CastTo(
                    none_builder,
                    none_builder.ReadLocal(storage, ElementStoragePointer()),
                    ElementPointer())},
                ElementType())));
    Deallocate(none_builder, queue, storage);
    const mir::ExprId condition = Leased(block, storage);
    block.AppendStmt(
        mir::IfStmt{
            .condition = condition,
            .then_scope = block.AddScope(std::move(leased)),
            .else_scope = block.AddScope(std::move(held_by_none))});
  }

  // Every element of the queue `queue` names let go of, the queue left
  // holding none and its ring kept.
  void ReleaseEach(
      mir::CallableCode& code, mir::Builder block, const PlaceAt& queue) {
    ForEachIndex(
        code, block, CoreCount(block, queue, &value::QueueCore::count),
        [&](mir::Builder body, mir::LocalId index) {
          const mir::LocalId storage = Declared(
              code, body, ElementStoragePointer(),
              CastTo(
                  body,
                  GetUncheckedPointer(
                      body, queue, IndexNow(body, index),
                      mir::Mutability::kMutable),
                  ElementStoragePointer()));
          Release(body, queue, storage);
        });
    SetCoreCount(
        block, queue, &value::QueueCore::count,
        mir::BuildMachineIntLiteral(Unit(), block, 0));
    SetCoreCount(
        block, queue, &value::QueueCore::head,
        mir::BuildMachineIntLiteral(Unit(), block, 0));
  }

  // The queue held to its bound, which drops what lies past it, each element
  // let go of as a removed one is, and says so where the operation was asked
  // for (LRM 7.10.5); a queue of no bound holds whatever it is given.
  void HoldToBound(
      mir::CallableCode& code, mir::Builder block, const PlaceAt& queue) {
    std::visit(
        Overloaded{
            [](const support::UnboundedQueue&) {},
            [&](const support::BoundedQueue& most) {
              DropPast(code, block, queue, most.count + 1);
            }},
        Unit().types.Get(WholeType()).Get<mir::LibraryType>().Bound());
  }
  void DropPast(
      mir::CallableCode& code, mir::Builder block, const PlaceAt& queue,
      std::uint64_t most) {
    const mir::TypeId machine = Unit().builtins.machine_int64;
    const mir::LocalId count = Declared(
        code, block, machine,
        CoreCount(block, queue, &value::QueueCore::count));
    const mir::LocalId kept = Declared(
        code, block, machine,
        mir::BuildMachineIntLiteral(
            Unit(), block, static_cast<std::int64_t>(most)));
    mir::Block over;
    mir::Builder over_builder = block.Into(over);
    const mir::LocalId dropped = Declared(
        code, over_builder, machine,
        mir::BuildMachineIntBinary(
            Unit(), over_builder, mir::BinaryOp::kSub,
            over_builder.ReadLocal(count, machine),
            over_builder.ReadLocal(kept, machine)));
    ForEachIndex(
        code, over_builder, over_builder.ReadLocal(dropped, machine),
        [&](mir::Builder body, mir::LocalId index) {
          const mir::LocalId storage = Declared(
              code, body, ElementStoragePointer(),
              CastTo(
                  body,
                  GetUncheckedPointer(
                      body, queue,
                      mir::BuildMachineIntBinary(
                          Unit(), body, mir::BinaryOp::kAdd,
                          body.ReadLocal(kept, machine), IndexNow(body, index)),
                      mir::Mutability::kMutable),
                  ElementStoragePointer()));
          Release(body, queue, storage);
        });
    SetCoreCount(
        over_builder, queue, &value::QueueCore::count,
        over_builder.ReadLocal(kept, machine));
    Statement(
        over_builder,
        over_builder.Add(
            mir::MakeCallExpr(
                mir::Direct{
                    .target = support::BuiltinFn::kQueueGrewPastItsBound},
                {}, Unit().builtins.void_type)));
    block.AppendIfThen(
        mir::BuildMachineIntComparison(
            Unit(), block, mir::BinaryOp::kGreaterThan,
            block.ReadLocal(count, machine), block.ReadLocal(kept, machine)),
        std::move(over));
  }

  // What the local `local` of `type` holds, moved out as its last use, as
  // Rust's `return made;` moves: the local's end goes with the value.
  static auto TakenOutOf(
      mir::Builder block, mir::LocalId local, mir::TypeId type) -> mir::ExprId {
    return block.Add(
        mir::Expr{
            .data = mir::MoveExpr{.operand = block.LocalPlace(local, type)},
            .type = type});
  }

  // `made`, an array of this type, held to its bound where it is a queue,
  // which may hold more elements than its type admits (LRM 7.10.5).
  auto HeldToItsBound(
      mir::CallableCode& code, mir::Builder block, mir::ExprId made)
      -> mir::ExprId {
    return std::visit(
        Overloaded{
            [&](const FixedSizeKind&) { return made; },
            [&](const DynamicArrayKind&) { return made; },
            [&](const QueueKind&) {
              return Answered(
                  code, block, Declared(code, block, WholeType(), made));
            }},
        kind_);
  }

  // What a body made in the local `out` of this type, as what it answers:
  // the value itself for a fixed-size array, and for a run-time-sized one
  // what the local holds taken out of it, a queue's first held to its bound
  // (LRM 7.10.5).
  auto Answered(mir::CallableCode& code, mir::Builder block, mir::LocalId out)
      -> mir::ExprId {
    return std::visit(
        Overloaded{
            [&](const FixedSizeKind&) {
              return block.ReadLocal(out, WholeType());
            },
            [&](const DynamicArrayKind&) {
              return TakenOutOf(block, out, WholeType());
            },
            [&](const QueueKind&) {
              HoldToBound(code, block, LocalArray(out));
              return TakenOutOf(block, out, WholeType());
            }},
        kind_);
  }

  // How many elements the array `array` lies in holds, as a position.
  auto Count(mir::Builder block, mir::ExprId array) -> mir::ExprId {
    return CountAsIntegral(
        block, ElementCount(Unit(), block, WholeType(), array),
        mir::PositionType(Unit().types));
  }
  // A machine count as an integral value of `type`.
  auto CountAsIntegral(mir::Builder block, mir::ExprId count, mir::TypeId type)
      -> mir::ExprId {
    return block.Add(
        mir::MakeBuiltinCall(
            Unit(), block.Body(), support::BuiltinFn::kIntegralFromInt,
            std::nullopt, {count}, type));
  }

  // Where an element lies, as an address owning nothing, to write through,
  // and at `mutability`.
  [[nodiscard]] auto ElementPointer() const -> mir::TypeId {
    return ElementPointerAt(mir::Mutability::kMutable);
  }
  [[nodiscard]] auto ElementPointerAt(mir::Mutability mutability) const
      -> mir::TypeId {
    return mir::BorrowedPointerTo(Unit().types, ElementType(), mutability);
  }

  // The machine index the local `index` holds at this point of the body.
  auto IndexNow(mir::Builder block, mir::LocalId index) -> mir::ExprId {
    return block.ReadLocal(index, Unit().builtins.machine_int64);
  }

  // The position the machine index the local `index` holds stands for, where
  // a body hands it to the program or adds it to a position the program
  // computed.
  auto PositionOf(mir::Builder block, mir::LocalId index) -> mir::ExprId {
    return CountAsIntegral(
        block, IndexNow(block, index), mir::PositionType(Unit().types));
  }

  // The element at `position` of the array `array` lies in, where it lies,
  // the element default where the position names none (LRM 7.4.5).
  auto Element(mir::Builder block, mir::ExprId array, mir::ExprId position)
      -> mir::ExprId {
    return block.Add(StepForRead(
        Unit(), block,
        DescentStep{
            .to = StepToElement{.position = position},
            .part_type = ElementType()},
        array));
  }

  // The element at the machine index the local `index` holds, of the array
  // `array` lies in, an array of this type's element type, reached unchecked
  // to read it, and the same element to write.
  auto GetUnchecked(mir::Builder block, mir::ExprId array, mir::LocalId index)
      -> mir::ExprId {
    return UncheckedElement(
        Unit(), block, array, IndexNow(block, index), ElementType(),
        mir::Mutability::kReadOnly);
  }
  auto GetUncheckedMut(
      mir::Builder block, mir::ExprId array, mir::LocalId index)
      -> mir::ExprId {
    return UncheckedElement(
        Unit(), block, array, IndexNow(block, index), ElementType(),
        mir::Mutability::kMutable);
  }
  // A write of `value` to the element at the machine index `index` of the
  // array `array` lies in, which names one the array holds.
  void StoredUnchecked(
      mir::Builder block, mir::ExprId array, mir::LocalId index,
      mir::ExprId value) {
    Statement(
        block,
        block.Add(BuildStoreExpr(
            Unit(), block,
            AccessPath{
                .owner = GetUncheckedMut(block, array, index), .descent = {}},
            value)));
  }

  // A new generation of `count` elements, a machine count, the one at each
  // machine index built as `value(body, index)` makes it, `index` the local
  // holding it, in storage the core of the array `array(block)` names in a
  // block hands out: that storage, and how many elements it holds. Nothing
  // the program wrote runs before an array takes it, so it is always taken.
  struct Generated {
    mir::LocalId data;
    mir::LocalId count;
  };
  template <typename ArrayAt, typename Value>
  auto Generate(
      mir::CallableCode& code, mir::Builder block, ArrayAt array,
      mir::ExprId count, Value value) -> Generated {
    const mir::LocalId held_count = DeclaredCount(code, block, count);
    const mir::TypeId pointer = ElementPointer();
    const mir::LocalId data = Declared(
        code, block, pointer,
        CastTo(
            block,
            Core(
                block, support::BuiltinFn::kDynamicArrayGeneration,
                array(block), {CountNow(block, held_count)}, Address()),
            pointer));
    ForEachIndex(
        code, block, CountNow(block, held_count),
        [&](mir::Builder body, mir::LocalId at) {
          const mir::ExprId built = value(body, at);
          const mir::ExprId index = IndexNow(body, at);
          const mir::ExprId storage = body.ReadLocal(data, pointer);
          Statement(
              body, body.Add(
                        mir::MakePlacementNewExpr(
                            built,
                            mir::PlacementAllocation{
                                .allocate = mir::ElementAllocationOf(
                                                Unit(), WholeType())
                                                ->allocate,
                                .arguments =
                                    {mir::Borrow(Unit(), body, array(body)),
                                     storage, index}},
                            pointer)));
        });
    return {.data = data, .count = held_count};
  }

  // The generation's storage, as the core takes it.
  auto DataOf(mir::Builder block, const Generated& generated) -> mir::ExprId {
    return block.ReadLocal(generated.data, ElementPointer());
  }

  // The elements of the dynamic array `array(block)` names ended, unless a
  // call leases them, which keeps them to the last lease (LRM 13.5.2) and
  // leaves the array holding none; the array's core says which.
  template <typename ArrayAt>
  void LetGoOfGeneration(
      mir::CallableCode& code, mir::Builder block, ArrayAt array) {
    const mir::TypeId boolean = Unit().builtins.machine_bool;
    const mir::LocalId ends = Declared(
        code, block, boolean,
        Core(
            block, support::BuiltinFn::kDynamicArrayLetGo, array(block), {},
            boolean));
    mir::Block ended;
    mir::Builder ended_builder = block.Into(ended);
    ForEachIndex(
        code, ended_builder,
        ElementCount(Unit(), ended_builder, WholeType(), array(ended_builder)),
        [&](mir::Builder body, mir::LocalId index) {
          Statement(
              body, body.Add(
                        mir::MakeCallExpr(
                            mir::Direct{.target = support::BuiltinFn::kPtrRead},
                            {GetUncheckedPointer(
                                body, array, IndexNow(body, index),
                                mir::Mutability::kMutable)},
                            ElementType())));
        });
    block.AppendIfThen(block.ReadLocal(ends, boolean), std::move(ended));
  }

  // The array `array(block)` names made to hold such a generation, ending the
  // elements it held, or leaving them to the last lease on them.
  template <typename ArrayAt, typename Value>
  void Regenerate(
      mir::CallableCode& code, mir::Builder block, ArrayAt array,
      mir::ExprId count, Value value) {
    // A queue made of new elements takes them from a queue built first, every
    // element read before its own end (LRM 7.10.3).
    const auto adopted = [&](const QueueKind&) {
      const mir::LocalId made =
          QueueFilled(code, block, WholeType(), count, value);
      // The queue lets go of what it held; its core then gives back the ring
      // and takes the new elements' order.
      ReleaseEach(code, block, PlaceAt(array));
      Statement(
          block, Core(
                     block, support::BuiltinFn::kQueueAdopt, array(block),
                     {LentToWrite(block, block.LocalPlace(made, WholeType()))},
                     Unit().builtins.void_type));
    };
    // A dynamic array lets go of the generation it held once the new one is
    // built, then has its core give that storage back and take the new one.
    const auto generated_anew = [&](const DynamicArrayKind&) {
      const Generated generated = Generate(code, block, array, count, value);
      LetGoOfGeneration(code, block, array);
      Statement(
          block, Core(
                     block, support::BuiltinFn::kDynamicArrayFree, array(block),
                     {}, Unit().builtins.void_type));
      Statement(
          block,
          Core(
              block, support::BuiltinFn::kDynamicArrayAdopt, array(block),
              {DataOf(block, generated), CountNow(block, generated.count)},
              Unit().builtins.void_type));
    };
    std::visit(
        Overloaded{
            [](const FixedSizeKind&) {
              throw InternalError(
                  "hir_to_mir: a fixed-size array is asked to take a new "
                  "generation of elements -- please report this as a bug");
            },
            generated_anew, adopted},
        kind_);
  }

  // A new array holding such a generation, built in storage the core of the
  // array `shape(block)` names hands out.
  template <typename ArrayAt, typename Value>
  auto Built(
      mir::CallableCode& code, mir::Builder block, ArrayAt shape,
      mir::ExprId count, Value value) -> mir::ExprId {
    const auto filled = [&](const QueueKind&) {
      return TakenOutOf(
          block, QueueFilled(code, block, WholeType(), count, value),
          WholeType());
    };
    const auto generated_anew = [&](const DynamicArrayKind&) {
      return BuiltFromGeneration(code, block, shape, count, value);
    };
    return std::visit(
        Overloaded{
            [](const FixedSizeKind&) -> mir::ExprId {
              throw InternalError(
                  "hir_to_mir: a fixed-size array is built from a new "
                  "generation of elements -- please report this as a bug");
            },
            generated_anew, filled},
        kind_);
  }

  // A new dynamic array holding such a generation, built in storage the core
  // of the array `shape(block)` names hands out.
  template <typename ArrayAt, typename Value>
  auto BuiltFromGeneration(
      mir::CallableCode& code, mir::Builder block, ArrayAt shape,
      mir::ExprId count, Value value) -> mir::ExprId {
    const Generated generated = Generate(code, block, shape, count, value);
    const mir::LocalId made = Declared(code, block, WholeType(), Empty(block));
    Statement(
        block, Core(
                   block, support::BuiltinFn::kDynamicArrayAdopt,
                   block.LocalPlace(made, WholeType()),
                   {DataOf(block, generated), CountNow(block, generated.count)},
                   Unit().builtins.void_type));
    return TakenOutOf(block, made, WholeType());
  }

  // A copy of every element of the array `from` lies in, in turn, as a new
  // generation of the array `into(block)` names in a block.
  template <typename ArrayAt>
  void CopyElementsInto(
      mir::CallableCode& code, mir::Builder block, ArrayAt into,
      mir::LocalId from) {
    Regenerate(
        code, block, into,
        ElementCount(Unit(), block, WholeType(), Read(block, from)),
        [&](mir::Builder body, mir::LocalId index) {
          return body.AsValue(
              Unit().types, GetUnchecked(body, Read(body, from), index));
        });
  }

  // A copy of each element of the source as a new generation of the array
  // written over, which then ends the elements it held; writing an array over
  // itself copies its elements before ending them.
  auto Assign() -> mir::CallableCode {
    return Defined(
        Declaration(support::ValueStep::kAssign),
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          CopyElementsInto(
              code, block, ParameterArray(p[0], mir::Mutability::kMutable),
              p[1]);
          block.AppendStmt(
              mir::ReturnStmt{
                  .value = block.ReadLocal(p[0], code.locals.Get(p[0]).type)});
        });
  }

  auto End() -> mir::CallableCode {
    return Defined(
        Declaration(support::ValueStep::kEnd),
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          EndInPlace(code, block, p[0]);
        });
  }

  // The array the parameter `array` refers to ended: each kind lets go of its
  // elements itself, then has its core give back the ring or the generation
  // they lay in. A fixed-size array's end follows from its parts.
  void EndInPlace(
      mir::CallableCode& code, mir::Builder block, mir::LocalId array) {
    std::visit(
        Overloaded{
            [](const FixedSizeKind&) {
              throw InternalError(
                  "hir_to_mir: a fixed-size array is ended by a body of its "
                  "own, and its end follows from its parts -- please report "
                  "this as a bug");
            },
            [&](const DynamicArrayKind&) {
              LetGoOfGeneration(
                  code, block,
                  ParameterArray(array, mir::Mutability::kMutable));
            },
            [&](const QueueKind&) {
              ReleaseEach(code, block, ParameterQueue(array));
            }},
        kind_);
    Statement(
        block, Core(
                   block, CoreEndOf(kind_), Written(block, array), {},
                   Unit().builtins.void_type));
  }

  // LRM Table 7-1: an array of no elements.
  auto Default(support::DefaultItem answers) -> mir::CallableCode {
    return Defined(
        Declaration(answers), [&](mir::CallableCode&, mir::Builder block,
                                  const std::vector<mir::LocalId>&) {
          block.AppendStmt(mir::ReturnStmt{.value = EmptyFromCore(block)});
        });
  }

  // The empty array of this type its core answers, told the record of the
  // element type, which is the value its core is, nothing else of it being
  // its own.
  auto EmptyFromCore(mir::Builder block) -> mir::ExprId {
    const mir::ExprId record = block.Add(
        mir::MakeBuiltinCall(
            Unit(), block.Body(), support::BuiltinFn::kTypeRecordOf,
            std::nullopt, std::nullopt, {ElementType()}, {},
            mir::BorrowedPointerTo(
                Unit().types,
                mir::LibraryTypeOf(
                    Unit().types, support::LibraryDecl::kValueType),
                mir::Mutability::kReadOnly)));
    return block.Add(
        mir::MakeBuiltinCall(
            Unit(), block.Body(), CoreConstructOf(kind_), std::nullopt,
            std::nullopt, {WholeType()}, {record}, WholeType()));
  }

  // LRM 7.5.1 `new[N]`: `N` elements, each the element type's default; and
  // `new[N](src)`, the first `N` of `src`'s, the rest the default. A negative
  // size is the design's own error.
  auto New() -> mir::CallableCode {
    return Defined(
        Declaration(support::BuiltinFn::kDynamicArrayNew),
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          const mir::ExprId count = RequestedCount(block, p[0]);
          // An array of no elements, whose core hands out the storage.
          const mir::LocalId shape =
              Declared(code, block, WholeType(), Empty(block));
          const mir::ExprId made = Built(
              code, block, LocalArray(shape), count,
              [&](mir::Builder body, mir::LocalId) {
                return body.Add(
                    BuildDefaultValueExpr(Unit(), body, ElementType()));
              });
          block.AppendStmt(mir::ReturnStmt{.value = made});
        });
  }
  auto NewCopy() -> mir::CallableCode {
    return Defined(
        Declaration(support::BuiltinFn::kDynamicArrayNewCopy),
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          block.AppendStmt(
              mir::ReturnStmt{
                  .value = KeptAndGrown(
                      code, block, p[1], RequestedCount(block, p[0]))});
        });
  }

  // An array `count` elements long, a machine count, each one the element at
  // the same index of the array the parameter `from` refers to as far as it
  // reaches and the element default past it (LRM 7.5.1).
  auto KeptAndGrown(
      mir::CallableCode& code, mir::Builder block, mir::LocalId from,
      mir::ExprId count) -> mir::ExprId {
    return Built(
        code, block, ParameterArray(from, mir::Mutability::kReadOnly), count,
        [&](mir::Builder body, mir::LocalId index) {
          const mir::ExprId kept = mir::BuildMachineIntComparison(
              Unit(), body, mir::BinaryOp::kLessThan, IndexNow(body, index),
              ElementCount(Unit(), body, WholeType(), Read(body, from)));
          return body.Add(
              mir::Expr{
                  .data =
                      mir::ConditionalExpr{
                          .condition = kept,
                          .then_value = body.AsValue(
                              Unit().types,
                              GetUnchecked(body, Read(body, from), index)),
                          .else_value = body.Add(BuildDefaultValueExpr(
                              Unit(), body, ElementType()))},
                  .type = ElementType()});
        });
  }

  // The array the parameter `from` refers to made the count the parameter
  // `count` refers to long, kept as far as it reaches and grown by element
  // defaults (LRM 7.5.1), a queue then held to its bound (LRM 7.10.5).
  auto Resized() -> mir::CallableCode {
    return Defined(
        Declaration(support::ResizeItem::kResized),
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          const mir::TypeId machine = Unit().builtins.machine_int64;
          const mir::ExprId count =
              block.AsValue(Unit().types, Referent(block, p[1], machine));
          block.AppendStmt(
              mir::ReturnStmt{
                  .value = HeldToItsBound(
                      code, block, KeptAndGrown(code, block, p[0], count))});
        });
  }

  // The element the machine index the second parameter refers to names in the
  // array the first refers to, by reference at `mutability`, the index naming
  // one, as Rust's `<[T]>::get_unchecked` answers it.
  auto GetUncheckedBody(
      support::ElementAccessItem item, mir::Mutability mutability)
      -> mir::CallableCode {
    return Defined(
        Declaration(item), [&, mutability](
                               mir::CallableCode&, mir::Builder block,
                               const std::vector<mir::LocalId>& p) {
          const mir::ExprId element = block.Add(
              mir::MakeDerefExpr(
                  GetUncheckedPointer(
                      block, ParameterArray(p[0], mutability),
                      block.AsValue(
                          Unit().types,
                          Referent(block, p[1], Unit().builtins.machine_int64)),
                      mutability),
                  ElementType()));
          block.AppendStmt(
              mir::ReturnStmt{
                  .value = block.Add(
                      mir::Expr{
                          .data = mir::BorrowExpr{.operand = element},
                          .type = Unit().types.Intern(
                              mir::Type{mir::ReferenceType{
                                  .referent = ElementType(),
                                  .mutability = mutability}})})});
        });
  }

  // LRM 7.6: a copy of the array the parameter refers to, answered as a
  // value, as Rust's `Clone::clone` answers one: the type's default, then a
  // copy of each element taken as its generation.
  auto Copy() -> mir::CallableCode {
    return Defined(
        Declaration(support::ValueStep::kCopy),
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          const mir::LocalId made =
              Declared(code, block, WholeType(), Empty(block));
          CopyElementsInto(code, block, LocalArray(made), p[0]);
          block.AppendStmt(
              mir::ReturnStmt{.value = TakenOutOf(block, made, WholeType())});
        });
  }

  // The array the place `array` names lent to be written through.
  auto LentToWrite(mir::Builder block, mir::ExprId array) -> mir::ExprId {
    return block.Add(
        mir::Expr{
            .data = mir::BorrowExpr{.operand = array},
            .type = Unit().types.Intern(
                mir::Type{mir::ReferenceType{
                    .referent = WholeType(),
                    .mutability = mir::Mutability::kMutable}})});
  }

  // The size a `new[]` was given, as a machine count, which the design is told
  // is wrong where it is negative (LRM 7.5.1).
  auto RequestedCount(mir::Builder block, mir::LocalId size) -> mir::ExprId {
    const mir::TypeId int_type = Unit().builtins.machine_int64;
    const mir::TypeId reference = Unit().types.Intern(
        mir::Type{mir::ReferenceType{
            .referent = int_type, .mutability = mir::Mutability::kReadOnly}});
    const auto read = [&] {
      return block.AsValue(
          Unit().types,
          block.Deref(
              Unit().types, block.LocalPlace(size, reference), int_type));
    };
    mir::AppendRunTimeCheck(
        Unit(), block,
        mir::BuildMachineIntComparison(
            Unit(), block, mir::BinaryOp::kGreaterEqual, read(),
            mir::BuildMachineIntLiteral(Unit(), block, 0)),
        base::DiagMessage(
            "the size given to a dynamic array's new[] is "
            "negative")
            .TakeText());
    return read();
  }

  // LRM 7.5.2, 7.10.2.1: how many elements the array holds, as an `int`.
  auto Size() -> mir::CallableCode {
    return Defined(
        Declaration(support::BuiltinFn::kSize),
        [&](mir::CallableCode&, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          block.AppendStmt(
              mir::ReturnStmt{
                  .value = CountAsIntegral(
                      block,
                      ElementCount(
                          Unit(), block, WholeType(), Read(block, p[0])),
                      Unit().builtins.int_type)});
        });
  }

  // What `combine(body, kept, each)` makes of every element's `each(body,
  // index)`, `index` the local holding its machine index, kept in a local of
  // `type` that starts as `first` builds it.
  template <typename First, typename Each, typename Combine>
  auto Reduced(
      mir::CallableCode& code, mir::Builder block, mir::LocalId array,
      mir::TypeId type, First first, Each each, Combine combine)
      -> mir::ExprId {
    const mir::LocalId kept = Declared(code, block, type, first(block));
    ForEachIndex(
        code, block,
        ElementCount(Unit(), block, WholeType(), Read(block, array)),
        [&](mir::Builder body, mir::LocalId index) {
          const mir::ExprId so_far = body.ReadLocal(kept, type);
          const mir::ExprId combined = combine(body, so_far, each(body, index));
          Statement(
              body,
              body.Add(
                  mir::MakeAssignExpr(
                      Unit().builtins, body.LocalPlace(kept, type), combined)));
        });
    return block.ReadLocal(kept, type);
  }

  auto Sum(mir::Builder body, mir::ExprId kept, mir::ExprId each)
      -> mir::ExprId {
    return body.Add(
        mir::MakeBinary(
            Unit(), body.Body(), mir::BinaryOp::kAdd, kept, each,
            Unit().builtins.int_type));
  }

  // LRM 20.6.2 `$bits`: the bits every element holds, laid end to end.
  auto BitWidth() -> mir::CallableCode {
    const mir::TypeId count = Unit().builtins.int_type;
    return Defined(
        Declaration(support::StreamItem::kBitstreamWidth),
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          if (!StreamsItsElements()) {
            RefuseTheStream(block, mir::BuildIntLiteral(Unit(), block, 0));
            return;
          }
          const mir::ExprId width = Reduced(
              code, block, p[0], count,
              [&](mir::Builder at) {
                return mir::BuildIntLiteral(Unit(), at, 0);
              },
              [&](mir::Builder body, mir::LocalId index) {
                return BuildBitWidth(
                    Unit(), body, GetUnchecked(body, Read(body, p[0]), index));
              },
              [&](mir::Builder body, mir::ExprId kept, mir::ExprId each) {
                return Sum(body, kept, each);
              });
          block.AppendStmt(mir::ReturnStmt{.value = width});
        });
  }

  // LRM 20.9 `$countbits`: the bits of each element that hold one of the
  // control set's values.
  auto CountBits() -> mir::CallableCode {
    const mir::TypeId count = Unit().builtins.int_type;
    const mir::TypeId control = mir::BitCountControlType(Unit().types);
    return Defined(
        Declaration(support::StreamItem::kCountBits),
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          if (!StreamsItsElements()) {
            RefuseTheStream(block, mir::BuildIntLiteral(Unit(), block, 0));
            return;
          }
          const mir::ExprId counted = Reduced(
              code, block, p[0], count,
              [&](mir::Builder at) {
                return mir::BuildIntLiteral(Unit(), at, 0);
              },
              [&](mir::Builder body, mir::LocalId index) {
                return BuildBitCount(
                    Unit(), body, GetUnchecked(body, Read(body, p[0]), index),
                    Referent(body, p[1], control));
              },
              [&](mir::Builder body, mir::ExprId kept, mir::ExprId each) {
                return Sum(body, kept, each);
              });
          block.AppendStmt(mir::ReturnStmt{.value = counted});
        });
  }

  // LRM 6.24.3: the elements' streams laid end to end into a run of bits, the
  // one at index 0 most significant. The run is as wide as the array's stream
  // before any element is written, so each element's bits are written where
  // they lie.
  auto ToBitVec() -> mir::CallableCode {
    const mir::TypeId run = Unit().builtins.bit_vec;
    const mir::TypeId machine = Unit().builtins.machine_int64;
    return Defined(
        Declaration(support::StreamItem::kToBitVec),
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          if (!StreamsItsElements()) {
            RefuseTheStream(
                block, BuildRunOfZeros(
                           Unit(), block,
                           mir::BuildMachineIntLiteral(Unit(), block, 0)));
            return;
          }
          const mir::LocalId bits = Declared(
              code, block, run,
              BuildRunOfZeros(
                  Unit(), block,
                  BuildToInt64Call(
                      Unit(), block,
                      BuildBitWidth(Unit(), block, Read(block, p[0])))));
          const mir::LocalId at = Declared(
              code, block, machine,
              mir::BuildMachineIntLiteral(Unit(), block, 0));
          ForEachIndex(
              code, block,
              ElementCount(Unit(), block, WholeType(), Read(block, p[0])),
              [&](mir::Builder body, mir::LocalId index) {
                auto element = BuildToBitstream(
                    Unit(), body, GetUnchecked(body, Read(body, p[0]), index),
                    {});
                if (!element) {
                  throw InternalError(
                      "hir_to_mir: an element of an array holding a stream "
                      "has none -- please report this as a bug");
                }
                const mir::LocalId part = Declared(
                    code, body, run,
                    BuildRunOf(
                        Unit(), body, body.AsValue(Unit().types, *element)));
                InsertIntoRun(
                    Unit(), body, bits, body.ReadLocal(at, machine),
                    body.ReadLocal(part, run));
                Statement(
                    body, body.Add(
                              mir::MakeAssignExpr(
                                  Unit().builtins, body.LocalPlace(at, machine),
                                  mir::BuildMachineIntBinary(
                                      Unit(), body, mir::BinaryOp::kAdd,
                                      body.ReadLocal(at, machine),
                                      BuildRunLength(
                                          Unit(), body,
                                          body.ReadLocal(part, run))))));
              });
          block.AppendStmt(
              mir::ReturnStmt{.value = block.ReadLocal(bits, run)});
        });
  }

  // The inverse, out of a run holding exactly the array's bits (LRM 6.24.3,
  // 11.4.14.4): as many elements as fill it, each taking its type's width,
  // or, where an element's width is the running program's, one taking every
  // bit. A run no whole number of elements fills is the design's own error.
  auto FromBitVec() -> mir::CallableCode {
    const mir::TypeId run = Unit().builtins.bit_vec;
    const mir::TypeId machine = Unit().builtins.machine_int64;
    const std::optional<mir::StreamShape> each =
        mir::FixedStreamShapeOf(Unit(), ElementType());
    return Defined(
        Declaration(support::StreamItem::kFromBitVec),
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          if (!StreamsItsElements()) {
            RefuseTheStream(block, Empty(block));
            return;
          }
          const auto source = [&](mir::Builder at) {
            return Referent(at, p[0], run);
          };
          const mir::LocalId length = Declared(
              code, block, machine,
              BuildRunLength(Unit(), block, source(block)));
          mir::ExprId count{};
          if (each.has_value()) {
            mir::AppendRunTimeCheck(
                Unit(), block,
                mir::BuildMachineIntComparison(
                    Unit(), block, mir::BinaryOp::kEquality,
                    mir::BuildMachineIntBinary(
                        Unit(), block, mir::BinaryOp::kMod,
                        block.ReadLocal(length, machine),
                        CountLiteral(block, each->width)),
                    CountLiteral(block, 0)),
                base::DiagMessage(
                    "a stream of bits does not divide into the elements of "
                    "the array it is read into")
                    .TakeText());
            count = mir::BuildMachineIntBinary(
                Unit(), block, mir::BinaryOp::kDiv,
                block.ReadLocal(length, machine),
                CountLiteral(block, each->width));
          } else {
            count = mir::BuildMachineIntConditional(
                Unit(), block,
                mir::BuildMachineIntComparison(
                    Unit(), block, mir::BinaryOp::kGreaterThan,
                    block.ReadLocal(length, machine), CountLiteral(block, 0)),
                CountLiteral(block, 1), CountLiteral(block, 0));
          }
          const mir::LocalId shape =
              Declared(code, block, WholeType(), Empty(block));
          const mir::ExprId made = Built(
              code, block, LocalArray(shape), count,
              [&](mir::Builder body, mir::LocalId index) {
                if (!each.has_value()) {
                  return BuildFromBitVec(
                      Unit(), body,
                      BuildRunExtract(
                          Unit(), body, source(body), CountLiteral(body, 0),
                          body.ReadLocal(length, machine)),
                      ElementType());
                }
                const mir::ExprId start = mir::BuildMachineIntBinary(
                    Unit(), body, mir::BinaryOp::kMul, IndexNow(body, index),
                    CountLiteral(body, each->width));
                return BuildFromBitstream(
                    Unit(), body,
                    BuildRunLoad(Unit(), body, source(body), start, *each),
                    ElementType());
              });
          block.AppendStmt(
              mir::ReturnStmt{.value = HeldToItsBound(code, block, made)});
        });
  }

  // LRM 11.4.11: the two arms of a conditional operator whose condition is
  // ambiguous, combined element by element -- an element the arms are known
  // to agree on survives, and one they disagree on, or cannot know, takes the
  // element default (Table 7-1). Arms of unequal size put no elements in
  // correspondence, so the result is the array of no elements.
  auto MergeConditional() -> mir::CallableCode {
    return Defined(
        Declaration(support::UnknownItem::kMergeConditional),
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          const mir::ExprId same_count = mir::BuildMachineIntComparison(
              Unit(), block, mir::BinaryOp::kEquality,
              ElementCount(Unit(), block, WholeType(), Read(block, p[0])),
              ElementCount(Unit(), block, WholeType(), Read(block, p[1])));
          mir::Block paired;
          mir::Builder at = block.Into(paired);
          const mir::ExprId merged = Built(
              code, at, ParameterArray(p[0], mir::Mutability::kReadOnly),
              ElementCount(Unit(), at, WholeType(), Read(at, p[0])),
              [&](mir::Builder body, mir::LocalId index) {
                const auto element = [&](mir::LocalId arm) {
                  return GetUnchecked(body, Read(body, arm), index);
                };
                const mir::ExprId agree = ReduceToCondition(
                    Unit(), body,
                    body.Add(BuildMirBinaryExpr(
                        Unit(), body, hir::BinaryOp::kEquality, element(p[0]),
                        element(p[1]),
                        mir::OneBitAnswerType(
                            Unit(), std::array{ElementType()}))));
                return body.Add(
                    mir::Expr{
                        .data =
                            mir::ConditionalExpr{
                                .condition = agree,
                                .then_value =
                                    body.AsValue(Unit().types, element(p[0])),
                                .else_value = body.Add(BuildDefaultValueExpr(
                                    Unit(), body, ElementType()))},
                        .type = ElementType()});
              });
          at.AppendStmt(mir::ReturnStmt{.value = merged});
          block.AppendStmt(
              mir::IfStmt{
                  .condition = same_count,
                  .then_scope = block.AddScope(std::move(paired)),
                  .else_scope = std::nullopt});
          block.AppendStmt(mir::ReturnStmt{.value = Empty(block)});
        });
  }

  // LRM 7.5.3, 7.10.2.3: the array holding no elements.
  auto Delete() -> mir::CallableCode {
    return Defined(
        Declaration(support::BuiltinFn::kDelete),
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          // A queue lets go of each element it holds; a dynamic array takes
          // a generation of none.
          std::visit(
              Overloaded{
                  [](const FixedSizeKind&) {
                    throw InternalError(
                        "hir_to_mir: a fixed-size array is asked to delete "
                        "its elements -- please report this as a bug");
                  },
                  [&](const DynamicArrayKind&) {
                    Regenerate(
                        code, block,
                        ParameterArray(p[0], mir::Mutability::kMutable),
                        CountLiteral(block, 0),
                        [&](mir::Builder body, mir::LocalId) {
                          return body.Add(BuildDefaultValueExpr(
                              Unit(), body, ElementType()));
                        });
                  },
                  [&](const QueueKind&) {
                    ReleaseEach(code, block, ParameterQueue(p[0]));
                  }},
              kind_);
        });
  }

  // LRM 7.10.2.3: the element a position names removed, and nothing where it
  // names none.
  auto DeleteIndex() -> mir::CallableCode {
    const mir::TypeId position_type = mir::PositionType(Unit().types);
    return Defined(
        Declaration(support::BuiltinFn::kDeleteIndex),
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          const mir::LocalId at = Declared(
              code, block, position_type,
              block.AsValue(
                  Unit().types, Referent(block, p[1], position_type)));
          mir::Block names;
          mir::Builder names_builder = block.Into(names);
          // The core takes the element out of the order, closing the gap,
          // and the queue lets it go as it lets any removed element go.
          const mir::LocalId storage = Declared(
              code, names_builder, ElementStoragePointer(),
              CastTo(
                  names_builder,
                  Core(
                      names_builder, support::BuiltinFn::kQueueUnlink,
                      Written(names_builder, p[0]),
                      {BuildToInt64Call(
                          Unit(), names_builder,
                          names_builder.ReadLocal(at, position_type))},
                      Address()),
                  ElementStoragePointer()));
          Release(names_builder, ParameterQueue(p[0]), storage);
          block.AppendIfThen(
              NamesAPosition(
                  block, block.ReadLocal(at, position_type),
                  Count(block, Written(block, p[0])), mir::BinaryOp::kLessThan),
              std::move(names));
        });
  }

  // Whether `position` names a position at or above 0 and, compared by `op`,
  // with `count`: below it for an element the queue holds, at most it for
  // where one may be inserted.
  auto NamesAPosition(
      mir::Builder block, mir::ExprId position, mir::ExprId count,
      mir::BinaryOp op) -> mir::ExprId {
    const std::array named{
        Known(block, position),
        PositionCompare(
            block, mir::BinaryOp::kGreaterEqual, position,
            BuildConstantPosition(Unit(), block, 0)),
        PositionCompare(block, op, position, count)};
    return AllHold(Unit(), block, named);
  }

  // LRM 7.10.2.2, 7.10.2.6, 7.10.2.7: an element built from a value put in
  // the queue -- at a position at most the count, where no side is named, which
  // inserts nothing where it names none, at the front, or at the back -- and
  // the queue held to its bound (LRM 7.10.5). The value is read before the
  // element is built, so a value read from the queue itself is a value of its
  // own.
  auto Inserted(support::ValueOperation answers, std::optional<QueueSide> side)
      -> mir::CallableCode {
    const mir::TypeId position_type = mir::PositionType(Unit().types);
    return Defined(
        Declaration(answers), [&](mir::CallableCode& code, mir::Builder block,
                                  const std::vector<mir::LocalId>& p) {
          const mir::LocalId item = p[side.has_value() ? 1 : 2];
          if (side.has_value()) {
            PutAtSide(
                code, block, ParameterQueue(p[0]), *side, [&](mir::Builder at) {
                  return at.AsValue(
                      Unit().types, Referent(at, item, ElementType()));
                });
            return;
          }
          const auto insert = [&](mir::Builder at, mir::ExprId position) {
            Statement(
                at,
                Core(
                    at, support::BuiltinFn::kQueueCommit, Written(at, p[0]),
                    {BuildToInt64Call(Unit(), at, position),
                     MadeInQueueStorage(
                         Unit(), at, Written(at, p[0]),
                         at.AsValue(
                             Unit().types, Referent(at, item, ElementType())),
                         ElementPointer())},
                    Unit().builtins.void_type));
            HoldToBound(code, at, ParameterQueue(p[0]));
          };
          const mir::LocalId at = Declared(
              code, block, position_type,
              block.AsValue(
                  Unit().types, Referent(block, p[1], position_type)));
          mir::Block names;
          mir::Builder names_builder = block.Into(names);
          insert(names_builder, names_builder.ReadLocal(at, position_type));
          block.AppendIfThen(
              NamesAPosition(
                  block, block.ReadLocal(at, position_type),
                  Count(block, Written(block, p[0])),
                  mir::BinaryOp::kLessEqual),
              std::move(names));
        });
  }

  // LRM 7.10.2.4, 7.10.2.5: the element at the front or the back, taken out
  // of the queue as the value answered, and the element default where the
  // queue holds none, which it leaves as it is. The value answered is the
  // element's own, taken out of its storage, unless a call leases the element,
  // which goes on reaching it until the call returns and so is answered a
  // copy (LRM 13.5.2).
  auto Popped(support::ValueOperation answers, QueueSide side)
      -> mir::CallableCode {
    const mir::TypeId machine = Unit().builtins.machine_int64;
    return Defined(
        Declaration(answers), [&](mir::CallableCode& code, mir::Builder block,
                                  const std::vector<mir::LocalId>& p) {
          const mir::LocalId count = Declared(
              code, block, machine,
              ElementCount(Unit(), block, WholeType(), Written(block, p[0])));
          mir::Block empty;
          mir::Builder empty_builder = block.Into(empty);
          empty_builder.AppendStmt(
              mir::ReturnStmt{
                  .value = empty_builder.Add(BuildDefaultValueExpr(
                      Unit(), empty_builder, ElementType()))});
          block.AppendIfThen(
              mir::BuildMachineIntComparison(
                  Unit(), block, mir::BinaryOp::kEquality,
                  block.ReadLocal(count, machine),
                  mir::BuildMachineIntLiteral(Unit(), block, 0)),
              std::move(empty));
          TakeAtSide(code, block, ParameterQueue(p[0]), side, count);
        });
  }

  // The position the parameter `position` refers to, kept in a local of the
  // body.
  auto HandedPosition(
      mir::CallableCode& code, mir::Builder block, mir::LocalId position)
      -> mir::LocalId {
    const mir::TypeId position_type = mir::PositionType(Unit().types);
    return Declared(
        code, block, position_type,
        block.AsValue(Unit().types, Referent(block, position, position_type)));
  }

  // Where the element at the machine index `index` of the array `array(block)`
  // names in a block lies, at `mutability`, as Rust's
  // `<*mut [T]>::get_unchecked_mut` answers it: that many elements past where
  // a dynamic array's block starts, and the storage a queue's ring holds that
  // many entries past its head.
  template <typename ArrayAt>
  auto GetUncheckedPointer(
      mir::Builder block, ArrayAt array, mir::ExprId index,
      mir::Mutability mutability) -> mir::ExprId {
    const mir::TypeId pointer = ElementPointerAt(mutability);
    return std::visit(
        Overloaded{
            [](const FixedSizeKind&) -> mir::ExprId {
              throw InternalError(
                  "hir_to_mir: an element of a fixed-size array is asked of a "
                  "body, where its type's layout places it -- please report "
                  "this as a bug");
            },
            [&](const DynamicArrayKind&) {
              return block.Add(
                  mir::MakeBinary(
                      Unit(), block.Body(), mir::BinaryOp::kAdd,
                      CastTo(
                          block,
                          block.Read(
                              mir::LibraryRecordMemberPlace(
                                  Unit(), block, array(block),
                                  &value::DynamicArrayCore::data)),
                          pointer),
                      index, pointer));
            },
            [&](const QueueKind&) {
              const PlaceAt queue = array;
              return CastTo(
                  block,
                  block.Read(
                      RingEntry(block, queue, EntryOf(block, queue, index))),
                  pointer);
            }},
        kind_);
  }

  // Where a position naming no element reads, the element default, or writes,
  // storage no read reaches (LRM 7.4.5, 7.10.1), as `answers` says.
  auto NoElement(
      mir::Builder block, support::BuiltinFn answers, mir::TypeId pointer)
      -> mir::ExprId {
    return block.Add(
        mir::MakeBuiltinCall(
            Unit(), block.Body(), answers, std::nullopt, std::nullopt,
            {ElementType()}, {}, pointer));
  }

  // Where the element the local `at` names lies in the array the parameter
  // `array` refers to, at `mutability`, where it names one the array holds,
  // and where `absent` says otherwise.
  auto Reached(
      mir::Builder block, mir::LocalId array, mir::LocalId at,
      mir::Mutability mutability, support::BuiltinFn absent) -> mir::ExprId {
    const mir::TypeId position_type = mir::PositionType(Unit().types);
    const mir::TypeId pointer = ElementPointerAt(mutability);
    return block.Add(
        mir::Expr{
            .data =
                mir::ConditionalExpr{
                    .condition = NamesAPosition(
                        block, block.ReadLocal(at, position_type),
                        Count(block, Array(block, array, mutability)),
                        mir::BinaryOp::kLessThan),
                    .then_value = GetUncheckedPointer(
                        block, ParameterArray(array, mutability),
                        BuildToInt64Call(
                            Unit(), block, block.ReadLocal(at, position_type)),
                        mutability),
                    .else_value = NoElement(block, absent, pointer)},
            .type = pointer});
  }

  // LRM 7.4.5, 7.10.1: where the element a position names lies, and where the
  // element default does where it names none, which a read answers without the
  // array growing.
  auto ElementRead() -> mir::CallableCode {
    return Defined(
        Declaration(support::BuiltinFn::kElement),
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          const mir::LocalId at = HandedPosition(code, block, p[1]);
          block.AppendStmt(
              mir::ReturnStmt{
                  .value = Reached(
                      block, p[0], at, mir::Mutability::kReadOnly,
                      support::BuiltinFn::kElementDefault)});
        });
  }

  // What a write's step finds: where the element lies, or no address, and
  // whether the write made it.
  auto Found(mir::Builder block, mir::ExprId element, mir::ExprId made)
      -> mir::ExprId {
    return block.Add(
        mir::Expr{
            .data = mir::CompositeExpr{.parts = {element, made}},
            .type = mir::FoundElementType(Unit(), ElementType())});
  }
  auto NoAddress(mir::Builder block) -> mir::ExprId {
    return block.Add(
        mir::Expr{.data = mir::NullLiteral{}, .type = ElementPointer()});
  }

  // LRM 7.4.5, 7.5, 7.10.1: where a write through a position lands, told where
  // the write was written. A dynamic array's element is there where the
  // position names one; a queue's is, and one past the last appends the
  // element default first, the queue then held to its bound (LRM 7.10.5),
  // which may drop it again; any other position names none, which a queue says
  // where the write was written.
  auto ElementPlace() -> mir::CallableCode {
    const mir::TypeId position_type = mir::PositionType(Unit().types);
    return Defined(
        Declaration(support::ElementAccessItem::kElementPlace),
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          const mir::LocalId at = HandedPosition(code, block, p[1]);
          const auto lying = [&](mir::Builder in) {
            return GetUncheckedPointer(
                in, ParameterArray(p[0], mir::Mutability::kMutable),
                BuildToInt64Call(Unit(), in, in.ReadLocal(at, position_type)),
                mir::Mutability::kMutable);
          };
          const auto names = [&](mir::Builder in, mir::BinaryOp op) {
            return NamesAPosition(
                in, in.ReadLocal(at, position_type),
                Count(in, Written(in, p[0])), op);
          };
          mir::Block held;
          mir::Builder held_builder = block.Into(held);
          held_builder.AppendStmt(
              mir::ReturnStmt{
                  .value = Found(
                      held_builder, lying(held_builder),
                      BuildMachineBool(Unit(), held_builder, false))});
          block.AppendIfThen(
              names(block, mir::BinaryOp::kLessThan), std::move(held));
          // Only a queue grows by being written, and says where a write
          // through an invalid index was written (LRM 7.10.1).
          std::visit(
              Overloaded{
                  [](const FixedSizeKind&) {
                    throw InternalError(
                        "hir_to_mir: a fixed-size array is asked where a write "
                        "through a position lands, which its layout answers -- "
                        "please report this as a bug");
                  },
                  [](const DynamicArrayKind&) {},
                  [&](const QueueKind&) {
                    mir::Block appended;
                    mir::Builder appended_builder = block.Into(appended);
                    PutAtSide(
                        code, appended_builder, ParameterQueue(p[0]),
                        QueueSide::kBack, [&](mir::Builder in) {
                          return in.Add(
                              BuildDefaultValueExpr(Unit(), in, ElementType()));
                        });
                    const mir::LocalId kept = Declared(
                        code, appended_builder, Unit().builtins.machine_bool,
                        names(appended_builder, mir::BinaryOp::kLessThan));
                    const mir::TypeId pointer = ElementPointer();
                    appended_builder.AppendStmt(
                        mir::ReturnStmt{
                            .value = Found(
                                appended_builder,
                                appended_builder.Add(
                                    mir::Expr{
                                        .data =
                                            mir::ConditionalExpr{
                                                .condition =
                                                    appended_builder.ReadLocal(
                                                        kept,
                                                        Unit()
                                                            .builtins
                                                            .machine_bool),
                                                .then_value =
                                                    lying(appended_builder),
                                                .else_value = NoAddress(
                                                    appended_builder)},
                                        .type = pointer}),
                                appended_builder.ReadLocal(
                                    kept, Unit().builtins.machine_bool))});
                    block.AppendIfThen(
                        names(block, mir::BinaryOp::kEquality),
                        std::move(appended));
                    Statement(
                        block, block.Add(
                                   mir::MakeCallExpr(
                                       mir::Direct{
                                           .target = support::BuiltinFn::
                                               kQueueWrittenAtAnInvalidIndex},
                                       {}, Unit().builtins.void_type)));
                  }},
              kind_);
          block.AppendStmt(
              mir::ReturnStmt{
                  .value = Found(
                      block, NoAddress(block),
                      BuildMachineBool(Unit(), block, false))});
        });
  }

  // LRM 7.4.5, 7.5, 7.10.1: where the element a position names lies, to write
  // it, and where a write lands that the array discards where it names none:
  // a dynamic array's where it lies, and a queue's where its own body finding
  // where a write lands puts it, which may append one.
  auto ElementWrite(support::ValueOperation answers) -> mir::CallableCode {
    const auto in_place = [&](const DynamicArrayKind&) {
      return Defined(
          Declaration(answers), [&](mir::CallableCode& code, mir::Builder block,
                                    const std::vector<mir::LocalId>& p) {
            const mir::LocalId at = HandedPosition(code, block, p[1]);
            block.AppendStmt(
                mir::ReturnStmt{
                    .value = Reached(
                        block, p[0], at, mir::Mutability::kMutable,
                        support::BuiltinFn::kDiscardedWrite)});
          });
    };
    const auto placed = [&](const QueueKind&) {
      return Defined(
          Declaration(answers), [&](mir::CallableCode& code, mir::Builder block,
                                    const std::vector<mir::LocalId>& p) {
            const mir::TypeId found_type =
                mir::FoundElementType(Unit(), ElementType());
            const mir::TypeId pointer = ElementPointer();
            const mir::LocalId found = Declared(
                code, block, found_type,
                block.Add(*ContainerOperationCall(
                    Unit(), block, support::ElementAccessItem::kElementPlace,
                    Written(block, p[0]),
                    {block.AsValue(
                        Unit().types,
                        Referent(
                            block, p[1], mir::PositionType(Unit().types)))},
                    found_type)));
            const auto element = [&](mir::Builder in) {
              return in.Read(in.Add(
                  mir::MakeFieldAccessExpr(
                      in.LocalPlace(found, found_type),
                      mir::ComponentFieldTarget{
                          .position = base::ComponentIndex{0}},
                      pointer)));
            };
            block.AppendStmt(
                mir::ReturnStmt{
                    .value = block.Add(
                        mir::Expr{
                            .data =
                                mir::ConditionalExpr{
                                    .condition = IsNull(block, element(block)),
                                    .then_value = NoElement(
                                        block,
                                        support::BuiltinFn::kDiscardedWrite,
                                        pointer),
                                    .else_value = element(block)},
                            .type = pointer})});
          });
    };
    return std::visit(
        Overloaded{
            [](const FixedSizeKind&) -> mir::CallableCode {
              throw InternalError(
                  "hir_to_mir: a fixed-size array's element is written by a "
                  "step within its layout -- please report this as a bug");
            },
            in_place, placed},
        kind_);
  }

  // LRM 7.10.2.6, 7.10.2.7: an element built from the value `value(block)`
  // makes put at the front or the back of the queue `queue` names, in storage
  // its recycler hands out, the ring grown first where it is full, and a
  // bounded queue then held to its bound (LRM 7.10.5).
  template <typename Value>
  void PutAtSide(
      mir::CallableCode& code, mir::Builder block, const PlaceAt& queue,
      QueueSide side, const Value& value) {
    const mir::TypeId machine = Unit().builtins.machine_int64;
    const mir::LocalId count = Declared(
        code, block, machine,
        CoreCount(block, queue, &value::QueueCore::count));
    mir::Block full;
    mir::Builder full_builder = block.Into(full);
    Statement(
        full_builder,
        Core(
            full_builder, support::BuiltinFn::kQueueReserve,
            queue(full_builder),
            {mir::BuildMachineIntBinary(
                Unit(), full_builder, mir::BinaryOp::kAdd,
                full_builder.ReadLocal(count, machine),
                mir::BuildMachineIntLiteral(Unit(), full_builder, 1))},
            Unit().builtins.void_type));
    // A queue with no ring holds no element and has room for none.
    block.AppendIfThen(
        mir::BuildMachineIntComparison(
            Unit(), block, mir::BinaryOp::kEquality,
            block.ReadLocal(count, machine),
            mir::BuildMachineIntConditional(
                Unit(), block,
                IsNull(
                    block, block.Read(CoreMember(
                               block, queue, &value::QueueCore::ring))),
                mir::BuildMachineIntLiteral(Unit(), block, 0),
                mir::BuildMachineIntBinary(
                    Unit(), block, mir::BinaryOp::kAdd,
                    CoreCount(block, queue, &value::QueueCore::mask),
                    mir::BuildMachineIntLiteral(Unit(), block, 1)))),
        std::move(full));
    const mir::LocalId storage = AllocatedStorage(code, block, queue);
    Statement(
        block,
        block.Add(
            mir::MakePlacementNewExpr(
                value(block),
                mir::PlacementAllocation{
                    .allocate = support::BuiltinFn::kDefaultPlacementNew,
                    .arguments = {CastTo(
                        block,
                        block.ReadLocal(storage, ElementStoragePointer()),
                        ElementPointer())}},
                ElementPointer())));
    const mir::ExprId entry = [&] {
      switch (side) {
        case QueueSide::kFront:
          SetCoreCount(
              block, queue, &value::QueueCore::head,
              mir::BuildMachineIntBinary(
                  Unit(), block, mir::BinaryOp::kBitwiseAnd,
                  mir::BuildMachineIntBinary(
                      Unit(), block, mir::BinaryOp::kAdd,
                      CoreCount(block, queue, &value::QueueCore::head),
                      CoreCount(block, queue, &value::QueueCore::mask)),
                  CoreCount(block, queue, &value::QueueCore::mask)));
          return CoreCount(block, queue, &value::QueueCore::head);
        case QueueSide::kBack:
          return EntryOf(block, queue, block.ReadLocal(count, machine));
      }
      throw InternalError("hir_to_mir: unknown side of a queue");
    }();
    Statement(
        block,
        block.Add(
            mir::MakeAssignExpr(
                Unit().builtins, RingEntry(block, queue, entry),
                CastTo(
                    block, block.ReadLocal(storage, ElementStoragePointer()),
                    Address()))));
    SetCoreCount(
        block, queue, &value::QueueCore::count,
        mir::BuildMachineIntBinary(
            Unit(), block, mir::BinaryOp::kAdd, block.ReadLocal(count, machine),
            mir::BuildMachineIntLiteral(Unit(), block, 1)));
    HoldToBound(code, block, queue);
  }

  // LRM 7.10.2.4, 7.10.2.5: the element at the front or the back of the queue
  // `queue` names, holding `count` elements, at least one,
  // taken out of its order and answered: its own value, its storage given back,
  // unless a call leases it, which goes on reaching it until the call returns
  // and so leaves the answer a copy, the queue letting it go to the last lease
  // (LRM 13.5.2).
  void TakeAtSide(
      mir::CallableCode& code, mir::Builder block, const PlaceAt& queue,
      QueueSide side, mir::LocalId count) {
    const mir::TypeId machine = Unit().builtins.machine_int64;
    const mir::ExprId entry = [&] {
      switch (side) {
        case QueueSide::kFront:
          return CoreCount(block, queue, &value::QueueCore::head);
        case QueueSide::kBack:
          return EntryOf(
              block, queue,
              mir::BuildMachineIntBinary(
                  Unit(), block, mir::BinaryOp::kSub,
                  block.ReadLocal(count, machine),
                  mir::BuildMachineIntLiteral(Unit(), block, 1)));
      }
      throw InternalError("hir_to_mir: unknown side of a queue");
    }();
    const mir::LocalId storage = Declared(
        code, block, ElementStoragePointer(),
        CastTo(
            block, block.Read(RingEntry(block, queue, entry)),
            ElementStoragePointer()));
    switch (side) {
      case QueueSide::kFront:
        SetCoreCount(
            block, queue, &value::QueueCore::head,
            mir::BuildMachineIntBinary(
                Unit(), block, mir::BinaryOp::kBitwiseAnd,
                mir::BuildMachineIntBinary(
                    Unit(), block, mir::BinaryOp::kAdd,
                    CoreCount(block, queue, &value::QueueCore::head),
                    mir::BuildMachineIntLiteral(Unit(), block, 1)),
                CoreCount(block, queue, &value::QueueCore::mask)));
        break;
      case QueueSide::kBack:
        break;
    }
    SetCoreCount(
        block, queue, &value::QueueCore::count,
        mir::BuildMachineIntBinary(
            Unit(), block, mir::BinaryOp::kSub, block.ReadLocal(count, machine),
            mir::BuildMachineIntLiteral(Unit(), block, 1)));
    const auto answer = [&](mir::Builder at, mir::LocalId kept) {
      at.AppendStmt(
          mir::ReturnStmt{
              .value = at.Add(
                  mir::Expr{
                      .data =
                          mir::MoveExpr{
                              .operand = at.LocalPlace(kept, ElementType())},
                      .type = ElementType()})});
    };
    mir::Block leased;
    mir::Builder leased_builder = block.Into(leased);
    const mir::LocalId copy = Declared(
        code, leased_builder, ElementType(),
        leased_builder.Read(ElementIn(
            leased_builder,
            leased_builder.ReadLocal(storage, ElementStoragePointer()))));
    Detach(leased_builder, storage);
    answer(leased_builder, copy);
    block.AppendIfThen(Leased(block, storage), std::move(leased));
    const mir::LocalId taken = Declared(
        code, block, ElementType(),
        block.Add(
            mir::MakeCallExpr(
                mir::Direct{.target = support::BuiltinFn::kPtrRead},
                {CastTo(
                    block, block.ReadLocal(storage, ElementStoragePointer()),
                    ElementPointer())},
                ElementType())));
    Deallocate(block, queue, storage);
    answer(block, taken);
  }

  // How many pairs of elements a comparison folds before it asks whether the
  // answer is settled.
  static constexpr std::int64_t kPairsPerRun = 256;

  // The pairs of elements at each machine index of the arrays the first two
  // parameters refer to, compared a run at a time, as `memcmp` compares a
  // block of bytes at a time: `start(run)` sets up what one run keeps,
  // `fold(run, index)` folds one pair into it with no exit between two pairs,
  // so the optimizer compares a run's pairs together, and `settled(run)` then
  // returns where the run has settled the answer. A run that settles nothing
  // leaves the rest to the next one, so an answer the first pairs settle reads
  // no more than one run. Where the two hold different numbers no element is
  // paired, and what `unequal` builds is returned (LRM 7.6, 11.4.5).
  template <typename Start, typename Fold, typename Settled, typename Unequal>
  void ComparedInRuns(
      mir::CallableCode& code, mir::Builder block,
      const std::vector<mir::LocalId>& p, Start start, Fold fold,
      Settled settled, Unequal unequal) {
    ReturnWhere(
        block,
        mir::BuildMachineIntComparison(
            Unit(), block, mir::BinaryOp::kInequality,
            ElementCount(Unit(), block, WholeType(), Read(block, p[0])),
            ElementCount(Unit(), block, WholeType(), Read(block, p[1]))),
        unequal);
    const mir::LocalId count = DeclaredCount(
        code, block,
        ElementCount(Unit(), block, WholeType(), Read(block, p[0])));
    ForEachRun(
        code, block, count,
        [&](mir::Builder run, mir::LocalId from, mir::LocalId to) {
          start(run);
          ForEachIndexBetween(
              code, run, CountNow(run, from), CountNow(run, to), fold);
          settled(run);
        });
  }

  // Writes into the machine boolean `kept` what it held combined by `op` with
  // the condition `holds`, both evaluated whatever the other is, so a fold of
  // them leaves the optimizer no exit between two pairs.
  void Combine(
      mir::Builder body, mir::LocalId kept, mir::BinaryOp op,
      mir::ExprId holds) {
    const mir::TypeId boolean = Unit().builtins.machine_bool;
    const mir::ExprId so_far = body.ReadLocal(kept, boolean);
    Statement(
        body,
        body.Add(
            mir::MakeAssignExpr(
                Unit().builtins, body.LocalPlace(kept, boolean),
                body.Add(
                    mir::MakeBinary(
                        Unit(), body.Body(), op, so_far,
                        ReduceToCondition(Unit(), body, holds), boolean)))));
  }

  // A question every pair has to answer yes to, answered for the arrays
  // the first two parameters refer to: `holds(run, index)` is one pair's
  // answer, and `yes` or `no` the whole's.
  template <typename Holds, typename Answer>
  void EveryPairHolds(
      mir::CallableCode& code, mir::Builder block,
      const std::vector<mir::LocalId>& p, Holds holds, Answer answer) {
    const mir::TypeId boolean = Unit().builtins.machine_bool;
    const mir::LocalId run_holds = code.AddLocal(boolean);
    ComparedInRuns(
        code, block, p,
        [&](mir::Builder run) {
          run.AppendStmt(
              mir::LocalDeclStmt{
                  .target = run_holds,
                  .init = BuildMachineBool(Unit(), run, true)});
        },
        [&](mir::Builder pair, mir::LocalId index) {
          Combine(
              pair, run_holds, mir::BinaryOp::kBitwiseAnd, holds(pair, index));
        },
        [&](mir::Builder run) {
          ReturnWhere(
              run,
              BuildConditionNot(Unit(), run, run.ReadLocal(run_holds, boolean)),
              [&](mir::Builder taken) { return answer(taken, false); });
        },
        [&](mir::Builder taken) { return answer(taken, false); });
    block.AppendStmt(mir::ReturnStmt{.value = answer(block, true)});
  }

  // LRM 11.4.5: equal when both hold as many elements and every pair is
  // equal, unknown where no pair is unequal and some pair is unknown. A pair
  // whose equality can be unknown is folded in two parts, whether it is known
  // unequal and whether it is unknown, so a run still has no exit between two
  // pairs.
  auto Equality() -> mir::CallableCode {
    const mir::TypeId answer = EqualityAnswer();
    const mir::TypeId boolean = Unit().builtins.machine_bool;
    const auto pair_equal = [&](mir::Builder pair, mir::LocalId index,
                                const std::vector<mir::LocalId>& p) {
      return PartEqual(
          pair, GetUnchecked(pair, Read(pair, p[0]), index),
          GetUnchecked(pair, Read(pair, p[1]), index), ElementType());
    };
    if (!mir::IsFourState(Unit().types.Get(answer).Integral().state_kind)) {
      return Defined(
          Declaration(support::EqualityItem::kEqual),
          [&](mir::CallableCode& code, mir::Builder block,
              const std::vector<mir::LocalId>& p) {
            EveryPairHolds(
                code, block, p,
                [&](mir::Builder pair, mir::LocalId index) {
                  return pair_equal(pair, index, p);
                },
                [&](mir::Builder at, bool holds) {
                  return KnownBit(at, answer, holds);
                });
          });
    }
    return Defined(
        Declaration(support::EqualityItem::kEqual),
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          const mir::LocalId unknown = Declared(
              code, block, boolean, BuildMachineBool(Unit(), block, false));
          const mir::LocalId run_unequal = code.AddLocal(boolean);
          ComparedInRuns(
              code, block, p,
              [&](mir::Builder run) {
                run.AppendStmt(
                    mir::LocalDeclStmt{
                        .target = run_unequal,
                        .init = BuildMachineBool(Unit(), run, false)});
              },
              [&](mir::Builder pair, mir::LocalId index) {
                const mir::LocalId equal =
                    Declared(code, pair, answer, pair_equal(pair, index, p));
                Combine(
                    pair, run_unequal, mir::BinaryOp::kBitwiseOr,
                    pair.Add(
                        mir::MakeUnary(
                            Unit(), pair.Body(), mir::UnaryOp::kLogicalNot,
                            pair.ReadLocal(equal, answer), answer)));
                Combine(
                    pair, unknown, mir::BinaryOp::kBitwiseOr,
                    BuildValueOperation(
                        Unit(), pair, support::UnknownItem::kHasUnknown,
                        pair.ReadLocal(equal, answer), {}, boolean));
              },
              [&](mir::Builder run) {
                ReturnWhere(
                    run, run.ReadLocal(run_unequal, boolean),
                    [&](mir::Builder taken) {
                      return KnownBit(taken, answer, false);
                    });
              },
              [&](mir::Builder taken) {
                return KnownBit(taken, answer, false);
              });
          block.AppendStmt(
              mir::ReturnStmt{
                  .value = Chosen(
                      block, block.ReadLocal(unknown, boolean),
                      mir::BuildIntegralLiteral(
                          Unit(), block, answer,
                          mir::IntegralConstant{
                              .value_words = {1U}, .state_words = {1U}}),
                      KnownBit(block, answer, true), answer)});
        });
  }

  // LRM 11.4.5 `===`, which is never unknown.
  auto CaseEqual() -> mir::CallableCode {
    return Defined(
        Declaration(support::EqualityItem::kCaseEqual),
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          EveryPairHolds(
              code, block, p,
              [&](mir::Builder pair, mir::LocalId index) {
                return BuildCaseEquality(
                    Unit(), pair, GetUnchecked(pair, Read(pair, p[0]), index),
                    GetUnchecked(pair, Read(pair, p[1]), index));
              },
              [&](mir::Builder at, bool holds) {
                return mir::BuildBit1Literal(Unit(), at, holds);
              });
        });
  }

  // LRM 9.4.2: whether a write over the array changed nothing.
  auto BitIdentical() -> mir::CallableCode {
    return Defined(
        Declaration(support::ChangeTestItem::kBitIdentical),
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          EveryPairHolds(
              code, block, p,
              [&](mir::Builder pair, mir::LocalId index) {
                return BuildBitIdentity(
                    Unit(), pair, GetUnchecked(pair, Read(pair, p[0]), index),
                    GetUnchecked(pair, Read(pair, p[1]), index));
              },
              [&](mir::Builder at, bool holds) {
                return BuildMachineBool(Unit(), at, holds);
              });
        });
  }

  // LRM 20.9: whether any element holds an x or a z, settled by the first that
  // does.
  auto HasUnknown() -> mir::CallableCode {
    const mir::TypeId boolean = Unit().builtins.machine_bool;
    return Defined(
        Declaration(support::UnknownItem::kHasUnknown),
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          ForEachIndex(
              code, block,
              ElementCount(Unit(), block, WholeType(), Read(block, p[0])),
              [&](mir::Builder body, mir::LocalId index) {
                ReturnWhere(
                    body,
                    BuildValueOperation(
                        Unit(), body, support::UnknownItem::kHasUnknown,
                        GetUnchecked(body, Read(body, p[0]), index), {},
                        boolean),
                    [&](mir::Builder taken) {
                      return BuildMachineBool(Unit(), taken, true);
                    });
              });
          block.AppendStmt(
              mir::ReturnStmt{.value = BuildMachineBool(Unit(), block, false)});
        });
  }

  // LRM 7.10.5: the queue held to its bound.
  auto HoldToBoundBody() -> mir::CallableCode {
    return Defined(
        Declaration(support::BuiltinFn::kQueueHoldToBound),
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          HoldToBound(code, block, ParameterQueue(p[0]));
        });
  }

  // LRM 7.6: a fixed-size array of this type taking the elements of the
  // run-time-sized array the method is taken at, in order, whose own bodies
  // build it; a source of another count is the design's own error.
  auto FromRunTimeSized(const mir::TypeOperationSource& source)
      -> mir::CallableCode {
    return ArrayBodies(Unit(), source.arguments.at(0), ElementType())
        .IntoFixedArray(WholeType());
  }

  ArrayKind kind_;

  friend struct ArrayMethodBodies;
};

// Each kind's inherent impl: the body of every method MIR declares for it.
struct ArrayMethodBodies {
  using MethodBody = ArrayBodies::MethodBody;

  // The methods on a run of elements every kind whose elements a position
  // names has, and those of an array whose elements lie one after another.
  static constexpr MethodBody kWindowToBitVecBody{
      .name = support::BuiltinFn::kWindowToBitVec,
      .build = [](ArrayBodies& b, const mir::TypeOperationSource&) {
        return b.WindowToBitVec();
      }};
  static constexpr MethodBody kWithWindowBody{
      .name = support::BuiltinFn::kWithWindow,
      .build = [](ArrayBodies& b, const mir::TypeOperationSource&) {
        return b.WithWindow();
      }};
  static constexpr MethodBody kReverseBody{
      .name = support::BuiltinFn::kReverse,
      .build = [](ArrayBodies& b, const mir::TypeOperationSource&) {
        return b.Reverse();
      }};
  static constexpr MethodBody kApplyOrderBody{
      .name = support::BuiltinFn::kApplyOrder,
      .build = [](ArrayBodies& b, const mir::TypeOperationSource&) {
        return b.ApplyOrder();
      }};
  static constexpr MethodBody kElementSliceBody{
      .name = support::BuiltinFn::kElementSlice,
      .build = [](ArrayBodies& b, const mir::TypeOperationSource& s) {
        return b.ElementSlice(s.arguments.at(0));
      }};
  static constexpr MethodBody kAssignElementSliceBody{
      .name = support::BuiltinFn::kAssignElementSlice,
      .build = [](ArrayBodies& b, const mir::TypeOperationSource& s) {
        return b.AssignElementSlice(s.arguments.at(0));
      }};
  static constexpr MethodBody kSizeBody{
      .name = support::BuiltinFn::kSize,
      .build = [](ArrayBodies& b, const mir::TypeOperationSource&) {
        return b.Size();
      }};
  static constexpr MethodBody kElementBody{
      .name = support::BuiltinFn::kElement,
      .build = [](ArrayBodies& b, const mir::TypeOperationSource&) {
        return b.ElementRead();
      }};
  static constexpr MethodBody kDeleteBody{
      .name = support::BuiltinFn::kDelete,
      .build = [](ArrayBodies& b, const mir::TypeOperationSource&) {
        return b.Delete();
      }};

  // A fixed-size array's own methods (LRM 7.4).
  static constexpr auto kFixedArrayBodies = std::to_array<MethodBody>({
      kElementSliceBody,
      kAssignElementSliceBody,
      kWindowToBitVecBody,
      kWithWindowBody,
      kReverseBody,
      kApplyOrderBody,
      {.name = support::BuiltinFn::kUnpackedArrayFromArray,
       .build =
           [](ArrayBodies& b, const mir::TypeOperationSource& s) {
             return b.FromRunTimeSized(s);
           }},
  });

  // A dynamic array's own methods (LRM 7.5).
  static constexpr auto kDynamicArrayBodies = std::to_array<MethodBody>({
      {.name = support::BuiltinFn::kDynamicArrayNew,
       .build = [](ArrayBodies& b,
                   const mir::TypeOperationSource&) { return b.New(); }},
      {.name = support::BuiltinFn::kDynamicArrayNewCopy,
       .build = [](ArrayBodies& b,
                   const mir::TypeOperationSource&) { return b.NewCopy(); }},
      kElementBody,
      {.name = support::BuiltinFn::kElementMut,
       .build =
           [](ArrayBodies& b, const mir::TypeOperationSource& s) {
             return b.ElementWrite(s.answers);
           }},
      kDeleteBody,
      kSizeBody,
      {.name = support::BuiltinFn::kDynamicArrayFromArray,
       .build =
           [](ArrayBodies& b, const mir::TypeOperationSource& s) {
             return b.FromArray(s.answers, s.arguments.at(0));
           }},
      kElementSliceBody,
      kAssignElementSliceBody,
      kWindowToBitVecBody,
      kWithWindowBody,
      kReverseBody,
      kApplyOrderBody,
  });

  // A queue's own methods (LRM 7.10).
  static constexpr auto kQueueBodies = std::to_array<MethodBody>({
      kSizeBody,
      kElementBody,
      {.name = support::BuiltinFn::kQueueElementMut,
       .build =
           [](ArrayBodies& b, const mir::TypeOperationSource& s) {
             return b.ElementWrite(s.answers);
           }},
      kDeleteBody,
      {.name = support::BuiltinFn::kDeleteIndex,
       .build =
           [](ArrayBodies& b, const mir::TypeOperationSource&) {
             return b.DeleteIndex();
           }},
      {.name = support::BuiltinFn::kInsert,
       .build =
           [](ArrayBodies& b, const mir::TypeOperationSource& s) {
             return b.Inserted(s.answers, std::nullopt);
           }},
      {.name = support::BuiltinFn::kPushBack,
       .build =
           [](ArrayBodies& b, const mir::TypeOperationSource& s) {
             return b.Inserted(s.answers, QueueSide::kBack);
           }},
      {.name = support::BuiltinFn::kPushFront,
       .build =
           [](ArrayBodies& b, const mir::TypeOperationSource& s) {
             return b.Inserted(s.answers, QueueSide::kFront);
           }},
      {.name = support::BuiltinFn::kPopBack,
       .build =
           [](ArrayBodies& b, const mir::TypeOperationSource& s) {
             return b.Popped(s.answers, QueueSide::kBack);
           }},
      {.name = support::BuiltinFn::kPopFront,
       .build =
           [](ArrayBodies& b, const mir::TypeOperationSource& s) {
             return b.Popped(s.answers, QueueSide::kFront);
           }},
      {.name = support::BuiltinFn::kQueueSlice,
       .build = [](ArrayBodies& b,
                   const mir::TypeOperationSource&) { return b.QueueSlice(); }},
      {.name = support::BuiltinFn::kAssignQueueSlice,
       .build =
           [](ArrayBodies& b, const mir::TypeOperationSource&) {
             return b.AssignQueueSlice();
           }},
      {.name = support::BuiltinFn::kQueueAllocate,
       .build = [](ArrayBodies& b,
                   const mir::TypeOperationSource&) { return b.Allocate(); }},
      {.name = support::BuiltinFn::kQueueHoldToBound,
       .build =
           [](ArrayBodies& b, const mir::TypeOperationSource&) {
             return b.HoldToBoundBody();
           }},
      {.name = support::BuiltinFn::kQueueFromArray,
       .build =
           [](ArrayBodies& b, const mir::TypeOperationSource& s) {
             return b.FromArray(s.answers, s.arguments.at(0));
           }},
      kWindowToBitVecBody,
      kWithWindowBody,
      kReverseBody,
      kApplyOrderBody,
  });
};

// Each kind's table builds exactly the methods MIR declares for the kind.
static_assert(mir::NamesTheSameMethods(
    mir::kFixedArrayMethods, ArrayMethodBodies::kFixedArrayBodies));
static_assert(mir::NamesTheSameMethods(
    mir::kDynamicArrayMethods, ArrayMethodBodies::kDynamicArrayBodies));
static_assert(mir::NamesTheSameMethods(
    mir::kQueueMethods, ArrayMethodBodies::kQueueBodies));

auto ArrayBodies::MethodBodyOf(support::BuiltinFn name) const
    -> const MethodBody& {
  const std::span<const MethodBody> rows = std::visit(
      Overloaded{
          [](const FixedSizeKind&) {
            return std::span<const MethodBody>(
                ArrayMethodBodies::kFixedArrayBodies);
          },
          [](const DynamicArrayKind&) {
            return std::span<const MethodBody>(
                ArrayMethodBodies::kDynamicArrayBodies);
          },
          [](const QueueKind&) {
            return std::span<const MethodBody>(ArrayMethodBodies::kQueueBodies);
          }},
      kind_);
  for (const MethodBody& row : rows) {
    if (row.name == name) {
      return row;
    }
  }
  NoBodyBuilt();
}

// How many elements the array `array` lies in holds, an array of `type` of any
// kind, as a machine count: its type's count for a fixed-size one, and what
// its core holds for a run-time-sized one.
auto ElementCount(
    const mir::CompilationUnit& unit, mir::Builder block, mir::TypeId type,
    mir::ExprId array) -> mir::ExprId {
  return std::visit(
      Overloaded{
          [&](const FixedSizeKind& fixed) {
            return mir::BuildMachineIntLiteral(unit, block, fixed.size);
          },
          [&](const DynamicArrayKind&) {
            return block.Add(
                mir::Expr{
                    .data =
                        mir::CastExpr{
                            .operand = block.Read(
                                mir::LibraryRecordMemberPlace(
                                    unit, block, array,
                                    &value::DynamicArrayCore::count))},
                    .type = unit.builtins.machine_int64});
          },
          [&](const QueueKind&) {
            return block.Add(
                mir::Expr{
                    .data =
                        mir::CastExpr{
                            .operand = block.Read(
                                mir::LibraryRecordMemberPlace(
                                    unit, block, array,
                                    &value::QueueCore::count))},
                    .type = unit.builtins.machine_int64});
          }},
      KindOf(unit, type));
}

}  // namespace

auto BuildDynamicArrayNew(
    const mir::CompilationUnit& unit, mir::Builder block, mir::TypeId type,
    mir::ExprId count, std::optional<mir::ExprId> source) -> mir::Expr {
  std::vector<mir::ExprId> arguments{mir::Borrow(unit, block, count)};
  if (source.has_value()) {
    arguments.push_back(mir::Borrow(unit, block, *source));
  }
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{
                      .target =
                          mir::TypeOperationTarget{
                              .type = type,
                              .answers =
                                  source.has_value()
                                      ? support::BuiltinFn::kDynamicArrayNewCopy
                                      : support::BuiltinFn::kDynamicArrayNew}},
              .arguments = std::move(arguments)},
      .type = type};
}

namespace {

// A container of `type` holding `count` replications of `items`, in order: the
// container `start(steps, body, total)` makes for that many elements, kept in
// a local, the item `j` of pass `k` laid down at `k * items + j` as
// `place(at, built, landing, item)` lays it in that local, and the container
// then as `finish(body, built)` leaves it.
template <typename Start, typename Place, typename Finish>
auto FilledFromList(
    const mir::CompilationUnit& unit, const WalkFrame& frame, mir::TypeId type,
    const std::vector<mir::ExprId>& items, mir::ExprId count, Start start,
    Place place, Finish finish) -> mir::Expr {
  const mir::TypeId element =
      unit.types.Get(type).ContainerElementType().value();
  const mir::TypeId machine = unit.builtins.machine_int64;
  std::vector<mir::LocalId> held;
  held.reserve(items.size());
  for (const mir::ExprId item : items) {
    held.push_back(DeclareLocal(frame, item));
  }
  const mir::LocalId repeats = DeclareLocal(frame, count);

  BlockBuilder steps(frame);
  mir::Builder body = steps.Body();
  const mir::ExprId total = mir::BuildMachineIntBinary(
      unit, body, mir::BinaryOp::kMul, body.ReadLocal(repeats, machine),
      mir::BuildMachineIntLiteral(
          unit, body, static_cast<std::int64_t>(items.size())));
  const mir::LocalId built = start(steps, body, total);

  const mir::LocalId pass =
      steps.DeclareLocal(machine, mir::BuildMachineIntLiteral(unit, body, 0));
  mir::Block each;
  mir::Builder at = body.Into(each);
  for (std::size_t j = 0; j < held.size(); ++j) {
    const mir::ExprId landing = mir::BuildMachineIntBinary(
        unit, at, mir::BinaryOp::kAdd,
        mir::BuildMachineIntBinary(
            unit, at, mir::BinaryOp::kMul, at.ReadLocal(pass, machine),
            mir::BuildMachineIntLiteral(
                unit, at, static_cast<std::int64_t>(held.size()))),
        mir::BuildMachineIntLiteral(unit, at, static_cast<std::int64_t>(j)));
    place(at, built, landing, at.ReadLocal(held[j], element));
  }
  at.AppendStmt(
      mir::ExprStmt{
          .expr = at.Add(
              mir::MakeAssignExpr(
                  unit.builtins, at.LocalPlace(pass, machine),
                  mir::BuildMachineIntBinary(
                      unit, at, mir::BinaryOp::kAdd,
                      at.ReadLocal(pass, machine),
                      mir::BuildMachineIntLiteral(unit, at, 1))))});
  const mir::BlockId scope = body.AddScope(std::move(each));
  body.AppendStmt(
      mir::WhileStmt{
          .condition = mir::BuildMachineIntComparison(
              unit, body, mir::BinaryOp::kLessThan,
              body.ReadLocal(pass, machine), body.ReadLocal(repeats, machine)),
          .scope = scope});
  finish(body, built);
  return steps.Build(body.ReadLocal(built, type));
}

}  // namespace

auto BuildContainerFromList(
    const mir::CompilationUnit& unit, const WalkFrame& frame, mir::TypeId type,
    std::vector<mir::ExprId> items, mir::ExprId count) -> mir::Expr {
  const mir::TypeId element =
      unit.types.Get(type).ContainerElementType().value();
  const mir::TypeId nothing = unit.builtins.void_type;
  const auto core = [&](mir::Builder at, support::BuiltinFn fn,
                        mir::ExprId array, std::vector<mir::ExprId> arguments) {
    at.AppendStmt(
        mir::ExprStmt{
            .expr = at.Add(
                mir::Expr{
                    .data =
                        mir::CallExpr{
                            .callee =
                                mir::Direct{.target = fn, .receiver = array},
                            .arguments = std::move(arguments)},
                    .type = nothing})});
  };
  const auto none = [](mir::Builder, mir::LocalId) {};
  // A dynamic array is made holding every element at once, each then written
  // where it lies.
  const auto dynamic_array = [&](const DynamicArrayKind&) {
    return FilledFromList(
        unit, frame, type, items, count,
        [&](BlockBuilder& steps, mir::Builder body, mir::ExprId total) {
          return steps.DeclareLocal(
              type, body.Add(BuildDynamicArrayNew(unit, body, type, total)));
        },
        [&](mir::Builder at, mir::LocalId built, mir::ExprId landing,
            mir::ExprId item) {
          // Every landing counts below the elements the array was made with.
          at.AppendStmt(
              mir::ExprStmt{
                  .expr = at.Add(BuildStoreExpr(
                      unit, at,
                      AccessPath{
                          .owner = UncheckedElement(
                              unit, at, at.LocalPlace(built, type), landing,
                              element, mir::Mutability::kMutable),
                          .descent = {}},
                      item))});
        },
        none);
  };
  // A queue is made holding none, with room for every one, each built in
  // storage its core hands out; a list longer than its bound leaves it
  // holding what the bound admits, said once (LRM 7.10.5).
  const auto queue = [&](const QueueKind&) {
    const mir::TypeId element_pointer =
        mir::BorrowedPointerTo(unit.types, element, mir::Mutability::kMutable);
    return FilledFromList(
        unit, frame, type, items, count,
        [&](BlockBuilder& steps, mir::Builder body, mir::ExprId total) {
          const mir::LocalId made = steps.DeclareLocal(
              type,
              body.Add(
                  mir::Expr{
                      .data =
                          mir::CallExpr{
                              .callee = mir::Construct{}, .arguments = {}},
                      .type = type}));
          core(
              body, support::BuiltinFn::kQueueReserve,
              body.LocalPlace(made, type), {total});
          return made;
        },
        [&](mir::Builder at, mir::LocalId built, mir::ExprId landing,
            mir::ExprId item) {
          core(
              at, support::BuiltinFn::kQueueCommit, at.LocalPlace(built, type),
              {landing, MadeInQueueStorage(
                            unit, at, at.LocalPlace(built, type), item,
                            element_pointer)});
        },
        [&](mir::Builder body, mir::LocalId built) {
          body.AppendStmt(
              mir::ExprStmt{
                  .expr = body.Add(*ContainerOperationCall(
                      unit, body, support::BuiltinFn::kQueueHoldToBound,
                      body.LocalPlace(built, type), {},
                      unit.builtins.void_type))});
        });
  };
  return std::visit(
      Overloaded{
          [](const FixedSizeKind&) -> mir::Expr {
            throw InternalError(
                "hir_to_mir: a fixed-size array is built as a container from "
                "a list -- please report this as a bug");
          },
          dynamic_array, queue},
      KindOf(unit, type));
}

auto ContainerOperationCall(
    const mir::CompilationUnit& unit, mir::Builder block,
    support::ValueOperation method, mir::ExprId receiver,
    const std::vector<mir::ExprId>& operands, mir::TypeId result_type)
    -> std::optional<mir::Expr> {
  const mir::TypeId type = block.Get(receiver).type;
  const std::optional<mir::CallableCode> declared = mir::OperationDeclarationOf(
      unit, mir::TypeOperationSource{.type = type, .answers = method});
  if (!declared.has_value() || !declared->receiver.has_value()) {
    return std::nullopt;
  }
  PassedOperands passed =
      PassedAsDeclared(unit, block, *declared, receiver, operands);
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{
                      .target =
                          mir::TypeOperationTarget{
                              .type = type, .answers = method},
                      .receiver = passed.receiver},
              .arguments = std::move(passed.arguments)},
      .type = result_type};
}

auto BuildArrayOperationBody(
    const mir::CompilationUnit& unit, const mir::TypeOperationSource& source)
    -> mir::CallableCode {
  const mir::TypeId type = source.type;
  const std::optional<mir::TypeId> element =
      unit.types.Get(type).ContainerElementType();
  if (!element.has_value()) {
    throw InternalError(
        "hir_to_mir: an array's operation is asked of a type that is no "
        "array -- please report this as a bug");
  }
  return ArrayBodies(unit, type, *element).Build(source);
}

}  // namespace lyra::lowering::hir_to_mir
