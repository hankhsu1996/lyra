#include "lyra/mir/type_interface.hpp"

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
#include <format>
#include <optional>
#include <span>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/array_impls.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/code_source.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/mir/value_parts.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/library_decl.hpp"
#include "lyra/support/value_operation.hpp"
#include "lyra/support/value_step.hpp"

namespace lyra::mir {

namespace {

// What a type whose operations its parts decide has of the operations a type
// may lack: a stream (LRM 6.24.3), one of fixed width, and, where its width is
// the running program's, a stream written out where the type holds no class
// handle, whose object's stream is not yet built (LRM 11.4.14.1), and one read
// back where the type may be a destination; its positions read out and built
// from, where every bit it has is a fixed one (LRM 7.3); and the positions a
// net resolved written in, where a net resolves it (LRM 6.7.1).
struct OptionalOperations {
  bool stream;
  bool fixed_stream;
  bool run_written;
  bool run_read;
  bool positions;
  bool net_positions;
};

// Whether a stream of `shape` is one a value is written into and read out of,
// which takes a fixed width of at least one bit.
auto IsFixedStream(const std::optional<StreamShape>& shape) -> bool {
  return shape.has_value() && shape->width != 0;
}

// What a fixed-size array has, read off the array itself.
auto OptionalOperationsOfArray(const CompilationUnit& unit, TypeId array)
    -> OptionalOperations {
  const bool stream = HasBitStream(unit, array);
  const bool fixed_stream = IsFixedStream(FixedStreamShapeOf(unit, array));
  return OptionalOperations{
      .stream = stream,
      .fixed_stream = fixed_stream,
      .run_written = stream && !fixed_stream && !HoldsClassHandle(unit, array),
      .run_read = stream && !fixed_stream && IsStreamDestination(unit, array),
      .positions = HasPositions(unit, array),
      .net_positions = NetResolves(unit, array)};
}

// What a product -- a structure or a tuple -- has, read off its components: a
// stream where every component has one, written out as a run of bits where
// none holds a class handle and read back where every one may be a
// destination, positions where every component has them, and a net's where a
// net resolves every component.
auto OptionalOperationsOfProduct(
    const CompilationUnit& unit, std::span<const TypeId> components)
    -> OptionalOperations {
  const auto every = [&](auto predicate) {
    return std::ranges::all_of(
        components, [&](TypeId part) { return predicate(unit, part); });
  };
  const bool stream = every(HasBitStream);
  const bool fixed_stream =
      IsFixedStream(FixedStreamShapeOfParts(unit, components));
  return OptionalOperations{
      .stream = stream,
      .fixed_stream = fixed_stream,
      .run_written = stream && !fixed_stream &&
                     every([](const CompilationUnit& in, TypeId part) {
                       return !HoldsClassHandle(in, part);
                     }),
      .run_read = stream && !fixed_stream && every(IsStreamDestination),
      .positions = every(HasPositions),
      .net_positions = every(NetResolves)};
}

// What an untagged union has, read off its members: a stream as its first
// member streams (LRM 11.4.14.1), no stream whose width is the program's, and
// positions and a net's as every member has them (LRM 7.3, 6.7.1).
auto OptionalOperationsOfUnion(
    const CompilationUnit& unit, std::span<const TypeId> members)
    -> OptionalOperations {
  const auto every = [&](auto predicate) {
    return std::ranges::all_of(
        members, [&](TypeId part) { return predicate(unit, part); });
  };
  return OptionalOperations{
      .stream = HasBitStream(unit, members.front()),
      .fixed_stream = IsFixedStream(FixedStreamShapeOf(unit, members.front())),
      .run_written = false,
      .run_read = false,
      .positions = every(HasPositions),
      .net_positions = every(NetResolves)};
}

// What a tagged union has, read off its members: no stream (LRM 11.4.14.1),
// positions where every member has them, which an untagged union holding it
// is a view of (LRM 7.3), and no net's, which no net type's tables read.
auto OptionalOperationsOfTaggedUnion(
    const CompilationUnit& unit, std::span<const TypeId> members)
    -> OptionalOperations {
  return OptionalOperations{
      .stream = false,
      .fixed_stream = false,
      .run_written = false,
      .run_read = false,
      .positions = std::ranges::all_of(
          members, [&](TypeId part) { return HasPositions(unit, part); }),
      .net_positions = false};
}

// The operations on a whole value every type whose parts decide them has:
// its default, its equalities, its bit identity and unknowns, taking some of
// its positions from another, and how many positions it holds, which the
// library asks where it holds an array of such values or a net resolves one.
auto WholeValueOperations() -> std::vector<support::ValueOperation> {
  using support::EqualityItem;
  return {
      support::DefaultItem::kDeclaredDefault,
      EqualityItem::kEqual,
      EqualityItem::kNotEqual,
      EqualityItem::kCaseEqual,
      support::ChangeTestItem::kBitIdentical,
      support::UnknownItem::kHasUnknown,
      support::PositionsItem::kTakePositions,
      support::PositionsItem::kPositionCount};
}

// What `had` adds to them: a stream's operations (LRM 6.24.3), its positions
// read out and built from (LRM 7.3), what a net writes in (LRM 6.7.1), and a
// stream whose width the running program decides.
void AddOptionalOperations(
    std::vector<support::ValueOperation>& operations,
    const OptionalOperations& had) {
  using support::StreamItem;
  if (had.stream) {
    operations.emplace_back(StreamItem::kBitstreamWidth);
    operations.emplace_back(StreamItem::kCountBits);
    if (had.fixed_stream) {
      operations.emplace_back(StreamItem::kToBitstream);
      operations.emplace_back(StreamItem::kFromBitstream);
    }
  }
  if (had.positions) {
    operations.emplace_back(support::PositionsItem::kToPositions);
    operations.emplace_back(support::PositionsItem::kFromPositions);
  }
  if (had.net_positions) {
    operations.emplace_back(support::PositionsItem::kWritePositions);
  }
  if (had.run_written) {
    operations.emplace_back(StreamItem::kToBitVec);
  }
  if (had.run_read) {
    operations.emplace_back(StreamItem::kFromBitVec);
  }
}

// Each operation the implementation of the library's traits for `type`
// requires, where it has one.
void AddImplItems(
    const CompilationUnit& unit, TypeId type,
    std::vector<support::ValueOperation>& operations) {
  const std::optional<Impl> impl = ImplOf(unit, type);
  if (!impl.has_value()) {
    return;
  }
  operations.insert(operations.end(), impl->items.begin(), impl->items.end());
}

// A type an operation takes or answers, named relative to the type `self` it
// is an operation of, as clang's builtin prototypes name theirs by a code its
// context decodes.
enum class SignatureType : std::uint8_t {
  // The type itself, and an array's element.
  kSelf,
  kElement,
  // The type a family is taken at.
  kAt,
  kPosition,
  // What names an element: a position for an array whose elements a position
  // names, and an associative array's index (LRM 7.8); and where an
  // associative array's index lies, to read it.
  kIndex,
  kIndexPointer,
  kMachine,
  kCount,
  kBool,
  kBit,
  // One bit, holding x where the type does (LRM 11.4.4), and the `integer`
  // type (LRM 6.11).
  kTruth,
  kInteger,
  kNothing,
  kRun,
  // What `==` answers at the type (LRM 11.4.5).
  kEqualityAnswer,
  // The packed vector of the type's fixed stream width (LRM 6.24.3).
  kFixedStream,
  kBitCountControl,
  // The packed vector of every position of the type, and the library's view of
  // positions a net resolved (LRM 6.7.1).
  kPositions,
  kPositionsRead,
  // The value written over, answered by reference.
  kSelfReference,
  // An element reached by a machine index, to read it or to write it.
  kElementReference,
  kElementReferenceMut,
  // Where an element a position names lies, to read it or to write it, and
  // where a write through a position lands.
  kElementPointer,
  kElementPointerMut,
  kFoundElement,
  // A queue of the element declaring no bound (LRM 7.10.1).
  kUnboundedQueue,
  // A dynamic array of the element.
  kDynamicArray,
  kChangedRange,
  kSortOrder,
  // The words a memory task moves (LRM 21.4, 21.5), and a DPI-C open array's
  // image (LRM 35.5.6.1).
  kMemoryWords,
  kDpiOpenArray,
  // The problem of a call of `randomize()` (LRM 18.6), and a variable of this
  // type named by a reference (LRM 13.5.2), through which a part of it is
  // named in turn and later written.
  kConstraintProblem,
  kVariableReferenceToSelf,
};

// Whether `type` is the type a family is taken at.
constexpr auto NamesTheTypeArgument(SignatureType type) -> bool {
  switch (type) {
    case SignatureType::kAt:
      return true;
    case SignatureType::kSelf:
    case SignatureType::kElement:
    case SignatureType::kPosition:
    case SignatureType::kIndex:
    case SignatureType::kIndexPointer:
    case SignatureType::kMachine:
    case SignatureType::kCount:
    case SignatureType::kBool:
    case SignatureType::kBit:
    case SignatureType::kTruth:
    case SignatureType::kInteger:
    case SignatureType::kNothing:
    case SignatureType::kRun:
    case SignatureType::kEqualityAnswer:
    case SignatureType::kFixedStream:
    case SignatureType::kBitCountControl:
    case SignatureType::kPositions:
    case SignatureType::kPositionsRead:
    case SignatureType::kSelfReference:
    case SignatureType::kElementReference:
    case SignatureType::kElementReferenceMut:
    case SignatureType::kElementPointer:
    case SignatureType::kElementPointerMut:
    case SignatureType::kFoundElement:
    case SignatureType::kUnboundedQueue:
    case SignatureType::kDynamicArray:
    case SignatureType::kChangedRange:
    case SignatureType::kSortOrder:
    case SignatureType::kMemoryWords:
    case SignatureType::kDpiOpenArray:
    case SignatureType::kConstraintProblem:
    case SignatureType::kVariableReferenceToSelf:
      return false;
  }
  throw InternalError("mir: unknown signature type");
}

// How an operation takes one parameter: by a reference it only reads, by one
// it writes through, or as the value itself, as a Rust function takes `&T`,
// `&mut T` or `T`.
enum class ParamPassing : std::uint8_t {
  kReadOnlyReference,
  kMutableReference,
  kValue,
};

// One parameter of an operation: its type, and how it is taken.
struct SignatureParam {
  SignatureType type;
  ParamPassing passing;
};

constexpr auto In(SignatureType type) -> SignatureParam {
  return SignatureParam{
      .type = type, .passing = ParamPassing::kReadOnlyReference};
}
constexpr auto InOut(SignatureType type) -> SignatureParam {
  return SignatureParam{
      .type = type, .passing = ParamPassing::kMutableReference};
}
constexpr auto ByValue(SignatureType type) -> SignatureParam {
  return SignatureParam{.type = type, .passing = ParamPassing::kValue};
}

// What an operation takes and answers, the same at every type that has it, as
// one trait method's signature is every implementation's.
struct Signature {
  SignatureType result;
  std::array<SignatureParam, 4> params;
  std::size_t param_count;

  [[nodiscard]] constexpr auto Params() const
      -> std::span<const SignatureParam> {
    return std::span(params).first(param_count);
  }
  // Whether the operation is a family taken at a type argument, as a generic
  // method of rustc's inherent impl is: whether it names the type it is taken
  // at.
  [[nodiscard]] constexpr auto TakenAtAType() const -> bool {
    return NamesTheTypeArgument(result) ||
           std::ranges::any_of(Params(), [](const SignatureParam& param) {
             return NamesTheTypeArgument(param.type);
           });
  }
};

constexpr auto Takes(
    SignatureType result, std::same_as<SignatureParam> auto... params)
    -> Signature {
  static_assert(sizeof...(params) <= 4);
  return Signature{
      .result = result,
      .params = {params...},
      .param_count = sizeof...(params)};
}

// An operation an array is called by -- one of its own methods or an item of a
// language trait it implements -- by its name, and its signature.
struct OperationDeclaration {
  support::BuiltinFn name;
  Signature signature;
};

// The signature of every operation an array is called by, each once whatever
// kinds have it.
constexpr auto kOperationDeclarations = [] {
  using support::BuiltinFn;
  using enum SignatureType;
  const auto operation = [](BuiltinFn name, Signature signature) {
    return OperationDeclaration{.name = name, .signature = signature};
  };
  return std::to_array<OperationDeclaration>({
      // LRM 7.5.1 `new[N]` and `new[N](src)`.
      operation(BuiltinFn::kDynamicArrayNew, Takes(kSelf, In(kMachine))),
      operation(
          BuiltinFn::kDynamicArrayNewCopy,
          Takes(kSelf, In(kMachine), In(kSelf))),
      operation(BuiltinFn::kDelete, Takes(kNothing, InOut(kSelf))),
      operation(BuiltinFn::kReverse, Takes(kNothing, InOut(kSelf))),
      operation(BuiltinFn::kSize, Takes(kCount, In(kSelf))),
      // LRM 7.9.3: 1 where an entry the index names exists, and 0 otherwise.
      operation(BuiltinFn::kExists, Takes(kCount, In(kSelf), In(kIndex))),
      // LRM 7.9.4 -- 7.9.7: where the least, the greatest, the next greater
      // and the next lesser index the array holds lies, or none.
      operation(BuiltinFn::kAssocFirst, Takes(kIndexPointer, In(kSelf))),
      operation(BuiltinFn::kAssocLast, Takes(kIndexPointer, In(kSelf))),
      operation(
          BuiltinFn::kAssocNext, Takes(kIndexPointer, In(kSelf), In(kIndex))),
      operation(
          BuiltinFn::kAssocPrev, Takes(kIndexPointer, In(kSelf), In(kIndex))),
      // LRM 7.9.11: where the value a `default:` clause gave the array lies,
      // none where no clause gave one, and that value set.
      operation(
          BuiltinFn::kAssocStatedDefault, Takes(kElementPointer, In(kSelf))),
      operation(
          BuiltinFn::kAssocSetStatedDefault,
          Takes(kNothing, InOut(kSelf), In(kElement))),
      // LRM 7.6: an array taking the elements of another array.
      operation(BuiltinFn::kDynamicArrayFromArray, Takes(kSelf, In(kAt))),
      operation(BuiltinFn::kQueueFromArray, Takes(kSelf, In(kAt))),
      operation(BuiltinFn::kUnpackedArrayFromArray, Takes(kSelf, In(kAt))),
      // LRM 7.10.2.
      operation(
          BuiltinFn::kDeleteIndex,
          Takes(kNothing, InOut(kSelf), In(kPosition))),
      operation(
          BuiltinFn::kAssocDeleteIndex,
          Takes(kNothing, InOut(kSelf), In(kIndex))),
      operation(
          BuiltinFn::kInsert,
          Takes(kNothing, InOut(kSelf), In(kPosition), In(kElement))),
      operation(
          BuiltinFn::kPushBack, Takes(kNothing, InOut(kSelf), In(kElement))),
      operation(
          BuiltinFn::kPushFront, Takes(kNothing, InOut(kSelf), In(kElement))),
      operation(BuiltinFn::kPopBack, Takes(kElement, InOut(kSelf))),
      operation(BuiltinFn::kPopFront, Takes(kElement, InOut(kSelf))),
      // LRM 7.10.5: the queue held to its bound.
      operation(BuiltinFn::kQueueHoldToBound, Takes(kNothing, InOut(kSelf))),
      // LRM 7.4.5, 7.10.1: where the element a position names lies, to read
      // it or to write it, and where the position names none; and the same of
      // an associative array's index (LRM 7.8.6, 7.8.7).
      operation(
          BuiltinFn::kElement,
          Takes(kElementPointer, In(kSelf), In(kPosition))),
      operation(
          BuiltinFn::kElementMut,
          Takes(kElementPointerMut, InOut(kSelf), In(kPosition))),
      operation(
          BuiltinFn::kAssocElement,
          Takes(kElementPointer, In(kSelf), In(kIndex))),
      operation(
          BuiltinFn::kAssocElementMut,
          Takes(kElementPointerMut, InOut(kSelf), In(kIndex))),
      operation(
          BuiltinFn::kQueueElementMut,
          Takes(kElementPointerMut, InOut(kSelf), In(kPosition))),
      // Storage for one element of a queue, holding no value.
      operation(
          BuiltinFn::kQueueAllocate, Takes(kElementPointerMut, InOut(kSelf))),
      // LRM 7.10.1: a queue's slice is a queue declaring no bound, and its
      // write answers the positions it changed, as an array's run write does.
      operation(
          BuiltinFn::kQueueSlice,
          Takes(kUnboundedQueue, In(kSelf), In(kPosition), In(kPosition))),
      operation(
          BuiltinFn::kAssignQueueSlice,
          Takes(
              kChangedRange, InOut(kSelf), In(kPosition), In(kPosition),
              In(kUnboundedQueue))),
      // LRM 7.4.6: a run of elements read and written as a fixed-size array.
      operation(BuiltinFn::kElementSlice, Takes(kAt, In(kSelf), In(kPosition))),
      operation(
          BuiltinFn::kAssignElementSlice,
          Takes(kChangedRange, InOut(kSelf), In(kPosition), In(kAt))),
      // LRM 11.4.14.4: a window of the elements as a run of bits, and the
      // array with the window's elements replaced.
      operation(
          BuiltinFn::kWindowToBitVec,
          Takes(kRun, In(kSelf), In(kPosition), In(kMachine))),
      operation(
          BuiltinFn::kWithWindow, Takes(
                                      kSelf, In(kSelf), In(kPosition),
                                      In(kMachine), In(kDynamicArray))),
      // LRM 7.12: the elements put in the order a sort answered.
      operation(
          BuiltinFn::kApplyOrder,
          Takes(kNothing, InOut(kSelf), In(kSortOrder))),
      // LRM 7.12.2: the positions of an array's keys in the order a sort
      // puts them, ascending or descending as the flag says, a dynamic array
      // of positions being the type it is a method of.
      operation(BuiltinFn::kSortOrder, Takes(kSelf, In(kAt), ByValue(kBool))),
      // LRM 21.4, 21.5: a memory loaded from the words a task read, and its
      // words handed to a dump.
      operation(
          BuiltinFn::kLoadWords,
          Takes(kNothing, InOut(kSelf), InOut(kMemoryWords))),
      operation(
          BuiltinFn::kStoreWords,
          Takes(kNothing, In(kSelf), InOut(kMemoryWords))),
      // LRM 35.5.6.1: a DPI-C open array's image filled from the array, and
      // the array read back out of it.
      operation(
          BuiltinFn::kToImage,
          Takes(kNothing, In(kSelf), InOut(kDpiOpenArray))),
      operation(
          BuiltinFn::kFromImage,
          Takes(kNothing, InOut(kSelf), In(kDpiOpenArray))),
      // LRM 18.4: the array a reference names, walked for a call of
      // `randomize()` whose problem it is handed.
      operation(
          BuiltinFn::kWalkRandom,
          Takes(
              kNothing, ByValue(kVariableReferenceToSelf),
              ByValue(kConstraintProblem))),
  });
}();

// Whether every one of `operations` has a signature here, which each kind's
// operations are held to at compile time.
consteval auto EveryOperationIsDeclared(
    std::span<const support::BuiltinFn> operations) -> bool {
  for (const support::BuiltinFn operation : operations) {
    bool declared = false;
    for (const OperationDeclaration& declaration : kOperationDeclarations) {
      declared = declared || declaration.name == operation;
    }
    if (!declared) {
      return false;
    }
  }
  return true;
}
static_assert(EveryOperationIsDeclared(kFixedArrayOperations));
static_assert(EveryOperationIsDeclared(kDynamicArrayOperations));
static_assert(EveryOperationIsDeclared(kQueueOperations));
static_assert(EveryOperationIsDeclared(kAssociativeArrayOperations));

// The signature of the array operation named `name`; a name no array has is its
// asker's defect.
auto OperationSignatureOf(support::BuiltinFn name) -> const Signature& {
  for (const OperationDeclaration& declaration : kOperationDeclarations) {
    if (declaration.name == name) {
      return declaration.signature;
    }
  }
  throw InternalError(
      "mir: an operation is asked that no type states -- please report this "
      "as a bug");
}

// An integral operator's, as the language states it (LRM 11.4): each integral
// value it reads by a reference it only reads and each machine value by value,
// and its answer. An operator answering at a type no operand settles -- a
// conversion, a replication, a value made of a machine number -- is an
// operation of the type it makes, as a converting constructor is its class's,
// and an integral operand is then taken at the type it is taken at; any other
// operator is an operation of its first integral operand's type, and another
// integral operand is taken at the type it is taken at. An operator handed
// what a foreign call or a net leaves behind is no operation a type answers,
// and asking for its signature is its asker's defect.
auto IntegralSignatureOf(support::IntegralOp op) -> Signature {
  using enum SignatureType;
  const support::IntegralOperation& operation =
      support::IntegralOperationOf(op);
  Signature signature{.result = kNothing, .params = {}, .param_count = 0};
  bool first_integral = !support::AnswersAtTheTypeOfTheCall(operation.answer);
  for (const support::IntegralOperandKind kind : operation.operands.Kinds()) {
    SignatureParam param = ByValue(kNothing);
    switch (kind) {
      case support::IntegralOperandKind::kBits:
      case support::IntegralOperandKind::kNumber:
        param = In(first_integral ? kSelf : kAt);
        first_integral = false;
        break;
      case support::IntegralOperandKind::kSameType:
        param = In(kSelf);
        break;
      case support::IntegralOperandKind::kMachineInt:
        param = ByValue(kMachine);
        break;
      case support::IntegralOperandKind::kMachineBool:
        param = ByValue(kBool);
        break;
      case support::IntegralOperandKind::kSvLogic:
      case support::IntegralOperandKind::kText:
      case support::IntegralOperandKind::kCanonicalBits:
      case support::IntegralOperandKind::kCanonicalLogic:
      case support::IntegralOperandKind::kPositions:
        throw InternalError(
            std::format(
                "mir: the signature of the integral operation \"{}\", which "
                "no type answers with a body, is asked -- please report this "
                "as a bug",
                operation.name));
    }
    signature.params.at(signature.param_count++) = param;
  }
  switch (operation.answer) {
    case support::IntegralAnswer::kOfFirstOperand:
      signature.result = kSelf;
      break;
    case support::IntegralAnswer::kOneBit:
      signature.result = kTruth;
      break;
    case support::IntegralAnswer::kTwoStateBit:
      signature.result = kBit;
      break;
    case support::IntegralAnswer::kInt:
      signature.result = kCount;
      break;
    case support::IntegralAnswer::kInteger:
      signature.result = kInteger;
      break;
    case support::IntegralAnswer::kPosition:
      signature.result = kPosition;
      break;
    case support::IntegralAnswer::kOfTheCall:
      signature.result = kSelf;
      break;
    case support::IntegralAnswer::kMachineBool:
      signature.result = kBool;
      break;
    case support::IntegralAnswer::kMachineInt:
      signature.result = kMachine;
      break;
  }
  return signature;
}

// What the operation `answers` of the type `a` takes and answers. Each trait's
// items are answered together, so a trait gaining an item stops here until
// its signature is stated.
auto SignatureOf(
    const CompilationUnit& unit, TypeId a, support::ValueOperation answers)
    -> Signature {
  using enum SignatureType;
  return std::visit(
      Overloaded{
          // A step of a value's life is an operation of a run-time-sized
          // container's alone; any other type's is a step the lowering below
          // builds from its parts.
          [&](support::ValueStep step) -> Signature {
            if (!IsLaidOutContainer(unit.types, a)) {
              throw InternalError(
                  "mir: a step of a value's life is asked as an operation of a "
                  "type whose steps follow from its parts -- please report "
                  "this as a bug");
            }
            switch (step) {
              // A copy answers the value it builds, as Rust's `Clone::clone`
              // does.
              case support::ValueStep::kCopy:
                return Takes(kSelf, In(kSelf));
              case support::ValueStep::kRelocate:
                throw InternalError(
                    "mir: a relocation is asked of a type as an operation, "
                    "and no type states one -- please report this as a bug");
              // An assignment answers the value written over, as a class's
              // copy assignment does.
              case support::ValueStep::kAssign:
                return Takes(kSelfReference, InOut(kSelf), In(kSelf));
              case support::ValueStep::kEnd:
                return Takes(kNothing, InOut(kSelf));
            }
            throw InternalError("mir: unknown value step");
          },
          // `==` and `!=` answer at the equality answer type, and `===`,
          // which is never unknown, a bit (LRM 11.4.5).
          [](support::EqualityItem item) -> Signature {
            switch (item) {
              case support::EqualityItem::kEqual:
              case support::EqualityItem::kNotEqual:
                return Takes(kEqualityAnswer, In(kSelf), In(kSelf));
              case support::EqualityItem::kCaseEqual:
                return Takes(kBit, In(kSelf), In(kSelf));
            }
            throw InternalError("mir: unknown equality item");
          },
          [](support::LibraryAnsweredItem trait_item) -> Signature {
            return std::visit(
                Overloaded{
                    [](support::ChangeTestItem item) -> Signature {
                      switch (item) {
                        case support::ChangeTestItem::kBitIdentical:
                          return Takes(kBool, In(kSelf), In(kSelf));
                      }
                      throw InternalError("mir: unknown change-test item");
                    },
                    // LRM 11.4.11: the value an unknown condition answers
                    // combines the two the arms answer.
                    [](support::UnknownItem item) -> Signature {
                      switch (item) {
                        case support::UnknownItem::kHasUnknown:
                          return Takes(kBool, In(kSelf));
                        case support::UnknownItem::kMergeConditional:
                          return Takes(kSelf, In(kSelf), In(kSelf));
                      }
                      throw InternalError("mir: unknown unknown-bit item");
                    },
                    // LRM 6.24.3: a fixed width's stream is a packed vector of
                    // that width, and a width the program decides is a run of
                    // bits.
                    [](support::StreamItem item) -> Signature {
                      switch (item) {
                        case support::StreamItem::kBitstreamWidth:
                          return Takes(kCount, In(kSelf));
                        case support::StreamItem::kCountBits:
                          return Takes(kCount, In(kSelf), In(kBitCountControl));
                        case support::StreamItem::kToBitstream:
                          return Takes(kFixedStream, In(kSelf));
                        case support::StreamItem::kFromBitstream:
                          return Takes(kSelf, In(kFixedStream));
                        case support::StreamItem::kToBitVec:
                          return Takes(kRun, In(kSelf));
                        case support::StreamItem::kFromBitVec:
                          return Takes(kSelf, In(kRun));
                      }
                      throw InternalError("mir: unknown stream item");
                    },
                    // LRM 7.6: a value taking the run of `count` positions
                    // starting at a machine index from another, and how many
                    // positions it numbers, which is the type's. LRM 7.3: its
                    // positions as one integral value, and the value they
                    // hold. LRM 6.7.1: the run of positions a net resolved,
                    // handed as the library's view of them, where in the value
                    // the first stands and how many there are, written in
                    // place, answering whether any changed.
                    [](support::PositionsItem item) -> Signature {
                      switch (item) {
                        case support::PositionsItem::kTakePositions:
                          return Takes(
                              kSelf, In(kSelf), In(kSelf), In(kMachine),
                              In(kMachine));
                        case support::PositionsItem::kPositionCount:
                          return Takes(kMachine);
                        case support::PositionsItem::kToPositions:
                          return Takes(kPositions, In(kSelf));
                        case support::PositionsItem::kFromPositions:
                          return Takes(kSelf, In(kPositions));
                        case support::PositionsItem::kWritePositions:
                          return Takes(
                              kBool, InOut(kSelf), ByValue(kPositionsRead),
                              ByValue(kMachine), ByValue(kMachine));
                      }
                      throw InternalError("mir: unknown positions item");
                    },
                    // LRM 7.4.5, 7.10.1: the element a machine index names, to
                    // read or to write, and where a write through a position
                    // lands.
                    [](support::ElementAccessItem item) -> Signature {
                      switch (item) {
                        case support::ElementAccessItem::kGetUnchecked:
                          return Takes(
                              kElementReference, In(kSelf), In(kMachine));
                        case support::ElementAccessItem::kGetUncheckedMut:
                          return Takes(
                              kElementReferenceMut, InOut(kSelf), In(kMachine));
                        case support::ElementAccessItem::kElementPlace:
                          return Takes(kFoundElement, InOut(kSelf), In(kIndex));
                      }
                      throw InternalError("mir: unknown element-access item");
                    },
                    [](support::ResizeItem item) -> Signature {
                      switch (item) {
                        case support::ResizeItem::kResized:
                          return Takes(kSelf, In(kSelf), In(kMachine));
                      }
                      throw InternalError("mir: unknown resize item");
                    },
                    // A default takes nothing and answers a value of the type.
                    [](support::DefaultItem item) -> Signature {
                      switch (item) {
                        case support::DefaultItem::kDeclaredDefault:
                          return Takes(kSelf);
                      }
                      throw InternalError("mir: unknown default item");
                    }},
                trait_item);
          },
          // A bound-free conversion is no operation of an interface: its body
          // declares itself.
          [](support::BoundFreeItem) -> Signature {
            throw InternalError(
                "mir: a bound-free conversion's declaration is asked of a "
                "type's interface -- please report this as a bug");
          },
          [](support::BuiltinFn operation) -> Signature {
            return OperationSignatureOf(operation);
          },
          [](support::IntegralOp op) -> Signature {
            return IntegralSignatureOf(op);
          }},
      answers);
}

// The type `type` names in a signature of an operation of `self`, taken at
// `at` where it is a family.
auto TypeNamed(
    const CompilationUnit& unit, TypeId self, std::optional<TypeId> at,
    SignatureType type) -> TypeId {
  const TypePool& types = unit.types;
  const auto element = [&] {
    return types.Get(self).ContainerElementType().value();
  };
  const auto reference_to = [&](TypeId referent, Mutability mutability) {
    return types.Intern(
        Type{ReferenceType{.referent = referent, .mutability = mutability}});
  };
  switch (type) {
    case SignatureType::kSelf:
      return self;
    case SignatureType::kElement:
      return element();
    case SignatureType::kAt:
      if (!at.has_value()) {
        throw InternalError(
            "mir: a family's signature is read at no type argument -- please "
            "report this as a bug");
      }
      return *at;
    case SignatureType::kPosition:
      return PositionType(types);
    case SignatureType::kIndex:
      return types.Get(self).IndexType().value_or(PositionType(types));
    case SignatureType::kIndexPointer:
      return BorrowedPointerTo(
          types, types.Get(self).IndexType().value(), Mutability::kReadOnly);
    case SignatureType::kMachine:
      return unit.builtins.machine_int64;
    case SignatureType::kCount:
      return unit.builtins.int_type;
    case SignatureType::kBool:
      return unit.builtins.machine_bool;
    case SignatureType::kBit:
      return unit.builtins.bit1;
    case SignatureType::kTruth:
      return PackedVectorOf(types, 1, types.Get(self).Integral().state_kind);
    case SignatureType::kInteger:
      return unit.builtins.integer;
    case SignatureType::kNothing:
      return unit.builtins.void_type;
    case SignatureType::kRun:
      return unit.builtins.bit_vec;
    case SignatureType::kEqualityAnswer:
      return EqualityAnswerType(unit, self);
    case SignatureType::kFixedStream: {
      const StreamShape shape = FixedStreamShapeOf(unit, self).value();
      return PackedVectorOf(types, shape.width, shape.state_kind);
    }
    case SignatureType::kBitCountControl:
      return BitCountControlType(types);
    case SignatureType::kPositions:
      return PositionsTypeOf(unit, self);
    case SignatureType::kPositionsRead:
      return LibraryTypeOf(types, support::LibraryDecl::kPositionsRead);
    case SignatureType::kSelfReference:
      return reference_to(self, Mutability::kMutable);
    case SignatureType::kElementReference:
      return reference_to(element(), Mutability::kReadOnly);
    case SignatureType::kElementReferenceMut:
      return reference_to(element(), Mutability::kMutable);
    case SignatureType::kElementPointer:
      return BorrowedPointerTo(types, element(), Mutability::kReadOnly);
    case SignatureType::kElementPointerMut:
      return BorrowedPointerTo(types, element(), Mutability::kMutable);
    case SignatureType::kFoundElement:
      return FoundElementType(unit, element());
    case SignatureType::kUnboundedQueue:
      return UnboundedQueueOf(types, element());
    case SignatureType::kDynamicArray:
      return DynamicArrayOf(types, element());
    case SignatureType::kChangedRange:
      return ChangedRangeType(types);
    case SignatureType::kSortOrder:
      return SortOrderType(types);
    case SignatureType::kMemoryWords:
      return LibraryTypeOf(types, support::LibraryDecl::kMemoryWords);
    case SignatureType::kDpiOpenArray:
      return LibraryTypeOf(types, support::LibraryDecl::kDpiOpenArray);
    case SignatureType::kConstraintProblem:
      return unit.builtins.constraint_problem;
    case SignatureType::kVariableReferenceToSelf:
      return types.Intern(
          Type{VariableRefType{
              .pointee = self, .mutability = Mutability::kMutable}});
  }
  throw InternalError("mir: unknown signature type");
}

// The type a parameter of `type` taken as `passing` is.
auto ParamTypeOf(const TypePool& types, TypeId type, ParamPassing passing)
    -> TypeId {
  const auto reference = [&](Mutability mutability) {
    return types.Intern(
        Type{ReferenceType{.referent = type, .mutability = mutability}});
  };
  switch (passing) {
    case ParamPassing::kReadOnlyReference:
      return reference(Mutability::kReadOnly);
    case ParamPassing::kMutableReference:
      return reference(Mutability::kMutable);
    case ParamPassing::kValue:
      return type;
  }
  throw InternalError("mir: unknown parameter passing");
}

// The declaration of the operation `answers` of `a`, taken at `at`: each
// parameter as its signature takes it, the first the receiver where the
// operation is asked of a value, then where it was called where it reports
// there.
auto Declared(
    const CompilationUnit& unit, TypeId a, std::optional<TypeId> at,
    support::ValueOperation answers, const Signature& signature)
    -> CallableCode {
  CallableCode code;
  for (const SignatureParam& param : signature.Params()) {
    code.params.push_back(code.AddLocal(ParamTypeOf(
        unit.types, TypeNamed(unit, a, at, param.type), param.passing)));
  }
  if (support::ReachesAReceiver(answers) && !code.params.empty()) {
    code.receiver = code.params.front();
  }
  code.result_type = TypeNamed(unit, a, at, signature.result);
  if (support::TakesCallerLocation(answers)) {
    code.caller_location = code.AddLocal(unit.builtins.source_location);
    code.params.push_back(*code.caller_location);
  }
  return code;
}

// What `answers` takes and answers over a value of `a`, at `arguments` where
// it is taken at a type, which is where its signature names the type; a source
// naming a type where it is taken at none, or none where it is taken at one,
// is its producer's defect.
auto ValueOperationDeclaration(
    const CompilationUnit& unit, TypeId a, support::ValueOperation answers,
    std::span<const TypeId> arguments) -> CallableCode {
  const Signature signature = SignatureOf(unit, a, answers);
  const bool taken_at_a_type = signature.TakenAtAType();
  if (arguments.size() != (taken_at_a_type ? 1U : 0U)) {
    throw InternalError(
        std::format(
            "mir: the operation \"{}\" is asked at {} type arguments, and its "
            "declaration takes {} -- please report this as a bug",
            support::ValueOperationName(answers), arguments.size(),
            taken_at_a_type ? 1 : 0));
  }
  const std::optional<TypeId> at =
      arguments.empty() ? std::nullopt
                        : std::optional<TypeId>(arguments.front());
  CallableCode declaration = Declared(unit, a, at, answers, signature);
  // Each operation of a run-time-sized container's value's life builds,
  // copies, writes over or ends one over its core with nothing of the design's
  // running, so it cannot unwind; nor can taking storage for an element, or
  // reaching one by a machine index to read it or to write it, which the steps
  // and every body walking the elements ask of the type. The unit's
  // verification holds every body built for them to that. Another operation
  // may build or ask an element with its element type's own operation, which
  // that type says can unwind or not, so it says nothing.
  static constexpr std::array<support::ValueOperation, 3> kReachingOnlyStorage{
      support::BuiltinFn::kQueueAllocate,
      support::ElementAccessItem::kGetUnchecked,
      support::ElementAccessItem::kGetUncheckedMut};
  // An integral operator answers a value of every operand it is handed,
  // whatever bits they hold, so a body answering one cannot unwind.
  if (std::holds_alternative<support::IntegralOp>(answers)) {
    declaration.can_unwind = false;
    return declaration;
  }
  if (!IsLaidOutContainer(unit.types, a)) {
    return declaration;
  }
  if (std::ranges::contains(support::kValueLife.requirements, answers) ||
      std::ranges::contains(kReachingOnlyStorage, answers)) {
    declaration.can_unwind = false;
  }
  return declaration;
}

// The interface of `type`: the items of each library trait it implements, in
// `operations`, and each operation it is `called` by -- its methods and the
// items of the language traits it implements -- among its operations or its
// families as the operation is one function or a family.
auto InterfaceOf(
    TypeId type, std::vector<support::ValueOperation> operations,
    std::span<const support::BuiltinFn> called = {}) -> TypeInterface {
  TypeInterface interface{
      .type = type, .operations = std::move(operations), .families = {}};
  for (const support::BuiltinFn operation : called) {
    (OperationSignatureOf(operation).TakenAtAType() ? interface.families
                                                    : interface.operations)
        .emplace_back(operation);
  }
  return interface;
}

// A fixed-size array's (LRM 7.4): each operation on its whole value its
// elements decide, the two arms of a conditional with an ambiguous condition
// combined element by element (LRM 11.4.11), its own methods and the items of
// its language traits.
auto FixedArrayInterface(const CompilationUnit& unit, TypeId array)
    -> TypeInterface {
  std::vector<support::ValueOperation> operations = WholeValueOperations();
  operations.emplace_back(support::UnknownItem::kMergeConditional);
  AddOptionalOperations(operations, OptionalOperationsOfArray(unit, array));
  return InterfaceOf(array, std::move(operations), kFixedArrayOperations);
}

// The library's traits a fixed-size array may implement: each asked of a
// value the library holds, whatever its type.
constexpr auto kFixedSizeArrayTraits =
    std::to_array<const support::LibraryTrait*>(
        {&support::kValueLife, &support::kChangeTest,
         &support::kWholeValueQuestions, &support::kTakeover,
         &support::kNetPositions, &support::kFixedStream, &support::kStream,
         &support::kStreamRead});

// The implementation of the library's traits for a fixed-size array, which no
// declaration lists. Its steps are its elements', so it states no value's
// life; the library asks each other item of a value apart from the rest, and
// answers one a type lacks with its failing of it, so each operation of the
// array's interface that some trait requires is an item, and the array
// implements each trait all of whose requirements are among them, as a
// generic impl of rustc's exists where its type arguments meet its bounds.
auto FixedArrayImpl(const CompilationUnit& unit, TypeId array) -> Impl {
  const TypeInterface interface = FixedArrayInterface(unit, array);
  const auto required = [](support::ValueOperation operation) {
    return std::ranges::any_of(
        kFixedSizeArrayTraits, [&](const support::LibraryTrait* trait) {
          return std::ranges::contains(trait->requirements, operation);
        });
  };
  Impl impl{.self_type = array, .traits = {}, .items = {}};
  for (const support::ValueOperation operation : interface.operations) {
    if (required(operation)) {
      impl.items.push_back(operation);
    }
  }
  for (const support::LibraryTrait* trait : kFixedSizeArrayTraits) {
    if (std::ranges::all_of(
            trait->requirements, [&](support::ValueOperation requirement) {
              return Declares(interface, requirement);
            })) {
      impl.traits.push_back(trait);
    }
  }
  return impl;
}

// A run-time-sized container's (LRM 7.5, 7.10): each item of the library's
// traits it implements, its default among them, the two arms of a
// conditional with an ambiguous condition combined element by element (LRM
// 11.4.11), its own methods and the items of its language traits.
auto ContainerInterface(
    const CompilationUnit& unit, TypeId container,
    std::span<const support::BuiltinFn> called) -> TypeInterface {
  std::vector<support::ValueOperation> operations;
  AddImplItems(unit, container, operations);
  operations.emplace_back(support::UnknownItem::kMergeConditional);
  return InterfaceOf(container, std::move(operations), called);
}

// A product's -- a structure's (LRM 7.2) or a tuple's -- or a union's (LRM
// 7.3): each operation on its whole value its parts decide.
auto PartsInterface(TypeId type, const OptionalOperations& had)
    -> TypeInterface {
  std::vector<support::ValueOperation> operations = WholeValueOperations();
  AddOptionalOperations(operations, had);
  return InterfaceOf(type, std::move(operations));
}

}  // namespace

auto ImplOf(const CompilationUnit& unit, TypeId type) -> std::optional<Impl> {
  if (unit.types.Get(type).Is<UnpackedArrayType>()) {
    return FixedArrayImpl(unit, type);
  }
  const auto* library = unit.types.Get(type).As<LibraryType>();
  if (library == nullptr) {
    return std::nullopt;
  }
  const std::span<const support::TraitImplementation> implemented =
      support::TraitsOf(library->decl);
  if (implemented.empty()) {
    return std::nullopt;
  }
  Impl impl{.self_type = type, .traits = {}, .items = {}};
  for (const support::TraitImplementation& implementation : implemented) {
    switch (implementation.where) {
      case support::ImplementedWhere::kAlways:
        break;
      case support::ImplementedWhere::kItStreams:
        if (!HasBitStream(unit, type)) {
          continue;
        }
        break;
    }
    impl.traits.push_back(implementation.trait);
    impl.items.insert(
        impl.items.end(), implementation.trait->requirements.begin(),
        implementation.trait->requirements.end());
  }
  return impl;
}

auto ConstructedByTheTypesDefault(
    const TypePool& types, const CallExpr& call, TypeId type)
    -> std::optional<TypeOperationTarget> {
  if (!std::holds_alternative<Construct>(call.callee) ||
      !call.arguments.empty() || !IsLaidOutContainer(types, type)) {
    return std::nullopt;
  }
  return TypeOperationTarget{
      .type = type, .answers = support::DefaultItem::kDeclaredDefault};
}

auto FoundElementType(const CompilationUnit& unit, TypeId element) -> TypeId {
  return unit.types.Intern(
      Type{TupleType{
          .elements = {
              BorrowedPointerTo(unit.types, element, Mutability::kMutable),
              unit.builtins.machine_bool}}});
}

auto ElementAllocationOf(const CompilationUnit& unit, TypeId type)
    -> std::optional<ElementAllocation> {
  const Type& stated = unit.types.Get(type);
  if (stated.IsDeclaredBy(support::LibraryDecl::kDynamicArray)) {
    return ElementAllocation{
        .allocate = support::BuiltinFn::kDynamicArrayPlace, .arguments = 3};
  }
  return std::nullopt;
}

// An integral type's laid out as words: each operator the language defines on
// it that a body built at its width answers, an operator taking or answering
// another integral type a family taken at that type.
auto WordsInterface(TypeId type) -> TypeInterface {
  TypeInterface interface{.type = type, .operations = {}, .families = {}};
  for (const support::IntegralOperation& operation :
       support::IntegralOperations()) {
    if (!support::IsAnsweredAtItsWidth(operation.op)) {
      continue;
    }
    (IntegralSignatureOf(operation.op).TakenAtAType() ? interface.families
                                                      : interface.operations)
        .emplace_back(operation.op);
  }
  return interface;
}

auto TypeInterfaceOf(const CompilationUnit& unit, TypeId type)
    -> std::optional<TypeInterface> {
  const Type& stated = unit.types.Get(type);
  if (IsExpandedIntegral(unit.types, type)) {
    return WordsInterface(type);
  }
  if (stated.IsDeclaredBy(support::LibraryDecl::kDynamicArray)) {
    return ContainerInterface(unit, type, kDynamicArrayOperations);
  }
  if (stated.IsDeclaredBy(support::LibraryDecl::kQueue)) {
    return ContainerInterface(unit, type, kQueueOperations);
  }
  // An associative array's own methods and its language traits' items beside
  // its library traits' items. The two arms of a conditional whose condition
  // is ambiguous are not combined entry by entry: where they differ, the
  // answer is the type's default (LRM 11.4.11).
  if (stated.IsDeclaredBy(support::LibraryDecl::kAssociativeArray)) {
    std::vector<support::ValueOperation> operations;
    AddImplItems(unit, type, operations);
    return InterfaceOf(
        type, std::move(operations), kAssociativeArrayOperations);
  }
  if (stated.Is<UnpackedArrayType>()) {
    return FixedArrayInterface(unit, type);
  }
  if (const auto* tuple = stated.As<TupleType>()) {
    // A tuple of anything but data -- the values a closure keeps -- is no
    // value the language operates on.
    if (!IsDataType(unit.types, type)) {
      return std::nullopt;
    }
    return PartsInterface(
        type, OptionalOperationsOfProduct(unit, tuple->elements));
  }
  if (const std::optional<RecordKind> kind = RecordKindOf(unit, type)) {
    const std::vector<TypeId> members = PartTypes(unit, type);
    switch (*kind) {
      case RecordKind::kStruct:
        return PartsInterface(type, OptionalOperationsOfProduct(unit, members));
      case RecordKind::kUnion:
        return PartsInterface(type, OptionalOperationsOfUnion(unit, members));
      case RecordKind::kTaggedUnion:
        return PartsInterface(
            type, OptionalOperationsOfTaggedUnion(unit, members));
    }
    throw InternalError("mir: unknown record kind");
  }
  return std::nullopt;
}

auto Declares(const TypeInterface& interface, support::ValueOperation answers)
    -> bool {
  return std::ranges::contains(interface.operations, answers) ||
         std::ranges::contains(interface.families, answers);
}

auto OperationDeclarationOf(
    const CompilationUnit& unit, const TypeOperationSource& source)
    -> std::optional<CallableCode> {
  const std::optional<TypeInterface> interface =
      TypeInterfaceOf(unit, source.type);
  if (!interface.has_value() || !Declares(*interface, source.answers)) {
    return std::nullopt;
  }
  return ValueOperationDeclaration(
      unit, source.type, source.answers, source.arguments);
}

auto OperationNamedByEntry(
    const CompilationUnit& unit, TypeId type, support::BuiltinFn entry)
    -> std::optional<support::ValueOperation> {
  const std::optional<TypeInterface> interface = TypeInterfaceOf(unit, type);
  if (!interface.has_value()) {
    return std::nullopt;
  }
  const auto named = [&](const support::ValueOperation& operation) {
    return std::visit(
        Overloaded{
            [&](support::LibraryAnsweredItem item) {
              return support::LibraryEntryOf(item) == entry;
            },
            [&](support::BuiltinFn method) { return method == entry; },
            [](support::EqualityItem) { return false; },
            [](support::BoundFreeItem) { return false; },
            [](support::ValueStep) { return false; },
            [](support::IntegralOp) { return false; }},
        operation);
  };
  for (const std::vector<support::ValueOperation>* declared :
       {&interface->operations, &interface->families}) {
    const auto found = std::ranges::find_if(*declared, named);
    if (found != declared->end()) {
      return *found;
    }
  }
  return std::nullopt;
}

auto ImplementsLanguageTrait(
    const CompilationUnit& unit, TypeId type,
    const support::LanguageTrait& trait) -> bool {
  const std::optional<TypeInterface> interface = TypeInterfaceOf(unit, type);
  return interface.has_value() &&
         std::ranges::all_of(
             trait.requirements, [&](support::BuiltinFn requirement) {
               return Declares(*interface, requirement);
             });
}

auto TypeOperationKindOf(
    const CompilationUnit& unit, TypeId type, support::ValueOperation answers)
    -> std::optional<TypeOperationKind> {
  using Kind = std::optional<TypeOperationKind>;
  // An operation a type has by declaring it: a record's default alone, which
  // its declaration states; what its interface declares; and the default of a
  // data type no interface states, which code holding an array asks by the
  // element type alone.
  const auto declared = [&](bool is_default) -> Kind {
    if (RecordKindOf(unit, type).has_value()) {
      return is_default ? Kind{TypeOperationKind::kDefault} : std::nullopt;
    }
    // An integral type's operators are bodies of its own, and its default is
    // a data type's, which its interface states nothing about.
    if (IsExpandedIntegral(unit.types, type)) {
      if (is_default) {
        return TypeOperationKind::kDefault;
      }
      return Declares(WordsInterface(type), answers)
                 ? Kind{TypeOperationKind::kExpandedIntegral}
                 : std::nullopt;
    }
    if (const std::optional<TypeInterface> interface =
            TypeInterfaceOf(unit, type)) {
      if (!Declares(*interface, answers)) {
        return std::nullopt;
      }
      return IsLaidOutContainer(unit.types, type)
                 ? Kind{TypeOperationKind::kContainer}
                 : Kind{TypeOperationKind::kStructural};
    }
    if (is_default && IsDataType(unit.types, type)) {
      return TypeOperationKind::kDefault;
    }
    return std::nullopt;
  };
  return std::visit(
      Overloaded{
          // Every type holding elements has both bound-free conversions.
          [&](support::BoundFreeItem item) -> Kind {
            if (!unit.types.Get(type).ContainerElementType().has_value()) {
              return std::nullopt;
            }
            switch (item) {
              case support::BoundFreeItem::kToBoundFree:
                return TypeOperationKind::kToBoundFree;
              case support::BoundFreeItem::kFromBoundFree:
                return TypeOperationKind::kFromBoundFree;
            }
            throw InternalError("mir: unknown bound-free conversion");
          },
          [&](support::LibraryAnsweredItem item) -> Kind {
            return declared(std::holds_alternative<support::DefaultItem>(item));
          },
          [&](support::EqualityItem) -> Kind { return declared(false); },
          [&](support::BuiltinFn) -> Kind { return declared(false); },
          [&](support::ValueStep) -> Kind { return declared(false); },
          [&](support::IntegralOp) -> Kind { return declared(false); }},
      answers);
}

auto SortOrderType(const TypePool& types) -> TypeId {
  return DynamicArrayOf(
      types, types.Intern(
                 Type{IntegralType{
                     .bit_width = 64,
                     .signedness = Signedness::kSigned,
                     .state_kind = IntegralStateKind::kTwoState}}));
}

auto UnboundedQueueOf(const TypePool& types, TypeId element) -> TypeId {
  return QueueOf(types, element, support::UnboundedQueue{});
}

auto ChangedRangeType(const TypePool& types) -> TypeId {
  const TypeId position = PositionType(types);
  return types.Intern(Type{TupleType{.elements = {position, position}}});
}

auto BitCountControlType(const TypePool& types) -> TypeId {
  return PackedVectorOf(types, kBitValueCount, IntegralStateKind::kFourState);
}

}  // namespace lyra::mir
