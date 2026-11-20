#include "lyra/support/value_operation.hpp"

#include <optional>
#include <string_view>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/value_step.hpp"

namespace lyra::support {

namespace {

auto EqualityItemName(EqualityItem item) -> std::string_view {
  switch (item) {
    case EqualityItem::kEqual:
      return "==";
    case EqualityItem::kNotEqual:
      return "!=";
    case EqualityItem::kCaseEqual:
      return "case_equal";
  }
  throw InternalError("value operation: unknown equality item");
}

}  // namespace

auto LibraryEntryOf(LibraryAnsweredItem item) -> BuiltinFn {
  return std::visit(
      Overloaded{
          [](ChangeTestItem asked) { return LibraryEntryOf(asked); },
          [](UnknownItem asked) { return LibraryEntryOf(asked); },
          [](StreamItem asked) { return LibraryEntryOf(asked); },
          [](PositionsItem asked) { return LibraryEntryOf(asked); },
          [](DefaultItem asked) { return LibraryEntryOf(asked); },
          [](ElementAccessItem asked) { return LibraryEntryOf(asked); },
          [](ResizeItem asked) { return LibraryEntryOf(asked); }},
      item);
}

auto LibraryEntryOf(BoundFreeItem item) -> BuiltinFn {
  switch (item) {
    case BoundFreeItem::kToBoundFree:
      return BuiltinFn::kToBoundFree;
    case BoundFreeItem::kFromBoundFree:
      return BuiltinFn::kFromBoundFree;
  }
  throw InternalError("value operation: unknown bound-free conversion");
}

auto LibraryEntryOf(ElementAccessItem item) -> BuiltinFn {
  switch (item) {
    case ElementAccessItem::kGetUnchecked:
      return BuiltinFn::kGetUnchecked;
    case ElementAccessItem::kGetUncheckedMut:
      return BuiltinFn::kGetUncheckedMut;
    case ElementAccessItem::kElementPlace:
      return BuiltinFn::kElementPlace;
  }
  throw InternalError("value operation: unknown element-access item");
}

auto LibraryEntryOf(ResizeItem item) -> BuiltinFn {
  switch (item) {
    case ResizeItem::kResized:
      return BuiltinFn::kResized;
  }
  throw InternalError("value operation: unknown resize item");
}

auto LibraryEntryOf(DefaultItem item) -> BuiltinFn {
  switch (item) {
    case DefaultItem::kDeclaredDefault:
      return BuiltinFn::kDeclaredDefault;
  }
  throw InternalError("value operation: unknown default item");
}

auto LibraryEntryOf(PositionsItem item) -> BuiltinFn {
  switch (item) {
    case PositionsItem::kTakePositions:
      return BuiltinFn::kTakePositions;
    case PositionsItem::kPositionCount:
      return BuiltinFn::kPositionCount;
    case PositionsItem::kToPositions:
      return BuiltinFn::kToPositions;
    case PositionsItem::kWritePositions:
      return BuiltinFn::kWritePositions;
    case PositionsItem::kFromPositions:
      return BuiltinFn::kFromPositions;
  }
  throw InternalError("value operation: unknown positions item");
}

auto LibraryEntryOf(ChangeTestItem item) -> BuiltinFn {
  switch (item) {
    case ChangeTestItem::kBitIdentical:
      return BuiltinFn::kBitIdentical;
  }
  throw InternalError("value operation: unknown change-test item");
}

auto LibraryEntryOf(UnknownItem item) -> BuiltinFn {
  switch (item) {
    case UnknownItem::kHasUnknown:
      return BuiltinFn::kHasUnknown;
    case UnknownItem::kMergeConditional:
      return BuiltinFn::kMergeConditional;
  }
  throw InternalError("value operation: unknown unknown-bit item");
}

auto LibraryEntryOf(StreamItem item) -> BuiltinFn {
  switch (item) {
    case StreamItem::kBitstreamWidth:
      return BuiltinFn::kBitstreamWidth;
    case StreamItem::kCountBits:
      return BuiltinFn::kCountBits;
    case StreamItem::kToBitstream:
      return BuiltinFn::kToBitstream;
    case StreamItem::kFromBitstream:
      return BuiltinFn::kFromBitstream;
    case StreamItem::kToBitVec:
      return BuiltinFn::kToBitVec;
    case StreamItem::kFromBitVec:
      return BuiltinFn::kFromBitVec;
  }
  throw InternalError("value operation: unknown stream item");
}

namespace {

// The library entry naming `operation` and stating how it is called, where an
// entry does: every operation but an equality's and a step's.
auto EntryNaming(ValueOperation operation) -> std::optional<BuiltinFn> {
  return std::visit(
      Overloaded{
          [](EqualityItem) -> std::optional<BuiltinFn> { return std::nullopt; },
          [](LibraryAnsweredItem item) -> std::optional<BuiltinFn> {
            return LibraryEntryOf(item);
          },
          [](BoundFreeItem item) -> std::optional<BuiltinFn> {
            return LibraryEntryOf(item);
          },
          [](BuiltinFn fn) -> std::optional<BuiltinFn> { return fn; },
          [](ValueStep) -> std::optional<BuiltinFn> { return std::nullopt; }},
      operation);
}

}  // namespace

// An operation a library entry names is asked and named as that entry is.
auto ReachesAReceiver(ValueOperation operation) -> bool {
  return EntryNaming(operation)
      .transform([](BuiltinFn fn) {
        return ActsOnAnObject(RuntimeEntryOf(fn).declaration);
      })
      .value_or(true);
}

// An equality is answered where it is asked, and a step runs no code of the
// design's, and storage that cannot be had ends the run, so nothing in one is
// reported where it was asked for.
auto TakesCallerLocation(ValueOperation operation) -> bool {
  return EntryNaming(operation)
      .transform(
          [](BuiltinFn fn) { return RuntimeEntryOf(fn).takes_the_location; })
      .value_or(false);
}

auto ValueOperationName(ValueOperation operation) -> std::string_view {
  return std::visit(
      Overloaded{
          [](EqualityItem item) { return EqualityItemName(item); },
          [](LibraryAnsweredItem item) {
            return RuntimeEntryOf(LibraryEntryOf(item)).name;
          },
          [](BoundFreeItem item) {
            return RuntimeEntryOf(LibraryEntryOf(item)).name;
          },
          [](BuiltinFn fn) { return RuntimeEntryOf(fn).name; },
          [](ValueStep step) { return ValueStepName(step); }},
      operation);
}

}  // namespace lyra::support
