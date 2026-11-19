#include "lyra/mir/value_parts.hpp"

#include <algorithm>
#include <bit>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <unordered_map>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/float_width.hpp"

namespace lyra::mir {

auto PartTypes(const CompilationUnit& unit, TypeId type)
    -> std::vector<TypeId> {
  if (const std::optional<std::span<const TypeId>> parts =
          ProductElements(unit, type)) {
    return {parts->begin(), parts->end()};
  }
  if (const std::optional<std::span<const TypeId>> members =
          OverlaidMembers(unit, type)) {
    return {members->begin(), members->end()};
  }
  if (const std::optional<TypeId> element =
          unit.types.Get(type).ContainerElementType()) {
    return {*element};
  }
  return {};
}

auto CarriesUnknowns(const CompilationUnit& unit, TypeId type) -> bool {
  const Type& t = unit.types.Get(type);
  if (t.IsIntegral()) {
    return IsFourState(t.Integral().state_kind);
  }
  return std::ranges::any_of(PartTypes(unit, type), [&](TypeId part) {
    return CarriesUnknowns(unit, part);
  });
}

auto OneBitAnswerType(
    const CompilationUnit& unit, std::span<const TypeId> operands) -> TypeId {
  const bool carries_unknowns = std::ranges::any_of(
      operands, [&](TypeId operand) { return CarriesUnknowns(unit, operand); });
  return carries_unknowns
             ? PackedVectorOf(unit.types, 1, IntegralStateKind::kFourState)
             : unit.builtins.bit1;
}

namespace {

auto WidenedWith(StreamShape shape, StreamShape part) -> StreamShape {
  return StreamShape{
      .width = shape.width + part.width,
      .state_kind = part.state_kind == IntegralStateKind::kFourState
                        ? IntegralStateKind::kFourState
                        : shape.state_kind};
}

// Whether `type`, or any type a value of it holds -- a member, an element --
// is one `holds` answers for.
template <typename Holds>
auto HoldsAnywhere(const CompilationUnit& unit, TypeId type, const Holds& holds)
    -> bool {
  const Type& resolved = unit.types.Get(type);
  if (holds(resolved)) {
    return true;
  }
  if (const RecordId* record = RecordOf(resolved)) {
    return std::ranges::any_of(
        MembersOf(unit.GetRecord(*record)),
        [&](TypeId element) { return HoldsAnywhere(unit, element, holds); });
  }
  if (const std::optional<TypeId> element = resolved.ContainerElementType()) {
    return HoldsAnywhere(unit, *element, holds);
  }
  return false;
}

template <typename Predicate>
auto EveryPart(
    const CompilationUnit& unit, std::span<const TypeId> parts, Predicate p)
    -> bool {
  return std::ranges::all_of(parts, [&](TypeId part) { return p(unit, part); });
}

}  // namespace

auto FixedStreamShapeOfParts(
    const CompilationUnit& unit, std::span<const TypeId> parts)
    -> std::optional<StreamShape> {
  StreamShape shape{.width = 0, .state_kind = IntegralStateKind::kTwoState};
  for (const TypeId component : parts) {
    const std::optional<StreamShape> part = FixedStreamShapeOf(unit, component);
    if (!part.has_value()) {
      return std::nullopt;
    }
    shape = WidenedWith(shape, *part);
  }
  return shape;
}

auto FixedStreamShapeOf(const CompilationUnit& unit, TypeId type)
    -> std::optional<StreamShape> {
  const Type& resolved = unit.types.Get(type);
  if (resolved.IsIntegral()) {
    const IntegralType& integral = resolved.Integral();
    return StreamShape{
        .width = integral.bit_width, .state_kind = integral.state_kind};
  }
  if (const auto* tuple = resolved.As<TupleType>()) {
    return FixedStreamShapeOfParts(unit, tuple->elements);
  }
  // A structure streams its members end to end, an untagged union as its first
  // member, and a tagged union not at all (LRM 11.4.14.1).
  if (const RecordId* record = RecordOf(resolved)) {
    const std::span<const TypeId> members = MembersOf(unit.GetRecord(*record));
    switch (RecordKindOf(unit, *record)) {
      case RecordKind::kStruct:
        return FixedStreamShapeOfParts(unit, members);
      case RecordKind::kUnion:
        return FixedStreamShapeOf(unit, members.front());
      case RecordKind::kTaggedUnion:
        return std::nullopt;
    }
    throw InternalError("FixedStreamShapeOf: unknown record kind");
  }
  if (const auto* array = resolved.As<UnpackedArrayType>()) {
    const std::optional<StreamShape> element =
        FixedStreamShapeOf(unit, array->element_type);
    if (!element.has_value()) {
      return std::nullopt;
    }
    return StreamShape{
        .width = array->Size() * element->width,
        .state_kind = element->state_kind};
  }
  return std::nullopt;
}

auto HasBitStream(const CompilationUnit& unit, TypeId type) -> bool {
  if (const std::optional<RecordKind> kind = RecordKindOf(unit, type)) {
    const std::vector<TypeId> members = PartTypes(unit, type);
    switch (*kind) {
      case RecordKind::kStruct:
        return EveryPart(unit, members, HasBitStream);
      case RecordKind::kUnion:
        return HasBitStream(unit, members.front());
      case RecordKind::kTaggedUnion:
        return false;
    }
    throw InternalError("HasBitStream: unknown record kind");
  }
  const Type& t = unit.types.Get(type);
  return !t.Is<RealType>() && !t.Is<UnmanagedRefType>() &&
         EveryPart(unit, PartTypes(unit, type), HasBitStream);
}

auto HoldsClassHandle(const CompilationUnit& unit, TypeId type) -> bool {
  return HoldsAnywhere(
      unit, type, [](const Type& part) { return part.Is<ManagedRefType>(); });
}

auto IsStreamDestination(const CompilationUnit& unit, TypeId type) -> bool {
  return !HoldsAnywhere(unit, type, [](const Type& part) {
    return part.Is<ManagedRefType>() || part.IndexType().has_value();
  });
}

// A tagged union is resolved by no net type's tables, which read no tag.
auto NetResolves(const CompilationUnit& unit, TypeId type) -> bool {
  const Type& t = unit.types.Get(type);
  if (t.IsIntegral()) {
    return true;
  }
  if (const std::optional<RecordKind> kind = RecordKindOf(unit, type)) {
    switch (*kind) {
      case RecordKind::kStruct:
      case RecordKind::kUnion:
        return EveryPart(unit, PartTypes(unit, type), NetResolves);
      case RecordKind::kTaggedUnion:
        return false;
    }
    throw InternalError("NetResolves: unknown record kind");
  }
  if (t.Is<UnpackedArrayType>()) {
    return EveryPart(unit, PartTypes(unit, type), NetResolves);
  }
  return false;
}

auto HasPositions(const CompilationUnit& unit, TypeId type) -> bool {
  const Type& t = unit.types.Get(type);
  if (t.IsIntegral() || t.Is<RealType>()) {
    return true;
  }
  if (RecordKindOf(unit, type).has_value() || t.Is<UnpackedArrayType>() ||
      t.Is<TupleType>()) {
    return EveryPart(unit, PartTypes(unit, type), HasPositions);
  }
  return false;
}

auto TakenWhole(const CompilationUnit& unit, TypeId type) -> bool {
  const Type& t = unit.types.Get(type);
  if (t.IsIntegral() || t.Is<UnpackedArrayType>() || t.Is<TupleType>()) {
    return false;
  }
  if (const std::optional<RecordKind> kind = RecordKindOf(unit, type)) {
    switch (*kind) {
      case RecordKind::kStruct:
      case RecordKind::kUnion:
        return false;
      case RecordKind::kTaggedUnion:
        return true;
    }
    throw InternalError("TakenWhole: unknown record kind");
  }
  return true;
}

auto TagPositionCountOf(std::size_t members) -> std::uint64_t {
  return members <= 1 ? 0 : std::bit_width(members - 1);
}

namespace {

// A type reached again -- components of one type -- counts what `counted`
// holds it counted the first time, so the count costs the type's edges, never
// its positions' number.
auto PositionCountOf(
    const CompilationUnit& unit, TypeId type,
    std::unordered_map<TypeId, std::uint64_t>& counted) -> std::uint64_t {
  if (const auto found = counted.find(type); found != counted.end()) {
    return found->second;
  }
  const Type& t = unit.types.Get(type);
  std::uint64_t count = 1;
  if (t.IsIntegral()) {
    count = t.Integral().bit_width;
  } else if (const auto* real = t.As<RealType>()) {
    count = support::BitsOf(real->width);
  } else if (const auto* array = t.As<UnpackedArrayType>()) {
    count = array->count * PositionCountOf(unit, array->element_type, counted);
  } else if (
      const std::optional<std::span<const TypeId>> components =
          ProductElements(unit, type)) {
    count = 0;
    for (const TypeId component : *components) {
      count += PositionCountOf(unit, component, counted);
    }
  } else if (
      // An untagged union's are its storage's, which every member is a view
      // of from its first position (LRM 7.3).
      const std::optional<TypeId> storage = UnionStorageOf(unit, type)) {
    count = PositionCountOf(unit, *storage, counted);
  } else if (IsTaggedUnion(unit, type)) {
    // A tagged union's are its widest member's, every member's from the
    // first, and then its tag's, as a packed tagged union lays them out (LRM
    // 7.3.2).
    const std::span<const TypeId> members = *OverlaidMembers(unit, type);
    std::uint64_t widest = 0;
    for (const TypeId member : members) {
      widest = std::max(widest, PositionCountOf(unit, member, counted));
    }
    count = widest + TagPositionCountOf(members.size());
  }
  counted.emplace(type, count);
  return count;
}

}  // namespace

auto PositionCountOf(const CompilationUnit& unit, TypeId type)
    -> std::uint64_t {
  std::unordered_map<TypeId, std::uint64_t> counted;
  return PositionCountOf(unit, type, counted);
}

auto WidestPositionCountOf(
    const CompilationUnit& unit, std::span<const TypeId> members)
    -> std::uint64_t {
  std::uint64_t widest = 0;
  for (const TypeId member : members) {
    widest = std::max(widest, PositionCountOf(unit, member));
  }
  return widest;
}

auto PositionsTypeOf(const CompilationUnit& unit, TypeId type) -> TypeId {
  return PackedVectorOf(
      unit.types, PositionCountOf(unit, type), IntegralStateKind::kFourState);
}

auto EqualityAnswerType(const CompilationUnit& unit, TypeId type) -> TypeId {
  const std::vector<TypeId> parts = PartTypes(unit, type);
  if (RecordKindOf(unit, type) == RecordKind::kUnion) {
    return OneBitAnswerType(unit, std::span{parts}.first(1));
  }
  return OneBitAnswerType(unit, parts);
}

}  // namespace lyra::mir
