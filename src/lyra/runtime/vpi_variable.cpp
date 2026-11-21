#include "lyra/runtime/vpi_variable.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <optional>
#include <ranges>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/abi/records.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/base/source_location.hpp"
#include "lyra/runtime/ambient_run_context.hpp"
#include "lyra/runtime/application_routine_calls.hpp"
#include "lyra/runtime/cancellation.hpp"
#include "lyra/runtime/hierarchical_name.hpp"
#include "lyra/runtime/reflection.hpp"
#include "lyra/runtime/running_state.hpp"
#include "lyra/runtime/runtime.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/scope.hpp"
#include "lyra/runtime/value_dump.hpp"
#include "lyra/runtime/vpi_object.hpp"
#include "lyra/support/data_object.hpp"
#include "lyra/support/force_targets.hpp"
#include "lyra/support/random_qualifier.hpp"
#include "lyra/value/any_value.hpp"
#include "lyra/value/bit_vec.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/integral_format.hpp"
#include "lyra/value/integral_value_type.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/library_value_types.hpp"
#include "lyra/value/real.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/value_type.hpp"
#include "sv_vpi_user.h"
#include "vpi_user.h"

namespace lyra::runtime {

namespace {

using support::TypespecKind;

// What a type is built as, which decides how a part of it is reached and what
// it answers.
enum class Shape : std::uint8_t {
  // A packed value: an integer type, a vector, a packed structure, union or
  // array, an enumeration.
  kIntegral,
  kReal,
  kText,
  // A chandle, a class handle, a virtual interface: none has a value a format
  // carries (LRM 37.17 detail 11).
  kHandle,
  kEvent,
  kUnpackedAggregate,
  kFixedArray,
  // A dynamic array or a queue, numbered from its front.
  kSizedArray,
  kAssociative,
};

// How a structure's or a union's members lie: none for any other type.
enum class MemberLayout : std::uint8_t { kNone, kStructure, kUnion };

// What a typespec of one kind is, for every question asked of it here.
struct KindFacts {
  Shape shape;
  MemberLayout members;
  // The type of a variable of the kind, and of a typespec of it (LRM 37.17,
  // 37.25).
  PLI_INT32 variable_type;
  PLI_INT32 typespec_type;
  // An array's kind, 0 for any other (LRM 37.17 detail 21).
  PLI_INT32 array_type;
  // Whether a value of it is a single bit where it states no range: a bit or
  // a logic, whose positions only a range numbers.
  bool single_bit;
};

auto FactsOf(TypespecKind kind) -> KindFacts {
  const auto integral = [](PLI_INT32 variable, PLI_INT32 typespec,
                           MemberLayout members = MemberLayout::kNone) {
    return KindFacts{
        .shape = Shape::kIntegral,
        .members = members,
        .variable_type = variable,
        .typespec_type = typespec,
        .array_type = 0,
        .single_bit = false};
  };
  const auto other = [](Shape shape, PLI_INT32 variable, PLI_INT32 typespec,
                        PLI_INT32 array_type = 0,
                        MemberLayout members = MemberLayout::kNone) {
    return KindFacts{
        .shape = shape,
        .members = members,
        .variable_type = variable,
        .typespec_type = typespec,
        .array_type = array_type,
        .single_bit = false};
  };
  switch (kind) {
    case TypespecKind::kBit: {
      KindFacts facts = integral(vpiBitVar, vpiBitTypespec);
      facts.single_bit = true;
      return facts;
    }
    case TypespecKind::kLogic: {
      KindFacts facts = integral(vpiLogicVar, vpiLogicTypespec);
      facts.single_bit = true;
      return facts;
    }
    case TypespecKind::kInt:
      return integral(vpiIntVar, vpiIntTypespec);
    case TypespecKind::kShortInt:
      return integral(vpiShortIntVar, vpiShortIntTypespec);
    case TypespecKind::kLongInt:
      return integral(vpiLongIntVar, vpiLongIntTypespec);
    case TypespecKind::kByte:
      return integral(vpiByteVar, vpiByteTypespec);
    case TypespecKind::kInteger:
      return integral(vpiIntegerVar, vpiIntegerTypespec);
    case TypespecKind::kTime:
      return integral(vpiTimeVar, vpiTimeTypespec);
    case TypespecKind::kEnum:
      return integral(vpiEnumVar, vpiEnumTypespec);
    case TypespecKind::kPackedStruct:
      return integral(
          vpiStructVar, vpiStructTypespec, MemberLayout::kStructure);
    case TypespecKind::kPackedUnion:
      return integral(vpiUnionVar, vpiUnionTypespec, MemberLayout::kUnion);
    case TypespecKind::kPackedArray:
      return integral(vpiPackedArrayVar, vpiPackedArrayTypespec);
    case TypespecKind::kReal:
    case TypespecKind::kRealtime:
      return other(Shape::kReal, vpiRealVar, vpiRealTypespec);
    case TypespecKind::kShortReal:
      return other(Shape::kReal, vpiShortRealVar, vpiShortRealTypespec);
    case TypespecKind::kEvent:
      return other(Shape::kEvent, vpiNamedEvent, vpiEventTypespec);
    case TypespecKind::kString:
      return other(Shape::kText, vpiStringVar, vpiStringTypespec);
    case TypespecKind::kChandle:
      return other(Shape::kHandle, vpiChandleVar, vpiChandleTypespec);
    case TypespecKind::kClass:
      return other(Shape::kHandle, vpiClassVar, vpiClassTypespec);
    case TypespecKind::kVirtualInterface:
      return other(
          Shape::kHandle, vpiVirtualInterfaceVar, vpiInterfaceTypespec);
    case TypespecKind::kUnpackedStruct:
      return other(
          Shape::kUnpackedAggregate, vpiStructVar, vpiStructTypespec, 0,
          MemberLayout::kStructure);
    case TypespecKind::kUnpackedUnion:
      return other(
          Shape::kUnpackedAggregate, vpiUnionVar, vpiUnionTypespec, 0,
          MemberLayout::kUnion);
    case TypespecKind::kFixedArray:
      return other(
          Shape::kFixedArray, vpiArrayVar, vpiArrayTypespec, vpiStaticArray);
    case TypespecKind::kDynamicArray:
      return other(
          Shape::kSizedArray, vpiArrayVar, vpiArrayTypespec, vpiDynamicArray);
    case TypespecKind::kQueue:
      return other(
          Shape::kSizedArray, vpiArrayVar, vpiArrayTypespec, vpiQueueArray);
    case TypespecKind::kAssociativeArray:
      return other(
          Shape::kAssociative, vpiArrayVar, vpiArrayTypespec, vpiAssocArray);
  }
  throw InternalError("FactsOf: unknown typespec kind");
}

auto FactsOf(const abi::Typespec& typespec) -> KindFacts {
  return FactsOf(typespec.Kind());
}

auto IsUnpackedArray(Shape shape) -> bool {
  switch (shape) {
    case Shape::kFixedArray:
    case Shape::kSizedArray:
    case Shape::kAssociative:
      return true;
    case Shape::kIntegral:
    case Shape::kReal:
    case Shape::kText:
    case Shape::kHandle:
    case Shape::kEvent:
    case Shape::kUnpackedAggregate:
      return false;
  }
  throw InternalError("IsUnpackedArray: unknown shape");
}

// What each version of the standard says of the objects of Table 36-10's
// first seven entries. Under 1364-2001, which allowed either shape for an
// array of `reg`, the later one is taken.
struct ModeFacts {
  // A one-dimensional array of `reg` is a memory and its elements memory
  // words (entries 1, 2).
  bool memories;
  // An array of `integer` or `time` is a variable of that type (entry 3), and
  // so is an array of `real` (entry 4).
  bool integer_arrays;
  bool real_arrays;
  // A scope's variables include its `reg`s and arrays of them (entry 5), an
  // array's `reg` iteration holds elements of any type (entry 6), and a
  // scope's `reg` array iteration arrays of any type (entry 7).
  bool regs_are_variables;
  bool regs_of_any_type;
  bool reg_arrays_of_any_type;
};

auto FactsOf(VpiMode mode) -> ModeFacts {
  const ModeFacts verilog{
      .memories = false,
      .integer_arrays = true,
      .real_arrays = true,
      .regs_are_variables = false,
      .regs_of_any_type = false,
      .reg_arrays_of_any_type = false};
  switch (mode) {
    case VpiMode::k1364v1995: {
      ModeFacts facts = verilog;
      facts.memories = true;
      facts.real_arrays = false;
      return facts;
    }
    case VpiMode::k1364v2001:
    case VpiMode::k1364v2005:
      return verilog;
    case VpiMode::k1800v2005:
    case VpiMode::k1800v2009:
    case VpiMode::k1800v2012:
      return ModeFacts{
          .memories = false,
          .integer_arrays = false,
          .real_arrays = false,
          .regs_are_variables = true,
          .regs_of_any_type = true,
          .reg_arrays_of_any_type = true};
  }
  throw InternalError("FactsOf: unknown VPI mode");
}

auto RecordOf(const VariableObject& object) -> const abi::DataObjectRecord& {
  return *ScopeDeclarationsOf(object.holder).data_objects.Entries()[object.row];
}

// The key foreign code names an entry by before the code settling the
// selection converts it into the key the array keeps it by: the number it
// wrote, as a run of bits read signed, or the text it wrote.
auto NamedKey(value::BitVec number) -> SelectedKey {
  return SelectedKey{
      .key = value::AnyValue::CopyOf(
          value::LibraryTypeOf<value::BitVec>(), &number)};
}
auto NamedKey(const value::String& text) -> SelectedKey {
  return SelectedKey{
      .key = value::AnyValue::CopyOf(
          value::LibraryTypeOf<value::String>(), &text)};
}

auto WithStep(const VariableObject& object, Selection::Step step)
    -> VariableObject {
  std::vector<Selection::Step> steps = object.selection.Steps();
  steps.push_back(std::move(step));
  return VariableObject{
      .holder = object.holder,
      .row = object.row,
      .selection = Selection(std::move(steps)),
      .packed = {}};
}

auto WithPacked(const VariableObject& object, PackedStep step)
    -> VariableObject {
  VariableObject stepped = object;
  stepped.packed.push_back(step);
  return stepped;
}

// The typespec one selection step reaches from `at`.
auto SelectedFrom(const abi::Typespec& at, const Selection::Step& step)
    -> const abi::Typespec& {
  switch (FactsOf(at).shape) {
    case Shape::kUnpackedAggregate:
      return *at.members
                  .Entries()[static_cast<std::size_t>(
                      std::get<std::int64_t>(step))]
                  ->typespec;
    case Shape::kFixedArray:
    case Shape::kSizedArray:
    case Shape::kAssociative:
      return *at.element;
    case Shape::kIntegral:
    case Shape::kReal:
    case Shape::kText:
    case Shape::kHandle:
    case Shape::kEvent:
      break;
  }
  throw InternalError(
      "vpi: a selection steps into a value of no aggregate -- please report "
      "this as a bug");
}

// The typespec of what `object`'s selection reaches, which what its bodies
// read and write is a value of.
auto ReachedTypespecOf(const VariableObject& object) -> const abi::Typespec& {
  const abi::Typespec* at = RecordOf(object).typespec;
  for (const Selection::Step& step : object.selection.Steps()) {
    at = &SelectedFrom(*at, step);
  }
  return *at;
}

// The bits of the packed value `object`'s selection reaches that it is --
// from the value's least significant, as many as it is wide -- and its own
// typespec, none for a bit of a value whose positions have none.
struct PackedPart {
  std::uint64_t lsb = 0;
  std::uint64_t width = 0;
  const abi::Typespec* typespec = nullptr;
};

// One dimension of a packed value: its range and its element's typespec,
// none where each position is a bit with no typespec of its own; a value of
// an integer type, an enumeration or a packed structure or union is a vector
// of its width counted from 0 at its least significant bit (LRM 7.2.1, 11.5.1).
struct PackedDimension {
  support::Range range;
  const abi::Typespec* element = nullptr;
  std::uint64_t element_width = 1;
};

auto DimensionOf(const abi::Typespec& typespec)
    -> std::optional<PackedDimension> {
  const KindFacts facts = FactsOf(typespec);
  if (facts.shape != Shape::kIntegral) return std::nullopt;
  if (const std::optional<support::Range> range = typespec.Range()) {
    return PackedDimension{
        .range = *range,
        .element = typespec.element,
        .element_width =
            typespec.element != nullptr ? typespec.element->width : 1U};
  }
  if (facts.single_bit) return std::nullopt;
  return PackedDimension{
      .range =
          support::Range{
              .left = static_cast<std::int64_t>(typespec.width) - 1,
              .right = 0},
      .element = nullptr,
      .element_width = 1};
}

// Where `index` lies in `range` counted from its right bound, which is the
// least significant position; none where it lies outside.
auto OffsetIn(support::Range range, std::int64_t index)
    -> std::optional<std::uint64_t> {
  if (index < std::min(range.left, range.right) ||
      index > std::max(range.left, range.right)) {
    return std::nullopt;
  }
  return static_cast<std::uint64_t>(
      range.left >= range.right ? index - range.right : range.right - index);
}

// The indices of `range` in the order the source writes them, left to right.
auto IndicesOf(support::Range range) -> std::vector<std::int64_t> {
  std::vector<std::int64_t> indices;
  const std::int64_t step = range.left <= range.right ? 1 : -1;
  for (std::int64_t at = range.left;; at += step) {
    indices.push_back(at);
    if (at == range.right) break;
  }
  return indices;
}

auto CountOf(support::Range range) -> std::int64_t {
  return std::max(range.left, range.right) - std::min(range.left, range.right) +
         1;
}

auto PackedPartOf(const VariableObject& object) -> PackedPart {
  const abi::Typespec& reached = ReachedTypespecOf(object);
  PackedPart part{.lsb = 0, .width = reached.width, .typespec = &reached};
  for (const PackedStep& step : object.packed) {
    const abi::Typespec& at = *part.typespec;
    part = std::visit(
        Overloaded{
            [&](const PackedMemberStep& member) {
              const std::span<const abi::TypespecMember* const> members =
                  at.members.Entries();
              const abi::Typespec& of = *members[member.member]->typespec;
              std::uint64_t below = 0;
              switch (FactsOf(at).members) {
                case MemberLayout::kStructure:
                  for (std::size_t later = member.member + 1;
                       later < members.size(); ++later) {
                    below += members[later]->typespec->width;
                  }
                  break;
                case MemberLayout::kUnion:
                  break;
                case MemberLayout::kNone:
                  throw InternalError(
                      "vpi: a member step into a value with no members -- "
                      "please report this as a bug");
              }
              return PackedPart{
                  .lsb = part.lsb + below, .width = of.width, .typespec = &of};
            },
            [&](const PackedIndexStep& index) {
              const std::optional<PackedDimension> dimension = DimensionOf(at);
              const std::optional<std::uint64_t> offset =
                  dimension.has_value()
                      ? OffsetIn(dimension->range, index.index)
                      : std::nullopt;
              if (!offset.has_value()) {
                throw InternalError(
                    "vpi: a packed index lies outside its dimension -- please "
                    "report this as a bug");
              }
              return PackedPart{
                  .lsb = part.lsb + (*offset * dimension->element_width),
                  .width = dimension->element_width,
                  .typespec = dimension->element};
            }},
        step);
    if (part.typespec == nullptr) break;
  }
  return part;
}

// Whether `object` is one bit of a packed value (LRM 37.17 detail 20): the
// last step is an index reaching a position that is a bit.
auto IsVarBit(const VariableObject& object) -> bool {
  if (object.packed.empty() ||
      !std::holds_alternative<PackedIndexStep>(object.packed.back())) {
    return false;
  }
  const PackedPart part = PackedPartOf(object);
  return part.typespec == nullptr ||
         (FactsOf(*part.typespec).single_bit && !part.typespec->Range());
}

// The typespec of what `object` is: the part's own, none for a bit with no
// typespec.
auto TypespecOfPart(const VariableObject& object) -> const abi::Typespec* {
  if (object.packed.empty()) return &ReachedTypespecOf(object);
  return PackedPartOf(object).typespec;
}

// The typespec each step of `object`'s selection is taken from, in order.
auto SteppedFrom(const VariableObject& object)
    -> std::vector<const abi::Typespec*> {
  std::vector<const abi::Typespec*> from;
  const abi::Typespec* at = RecordOf(object).typespec;
  for (const Selection::Step& step : object.selection.Steps()) {
    from.push_back(at);
    at = &SelectedFrom(*at, step);
  }
  return from;
}

// Whether each of `object`'s steps, in order -- the selection's and then the
// packed ones -- steps to a member, as against to an index.
auto StepsToMembers(const VariableObject& object) -> std::vector<bool> {
  std::vector<bool> members;
  for (const abi::Typespec* at : SteppedFrom(object)) {
    switch (FactsOf(*at).shape) {
      case Shape::kUnpackedAggregate:
        members.push_back(true);
        break;
      case Shape::kFixedArray:
      case Shape::kSizedArray:
      case Shape::kAssociative:
      case Shape::kIntegral:
      case Shape::kReal:
      case Shape::kText:
      case Shape::kHandle:
      case Shape::kEvent:
        members.push_back(false);
        break;
    }
  }
  for (const PackedStep& step : object.packed) {
    members.push_back(std::holds_alternative<PackedMemberStep>(step));
  }
  return members;
}

// The object `object` is a step of: none for a variable itself.
auto Prefix(const VariableObject& object) -> std::optional<VariableObject> {
  VariableObject prefix = object;
  if (!prefix.packed.empty()) {
    prefix.packed.pop_back();
    return prefix;
  }
  std::vector<Selection::Step> steps = object.selection.Steps();
  if (steps.empty()) return std::nullopt;
  steps.pop_back();
  prefix.selection = Selection(std::move(steps));
  return prefix;
}

// Whether a step into a dynamically sized array lies on the way to `object`,
// whose element then lives in storage the run allocated (LRM 37.3.7, 37.17
// detail 27).
auto ReachesSizedElement(const VariableObject& object) -> bool {
  for (const abi::Typespec* at : SteppedFrom(object)) {
    switch (FactsOf(*at).shape) {
      case Shape::kSizedArray:
      case Shape::kAssociative:
        return true;
      case Shape::kIntegral:
      case Shape::kReal:
      case Shape::kText:
      case Shape::kHandle:
      case Shape::kEvent:
      case Shape::kUnpackedAggregate:
      case Shape::kFixedArray:
        break;
    }
  }
  return false;
}

// Where what foreign code asks of the design fails, as the design's own
// failure is reported: the design's call of the context import it runs under
// (LRM 35.5.3).
auto ForeignCallSite() -> base::SourceLocation {
  const RunningState* running =
      AmbientRunContext::Current().Effects().TryRunning();
  if (running == nullptr || !running->application_calls.InsideAnImportCall()) {
    return base::SourceLocation{base::kNoSourceLocation};
  }
  return running->application_calls.WrittenAt();
}

// The bodies of the scope `object` lies in, through which a call reaching the
// design's storage enters code of the design -- a read as a write does -- and
// none where no code of the design is entered: what called into foreign code
// is leaving, disabled or having ended the run (LRM 9.6.2, 20.2), and the
// design runs nothing past that, as a call foreign code makes into it runs
// nothing then (LRM 35.9). What answers from the records alone -- a name, a
// type, a relation, a selection settled by the types -- enters none.
auto EnteredBodies(const VariableObject& object)
    -> VpiAnswer<const abi::ScopeDeclarations*> {
  if (RunningDepartureIsDue(AmbientRunContext::Current().Effects())) {
    return std::unexpected<std::string>(
        "what called into foreign code has ended");
  }
  return &ScopeDeclarationsOf(object.holder);
}

auto ReadInto(const VariableObject& object, ValueSerializer& serializer)
    -> VpiAnswer<void> {
  const VpiAnswer<const abi::ScopeDeclarations*> entered =
      EnteredBodies(object);
  if (!entered.has_value()) return std::unexpected(entered.error());
  const abi::ScopeDeclarations& declarations = **entered;
  declarations.read(
      InstanceOf(object.holder), object.row, object.selection, serializer,
      ForeignCallSite());
  return {};
}

// What a read hands back: the value of a part holding one, the object a class
// variable names, and where the selection ends at an array whose size the run
// sets, how many elements it holds and, for a read asking for them, the keys of
// an associative array's entries, each as the array keeps it.
class CapturedRead final : public ValueSerializer {
 public:
  // A read of a value or a size, and one of an associative array's keys.
  [[nodiscard]] static auto OfValue() -> CapturedRead {
    return CapturedRead(false);
  }
  [[nodiscard]] static auto OfKeys() -> CapturedRead {
    return CapturedRead(true);
  }

  [[nodiscard]] auto WantsKeys() const -> bool override {
    return wants_keys_;
  }

  void Receive(const Serialized& handed) override {
    std::visit(
        Overloaded{
            [&](const value::ConstIntegralView& bits) {
              held_ = value::BitVec::Of(bits);
            },
            [&](double real) { held_ = real; },
            [&](const value::String* text) { held_ = *text; },
            [&](const value::ObjectRef* named) { object_ = *named; },
            [&](const SerializedSize& size) { size_ = size.count; },
            [&](const SerializedKey& key) {
              keys_.push_back(value::AnyValue::CopyOf(*key.type, key.bytes));
            }},
        handed);
  }

  [[nodiscard]] auto Held() -> std::optional<HeldValue>& {
    return held_;
  }
  [[nodiscard]] auto Size() const -> std::optional<std::int64_t> {
    return size_;
  }
  [[nodiscard]] auto Keys() -> std::vector<value::AnyValue>& {
    return keys_;
  }
  [[nodiscard]] auto Object() const -> const std::optional<value::ObjectRef>& {
    return object_;
  }

 private:
  explicit CapturedRead(bool wants_keys) : wants_keys_(wants_keys) {
  }

  bool wants_keys_;
  std::optional<HeldValue> held_;
  std::optional<std::int64_t> size_;
  std::vector<value::AnyValue> keys_;
  std::optional<value::ObjectRef> object_;
};

// How many elements the array `object` reaches holds now.
auto SizeOf(const VariableObject& object) -> VpiAnswer<std::int64_t> {
  CapturedRead captured = CapturedRead::OfValue();
  VpiAnswer<void> read = ReadInto(object, captured);
  if (!read.has_value()) return std::unexpected(std::move(read.error()));
  return captured.Size().value_or(0);
}

// The steps to each element of the array `object` reaches, in its order: a
// fixed-size array's indices left to right, a dynamic array's or a queue's
// positions from the front, an associative array's indices in the order it
// keeps them (LRM 7.9.4).
auto ElementStepsOf(const VariableObject& object)
    -> VpiAnswer<std::vector<Selection::Step>> {
  const abi::Typespec& array = ReachedTypespecOf(object);
  std::vector<Selection::Step> steps;
  switch (FactsOf(array).shape) {
    case Shape::kFixedArray:
      for (const std::int64_t index : IndicesOf(*array.Range())) {
        steps.emplace_back(index);
      }
      return steps;
    case Shape::kSizedArray: {
      const VpiAnswer<std::int64_t> size = SizeOf(object);
      if (!size.has_value()) return std::unexpected(size.error());
      for (std::int64_t at = 0; at < *size; ++at) steps.emplace_back(at);
      return steps;
    }
    // An entry is named by the key the array keeps it by. A wildcard index's
    // keys are listed by no operation of the language (LRM 7.8.1, 7.9).
    case Shape::kAssociative: {
      if (array.index == nullptr) {
        return VpiNotYet(
            "listing the entries of an associative array with a wildcard "
            "index");
      }
      CapturedRead captured = CapturedRead::OfKeys();
      VpiAnswer<void> read = ReadInto(object, captured);
      if (!read.has_value()) return std::unexpected(std::move(read.error()));
      for (value::AnyValue& key : captured.Keys()) {
        steps.emplace_back(SelectedKey{.key = std::move(key)});
      }
      return steps;
    }
    case Shape::kIntegral:
    case Shape::kReal:
    case Shape::kText:
    case Shape::kHandle:
    case Shape::kEvent:
    case Shape::kUnpackedAggregate:
      return steps;
  }
  throw InternalError("ElementStepsOf: unknown shape");
}

// The elements of every dimension of the unpacked array `object` reaches, down
// to the first that is no unpacked array (LRM 37.17 detail 5, 37.19). A named
// event is no variable these routines reach yet, however it is reached.
auto CollectElements(const VariableObject& object, std::vector<VpiObject>& out)
    -> VpiAnswer<void> {
  const Shape shape = FactsOf(ReachedTypespecOf(object)).shape;
  if (shape == Shape::kEvent) return VpiNotYet("a named event");
  if (!object.packed.empty() || !IsUnpackedArray(shape)) {
    out.emplace_back(object);
    return {};
  }
  VpiAnswer<std::vector<Selection::Step>> steps = ElementStepsOf(object);
  if (!steps.has_value()) return std::unexpected(std::move(steps.error()));
  for (Selection::Step& step : *steps) {
    VpiAnswer<void> collected =
        CollectElements(WithStep(object, std::move(step)), out);
    if (!collected.has_value()) return collected;
  }
  return {};
}

// Each bit of the packed value `object` is, its leftmost first, through every
// dimension (LRM 37.17 detail 12).
void CollectBits(const VariableObject& object, std::vector<VpiObject>& out) {
  const abi::Typespec* typespec = TypespecOfPart(object);
  const std::optional<PackedDimension> dimension =
      typespec != nullptr ? DimensionOf(*typespec) : std::nullopt;
  if (!dimension.has_value()) {
    out.emplace_back(object);
    return;
  }
  for (const std::int64_t index : IndicesOf(dimension->range)) {
    CollectBits(WithPacked(object, PackedIndexStep{.index = index}), out);
  }
}

// The array type `array` is in `mode` (LRM 37.17, 36.12.1 entries 1, 3, 4).
auto ArrayTypeIn(const abi::Typespec& array, VpiMode mode) -> PLI_INT32 {
  const abi::Typespec* leaf = &array;
  std::size_t dimensions = 0;
  while (IsUnpackedArray(FactsOf(*leaf).shape)) {
    leaf = leaf->element;
    ++dimensions;
  }
  const ModeFacts facts = FactsOf(mode);
  const PLI_INT32 leaf_type = FactsOf(*leaf).variable_type;
  if (facts.memories && dimensions == 1 &&
      FactsOf(array).shape == Shape::kFixedArray && leaf_type == vpiReg) {
    return vpiMemory;
  }
  if (facts.integer_arrays &&
      (leaf_type == vpiIntegerVar || leaf_type == vpiTimeVar)) {
    return leaf_type;
  }
  if (facts.real_arrays && leaf_type == vpiRealVar) {
    return vpiRealVar;
  }
  return vpiArrayVar;
}

// Whether `array` is a memory as LRM 37.20 has one: a fixed-size array of one
// dimension of `reg`.
auto IsMemory(const abi::Typespec& array) -> bool {
  return FactsOf(array).shape == Shape::kFixedArray &&
         FactsOf(*array.element).variable_type == vpiReg &&
         !IsUnpackedArray(FactsOf(*array.element).shape);
}

// The type of the variable `object` names, which for a reference is the one it
// is bound to: a reference and its actual are of equivalent types (LRM
// 23.3.3.2, 13.5.2).
auto ActualVariableType(const VariableObject& object, VpiMode mode)
    -> PLI_INT32 {
  if (!object.packed.empty()) {
    if (IsVarBit(object)) return vpiVarBit;
    return FactsOf(*PackedPartOf(object).typespec).variable_type;
  }
  const abi::Typespec& reached = ReachedTypespecOf(object);
  if (IsUnpackedArray(FactsOf(reached).shape)) {
    return ArrayTypeIn(reached, mode);
  }
  if (FactsOf(mode).memories) {
    const std::optional<VariableObject> prefix = Prefix(object);
    if (prefix.has_value() && prefix->packed.empty() &&
        IsUnpackedArray(FactsOf(ReachedTypespecOf(*prefix)).shape) &&
        ArrayTypeIn(ReachedTypespecOf(*prefix), mode) == vpiMemory) {
      return vpiMemoryWord;
    }
  }
  return FactsOf(reached).variable_type;
}

auto VariableType(const VariableObject& object, VpiMode mode) -> PLI_INT32 {
  if (RecordOf(object).Reference()) return vpiRefObj;
  return ActualVariableType(object, mode);
}

// The names a part of a variable answers (LRM 37.17 detail 28): the last
// identifier on the way to it with each index after that identifier, and the
// whole of the way from the variable.
struct PartNames {
  std::string name;
  std::string decompile;
};

// A step as the source writes the index it selects by: a number; a key the
// way its literal is written, an integral one by its number at its type's
// signedness and a string one as a string literal. A key of a class handle or
// a `chandle` has no literal, so it is spelled by nothing.
auto IndexText(const Selection::Step& step) -> std::optional<std::string> {
  return std::visit(
      Overloaded{
          [](std::int64_t index) -> std::optional<std::string> {
            return std::to_string(index);
          },
          [](const SelectedKey& selected) -> std::optional<std::string> {
            const value::ValueType& type = selected.key.Type();
            if (const value::IntegralValueType* integral = type.AsIntegral()) {
              value::FormatSpec spec;
              spec.kind = value::FormatKind::kDecimal;
              spec.width = 0;
              return value::FormatIntegral(
                  spec, integral->Load(selected.key.Bytes()).View());
            }
            if (&type == &value::LibraryTypeOf<value::String>()) {
              return StringLiteralOf(
                  static_cast<const value::String*>(selected.key.Bytes())
                      ->View());
            }
            return std::nullopt;
          }},
      step);
}

// The constant a step selects by, as `vpiIndex` answers it (LRM 37.18 detail
// 6, 37.19).
auto IndexObjectOf(const Selection::Step& step) -> VpiAnswer<VpiObject> {
  if (const std::optional<std::int64_t> index = IndexOfStep(step)) {
    return VpiObject{ConstantObject{.value = *index}};
  }
  return VpiNotYet(
      "the index of an entry of an associative array indexed by a string, a "
      "class, a chandle or a value wider than 64 bits");
}

// `object` naming each entry of an associative array by the key the array
// keeps it by, which the code settling its selection converts from the number
// or the text foreign code named it with, by the types alone (LRM 7.8.4); no
// code of the design runs for it, so it is settled however the run stands, and
// what is answered of it later -- a name, an identity, an index -- the records
// alone answer.
auto Settled(VariableObject object) -> VariableObject {
  SelectionSettler settler;
  ScopeDeclarationsOf(object.holder)
      .settle(
          InstanceOf(object.holder), object.row, object.selection, settler,
          ForeignCallSite());
  object.selection = settler.Settled(object.selection);
  return object;
}

auto NamesOf(const VariableObject& object) -> std::optional<PartNames> {
  const abi::DataObjectRecord& record = RecordOf(object);
  PartNames names{.name = record.identifier, .decompile = record.identifier};
  const auto member = [&](const char* identifier) {
    names.decompile.append(".").append(identifier);
    names.name = identifier;
  };
  const auto index = [&](const std::string& text) {
    const std::string written = "[" + text + "]";
    names.decompile += written;
    names.name += written;
  };
  const abi::Typespec* at = record.typespec;
  for (const Selection::Step& step : object.selection.Steps()) {
    if (FactsOf(*at).shape == Shape::kUnpackedAggregate) {
      member(
          at->members
              .Entries()[static_cast<std::size_t>(std::get<std::int64_t>(step))]
              ->identifier);
    } else {
      const std::optional<std::string> text = IndexText(step);
      if (!text.has_value()) return std::nullopt;
      index(*text);
    }
    at = &SelectedFrom(*at, step);
  }
  for (const PackedStep& step : object.packed) {
    std::visit(
        Overloaded{
            [&](const PackedMemberStep& stepped) {
              member(at->members.Entries()[stepped.member]->identifier);
              at = at->members.Entries()[stepped.member]->typespec;
            },
            [&](const PackedIndexStep& stepped) {
              index(std::to_string(stepped.index));
              const std::optional<PackedDimension> dimension = DimensionOf(*at);
              if (dimension.has_value() && dimension->element != nullptr) {
                at = dimension->element;
              }
            }},
        step);
  }
  return names;
}

// The part `object` is of at `index`, one dimension further in: an element
// of an unpacked array, of a packed array or a bit (LRM 38.19). None where
// the index names nothing, which no legal select of the source could write.
auto IndexInto(const VariableObject& object, std::int64_t index)
    -> VpiAnswer<std::optional<VariableObject>> {
  const abi::Typespec* typespec = TypespecOfPart(object);
  if (typespec == nullptr) return std::nullopt;
  if (object.packed.empty()) {
    switch (FactsOf(*typespec).shape) {
      case Shape::kFixedArray:
        if (!OffsetIn(*typespec->Range(), index).has_value()) {
          return std::nullopt;
        }
        return WithStep(object, index);
      case Shape::kSizedArray: {
        const VpiAnswer<std::int64_t> size = SizeOf(object);
        if (!size.has_value()) return std::unexpected(size.error());
        if (index < 0 || index >= *size) return std::nullopt;
      }
        return WithStep(object, index);
      // A number names an entry of an associative array whose index is a
      // wildcard or integral, converted into its key as the design's own
      // select converts it (LRM 7.8.1, 7.8.4); one indexed by a string, a
      // class or a `chandle` is indexed by no number.
      case Shape::kAssociative:
        if (typespec->index != nullptr &&
            FactsOf(*typespec->index).shape != Shape::kIntegral) {
          return std::nullopt;
        }
        // A wildcard index is read unsigned (LRM 7.8.1), so a negative one
        // a routine hands, a 32-bit integer, is the number its bits are.
        if (typespec->index == nullptr && index < 0) {
          if (index < std::numeric_limits<std::int32_t>::min()) {
            return VpiNotYet(
                "a negative index wider than 32 bits of an associative array "
                "with a wildcard index");
          }
          return WithStep(object, index + (std::int64_t{1} << 32));
        }
        if (typespec->index == nullptr) return WithStep(object, index);
        {
          value::BitVec number = value::BitVec::Zeros(64);
          value::FromInt(number.MutableView(), index);
          return WithStep(object, NamedKey(std::move(number)));
        }
      case Shape::kIntegral:
        break;
      case Shape::kReal:
      case Shape::kText:
      case Shape::kHandle:
      case Shape::kEvent:
      case Shape::kUnpackedAggregate:
        return std::nullopt;
    }
  }
  const std::optional<PackedDimension> dimension = DimensionOf(*typespec);
  if (!dimension.has_value() ||
      !OffsetIn(dimension->range, index).has_value()) {
    return std::nullopt;
  }
  return WithPacked(object, PackedIndexStep{.index = index});
}

// The entry of the associative array indexed by `string` that `object`
// reaches whose key is `key`; none where it reaches no such array.
auto KeyInto(const VariableObject& object, const std::string& key)
    -> std::optional<VariableObject> {
  const abi::Typespec* typespec = TypespecOfPart(object);
  if (typespec == nullptr || !object.packed.empty() ||
      FactsOf(*typespec).shape != Shape::kAssociative ||
      typespec->index == nullptr ||
      FactsOf(*typespec->index).shape != Shape::kText) {
    return std::nullopt;
  }
  return WithStep(object, NamedKey(value::String(key)));
}

// The entry of the associative array indexed by an integral type that
// `object` reaches whose key is `bits`, a number no 64-bit integer holds;
// none where it reaches no such array.
auto WideInto(const VariableObject& object, const value::BitVec& bits)
    -> VpiAnswer<std::optional<VariableObject>> {
  const abi::Typespec* typespec = TypespecOfPart(object);
  if (typespec == nullptr || !object.packed.empty() ||
      FactsOf(*typespec).shape != Shape::kAssociative) {
    return std::nullopt;
  }
  if (typespec->index == nullptr) {
    return VpiNotYet(
        "an index of an associative array with a wildcard index that no "
        "64-bit integer holds");
  }
  if (FactsOf(*typespec->index).shape != Shape::kIntegral) {
    return std::nullopt;
  }
  return WithStep(object, NamedKey(bits));
}

// The part of `object` its indices select, one at a time; none where one
// selects nothing. A named event is no variable these routines reach yet.
auto IndexedBy(const VariableObject& object, std::span<const NameIndex> indices)
    -> VpiAnswer<std::optional<VpiObject>> {
  std::optional<VariableObject> at = object;
  for (const NameIndex& index : indices) {
    if (!at.has_value()) return std::optional<VpiObject>{};
    VpiAnswer<std::optional<VariableObject>> next = std::visit(
        Overloaded{
            [&](std::int64_t number) { return IndexInto(*at, number); },
            [&](const value::BitVec& bits)
                -> VpiAnswer<std::optional<VariableObject>> {
              return WideInto(*at, bits);
            },
            [&](const std::string& key)
                -> VpiAnswer<std::optional<VariableObject>> {
              return KeyInto(*at, key);
            }},
        index);
    if (!next.has_value()) return std::unexpected(std::move(next.error()));
    at = *std::move(next);
  }
  if (!at.has_value()) return std::optional<VpiObject>{};
  const abi::Typespec* reached = TypespecOfPart(*at);
  if (reached != nullptr && FactsOf(*reached).shape == Shape::kEvent) {
    return VpiNotYet("a named event");
  }
  return std::optional<VpiObject>{Settled(*std::move(at))};
}

// Whether `object` is a variable of a structure or union type (LRM 37.17
// detail 26), which a part of it has as its parent.
auto IsAggregateVariable(const VariableObject& object) -> bool {
  const abi::Typespec* typespec = TypespecOfPart(object);
  return typespec != nullptr &&
         FactsOf(*typespec).members != MemberLayout::kNone;
}

// The parent of `object` (LRM 37.17 detail 26): the first of its prefixes,
// from the nearest, that is a structure or union variable, a member, the
// largest packed array containing it or the largest unpacked array
// containing it.
auto ParentOf(const VariableObject& object) -> std::optional<VariableObject> {
  const std::vector<bool> members = StepsToMembers(object);
  std::optional<VariableObject> prefix = Prefix(object);
  for (std::size_t steps = members.size(); prefix.has_value(); --steps) {
    // `prefix` takes `steps - 1` steps; whether its own last one steps to a
    // member, and whether it ends at a packed value or an unpacked array that
    // nothing of its own kind contains.
    const std::size_t taken = steps - 1;
    const bool member = taken > 0 && members[taken - 1];
    const bool packed_root =
        prefix->packed.empty() ||
        std::holds_alternative<PackedMemberStep>(prefix->packed.back());
    const bool unpacked_root =
        prefix->packed.empty() &&
        (prefix->selection.Steps().empty() || members[taken - 1]);
    const abi::Typespec* typespec = TypespecOfPart(*prefix);
    if (IsAggregateVariable(*prefix) || member) return prefix;
    if (typespec != nullptr) {
      const Shape shape = FactsOf(*typespec).shape;
      if (shape == Shape::kIntegral && packed_root &&
          DimensionOf(*typespec).has_value() && !object.packed.empty()) {
        return prefix;
      }
      if (IsUnpackedArray(shape) && unpacked_root) return prefix;
    }
    prefix = Prefix(*prefix);
  }
  return std::nullopt;
}

// The indices `object` is selected by from what it is an element of, its own
// first and working outward (LRM 37.17 details 5, 13, 37.18 detail 6): those
// of a part of a packed value go no further than that value, the largest
// packed array holding it, which is its parent (LRM 37.17 detail 26).
auto TrailingIndicesOf(const VariableObject& object)
    -> std::vector<Selection::Step> {
  std::vector<Selection::Step> indices;
  for (const PackedStep& step : std::views::reverse(object.packed)) {
    const auto* index = std::get_if<PackedIndexStep>(&step);
    if (index == nullptr) return indices;
    indices.emplace_back(index->index);
  }
  if (!object.packed.empty()) return indices;
  const std::vector<bool> members = StepsToMembers(object);
  const std::vector<Selection::Step>& steps = object.selection.Steps();
  for (std::size_t at = steps.size(); at-- > 0;) {
    if (members[at]) return indices;
    indices.push_back(steps[at]);
  }
  return indices;
}

// The ranges of `object`'s dimensions, leftmost first (LRM 37.17 detail 4):
// an unpacked array's, the first of a dynamic array or a queue being what it
// holds now and every other one sized by the run empty; or a packed value's,
// without a structure's, a union's or an enumeration's own.
auto RangesOf(const VariableObject& object)
    -> VpiAnswer<std::vector<RangeObject>> {
  std::vector<RangeObject> ranges;
  const abi::Typespec* at = TypespecOfPart(object);
  if (at == nullptr) return ranges;
  if (object.packed.empty() && IsUnpackedArray(FactsOf(*at).shape)) {
    bool first = true;
    while (IsUnpackedArray(FactsOf(*at).shape)) {
      switch (FactsOf(*at).shape) {
        case Shape::kFixedArray:
          ranges.push_back(RangeObject{.range = at->Range()});
          break;
        case Shape::kSizedArray: {
          std::int64_t size = 0;
          if (first) {
            const VpiAnswer<std::int64_t> held = SizeOf(object);
            if (!held.has_value()) return std::unexpected(held.error());
            size = *held;
          }
          ranges.push_back(
              RangeObject{
                  .range = size > 0 ? std::optional(
                                          support::Range{
                                              .left = 0, .right = size - 1})
                                    : std::nullopt});
          break;
        }
        case Shape::kAssociative:
        case Shape::kIntegral:
        case Shape::kReal:
        case Shape::kText:
        case Shape::kHandle:
        case Shape::kEvent:
        case Shape::kUnpackedAggregate:
          ranges.push_back(RangeObject{.range = std::nullopt});
          break;
      }
      first = false;
      at = at->element;
    }
    return ranges;
  }
  for (; at != nullptr && at->Range().has_value(); at = at->element) {
    ranges.push_back(RangeObject{.range = at->Range()});
  }
  return ranges;
}

// The ranges a typespec states (LRM 37.25 detail 10): an array typespec's
// unpacked dimensions, each one the run sizes empty, or a vector's packed
// ones.
auto RangesOf(const abi::Typespec& typespec) -> std::vector<RangeObject> {
  std::vector<RangeObject> ranges;
  const abi::Typespec* at = &typespec;
  if (IsUnpackedArray(FactsOf(*at).shape)) {
    for (; IsUnpackedArray(FactsOf(*at).shape); at = at->element) {
      ranges.push_back(RangeObject{.range = at->Range()});
    }
    return ranges;
  }
  for (; at != nullptr && at->Range().has_value(); at = at->element) {
    ranges.push_back(RangeObject{.range = at->Range()});
  }
  return ranges;
}

auto BoundOf(const RangeObject& range, bool left) -> std::optional<VpiObject> {
  if (!range.range.has_value()) return std::nullopt;
  return VpiObject{
      ConstantObject{.value = left ? range.range->left : range.range->right}};
}

auto LeftmostRange(const std::vector<RangeObject>& ranges)
    -> std::optional<RangeObject> {
  if (ranges.empty()) return std::nullopt;
  return ranges.front();
}

// The members of the structure or union `object` is, as variables (LRM
// 37.26).
auto MembersOf(const VariableObject& object) -> std::vector<VpiObject> {
  std::vector<VpiObject> members;
  const abi::Typespec* typespec = TypespecOfPart(object);
  if (typespec == nullptr ||
      FactsOf(*typespec).members == MemberLayout::kNone) {
    return members;
  }
  const std::size_t count = typespec->members.Entries().size();
  const bool unpacked = object.packed.empty() &&
                        FactsOf(*typespec).shape == Shape::kUnpackedAggregate;
  for (std::size_t at = 0; at < count; ++at) {
    if (unpacked) {
      members.emplace_back(WithStep(object, static_cast<std::int64_t>(at)));
    } else {
      members.emplace_back(WithPacked(
          object, PackedMemberStep{.member = static_cast<std::uint32_t>(at)}));
    }
  }
  return members;
}

// The elements of the outermost dimension of the packed array `object` is,
// each of the next dimension's typespec (LRM 37.18 detail 3).
auto PackedElementsOf(const VariableObject& object) -> std::vector<VpiObject> {
  std::vector<VpiObject> elements;
  const abi::Typespec* typespec = TypespecOfPart(object);
  if (typespec == nullptr || !typespec->Range().has_value() ||
      typespec->element == nullptr) {
    return elements;
  }
  for (const std::int64_t index : IndicesOf(*typespec->Range())) {
    elements.emplace_back(WithPacked(object, PackedIndexStep{.index = index}));
  }
  return elements;
}

// The format a value of the integral type `typespec` is its own in (LRM
// 38.15): an integer's, a time's, a scalar's or a vector's.
auto IntegralFormatOf(const abi::Typespec& typespec) -> PLI_INT32 {
  const abi::Typespec& value =
      typespec.base != nullptr ? *typespec.base : typespec;
  const PLI_INT32 type = FactsOf(value).variable_type;
  if (type == vpiIntVar || type == vpiIntegerVar || type == vpiShortIntVar ||
      type == vpiByteVar) {
    return vpiIntVal;
  }
  if (type == vpiTimeVar) return vpiTimeVal;
  if (FactsOf(value).single_bit && !value.Range().has_value()) {
    return vpiScalarVal;
  }
  return vpiVectorVal;
}

// The shape of `object`'s value, none where it has no value a format reads or
// writes (LRM 37.17 detail 11, 37.26 detail 1).
auto ValueShapeOfVariable(const VariableObject& object)
    -> std::optional<ValueShape> {
  if (!object.packed.empty()) {
    const PackedPart part = PackedPartOf(object);
    const bool bit = IsVarBit(object);
    PLI_INT32 natural = vpiVectorVal;
    if (bit) {
      natural = vpiScalarVal;
    } else if (part.typespec != nullptr) {
      natural = IntegralFormatOf(*part.typespec);
    }
    return ValueShape{
        .held =
            IntegralShape{
                .width = part.width,
                .is_signed = !bit && part.typespec != nullptr &&
                             part.typespec->IsSigned()},
        .natural_format = natural};
  }
  const abi::Typespec& reached = ReachedTypespecOf(object);
  switch (FactsOf(reached).shape) {
    case Shape::kIntegral:
      return ValueShape{
          .held =
              IntegralShape{
                  .width = reached.width, .is_signed = reached.IsSigned()},
          .natural_format = IntegralFormatOf(reached)};
    case Shape::kReal:
      return ValueShape{.held = RealShape{}, .natural_format = vpiRealVal};
    case Shape::kText:
      return ValueShape{.held = TextShape{}, .natural_format = vpiStringVal};
    case Shape::kHandle:
    case Shape::kEvent:
    case Shape::kUnpackedAggregate:
    case Shape::kFixedArray:
    case Shape::kSizedArray:
    case Shape::kAssociative:
      return std::nullopt;
  }
  throw InternalError("ValueShapeOfVariable: unknown shape");
}

// The bits `object` is of the packed value its selection reaches, which a
// read of that value hands whole.
auto PartOfLeaf(const value::BitVec& leaf, const PackedPart& part)
    -> value::BitVec {
  const auto width = static_cast<std::int64_t>(leaf.Len());
  return leaf.ExtractBits(
      width - static_cast<std::int64_t>(part.lsb + part.width),
      static_cast<std::int64_t>(part.width));
}

// What a write from foreign code hands the code writing a data object.
class HandedValue final : public ValueDeserializer {
 public:
  explicit HandedValue(const HeldValue& held) : held_(&held) {
  }

  auto Bits() -> value::BitVec override {
    return std::get<value::BitVec>(*held_);
  }
  auto Real() -> value::Real override {
    return value::Real(std::get<double>(*held_));
  }
  auto Text() -> value::String override {
    return std::get<value::String>(*held_);
  }

 private:
  const HeldValue* held_;
};

// The value `object` holds: of a part of a packed value, that part's bits.
auto ReadVariable(const VariableObject& object) -> VpiAnswer<HeldValue> {
  if (!ValueShapeOfVariable(object).has_value()) {
    return std::unexpected<std::string>("the object has no value");
  }
  CapturedRead captured = CapturedRead::OfValue();
  VpiAnswer<void> read = ReadInto(object, captured);
  if (!read.has_value()) return std::unexpected(std::move(read.error()));
  if (!captured.Held().has_value()) {
    throw InternalError(
        "vpi: a variable's read handed no value -- please report this as a "
        "bug");
  }
  auto* leaf = std::get_if<value::BitVec>(&*captured.Held());
  if (leaf == nullptr || object.packed.empty()) {
    return *std::move(captured.Held());
  }
  return HeldValue{PartOfLeaf(*leaf, PackedPartOf(object))};
}

// Each data object of every scope and namespace of the run, as a variable.
void CollectDesignVariables(Scope& scope, std::vector<VariableObject>& out) {
  const std::size_t count = scope.Declarations().data_objects.Entries().size();
  for (std::size_t row = 0; row < count; ++row) {
    out.push_back(
        VariableObject{
            .holder = &scope,
            .row = static_cast<std::uint32_t>(row),
            .selection = {},
            .packed = {}});
  }
  scope.ForEachChild([&](Scope& child) { CollectDesignVariables(child, out); });
}

// What a wait for a change of `object`'s row would enrol on, which is the
// storage it names however it was reached.
auto IdentityOf(const VariableObject& object) -> const void* {
  return DumpIdentityOf(
      DumpedObject{
          .instance = std::get_if<Scope*>(&object.holder) != nullptr
                          ? std::get<Scope*>(object.holder)
                          : nullptr,
          .declarations = &ScopeDeclarationsOf(object.holder),
          .position = object.row});
}

// The variable the reference `reference` names (LRM 37.15 detail 3), with its
// own steps taken in it.
auto ActualOf(RuntimeEffects& run, const VariableObject& reference)
    -> std::optional<VariableObject> {
  const void* storage = IdentityOf(
      VariableObject{
          .holder = reference.holder,
          .row = reference.row,
          .selection = {},
          .packed = {}});
  if (storage == nullptr) return std::nullopt;
  std::optional<VariableObject> actual =
      AmbientRunContext::Current().StoredVariables().Find(run, storage);
  if (!actual.has_value()) return std::nullopt;
  actual->selection = reference.selection;
  actual->packed = reference.packed;
  return actual;
}

// Whether `relation` from a scope names a kind of variable it declares (LRM
// 37.15, 37.17, 37.20).
auto IsDeclaredVariableRelation(PLI_INT32 relation) -> bool {
  switch (relation) {
    case vpiVariables:
    case vpiReg:
    case vpiRegArray:
    case vpiMemory:
    case vpiRefObj:
      return true;
    default:
      return false;
  }
}

// Whether `object` is one of the kind of variable `relation` names, in `mode`
// (LRM 36.12.1 entries 5 to 7).
auto IsOfRelation(
    const VariableObject& object, PLI_INT32 relation, VpiMode mode) -> bool {
  const abi::DataObjectRecord& record = RecordOf(object);
  if (record.NetType().has_value() ||
      FactsOf(*record.typespec).shape == Shape::kEvent) {
    return false;
  }
  const PLI_INT32 type = VariableType(object, mode);
  const ModeFacts facts = FactsOf(mode);
  switch (relation) {
    case vpiVariables:
      if (record.Reference()) return false;
      return facts.regs_are_variables ||
             (type != vpiReg && type != vpiRegArray && type != vpiMemory);
    case vpiReg:
      return type == vpiReg;
    // An array of `reg` under 1364 is fixed in every dimension, each element of
    // it a `reg` (LRM 36.12.1 Table 36-10 entry 7).
    case vpiRegArray: {
      if (type != vpiRegArray) return false;
      if (facts.reg_arrays_of_any_type) return true;
      const abi::Typespec* leaf = record.typespec;
      while (IsUnpackedArray(FactsOf(*leaf).shape)) {
        if (FactsOf(*leaf).shape != Shape::kFixedArray) return false;
        leaf = leaf->element;
      }
      return FactsOf(*leaf).variable_type == vpiReg;
    }
    case vpiMemory:
      return !record.Reference() && IsMemory(*record.typespec);
    case vpiRefObj:
      return record.Reference();
    default:
      return false;
  }
}

}  // namespace

auto VariablesByStorage::Find(RuntimeEffects& run, const void* storage)
    -> std::optional<VariableObject> {
  if (!table_.has_value()) {
    Runtime& runtime = AsRuntime(run);
    std::vector<VariableObject> variables;
    for (const abi::ScopeDeclarations* ns : runtime.Namespaces()) {
      const std::size_t count = ns->data_objects.Entries().size();
      for (std::size_t row = 0; row < count; ++row) {
        variables.push_back(
            VariableObject{
                .holder = NamespaceObject{.scope_declarations = ns},
                .row = static_cast<std::uint32_t>(row),
                .selection = {},
                .packed = {}});
      }
    }
    runtime.DesignRoot().ForEachChild(
        [&](Scope& top) { CollectDesignVariables(top, variables); });
    table_.emplace();
    for (VariableObject& variable : variables) {
      const abi::DataObjectRecord& record = RecordOf(variable);
      if (record.Reference() || record.NetType().has_value()) continue;
      const void* held = IdentityOf(variable);
      if (held != nullptr) table_->emplace(held, std::move(variable));
    }
  }
  const auto found = table_->find(storage);
  if (found == table_->end()) return std::nullopt;
  return found->second;
}

auto IsVariablePart(const VpiObject& object) -> bool {
  return std::holds_alternative<VariableObject>(object) ||
         std::holds_alternative<TypespecObject>(object) ||
         std::holds_alternative<TypespecMemberObject>(object) ||
         std::holds_alternative<RangeObject>(object) ||
         std::holds_alternative<ConstantObject>(object) ||
         std::holds_alternative<EnumConstantObject>(object);
}

auto SameVariablePart(const VpiObject& a, const VpiObject& b) -> bool {
  const auto* first = std::get_if<VariableObject>(&a);
  const auto* second = std::get_if<VariableObject>(&b);
  if (first == nullptr || second == nullptr) return a == b;
  if (first->holder != second->holder || first->row != second->row ||
      !(first->selection == second->selection)) {
    return false;
  }
  if (first->packed.empty() && second->packed.empty()) return true;
  const PackedPart one = PackedPartOf(*first);
  const PackedPart other = PackedPartOf(*second);
  return one.lsb == other.lsb && one.width == other.width &&
         VariableType(*first, VpiMode::k1800v2012) ==
             VariableType(*second, VpiMode::k1800v2012);
}

auto VariableNamed(
    const ScopeHolder& holder, const abi::LexicalScopeRecord* enclosing,
    const NamePart& part) -> VpiAnswer<std::optional<VpiObject>> {
  const std::span<const abi::DataObjectRecord* const> rows =
      ScopeDeclarationsOf(holder).data_objects.Entries();
  for (std::size_t row = 0; row < rows.size(); ++row) {
    const abi::DataObjectRecord& record = *rows[row];
    if (record.enclosing != enclosing ||
        std::string_view(record.identifier) != part.identifier) {
      continue;
    }
    if (record.NetType().has_value()) return VpiNotYet("a net");
    const VariableObject variable{
        .holder = holder,
        .row = static_cast<std::uint32_t>(row),
        .selection = {},
        .packed = {}};
    return IndexedBy(variable, part.indices);
  }
  return std::optional<VpiObject>{};
}

auto MemberNamed(const VariableObject& at, const NamePart& part)
    -> VpiAnswer<std::optional<VpiObject>> {
  const abi::Typespec* typespec = TypespecOfPart(at);
  if (typespec == nullptr ||
      FactsOf(*typespec).members == MemberLayout::kNone) {
    return std::optional<VpiObject>{};
  }
  const std::span<const abi::TypespecMember* const> members =
      typespec->members.Entries();
  for (std::size_t index = 0; index < members.size(); ++index) {
    if (std::string_view(members[index]->identifier) != part.identifier) {
      continue;
    }
    const bool unpacked = at.packed.empty() &&
                          FactsOf(*typespec).shape == Shape::kUnpackedAggregate;
    const VariableObject member =
        unpacked ? WithStep(at, static_cast<std::int64_t>(index))
                 : WithPacked(
                       at, PackedMemberStep{
                               .member = static_cast<std::uint32_t>(index)});
    return IndexedBy(member, part.indices);
  }
  return std::optional<VpiObject>{};
}

auto VariablesDeclaredIn(
    const ScopeHolder& holder, const abi::LexicalScopeRecord* enclosing,
    PLI_INT32 relation, VpiMode mode) -> std::optional<std::vector<VpiObject>> {
  if (!IsDeclaredVariableRelation(relation)) return std::nullopt;
  std::vector<VpiObject> found;
  const std::span<const abi::DataObjectRecord* const> rows =
      ScopeDeclarationsOf(holder).data_objects.Entries();
  for (std::size_t row = 0; row < rows.size(); ++row) {
    if (rows[row]->enclosing != enclosing) continue;
    const VariableObject variable{
        .holder = holder,
        .row = static_cast<std::uint32_t>(row),
        .selection = {},
        .packed = {}};
    if (IsOfRelation(variable, relation, mode)) found.emplace_back(variable);
  }
  return found;
}

auto VariableElementAt(
    const VariableObject& of, std::span<const std::int64_t> indices)
    -> VpiAnswer<std::optional<VpiObject>> {
  const std::vector<NameIndex> named(indices.begin(), indices.end());
  return IndexedBy(of, named);
}

auto TypeOfVariablePart(const VpiObject& object, VpiMode mode) -> PLI_INT32 {
  if (const auto* variable = std::get_if<VariableObject>(&object)) {
    return VariableType(*variable, mode);
  }
  if (const auto* typespec = std::get_if<TypespecObject>(&object)) {
    return FactsOf(*typespec->typespec).typespec_type;
  }
  if (std::holds_alternative<TypespecMemberObject>(object)) {
    return vpiTypespecMember;
  }
  if (std::holds_alternative<RangeObject>(object)) return vpiRange;
  if (std::holds_alternative<ConstantObject>(object)) return vpiConstant;
  if (std::holds_alternative<EnumConstantObject>(object)) return vpiEnumConst;
  throw InternalError("TypeOfVariablePart: no part of a variable");
}

auto TypeOfActual(const VpiObject& object, VpiMode mode) -> PLI_INT32 {
  if (const auto* variable = std::get_if<VariableObject>(&object)) {
    return ActualVariableType(*variable, mode);
  }
  return TypeOfVariablePart(object, mode);
}

auto RelatedToVariablePart(
    PLI_INT32 relation, const VpiObject& from, VpiMode mode)
    -> VpiAnswer<std::vector<VpiObject>> {
  if (const auto* variable = std::get_if<VariableObject>(&from)) {
    const abi::Typespec* typespec = TypespecOfPart(*variable);
    switch (relation) {
      case vpiVarSelect:
      case vpiMemoryWord: {
        std::vector<VpiObject> elements;
        if (variable->packed.empty() && typespec != nullptr &&
            IsUnpackedArray(FactsOf(*typespec).shape) &&
            (relation == vpiVarSelect || IsMemory(*typespec))) {
          VpiAnswer<void> collected = CollectElements(*variable, elements);
          if (!collected.has_value()) {
            return std::unexpected(std::move(collected.error()));
          }
        }
        return elements;
      }
      case vpiReg: {
        std::vector<VpiObject> elements;
        if (variable->packed.empty() && typespec != nullptr &&
            IsUnpackedArray(FactsOf(*typespec).shape)) {
          VpiAnswer<void> collected = CollectElements(*variable, elements);
          if (!collected.has_value()) {
            return std::unexpected(std::move(collected.error()));
          }
        }
        if (!FactsOf(mode).regs_of_any_type) {
          std::erase_if(elements, [&](const VpiObject& element) {
            return TypeOfVariablePart(element, mode) != vpiReg;
          });
        }
        return elements;
      }
      case vpiMember:
        return MembersOf(*variable);
      case vpiBit: {
        std::vector<VpiObject> bits;
        if (typespec != nullptr && DimensionOf(*typespec).has_value() &&
            !IsVarBit(*variable) &&
            FactsOf(*typespec).variable_type != vpiEnumVar) {
          CollectBits(*variable, bits);
        }
        return bits;
      }
      case vpiElement:
        return PackedElementsOf(*variable);
      case vpiRange: {
        VpiAnswer<std::vector<RangeObject>> held = RangesOf(*variable);
        if (!held.has_value()) return std::unexpected(std::move(held.error()));
        std::vector<VpiObject> ranges;
        for (RangeObject& range : *held) ranges.emplace_back(range);
        return ranges;
      }
      case vpiIndex: {
        std::vector<VpiObject> indices;
        for (const Selection::Step& step : TrailingIndicesOf(*variable)) {
          const VpiAnswer<VpiObject> index = IndexObjectOf(step);
          if (!index.has_value()) return std::unexpected(index.error());
          indices.push_back(*index);
        }
        return indices;
      }
      default:
        return VpiNotYet("this relation from a variable");
    }
  }
  if (const auto* typespec = std::get_if<TypespecObject>(&from)) {
    const abi::Typespec& of = *typespec->typespec;
    switch (relation) {
      case vpiTypespecMember: {
        std::vector<VpiObject> members;
        for (const abi::TypespecMember* member : of.members.Entries()) {
          members.emplace_back(TypespecMemberObject{.member = member});
        }
        return members;
      }
      case vpiEnumConst: {
        std::vector<VpiObject> constants;
        if (of.enumeration != nullptr) {
          for (std::size_t at = 0; at < of.enumeration->Count(); ++at) {
            constants.emplace_back(
                EnumConstantObject{
                    .enumeration = &of,
                    .position = static_cast<std::uint32_t>(at)});
          }
        }
        return constants;
      }
      case vpiRange: {
        std::vector<VpiObject> ranges;
        for (RangeObject& range : RangesOf(of)) ranges.emplace_back(range);
        return ranges;
      }
      default:
        return VpiNotYet("this relation from a typespec");
    }
  }
  return VpiNotYet("this relation from this object");
}

auto OneRelatedToVariablePart(
    RuntimeEffects& run, PLI_INT32 relation, const VpiObject& from)
    -> VpiAnswer<std::optional<VpiObject>> {
  using Answer = VpiAnswer<std::optional<VpiObject>>;
  const auto typespec_object = [](const abi::Typespec* typespec) -> Answer {
    if (typespec == nullptr) return std::optional<VpiObject>{};
    return std::optional<VpiObject>{TypespecObject{.typespec = typespec}};
  };
  if (const auto* variable = std::get_if<VariableObject>(&from)) {
    switch (relation) {
      case vpiTypespec:
        return typespec_object(TypespecOfPart(*variable));
      case vpiParent: {
        const std::optional<VariableObject> parent = ParentOf(*variable);
        if (!parent.has_value()) return std::optional<VpiObject>{};
        return std::optional<VpiObject>{*parent};
      }
      case vpiLeftRange:
      case vpiRightRange: {
        VpiAnswer<std::vector<RangeObject>> held = RangesOf(*variable);
        if (!held.has_value()) return std::unexpected(std::move(held.error()));
        const std::optional<RangeObject> leftmost = LeftmostRange(*held);
        if (!leftmost.has_value()) return std::optional<VpiObject>{};
        return BoundOf(*leftmost, relation == vpiLeftRange);
      }
      case vpiIndex: {
        const std::vector<Selection::Step> indices =
            TrailingIndicesOf(*variable);
        if (indices.empty()) return std::optional<VpiObject>{};
        const VpiAnswer<VpiObject> index = IndexObjectOf(indices.front());
        if (!index.has_value()) return std::unexpected(index.error());
        return std::optional<VpiObject>{*index};
      }
      case vpiScope: {
        const abi::DataObjectRecord& record = RecordOf(*variable);
        if (record.enclosing != nullptr) {
          return std::optional<VpiObject>{LexicalScope{
              .holder = variable->holder, .declaration = record.enclosing}};
        }
        return std::visit(
            Overloaded{
                [](Scope* scope) -> Answer {
                  return std::optional<VpiObject>{scope};
                },
                [](const NamespaceObject& ns) -> Answer {
                  return std::optional<VpiObject>{ns};
                }},
            variable->holder);
      }
      case vpiActual: {
        if (!RecordOf(*variable).Reference()) {
          return std::unexpected<std::string>(
              "only a reference object has an actual object");
        }
        const std::optional<VariableObject> actual = ActualOf(run, *variable);
        if (!actual.has_value()) return std::optional<VpiObject>{};
        return std::optional<VpiObject>{*actual};
      }
      case vpiExpr:
        return VpiNotYet("a variable's initialization expression");
      default:
        return VpiNotYet("this relation from a variable");
    }
  }
  if (const auto* typespec = std::get_if<TypespecObject>(&from)) {
    const abi::Typespec& of = *typespec->typespec;
    switch (relation) {
      // An array's or a packed array's typespec with its leftmost dimension
      // removed, none for one with no dimension left (LRM 37.25 detail 8).
      case vpiElemTypespec:
        if (IsUnpackedArray(FactsOf(of).shape) || of.Range().has_value()) {
          return typespec_object(of.element);
        }
        return std::optional<VpiObject>{};
      case vpiIndexTypespec:
        return typespec_object(of.index);
      case vpiBaseTypespec:
        return typespec_object(of.base);
      case vpiTypedefAlias:
        return typespec_object(of.alias);
      case vpiLeftRange:
      case vpiRightRange: {
        const std::optional<RangeObject> leftmost = LeftmostRange(RangesOf(of));
        if (!leftmost.has_value()) return std::optional<VpiObject>{};
        return BoundOf(*leftmost, relation == vpiLeftRange);
      }
      default:
        return VpiNotYet("this relation from a typespec");
    }
  }
  if (const auto* member = std::get_if<TypespecMemberObject>(&from)) {
    switch (relation) {
      case vpiTypespec:
        return typespec_object(member->member->typespec);
      case vpiExpr:
        return VpiNotYet("a member's default value");
      default:
        return VpiNotYet("this relation from a typespec member");
    }
  }
  if (const auto* range = std::get_if<RangeObject>(&from)) {
    switch (relation) {
      case vpiLeftRange:
      case vpiRightRange:
        return BoundOf(*range, relation == vpiLeftRange);
      default:
        return VpiNotYet("this relation from a range");
    }
  }
  return VpiNotYet("this relation from this object");
}

auto IndexOfStep(const Selection::Step& step) -> std::optional<std::int64_t> {
  return std::visit(
      Overloaded{
          [](std::int64_t index) -> std::optional<std::int64_t> {
            return index;
          },
          [](const SelectedKey& selected) -> std::optional<std::int64_t> {
            const value::IntegralValueType* integral =
                selected.key.Type().AsIntegral();
            if (integral == nullptr || integral->Shape().width > 64) {
              return std::nullopt;
            }
            return value::ToInt64(integral->Load(selected.key.Bytes()).View());
          }},
      step);
}

auto VariablePartIntegerProperty(
    PLI_INT32 property, const VpiObject& of, VpiMode mode)
    -> VpiAnswer<PLI_INT64> {
  const auto truth = [](bool holds) -> PLI_INT64 { return holds ? 1 : 0; };
  if (property == vpiType) return TypeOfVariablePart(of, mode);
  if (const auto* variable = std::get_if<VariableObject>(&of)) {
    const abi::DataObjectRecord& record = RecordOf(*variable);
    const abi::Typespec* typespec = TypespecOfPart(*variable);
    const bool bit = IsVarBit(*variable);
    const std::vector<bool> members = StepsToMembers(*variable);
    const bool array_member =
        !members.empty() && !members.back() && variable->packed.empty();
    // The scalar and vector properties of what an enumeration's or an array's
    // value is (LRM 37.17 detail 20).
    const abi::Typespec* valued = typespec;
    while (valued != nullptr && IsUnpackedArray(FactsOf(*valued).shape)) {
      valued = valued->element;
    }
    if (valued != nullptr && valued->base != nullptr) valued = valued->base;
    const bool integral =
        valued != nullptr && FactsOf(*valued).shape == Shape::kIntegral;
    const bool scalar = bit || (integral && FactsOf(*valued).single_bit &&
                                !valued->Range().has_value());
    switch (property) {
      case vpiSize: {
        if (bit) return 1;
        if (typespec == nullptr) return PLI_INT64{vpiUndefined};
        switch (FactsOf(*typespec).shape) {
          case Shape::kIntegral:
            return static_cast<PLI_INT64>(
                variable->packed.empty() ? typespec->width
                                         : PackedPartOf(*variable).width);
          case Shape::kUnpackedAggregate:
            return static_cast<PLI_INT64>(typespec->members.Entries().size());
          case Shape::kText: {
            const VpiAnswer<HeldValue> text = ReadVariable(*variable);
            if (!text.has_value()) return std::unexpected(text.error());
            return static_cast<PLI_INT64>(
                std::get<value::String>(*text).View().size());
          }
          case Shape::kFixedArray:
          case Shape::kSizedArray:
          case Shape::kAssociative: {
            // An array of one dimension sized by the run holds as many
            // variables as the size it hands, which needs no entry named.
            if (FactsOf(*typespec).shape != Shape::kFixedArray &&
                !IsUnpackedArray(FactsOf(*typespec->element).shape)) {
              const VpiAnswer<std::int64_t> size = SizeOf(*variable);
              if (!size.has_value()) return std::unexpected(size.error());
              return static_cast<PLI_INT64>(*size);
            }
            std::vector<VpiObject> elements;
            VpiAnswer<void> collected = CollectElements(*variable, elements);
            if (!collected.has_value()) {
              return std::unexpected(std::move(collected.error()));
            }
            return static_cast<PLI_INT64>(elements.size());
          }
          case Shape::kReal:
          case Shape::kHandle:
          case Shape::kEvent:
            return PLI_INT64{vpiUndefined};
        }
        return PLI_INT64{vpiUndefined};
      }
      case vpiSigned:
        return truth(!bit && typespec != nullptr && typespec->IsSigned());
      case vpiScalar:
        return truth(scalar);
      case vpiVector:
        return truth(integral && !scalar);
      case vpiArrayMember:
      case vpiArray:
        return truth(array_member);
      case vpiArrayType:
        if (typespec == nullptr || FactsOf(*typespec).array_type == 0 ||
            !variable->packed.empty()) {
          return VpiNotAProperty();
        }
        return FactsOf(*typespec).array_type;
      case vpiStructUnionMember:
        return truth(!members.empty() && members.back());
      case vpiPackedArrayMember: {
        const std::optional<VariableObject> parent = ParentOf(*variable);
        return truth(
            parent.has_value() &&
            VariableType(*parent, mode) == vpiPackedArrayVar);
      }
      // A structure, a union or a packed array is packed or not (LRM 37.18,
      // 37.26); a part of no such type has no such property.
      case vpiPacked: {
        if (typespec == nullptr) return VpiNotAProperty();
        const KindFacts facts = FactsOf(*typespec);
        if (facts.members == MemberLayout::kNone &&
            facts.variable_type != vpiPackedArrayVar) {
          return VpiNotAProperty();
        }
        return truth(facts.shape == Shape::kIntegral);
      }
      case vpiTagged:
        return truth(typespec != nullptr && typespec->Tagged());
      case vpiSoft:
        return truth(typespec != nullptr && typespec->Soft());
      case vpiConstantSelect:
        return truth(!ReachesSizedElement(*variable));
      case vpiAutomatic:
        return 0;
      case vpiAllocScheme:
        return ReachesSizedElement(*variable) ? vpiDynamicScheme
                                              : vpiOtherScheme;
      case vpiConstantVariable:
        return truth(record.Constant());
      case vpiIsRandomized:
        return 0;
      case vpiRandType:
        return vpiNotRand;
      case vpiVisibility:
        return vpiPublicVis;
      case vpiIsMemory:
        return truth(
            typespec != nullptr && variable->packed.empty() &&
            IsMemory(*typespec));
      default:
        return VpiNotAProperty();
    }
  }
  if (const auto* typespec = std::get_if<TypespecObject>(&of)) {
    const abi::Typespec& spec = *typespec->typespec;
    switch (property) {
      case vpiTagged:
        return truth(spec.Tagged());
      case vpiSoft:
        return truth(spec.Soft());
      case vpiPacked:
        return truth(
            FactsOf(spec).members != MemberLayout::kNone &&
            FactsOf(spec).shape == Shape::kIntegral);
      case vpiVector:
        return truth(
            FactsOf(spec).shape == Shape::kIntegral &&
            (spec.Range().has_value() || !FactsOf(spec).single_bit));
      case vpiArrayType:
        if (FactsOf(spec).array_type == 0) return VpiNotAProperty();
        return FactsOf(spec).array_type;
      case vpiSigned:
        return truth(spec.IsSigned());
      default:
        return VpiNotAProperty();
    }
  }
  if (const auto* member = std::get_if<TypespecMemberObject>(&of)) {
    if (property != vpiRandType) return VpiNotAProperty();
    switch (member->member->Random()) {
      case support::RandomQualifier::kNone:
        return vpiNotRand;
      case support::RandomQualifier::kRand:
        return vpiRand;
      case support::RandomQualifier::kRandc:
        return vpiRandC;
    }
    return VpiNotAProperty();
  }
  if (const auto* range = std::get_if<RangeObject>(&of)) {
    if (property != vpiSize) return VpiNotAProperty();
    return range->range.has_value() ? CountOf(*range->range) : 0;
  }
  if (const auto* constant = std::get_if<ConstantObject>(&of)) {
    switch (property) {
      case vpiConstType:
        return vpiDecConst;
      case vpiSize:
        return constant->value >= std::numeric_limits<std::int32_t>::min() &&
                       constant->value <=
                           std::numeric_limits<std::int32_t>::max()
                   ? 32
                   : 64;
      default:
        return VpiNotAProperty();
    }
  }
  return VpiNotAProperty();
}

auto VariablePartStringProperty(
    PLI_INT32 property, const VpiObject& of, VpiMode mode)
    -> VpiAnswer<std::optional<std::string>> {
  if (property == vpiType) {
    const std::optional<std::string_view> name =
        TypeName(TypeOfVariablePart(of, mode), mode);
    if (!name.has_value()) return VpiNotAProperty();
    return std::string(*name);
  }
  if (const auto* variable = std::get_if<VariableObject>(&of)) {
    const std::optional<PartNames> spelled = NamesOf(*variable);
    if (!spelled.has_value()) {
      return VpiNotYet(
          "the name of an entry of an associative array indexed by a class "
          "or a chandle");
    }
    const PartNames& names = *spelled;
    switch (property) {
      case vpiName:
        return names.name;
      case vpiDecompile:
        return names.decompile;
      case vpiFullName:
        return DeclarationPrefix(
                   variable->holder, RecordOf(*variable).enclosing) +
               names.decompile;
      default:
        return VpiNotAProperty();
    }
  }
  if (const auto* typespec = std::get_if<TypespecObject>(&of)) {
    if (property != vpiName) return VpiNotAProperty();
    if (std::string_view(typespec->typespec->name).empty()) {
      return std::optional<std::string>{};
    }
    return std::string(typespec->typespec->name);
  }
  if (const auto* member = std::get_if<TypespecMemberObject>(&of)) {
    if (property != vpiName) return VpiNotAProperty();
    return std::string(member->member->identifier);
  }
  if (const auto* constant = std::get_if<EnumConstantObject>(&of)) {
    if (property != vpiName) return VpiNotAProperty();
    return std::string(constant->enumeration->enumeration->names
                           .Entries()[constant->position]);
  }
  if (const auto* constant = std::get_if<ConstantObject>(&of)) {
    if (property != vpiDecompile) return VpiNotAProperty();
    return std::to_string(constant->value);
  }
  return VpiNotAProperty();
}

auto ValueShapeOf(const VpiObject& object) -> VpiAnswer<ValueShape> {
  if (const auto* variable = std::get_if<VariableObject>(&object)) {
    const std::optional<ValueShape> shape = ValueShapeOfVariable(*variable);
    if (!shape.has_value()) {
      return std::unexpected<std::string>("the object has no value");
    }
    return *shape;
  }
  if (const auto* constant = std::get_if<ConstantObject>(&object)) {
    const PLI_INT64 width =
        *VariablePartIntegerProperty(vpiSize, *constant, VpiMode::k1800v2012);
    return ValueShape{
        .held =
            IntegralShape{
                .width = static_cast<std::uint64_t>(width), .is_signed = true},
        .natural_format = vpiIntVal};
  }
  if (const auto* constant = std::get_if<EnumConstantObject>(&object)) {
    const abi::Typespec& base = *constant->enumeration->base;
    return ValueShape{
        .held =
            IntegralShape{
                .width = constant->enumeration->width,
                .is_signed = base.IsSigned()},
        .natural_format = vpiIntVal};
  }
  return std::unexpected<std::string>("the object has no value");
}

auto WatchOfVariablePart(const VariableObject& object) -> Trigger {
  const abi::ScopeDeclarations& declarations =
      ScopeDeclarationsOf(object.holder);
  Trigger watched =
      TriggerOfDataObject(declarations, InstanceOf(object.holder), object.row);
  SelectionSettler settler;
  declarations.settle(
      InstanceOf(object.holder), object.row, object.selection, settler,
      ForeignCallSite());
  watched.part = watched.part.Join(settler.WatchedPath());
  if (!watched.part.IsInexact() && !object.packed.empty()) {
    const PackedPart bits = PackedPartOf(object);
    watched.reads = {.lsb = bits.lsb, .width = bits.width};
  }
  return watched;
}

auto ReadVariablePart(const VpiObject& object) -> VpiAnswer<HeldValue> {
  if (const auto* variable = std::get_if<VariableObject>(&object)) {
    return ReadVariable(*variable);
  }
  if (const auto* constant = std::get_if<ConstantObject>(&object)) {
    const VpiAnswer<ValueShape> shape = ValueShapeOf(object);
    const std::uint64_t width = std::get<IntegralShape>(shape->held).width;
    value::BitVec bits = value::BitVec::Zeros(width);
    value::FromInt(bits.MutableView(), constant->value);
    return HeldValue{std::move(bits)};
  }
  if (const auto* constant = std::get_if<EnumConstantObject>(&object)) {
    const abi::Typespec& enumeration = *constant->enumeration;
    return HeldValue{value::BitVec::Of(
        enumeration.enumeration->MemberAt(constant->position))};
  }
  return std::unexpected<std::string>("the object has no value");
}

auto ObjectNamedByVariablePart(const VpiObject& object)
    -> VpiAnswer<value::ObjectRef> {
  const auto* variable = std::get_if<VariableObject>(&object);
  if (variable == nullptr) {
    return std::unexpected<std::string>("the object is no class variable");
  }
  CapturedRead captured = CapturedRead::OfValue();
  VpiAnswer<void> read = ReadInto(*variable, captured);
  if (!read.has_value()) return std::unexpected(std::move(read.error()));
  if (!captured.Object().has_value()) {
    throw InternalError(
        "vpi: a class variable's read handed no object -- please report this "
        "as a bug");
  }
  return *captured.Object();
}

namespace {

// What a write or a force of `object` lands on the leaf it lies in: `part`
// itself where the object is the leaf, and otherwise the leaf's value with the
// bits the object is replaced by `part`, so what waits on the other bits sees
// no change of them (LRM 9.4.2).
auto Landed(const VariableObject& object, const HeldValue& part)
    -> VpiAnswer<HeldValue> {
  if (object.packed.empty()) return part;
  VariableObject leaf = object;
  leaf.packed.clear();
  VpiAnswer<HeldValue> whole = ReadVariable(leaf);
  if (!whole.has_value()) return whole;
  auto& bits = std::get<value::BitVec>(*whole);
  const PackedPart at = PackedPartOf(object);
  bits.InsertBits(
      bits.Len() - static_cast<std::int64_t>(at.lsb + at.width),
      std::get<value::BitVec>(part));
  return whole;
}

}  // namespace

auto WriteVariablePart(const VpiObject& object, const HeldValue& written)
    -> VpiAnswer<void> {
  const auto* variable = std::get_if<VariableObject>(&object);
  if (variable == nullptr || !ValueShapeOfVariable(*variable).has_value()) {
    return std::unexpected<std::string>("the object has no value to write");
  }
  if (RecordOf(*variable).Constant()) {
    return std::unexpected<std::string>("the variable is declared const");
  }
  const VpiAnswer<const abi::ScopeDeclarations*> entered =
      EnteredBodies(*variable);
  if (!entered.has_value()) return std::unexpected(entered.error());
  VpiAnswer<HeldValue> landed = Landed(*variable, written);
  if (!landed.has_value()) return std::unexpected(landed.error());
  HandedValue handed(*landed);
  (*entered)->write(
      InstanceOf(variable->holder), variable->row, variable->selection, handed,
      ForeignCallSite());
  return {};
}

namespace {

// Which positions of the leaf `object` lies in a force or a release of it
// takes over (LRM 10.6.2): the bits of a packed value it is, or the one
// position of any other value.
struct ForcedPositions {
  std::int64_t first = 0;
  std::int64_t count = 0;
};

auto ForcedPositionsOf(const VariableObject& object)
    -> VpiAnswer<ForcedPositions> {
  if (!ValueShapeOfVariable(object).has_value()) {
    return std::unexpected<std::string>("the object has no value to force");
  }
  // A variable declared `const` is not written (LRM 6.20.6), and a force
  // writes what it takes over.
  if (RecordOf(object).Constant()) {
    return std::unexpected<std::string>("the variable is declared const");
  }
  if (FactsOf(ReachedTypespecOf(object)).shape != Shape::kIntegral) {
    return ForcedPositions{.first = 0, .count = 1};
  }
  const PackedPart part = PackedPartOf(object);
  return ForcedPositions{
      .first = static_cast<std::int64_t>(part.lsb),
      .count = static_cast<std::int64_t>(part.width)};
}

// What a force or a release came to, as the body that tried it answers: the
// part taken, or why it was not.
auto TakeoverOutcome(std::int64_t answer, std::string_view what)
    -> VpiAnswer<void> {
  switch (static_cast<support::TakeoverAnswer>(answer)) {
    case support::TakeoverAnswer::kTaken:
      return {};
    case support::TakeoverAnswer::kForbidden:
      return std::unexpected<std::string>(
          std::string(what) + " takes no part of a variable");
    case support::TakeoverAnswer::kNotCarriedOut:
      return VpiNotYet(std::string(what) + " of this part of a variable");
  }
  throw InternalError("vpi: a takeover answered nothing it can answer");
}

}  // namespace

auto ForceVariablePart(const VpiObject& object, const HeldValue& forced)
    -> VpiAnswer<void> {
  const auto* variable = std::get_if<VariableObject>(&object);
  if (variable == nullptr) {
    return std::unexpected<std::string>("the object has no value to force");
  }
  const VpiAnswer<ForcedPositions> positions = ForcedPositionsOf(*variable);
  if (!positions.has_value()) return std::unexpected(positions.error());
  const VpiAnswer<const abi::ScopeDeclarations*> entered =
      EnteredBodies(*variable);
  if (!entered.has_value()) return std::unexpected(entered.error());
  // A takeover is driven a value of the whole leaf, of which it shows the
  // positions it holds.
  VpiAnswer<HeldValue> driven = Landed(*variable, forced);
  if (!driven.has_value()) return std::unexpected(driven.error());
  HandedValue handed(*driven);
  SelectionSettler settler;
  return TakeoverOutcome(
      (*entered)->force(
          InstanceOf(variable->holder), variable->row, variable->selection,
          settler, positions->first, positions->count, handed,
          ForeignCallSite()),
      "a force");
}

auto ReleaseVariablePart(const VpiObject& object) -> VpiAnswer<HeldValue> {
  const auto* variable = std::get_if<VariableObject>(&object);
  if (variable == nullptr) {
    return std::unexpected<std::string>("the object has no value to release");
  }
  const VpiAnswer<ForcedPositions> positions = ForcedPositionsOf(*variable);
  if (!positions.has_value()) return std::unexpected(positions.error());
  const VpiAnswer<const abi::ScopeDeclarations*> entered =
      EnteredBodies(*variable);
  if (!entered.has_value()) return std::unexpected(entered.error());
  SelectionSettler settler;
  const VpiAnswer<void> released = TakeoverOutcome(
      (*entered)->release(
          InstanceOf(variable->holder), variable->row, variable->selection,
          settler, positions->first, positions->count, ForeignCallSite()),
      "a release");
  if (!released.has_value()) return std::unexpected(released.error());
  return ReadVariable(*variable);
}

}  // namespace lyra::runtime
