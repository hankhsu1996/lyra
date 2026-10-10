#include "lyra/value/runtime_unpacked_array.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/value/any_value.hpp"
#include "lyra/value/basic_dynamic_array.hpp"
#include "lyra/value/element_sequence.hpp"
#include "lyra/value/formation.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_value_type.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/unpacked_array.hpp"
#include "lyra/value/value_type.hpp"
#include "lyra/value/witnessed_elem.hpp"

namespace lyra::value {

namespace {

// An array of byte elements of `element`, the first of them the bytes `bytes`
// holds and the rest the element default (LRM 5.9).
auto FromBytes(
    std::string_view bytes, const IntegralValueType& element,
    std::int64_t count) -> RuntimeUnpackedArray {
  const auto element_count = static_cast<std::size_t>(count);
  const auto laid_out = [&](const LoadedWords& planes) {
    return AnyValue::Built(element, [&](void* out) { planes.StoreTo(out); });
  };
  LoadedWords unknown(element.Shape());
  FillScalar(unknown.Write(), element.Shape().width, FourStateBit::kUnknown);
  const AnyValue element_default = laid_out(unknown);
  std::vector<AnyValue> elements;
  elements.reserve(element_count);
  for (std::size_t i = 0; i < element_count; ++i) {
    if (i >= bytes.size()) {
      elements.push_back(element_default);
      continue;
    }
    LoadedWords byte(element.Shape());
    FromInt(
        byte.Write(), element.Shape().width,
        static_cast<unsigned char>(bytes[i]));
    elements.push_back(laid_out(byte));
  }
  std::vector<const void*> items;
  items.reserve(elements.size());
  for (const AnyValue& held : elements) {
    items.push_back(held.Bytes());
  }
  return {element, element_default.Bytes(), items};
}

}  // namespace

RuntimeUnpackedArray::RuntimeUnpackedArray() = default;

RuntimeUnpackedArray::RuntimeUnpackedArray(
    const ValueType& element, const void* element_default,
    std::span<const void* const> items)
    : core_(
          Core::Built(
              WitnessedElem(element, element_default), items.size(),
              [&](std::size_t i, void* out) { element.Copy(items[i], out); })) {
}

RuntimeUnpackedArray::RuntimeUnpackedArray(Core core) : core_(std::move(core)) {
}

RuntimeUnpackedArray::RuntimeUnpackedArray(const RuntimeUnpackedArray&) =
    default;
RuntimeUnpackedArray::RuntimeUnpackedArray(RuntimeUnpackedArray&&) noexcept =
    default;
auto RuntimeUnpackedArray::operator=(const RuntimeUnpackedArray&)
    -> RuntimeUnpackedArray& = default;
auto RuntimeUnpackedArray::operator=(RuntimeUnpackedArray&&) noexcept
    -> RuntimeUnpackedArray& = default;
RuntimeUnpackedArray::~RuntimeUnpackedArray() = default;

// An array whose declaration has not installed its element type has no
// elements to act on, so being asked to act on one is a lowering defect.
void RuntimeUnpackedArray::RequireInstalled() const {
  if (!core_.has_value()) {
    throw InternalError(
        "RuntimeUnpackedArray: an array is used before its declaration "
        "installs its element type -- please report this as a bug");
  }
}

auto RuntimeUnpackedArray::Installed() const -> const Core& {
  RequireInstalled();
  return *core_;
}

auto RuntimeUnpackedArray::Installed() -> Core& {
  RequireInstalled();
  return *core_;
}

auto RuntimeUnpackedArray::FromElements(
    const ValueType& element, const void* element_default,
    std::span<const void* const> items, std::int64_t declared)
    -> RuntimeUnpackedArray {
  if (static_cast<std::int64_t>(items.size()) != declared) {
    throw SimulationError(
        std::format(
            "a fixed-size unpacked array of {} elements cannot be assigned an "
            "array of {} (LRM 7.6)",
            declared, items.size()));
  }
  return {element, element_default, items};
}

auto RuntimeUnpackedArray::FromString(
    const String& text, const IntegralValueType& element, std::int64_t count)
    -> RuntimeUnpackedArray {
  return FromBytes(text.View(), element, count);
}

auto RuntimeUnpackedArray::FromIntegral(
    const ConstIntegralView& bits, const IntegralValueType& element,
    std::int64_t count) -> RuntimeUnpackedArray {
  return FromBytes(BytesOf(bits), element, count);
}

auto RuntimeUnpackedArray::ToByteString() const -> String {
  const IntegralValueType* byte_type = ElementType().AsIntegral();
  if (byte_type == nullptr) {
    throw InternalError(
        "RuntimeUnpackedArray::ToByteString: a byte array holds integral "
        "elements");
  }
  std::string out;
  out.reserve(Count());
  const IntegralShape shape = byte_type->Shape();
  for (std::size_t i = 0; i < Count(); ++i) {
    const LoadedWords element = byte_type->Load(ElementAt(i));
    out.push_back(
        static_cast<char>(
            ToInt64(element.Read(), shape.width, shape.signedness) & 0xFF));
  }
  return String{std::move(out)};
}

auto RuntimeUnpackedArray::ElementType() const -> const ValueType& {
  return Installed().Element().Type();
}

auto RuntimeUnpackedArray::ElementDefault() const -> const void* {
  return Installed().Element().Default();
}

auto RuntimeUnpackedArray::Count() const -> std::size_t {
  return core_.has_value() ? core_->Count() : 0;
}

auto RuntimeUnpackedArray::Size() const -> Int {
  return Int::FromInt(static_cast<std::int64_t>(Count()));
}

auto RuntimeUnpackedArray::ElementAt(std::size_t position) const -> const
    void* {
  if (position >= Count()) {
    throw InternalError(
        "RuntimeUnpackedArray::ElementAt: the position is past the last");
  }
  return Installed().At(position);
}

auto RuntimeUnpackedArray::ElementAt(std::size_t position) -> void* {
  if (position >= Count()) {
    throw InternalError(
        "RuntimeUnpackedArray::ElementAt: the position is past the last");
  }
  return Installed().At(position);
}

auto RuntimeUnpackedArray::Element(std::optional<std::int64_t> position) const
    -> const void* {
  return Installed().ElementAt(position);
}

auto RuntimeUnpackedArray::ElementRef(
    std::optional<std::int64_t> position, Formation& formed) -> void* {
  return Installed().ElementRef(position, formed);
}

auto RuntimeUnpackedArray::SliceElements(
    std::optional<std::int64_t> start, std::int64_t count) const
    -> std::vector<const void*> {
  return Installed().SliceElements(start, SliceCount(count));
}

auto RuntimeUnpackedArray::Slice(
    std::optional<std::int64_t> start, std::int64_t count) const
    -> RuntimeUnpackedArray {
  return {ElementType(), ElementDefault(), SliceElements(start, count)};
}

auto RuntimeUnpackedArray::AssignSlice(
    std::optional<std::int64_t> start, std::int64_t count,
    std::span<const void* const> replacement) -> bool {
  return Installed().AssignSlice(start, SliceCount(count), replacement);
}

void RuntimeUnpackedArray::Permute(std::span<const std::size_t> order) {
  Installed().Permute(order);
}

// An array with no element type yet holds no elements, which is all a
// comparison with one can read.
auto RuntimeUnpackedArray::operator==(const RuntimeUnpackedArray& other) const
    -> FourStateBit {
  if (!core_.has_value() || !other.core_.has_value()) {
    return detail::ScalarOf(Count() == other.Count());
  }
  return detail::SequenceEqual(*core_, *other.core_);
}

auto RuntimeUnpackedArray::operator!=(const RuntimeUnpackedArray& other) const
    -> FourStateBit {
  return Inverted(*this == other);
}

auto RuntimeUnpackedArray::CaseEqual(const RuntimeUnpackedArray& other) const
    -> Bit {
  if (!core_.has_value() || !other.core_.has_value()) {
    return Bit::FromBool(Count() == other.Count());
  }
  return detail::SequenceCaseEqual(*core_, *other.core_);
}

auto RuntimeUnpackedArray::MergeConditional(
    const RuntimeUnpackedArray& other) const -> RuntimeUnpackedArray {
  return RuntimeUnpackedArray(Installed().MergeConditional(other.Installed()));
}

auto RuntimeUnpackedArray::ResolveTriState(
    const RuntimeUnpackedArray& other) const -> RuntimeUnpackedArray {
  return RuntimeUnpackedArray(
      Installed().Resolved(NetResolution::kTriState, other.Installed()));
}

auto RuntimeUnpackedArray::ResolveWiredAnd(
    const RuntimeUnpackedArray& other) const -> RuntimeUnpackedArray {
  return RuntimeUnpackedArray(
      Installed().Resolved(NetResolution::kWiredAnd, other.Installed()));
}

auto RuntimeUnpackedArray::ResolveWiredOr(
    const RuntimeUnpackedArray& other) const -> RuntimeUnpackedArray {
  return RuntimeUnpackedArray(
      Installed().Resolved(NetResolution::kWiredOr, other.Installed()));
}

auto RuntimeUnpackedArray::Dominating(const RuntimeUnpackedArray& weaker) const
    -> RuntimeUnpackedArray {
  return RuntimeUnpackedArray(Installed().Dominating(weaker.Installed()));
}

auto RuntimeUnpackedArray::FilledLike(
    const RuntimeUnpackedArray& prototype, const Logic& fill)
    -> RuntimeUnpackedArray {
  return RuntimeUnpackedArray(prototype.Installed().FilledLike(fill));
}

auto RuntimeUnpackedArray::IsBitIdentical(
    const RuntimeUnpackedArray& other) const -> bool {
  if (!core_.has_value() || !other.core_.has_value()) {
    return core_.has_value() == other.core_.has_value();
  }
  return detail::SequenceBitIdentical(*core_, *other.core_);
}

auto RuntimeUnpackedArray::HasUnknown() const -> bool {
  return core_.has_value() && detail::SequenceHasUnknown(*core_);
}

auto RuntimeUnpackedArray::IsUnknown() const -> Bit {
  return Bit::FromBool(HasUnknown());
}

auto RuntimeUnpackedArray::BitstreamWidth() const -> Int {
  return Int::FromInt(
      core_.has_value() ? detail::SequenceBitstreamWidth(*core_) : 0);
}

auto RuntimeUnpackedArray::CountBits(
    const ConstIntegralView& control_bits) const -> Int {
  return Int::FromInt(
      core_.has_value() ? detail::SequenceCountBits(*core_, control_bits) : 0);
}

auto RuntimeUnpackedArray::WriteToStream(
    Planes stream, std::uint64_t stream_width, std::uint64_t filled) const
    -> std::uint64_t {
  return Installed().WriteToStream(stream, stream_width, filled);
}

auto RuntimeUnpackedArray::ReadFromStream(
    ConstPlanes stream, std::uint64_t stream_width, std::uint64_t taken) const
    -> std::pair<RuntimeUnpackedArray, std::uint64_t> {
  auto [read, after] = Installed().ReadFromStream(stream, stream_width, taken);
  return {RuntimeUnpackedArray(std::move(read)), after};
}

}  // namespace lyra::value
