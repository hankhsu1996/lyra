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
#include "lyra/value/basic_dynamic_array.hpp"
#include "lyra/value/element_policy.hpp"
#include "lyra/value/element_sequence.hpp"
#include "lyra/value/formation.hpp"
#include "lyra/value/library_value_types.hpp"
#include "lyra/value/net_resolution.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/packed_type.hpp"
#include "lyra/value/position.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/unpacked_array.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::value {

namespace {

// An array of byte elements of `element_type`, the first of them the bytes
// `bytes` holds and the rest the element default (LRM 5.9).
auto FromBytes(
    std::string_view bytes, const PackedType& element_type,
    const PackedArray& count) -> RuntimeUnpackedArray {
  const auto element_count = static_cast<std::size_t>(count.ToInt64());
  const PackedArray element_default{element_type};
  std::vector<PackedArray> elements;
  elements.reserve(element_count);
  for (std::size_t i = 0; i < element_count; ++i) {
    elements.push_back(
        i < bytes.size()
            ? PackedArray::FromInt(
                  static_cast<unsigned char>(bytes[i]), element_type)
            : element_default);
  }
  std::vector<const void*> items;
  items.reserve(elements.size());
  for (const PackedArray& element : elements) {
    items.push_back(&element);
  }
  return {lyra_rt_packed_value_type, &element_default, items};
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
    const String& text, const PackedType& element_type,
    const PackedArray& count) -> RuntimeUnpackedArray {
  return FromBytes(text.View(), element_type, count);
}

auto RuntimeUnpackedArray::FromPackedArray(
    const PackedArray& bits, const PackedType& element_type,
    const PackedArray& count) -> RuntimeUnpackedArray {
  return FromBytes(bits.ByteString(), element_type, count);
}

auto RuntimeUnpackedArray::ToByteString() const -> String {
  if (&ElementType() != &lyra_rt_packed_value_type) {
    throw InternalError(
        "RuntimeUnpackedArray::ToByteString: a byte array holds packed "
        "elements");
  }
  std::string out;
  out.reserve(Count());
  for (std::size_t i = 0; i < Count(); ++i) {
    const auto& byte = *static_cast<const PackedArray*>(ElementAt(i));
    out.push_back(static_cast<char>(byte.ToInt64() & 0xFF));
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

auto RuntimeUnpackedArray::Size() const -> PackedArray {
  return PackedArray::Int(static_cast<std::int32_t>(Count()));
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

auto RuntimeUnpackedArray::Element(const PackedArray& position) const -> const
    void* {
  return Installed().ElementAt(position);
}

auto RuntimeUnpackedArray::ElementRef(
    const PackedArray& position, Formation& formed) -> void* {
  return Installed().ElementRef(position, formed);
}

auto RuntimeUnpackedArray::SliceElements(
    const PackedArray& start, std::int64_t count) const
    -> std::vector<const void*> {
  return Installed().SliceElements(ReadPosition(start), SliceCount(count));
}

auto RuntimeUnpackedArray::Slice(const PackedArray& start, std::int64_t count)
    const -> RuntimeUnpackedArray {
  return {ElementType(), ElementDefault(), SliceElements(start, count)};
}

auto RuntimeUnpackedArray::AssignSlice(
    const PackedArray& start, std::int64_t count,
    std::span<const void* const> replacement) -> bool {
  return Installed().AssignSlice(
      ReadPosition(start), SliceCount(count), replacement);
}

void RuntimeUnpackedArray::Permute(std::span<const std::size_t> order) {
  Installed().Permute(order);
}

// An array with no element type yet holds no elements, which is all a
// comparison with one can read.
auto RuntimeUnpackedArray::operator==(const RuntimeUnpackedArray& other) const
    -> PackedArray {
  if (!core_.has_value() || !other.core_.has_value()) {
    return PackedArray::Bit(Count() == other.Count());
  }
  return detail::SequenceEqual(*core_, *other.core_);
}

auto RuntimeUnpackedArray::operator!=(const RuntimeUnpackedArray& other) const
    -> PackedArray {
  return !(*this == other);
}

auto RuntimeUnpackedArray::CaseEqual(const RuntimeUnpackedArray& other) const
    -> PackedArray {
  if (!core_.has_value() || !other.core_.has_value()) {
    return PackedArray::Bit(Count() == other.Count());
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
    const RuntimeUnpackedArray& prototype, const PackedArray& fill)
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

auto RuntimeUnpackedArray::IsUnknown() const -> PackedArray {
  return PackedArray::Bit(HasUnknown());
}

auto RuntimeUnpackedArray::BitstreamWidth() const -> PackedArray {
  return core_.has_value() ? detail::SequenceBitstreamWidth(*core_)
                           : PackedArray::Int(0);
}

auto RuntimeUnpackedArray::CountBits(const PackedArray& control_bits) const
    -> PackedArray {
  return core_.has_value() ? detail::SequenceCountBits(*core_, control_bits)
                           : PackedArray::Int(0);
}

auto RuntimeUnpackedArray::ToBitstream() const -> PackedArray {
  return Installed().ToBitstream();
}

auto RuntimeUnpackedArray::FromBitstream(
    const PackedArray& bits, const RuntimeUnpackedArray& prototype)
    -> RuntimeUnpackedArray {
  return RuntimeUnpackedArray(prototype.Installed().FromBitstream(bits));
}

}  // namespace lyra::value
