#include "lyra/runtime/reflection.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/value/any_value.hpp"
#include "lyra/value/bit_vec.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_value_type.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/library_value_types.hpp"
#include "lyra/value/object_ref.hpp"
#include "lyra/value/real.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::runtime {

namespace {

// The key of index type `type` a number names, read as a signed number of any
// width: the number converted into the type as an index is (LRM 7.8.4), by the
// conversion an integral value takes into another integral type (LRM 6.24.1).
auto KeyOfNumber(const value::ValueType& type, value::ConstIntegralView number)
    -> value::AnyValue {
  const value::IntegralValueType* integral = type.AsIntegral();
  if (integral == nullptr) {
    throw InternalError(
        "Selection::KeyAt: a number names an entry of an associative array "
        "indexed by no integral type -- please report this as a bug");
  }
  number.signedness = value::Signedness::kSigned;
  return value::AnyValue::Built(type, [&](void* bytes) {
    value::WrittenWords written(integral->Shape());
    value::Convert(written.MutableView(), number);
    written.StoreTo(bytes);
  });
}

// The key of index type `type` a text names, which only a `string` index is.
auto KeyOfText(const value::ValueType& type, const value::String& text)
    -> value::AnyValue {
  if (&type != &value::LibraryTypeOf<value::String>()) {
    throw InternalError(
        "Selection::KeyAt: a text names an entry of an associative array "
        "indexed by no string -- please report this as a bug");
  }
  return value::AnyValue::CopyOf(type, &text);
}

}  // namespace

auto Selection::StepAt(std::int64_t depth) const -> const Step& {
  if (depth < 0 || static_cast<std::size_t>(depth) >= steps_.size()) {
    throw InternalError(
        "Selection: the code reaching a part asks for a step past the "
        "selection's end -- please report this as a bug");
  }
  return steps_[static_cast<std::size_t>(depth)];
}

auto SelectedKey::operator==(const SelectedKey& other) const -> bool {
  return key.IsBitIdentical(other.key);
}

Selection::Selection(std::vector<Step> steps) : steps_(std::move(steps)) {
}

auto Selection::Depth() const -> std::int64_t {
  return static_cast<std::int64_t>(steps_.size());
}

auto Selection::Number(std::int64_t depth) const -> std::int64_t {
  const auto* number = std::get_if<std::int64_t>(&StepAt(depth));
  if (number == nullptr) {
    throw InternalError(
        "Selection::Number: the step is an entry's key, which the code "
        "reaching it asks for as a key -- please report this as a bug");
  }
  return *number;
}

auto Selection::KeyAt(std::int64_t depth, const value::ValueType& type) const
    -> value::AnyValue {
  const auto* selected = std::get_if<SelectedKey>(&StepAt(depth));
  if (selected == nullptr) {
    throw InternalError(
        "Selection::KeyAt: the step is a number, which the code reaching it "
        "asks for as a number -- please report this as a bug");
  }
  const value::AnyValue& key = selected->key;
  const value::ValueType& named = key.Type();
  if (&named == &type) return key;
  if (&named == &value::LibraryTypeOf<value::BitVec>()) {
    return KeyOfNumber(
        type, static_cast<const value::BitVec*>(key.Bytes())->View());
  }
  if (&named == &value::LibraryTypeOf<value::String>()) {
    return KeyOfText(type, *static_cast<const value::String*>(key.Bytes()));
  }
  throw InternalError(
      "Selection::KeyAt: an entry is named by a key of another index type -- "
      "please report this as a bug");
}

void SelectionSettler::SettleKey(
    std::int64_t depth, const void* key, const value::ValueType& type) {
  keys_.emplace_back(
      depth, SelectedKey{.key = value::AnyValue::CopyOf(type, key)});
}

void SelectionSettler::SettlePositions(std::int64_t before) {
  positions_ += before;
}

void SelectionSettler::SettleUnfixed() {
  fixed_ = false;
}

auto SelectionSettler::Settled(const Selection& named) const -> Selection {
  std::vector<Selection::Step> steps = named.Steps();
  for (const auto& [depth, key] : keys_) {
    if (depth < 0 || static_cast<std::size_t>(depth) >= steps.size()) {
      throw InternalError(
          "SelectionSettler: a key is settled past the selection's end -- "
          "please report this as a bug");
    }
    steps[static_cast<std::size_t>(depth)] = key;
  }
  return Selection(std::move(steps));
}

auto SelectionSettler::Fixed() const -> bool {
  return fixed_;
}

auto SelectionSettler::Positions() const -> std::int64_t {
  return positions_;
}

auto Selection::operator==(const Selection& other) const -> bool {
  return steps_ == other.steps_;
}

void ValueSerializer::SerializeBits(const value::ConstIntegralView& value) {
  Receive(Serialized{value});
}

void ValueSerializer::SerializeReal(double value) {
  Receive(Serialized{value});
}

void ValueSerializer::SerializeText(const value::String& value) {
  Receive(Serialized{&value});
}

void ValueSerializer::SerializeEvent(const value::ObjectRef& event) {
  Receive(Serialized{&event});
}

auto ValueSerializer::SerializeSize(std::int64_t size) -> bool {
  Receive(Serialized{SerializedSize{.count = size}});
  return WantsKeys();
}

void ValueSerializer::SerializeKey(
    const void* key, const value::ValueType& type) {
  Receive(Serialized{SerializedKey{.bytes = key, .type = &type}});
}

void ValueSerializer::Real(const value::Real& value) {
  SerializeReal(value.Value());
}

void ValueSerializer::Real(const value::ShortReal& value) {
  SerializeReal(static_cast<double>(value.Value()));
}

}  // namespace lyra::runtime
