#include "lyra/trace/text_trace_sink.hpp"

#include <algorithm>
#include <cstdint>
#include <cstdio>
#include <format>
#include <string>
#include <string_view>
#include <type_traits>
#include <variant>

#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/output_sink.hpp"
#include "lyra/trace/trace_event.hpp"

namespace lyra::trace {

TextTraceSink::TextTraceSink(const runtime::TraceSignalMetaRegistry* meta)
    : meta_(meta), output_(nullptr), owns_file_(false) {
}

TextTraceSink::TextTraceSink(
    const runtime::TraceSignalMetaRegistry* meta, const std::string& path)
    : meta_(meta),
      output_(
          std::fopen(
              path.c_str(), "w")),  // NOLINT(cppcoreguidelines-owning-memory)
      owns_file_(true) {
  if (output_ == nullptr) {
    throw common::InternalError(
        "TextTraceSink", std::format("cannot open '{}' for writing", path));
  }
}

TextTraceSink::~TextTraceSink() {
  if (owns_file_ && output_ != nullptr) {
    std::fclose(output_);  // NOLINT(cppcoreguidelines-owning-memory)
  }
}

void TextTraceSink::OnEvent(const TraceEvent& event) {
  std::visit(
      [this](const auto& e) {
        using T = std::decay_t<decltype(e)>;
        if constexpr (std::is_same_v<T, TimeAdvance>) {
          HandleTimeAdvance(e);
        } else if constexpr (std::is_same_v<T, ValueChange>) {
          HandleValueChange(e);
        }
      },
      event);
}

void TextTraceSink::HandleTimeAdvance(const TimeAdvance& ta) {
  current_time_ = ta.time;
  current_delta_ = ta.delta;
}

namespace {

constexpr std::string_view kHexDigits = "0123456789abcdef";

// Render a packed snapshot as an SV-style literal.
// 1-bit: 1'b0 / 1'b1
// Wider: N'h<lowercase hex, MSB-first>
auto RenderPacked(const PackedSnapshot& snap, uint32_t bit_width)
    -> std::string {
  if (bit_width == 1) {
    uint8_t val = snap.bytes.empty() ? 0 : (snap.bytes.at(0) & 1);
    return std::format("1'b{}", val);
  }

  // Value bytes are in little-endian order. Render MSB-first.
  uint32_t value_size = std::min((bit_width + 7) / 8, snap.byte_size);

  std::string hex;
  bool leading = true;
  for (int i = static_cast<int>(value_size) - 1; i >= 0; --i) {
    uint8_t byte = (static_cast<size_t>(i) < snap.bytes.size())
                       ? snap.bytes.at(static_cast<size_t>(i))
                       : uint8_t{0};
    uint8_t hi = (byte >> 4) & 0x0F;
    uint8_t lo = byte & 0x0F;

    if (leading && hi == 0 && i > 0) {
      if (lo == 0) continue;
      hex += kHexDigits.at(lo);
      leading = false;
    } else {
      if (!leading || hi != 0 || i == 0) {
        hex += kHexDigits.at(hi);
        leading = false;
      }
      hex += kHexDigits.at(lo);
    }
  }

  if (hex.empty()) {
    hex = "0";
  }

  return std::format("{}'h{}", bit_width, hex);
}

}  // namespace

void TextTraceSink::HandleValueChange(const ValueChange& vc) {
  if (meta_ == nullptr || !meta_->IsPopulated()) return;
  if (vc.slot_id >= meta_->Count()) return;

  auto name = meta_->Name(vc.slot_id);
  const auto& signal_meta = meta_->Get(vc.slot_id);

  std::string value_str;
  if (const auto* packed = std::get_if<PackedSnapshot>(&vc.value)) {
    value_str = RenderPacked(*packed, signal_meta.bit_width);
  } else if (const auto* str = std::get_if<std::string>(&vc.value)) {
    value_str = std::format("\"{}\"", *str);
  }

  auto line = std::format(
      "t={} d={} {} = {}\n", current_time_, current_delta_, name, value_str);

  if (output_ != nullptr) {
    std::fwrite(line.data(), 1, line.size(), output_);
  } else {
    runtime::WriteOutput(line);
  }
}

}  // namespace lyra::trace
