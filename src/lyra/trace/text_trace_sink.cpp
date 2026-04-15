#include "lyra/trace/text_trace_sink.hpp"

#include <cstdint>
#include <cstdio>
#include <format>
#include <string>
#include <string_view>
#include <type_traits>
#include <variant>

#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/instance_observability.hpp"
#include "lyra/runtime/output_sink.hpp"
#include "lyra/runtime/runtime_instance.hpp"
#include "lyra/trace/trace_event.hpp"

namespace lyra::trace {

TextTraceSink::TextTraceSink(
    const runtime::TraceSignalMetaRegistry* meta,
    runtime::OutputDispatcher* output_dispatcher)
    : meta_(meta), output_dispatcher_(output_dispatcher) {
}

TextTraceSink::TextTraceSink(
    const runtime::TraceSignalMetaRegistry* meta, const std::string& path)
    : meta_(meta), output_(std::fopen(path.c_str(), "w")) {
  if (output_ == nullptr) {
    throw common::InternalError(
        "TextTraceSink", std::format("cannot open '{}' for writing", path));
  }
}

TextTraceSink::~TextTraceSink() = default;

void TextTraceSink::OnEvent(const TraceEvent& event) {
  std::visit(
      [this](const auto& e) {
        using T = std::decay_t<decltype(e)>;
        if constexpr (std::is_same_v<T, TimeAdvance>) {
          HandleTimeAdvance(e);
        } else if constexpr (std::is_same_v<T, GlobalValueChange>) {
          HandleGlobalValueChange(e);
        } else if constexpr (std::is_same_v<T, LocalValueChange>) {
          HandleLocalValueChange(e);
        }
        // Ignore GlobalMemoryDirty and LocalMemoryDirty.
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

auto RenderTraceValue(const TraceValue& value, uint32_t bit_width)
    -> std::string {
  if (const auto* packed = std::get_if<PackedSnapshot>(&value)) {
    return RenderPacked(*packed, bit_width);
  }
  if (const auto* str = std::get_if<std::string>(&value)) {
    return std::format("\"{}\"", *str);
  }
  return "?";
}

}  // namespace

void TextTraceSink::HandleGlobalValueChange(const GlobalValueChange& vc) {
  if (meta_ == nullptr || !meta_->IsPopulated()) {
    throw common::InternalError(
        "TextTraceSink::HandleGlobalValueChange",
        "global trace event received without populated global trace metadata");
  }
  if (vc.signal_id.value >= meta_->Count()) {
    throw common::InternalError(
        "TextTraceSink::HandleGlobalValueChange",
        std::format(
            "global signal id {} out of range {}", vc.signal_id.value,
            meta_->Count()));
  }

  auto name = meta_->Name(vc.signal_id.value);
  const auto& signal_meta = meta_->Get(vc.signal_id.value);
  auto value_str = RenderTraceValue(vc.value, signal_meta.bit_width);

  auto line = std::format(
      "t={} d={} {} = {}\n", current_time_, current_delta_, name, value_str);

  if (output_) {
    std::fwrite(line.data(), 1, line.size(), output_.get());
  } else {
    output_dispatcher_->DrainSimOutputBuffer();
    output_dispatcher_->WriteProtocolRecord(line);
  }
}

void TextTraceSink::HandleLocalValueChange(const LocalValueChange& vc) {
  if (vc.instance == nullptr) {
    throw common::InternalError(
        "TextTraceSink::HandleLocalValueChange",
        "local trace event with null instance");
  }

  const auto* inst = vc.instance;
  const auto* layout = inst->observability.layout;
  if (layout == nullptr) {
    throw common::InternalError(
        "TextTraceSink::HandleLocalValueChange",
        std::format(
            "instance '{}' has no observability layout", inst->path_c_str));
  }

  if (vc.signal_id.value >= layout->trace_meta.size()) {
    throw common::InternalError(
        "TextTraceSink::HandleLocalValueChange",
        std::format(
            "instance '{}' local signal {} out of range {}", inst->path_c_str,
            vc.signal_id.value, layout->trace_meta.size()));
  }

  auto name =
      runtime::ComposeHierarchicalTraceName(*inst, vc.signal_id, *layout);
  const auto& trace_meta = layout->trace_meta[vc.signal_id.value];
  auto value_str = RenderTraceValue(vc.value, trace_meta.bit_width);

  auto line = std::format(
      "t={} d={} {} = {}\n", current_time_, current_delta_, name, value_str);

  if (output_) {
    std::fwrite(line.data(), 1, line.size(), output_.get());
  } else {
    output_dispatcher_->DrainSimOutputBuffer();
    output_dispatcher_->WriteProtocolRecord(line);
  }
}

}  // namespace lyra::trace
