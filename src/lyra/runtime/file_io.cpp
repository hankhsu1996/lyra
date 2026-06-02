#include "lyra/runtime/file_io.hpp"

#include <cstdint>
#include <fstream>
#include <iostream>
#include <optional>
#include <ostream>
#include <span>
#include <string>
#include <string_view>
#include <variant>

#include "lyra/base/overloaded.hpp"
#include "lyra/runtime/file_table.hpp"
#include "lyra/runtime/runtime_services.hpp"
#include "lyra/runtime/stream_dispatcher.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

namespace {

auto FormatItemsToString(std::span<const value::PrintItem> items)
    -> std::string {
  std::string out;
  for (const value::PrintItem& item : items) {
    std::visit(
        Overloaded{
            [&](const value::PrintLiteralItem& lit) {
              out.append(std::string_view{lit.data, lit.size});
            },
            [&](const value::PrintValueItem& v) {
              out.append(value::FormatValue(v.spec, v.value));
            },
        },
        item);
  }
  return out;
}

void WriteToFile(
    std::ostream& sink, std::string_view body, bool append_newline) {
  sink.write(body.data(), static_cast<std::streamsize>(body.size()));
  if (append_newline) sink.put('\n');
}

void WriteToStream(
    StreamDispatcher& stream, std::string_view body, bool append_newline) {
  stream.Append(body);
  stream.FinishRecord(append_newline);
}

}  // namespace

auto LyraFOpen(
    RuntimeServices& services, const value::String& name,
    std::optional<value::String> mode) -> value::PackedArray {
  std::optional<std::string_view> mode_view = std::nullopt;
  if (mode.has_value()) mode_view = mode->View();
  return value::PackedArray::Int(services.Files().Open(name.View(), mode_view));
}

void LyraFClose(
    RuntimeServices& services, const value::PackedArray& descriptor) {
  services.Files().Close(static_cast<std::int32_t>(descriptor.ToInt64()));
}

void LyraFPrint(
    RuntimeServices& services, value::PrintKind kind,
    const value::PackedArray& descriptor_pa,
    std::span<const value::PrintItem> items) {
  const auto descriptor = static_cast<std::int32_t>(descriptor_pa.ToInt64());
  if (descriptor == 0) return;
  const bool append_newline =
      kind == value::PrintKind::kDisplay || kind == value::PrintKind::kFDisplay;
  const std::string body = FormatItemsToString(items);
  const auto raw = static_cast<std::uint32_t>(descriptor);

  // FD path: bit 31 set => single descriptor. Stdio sentinels route through
  // services.Stream() / std::cerr so test-harness ordering with $display is
  // preserved; other FDs go to their owned fstream.
  if ((raw & (1U << 31U)) != 0U) {
    if (descriptor == FileTable::kStdoutFd) {
      WriteToStream(services.Stream(), body, append_newline);
      return;
    }
    if (descriptor == FileTable::kStderrFd) {
      WriteToFile(std::cerr, body, append_newline);
      return;
    }
    std::fstream* sink = services.Files().Resolve(descriptor);
    if (sink == nullptr) return;
    WriteToFile(*sink, body, append_newline);
    return;
  }

  // MCD path: iterate bits 0..30 in ascending order. Bit 0 -> stream sink.
  // Bits 1..30 -> FileTable. Unset / unmapped bits silently no-op.
  for (std::uint32_t bit = 0; bit <= 30U; ++bit) {
    if ((raw & (1U << bit)) == 0U) continue;
    if (bit == 0U) {
      WriteToStream(services.Stream(), body, append_newline);
      continue;
    }
    const auto channel = static_cast<std::int32_t>(1U << bit);
    std::fstream* sink = services.Files().Resolve(channel);
    if (sink == nullptr) continue;
    WriteToFile(*sink, body, append_newline);
  }
}

}  // namespace lyra::runtime
