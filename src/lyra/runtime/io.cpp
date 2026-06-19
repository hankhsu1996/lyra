#include "lyra/runtime/io.hpp"

#include <functional>
#include <span>
#include <string_view>
#include <utility>
#include <variant>

#include "lyra/base/overloaded.hpp"
#include "lyra/runtime/file_io.hpp"
#include "lyra/runtime/runtime_services.hpp"
#include "lyra/runtime/stream_dispatcher.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::runtime {

void LyraPrint(
    RuntimeServices& services, value::PrintKind kind,
    std::span<const value::PrintItem> items) {
  auto& stream = services.Stream();
  for (const value::PrintItem& item : items) {
    std::visit(
        Overloaded{
            [&](const value::PrintLiteralItem& lit) {
              stream.Append(std::string_view{lit.data, lit.size});
            },
            [&](const value::PrintValueItem& v) {
              stream.Append(
                  value::Format(
                      v.spec, v.arg,
                      value::FormatContext{
                          .time_format = &services.TimeFormat()}));
            },
        },
        item);
  }

  const bool append_newline =
      kind == value::PrintKind::kDisplay || kind == value::PrintKind::kFDisplay;
  stream.FinishRecord(append_newline);
}

void LyraDisplay(
    RuntimeServices& services, std::span<const value::PrintItem> items) {
  LyraPrint(services, value::PrintKind::kDisplay, items);
}

void LyraWrite(
    RuntimeServices& services, std::span<const value::PrintItem> items) {
  LyraPrint(services, value::PrintKind::kWrite, items);
}

void LyraFDisplay(
    RuntimeServices& services, const value::PackedArray& descriptor,
    std::span<const value::PrintItem> items) {
  LyraFPrint(services, value::PrintKind::kFDisplay, descriptor, items);
}

void LyraFWrite(
    RuntimeServices& services, const value::PackedArray& descriptor,
    std::span<const value::PrintItem> items) {
  LyraFPrint(services, value::PrintKind::kFWrite, descriptor, items);
}

void LyraSubmitStrobe(
    RuntimeServices& services, std::function<void()> print_action) {
  services.SubmitPostponed(std::move(print_action));
}

}  // namespace lyra::runtime
