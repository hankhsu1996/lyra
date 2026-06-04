#include "lyra/runtime/io.hpp"

#include <functional>
#include <span>
#include <string_view>
#include <utility>
#include <variant>

#include "lyra/base/overloaded.hpp"
#include "lyra/runtime/runtime_services.hpp"
#include "lyra/runtime/stream_dispatcher.hpp"
#include "lyra/value/format.hpp"

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

void LyraSubmitStrobe(
    RuntimeServices& services, std::function<void()> print_action) {
  services.SubmitPostponed(std::move(print_action));
}

}  // namespace lyra::runtime
