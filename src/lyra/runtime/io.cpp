#include "lyra/runtime/io.hpp"

#include <span>
#include <string_view>
#include <variant>

#include "lyra/base/overloaded.hpp"
#include "lyra/runtime/output_sink.hpp"
#include "lyra/runtime/runtime_services.hpp"
#include "lyra/value/format.hpp"

namespace lyra::runtime {

void LyraPrint(
    RuntimeServices& services, value::PrintKind kind,
    std::span<const value::PrintItem> items) {
  auto& output = services.Output();
  for (const value::PrintItem& item : items) {
    std::visit(
        Overloaded{
            [&](const value::PrintLiteralItem& lit) {
              output.Append(std::string_view{lit.data, lit.size});
            },
            [&](const value::PrintValueItem& v) {
              output.Append(value::FormatValue(v.spec, v.value));
            },
        },
        item);
  }

  const bool append_newline =
      kind == value::PrintKind::kDisplay || kind == value::PrintKind::kFDisplay;
  output.FinishRecord(append_newline);
}

}  // namespace lyra::runtime
