#include "lyra/runtime/io.hpp"

#include <span>
#include <string_view>
#include <variant>

#include "lyra/base/overloaded.hpp"
#include "lyra/runtime/format.hpp"
#include "lyra/runtime/output_sink.hpp"
#include "lyra/runtime/runtime_services.hpp"

namespace lyra::runtime {

void LyraPrint(
    RuntimeServices& services, PrintKind kind,
    std::span<const PrintItem> items) {
  auto& output = services.Output();
  for (const PrintItem& item : items) {
    std::visit(
        Overloaded{
            [&](const PrintLiteralItem& lit) {
              output.Append(std::string_view{lit.data, lit.size});
            },
            [&](const PrintValueItem& v) {
              output.Append(FormatValue(v.spec, v.value));
            },
        },
        item);
  }

  const bool append_newline =
      kind == PrintKind::kDisplay || kind == PrintKind::kFDisplay;
  output.FinishRecord(append_newline);
}

}  // namespace lyra::runtime
