#include "lyra/runtime/io.hpp"

#include <span>
#include <string_view>
#include <variant>

#include "lyra/base/overloaded.hpp"
#include "lyra/runtime/engine.hpp"
#include "lyra/runtime/format.hpp"
#include "lyra/runtime/output_sink.hpp"

namespace lyra::runtime {

void LyraPrint(
    Engine& engine, PrintKind kind, std::span<const PrintItem> items) {
  for (const PrintItem& item : items) {
    std::visit(
        Overloaded{
            [&](const PrintLiteralItem& lit) {
              engine.Output().Append(std::string_view{lit.data, lit.size});
            },
            [&](const PrintValueItem& v) {
              engine.Output().Append(FormatValue(v.spec, v.value));
            },
        },
        item);
  }

  const bool append_newline =
      kind == PrintKind::kDisplay || kind == PrintKind::kFDisplay;
  engine.Output().FinishRecord(append_newline);
}

}  // namespace lyra::runtime
