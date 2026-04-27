#include "lyra/runtime/io.hpp"

#include <cstdint>
#include <string_view>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/engine.hpp"
#include "lyra/runtime/output_sink.hpp"

namespace lyra::runtime {

void LyraPrintStart(Engine& engine, PrintKind kind) {
  (void)engine;
  (void)kind;
}

void LyraPrintLiteral(Engine& engine, const char* data, std::uint32_t size) {
  engine.Output().Append(std::string_view{data, size});
}

void LyraPrintValue(
    Engine& engine, const FormatSpec* spec, const RuntimeValueView* value) {
  if (spec == nullptr || value == nullptr) {
    throw InternalError("LyraPrintValue: null payload");
  }
  engine.Output().Append(FormatValue(*spec, *value));
}

void LyraPrintEnd(Engine& engine, PrintKind print_kind) {
  const bool append_newline =
      print_kind == PrintKind::kDisplay || print_kind == PrintKind::kFDisplay;
  if (append_newline) {
    engine.Output().FinishRecord(true);
  }
}

}  // namespace lyra::runtime
