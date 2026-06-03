#include "lyra/runtime/sformat.hpp"

#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>

#include "lyra/base/overloaded.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

auto LyraSFormat(std::span<const value::PrintItem> items) -> value::String {
  std::string buf;
  for (const value::PrintItem& item : items) {
    std::visit(
        Overloaded{
            [&](const value::PrintLiteralItem& lit) {
              buf.append(std::string_view{lit.data, lit.size});
            },
            [&](const value::PrintValueItem& v) {
              buf.append(value::FormatValue(v.spec, v.value));
            },
        },
        item);
  }
  return value::String(std::move(buf));
}

}  // namespace lyra::runtime
