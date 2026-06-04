#include "lyra/runtime/sformat.hpp"

#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>

#include "lyra/base/overloaded.hpp"
#include "lyra/runtime/runtime_services.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

auto LyraSFormat(
    RuntimeServices& services, std::span<const value::PrintItem> items)
    -> value::String {
  std::string buf;
  for (const value::PrintItem& item : items) {
    std::visit(
        Overloaded{
            [&](const value::PrintLiteralItem& lit) {
              buf.append(std::string_view{lit.data, lit.size});
            },
            [&](const value::PrintValueItem& v) {
              buf.append(
                  value::Format(
                      v.spec, v.arg,
                      value::FormatContext{
                          .time_format = &services.TimeFormat()}));
            },
        },
        item);
  }
  return value::String(std::move(buf));
}

}  // namespace lyra::runtime
