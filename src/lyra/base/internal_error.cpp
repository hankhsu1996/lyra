#include "lyra/base/internal_error.hpp"

#include <string>
#include <string_view>
#include <utility>

namespace lyra {

namespace {

constexpr std::string_view kBugReportRequest =
    "note: this is a bug in Lyra; report it at "
    "https://github.com/hankhsu1996/lyra/issues";

}  // namespace

InternalError::InternalError(std::string message)
    : std::logic_error(
          std::move(message) + "\n" + std::string(kBugReportRequest)) {
}

InternalError::~InternalError() = default;

auto InternalError::Invariant() const -> std::string_view {
  std::string_view whole = what();
  whole.remove_suffix(kBugReportRequest.size() + 1);
  return whole;
}

auto BugReportRequest() -> std::string_view {
  return kBugReportRequest;
}

}  // namespace lyra
