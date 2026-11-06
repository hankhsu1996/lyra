#pragma once

#include <cerrno>
#include <system_error>

namespace lyra::base {

// What the system answered the last call that failed, as an error code.
inline auto ErrnoAsErrorCode() noexcept -> std::error_code {
  return {errno, std::system_category()};
}

}  // namespace lyra::base
