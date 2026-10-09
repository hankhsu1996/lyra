#include "lyra/driver/claimed_file.hpp"

#include <cerrno>
#include <filesystem>
#include <format>
#include <fstream>
#include <string_view>
#include <system_error>
#include <utility>

#include "lyra/diag/diag_code.hpp"
#include "lyra/driver/artifact_store.hpp"
#include "lyra/driver/signals.hpp"

namespace lyra::driver {

namespace {

auto CannotWrite(const std::filesystem::path& destination, std::string_view why)
    -> diag::Diagnostic {
  return diag::Make(
      diag::DiagCode::kHostIoError,
      std::format("cannot write '{}': {}", destination.string(), why));
}

}  // namespace

auto ClaimedFile::Claim(const std::filesystem::path& destination)
    -> diag::Result<ClaimedFile> {
  std::error_code ec;
  if (std::filesystem::is_directory(destination, ec)) {
    return std::unexpected(CannotWrite(
        destination, "it is a directory, and what goes there is one file"));
  }
  if (destination.has_parent_path()) {
    std::filesystem::create_directories(destination.parent_path(), ec);
    if (ec) {
      return std::unexpected(CannotWrite(destination, ec.message()));
    }
  }
  std::filesystem::path working = TemporaryBeside(destination);
  errno = 0;
  if (!std::ofstream(working)) {
    return std::unexpected(
        CannotWrite(destination, std::generic_category().message(errno)));
  }
  RemoveOnSignal(working);
  return ClaimedFile(destination, std::move(working));
}

auto ClaimedFile::operator=(ClaimedFile&& other) noexcept -> ClaimedFile& {
  if (this != &other) {
    Drop();
    destination_ = std::move(other.destination_);
    working_ = std::exchange(other.working_, {});
  }
  return *this;
}

ClaimedFile::~ClaimedFile() {
  Drop();
}

auto ClaimedFile::Finish() -> diag::Result<void> {
  std::error_code ec;
  std::filesystem::rename(working_, destination_, ec);
  if (ec) {
    return std::unexpected(CannotWrite(destination_, ec.message()));
  }
  DontRemoveOnSignal(working_);
  working_.clear();
  return {};
}

void ClaimedFile::Drop() noexcept {
  if (working_.empty()) {
    return;
  }
  std::error_code ignored;
  std::filesystem::remove(working_, ignored);
  DontRemoveOnSignal(working_);
  working_.clear();
}

}  // namespace lyra::driver
