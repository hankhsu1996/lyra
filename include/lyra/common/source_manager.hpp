#pragma once

#include <cstdint>
#include <string>
#include <vector>

namespace lyra {

struct FileId {
  uint32_t value = 0;

  explicit operator bool() const {
    return value != 0;
  }
  auto operator==(const FileId&) const -> bool = default;
};

inline constexpr FileId kInvalidFileId{};

struct FileInfo {
  std::string path;
  std::string content;
};

class SourceManager {
 public:
  auto AddFile(std::string path, std::string content) -> FileId {
    auto value = static_cast<uint32_t>(files_.size() + 1);
    files_.push_back(
        FileInfo{.path = std::move(path), .content = std::move(content)});
    return FileId{.value = value};
  }

  [[nodiscard]] auto GetFile(FileId id) const -> const FileInfo* {
    if (!id || id.value > files_.size()) {
      return nullptr;
    }
    return &files_[id.value - 1];
  }

 private:
  std::vector<FileInfo> files_;
};

}  // namespace lyra
