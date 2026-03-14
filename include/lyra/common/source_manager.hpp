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

  struct LineCol {
    uint32_t line = 0;
    uint32_t col = 0;
  };

  // Convert a byte offset within a file to line/col (1-based).
  // Returns {0,0} if the file is invalid.
  [[nodiscard]] auto OffsetToLineCol(FileId id, uint32_t offset) const
      -> LineCol {
    const FileInfo* file = GetFile(id);
    if (file == nullptr) {
      return {};
    }
    uint32_t line = 1;
    uint32_t col = 1;
    for (uint32_t i = 0; i < offset && i < file->content.size(); ++i) {
      if (file->content[i] == '\n') {
        ++line;
        col = 1;
      } else {
        ++col;
      }
    }
    return {.line = line, .col = col};
  }

 private:
  std::vector<FileInfo> files_;
};

}  // namespace lyra
