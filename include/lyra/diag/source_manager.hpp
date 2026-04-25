#pragma once

#include <algorithm>
#include <cstdint>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace lyra::diag {

struct FileId {
  std::uint32_t value = 0;

  explicit operator bool() const {
    return value != 0;
  }
  auto operator==(const FileId&) const -> bool = default;
};

inline constexpr FileId kInvalidFileId{};

struct FileInfo {
  std::string path;
  std::string content;
  // Byte offset of the start of each line; line N is at line_starts[N-1].
  std::vector<std::uint32_t> line_starts;
};

class SourceManager {
 public:
  auto AddFile(std::string path, std::string content) -> FileId {
    const auto value = static_cast<std::uint32_t>(files_.size() + 1);
    auto starts = ComputeLineStarts(content);
    files_.push_back(
        FileInfo{
            .path = std::move(path),
            .content = std::move(content),
            .line_starts = std::move(starts),
        });
    return FileId{.value = value};
  }

  [[nodiscard]] auto GetFile(FileId id) const -> const FileInfo* {
    if (!id || id.value > files_.size()) {
      return nullptr;
    }
    return &files_[id.value - 1];
  }

  struct LineCol {
    std::uint32_t line = 0;
    std::uint32_t col = 0;
  };

  // 1-based line/col. {0,0} on unknown file.
  [[nodiscard]] auto OffsetToLineCol(FileId id, std::uint32_t offset) const
      -> LineCol {
    const FileInfo* file = GetFile(id);
    if (file == nullptr) {
      return {};
    }
    const auto& starts = file->line_starts;
    auto it = std::ranges::upper_bound(starts, offset);
    if (it == starts.begin()) {
      return {.line = 1, .col = 1};
    }
    const auto prev = it - 1;
    const auto line = static_cast<std::uint32_t>(prev - starts.begin()) + 1;
    return {.line = line, .col = offset - *prev + 1};
  }

 private:
  static auto ComputeLineStarts(std::string_view content)
      -> std::vector<std::uint32_t> {
    std::vector<std::uint32_t> starts;
    starts.push_back(0);
    for (std::uint32_t i = 0; i < content.size(); ++i) {
      if (content[i] == '\n') {
        starts.push_back(i + 1);
      }
    }
    return starts;
  }

  std::vector<FileInfo> files_;
};

}  // namespace lyra::diag
