#include "lyra/runtime/process_meta.hpp"

#include <cstdint>
#include <cstring>
#include <format>
#include <string>
#include <string_view>
#include <unistd.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/output_sink.hpp"
#include "lyra/runtime/process_meta_abi.hpp"

namespace lyra::runtime {

namespace {

auto KindName(ProcessKind kind) -> std::string_view {
  switch (kind) {
    case ProcessKind::kInitial:
      return "initial";
    case ProcessKind::kAlways:
      return "always";
    case ProcessKind::kAlwaysComb:
      return "always_comb";
    case ProcessKind::kAlwaysFf:
      return "always_ff";
    case ProcessKind::kFinal:
      return "final";
    case ProcessKind::kConnection:
      return "connection";
  }
  return "unknown";
}

// Async-signal-safe: write a NUL-terminated C string to fd.
void WriteStr(int fd, const char* s) {
  if (s == nullptr) return;
  auto len = strlen(s);
  // NOLINTNEXTLINE(cppcoreguidelines-pro-type-vararg)
  static_cast<void>(write(fd, s, len));
}

// Async-signal-safe: write a uint32 in decimal to fd.
void WriteU32(int fd, uint32_t val) {
  char buf[16];
  int pos = 15;
  buf[pos] = '\0';
  if (val == 0) {
    buf[--pos] = '0';
  } else {
    while (val > 0) {
      buf[--pos] = static_cast<char>('0' + (val % 10));
      val /= 10;
    }
  }
  // NOLINTNEXTLINE(cppcoreguidelines-pro-type-vararg)
  static_cast<void>(write(fd, buf + pos, 15 - pos));
}

}  // namespace

ProcessMetaRegistry::ProcessMetaRegistry(
    const uint32_t* words, uint32_t count, const char* pool,
    uint32_t pool_size) {
  if (pool != nullptr && pool_size > 0) {
    string_pool_.assign(pool, pool + pool_size);
  }

  metas_.reserve(count);
  auto total_words = static_cast<size_t>(count) * process_meta_abi::kStride;
  for (uint32_t i = 0; i < count; ++i) {
    auto base = static_cast<size_t>(i) * process_meta_abi::kStride;
    if (base + process_meta_abi::kStride > total_words) {
      throw common::InternalError(
          "ProcessMetaRegistry", "word table truncated");
    }

    uint32_t inst_off = words[base + process_meta_abi::kFieldInstancePathOff];
    uint32_t kind_packed = words[base + process_meta_abi::kFieldKindPacked];
    uint32_t file_off = words[base + process_meta_abi::kFieldFileStrOff];
    uint32_t line = words[base + process_meta_abi::kFieldLine];
    uint32_t col = words[base + process_meta_abi::kFieldCol];

    auto kind = static_cast<ProcessKind>(kind_packed & 0xFF);

    metas_.push_back(
        ProcessMeta{
            .instance_path_str_off = inst_off,
            .kind = kind,
            .loc =
                SvLoc{
                    .file_str_off = file_off,
                    .line = line,
                    .col = col,
                },
        });
  }
}

auto ProcessMetaRegistry::Size() const -> uint32_t {
  return static_cast<uint32_t>(metas_.size());
}

auto ProcessMetaRegistry::IsPopulated() const -> bool {
  return !metas_.empty();
}

auto ProcessMetaRegistry::Get(uint32_t process_id) const -> const ProcessMeta& {
  if (process_id >= metas_.size()) {
    throw common::InternalError(
        "ProcessMetaRegistry::Get",
        std::format(
            "process_id {} out of range (size {})", process_id, metas_.size()));
  }
  return metas_[process_id];
}

auto ProcessMetaRegistry::GetPoolString(uint32_t offset) const -> const char* {
  return PoolString(offset);
}

auto ProcessMetaRegistry::PoolString(uint32_t offset) const -> const char* {
  if (string_pool_.empty() || offset >= string_pool_.size()) {
    return "";
  }
  return string_pool_.data() + offset;
}

auto ProcessMetaRegistry::Format(uint32_t process_id) const -> std::string {
  if (process_id >= metas_.size()) {
    return std::format("<process {}>", process_id);
  }

  const auto& meta = metas_[process_id];
  std::string result = std::format("{} process", KindName(meta.kind));

  const char* inst = PoolString(meta.instance_path_str_off);
  if (inst[0] != '\0') {
    result += std::format(" in {}", inst);
  }

  if (meta.loc.line > 0) {
    const char* file = PoolString(meta.loc.file_str_off);
    if (file[0] != '\0') {
      result += std::format(" ({}:{}:{})", file, meta.loc.line, meta.loc.col);
    }
  }

  return result;
}

void ProcessMetaRegistry::WriteAsyncSignalSafe(
    int fd, uint32_t process_id) const {
  if (process_id >= metas_.size()) {
    WriteStr(fd, "<process ");
    WriteU32(fd, process_id);
    WriteStr(fd, ">");
    return;
  }

  const auto& meta = metas_[process_id];

  // Kind name (safe, string literal)
  switch (meta.kind) {
    case ProcessKind::kInitial:
      WriteStr(fd, "initial");
      break;
    case ProcessKind::kAlways:
      WriteStr(fd, "always");
      break;
    case ProcessKind::kAlwaysComb:
      WriteStr(fd, "always_comb");
      break;
    case ProcessKind::kAlwaysFf:
      WriteStr(fd, "always_ff");
      break;
    case ProcessKind::kFinal:
      WriteStr(fd, "final");
      break;
    case ProcessKind::kConnection:
      WriteStr(fd, "connection");
      break;
  }
  WriteStr(fd, " process");

  const char* inst = PoolString(meta.instance_path_str_off);
  if (inst[0] != '\0') {
    WriteStr(fd, " in ");
    WriteStr(fd, inst);
  }

  if (meta.loc.line > 0) {
    const char* file = PoolString(meta.loc.file_str_off);
    if (file[0] != '\0') {
      WriteStr(fd, " (");
      WriteStr(fd, file);
      WriteStr(fd, ":");
      WriteU32(fd, meta.loc.line);
      WriteStr(fd, ":");
      WriteU32(fd, meta.loc.col);
      WriteStr(fd, ")");
    }
  }
}

void ProcessMetaRegistry::DumpSummary() const {
  WriteOutput(
      std::format(
          "__LYRA_PROCESS_META__: version={} count={}\n",
          process_meta_abi::kVersion, metas_.size()));

  for (uint32_t i = 0; i < metas_.size(); ++i) {
    WriteOutput(
        std::format("__LYRA_PROCESS_META__: process={} {}\n", i, Format(i)));
  }
}

}  // namespace lyra::runtime
