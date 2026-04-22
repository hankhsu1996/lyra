#include "lyra/runtime/process_meta.hpp"

#include <cstdint>
#include <cstring>
#include <format>
#include <span>
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

// NOLINTBEGIN(cppcoreguidelines-pro-bounds-pointer-arithmetic,cppcoreguidelines-avoid-c-arrays,cppcoreguidelines-pro-bounds-constant-array-index,cppcoreguidelines-pro-bounds-array-to-pointer-decay)
// Async-signal-safe utilities: must use only raw POSIX I/O, C arrays,
// and pointer arithmetic. No std:: containers or heap allocation.

void WriteStr(int fd, const char* s) {
  if (s == nullptr) return;
  auto len = strlen(s);
  static_cast<void>(write(fd, s, len));
}

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
  static_cast<void>(write(fd, buf + pos, 15 - pos));
}

// NOLINTEND(cppcoreguidelines-pro-bounds-pointer-arithmetic,cppcoreguidelines-avoid-c-arrays,cppcoreguidelines-pro-bounds-constant-array-index,cppcoreguidelines-pro-bounds-array-to-pointer-decay)

}  // namespace

ProcessMetaRegistry::ProcessMetaRegistry(
    const uint32_t* words, uint32_t count, const char* pool,
    uint32_t pool_size) {
  if (pool != nullptr && pool_size > 0) {
    auto pool_span = std::span(pool, pool_size);
    string_pool_.assign(pool_span.begin(), pool_span.end());
  }

  auto word_span =
      std::span(words, static_cast<size_t>(count) * process_meta_abi::kStride);
  metas_.reserve(count);
  for (uint32_t i = 0; i < count; ++i) {
    auto base = static_cast<size_t>(i) * process_meta_abi::kStride;

    uint32_t inst_off =
        word_span[base + process_meta_abi::kFieldInstancePathOff];
    uint32_t kind_packed = word_span[base + process_meta_abi::kFieldKindPacked];
    uint32_t file_off = word_span[base + process_meta_abi::kFieldFileStrOff];
    uint32_t line = word_span[base + process_meta_abi::kFieldLine];
    uint32_t col = word_span[base + process_meta_abi::kFieldCol];

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
  return &string_pool_[offset];
}

auto ProcessMetaRegistry::Format(uint32_t process_id) const -> std::string {
  if (process_id >= metas_.size()) {
    return std::format("<process {}>", process_id);
  }

  const auto& meta = metas_[process_id];
  std::string result = std::format("{} process", KindName(meta.kind));

  const char* inst = PoolString(meta.instance_path_str_off);
  if (*inst != '\0') {
    result += std::format(" in {}", inst);
  }

  if (meta.loc.line > 0) {
    const char* file = PoolString(meta.loc.file_str_off);
    if (*file != '\0') {
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
  if (*inst != '\0') {
    WriteStr(fd, " in ");
    WriteStr(fd, inst);
  }

  if (meta.loc.line > 0) {
    const char* file = PoolString(meta.loc.file_str_off);
    if (*file != '\0') {
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

void ProcessMetaRegistry::DumpSummary(OutputDispatcher& out) const {
  out.DrainSimOutputBuffer();
  out.WriteProtocolRecord(
      std::format(
          "__LYRA_PROCESS_META__: version={} count={}\n",
          process_meta_abi::kVersion, metas_.size()));

  for (uint32_t i = 0; i < metas_.size(); ++i) {
    out.WriteProtocolRecord(
        std::format("__LYRA_PROCESS_META__: process={} {}\n", i, Format(i)));
  }
}

}  // namespace lyra::runtime
