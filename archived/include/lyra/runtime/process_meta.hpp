#pragma once

#include <cstdint>
#include <string>
#include <vector>

namespace lyra::runtime {

class OutputDispatcher;

enum class ProcessKind : uint8_t {
  kInitial,
  kAlways,
  kAlwaysComb,
  kAlwaysFf,
  kFinal,
  kConnection,
};

struct SvLoc {
  uint32_t file_str_off = 0;  // Offset into string pool (0 = unknown)
  uint32_t line = 0;          // 1-based (0 = unknown)
  uint32_t col = 0;           // 1-based (0 = unknown)
};

struct ProcessMeta {
  uint32_t instance_path_str_off = 0;  // Offset into string pool
  ProcessKind kind = ProcessKind::kInitial;
  uint8_t reserved0 = 0;
  uint16_t reserved1 = 0;
  SvLoc loc;
};

// Dense registry of process metadata, indexed by process_id.
// Owns stable storage (string pool bytes) for signal-safe access.
class ProcessMetaRegistry {
 public:
  ProcessMetaRegistry() = default;

  // Parse from flat uint32_t word table + separate string pool.
  ProcessMetaRegistry(
      const uint32_t* words, uint32_t count, const char* pool,
      uint32_t pool_size);

  [[nodiscard]] auto Size() const -> uint32_t;
  [[nodiscard]] auto IsPopulated() const -> bool;

  [[nodiscard]] auto Get(uint32_t process_id) const -> const ProcessMeta&;

  // Normal formatting (allocates): "always_comb process in top.u_core
  // (test.sv:42:5)"
  [[nodiscard]] auto Format(uint32_t process_id) const -> std::string;

  // Signal-safe: writes one line to fd via write(2). No allocation.
  void WriteAsyncSignalSafe(int fd, uint32_t process_id) const;

  // Debug dump via output dispatcher.
  void DumpSummary(OutputDispatcher& out) const;

  // Access a NUL-terminated string from the pool by offset.
  // Returns "" for offset 0 or out-of-range.
  [[nodiscard]] auto GetPoolString(uint32_t offset) const -> const char*;

 private:
  [[nodiscard]] auto PoolString(uint32_t offset) const -> const char*;

  std::vector<ProcessMeta> metas_;
  std::vector<char> string_pool_;
};

}  // namespace lyra::runtime
