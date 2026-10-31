#pragma once

#include <cstdint>
#include <memory>
#include <string>
#include <string_view>
#include <variant>

#include "lyra/abi/records.hpp"
#include "lyra/base/time.hpp"
#include "lyra/support/dump_kind.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::runtime {

// The number a dump names one dumped thing by, from 0 upward in the order the
// things are declared. A format spells it as it likes: LRM 21.7.2.1 has VCD
// write it in the printable characters from `!` to `~`.
struct DumpCode {
  std::uint32_t value = 0;
};

// The name a declaration is written under, its declared range after it (LRM
// 21.7.2.3 `$var` reference).
auto DumpReference(const abi::DumpEntry& entry) -> std::string;

// The character LRM 21.7.2.1 `value` writes a scalar as.
auto DumpBitCharacter(value::FourStateBit bit) -> char;

// A named event was triggered in the time step (LRM 21.7.2.2): only that it
// happened is recorded.
struct DumpTriggered {};

// The value a dump records of every thing when it is switched off (LRM
// 21.7.1.3): unknown, whatever the thing holds, so each format spells it as
// its own kind of thing allows.
struct DumpUnknown {};

// What a change records: an integral value as its planes, a real, that an
// event was triggered, or that the value is unknown.
using DumpValue =
    std::variant<value::ConstIntegralView, double, DumpTriggered, DumpUnknown>;

// Which section of values a checkpoint writes (LRM 21.7.2.3): the first values
// `$dumpvars` starts with, the current values `$dumpall` and `$dumpon` record,
// and the unknown value `$dumpoff` records for each.
enum class DumpCheckpoint : std::uint8_t {
  kDumpvars,
  kDumpall,
  kDumpon,
  kDumpoff,
};

// A file format one dump is written in. It is handed the dump in the order the
// standard lays a VCD file out (LRM 21.7.2.1): the definitions, scope by scope,
// then the value changes time step by time step. Every call reaches it on the
// one thread that writes the file, so a format holds no lock of its own.
class DumpWriter {
 public:
  DumpWriter() = default;
  DumpWriter(const DumpWriter&) = delete;
  auto operator=(const DumpWriter&) -> DumpWriter& = delete;
  DumpWriter(DumpWriter&&) = delete;
  auto operator=(DumpWriter&&) -> DumpWriter& = delete;
  virtual ~DumpWriter();

  // The design's time precision as a power of ten (LRM 3.14.3), which every
  // time the dump states is a count of.
  virtual void Timescale(std::int8_t precision_power) = 0;
  virtual void Scope(support::DumpScopeKind kind, std::string_view name) = 0;
  virtual void Upscope() = 0;
  virtual void Var(DumpCode code, const abi::DumpEntry& entry) = 0;
  // Several names may stand for one thing (LRM 21.7.2.3 `$var` b): a port and
  // what it is connected to.
  virtual void Alias(DumpCode code, const abi::DumpEntry& entry) = 0;
  virtual void EndDefinitions() = 0;
  virtual void Time(SimTime time) = 0;
  virtual void Change(DumpCode code, const DumpValue& value) = 0;
  virtual void BeginCheckpoint(DumpCheckpoint checkpoint) = 0;
  virtual void EndCheckpoint() = 0;
  virtual void Comment(std::string_view text) = 0;
  // Hands what is written so far to the file, so a program reading the file
  // while the run goes on sees it (LRM 21.7.1.6).
  virtual void Flush() = 0;
  // Ends the file: what is written so far is handed to it, and nothing more
  // is. A failure to write is reported here rather than where the writer ends,
  // which reports nothing.
  virtual void Close() = 0;
  // Bounds the file at `bytes` (LRM 21.7.1.5): once it reaches them, nothing
  // past them is written, which the format records as it records a comment.
  virtual void LimitTo(std::uint64_t bytes) = 0;
  // Whether the file has reached the bound it was given.
  [[nodiscard]] virtual auto ReachedLimit() const -> bool = 0;
};

// The writer for the file `path` names, by its extension: `.fst` is the FST
// format; `.txt` is text a person reads, one line a change naming the thing by
// its hierarchical name; and any other path is the VCD format the standard
// defines (LRM 21.7.2).
auto OpenDumpWriter(const std::string& path) -> std::unique_ptr<DumpWriter>;
auto OpenVcdWriter(const std::string& path) -> std::unique_ptr<DumpWriter>;
auto OpenFstWriter(const std::string& path) -> std::unique_ptr<DumpWriter>;
auto OpenTextWriter(const std::string& path) -> std::unique_ptr<DumpWriter>;
// The extended format a dump of ports is written in (LRM 21.7.4), whatever
// the file's name.
auto OpenExtendedVcdWriter(const std::string& path)
    -> std::unique_ptr<DumpWriter>;

}  // namespace lyra::runtime
