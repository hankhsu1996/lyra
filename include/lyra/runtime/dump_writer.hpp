#pragma once

#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <system_error>
#include <variant>

#include "lyra/abi/records.hpp"
#include "lyra/base/time.hpp"
#include "lyra/runtime/dump_kind.hpp"
#include "lyra/runtime/dump_scope_kind.hpp"
#include "lyra/support/data_object.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::runtime {

// The number a dump names one dumped thing by, from 0 upward in the order the
// things are declared. A format spells it as it likes: LRM 21.7.2.1 has VCD
// write it in the printable characters from `!` to `~`.
struct DumpCode {
  std::uint32_t value = 0;
};

// A port as an extended dump records it: the direction its values are read
// by and its position among the ports.
struct DumpedPort {
  DumpPortDirection direction;
  std::uint32_t position;
};

// One data object as a dump declares it (LRM 21.7.2.1 `$var`): the name it is
// written under, the keyword of its kind, its size, and the range a vector
// declares, written beside the name (LRM 21.7.2.3); and, where an extended
// dump declares it as a port of its scope that carries a value, the port's
// direction and position among the ports, which that dump reads its values by
// and records them in (LRM 21.7.3.1, 21.7.4.2), the name then being the
// port's.
struct DumpVarDeclaration {
  const char* identifier = nullptr;
  DumpVarKind kind = DumpVarKind::kWire;
  std::uint32_t width = 0;
  std::optional<support::Range> range;
  std::optional<DumpedPort> port;
};

// How a dump declares the data object `object` states (LRM 21.7.2.1, 21.7.5
// Table 21-11): an event by `event` and a real by `real` or `realtime`
// whatever holds it, a net by its net type, a `uwire` as a `wire` (LRM
// 21.7.2.3), and a variable by its type -- an `int`, an `integer` and an
// enumeration whose base is one as an `integer`, a `time` as a `time`, and
// every other integral one as a `reg` of all its bits. An event is declared
// one bit wide, and a real as wide as a double. None for an object of a type a
// dump records nothing of (LRM 21.7.2.1): an unpacked aggregate, a string, a
// handle and a container.
auto DumpVarDeclarationOf(const abi::DataObjectRecord& object)
    -> std::optional<DumpVarDeclaration>;

// How an extended dump declares the port `port` at `position` among its
// scope's ports (LRM 21.7.4.2): as `connects`, the object it connects, under
// the port's name, none for a port bound by reference, which carries no value
// across it, and for one connecting an event, which the format has no value
// for.
auto DumpPortDeclarationOf(
    const abi::DataObjectRecord& connects, const abi::DescribedPort& port,
    std::uint32_t position) -> std::optional<DumpVarDeclaration>;

// The name a declaration is written under, its declared range after it (LRM
// 21.7.2.3 `$var` reference).
auto DumpReference(const DumpVarDeclaration& variable) -> std::string;

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
  virtual void Scope(DumpScopeKind kind, std::string_view name) = 0;
  virtual void Upscope() = 0;
  virtual void Var(DumpCode code, const DumpVarDeclaration& variable) = 0;
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
  // is.
  virtual void Close() = 0;
  // Bounds the file at `bytes` (LRM 21.7.1.5): once it reaches them, nothing
  // past them is written, which the format records as it records a comment.
  virtual void LimitTo(std::uint64_t bytes) = 0;
  // Whether the file has reached the bound it was given.
  [[nodiscard]] virtual auto ReachedLimit() const -> bool = 0;
  // Why the system refused to write the file, the ending of it included,
  // after which nothing more is written; nothing while it has taken
  // everything.
  [[nodiscard]] virtual auto Refused() const -> std::error_code = 0;
};

// The writer for the file `path` names, by its extension: `.fst` is the FST
// format; `.txt` is text a person reads, one line a change naming the thing by
// its hierarchical name; and any other path is the VCD format the standard
// defines (LRM 21.7.2). Each answers nothing where the file cannot be opened:
// who asked for the dump says whose failure that is.
auto OpenDumpWriter(const std::string& path) -> std::unique_ptr<DumpWriter>;
auto OpenVcdWriter(const std::string& path) -> std::unique_ptr<DumpWriter>;
auto OpenFstWriter(const std::string& path) -> std::unique_ptr<DumpWriter>;
auto OpenTextWriter(const std::string& path) -> std::unique_ptr<DumpWriter>;
// The extended format a dump of ports is written in (LRM 21.7.4), whatever
// the file's name.
auto OpenExtendedVcdWriter(const std::string& path)
    -> std::unique_ptr<DumpWriter>;

}  // namespace lyra::runtime
