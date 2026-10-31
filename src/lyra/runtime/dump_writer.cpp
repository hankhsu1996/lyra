#include "lyra/runtime/dump_writer.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <fstream>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/base/time.hpp"
#include "lyra/support/dump_kind.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::runtime {

DumpWriter::~DumpWriter() = default;

auto DumpReference(const abi::DumpEntry& entry) -> std::string {
  std::string reference = entry.identifier;
  if (const auto range = entry.Range()) {
    reference += std::format(" [{}:{}]", range->left, range->right);
  }
  return reference;
}

auto DumpBitCharacter(value::FourStateBit bit) -> char {
  switch (bit) {
    case value::FourStateBit::kZero:
      return '0';
    case value::FourStateBit::kOne:
      return '1';
    case value::FourStateBit::kHighImpedance:
      return 'z';
    case value::FourStateBit::kUnknown:
      return 'x';
  }
  throw InternalError("dump: unknown scalar");
}

namespace {

// What a vector's leftmost position is extended with to fill its width (LRM
// Table 21-8): a 0 or a 1 is extended with 0, an x with x and a z with z.
auto ExtendedWith(char leftmost) -> char {
  return leftmost == '1' ? '0' : leftmost;
}

// A vector's positions from the most significant, every one of them.
auto SpellWhole(const value::ConstIntegralView& bits) -> std::string {
  std::string spelled;
  spelled.reserve(bits.width);
  for (std::uint64_t position = bits.width; position-- > 0;) {
    spelled.push_back(DumpBitCharacter(value::ScalarAt(bits, position)));
  }
  return spelled;
}

// The same in the shortest form extending it back gives (LRM 21.7.2.2, Table
// 21-9).
auto SpellVector(const value::ConstIntegralView& bits) -> std::string {
  const std::string spelled = SpellWhole(bits);
  std::size_t first = 0;
  while (spelled.size() - first > 1 &&
         ExtendedWith(spelled[first + 1]) == spelled[first]) {
    ++first;
  }
  return spelled.substr(first);
}

// The keyword a section of values opens with (LRM 21.7.2.3).
auto CheckpointKeyword(DumpCheckpoint checkpoint) -> std::string_view {
  switch (checkpoint) {
    case DumpCheckpoint::kDumpvars:
      return "$dumpvars";
    case DumpCheckpoint::kDumpall:
      return "$dumpall";
    case DumpCheckpoint::kDumpon:
      return "$dumpon";
    case DumpCheckpoint::kDumpoff:
      return "$dumpoff";
  }
  throw InternalError("dump: unknown checkpoint");
}

// The keyword a section of port values opens with (LRM 21.7.4.1
// `simulation_keyword`).
auto PortCheckpointKeyword(DumpCheckpoint checkpoint) -> std::string_view {
  switch (checkpoint) {
    case DumpCheckpoint::kDumpvars:
      return "$dumpports";
    case DumpCheckpoint::kDumpall:
      return "$dumpportsall";
    case DumpCheckpoint::kDumpon:
      return "$dumpportson";
    case DumpCheckpoint::kDumpoff:
      return "$dumpportsoff";
  }
  throw InternalError("dump: unknown checkpoint");
}

// A dump file written as text: what is handed to it is held and written a
// large piece at a time, and its size is what is held and what is written.
class DumpTextFile {
 public:
  explicit DumpTextFile(const std::string& path)
      : file_(path, std::ios::out | std::ios::trunc | std::ios::binary),
        path_(path) {
    if (!file_.is_open()) {
      throw SimulationError("the dump file '{}' could not be opened", path);
    }
  }

  void Put(std::string_view text) {
    pending_ += text;
    DrainIfLarge();
  }

  void Put(char character) {
    pending_.push_back(character);
    DrainIfLarge();
  }

  void Flush() {
    Drain();
    file_.flush();
  }

  void Close() {
    Drain();
    file_.close();
  }

  void LimitTo(std::uint64_t bytes) {
    limit_ = bytes;
  }

  [[nodiscard]] auto ReachedLimit() const -> bool {
    return limit_.has_value() && written_ + pending_.size() >= *limit_;
  }

 private:
  void DrainIfLarge() {
    constexpr std::size_t kChunk = std::size_t{1} << 20;
    if (pending_.size() >= kChunk) {
      Drain();
    }
  }

  void Drain() {
    if (pending_.empty()) {
      return;
    }
    file_.write(pending_.data(), static_cast<std::streamsize>(pending_.size()));
    if (!file_.good()) {
      throw SimulationError("the dump file '{}' could not be written", path_);
    }
    written_ += pending_.size();
    pending_.clear();
  }

  std::ofstream file_;
  std::string path_;
  std::string pending_;
  std::uint64_t written_ = 0;
  std::optional<std::uint64_t> limit_;
};

// What the standard's format and its extended one share (LRM 21.7.2.1,
// 21.7.4.1): the keyword commands -- the header, the scopes, the time, a
// section's end and a comment -- and the file they are written to.
class VcdCommandWriter : public DumpWriter {
 public:
  explicit VcdCommandWriter(const std::string& path) : file_(path) {
    file_.Put("$version\n   Lyra\n$end\n");
  }

  void Timescale(std::int8_t precision_power) override {
    const value::TimeUnitSpelling timescale =
        value::TimeUnitSpellingOf(precision_power);
    file_.Put(
        std::format(
            "$timescale\n   {}{}\n$end\n", timescale.count, timescale.unit));
  }

  void Scope(support::DumpScopeKind kind, std::string_view name) override {
    file_.Put(
        std::format(
            "$scope {} {} $end\n", support::DumpScopeKindKeyword(kind), name));
  }

  void Upscope() override {
    file_.Put("$upscope $end\n");
  }

  void EndDefinitions() override {
    file_.Put("$enddefinitions $end\n");
  }

  void Time(SimTime time) override {
    file_.Put(std::format("#{}\n", time));
  }

  void EndCheckpoint() override {
    file_.Put("$end\n");
  }

  void Comment(std::string_view text) override {
    file_.Put(std::format("$comment\n   {}\n$end\n", text));
  }

  void Flush() override {
    file_.Flush();
  }

  void Close() override {
    file_.Close();
  }

  void LimitTo(std::uint64_t bytes) override {
    file_.LimitTo(bytes);
  }

  [[nodiscard]] auto ReachedLimit() const -> bool override {
    return file_.ReachedLimit();
  }

 protected:
  [[nodiscard]] auto File() -> DumpTextFile& {
    return file_;
  }

 private:
  DumpTextFile file_;
};

// The writer of the format the standard defines (LRM 21.7.2): text, one
// command or one change a line.
class VcdWriter final : public VcdCommandWriter {
 public:
  using VcdCommandWriter::VcdCommandWriter;

  void Var(DumpCode code, const abi::DumpEntry& entry) override {
    if (scalar_.size() <= code.value) {
      scalar_.resize(code.value + 1);
    }
    scalar_[code.value] = entry.width == 1;
    Declare(code, entry);
  }

  void Alias(DumpCode code, const abi::DumpEntry& entry) override {
    Declare(code, entry);
  }

  void Change(DumpCode code, const DumpValue& value) override {
    std::visit(
        Overloaded{
            [&](const value::ConstIntegralView& bits) { PutBits(code, bits); },
            [&](double real) {
              File().Put(std::format("r{:.16g} {}\n", real, Identifier(code)));
            },
            [&](const DumpTriggered&) {
              File().Put(std::format("1{}\n", Identifier(code)));
            },
            [&](const DumpUnknown&) {
              File().Put(
                  std::format(
                      "{}{}\n", scalar_.at(code.value) ? "x" : "bx ",
                      Identifier(code)));
            }},
        value);
  }

  void BeginCheckpoint(DumpCheckpoint checkpoint) override {
    File().Put(std::format("{}\n", CheckpointKeyword(checkpoint)));
  }

 private:
  // LRM 21.7.2.1: a code is spelled in the printable characters from `!` to
  // `~`, as many as the number needs.
  static auto Identifier(DumpCode code) -> std::string {
    constexpr std::uint32_t kDigits = '~' - '!' + 1;
    std::string spelled(1, static_cast<char>('!' + (code.value % kDigits)));
    for (std::uint32_t rest = code.value / kDigits; rest != 0;
         rest /= kDigits) {
      spelled.push_back(static_cast<char>('!' + (rest % kDigits)));
    }
    return spelled;
  }

  void Declare(DumpCode code, const abi::DumpEntry& entry) {
    File().Put(
        std::format(
            "$var {} {} {} {} $end\n",
            support::DumpVarKindKeyword(entry.Kind()), entry.width,
            Identifier(code), DumpReference(entry)));
  }

  // A scalar is its character and its code with nothing between them; a vector
  // is `b`, its positions from the most significant, a space and its code, in
  // the shortest form extending it back gives (LRM 21.7.2.2, Table 21-9).
  void PutBits(DumpCode code, const value::ConstIntegralView& bits) {
    if (bits.width == 1) {
      File().Put(DumpBitCharacter(value::ScalarAt(bits, 0)));
    } else {
      File().Put('b');
      File().Put(SpellVector(bits));
      File().Put(' ');
    }
    File().Put(Identifier(code));
    File().Put('\n');
  }

  // Whether each thing, by its code, is a scalar, which a value is written
  // beside with nothing between them.
  std::vector<bool> scalar_;
};

// The state character a port's position is written as (LRM 21.7.4.3.1), read
// by who drives it: the test fixture into an input, the device out of an
// output, and through an inout, whose driver is not told apart, in the
// characters of a port of unknown direction. Each string is indexed by the
// scalar's planes as a number: 0, 1, z, x.
auto PortStateCharacter(
    support::DumpPortDirection direction, value::FourStateBit bit) -> char {
  const auto planes = static_cast<std::size_t>(std::to_underlying(bit));
  switch (direction) {
    case support::DumpPortDirection::kInput:
      return std::string_view("DUZN")[planes];
    case support::DumpPortDirection::kOutput:
      return std::string_view("LHTX")[planes];
    case support::DumpPortDirection::kInOut:
      return std::string_view("01F?")[planes];
  }
  throw InternalError("dump: unknown port direction");
}

// The extended format a dump of ports is written in (LRM 21.7.4): each port a
// `p`, a state character a position, and the strength the value is driven at
// to 0 and to 1 (LRM 21.7.4.3), its code a decimal number after a `<`; a real
// is written as `%.16g` after an `r` (LRM 21.7.4.1). Strength is not carried
// by the values a dump reads, so a driven value is recorded as driven strongly
// toward the values it holds and not toward the others, and a three-stated
// one as driven toward neither.
class ExtendedVcdWriter final : public VcdCommandWriter {
 public:
  using VcdCommandWriter::VcdCommandWriter;

  // LRM 21.7.4.2: the size is the index range the declaration states, and
  // where it states none -- a single bit, a real, an int -- the port is
  // written as scalar, 1.
  void Var(DumpCode code, const abi::DumpEntry& entry) override {
    const std::optional<abi::DumpEntry::Connection> port = entry.Port();
    if (!port.has_value()) {
      throw InternalError("ExtendedVcdWriter: a dumped port has a direction");
    }
    if (ports_.size() <= code.value) {
      ports_.resize(code.value + 1);
    }
    ports_[code.value] = {
        .direction = port->direction, .width = UnknownWidth(entry)};
    std::string size = "1";
    if (const auto range = entry.Range()) {
      size = std::format("[{}:{}]", range->left, range->right);
    }
    File().Put(
        std::format(
            "$var port {} <{} {} $end\n", size, code.value, entry.identifier));
  }

  void Alias(DumpCode code, const abi::DumpEntry& entry) override {
    Var(code, entry);
  }

  void Time(SimTime time) override {
    time_ = time;
    VcdCommandWriter::Time(time);
  }

  void Change(DumpCode code, const DumpValue& value) override {
    const Port& port = ports_.at(code.value);
    std::visit(
        Overloaded{
            [&](const value::ConstIntegralView& bits) {
              std::string states;
              bool toward_0 = false;
              bool toward_1 = false;
              for (std::uint64_t position = bits.width; position-- > 0;) {
                const value::FourStateBit bit = value::ScalarAt(bits, position);
                // A 0 and an x are driven toward 0, a 1 and an x toward 1.
                toward_0 = toward_0 ||
                           value::ValueBitOf(bit) == value::UnknownBitOf(bit);
                toward_1 = toward_1 || value::ValueBitOf(bit);
                states.push_back(PortStateCharacter(port.direction, bit));
              }
              PutPort(code, states, toward_0, toward_1);
            },
            [&](double real) {
              File().Put(std::format("r{:.16g} <{}\n", real, code.value));
            },
            [&](const DumpTriggered&) {
              throw InternalError("ExtendedVcdWriter: a port is no event");
            },
            [&](const DumpUnknown&) {
              PutPort(
                  code,
                  std::string(
                      port.width,
                      PortStateCharacter(
                          port.direction, value::FourStateBit::kUnknown)),
                  true, true);
            }},
        value);
  }

  void BeginCheckpoint(DumpCheckpoint checkpoint) override {
    File().Put(std::format("{}\n", PortCheckpointKeyword(checkpoint)));
  }

  // LRM 21.7.3.6.1: the file ends naming the time it was closed at.
  void Close() override {
    File().Put(std::format("$vcdclose #{} $end\n", time_));
    VcdCommandWriter::Close();
  }

 private:
  // A port's direction, and how many positions an unknown value of it is
  // written with.
  struct Port {
    support::DumpPortDirection direction = support::DumpPortDirection::kInput;
    std::uint32_t width = 0;
  };

  // An unknown real has no position to spell, so it is spelled as one.
  static auto UnknownWidth(const abi::DumpEntry& entry) -> std::uint32_t {
    switch (support::DumpValueFormOf(entry.Kind())) {
      case support::DumpValueForm::kReal:
        return 1;
      case support::DumpValueForm::kTriggered:
        throw InternalError("ExtendedVcdWriter: a port is no event");
      case support::DumpValueForm::kBits:
        return entry.width;
    }
    throw InternalError("ExtendedVcdWriter: unknown value");
  }

  void PutPort(
      DumpCode code, std::string_view states, bool toward_0, bool toward_1) {
    constexpr int kStrong = 6;
    File().Put(
        std::format(
            "p{} {} {} <{}\n", states, toward_0 ? kStrong : 0,
            toward_1 ? kStrong : 0, code.value));
  }

  std::vector<Port> ports_;
  SimTime time_ = 0;
};

// Text a person reads: one line a change, `<time> <names> = <value>`, the
// time with its unit and a vector's every position from the most significant.
// A thing several names stand for is named by all of them.
class TextWriter final : public DumpWriter {
 public:
  explicit TextWriter(const std::string& path) : file_(path) {
  }

  void Timescale(std::int8_t precision_power) override {
    timescale_ = value::TimeUnitSpellingOf(precision_power);
  }

  void Scope(support::DumpScopeKind kind, std::string_view name) override {
    scopes_.emplace_back(name);
    file_.Put(
        std::format(
            "{} {}\n", support::DumpScopeKindKeyword(kind), ScopePath()));
  }

  void Upscope() override {
    scopes_.pop_back();
  }

  void Var(DumpCode code, const abi::DumpEntry& entry) override {
    Name(code, entry);
  }

  void Alias(DumpCode code, const abi::DumpEntry& entry) override {
    Name(code, entry);
  }

  void EndDefinitions() override {
  }

  void Time(SimTime time) override {
    time_ = time;
  }

  void Change(DumpCode code, const DumpValue& value) override {
    const std::string spelled = std::visit(
        Overloaded{
            [](const value::ConstIntegralView& bits) {
              return SpellWhole(bits);
            },
            [](double real) { return std::format("{:.16g}", real); },
            [](const DumpTriggered&) { return std::string("triggered"); },
            [](const DumpUnknown&) { return std::string("x"); }},
        value);
    file_.Put(
        std::format("{} {} = {}\n", Now(), names_.at(code.value), spelled));
  }

  void BeginCheckpoint(DumpCheckpoint checkpoint) override {
    file_.Put(std::format("{} {}\n", Now(), CheckpointKeyword(checkpoint)));
  }

  void EndCheckpoint() override {
  }

  void Comment(std::string_view text) override {
    file_.Put(std::format("{} {}\n", Now(), text));
  }

  void Flush() override {
    file_.Flush();
  }

  void Close() override {
    file_.Close();
  }

  void LimitTo(std::uint64_t bytes) override {
    file_.LimitTo(bytes);
  }

  [[nodiscard]] auto ReachedLimit() const -> bool override {
    return file_.ReachedLimit();
  }

 private:
  [[nodiscard]] auto Now() const -> std::string {
    return std::format("{}{}", time_ * timescale_.count, timescale_.unit);
  }

  [[nodiscard]] auto ScopePath() const -> std::string {
    std::string joined;
    for (const std::string& scope : scopes_) {
      if (!joined.empty()) {
        joined.push_back('.');
      }
      joined += scope;
    }
    return joined;
  }

  void Name(DumpCode code, const abi::DumpEntry& entry) {
    if (names_.size() <= code.value) {
      names_.resize(code.value + 1);
    }
    std::string& names = names_[code.value];
    if (!names.empty()) {
      names += ", ";
    }
    names += ScopePath();
    names.push_back('.');
    names += entry.identifier;
  }

  DumpTextFile file_;
  value::TimeUnitSpelling timescale_{.count = 1, .unit = "s"};
  std::vector<std::string> scopes_;
  std::vector<std::string> names_;
  SimTime time_ = 0;
};

}  // namespace

auto OpenDumpWriter(const std::string& path) -> std::unique_ptr<DumpWriter> {
  if (path.ends_with(".fst")) {
    return OpenFstWriter(path);
  }
  if (path.ends_with(".txt")) {
    return OpenTextWriter(path);
  }
  return OpenVcdWriter(path);
}

auto OpenTextWriter(const std::string& path) -> std::unique_ptr<DumpWriter> {
  return std::make_unique<TextWriter>(path);
}

auto OpenVcdWriter(const std::string& path) -> std::unique_ptr<DumpWriter> {
  return std::make_unique<VcdWriter>(path);
}

auto OpenExtendedVcdWriter(const std::string& path)
    -> std::unique_ptr<DumpWriter> {
  return std::make_unique<ExtendedVcdWriter>(path);
}

}  // namespace lyra::runtime
