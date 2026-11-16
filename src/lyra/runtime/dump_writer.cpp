#include "lyra/runtime/dump_writer.hpp"

#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <format>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <system_error>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/errno_as_error_code.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/base/time.hpp"
#include "lyra/runtime/dump_kind.hpp"
#include "lyra/runtime/file.hpp"
#include "lyra/support/data_object.hpp"
#include "lyra/support/port_direction.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::runtime {

DumpWriter::~DumpWriter() = default;

namespace {

// The keyword a variable of `type` is declared by (LRM 21.7.5 Table 21-11),
// an enumeration's being its base's.
auto VariableKeyword(const abi::Typespec& type) -> DumpVarKind {
  switch (type.Kind()) {
    case support::TypespecKind::kEvent:
      return DumpVarKind::kEvent;
    case support::TypespecKind::kReal:
    case support::TypespecKind::kShortReal:
      return DumpVarKind::kReal;
    case support::TypespecKind::kRealtime:
      return DumpVarKind::kRealtime;
    case support::TypespecKind::kInt:
    case support::TypespecKind::kInteger:
      return DumpVarKind::kInteger;
    case support::TypespecKind::kTime:
      return DumpVarKind::kTime;
    case support::TypespecKind::kEnum:
      if (type.base == nullptr) {
        throw InternalError(
            "dump: an enumeration's type states no base it is declared by -- "
            "please report this as a bug");
      }
      return VariableKeyword(*type.base);
    case support::TypespecKind::kBit:
    case support::TypespecKind::kLogic:
    case support::TypespecKind::kShortInt:
    case support::TypespecKind::kLongInt:
    case support::TypespecKind::kByte:
    case support::TypespecKind::kPackedStruct:
    case support::TypespecKind::kPackedUnion:
    case support::TypespecKind::kPackedArray:
      return DumpVarKind::kReg;
  }
  throw InternalError("dump: unknown type kind");
}

// The keyword a net of `net_type` is declared by (LRM 21.7.2.1).
auto NetKeyword(support::NetType net_type) -> DumpVarKind {
  switch (net_type) {
    case support::NetType::kWire:
    case support::NetType::kUwire:
      return DumpVarKind::kWire;
    case support::NetType::kTri:
      return DumpVarKind::kTri;
    case support::NetType::kWand:
      return DumpVarKind::kWand;
    case support::NetType::kTriand:
      return DumpVarKind::kTriand;
    case support::NetType::kWor:
      return DumpVarKind::kWor;
    case support::NetType::kTrior:
      return DumpVarKind::kTrior;
    case support::NetType::kTri0:
      return DumpVarKind::kTri0;
    case support::NetType::kTri1:
      return DumpVarKind::kTri1;
    case support::NetType::kSupply0:
      return DumpVarKind::kSupply0;
    case support::NetType::kSupply1:
      return DumpVarKind::kSupply1;
    case support::NetType::kTrireg:
      return DumpVarKind::kTrireg;
  }
  throw InternalError("dump: unknown net type");
}

auto KeywordOf(const abi::DataObjectRecord& object) -> DumpVarKind {
  const abi::Typespec& type = *object.typespec;
  switch (type.Kind()) {
    case support::TypespecKind::kEvent:
    case support::TypespecKind::kReal:
    case support::TypespecKind::kShortReal:
    case support::TypespecKind::kRealtime:
      return VariableKeyword(type);
    case support::TypespecKind::kBit:
    case support::TypespecKind::kLogic:
    case support::TypespecKind::kInt:
    case support::TypespecKind::kShortInt:
    case support::TypespecKind::kLongInt:
    case support::TypespecKind::kByte:
    case support::TypespecKind::kInteger:
    case support::TypespecKind::kTime:
    case support::TypespecKind::kEnum:
    case support::TypespecKind::kPackedStruct:
    case support::TypespecKind::kPackedUnion:
    case support::TypespecKind::kPackedArray:
      break;
  }
  const std::optional<support::NetType> net_type = object.NetType();
  return net_type.has_value() ? NetKeyword(*net_type) : VariableKeyword(type);
}

// The port an extended dump records `port`, at `position` among its scope's
// ports, as (LRM 21.7.3.1), none for a port bound by reference, which carries
// no value across it.
auto DumpedPortOf(const abi::DescribedPort& described, std::uint32_t position)
    -> std::optional<DumpedPort> {
  const auto port = [&](DumpPortDirection direction) {
    return DumpedPort{.direction = direction, .position = position};
  };
  switch (described.Direction()) {
    case support::PortDirection::kInput:
      return port(DumpPortDirection::kInput);
    case support::PortDirection::kOutput:
      return port(DumpPortDirection::kOutput);
    case support::PortDirection::kInOut:
      return port(DumpPortDirection::kInOut);
    case support::PortDirection::kRef:
    case support::PortDirection::kConstRef:
      return std::nullopt;
  }
  throw InternalError("dump: unknown port direction");
}

// The range a dump declares a value of `type` under (LRM 21.7.5): an
// enumeration's base's, since an enumeration whose declaration states a type
// is dumped as that type, and for a packed array of several dimensions one
// spanning all its bits, since those dimensions are collapsed into one. The
// type as declared stays the row's, which a reader of the design by name asks.
auto DumpedRangeOf(const abi::Typespec& type) -> std::optional<support::Range> {
  const auto collapsed = [&]() -> std::optional<support::Range> {
    const std::optional<support::Range> outermost = type.Range();
    if (!outermost.has_value()) {
      return std::nullopt;
    }
    const std::int64_t span = outermost->left > outermost->right
                                  ? outermost->left - outermost->right + 1
                                  : outermost->right - outermost->left + 1;
    if (span == static_cast<std::int64_t>(type.width)) {
      return outermost;
    }
    return support::Range{
        .left = static_cast<std::int64_t>(type.width) - 1, .right = 0};
  };
  switch (type.Kind()) {
    case support::TypespecKind::kEnum:
      return type.base != nullptr ? DumpedRangeOf(*type.base) : std::nullopt;
    case support::TypespecKind::kBit:
    case support::TypespecKind::kLogic:
    case support::TypespecKind::kPackedArray:
      return collapsed();
    case support::TypespecKind::kInt:
    case support::TypespecKind::kShortInt:
    case support::TypespecKind::kLongInt:
    case support::TypespecKind::kByte:
    case support::TypespecKind::kInteger:
    case support::TypespecKind::kTime:
    case support::TypespecKind::kReal:
    case support::TypespecKind::kShortReal:
    case support::TypespecKind::kRealtime:
    case support::TypespecKind::kEvent:
    case support::TypespecKind::kPackedStruct:
    case support::TypespecKind::kPackedUnion:
      return type.Range();
  }
  throw InternalError("dump: unknown type kind");
}

}  // namespace

auto DumpVarDeclarationOf(const abi::DataObjectRecord& object)
    -> DumpVarDeclaration {
  const DumpVarKind kind = KeywordOf(object);
  const auto width = [&]() -> std::uint32_t {
    switch (DumpValueFormOf(kind)) {
      case DumpValueForm::kTriggered:
        return 1;
      case DumpValueForm::kReal:
        return 64;
      case DumpValueForm::kBits:
        return object.typespec->width;
    }
    throw InternalError("dump: unknown value");
  }();
  return DumpVarDeclaration{
      .identifier = object.identifier,
      .kind = kind,
      .width = width,
      .range = DumpedRangeOf(*object.typespec),
      .port = std::nullopt};
}

auto DumpPortDeclarationOf(
    const abi::DataObjectRecord& connects, const abi::DescribedPort& port,
    std::uint32_t position) -> std::optional<DumpVarDeclaration> {
  DumpVarDeclaration declared = DumpVarDeclarationOf(connects);
  switch (DumpValueFormOf(declared.kind)) {
    case DumpValueForm::kTriggered:
      return std::nullopt;
    case DumpValueForm::kBits:
    case DumpValueForm::kReal:
      break;
  }
  declared.identifier = port.name;
  declared.port = DumpedPortOf(port, position);
  if (!declared.port.has_value()) {
    return std::nullopt;
  }
  return declared;
}

auto DumpReference(const DumpVarDeclaration& variable) -> std::string {
  std::string reference = variable.identifier;
  if (variable.range.has_value()) {
    reference +=
        std::format(" [{}:{}]", variable.range->left, variable.range->right);
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
  explicit DumpTextFile(OpenedStream file) : file_(std::move(file)) {
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
  }

  void Close() {
    Drain();
    if (std::fclose(file_.release()) != 0 && !refused_) {
      refused_ = base::ErrnoAsErrorCode();
    }
  }

  [[nodiscard]] auto Refused() const -> std::error_code {
    return refused_;
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
    if (!refused_ &&
        std::fwrite(pending_.data(), 1, pending_.size(), file_.get()) !=
            pending_.size()) {
      refused_ = base::ErrnoAsErrorCode();
    }
    written_ += pending_.size();
    pending_.clear();
  }

  OpenedStream file_;
  std::error_code refused_;
  std::string pending_;
  std::uint64_t written_ = 0;
  std::optional<std::uint64_t> limit_;
};

// What the standard's format and its extended one share (LRM 21.7.2.1,
// 21.7.4.1): the keyword commands -- the header, the scopes, the time, a
// section's end and a comment -- and the file they are written to.
class VcdCommandWriter : public DumpWriter {
 public:
  explicit VcdCommandWriter(OpenedStream file) : file_(std::move(file)) {
    file_.Put("$version\n   Lyra\n$end\n");
  }

  void Timescale(std::int8_t precision_power) override {
    const value::TimeUnitSpelling timescale =
        value::TimeUnitSpellingOf(precision_power);
    file_.Put(
        std::format(
            "$timescale\n   {}{}\n$end\n", timescale.count, timescale.unit));
  }

  void Scope(DumpScopeKind kind, std::string_view name) override {
    file_.Put(
        std::format("$scope {} {} $end\n", DumpScopeKindKeyword(kind), name));
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

  [[nodiscard]] auto Refused() const -> std::error_code override {
    return file_.Refused();
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

  void Var(DumpCode code, const DumpVarDeclaration& variable) override {
    if (scalar_.size() <= code.value) {
      scalar_.resize(code.value + 1);
    }
    scalar_[code.value] = variable.width == 1;
    Declare(code, variable);
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

  void Declare(DumpCode code, const DumpVarDeclaration& variable) {
    File().Put(
        std::format(
            "$var {} {} {} {} $end\n", DumpVarKindKeyword(variable.kind),
            variable.width, Identifier(code), DumpReference(variable)));
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
auto PortStateCharacter(DumpPortDirection direction, value::FourStateBit bit)
    -> char {
  const auto planes = static_cast<std::size_t>(std::to_underlying(bit));
  switch (direction) {
    case DumpPortDirection::kInput:
      return std::string_view("DUZN")[planes];
    case DumpPortDirection::kOutput:
      return std::string_view("LHTX")[planes];
    case DumpPortDirection::kInOut:
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
  void Var(DumpCode code, const DumpVarDeclaration& variable) override {
    const std::optional<DumpedPort>& port = variable.port;
    if (!port.has_value()) {
      throw InternalError("ExtendedVcdWriter: a dumped port has a direction");
    }
    if (ports_.size() <= code.value) {
      ports_.resize(code.value + 1);
    }
    ports_[code.value] = {
        .direction = port->direction, .width = UnknownWidth(variable)};
    std::string size = "1";
    if (variable.range.has_value()) {
      size =
          std::format("[{}:{}]", variable.range->left, variable.range->right);
    }
    File().Put(
        std::format(
            "$var port {} <{} {} $end\n", size, code.value,
            variable.identifier));
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
    DumpPortDirection direction = DumpPortDirection::kInput;
    std::uint32_t width = 0;
  };

  // An unknown real has no position to spell, so it is spelled as one.
  static auto UnknownWidth(const DumpVarDeclaration& variable)
      -> std::uint32_t {
    switch (DumpValueFormOf(variable.kind)) {
      case DumpValueForm::kReal:
        return 1;
      case DumpValueForm::kTriggered:
        throw InternalError("ExtendedVcdWriter: a port is no event");
      case DumpValueForm::kBits:
        return variable.width;
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

// Text a person reads: one line a change, `<time> <name> = <value>`, the time
// with its unit and a vector's every position from the most significant.
class TextWriter final : public DumpWriter {
 public:
  explicit TextWriter(OpenedStream file) : file_(std::move(file)) {
  }

  void Timescale(std::int8_t precision_power) override {
    timescale_ = value::TimeUnitSpellingOf(precision_power);
  }

  void Scope(DumpScopeKind kind, std::string_view name) override {
    const std::string path = MemberPath(name);
    file_.Put(std::format("{} {}\n", DumpScopeKindKeyword(kind), path));
    scopes_.push_back(Named{.path = path, .kind = kind});
  }

  void Upscope() override {
    scopes_.pop_back();
  }

  void Var(DumpCode code, const DumpVarDeclaration& variable) override {
    if (names_.size() <= code.value) {
      names_.resize(code.value + 1);
    }
    names_[code.value] = MemberPath(variable.identifier);
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

  [[nodiscard]] auto Refused() const -> std::error_code override {
    return file_.Refused();
  }

 private:
  [[nodiscard]] auto Now() const -> std::string {
    return std::format("{}{}", time_ * timescale_.count, timescale_.unit);
  }

  // The hierarchical name of `name` declared in the innermost scope open: the
  // scope's name and then `name`, after `::` where the scope is a package, as
  // LRM 26.3 names what a package declares, and after `.` otherwise (LRM 23.6),
  // so a package's member is never spelled as a top module's would be.
  [[nodiscard]] auto MemberPath(std::string_view name) const -> std::string {
    if (scopes_.empty()) {
      return std::string(name);
    }
    const Named& within = scopes_.back();
    return std::format(
        "{}{}{}", within.path, MemberSeparatorOf(within.kind), name);
  }

  static auto MemberSeparatorOf(DumpScopeKind kind) -> std::string_view {
    switch (kind) {
      case DumpScopeKind::kPackage:
        return "::";
      case DumpScopeKind::kModule:
      case DumpScopeKind::kTask:
      case DumpScopeKind::kFunction:
      case DumpScopeKind::kBegin:
      case DumpScopeKind::kFork:
        return ".";
    }
    throw InternalError("TextWriter: unknown scope kind");
  }

  // A scope open, by the hierarchical name it is written under and what it
  // is.
  struct Named {
    std::string path;
    DumpScopeKind kind;
  };

  DumpTextFile file_;
  value::TimeUnitSpelling timescale_{.count = 1, .unit = "s"};
  std::vector<Named> scopes_;
  std::vector<std::string> names_;
  SimTime time_ = 0;
};

template <typename Writer>
auto WriterOf(const std::string& path) -> std::unique_ptr<DumpWriter> {
  // The text is held a large piece at a time already, so each piece reaches
  // the system as it is handed over, and the system's answer with it.
  OpenedStream file(std::fopen(path.c_str(), "wb"), &std::fclose);
  if (file == nullptr || std::setvbuf(file.get(), nullptr, _IONBF, 0) != 0) {
    return nullptr;
  }
  return std::make_unique<Writer>(std::move(file));
}

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
  return WriterOf<TextWriter>(path);
}

auto OpenVcdWriter(const std::string& path) -> std::unique_ptr<DumpWriter> {
  return WriterOf<VcdWriter>(path);
}

auto OpenExtendedVcdWriter(const std::string& path)
    -> std::unique_ptr<DumpWriter> {
  return WriterOf<ExtendedVcdWriter>(path);
}

}  // namespace lyra::runtime
