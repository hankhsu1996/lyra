#include "lyra/runtime/value_dump.hpp"

#include <atomic>
#include <chrono>
#include <cstdint>
#include <format>
#include <gtest/gtest.h>
#include <memory>
#include <mutex>
#include <string>
#include <string_view>
#include <system_error>
#include <thread>
#include <utility>
#include <vector>

#include "lyra/base/simulation_error.hpp"
#include "lyra/base/source_location.hpp"
#include "lyra/runtime/dump_writer.hpp"
#include "lyra/runtime/scope.hpp"
#include "lyra/runtime/value_dumps.hpp"

namespace {

using lyra::SimulationError;
using lyra::abi::DumpEntry;
using lyra::runtime::DumpCheckpoint;
using lyra::runtime::DumpCode;
using lyra::runtime::DumpScopeKind;
using lyra::runtime::DumpSelection;
using lyra::runtime::DumpValue;
using lyra::runtime::DumpWriter;
using lyra::runtime::Scope;
using lyra::runtime::ValueDump;
using lyra::runtime::ValueDumps;

// What a format was handed, one line a call, and what it does meanwhile:
// waits at its first checkpoint until `opened` is set, and refuses everything
// once `refusing` is.
struct Handed {
  std::mutex mutex;
  std::vector<std::string> calls;
  std::atomic<bool> opened = true;
  std::error_code refusing;
};

class RecordingWriter final : public DumpWriter {
 public:
  explicit RecordingWriter(Handed& handed) : handed_(&handed) {
  }

  void Timescale(std::int8_t precision_power) override {
    Record(std::format("timescale {}", precision_power));
  }
  void Scope(DumpScopeKind kind, std::string_view name) override {
    Record(std::format("scope {} {}", std::to_underlying(kind), name));
  }
  void Upscope() override {
    Record("upscope");
  }
  void Var(DumpCode code, const DumpEntry& entry) override {
    Record(std::format("var {} {}", code.value, entry.identifier));
  }
  void Alias(DumpCode code, const DumpEntry& entry) override {
    Record(std::format("alias {} {}", code.value, entry.identifier));
  }
  void EndDefinitions() override {
    Record("enddefinitions");
  }
  void Time(lyra::SimTime time) override {
    Record(std::format("#{}", time));
  }
  void Change(DumpCode code, const DumpValue& value) override {
    Record(std::format("change {} {}", code.value, value.index()));
  }
  void BeginCheckpoint(DumpCheckpoint checkpoint) override {
    while (!handed_->opened.load()) {
      std::this_thread::sleep_for(std::chrono::milliseconds{1});
    }
    Record(Spelled(checkpoint));
  }
  void EndCheckpoint() override {
  }
  void Comment(std::string_view text) override {
    Record(std::format("comment {}", text));
  }
  void Flush() override {
    Record("flush");
  }
  void Close() override {
    Record("close");
  }
  void LimitTo(std::uint64_t bytes) override {
    Record(std::format("limit {}", bytes));
  }
  [[nodiscard]] auto ReachedLimit() const -> bool override {
    return false;
  }
  [[nodiscard]] auto Refused() const -> std::error_code override {
    const std::scoped_lock lock(handed_->mutex);
    return handed_->refusing;
  }

 private:
  static auto Spelled(DumpCheckpoint checkpoint) -> std::string {
    switch (checkpoint) {
      case DumpCheckpoint::kDumpvars:
        return "dumpvars";
      case DumpCheckpoint::kDumpall:
        return "dumpall";
      case DumpCheckpoint::kDumpon:
        return "on";
      case DumpCheckpoint::kDumpoff:
        return "off";
    }
    return "unknown checkpoint";
  }

  void Record(std::string call) {
    const std::scoped_lock lock(handed_->mutex);
    handed_->calls.push_back(std::move(call));
  }

  Handed* handed_;
};

auto CallsOf(Handed& handed) -> std::vector<std::string> {
  const std::scoped_lock lock(handed.mutex);
  return handed.calls;
}

auto DumpWritingTo(Handed& handed) -> std::unique_ptr<ValueDump> {
  return std::make_unique<ValueDump>(
      std::make_unique<RecordingWriter>(handed), -9, "recorded.vcd",
      lyra::base::SourceLocation{});
}

// A writer that falls far behind makes the simulation wait for room, and what
// it is handed is everything recorded, in the order it was recorded.
TEST(ValueDumpTest, AWriterBehindHoldsTheSimulationBackAndLosesNothing) {
  Handed handed;
  handed.opened = false;
  const std::unique_ptr<ValueDump> dump = DumpWritingTo(handed);
  std::jthread opener([&] {
    std::this_thread::sleep_for(std::chrono::milliseconds{50});
    handed.opened = true;
  });
  constexpr lyra::SimTime kToggles = 3000;
  for (lyra::SimTime i = 0; i < kToggles; ++i) {
    dump->Off(2 * i);
    dump->On((2 * i) + 1);
  }
  dump->Close(2 * kToggles);

  std::vector<std::string> expected = {"timescale -9"};
  for (lyra::SimTime i = 0; i < kToggles; ++i) {
    expected.push_back(std::format("#{}", 2 * i));
    expected.emplace_back("off");
    expected.push_back(std::format("#{}", (2 * i) + 1));
    expected.emplace_back("on");
  }
  expected.push_back(std::format("#{}", 2 * kToggles));
  expected.emplace_back("close");
  EXPECT_EQ(CallsOf(handed), expected);
}

// An ending hands the dump to its writer, which writes everything recorded,
// ends the file at the time the run was at, and only then lets the ending go
// on. An ending that read the time before the simulation published a later
// step -- one on a thread the simulation goes on beside -- never takes the
// file's time back.
TEST(ValueDumpTest, AnEndingReturnsOnceTheFileIsEndedWhereTheRunWas) {
  for (const lyra::SimTime ending_at : {7, 2}) {
    Handed handed;
    ValueDumps dumps({});
    const std::unique_ptr<ValueDump> dump = DumpWritingTo(handed);
    // A design with no tops, so the dump declares nothing.
    Scope root;
    dump->Start(DumpSelection{}, root, 0, dumps);
    dump->Off(3);
    dump->FinishAtEnding(ending_at);
    std::vector<std::string> expected = {
        "timescale -9", "enddefinitions", "#0", "dumpvars", "#3", "off"};
    if (ending_at > 3) {
      expected.push_back(std::format("#{}", ending_at));
    }
    expected.emplace_back("close");
    EXPECT_EQ(CallsOf(handed), expected) << "ending at " << ending_at;
  }
}

// What the system refused is the failure of whoever asked for the dump, said
// where the simulation next hands the file something -- at the latest where it
// waits on the file -- and said once.
TEST(ValueDumpTest, ARefusalIsRaisedOnceWhereTheSimulationWaits) {
  Handed handed;
  handed.refusing = std::make_error_code(std::errc::no_space_on_device);
  const std::unique_ptr<ValueDump> dump = DumpWritingTo(handed);
  try {
    dump->Off(1);
    dump->Flush();
    FAIL() << "a refusal was not raised";
  } catch (const SimulationError& refused) {
    EXPECT_EQ(
        std::string_view(refused.what()),
        std::format(
            "the dump file 'recorded.vcd' could not be written ({})",
            handed.refusing.message()));
  }
  EXPECT_NO_THROW(dump->Close(2));
}

}  // namespace
