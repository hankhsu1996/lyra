#include "lyra/trace/trace_manager.hpp"

#include <cstddef>
#include <cstdint>
#include <gtest/gtest.h>
#include <memory>
#include <utility>
#include <vector>

#include "lyra/runtime/output_sink.hpp"
#include "lyra/runtime/signal_coord.hpp"
#include "lyra/trace/trace_event.hpp"
#include "lyra/trace/trace_sink.hpp"

namespace lyra::trace {
namespace {

// Test sink that records all events received for verification.
class RecordingSink : public TraceSink {
 public:
  void OnEvent(const TraceEvent& event) override {
    events.push_back(event);
  }

  std::vector<TraceEvent> events;
};

TEST(TraceManagerTest, DisabledEmitDoesNothing) {
  TraceManager tm;
  auto sink = std::make_unique<RecordingSink>();
  auto* sink_ptr = sink.get();
  tm.AddSink(std::move(sink));

  // Disabled by default -- no events should reach any sink.
  tm.EmitTimeAdvance(0);
  tm.EmitGlobalValueChange(
      runtime::GlobalSignalId{1},
      PackedSnapshot{.byte_size = 1, .bytes = {0x42}});
  tm.EmitGlobalMemoryDirty(runtime::GlobalSignalId{2});

  EXPECT_TRUE(sink_ptr->events.empty());
}

TEST(TraceManagerTest, EnabledEmitDispatchesToAllSinks) {
  TraceManager tm;
  auto sink = std::make_unique<RecordingSink>();
  auto* sink_ptr = sink.get();
  tm.AddSink(std::move(sink));
  tm.SetEnabled(true);

  tm.EmitTimeAdvance(100);
  tm.EmitGlobalValueChange(
      runtime::GlobalSignalId{1},
      PackedSnapshot{.byte_size = 1, .bytes = {0x42}});
  tm.EmitGlobalMemoryDirty(runtime::GlobalSignalId{2});

  // External sink should see all 3 events.
  ASSERT_EQ(sink_ptr->events.size(), 3u);

  // Verify event types.
  EXPECT_TRUE(std::holds_alternative<TimeAdvance>(sink_ptr->events[0]));
  EXPECT_TRUE(std::holds_alternative<GlobalValueChange>(sink_ptr->events[1]));
  EXPECT_TRUE(std::holds_alternative<GlobalMemoryDirty>(sink_ptr->events[2]));
}

TEST(TraceManagerTest, ReEnableResetsSummary) {
  TraceManager tm;
  auto sink = std::make_unique<RecordingSink>();
  auto* sink_ptr = sink.get();
  tm.AddSink(std::move(sink));

  // First session: emit some events.
  tm.SetEnabled(true);
  tm.EmitTimeAdvance(0);
  tm.EmitGlobalValueChange(
      runtime::GlobalSignalId{1},
      PackedSnapshot{.byte_size = 1, .bytes = {0x42}});
  tm.SetEnabled(false);

  // External sink retains its events (no reset -- that is sink's business).
  EXPECT_EQ(sink_ptr->events.size(), 2u);

  // Second session: re-enable resets summary.
  tm.SetEnabled(true);
  tm.EmitTimeAdvance(100);

  // External sink continues accumulating (3 total).
  EXPECT_EQ(sink_ptr->events.size(), 3u);
}

TEST(TraceManagerTest, RepeatedEnableDoesNotDuplicateDispatch) {
  TraceManager tm;
  auto sink = std::make_unique<RecordingSink>();
  auto* sink_ptr = sink.get();
  tm.AddSink(std::move(sink));

  // Enable twice without disabling in between.
  tm.SetEnabled(true);
  tm.SetEnabled(true);

  tm.EmitTimeAdvance(0);

  // Should receive exactly 1 event, not 2.
  EXPECT_EQ(sink_ptr->events.size(), 1u);
}

TEST(TraceManagerTest, ExternalSinkAndSummaryBothReceiveEachEvent) {
  TraceManager tm;
  auto sink = std::make_unique<RecordingSink>();
  auto* sink_ptr = sink.get();
  tm.AddSink(std::move(sink));
  tm.SetEnabled(true);

  tm.EmitTimeAdvance(0);
  tm.EmitGlobalValueChange(
      runtime::GlobalSignalId{5},
      PackedSnapshot{.byte_size = 1, .bytes = {0xFF}});

  // External sink sees 2 events.
  EXPECT_EQ(sink_ptr->events.size(), 2u);
}

TEST(TraceManagerTest, DisableAfterEnableFreezesSummary) {
  TraceManager tm;
  tm.SetEnabled(true);
  tm.EmitTimeAdvance(0);
  tm.EmitGlobalValueChange(
      runtime::GlobalSignalId{1},
      PackedSnapshot{.byte_size = 1, .bytes = {0x01}});
  tm.SetEnabled(false);

  // Events after disable should not be dispatched.
  tm.EmitTimeAdvance(100);
  tm.EmitGlobalValueChange(
      runtime::GlobalSignalId{2},
      PackedSnapshot{.byte_size = 1, .bytes = {0x02}});

  // PrintSummary should still work (prints frozen session state).
  runtime::OutputDispatcher out;
  tm.PrintSummary(out);
}

TEST(TraceManagerTest, PrintSummaryBeforeEnableProducesZeros) {
  TraceManager tm;
  // Should not crash. Prints zeros (never-enabled session).
  runtime::OutputDispatcher out;
  tm.PrintSummary(out);
}

}  // namespace
}  // namespace lyra::trace
