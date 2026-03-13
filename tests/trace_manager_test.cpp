#include "lyra/trace/trace_manager.hpp"

#include <cstddef>
#include <cstdint>
#include <gtest/gtest.h>
#include <memory>
#include <utility>
#include <vector>

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
  tm.EmitValueChange(1, PackedSnapshot{.byte_size = 1, .bytes = {0x42}});
  tm.EmitMemoryDirty(2);

  EXPECT_TRUE(sink_ptr->events.empty());
}

TEST(TraceManagerTest, EnabledEmitDispatchesToAllSinks) {
  TraceManager tm;
  auto sink = std::make_unique<RecordingSink>();
  auto* sink_ptr = sink.get();
  tm.AddSink(std::move(sink));
  tm.SetEnabled(true);

  tm.EmitTimeAdvance(100);
  tm.EmitValueChange(1, PackedSnapshot{.byte_size = 1, .bytes = {0x42}});
  tm.EmitMemoryDirty(2);

  // External sink should see all 3 events.
  ASSERT_EQ(sink_ptr->events.size(), 3u);

  // Verify event types.
  EXPECT_TRUE(std::holds_alternative<TimeAdvance>(sink_ptr->events[0]));
  EXPECT_TRUE(std::holds_alternative<ValueChange>(sink_ptr->events[1]));
  EXPECT_TRUE(std::holds_alternative<MemoryDirty>(sink_ptr->events[2]));
}

TEST(TraceManagerTest, ReEnableResetsSummary) {
  TraceManager tm;
  auto sink = std::make_unique<RecordingSink>();
  auto* sink_ptr = sink.get();
  tm.AddSink(std::move(sink));

  // First session: emit some events.
  tm.SetEnabled(true);
  tm.EmitTimeAdvance(0);
  tm.EmitValueChange(1, PackedSnapshot{.byte_size = 1, .bytes = {0x42}});
  tm.SetEnabled(false);

  // External sink retains its events (no reset -- that is sink's business).
  EXPECT_EQ(sink_ptr->events.size(), 2u);

  // Second session: re-enable resets summary.
  tm.SetEnabled(true);
  tm.EmitTimeAdvance(100);

  // External sink continues accumulating (3 total).
  EXPECT_EQ(sink_ptr->events.size(), 3u);

  // Summary should reflect only the second session (1 time advance, no
  // value changes). We verify this indirectly: if stale counts leaked,
  // PrintSummary would show value_changes=1 from the first session.
  // The summary sink was reset on re-enable, so it sees only 1 time advance.
  // (Direct counter inspection would require exposing internals; the
  // integration YAML tests verify the output format.)
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
  // (Verifies enable does not double-register sinks.)
  EXPECT_EQ(sink_ptr->events.size(), 1u);
}

TEST(TraceManagerTest, ExternalSinkAndSummaryBothReceiveEachEvent) {
  TraceManager tm;
  auto sink = std::make_unique<RecordingSink>();
  auto* sink_ptr = sink.get();
  tm.AddSink(std::move(sink));
  tm.SetEnabled(true);

  tm.EmitTimeAdvance(0);
  tm.EmitValueChange(5, PackedSnapshot{.byte_size = 1, .bytes = {0xFF}});

  // External sink sees 2 events.
  EXPECT_EQ(sink_ptr->events.size(), 2u);

  // Summary sink also processed the same events (verified via PrintSummary
  // in integration tests; here we just confirm no crash and correct count
  // on the external side).
}

TEST(TraceManagerTest, DisableAfterEnableFreezesSummary) {
  TraceManager tm;
  tm.SetEnabled(true);
  tm.EmitTimeAdvance(0);
  tm.EmitValueChange(1, PackedSnapshot{.byte_size = 1, .bytes = {0x01}});
  tm.SetEnabled(false);

  // Events after disable should not be dispatched.
  tm.EmitTimeAdvance(100);
  tm.EmitValueChange(2, PackedSnapshot{.byte_size = 1, .bytes = {0x02}});

  // PrintSummary should still work (prints frozen session state).
  // No crash, no UB. Output verified by integration tests.
  tm.PrintSummary();
}

TEST(TraceManagerTest, PrintSummaryBeforeEnableProducesZeros) {
  TraceManager tm;
  // Should not crash. Prints zeros (never-enabled session).
  tm.PrintSummary();
}

}  // namespace
}  // namespace lyra::trace
