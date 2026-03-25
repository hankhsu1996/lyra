#include "lyra/runtime/trace_selection.hpp"

#include <array>
#include <cstdint>
#include <gtest/gtest.h>
#include <memory>
#include <utility>
#include <vector>

#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/slot_meta.hpp"
#include "lyra/runtime/trace_flush.hpp"
#include "lyra/runtime/trace_signal_meta.hpp"
#include "lyra/runtime/update_set.hpp"
#include "lyra/trace/trace_event.hpp"
#include "lyra/trace/trace_manager.hpp"
#include "lyra/trace/trace_sink.hpp"

namespace lyra::runtime {
namespace {

TEST(TraceSelectionTest, DefaultIsUnconfigured) {
  TraceSelectionRegistry sel;
  EXPECT_FALSE(sel.IsConfigured());
  EXPECT_FALSE(sel.IsSelected(0));
  EXPECT_FALSE(sel.IsSelected(42));
}

TEST(TraceSelectionTest, InitDefaultsAllSelected) {
  TraceSelectionRegistry sel;
  sel.Init(2);
  EXPECT_TRUE(sel.IsConfigured());
  EXPECT_TRUE(sel.IsSelected(0));
  EXPECT_TRUE(sel.IsSelected(1));
}

TEST(TraceSelectionTest, SelectNoneDeselectsAll) {
  TraceSelectionRegistry sel;
  sel.Init(2);
  sel.SelectNone();
  EXPECT_FALSE(sel.IsSelected(0));
  EXPECT_FALSE(sel.IsSelected(1));
}

TEST(TraceSelectionTest, SelectAllReselectsAll) {
  TraceSelectionRegistry sel;
  sel.Init(2);
  sel.SelectNone();
  sel.SelectAll();
  EXPECT_TRUE(sel.IsSelected(0));
  EXPECT_TRUE(sel.IsSelected(1));
}

TEST(TraceSelectionTest, SetSelectedTogglesIndividualSlots) {
  TraceSelectionRegistry sel;
  sel.Init(2);

  sel.SetSelected(0, false);
  EXPECT_FALSE(sel.IsSelected(0));
  EXPECT_TRUE(sel.IsSelected(1));

  sel.SetSelected(0, true);
  EXPECT_TRUE(sel.IsSelected(0));
}

TEST(TraceSelectionTest, SetSelectedOutOfRangeThrows) {
  TraceSelectionRegistry sel;
  sel.Init(2);
  EXPECT_THROW(sel.SetSelected(2, true), common::InternalError);
  EXPECT_THROW(sel.SetSelected(100, false), common::InternalError);
}

TEST(TraceSelectionTest, IsSelectedOutOfRangeThrowsWhenConfigured) {
  TraceSelectionRegistry sel;
  sel.Init(2);
  EXPECT_THROW(static_cast<void>(sel.IsSelected(2)), common::InternalError);
  EXPECT_THROW(static_cast<void>(sel.IsSelected(100)), common::InternalError);
}

TEST(TraceSelectionTest, ClearResetsToUnconfigured) {
  TraceSelectionRegistry sel;
  sel.Init(2);
  EXPECT_TRUE(sel.IsConfigured());

  sel.Clear();
  EXPECT_FALSE(sel.IsConfigured());
  EXPECT_FALSE(sel.IsSelected(0));
}

TEST(TraceSelectionTest, ReinitAfterClearSucceeds) {
  TraceSelectionRegistry sel;
  sel.Init(2);
  sel.Clear();
  sel.Init(3);
  EXPECT_TRUE(sel.IsConfigured());
  EXPECT_TRUE(sel.IsSelected(2));
}

TEST(TraceSelectionTest, ReinitWithoutClearThrows) {
  TraceSelectionRegistry sel;
  sel.Init(2);
  EXPECT_THROW(sel.Init(3), common::InternalError);
}

TEST(TraceSelectionTest, UnconfiguredMutationThrows) {
  TraceSelectionRegistry sel;
  EXPECT_THROW(sel.SelectAll(), common::InternalError);
  EXPECT_THROW(sel.SelectNone(), common::InternalError);
  EXPECT_THROW(sel.SetSelected(0, true), common::InternalError);
}

class RecordingSink : public trace::TraceSink {
 public:
  void OnEvent(const trace::TraceEvent& event) override {
    events.push_back(event);
  }

  std::vector<trace::TraceEvent> events;
};

// Minimal signal metadata: 2 slots, each 1-bit wide.
// Word table: stride=4, [name_off, bit_width, kind, storage_owner] per slot.
// String pool: "a\0b\0"
constexpr std::array<uint32_t, 8> kSignalWords = {
    0, 1, 0, 0, 2, 1, 0, 1,
};
constexpr std::array<char, 4> kSignalPool = {'a', '\0', 'b', '\0'};

auto MakeSignalMeta() -> TraceSignalMetaRegistry {
  return {
      kSignalWords.data(), static_cast<uint32_t>(kSignalWords.size()),
      kSignalPool.data(), static_cast<uint32_t>(kSignalPool.size())};
}

// Build a minimal SlotMetaRegistry from (base_off, total_bytes) pairs.
// Stride=8: [base_off, total_bytes, kind, val_off, val_bytes, unk_off,
//            unk_bytes, storage_owner_slot_id]
auto MakeSlotMeta(std::vector<std::pair<uint32_t, uint32_t>> slots)
    -> SlotMetaRegistry {
  std::vector<uint32_t> words;
  for (uint32_t i = 0; i < slots.size(); ++i) {
    const auto& [base_off, total_bytes] = slots[i];
    words.push_back(base_off);
    words.push_back(total_bytes);
    words.push_back(0);
    words.push_back(0);
    words.push_back(0);
    words.push_back(0);
    words.push_back(0);
    words.push_back(i);  // storage_owner_slot_id = self
  }
  return SlotMetaRegistry(words.data(), static_cast<uint32_t>(slots.size()));
}

TEST(TraceSelectionTest, ProducerSkipsDeselectedSlots) {
  // Set up design state: 2 one-byte slots at offsets 0 and 1.
  std::array<uint8_t, 2> design_state = {0xAA, 0xBB};

  auto slot_meta = MakeSlotMeta({{0, 1}, {1, 1}});

  // Set up UpdateSet with both slots dirty.
  UpdateSet updates;
  std::vector<uint32_t> sizes = {1, 1};
  updates.Init(2, sizes);
  updates.MarkSlotDirty(0);
  updates.MarkSlotDirty(1);

  // Set up TraceManager with recording sink and signal metadata.
  trace::TraceManager tm;
  auto signal_meta = MakeSignalMeta();
  tm.SetSignalMeta(&signal_meta);
  tm.SetEnabled(true);

  auto sink = std::make_unique<RecordingSink>();
  auto* sink_ptr = sink.get();
  tm.AddSink(std::move(sink));

  // Set up selection registry -- all selected by default.
  TraceSelectionRegistry selection;
  selection.Init(2);

  // All selected: both slots should emit.
  FlushDirtySlotsToTrace(
      tm, slot_meta, signal_meta, design_state.data(), updates, selection);

  uint32_t value_changes = 0;
  for (const auto& event : sink_ptr->events) {
    if (std::holds_alternative<trace::ValueChange>(event)) {
      ++value_changes;
    }
  }
  EXPECT_EQ(value_changes, 2);

  // Deselect slot 0, re-dirty both, flush again.
  sink_ptr->events.clear();
  updates.Clear();
  updates.MarkSlotDirty(0);
  updates.MarkSlotDirty(1);
  selection.SetSelected(0, false);

  FlushDirtySlotsToTrace(
      tm, slot_meta, signal_meta, design_state.data(), updates, selection);

  value_changes = 0;
  std::vector<uint32_t> emitted_slots;
  for (const auto& event : sink_ptr->events) {
    if (const auto* vc = std::get_if<trace::ValueChange>(&event)) {
      ++value_changes;
      emitted_slots.push_back(vc->slot_id);
    }
  }
  EXPECT_EQ(value_changes, 1);
  ASSERT_EQ(emitted_slots.size(), 1);
  EXPECT_EQ(emitted_slots[0], 1);

  // SelectNone: no value changes should emit.
  sink_ptr->events.clear();
  updates.Clear();
  updates.MarkSlotDirty(0);
  updates.MarkSlotDirty(1);
  selection.SelectNone();

  FlushDirtySlotsToTrace(
      tm, slot_meta, signal_meta, design_state.data(), updates, selection);

  value_changes = 0;
  for (const auto& event : sink_ptr->events) {
    if (std::holds_alternative<trace::ValueChange>(event)) {
      ++value_changes;
    }
  }
  EXPECT_EQ(value_changes, 0);
}

}  // namespace
}  // namespace lyra::runtime
