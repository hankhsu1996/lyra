#include "lyra/runtime/trace_signal_meta.hpp"

#include <cstdint>
#include <gtest/gtest.h>
#include <string>
#include <vector>

#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/trace_signal_meta_abi.hpp"

namespace lyra::runtime {
namespace {

namespace abi = trace_signal_meta_abi;

// Helper: build a string pool from a list of strings, returning offsets.
struct PoolBuilder {
  std::vector<char> pool;
  std::vector<uint32_t> offsets;

  auto Add(const std::string& s) -> uint32_t {
    auto off = static_cast<uint32_t>(pool.size());
    pool.insert(pool.end(), s.begin(), s.end());
    pool.push_back('\0');
    offsets.push_back(off);
    return off;
  }
};

// Helper: append one entry to a word table.
// storage_owner_slot_id defaults to self (entry index derived from word count).
void PushEntry(
    std::vector<uint32_t>& words, uint32_t name_off, uint32_t bit_width,
    TraceSignalKind kind) {
  auto slot_id = static_cast<uint32_t>(words.size() / abi::kStride);
  words.push_back(name_off);
  words.push_back(bit_width);
  words.push_back(static_cast<uint32_t>(kind));
  words.push_back(slot_id);
}

TEST(TraceSignalMetaRegistryTest, DefaultConstructedIsEmpty) {
  TraceSignalMetaRegistry reg;
  EXPECT_EQ(reg.Count(), 0u);
  EXPECT_FALSE(reg.IsPopulated());
}

TEST(TraceSignalMetaRegistryTest, SingleSlot) {
  PoolBuilder pb;
  auto off = pb.Add("top.u_cpu.pc");

  std::vector<uint32_t> words;
  PushEntry(words, off, 32, TraceSignalKind::kVariable);

  TraceSignalMetaRegistry reg(
      words.data(), static_cast<uint32_t>(words.size()), pb.pool.data(),
      static_cast<uint32_t>(pb.pool.size()));

  ASSERT_EQ(reg.Count(), 1u);
  EXPECT_TRUE(reg.IsPopulated());

  const auto& meta = reg.Get(0);
  EXPECT_EQ(meta.bit_width, 32u);
  EXPECT_EQ(meta.kind, TraceSignalKind::kVariable);
  EXPECT_EQ(reg.Name(0), "top.u_cpu.pc");
}

TEST(TraceSignalMetaRegistryTest, MultipleSlots) {
  PoolBuilder pb;
  auto off0 = pb.Add("top.clk");
  auto off1 = pb.Add("top.data");
  auto off2 = pb.Add("pkg.PARAM");

  std::vector<uint32_t> words;
  PushEntry(words, off0, 1, TraceSignalKind::kNet);
  PushEntry(words, off1, 8, TraceSignalKind::kVariable);
  PushEntry(words, off2, 16, TraceSignalKind::kParam);

  TraceSignalMetaRegistry reg(
      words.data(), static_cast<uint32_t>(words.size()), pb.pool.data(),
      static_cast<uint32_t>(pb.pool.size()));

  ASSERT_EQ(reg.Count(), 3u);

  EXPECT_EQ(reg.Name(0), "top.clk");
  EXPECT_EQ(reg.Get(0).bit_width, 1u);
  EXPECT_EQ(reg.Get(0).kind, TraceSignalKind::kNet);

  EXPECT_EQ(reg.Name(1), "top.data");
  EXPECT_EQ(reg.Get(1).bit_width, 8u);
  EXPECT_EQ(reg.Get(1).kind, TraceSignalKind::kVariable);

  EXPECT_EQ(reg.Name(2), "pkg.PARAM");
  EXPECT_EQ(reg.Get(2).bit_width, 16u);
  EXPECT_EQ(reg.Get(2).kind, TraceSignalKind::kParam);
}

TEST(TraceSignalMetaRegistryTest, ZeroBitWidth) {
  PoolBuilder pb;
  auto off = pb.Add("top.mem");

  std::vector<uint32_t> words;
  PushEntry(words, off, 0, TraceSignalKind::kVariable);

  TraceSignalMetaRegistry reg(
      words.data(), static_cast<uint32_t>(words.size()), pb.pool.data(),
      static_cast<uint32_t>(pb.pool.size()));

  EXPECT_EQ(reg.Get(0).bit_width, 0u);
}

TEST(TraceSignalMetaRegistryTest, EmptyWordTableProducesEmptyRegistry) {
  TraceSignalMetaRegistry reg(nullptr, 0, nullptr, 0);
  EXPECT_EQ(reg.Count(), 0u);
  EXPECT_FALSE(reg.IsPopulated());
}

TEST(TraceSignalMetaRegistryTest, OutOfRangeGetThrows) {
  TraceSignalMetaRegistry reg;
  EXPECT_THROW((void)reg.Get(0), lyra::common::InternalError);
}

TEST(TraceSignalMetaRegistryTest, OutOfRangeNameThrows) {
  TraceSignalMetaRegistry reg;
  EXPECT_THROW((void)reg.Name(0), lyra::common::InternalError);
}

TEST(TraceSignalMetaRegistryTest, InvalidStrideThrows) {
  // word_count not divisible by stride=3.
  std::vector<uint32_t> words = {0, 32};
  EXPECT_THROW(
      TraceSignalMetaRegistry(words.data(), 2, nullptr, 0),
      lyra::common::InternalError);
}

TEST(TraceSignalMetaRegistryTest, InvalidNameOffsetThrows) {
  std::vector<uint32_t> words;
  // name_off=99 points past pool
  PushEntry(words, 99, 8, TraceSignalKind::kVariable);

  std::vector<char> pool = {'\0'};
  EXPECT_THROW(
      TraceSignalMetaRegistry(
          words.data(), static_cast<uint32_t>(words.size()), pool.data(),
          static_cast<uint32_t>(pool.size())),
      lyra::common::InternalError);
}

TEST(TraceSignalMetaRegistryTest, InvalidTraceKindThrows) {
  PoolBuilder pb;
  auto off = pb.Add("top.x");

  std::vector<uint32_t> words;
  words.push_back(off);
  words.push_back(8);
  words.push_back(99);

  EXPECT_THROW(
      TraceSignalMetaRegistry(
          words.data(), static_cast<uint32_t>(words.size()), pb.pool.data(),
          static_cast<uint32_t>(pb.pool.size())),
      lyra::common::InternalError);
}

TEST(TraceSignalMetaRegistryTest, NullWordsWithCountThrows) {
  EXPECT_THROW(
      TraceSignalMetaRegistry(nullptr, 3, nullptr, 0),
      lyra::common::InternalError);
}

TEST(TraceSignalMetaRegistryTest, NullPoolWithSizeThrows) {
  std::vector<uint32_t> words;
  PushEntry(words, 0, 8, TraceSignalKind::kVariable);

  EXPECT_THROW(
      TraceSignalMetaRegistry(
          words.data(), static_cast<uint32_t>(words.size()), nullptr, 10),
      lyra::common::InternalError);
}

TEST(TraceSignalMetaRegistryTest, MoveSemantics) {
  PoolBuilder pb;
  auto off = pb.Add("top.x");

  std::vector<uint32_t> words;
  PushEntry(words, off, 4, TraceSignalKind::kVariable);

  TraceSignalMetaRegistry reg1(
      words.data(), static_cast<uint32_t>(words.size()), pb.pool.data(),
      static_cast<uint32_t>(pb.pool.size()));

  TraceSignalMetaRegistry reg2 = std::move(reg1);
  EXPECT_EQ(reg2.Count(), 1u);
  EXPECT_EQ(reg2.Name(0), "top.x");
}

}  // namespace
}  // namespace lyra::runtime
