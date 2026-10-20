#include "lyra/runtime/dump_writer.hpp"

#include <array>
#include <cstdint>
#include <filesystem>
#include <fstapi.h>
#include <fstream>
#include <gtest/gtest.h>
#include <sstream>
#include <string>
#include <vector>

#include "lyra/support/dump_kind.hpp"
#include "lyra/value/integral_words.hpp"

namespace {

using lyra::runtime::DumpCheckpoint;
using lyra::runtime::DumpCode;
using lyra::runtime::DumpEntry;
using lyra::runtime::DumpTriggered;
using lyra::runtime::DumpUnknown;
using lyra::runtime::DumpWriter;
using lyra::support::DumpScopeKind;
using lyra::support::DumpVarKind;
using lyra::value::ConstIntegralView;

auto PathOf(const std::string& name) -> std::string {
  return (std::filesystem::temp_directory_path() / name).string();
}

// The constant a class states of a declaration, a vector declared `[width-1:0]`
// where `ranged`.
auto Entry(
    const char* identifier, DumpVarKind kind, std::uint32_t width, bool ranged)
    -> DumpEntry {
  return DumpEntry{
      .identifier = identifier,
      .left = ranged ? static_cast<std::int64_t>(width) - 1 : 0,
      .right = 0,
      .width = width,
      .kind = static_cast<std::uint8_t>(kind),
      .has_range = static_cast<std::uint8_t>(ranged ? 1 : 0),
      .has_port = 0,
      .port_direction = 0,
      .port_position = 0};
}

auto ReadFile(const std::string& path) -> std::string {
  std::ifstream file(path);
  std::stringstream text;
  text << file.rdbuf();
  return text.str();
}

// One dump every format is handed: a module with a 4-bit reg, a real and an
// event, and an instance inside it whose port names the reg again. The reg
// starts at 0011, goes to x1z0, the event is triggered at time 2, and the dump
// is switched off at time 3.
void WriteDump(DumpWriter& writer) {
  const DumpEntry reg = Entry("a", DumpVarKind::kReg, 4, true);
  const DumpEntry real = Entry("r", DumpVarKind::kReal, 64, false);
  const DumpEntry event = Entry("e", DumpVarKind::kEvent, 1, false);
  const DumpEntry port = Entry("p", DumpVarKind::kWire, 4, true);

  writer.Timescale(-9);
  writer.Scope(DumpScopeKind::kModule, "Top");
  writer.Var(DumpCode{0}, reg);
  writer.Var(DumpCode{1}, real);
  writer.Var(DumpCode{2}, event);
  writer.Scope(DumpScopeKind::kModule, "u");
  writer.Alias(DumpCode{0}, port);
  writer.Upscope();
  writer.Upscope();
  writer.EndDefinitions();

  const std::array<std::uint64_t, 1> start_value{0b0011};
  const std::array<std::uint64_t, 1> start_unknown{0};
  const std::array<std::uint64_t, 1> later_value{0b1100};
  const std::array<std::uint64_t, 1> later_unknown{0b1010};

  writer.Time(0);
  writer.BeginCheckpoint(DumpCheckpoint::kDumpvars);
  writer.Change(
      DumpCode{0},
      ConstIntegralView{
          .value = start_value, .unknown = start_unknown, .width = 4});
  writer.Change(DumpCode{1}, 1.5);
  writer.EndCheckpoint();
  writer.Time(1);
  // Where the unknown plane is set, a set value plane is x and a clear one z.
  writer.Change(
      DumpCode{0},
      ConstIntegralView{
          .value = later_value, .unknown = later_unknown, .width = 4});
  writer.Time(2);
  writer.Change(DumpCode{2}, DumpTriggered{});
  writer.Change(DumpCode{1}, 2.25);
  writer.Time(3);
  writer.BeginCheckpoint(DumpCheckpoint::kDumpoff);
  writer.Change(DumpCode{0}, DumpUnknown{});
  writer.Change(DumpCode{1}, DumpUnknown{});
  writer.Change(DumpCode{2}, DumpUnknown{});
  writer.EndCheckpoint();
  writer.Close();
}

TEST(DumpWriterTest, VcdWritesTheStandardLayout) {
  const std::string path = PathOf("dump.vcd");
  WriteDump(*lyra::runtime::OpenVcdWriter(path));
  EXPECT_EQ(
      ReadFile(path),
      "$version\n"
      "   Lyra\n"
      "$end\n"
      "$timescale\n"
      "   1ns\n"
      "$end\n"
      "$scope module Top $end\n"
      "$var reg 4 ! a [3:0] $end\n"
      "$var real 64 \" r $end\n"
      "$var event 1 # e $end\n"
      "$scope module u $end\n"
      "$var wire 4 ! p [3:0] $end\n"
      "$upscope $end\n"
      "$upscope $end\n"
      "$enddefinitions $end\n"
      "#0\n"
      "$dumpvars\n"
      "b11 !\n"
      "r1.5 \"\n"
      "$end\n"
      "#1\n"
      "bx1z0 !\n"
      "#2\n"
      "1#\n"
      "r2.25 \"\n"
      "#3\n"
      "$dumpoff\n"
      "bx !\n"
      "bx \"\n"
      "x#\n"
      "$end\n");
}

TEST(DumpWriterTest, TextNamesEveryPositionAndEveryName) {
  const std::string path = PathOf("dump.txt");
  WriteDump(*lyra::runtime::OpenTextWriter(path));
  EXPECT_EQ(
      ReadFile(path),
      "module Top\n"
      "module Top.u\n"
      "0ns $dumpvars\n"
      "0ns Top.a, Top.u.p = 0011\n"
      "0ns Top.r = 1.5\n"
      "1ns Top.a, Top.u.p = x1z0\n"
      "2ns Top.e = triggered\n"
      "2ns Top.r = 2.25\n"
      "3ns $dumpoff\n"
      "3ns Top.a, Top.u.p = x\n"
      "3ns Top.r = x\n"
      "3ns Top.e = x\n");
}

// The value libfst's reader finds `handle` holding at `time`, a real written
// after an `r` as a VCD writes it.
auto FstValueAt(fstReaderContext* reader, std::uint64_t time, fstHandle handle)
    -> std::string {
  std::array<char, 64> spelled{};
  return fstReaderGetValueFromHandleAtTime(
      reader, time, handle, spelled.data());
}

// What libfst's own reader finds in the file: the hierarchy as one line an
// entry, a real's length counted in bytes as FST counts it, and what each thing
// holds at each time.
TEST(DumpWriterTest, FstReadsBackThroughLibfst) {
  const std::string path = PathOf("dump.fst");
  WriteDump(*lyra::runtime::OpenFstWriter(path));

  fstReaderContext* reader = fstReaderOpen(path.c_str());
  ASSERT_NE(reader, nullptr);
  EXPECT_EQ(fstReaderGetTimescale(reader), -9);
  EXPECT_EQ(fstReaderGetVarCount(reader), 4U);
  EXPECT_EQ(fstReaderGetAliasCount(reader), 1U);

  std::vector<std::string> hierarchy;
  while (const fstHier* entry = fstReaderIterateHier(reader)) {
    switch (entry->htyp) {
      case FST_HT_SCOPE:
        hierarchy.push_back(std::string("scope ") + entry->u.scope.name);
        break;
      case FST_HT_UPSCOPE:
        hierarchy.emplace_back("upscope");
        break;
      case FST_HT_VAR:
        hierarchy.push_back(
            std::string(entry->u.var.name) + " width " +
            std::to_string(entry->u.var.length) + " handle " +
            std::to_string(entry->u.var.handle) +
            (entry->u.var.is_alias != 0 ? " alias" : ""));
        break;
      default:
        break;
    }
  }
  EXPECT_EQ(
      hierarchy, (std::vector<std::string>{
                     "scope Top", "a [3:0] width 4 handle 1",
                     "r width 8 handle 2", "e width 1 handle 3", "scope u",
                     "p [3:0] width 4 handle 1 alias", "upscope", "upscope"}));

  EXPECT_EQ(FstValueAt(reader, 0, 1), "0011");
  EXPECT_EQ(FstValueAt(reader, 0, 2), "r1.5");
  EXPECT_EQ(FstValueAt(reader, 1, 1), "x1z0");
  EXPECT_EQ(FstValueAt(reader, 2, 2), "r2.25");
  EXPECT_EQ(FstValueAt(reader, 2, 3), "1");
  EXPECT_EQ(FstValueAt(reader, 3, 1), "xxxx");
  EXPECT_EQ(FstValueAt(reader, 3, 2), "rnan");
  EXPECT_EQ(FstValueAt(reader, 3, 3), "x");
  fstReaderClose(reader);
}

}  // namespace
