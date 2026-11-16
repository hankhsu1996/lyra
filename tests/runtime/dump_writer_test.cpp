#include "lyra/runtime/dump_writer.hpp"

#include <array>
#include <cstdint>
#include <cstdio>
#include <filesystem>
#include <fstapi.h>
#include <fstream>
#include <gtest/gtest.h>
#include <optional>
#include <sstream>
#include <string>

#include "lyra/runtime/dump_kind.hpp"
#include "lyra/runtime/file.hpp"
#include "lyra/support/data_object.hpp"
#include "lyra/value/integral_words.hpp"

namespace {

using lyra::runtime::DumpCheckpoint;
using lyra::runtime::DumpCode;
using lyra::runtime::DumpScopeKind;
using lyra::runtime::DumpTriggered;
using lyra::runtime::DumpUnknown;
using lyra::runtime::DumpVarDeclaration;
using lyra::runtime::DumpVarKind;
using lyra::runtime::DumpWriter;
using lyra::value::ConstIntegralView;

auto PathOf(const std::string& name) -> std::string {
  return (std::filesystem::temp_directory_path() / name).string();
}

// A declaration as a dump declares it, a vector declared `[width-1:0]` where
// `ranged`.
auto Declared(
    const char* identifier, DumpVarKind kind, std::uint32_t width, bool ranged)
    -> DumpVarDeclaration {
  DumpVarDeclaration variable{
      .identifier = identifier,
      .kind = kind,
      .width = width,
      .range = std::nullopt,
      .port = std::nullopt};
  if (ranged) {
    variable.range = lyra::support::Range{
        .left = static_cast<std::int64_t>(width) - 1, .right = 0};
  }
  return variable;
}

auto ReadFile(const std::string& path) -> std::string {
  std::ifstream file(path);
  std::stringstream text;
  text << file.rdbuf();
  return text.str();
}

// One dump every format is handed: a module with a 4-bit reg, a real and an
// event, and an instance inside it declaring nothing; and a package beside the
// tops (LRM 26.2), with its variable `v` and the static `s` of its function
// `touch`. The reg starts at 0011, goes to x1z0, the event is triggered at
// time 2, `v` and `s` hold 1001 and 10 from the start, and the dump is
// switched off at time 3.
void WriteDump(DumpWriter& writer) {
  const DumpVarDeclaration reg = Declared("a", DumpVarKind::kReg, 4, true);
  const DumpVarDeclaration real = Declared("r", DumpVarKind::kReal, 64, false);
  const DumpVarDeclaration event = Declared("e", DumpVarKind::kEvent, 1, false);

  writer.Timescale(-9);
  writer.Scope(DumpScopeKind::kModule, "Top");
  writer.Var(DumpCode{0}, reg);
  writer.Var(DumpCode{1}, real);
  writer.Var(DumpCode{2}, event);
  writer.Scope(DumpScopeKind::kModule, "u");
  writer.Upscope();
  writer.Upscope();
  writer.Scope(DumpScopeKind::kPackage, "p");
  writer.Var(DumpCode{3}, Declared("v", DumpVarKind::kReg, 4, true));
  writer.Scope(DumpScopeKind::kFunction, "touch");
  writer.Var(DumpCode{4}, Declared("s", DumpVarKind::kReg, 2, true));
  writer.Upscope();
  writer.Upscope();
  writer.EndDefinitions();

  const std::array<std::uint64_t, 1> start_value{0b0011};
  const std::array<std::uint64_t, 1> start_unknown{0};
  const std::array<std::uint64_t, 1> later_value{0b1100};
  const std::array<std::uint64_t, 1> later_unknown{0b1010};
  const std::array<std::uint64_t, 1> v{0b1001};
  const std::array<std::uint64_t, 1> s{0b10};

  writer.Time(0);
  writer.BeginCheckpoint(DumpCheckpoint::kDumpvars);
  writer.Change(
      DumpCode{0},
      ConstIntegralView{
          .value = start_value, .unknown = start_unknown, .width = 4});
  writer.Change(DumpCode{1}, 1.5);
  writer.Change(
      DumpCode{3},
      ConstIntegralView{.value = v, .unknown = start_unknown, .width = 4});
  writer.Change(
      DumpCode{4},
      ConstIntegralView{.value = s, .unknown = start_unknown, .width = 2});
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
  writer.Change(DumpCode{3}, DumpUnknown{});
  writer.Change(DumpCode{4}, DumpUnknown{});
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
      "$upscope $end\n"
      "$upscope $end\n"
      "$scope package p $end\n"
      "$var reg 4 $ v [3:0] $end\n"
      "$scope function touch $end\n"
      "$var reg 2 % s [1:0] $end\n"
      "$upscope $end\n"
      "$upscope $end\n"
      "$enddefinitions $end\n"
      "#0\n"
      "$dumpvars\n"
      "b11 !\n"
      "r1.5 \"\n"
      "b1001 $\n"
      "b10 %\n"
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
      "bx $\n"
      "bx %\n"
      "$end\n");
}

// A package's members are named after the package's name and `::`, as LRM
// 26.3 names them, so none is spelled as a member of a top module of the same
// name would be.
TEST(DumpWriterTest, TextNamesEveryPositionAndItsThing) {
  const std::string path = PathOf("dump.txt");
  WriteDump(*lyra::runtime::OpenTextWriter(path));
  EXPECT_EQ(
      ReadFile(path),
      "module Top\n"
      "module Top.u\n"
      "package p\n"
      "function p::touch\n"
      "0ns $dumpvars\n"
      "0ns Top.a = 0011\n"
      "0ns Top.r = 1.5\n"
      "0ns p::v = 1001\n"
      "0ns p::touch.s = 10\n"
      "1ns Top.a = x1z0\n"
      "2ns Top.e = triggered\n"
      "2ns Top.r = 2.25\n"
      "3ns $dumpoff\n"
      "3ns Top.a = x\n"
      "3ns Top.r = x\n"
      "3ns Top.e = x\n"
      "3ns p::v = x\n"
      "3ns p::touch.s = x\n");
}

// The value libfst's reader finds `handle` holding at `time`, a real written
// after an `r` as a VCD writes it.
auto FstValueAt(fstReaderContext* reader, std::uint64_t time, fstHandle handle)
    -> std::string {
  std::array<char, 64> spelled{};
  return fstReaderGetValueFromHandleAtTime(
      reader, time, handle, spelled.data());
}

// The hierarchy libfst's own reader finds in the FST file at `path`, as the
// VCD declarations libfst renders it to: each scope by its kind and name, and
// each variable by its kind, width, the code its handle is written as and its
// name.
auto FstDeclarations(const std::string& path) -> std::string {
  fstReaderContext* reader = fstReaderOpen(path.c_str());
  if (reader == nullptr) {
    return {};
  }
  const std::string rendered = path + ".declarations";
  {
    const lyra::runtime::OpenedStream out(
        std::fopen(rendered.c_str(), "w"), &std::fclose);
    fstReaderProcessHier(reader, out.get());
  }
  fstReaderClose(reader);
  return ReadFile(rendered);
}

// What libfst's own reader finds in the file: the hierarchy the VCD writer
// declares for the same dump, each thing a variable of its own, and what each
// holds at each time.
TEST(DumpWriterTest, FstReadsBackThroughLibfst) {
  const std::string path = PathOf("dump.fst");
  WriteDump(*lyra::runtime::OpenFstWriter(path));

  const std::string declarations = FstDeclarations(path);
  EXPECT_NE(
      declarations.find(
          "$scope module Top $end\n"
          "$var reg 4 ! a [3:0] $end\n"
          "$var real 64 \" r $end\n"
          "$var event 1 # e $end\n"
          "$scope module u $end\n"
          "$upscope $end\n"
          "$upscope $end\n"
          "$scope package p $end\n"
          "$var reg 4 $ v [3:0] $end\n"
          "$scope function touch $end\n"
          "$var reg 2 % s [1:0] $end\n"
          "$upscope $end\n"
          "$upscope $end\n"
          "$enddefinitions $end\n"),
      std::string::npos)
      << declarations;

  fstReaderContext* reader = fstReaderOpen(path.c_str());
  ASSERT_NE(reader, nullptr);
  EXPECT_EQ(fstReaderGetTimescale(reader), -9);
  EXPECT_EQ(fstReaderGetVarCount(reader), 5U);
  EXPECT_EQ(fstReaderGetAliasCount(reader), 0U);
  EXPECT_EQ(FstValueAt(reader, 0, 1), "0011");
  EXPECT_EQ(FstValueAt(reader, 0, 2), "r1.5");
  EXPECT_EQ(FstValueAt(reader, 0, 4), "1001");
  EXPECT_EQ(FstValueAt(reader, 0, 5), "10");
  EXPECT_EQ(FstValueAt(reader, 1, 1), "x1z0");
  EXPECT_EQ(FstValueAt(reader, 2, 2), "r2.25");
  EXPECT_EQ(FstValueAt(reader, 2, 3), "1");
  EXPECT_EQ(FstValueAt(reader, 3, 1), "xxxx");
  EXPECT_EQ(FstValueAt(reader, 3, 2), "rnan");
  EXPECT_EQ(FstValueAt(reader, 3, 3), "x");
  fstReaderClose(reader);
}

}  // namespace
