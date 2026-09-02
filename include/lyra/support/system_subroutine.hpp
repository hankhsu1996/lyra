#pragma once

#include <array>
#include <compare>
#include <cstddef>
#include <cstdint>
#include <span>
#include <string_view>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::support {

struct SystemSubroutineId {
  std::uint16_t value;

  auto operator<=>(const SystemSubroutineId&) const
      -> std::strong_ordering = default;
};

enum class SystemSubroutineKind : std::uint8_t {
  kTask,
  kFunction,
};

// `kInt32` is SV `int` (2-state); `kInteger` is SV `integer` (4-state).
// Both surface as 32-bit signed, but slang's propagated conversions in
// arithmetic context align on the 4-state form, so functions whose LRM
// example types the return as `integer` (e.g. `$sscanf`, `$fscanf` per
// LRM 21.3.4.3) must pick `kInteger` to avoid state-axis mismatch on
// surrounding operators. `kIntUnsigned` is SV `int unsigned`, which the
// standard states in the function's own prototype rather than leaving to
// context (LRM 18.13.1).
enum class ReturnConvention : std::uint8_t {
  kVoid,
  kInt32,
  kIntUnsigned,
  kInteger,
  kBit,
  kString,
  kTime64,
  kRealTime,
};

struct ArgCountPolicy {
  std::uint16_t min_args;
  std::uint16_t max_args;

  [[nodiscard]] constexpr auto Accepts(std::size_t count) const -> bool {
    return count >= min_args && count <= max_args;
  }
};

enum class PrintRadix : std::uint8_t {
  kDecimal,
  kBinary,
  kOctal,
  kHex,
};

enum class PrintSinkKind : std::uint8_t {
  kStdout,
  kFile,
};

struct PrintSystemSubroutineInfo {
  PrintRadix radix;
  bool append_newline;
  bool is_strobe;
  PrintSinkKind sink_kind;
};

// LRM 20.2 simulation control tasks ($finish, $stop, $exit). All three end a
// non-interactive run through the same runtime request; `default_level` is the
// diagnostic-message selector (0, 1, or 2, Table 20-1) a call takes when it
// names none.
struct TerminationSystemSubroutineInfo {
  int default_level;
};

// LRM 20.10 severity-fixed diagnostic tasks. The MIR-side identity is the
// `BuiltinFn` Emit method on the diagnostic broker; the descriptor stores it
// directly so the lowering reads `info.builtin_fn` and routes the runtime
// call without a parallel severity enum.
struct DiagnosticSystemSubroutineInfo {
  BuiltinFn builtin_fn;
};

// The MIR-side callee identity for the SV system task. The same closed
// namespace `BuiltinFn` carries every recognized runtime entry; descriptors
// store it directly so lowering reads `info.builtin_fn` and renders without
// a parallel SV-kind axis.
struct FileIOSystemSubroutineInfo {
  BuiltinFn builtin_fn;
};

// LRM 21.3.4.3 scan family ($sscanf / $fscanf). The kind axis tracks where
// the scanned characters come from; the scanner core is shared and only
// the source-adapter and runtime entry differ.
enum class ScanSourceKind : std::uint8_t {
  kString,
  kFile,
};

struct ScanSystemSubroutineInfo {
  ScanSourceKind source;
};

// LRM 21.3.3 string-format family. The conversion engine is shared with
// $display / $write; this descriptor only carries the axes the lowering
// pass needs to dispatch correctly: the default radix for the auto-format
// $swrite* variants, whether an explicit format string is expected
// (false for $swrite*, true for $sformat / $sformatf), and whether the
// call carries an output-var arg (true for $sformat / $swrite*, false
// for $sformatf which yields its result as the call's rvalue).
struct SFormatSystemSubroutineInfo {
  PrintRadix radix;
  bool expects_format_string;
  bool has_output_arg;
};

// LRM 20.3 simulation-time read functions. The kind selects $time (64-bit
// integer, rounded), $stime (low 32 bits) or $realtime (real, fraction kept);
// all three scale the current time from the design-global tick to the calling
// scope's time unit, so they share one runtime scaling core.
enum class TimeKind : std::uint8_t {
  kTime,
  kStime,
  kRealtime,
};

struct TimeSystemSubroutineInfo {
  TimeKind kind;
};

// LRM 20.4.3: `$timeformat` sets the design-wide `%t` display unit, precision,
// suffix, and minimum field width (or restores the defaults when called with no
// arguments).
struct TimeFormatSystemSubroutineInfo {};

// LRM 20.4.2: `$printtimescale` prints a design element's time unit and
// precision. Only the no-argument (current scope) form is modeled.
struct PrintTimescaleSystemSubroutineInfo {};

// LRM 21.6 command-line plusargs: `$test$plusargs` tests whether any plusarg
// starts with the user-supplied prefix; `$value$plusargs` also converts the
// matching plusarg's remainder per the format specifier and writes it to the
// output lvalue. The kind axis is what the HIR-to-MIR dispatcher branches on
// to pick the runtime helper.
enum class PlusargsKind : std::uint8_t { kTest, kValue };
struct PlusargsSystemSubroutineInfo {
  PlusargsKind kind;
};

// LRM 21.4 / 21.5 memory file transfer. `$readmem{h,b}` load a memory from a
// text file; `$writemem{h,b}` dump one to a file the load reads back. `base` is
// the digit radix (16 for the `h` forms, 2 for the `b` forms), emitted as a
// literal operand of the runtime call so each word is read / written at that
// radix. `direction` selects load vs dump: a load writes its memory argument
// (an output argument, copy-out desugared), a dump reads it (an input
// argument), which is the whole difference the statement-form lowering branches
// on.
enum class MemFileDirection : std::uint8_t { kLoad, kStore };
struct MemFileSystemSubroutineInfo {
  unsigned base;
  MemFileDirection direction;
};

// LRM 20.9 bit vector system functions. Every one of them counts the operand's
// bits that carry one of a set of four-state values, then reports the count or
// a property of it; the two axes below are the whole difference between them.
//
// `$countbits` names the set at the call site, one control bit per trailing
// argument (LRM 20.9 spells each as a literal); the other four fix it, and take
// the operand alone.
enum class BitValueSet : std::uint8_t {
  kControlArguments,
  kOnes,
  kUnknowns,
};

// What LRM 20.9 reports about the count: the count itself (`$countbits`,
// `$countones`), or whether it is exactly one (`$onehot`), at most one
// (`$onehot0`), or non-zero (`$isunknown`).
enum class BitCountReading : std::uint8_t {
  kCount,
  kExactlyOne,
  kAtMostOne,
  kAny,
};

struct BitVectorSystemSubroutineInfo {
  BitValueSet values;
  BitCountReading reading;
};

// LRM 20.17.1: `$system` executes its argument through the host's command
// processor as if it had been typed at the terminal and reports what the host
// answered, or, called with no argument, reaches the host with the null
// command, which runs nothing and reports whether a command processor exists.
// The standard admits it as either a task or a function, which is why it is the
// one subroutine here typed a task that still answers with a value; a call
// written as a statement discards that value like any other.
struct HostCommandSystemSubroutineInfo {};

// LRM 18.13.1 -- 18.13.2 unconstrained random number functions. Both draw from
// the calling process's own generator, which is what makes the values a process
// observes independent of the order in which processes run (LRM 18.14.2). The
// kind axis is what the lowering branches on to pick the runtime entry, since
// the two differ in what they do with their arguments rather than in where the
// bits come from: `$urandom`'s optional argument re-seeds the generator before
// the draw, while `$urandom_range`'s two bound the result.
enum class RandomKind : std::uint8_t { kUrandom, kUrandomRange };
struct RandomSystemSubroutineInfo {
  RandomKind kind;
};

// LRM 20.14 probabilistic distribution functions. These carry their own
// generator state instead of drawing from a process: the seed is an `inout`
// argument, so a call both reads the design's seed variable and writes the
// advanced seed back, and the same seed always answers with the same value
// (LRM 20.14.2). That is what puts them outside the random stability model of
// LRM 18.14, whose list does not include them. The generation algorithm is
// itself part of the standard (LRM Annex N), so a seeded stream is the same one
// on every implementation. `$random` belongs here because Annex N's Table N.1
// defines it as a uniform draw over the whole signed range; its seed is
// optional, and a call that omits one has no stream of its own to advance.
enum class DistributionKind : std::uint8_t {
  kRandom,
  kUniform,
  kNormal,
  kExponential,
  kPoisson,
  kChiSquare,
  kT,
  kErlang,
};
struct DistributionSystemSubroutineInfo {
  DistributionKind kind;
};

using SystemSubroutineSemantic = std::variant<
    PrintSystemSubroutineInfo, TerminationSystemSubroutineInfo,
    DiagnosticSystemSubroutineInfo, FileIOSystemSubroutineInfo,
    ScanSystemSubroutineInfo, SFormatSystemSubroutineInfo,
    TimeSystemSubroutineInfo, TimeFormatSystemSubroutineInfo,
    PrintTimescaleSystemSubroutineInfo, PlusargsSystemSubroutineInfo,
    MemFileSystemSubroutineInfo, BitVectorSystemSubroutineInfo,
    HostCommandSystemSubroutineInfo, RandomSystemSubroutineInfo,
    DistributionSystemSubroutineInfo>;

struct SystemSubroutineDesc {
  SystemSubroutineId id;
  std::string_view name;
  SystemSubroutineKind kind;
  ReturnConvention result_conv;
  ArgCountPolicy arg_policy;
  SystemSubroutineSemantic semantic;
  // Invoking this subroutine suspends the calling process ($finish suspends
  // and never resumes; the engine drops the process on the next dispatch,
  // LRM 20.2). Stated as a fact so HIR-to-MIR lowers a suspending call through
  // an awaited expression rather than inferring it from the subroutine's
  // semantic kind; each backend then realizes the await in its target (C++
  // `co_await`, LLVM's own mechanism).
  bool suspends = false;
};

namespace detail {

inline constexpr std::array kSystemSubroutines = {
    SystemSubroutineDesc{
        .id = SystemSubroutineId{0},
        .name = "$display",
        .kind = SystemSubroutineKind::kTask,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 0, .max_args = 255},
        .semantic =
            PrintSystemSubroutineInfo{
                .radix = PrintRadix::kDecimal,
                .append_newline = true,
                .is_strobe = false,
                .sink_kind = PrintSinkKind::kStdout},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{1},
        .name = "$displayb",
        .kind = SystemSubroutineKind::kTask,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 0, .max_args = 255},
        .semantic =
            PrintSystemSubroutineInfo{
                .radix = PrintRadix::kBinary,
                .append_newline = true,
                .is_strobe = false,
                .sink_kind = PrintSinkKind::kStdout},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{2},
        .name = "$displayh",
        .kind = SystemSubroutineKind::kTask,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 0, .max_args = 255},
        .semantic =
            PrintSystemSubroutineInfo{
                .radix = PrintRadix::kHex,
                .append_newline = true,
                .is_strobe = false,
                .sink_kind = PrintSinkKind::kStdout},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{3},
        .name = "$displayo",
        .kind = SystemSubroutineKind::kTask,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 0, .max_args = 255},
        .semantic =
            PrintSystemSubroutineInfo{
                .radix = PrintRadix::kOctal,
                .append_newline = true,
                .is_strobe = false,
                .sink_kind = PrintSinkKind::kStdout},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{4},
        .name = "$write",
        .kind = SystemSubroutineKind::kTask,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 0, .max_args = 255},
        .semantic =
            PrintSystemSubroutineInfo{
                .radix = PrintRadix::kDecimal,
                .append_newline = false,
                .is_strobe = false,
                .sink_kind = PrintSinkKind::kStdout},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{5},
        .name = "$writeb",
        .kind = SystemSubroutineKind::kTask,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 0, .max_args = 255},
        .semantic =
            PrintSystemSubroutineInfo{
                .radix = PrintRadix::kBinary,
                .append_newline = false,
                .is_strobe = false,
                .sink_kind = PrintSinkKind::kStdout},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{6},
        .name = "$writeh",
        .kind = SystemSubroutineKind::kTask,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 0, .max_args = 255},
        .semantic =
            PrintSystemSubroutineInfo{
                .radix = PrintRadix::kHex,
                .append_newline = false,
                .is_strobe = false,
                .sink_kind = PrintSinkKind::kStdout},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{7},
        .name = "$writeo",
        .kind = SystemSubroutineKind::kTask,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 0, .max_args = 255},
        .semantic =
            PrintSystemSubroutineInfo{
                .radix = PrintRadix::kOctal,
                .append_newline = false,
                .is_strobe = false,
                .sink_kind = PrintSinkKind::kStdout},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{8},
        .name = "$fdisplay",
        .kind = SystemSubroutineKind::kTask,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 1, .max_args = 255},
        .semantic =
            PrintSystemSubroutineInfo{
                .radix = PrintRadix::kDecimal,
                .append_newline = true,
                .is_strobe = false,
                .sink_kind = PrintSinkKind::kFile},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{9},
        .name = "$fdisplayb",
        .kind = SystemSubroutineKind::kTask,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 1, .max_args = 255},
        .semantic =
            PrintSystemSubroutineInfo{
                .radix = PrintRadix::kBinary,
                .append_newline = true,
                .is_strobe = false,
                .sink_kind = PrintSinkKind::kFile},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{10},
        .name = "$fdisplayh",
        .kind = SystemSubroutineKind::kTask,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 1, .max_args = 255},
        .semantic =
            PrintSystemSubroutineInfo{
                .radix = PrintRadix::kHex,
                .append_newline = true,
                .is_strobe = false,
                .sink_kind = PrintSinkKind::kFile},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{11},
        .name = "$fdisplayo",
        .kind = SystemSubroutineKind::kTask,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 1, .max_args = 255},
        .semantic =
            PrintSystemSubroutineInfo{
                .radix = PrintRadix::kOctal,
                .append_newline = true,
                .is_strobe = false,
                .sink_kind = PrintSinkKind::kFile},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{12},
        .name = "$fwrite",
        .kind = SystemSubroutineKind::kTask,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 1, .max_args = 255},
        .semantic =
            PrintSystemSubroutineInfo{
                .radix = PrintRadix::kDecimal,
                .append_newline = false,
                .is_strobe = false,
                .sink_kind = PrintSinkKind::kFile},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{13},
        .name = "$fwriteb",
        .kind = SystemSubroutineKind::kTask,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 1, .max_args = 255},
        .semantic =
            PrintSystemSubroutineInfo{
                .radix = PrintRadix::kBinary,
                .append_newline = false,
                .is_strobe = false,
                .sink_kind = PrintSinkKind::kFile},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{14},
        .name = "$fwriteh",
        .kind = SystemSubroutineKind::kTask,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 1, .max_args = 255},
        .semantic =
            PrintSystemSubroutineInfo{
                .radix = PrintRadix::kHex,
                .append_newline = false,
                .is_strobe = false,
                .sink_kind = PrintSinkKind::kFile},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{15},
        .name = "$fwriteo",
        .kind = SystemSubroutineKind::kTask,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 1, .max_args = 255},
        .semantic =
            PrintSystemSubroutineInfo{
                .radix = PrintRadix::kOctal,
                .append_newline = false,
                .is_strobe = false,
                .sink_kind = PrintSinkKind::kFile},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{16},
        .name = "$finish",
        .kind = SystemSubroutineKind::kTask,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 0, .max_args = 1},
        .semantic = TerminationSystemSubroutineInfo{.default_level = 1},
        .suspends = true,
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{17},
        .name = "$info",
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 0, .max_args = 255},
        .semantic =
            DiagnosticSystemSubroutineInfo{.builtin_fn = BuiltinFn::kEmitInfo},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{18},
        .name = "$warning",
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 0, .max_args = 255},
        .semantic =
            DiagnosticSystemSubroutineInfo{
                .builtin_fn = BuiltinFn::kEmitWarning},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{19},
        .name = "$error",
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 0, .max_args = 255},
        .semantic =
            DiagnosticSystemSubroutineInfo{.builtin_fn = BuiltinFn::kEmitError},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{20},
        .name = "$fopen",
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kInt32,
        .arg_policy = ArgCountPolicy{.min_args = 1, .max_args = 2},
        .semantic =
            FileIOSystemSubroutineInfo{.builtin_fn = BuiltinFn::kFileOpen},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{21},
        .name = "$fclose",
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 1, .max_args = 1},
        .semantic =
            FileIOSystemSubroutineInfo{.builtin_fn = BuiltinFn::kFileClose},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{22},
        .name = "$fgetc",
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kInt32,
        .arg_policy = ArgCountPolicy{.min_args = 1, .max_args = 1},
        .semantic =
            FileIOSystemSubroutineInfo{.builtin_fn = BuiltinFn::kFileGetc},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{23},
        .name = "$ungetc",
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kInt32,
        .arg_policy = ArgCountPolicy{.min_args = 2, .max_args = 2},
        .semantic =
            FileIOSystemSubroutineInfo{.builtin_fn = BuiltinFn::kFileUngetc},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{24},
        .name = "$fgets",
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kInt32,
        .arg_policy = ArgCountPolicy{.min_args = 2, .max_args = 2},
        .semantic =
            FileIOSystemSubroutineInfo{.builtin_fn = BuiltinFn::kFileGets},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{25},
        .name = "$fread",
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kInt32,
        // LRM 21.3.4.4: integral form has 2 args; memory form supports
        // (mem, fd), (mem, fd, start), (mem, fd, start, count), and the
        // (mem, fd, , count) comma-elision shape -- all 2..4 positional.
        .arg_policy = ArgCountPolicy{.min_args = 2, .max_args = 4},
        .semantic =
            FileIOSystemSubroutineInfo{.builtin_fn = BuiltinFn::kFileRead},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{26},
        .name = "$fseek",
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kInt32,
        .arg_policy = ArgCountPolicy{.min_args = 3, .max_args = 3},
        .semantic =
            FileIOSystemSubroutineInfo{.builtin_fn = BuiltinFn::kFileSeek},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{27},
        .name = "$rewind",
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kInt32,
        .arg_policy = ArgCountPolicy{.min_args = 1, .max_args = 1},
        .semantic =
            FileIOSystemSubroutineInfo{.builtin_fn = BuiltinFn::kFileRewind},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{28},
        .name = "$ftell",
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kInt32,
        .arg_policy = ArgCountPolicy{.min_args = 1, .max_args = 1},
        .semantic =
            FileIOSystemSubroutineInfo{.builtin_fn = BuiltinFn::kFileTell},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{29},
        .name = "$feof",
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kInt32,
        .arg_policy = ArgCountPolicy{.min_args = 1, .max_args = 1},
        .semantic =
            FileIOSystemSubroutineInfo{.builtin_fn = BuiltinFn::kFileEof},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{30},
        .name = "$ferror",
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kInt32,
        .arg_policy = ArgCountPolicy{.min_args = 2, .max_args = 2},
        .semantic =
            FileIOSystemSubroutineInfo{.builtin_fn = BuiltinFn::kFileError},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{31},
        .name = "$fflush",
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 0, .max_args = 1},
        .semantic =
            FileIOSystemSubroutineInfo{.builtin_fn = BuiltinFn::kFileFlush},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{32},
        .name = "$sscanf",
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kInteger,
        .arg_policy = ArgCountPolicy{.min_args = 3, .max_args = 255},
        .semantic = ScanSystemSubroutineInfo{.source = ScanSourceKind::kString},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{33},
        .name = "$fscanf",
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kInteger,
        .arg_policy = ArgCountPolicy{.min_args = 3, .max_args = 255},
        .semantic = ScanSystemSubroutineInfo{.source = ScanSourceKind::kFile},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{34},
        .name = "$swrite",
        .kind = SystemSubroutineKind::kTask,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 1, .max_args = 255},
        .semantic =
            SFormatSystemSubroutineInfo{
                .radix = PrintRadix::kDecimal,
                .expects_format_string = false,
                .has_output_arg = true},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{35},
        .name = "$swriteb",
        .kind = SystemSubroutineKind::kTask,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 1, .max_args = 255},
        .semantic =
            SFormatSystemSubroutineInfo{
                .radix = PrintRadix::kBinary,
                .expects_format_string = false,
                .has_output_arg = true},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{36},
        .name = "$swriteh",
        .kind = SystemSubroutineKind::kTask,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 1, .max_args = 255},
        .semantic =
            SFormatSystemSubroutineInfo{
                .radix = PrintRadix::kHex,
                .expects_format_string = false,
                .has_output_arg = true},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{37},
        .name = "$swriteo",
        .kind = SystemSubroutineKind::kTask,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 1, .max_args = 255},
        .semantic =
            SFormatSystemSubroutineInfo{
                .radix = PrintRadix::kOctal,
                .expects_format_string = false,
                .has_output_arg = true},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{38},
        .name = "$sformat",
        .kind = SystemSubroutineKind::kTask,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 2, .max_args = 255},
        .semantic =
            SFormatSystemSubroutineInfo{
                .radix = PrintRadix::kDecimal,
                .expects_format_string = true,
                .has_output_arg = true},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{39},
        .name = "$sformatf",
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kString,
        .arg_policy = ArgCountPolicy{.min_args = 1, .max_args = 255},
        .semantic =
            SFormatSystemSubroutineInfo{
                .radix = PrintRadix::kDecimal,
                .expects_format_string = true,
                .has_output_arg = false},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{40},
        .name = "$strobe",
        .kind = SystemSubroutineKind::kTask,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 0, .max_args = 255},
        .semantic =
            PrintSystemSubroutineInfo{
                .radix = PrintRadix::kDecimal,
                .append_newline = true,
                .is_strobe = true,
                .sink_kind = PrintSinkKind::kStdout},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{41},
        .name = "$strobeb",
        .kind = SystemSubroutineKind::kTask,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 0, .max_args = 255},
        .semantic =
            PrintSystemSubroutineInfo{
                .radix = PrintRadix::kBinary,
                .append_newline = true,
                .is_strobe = true,
                .sink_kind = PrintSinkKind::kStdout},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{42},
        .name = "$strobeh",
        .kind = SystemSubroutineKind::kTask,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 0, .max_args = 255},
        .semantic =
            PrintSystemSubroutineInfo{
                .radix = PrintRadix::kHex,
                .append_newline = true,
                .is_strobe = true,
                .sink_kind = PrintSinkKind::kStdout},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{43},
        .name = "$strobeo",
        .kind = SystemSubroutineKind::kTask,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 0, .max_args = 255},
        .semantic =
            PrintSystemSubroutineInfo{
                .radix = PrintRadix::kOctal,
                .append_newline = true,
                .is_strobe = true,
                .sink_kind = PrintSinkKind::kStdout},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{44},
        .name = "$time",
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kTime64,
        .arg_policy = ArgCountPolicy{.min_args = 0, .max_args = 0},
        .semantic = TimeSystemSubroutineInfo{.kind = TimeKind::kTime},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{45},
        .name = "$stime",
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kInt32,
        .arg_policy = ArgCountPolicy{.min_args = 0, .max_args = 0},
        .semantic = TimeSystemSubroutineInfo{.kind = TimeKind::kStime},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{46},
        .name = "$realtime",
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kRealTime,
        .arg_policy = ArgCountPolicy{.min_args = 0, .max_args = 0},
        .semantic = TimeSystemSubroutineInfo{.kind = TimeKind::kRealtime},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{47},
        .name = "$fstrobe",
        .kind = SystemSubroutineKind::kTask,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 1, .max_args = 255},
        .semantic =
            PrintSystemSubroutineInfo{
                .radix = PrintRadix::kDecimal,
                .append_newline = true,
                .is_strobe = true,
                .sink_kind = PrintSinkKind::kFile},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{48},
        .name = "$fstrobeb",
        .kind = SystemSubroutineKind::kTask,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 1, .max_args = 255},
        .semantic =
            PrintSystemSubroutineInfo{
                .radix = PrintRadix::kBinary,
                .append_newline = true,
                .is_strobe = true,
                .sink_kind = PrintSinkKind::kFile},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{49},
        .name = "$fstrobeh",
        .kind = SystemSubroutineKind::kTask,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 1, .max_args = 255},
        .semantic =
            PrintSystemSubroutineInfo{
                .radix = PrintRadix::kHex,
                .append_newline = true,
                .is_strobe = true,
                .sink_kind = PrintSinkKind::kFile},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{50},
        .name = "$fstrobeo",
        .kind = SystemSubroutineKind::kTask,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 1, .max_args = 255},
        .semantic =
            PrintSystemSubroutineInfo{
                .radix = PrintRadix::kOctal,
                .append_newline = true,
                .is_strobe = true,
                .sink_kind = PrintSinkKind::kFile},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{51},
        .name = "$timeformat",
        .kind = SystemSubroutineKind::kTask,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 0, .max_args = 4},
        .semantic = TimeFormatSystemSubroutineInfo{},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{52},
        .name = "$printtimescale",
        .kind = SystemSubroutineKind::kTask,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 0, .max_args = 0},
        .semantic = PrintTimescaleSystemSubroutineInfo{},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{53},
        .name = "$fatal",
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 0, .max_args = 255},
        .semantic =
            DiagnosticSystemSubroutineInfo{.builtin_fn = BuiltinFn::kEmitFatal},
        .suspends = true,
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{54},
        .name = "$test$plusargs",
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kInt32,
        .arg_policy = ArgCountPolicy{.min_args = 1, .max_args = 1},
        .semantic = PlusargsSystemSubroutineInfo{.kind = PlusargsKind::kTest},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{55},
        .name = "$value$plusargs",
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kInt32,
        .arg_policy = ArgCountPolicy{.min_args = 2, .max_args = 2},
        .semantic = PlusargsSystemSubroutineInfo{.kind = PlusargsKind::kValue},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{56},
        .name = "$readmemh",
        .kind = SystemSubroutineKind::kTask,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 2, .max_args = 4},
        .semantic =
            MemFileSystemSubroutineInfo{
                .base = 16, .direction = MemFileDirection::kLoad},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{57},
        .name = "$readmemb",
        .kind = SystemSubroutineKind::kTask,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 2, .max_args = 4},
        .semantic =
            MemFileSystemSubroutineInfo{
                .base = 2, .direction = MemFileDirection::kLoad},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{58},
        .name = "$writememh",
        .kind = SystemSubroutineKind::kTask,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 2, .max_args = 4},
        .semantic =
            MemFileSystemSubroutineInfo{
                .base = 16, .direction = MemFileDirection::kStore},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{59},
        .name = "$writememb",
        .kind = SystemSubroutineKind::kTask,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 2, .max_args = 4},
        .semantic =
            MemFileSystemSubroutineInfo{
                .base = 2, .direction = MemFileDirection::kStore},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{60},
        .name = "$countbits",
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kInt32,
        .arg_policy = ArgCountPolicy{.min_args = 2, .max_args = 255},
        .semantic =
            BitVectorSystemSubroutineInfo{
                .values = BitValueSet::kControlArguments,
                .reading = BitCountReading::kCount},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{61},
        .name = "$countones",
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kInt32,
        .arg_policy = ArgCountPolicy{.min_args = 1, .max_args = 1},
        .semantic =
            BitVectorSystemSubroutineInfo{
                .values = BitValueSet::kOnes,
                .reading = BitCountReading::kCount},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{62},
        .name = "$onehot",
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kBit,
        .arg_policy = ArgCountPolicy{.min_args = 1, .max_args = 1},
        .semantic =
            BitVectorSystemSubroutineInfo{
                .values = BitValueSet::kOnes,
                .reading = BitCountReading::kExactlyOne},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{63},
        .name = "$onehot0",
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kBit,
        .arg_policy = ArgCountPolicy{.min_args = 1, .max_args = 1},
        .semantic =
            BitVectorSystemSubroutineInfo{
                .values = BitValueSet::kOnes,
                .reading = BitCountReading::kAtMostOne},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{64},
        .name = "$isunknown",
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kBit,
        .arg_policy = ArgCountPolicy{.min_args = 1, .max_args = 1},
        .semantic =
            BitVectorSystemSubroutineInfo{
                .values = BitValueSet::kUnknowns,
                .reading = BitCountReading::kAny},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{65},
        .name = "$system",
        .kind = SystemSubroutineKind::kTask,
        .result_conv = ReturnConvention::kInt32,
        .arg_policy = ArgCountPolicy{.min_args = 0, .max_args = 1},
        .semantic = HostCommandSystemSubroutineInfo{},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{66},
        .name = "$urandom",
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kIntUnsigned,
        .arg_policy = ArgCountPolicy{.min_args = 0, .max_args = 1},
        .semantic = RandomSystemSubroutineInfo{.kind = RandomKind::kUrandom},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{67},
        .name = "$urandom_range",
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kIntUnsigned,
        .arg_policy = ArgCountPolicy{.min_args = 1, .max_args = 2},
        .semantic =
            RandomSystemSubroutineInfo{.kind = RandomKind::kUrandomRange},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{68},
        .name = "$random",
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kInt32,
        .arg_policy = ArgCountPolicy{.min_args = 0, .max_args = 1},
        .semantic =
            DistributionSystemSubroutineInfo{.kind = DistributionKind::kRandom},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{69},
        .name = "$dist_uniform",
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kInt32,
        .arg_policy = ArgCountPolicy{.min_args = 3, .max_args = 3},
        .semantic =
            DistributionSystemSubroutineInfo{
                .kind = DistributionKind::kUniform},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{70},
        .name = "$dist_normal",
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kInt32,
        .arg_policy = ArgCountPolicy{.min_args = 3, .max_args = 3},
        .semantic =
            DistributionSystemSubroutineInfo{.kind = DistributionKind::kNormal},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{71},
        .name = "$dist_exponential",
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kInt32,
        .arg_policy = ArgCountPolicy{.min_args = 2, .max_args = 2},
        .semantic =
            DistributionSystemSubroutineInfo{
                .kind = DistributionKind::kExponential},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{72},
        .name = "$dist_poisson",
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kInt32,
        .arg_policy = ArgCountPolicy{.min_args = 2, .max_args = 2},
        .semantic =
            DistributionSystemSubroutineInfo{
                .kind = DistributionKind::kPoisson},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{73},
        .name = "$dist_chi_square",
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kInt32,
        .arg_policy = ArgCountPolicy{.min_args = 2, .max_args = 2},
        .semantic =
            DistributionSystemSubroutineInfo{
                .kind = DistributionKind::kChiSquare},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{74},
        .name = "$dist_t",
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kInt32,
        .arg_policy = ArgCountPolicy{.min_args = 2, .max_args = 2},
        .semantic =
            DistributionSystemSubroutineInfo{.kind = DistributionKind::kT},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{75},
        .name = "$dist_erlang",
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kInt32,
        .arg_policy = ArgCountPolicy{.min_args = 3, .max_args = 3},
        .semantic =
            DistributionSystemSubroutineInfo{.kind = DistributionKind::kErlang},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{76},
        .name = "$stop",
        .kind = SystemSubroutineKind::kTask,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 0, .max_args = 1},
        .semantic = TerminationSystemSubroutineInfo{.default_level = 1},
        .suspends = true,
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{77},
        .name = "$exit",
        .kind = SystemSubroutineKind::kTask,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 0, .max_args = 0},
        .semantic = TerminationSystemSubroutineInfo{.default_level = 1},
        .suspends = true,
    },
};

}  // namespace detail

[[nodiscard]] inline auto FindSystemSubroutine(std::string_view name)
    -> const SystemSubroutineDesc* {
  for (const auto& desc : detail::kSystemSubroutines) {
    if (desc.name == name) {
      return &desc;
    }
  }
  return nullptr;
}

[[nodiscard]] inline auto LookupSystemSubroutine(SystemSubroutineId id)
    -> const SystemSubroutineDesc& {
  const std::span<const SystemSubroutineDesc> view{detail::kSystemSubroutines};
  if (id.value >= view.size()) {
    throw InternalError(
        "LookupSystemSubroutine: SystemSubroutineId out of range");
  }
  return view[id.value];
}

[[nodiscard]] inline auto GetPrintInfo(const SystemSubroutineDesc& desc)
    -> const PrintSystemSubroutineInfo* {
  return std::get_if<PrintSystemSubroutineInfo>(&desc.semantic);
}

[[nodiscard]] inline auto GetDiagnosticInfo(const SystemSubroutineDesc& desc)
    -> const DiagnosticSystemSubroutineInfo* {
  return std::get_if<DiagnosticSystemSubroutineInfo>(&desc.semantic);
}

[[nodiscard]] inline auto GetFileIOInfo(const SystemSubroutineDesc& desc)
    -> const FileIOSystemSubroutineInfo* {
  return std::get_if<FileIOSystemSubroutineInfo>(&desc.semantic);
}

[[nodiscard]] inline auto GetScanInfo(const SystemSubroutineDesc& desc)
    -> const ScanSystemSubroutineInfo* {
  return std::get_if<ScanSystemSubroutineInfo>(&desc.semantic);
}

[[nodiscard]] inline auto GetSFormatInfo(const SystemSubroutineDesc& desc)
    -> const SFormatSystemSubroutineInfo* {
  return std::get_if<SFormatSystemSubroutineInfo>(&desc.semantic);
}

[[nodiscard]] inline auto GetTimeInfo(const SystemSubroutineDesc& desc)
    -> const TimeSystemSubroutineInfo* {
  return std::get_if<TimeSystemSubroutineInfo>(&desc.semantic);
}

[[nodiscard]] inline auto GetPlusargsInfo(const SystemSubroutineDesc& desc)
    -> const PlusargsSystemSubroutineInfo* {
  return std::get_if<PlusargsSystemSubroutineInfo>(&desc.semantic);
}

}  // namespace lyra::support
