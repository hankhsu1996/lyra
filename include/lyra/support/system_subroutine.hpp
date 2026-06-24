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

enum class SystemSubroutineOrigin : std::uint8_t {
  kLanguageBuiltin,
  kLyraExtension,
  kExternalExtension,
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
// surrounding operators.
enum class ReturnConvention : std::uint8_t {
  kVoid,
  kInt32,
  kInteger,
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

enum class TerminationKind : std::uint8_t {
  kFinish,
};

struct TerminationSystemSubroutineInfo {
  TerminationKind kind;
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

using SystemSubroutineSemantic = std::variant<
    PrintSystemSubroutineInfo, TerminationSystemSubroutineInfo,
    DiagnosticSystemSubroutineInfo, FileIOSystemSubroutineInfo,
    ScanSystemSubroutineInfo, SFormatSystemSubroutineInfo,
    TimeSystemSubroutineInfo, TimeFormatSystemSubroutineInfo,
    PrintTimescaleSystemSubroutineInfo>;

struct SystemSubroutineDesc {
  SystemSubroutineId id;
  std::string_view name;
  SystemSubroutineOrigin origin;
  SystemSubroutineKind kind;
  ReturnConvention result_conv;
  ArgCountPolicy arg_policy;
  SystemSubroutineSemantic semantic;
  // Invoking this subroutine suspends the calling process ($finish suspends
  // and never resumes; the engine drops the process on the next dispatch,
  // LRM 20.2). Stated as a fact so HIR-to-MIR lowers a suspending call to a
  // suspension point (`mir::AwaitStmt`) rather than inferring it from the
  // subroutine's semantic kind; each backend then realizes the await in its
  // target (C++ `co_await`, LLVM's own mechanism).
  bool suspends = false;
};

namespace detail {

inline constexpr std::array kSystemSubroutines = {
    SystemSubroutineDesc{
        .id = SystemSubroutineId{0},
        .name = "$display",
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
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
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
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
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
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
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
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
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
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
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
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
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
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
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
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
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
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
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
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
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
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
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
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
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
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
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
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
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
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
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
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
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
        .kind = SystemSubroutineKind::kTask,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 0, .max_args = 1},
        .semantic =
            TerminationSystemSubroutineInfo{
                .kind = TerminationKind::kFinish, .default_level = 1},
        .suspends = true,
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{17},
        .name = "$info",
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 0, .max_args = 255},
        .semantic =
            DiagnosticSystemSubroutineInfo{.builtin_fn = BuiltinFn::kEmitInfo},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{18},
        .name = "$warning",
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
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
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 0, .max_args = 255},
        .semantic =
            DiagnosticSystemSubroutineInfo{.builtin_fn = BuiltinFn::kEmitError},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{20},
        .name = "$fopen",
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kInt32,
        .arg_policy = ArgCountPolicy{.min_args = 1, .max_args = 2},
        .semantic =
            FileIOSystemSubroutineInfo{.builtin_fn = BuiltinFn::kFileOpen},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{21},
        .name = "$fclose",
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 1, .max_args = 1},
        .semantic =
            FileIOSystemSubroutineInfo{.builtin_fn = BuiltinFn::kFileClose},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{22},
        .name = "$fgetc",
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kInt32,
        .arg_policy = ArgCountPolicy{.min_args = 1, .max_args = 1},
        .semantic =
            FileIOSystemSubroutineInfo{.builtin_fn = BuiltinFn::kFileGetc},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{23},
        .name = "$ungetc",
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kInt32,
        .arg_policy = ArgCountPolicy{.min_args = 2, .max_args = 2},
        .semantic =
            FileIOSystemSubroutineInfo{.builtin_fn = BuiltinFn::kFileUngetc},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{24},
        .name = "$fgets",
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kInt32,
        .arg_policy = ArgCountPolicy{.min_args = 2, .max_args = 2},
        .semantic =
            FileIOSystemSubroutineInfo{.builtin_fn = BuiltinFn::kFileGets},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{25},
        .name = "$fread",
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
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
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kInt32,
        .arg_policy = ArgCountPolicy{.min_args = 3, .max_args = 3},
        .semantic =
            FileIOSystemSubroutineInfo{.builtin_fn = BuiltinFn::kFileSeek},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{27},
        .name = "$rewind",
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kInt32,
        .arg_policy = ArgCountPolicy{.min_args = 1, .max_args = 1},
        .semantic =
            FileIOSystemSubroutineInfo{.builtin_fn = BuiltinFn::kFileRewind},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{28},
        .name = "$ftell",
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kInt32,
        .arg_policy = ArgCountPolicy{.min_args = 1, .max_args = 1},
        .semantic =
            FileIOSystemSubroutineInfo{.builtin_fn = BuiltinFn::kFileTell},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{29},
        .name = "$feof",
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kInt32,
        .arg_policy = ArgCountPolicy{.min_args = 1, .max_args = 1},
        .semantic =
            FileIOSystemSubroutineInfo{.builtin_fn = BuiltinFn::kFileEof},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{30},
        .name = "$ferror",
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kInt32,
        .arg_policy = ArgCountPolicy{.min_args = 2, .max_args = 2},
        .semantic =
            FileIOSystemSubroutineInfo{.builtin_fn = BuiltinFn::kFileError},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{31},
        .name = "$fflush",
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 0, .max_args = 1},
        .semantic =
            FileIOSystemSubroutineInfo{.builtin_fn = BuiltinFn::kFileFlush},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{32},
        .name = "$sscanf",
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kInteger,
        .arg_policy = ArgCountPolicy{.min_args = 3, .max_args = 255},
        .semantic = ScanSystemSubroutineInfo{.source = ScanSourceKind::kString},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{33},
        .name = "$fscanf",
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kInteger,
        .arg_policy = ArgCountPolicy{.min_args = 3, .max_args = 255},
        .semantic = ScanSystemSubroutineInfo{.source = ScanSourceKind::kFile},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{34},
        .name = "$swrite",
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
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
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
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
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
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
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
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
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
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
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
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
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
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
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
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
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
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
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
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
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kTime64,
        .arg_policy = ArgCountPolicy{.min_args = 0, .max_args = 0},
        .semantic = TimeSystemSubroutineInfo{.kind = TimeKind::kTime},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{45},
        .name = "$stime",
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kInt32,
        .arg_policy = ArgCountPolicy{.min_args = 0, .max_args = 0},
        .semantic = TimeSystemSubroutineInfo{.kind = TimeKind::kStime},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{46},
        .name = "$realtime",
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kRealTime,
        .arg_policy = ArgCountPolicy{.min_args = 0, .max_args = 0},
        .semantic = TimeSystemSubroutineInfo{.kind = TimeKind::kRealtime},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{47},
        .name = "$fstrobe",
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
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
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
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
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
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
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
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
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
        .kind = SystemSubroutineKind::kTask,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 0, .max_args = 4},
        .semantic = TimeFormatSystemSubroutineInfo{},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{52},
        .name = "$printtimescale",
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
        .kind = SystemSubroutineKind::kTask,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 0, .max_args = 0},
        .semantic = PrintTimescaleSystemSubroutineInfo{},
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

}  // namespace lyra::support
