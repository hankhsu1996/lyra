#pragma once

#include <array>
#include <compare>
#include <cstddef>
#include <cstdint>
#include <span>
#include <string_view>
#include <variant>

#include "lyra/base/internal_error.hpp"

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

enum class ReturnConvention : std::uint8_t {
  kVoid,
  kInt32,
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

enum class DiagnosticSeverityKind : std::uint8_t {
  kInfo,
  kWarning,
  kError,
};

struct DiagnosticSystemSubroutineInfo {
  DiagnosticSeverityKind severity;
};

enum class FileIOKind : std::uint8_t {
  kOpen,
  kClose,
  kGetc,
  kUngetc,
  kGets,
  kRead,
  kSeek,
  kRewind,
  kTell,
  kEof,
  kError,
  kFlush,
};

struct FileIOSystemSubroutineInfo {
  FileIOKind kind;
};

// LRM 21.3.4: $fgets writes its first arg, $fread writes its first arg, and
// $ferror writes its second arg. The kind alone implies which slot is the
// output destination; consumers branch on `kind` rather than carry a
// per-position direction array.
[[nodiscard]] constexpr auto FileIOHasOutputArg(FileIOKind kind) -> bool {
  return kind == FileIOKind::kGets || kind == FileIOKind::kRead ||
         kind == FileIOKind::kError;
}

// LRM 21.3.4.3 scan family ($sscanf / $fscanf). The kind axis tracks where
// the scanned characters come from. Only kString is implemented today; the
// kFile variant ($fscanf) wires the same scanner core over a file-source
// adapter in a follow-up.
enum class ScanSourceKind : std::uint8_t {
  kString,
};

struct ScanSystemSubroutineInfo {
  ScanSourceKind source;
};

using SystemSubroutineSemantic = std::variant<
    PrintSystemSubroutineInfo, TerminationSystemSubroutineInfo,
    DiagnosticSystemSubroutineInfo, FileIOSystemSubroutineInfo,
    ScanSystemSubroutineInfo>;

struct SystemSubroutineDesc {
  SystemSubroutineId id;
  std::string_view name;
  SystemSubroutineOrigin origin;
  SystemSubroutineKind kind;
  ReturnConvention result_conv;
  ArgCountPolicy arg_policy;
  SystemSubroutineSemantic semantic;
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
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{17},
        .name = "$info",
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 0, .max_args = 255},
        .semantic =
            DiagnosticSystemSubroutineInfo{
                .severity = DiagnosticSeverityKind::kInfo},
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
                .severity = DiagnosticSeverityKind::kWarning},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{19},
        .name = "$error",
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 0, .max_args = 255},
        .semantic =
            DiagnosticSystemSubroutineInfo{
                .severity = DiagnosticSeverityKind::kError},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{20},
        .name = "$fopen",
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kInt32,
        .arg_policy = ArgCountPolicy{.min_args = 1, .max_args = 2},
        .semantic = FileIOSystemSubroutineInfo{.kind = FileIOKind::kOpen},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{21},
        .name = "$fclose",
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 1, .max_args = 1},
        .semantic = FileIOSystemSubroutineInfo{.kind = FileIOKind::kClose},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{22},
        .name = "$fgetc",
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kInt32,
        .arg_policy = ArgCountPolicy{.min_args = 1, .max_args = 1},
        .semantic = FileIOSystemSubroutineInfo{.kind = FileIOKind::kGetc},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{23},
        .name = "$ungetc",
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kInt32,
        .arg_policy = ArgCountPolicy{.min_args = 2, .max_args = 2},
        .semantic = FileIOSystemSubroutineInfo{.kind = FileIOKind::kUngetc},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{24},
        .name = "$fgets",
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kInt32,
        .arg_policy = ArgCountPolicy{.min_args = 2, .max_args = 2},
        .semantic = FileIOSystemSubroutineInfo{.kind = FileIOKind::kGets},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{25},
        .name = "$fread",
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kInt32,
        .arg_policy = ArgCountPolicy{.min_args = 2, .max_args = 2},
        .semantic = FileIOSystemSubroutineInfo{.kind = FileIOKind::kRead},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{26},
        .name = "$fseek",
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kInt32,
        .arg_policy = ArgCountPolicy{.min_args = 3, .max_args = 3},
        .semantic = FileIOSystemSubroutineInfo{.kind = FileIOKind::kSeek},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{27},
        .name = "$rewind",
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kInt32,
        .arg_policy = ArgCountPolicy{.min_args = 1, .max_args = 1},
        .semantic = FileIOSystemSubroutineInfo{.kind = FileIOKind::kRewind},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{28},
        .name = "$ftell",
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kInt32,
        .arg_policy = ArgCountPolicy{.min_args = 1, .max_args = 1},
        .semantic = FileIOSystemSubroutineInfo{.kind = FileIOKind::kTell},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{29},
        .name = "$feof",
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kInt32,
        .arg_policy = ArgCountPolicy{.min_args = 1, .max_args = 1},
        .semantic = FileIOSystemSubroutineInfo{.kind = FileIOKind::kEof},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{30},
        .name = "$ferror",
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kInt32,
        .arg_policy = ArgCountPolicy{.min_args = 2, .max_args = 2},
        .semantic = FileIOSystemSubroutineInfo{.kind = FileIOKind::kError},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{31},
        .name = "$fflush",
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 0, .max_args = 1},
        .semantic = FileIOSystemSubroutineInfo{.kind = FileIOKind::kFlush},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{32},
        .name = "$sscanf",
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
        .kind = SystemSubroutineKind::kFunction,
        .result_conv = ReturnConvention::kInt32,
        .arg_policy = ArgCountPolicy{.min_args = 3, .max_args = 255},
        .semantic = ScanSystemSubroutineInfo{.source = ScanSourceKind::kString},
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

}  // namespace lyra::support
