// The VPI routines of LRM clause 38, linked into the program and resolved
// against a user's foreign code by name, as the DPI-C surface is. The
// declarations are the standard's own (Annex K), which holds each definition
// here to its prototype. A routine taking a variable argument list is C, since
// only C reads one, and comes here once its arguments are read.
//
// A routine is defined only once what it acts on is: a program calling one
// that is not fails to link, naming it, as a design using a construct Lyra
// does not yet carry out is refused where it is built. That holds of
// `vpi_get_data` and `vpi_put_data` for good, since they exist only for save
// and restart, which Lyra does not have and LRM 38.36.3 lets a tool lack.
// Nothing here throws across the C boundary.

#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <optional>
#include <span>
#include <string>
#include <string_view>

#include "lyra/runtime/ambient_run_context.hpp"
#include "lyra/runtime/command_line.hpp"
#include "lyra/runtime/dpi_context.hpp"
#include "lyra/runtime/file_table.hpp"
#include "lyra/runtime/import_call_chain.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/vpi_status.hpp"
#include "lyra/value/string.hpp"
#include "vpi_user.h"

namespace {

using lyra::runtime::ClearVpiError;
using lyra::runtime::FileTable;
using lyra::runtime::ReportVpiError;

// The run foreign code is reaching, where one is running on this thread.
auto RunningSimulation(std::string_view routine)
    -> lyra::runtime::RuntimeEffects* {
  lyra::runtime::AmbientRunContext* context =
      lyra::runtime::AmbientRunContext::TryCurrent();
  if (context == nullptr) {
    ReportVpiError(
        routine, std::string(routine) + " needs a running simulation");
    return nullptr;
  }
  return &context->Effects();
}

// Channel 1 of a multichannel descriptor is the tool's output (LRM 38.27),
// which foreign code writes through the C library's stream, as it writes
// everything else it prints, so what it prints stays in the order it printed
// it; the design's own output was handed over before foreign code ran.
constexpr PLI_UINT32 kToolOutput = 1U;

// Writes `text` to the tool's output, as the C library's stream holds it.
auto WriteToolOutput(std::string_view text, std::string_view routine) -> bool {
  if (std::fwrite(text.data(), 1, text.size(), stdout) != text.size()) {
    ReportVpiError(routine, "the tool's output refused what was written");
    return false;
  }
  return true;
}

// The characters a routine hands back for a name it answers, kept until the
// routine answers again (LRM 38.26), since the file table may move its own.
auto HandedOutName(std::string_view name) -> PLI_BYTE8* {
  static thread_local std::string kept;
  kept = name;
  return kept.data();
}

}  // namespace

extern "C" {

auto LyraVpiPrint(const char* text, std::size_t length, const char* routine)
    -> PLI_INT32 {
  ClearVpiError();
  if (text == nullptr) {
    ReportVpiError(routine, "the format could not be applied to its arguments");
    return EOF;
  }
  const std::string_view written(text, length);
  return WriteToolOutput(written, routine) ? static_cast<PLI_INT32>(length)
                                           : EOF;
}

// A file descriptor is no channel to write to (LRM 38.28), and a channel not
// open is none either, so nothing is written to any channel unless every one
// named is open.
auto LyraVpiPrintTo(
    PLI_UINT32 mcd, const char* text, std::size_t length, const char* routine)
    -> PLI_INT32 {
  ClearVpiError();
  if (text == nullptr) {
    ReportVpiError(routine, "the format could not be applied to its arguments");
    return EOF;
  }
  if (mcd == 0U || FileTable::IsFileDescriptor(mcd)) {
    ReportVpiError(routine, "the descriptor is no multichannel descriptor");
    return EOF;
  }
  const PLI_UINT32 files = mcd & ~kToolOutput;
  lyra::runtime::RuntimeEffects* run = nullptr;
  if (files != 0U) {
    run = RunningSimulation(routine);
    if (run == nullptr) {
      return EOF;
    }
    if (static_cast<PLI_UINT32>(run->Files().OpenPartOf(files)) != files) {
      ReportVpiError(
          routine, "the descriptor names a channel that is not open");
      return EOF;
    }
  }
  const std::string_view written(text, length);
  if ((mcd & kToolOutput) != 0U && !WriteToolOutput(written, routine)) {
    return EOF;
  }
  if (run != nullptr) {
    run->Files().Write(files, lyra::value::String(written));
  }
  return static_cast<PLI_INT32>(length);
}

auto LyraVpiControl(PLI_INT32 operation, PLI_INT32 finish_number) -> PLI_INT32 {
  using lyra::runtime::ForeignControl;
  ClearVpiError();
  std::optional<ForeignControl::Task> task;
  switch (operation) {
    case vpiFinish:
      task = ForeignControl::Task::kFinish;
      break;
    case vpiStop:
      task = ForeignControl::Task::kStop;
      break;
    case vpiReset:
      ReportVpiError(
          "vpi_control",
          "vpi_control(vpiReset) asks for $reset, which this simulator does "
          "not have");
      return 0;
    case vpiSetInteractiveScope:
      ReportVpiError(
          "vpi_control",
          "vpi_control(vpiSetInteractiveScope) asks for an interactive scope, "
          "and this simulator has no interactive mode");
      return 0;
    default:
      ReportVpiError(
          "vpi_control",
          "vpi_control was asked for an operation the standard does not "
          "define");
      return 0;
  }
  lyra::runtime::RuntimeEffects* run = RunningSimulation("vpi_control");
  if (run == nullptr) {
    return 0;
  }
  if (!lyra::runtime::RequestControl(
          *run,
          ForeignControl{.task = *task, .finish_number = finish_number})) {
    ReportVpiError(
        "vpi_control",
        "vpi_control is answered when a context import returns, and it was "
        "called from no context import's call");
    return 0;
  }
  return 1;
}

auto vpi_chk_error(p_vpi_error_info error_info_p) -> PLI_INT32 {
  return lyra::runtime::CheckVpiError(error_info_p);
}

auto vpi_flush() -> PLI_INT32 {
  ClearVpiError();
  if (std::fflush(stdout) != 0) {
    ReportVpiError("vpi_flush", "the tool's output refused to be flushed");
    return 1;
  }
  return 0;
}

// A file already open, whether `$fopen` or this routine opened it, is answered
// with the descriptor it has (LRM 38.27), rather than opened again.
auto vpi_mcd_open(PLI_BYTE8* fileName) -> PLI_UINT32 {
  ClearVpiError();
  if (fileName == nullptr) {
    ReportVpiError("vpi_mcd_open", "no file name was given");
    return 0U;
  }
  lyra::runtime::RuntimeEffects* run = RunningSimulation("vpi_mcd_open");
  if (run == nullptr) {
    return 0U;
  }
  FileTable& files = run->Files();
  if (const std::int32_t open = files.DescriptorOpenedAs(fileName); open != 0) {
    return static_cast<PLI_UINT32>(open);
  }
  const std::int32_t opened = files.Open(fileName, std::nullopt);
  if (opened == 0) {
    ReportVpiError(
        "vpi_mcd_open",
        std::string("the file '") + fileName + "' could not be opened");
  }
  return static_cast<PLI_UINT32>(opened);
}

// Closes the files `mcd` names, a file descriptor `$fopen` opened among them
// (LRM 38.24), answering 0, or where any named is not closed -- a pre-opened
// file, the tool's output among them, or one not open -- the descriptor of
// those.
auto vpi_mcd_close(PLI_UINT32 mcd) -> PLI_UINT32 {
  ClearVpiError();
  lyra::runtime::RuntimeEffects* run = RunningSimulation("vpi_mcd_close");
  if (run == nullptr) {
    return mcd;
  }
  FileTable& files = run->Files();
  const auto closable = static_cast<PLI_UINT32>(files.ClosablePartOf(mcd));
  if (closable != 0U) {
    files.Close(closable);
  }
  // A file descriptor is closable whole or not at all, so this is the whole of
  // it or nothing.
  const PLI_UINT32 not_closed = mcd & ~closable;
  if (not_closed != 0U) {
    ReportVpiError("vpi_mcd_close", "the descriptor names a file not closed");
  }
  return not_closed;
}

// Flushes the channels `mcd` names (LRM 38.25), answering 0, or nonzero where
// a channel named is not open or refused.
auto vpi_mcd_flush(PLI_UINT32 mcd) -> PLI_INT32 {
  ClearVpiError();
  if (mcd == 0U || FileTable::IsFileDescriptor(mcd)) {
    ReportVpiError("vpi_mcd_flush", "the descriptor is no multichannel one");
    return 1;
  }
  if ((mcd & kToolOutput) != 0U && std::fflush(stdout) != 0) {
    ReportVpiError("vpi_mcd_flush", "the tool's output refused to be flushed");
    return 1;
  }
  if (const PLI_UINT32 files = mcd & ~kToolOutput; files != 0U) {
    lyra::runtime::RuntimeEffects* run = RunningSimulation("vpi_mcd_flush");
    if (run == nullptr) {
      return 1;
    }
    if (static_cast<PLI_UINT32>(run->Files().OpenPartOf(files)) != files) {
      ReportVpiError(
          "vpi_mcd_flush", "the descriptor names a channel that is not open");
      return 1;
    }
    run->Files().Flush(files);
  }
  return 0;
}

// The name of the file one channel, or a file descriptor `$fopen` opened, was
// opened under (LRM 38.26).
auto vpi_mcd_name(PLI_UINT32 cd) -> PLI_BYTE8* {
  ClearVpiError();
  lyra::runtime::RuntimeEffects* run = RunningSimulation("vpi_mcd_name");
  if (run == nullptr) {
    return nullptr;
  }
  const std::optional<std::string_view> name = run->Files().NameOf(cd);
  if (!name.has_value()) {
    ReportVpiError("vpi_mcd_name", "the descriptor names no one open file");
    return nullptr;
  }
  return HandedOutName(*name);
}

auto vpi_get_vlog_info(p_vpi_vlog_info vlog_info_p) -> PLI_INT32 {
  ClearVpiError();
  if (vlog_info_p == nullptr) {
    ReportVpiError("vpi_get_vlog_info", "no information structure was given");
    return 0;
  }
  lyra::runtime::RuntimeEffects* run = RunningSimulation("vpi_get_vlog_info");
  if (run == nullptr) {
    return 0;
  }
  const std::span<char*> words = run->CommandLine().Words();
  vlog_info_p->argc = static_cast<PLI_INT32>(words.size());
  vlog_info_p->argv = words.data();
  vlog_info_p->product = lyra::runtime::VpiProduct();
  vlog_info_p->version = lyra::runtime::VpiVersion();
  return 1;
}

}  // extern "C"
