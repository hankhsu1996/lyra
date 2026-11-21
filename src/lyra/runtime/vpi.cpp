// The VPI routines of LRM clause 38, linked into the program and resolved
// against a user's foreign code by name, as the DPI-C surface is. The
// declarations are the standard's own (Annex K), which holds each definition
// here to its prototype. A routine taking a variable argument list is C, since
// only C reads one, and comes here once its arguments are read.
//
// A routine is defined only once what it acts on is: a program calling one
// that is not fails to link, naming it, as a design using a construct Lyra
// does not yet carry out is refused where it is built; a defined routine asked
// of an object or a relation not yet reached answers nothing and says so as
// its error (LRM 38.2). That holds of `vpi_get_data` and `vpi_put_data` for
// good, since they exist only for save and restart, which Lyra does not have
// and LRM 38.36.3 lets a tool lack.
// Nothing here throws across the C boundary.

#include <array>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <exception>
#include <expected>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/overloaded.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/runtime/ambient_run_context.hpp"
#include "lyra/runtime/cancellation.hpp"
#include "lyra/runtime/command_line.hpp"
#include "lyra/runtime/dpi_context.hpp"
#include "lyra/runtime/file_table.hpp"
#include "lyra/runtime/runtime.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/vpi_callbacks.hpp"
#include "lyra/runtime/vpi_object.hpp"
#include "lyra/runtime/vpi_routines.hpp"
#include "lyra/runtime/vpi_status.hpp"
#include "lyra/runtime/vpi_time.hpp"
#include "lyra/runtime/vpi_value.hpp"
#include "lyra/runtime/vpi_variable.hpp"
#include "lyra/value/string.hpp"
#include "vpi_user.h"

namespace {

using lyra::runtime::ClearVpiError;
using lyra::runtime::FileTable;
using lyra::runtime::Referent;
using lyra::runtime::ReportVpiError;
using lyra::runtime::Simulation;
using lyra::runtime::VpiIteration;
using lyra::runtime::VpiMode;
using lyra::runtime::VpiObject;
using lyra::runtime::VpiObjectHandles;

using lyra::runtime::AmbientRunContext;

// The run foreign code is reaching, with the handles it has handed it, where
// one is running on this thread.
auto RunningContext(std::string_view routine) -> AmbientRunContext* {
  AmbientRunContext* context = AmbientRunContext::TryCurrent();
  if (context == nullptr) {
    ReportVpiError(
        routine, std::string(routine) + " needs a running simulation");
  }
  return context;
}

auto RunningSimulation(std::string_view routine)
    -> lyra::runtime::RuntimeEffects* {
  AmbientRunContext* context = RunningContext(routine);
  return context == nullptr ? nullptr : &context->Effects();
}

// What `handle` refers to: the simulation where it is null, or what this run
// handed it out for; none where it is nothing this run handed out, which is
// reported as the routine's error.
auto Resolve(
    VpiObjectHandles& handles, vpiHandle handle, std::string_view routine)
    -> std::optional<Referent> {
  if (handle == nullptr) return Referent{Simulation{}};
  VpiObjectHandles::Target* target = handles.Find(handle);
  if (target == nullptr) {
    ReportVpiError(routine, "the handle is none this simulation handed out");
    return std::nullopt;
  }
  return std::visit(
      lyra::Overloaded{
          [](const VpiObject& object) { return Referent{object}; },
          [](const VpiIteration& iteration) { return Referent{&iteration}; }},
      *target);
}

// The object `handle` refers to, none where it refers to anything else, which
// is reported as the routine's error.
auto ObjectOf(
    VpiObjectHandles& handles, vpiHandle handle, std::string_view routine)
    -> std::optional<VpiObject> {
  const std::optional<Referent> referent = Resolve(handles, handle, routine);
  if (!referent.has_value()) return std::nullopt;
  if (const auto* object = std::get_if<VpiObject>(&*referent)) return *object;
  ReportVpiError(routine, "the handle is no object");
  return std::nullopt;
}

// The callback `handle` refers to; none where it refers to anything else, or to
// a callback removed or ended, which is reported as the routine's error.
auto CallbackOf(
    AmbientRunContext& context, vpiHandle handle, std::string_view routine)
    -> lyra::runtime::VpiCallback* {
  const std::optional<VpiObject> object =
      ObjectOf(context.ObjectHandles(), handle, routine);
  if (!object.has_value()) return nullptr;
  const auto* named = std::get_if<lyra::runtime::CallbackObject>(&*object);
  if (named == nullptr) {
    ReportVpiError(routine, "the handle is no callback");
    return nullptr;
  }
  lyra::runtime::VpiCallbacks* callbacks =
      lyra::runtime::AsRuntime(context.Effects()).CallbacksIfMade();
  lyra::runtime::VpiCallback* callback =
      callbacks == nullptr ? nullptr : callbacks->Find(*named);
  if (callback == nullptr) {
    ReportVpiError(routine, "the callback was removed or has ended");
  }
  return callback;
}

// What `compute`, a routine's answer, comes to, where nothing it raises
// crosses into foreign code (LRM 35.9). What foreign code asked of the design
// that the design cannot do -- a member of a tagged union other than the one
// its tag names, an operation not yet carried out -- is that request's error,
// answered as the routine's (LRM 38.2), and the run goes on as it would had the
// request not been made. A departure -- a disable, the run's end -- was
// recorded where it was raised, so it is taken where the design called into
// foreign code, as that call returns. Anything else is a fault of the run
// itself, which ends it and is reported as one (LRM Table 38-1). Whatever it
// answers nothing for is reported as the error of the routine `routine`.
template <typename Compute>
auto Guarded(
    AmbientRunContext& context, std::string_view routine,
    const Compute& compute) -> std::invoke_result_t<const Compute&> {
  const auto refused = [&](std::string why, PLI_INT32 level) {
    ReportVpiError(routine, why, level);
    return std::unexpected<std::string>(std::move(why));
  };
  try {
    std::invoke_result_t<const Compute&> answer = compute();
    if (!answer.has_value()) ReportVpiError(routine, answer.error());
    return answer;
  } catch (const lyra::runtime::ControlEffect&) {
    return refused("what called into foreign code has ended", vpiError);
  } catch (const lyra::SimulationError& raised) {
    return refused(raised.what(), vpiError);
  } catch (const std::exception&) {
    lyra::runtime::SettleRaised(context.Effects(), std::current_exception());
    return refused("a fault of the tool's own ends the run", vpiInternal);
  }
}

// What `compute` answers, or `fallback` where it answers nothing, which is
// reported as the routine's error.
template <typename T, typename Compute>
auto Answered(
    AmbientRunContext& context, std::string_view routine, T fallback,
    const Compute& compute) -> T {
  lyra::runtime::VpiAnswer<T> answer = Guarded(context, routine, compute);
  if (!answer.has_value()) return fallback;
  return *std::move(answer);
}

// A handle to the object `find` finds, null where it finds none; an answer of
// nothing is an error where it says why.
template <typename Find>
auto HandOut(
    AmbientRunContext& context, std::string_view routine, const Find& find)
    -> vpiHandle {
  const std::optional<VpiObject> object =
      Answered(context, routine, std::optional<VpiObject>{}, find);
  return object.has_value() ? context.ObjectHandles().Issue(*object) : nullptr;
}

// What `vpi_get` and `vpi_get64` answer of `object` in `mode`, the one named
// `routine` reporting why where it answers nothing.
auto IntegerPropertyOf(
    VpiMode mode, PLI_INT32 property, vpiHandle object,
    std::string_view routine) -> PLI_INT64 {
  ClearVpiError();
  AmbientRunContext* context = RunningContext(routine);
  if (context == nullptr) return vpiUndefined;
  const std::optional<Referent> of =
      Resolve(context->ObjectHandles(), object, routine);
  if (!of.has_value()) return vpiUndefined;
  return Answered(*context, routine, PLI_INT64{vpiUndefined}, [&] {
    return lyra::runtime::IntegerProperty(
        context->Effects(), property, *of, mode);
  });
}

// The version of the standard an application compiled for none is read in
// (LRM 36.12.2.2): the one the run's `+lyra+vpi+compatibility=` names, by the
// suffix its compiler symbol carries (LRM 36.12.2.1), and the latest where it
// names none or no version there is.
auto DefaultMode(AmbientRunContext& context) -> VpiMode {
  constexpr std::string_view kOption = "lyra+vpi+compatibility=";
  constexpr std::array<std::pair<std::string_view, VpiMode>, 8> kVersions = {{
      {"1364v1995", VpiMode::k1364v1995},
      {"1364v2001", VpiMode::k1364v2001},
      {"1364v2005", VpiMode::k1364v2005},
      {"1800v2005", VpiMode::k1800v2005},
      {"1800v2009", VpiMode::k1800v2009},
      {"1800v2012", VpiMode::k1800v2012},
      {"1800v2017", VpiMode::k1800v2012},
      {"1800v2023", VpiMode::k1800v2012},
  }};
  VpiMode mode = VpiMode::k1800v2012;
  for (const std::string& argument :
       context.Effects().CommandLine().Plusargs()) {
    const std::string_view text(argument);
    if (!text.starts_with(kOption)) continue;
    const std::string_view named = text.substr(kOption.size());
    for (const auto& [version, versioned] : kVersions) {
      if (named == version) mode = versioned;
    }
  }
  return mode;
}

// The default version, where a running simulation is there to say it; the
// routine itself reports that one is not.
auto DefaultMode() -> VpiMode {
  AmbientRunContext* context = AmbientRunContext::TryCurrent();
  return context == nullptr ? VpiMode::k1800v2012 : DefaultMode(*context);
}

// Runs `access`, what a routine does, reporting why it did nothing where it
// did nothing.
template <typename Access>
void ReportFailure(
    AmbientRunContext& context, std::string_view routine,
    const Access& access) {
  Guarded(context, routine, access);
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
// routine answers again (LRM 38.11, 38.26), since the file table may move its
// own.
auto HandedOutName(std::string_view name) -> PLI_BYTE8* {
  static thread_local std::string kept;
  kept = name;
  return kept.data();
}

}  // namespace

namespace lyra::runtime {

auto VpiGet(VpiMode mode, PLI_INT32 property, vpiHandle object) -> PLI_INT32 {
  return static_cast<PLI_INT32>(
      IntegerPropertyOf(mode, property, object, "vpi_get"));
}

auto VpiGetStr(VpiMode mode, PLI_INT32 property, vpiHandle object)
    -> PLI_BYTE8* {
  ClearVpiError();
  AmbientRunContext* context = RunningContext("vpi_get_str");
  if (context == nullptr) return nullptr;
  const std::optional<Referent> of =
      Resolve(context->ObjectHandles(), object, "vpi_get_str");
  if (!of.has_value()) return nullptr;
  const std::optional<std::string> answer = Answered(
      *context, "vpi_get_str", std::optional<std::string>{},
      [&] { return StringProperty(property, *of, mode); });
  return answer.has_value() ? HandedOutName(*answer) : nullptr;
}

// An iteration that holds nothing is no iterator: the routine answers null
// (LRM 38.23).
auto VpiIterate(VpiMode mode, PLI_INT32 type, vpiHandle refHandle)
    -> vpiHandle {
  ClearVpiError();
  AmbientRunContext* context = RunningContext("vpi_iterate");
  if (context == nullptr) return nullptr;
  VpiObjectHandles& handles = context->ObjectHandles();
  const std::optional<Referent> from =
      Resolve(handles, refHandle, "vpi_iterate");
  if (!from.has_value()) return nullptr;
  std::vector<VpiObject> related = Answered(
      *context, "vpi_iterate", std::vector<VpiObject>{},
      [&] { return Related(context->Effects(), type, *from, mode); });
  if (related.empty()) return nullptr;
  const auto* use = std::get_if<VpiObject>(&*from);
  return handles.Issue(
      VpiIteration{
          .relation = type,
          .use = use == nullptr ? std::nullopt : std::optional(*use),
          .objects = std::move(related),
          .next = 0});
}

}  // namespace lyra::runtime

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

auto vpi_handle_by_name(PLI_BYTE8* name, vpiHandle scope) -> vpiHandle {
  ClearVpiError();
  AmbientRunContext* context = RunningContext("vpi_handle_by_name");
  if (context == nullptr) return nullptr;
  VpiObjectHandles& handles = context->ObjectHandles();
  if (name == nullptr) {
    ReportVpiError("vpi_handle_by_name", "no name was given");
    return nullptr;
  }
  const std::optional<Referent> within =
      Resolve(handles, scope, "vpi_handle_by_name");
  if (!within.has_value()) return nullptr;
  return HandOut(*context, "vpi_handle_by_name", [&] {
    return lyra::runtime::ObjectNamed(context->Effects(), name, *within);
  });
}

// An iterator's use is the object it was made on, none where it was made on
// none (LRM 37.84).
auto vpi_handle(PLI_INT32 type, vpiHandle refHandle) -> vpiHandle {
  ClearVpiError();
  AmbientRunContext* context = RunningContext("vpi_handle");
  if (context == nullptr) return nullptr;
  VpiObjectHandles& handles = context->ObjectHandles();
  const std::optional<Referent> from =
      Resolve(handles, refHandle, "vpi_handle");
  if (!from.has_value()) return nullptr;
  return HandOut(*context, "vpi_handle", [&] {
    return lyra::runtime::RelatedOne(context->Effects(), type, *from);
  });
}

auto vpi_iterate(PLI_INT32 type, vpiHandle refHandle) -> vpiHandle {
  return lyra::runtime::VpiIterate(DefaultMode(), type, refHandle);
}

// The iterator is freed where it has no object left (LRM 38.40).
auto vpi_scan(vpiHandle iterator) -> vpiHandle {
  ClearVpiError();
  AmbientRunContext* context = RunningContext("vpi_scan");
  if (context == nullptr) return nullptr;
  VpiObjectHandles& handles = context->ObjectHandles();
  VpiObjectHandles::Target* target = handles.Find(iterator);
  auto* iteration =
      target == nullptr ? nullptr : std::get_if<VpiIteration>(target);
  if (iteration == nullptr) {
    ReportVpiError("vpi_scan", "the handle is no iterator");
    return nullptr;
  }
  if (iteration->next == iteration->objects.size()) {
    handles.Release(iterator);
    return nullptr;
  }
  VpiObject next = iteration->objects[iteration->next];
  ++iteration->next;
  return handles.Issue(std::move(next));
}

auto vpi_handle_by_index(vpiHandle object, PLI_INT32 indx) -> vpiHandle {
  ClearVpiError();
  AmbientRunContext* context = RunningContext("vpi_handle_by_index");
  if (context == nullptr) return nullptr;
  VpiObjectHandles& handles = context->ObjectHandles();
  const std::optional<VpiObject> of =
      ObjectOf(handles, object, "vpi_handle_by_index");
  if (!of.has_value()) return nullptr;
  const std::int64_t index = indx;
  return HandOut(*context, "vpi_handle_by_index", [&] {
    return lyra::runtime::ElementAt(*of, std::span(&index, 1));
  });
}

auto vpi_handle_by_multi_index(
    vpiHandle obj, PLI_INT32 num_index, PLI_INT32* index_array) -> vpiHandle {
  ClearVpiError();
  AmbientRunContext* context = RunningContext("vpi_handle_by_multi_index");
  if (context == nullptr) return nullptr;
  VpiObjectHandles& handles = context->ObjectHandles();
  if (num_index < 1 || index_array == nullptr) {
    ReportVpiError("vpi_handle_by_multi_index", "no index was given");
    return nullptr;
  }
  const std::optional<VpiObject> of =
      ObjectOf(handles, obj, "vpi_handle_by_multi_index");
  if (!of.has_value()) return nullptr;
  const std::span<const PLI_INT32> given(
      index_array, static_cast<std::size_t>(num_index));
  const std::vector<std::int64_t> indices(given.begin(), given.end());
  return HandOut(*context, "vpi_handle_by_multi_index", [&] {
    return lyra::runtime::ElementAt(*of, indices);
  });
}

auto vpi_get64(PLI_INT32 property, vpiHandle object) -> PLI_INT64 {
  return IntegerPropertyOf(DefaultMode(), property, object, "vpi_get64");
}

auto vpi_get(PLI_INT32 property, vpiHandle object) -> PLI_INT32 {
  return lyra::runtime::VpiGet(DefaultMode(), property, object);
}

auto vpi_get_str(PLI_INT32 property, vpiHandle object) -> PLI_BYTE8* {
  return lyra::runtime::VpiGetStr(DefaultMode(), property, object);
}

// The value of `expr` in the format `value_p` names (LRM 38.15).
void vpi_get_value(vpiHandle expr, p_vpi_value value_p) {
  ClearVpiError();
  AmbientRunContext* context = RunningContext("vpi_get_value");
  if (context == nullptr) return;
  if (value_p == nullptr) {
    ReportVpiError("vpi_get_value", "no value structure was given");
    return;
  }
  const std::optional<VpiObject> object =
      ObjectOf(context->ObjectHandles(), expr, "vpi_get_value");
  if (!object.has_value()) return;
  ReportFailure(
      *context, "vpi_get_value", [&]() -> lyra::runtime::VpiAnswer<void> {
        const lyra::runtime::VpiAnswer<lyra::runtime::ValueShape> shape =
            lyra::runtime::ValueShapeOf(*object);
        if (!shape.has_value()) return std::unexpected(shape.error());
        const lyra::runtime::VpiAnswer<lyra::runtime::HeldValue> held =
            lyra::runtime::ReadVariablePart(*object);
        if (!held.has_value()) return std::unexpected(held.error());
        return lyra::runtime::FillValue(
            *shape, *held, *value_p, lyra::runtime::ReadValueStore());
      });
}

// Writes `object` the value `value_p` holds as `flags` says (LRM 38.34): with
// no delay as the design's own write would land, or as a procedural force or
// release of it (LRM 10.6.2), the value it holds after a release handed back.
// A write after a delay schedules an event, which nothing here yet does, so no
// event is ever handed back.
auto vpi_put_value(
    vpiHandle object, p_vpi_value value_p, p_vpi_time time_p, PLI_INT32 flags)
    -> vpiHandle {
  ClearVpiError();
  AmbientRunContext* context = RunningContext("vpi_put_value");
  if (context == nullptr) return nullptr;
  const std::optional<VpiObject> target =
      ObjectOf(context->ObjectHandles(), object, "vpi_put_value");
  if (!target.has_value()) return nullptr;
  // LRM 4.4.2.9, 4.4.3.10: nothing is written in the Postponed region.
  if (!lyra::runtime::AsRuntime(context->Effects()).MayWriteNow()) {
    ReportVpiError(
        "vpi_put_value",
        "no value may be written in the read-only Postponed region");
    return nullptr;
  }
  ReportFailure(
      *context, "vpi_put_value", [&]() -> lyra::runtime::VpiAnswer<void> {
        // The flags name one delay mode, beside the bits asking for an event
        // handed back and how values are handed (LRM 38.34, Annex K). A
        // cancel is handed no value, and no object reached here is a scheduled
        // event.
        const PLI_INT32 mode = flags & ~(vpiReturnEvent | vpiUserAllocFlag |
                                         vpiOneValue | vpiPropagateOff);
        if (mode == vpiCancelEvent) {
          return std::unexpected<std::string>(
              "the object is no scheduled event");
        }
        if (std::holds_alternative<lyra::runtime::VariableObject>(*target) &&
            lyra::runtime::TypeOfActual(*target, DefaultMode()) ==
                vpiNamedEvent) {
          return lyra::runtime::VpiNotYet("a toggle of a named event");
        }
        if (value_p == nullptr) {
          return std::unexpected<std::string>("no value structure was given");
        }
        const lyra::runtime::VpiAnswer<lyra::runtime::ValueShape> shape =
            lyra::runtime::ValueShapeOf(*target);
        if (!shape.has_value()) return std::unexpected(shape.error());
        if (mode == vpiReleaseFlag) {
          const lyra::runtime::VpiAnswer<lyra::runtime::HeldValue> after =
              lyra::runtime::ReleaseVariablePart(*target);
          if (!after.has_value()) return std::unexpected(after.error());
          return lyra::runtime::FillValue(
              *shape, *after, *value_p, lyra::runtime::ReadValueStore());
        }
        const lyra::runtime::VpiAnswer<lyra::runtime::HeldValue> written =
            lyra::runtime::ValueToWrite(*shape, *value_p);
        if (!written.has_value()) return std::unexpected(written.error());
        switch (mode) {
          case vpiNoDelay:
            return lyra::runtime::WriteVariablePart(*target, *written);
          case vpiForceFlag:
            return lyra::runtime::ForceVariablePart(*target, *written);
          case vpiInertialDelay:
          case vpiTransportDelay:
          case vpiPureTransportDelay:
            if (time_p == nullptr) {
              return std::unexpected<std::string>("no delay was given");
            }
            return lyra::runtime::VpiNotYet("a write after a delay");
          default:
            return std::unexpected<std::string>("the flags name no delay mode");
        }
      });
  return nullptr;
}

// Two handles to one object compare equal however each was reached, a part of
// a packed value by the bits it is; an iterator is an object of its own (LRM
// 38.3).
auto vpi_compare_objects(vpiHandle object1, vpiHandle object2) -> PLI_INT32 {
  ClearVpiError();
  AmbientRunContext* context = RunningContext("vpi_compare_objects");
  if (context == nullptr) return 0;
  VpiObjectHandles& handles = context->ObjectHandles();
  VpiObjectHandles::Target* first = handles.Find(object1);
  VpiObjectHandles::Target* second = handles.Find(object2);
  if (first == nullptr || second == nullptr) {
    ReportVpiError(
        "vpi_compare_objects", "a handle is none this simulation handed out");
    return 0;
  }
  const auto* a = std::get_if<VpiObject>(first);
  const auto* b = std::get_if<VpiObject>(second);
  if (a == nullptr || b == nullptr) return object1 == object2 ? 1 : 0;
  return Answered(
      *context, "vpi_compare_objects", PLI_INT32{0},
      [&]() -> lyra::runtime::VpiAnswer<PLI_INT32> {
        return lyra::runtime::SameVariablePart(*a, *b) ? 1 : 0;
      });
}

auto vpi_release_handle(vpiHandle object) -> PLI_INT32 {
  ClearVpiError();
  AmbientRunContext* context = RunningContext("vpi_release_handle");
  if (context == nullptr) return 0;
  if (!context->ObjectHandles().Release(object)) {
    ReportVpiError(
        "vpi_release_handle", "the handle is none this simulation handed out");
    return 0;
  }
  return 1;
}

// The callback `cb_data_p` describes (LRM 38.36), a handle to it answered;
// none where it is refused, which says why through `vpi_chk_error`.
auto vpi_register_cb(p_cb_data cb_data_p) -> vpiHandle {
  ClearVpiError();
  AmbientRunContext* context = RunningContext("vpi_register_cb");
  if (context == nullptr) return nullptr;
  if (cb_data_p == nullptr) {
    ReportVpiError("vpi_register_cb", "no callback data was given");
    return nullptr;
  }
  std::optional<VpiObject> object;
  if (cb_data_p->obj != nullptr) {
    object =
        ObjectOf(context->ObjectHandles(), cb_data_p->obj, "vpi_register_cb");
    if (!object.has_value()) return nullptr;
  }
  return HandOut(
      *context, "vpi_register_cb",
      [&]() -> lyra::runtime::VpiAnswer<std::optional<VpiObject>> {
        const lyra::runtime::VpiAnswer<lyra::runtime::CallbackObject>
            registered = lyra::runtime::AsRuntime(context->Effects())
                             .Callbacks()
                             .Register(*cb_data_p, object);
        if (!registered.has_value()) {
          return std::unexpected(registered.error());
        }
        return std::optional<VpiObject>{*registered};
      });
}

// A removed callback is not called again, and the handle it was removed by is
// no longer valid (LRM 38.39).
auto vpi_remove_cb(vpiHandle cb_obj) -> PLI_INT32 {
  ClearVpiError();
  AmbientRunContext* context = RunningContext("vpi_remove_cb");
  if (context == nullptr) return 0;
  lyra::runtime::VpiCallback* callback =
      CallbackOf(*context, cb_obj, "vpi_remove_cb");
  if (callback == nullptr) return 0;
  lyra::runtime::AsRuntime(context->Effects()).Callbacks().Remove(*callback);
  context->ObjectHandles().Release(cb_obj);
  return 1;
}

// What a callback was registered with (LRM 38.8), in structures the run keeps
// for as long as the callback stands.
void vpi_get_cb_info(vpiHandle object, p_cb_data cb_data_p) {
  ClearVpiError();
  AmbientRunContext* context = RunningContext("vpi_get_cb_info");
  if (context == nullptr) return;
  if (cb_data_p == nullptr) {
    ReportVpiError("vpi_get_cb_info", "no callback data structure was given");
    return;
  }
  lyra::runtime::VpiCallback* callback =
      CallbackOf(*context, object, "vpi_get_cb_info");
  if (callback == nullptr) return;
  callback->Info(*cb_data_p);
}

// The time now in the unit of the object named, the simulation's where none
// is, as two words or as a real (LRM 38.13); of a time queue, the time it is
// at (LRM 37.81).
void vpi_get_time(vpiHandle object, p_vpi_time time_p) {
  ClearVpiError();
  AmbientRunContext* context = RunningContext("vpi_get_time");
  if (context == nullptr) return;
  if (time_p == nullptr) {
    ReportVpiError("vpi_get_time", "no time structure was given");
    return;
  }
  const std::optional<Referent> of =
      Resolve(context->ObjectHandles(), object, "vpi_get_time");
  if (!of.has_value()) return;
  ReportFailure(
      *context, "vpi_get_time", [&]() -> lyra::runtime::VpiAnswer<void> {
        lyra::runtime::RuntimeEffects& run = context->Effects();
        const lyra::runtime::VpiAnswer<std::int8_t> unit =
            lyra::runtime::TimeUnitOf(run, *of);
        if (!unit.has_value()) return std::unexpected(unit.error());
        const auto* object = std::get_if<VpiObject>(&*of);
        const auto* queue =
            object == nullptr
                ? nullptr
                : std::get_if<lyra::runtime::TimeQueueObject>(object);
        const lyra::SimTime at = queue == nullptr ? run.Now() : queue->at;
        if (!lyra::runtime::FillTimeAt(run, *unit, at, *time_p)) {
          return std::unexpected<std::string>(
              "the time structure names no format");
        }
        return {};
      });
}

}  // extern "C"
