#include "lyra/runtime/jit_execution.hpp"

#include <span>
#include <utility>
#include <vector>

#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/file_table.hpp"
#include "lyra/runtime/generated_call_scope.hpp"
#include "lyra/runtime/runtime_services.hpp"
#include "lyra/runtime/scope.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

namespace {

using GeneratedEntry = void (*)(void*);

// The one C++ coroutine function the runtime owns on the generated side's
// behalf: a generated process cannot build a coroutine frame, so it hands over
// a plain entry plus its environment and the runtime wraps them. Lazy like
// every Coroutine, so the body runs only when the engine resumes it; the
// generated-call scope is entered there, around the generated call.
auto RunGeneratedProcess(GeneratedEntry entry, void* env) -> Coroutine<void> {
  {
    GeneratedCallScope scope;
    entry(env);
  }
  co_return;
}

}  // namespace

}  // namespace lyra::runtime

using lyra::runtime::Coroutine;
using lyra::runtime::FileTable;
using lyra::runtime::GeneratedCallScope;
using lyra::runtime::RuntimeServices;
using lyra::runtime::Scope;
using lyra::value::Format;
using lyra::value::PackedArray;
using lyra::value::PrintItem;
using lyra::value::PrintLiteralItem;
using lyra::value::String;
using lyra::value::TimeFormat;

extern "C" {

auto lyra_rt_services(void* self) -> void* {
  return &static_cast<Scope*>(self)->AbiServices();
}

auto lyra_rt_files(void* services) -> void* {
  return &static_cast<RuntimeServices*>(services)->Files();
}

auto lyra_rt_time_format(void* services) -> const void* {
  return &static_cast<RuntimeServices*>(services)->TimeFormat();
}

auto lyra_rt_make_string(void* cstr) -> void* {
  return GeneratedCallScope::Current().Arena().New<String>(
      static_cast<const char*>(cstr));
}

auto lyra_rt_make_print_literal_item(void* string_value) -> void* {
  return GeneratedCallScope::Current().Arena().New<PrintItem>(
      PrintLiteralItem(*static_cast<String*>(string_value)));
}

auto lyra_rt_format(LyraSpan items, const void* time_format) -> void* {
  std::span<PrintItem*> handles(
      static_cast<PrintItem**>(items.data), items.count);
  std::vector<PrintItem> collected;
  collected.reserve(items.count);
  for (PrintItem* handle : handles) {
    collected.push_back(*handle);
  }
  return GeneratedCallScope::Current().Arena().New<String>(
      Format(collected, *static_cast<const TimeFormat*>(time_format)));
}

auto lyra_rt_packed_const(
    std::int64_t value, std::int32_t width, bool is_signed, bool is_four_state)
    -> void* {
  return GeneratedCallScope::Current().Arena().New<PackedArray>(
      PackedArray::FromInt(
          value, static_cast<std::uint64_t>(width), is_signed, is_four_state));
}

void lyra_rt_writeln(void* files, void* descriptor, void* text) {
  static_cast<FileTable*>(files)->Writeln(
      *static_cast<PackedArray*>(descriptor), *static_cast<String*>(text));
}

void lyra_rt_write(void* files, void* descriptor, void* text) {
  static_cast<FileTable*>(files)->Write(
      *static_cast<PackedArray*>(descriptor), *static_cast<String*>(text));
}

auto lyra_rt_make_coroutine(void (*entry)(void*), void* env) -> void* {
  return GeneratedCallScope::Current().Arena().New<Coroutine<void>>(
      lyra::runtime::RunGeneratedProcess(entry, env));
}

void lyra_rt_register_initial(void* self, void* coroutine) {
  static_cast<Scope*>(self)->AbiRegisterInitial(
      std::move(*static_cast<Coroutine<void>*>(coroutine)));
}

void lyra_rt_register_final(void* self, void* coroutine) {
  static_cast<Scope*>(self)->AbiRegisterFinal(
      std::move(*static_cast<Coroutine<void>*>(coroutine)));
}
}
