#include "lyra/runtime/jit_execution.hpp"

#include <cstdint>
#include <memory>
#include <span>
#include <string>
#include <utility>
#include <vector>

#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/file_table.hpp"
#include "lyra/runtime/generated_call_scope.hpp"
#include "lyra/runtime/hierarchy_segment.hpp"
#include "lyra/runtime/runtime_services.hpp"
#include "lyra/runtime/scope.hpp"
#include "lyra/runtime/scope_program.hpp"
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
using lyra::runtime::GeneratedInstance;
using lyra::runtime::HierarchySegment;
using lyra::runtime::RuntimeServices;
using lyra::runtime::Scope;
using lyra::runtime::UnitDefinition;
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

auto lyra_rt_make_segment(void* label, LyraSpan indices) -> void* {
  const std::span<const std::int32_t> values(
      static_cast<const std::int32_t*>(indices.data), indices.count);
  std::vector<PackedArray> resolved;
  resolved.reserve(values.size());
  for (const std::int32_t index : values) {
    resolved.push_back(PackedArray::FromInt(index, 32, false, false));
  }
  return GeneratedCallScope::Current().Arena().New<HierarchySegment>(
      std::string(static_cast<const char*>(label)), resolved);
}

auto lyra_rt_make_unit(
    const void* definition, void* parent, void* segment, void* services)
    -> void* {
  const auto* def = static_cast<const UnitDefinition*>(definition);
  auto instance = std::make_unique<GeneratedInstance>(
      static_cast<Scope*>(parent), *static_cast<HierarchySegment*>(segment),
      *static_cast<RuntimeServices*>(services), def);
  {
    GeneratedCallScope scope;
    def->construct(instance.get());
  }
  return instance.release();
}

auto lyra_rt_add_owned_child(void* parent, void* child) -> void* {
  return static_cast<Scope*>(parent)->AddOwnedChild(
      std::unique_ptr<Scope>(static_cast<Scope*>(child)));
}

auto lyra_rt_load_field(void* self, std::uint32_t index) -> void* {
  return static_cast<GeneratedInstance*>(self)->Slot(index);
}

void lyra_rt_store_field(void* self, std::uint32_t index, void* value) {
  static_cast<GeneratedInstance*>(self)->Slot(index) = value;
}
}
