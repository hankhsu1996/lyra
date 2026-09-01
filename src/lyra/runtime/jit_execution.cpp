#include "lyra/runtime/jit_execution.hpp"

#include <algorithm>
#include <coroutine>
#include <cstddef>
#include <cstdint>
#include <deque>
#include <functional>
#include <memory>
#include <span>
#include <string>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/time.hpp"
#include "lyra/runtime/activation_value_cell.hpp"
#include "lyra/runtime/closure.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/delay.hpp"
#include "lyra/runtime/diagnostic.hpp"
#include "lyra/runtime/distribution.hpp"
#include "lyra/runtime/file_table.hpp"
#include "lyra/runtime/generated_call_scope.hpp"
#include "lyra/runtime/hierarchy_segment.hpp"
#include "lyra/runtime/host_command.hpp"
#include "lyra/runtime/plusargs.hpp"
#include "lyra/runtime/random.hpp"
#include "lyra/runtime/runtime.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/scope.hpp"
#include "lyra/runtime/scope_program.hpp"
#include "lyra/runtime/sim_time.hpp"
#include "lyra/runtime/var.hpp"
#include "lyra/value/chandle.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/real.hpp"
#include "lyra/value/runtime_associative_array.hpp"
#include "lyra/value/runtime_dynamic_array.hpp"
#include "lyra/value/runtime_queue.hpp"
#include "lyra/value/runtime_tuple.hpp"
#include "lyra/value/runtime_unpacked_array.hpp"
#include "lyra/value/runtime_value.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/string_op.hpp"

namespace lyra::runtime {

namespace {

using GeneratedRamp = void* (*)(void* env);

// An RAII owner of a generated body's own coroutine frame, reached as an opaque
// address the ramp returns. Destroying it destroys the frame, so the generated
// body is torn down on every path its driver leaves -- normal completion,
// cancellation, or shutdown -- and never leaks when the driver is released
// while the body is still suspended.
class GeneratedCoroutine {
 public:
  GeneratedCoroutine() = default;
  explicit GeneratedCoroutine(void* frame)
      : handle_(std::coroutine_handle<>::from_address(frame)) {
  }
  GeneratedCoroutine(const GeneratedCoroutine&) = delete;
  auto operator=(const GeneratedCoroutine&) -> GeneratedCoroutine& = delete;
  GeneratedCoroutine(GeneratedCoroutine&& other) noexcept
      : handle_(std::exchange(other.handle_, {})) {
  }
  auto operator=(GeneratedCoroutine&& other) noexcept -> GeneratedCoroutine& {
    if (handle_ != nullptr) {
      handle_.destroy();
    }
    handle_ = std::exchange(other.handle_, {});
    return *this;
  }
  ~GeneratedCoroutine() {
    if (handle_ != nullptr) {
      handle_.destroy();
    }
  }

  [[nodiscard]] auto Done() const -> bool {
    return handle_.done();
  }
  void Resume() const {
    handle_.resume();
  }

 private:
  std::coroutine_handle<> handle_;
};

// The runtime-owned coroutine that is the process the engine schedules, and
// which drives the generated body's own coroutine.
//
// The engine's activation token is a C++ promise carrying non-trivial members.
// A generated body is free of it: it holds only its own coroutine, and this
// adapter owns the promise on its behalf. So the frame a code generator lays
// out never has to embed a runtime C++ type -- only a coroutine's resume, done,
// and destroy cross the boundary. That is what this adapter buys, and why the
// engine resumes it rather than the generated coroutine.
//
// The generated body runs to its next suspension, having already registered its
// own wakeup; the adapter then parks, and resumes it when the engine runs the
// adapter again.
//
// This adapter frame realizes the activation. It owns two lifetimes the
// generated body needs but cannot hold itself. The `activation_frame` holds
// every value whose lifetime crosses a suspension -- a procedural local a
// suspending body reads after it resumes -- so a handle the generated frame
// carries across a suspension points here, not into a per-stretch scope that is
// released when the stretch returns. And `generated` RAII-owns the body's own
// coroutine, so both are destroyed together, on every path the adapter leaves.
// Each stretch of generated code runs in its own generated-call scope naming
// this frame; a transient the stretch materializes still lives in that scope
// and is released when the stretch returns.
auto RunGeneratedProcess(GeneratedRamp ramp, void* env) -> Coroutine<void> {
  ActivationFrameStorage activation_frame;
  GeneratedCoroutine generated;
  {
    GeneratedCallScope scope(&activation_frame);
    generated = GeneratedCoroutine{ramp(env)};
  }
  while (!generated.Done()) {
    co_await std::suspend_always{};
    {
      GeneratedCallScope scope(&activation_frame);
      generated.Resume();
    }
  }
  // A body that cannot be unwound through reports a control effect no region of
  // it claimed as an outcome rather than by leaving; raising it here is what
  // settles this activation cancelled instead of finished (LRM 9.6.2, 9.7).
  if (CancellationTarget* target = activation_frame.CancelledBy();
      target != nullptr) {
    RaiseControlEffect(target);
  }
  co_return;
}

// A value crossing the boundary is an opaque handle to a runtime object. These
// name the two directions of that correspondence so the entry points below read
// as the operation they perform, not as a wall of casts.
template <typename T>
auto Read(const void* handle) -> const T& {
  return *static_cast<const T*>(handle);
}

template <typename T>
auto Own(T value) -> void* {
  return GeneratedCallScope::Current().Arena().New<T>(std::move(value));
}

// Takes over the erased value a boxed handle carries. A value crosses this way
// when it is what states a representation, so nothing on this side could have
// read that representation off anything else. The handle names a transient the
// caller boxed for this call, so its contents move rather than copy.
auto ErasedValue(void* handle) -> value::RuntimeValue {
  return std::move(*static_cast<value::RuntimeValue*>(handle));
}

// Storage for one value the generated program builds once and then holds by
// address for the rest of the run. Every other value crossing the boundary
// belongs to the arena of the stretch that made it, which is released when that
// stretch returns; these cannot, because generated code keeps their addresses
// across calls. The store only grows, and it grows to what the design declares.
template <class T>
auto ProgramLifetime(T value) -> const T* {
  static std::deque<T> stored;
  stored.push_back(std::move(value));
  return &stored.back();
}

// Copies one element out across the opaque-handle boundary as a handle of the
// element's own domain. A chandle's handle is the pointer it carries rather
// than a pointer to a runtime object, the same divergence the box family has.
auto ElementHandle(const value::RuntimeValue& element) -> void* {
  return std::visit(
      [](const auto& value) -> void* {
        using T = std::decay_t<decltype(value)>;
        if constexpr (std::is_same_v<T, value::Chandle>) {
          return value.Ptr();
        } else {
          return Own(value);
        }
      },
      element.value);
}

// Erases an incoming element handle into the domain the container's element
// default names, which is where a container reads the target domain from.
auto ElementFrom(const value::RuntimeValue& element_default, void* value)
    -> value::RuntimeValue {
  return std::visit(
      [&](const auto& prototype) -> value::RuntimeValue {
        using T = std::decay_t<decltype(prototype)>;
        if constexpr (std::is_same_v<T, value::Chandle>) {
          return value::RuntimeValue{value::Chandle{value}};
        } else {
          return value::RuntimeValue{Read<T>(value)};
        }
      },
      element_default.value);
}

// Stores each of a literal's entries under the index it names. An entry is the
// product of the two, and a product's components are already erased, so nothing
// here converts either one (LRM 7.9.11).
auto SeedAssociativeEntries(
    value::RuntimeAssociativeArray array, LyraSpan entries)
    -> value::RuntimeAssociativeArray {
  const std::span<value::RuntimeTuple* const> handles(
      static_cast<value::RuntimeTuple* const*>(entries.data), entries.count);
  for (const value::RuntimeTuple* entry : handles) {
    array = array.WithElement(entry->Component(0), entry->Component(1));
  }
  return array;
}

// A literal's element handles, erased into the domain the prototype names and
// repeated `count` times (LRM 10.9.1). A container holds its contents erased,
// and each element conforms to the representation the prototype beside it
// states, so the erasure is the container's own and the caller hands over a
// literal's storage without naming a representation at all. An enumerated
// element list is this with a count of one, which is why a uniform array, a
// replicated pattern and a plain list all reach one entry.
auto ReplicateLiteral(
    const value::RuntimeValue& element_default, LyraSpan unit,
    std::int64_t count) -> std::vector<value::RuntimeValue> {
  std::span<void* const> handles(
      static_cast<void* const*>(unit.data), unit.count);
  std::vector<value::RuntimeValue> collected;
  collected.reserve(unit.count * static_cast<std::size_t>(count));
  for (std::int64_t i = 0; i < count; ++i) {
    for (void* handle : handles) {
      collected.push_back(ElementFrom(element_default, handle));
    }
  }
  return collected;
}

// The per-axis indices that pick one instance out of an array of them. They
// are settled while the design elaborates, so they cross as machine integers
// rather than as handles to runtime-owned values, and the scope tree they are
// matched against holds them as values.
auto IndicesOf(LyraSpan indices) -> std::vector<value::PackedArray> {
  const std::span<const std::int32_t> raw(
      static_cast<const std::int32_t*>(indices.data), indices.count);
  std::vector<value::PackedArray> resolved;
  resolved.reserve(raw.size());
  for (const std::int32_t index : raw) {
    resolved.push_back(
        value::PackedArray::IntUnsigned(static_cast<std::uint32_t>(index)));
  }
  return resolved;
}

// A submitted closure runs after the stretch that built it has returned, so the
// region takes the value out of that stretch's scope rather than borrowing it.
// The region holds what it takes by a shared handle because a region queue
// holds copyable callables, while a closure value owns its captures and is
// therefore only movable.
auto TakeClosure(void* closure) -> std::function<void()> {
  return [held = std::make_shared<ClosureValue>(std::move(
              *static_cast<ClosureValue*>(closure)))] { held->Invoke(); };
}

// A distribution draw is a product of the value drawn and the advanced seed
// (LRM 20.14.2), so it crosses the boundary as the type-erased product every
// other product crosses as.
auto OwnDraw(const DistributionDraw& draw) -> void* {
  return Own(
      value::RuntimeTuple(
          std::vector<value::RuntimeValue>{
              value::RuntimeValue{draw.Get<0>()},
              value::RuntimeValue{draw.Get<1>()}}));
}

}  // namespace

}  // namespace lyra::runtime

using lyra::runtime::ActivationValueCell;
using lyra::runtime::CancellationTarget;
using lyra::runtime::ChannelCancellation;
using lyra::runtime::ClosureDefinition;
using lyra::runtime::ClosureValue;
using lyra::runtime::Coroutine;
using lyra::runtime::CoroutineHandle;
using lyra::runtime::DiagnosticDispatcher;
using lyra::runtime::EnterCancellationTarget;
using lyra::runtime::FileTable;
using lyra::runtime::GeneratedCallScope;
using lyra::runtime::GeneratedScope;
using lyra::runtime::HierarchySegment;
using lyra::runtime::IndicesOf;
using lyra::runtime::LeaveCancellationTarget;
using lyra::runtime::Observable;
using lyra::runtime::Own;
using lyra::runtime::OwnDraw;
using lyra::runtime::ProgramLifetime;
using lyra::runtime::Read;
using lyra::runtime::RealTimeInUnit;
using lyra::runtime::RunHostCommand;
using lyra::runtime::RuntimeEffects;
using lyra::runtime::Scope;
using lyra::runtime::ScopeDefinition;
using lyra::runtime::SimTimeInUnit;
using lyra::runtime::STimeInUnit;
using lyra::runtime::SubscribeValueChange;
using lyra::runtime::TakeClosure;
using lyra::runtime::TestPlusargs;
using lyra::runtime::Trigger;
using lyra::runtime::Var;
using lyra::value::Chandle;
using lyra::value::Concat;
using lyra::value::Format;
using lyra::value::FormatSpec;
using lyra::value::PackedArray;
using lyra::value::PackedRange;
using lyra::value::PackedType;
using lyra::value::PrintItem;
using lyra::value::PrintLiteralItem;
using lyra::value::PrintValueItem;
using lyra::value::Real;
using lyra::value::Replicate;
using lyra::value::RuntimeAssociativeArray;
using lyra::value::RuntimeDynamicArray;
using lyra::value::RuntimeQueue;
using lyra::value::RuntimeTuple;
using lyra::value::RuntimeUnpackedArray;
using lyra::value::RuntimeValue;
using lyra::value::ShortReal;
using lyra::value::String;
using lyra::value::TimeFormat;

extern "C" {

auto lyra_rt_current_runtime() -> void* {
  return &lyra::runtime::current_runtime();
}

auto lyra_rt_files(void* runtime) -> void* {
  return &static_cast<RuntimeEffects*>(runtime)->Files();
}

auto lyra_rt_time_format(void* runtime) -> const void* {
  return &static_cast<RuntimeEffects*>(runtime)->TimeFormat();
}

void lyra_rt_set_time_format(
    void* runtime, const void* units_power, const void* precision,
    const void* suffix, const void* min_width) {
  static_cast<RuntimeEffects*>(runtime)->SetTimeFormat(
      Read<PackedArray>(units_power), Read<PackedArray>(precision),
      Read<String>(suffix), Read<PackedArray>(min_width));
}

void lyra_rt_reset_time_format(void* runtime) {
  static_cast<RuntimeEffects*>(runtime)->ResetTimeFormat();
}

auto lyra_rt_file_open(void* files, const void* name) -> void* {
  return Own(static_cast<FileTable*>(files)->Open(Read<String>(name)));
}

auto lyra_rt_file_open_mode(void* files, const void* name, const void* mode)
    -> void* {
  return Own(
      static_cast<FileTable*>(files)->Open(
          Read<String>(name), Read<String>(mode)));
}

void lyra_rt_file_close(void* files, const void* descriptor) {
  static_cast<FileTable*>(files)->Close(Read<PackedArray>(descriptor));
}

auto lyra_rt_file_getc(void* files, const void* fd) -> void* {
  return Own(static_cast<FileTable*>(files)->Getc(Read<PackedArray>(fd)));
}

auto lyra_rt_file_ungetc(void* files, const void* c, const void* fd) -> void* {
  return Own(
      static_cast<FileTable*>(files)->Ungetc(
          Read<PackedArray>(c), Read<PackedArray>(fd)));
}

auto lyra_rt_file_seek(
    void* files, const void* fd, const void* offset, const void* operation)
    -> void* {
  return Own(
      static_cast<FileTable*>(files)->Seek(
          Read<PackedArray>(fd), Read<PackedArray>(offset),
          Read<PackedArray>(operation)));
}

auto lyra_rt_file_rewind(void* files, const void* fd) -> void* {
  return Own(static_cast<FileTable*>(files)->Rewind(Read<PackedArray>(fd)));
}

auto lyra_rt_file_tell(void* files, const void* fd) -> void* {
  return Own(static_cast<FileTable*>(files)->Tell(Read<PackedArray>(fd)));
}

auto lyra_rt_file_eof(void* files, const void* fd) -> void* {
  return Own(static_cast<FileTable*>(files)->Eof(Read<PackedArray>(fd)));
}

void lyra_rt_file_flush(void* files, const void* descriptor) {
  static_cast<FileTable*>(files)->Flush(Read<PackedArray>(descriptor));
}

void lyra_rt_file_flush_all(void* files) {
  static_cast<FileTable*>(files)->Flush();
}

auto lyra_rt_cancellation_for(void* files, const void* descriptor) -> void* {
  return Own(
      static_cast<FileTable*>(files)->CancellationFor(
          Read<PackedArray>(descriptor)));
}

auto lyra_rt_is_cancelled(const void* cancellation) -> void* {
  return Own(Read<ChannelCancellation>(cancellation).IsCancelled());
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

auto lyra_rt_packed_from_words(
    LyraSpan value_words, LyraSpan unknown_words, const void* type) -> void* {
  return Own(
      PackedArray::FromWords(
          std::span<const std::uint64_t>{
              static_cast<const std::uint64_t*>(value_words.data),
              value_words.count},
          std::span<const std::uint64_t>{
              static_cast<const std::uint64_t*>(unknown_words.data),
              unknown_words.count},
          Read<PackedType>(type)));
}

auto lyra_rt_make_packed_range(std::int64_t left, std::int64_t right) -> const
    void* {
  return ProgramLifetime(PackedRange{.left = left, .right = right});
}

auto lyra_rt_make_packed_type(LyraSpan dims, bool is_signed, bool is_four_state)
    -> const void* {
  const std::span<const void* const> entries{
      static_cast<const void* const*>(dims.data), dims.count};
  PackedType::Dims ranges(entries.size());
  std::ranges::transform(entries, ranges.begin(), [](const void* entry) {
    return *static_cast<const PackedRange*>(entry);
  });
  return ProgramLifetime(
      PackedType{std::move(ranges), is_signed, is_four_state});
}

void lyra_rt_writeln(void* files, void* descriptor, void* text) {
  static_cast<FileTable*>(files)->Writeln(
      *static_cast<PackedArray*>(descriptor), *static_cast<String*>(text));
}

void lyra_rt_write(void* files, void* descriptor, void* text) {
  static_cast<FileTable*>(files)->Write(
      *static_cast<PackedArray*>(descriptor), *static_cast<String*>(text));
}

auto lyra_rt_diagnostic(void* runtime) -> void* {
  return &static_cast<RuntimeEffects*>(runtime)->Diagnostic();
}

void lyra_rt_emit_info(void* dispatcher, const void* origin, const void* text) {
  static_cast<DiagnosticDispatcher*>(dispatcher)
      ->EmitInfo(Read<String>(origin), Read<String>(text));
}

void lyra_rt_emit_warning(
    void* dispatcher, const void* origin, const void* text) {
  static_cast<DiagnosticDispatcher*>(dispatcher)
      ->EmitWarning(Read<String>(origin), Read<String>(text));
}

void lyra_rt_emit_error(
    void* dispatcher, const void* origin, const void* text) {
  static_cast<DiagnosticDispatcher*>(dispatcher)
      ->EmitError(Read<String>(origin), Read<String>(text));
}

void lyra_rt_emit_fatal(
    void* dispatcher, const void* origin, const void* text) {
  static_cast<DiagnosticDispatcher*>(dispatcher)
      ->EmitFatal(Read<String>(origin), Read<String>(text));
}

auto lyra_rt_make_coroutine(void* (*ramp)(void*), void* env) -> void* {
  return GeneratedCallScope::Current().Arena().New<Coroutine<void>>(
      lyra::runtime::RunGeneratedProcess(ramp, env));
}

auto lyra_rt_closure_make(const void* definition, LyraSpan captures) -> void* {
  return GeneratedCallScope::Current().Arena().New<ClosureValue>(
      static_cast<const ClosureDefinition*>(definition),
      std::span<void* const>(
          static_cast<void* const*>(captures.data), captures.count));
}

auto lyra_rt_closure_capture(void* self, std::uint32_t index) -> void* {
  return static_cast<ClosureValue*>(self)->Capture(index);
}

void lyra_rt_submit_nba(void* runtime, void* closure) {
  static_cast<RuntimeEffects*>(runtime)->SubmitNba(TakeClosure(closure));
}

void lyra_rt_submit_postponed(void* runtime, void* closure) {
  static_cast<RuntimeEffects*>(runtime)->SubmitPostponed(TakeClosure(closure));
}

void lyra_rt_submit_observed(void* runtime, void* closure) {
  static_cast<RuntimeEffects*>(runtime)->SubmitObserved(TakeClosure(closure));
}

void lyra_rt_delay(
    void* runtime, const void* ticks, const void* precision_power) {
  auto& svc = *static_cast<RuntimeEffects*>(runtime);
  const CoroutineHandle token = svc.CurrentProcess().TopHandle();
  const std::int64_t tick_count = Read<PackedArray>(ticks).ToInt64();
  if (tick_count == 0) {
    svc.ScheduleInactive(token);
    return;
  }
  const lyra::SimDuration global = lyra::runtime::ScaleToGlobalTicks(
      static_cast<lyra::SimDuration>(tick_count),
      static_cast<std::int8_t>(Read<PackedArray>(precision_power).ToInt64()),
      svc.GlobalPrecisionPower());
  svc.ScheduleAtTime(svc.Now() + global, token);
}

auto lyra_rt_make_trigger(
    void* observable, const void* edge, const void* lsb_bit_offset,
    const void* bit_width) -> void* {
  return Own(Trigger(
      static_cast<Observable*>(observable), Read<PackedArray>(edge),
      Read<PackedArray>(lsb_bit_offset), Read<PackedArray>(bit_width)));
}

// The generated frame the process suspends is not a frame the engine ever sees
// -- it resumes the runtime-owned coroutine that drives it -- so the process to
// wake is the running one, read from the runtime.
void lyra_rt_wait_any(void* runtime, LyraSpan triggers) {
  auto& svc = *static_cast<RuntimeEffects*>(runtime);
  const std::span<Trigger* const> handles(
      static_cast<Trigger* const*>(triggers.data), triggers.count);
  std::vector<Trigger> collected;
  collected.reserve(triggers.count);
  for (const Trigger* handle : handles) {
    collected.push_back(*handle);
  }
  SubscribeValueChange(svc.CurrentProcess().TopHandle(), collected);
}

void lyra_rt_enter_target(void* runtime, void* target) {
  EnterCancellationTarget(
      *static_cast<RuntimeEffects*>(runtime),
      static_cast<CancellationTarget*>(target));
}

void lyra_rt_leave_target(void* runtime, void* target) {
  LeaveCancellationTarget(
      *static_cast<RuntimeEffects*>(runtime),
      static_cast<CancellationTarget*>(target));
}

void lyra_rt_disable(void* target, void* runtime) {
  static_cast<CancellationTarget*>(target)->Invalidate(
      *static_cast<RuntimeEffects*>(runtime));
}

auto lyra_rt_effect_names_target(void* effect, void* target) -> void* {
  // A control effect crosses as the target it names, since that is all one
  // carries, so naming a target is comparing the two.
  return Own(PackedArray::Bit(effect == target));
}

auto lyra_rt_invalidated_target(void* runtime) -> void* {
  return static_cast<RuntimeEffects*>(runtime)
      ->CurrentProcess()
      .OutermostInvalidatedTarget();
}

auto lyra_rt_has_invalidated_target(void* runtime) -> bool {
  return lyra_rt_invalidated_target(runtime) != nullptr;
}

void lyra_rt_settle_cancelled(void* effect) {
  GeneratedCallScope::Current().ActivationFrame().SettleCancelled(
      static_cast<CancellationTarget*>(effect));
}

auto lyra_rt_sim_time(void* runtime, const void* unit_power) -> void* {
  return Own(SimTimeInUnit(
      *static_cast<RuntimeEffects*>(runtime), Read<PackedArray>(unit_power)));
}

auto lyra_rt_stime(void* runtime, const void* unit_power) -> void* {
  return Own(STimeInUnit(
      *static_cast<RuntimeEffects*>(runtime), Read<PackedArray>(unit_power)));
}

auto lyra_rt_realtime(void* runtime, const void* unit_power) -> void* {
  return Own(RealTimeInUnit(
      *static_cast<RuntimeEffects*>(runtime), Read<PackedArray>(unit_power)));
}

void lyra_rt_finish(void* runtime, const void* level) {
  static_cast<RuntimeEffects*>(runtime)->RequestFinish(
      static_cast<int>(Read<PackedArray>(level).ToInt64()));
}

void lyra_rt_fatal_finish(void* runtime, const void* level) {
  static_cast<RuntimeEffects*>(runtime)->RequestFinish(
      static_cast<int>(Read<PackedArray>(level).ToInt64()), true);
}

auto lyra_rt_run_host_command(void* runtime, const void* command) -> void* {
  return Own(RunHostCommand(
      *static_cast<RuntimeEffects*>(runtime), Read<String>(command)));
}

auto lyra_rt_run_null_host_command() -> void* {
  return Own(RunHostCommand());
}

auto lyra_rt_test_plusargs(void* runtime, const void* user_string) -> void* {
  return Own(TestPlusargs(
      *static_cast<RuntimeEffects*>(runtime), Read<String>(user_string)));
}

auto lyra_rt_urandom(void* runtime) -> void* {
  return Own(lyra::runtime::Urandom(*static_cast<RuntimeEffects*>(runtime)));
}

auto lyra_rt_urandom_seeded(void* runtime, const void* seed) -> void* {
  return Own(
      lyra::runtime::UrandomSeeded(
          *static_cast<RuntimeEffects*>(runtime), Read<PackedArray>(seed)));
}

auto lyra_rt_urandom_range(
    void* runtime, const void* maxval, const void* minval) -> void* {
  return Own(
      lyra::runtime::UrandomRange(
          *static_cast<RuntimeEffects*>(runtime), Read<PackedArray>(maxval),
          Read<PackedArray>(minval)));
}

auto lyra_rt_random(void* runtime) -> void* {
  return Own(lyra::runtime::Random(*static_cast<RuntimeEffects*>(runtime)));
}

auto lyra_rt_dist_uniform(const void* seed, const void* start, const void* end)
    -> void* {
  return OwnDraw(
      lyra::runtime::DistUniform(
          Read<PackedArray>(seed), Read<PackedArray>(start),
          Read<PackedArray>(end)));
}

auto lyra_rt_dist_normal(
    const void* seed, const void* mean, const void* standard_deviation)
    -> void* {
  return OwnDraw(
      lyra::runtime::DistNormal(
          Read<PackedArray>(seed), Read<PackedArray>(mean),
          Read<PackedArray>(standard_deviation)));
}

auto lyra_rt_dist_exponential(const void* seed, const void* mean) -> void* {
  return OwnDraw(
      lyra::runtime::DistExponential(
          Read<PackedArray>(seed), Read<PackedArray>(mean)));
}

auto lyra_rt_dist_poisson(const void* seed, const void* mean) -> void* {
  return OwnDraw(
      lyra::runtime::DistPoisson(
          Read<PackedArray>(seed), Read<PackedArray>(mean)));
}

auto lyra_rt_dist_chi_square(const void* seed, const void* degrees_of_freedom)
    -> void* {
  return OwnDraw(
      lyra::runtime::DistChiSquare(
          Read<PackedArray>(seed), Read<PackedArray>(degrees_of_freedom)));
}

auto lyra_rt_dist_t(const void* seed, const void* degrees_of_freedom) -> void* {
  return OwnDraw(
      lyra::runtime::DistT(
          Read<PackedArray>(seed), Read<PackedArray>(degrees_of_freedom)));
}

auto lyra_rt_dist_erlang(const void* seed, const void* stages, const void* mean)
    -> void* {
  return OwnDraw(
      lyra::runtime::DistErlang(
          Read<PackedArray>(seed), Read<PackedArray>(stages),
          Read<PackedArray>(mean)));
}

void lyra_rt_register_initial(
    void* self, void* unit_instance, void* coroutine) {
  RegisterInitialProcess(
      static_cast<Scope*>(self), static_cast<Scope*>(unit_instance),
      std::move(*static_cast<Coroutine<void>*>(coroutine)));
}

void lyra_rt_register_final(void* self, void* unit_instance, void* coroutine) {
  RegisterFinalProcess(
      static_cast<Scope*>(self), static_cast<Scope*>(unit_instance),
      std::move(*static_cast<Coroutine<void>*>(coroutine)));
}

auto lyra_rt_make_segment(void* label, LyraSpan indices) -> void* {
  return GeneratedCallScope::Current().Arena().New<HierarchySegment>(
      std::string(static_cast<const char*>(label)), IndicesOf(indices));
}

auto lyra_rt_make_scope(const void* definition, void* parent, void* segment)
    -> void* {
  const auto* def = static_cast<const ScopeDefinition*>(definition);
  auto instance = std::make_unique<GeneratedScope>(
      static_cast<Scope*>(parent), *static_cast<HierarchySegment*>(segment),
      def);
  {
    GeneratedCallScope scope;
    def->construct(instance.get());
  }
  return instance.release();
}

auto lyra_rt_hierarchical_path(void* self) -> void* {
  return GeneratedCallScope::Current().Arena().New<String>(
      static_cast<Scope*>(self)->HierarchicalPath());
}

auto lyra_rt_add_owned_child(void* parent, void* child) -> void* {
  return static_cast<Scope*>(parent)->AddOwnedChild(
      std::unique_ptr<Scope>(static_cast<Scope*>(child)));
}

auto lyra_rt_resolve_visible_child(
    void* self, const void* head_name, LyraSpan head_indices) -> void* {
  return static_cast<Scope*>(self)->ResolveVisibleChild(
      static_cast<const char*>(head_name), IndicesOf(head_indices));
}

auto lyra_rt_get_child(void* self, const void* name, LyraSpan indices)
    -> void* {
  return static_cast<Scope*>(self)->GetChild(
      static_cast<const char*>(name), IndicesOf(indices));
}

auto lyra_rt_member_addr(void* self, std::uint32_t index) -> void* {
  return static_cast<GeneratedScope*>(self)->MemberAddress(index);
}

void lyra_rt_register_signal(void* self, const void* name, void* cell) {
  static_cast<Scope*>(self)->RegisterSignal(
      static_cast<const char*>(name), cell);
}

auto lyra_rt_get_signal(void* self, const void* name) -> void* {
  return static_cast<Scope*>(self)->GetSignal(static_cast<const char*>(name));
}

auto lyra_rt_cell_packed_get(void* cell) -> void* {
  return Own(static_cast<Var<PackedArray>*>(cell)->Get());
}

void lyra_rt_cell_packed_initialize(void* cell, const void* prototype) {
  static_cast<Var<PackedArray>*>(cell)->Initialize(
      Read<PackedArray>(prototype));
}

void lyra_rt_cell_packed_set(void* cell, const void* value) {
  static_cast<Var<PackedArray>*>(cell)->Set(
      lyra::runtime::current_runtime(), Read<PackedArray>(value));
}

auto lyra_rt_cell_string_get(void* cell) -> void* {
  return Own(static_cast<Var<String>*>(cell)->Get());
}

void lyra_rt_cell_string_initialize(void* cell, const void* prototype) {
  static_cast<Var<String>*>(cell)->Initialize(Read<String>(prototype));
}

void lyra_rt_cell_string_set(void* cell, const void* value) {
  static_cast<Var<String>*>(cell)->Set(
      lyra::runtime::current_runtime(), Read<String>(value));
}

auto lyra_rt_cell_real_get(void* cell) -> void* {
  return Own(static_cast<Var<Real>*>(cell)->Get());
}

void lyra_rt_cell_real_initialize(void* cell, const void* prototype) {
  static_cast<Var<Real>*>(cell)->Initialize(Read<Real>(prototype));
}

void lyra_rt_cell_real_set(void* cell, const void* value) {
  static_cast<Var<Real>*>(cell)->Set(
      lyra::runtime::current_runtime(), Read<Real>(value));
}

auto lyra_rt_cell_shortreal_get(void* cell) -> void* {
  return Own(static_cast<Var<ShortReal>*>(cell)->Get());
}

void lyra_rt_cell_shortreal_initialize(void* cell, const void* prototype) {
  static_cast<Var<ShortReal>*>(cell)->Initialize(Read<ShortReal>(prototype));
}

void lyra_rt_cell_shortreal_set(void* cell, const void* value) {
  static_cast<Var<ShortReal>*>(cell)->Set(
      lyra::runtime::current_runtime(), Read<ShortReal>(value));
}

// A procedural local whose value crosses a suspension. The cell is allocated in
// the activation frame (the driving coroutine owns it), so the handle the
// generated frame carries across a suspension points into activation-lifetime
// storage. A store overwrites the cell in place -- the first store installs the
// declared representation -- and a load copies the current value into the
// per-stretch scope, like any other value the boundary hands back. A procedural
// local is not observable, so no runtime handle threads through and no
// subscriber wakes.
auto lyra_rt_activation_frame_alloc_packed() -> void* {
  return GeneratedCallScope::Current()
      .ActivationFrame()
      .New<ActivationValueCell<PackedArray>>();
}

auto lyra_rt_activation_frame_alloc_string() -> void* {
  return GeneratedCallScope::Current()
      .ActivationFrame()
      .New<ActivationValueCell<String>>();
}

void lyra_rt_activation_frame_store_packed(void* cell, const void* value) {
  static_cast<ActivationValueCell<PackedArray>*>(cell)->Store(
      Read<PackedArray>(value));
}

void lyra_rt_activation_frame_store_string(void* cell, const void* value) {
  static_cast<ActivationValueCell<String>*>(cell)->Store(Read<String>(value));
}

auto lyra_rt_activation_frame_load_packed(const void* cell) -> void* {
  return Own(static_cast<const ActivationValueCell<PackedArray>*>(cell)->Get());
}

auto lyra_rt_activation_frame_load_string(const void* cell) -> void* {
  return Own(static_cast<const ActivationValueCell<String>*>(cell)->Get());
}

auto lyra_rt_packed_add(const void* lhs, const void* rhs) -> void* {
  return Own(Read<PackedArray>(lhs) + Read<PackedArray>(rhs));
}

auto lyra_rt_packed_sub(const void* lhs, const void* rhs) -> void* {
  return Own(Read<PackedArray>(lhs) - Read<PackedArray>(rhs));
}

auto lyra_rt_packed_mul(const void* lhs, const void* rhs) -> void* {
  return Own(Read<PackedArray>(lhs) * Read<PackedArray>(rhs));
}

auto lyra_rt_packed_div(const void* lhs, const void* rhs) -> void* {
  return Own(Read<PackedArray>(lhs) / Read<PackedArray>(rhs));
}

auto lyra_rt_packed_mod(const void* lhs, const void* rhs) -> void* {
  return Own(Read<PackedArray>(lhs) % Read<PackedArray>(rhs));
}

auto lyra_rt_packed_and(const void* lhs, const void* rhs) -> void* {
  return Own(Read<PackedArray>(lhs) & Read<PackedArray>(rhs));
}

auto lyra_rt_packed_or(const void* lhs, const void* rhs) -> void* {
  return Own(Read<PackedArray>(lhs) | Read<PackedArray>(rhs));
}

auto lyra_rt_packed_xor(const void* lhs, const void* rhs) -> void* {
  return Own(Read<PackedArray>(lhs) ^ Read<PackedArray>(rhs));
}

auto lyra_rt_packed_eq(const void* lhs, const void* rhs) -> void* {
  return Own(Read<PackedArray>(lhs) == Read<PackedArray>(rhs));
}

auto lyra_rt_packed_ne(const void* lhs, const void* rhs) -> void* {
  return Own(Read<PackedArray>(lhs) != Read<PackedArray>(rhs));
}

auto lyra_rt_packed_lt(const void* lhs, const void* rhs) -> void* {
  return Own(Read<PackedArray>(lhs) < Read<PackedArray>(rhs));
}

auto lyra_rt_packed_le(const void* lhs, const void* rhs) -> void* {
  return Own(Read<PackedArray>(lhs) <= Read<PackedArray>(rhs));
}

auto lyra_rt_packed_gt(const void* lhs, const void* rhs) -> void* {
  return Own(Read<PackedArray>(lhs) > Read<PackedArray>(rhs));
}

auto lyra_rt_packed_ge(const void* lhs, const void* rhs) -> void* {
  return Own(Read<PackedArray>(lhs) >= Read<PackedArray>(rhs));
}

auto lyra_rt_packed_logical_and(const void* lhs, const void* rhs) -> void* {
  return Own(Read<PackedArray>(lhs) && Read<PackedArray>(rhs));
}

auto lyra_rt_packed_logical_or(const void* lhs, const void* rhs) -> void* {
  return Own(Read<PackedArray>(lhs) || Read<PackedArray>(rhs));
}

auto lyra_rt_packed_neg(const void* operand) -> void* {
  return Own(-Read<PackedArray>(operand));
}

auto lyra_rt_packed_not(const void* operand) -> void* {
  return Own(~Read<PackedArray>(operand));
}

auto lyra_rt_packed_logical_not(const void* operand) -> void* {
  return Own(!Read<PackedArray>(operand));
}

auto lyra_rt_packed_inc(const void* operand) -> void* {
  PackedArray value = Read<PackedArray>(operand);
  ++value;
  return Own(std::move(value));
}

auto lyra_rt_packed_dec(const void* operand) -> void* {
  PackedArray value = Read<PackedArray>(operand);
  --value;
  return Own(std::move(value));
}

auto lyra_rt_packed_to_bool(const void* operand) -> bool {
  return static_cast<bool>(Read<PackedArray>(operand));
}

auto lyra_rt_packed_convert_from(const void* src, const void* type) -> void* {
  return Own(
      PackedArray::ConvertFrom(Read<PackedArray>(src), Read<PackedType>(type)));
}

auto lyra_rt_packed_from_int(std::int64_t value, const void* type) -> void* {
  return Own(PackedArray::FromInt(value, Read<PackedType>(type)));
}

auto lyra_rt_packed_from_bool(bool value) -> void* {
  return Own(PackedArray::FromBool(value));
}

auto lyra_rt_packed_to_int64(const void* value) -> std::int64_t {
  return Read<PackedArray>(value).ToInt64();
}

auto lyra_rt_packed_is_unknown(const void* value) -> void* {
  return Own(Read<PackedArray>(value).IsUnknown());
}

auto lyra_rt_packed_count_bits(const void* value, const void* control_bits)
    -> void* {
  return Own(
      Read<PackedArray>(value).CountBits(Read<PackedArray>(control_bits)));
}

auto lyra_rt_packed_clog2(const void* value) -> void* {
  return Own(Read<PackedArray>(value).Clog2());
}

auto lyra_rt_packed_pow(const void* base, const void* exponent) -> void* {
  return Own(Read<PackedArray>(base).Pow(Read<PackedArray>(exponent)));
}

auto lyra_rt_packed_concat(const void* lhs, const void* rhs) -> void* {
  return Own(
      PackedArray::Concat({Read<PackedArray>(lhs), Read<PackedArray>(rhs)}));
}

auto lyra_rt_packed_replicate(const void* operand, std::int64_t count)
    -> void* {
  return Own(
      PackedArray::Replicate(
          Read<PackedArray>(operand), static_cast<std::uint64_t>(count)));
}

auto lyra_rt_packed_shift_left(const void* value, const void* amount) -> void* {
  return Own(Read<PackedArray>(value).ShiftLeft(Read<PackedArray>(amount)));
}

auto lyra_rt_packed_logical_shift_right(const void* value, const void* amount)
    -> void* {
  return Own(
      Read<PackedArray>(value).LogicalShiftRight(Read<PackedArray>(amount)));
}

auto lyra_rt_packed_arithmetic_shift_right(
    const void* value, const void* amount) -> void* {
  return Own(
      Read<PackedArray>(value).ArithmeticShiftRight(Read<PackedArray>(amount)));
}

auto lyra_rt_packed_bitwise_xnor(const void* lhs, const void* rhs) -> void* {
  return Own(Read<PackedArray>(lhs).BitwiseXnor(Read<PackedArray>(rhs)));
}

auto lyra_rt_packed_logical_implication(const void* lhs, const void* rhs)
    -> void* {
  return Own(Read<PackedArray>(lhs).LogicalImplication(Read<PackedArray>(rhs)));
}

auto lyra_rt_packed_logical_equivalence(const void* lhs, const void* rhs)
    -> void* {
  return Own(Read<PackedArray>(lhs).LogicalEquivalence(Read<PackedArray>(rhs)));
}

auto lyra_rt_packed_case_equal(const void* lhs, const void* rhs) -> void* {
  return Own(Read<PackedArray>(lhs).CaseEqual(Read<PackedArray>(rhs)));
}

auto lyra_rt_packed_wildcard_equals(const void* lhs, const void* rhs) -> void* {
  return Own(Read<PackedArray>(lhs).WildcardEquals(Read<PackedArray>(rhs)));
}

auto lyra_rt_packed_casez_equals(const void* lhs, const void* rhs) -> void* {
  return Own(Read<PackedArray>(lhs).CasezEquals(Read<PackedArray>(rhs)));
}

auto lyra_rt_packed_casex_equals(const void* lhs, const void* rhs) -> void* {
  return Own(Read<PackedArray>(lhs).CasexEquals(Read<PackedArray>(rhs)));
}

auto lyra_rt_packed_merge_conditional(const void* lhs, const void* rhs)
    -> void* {
  return Own(Read<PackedArray>(lhs).MergeConditional(Read<PackedArray>(rhs)));
}

auto lyra_rt_packed_reduction_and(const void* value) -> void* {
  return Own(Read<PackedArray>(value).ReductionAnd());
}

auto lyra_rt_packed_reduction_or(const void* value) -> void* {
  return Own(Read<PackedArray>(value).ReductionOr());
}

auto lyra_rt_packed_reduction_xor(const void* value) -> void* {
  return Own(Read<PackedArray>(value).ReductionXor());
}

auto lyra_rt_packed_reduction_nand(const void* value) -> void* {
  return Own(Read<PackedArray>(value).ReductionNand());
}

auto lyra_rt_packed_reduction_nor(const void* value) -> void* {
  return Own(Read<PackedArray>(value).ReductionNor());
}

auto lyra_rt_packed_reduction_xnor(const void* value) -> void* {
  return Own(Read<PackedArray>(value).ReductionXnor());
}

auto lyra_rt_packed_element(const void* value, const void* index) -> void* {
  return Own(Read<PackedArray>(value).Element(Read<PackedArray>(index)));
}

auto lyra_rt_packed_with_element(
    const void* value, const void* index, const void* replacement) -> void* {
  return Own(
      Read<PackedArray>(value).WithElement(
          Read<PackedArray>(index), Read<PackedArray>(replacement)));
}

auto lyra_rt_packed_slice(
    const void* value, const void* a, const void* b, const void* form,
    const void* shape) -> void* {
  return Own(
      Read<PackedArray>(value).Slice(
          Read<PackedArray>(a), Read<PackedArray>(b), Read<PackedArray>(form),
          Read<PackedType>(shape)));
}

auto lyra_rt_packed_with_slice(
    const void* value, const void* a, const void* b, const void* form,
    const void* shape, const void* replacement) -> void* {
  return Own(
      Read<PackedArray>(value).WithSlice(
          Read<PackedArray>(a), Read<PackedArray>(b), Read<PackedArray>(form),
          Read<PackedType>(shape), Read<PackedArray>(replacement)));
}

// Materializes a borrowed packed view (a container element or slice read) into
// an owning value. On the execution backend a container access already copies
// the element out, so this is an idempotent copy that keeps the ownership shape
// the source-level `to_owned` names.
auto lyra_rt_packed_to_owned(const void* value) -> void* {
  return Own(Read<PackedArray>(value).ToOwned());
}

auto lyra_rt_string_from_packed_array(const void* bits) -> void* {
  return Own(String::FromPackedArray(Read<PackedArray>(bits)));
}

auto lyra_rt_string_string_cstr(const void* value) -> const char* {
  return Read<String>(value).CStr();
}

auto lyra_rt_string_len(const void* value) -> void* {
  return Own(Read<String>(value).Len());
}

auto lyra_rt_string_getc(const void* value, const void* index) -> void* {
  return Own(Read<String>(value).Getc(Read<PackedArray>(index)));
}

auto lyra_rt_string_element(const void* value, const void* index) -> void* {
  return Own(Read<String>(value).Element(Read<PackedArray>(index)));
}

// The functional character write (LRM 6.16.2): a new string with character
// `index` replaced. Synthesized at MIR-to-LIR for a string reached by an opaque
// handle, the string counterpart of `lyra_rt_dynarray_with_element`.
auto lyra_rt_string_with_element(
    const void* value, const void* index, const void* replacement) -> void* {
  return Own(
      Read<String>(value).WithElement(
          Read<PackedArray>(index), Read<PackedArray>(replacement)));
}

auto lyra_rt_string_toupper(const void* value) -> void* {
  return Own(Read<String>(value).Toupper());
}

auto lyra_rt_string_tolower(const void* value) -> void* {
  return Own(Read<String>(value).Tolower());
}

auto lyra_rt_string_compare(const void* lhs, const void* rhs) -> void* {
  return Own(Read<String>(lhs).Compare(Read<String>(rhs)));
}

auto lyra_rt_string_icompare(const void* lhs, const void* rhs) -> void* {
  return Own(Read<String>(lhs).Icompare(Read<String>(rhs)));
}

auto lyra_rt_string_substr(
    const void* value, const void* first, const void* last) -> void* {
  return Own(
      Read<String>(value).Substr(
          Read<PackedArray>(first), Read<PackedArray>(last)));
}

auto lyra_rt_string_concat(const void* lhs, const void* rhs) -> void* {
  return Own(Concat(Read<String>(lhs), Read<String>(rhs)));
}

auto lyra_rt_string_replicate(const void* operand, std::int64_t count)
    -> void* {
  return Own(Replicate(Read<String>(operand), count));
}

auto lyra_rt_string_atoi(const void* value) -> void* {
  return Own(Read<String>(value).Atoi());
}

auto lyra_rt_string_atohex(const void* value) -> void* {
  return Own(Read<String>(value).Atohex());
}

auto lyra_rt_string_atooct(const void* value) -> void* {
  return Own(Read<String>(value).Atooct());
}

auto lyra_rt_string_atobin(const void* value) -> void* {
  return Own(Read<String>(value).Atobin());
}

auto lyra_rt_string_atoreal(const void* value) -> void* {
  return Own(Read<String>(value).Atoreal());
}

// The formatting family mutates its receiver in the source language, so each
// entry copies the receiver, applies the mutation to the copy, and returns it.
auto lyra_rt_string_putc(
    const void* value, const void* index, const void* character) -> void* {
  String result = Read<String>(value);
  result.Putc(Read<PackedArray>(index), Read<PackedArray>(character));
  return Own(std::move(result));
}

auto lyra_rt_string_itoa(const void* value, const void* number) -> void* {
  String result = Read<String>(value);
  result.Itoa(Read<PackedArray>(number));
  return Own(std::move(result));
}

auto lyra_rt_string_hextoa(const void* value, const void* number) -> void* {
  String result = Read<String>(value);
  result.Hextoa(Read<PackedArray>(number));
  return Own(std::move(result));
}

auto lyra_rt_string_octtoa(const void* value, const void* number) -> void* {
  String result = Read<String>(value);
  result.Octtoa(Read<PackedArray>(number));
  return Own(std::move(result));
}

auto lyra_rt_string_bintoa(const void* value, const void* number) -> void* {
  String result = Read<String>(value);
  result.Bintoa(Read<PackedArray>(number));
  return Own(std::move(result));
}

auto lyra_rt_string_realtoa(const void* value, const void* number) -> void* {
  String result = Read<String>(value);
  result.Realtoa(Read<Real>(number));
  return Own(std::move(result));
}

auto lyra_rt_string_add(const void* lhs, const void* rhs) -> void* {
  return Own(Read<String>(lhs) + Read<String>(rhs));
}

auto lyra_rt_string_eq(const void* lhs, const void* rhs) -> void* {
  return Own(Read<String>(lhs) == Read<String>(rhs));
}

auto lyra_rt_string_case_equal(const void* lhs, const void* rhs) -> void* {
  return Own(Read<String>(lhs) == Read<String>(rhs));
}

auto lyra_rt_string_ne(const void* lhs, const void* rhs) -> void* {
  return Own(Read<String>(lhs) != Read<String>(rhs));
}

auto lyra_rt_string_lt(const void* lhs, const void* rhs) -> void* {
  return Own(Read<String>(lhs) < Read<String>(rhs));
}

auto lyra_rt_string_le(const void* lhs, const void* rhs) -> void* {
  return Own(Read<String>(lhs) <= Read<String>(rhs));
}

auto lyra_rt_string_gt(const void* lhs, const void* rhs) -> void* {
  return Own(Read<String>(lhs) > Read<String>(rhs));
}

auto lyra_rt_string_ge(const void* lhs, const void* rhs) -> void* {
  return Own(Read<String>(lhs) >= Read<String>(rhs));
}

auto lyra_rt_make_format_spec_of_kind(const void* kind) -> void* {
  return Own(FormatSpec(Read<PackedArray>(kind)));
}

auto lyra_rt_make_format_spec(
    const void* kind, const void* width, const void* precision,
    const void* zero_pad, const void* left_align, const void* timeunit_power)
    -> void* {
  return Own(FormatSpec(
      Read<PackedArray>(kind), Read<PackedArray>(width),
      Read<PackedArray>(precision), Read<PackedArray>(zero_pad),
      Read<PackedArray>(left_align), Read<PackedArray>(timeunit_power)));
}

auto lyra_rt_make_print_value_item_packed(const void* value, const void* spec)
    -> void* {
  return Own(PrintItem(
      PrintValueItem(Read<PackedArray>(value), Read<FormatSpec>(spec))));
}

auto lyra_rt_make_print_value_item_string(const void* value, const void* spec)
    -> void* {
  return Own(
      PrintItem(PrintValueItem(Read<String>(value), Read<FormatSpec>(spec))));
}

auto lyra_rt_real_add(const void* lhs, const void* rhs) -> void* {
  return Own(Read<Real>(lhs) + Read<Real>(rhs));
}

auto lyra_rt_real_sub(const void* lhs, const void* rhs) -> void* {
  return Own(Read<Real>(lhs) - Read<Real>(rhs));
}

auto lyra_rt_real_mul(const void* lhs, const void* rhs) -> void* {
  return Own(Read<Real>(lhs) * Read<Real>(rhs));
}

auto lyra_rt_real_div(const void* lhs, const void* rhs) -> void* {
  return Own(Read<Real>(lhs) / Read<Real>(rhs));
}

auto lyra_rt_real_neg(const void* operand) -> void* {
  return Own(-Read<Real>(operand));
}

auto lyra_rt_real_inc(const void* operand) -> void* {
  Real value = Read<Real>(operand);
  ++value;
  return Own(std::move(value));
}

auto lyra_rt_real_dec(const void* operand) -> void* {
  Real value = Read<Real>(operand);
  --value;
  return Own(std::move(value));
}

auto lyra_rt_real_eq(const void* lhs, const void* rhs) -> void* {
  return Own(Read<Real>(lhs) == Read<Real>(rhs));
}

auto lyra_rt_real_ne(const void* lhs, const void* rhs) -> void* {
  return Own(Read<Real>(lhs) != Read<Real>(rhs));
}

auto lyra_rt_real_lt(const void* lhs, const void* rhs) -> void* {
  return Own(Read<Real>(lhs) < Read<Real>(rhs));
}

auto lyra_rt_real_le(const void* lhs, const void* rhs) -> void* {
  return Own(Read<Real>(lhs) <= Read<Real>(rhs));
}

auto lyra_rt_real_gt(const void* lhs, const void* rhs) -> void* {
  return Own(Read<Real>(lhs) > Read<Real>(rhs));
}

auto lyra_rt_real_ge(const void* lhs, const void* rhs) -> void* {
  return Own(Read<Real>(lhs) >= Read<Real>(rhs));
}

auto lyra_rt_real_to_bool(const void* operand) -> bool {
  return static_cast<bool>(Read<Real>(operand));
}

auto lyra_rt_real_pow(const void* base, const void* exponent) -> void* {
  return Own(Read<Real>(base).Pow(Read<Real>(exponent)));
}

auto lyra_rt_real_round(const void* value) -> std::int64_t {
  return Read<Real>(value).Round();
}

auto lyra_rt_real_real_value(const void* value) -> double {
  return Read<Real>(value).Value();
}

auto lyra_rt_real_truncate(const void* value) -> std::int64_t {
  return Read<Real>(value).Truncate();
}

auto lyra_rt_real_to_bits(const void* value) -> std::int64_t {
  return Read<Real>(value).ToBits();
}

auto lyra_rt_real_from_bits(std::int64_t bits) -> void* {
  return Own(Real::FromBits(bits));
}

auto lyra_rt_real_ln(const void* value) -> void* {
  return Own(Read<Real>(value).Ln());
}

auto lyra_rt_real_log10(const void* value) -> void* {
  return Own(Read<Real>(value).Log10());
}

auto lyra_rt_real_exp(const void* value) -> void* {
  return Own(Read<Real>(value).Exp());
}

auto lyra_rt_real_sqrt(const void* value) -> void* {
  return Own(Read<Real>(value).Sqrt());
}

auto lyra_rt_real_floor(const void* value) -> void* {
  return Own(Read<Real>(value).Floor());
}

auto lyra_rt_real_ceil(const void* value) -> void* {
  return Own(Read<Real>(value).Ceil());
}

auto lyra_rt_real_sin(const void* value) -> void* {
  return Own(Read<Real>(value).Sin());
}

auto lyra_rt_real_cos(const void* value) -> void* {
  return Own(Read<Real>(value).Cos());
}

auto lyra_rt_real_tan(const void* value) -> void* {
  return Own(Read<Real>(value).Tan());
}

auto lyra_rt_real_asin(const void* value) -> void* {
  return Own(Read<Real>(value).Asin());
}

auto lyra_rt_real_acos(const void* value) -> void* {
  return Own(Read<Real>(value).Acos());
}

auto lyra_rt_real_atan(const void* value) -> void* {
  return Own(Read<Real>(value).Atan());
}

auto lyra_rt_real_atan2(const void* y, const void* x) -> void* {
  return Own(Read<Real>(y).Atan2(Read<Real>(x)));
}

auto lyra_rt_real_hypot(const void* x, const void* y) -> void* {
  return Own(Read<Real>(x).Hypot(Read<Real>(y)));
}

auto lyra_rt_real_sinh(const void* value) -> void* {
  return Own(Read<Real>(value).Sinh());
}

auto lyra_rt_real_cosh(const void* value) -> void* {
  return Own(Read<Real>(value).Cosh());
}

auto lyra_rt_real_tanh(const void* value) -> void* {
  return Own(Read<Real>(value).Tanh());
}

auto lyra_rt_real_asinh(const void* value) -> void* {
  return Own(Read<Real>(value).Asinh());
}

auto lyra_rt_real_acosh(const void* value) -> void* {
  return Own(Read<Real>(value).Acosh());
}

auto lyra_rt_real_atanh(const void* value) -> void* {
  return Own(Read<Real>(value).Atanh());
}

auto lyra_rt_real_const(double value) -> void* {
  return Own(Real{value});
}

auto lyra_rt_real_from_int64(std::int64_t value) -> void* {
  return Own(Real::FromInt(value));
}

auto lyra_rt_real_from_shortreal(const void* value) -> void* {
  return Own(Real{Read<ShortReal>(value)});
}

auto lyra_rt_real_from_real(const void* value) -> void* {
  return Own(Read<Real>(value));
}

auto lyra_rt_activation_frame_alloc_real() -> void* {
  return GeneratedCallScope::Current()
      .ActivationFrame()
      .New<ActivationValueCell<Real>>();
}

void lyra_rt_activation_frame_store_real(void* cell, const void* value) {
  static_cast<ActivationValueCell<Real>*>(cell)->Store(Read<Real>(value));
}

auto lyra_rt_activation_frame_load_real(const void* cell) -> void* {
  return Own(static_cast<const ActivationValueCell<Real>*>(cell)->Get());
}

auto lyra_rt_make_print_value_item_real(const void* value, const void* spec)
    -> void* {
  return Own(
      PrintItem(PrintValueItem(Read<Real>(value), Read<FormatSpec>(spec))));
}

auto lyra_rt_shortreal_add(const void* lhs, const void* rhs) -> void* {
  return Own(Read<ShortReal>(lhs) + Read<ShortReal>(rhs));
}

auto lyra_rt_shortreal_sub(const void* lhs, const void* rhs) -> void* {
  return Own(Read<ShortReal>(lhs) - Read<ShortReal>(rhs));
}

auto lyra_rt_shortreal_mul(const void* lhs, const void* rhs) -> void* {
  return Own(Read<ShortReal>(lhs) * Read<ShortReal>(rhs));
}

auto lyra_rt_shortreal_div(const void* lhs, const void* rhs) -> void* {
  return Own(Read<ShortReal>(lhs) / Read<ShortReal>(rhs));
}

auto lyra_rt_shortreal_neg(const void* operand) -> void* {
  return Own(-Read<ShortReal>(operand));
}

auto lyra_rt_shortreal_inc(const void* operand) -> void* {
  ShortReal value = Read<ShortReal>(operand);
  ++value;
  return Own(std::move(value));
}

auto lyra_rt_shortreal_dec(const void* operand) -> void* {
  ShortReal value = Read<ShortReal>(operand);
  --value;
  return Own(std::move(value));
}

auto lyra_rt_shortreal_eq(const void* lhs, const void* rhs) -> void* {
  return Own(Read<ShortReal>(lhs) == Read<ShortReal>(rhs));
}

auto lyra_rt_shortreal_ne(const void* lhs, const void* rhs) -> void* {
  return Own(Read<ShortReal>(lhs) != Read<ShortReal>(rhs));
}

auto lyra_rt_shortreal_lt(const void* lhs, const void* rhs) -> void* {
  return Own(Read<ShortReal>(lhs) < Read<ShortReal>(rhs));
}

auto lyra_rt_shortreal_le(const void* lhs, const void* rhs) -> void* {
  return Own(Read<ShortReal>(lhs) <= Read<ShortReal>(rhs));
}

auto lyra_rt_shortreal_gt(const void* lhs, const void* rhs) -> void* {
  return Own(Read<ShortReal>(lhs) > Read<ShortReal>(rhs));
}

auto lyra_rt_shortreal_ge(const void* lhs, const void* rhs) -> void* {
  return Own(Read<ShortReal>(lhs) >= Read<ShortReal>(rhs));
}

auto lyra_rt_shortreal_to_bool(const void* operand) -> bool {
  return static_cast<bool>(Read<ShortReal>(operand));
}

auto lyra_rt_shortreal_pow(const void* base, const void* exponent) -> void* {
  return Own(Read<ShortReal>(base).Pow(Read<ShortReal>(exponent)));
}

auto lyra_rt_shortreal_round(const void* value) -> std::int64_t {
  return Read<ShortReal>(value).Round();
}

auto lyra_rt_shortreal_real_value(const void* value) -> float {
  return Read<ShortReal>(value).Value();
}

auto lyra_rt_shortreal_to_bits(const void* value) -> std::int64_t {
  return Read<ShortReal>(value).ToBits();
}

auto lyra_rt_shortreal_from_bits(std::int64_t bits) -> void* {
  return Own(ShortReal::FromBits(bits));
}

auto lyra_rt_shortreal_const(float value) -> void* {
  return Own(ShortReal{value});
}

auto lyra_rt_shortreal_from_int64(std::int64_t value) -> void* {
  return Own(ShortReal::FromInt(value));
}

auto lyra_rt_shortreal_from_real(const void* value) -> void* {
  return Own(ShortReal{Read<Real>(value)});
}

auto lyra_rt_activation_frame_alloc_shortreal() -> void* {
  return GeneratedCallScope::Current()
      .ActivationFrame()
      .New<ActivationValueCell<ShortReal>>();
}

void lyra_rt_activation_frame_store_shortreal(void* cell, const void* value) {
  static_cast<ActivationValueCell<ShortReal>*>(cell)->Store(
      Read<ShortReal>(value));
}

auto lyra_rt_activation_frame_load_shortreal(const void* cell) -> void* {
  return Own(static_cast<const ActivationValueCell<ShortReal>*>(cell)->Get());
}

auto lyra_rt_make_print_value_item_shortreal(
    const void* value, const void* spec) -> void* {
  return Own(PrintItem(
      PrintValueItem(Read<ShortReal>(value), Read<FormatSpec>(spec))));
}

auto lyra_rt_chandle_eq(void* lhs, void* rhs) -> void* {
  return Own(Chandle{lhs} == Chandle{rhs});
}

auto lyra_rt_chandle_ne(void* lhs, void* rhs) -> void* {
  return Own(Chandle{lhs} != Chandle{rhs});
}

auto lyra_rt_chandle_case_equal(void* lhs, void* rhs) -> void* {
  return Own(Chandle{lhs}.CaseEqual(Chandle{rhs}));
}

auto lyra_rt_chandle_to_bool(void* operand) -> bool {
  return static_cast<bool>(Chandle{operand});
}

// Boxes a value-domain handle into a type-erased `RuntimeValue`. A value
// crosses this way exactly where it states a representation the entry receiving
// it has no other way to know: a product's components, each of its own domain,
// and a container construction's element prototype. The domain rides in the
// symbol name, so the generated side never inspects the value's runtime
// representation.
auto lyra_rt_value_box_packed(const void* value) -> void* {
  return Own(RuntimeValue{Read<PackedArray>(value)});
}

auto lyra_rt_value_box_string(const void* value) -> void* {
  return Own(RuntimeValue{Read<String>(value)});
}

auto lyra_rt_value_box_real(const void* value) -> void* {
  return Own(RuntimeValue{Read<Real>(value)});
}

auto lyra_rt_value_box_shortreal(const void* value) -> void* {
  return Own(RuntimeValue{Read<ShortReal>(value)});
}

// A chandle's handle is the pointer it carries, not a pointer to a runtime
// object, so its box wraps the pointer directly rather than reading a value
// object out of it.
auto lyra_rt_value_box_chandle(void* value) -> void* {
  return Own(RuntimeValue{Chandle{value}});
}

auto lyra_rt_value_box_tuple(const void* value) -> void* {
  return Own(RuntimeValue{Read<RuntimeTuple>(value)});
}

auto lyra_rt_value_box_dynarray(const void* value) -> void* {
  return Own(RuntimeValue{Read<RuntimeDynamicArray>(value)});
}

auto lyra_rt_value_box_unpackedarray(const void* value) -> void* {
  return Own(RuntimeValue{Read<RuntimeUnpackedArray>(value)});
}

auto lyra_rt_tuple_make(LyraSpan components) -> void* {
  std::span<RuntimeValue*> handles(
      static_cast<RuntimeValue**>(components.data), components.count);
  std::vector<RuntimeValue> collected;
  collected.reserve(components.count);
  for (RuntimeValue* handle : handles) {
    collected.push_back(std::move(*handle));
  }
  return Own(RuntimeTuple(std::move(collected)));
}

auto lyra_rt_tuple_extract(const void* tuple, std::int64_t index) -> void* {
  const RuntimeValue& component =
      Read<RuntimeTuple>(tuple).Component(static_cast<std::size_t>(index));
  return std::visit(
      [](const auto& value) -> void* {
        using T = std::decay_t<decltype(value)>;
        if constexpr (std::is_same_v<T, Chandle>) {
          return value.Ptr();
        } else {
          return Own(value);
        }
      },
      component.value);
}

auto lyra_rt_tuple_update(const void* tuple, std::int64_t index, void* value)
    -> void* {
  RuntimeTuple result = Read<RuntimeTuple>(tuple);
  const auto slot = static_cast<std::size_t>(index);
  RuntimeValue replacement = std::visit(
      [&](const auto& current) -> RuntimeValue {
        using T = std::decay_t<decltype(current)>;
        if constexpr (std::is_same_v<T, Chandle>) {
          return RuntimeValue{Chandle{value}};
        } else {
          return RuntimeValue{Read<T>(value)};
        }
      },
      result.Component(slot).value);
  result.SetComponent(slot, std::move(replacement));
  return Own(std::move(result));
}

auto lyra_rt_tuple_eq(const void* lhs, const void* rhs) -> void* {
  return Own(Read<RuntimeTuple>(lhs) == Read<RuntimeTuple>(rhs));
}

auto lyra_rt_tuple_ne(const void* lhs, const void* rhs) -> void* {
  return Own(Read<RuntimeTuple>(lhs) != Read<RuntimeTuple>(rhs));
}

auto lyra_rt_tuple_case_equal(const void* lhs, const void* rhs) -> void* {
  return Own(Read<RuntimeTuple>(lhs).CaseEqual(Read<RuntimeTuple>(rhs)));
}

auto lyra_rt_tuple_is_unknown(const void* value) -> void* {
  return Own(Read<RuntimeTuple>(value).IsUnknown());
}

auto lyra_rt_cell_tuple_get(void* cell) -> void* {
  return Own(static_cast<Var<RuntimeTuple>*>(cell)->Get());
}

void lyra_rt_cell_tuple_initialize(void* cell, const void* prototype) {
  static_cast<Var<RuntimeTuple>*>(cell)->Initialize(
      Read<RuntimeTuple>(prototype));
}

void lyra_rt_cell_tuple_set(void* cell, const void* value) {
  static_cast<Var<RuntimeTuple>*>(cell)->Set(
      lyra::runtime::current_runtime(), Read<RuntimeTuple>(value));
}

auto lyra_rt_activation_frame_alloc_tuple() -> void* {
  return GeneratedCallScope::Current()
      .ActivationFrame()
      .New<ActivationValueCell<RuntimeTuple>>();
}

void lyra_rt_activation_frame_store_tuple(void* cell, const void* value) {
  static_cast<ActivationValueCell<RuntimeTuple>*>(cell)->Store(
      Read<RuntimeTuple>(value));
}

auto lyra_rt_activation_frame_load_tuple(const void* cell) -> void* {
  return Own(
      static_cast<const ActivationValueCell<RuntimeTuple>*>(cell)->Get());
}

auto lyra_rt_dynarray_default(void* prototype) -> void* {
  return Own(RuntimeDynamicArray(lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_dynarray_new(const void* size, void* prototype) -> void* {
  return Own(RuntimeDynamicArray(
      Read<PackedArray>(size), lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_dynarray_new_copy(
    const void* size, void* prototype, const void* src) -> void* {
  return Own(RuntimeDynamicArray(
      Read<PackedArray>(size), lyra::runtime::ErasedValue(prototype),
      Read<RuntimeDynamicArray>(src)));
}

auto lyra_rt_dynarray_from_literal(
    void* prototype, LyraSpan unit, std::int64_t count) -> void* {
  RuntimeValue element_default = lyra::runtime::ErasedValue(prototype);
  std::vector<RuntimeValue> elements =
      lyra::runtime::ReplicateLiteral(element_default, unit, count);
  return Own(
      RuntimeDynamicArray(std::move(element_default), std::move(elements)));
}

// Reads element `index`, copying it out across the opaque-handle boundary as a
// handle of the element's own domain (a chandle's handle is the pointer it
// carries). An out-of-range index reads the element default (LRM 7.4.5).
auto lyra_rt_dynarray_element(const void* array, const void* index) -> void* {
  return lyra::runtime::ElementHandle(
      Read<RuntimeDynamicArray>(array).Element(Read<PackedArray>(index)));
}

// The functional element write (LRM 7.4.6): yields a new array with element
// `index` replaced. The incoming value is a handle of the element domain, boxed
// into the erased representation by the domain the element default names.
auto lyra_rt_dynarray_with_element(
    const void* array, const void* index, void* value) -> void* {
  const auto& source = Read<RuntimeDynamicArray>(array);
  return Own(source.WithElement(
      Read<PackedArray>(index),
      lyra::runtime::ElementFrom(source.ElementDefault(), value)));
}

auto lyra_rt_dynarray_delete(const void* array) -> void* {
  return Own(Read<RuntimeDynamicArray>(array).Delete());
}

auto lyra_rt_dynarray_size(const void* array) -> void* {
  return Own(Read<RuntimeDynamicArray>(array).Size());
}

auto lyra_rt_dynarray_eq(const void* lhs, const void* rhs) -> void* {
  return Own(Read<RuntimeDynamicArray>(lhs) == Read<RuntimeDynamicArray>(rhs));
}

auto lyra_rt_dynarray_ne(const void* lhs, const void* rhs) -> void* {
  return Own(Read<RuntimeDynamicArray>(lhs) != Read<RuntimeDynamicArray>(rhs));
}

auto lyra_rt_dynarray_case_equal(const void* lhs, const void* rhs) -> void* {
  return Own(
      Read<RuntimeDynamicArray>(lhs).CaseEqual(Read<RuntimeDynamicArray>(rhs)));
}

auto lyra_rt_cell_dynarray_get(void* cell) -> void* {
  return Own(static_cast<Var<RuntimeDynamicArray>*>(cell)->Get());
}

void lyra_rt_cell_dynarray_initialize(void* cell, const void* prototype) {
  static_cast<Var<RuntimeDynamicArray>*>(cell)->Initialize(
      Read<RuntimeDynamicArray>(prototype));
}

void lyra_rt_cell_dynarray_set(void* cell, const void* value) {
  static_cast<Var<RuntimeDynamicArray>*>(cell)->Set(
      lyra::runtime::current_runtime(), Read<RuntimeDynamicArray>(value));
}

auto lyra_rt_activation_frame_alloc_dynarray() -> void* {
  return GeneratedCallScope::Current()
      .ActivationFrame()
      .New<ActivationValueCell<RuntimeDynamicArray>>();
}

void lyra_rt_activation_frame_store_dynarray(void* cell, const void* value) {
  static_cast<ActivationValueCell<RuntimeDynamicArray>*>(cell)->Store(
      Read<RuntimeDynamicArray>(value));
}

auto lyra_rt_activation_frame_load_dynarray(const void* cell) -> void* {
  return Own(
      static_cast<const ActivationValueCell<RuntimeDynamicArray>*>(cell)
          ->Get());
}

auto lyra_rt_unpackedarray_from_literal(
    void* prototype, LyraSpan unit, std::int64_t count) -> void* {
  RuntimeValue element_default = lyra::runtime::ErasedValue(prototype);
  std::vector<RuntimeValue> unit_elements =
      lyra::runtime::ReplicateLiteral(element_default, unit, 1);
  return Own(RuntimeUnpackedArray(
      std::move(element_default), std::move(unit_elements),
      static_cast<std::size_t>(count)));
}

auto lyra_rt_unpackedarray_merge_conditional(const void* lhs, const void* rhs)
    -> void* {
  return Own(
      Read<RuntimeUnpackedArray>(lhs).MergeConditional(
          Read<RuntimeUnpackedArray>(rhs)));
}

// Reads the element the source index names, resolved against the declared range
// `[left:right]` the receiver's static type supplies. An index the range does
// not name reads the element default (LRM 7.4.5).
auto lyra_rt_unpackedarray_element(
    const void* array, const void* index, const void* left, const void* right)
    -> void* {
  return lyra::runtime::ElementHandle(
      Read<RuntimeUnpackedArray>(array).Element(
          Read<PackedArray>(index), Read<PackedArray>(left),
          Read<PackedArray>(right)));
}

// The functional element write (LRM 7.4.5): yields a new array with the named
// element replaced, and the original unchanged when the range does not name it.
auto lyra_rt_unpackedarray_with_element(
    const void* array, const void* index, const void* left, const void* right,
    void* value) -> void* {
  const auto& source = Read<RuntimeUnpackedArray>(array);
  return Own(source.WithElement(
      Read<PackedArray>(index), Read<PackedArray>(left),
      Read<PackedArray>(right),
      lyra::runtime::ElementFrom(source.ElementDefault(), value)));
}

auto lyra_rt_packed_from_string(const void* text, const void* type) -> void* {
  return Own(
      PackedArray::FromString(Read<String>(text), Read<PackedType>(type)));
}

auto lyra_rt_unpackedarray_from_string(
    const void* text, const void* element_type, const void* count) -> void* {
  return Own(
      RuntimeUnpackedArray::FromString(
          Read<String>(text), Read<PackedType>(element_type),
          Read<PackedArray>(count)));
}

auto lyra_rt_unpackedarray_from_packed_array(
    const void* bits, const void* element_type, const void* count) -> void* {
  return Own(
      RuntimeUnpackedArray::FromPackedArray(
          Read<PackedArray>(bits), Read<PackedType>(element_type),
          Read<PackedArray>(count)));
}

auto lyra_rt_unpackedarray_count_bits(
    const void* value, const void* control_bits) -> void* {
  return Own(
      Read<RuntimeUnpackedArray>(value).CountBits(
          Read<PackedArray>(control_bits)));
}

auto lyra_rt_tuple_count_bits(const void* value, const void* control_bits)
    -> void* {
  return Own(
      Read<RuntimeTuple>(value).CountBits(Read<PackedArray>(control_bits)));
}

auto lyra_rt_dynarray_count_bits(const void* value, const void* control_bits)
    -> void* {
  return Own(
      Read<RuntimeDynamicArray>(value).CountBits(
          Read<PackedArray>(control_bits)));
}

auto lyra_rt_string_count_bits(const void* value, const void* control_bits)
    -> void* {
  return Own(Read<String>(value).CountBits(Read<PackedArray>(control_bits)));
}

auto lyra_rt_string_bitstream_width(const void* value) -> void* {
  return Own(Read<String>(value).BitstreamWidth());
}

auto lyra_rt_tuple_bitstream_width(const void* value) -> void* {
  return Own(Read<RuntimeTuple>(value).BitstreamWidth());
}

auto lyra_rt_dynarray_bitstream_width(const void* value) -> void* {
  return Own(Read<RuntimeDynamicArray>(value).BitstreamWidth());
}

auto lyra_rt_unpackedarray_bitstream_width(const void* value) -> void* {
  return Own(Read<RuntimeUnpackedArray>(value).BitstreamWidth());
}

auto lyra_rt_unpackedarray_size(const void* array) -> void* {
  return Own(Read<RuntimeUnpackedArray>(array).Size());
}

auto lyra_rt_unpackedarray_slice(
    const void* array, const void* a, const void* b, const void* form,
    const void* left, const void* right) -> void* {
  return Own(
      Read<RuntimeUnpackedArray>(array).Slice(
          Read<PackedArray>(a), Read<PackedArray>(b), Read<PackedArray>(form),
          Read<PackedArray>(left), Read<PackedArray>(right)));
}

auto lyra_rt_unpackedarray_eq(const void* lhs, const void* rhs) -> void* {
  return Own(
      Read<RuntimeUnpackedArray>(lhs) == Read<RuntimeUnpackedArray>(rhs));
}

auto lyra_rt_unpackedarray_ne(const void* lhs, const void* rhs) -> void* {
  return Own(
      Read<RuntimeUnpackedArray>(lhs) != Read<RuntimeUnpackedArray>(rhs));
}

auto lyra_rt_unpackedarray_case_equal(const void* lhs, const void* rhs)
    -> void* {
  return Own(
      Read<RuntimeUnpackedArray>(lhs).CaseEqual(
          Read<RuntimeUnpackedArray>(rhs)));
}

auto lyra_rt_cell_unpackedarray_get(void* cell) -> void* {
  return Own(static_cast<Var<RuntimeUnpackedArray>*>(cell)->Get());
}

void lyra_rt_cell_unpackedarray_initialize(void* cell, const void* prototype) {
  static_cast<Var<RuntimeUnpackedArray>*>(cell)->Initialize(
      Read<RuntimeUnpackedArray>(prototype));
}

void lyra_rt_cell_unpackedarray_set(void* cell, const void* value) {
  static_cast<Var<RuntimeUnpackedArray>*>(cell)->Set(
      lyra::runtime::current_runtime(), Read<RuntimeUnpackedArray>(value));
}

auto lyra_rt_activation_frame_alloc_unpackedarray() -> void* {
  return GeneratedCallScope::Current()
      .ActivationFrame()
      .New<ActivationValueCell<RuntimeUnpackedArray>>();
}

void lyra_rt_activation_frame_store_unpackedarray(
    void* cell, const void* value) {
  static_cast<ActivationValueCell<RuntimeUnpackedArray>*>(cell)->Store(
      Read<RuntimeUnpackedArray>(value));
}

auto lyra_rt_activation_frame_load_unpackedarray(const void* cell) -> void* {
  return Own(
      static_cast<const ActivationValueCell<RuntimeUnpackedArray>*>(cell)
          ->Get());
}

auto lyra_rt_queue_default(void* prototype) -> void* {
  return Own(RuntimeQueue(lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_queue_default_bounded(void* prototype, const void* max_bound)
    -> void* {
  return Own(RuntimeQueue(
      lyra::runtime::ErasedValue(prototype), Read<PackedArray>(max_bound)));
}

auto lyra_rt_queue_from_literal(
    void* prototype, LyraSpan unit, std::int64_t count) -> void* {
  RuntimeValue element_default = lyra::runtime::ErasedValue(prototype);
  std::vector<RuntimeValue> elements =
      lyra::runtime::ReplicateLiteral(element_default, unit, count);
  return Own(RuntimeQueue(std::move(element_default), std::move(elements)));
}

auto lyra_rt_queue_from_literal_bounded(
    void* prototype, LyraSpan unit, std::int64_t count, const void* max_bound)
    -> void* {
  RuntimeValue element_default = lyra::runtime::ErasedValue(prototype);
  std::vector<RuntimeValue> elements =
      lyra::runtime::ReplicateLiteral(element_default, unit, count);
  return Own(RuntimeQueue(
      std::move(element_default), std::move(elements),
      Read<PackedArray>(max_bound)));
}

auto lyra_rt_queue_conform_bound(const void* queue, const void* max_bound)
    -> void* {
  return Own(
      Read<RuntimeQueue>(queue).ConformBound(Read<PackedArray>(max_bound)));
}

auto lyra_rt_queue_element(const void* queue, const void* index) -> void* {
  return lyra::runtime::ElementHandle(
      Read<RuntimeQueue>(queue).Element(Read<PackedArray>(index)));
}

auto lyra_rt_queue_with_element(
    const void* queue, const void* index, void* value) -> void* {
  const auto& source = Read<RuntimeQueue>(queue);
  return Own(source.WithElement(
      Read<PackedArray>(index),
      lyra::runtime::ElementFrom(source.ElementDefault(), value)));
}

auto lyra_rt_queue_slice(
    const void* queue, const void* anchor, const void* extent, const void* form)
    -> void* {
  return Own(
      Read<RuntimeQueue>(queue).Slice(
          Read<PackedArray>(anchor), Read<PackedArray>(extent),
          Read<PackedArray>(form)));
}

auto lyra_rt_queue_size(const void* queue) -> void* {
  return Own(Read<RuntimeQueue>(queue).Size());
}

auto lyra_rt_queue_push_back(const void* queue, void* item) -> void* {
  const auto& source = Read<RuntimeQueue>(queue);
  return Own(source.PushBack(
      lyra::runtime::ElementFrom(source.ElementDefault(), item)));
}

auto lyra_rt_queue_push_front(const void* queue, void* item) -> void* {
  const auto& source = Read<RuntimeQueue>(queue);
  return Own(source.PushFront(
      lyra::runtime::ElementFrom(source.ElementDefault(), item)));
}

auto lyra_rt_queue_insert(const void* queue, const void* index, void* item)
    -> void* {
  const auto& source = Read<RuntimeQueue>(queue);
  return Own(source.Insert(
      Read<PackedArray>(index),
      lyra::runtime::ElementFrom(source.ElementDefault(), item)));
}

auto lyra_rt_queue_delete(const void* queue) -> void* {
  return Own(Read<RuntimeQueue>(queue).Delete());
}

auto lyra_rt_queue_eq(const void* lhs, const void* rhs) -> void* {
  return Own(Read<RuntimeQueue>(lhs) == Read<RuntimeQueue>(rhs));
}

auto lyra_rt_queue_ne(const void* lhs, const void* rhs) -> void* {
  return Own(Read<RuntimeQueue>(lhs) != Read<RuntimeQueue>(rhs));
}

auto lyra_rt_queue_case_equal(const void* lhs, const void* rhs) -> void* {
  return Own(Read<RuntimeQueue>(lhs).CaseEqual(Read<RuntimeQueue>(rhs)));
}

auto lyra_rt_queue_bitstream_width(const void* queue) -> void* {
  return Own(Read<RuntimeQueue>(queue).BitstreamWidth());
}

auto lyra_rt_queue_count_bits(const void* queue, const void* control_bits)
    -> void* {
  return Own(
      Read<RuntimeQueue>(queue).CountBits(Read<PackedArray>(control_bits)));
}

auto lyra_rt_value_box_queue(const void* value) -> void* {
  return Own(RuntimeValue{Read<RuntimeQueue>(value)});
}

auto lyra_rt_cell_queue_get(void* cell) -> void* {
  return Own(static_cast<Var<RuntimeQueue>*>(cell)->Get());
}

void lyra_rt_cell_queue_initialize(void* cell, const void* prototype) {
  static_cast<Var<RuntimeQueue>*>(cell)->Initialize(
      Read<RuntimeQueue>(prototype));
}

void lyra_rt_cell_queue_set(void* cell, const void* value) {
  static_cast<Var<RuntimeQueue>*>(cell)->Set(
      lyra::runtime::current_runtime(), Read<RuntimeQueue>(value));
}

auto lyra_rt_activation_frame_alloc_queue() -> void* {
  return GeneratedCallScope::Current()
      .ActivationFrame()
      .New<ActivationValueCell<RuntimeQueue>>();
}

void lyra_rt_activation_frame_store_queue(void* cell, const void* value) {
  static_cast<ActivationValueCell<RuntimeQueue>*>(cell)->Store(
      Read<RuntimeQueue>(value));
}

auto lyra_rt_activation_frame_load_queue(const void* cell) -> void* {
  return Own(
      static_cast<const ActivationValueCell<RuntimeQueue>*>(cell)->Get());
}

auto lyra_rt_assocarray_default(void* prototype) -> void* {
  return Own(RuntimeAssociativeArray(lyra::runtime::ErasedValue(prototype)));
}

// LRM 7.9.11 `'{index: value, ...}`: each entry crosses as the product of the
// index and the element it stores. A product already holds its components
// erased, which is the form a keyed container needs both of them in: it knows
// the representation of neither in advance.
auto lyra_rt_assocarray_from_entries(void* prototype, LyraSpan entries)
    -> void* {
  return Own(
      lyra::runtime::SeedAssociativeEntries(
          RuntimeAssociativeArray(lyra::runtime::ErasedValue(prototype)),
          entries));
}

auto lyra_rt_assocarray_from_entries_default(
    void* prototype, LyraSpan entries, void* user_default) -> void* {
  RuntimeValue element_default = lyra::runtime::ErasedValue(prototype);
  RuntimeValue miss = lyra::runtime::ElementFrom(element_default, user_default);
  return Own(
      lyra::runtime::SeedAssociativeEntries(
          RuntimeAssociativeArray(std::move(element_default), std::move(miss)),
          entries));
}

auto lyra_rt_assocarray_element(const void* array, const void* index) -> void* {
  return lyra::runtime::ElementHandle(
      Read<RuntimeAssociativeArray>(array).Element(Read<RuntimeValue>(index)));
}

auto lyra_rt_assocarray_with_element(
    const void* array, const void* index, void* value) -> void* {
  const auto& source = Read<RuntimeAssociativeArray>(array);
  return Own(source.WithElement(
      Read<RuntimeValue>(index),
      lyra::runtime::ElementFrom(source.ElementDefault(), value)));
}

auto lyra_rt_assocarray_exists(const void* array, const void* index) -> void* {
  return Own(
      Read<RuntimeAssociativeArray>(array).Exists(Read<RuntimeValue>(index)));
}

auto lyra_rt_assocarray_size(const void* array) -> void* {
  return Own(Read<RuntimeAssociativeArray>(array).Size());
}

auto lyra_rt_assocarray_delete(const void* array) -> void* {
  return Own(Read<RuntimeAssociativeArray>(array).Delete());
}

auto lyra_rt_assocarray_eq(const void* lhs, const void* rhs) -> void* {
  return Own(
      Read<RuntimeAssociativeArray>(lhs) == Read<RuntimeAssociativeArray>(rhs));
}

auto lyra_rt_assocarray_ne(const void* lhs, const void* rhs) -> void* {
  return Own(
      Read<RuntimeAssociativeArray>(lhs) != Read<RuntimeAssociativeArray>(rhs));
}

auto lyra_rt_assocarray_case_equal(const void* lhs, const void* rhs) -> void* {
  return Own(
      Read<RuntimeAssociativeArray>(lhs).CaseEqual(
          Read<RuntimeAssociativeArray>(rhs)));
}

auto lyra_rt_assocarray_bitstream_width(const void* array) -> void* {
  return Own(Read<RuntimeAssociativeArray>(array).BitstreamWidth());
}

auto lyra_rt_assocarray_assoc_min_index(const void* array, void* empty)
    -> void* {
  const std::optional<RuntimeValue> index =
      Read<RuntimeAssociativeArray>(array).FirstIndex();
  return index.has_value() ? lyra::runtime::ElementHandle(*index) : empty;
}

auto lyra_rt_assocarray_assoc_max_index(const void* array, void* empty)
    -> void* {
  const std::optional<RuntimeValue> index =
      Read<RuntimeAssociativeArray>(array).LastIndex();
  return index.has_value() ? lyra::runtime::ElementHandle(*index) : empty;
}

auto lyra_rt_assocarray_count_bits(const void* array, const void* control_bits)
    -> void* {
  return Own(
      Read<RuntimeAssociativeArray>(array).CountBits(
          Read<PackedArray>(control_bits)));
}

auto lyra_rt_value_box_assocarray(const void* value) -> void* {
  return Own(RuntimeValue{Read<RuntimeAssociativeArray>(value)});
}

auto lyra_rt_cell_assocarray_get(void* cell) -> void* {
  return Own(static_cast<Var<RuntimeAssociativeArray>*>(cell)->Get());
}

void lyra_rt_cell_assocarray_initialize(void* cell, const void* prototype) {
  static_cast<Var<RuntimeAssociativeArray>*>(cell)->Initialize(
      Read<RuntimeAssociativeArray>(prototype));
}

void lyra_rt_cell_assocarray_set(void* cell, const void* value) {
  static_cast<Var<RuntimeAssociativeArray>*>(cell)->Set(
      lyra::runtime::current_runtime(), Read<RuntimeAssociativeArray>(value));
}

auto lyra_rt_activation_frame_alloc_assocarray() -> void* {
  return GeneratedCallScope::Current()
      .ActivationFrame()
      .New<ActivationValueCell<RuntimeAssociativeArray>>();
}

void lyra_rt_activation_frame_store_assocarray(void* cell, const void* value) {
  static_cast<ActivationValueCell<RuntimeAssociativeArray>*>(cell)->Store(
      Read<RuntimeAssociativeArray>(value));
}

auto lyra_rt_activation_frame_load_assocarray(const void* cell) -> void* {
  return Own(
      static_cast<const ActivationValueCell<RuntimeAssociativeArray>*>(cell)
          ->Get());
}
}
