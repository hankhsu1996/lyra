#include "lyra/runtime/jit_execution.hpp"

#include <coroutine>
#include <cstdint>
#include <memory>
#include <span>
#include <string>
#include <utility>
#include <vector>

#include "lyra/base/time.hpp"
#include "lyra/runtime/activation_value_cell.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/delay.hpp"
#include "lyra/runtime/file_table.hpp"
#include "lyra/runtime/generated_call_scope.hpp"
#include "lyra/runtime/hierarchy_segment.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/runtime_services.hpp"
#include "lyra/runtime/scope.hpp"
#include "lyra/runtime/scope_program.hpp"
#include "lyra/runtime/var.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/real.hpp"
#include "lyra/value/string.hpp"

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

}  // namespace

}  // namespace lyra::runtime

using lyra::runtime::ActivationValueCell;
using lyra::runtime::Coroutine;
using lyra::runtime::CoroutineHandle;
using lyra::runtime::FileTable;
using lyra::runtime::GeneratedCallScope;
using lyra::runtime::GeneratedInstance;
using lyra::runtime::HierarchySegment;
using lyra::runtime::Observable;
using lyra::runtime::Own;
using lyra::runtime::Read;
using lyra::runtime::RuntimeServices;
using lyra::runtime::Scope;
using lyra::runtime::SubscribeValueChange;
using lyra::runtime::Trigger;
using lyra::runtime::UnitDefinition;
using lyra::runtime::Var;
using lyra::value::Format;
using lyra::value::FormatSpec;
using lyra::value::PackedArray;
using lyra::value::PrintItem;
using lyra::value::PrintLiteralItem;
using lyra::value::PrintValueItem;
using lyra::value::Real;
using lyra::value::ShortReal;
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

auto lyra_rt_make_coroutine(void* (*ramp)(void*), void* env) -> void* {
  return GeneratedCallScope::Current().Arena().New<Coroutine<void>>(
      lyra::runtime::RunGeneratedProcess(ramp, env));
}

void lyra_rt_delay(
    void* services, const void* ticks, const void* precision_power) {
  auto& svc = *static_cast<RuntimeServices*>(services);
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
void lyra_rt_wait_any(void* services, LyraSpan triggers) {
  auto& svc = *static_cast<RuntimeServices*>(services);
  const std::span<Trigger* const> handles(
      static_cast<Trigger* const*>(triggers.data), triggers.count);
  std::vector<Trigger> collected;
  collected.reserve(triggers.count);
  for (const Trigger* handle : handles) {
    collected.push_back(*handle);
  }
  SubscribeValueChange(svc.CurrentProcess().TopHandle(), collected);
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

auto lyra_rt_member_addr(void* self, std::uint32_t index) -> void* {
  return static_cast<GeneratedInstance*>(self)->MemberAddress(index);
}

void lyra_rt_register_signal(void* self, const void* name, void* cell) {
  static_cast<Scope*>(self)->RegisterSignal(
      static_cast<const char*>(name), cell);
}

auto lyra_rt_cell_packed_get(void* cell) -> const void* {
  return &static_cast<Var<PackedArray>*>(cell)->Get();
}

void lyra_rt_cell_packed_initialize(void* cell, const void* prototype) {
  static_cast<Var<PackedArray>*>(cell)->Initialize(
      Read<PackedArray>(prototype));
}

void lyra_rt_cell_packed_set(void* cell, void* services, const void* value) {
  static_cast<Var<PackedArray>*>(cell)->Set(
      *static_cast<RuntimeServices*>(services), Read<PackedArray>(value));
}

auto lyra_rt_cell_string_get(void* cell) -> const void* {
  return &static_cast<Var<String>*>(cell)->Get();
}

void lyra_rt_cell_string_initialize(void* cell, const void* prototype) {
  static_cast<Var<String>*>(cell)->Initialize(Read<String>(prototype));
}

void lyra_rt_cell_string_set(void* cell, void* services, const void* value) {
  static_cast<Var<String>*>(cell)->Set(
      *static_cast<RuntimeServices*>(services), Read<String>(value));
}

auto lyra_rt_cell_real_get(void* cell) -> const void* {
  return &static_cast<Var<Real>*>(cell)->Get();
}

void lyra_rt_cell_real_initialize(void* cell, const void* prototype) {
  static_cast<Var<Real>*>(cell)->Initialize(Read<Real>(prototype));
}

void lyra_rt_cell_real_set(void* cell, void* services, const void* value) {
  static_cast<Var<Real>*>(cell)->Set(
      *static_cast<RuntimeServices*>(services), Read<Real>(value));
}

auto lyra_rt_cell_shortreal_get(void* cell) -> const void* {
  return &static_cast<Var<ShortReal>*>(cell)->Get();
}

void lyra_rt_cell_shortreal_initialize(void* cell, const void* prototype) {
  static_cast<Var<ShortReal>*>(cell)->Initialize(Read<ShortReal>(prototype));
}

void lyra_rt_cell_shortreal_set(void* cell, void* services, const void* value) {
  static_cast<Var<ShortReal>*>(cell)->Set(
      *static_cast<RuntimeServices*>(services), Read<ShortReal>(value));
}

// A procedural local whose value crosses a suspension. The cell is allocated in
// the activation frame (the driving coroutine owns it), so the handle the
// generated frame carries across a suspension points into activation-lifetime
// storage. A store overwrites the cell in place -- the first store installs the
// declared representation -- and a load copies the current value into the
// per-stretch scope, like any other value the boundary hands back. A procedural
// local is not observable, so no services thread through and no subscriber
// wakes.
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

auto lyra_rt_packed_convert_from(const void* src, const void* prototype)
    -> void* {
  return Own(
      PackedArray::ConvertFrom(
          Read<PackedArray>(src), Read<PackedArray>(prototype)));
}

auto lyra_rt_packed_from_int(std::int64_t value, const void* prototype)
    -> void* {
  return Own(PackedArray::FromInt(value, Read<PackedArray>(prototype)));
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

auto lyra_rt_packed_pow(const void* base, const void* exponent) -> void* {
  return Own(Read<PackedArray>(base).Pow(Read<PackedArray>(exponent)));
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

auto lyra_rt_string_add(const void* lhs, const void* rhs) -> void* {
  return Own(Read<String>(lhs) + Read<String>(rhs));
}

auto lyra_rt_string_eq(const void* lhs, const void* rhs) -> void* {
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

auto lyra_rt_real_const(double value) -> void* {
  return Own(Real{value});
}

auto lyra_rt_real_from_int64(std::int64_t value) -> void* {
  return Own(Real::FromInt64(value));
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

auto lyra_rt_shortreal_const(float value) -> void* {
  return Own(ShortReal{value});
}

auto lyra_rt_shortreal_from_int64(std::int64_t value) -> void* {
  return Own(ShortReal::FromInt64(value));
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
}
