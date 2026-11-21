#include "lyra/runtime/runtime_abi.hpp"

#include <algorithm>
#include <bit>
#include <coroutine>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <deque>
#include <exception>
#include <format>
#include <memory>
#include <optional>
#include <ranges>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/base/source_location.hpp"
#include "lyra/runtime/ambient_run_context.hpp"
#include "lyra/runtime/any.hpp"
#include "lyra/runtime/assertion_control.hpp"
#include "lyra/runtime/cancellation.hpp"
#include "lyra/runtime/class_object.hpp"
#include "lyra/runtime/closure.hpp"
#include "lyra/runtime/constraint_problem.hpp"
#include "lyra/runtime/construction_table.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/delay.hpp"
#include "lyra/runtime/diagnostic.hpp"
#include "lyra/runtime/distribution.hpp"
#include "lyra/runtime/dpi_context.hpp"
#include "lyra/runtime/erased_value_families.hpp"
#include "lyra/runtime/evaluation_attempts.hpp"
#include "lyra/runtime/file_table.hpp"
#include "lyra/runtime/finish.hpp"
#include "lyra/runtime/fork.hpp"
#include "lyra/runtime/generated_call_scope.hpp"
#include "lyra/runtime/hierarchy_segment.hpp"
#include "lyra/runtime/host_command.hpp"
#include "lyra/runtime/loop_blocks.hpp"
#include "lyra/runtime/nba_region.hpp"
#include "lyra/runtime/net.hpp"
#include "lyra/runtime/object_change.hpp"
#include "lyra/runtime/object_layout.hpp"
#include "lyra/runtime/object_ref.hpp"
#include "lyra/runtime/open_write.hpp"
#include "lyra/runtime/plusargs.hpp"
#include "lyra/runtime/process_control.hpp"
#include "lyra/runtime/property_wait.hpp"
#include "lyra/runtime/random.hpp"
#include "lyra/runtime/randomize.hpp"
#include "lyra/runtime/reaction.hpp"
#include "lyra/runtime/read_report.hpp"
#include "lyra/runtime/reflection.hpp"
#include "lyra/runtime/runtime.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/sampled_history.hpp"
#include "lyra/runtime/scope.hpp"
#include "lyra/runtime/shared_pointer.hpp"
#include "lyra/runtime/sim_time.hpp"
#include "lyra/runtime/simulation_entry.hpp"
#include "lyra/runtime/synchronization_object.hpp"
#include "lyra/runtime/timer.hpp"
#include "lyra/runtime/value_cell.hpp"
#include "lyra/runtime/value_change_wait.hpp"
#include "lyra/runtime/value_dump.hpp"
#include "lyra/runtime/value_dumps.hpp"
#include "lyra/runtime/value_families.hpp"
#include "lyra/runtime/value_handle.hpp"
#include "lyra/runtime/var.hpp"
#include "lyra/runtime/weak_reference.hpp"
#include "lyra/support/callable_bounds.hpp"
#include "lyra/support/library_decl.hpp"
#include "lyra/support/random_qualifier.hpp"
#include "lyra/value/any.hpp"
#include "lyra/value/any_value.hpp"
#include "lyra/value/array_report.hpp"
#include "lyra/value/assert.hpp"
#include "lyra/value/associative_array_type.hpp"
#include "lyra/value/bit_vec.hpp"
#include "lyra/value/container_core.hpp"
#include "lyra/value/dpi_canonical.hpp"
#include "lyra/value/dpi_open_array.hpp"
#include "lyra/value/dynamic_array_type.hpp"
#include "lyra/value/element_policy.hpp"
#include "lyra/value/element_type.hpp"
#include "lyra/value/enumeration.hpp"
#include "lyra/value/fixed_array_type.hpp"
#include "lyra/value/fixed_array_value.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/formation.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_value_type.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/laid_out_value.hpp"
#include "lyra/value/library_value_types.hpp"
#include "lyra/value/ptr_read.hpp"
#include "lyra/value/queue_type.hpp"
#include "lyra/value/real.hpp"
#include "lyra/value/scan.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/take_built.hpp"
#include "lyra/value/tuple_type.hpp"
#include "lyra/value/unmanaged_ref.hpp"
#include "lyra/value/unpacked_array.hpp"
#include "lyra/value/unpacked_range.hpp"
#include "lyra/value/value_type_kind.hpp"
#include "lyra/value/wide.hpp"
#include "lyra/value/wildcard_index.hpp"
#include "svdpi.h"

namespace lyra::runtime {

namespace {

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
// A generated body is free of it: it holds only its own coroutine, and this one
// owns the promise on its behalf. So the frame a code generator lays out never
// has to embed a runtime C++ type -- only a coroutine's resume, done, and
// destroy cross the boundary. That is what this buys, and why the engine
// resumes this rather than the generated body.
//
// This is the activation. It holds what the generated body needs and cannot
// hold itself -- that body's own coroutine and this execution's value store --
// so both are released together, on every path this leaves. The ramp laid the
// body's frame out and stopped before its first statement, so no generated
// code has run yet and nothing has reached for the store.
auto RunGeneratedProcess(GeneratedCoroutine generated) -> Coroutine<void> {
  ActivationValueStore values;

  // Every stretch of the body runs in a scope of its own naming this store, and
  // the parking between two of them holds none: a scope open across a park
  // would still be the innermost one while some other execution ran.
  //
  // A body completes whether it returned or departed, so the departure it
  // settled is carried on from here, where it leaves this activation exactly
  // as it would have left the body.
  std::exception_ptr departure;
  for (;;) {
    {
      GeneratedCallScope scope(values, &departure);
      generated.Resume();
    }
    if (generated.Done()) {
      if (departure != nullptr) {
        std::rethrow_exception(departure);
      }
      break;
    }
    co_await std::suspend_always{};
  }
  co_return;
}

// Builds the execution that drives a generated body, suspended before its first
// statement, in the storage the enabling body gave it. Whoever enables it hands
// it straight to the engine or to an awaiting frame, either of which takes it
// from there, so nothing is left in that storage to end.
auto StartGeneratedProcess(void* out, GeneratedCoroutine body) -> void* {
  return std::construct_at(
      static_cast<Coroutine<void>*>(out), RunGeneratedProcess(std::move(body)));
}

// The branches one `fork` spawned, taken out of the storage the spawning body
// built them in. The engine outlives that body's statement, so it takes rather
// than borrows, exactly as a region takes a submitted closure.
// A run of `count` values of `T` this call does not own, which crosses as
// where it lies and how many there are.
template <typename T>
auto SpanAt(void* first, std::uint64_t count) -> std::span<T> {
  return {static_cast<T*>(first), count};
}

auto TakeBranches(void* branches, std::uint64_t branches_count)
    -> std::vector<Coroutine<void>> {
  const std::span<Coroutine<void>* const> handles =
      SpanAt<Coroutine<void>* const>(branches, branches_count);
  std::vector<Coroutine<void>> taken;
  taken.reserve(handles.size());
  for (Coroutine<void>* handle : handles) {
    taken.push_back(std::move(*handle));
  }
  return taken;
}

// A reference, behind the address the ABI carries it as. A body holding one is
// lowered once for every caller and so cannot ask what it was handed (LRM
// 13.5.2); what holds the storage travels in the reference, and every access
// through it asks that.
auto ErasedAt(const void* reference) -> const ErasedReference& {
  return *static_cast<const ErasedReference*>(reference);
}

// Storage a declaration holds, built where its owner was laid out, told what
// it is built with; what it holds arrives afterwards through its own access.
template <typename T, typename... Told>
void BuildAt(void* storage, const Told&... told) {
  std::construct_at(static_cast<T*>(storage), told...);
}

template <value::LyraValue T>
auto LentAt(const void* reference) -> VariableRef<T> {
  return VariableRef<T>{ErasedAt(reference)};
}

auto BuildReference(void* out, const ErasedReference& reference) -> void* {
  return std::construct_at(static_cast<ErasedReference*>(out), reference);
}

// A variable of `T`, and a value cell of one, behind the addresses the
// ABI carries them as.
template <class T>
auto VariableAt(void* cell) -> typename VariableOf<T>::Type& {
  return *static_cast<typename VariableOf<T>::Type*>(cell);
}

template <class T>
auto ValueCellAt(void* cell) -> typename ValueCellFor<T>::Type& {
  return *static_cast<typename ValueCellFor<T>::Type*>(cell);
}

// A counted hold on a new variable of `T`, told the width or the type of a
// value it keeps after itself, or the type of a container it holds.
template <class T, class... Told>
auto MakeSharedCell(void* out, const Told&... told) -> void* {
  if constexpr (DynamicallySized<T>) {
    return std::construct_at(
        static_cast<SharedPointer*>(out),
        SharedPointer::MakeTailCell<typename StorageFor<T>::Type>(told...));
  } else {
    return std::construct_at(
        static_cast<SharedPointer*>(out), SharedPointer::MakeCell<T>(told...));
  }
}

// A reference to the whole of a subscribable variable, which a write through it
// is a write of.
template <value::LyraValue T>
auto ReferToCell(void* cell, void* out) -> void* {
  return BuildReference(out, WholeReference(VariableAt<T>(cell)));
}

// Reading and writing storage a caller lent, which answers through the variable
// it belongs to where it belongs to one.
template <value::LyraValue T>
auto RefGet(void* reference) -> const void* {
  return &LentAt<T>(reference).Get();
}

// Opening a write into what a wrapper stands for, in the storage the caller
// gave. A cell and a watchable referent report the write when it ends;
// storage nothing can watch takes it with nothing to report; a driver's net
// is told what the write did to that driver's value.
template <value::LyraValue T>
auto OpenCellWrite(void* cell, void* out) -> void* {
  return std::construct_at(
      static_cast<OpenWrite*>(out), VariableRef<T>{VariableAt<T>(cell)});
}

template <value::LyraValue T>
auto OpenRefWrite(void* reference, void* out) -> void* {
  return std::construct_at(static_cast<OpenWrite*>(out), LentAt<T>(reference));
}

template <value::LyraValue T>
void RefSet(void* reference, const void* value) {
  LentAt<T>(reference).Set(Read<T>(value));
}

template <value::LyraValue T>
void RefArmSampling(void* reference) {
  LentAt<T>(reference).ArmSampling();
}

template <value::LyraValue T>
auto RefSampledGet(void* reference, base::SourceLocation at) -> const void* {
  return HandleTo(LentAt<T>(reference).SampledGet(at));
}

// A procedural continuous assignment on a variable, or on part of it (LRM
// 10.6), reached through its cell or through storage a caller lent.
template <value::LyraValue T>
auto CellBeginTakeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count,
    base::SourceLocation at) -> std::int64_t {
  return VariableAt<T>(cell).BeginTakeover(level, first, count, at);
}

template <value::LyraValue T>
auto CellDriveTakeover(
    void* cell, std::int64_t level, std::int64_t generation, const void* value,
    base::SourceLocation at) -> bool {
  return VariableAt<T>(cell).DriveTakeover(
      level, generation, Read<T>(value), at);
}

template <value::LyraValue T>
void CellEndTakeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count,
    base::SourceLocation at) {
  VariableAt<T>(cell).EndTakeover(level, first, count, at);
}

template <value::LyraValue T>
auto RefBeginTakeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count,
    base::SourceLocation at) -> std::int64_t {
  return LentAt<T>(reference).BeginTakeover(level, first, count, at);
}

template <value::LyraValue T>
auto RefDriveTakeover(
    void* reference, std::int64_t level, std::int64_t generation,
    const void* value, base::SourceLocation at) -> bool {
  return LentAt<T>(reference).DriveTakeover(
      level, generation, Read<T>(value), at);
}

template <value::LyraValue T>
void RefEndTakeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count,
    base::SourceLocation at) {
  LentAt<T>(reference).EndTakeover(level, first, count, at);
}

// Only a reference to the whole of a variable has a variable of its own type
// to arm, which is what keeps a sampled value (LRM 16.5.1). `C` is the class
// of cell such a variable is.
template <class C>
void ArmWholeVariable(void* reference) {
  const ErasedReference& lent = ErasedAt(reference);
  if (lent.whole) {
    WholeVariable<C>(lent).ArmSampling();
  }
}

// Where the sampled value of a `T` a reference names lies (LRM 16.5.1), for a
// value lying after what holds it: the variable keeps it apart once armed, and
// storage nothing watches changes only where the reference is, so its sampled
// value is its current one.
template <class T>
auto TailRefSampledGet(void* reference, base::SourceLocation at) -> const
    void* {
  const base::SourceLocationScope bound(at);
  const ErasedReference& lent = ErasedAt(reference);
  return std::visit(
      Overloaded{
          [&](std::monostate) -> const void* { return lent.storage; },
          [&](VariableCell*) -> const void* {
            if (!lent.whole) {
              throw SimulationError(
                  "a sampled value of part of a variable lent by reference "
                  "is not yet supported");
            }
            return WholeVariable<typename VariableOf<T>::Type>(lent)
                .SampledBytes(at);
          },
          [&](GcObject*) -> const void* {
            throw SimulationError(
                "a sampled value of a class property lent by reference is not "
                "yet supported");
          },
          [&](HeldByNet) -> const void* { return lent.storage; }},
      lent.holder);
}

// The integral type an entry is told where it builds or reads values of one
// -- a word of a memory, an element of a byte array.
auto IntegralTypeAt(const void* type) -> const value::IntegralValueType& {
  return *static_cast<const value::IntegralValueType*>(type);
}

// The words of a memory task an entry is handed, and those words built in
// `out` of the request a call made (LRM 21.4, 21.5).
auto WordsAt(void* words) -> MemoryWords& {
  return *static_cast<MemoryWords*>(words);
}
auto WordsAt(const void* words) -> const MemoryWords& {
  return *static_cast<const MemoryWords*>(words);
}
auto MemoryRequest(
    void* runtime, WordValues values, const void* name, void* dims,
    std::uint64_t dims_count, std::int64_t base, std::int64_t start,
    std::optional<std::int64_t> finish, void* out) -> void* {
  return std::construct_at(
      static_cast<MemoryWords*>(out), *static_cast<RuntimeEffects*>(runtime),
      std::move(values), *static_cast<const value::String*>(name),
      SpanAt<const std::int64_t>(dims, dims_count), base, start, finish);
}

// The type of a fixed-size unpacked array an entry is told beside the array's
// bytes, which are its elements and state nothing of what they are.
auto ArrayTypeAt(const void* type) -> const value::FixedArrayType& {
  return *static_cast<const value::FixedArrayType*>(type);
}

// The type of a tuple an entry is told beside the tuple's bytes, which state
// nothing of what they are, or of a tuple it builds.
auto TupleTypeAt(const void* type) -> const value::TupleType& {
  return *static_cast<const value::TupleType*>(type);
}

// The type of a queue an entry is told beside the queue, which states its
// elements and its bound, and of a dynamic array, which states its elements.
auto QueueTypeAt(const void* type) -> const value::QueueType& {
  return *static_cast<const value::QueueType*>(type);
}
auto DynamicArrayTypeAt(const void* type) -> const value::DynamicArrayType& {
  return *static_cast<const value::DynamicArrayType*>(type);
}

auto AssociativeArrayTypeAt(const void* type)
    -> const value::AssociativeArrayType& {
  return *static_cast<const value::AssociativeArrayType*>(type);
}

// The core an associative array is, where it lies, to read or to write.
auto AssociativeCoreAt(const void* array)
    -> const value::AssociativeArrayCore& {
  return *static_cast<const value::AssociativeArrayCore*>(array);
}
auto AssociativeCoreAt(void* array) -> value::AssociativeArrayCore& {
  return *static_cast<value::AssociativeArrayCore*>(array);
}

// A type with how a value of it starts, as an entry is told one: by one
// building a dynamic array or an associative array, which is all such an
// array learns of its elements, by one asking what the type answers for an
// index naming no element, and by one building a holder or a value from
// nothing.
auto ElementTypeAt(const void* type) -> const value::ElementType& {
  return *static_cast<const value::ElementType*>(type);
}

// An array that crossed as its bytes with its type, read where it lies by an
// entry that keeps nothing of it past the call. The array is the caller's,
// borrowed for the call.
auto ArrayAt(const void* bytes, const void* type) -> value::FixedArrayView {
  return {.type = &ArrayTypeAt(type), .bytes = bytes};
}

// What an entry is told of an integral operand it reads as a number: how wide
// it is, whether it is signed, and whether an unknown plane follows the value
// plane.
auto NumberShape(std::int64_t width, bool is_signed, bool is_four_state)
    -> value::IntegralShape {
  return value::IntegralShape{
      .width = static_cast<std::uint64_t>(width),
      .signedness =
          is_signed ? value::Signedness::kSigned : value::Signedness::kUnsigned,
      .domain = is_four_state ? value::StateDomain::kFourState
                              : value::StateDomain::kTwoState};
}

// Such an operand read as words.
auto NumberAt(
    const void* value, std::int64_t width, bool is_signed, bool is_four_state)
    -> value::LoadedWords {
  return value::LoadedWords::Load(
      value, NumberShape(width, is_signed, is_four_state));
}

// Storage for one integral value of a shape, held while a function of the
// design makes it or reads it. It ends with the scope that holds it, on a
// departure out of that function as on its return.
class StorageForBits {
 public:
  explicit StorageForBits(const value::IntegralShape& shape)
      : words_(value::WordCountForBits(shape.Bytes() * 8U)) {
  }

  [[nodiscard]] auto Bytes() -> void* {
    return words_.data();
  }

 private:
  std::vector<std::uint64_t> words_;
};

// The shape of an operand the entry reads the bits of, whose signedness it is
// told nothing of and reads none of.
auto BitsShape(std::int64_t width, bool is_four_state) -> value::IntegralShape {
  return NumberShape(width, false, is_four_state);
}

auto BitsAt(const void* value, std::int64_t width, bool is_four_state)
    -> value::LoadedWords {
  return value::LoadedWords::Load(value, BitsShape(width, is_four_state));
}

// The machine number such an operand holds.
auto IntOf(const value::LoadedWords& number) -> std::int64_t {
  return value::ToInt64(number.View());
}

// The position a position value names, none where it holds x or z (LRM
// 7.4.5).
auto PositionNamedAt(const void* position) -> std::optional<std::int64_t> {
  return value::ReadPosition(Read<value::Position>(position));
}

// An integral value wider than a word is named by a reference as its bytes
// where they lie, for the reason a tuple is. Nothing in those bytes says how
// many of them the value is, so an access that has to know is told how wide
// the value is.
//
// A procedural continuous assignment through a reference to a wide value acts
// on what it names, as one through any reference does; a member is given
// storage that is named the same way (LRM 10.6.2).
template <value::HeldAsWords T>
auto WideRefBeginTakeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count,
    base::SourceLocation at) -> std::int64_t {
  using Words = typename VariableOf<T>::Type;
  return BeginTakeoverThrough<Words>(
      ErasedAt(reference), level, first, count, at, {},
      [](const Words& showing) {
        std::shared_ptr<Words> forced =
            MakeTailCell<typename StorageFor<T>::Type>(
                showing.HeldStorage().Width());
        forced->Initialize(showing.Bytes());
        return forced;
      },
      [](Words& forced) { return WholeReference(forced); });
}

template <value::HeldAsWords T>
auto WideRefDriveTakeover(
    void* reference, std::int64_t level, std::int64_t generation,
    const void* value, base::SourceLocation at) -> bool {
  using Words = typename VariableOf<T>::Type;
  return DriveTakeoverThrough<Words>(
      ErasedAt(reference), level, [&](Words& variable) {
        return variable.DriveTakeoverBytes(level, generation, value, at);
      });
}

// How a takeover of a value laid out by its type shows what it holds: the
// type's own function, handed over as an address.
auto TakePositionsAt(const void* take_positions)
    -> TakePositionsOf<value::LaidOutValue> {
  return TakePositionsOf<value::LaidOutValue>{
      std::bit_cast<value::TakePositionsWitness>(take_positions)};
}

// The same through a reference to a value laid out by its type, whose member
// is given a cell laid out by the type of the cell it showed.
auto LaidOutRefBeginTakeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count,
    base::SourceLocation at, const void* value_take_positions) -> std::int64_t {
  using LaidOut = VariableOf<value::LaidOutValue>::Type;
  return BeginTakeoverThrough<LaidOut>(
      ErasedAt(reference), level, first, count, at,
      TakePositionsAt(value_take_positions),
      [](const LaidOut& showing) {
        std::shared_ptr<LaidOut> forced =
            MakeTailCell<LaidOutStorage>(showing.HeldStorage().Held());
        forced->Initialize(showing.Bytes());
        return forced;
      },
      [](LaidOut& forced) { return WholeReference(forced); });
}

auto LaidOutRefDriveTakeover(
    void* reference, std::int64_t level, std::int64_t generation,
    const void* value, base::SourceLocation at) -> bool {
  using LaidOut = VariableOf<value::LaidOutValue>::Type;
  return DriveTakeoverThrough<LaidOut>(
      ErasedAt(reference), level, [&](LaidOut& variable) {
        return variable.DriveTakeoverBytes(level, generation, value, at);
      });
}

// The value of `T` a reference names, where it lies, `width` bits wide.
template <value::HeldAsWords T>
auto WideLentAt(const ErasedReference& lent, std::int64_t width)
    -> value::WideAt<T::kDomain> {
  return {.bytes = lent.storage, .width = static_cast<std::uint64_t>(width)};
}

// A write a variable's takeover overrides (LRM 10.6), handed to the variable
// as the whole value it is, where `lent` names the whole of it, and otherwise
// landed by `land` in the part `lent` names where it lies, which the variable
// then takes as any overridden write. `Owned` is the form the variable keeps
// its value in.
template <class Owned, class Land>
void ApplyOverriddenWriteThrough(
    const ErasedReference& lent, const void* value, Land land) {
  if (lent.whole) {
    WholeVariable<typename VariableOf<Owned>::Type>(lent)
        .ApplyOverriddenWriteBytes(value);
    return;
  }
  lent.WritePart(OwnedCall(land));
}

template <value::HeldAsWords T>
void WideRefSet(void* reference, std::int64_t width, const void* value) {
  const ErasedReference& lent = ErasedAt(reference);
  const value::WideAt<T::kDomain> held = WideLentAt<T>(lent, width);
  const auto assign = [&] { std::memmove(held.bytes, value, held.ByteSize()); };
  SetThrough(
      lent, [&] { ApplyOverriddenWriteThrough<T>(lent, value, assign); },
      assign,
      [&] { return std::memcmp(held.bytes, value, held.ByteSize()) == 0; },
      [&] {
        return WrittenPartThrough(lent).BeforeModify(
            PartPath{}, [&](EffectStorage& into) {
              return NewOriginal<WordsOriginal<value::WideAt<T::kDomain>>>(
                  into, held);
            });
      },
      [&](const auto& tell) {
        ReplaceWhole(held, lent.WatchedInside(), assign, tell);
      });
}

// A value lying after what holds it, lent by reference or by its holder as a
// write reaches it: an integral value wider than a word, of which the write
// reads bits by position, and a value laid out by its type -- a tuple, a
// union, a fixed-size unpacked array. A copy is a value of its own, which is
// what an overridden write lands in (LRM 10.6). Each is lent told
// what its holder is told, and lends the same of other bytes.
template <value::HeldAsWords T>
class LentWide {
 public:
  // The value a variable lending these bytes whole keeps.
  using Owned = T;

  LentWide(std::uint64_t width, void* bytes) : width_(width), bytes_(bytes) {
  }
  LentWide(const WordsStorage<T::kDomain>& held, void* bytes)
      : LentWide(held.Width(), bytes) {
  }
  LentWide(const LentWide& other)
      : width_(other.width_),
        kept_(other.width_, other.bytes_),
        bytes_(kept_.Bytes()) {
  }
  auto operator=(const LentWide&) -> LentWide& = delete;
  LentWide(LentWide&&) = delete;
  auto operator=(LentWide&&) -> LentWide& = delete;
  ~LentWide() = default;

  [[nodiscard]] auto At(void* bytes) const -> LentWide {
    return {width_, bytes};
  }
  [[nodiscard]] auto Bytes() const -> void* {
    return bytes_;
  }
  [[nodiscard]] auto Read() const -> value::ConstIntegralView {
    return value::WideViewAt(
        static_cast<const void*>(bytes_), width_, T::kDomain);
  }
  // This value written into `bytes`, which hold one of its width.
  void AssignTo(void* bytes) const {
    std::memmove(
        bytes, bytes_,
        value::WideAt<T::kDomain>{.bytes = bytes, .width = width_}.ByteSize());
  }

 private:
  std::uint64_t width_;
  // Holds a value only in a copy.
  T kept_;
  void* bytes_;
};

class LentLaidOut {
 public:
  // The value a variable lending these bytes whole keeps.
  using Owned = value::LaidOutValue;

  LentLaidOut(const value::ValueType& type, void* bytes)
      : type_(&type), bytes_(bytes) {
  }
  LentLaidOut(const LaidOutStorage& held, void* bytes)
      : LentLaidOut(held.Type(), bytes) {
  }
  LentLaidOut(const LentLaidOut& other)
      : type_(other.type_),
        kept_(value::AnyValue::CopyOf(*other.type_, other.bytes_)),
        bytes_(kept_.Bytes()) {
  }
  auto operator=(const LentLaidOut&) -> LentLaidOut& = delete;
  LentLaidOut(LentLaidOut&&) = delete;
  auto operator=(LentLaidOut&&) -> LentLaidOut& = delete;
  ~LentLaidOut() = default;

  [[nodiscard]] auto At(void* bytes) const -> LentLaidOut {
    return {*type_, bytes};
  }
  [[nodiscard]] auto Bytes() const -> void* {
    return bytes_;
  }
  // This value written into `bytes`, which hold one of its type.
  void AssignTo(void* bytes) const {
    type_->Assign(bytes, bytes_);
  }

 private:
  const value::ValueType* type_;
  // Holds a value only in a copy.
  value::AnyValue kept_;
  void* bytes_;
};

// How a value of `T` lying after its holder is lent.
template <class T>
struct LentOf;
template <value::HeldAsWords T>
struct LentOf<T> {
  using Type = LentWide<T>;
};
template <value::LaidOutByType T>
struct LentOf<T> {
  using Type = LentLaidOut;
};

// The sink a write through a reference to such a value writes into: the value
// where the reference lends it, and what holds that storage to tell.
template <class Lent>
class ReferenceWrite {
 public:
  using ValueType = Lent;

  template <class Told>
  ReferenceWrite(const ErasedReference& lent, const Told& told)
      : lent_(lent), beneath_(BeneathOf(lent)), place_(told, LandsIn()) {
  }
  // A copy is the same reference, so it lends the same place.
  ReferenceWrite(const ReferenceWrite& other)
      : lent_(other.lent_),
        beneath_(other.beneath_),
        place_(other.place_.At(LandsIn())) {
  }
  ReferenceWrite(ReferenceWrite&& other) noexcept
      : lent_(other.lent_),
        beneath_(other.beneath_),
        place_(other.place_.At(LandsIn())) {
  }
  auto operator=(const ReferenceWrite&) -> ReferenceWrite& = delete;
  auto operator=(ReferenceWrite&&) -> ReferenceWrite& = delete;
  ~ReferenceWrite() = default;

  [[nodiscard]] auto OverridesWrites() const -> bool {
    return lent_.OverridesWrites();
  }
  // An overridden write opened through a reference to the whole of a
  // variable lands where the variable keeps what a continuous driver gave it,
  // as one opened on the variable itself does, or in a copy of what the
  // reference names; the variable is handed it when the write is over.
  [[nodiscard]] auto OverriddenWriteStorage() -> Lent* {
    return beneath_ != nullptr ? &place_ : nullptr;
  }
  void ApplyOverriddenWrite(const Lent& left) const {
    ApplyOverriddenWriteThrough<typename Lent::Owned>(
        lent_, left.Bytes(), [&] { left.AssignTo(lent_.storage); });
  }
  [[nodiscard]] auto MutationStorage() -> Lent& {
    return place_;
  }
  [[nodiscard]] auto Watched() const -> bool {
    return lent_.Watched();
  }
  void AfterModify(
      const PartPath& reached, const Change& change, bool accumulated) const {
    lent_.AfterModify(reached, change, accumulated);
  }
  [[nodiscard]] auto WrittenPart() const -> lyra::runtime::WrittenPart {
    return WrittenPartThrough(lent_);
  }

 private:
  // What a continuous driver gave the variable `lent` names the whole of,
  // kept beneath the takeover overriding the write, where there is one.
  static auto BeneathOf(const ErasedReference& lent) -> void* {
    if (!lent.whole || !lent.OverridesWrites()) {
      return nullptr;
    }
    auto* kept =
        WholeVariable<typename VariableOf<typename Lent::Owned>::Type>(lent)
            .OverriddenWriteStorage();
    return kept != nullptr ? kept->Bytes() : nullptr;
  }

  // Where a write through the reference lands: what is kept beneath, or what
  // the reference names.
  [[nodiscard]] auto LandsIn() const -> void* {
    return beneath_ != nullptr ? beneath_ : lent_.storage;
  }

  ErasedReference lent_;
  void* beneath_;
  Lent place_;
};

static_assert(
    OverridableSink<ReferenceWrite<LentWide<value::WideLogicVector>>>);

// The sink a write opened on what holds such a value -- a variable, or a
// driver of a net -- writes into: its bytes, and, for a variable, what it keeps
// beneath a procedural continuous assignment, which is where an overridden
// write lands (LRM 10.6.2).
template <class Lent, class Holder>
class HolderWrite {
 public:
  using ValueType = Lent;

  // Only a variable's writes are overridden; a driver's write under an
  // override lands in its contribution, which its net resolves under the
  // override.
  static constexpr bool kOverridable =
      requires(Holder& held) { held.OverridesWrites(); };

  explicit HolderWrite(Holder& holder)
      : holder_(&holder), place_(holder.HeldStorage(), holder.Bytes()) {
    if constexpr (kOverridable) {
      if (auto* beneath = holder.OverriddenWriteStorage()) {
        beneath_.emplace(holder.HeldStorage(), beneath->Bytes());
      }
    }
  }
  // A copy is the same write's sink, so it names the same places.
  HolderWrite(const HolderWrite& other) : HolderWrite(*other.holder_) {
  }
  HolderWrite(HolderWrite&& other) noexcept : HolderWrite(*other.holder_) {
  }
  auto operator=(const HolderWrite&) -> HolderWrite& = delete;
  auto operator=(HolderWrite&&) -> HolderWrite& = delete;
  ~HolderWrite() = default;

  [[nodiscard]] auto OverridesWrites() const -> bool
    requires kOverridable
  {
    return holder_->OverridesWrites();
  }
  [[nodiscard]] auto OverriddenWriteStorage() -> Lent*
    requires kOverridable
  {
    return beneath_.has_value() ? &*beneath_ : nullptr;
  }
  void ApplyOverriddenWrite(const Lent& left) const
    requires kOverridable
  {
    holder_->ApplyOverriddenWriteBytes(left.Bytes());
  }
  [[nodiscard]] auto MutationStorage() -> Lent& {
    return place_;
  }
  [[nodiscard]] auto Watched() const -> bool {
    return holder_->Watched();
  }
  void AfterModify(
      const PartPath& reached, const Change& change, bool accumulated) const {
    holder_->AfterModify(reached, change, accumulated);
  }
  // A variable keeps its parts' originals; a driver writes what its net
  // resolves.
  [[nodiscard]] auto WrittenPart() const -> lyra::runtime::WrittenPart {
    if constexpr (kOverridable) {
      return {.place = holder_};
    } else {
      return {};
    }
  }

 private:
  Holder* holder_;
  Lent place_;
  std::optional<Lent> beneath_;
};

static_assert(OverridableSink<HolderWrite<
                  LentWide<value::WideLogicVector>,
                  VariableOf<value::WideLogicVector>::Type>>);

// Opening a write through a reference to a value of `T` lying after its
// holder, told what that holder is told; on a variable of one; and on a driver
// of a net of one.
template <DynamicallySized T, class Told>
auto OpenTailRefWrite(void* reference, const Told& told, void* out) -> void* {
  return std::construct_at(
      static_cast<OpenWrite*>(out),
      ReferenceWrite<typename LentOf<T>::Type>{ErasedAt(reference), told});
}

template <DynamicallySized T>
auto OpenTailCellWrite(void* cell, void* out) -> void* {
  return std::construct_at(
      static_cast<OpenWrite*>(out),
      HolderWrite<typename LentOf<T>::Type, typename VariableOf<T>::Type>{
          VariableAt<T>(cell)});
}

}  // namespace

void WriteLaidOutThrough(
    const ErasedReference& lent, const value::ValueType& type,
    const void* value) {
  const auto assign = [&] { type.Assign(lent.storage, value); };
  SetThrough(
      lent,
      [&] {
        ApplyOverriddenWriteThrough<value::LaidOutValue>(lent, value, assign);
      },
      assign, [&] { return type.BitIdentical(lent.storage, value); },
      [&] {
        return WrittenPartThrough(lent).BeforeModify(
            PartPath{}, [&](EffectStorage& into) {
              return NewOriginal<LaidOutOriginal>(into, type, lent.storage);
            });
      },
      [&](const auto& tell) {
        ReplaceLaidOut(type, lent.storage, lent.WatchedInside(), assign, tell);
      });
}

namespace {

// The same over a value laid out by its type -- a structure, a union, a tuple,
// a fixed-size unpacked array -- which a reference names as its bytes where it
// lies: no object of the runtime's stands for one there, since a component, a
// member or an element of one is bytes of what holds it.
// Nothing in those bytes says what they are, so an access that has to know is
// told the type.
void LaidOutRefSet(
    void* reference, const value::ValueType& type, const void* value) {
  WriteLaidOutThrough(ErasedAt(reference), type, value);
}

// A net and one of its drivers, behind the addresses the ABI carries them as.
// The fold a net resolves under travels in the net object itself, so one
// recovery serves every net type: the address names a net, not a net of a
// particular fold.
template <typename T>
auto NetOf(void* net) -> ResolvedNet<T>& {
  return *static_cast<ResolvedNet<T>*>(net);
}

template <typename T>
auto UnresolvedNetOf(void* net) -> UnresolvedNet<T>& {
  return *static_cast<UnresolvedNet<T>*>(net);
}

// A net as what its driver carries is stated to, which every net is whichever
// way it arrives at its value.
auto NetAt(void* net) -> lyra::runtime::Net& {
  return *static_cast<lyra::runtime::Net*>(net);
}

// A driver giving positions is the slot the net keeps for it, which a handle
// is made over; the one driver of a net with no resolution function is its
// handle.
template <typename T>
auto DriverOf(void* driver) -> decltype(auto) {
  if constexpr (BitAddressed<T>) {
    return Driver<T>(*static_cast<const DriverSlot*>(driver));
  } else {
    return *static_cast<Driver<T>*>(driver);
  }
}

template <NetValue T>
auto OpenDriverWrite(void* driver, void* out) -> void* {
  return std::construct_at(static_cast<OpenWrite*>(out), DriverOf<T>(driver));
}

template <DynamicallySized T>
auto OpenTailDriverWrite(void* driver, void* out) -> void* {
  return std::construct_at(
      static_cast<OpenWrite*>(out),
      HolderWrite<typename LentOf<T>::Type, Driver<T>>{DriverOf<T>(driver)});
}

// A net told the object its driver of the whole of it carries, and asked at
// that driver's turn whether it shows that object.
template <NetValue T>
void DriveFromVariable(void* net, void* cell) {
  NetAt(net).DriveFrom(&VariableAt<T>(cell));
}

// The source is a net of the same type as the one it drives, so it arrives at
// its value the same way.
template <class SourceNet>
void DriveFromNet(void* net, void* source) {
  NetAt(net).DriveFrom(static_cast<SourceNet*>(source));
}

void DriveFromReference(void* net, void* reference) {
  NetAt(net).DriveFrom(static_cast<ErasedReference*>(reference));
}

template <class T>
auto CellReportsTo(void* cell) -> void* {
  return VariableAt<T>(cell).ReportsTo().Word();
}

// What any reference reports to is a fact of the reference, whatever it
// names, so every representation's entry answers alike.
auto RefReportsTo(const void* reference) -> void* {
  return WatchedPlace::Through(ErasedAt(reference)).Word();
}

template <class N>
auto NetReportsTo(void* net) -> void* {
  return static_cast<N*>(net)->ReportsTo().Word();
}

auto NetShowsSource(void* net) -> bool {
  return NetAt(net).ShowSource();
}

// The type an erased value crosses beside, which is what states its
// representation where nothing on this side could have read it off anything
// else.
auto TypeAt(const void* type) -> const value::ValueType& {
  return *static_cast<const value::ValueType*>(type);
}

// The operations of a type, handed over as an address.
auto OperationsAt(const void* operations) -> const value::ValueOperations& {
  return *static_cast<const value::ValueOperations*>(operations);
}

// A copy of a value that crossed with its type, held with that type. The value
// is the caller's, borrowed for the call.
auto OwnedCopy(const void* value, const void* type) -> value::AnyValue {
  return value::AnyValue::CopyOf(TypeAt(type), value);
}

// The same of a value laid out by its type -- a record or a fixed-size
// unpacked array -- that crossed as its bytes with its type, held as the
// runtime holds one, for what goes on holding it once the call is over.
auto OwnedLaidOutCopy(const void* bytes, const void* type)
    -> value::LaidOutValue {
  return value::LaidOutValue::CopyOf(TypeAt(type), bytes);
}

// Storage for one value the generated program builds and then holds by address
// for the rest of the run. Every other value crossing the boundary lives in
// storage the generated body gave it and ends with the evaluation that made
// it; these cannot, because generated code keeps their addresses across calls.
// The store only grows, and it grows to what the design declares.
template <class T>
auto ProgramLifetime(T value) -> T* {
  static std::deque<T> stored;
  stored.push_back(std::move(value));
  return &stored.back();
}

// A place designated within a write in progress, behind the address the ABI
// carries it as.
auto DesignationAt(const void* designation) -> const ErasedDesignation& {
  return *static_cast<const ErasedDesignation*>(designation);
}

// The designation of the element `element` a write stepped to within the part
// it designates, which forming it did `formed` to, by the step `step` reports.
auto ElementDesignation(
    const ErasedDesignation& within, void* element, value::Formation formed,
    const PartPath& step, void* out) -> void* {
  within.write->Formed(StepFormedAt(formed, step));
  if (within.write->Tells()) {
    within.write->Stepped(step);
  }
  return std::construct_at(
      static_cast<ErasedDesignation*>(out),
      ErasedDesignation{.write = within.write, .part = element});
}

template <typename Part>
auto LandDesignation(const void* designation) noexcept -> void* {
  const ErasedDesignation& landed = DesignationAt(designation);
  landed.write->Land(*static_cast<Part*>(landed.part));
  return landed.part;
}

// An integral value wider than a word designated where it lies is its bytes,
// which no object stands for, and they do not say how many of them it is, so
// the landing is told how wide the value is.
template <value::HeldAsWords T>
auto LandWideDesignation(const void* designation, std::int64_t width) noexcept
    -> void* {
  const ErasedDesignation& landed = DesignationAt(designation);
  landed.write->LandWide(
      value::WideAt<T::kDomain>{
          .bytes = landed.part, .width = static_cast<std::uint64_t>(width)});
  return landed.part;
}

// So is a value laid out by its type -- a tuple, a union, a fixed-size
// unpacked array -- whose bytes do not say what it is, so the landing and each
// step into one are told its type.
auto LandLaidOutDesignation(
    const void* designation, const value::ValueType& type) noexcept -> void* {
  const ErasedDesignation& landed = DesignationAt(designation);
  landed.write->LandLaidOut(type, landed.part);
  return landed.part;
}

// The step a write or a reference takes to the element the position at
// `position` points at.
auto ElementStepAt(const void* position) -> PartPath {
  return PartPath::Written(
      support::PartStep::kElement, PositionNamedAt(position));
}

// What a write through a position finds, as the formation a write in progress
// is told and where it lands: storage no read reaches where it found none.
auto FoundAs(
    const value::FoundElement& found, const value::ElementType& element,
    value::Formation& formed) -> void* {
  formed = value::FormationOf(found.element != nullptr, found.made);
  return found.element == nullptr ? element.DiscardedWrite() : found.element;
}

// Whether a step into the queue `queue` at `at` appends an element, one past
// its last (LRM 7.10.1), which moves the elements it holds, so the queue is
// kept whole, as its type copies it, before the step. No other step into a
// queue, and none into a dynamic array, reshapes it: a write past the end
// anywhere else is discarded (LRM 7.5, 7.10.1).
auto QueueStepAppends(const void* queue, std::optional<std::int64_t> at)
    -> bool {
  return at.has_value() &&
         *at == static_cast<std::int64_t>(value::QueueType::Count(queue));
}

// LRM 7.4.5, 7.10.1: the element the position `position` points at names in a
// dynamic array or a queue, as storage a write lands in, where no read reaches
// where it names none: where its type's own body finds it, a queue appending
// an element where the position is one past its last. A fixed-size array's
// lies at its position, which its type answers itself.
template <typename ArrayType>
auto FoundElementMut(
    const ArrayType& type, void* array, const void* position,
    value::Formation& formed) -> void* {
  return FoundAs(
      type.ElementPlace(array, position), type.ElementType(), formed);
}

auto DesignateFixedArrayElement(
    const void* designation, const value::FixedArrayType& type,
    const void* position, void* out) -> void* {
  const ErasedDesignation& within = DesignationAt(designation);
  value::Formation formed{};
  void* element =
      type.ElementMut(within.part, PositionNamedAt(position), formed);
  return ElementDesignation(
      within, element, formed, ElementStepAt(position), out);
}
// A dynamic array or a queue moves and ends its elements as it grows and
// shrinks (LRM 7.5, 7.10), so it is kept whole, as its type copies it, before
// a write steps into it.
template <typename ArrayType>
auto DesignateFoundElement(
    const void* designation, const ArrayType& type, const void* position,
    void* out) -> void* {
  const ErasedDesignation& within = DesignationAt(designation);
  within.write->BeforeModifyLaidOut(type.Type(), within.part);
  value::Formation formed{};
  void* element = FoundElementMut(type, within.part, position, formed);
  return ElementDesignation(
      within, element, formed, ElementStepAt(position), out);
}
// The same into an associative array, the entry an index names having no
// position, so the path the write reports stops there (LRM 7.8).
auto DesignateAssociativeArrayElement(
    const void* designation, const value::AssociativeArrayType& type,
    const void* index, void* out) -> void* {
  const ErasedDesignation& within = DesignationAt(designation);
  within.write->BeforeModifyLaidOut(type.Type(), within.part);
  value::Formation formed{};
  void* element = FoundElementMut(type, within.part, index, formed);
  return ElementDesignation(
      within, element, formed,
      PartPath::Written(support::PartStep::kAssocElement, std::nullopt), out);
}

// A reference to storage nothing is told about -- a variable of a function, a
// closure's own copy of a variable -- which a write through it only writes.
auto ReferToStorage(void* storage, void* out) -> void* {
  return BuildReference(out, ErasedReference{.holder = {}, .storage = storage});
}

// A reference to a property of an object, held by the object so a write
// through it tells the object.
auto ReferToProperty(void* object, void* storage, void* out) -> void* {
  return BuildReference(
      out, ErasedReference{
               .holder = static_cast<GcObject*>(object), .storage = storage});
}

// An element a step into a container reaches, what forming it did, and the
// reference to it, which belongs to whatever the reference the step was taken
// on belongs to.
struct SteppedElement {
  void* element = nullptr;
  StepFormed formed;
  ErasedReference part;
};

// The step a reference takes into an element of the associative array it
// names, where the array type's own body finds or makes it (LRM 7.8.7). A step
// that makes an entry changes the array whole, so the array is kept, as its
// type copies it, before the step; the reference goes on naming the element
// however the array changes around it (LRM 13.5.2), so its path stops at the
// array, inexact.
auto StepIntoAssociativeArrayElement(
    const ErasedReference& from, const value::AssociativeArrayType& type,
    const void* index, base::SourceLocation at) -> SteppedElement {
  const base::SourceLocationScope bound(at);
  from.AdmitStep();
  const bool accumulated = WrittenPartThrough(from).BeforeModify(
      PartPath{}, [&](EffectStorage& into) {
        return NewOriginal<LaidOutOriginal>(into, type.Type(), from.storage);
      });
  value::Formation formation{};
  void* element = FoundElementMut(type, from.storage, index, formation);
  const StepFormed formed = StepFormedAt(formation, PartPath::Inexact());
  return {
      .element = element,
      .formed = formed,
      .part = from.Part(element, formed, PartPath::Inexact(), accumulated)};
}

auto ReferAssociativeArrayElement(
    const void* reference, const value::AssociativeArrayType& type,
    const void* index, base::SourceLocation at, void* out) -> void* {
  return BuildReference(
      out, StepIntoAssociativeArrayElement(ErasedAt(reference), type, index, at)
               .part);
}

// The same, bound by a call, with a lease on the entry the array keeps, which
// the array cannot then end before the call returns (LRM 13.5.2). An index
// naming no element names nothing to lease.
auto ReferAssociativeArrayElementLeased(
    const void* reference, const value::AssociativeArrayType& type,
    const void* index, base::SourceLocation at, void* out) -> void* {
  const ErasedReference& from = ErasedAt(reference);
  const SteppedElement stepped =
      StepIntoAssociativeArrayElement(from, type, index, at);
  auto* leased = static_cast<LeasedReference*>(out);
  if (std::holds_alternative<ElementNowhere>(stepped.formed)) {
    return std::construct_at(leased, StorageLease{}, stepped.part);
  }
  return std::construct_at(
      leased,
      StorageLease(
          value::AssociativeArrayLease(
              *static_cast<const value::AssociativeArrayCore*>(from.storage),
              stepped.element)),
      stepped.part);
}

// The same step into an array laid out by its type, each kind told its own
// type. The step a reference into an element keeps (LRM 13.5.2) is to its
// position, but an array whose size changes -- a dynamic array, a queue --
// goes on holding the element while it grows, shrinks or is replaced around
// it (LRM 7.10.3), so the path stops at the array, inexact.
auto ReferFixedArrayElement(
    const void* reference, const value::FixedArrayType& type,
    const void* position, base::SourceLocation at, void* out) -> void* {
  const base::SourceLocationScope bound(at);
  const ErasedReference& from = ErasedAt(reference);
  from.AdmitStep();
  const PartPath step = ElementStepAt(position);
  value::Formation formed{};
  void* element =
      type.ElementMut(from.storage, PositionNamedAt(position), formed);
  const bool accumulated = false;
  return BuildReference(
      out, from.Part(element, StepFormedAt(formed, step), step, accumulated));
}
auto ReferDynamicArrayElement(
    const void* reference, const value::DynamicArrayType& type,
    const void* position, base::SourceLocation at, void* out) -> void* {
  const base::SourceLocationScope bound(at);
  const ErasedReference& from = ErasedAt(reference);
  from.AdmitStep();
  value::Formation formed{};
  void* element = FoundElementMut(type, from.storage, position, formed);
  const bool accumulated = false;
  return BuildReference(
      out, from.Part(
               element, StepFormedAt(formed, ElementStepAt(position)),
               PartPath::Inexact(), accumulated));
}
// The queue a reference names kept whole, as its type copies it, before a
// step into it at `position` that appends an element, and whether the open
// interval accumulated that.
auto KeepQueueBeforeAppend(
    const ErasedReference& from, const value::QueueType& type,
    const void* position) -> bool {
  return QueueStepAppends(from.storage, PositionNamedAt(position)) &&
         WrittenPartThrough(from).BeforeModify(
             PartPath{}, [&](EffectStorage& into) {
               return NewOriginal<LaidOutOriginal>(
                   into, type.Type(), from.storage);
             });
}

auto ReferQueueElement(
    const void* reference, const value::QueueType& type, const void* position,
    base::SourceLocation at, void* out) -> void* {
  const base::SourceLocationScope bound(at);
  const ErasedReference& from = ErasedAt(reference);
  from.AdmitStep();
  const bool accumulated = KeepQueueBeforeAppend(from, type, position);
  value::Formation formed{};
  void* element = FoundElementMut(type, from.storage, position, formed);
  return BuildReference(
      out, from.Part(
               element, StepFormedAt(formed, ElementStepAt(position)),
               PartPath::Inexact(), accumulated));
}

// The same, leased: a call's binding to an element of a dynamic array or a
// queue, which the array cannot then end before the call returns (LRM 13.5.2):
// a dynamic array's generation keeps the count of its leases, and a queue's
// element keeps it in its own storage. A position naming no element names
// nothing to lease.
auto ReferDynamicArrayElementLeased(
    const void* reference, const value::DynamicArrayType& type,
    const void* position, base::SourceLocation at, void* out) -> void* {
  const base::SourceLocationScope bound(at);
  const ErasedReference& from = ErasedAt(reference);
  from.AdmitStep();
  value::Formation formed{};
  void* element = FoundElementMut(type, from.storage, position, formed);
  const bool accumulated = false;
  const ErasedReference part = from.Part(
      element, StepFormedAt(formed, ElementStepAt(position)),
      PartPath::Inexact(), accumulated);
  auto* leased = static_cast<LeasedReference*>(out);
  if (formed == value::Formation::kNowhere) {
    return std::construct_at(leased, StorageLease{}, part);
  }
  return std::construct_at(
      leased,
      StorageLease(
          value::DynamicArrayLease(
              *static_cast<const value::DynamicArrayCore*>(from.storage),
              element)),
      part);
}
auto ReferQueueElementLeased(
    const void* reference, const value::QueueType& type, const void* position,
    base::SourceLocation at, void* out) -> void* {
  const base::SourceLocationScope bound(at);
  const ErasedReference& from = ErasedAt(reference);
  from.AdmitStep();
  const bool accumulated = KeepQueueBeforeAppend(from, type, position);
  value::Formation formed{};
  void* element = FoundElementMut(type, from.storage, position, formed);
  const ErasedReference part = from.Part(
      element, StepFormedAt(formed, ElementStepAt(position)),
      PartPath::Inexact(), accumulated);
  auto* leased = static_cast<LeasedReference*>(out);
  if (formed == value::Formation::kNowhere) {
    return std::construct_at(leased, StorageLease{}, part);
  }
  return std::construct_at(
      leased,
      StorageLease(value::QueueLease(type.ElementType().Type(), element)),
      part);
}

// What a body hands over when something longer-lived takes a closure: the box
// it made the closure in, called at the signature the entry taking it states.
// The closure itself stays where it was made.
template <typename Signature>
auto AdoptClosure(void* closure) -> OwnedClosure<Signature> {
  return OwnedClosure<Signature>(static_cast<ClosureValue*>(closure));
}

// One domain's own whole-value operations, which a structure's functions apply
// to a member of that domain (LRM 7.2).
template <typename T>
auto BitIdentical(const void* lhs, const void* rhs) -> bool {
  return Read<T>(lhs).IsBitIdentical(Read<T>(rhs));
}

template <typename T>
auto HasUnknown(const void* value) -> bool {
  return Read<T>(value).HasUnknown();
}

// A format operand borrowing an integral value, which reads under a conversion
// through its planes (LRM 21.2.1) as the number its shape makes of them.
auto IntegralFormatArg(const void* value, value::IntegralShape shape)
    -> value::FormatArg {
  value::FormatArg arg;
  arg.ptr = value;
  arg.read_as = shape;
  arg.format_fn = [](const value::FormatSpec& spec,
                     const value::FormatArg& operand,
                     const value::FormatContext& ctx) -> std::string {
    const value::LoadedWords words =
        value::LoadedWords::Load(operand.ptr, operand.read_as);
    return value::FormatIntegralOperand(
        spec, words.View(), &ctx.time_format.get());
  };
  return arg;
}

// A value of one of the library's own kinds, held with its type.
template <typename T>
auto Held(T value) -> value::AnyValue {
  return value::AnyValue::Built(value::LibraryTypeOf<T>(), [&](void* out) {
    std::construct_at(static_cast<T*>(out), std::move(value));
  });
}

// A call that answers with more than one value completes with the product of
// them, of the type `type` the call states, laid out in the storage the caller
// gave. Stated once here, so each entry below says only what its own
// components are.
auto EmplaceCompletion(
    void* out, const value::TupleType& type,
    std::vector<value::AnyValue> components) -> void* {
  const std::span<const value::TupleComponent> stated = type.Components();
  if (components.size() != stated.size()) {
    throw InternalError(
        "a completion is built from a component count its type does not "
        "have -- please report this as a bug");
  }
  for (std::size_t i = 0; i < components.size(); ++i) {
    value::AnyValue& component = components[i];
    if (!value::SameType(*stated[i].type, component.Type())) {
      throw InternalError(
          "a completion's component is a value of a type its tuple type does "
          "not state -- please report this as a bug");
    }
    std::move(component).RelocateInto(type.ComponentAt(out, i));
  }
  return out;
}

// Where a scan's completion puts its first parsed value: the matched-conversion
// count leads it.
constexpr std::size_t kScanFirstValue = 1;

// The matched-conversion count and one value per conversion (LRM 21.3.4.3).
// Each value starts as the prototype the call supplied and is parsed in place,
// so a conversion that never ran carries its prototype back and the caller's
// own destination stays as it was. A scan destination is an integral or a
// string and lowering rejects anything else, so a value of any other type
// reaching here is a compiler bug. An integral one is parsed in its planes,
// which are laid back out in the value once the scan is over.
auto EmplaceScan(
    void* out, const value::TupleType& type, value::ScanInput& input,
    const value::String& format, const void* prototypes,
    const value::TupleType& prototypes_type, base::SourceLocation at) -> void* {
  const base::SourceLocationScope bound(at);
  const std::span<const value::TupleComponent> stated =
      prototypes_type.Components();
  std::vector<value::AnyValue> components;
  components.reserve(stated.size() + 1);
  components.emplace_back();
  for (std::size_t i = 0; i < stated.size(); ++i) {
    components.push_back(
        value::AnyValue::CopyOf(
            *stated[i].type, prototypes_type.ComponentAt(prototypes, i)));
  }
  std::deque<value::WrittenWords> parsed;
  std::vector<value::ScanTarget> targets;
  targets.reserve(stated.size());
  for (std::size_t i = kScanFirstValue; i < components.size(); ++i) {
    value::AnyValue& component = components[i];
    if (const value::IntegralValueType* integral =
            component.Type().AsIntegral()) {
      parsed.push_back(
          value::WrittenWords::CopyOf(component.Bytes(), integral->Shape()));
      targets.emplace_back(parsed.back().MutableView());
    } else if (&component.Type() == &value::lyra_rt_string_value_type) {
      targets.emplace_back(static_cast<value::String*>(component.Bytes()));
    } else {
      throw InternalError(
          "a scan parses into an integral or a string (LRM 21.3.4.3)");
    }
  }

  const std::int64_t items = value::detail::ScanImpl(input, format, targets);
  auto words = parsed.cbegin();
  for (std::size_t i = kScanFirstValue; i < components.size(); ++i) {
    if (components[i].Type().AsIntegral() != nullptr) {
      words->StoreTo(components[i].Bytes());
      ++words;
    }
  }
  components[0] = Held(value::Integer::FromInt(items));
  return EmplaceCompletion(out, type, std::move(components));
}

// A completion of a value the library types and an integral value of a type
// the caller stated, whose planes are laid out where the product's type puts
// its second component.
template <typename First>
auto EmplaceWithWords(
    void* out, const value::TupleType& type, const First& first,
    const value::WrittenWords& second) -> void* {
  constexpr std::size_t kComponents = 2;
  const std::span<const value::TupleComponent> stated = type.Components();
  value::AnyValue leading = Held(first);
  if (stated.size() != kComponents ||
      !value::SameType(*stated[0].type, leading.Type())) {
    throw InternalError(
        "a completion is built from components its type does not state -- "
        "please report this as a bug");
  }
  std::move(leading).RelocateInto(type.ComponentAt(out, 0));
  second.StoreTo(type.ComponentAt(out, 1));
  return out;
}

// A completion the runtime already assembled as a pair, laid out component by
// component. Which two values they are is the entry's own business -- the
// value drawn and the seed it advanced (LRM 20.14.2), a byte count and the text
// or memory those bytes filled (LRM 21.3.4.2, 21.3.4.4, 21.3.7).
template <typename First, typename Second>
auto EmplaceBoth(
    void* out, const value::TupleType& type,
    const value::Tuple<First, Second>& completion) -> void* {
  std::vector<value::AnyValue> components;
  components.push_back(Held(completion.template Component<0>()));
  components.push_back(Held(completion.template Component<1>()));
  return EmplaceCompletion(out, type, std::move(components));
}

// The member an enumeration step lands on (LRM 6.19.5.3, 6.19.5.4), laid out
// in `out` at the type of the value stepped from, or that type's default where
// the value is no member.
auto EmplaceEnumerationMember(
    void* out, value::IntegralShape held,
    std::optional<value::ConstIntegralView> member) -> void* {
  value::WrittenWords landed(held);
  const value::IntegralView written = landed.MutableView();
  if (member.has_value()) {
    std::ranges::copy(member->value, written.value.begin());
    std::ranges::copy(member->unknown, written.unknown.begin());
  } else {
    value::FillDefault(written);
  }
  landed.StoreTo(out);
  return out;
}

// The layouts an integral value no wider than a word has, each named by the
// widest type laid out that way. A holder over one of these holds a value of
// every integral type of that layout, as the bytes its type lays it out in.
using Bit8 = value::BitVector<8>;
using Bit16 = value::BitVector<16>;
using Bit32 = value::BitVector<32>;
using Bit64 = value::BitVector<64>;
using Logic8 = value::LogicVector<8>;
using Logic16 = value::LogicVector<16>;
using Logic32 = value::LogicVector<32>;
using Logic64 = value::LogicVector<64>;

// What holds an integral value wider than a word, two-state and four-state:
// its words, as many as the width its install was told asks for.
using BitWide = value::WideBitVector;
using LogicWide = value::WideLogicVector;

// A history of one layout, behind the address the ABI carries it as.
template <BitAddressed T>
auto HistoryAt(void* history) -> SampledHistory<T>& {
  return *static_cast<SampledHistory<T>*>(history);
}

// A cell the running activation owns, for what an awaited call answers, told
// the width or the type of a value it keeps after itself, or the type of a
// container it holds.
template <class T, class... Told>
auto AllocateValueCell(const Told&... told) noexcept -> void* {
  using Cell = typename ValueCellFor<T>::Type;
  ActivationValueStore& values =
      GeneratedCallScope::Current().ActivationValues();
  if constexpr (DynamicallySized<T>) {
    return values.NewWithTail<Cell>(
        StorageFor<T>::Type::ValueLayoutFor(told...), told...);
  } else {
    return values.New<Cell>(told...);
  }
}

// A value `width` bits wide of the bytes at `value`, which is what installs a
// holder of one.
template <value::HeldAsWords T>
auto WideOf(std::int64_t width, const void* value) -> T {
  return T(static_cast<std::uint64_t>(width), value);
}

// Where bits a write is handed land in a value: the position their start names
// in the value's own numbering, and the positions of the value they reach.
struct ResolvedBitRange {
  std::int64_t start;
  value::BitPositions clamped;
};

// Where `count` bits starting at `start` within the innermost of `outer_parts`
// land in a value `width` bits wide (LRM 11.5.1). Each part starts where it
// says within the part before it, the value itself for the first, so the
// starts add up and each part reaches no further than the one it is taken in.
// None where a start names no position, and none where the bits are left no
// position of the value.
auto ResolveBitRange(
    std::int64_t width, void* outer_parts, std::uint64_t outer_parts_count,
    const void* start, std::int64_t count) -> std::optional<ResolvedBitRange> {
  const auto whole = static_cast<std::uint64_t>(width);
  std::int64_t at = 0;
  std::optional<value::BitPositions> window =
      value::BitPositions{.lsb = 0, .width = whole};
  for (const LyraBitsPart& part :
       SpanAt<const LyraBitsPart>(outer_parts, outer_parts_count)) {
    const std::optional<std::int64_t> named = PositionNamedAt(part.start);
    if (!named.has_value()) {
      return std::nullopt;
    }
    at += *named;
    window = value::ClampedBitRange(
        whole, static_cast<std::uint64_t>(part.width), at, *window);
    if (!window.has_value()) {
      return std::nullopt;
    }
  }
  const std::optional<std::int64_t> named = PositionNamedAt(start);
  if (!named.has_value()) {
    return std::nullopt;
  }
  const std::optional<value::BitPositions> reached = value::ClampedBitRange(
      whole, static_cast<std::uint64_t>(count), at + *named, *window);
  if (!reached.has_value()) {
    return std::nullopt;
  }
  return ResolvedBitRange{.start = at + *named, .clamped = *reached};
}

// Writes the `count` bits laid out at `bits` into `whole` where `range`
// says. The bits are read where they lie, whatever integer they are laid out
// in.
template <
    value::PlaneWord Word, std::size_t kValueWords, std::size_t kUnknownWords>
void InsertBitsInto(
    value::IntegralViewOf<Word, kValueWords, kUnknownWords> whole,
    const void* bits, std::int64_t count, bool bits_are_four_state,
    const ResolvedBitRange& range) {
  value::VisitLaidOut(
      bits, BitsShape(count, bits_are_four_state),
      [&](value::AnyConstIntegralView auto read) {
        value::InsertWithin(whole, read, range.start, range.clamped);
      });
}

// Bits of a designated value of one layout written where they lie (LRM
// 11.5.1), in a value whose declared type has `width` positions. A value of
// one layout is no more than a word of each plane, so what the write keeps of
// it from before is the value: a write that leaves it as it was has nothing
// to tell, and only one that moved it asks whether anything reads which bits
// moved.
template <value::IntegralValue T>
void InsertDesignatedBits(
    const void* designation, std::int64_t width, void* outer_parts,
    std::uint64_t outer_parts_count, const void* start, const void* bits,
    std::int64_t count, bool bits_are_four_state) {
  const std::optional<ResolvedBitRange> range =
      ResolveBitRange(width, outer_parts, outer_parts_count, start, count);
  if (!range.has_value()) {
    return;
  }
  const ErasedDesignation& within = DesignationAt(designation);
  T& whole = *static_cast<T*>(within.part);
  within.write->BeforeModifyPart(whole);
  const T before = whole;
  InsertBitsInto(
      whole.MutableView().Truncated(static_cast<std::uint64_t>(width)), bits,
      count, bits_are_four_state, *range);
  if (whole.IsBitIdentical(before) || !within.write->Tells()) {
    return;
  }
  WriteOriginal<T> kept(before, range->clamped);
  if (const std::optional<Change> change = kept.ChangeTo(whole)) {
    within.write->Landed(*change);
  }
}

// The same over a designated value wider than a word, which is `width` bits of
// the bytes the designation names. What the write keeps of it from before is
// the bits it reaches, and only where anything reads what it did.
template <value::HeldAsWords T>
void InsertDesignatedWideBits(
    const void* designation, std::int64_t width, void* outer_parts,
    std::uint64_t outer_parts_count, const void* start, const void* bits,
    std::int64_t count, bool bits_are_four_state) {
  const std::optional<ResolvedBitRange> range =
      ResolveBitRange(width, outer_parts, outer_parts_count, start, count);
  if (!range.has_value()) {
    return;
  }
  const ErasedDesignation& within = DesignationAt(designation);
  const value::WideAt<T::kDomain> whole{
      .bytes = within.part, .width = static_cast<std::uint64_t>(width)};
  within.write->BeforeModifyBits(whole.MutableView(), range->clamped);
  if (!within.write->Tells()) {
    InsertBitsInto(
        whole.MutableView(), bits, count, bits_are_four_state, *range);
    return;
  }
  WriteOriginal<value::WideAt<T::kDomain>> kept(whole, range->clamped);
  InsertBitsInto(whole.MutableView(), bits, count, bits_are_four_state, *range);
  if (const std::optional<Change> change = kept.ChangeTo(whole)) {
    within.write->Landed(*change);
  }
}

// Bits read out of an integral value `width` bits wide lying at `bytes` (LRM
// 11.5.1): read where the value lies and kept where a part reaches, every other
// position of the answer reading x, or 0 in a two-state answer.
void ExtractBitsAt(
    const void* bytes, std::int64_t width, bool is_four_state,
    void* outer_parts, std::uint64_t outer_parts_count, const void* start,
    void* bits, std::int64_t count, bool bits_are_four_state) {
  const value::IntegralShape answer_shape =
      BitsShape(count, bits_are_four_state);
  value::ScopedWordsWrite answer(bits, answer_shape);
  value::FillDefault(answer.MutableView());
  const std::optional<ResolvedBitRange> range =
      ResolveBitRange(width, outer_parts, outer_parts_count, start, count);
  if (!range.has_value()) {
    return;
  }
  value::VisitLaidOut(
      bytes, BitsShape(width, is_four_state),
      [&](value::AnyConstIntegralView auto whole) {
        value::InsertWithin(
            answer.MutableView(), whole, -range->start,
            value::BitPositions{
                .lsb = range->clamped.lsb -
                       static_cast<std::uint64_t>(range->start),
                .width = range->clamped.width});
      });
}

// Bits written into an integral value `width` bits wide lying at `bytes`,
// which nothing is told of (LRM 11.5.1).
void InsertBitsAt(
    void* bytes, std::int64_t width, bool is_four_state, void* outer_parts,
    std::uint64_t outer_parts_count, const void* start, const void* bits,
    std::int64_t count, bool bits_are_four_state) {
  const std::optional<ResolvedBitRange> range =
      ResolveBitRange(width, outer_parts, outer_parts_count, start, count);
  if (!range.has_value()) {
    return;
  }
  value::ScopedWordsWrite whole(bytes, BitsShape(width, is_four_state));
  InsertBitsInto(whole.MutableView(), bits, count, bits_are_four_state, *range);
}

// What a comparison answered, as it crosses: the number its scalar is.
template <value::ComparisonAnswer R>
auto Compared(const R& answer) -> std::uint8_t {
  return std::to_underlying(value::AnswerScalar(answer));
}

// A place the program hands over -- where the operation calling an entry was
// written, or where a procedure it registers is -- which crosses as the one
// word it is.
auto SourceLocationAt(const void* location) -> base::SourceLocation {
  return base::SourceLocation(
      static_cast<const base::SourceLocation::Impl*>(location));
}

}  // namespace

}  // namespace lyra::runtime

using lyra::abi::ConstructionTable;
using lyra::abi::ElaboratedScopeRecord;
using lyra::abi::ObjectDefinition;
using lyra::abi::ScopeDeclarations;
using lyra::runtime::AdoptClosure;
using lyra::runtime::AdoptObject;
using lyra::runtime::AllocateValueCell;
using lyra::runtime::AnyReference;
using lyra::runtime::ArmWholeVariable;
using lyra::runtime::ArrayAt;
using lyra::runtime::ArrayTypeAt;
using lyra::runtime::AssertionControl;
using lyra::runtime::AssociativeCoreAt;
using lyra::runtime::AttemptsOf;
using lyra::runtime::BindPropertyEvaluation;
using lyra::runtime::Bit16;
using lyra::runtime::Bit32;
using lyra::runtime::Bit64;
using lyra::runtime::Bit8;
using lyra::runtime::BitsAt;
using lyra::runtime::BitWide;
using lyra::runtime::BlockEntry;
using lyra::runtime::BlockTable;
using lyra::runtime::BuildAt;
using lyra::runtime::BuildReference;
using lyra::runtime::CancellationTarget;
using lyra::runtime::CellBeginTakeover;
using lyra::runtime::CellDriveTakeover;
using lyra::runtime::CellEndTakeover;
using lyra::runtime::CellReportsTo;
using lyra::runtime::ChannelCancellation;
using lyra::runtime::ClassObject;
using lyra::runtime::ClosureValue;
using lyra::runtime::Compared;
using lyra::runtime::ControlEffect;
using lyra::runtime::Coroutine;
using lyra::runtime::CoroutineThread;
using lyra::runtime::current_runtime;
using lyra::runtime::CurrentExportScope;
using lyra::runtime::CurrentForeignThread;
using lyra::runtime::DeferredEffects;
using lyra::runtime::Delay;
using lyra::runtime::DelayReal;
using lyra::runtime::DelayTicks;
using lyra::runtime::DelayTicksReal;
using lyra::runtime::DesignateAssociativeArrayElement;
using lyra::runtime::DesignateFixedArrayElement;
using lyra::runtime::DesignateFoundElement;
using lyra::runtime::DesignationAt;
using lyra::runtime::DiagnosticDispatcher;
using lyra::runtime::DriveFromNet;
using lyra::runtime::DriveFromReference;
using lyra::runtime::DriveFromVariable;
using lyra::runtime::DriveOnForeignStack;
using lyra::runtime::DriverOf;
using lyra::runtime::DumpAll;
using lyra::runtime::DumpFile;
using lyra::runtime::DumpFlush;
using lyra::runtime::DumpLimit;
using lyra::runtime::DumpOff;
using lyra::runtime::DumpOn;
using lyra::runtime::DumpPortsAll;
using lyra::runtime::DumpPortsFlush;
using lyra::runtime::DumpPortsLimit;
using lyra::runtime::DumpPortsOf;
using lyra::runtime::DumpPortsOff;
using lyra::runtime::DumpPortsOn;
using lyra::runtime::DumpVarsLevels;
using lyra::runtime::DumpVarsOf;
using lyra::runtime::DumpVarsOfVariable;
using lyra::runtime::DynamicArrayTypeAt;
using lyra::runtime::Emplace;
using lyra::runtime::EndClosure;
using lyra::runtime::EnterCancellationTarget;
using lyra::runtime::ErasedAt;
using lyra::runtime::ErasedDesignation;
using lyra::runtime::ErasedObjectWrite;
using lyra::runtime::ErasedReference;
using lyra::runtime::EvaluationAttempt;
using lyra::runtime::EvaluationAttempts;
using lyra::runtime::EventSourceOf;
using lyra::runtime::FileTable;
using lyra::runtime::FindExportEntry;
using lyra::runtime::ForkWaitAll;
using lyra::runtime::ForkWaitFirst;
using lyra::runtime::FrameActivation;
using lyra::runtime::GcObject;
using lyra::runtime::GeneratedCallScope;
using lyra::runtime::HandleTo;
using lyra::runtime::HierarchySegment;
using lyra::runtime::HistoryAt;
using lyra::runtime::InsertDesignatedBits;
using lyra::runtime::InstantiationEntry;
using lyra::runtime::InstantiationTable;
using lyra::runtime::IntegralTypeAt;
using lyra::runtime::IntOf;
using lyra::runtime::LaidOutRefSet;
using lyra::runtime::LandDesignation;
using lyra::runtime::LandLaidOutDesignation;
using lyra::runtime::LeasedReference;
using lyra::runtime::LeaseObject;
using lyra::runtime::LeaveCancellationTarget;
using lyra::runtime::Logic16;
using lyra::runtime::Logic32;
using lyra::runtime::Logic64;
using lyra::runtime::Logic8;
using lyra::runtime::LogicWide;
using lyra::runtime::LoopBlocks;
using lyra::runtime::MakeForeignExecution;
using lyra::runtime::MakeSharedCell;
using lyra::runtime::NetOf;
using lyra::runtime::NetReportsTo;
using lyra::runtime::NetShowsSource;
using lyra::runtime::NumberAt;
using lyra::runtime::Observable;
using lyra::runtime::Observation;
using lyra::runtime::OpenCellWrite;
using lyra::runtime::OpenDriverWrite;
using lyra::runtime::OpenRefWrite;
using lyra::runtime::OpenTailCellWrite;
using lyra::runtime::OpenTailDriverWrite;
using lyra::runtime::OpenTailRefWrite;
using lyra::runtime::OpenWrite;
using lyra::runtime::OwnedLaidOutCopy;
using lyra::runtime::ParkAt;
using lyra::runtime::PositionNamedAt;
using lyra::runtime::ProcessAwait;
using lyra::runtime::ProcessKill;
using lyra::runtime::ProcessResume;
using lyra::runtime::ProcessSelf;
using lyra::runtime::ProcessStatus;
using lyra::runtime::ProcessSuspend;
using lyra::runtime::ProgramLifetime;
using lyra::runtime::QueueStepAppends;
using lyra::runtime::QueueTypeAt;
using lyra::runtime::RaiseDeclinedDeparture;
using lyra::runtime::Read;
using lyra::runtime::ReadReport;
using lyra::runtime::RealTimeInUnit;
using lyra::runtime::RefArmSampling;
using lyra::runtime::RefBeginTakeover;
using lyra::runtime::RefDriveTakeover;
using lyra::runtime::RefEndTakeover;
using lyra::runtime::ReferAssociativeArrayElement;
using lyra::runtime::ReferAssociativeArrayElementLeased;
using lyra::runtime::ReferDynamicArrayElement;
using lyra::runtime::ReferDynamicArrayElementLeased;
using lyra::runtime::ReferFixedArrayElement;
using lyra::runtime::ReferQueueElement;
using lyra::runtime::ReferQueueElementLeased;
using lyra::runtime::ReferToCell;
using lyra::runtime::ReferToProperty;
using lyra::runtime::ReferToStorage;
using lyra::runtime::RefGet;
using lyra::runtime::RefReportsTo;
using lyra::runtime::RefSampledGet;
using lyra::runtime::RefSet;
using lyra::runtime::RefuseReport;
using lyra::runtime::ResolvedNet;
using lyra::runtime::ResumeInNbaRegion;
using lyra::runtime::ResumeInRegionAfter;
using lyra::runtime::ResumeInRegionAfterReal;
using lyra::runtime::RunDesignRoot;
using lyra::runtime::RunHostCommand;
using lyra::runtime::RunNullHostCommand;
using lyra::runtime::RuntimeEffects;
using lyra::runtime::SampledHistory;
using lyra::runtime::ScalarPropagationDelay;
using lyra::runtime::Scope;
using lyra::runtime::SharedPointer;
using lyra::runtime::SimTimeInUnit;
using lyra::runtime::SourceLocationAt;
using lyra::runtime::SpanAt;
using lyra::runtime::SpawnAll;
using lyra::runtime::STimeInUnit;
using lyra::runtime::TailRefSampledGet;
using lyra::runtime::TakeBranches;
using lyra::runtime::TestPlusargs;
using lyra::runtime::Timer;
using lyra::runtime::Trigger;
using lyra::runtime::UnresolvedNet;
using lyra::runtime::UnresolvedNetOf;
using lyra::runtime::ValueCell;
using lyra::runtime::ValueCellAt;
using lyra::runtime::ValueCellFor;
using lyra::runtime::Var;
using lyra::runtime::VariableAt;
using lyra::runtime::VariableOf;
using lyra::runtime::VectorPropagationDelay;
using lyra::runtime::Wait;
using lyra::runtime::WaitFork;
using lyra::runtime::WaitOn;
using lyra::runtime::WaitOnPropertyEvaluation;
using lyra::runtime::WaitPerChangeOnImplicitList;
using lyra::runtime::WaitPerIntervalOnImplicitList;
using lyra::runtime::WaitRecollecting;
using lyra::runtime::WaitUntil;
using lyra::runtime::WatchedPlace;
using lyra::runtime::WeakObjectRef;
using lyra::value::BitVec;
using lyra::value::DpiBitBuffer;
using lyra::value::DpiLogicBuffer;
using lyra::value::DpiOpenArray;
using lyra::value::Enumeration;
using lyra::value::Format;
using lyra::value::FormatArg;
using lyra::value::FormatSpec;
using lyra::value::LaidOutValue;
using lyra::value::MakeFormatArg;
using lyra::value::ObjectRef;
using lyra::value::PrintItem;
using lyra::value::Real;
using lyra::value::Relocate;
using lyra::value::ShortReal;
using lyra::value::String;
using lyra::value::TimeFormat;
using lyra::value::UnmanagedRef;
using lyra::value::UnpackedRange;

extern "C" {

auto lyra_rt_current_runtime() noexcept -> void* {
  return &lyra::runtime::current_runtime();
}

auto lyra_rt_files(void* runtime) -> void* {
  return &static_cast<RuntimeEffects*>(runtime)->Files();
}

auto lyra_rt_time_format(void* runtime) -> const void* {
  return &static_cast<RuntimeEffects*>(runtime)->TimeFormat();
}

void lyra_rt_set_time_format(
    void* runtime, std::int64_t units_power, std::int64_t precision,
    const void* suffix, std::int64_t min_width) {
  static_cast<RuntimeEffects*>(runtime)->SetTimeFormat(
      units_power, precision, Read<String>(suffix), min_width);
}

void lyra_rt_reset_time_format(void* runtime) {
  static_cast<RuntimeEffects*>(runtime)->ResetTimeFormat();
}

auto lyra_rt_file_open(void* files, const void* name, void* out) -> void* {
  return Emplace(out, static_cast<FileTable*>(files)->Open(Read<String>(name)));
}

auto lyra_rt_file_open_mode(
    void* files, const void* name, const void* mode, void* out) -> void* {
  return Emplace(
      out, static_cast<FileTable*>(files)->OpenWithMode(
               Read<String>(name), Read<String>(mode)));
}

void lyra_rt_file_close(void* files, std::int64_t descriptor) {
  static_cast<FileTable*>(files)->Close(descriptor);
}

auto lyra_rt_file_getc(void* files, std::int64_t fd, void* out) -> void* {
  return Emplace(out, static_cast<FileTable*>(files)->Getc(fd));
}

auto lyra_rt_file_gets(
    void* files, std::int64_t fd, const void* answer_type, void* out) -> void* {
  return lyra::runtime::EmplaceBoth(
      out, lyra::runtime::TupleTypeAt(answer_type),
      static_cast<FileTable*>(files)->Gets(fd));
}

auto lyra_rt_file_scan(
    void* files, std::int64_t fd, const void* format, const void* prototypes,
    const void* prototypes_type, const void* location, const void* answer_type,
    void* out) -> void* {
  lyra::runtime::FileScanInput source =
      static_cast<FileTable*>(files)->ScanInputOf(fd);
  return lyra::runtime::EmplaceScan(
      out, lyra::runtime::TupleTypeAt(answer_type), source,
      Read<String>(format), prototypes,
      lyra::runtime::TupleTypeAt(prototypes_type), SourceLocationAt(location));
}

auto lyra_rt_file_error(
    void* files, std::int64_t fd, const void* answer_type, void* out) -> void* {
  return lyra::runtime::EmplaceBoth(
      out, lyra::runtime::TupleTypeAt(answer_type),
      static_cast<FileTable*>(files)->Error(fd));
}

auto lyra_rt_file_read(
    void* files, const void* dest, std::int64_t dest_width, bool dest_is_signed,
    bool dest_is_four_state, std::int64_t fd, const void* answer_type,
    void* out) -> void* {
  lyra::value::WrittenWords read_into = lyra::value::WrittenWords::CopyOf(
      dest, lyra::runtime::NumberShape(
                dest_width, dest_is_signed, dest_is_four_state));
  const lyra::value::Int read =
      static_cast<FileTable*>(files)->ReadInto(read_into.MutableView(), fd);
  return lyra::runtime::EmplaceWithWords(
      out, lyra::runtime::TupleTypeAt(answer_type), read, read_into);
}

auto lyra_rt_file_read_words(
    void* files, std::int64_t fd, void* bounds, std::uint64_t bounds_count,
    std::int64_t start, std::int64_t count, std::int64_t word_width, void* out)
    -> void* {
  return std::construct_at(
      static_cast<lyra::runtime::MemoryWords*>(out),
      static_cast<FileTable*>(files)->ReadWords(
          fd, SpanAt<const std::int64_t>(bounds, bounds_count), start, count,
          word_width));
}

auto lyra_rt_file_ungetc(
    void* files, std::int64_t c, std::int64_t fd, void* out) -> void* {
  return Emplace(out, static_cast<FileTable*>(files)->Ungetc(c, fd));
}

auto lyra_rt_file_seek(
    void* files, std::int64_t fd, std::int64_t offset, std::int64_t operation,
    void* out) -> void* {
  return Emplace(
      out, static_cast<FileTable*>(files)->Seek(fd, offset, operation));
}

auto lyra_rt_file_rewind(void* files, std::int64_t fd, void* out) -> void* {
  return Emplace(out, static_cast<FileTable*>(files)->Rewind(fd));
}

auto lyra_rt_file_tell(void* files, std::int64_t fd, void* out) -> void* {
  return Emplace(out, static_cast<FileTable*>(files)->Tell(fd));
}

auto lyra_rt_file_eof(void* files, std::int64_t fd, void* out) -> void* {
  return Emplace(out, static_cast<FileTable*>(files)->Eof(fd));
}

void lyra_rt_file_flush(void* files, std::int64_t descriptor) {
  static_cast<FileTable*>(files)->Flush(descriptor);
}

void lyra_rt_file_flush_all(void* files) {
  static_cast<FileTable*>(files)->FlushAll();
}

auto lyra_rt_cancellation_for(void* files, std::int64_t descriptor, void* out)
    -> void* {
  return Emplace(
      out, static_cast<FileTable*>(files)->CancellationFor(descriptor));
}

auto lyra_rt_is_cancelled(const void* cancellation, void* out) -> void* {
  return Emplace(out, Read<ChannelCancellation>(cancellation).IsCancelled());
}

auto lyra_rt_string_from_text(void* cstr, void* out) -> void* {
  return Emplace(out, String(static_cast<const char*>(cstr)));
}

auto lyra_rt_string_default(void* out) -> void* {
  return Emplace(out, String());
}

auto lyra_rt_text_print_item_make(const void* text, void* out) -> void* {
  return Emplace(out, PrintItem::Text(Read<String>(text)));
}

auto lyra_rt_format(
    void* items, std::uint64_t items_count, const void* time_format,
    const void* location, void* out) -> void* {
  const std::span<PrintItem*> handles = SpanAt<PrintItem*>(items, items_count);
  std::vector<PrintItem> collected;
  collected.reserve(items_count);
  for (PrintItem* handle : handles) {
    collected.push_back(*handle);
  }
  return Emplace(
      out, Format(
               collected, *static_cast<const TimeFormat*>(time_format),
               SourceLocationAt(location)));
}

void lyra_rt_writeln(void* files, std::int64_t descriptor, void* text) {
  static_cast<FileTable*>(files)->Writeln(
      descriptor, *static_cast<String*>(text));
}

void lyra_rt_write(void* files, std::int64_t descriptor, void* text) {
  static_cast<FileTable*>(files)->Write(
      descriptor, *static_cast<String*>(text));
}

auto lyra_rt_diagnostic(void* runtime) -> void* {
  return &static_cast<RuntimeEffects*>(runtime)->Diagnostic();
}

void lyra_rt_emit_info(
    void* dispatcher, const void* text, const void* scope,
    const void* location) {
  static_cast<DiagnosticDispatcher*>(dispatcher)
      ->EmitInfo(
          Read<String>(text), Read<String>(scope), SourceLocationAt(location));
}

void lyra_rt_emit_warning(
    void* dispatcher, const void* text, const void* scope,
    const void* location) {
  static_cast<DiagnosticDispatcher*>(dispatcher)
      ->EmitWarning(
          Read<String>(text), Read<String>(scope), SourceLocationAt(location));
}

void lyra_rt_emit_error(
    void* dispatcher, const void* text, const void* scope,
    const void* location) {
  static_cast<DiagnosticDispatcher*>(dispatcher)
      ->EmitError(
          Read<String>(text), Read<String>(scope), SourceLocationAt(location));
}

void lyra_rt_emit_fatal(
    void* dispatcher, const void* text, const void* scope,
    const void* location) {
  static_cast<DiagnosticDispatcher*>(dispatcher)
      ->EmitFatal(
          Read<String>(text), Read<String>(scope), SourceLocationAt(location));
}

void lyra_rt_emit_info_at_caller(
    void* dispatcher, const void* text, const void* location) {
  static_cast<DiagnosticDispatcher*>(dispatcher)
      ->EmitInfoAtCaller(Read<String>(text), SourceLocationAt(location));
}

void lyra_rt_emit_warning_at_caller(
    void* dispatcher, const void* text, const void* location) {
  static_cast<DiagnosticDispatcher*>(dispatcher)
      ->EmitWarningAtCaller(Read<String>(text), SourceLocationAt(location));
}

void lyra_rt_emit_error_at_caller(
    void* dispatcher, const void* text, const void* location) {
  static_cast<DiagnosticDispatcher*>(dispatcher)
      ->EmitErrorAtCaller(Read<String>(text), SourceLocationAt(location));
}

void lyra_rt_emit_fatal_at_caller(
    void* dispatcher, const void* text, const void* location) {
  static_cast<DiagnosticDispatcher*>(dispatcher)
      ->EmitFatalAtCaller(Read<String>(text), SourceLocationAt(location));
}

void lyra_rt_record_coverage(void* runtime, const void* site, bool succeeded) {
  static_cast<RuntimeEffects*>(runtime)->RecordCoverage(
      SourceLocationAt(site), succeeded);
}

auto lyra_rt_enter_coroutine(void* frame, void* out) noexcept -> void* {
  return lyra::runtime::StartGeneratedProcess(
      out, lyra::runtime::GeneratedCoroutine(frame));
}

auto lyra_rt_await_coroutine(void* runtime, void* activation) -> bool {
  auto& svc = *static_cast<RuntimeEffects*>(runtime);
  CoroutineThread& thread = svc.RunningThread();
  FrameActivation* const caller = thread.Leaf();
  FrameActivation* const called = thread.PushActivation(
      std::move(*static_cast<Coroutine<void>*>(activation)));
  called->coroutine.resume();
  // An activation that consumed no time is over before its caller could have
  // waited for it, and the caller is still on the stack below, so nothing
  // continues it and it must not park.
  if (called->coroutine.done()) {
    return false;
  }
  called->continuation = caller->coroutine;
  return true;
}

void lyra_rt_release_coroutine(void* runtime) {
  CoroutineThread& thread =
      static_cast<RuntimeEffects*>(runtime)->RunningThread();
  // A run-time error that left the called body was stored rather than allowed
  // to travel, because a coroutine's promise stores whatever escapes the body
  // it drives. The call is one statement of this thread, so it continues from
  // here -- taken before the activation is released, which destroys the promise
  // holding it.
  std::exception_ptr raised = thread.TakeInnermostRaisedError();
  thread.PopActivation();
  if (raised) {
    std::rethrow_exception(raised);
  }
}

void lyra_rt_spawn_all(
    void* runtime, void* branches, std::uint64_t branches_count) {
  auto& svc = *static_cast<RuntimeEffects*>(runtime);
  std::vector<Coroutine<void>> taken = TakeBranches(branches, branches_count);
  SpawnAll(svc, std::span<Coroutine<void>>{taken});
}

auto lyra_rt_fork_wait_all(
    void* runtime, void* branches, std::uint64_t branches_count, void* out)
    -> void* {
  auto& svc = *static_cast<RuntimeEffects*>(runtime);
  std::vector<Coroutine<void>> taken = TakeBranches(branches, branches_count);
  return Emplace(out, ForkWaitAll(svc, std::span<Coroutine<void>>{taken}));
}

auto lyra_rt_fork_wait_first(
    void* runtime, void* branches, std::uint64_t branches_count, void* out)
    -> void* {
  auto& svc = *static_cast<RuntimeEffects*>(runtime);
  std::vector<Coroutine<void>> taken = TakeBranches(branches, branches_count);
  return Emplace(out, ForkWaitFirst(svc, std::span<Coroutine<void>>{taken}));
}

auto lyra_rt_wait_fork(void* runtime, void* out) -> void* {
  return Emplace(out, WaitFork(*static_cast<RuntimeEffects*>(runtime)));
}

void lyra_rt_disable_fork(void* runtime) {
  lyra::runtime::DisableFork(*static_cast<RuntimeEffects*>(runtime));
}

void lyra_rt_require_caller_may_schedule_after_return(
    void* runtime, const void* location) {
  lyra::runtime::RequireCallerMayScheduleAfterReturn(
      *static_cast<RuntimeEffects*>(runtime), SourceLocationAt(location));
}

auto lyra_rt_take_no_thread_context(void* runtime, void* out) -> void* {
  return Emplace(
      out, lyra::runtime::TakeNoThreadContext(
               *static_cast<RuntimeEffects*>(runtime)));
}

void lyra_rt_enter_no_thread(const void* taken) {
  static_cast<const lyra::runtime::NoThreadContext*>(taken)->Enter();
}

auto lyra_rt_process_self(void* runtime, void* out) -> void* {
  return Emplace(out, ProcessSelf(*static_cast<RuntimeEffects*>(runtime)));
}

auto lyra_rt_process_status(const void* self, const void* location, void* out)
    -> void* {
  return Emplace(
      out, ProcessStatus(Read<ObjectRef>(self), SourceLocationAt(location)));
}

void lyra_rt_process_kill(
    const void* self, void* runtime, const void* location) {
  ProcessKill(
      Read<ObjectRef>(self), *static_cast<RuntimeEffects*>(runtime),
      SourceLocationAt(location));
}

auto lyra_rt_process_await(
    const void* self, void* runtime, const void* location, void* out) -> void* {
  return Emplace(
      out, ProcessAwait(
               Read<ObjectRef>(self), *static_cast<RuntimeEffects*>(runtime),
               SourceLocationAt(location)));
}

void lyra_rt_process_suspend(
    const void* self, void* runtime, const void* location) {
  ProcessSuspend(
      Read<ObjectRef>(self), *static_cast<RuntimeEffects*>(runtime),
      SourceLocationAt(location));
}

void lyra_rt_process_resume(
    const void* self, void* runtime, const void* location) {
  ProcessResume(
      Read<ObjectRef>(self), *static_cast<RuntimeEffects*>(runtime),
      SourceLocationAt(location));
}

void lyra_rt_object_srandom(
    void* object, const void* seed, std::int64_t seed_width,
    bool seed_is_signed, bool seed_is_four_state) {
  lyra::runtime::ObjectSrandom(
      static_cast<ClassObject*>(object),
      NumberAt(seed, seed_width, seed_is_signed, seed_is_four_state).View());
}

auto lyra_rt_object_get_randstate(void* object, void* out) -> void* {
  return Emplace(
      out,
      lyra::runtime::ObjectGetRandstate(static_cast<ClassObject*>(object)));
}

void lyra_rt_object_set_randstate(
    void* object, const void* state, const void* location) {
  lyra::runtime::ObjectSetRandstate(
      static_cast<ClassObject*>(object), Read<String>(state),
      SourceLocationAt(location));
}

void lyra_rt_builtin_pre_randomize() {
  lyra::runtime::BuiltinPreRandomize();
}

void lyra_rt_builtin_post_randomize() {
  lyra::runtime::BuiltinPostRandomize();
}

void lyra_rt_object_pre_randomize(void* object) {
  lyra::runtime::ObjectPreRandomize(static_cast<GcObject*>(object));
}

void lyra_rt_object_post_randomize(void* object) {
  lyra::runtime::ObjectPostRandomize(static_cast<GcObject*>(object));
}

auto lyra_rt_weak_downgrade(const void* strong, void* out) -> void* {
  return Emplace(out, lyra::runtime::Downgrade(Read<ObjectRef>(strong)));
}

auto lyra_rt_weak_upgrade(const void* weak, void* evaluation, void* out)
    -> void* {
  return Emplace(
      out,
      lyra::runtime::Upgrade(
          Read<WeakObjectRef>(weak), static_cast<ReadReport*>(evaluation)));
}

auto lyra_rt_weak_refers_to(const void* weak, const void* strong) -> bool {
  return lyra::runtime::WeakRefersTo(
      Read<WeakObjectRef>(weak), Read<ObjectRef>(strong));
}

auto lyra_rt_object_identity(const void* object, void* out) -> void* {
  return Emplace(out, lyra::runtime::ObjectIdentity(Read<ObjectRef>(object)));
}

auto lyra_rt_randomize_object(
    void* object, void* runtime, const void* location, void* out) -> void* {
  return Emplace(
      out,
      lyra::runtime::RandomizeObject(
          static_cast<ClassObject*>(object),
          *static_cast<RuntimeEffects*>(runtime), SourceLocationAt(location)));
}

auto lyra_rt_randomize_object_listed(
    void* object, void* runtime, const void* location, void* out) -> void* {
  return Emplace(
      out,
      lyra::runtime::RandomizeObjectListed(
          static_cast<ClassObject*>(object),
          *static_cast<RuntimeEffects*>(runtime), SourceLocationAt(location)));
}

auto lyra_rt_randomize_scope(void* runtime, const void* location, void* out)
    -> void* {
  return Emplace(
      out,
      lyra::runtime::RandomizeScope(
          *static_cast<RuntimeEffects*>(runtime), SourceLocationAt(location)));
}

void lyra_rt_randomize_list(const void* problem, const void* reference) {
  lyra::runtime::RandomWalkList(Read<ObjectRef>(problem), ErasedAt(reference));
}

void lyra_rt_randomize_constrain(const void* problem) {
  lyra::runtime::RandomizeConstrain(Read<ObjectRef>(problem));
}

auto lyra_rt_randomize_finish(const void* problem, void* out) -> void* {
  return Emplace(out, lyra::runtime::RandomizeFinish(Read<ObjectRef>(problem)));
}

void lyra_rt_constraint_members(
    const void* problem, const void* reference, const void* enumeration) {
  lyra::runtime::ConstraintMembersOf(
      Read<ObjectRef>(problem), ErasedAt(reference),
      Read<Enumeration>(enumeration));
}

void lyra_rt_constraint_rand(const void* problem, const void* reference) {
  lyra::runtime::RandomWalkDeclare(
      Read<ObjectRef>(problem), ErasedAt(reference),
      lyra::support::RandomQualifier::kRand);
}

void lyra_rt_constraint_randc(const void* problem, const void* reference) {
  lyra::runtime::RandomWalkDeclare(
      Read<ObjectRef>(problem), ErasedAt(reference),
      lyra::support::RandomQualifier::kRandc);
}

void lyra_rt_random_walk_fixed_array(const void* problem) {
  lyra::runtime::RandomWalkFixedArray(Read<ObjectRef>(problem));
}
void lyra_rt_random_walk_resizable_array(
    const void* problem, const void* reference, const void* type) {
  const lyra::runtime::ErasedReference& array = ErasedAt(reference);
  lyra::runtime::RandomWalkResizableArray(
      Read<ObjectRef>(problem), array,
      lyra::runtime::ResizeOf(array, lyra::runtime::TypeAt(type)));
}
void lyra_rt_random_walk_enter(const void* problem, std::int64_t position) {
  lyra::runtime::RandomWalkEnter(Read<ObjectRef>(problem), position);
}
void lyra_rt_random_walk_leave(const void* problem) {
  lyra::runtime::RandomWalkLeave(Read<ObjectRef>(problem));
}
void lyra_rt_random_walk_leaf(
    const void* problem, const void* reference, const void* type) {
  lyra::runtime::RandomWalkLeaf(
      Read<ObjectRef>(problem), ErasedAt(reference),
      &lyra::runtime::TypeAt(type));
}
void lyra_rt_random_walk_end(const void* problem) {
  lyra::runtime::RandomWalkEnd(Read<ObjectRef>(problem));
}

auto lyra_rt_constraint_variable(
    const void* problem, const void* reference, const void* value,
    const void* value_type) -> std::int64_t {
  return lyra::runtime::ConstraintVariableErased(
      Read<ObjectRef>(problem), ErasedAt(reference), value,
      lyra::runtime::TypeAt(value_type));
}

auto lyra_rt_constraint_constant(
    const void* problem, const void* value, const void* value_type)
    -> std::int64_t {
  return lyra::runtime::ConstraintConstantErased(
      Read<ObjectRef>(problem), value, lyra::runtime::TypeAt(value_type));
}

void lyra_rt_randomize_nested(const void* problem, const void* reference) {
  lyra::runtime::RandomWalkNested(
      Read<ObjectRef>(problem), ErasedAt(reference));
}

auto lyra_rt_constraint_block_enabled(void* object, const void* name) -> bool {
  return lyra::runtime::ConstraintBlockEnabled(
      static_cast<GcObject*>(object), Read<String>(name));
}

auto lyra_rt_static_constraint_block_enabled(const void* key) -> bool {
  return lyra::runtime::StaticConstraintBlockEnabled(Read<String>(key));
}

auto lyra_rt_constraint_negate(const void* problem, std::int64_t a)
    -> std::int64_t {
  return lyra::runtime::ConstraintNegate(Read<ObjectRef>(problem), a);
}

auto lyra_rt_constraint_bitwise_not(const void* problem, std::int64_t a)
    -> std::int64_t {
  return lyra::runtime::ConstraintBitwiseNot(Read<ObjectRef>(problem), a);
}

auto lyra_rt_constraint_reduce_and(const void* problem, std::int64_t a)
    -> std::int64_t {
  return lyra::runtime::ConstraintReduceAnd(Read<ObjectRef>(problem), a);
}

auto lyra_rt_constraint_reduce_or(const void* problem, std::int64_t a)
    -> std::int64_t {
  return lyra::runtime::ConstraintReduceOr(Read<ObjectRef>(problem), a);
}

auto lyra_rt_constraint_reduce_xor(const void* problem, std::int64_t a)
    -> std::int64_t {
  return lyra::runtime::ConstraintReduceXor(Read<ObjectRef>(problem), a);
}

auto lyra_rt_constraint_logical_not(const void* problem, std::int64_t a)
    -> std::int64_t {
  return lyra::runtime::ConstraintLogicalNot(Read<ObjectRef>(problem), a);
}

auto lyra_rt_constraint_add(const void* problem, std::int64_t a, std::int64_t b)
    -> std::int64_t {
  return lyra::runtime::ConstraintAdd(Read<ObjectRef>(problem), a, b);
}

auto lyra_rt_constraint_subtract(
    const void* problem, std::int64_t a, std::int64_t b) -> std::int64_t {
  return lyra::runtime::ConstraintSubtract(Read<ObjectRef>(problem), a, b);
}

auto lyra_rt_constraint_multiply(
    const void* problem, std::int64_t a, std::int64_t b) -> std::int64_t {
  return lyra::runtime::ConstraintMultiply(Read<ObjectRef>(problem), a, b);
}

auto lyra_rt_constraint_bitwise_and(
    const void* problem, std::int64_t a, std::int64_t b) -> std::int64_t {
  return lyra::runtime::ConstraintBitwiseAnd(Read<ObjectRef>(problem), a, b);
}

auto lyra_rt_constraint_bitwise_or(
    const void* problem, std::int64_t a, std::int64_t b) -> std::int64_t {
  return lyra::runtime::ConstraintBitwiseOr(Read<ObjectRef>(problem), a, b);
}

auto lyra_rt_constraint_bitwise_xor(
    const void* problem, std::int64_t a, std::int64_t b) -> std::int64_t {
  return lyra::runtime::ConstraintBitwiseXor(Read<ObjectRef>(problem), a, b);
}

auto lyra_rt_constraint_equal(
    const void* problem, std::int64_t a, std::int64_t b) -> std::int64_t {
  return lyra::runtime::ConstraintEqual(Read<ObjectRef>(problem), a, b);
}

auto lyra_rt_constraint_logical_and(
    const void* problem, std::int64_t a, std::int64_t b) -> std::int64_t {
  return lyra::runtime::ConstraintLogicalAnd(Read<ObjectRef>(problem), a, b);
}

auto lyra_rt_constraint_logical_or(
    const void* problem, std::int64_t a, std::int64_t b) -> std::int64_t {
  return lyra::runtime::ConstraintLogicalOr(Read<ObjectRef>(problem), a, b);
}

auto lyra_rt_constraint_divide(
    const void* problem, std::int64_t a, std::int64_t b, bool is_signed)
    -> std::int64_t {
  return lyra::runtime::ConstraintDivide(
      Read<ObjectRef>(problem), a, b, is_signed);
}

auto lyra_rt_constraint_remainder(
    const void* problem, std::int64_t a, std::int64_t b, bool is_signed)
    -> std::int64_t {
  return lyra::runtime::ConstraintRemainder(
      Read<ObjectRef>(problem), a, b, is_signed);
}

auto lyra_rt_constraint_shift_left(
    const void* problem, std::int64_t a, std::int64_t amount) -> std::int64_t {
  return lyra::runtime::ConstraintShiftLeft(
      Read<ObjectRef>(problem), a, amount);
}

auto lyra_rt_constraint_shift_right(
    const void* problem, std::int64_t a, std::int64_t amount, bool arithmetic)
    -> std::int64_t {
  return lyra::runtime::ConstraintShiftRight(
      Read<ObjectRef>(problem), a, amount, arithmetic);
}

auto lyra_rt_constraint_less(
    const void* problem, std::int64_t a, std::int64_t b, bool is_signed)
    -> std::int64_t {
  return lyra::runtime::ConstraintLess(
      Read<ObjectRef>(problem), a, b, is_signed);
}

auto lyra_rt_constraint_concatenate(
    const void* problem, std::int64_t high, std::int64_t low) -> std::int64_t {
  return lyra::runtime::ConstraintConcatenate(
      Read<ObjectRef>(problem), high, low);
}

auto lyra_rt_constraint_select(
    const void* problem, std::int64_t condition, std::int64_t then_term,
    std::int64_t else_term) -> std::int64_t {
  return lyra::runtime::ConstraintSelect(
      Read<ObjectRef>(problem), condition, then_term, else_term);
}

auto lyra_rt_constraint_resize(
    const void* problem, std::int64_t a, std::int64_t bits, bool sign_extend)
    -> std::int64_t {
  return lyra::runtime::ConstraintResize(
      Read<ObjectRef>(problem), a, bits, sign_extend);
}

auto lyra_rt_constraint_slice(
    const void* problem, std::int64_t a, std::int64_t offset, std::int64_t bits)
    -> std::int64_t {
  return lyra::runtime::ConstraintSlice(
      Read<ObjectRef>(problem), a, offset, bits);
}

void lyra_rt_constraint_require(const void* problem, std::int64_t condition) {
  lyra::runtime::ConstraintRequire(Read<ObjectRef>(problem), condition);
}

void lyra_rt_constraint_prefer(const void* problem, std::int64_t condition) {
  lyra::runtime::ConstraintPrefer(Read<ObjectRef>(problem), condition);
}

void lyra_rt_constraint_disable_soft(
    const void* problem, std::int64_t variable) {
  lyra::runtime::ConstraintDisableSoft(Read<ObjectRef>(problem), variable);
}

void lyra_rt_constraint_solve_before(
    const void* problem, std::int64_t before, std::int64_t after) {
  lyra::runtime::ConstraintSolveBefore(Read<ObjectRef>(problem), before, after);
}

auto lyra_rt_constraint_distribution(
    const void* problem, std::int64_t subject, std::int64_t guard,
    bool is_signed, bool soft) -> std::int64_t {
  return lyra::runtime::ConstraintDistribution(
      Read<ObjectRef>(problem), subject, guard, is_signed, soft);
}

void lyra_rt_constraint_distribution_item(
    const void* problem, std::int64_t distribution, std::int64_t low,
    std::int64_t high, std::int64_t weight, bool per_value) {
  lyra::runtime::ConstraintDistributionItem(
      Read<ObjectRef>(problem), distribution, low, high, weight, per_value);
}

void lyra_rt_constraint_distribution_default(
    const void* problem, std::int64_t distribution, std::int64_t weight) {
  lyra::runtime::ConstraintDistributionDefault(
      Read<ObjectRef>(problem), distribution, weight);
}

auto lyra_rt_constraint_unique_set(const void* problem) -> std::int64_t {
  return lyra::runtime::ConstraintUniqueSet(Read<ObjectRef>(problem));
}

void lyra_rt_constraint_unique_member(
    const void* problem, std::int64_t set, std::int64_t term) {
  lyra::runtime::ConstraintUniqueMember(Read<ObjectRef>(problem), set, term);
}

void lyra_rt_constraint_unique_array(
    const void* problem, std::int64_t set, const void* reference) {
  lyra::runtime::RandomWalkUnique(
      Read<ObjectRef>(problem), set, ErasedAt(reference));
}

void lyra_rt_constraint_unique_require(
    const void* problem, std::int64_t set, std::int64_t guard) {
  lyra::runtime::ConstraintUniqueRequire(Read<ObjectRef>(problem), set, guard);
}

auto lyra_rt_constraint_array_size(
    const void* problem, const void* reference, std::int64_t count)
    -> std::int64_t {
  return lyra::runtime::ConstraintArraySize(
      Read<ObjectRef>(problem), ErasedAt(reference), count);
}

void lyra_rt_constraint_want(const void* problem, std::int64_t variable) {
  lyra::runtime::ConstraintWant(Read<ObjectRef>(problem), variable);
}

auto lyra_rt_randomize_settle_first(const void* problem) -> bool {
  return lyra::runtime::RandomizeSettleFirst(Read<ObjectRef>(problem));
}

void lyra_rt_rand_mode_set(
    const void* declared, void* positions, std::uint64_t positions_count,
    bool on) {
  lyra::runtime::RandModeSet(
      ErasedAt(declared),
      SpanAt<const std::int64_t>(positions, positions_count), on);
}

auto lyra_rt_rand_mode_get(
    const void* declared, void* positions, std::uint64_t positions_count,
    void* out) -> void* {
  return Emplace(
      out, lyra::runtime::RandModeGet(
               ErasedAt(declared),
               SpanAt<const std::int64_t>(positions, positions_count)));
}

void lyra_rt_object_rand_mode_set(void* object, bool on) {
  lyra::runtime::ObjectRandModeSet(static_cast<GcObject*>(object), on);
}

void lyra_rt_constraint_mode_set(void* object, const void* name, bool on) {
  lyra::runtime::ConstraintModeSet(
      static_cast<GcObject*>(object), Read<String>(name), on);
}

auto lyra_rt_constraint_mode_get(void* object, const void* name, void* out)
    -> void* {
  return Emplace(
      out, lyra::runtime::ConstraintModeGet(
               static_cast<GcObject*>(object), Read<String>(name)));
}

void lyra_rt_object_constraint_mode_set(void* object, bool on) {
  lyra::runtime::ObjectConstraintModeSet(static_cast<GcObject*>(object), on);
}

void lyra_rt_static_constraint_mode_set(const void* key, bool on) {
  lyra::runtime::StaticConstraintModeSet(Read<String>(key), on);
}

auto lyra_rt_static_constraint_mode_get(const void* key, void* out) -> void* {
  return Emplace(
      out, lyra::runtime::StaticConstraintModeGet(Read<String>(key)));
}

void lyra_rt_closure_box_end(void* closure) {
  EndClosure{}(static_cast<ClosureValue*>(closure));
}

auto lyra_rt_object_adopt(void* object, void* out) -> void* {
  return Emplace(out, AdoptObject(static_cast<GcObject*>(object)));
}

auto lyra_rt_string_shared_cell_make(void* out) -> void* {
  return MakeSharedCell<String>(out);
}

auto lyra_rt_real_shared_cell_make(void* out) -> void* {
  return MakeSharedCell<Real>(out);
}

auto lyra_rt_shortreal_shared_cell_make(void* out) -> void* {
  return MakeSharedCell<ShortReal>(out);
}

auto lyra_rt_unmanagedref_shared_cell_make(void* out) -> void* {
  return MakeSharedCell<UnmanagedRef>(out);
}

auto lyra_rt_managedref_shared_cell_make(void* out) -> void* {
  return MakeSharedCell<ObjectRef>(out);
}

auto lyra_rt_laidout_shared_cell_make(const void* held_type, void* out)
    -> void* {
  return MakeSharedCell<LaidOutValue>(
      out, lyra::runtime::ElementTypeAt(held_type));
}

auto lyra_rt_shared_pointer_deref(void* handle) -> void* {
  return Read<SharedPointer>(handle).Pointee();
}

auto lyra_rt_nba_effects(void* runtime) -> void* {
  return static_cast<RuntimeEffects*>(runtime)->NbaEffects();
}

auto lyra_rt_nba_effects_after(
    void* runtime, const void* duration, std::int64_t duration_width,
    bool duration_is_signed, bool duration_is_four_state,
    std::int64_t unit_power, std::int64_t precision_power) -> void* {
  const lyra::value::LoadedWords amount = NumberAt(
      duration, duration_width, duration_is_signed, duration_is_four_state);
  return static_cast<RuntimeEffects*>(runtime)->NbaEffectsAfter(
      amount.View(), unit_power, precision_power);
}

auto lyra_rt_nba_effects_after_real(
    void* runtime, const void* duration, std::int64_t unit_power,
    std::int64_t precision_power) -> void* {
  return static_cast<RuntimeEffects*>(runtime)->NbaEffectsAfterReal(
      Read<Real>(duration), unit_power, precision_power);
}

auto lyra_rt_region_effects_after(
    void* runtime, std::int64_t region, const void* duration,
    std::int64_t duration_width, bool duration_is_signed,
    bool duration_is_four_state, std::int64_t unit_power,
    std::int64_t precision_power) -> void* {
  const lyra::value::LoadedWords amount = NumberAt(
      duration, duration_width, duration_is_signed, duration_is_four_state);
  return static_cast<RuntimeEffects*>(runtime)->RegionEffectsAfter(
      region, amount.View(), unit_power, precision_power);
}

auto lyra_rt_region_effects_after_real(
    void* runtime, std::int64_t region, const void* duration,
    std::int64_t unit_power, std::int64_t precision_power) -> void* {
  return static_cast<RuntimeEffects*>(runtime)->RegionEffectsAfterReal(
      region, Read<Real>(duration), unit_power, precision_power);
}

auto lyra_rt_pass_end_effects(void* runtime) -> void* {
  return static_cast<RuntimeEffects*>(runtime)->PassEndEffects();
}

auto lyra_rt_deferred_effects_allocate(
    void* effects, std::uint64_t size, std::uint64_t alignment) -> void* {
  return static_cast<DeferredEffects*>(effects)->Allocate(size, alignment);
}

void lyra_rt_deferred_effects_commit(void* effects, void* made) {
  static_cast<DeferredEffects*>(effects)->Commit(made);
}

void lyra_rt_run_detached(void* runtime, void* carrier) {
  static_cast<RuntimeEffects*>(runtime)->RunDetached(
      std::move(*static_cast<Coroutine<void>*>(carrier)));
}

void lyra_rt_run_observing(void* runtime, void* started) {
  static_cast<RuntimeEffects*>(runtime)->RunObserving(
      std::move(*static_cast<Coroutine<void>*>(started)));
}

void lyra_rt_submit_at_run_end(void* runtime, void* closure) {
  static_cast<RuntimeEffects*>(runtime)->SubmitAtRunEnd(
      AdoptClosure<lyra::support::EffectSignature>(closure));
}

auto lyra_rt_postponed_effects(void* runtime) -> void* {
  return static_cast<RuntimeEffects*>(runtime)->PostponedEffects();
}

auto lyra_rt_observed_effects(void* runtime) -> void* {
  return static_cast<RuntimeEffects*>(runtime)->ObservedEffects();
}

auto lyra_rt_assertion_control_allocate(
    void* control, std::uint64_t size, std::uint64_t alignment) -> void* {
  return static_cast<AssertionControl*>(control)->Allocate(size, alignment);
}

void lyra_rt_assertion_control_commit(const void* control, void* made) {
  static_cast<const AssertionControl*>(control)->Commit(made);
}

void lyra_rt_assertion_control_describe(
    void* control, std::uint64_t type, std::uint64_t directive) {
  static_cast<AssertionControl*>(control)->Describe(type, directive);
}

void lyra_rt_assertion_control_register_in(
    void* control, void* effects, void* scope) {
  static_cast<AssertionControl*>(control)->RegisterIn(
      *static_cast<RuntimeEffects*>(effects), static_cast<Scope*>(scope));
}

void lyra_rt_assertion_control_register(void* control, void* effects) {
  static_cast<AssertionControl*>(control)->Register(
      *static_cast<RuntimeEffects*>(effects));
}

void lyra_rt_assertion_control_register_within(
    void* control, void* effects, const void* block) {
  static_cast<AssertionControl*>(control)->RegisterWithin(
      *static_cast<RuntimeEffects*>(effects),
      static_cast<const CancellationTarget*>(block));
}

auto lyra_rt_assertion_control_allowed(const void* control) -> std::uint64_t {
  return static_cast<const AssertionControl*>(control)->Allowed();
}

void lyra_rt_assertion_control_submit_observed(
    const void* control, void* body, const void* location) {
  static_cast<const AssertionControl*>(control)->SubmitObserved(
      std::move(*static_cast<Coroutine<void>*>(body)),
      SourceLocationAt(location));
}

void lyra_rt_assertion_control_submit_final(const void* control, void* body) {
  static_cast<const AssertionControl*>(control)->SubmitFinal(
      std::move(*static_cast<Coroutine<void>*>(body)));
}

void lyra_rt_assert_control_of_design(
    void* runtime, std::int64_t control_type, std::int64_t types,
    std::int64_t directives, std::int64_t levels, const void* location) {
  static_cast<RuntimeEffects*>(runtime)->AssertControlOfDesign(
      control_type, types, directives, levels, SourceLocationAt(location));
}

void lyra_rt_assert_control_of_scope(
    void* runtime, void* scope, std::int64_t control_type, std::int64_t types,
    std::int64_t directives, std::int64_t levels, const void* location) {
  static_cast<RuntimeEffects*>(runtime)->AssertControlOfScope(
      static_cast<Scope*>(scope), control_type, types, directives, levels,
      SourceLocationAt(location));
}

void lyra_rt_assert_control_of_block(
    void* runtime, const void* block, std::int64_t control_type,
    std::int64_t types, std::int64_t directives, const void* location) {
  static_cast<RuntimeEffects*>(runtime)->AssertControlOfBlock(
      static_cast<const CancellationTarget*>(block), control_type, types,
      directives, SourceLocationAt(location));
}

void lyra_rt_assert_control_of_loop(
    void* runtime, const void* blocks, std::int64_t control_type,
    std::int64_t types, std::int64_t directives, std::int64_t levels,
    const void* location) {
  static_cast<RuntimeEffects*>(runtime)->AssertControlOfLoop(
      static_cast<const LoopBlocks*>(blocks), control_type, types, directives,
      levels, SourceLocationAt(location));
}

auto lyra_rt_delay(
    void* runtime, const void* duration, std::int64_t duration_width,
    bool duration_is_signed, bool duration_is_four_state,
    std::int64_t unit_power, std::int64_t precision_power, void* out) -> void* {
  const lyra::value::LoadedWords amount = NumberAt(
      duration, duration_width, duration_is_signed, duration_is_four_state);
  return Emplace(
      out, Delay(
               *static_cast<RuntimeEffects*>(runtime), amount.View(),
               unit_power, precision_power));
}

auto lyra_rt_delay_real(
    void* runtime, const void* duration, std::int64_t unit_power,
    std::int64_t precision_power, void* out) -> void* {
  return Emplace(
      out, DelayReal(
               *static_cast<RuntimeEffects*>(runtime), Read<Real>(duration),
               unit_power, precision_power));
}

auto lyra_rt_delay_ticks(
    const void* duration, std::int64_t duration_width, bool duration_is_signed,
    bool duration_is_four_state, std::int64_t unit_power,
    std::int64_t precision_power) -> std::uint64_t {
  const lyra::value::LoadedWords amount = NumberAt(
      duration, duration_width, duration_is_signed, duration_is_four_state);
  return DelayTicks(
      amount.View(), static_cast<std::int8_t>(unit_power),
      static_cast<std::int8_t>(precision_power));
}

auto lyra_rt_delay_ticks_real(
    const void* duration, std::int64_t unit_power, std::int64_t precision_power)
    -> std::uint64_t {
  return DelayTicksReal(
      Read<Real>(duration), static_cast<std::int8_t>(unit_power),
      static_cast<std::int8_t>(precision_power));
}

auto lyra_rt_scalar_propagation_delay(
    const void* value, std::int64_t value_width, bool value_is_four_state,
    std::uint64_t rise, std::uint64_t fall, std::uint64_t turn_off)
    -> std::uint64_t {
  const lyra::value::LoadedWords changed_to =
      NumberAt(value, value_width, false, value_is_four_state);
  return ScalarPropagationDelay(changed_to.View(), rise, fall, turn_off);
}

auto lyra_rt_vector_propagation_delay(
    const void* value, std::int64_t value_width, bool value_is_four_state,
    std::uint64_t rise, std::uint64_t fall, std::uint64_t turn_off)
    -> std::uint64_t {
  const lyra::value::LoadedWords changed_to =
      NumberAt(value, value_width, false, value_is_four_state);
  return VectorPropagationDelay(changed_to.View(), rise, fall, turn_off);
}

void lyra_rt_timer_bind(void* timer, void* closure) {
  static_cast<Timer*>(timer)->Bind(
      AdoptClosure<lyra::support::EffectSignature>(closure));
}

void lyra_rt_timer_arm(
    void* timer, void* runtime, std::uint64_t ticks,
    std::int64_t precision_power) {
  static_cast<Timer*>(timer)->Arm(
      *static_cast<RuntimeEffects*>(runtime), ticks, precision_power);
}

// What crosses is the place's one word, which the wrapper the place is reached
// through answered.
auto lyra_rt_trigger_make(
    void* place, const void* observation, void* steps,
    std::uint64_t steps_count, std::int64_t lsb_bit_offset,
    std::int64_t bit_width, void* out) -> void* {
  return Emplace(
      out, Trigger(
               WatchedPlace::FromWord(place), Read<Observation>(observation),
               SpanAt<const std::int64_t>(steps, steps_count), lsb_bit_offset,
               bit_width));
}

auto lyra_rt_observation_on_reaching(void* out) -> void* {
  return Emplace(out, Observation::OnReaching());
}

auto lyra_rt_observation_of_watch(void* watch, void* out) -> void* {
  return Emplace(
      out,
      Observation::OfWatch(AdoptClosure<lyra::support::WatchSignature>(watch)));
}

auto lyra_rt_observation_of_watch_qualified(
    void* watch, void* condition, void* out) -> void* {
  return Emplace(
      out, Observation::OfWatchQualified(
               AdoptClosure<lyra::support::WatchSignature>(watch),
               AdoptClosure<lyra::support::ConditionSignature>(condition)));
}

auto lyra_rt_observation_fires(const void* observation) -> std::int64_t {
  return static_cast<const Observation*>(observation)->Fires() ? 1 : 0;
}

auto lyra_rt_wait_recollecting(
    void* reports, std::uint64_t reports_count, void* observations,
    std::uint64_t observations_count, void* out) -> void* {
  return Emplace(
      out,
      WaitRecollecting(
          SpanAt<ReadReport* const>(reports, reports_count),
          SpanAt<const Observation* const>(observations, observations_count)));
}

auto lyra_rt_wait_until(void* reports, std::uint64_t reports_count, void* out)
    -> void* {
  return Emplace(
      out, WaitUntil(SpanAt<ReadReport* const>(reports, reports_count)));
}

auto lyra_rt_wait_on(void* triggers, std::uint64_t triggers_count, void* out)
    -> void* {
  return Emplace(
      out, WaitOn(SpanAt<const Trigger* const>(triggers, triggers_count)));
}

auto lyra_rt_wait_per_change_on_implicit_list(const void* report, void* out)
    -> void* {
  return Emplace(
      out, WaitPerChangeOnImplicitList(static_cast<const ReadReport*>(report)));
}

auto lyra_rt_wait_per_interval_on_implicit_list(const void* report, void* out)
    -> void* {
  return Emplace(
      out,
      WaitPerIntervalOnImplicitList(static_cast<const ReadReport*>(report)));
}

auto lyra_rt_wait_continuously_on(
    void* triggers, std::uint64_t triggers_count, void* out) -> void* {
  return Emplace(
      out, WaitContinuouslyOn(
               SpanAt<const Trigger* const>(triggers, triggers_count)));
}

auto lyra_rt_wait_on_property_evaluation(
    void* runtime, void* clock, std::uint64_t clock_count, void* disable_reads,
    std::uint64_t disable_reads_count, void* start, std::uint64_t start_count,
    const void* control, void* out) -> void* {
  return Emplace(
      out, WaitOnPropertyEvaluation(
               *static_cast<RuntimeEffects*>(runtime),
               SpanAt<const Trigger* const>(clock, clock_count),
               SpanAt<const Trigger* const>(disable_reads, disable_reads_count),
               SpanAt<const std::uint64_t>(start, start_count),
               static_cast<const AssertionControl*>(control)));
}

auto lyra_rt_attempts_of(void* wait) -> void* {
  return AttemptsOf(static_cast<Wait*>(wait));
}

void lyra_rt_bind_property_evaluation(void* wait, void* step, void* closure) {
  BindPropertyEvaluation(
      static_cast<Wait*>(wait),
      AdoptClosure<lyra::support::EffectSignature>(step),
      lyra::runtime::EvaluationAttempt::Closure(
          static_cast<lyra::runtime::ClosureValue*>(closure)));
}

auto lyra_rt_park_at(void* runtime, void* wait) -> bool {
  return ParkAt(
      *static_cast<RuntimeEffects*>(runtime), static_cast<Wait*>(wait));
}

auto lyra_rt_read_report_empty(void* out) -> void* {
  return Emplace(out, ReadReport::Empty());
}

auto lyra_rt_read_report_for_implicit_list(void* out) -> void* {
  return Emplace(out, ReadReport::ForImplicitList());
}

void lyra_rt_read_report_add(
    void* report, void* place, void* steps, std::uint64_t steps_count,
    std::int64_t lsb_bit_offset, std::int64_t bit_width) noexcept {
  static_cast<ReadReport*>(report)->Add(
      WatchedPlace::FromWord(place),
      SpanAt<const std::int64_t>(steps, steps_count), lsb_bit_offset,
      bit_width);
}

void lyra_rt_read_report_add_object(void* report, void* object) {
  static_cast<ReadReport*>(report)->AddObject(static_cast<GcObject*>(object));
}

void lyra_rt_read_report_add_write(
    void* report, void* place, void* steps, std::uint64_t steps_count,
    std::int64_t lsb_bit_offset, std::int64_t bit_width) noexcept {
  static_cast<ReadReport*>(report)->AddWrite(
      WatchedPlace::FromWord(place),
      SpanAt<const std::int64_t>(steps, steps_count), lsb_bit_offset,
      bit_width);
}

void lyra_rt_read_report_settle_as_implicit_list(void* report) {
  static_cast<ReadReport*>(report)->SettleAsImplicitList();
}

auto lyra_rt_read_report_enter(void* report) noexcept -> std::int64_t {
  return static_cast<ReadReport*>(report)->Enter();
}

void lyra_rt_read_report_leave(void* report) noexcept {
  static_cast<ReadReport*>(report)->Leave();
}

auto lyra_rt_read_report_runs_the_body(const void* report) noexcept
    -> std::int64_t {
  return static_cast<const ReadReport*>(report)->RunsTheBody();
}

void lyra_rt_read_report_begin_evaluation(void* report, void* runtime) {
  static_cast<ReadReport*>(report)->BeginEvaluation(
      *static_cast<RuntimeEffects*>(runtime));
}

void lyra_rt_read_report_end_evaluation(void* report, void* runtime) noexcept {
  static_cast<ReadReport*>(report)->EndEvaluation(
      *static_cast<RuntimeEffects*>(runtime));
}

auto lyra_rt_evaluation_in_progress(void* runtime) -> void* {
  return lyra::runtime::EvaluationInProgress(
      *static_cast<RuntimeEffects*>(runtime));
}

void lyra_rt_refuse_report(const void* why, const void* location) {
  RefuseReport(static_cast<const char*>(why), SourceLocationAt(location));
}

auto lyra_rt_resume_in_nba_region(void* out) -> void* {
  return Emplace(out, ResumeInNbaRegion());
}

auto lyra_rt_resume_in_region_after(
    void* runtime, std::int64_t region, const void* duration,
    std::int64_t duration_width, bool duration_is_signed,
    bool duration_is_four_state, std::int64_t unit_power,
    std::int64_t precision_power, void* out) -> void* {
  const lyra::value::LoadedWords amount = NumberAt(
      duration, duration_width, duration_is_signed, duration_is_four_state);
  return Emplace(
      out, ResumeInRegionAfter(
               *static_cast<RuntimeEffects*>(runtime), region, amount.View(),
               unit_power, precision_power));
}

auto lyra_rt_resume_in_region_after_real(
    void* runtime, std::int64_t region, const void* duration,
    std::int64_t unit_power, std::int64_t precision_power, void* out) -> void* {
  return Emplace(
      out, ResumeInRegionAfterReal(
               *static_cast<RuntimeEffects*>(runtime), region,
               Read<Real>(duration), unit_power, precision_power));
}

auto lyra_rt_event_new(void* out) -> void* {
  return Emplace(out, lyra::runtime::EventNew());
}

void lyra_rt_trigger(const void* event, void* runtime) {
  lyra::runtime::EventTrigger(
      Read<ObjectRef>(event), *static_cast<RuntimeEffects*>(runtime));
}

auto lyra_rt_triggered(
    const void* event, void* runtime, void* evaluation, void* out) -> void* {
  return Emplace(
      out, lyra::runtime::EventTriggered(
               Read<ObjectRef>(event), *static_cast<RuntimeEffects*>(runtime),
               static_cast<ReadReport*>(evaluation)));
}

auto lyra_rt_event_occurrences(const void* event, void* evaluation)
    -> std::int64_t {
  return lyra::runtime::EventOccurrences(
      Read<ObjectRef>(event), static_cast<ReadReport*>(evaluation));
}

auto lyra_rt_event_reports_to(const void* event) -> void* {
  return lyra::runtime::EventReportsTo(Read<ObjectRef>(event)).Word();
}

auto lyra_rt_receive_departure() -> void* {
  // Asked inside a handler that has begun handling what arrived, as a C++
  // handler asks it; what is not the design's leaves from inside the question.
  return lyra::runtime::ReceiveDeparture().target;
}

void lyra_rt_decline_departure(void* target) {
  // Carries on the departure the handler holds, which is not always what the
  // unwinder brought: an error it received is carried on as the departure it
  // became, so no later handler receives the error a second time.
  RaiseDeclinedDeparture(
      ControlEffect{.target = static_cast<CancellationTarget*>(target)});
}

void lyra_rt_settle_departure(void* target) {
  // Settles the departure the handler holds rather than what the unwinder
  // brought, for the reason declining carries that one on.
  GeneratedCallScope::Current().SettleDeparture(
      std::make_exception_ptr(
          ControlEffect{.target = static_cast<CancellationTarget*>(target)}));
}

void lyra_rt_enter_target(void* runtime, void* target) {
  EnterCancellationTarget(
      *static_cast<RuntimeEffects*>(runtime),
      static_cast<CancellationTarget*>(target));
}

void lyra_rt_leave_target(void* runtime, void* target) noexcept {
  LeaveCancellationTarget(
      *static_cast<RuntimeEffects*>(runtime),
      static_cast<CancellationTarget*>(target));
}

void lyra_rt_disable(void* target, void* runtime) {
  Disable(
      static_cast<CancellationTarget*>(target),
      *static_cast<RuntimeEffects*>(runtime));
}

auto lyra_rt_region_lands_effect(
    void* runtime, void* effect, void* target, void* out) noexcept -> void* {
  // A control effect crosses as the target it names, since that is all one
  // carries.
  return Emplace(
      out,
      lyra::value::Bit::FromBool(RegionLandsEffect(
          *static_cast<RuntimeEffects*>(runtime),
          ControlEffect{.target = static_cast<CancellationTarget*>(effect)},
          static_cast<CancellationTarget*>(target))));
}

void lyra_rt_take_departure_if_due(void* runtime) {
  // A foreign call made before any procedure starts runs with no process at all
  // (LRM 10.5, 26.2), and a point one returns through is not an execution that
  // could have been disabled or terminated, so nothing is owed there.
  TakeDepartureIfDue(*static_cast<RuntimeEffects*>(runtime));
}

auto lyra_rt_sim_time(void* runtime, std::int64_t unit_power, void* out)
    -> void* {
  return Emplace(
      out, SimTimeInUnit(*static_cast<RuntimeEffects*>(runtime), unit_power));
}

auto lyra_rt_stime(void* runtime, std::int64_t unit_power, void* out) -> void* {
  return Emplace(
      out, STimeInUnit(*static_cast<RuntimeEffects*>(runtime), unit_power));
}

auto lyra_rt_realtime(void* runtime, std::int64_t unit_power, void* out)
    -> void* {
  return Emplace(
      out, RealTimeInUnit(*static_cast<RuntimeEffects*>(runtime), unit_power));
}

void lyra_rt_finish(
    void* runtime, std::int64_t finish_number, const void* location) {
  Finish(
      *static_cast<RuntimeEffects*>(runtime), finish_number,
      SourceLocationAt(location));
}

void lyra_rt_stop(
    void* runtime, std::int64_t finish_number, const void* location) {
  Stop(
      *static_cast<RuntimeEffects*>(runtime), finish_number,
      SourceLocationAt(location));
}

void lyra_rt_exit(void* runtime, const void* location) {
  Exit(*static_cast<RuntimeEffects*>(runtime), SourceLocationAt(location));
}

auto lyra_rt_run_host_command(void* runtime, const void* command, void* out)
    -> void* {
  return Emplace(
      out, RunHostCommand(
               *static_cast<RuntimeEffects*>(runtime), Read<String>(command)));
}

auto lyra_rt_run_null_host_command(void* out) -> void* {
  return Emplace(out, RunNullHostCommand());
}

void lyra_rt_dump_file(void* runtime, const void* name, const void* location) {
  DumpFile(
      *static_cast<RuntimeEffects*>(runtime), Read<String>(name),
      SourceLocationAt(location));
}
void lyra_rt_dump_vars_levels(
    void* runtime, std::int64_t levels, const void* location) {
  DumpVarsLevels(
      *static_cast<RuntimeEffects*>(runtime), levels,
      SourceLocationAt(location));
}
void lyra_rt_dump_vars_of(
    void* runtime, std::int64_t levels, void* instance, const void* location) {
  DumpVarsOf(
      *static_cast<RuntimeEffects*>(runtime), levels,
      static_cast<Scope*>(instance), SourceLocationAt(location));
}
void lyra_rt_dump_vars_of_variable(
    void* runtime, const void* variable, const void* location) {
  DumpVarsOfVariable(
      *static_cast<RuntimeEffects*>(runtime),
      *static_cast<const lyra::runtime::Trigger*>(variable),
      SourceLocationAt(location));
}
void lyra_rt_dump_off(void* runtime) {
  DumpOff(*static_cast<RuntimeEffects*>(runtime));
}
void lyra_rt_dump_on(void* runtime) {
  DumpOn(*static_cast<RuntimeEffects*>(runtime));
}
void lyra_rt_dump_all(void* runtime) {
  DumpAll(*static_cast<RuntimeEffects*>(runtime));
}
void lyra_rt_dump_limit(void* runtime, std::int64_t bytes) {
  DumpLimit(*static_cast<RuntimeEffects*>(runtime), bytes);
}
void lyra_rt_dump_flush(void* runtime) {
  DumpFlush(*static_cast<RuntimeEffects*>(runtime));
}
void lyra_rt_dump_ports_of(
    void* runtime, void* instance, const void* name, const void* location) {
  DumpPortsOf(
      *static_cast<RuntimeEffects*>(runtime), static_cast<Scope*>(instance),
      Read<String>(name), SourceLocationAt(location));
}
void lyra_rt_dump_ports_off(void* runtime, const void* name) {
  DumpPortsOff(*static_cast<RuntimeEffects*>(runtime), Read<String>(name));
}
void lyra_rt_dump_ports_on(void* runtime, const void* name) {
  DumpPortsOn(*static_cast<RuntimeEffects*>(runtime), Read<String>(name));
}
void lyra_rt_dump_ports_all(void* runtime, const void* name) {
  DumpPortsAll(*static_cast<RuntimeEffects*>(runtime), Read<String>(name));
}
void lyra_rt_dump_ports_limit(
    void* runtime, std::int64_t bytes, const void* name) {
  DumpPortsLimit(
      *static_cast<RuntimeEffects*>(runtime), bytes, Read<String>(name));
}
void lyra_rt_dump_ports_flush(void* runtime, const void* name) {
  DumpPortsFlush(*static_cast<RuntimeEffects*>(runtime), Read<String>(name));
}

void lyra_rt_value_serializer_bits(
    void* serializer, const void* bits, std::int64_t bits_width,
    bool bits_are_four_state) {
  static_cast<lyra::runtime::ValueSerializer*>(serializer)
      ->SerializeBits(BitsAt(bits, bits_width, bits_are_four_state).View());
}

void lyra_rt_real_value_serializer_real(void* serializer, const void* value) {
  static_cast<lyra::runtime::ValueSerializer*>(serializer)
      ->SerializeReal(*static_cast<const double*>(value));
}

void lyra_rt_shortreal_value_serializer_real(
    void* serializer, const void* value) {
  static_cast<lyra::runtime::ValueSerializer*>(serializer)
      ->SerializeReal(*static_cast<const float*>(value));
}

void lyra_rt_value_serializer_text(void* serializer, const void* text) {
  static_cast<lyra::runtime::ValueSerializer*>(serializer)
      ->SerializeText(Read<String>(text));
}

void lyra_rt_value_serializer_object(void* serializer, const void* object) {
  static_cast<lyra::runtime::ValueSerializer*>(serializer)
      ->SerializeObject(Read<ObjectRef>(object));
}

auto lyra_rt_value_serializer_size(void* serializer, std::int64_t size)
    -> bool {
  return static_cast<lyra::runtime::ValueSerializer*>(serializer)
      ->SerializeSize(size);
}

void lyra_rt_value_serializer_key(
    void* serializer, const void* key, const void* key_type) {
  static_cast<lyra::runtime::ValueSerializer*>(serializer)
      ->SerializeKey(key, lyra::runtime::TypeAt(key_type));
}

auto lyra_rt_selection_depth(const void* selection) -> std::int64_t {
  return static_cast<const lyra::runtime::Selection*>(selection)->Depth();
}

auto lyra_rt_selection_number(const void* selection, std::int64_t depth)
    -> std::int64_t {
  return static_cast<const lyra::runtime::Selection*>(selection)->Number(depth);
}

auto lyra_rt_selection_key(
    const void* selection, std::int64_t depth, const void* answer_type,
    void* out) -> void* {
  const lyra::value::AnyValue key =
      static_cast<const lyra::runtime::Selection*>(selection)->KeyAt(
          depth, lyra::runtime::TypeAt(answer_type));
  key.Type().Copy(key.Bytes(), out);
  return out;
}

void lyra_rt_selection_settler_settle_key(
    void* settler, std::int64_t depth, const void* key, const void* key_type) {
  static_cast<lyra::runtime::SelectionSettler*>(settler)->SettleKey(
      depth, key, lyra::runtime::TypeAt(key_type));
}

void lyra_rt_selection_settler_settle_positions(
    void* settler, std::int64_t before) {
  static_cast<lyra::runtime::SelectionSettler*>(settler)->SettlePositions(
      before);
}

void lyra_rt_selection_settler_settle_unfixed(void* settler) {
  static_cast<lyra::runtime::SelectionSettler*>(settler)->SettleUnfixed();
}

void lyra_rt_selection_settler_settle_watched_step(
    void* settler, std::int64_t step) {
  static_cast<lyra::runtime::SelectionSettler*>(settler)->SettleWatchedStep(
      step);
}

void lyra_rt_selection_settler_settle_unwatched(void* settler) {
  static_cast<lyra::runtime::SelectionSettler*>(settler)->SettleUnwatched();
}

auto lyra_rt_selection_settler_fixed(void* settler) -> bool {
  return static_cast<lyra::runtime::SelectionSettler*>(settler)->Fixed();
}

auto lyra_rt_selection_settler_positions(void* settler) -> std::int64_t {
  return static_cast<lyra::runtime::SelectionSettler*>(settler)->Positions();
}

auto lyra_rt_value_deserializer_bits(void* deserializer, void* out) -> void* {
  return Emplace(
      out,
      static_cast<lyra::runtime::ValueDeserializer*>(deserializer)->Bits());
}

auto lyra_rt_value_deserializer_real(void* deserializer, void* out) -> void* {
  return Emplace(
      out,
      static_cast<lyra::runtime::ValueDeserializer*>(deserializer)->Real());
}

auto lyra_rt_value_deserializer_text(void* deserializer, void* out) -> void* {
  return Emplace(
      out,
      static_cast<lyra::runtime::ValueDeserializer*>(deserializer)->Text());
}

void lyra_rt_subscriber_subscribe(void* subscriber, const void* trigger) {
  static_cast<lyra::runtime::Subscriber*>(subscriber)
      ->Subscribe(*static_cast<const lyra::runtime::Trigger*>(trigger));
}

auto lyra_rt_test_plusargs(void* runtime, const void* user_string, void* out)
    -> void* {
  return Emplace(
      out,
      TestPlusargs(
          *static_cast<RuntimeEffects*>(runtime), Read<String>(user_string)));
}

auto lyra_rt_integral_value_plusargs(
    void* runtime, const void* user_string, const void* destination,
    std::int64_t destination_width, bool destination_is_signed,
    bool destination_is_four_state, const void* answer_type, void* out)
    -> void* {
  lyra::value::WrittenWords written = lyra::value::WrittenWords::CopyOf(
      destination,
      lyra::runtime::NumberShape(
          destination_width, destination_is_signed, destination_is_four_state));
  const lyra::value::Int matched = lyra::runtime::ValuePlusargsInto(
      *static_cast<RuntimeEffects*>(runtime), Read<String>(user_string),
      written.MutableView());
  return lyra::runtime::EmplaceWithWords(
      out, lyra::runtime::TupleTypeAt(answer_type), matched, written);
}

auto lyra_rt_string_value_plusargs(
    void* runtime, const void* user_string, const void* destination,
    const void* answer_type, void* out) -> void* {
  return lyra::runtime::EmplaceBoth(
      out, lyra::runtime::TupleTypeAt(answer_type),
      ValuePlusargs(
          *static_cast<RuntimeEffects*>(runtime), Read<String>(user_string),
          Read<String>(destination)));
}

auto lyra_rt_urandom(void* runtime, void* out) -> void* {
  return Emplace(
      out, lyra::runtime::Urandom(*static_cast<RuntimeEffects*>(runtime)));
}

auto lyra_rt_urandom_seeded(void* runtime, std::int64_t seed, void* out)
    -> void* {
  return Emplace(
      out, lyra::runtime::UrandomSeeded(
               *static_cast<RuntimeEffects*>(runtime), seed));
}

auto lyra_rt_urandom_range(
    void* runtime, std::int64_t maxval, std::int64_t minval, void* out)
    -> void* {
  return Emplace(
      out, lyra::runtime::UrandomRange(
               *static_cast<RuntimeEffects*>(runtime), maxval, minval));
}

auto lyra_rt_random(void* runtime, void* out) -> void* {
  return Emplace(
      out, lyra::runtime::Random(*static_cast<RuntimeEffects*>(runtime)));
}

auto lyra_rt_dist_uniform(
    std::int64_t seed, std::int64_t start, std::int64_t end,
    const void* answer_type, void* out) -> void* {
  return lyra::runtime::EmplaceBoth(
      out, lyra::runtime::TupleTypeAt(answer_type),
      lyra::runtime::DistUniform(seed, start, end));
}

auto lyra_rt_dist_normal(
    std::int64_t seed, std::int64_t mean, std::int64_t standard_deviation,
    const void* answer_type, void* out) -> void* {
  return lyra::runtime::EmplaceBoth(
      out, lyra::runtime::TupleTypeAt(answer_type),
      lyra::runtime::DistNormal(seed, mean, standard_deviation));
}

auto lyra_rt_dist_exponential(
    std::int64_t seed, std::int64_t mean, const void* location,
    const void* answer_type, void* out) -> void* {
  return lyra::runtime::EmplaceBoth(
      out, lyra::runtime::TupleTypeAt(answer_type),
      lyra::runtime::DistExponential(seed, mean, SourceLocationAt(location)));
}

auto lyra_rt_dist_poisson(
    std::int64_t seed, std::int64_t mean, const void* location,
    const void* answer_type, void* out) -> void* {
  return lyra::runtime::EmplaceBoth(
      out, lyra::runtime::TupleTypeAt(answer_type),
      lyra::runtime::DistPoisson(seed, mean, SourceLocationAt(location)));
}

auto lyra_rt_dist_chi_square(
    std::int64_t seed, std::int64_t degrees_of_freedom, const void* location,
    const void* answer_type, void* out) -> void* {
  return lyra::runtime::EmplaceBoth(
      out, lyra::runtime::TupleTypeAt(answer_type),
      lyra::runtime::DistChiSquare(
          seed, degrees_of_freedom, SourceLocationAt(location)));
}

auto lyra_rt_dist_t(
    std::int64_t seed, std::int64_t degrees_of_freedom, const void* location,
    const void* answer_type, void* out) -> void* {
  return lyra::runtime::EmplaceBoth(
      out, lyra::runtime::TupleTypeAt(answer_type),
      lyra::runtime::DistT(
          seed, degrees_of_freedom, SourceLocationAt(location)));
}

auto lyra_rt_dist_erlang(
    std::int64_t seed, std::int64_t stages, std::int64_t mean,
    const void* location, const void* answer_type, void* out) -> void* {
  return lyra::runtime::EmplaceBoth(
      out, lyra::runtime::TupleTypeAt(answer_type),
      lyra::runtime::DistErlang(
          seed, stages, mean, SourceLocationAt(location)));
}

void lyra_rt_register_coroutine(
    void* self, void* unit_instance, void* runs_as, const void* definition,
    void* coroutine) {
  RegisterCoroutine(
      static_cast<Scope*>(self), static_cast<Scope*>(unit_instance),
      static_cast<Scope*>(runs_as),
      static_cast<const lyra::abi::ProcessDefinition*>(definition),
      std::move(*static_cast<Coroutine<void>*>(coroutine)));
}

void lyra_rt_register_reaction(
    void* self, void* unit_instance, void* runs_as, const void* definition,
    const void* reaction, void* object) {
  RegisterReaction(
      static_cast<Scope*>(self), static_cast<Scope*>(unit_instance),
      static_cast<Scope*>(runs_as),
      static_cast<const lyra::abi::ProcessDefinition*>(definition),
      static_cast<const lyra::abi::ReactionDefinition*>(reaction), object);
}

void lyra_rt_register_final(
    void* self, void* unit_instance, void* closure, const void* written_at) {
  RegisterFinalProcedure(
      static_cast<Scope*>(self), static_cast<Scope*>(unit_instance),
      AdoptClosure<lyra::support::EffectSignature>(closure),
      SourceLocationAt(written_at));
}

void lyra_rt_run_assertion_action(
    void* self, void* unit_instance, void* coroutine, const void* written_at) {
  RunAssertionAction(
      static_cast<Scope*>(self), static_cast<Scope*>(unit_instance),
      std::move(*static_cast<Coroutine<void>*>(coroutine)),
      SourceLocationAt(written_at));
}

void lyra_rt_enter_scope_static_init(void* runtime, void* unit_instance) {
  EnterScopeStaticInit(
      *static_cast<RuntimeEffects*>(runtime),
      static_cast<Scope*>(unit_instance));
}

void lyra_rt_enter_namespace_static_init(void* runtime, const char* name) {
  EnterNamespaceStaticInit(*static_cast<RuntimeEffects*>(runtime), name);
}

void lyra_rt_leave_static_init(void* runtime) noexcept {
  LeaveStaticInit(*static_cast<RuntimeEffects*>(runtime));
}

void lyra_rt_enter_dpi_scope(
    void* runtime, void* decl_scope, const void* location) {
  EnterDpiScope(
      *static_cast<RuntimeEffects*>(runtime), static_cast<Scope*>(decl_scope),
      SourceLocationAt(location));
}

void lyra_rt_leave_dpi_scope(void* runtime) noexcept {
  LeaveDpiScope(*static_cast<RuntimeEffects*>(runtime));
}

auto lyra_rt_exported_task_return_value(void* runtime) -> std::int32_t {
  return ExportedTaskReturnValue(*static_cast<RuntimeEffects*>(runtime));
}

void lyra_rt_context_import_task_returned(
    void* runtime, std::int32_t returned, const void* location) {
  ContextImportTaskReturned(
      *static_cast<RuntimeEffects*>(runtime), returned,
      SourceLocationAt(location));
}

void lyra_rt_context_import_function_returned(
    void* runtime, const void* location) {
  ContextImportFunctionReturned(
      *static_cast<RuntimeEffects*>(runtime), SourceLocationAt(location));
}

void lyra_rt_check_export_reachable(void* runtime) {
  CheckExportReachable(*static_cast<RuntimeEffects*>(runtime));
}

void lyra_rt_before_foreign_code_runs(void* runtime) noexcept {
  BeforeForeignCodeRuns(*static_cast<RuntimeEffects*>(runtime));
}

auto lyra_rt_hand_string_to_foreign_caller(void* runtime, const void* text)
    -> const char* {
  return HandStringToForeignCaller(
      *static_cast<RuntimeEffects*>(runtime), Read<String>(text));
}

auto lyra_rt_claim_namespace_initialize(void* runtime, const char* name)
    -> std::int64_t {
  return ClaimNamespaceInitialization(
      *static_cast<RuntimeEffects*>(runtime), name);
}

void lyra_rt_register_namespace(void* runtime, const void* scope_declarations) {
  RegisterNamespace(
      *static_cast<RuntimeEffects*>(runtime),
      static_cast<const ScopeDeclarations*>(scope_declarations));
}

auto lyra_rt_current_export_scope() -> void* {
  return CurrentExportScope();
}

auto lyra_rt_find_export_entry(void* scope, const void* subroutine)
    -> LyraMethodEntry {
  return FindExportEntry(
      static_cast<Scope*>(scope), static_cast<const char*>(subroutine));
}

auto lyra_rt_run_foreign_task_on_fiber(void* runtime, void* closure) -> bool {
  return lyra::runtime::RunForeignTaskOnFiber(
      *static_cast<RuntimeEffects*>(runtime),
      AdoptClosure<lyra::support::EffectSignature>(closure));
}

void lyra_rt_run_exported_task_to_completion(void* activation) {
  // A stop this thread makes parks its innermost activation, and the body
  // reached here is one: it runs in the thread that entered the foreign call
  // (LRM 9.5), so entering it is what makes a delay inside it park the right
  // frame rather than the one that called out.
  CoroutineThread& thread = CurrentForeignThread();
  FrameActivation* const called = thread.PushActivation(
      std::move(*static_cast<Coroutine<void>*>(activation)));
  DriveOnForeignStack(*called);
  // A run-time error that left the body was stored rather than allowed to
  // travel through the frames that drove it, so it travels on from here, where
  // the entry's own region receives it before it can reach the foreign frame
  // above. Taken before the activation is released, which destroys what holds
  // it.
  std::exception_ptr raised = thread.TakeInnermostRaisedError();
  thread.PopActivation();
  if (raised) {
    std::rethrow_exception(raised);
  }
}

auto lyra_rt_hierarchy_segment_make(
    const void* declaration, void* indices, std::uint64_t indices_count,
    void* out) -> void* {
  return std::construct_at(
      static_cast<HierarchySegment*>(out),
      static_cast<const ElaboratedScopeRecord*>(declaration),
      SpanAt<const std::int64_t>(indices, indices_count));
}

auto lyra_rt_hierarchical_path_of(void* self, const void* below, void* out)
    -> void* {
  return Emplace(
      out, static_cast<Scope*>(self)->HierarchicalPathOf(Read<String>(below)));
}

void lyra_rt_set_scope_place(void* self, void* parent, const void* segment) {
  static_cast<Scope*>(self)->SetPlace(
      static_cast<Scope*>(parent), Read<HierarchySegment>(segment));
}

auto lyra_rt_attach_scope(void* child) -> void* {
  return Scope::Attach(
      lyra::runtime::OwnedObject<Scope>(static_cast<Scope*>(child)));
}

void lyra_rt_extract_bits(
    const void* whole, std::int64_t whole_width, bool whole_is_four_state,
    void* outer_parts, std::uint64_t outer_parts_count, const void* start,
    void* bits, std::int64_t bits_width, bool bits_are_four_state) {
  lyra::runtime::ExtractBitsAt(
      whole, whole_width, whole_is_four_state, outer_parts, outer_parts_count,
      start, bits, bits_width, bits_are_four_state);
}

void lyra_rt_insert_bits(
    void* whole, std::int64_t whole_width, bool whole_is_four_state,
    void* outer_parts, std::uint64_t outer_parts_count, const void* start,
    const void* bits, std::int64_t bits_width, bool bits_are_four_state) {
  lyra::runtime::InsertBitsAt(
      whole, whole_width, whole_is_four_state, outer_parts, outer_parts_count,
      start, bits, bits_width, bits_are_four_state);
}

auto lyra_rt_enclosing_scope(void* self, const void* definition) -> void* {
  return static_cast<Scope*>(self)->EnclosingScope(
      static_cast<const ObjectDefinition*>(definition));
}

void lyra_rt_loop_blocks_add(void* blocks, std::int64_t index, void* block) {
  static_cast<LoopBlocks*>(blocks)->Add(index, static_cast<Scope*>(block));
}

auto lyra_rt_loop_blocks_at(const void* blocks, std::int64_t index) -> void* {
  return static_cast<const LoopBlocks*>(blocks)->At(index);
}

auto lyra_rt_sequence_make(void* handles, std::uint64_t handles_count)
    -> void* {
  const std::span<void* const> raw =
      SpanAt<void* const>(handles, handles_count);
  return ProgramLifetime(std::vector<void*>(raw.begin(), raw.end()));
}

auto lyra_rt_sequence_extend(void* sequence, void* element) -> void* {
  static_cast<std::vector<void*>*>(sequence)->push_back(element);
  return sequence;
}

auto lyra_rt_sequence_element(const void* sequence, std::int64_t index)
    -> void* {
  const auto& handles = *static_cast<const std::vector<void*>*>(sequence);
  const auto position = static_cast<std::size_t>(index);
  if (index < 0 || position >= handles.size()) {
    throw lyra::InternalError(
        "lyra_rt_sequence_element: the coordinate names no object the "
        "declaration built");
  }
  return handles[position];
}

auto lyra_rt_object_of(const void* handle, const void* location) -> void* {
  return Read<ObjectRef>(handle).Object<lyra::runtime::GcObject>(
      SourceLocationAt(location));
}

auto lyra_rt_self_handle(void* self, void* out) -> void* {
  return Emplace(out, lyra::runtime::SelfHandle(static_cast<GcObject*>(self)));
}

auto lyra_rt_dynamic_cast(const void* handle, const void* definition, void* out)
    -> void* {
  return Emplace(
      out, lyra::runtime::DynamicCast(
               Read<ObjectRef>(handle),
               static_cast<const ObjectDefinition*>(definition)));
}

auto lyra_rt_is_of_interface_of(
    const void* handle, const void* iface, const void* instance) -> bool {
  return lyra::runtime::IsOfInterfaceOf(
      Read<ObjectRef>(handle), static_cast<const ObjectDefinition*>(iface),
      static_cast<const lyra::runtime::Scope*>(instance));
}

auto lyra_rt_interface_table(const void* object, const void* iface) -> const
    void* {
  return lyra::runtime::InterfaceTableOf(
      static_cast<const GcObject*>(object)->definition,
      static_cast<const ObjectDefinition*>(iface));
}

void lyra_rt_begin_library_part(std::uint8_t which, void* at) {
  lyra::runtime::BeginLibraryPart(
      static_cast<lyra::support::LibraryDecl>(which), at);
}

void lyra_rt_end_library_part(std::uint8_t which, void* at) {
  lyra::runtime::EndLibraryPart(
      static_cast<lyra::support::LibraryDecl>(which), at);
}

auto lyra_rt_object_event_source(void* object) -> void* {
  return EventSourceOf(static_cast<GcObject*>(object));
}

auto lyra_rt_open_object_write(void* object, void* out) -> void* {
  return std::construct_at(
      static_cast<ErasedObjectWrite*>(out), static_cast<GcObject*>(object));
}

auto lyra_rt_written_object(const void* write) -> void* {
  return static_cast<const ErasedObjectWrite*>(write)->Object();
}

auto lyra_rt_enumeration_has(
    const void* enumeration, const void* value, std::int64_t value_width,
    bool value_is_four_state) -> std::int64_t {
  const lyra::value::LoadedWords asked =
      BitsAt(value, value_width, value_is_four_state);
  return Read<Enumeration>(enumeration).PositionOf(asked.View()).has_value()
             ? 1
             : 0;
}

auto lyra_rt_enumeration_name(
    const void* enumeration, const void* value, std::int64_t value_width,
    bool value_is_four_state, void* out) -> void* {
  const lyra::value::LoadedWords asked =
      BitsAt(value, value_width, value_is_four_state);
  return Emplace(out, Read<Enumeration>(enumeration).NameOf(asked.View()));
}

auto lyra_rt_enumeration_next(
    const void* enumeration, const void* value, std::int64_t value_width,
    bool value_is_four_state, std::int64_t count, void* out) -> void* {
  const lyra::value::LoadedWords from =
      BitsAt(value, value_width, value_is_four_state);
  return lyra::runtime::EmplaceEnumerationMember(
      out, from.Shape(),
      Read<Enumeration>(enumeration).MemberAfter(from.View(), count));
}

auto lyra_rt_enumeration_prev(
    const void* enumeration, const void* value, std::int64_t value_width,
    bool value_is_four_state, std::int64_t count, void* out) -> void* {
  const lyra::value::LoadedWords from =
      BitsAt(value, value_width, value_is_four_state);
  return lyra::runtime::EmplaceEnumerationMember(
      out, from.Shape(),
      Read<Enumeration>(enumeration).MemberBefore(from.View(), count));
}

auto lyra_rt_run_program(
    std::int32_t argc, char** argv, void* (*make)(void*, const void*))
    -> std::int32_t {
  return RunDesignRoot(
      argc, argv, [make](Scope* parent, HierarchySegment segment) {
        return lyra::runtime::OwnedObject<Scope>(
            static_cast<Scope*>(make(parent, &segment)));
      });
}

auto lyra_rt_instantiation_entry(const void* table, std::int64_t at)
    -> LyraMethodEntry {
  return InstantiationEntry(static_cast<const ConstructionTable*>(table), at);
}

auto lyra_rt_instantiation_table(const void* table, std::int64_t at) -> const
    void* {
  return InstantiationTable(static_cast<const ConstructionTable*>(table), at);
}

auto lyra_rt_block_entry(
    const void* table, std::int64_t generate, std::int64_t place)
    -> LyraMethodEntry {
  return BlockEntry(
      static_cast<const ConstructionTable*>(table), generate, place);
}

auto lyra_rt_block_table(
    const void* table, std::int64_t generate, std::int64_t place) -> const
    void* {
  return BlockTable(
      static_cast<const ConstructionTable*>(table), generate, place);
}

auto lyra_rt_refer_storage(void* storage, void* out) -> void* {
  return ReferToStorage(storage, out);
}

auto lyra_rt_refer_property(void* object, void* storage, void* out) -> void* {
  return ReferToProperty(object, storage, out);
}

auto lyra_rt_lease_object(const void* reference, void* out) -> void* {
  return std::construct_at(
      static_cast<LeasedReference*>(out), LeaseObject(ErasedAt(reference)));
}

auto lyra_rt_leased_reference(const void* leased, void* out) -> void* {
  return BuildReference(
      out, static_cast<const LeasedReference*>(leased)->Erased());
}

void lyra_rt_bind_member(void* member, const void* bound) {
  lyra::runtime::current_runtime().Bound().Bind(
      *static_cast<ErasedReference*>(member), ErasedAt(bound));
}

void lyra_rt_bind_alias(void* alias, const void* bound) {
  lyra::runtime::current_runtime().Bound().Alias(
      *static_cast<ErasedReference*>(alias), ErasedAt(bound));
}

void lyra_rt_bind_reference(void* member, const void* bound) {
  *static_cast<ErasedReference*>(member) = ErasedAt(bound);
}

void lyra_rt_drives_continuously(const void* reference) {
  ErasedAt(reference).DriveContinuously();
}

auto lyra_rt_string_cell_refer(void* cell, void* out) -> void* {
  return ReferToCell<String>(cell, out);
}
auto lyra_rt_real_cell_refer(void* cell, void* out) -> void* {
  return ReferToCell<Real>(cell, out);
}
auto lyra_rt_shortreal_cell_refer(void* cell, void* out) -> void* {
  return ReferToCell<ShortReal>(cell, out);
}
auto lyra_rt_unmanagedref_cell_refer(void* cell, void* out) -> void* {
  return ReferToCell<UnmanagedRef>(cell, out);
}
auto lyra_rt_managedref_cell_refer(void* cell, void* out) -> void* {
  return ReferToCell<ObjectRef>(cell, out);
}
auto lyra_rt_laidout_cell_refer(void* cell, void* out) -> void* {
  return ReferToCell<LaidOutValue>(cell, out);
}

auto lyra_rt_laidout_refer_element(
    const void* reference, const void* position, const void* location,
    const void* array_type, void* out) -> void* {
  return ReferFixedArrayElement(
      reference, ArrayTypeAt(array_type), position, SourceLocationAt(location),
      out);
}
auto lyra_rt_laidout_dynamic_array_refer_element(
    const void* reference, const void* position, const void* location,
    const void* array_type, void* out) -> void* {
  return ReferDynamicArrayElement(
      reference, DynamicArrayTypeAt(array_type), position,
      SourceLocationAt(location), out);
}
auto lyra_rt_laidout_queue_refer_element(
    const void* reference, const void* position, const void* location,
    const void* array_type, void* out) -> void* {
  return ReferQueueElement(
      reference, QueueTypeAt(array_type), position, SourceLocationAt(location),
      out);
}
auto lyra_rt_laidout_assoc_refer_element(
    const void* reference, const void* index, const void* location,
    const void* array_type, void* out) -> void* {
  return ReferAssociativeArrayElement(
      reference, lyra::runtime::AssociativeArrayTypeAt(array_type), index,
      SourceLocationAt(location), out);
}
auto lyra_rt_laidout_dynamic_array_refer_element_leased(
    const void* reference, const void* position, const void* location,
    const void* array_type, void* out) -> void* {
  return ReferDynamicArrayElementLeased(
      reference, DynamicArrayTypeAt(array_type), position,
      SourceLocationAt(location), out);
}
auto lyra_rt_laidout_queue_refer_element_leased(
    const void* reference, const void* position, const void* location,
    const void* array_type, void* out) -> void* {
  return ReferQueueElementLeased(
      reference, QueueTypeAt(array_type), position, SourceLocationAt(location),
      out);
}
auto lyra_rt_laidout_assoc_refer_element_leased(
    const void* reference, const void* index, const void* location,
    const void* array_type, void* out) -> void* {
  return ReferAssociativeArrayElementLeased(
      reference, lyra::runtime::AssociativeArrayTypeAt(array_type), index,
      SourceLocationAt(location), out);
}
auto lyra_rt_laidout_refer_component(
    const void* reference, std::int64_t index, const void* tuple_type,
    void* out) -> void* {
  const ErasedReference& from = ErasedAt(reference);
  const bool accumulated = false;
  return BuildReference(
      out, from.Part(
               lyra::runtime::TupleTypeAt(tuple_type)
                   .ComponentAt(from.storage, static_cast<std::size_t>(index)),
               lyra::runtime::ElementThere{},
               lyra::runtime::PartPath::Written(
                   lyra::support::PartStep::kComponent, index),
               accumulated));
}

auto lyra_rt_string_ref_get(void* reference) -> const void* {
  return RefGet<String>(reference);
}

void lyra_rt_string_ref_set(void* reference, const void* value) {
  RefSet<String>(reference, value);
}

void lyra_rt_string_ref_arm_sampling(void* reference) {
  RefArmSampling<String>(reference);
}

auto lyra_rt_string_ref_sampled_get(void* reference, const void* location)
    -> const void* {
  return RefSampledGet<String>(reference, SourceLocationAt(location));
}

auto lyra_rt_real_ref_get(void* reference) -> const void* {
  return RefGet<Real>(reference);
}

void lyra_rt_real_ref_set(void* reference, const void* value) {
  RefSet<Real>(reference, value);
}

void lyra_rt_real_ref_arm_sampling(void* reference) {
  RefArmSampling<Real>(reference);
}

auto lyra_rt_real_ref_sampled_get(void* reference, const void* location)
    -> const void* {
  return RefSampledGet<Real>(reference, SourceLocationAt(location));
}

auto lyra_rt_shortreal_ref_get(void* reference) -> const void* {
  return RefGet<ShortReal>(reference);
}

void lyra_rt_shortreal_ref_set(void* reference, const void* value) {
  RefSet<ShortReal>(reference, value);
}

void lyra_rt_shortreal_ref_arm_sampling(void* reference) {
  RefArmSampling<ShortReal>(reference);
}

auto lyra_rt_shortreal_ref_sampled_get(void* reference, const void* location)
    -> const void* {
  return RefSampledGet<ShortReal>(reference, SourceLocationAt(location));
}

auto lyra_rt_unmanagedref_ref_get(void* reference) -> const void* {
  return RefGet<UnmanagedRef>(reference);
}

void lyra_rt_unmanagedref_ref_set(void* reference, const void* value) {
  RefSet<UnmanagedRef>(reference, value);
}

void lyra_rt_unmanagedref_ref_arm_sampling(void* reference) {
  RefArmSampling<UnmanagedRef>(reference);
}

auto lyra_rt_unmanagedref_ref_sampled_get(void* reference, const void* location)
    -> const void* {
  return RefSampledGet<UnmanagedRef>(reference, SourceLocationAt(location));
}

auto lyra_rt_managedref_ref_get(void* reference) -> const void* {
  return RefGet<ObjectRef>(reference);
}

void lyra_rt_managedref_ref_set(void* reference, const void* value) {
  RefSet<ObjectRef>(reference, value);
}

void lyra_rt_managedref_ref_arm_sampling(void* reference) {
  RefArmSampling<ObjectRef>(reference);
}

auto lyra_rt_managedref_ref_sampled_get(void* reference, const void* location)
    -> const void* {
  return RefSampledGet<ObjectRef>(reference, SourceLocationAt(location));
}

auto lyra_rt_laidout_ref_get(void* reference) -> const void* {
  return ErasedAt(reference).storage;
}

void lyra_rt_laidout_ref_set(
    void* reference, const void* value, const void* value_type) {
  LaidOutRefSet(reference, lyra::runtime::TypeAt(value_type), value);
}

void lyra_rt_laidout_ref_arm_sampling(void* reference) {
  ArmWholeVariable<VariableOf<LaidOutValue>::Type>(reference);
}

auto lyra_rt_laidout_ref_sampled_get(void* reference, const void* location)
    -> const void* {
  return TailRefSampledGet<LaidOutValue>(reference, SourceLocationAt(location));
}

auto lyra_rt_string_cell_get(void* cell) -> const void* {
  return &VariableAt<String>(cell).Get();
}

void lyra_rt_string_cell_initialize(
    void* cell, const void* prototype) noexcept {
  VariableAt<String>(cell).Initialize(Read<String>(prototype));
}

void lyra_rt_string_cell_set(void* cell, const void* value) {
  VariableAt<String>(cell).Set(Read<String>(value));
}

void lyra_rt_string_cell_arm_sampling(void* cell) {
  VariableAt<String>(cell).ArmSampling();
}

auto lyra_rt_string_cell_sampled_get(void* cell, const void* location) -> const
    void* {
  return &VariableAt<String>(cell).SampledGet(SourceLocationAt(location));
}

auto lyra_rt_bitvec_cell_get(void* cell) -> const void* {
  return &VariableAt<BitVec>(cell).Get();
}

void lyra_rt_bitvec_cell_initialize(
    void* cell, const void* prototype) noexcept {
  VariableAt<BitVec>(cell).Initialize(Read<BitVec>(prototype));
}

void lyra_rt_bitvec_cell_set(void* cell, const void* value) {
  VariableAt<BitVec>(cell).Set(Read<BitVec>(value));
}

auto lyra_rt_real_cell_get(void* cell) -> const void* {
  return &VariableAt<Real>(cell).Get();
}

void lyra_rt_real_cell_initialize(void* cell, const void* prototype) noexcept {
  VariableAt<Real>(cell).Initialize(Read<Real>(prototype));
}

void lyra_rt_real_cell_set(void* cell, const void* value) {
  VariableAt<Real>(cell).Set(Read<Real>(value));
}

void lyra_rt_real_cell_arm_sampling(void* cell) {
  VariableAt<Real>(cell).ArmSampling();
}

auto lyra_rt_real_cell_sampled_get(void* cell, const void* location) -> const
    void* {
  return &VariableAt<Real>(cell).SampledGet(SourceLocationAt(location));
}

auto lyra_rt_shortreal_cell_get(void* cell) -> const void* {
  return &VariableAt<ShortReal>(cell).Get();
}

void lyra_rt_shortreal_cell_initialize(
    void* cell, const void* prototype) noexcept {
  VariableAt<ShortReal>(cell).Initialize(Read<ShortReal>(prototype));
}

void lyra_rt_shortreal_cell_set(void* cell, const void* value) {
  VariableAt<ShortReal>(cell).Set(Read<ShortReal>(value));
}

void lyra_rt_shortreal_cell_arm_sampling(void* cell) {
  VariableAt<ShortReal>(cell).ArmSampling();
}

auto lyra_rt_shortreal_cell_sampled_get(void* cell, const void* location)
    -> const void* {
  return &VariableAt<ShortReal>(cell).SampledGet(SourceLocationAt(location));
}

auto lyra_rt_string_cell_begin_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) -> std::int64_t {
  return CellBeginTakeover<String>(
      cell, level, first, count, SourceLocationAt(location));
}

auto lyra_rt_string_cell_drive_takeover(
    void* cell, std::int64_t level, std::int64_t generation, const void* value,
    const void* location) -> bool {
  return CellDriveTakeover<String>(
      cell, level, generation, value, SourceLocationAt(location));
}

void lyra_rt_string_cell_end_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) {
  CellEndTakeover<String>(
      cell, level, first, count, SourceLocationAt(location));
}

auto lyra_rt_string_ref_begin_takeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) -> std::int64_t {
  return RefBeginTakeover<String>(
      reference, level, first, count, SourceLocationAt(location));
}

auto lyra_rt_string_ref_drive_takeover(
    void* reference, std::int64_t level, std::int64_t generation,
    const void* value, const void* location) -> bool {
  return RefDriveTakeover<String>(
      reference, level, generation, value, SourceLocationAt(location));
}

void lyra_rt_string_ref_end_takeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) {
  RefEndTakeover<String>(
      reference, level, first, count, SourceLocationAt(location));
}

auto lyra_rt_real_cell_begin_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) -> std::int64_t {
  return CellBeginTakeover<Real>(
      cell, level, first, count, SourceLocationAt(location));
}

auto lyra_rt_real_cell_drive_takeover(
    void* cell, std::int64_t level, std::int64_t generation, const void* value,
    const void* location) -> bool {
  return CellDriveTakeover<Real>(
      cell, level, generation, value, SourceLocationAt(location));
}

void lyra_rt_real_cell_end_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) {
  CellEndTakeover<Real>(cell, level, first, count, SourceLocationAt(location));
}

auto lyra_rt_real_ref_begin_takeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) -> std::int64_t {
  return RefBeginTakeover<Real>(
      reference, level, first, count, SourceLocationAt(location));
}

auto lyra_rt_real_ref_drive_takeover(
    void* reference, std::int64_t level, std::int64_t generation,
    const void* value, const void* location) -> bool {
  return RefDriveTakeover<Real>(
      reference, level, generation, value, SourceLocationAt(location));
}

void lyra_rt_real_ref_end_takeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) {
  RefEndTakeover<Real>(
      reference, level, first, count, SourceLocationAt(location));
}

auto lyra_rt_shortreal_cell_begin_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) -> std::int64_t {
  return CellBeginTakeover<ShortReal>(
      cell, level, first, count, SourceLocationAt(location));
}

auto lyra_rt_shortreal_cell_drive_takeover(
    void* cell, std::int64_t level, std::int64_t generation, const void* value,
    const void* location) -> bool {
  return CellDriveTakeover<ShortReal>(
      cell, level, generation, value, SourceLocationAt(location));
}

void lyra_rt_shortreal_cell_end_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) {
  CellEndTakeover<ShortReal>(
      cell, level, first, count, SourceLocationAt(location));
}

auto lyra_rt_shortreal_ref_begin_takeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) -> std::int64_t {
  return RefBeginTakeover<ShortReal>(
      reference, level, first, count, SourceLocationAt(location));
}

auto lyra_rt_shortreal_ref_drive_takeover(
    void* reference, std::int64_t level, std::int64_t generation,
    const void* value, const void* location) -> bool {
  return RefDriveTakeover<ShortReal>(
      reference, level, generation, value, SourceLocationAt(location));
}

void lyra_rt_shortreal_ref_end_takeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) {
  RefEndTakeover<ShortReal>(
      reference, level, first, count, SourceLocationAt(location));
}

void lyra_rt_string_sampled_history_install(
    void* history, const void* default_value, std::int64_t depth) {
  static_cast<SampledHistory<String>*>(history)->Install(
      Read<String>(default_value), depth);
}

void lyra_rt_string_sampled_history_push(void* history, const void* value) {
  static_cast<SampledHistory<String>*>(history)->Push(Read<String>(value));
}

auto lyra_rt_string_sampled_history_at(
    const void* history, std::int64_t ticks_back) -> const void* {
  return HandleTo(
      static_cast<const SampledHistory<String>*>(history)->At(ticks_back));
}

void lyra_rt_real_sampled_history_install(
    void* history, const void* default_value, std::int64_t depth) {
  static_cast<SampledHistory<Real>*>(history)->Install(
      Read<Real>(default_value), depth);
}

void lyra_rt_real_sampled_history_push(void* history, const void* value) {
  static_cast<SampledHistory<Real>*>(history)->Push(Read<Real>(value));
}

auto lyra_rt_real_sampled_history_at(
    const void* history, std::int64_t ticks_back) -> const void* {
  return HandleTo(
      static_cast<const SampledHistory<Real>*>(history)->At(ticks_back));
}

void lyra_rt_shortreal_sampled_history_install(
    void* history, const void* default_value, std::int64_t depth) {
  static_cast<SampledHistory<ShortReal>*>(history)->Install(
      Read<ShortReal>(default_value), depth);
}

void lyra_rt_shortreal_sampled_history_push(void* history, const void* value) {
  static_cast<SampledHistory<ShortReal>*>(history)->Push(
      Read<ShortReal>(value));
}

auto lyra_rt_shortreal_sampled_history_at(
    const void* history, std::int64_t ticks_back) -> const void* {
  return HandleTo(
      static_cast<const SampledHistory<ShortReal>*>(history)->At(ticks_back));
}

void lyra_rt_laidout_sampled_history_install(
    void* history, const void* default_value, std::int64_t depth,
    const void* default_value_type) {
  static_cast<SampledHistory<LaidOutValue>*>(history)->Install(
      lyra::runtime::OwnedLaidOutCopy(default_value, default_value_type),
      depth);
}

void lyra_rt_laidout_sampled_history_push(void* history, const void* value) {
  static_cast<SampledHistory<LaidOutValue>*>(history)->PushBytes(value);
}

auto lyra_rt_laidout_sampled_history_at(
    const void* history, std::int64_t ticks_back) -> const void* {
  return HandleTo(
      static_cast<const SampledHistory<LaidOutValue>*>(history)->At(
          ticks_back));
}

// Each entry keeps a share of the object its handle names, so an object the
// program no longer names anywhere is still there for a past tick to answer
// with -- which LRM 8.4 requires, since it reclaims an object only once nothing
// references it and a kept sampled value is a reference.
void lyra_rt_managedref_sampled_history_install(
    void* history, const void* default_value, std::int64_t depth) {
  static_cast<SampledHistory<ObjectRef>*>(history)->Install(
      Read<ObjectRef>(default_value), depth);
}

void lyra_rt_managedref_sampled_history_push(void* history, const void* value) {
  static_cast<SampledHistory<ObjectRef>*>(history)->Push(
      Read<ObjectRef>(value));
}

auto lyra_rt_managedref_sampled_history_at(
    const void* history, std::int64_t ticks_back) -> const void* {
  return HandleTo(
      static_cast<const SampledHistory<ObjectRef>*>(history)->At(ticks_back));
}

void lyra_rt_evaluation_attempts_install(
    void* attempts, void* effects, void* start, std::uint64_t start_count,
    void* step, void* control) {
  static_cast<EvaluationAttempts*>(attempts)->Install(
      *static_cast<RuntimeEffects*>(effects),
      SpanAt<const std::uint64_t>(start, start_count),
      AdoptClosure<lyra::support::EffectSignature>(step),
      static_cast<AssertionControl*>(control));
}

auto lyra_rt_evaluation_attempts_allocate(
    void* attempts, std::uint64_t size, std::uint64_t alignment) -> void* {
  return static_cast<const EvaluationAttempts*>(attempts)->Allocate(
      size, alignment);
}

void lyra_rt_evaluation_attempts_commit(void* attempts, void* made) {
  static_cast<EvaluationAttempts*>(attempts)->Commit(made);
}

void lyra_rt_evaluation_attempts_tick(void* attempts) {
  static_cast<EvaluationAttempts*>(attempts)->Tick();
}

void lyra_rt_evaluation_attempts_run(
    void* attempts, void* shared, std::uint64_t shared_count) {
  static_cast<EvaluationAttempts*>(attempts)->Run(
      SpanAt<const std::uint64_t>(shared, shared_count));
}

void lyra_rt_evaluation_attempts_test_disable(void* attempts) {
  static_cast<EvaluationAttempts*>(attempts)->TestDisable();
}

auto lyra_rt_evaluation_attempts_live_word(
    const void* attempts, std::uint64_t word) -> std::uint64_t {
  return static_cast<const EvaluationAttempts*>(attempts)->LiveWord(word);
}

auto lyra_rt_evaluation_attempts_answer(const void* attempts) -> std::uint64_t {
  return static_cast<const EvaluationAttempts*>(attempts)->Answer();
}

auto lyra_rt_evaluation_attempts_action_runs(const void* attempts) -> bool {
  return static_cast<const EvaluationAttempts*>(attempts)->ActionRuns();
}

auto lyra_rt_evaluation_attempt_shared_word(
    const void* attempt, std::uint64_t word) -> std::uint64_t {
  return static_cast<const EvaluationAttempt*>(attempt)->SharedWord(word);
}

auto lyra_rt_evaluation_attempt_live_word(
    const void* attempt, std::uint64_t word) -> std::uint64_t {
  return static_cast<const EvaluationAttempt*>(attempt)->LiveWord(word);
}

auto lyra_rt_evaluation_attempt_next_unstepped(void* attempt) -> std::int64_t {
  return static_cast<EvaluationAttempt*>(attempt)->NextUnstepped();
}

auto lyra_rt_evaluation_attempt_bits_at(
    const void* attempt, std::int64_t index, std::uint64_t word)
    -> std::uint64_t {
  return static_cast<const EvaluationAttempt*>(attempt)->BitsAt(index, word);
}

void lyra_rt_evaluation_attempt_set_word(
    void* attempt, std::int64_t index, std::uint64_t word, std::uint64_t bits) {
  static_cast<EvaluationAttempt*>(attempt)->SetWord(index, word, bits);
}

void lyra_rt_evaluation_attempt_step(void* attempt, std::int64_t index) {
  static_cast<EvaluationAttempt*>(attempt)->Step(index);
}

void lyra_rt_evaluation_attempt_seed_word(
    void* attempt, std::uint64_t word, std::uint64_t bits) {
  static_cast<EvaluationAttempt*>(attempt)->SeedWord(word, bits);
}

void lyra_rt_evaluation_attempt_seed(
    void* attempt, std::int64_t index, std::uint64_t implication,
    std::uint64_t part) {
  static_cast<EvaluationAttempt*>(attempt)->Seed(index, implication, part);
}

auto lyra_rt_evaluation_attempt_count(const void* attempt) -> std::int64_t {
  return static_cast<const EvaluationAttempt*>(attempt)->Count();
}

auto lyra_rt_evaluation_attempt_part_of(const void* attempt, std::int64_t index)
    -> std::uint64_t {
  return static_cast<const EvaluationAttempt*>(attempt)->PartOf(index);
}

auto lyra_rt_evaluation_attempt_implication_answer(
    const void* attempt, std::int64_t index, std::uint64_t implication)
    -> std::uint64_t {
  return static_cast<const EvaluationAttempt*>(attempt)->ImplicationAnswer(
      index, implication);
}

auto lyra_rt_evaluation_attempt_implication_nonvacuous(
    const void* attempt, std::int64_t index, std::uint64_t implication)
    -> std::uint64_t {
  return static_cast<const EvaluationAttempt*>(attempt)->ImplicationNonvacuous(
      index, implication);
}

void lyra_rt_evaluation_attempt_resolve(
    void* attempt, std::int64_t index, std::uint64_t answer,
    std::uint64_t nonvacuous) {
  static_cast<EvaluationAttempt*>(attempt)->Resolve(index, answer, nonvacuous);
}

// What a body that awaits a call holds of its answer across the suspension. The
// cell is allocated in the running execution's own value store, so the handle
// the generated frame carries points at storage that outlives every stretch of
// that body. A store overwrites the cell in place -- the first store installs
// the declared representation -- and a load answers with the value where the
// cell holds it. Nothing waits on it, so no runtime handle threads through and
// no subscriber wakes.
auto lyra_rt_string_value_cell_alloc() noexcept -> void* {
  return AllocateValueCell<String>();
}

void lyra_rt_string_value_cell_store(void* cell, const void* value) noexcept {
  ValueCellAt<String>(cell).Assign(Read<String>(value));
}

auto lyra_rt_string_value_cell_load(void* cell) noexcept -> void* {
  return &ValueCellAt<String>(cell).Storage();
}

void lyra_rt_assert(bool holds, const char* message, const void* location) {
  lyra::value::Assert(holds, message, SourceLocationAt(location));
}

auto lyra_rt_string_from_bits(
    const void* bits, std::int64_t bits_width, bool bits_are_four_state,
    void* out) -> void* {
  const lyra::value::LoadedWords words =
      BitsAt(bits, bits_width, bits_are_four_state);
  return Emplace(out, String::FromIntegral(words.View()));
}

auto lyra_rt_string_from_byte_array(
    const void* bytes, const void* bytes_type, void* out) -> void* {
  return Emplace(out, lyra::value::ByteStringOf(ArrayAt(bytes, bytes_type)));
}

auto lyra_rt_string_cstr(const void* value) -> const char* {
  return Read<String>(value).CStr();
}

auto lyra_rt_string_len(const void* value, void* out) -> void* {
  return Emplace(out, Read<String>(value).Len());
}

auto lyra_rt_string_getc(const void* value, const void* position, void* out)
    -> void* {
  return Emplace(
      out, Read<String>(value).Getc(Read<lyra::value::Position>(position)));
}

auto lyra_rt_string_toupper(const void* value, void* out) -> void* {
  return Emplace(out, Read<String>(value).Toupper());
}

auto lyra_rt_string_tolower(const void* value, void* out) -> void* {
  return Emplace(out, Read<String>(value).Tolower());
}

auto lyra_rt_string_compare(const void* lhs, const void* rhs, void* out)
    -> void* {
  return Emplace(out, Read<String>(lhs).Compare(Read<String>(rhs)));
}

auto lyra_rt_string_icompare(const void* lhs, const void* rhs, void* out)
    -> void* {
  return Emplace(out, Read<String>(lhs).Icompare(Read<String>(rhs)));
}

auto lyra_rt_string_substr(
    const void* value, const void* first, const void* last, void* out)
    -> void* {
  return Emplace(
      out, Read<String>(value).Substr(
               Read<lyra::value::Position>(first),
               Read<lyra::value::Position>(last)));
}

auto lyra_rt_string_concat(void* parts, std::uint64_t parts_count, void* out)
    -> void* {
  const std::span<const void* const> handles =
      SpanAt<const void* const>(parts, parts_count);
  return Emplace(
      out, String::Concat(handles.size(), [&](std::size_t i) -> const String& {
        return Read<String>(handles[i]);
      }));
}

auto lyra_rt_replicate_string(
    const void* operand, std::int64_t count, void* out) -> void* {
  return Emplace(out, Read<String>(operand).Replicate(count));
}

auto lyra_rt_string_atoi(const void* value, void* out) -> void* {
  return Emplace(out, Read<String>(value).Atoi());
}

auto lyra_rt_string_atohex(const void* value, void* out) -> void* {
  return Emplace(out, Read<String>(value).Atohex());
}

auto lyra_rt_string_atooct(const void* value, void* out) -> void* {
  return Emplace(out, Read<String>(value).Atooct());
}

auto lyra_rt_string_atobin(const void* value, void* out) -> void* {
  return Emplace(out, Read<String>(value).Atobin());
}

auto lyra_rt_string_atoreal(const void* value, void* out) -> void* {
  return Emplace(out, Read<String>(value).Atoreal());
}

// The character write and the formatting family change their receiver (LRM
// 6.16.3, 6.16.14 -- 6.16.18), which each does where the string lies.
void lyra_rt_string_putc(
    void* value, const void* position, const void* character,
    std::int64_t character_width, bool character_is_signed,
    bool character_is_four_state) {
  static_cast<String*>(value)->PutCharacter(
      Read<lyra::value::Position>(position),
      IntOf(NumberAt(
          character, character_width, character_is_signed,
          character_is_four_state)));
}

void lyra_rt_string_itoa(
    void* value, const void* number, std::int64_t number_width,
    bool number_is_signed, bool number_is_four_state) {
  static_cast<String*>(value)->SetDecimal(IntOf(
      NumberAt(number, number_width, number_is_signed, number_is_four_state)));
}

void lyra_rt_string_hextoa(
    void* value, const void* number, std::int64_t number_width,
    bool number_is_signed, bool number_is_four_state) {
  static_cast<String*>(value)->SetHex(IntOf(
      NumberAt(number, number_width, number_is_signed, number_is_four_state)));
}

void lyra_rt_string_octtoa(
    void* value, const void* number, std::int64_t number_width,
    bool number_is_signed, bool number_is_four_state) {
  static_cast<String*>(value)->SetOctal(IntOf(
      NumberAt(number, number_width, number_is_signed, number_is_four_state)));
}

void lyra_rt_string_bintoa(
    void* value, const void* number, std::int64_t number_width,
    bool number_is_signed, bool number_is_four_state) {
  static_cast<String*>(value)->SetBinary(IntOf(
      NumberAt(number, number_width, number_is_signed, number_is_four_state)));
}

void lyra_rt_string_realtoa(void* value, const void* number) {
  static_cast<String*>(value)->Realtoa(Read<Real>(number));
}

auto lyra_rt_string_scan_string(
    const void* input, const void* format, const void* prototypes,
    const void* prototypes_type, const void* location, const void* answer_type,
    void* out) -> void* {
  lyra::value::StringScanInput source(Read<String>(input).View());
  return lyra::runtime::EmplaceScan(
      out, lyra::runtime::TupleTypeAt(answer_type), source,
      Read<String>(format), prototypes,
      lyra::runtime::TupleTypeAt(prototypes_type), SourceLocationAt(location));
}

auto lyra_rt_string_add(const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(out, Read<String>(lhs) + Read<String>(rhs));
}

auto lyra_rt_string_eq(const void* lhs, const void* rhs) -> std::uint8_t {
  return Compared(Read<String>(lhs) == Read<String>(rhs));
}

auto lyra_rt_string_case_equal(const void* lhs, const void* rhs, void* out)
    -> void* {
  return Emplace(out, Read<String>(lhs) == Read<String>(rhs));
}

auto lyra_rt_string_ne(const void* lhs, const void* rhs) -> std::uint8_t {
  return Compared(Read<String>(lhs) != Read<String>(rhs));
}

auto lyra_rt_string_lt(const void* lhs, const void* rhs) -> std::uint8_t {
  return Compared(Read<String>(lhs) < Read<String>(rhs));
}

auto lyra_rt_string_le(const void* lhs, const void* rhs) -> std::uint8_t {
  return Compared(Read<String>(lhs) <= Read<String>(rhs));
}

auto lyra_rt_string_gt(const void* lhs, const void* rhs) -> std::uint8_t {
  return Compared(Read<String>(lhs) > Read<String>(rhs));
}

auto lyra_rt_string_ge(const void* lhs, const void* rhs) -> std::uint8_t {
  return Compared(Read<String>(lhs) >= Read<String>(rhs));
}

auto lyra_rt_format_spec_make(
    std::int64_t kind, std::int64_t field_width, std::int64_t precision,
    std::int64_t zero_pad, std::int64_t left_align, std::int64_t timeunit_power,
    void* out) -> void* {
  return Emplace(
      out,
      FormatSpec(
          kind, field_width, precision, zero_pad, left_align, timeunit_power));
}

auto lyra_rt_integral_print_item_make(
    const void* value, std::int64_t value_width, bool value_is_signed,
    bool value_is_four_state, const void* spec, void* out) -> void* {
  return Emplace(
      out,
      PrintItem(
          lyra::runtime::IntegralFormatArg(
              value, lyra::runtime::NumberShape(
                         value_width, value_is_signed, value_is_four_state)),
          Read<FormatSpec>(spec)));
}

auto lyra_rt_string_print_item_make(
    const void* value, const void* spec, void* out) -> void* {
  return Emplace(out, PrintItem(Read<String>(value), Read<FormatSpec>(spec)));
}

auto lyra_rt_unmanagedref_print_item_make(
    const void* value, const void* spec, void* out) -> void* {
  return Emplace(
      out, PrintItem(Read<UnmanagedRef>(value), Read<FormatSpec>(spec)));
}

auto lyra_rt_managedref_print_item_make(
    const void* value, const void* spec, void* out) -> void* {
  return Emplace(
      out, PrintItem(Read<ObjectRef>(value), Read<FormatSpec>(spec)));
}

auto lyra_rt_real_case_equal(const void* lhs, const void* rhs, void* out)
    -> void* {
  return Emplace(out, Read<Real>(lhs).CaseEqual(Read<Real>(rhs)));
}

auto lyra_rt_real_pow(const void* base, const void* exponent, void* out)
    -> void* {
  return Emplace(out, Read<Real>(base).Pow(Read<Real>(exponent)));
}

auto lyra_rt_real_to_integral(
    const void* value, std::int64_t out_width, bool out_is_four_state,
    void* out) -> void* {
  lyra::value::WrittenWords bits(
      lyra::runtime::BitsShape(out_width, out_is_four_state));
  Read<Real>(value).RoundInto(bits.MutableView());
  bits.StoreTo(out);
  return out;
}

auto lyra_rt_real_truncate(const void* value) -> std::int64_t {
  return Read<Real>(value).Truncate();
}

auto lyra_rt_real_to_bits(const void* value) -> std::int64_t {
  return Read<Real>(value).ToBits();
}

auto lyra_rt_real_from_bits(std::int64_t bits, void* out) -> void* {
  return Emplace(out, Real::FromBits(bits));
}

auto lyra_rt_real_ln(const void* value, void* out) -> void* {
  return Emplace(out, Read<Real>(value).Ln());
}

auto lyra_rt_real_log10(const void* value, void* out) -> void* {
  return Emplace(out, Read<Real>(value).Log10());
}

auto lyra_rt_real_exp(const void* value, void* out) -> void* {
  return Emplace(out, Read<Real>(value).Exp());
}

auto lyra_rt_real_sqrt(const void* value, void* out) -> void* {
  return Emplace(out, Read<Real>(value).Sqrt());
}

auto lyra_rt_real_floor(const void* value, void* out) -> void* {
  return Emplace(out, Read<Real>(value).Floor());
}

auto lyra_rt_real_ceil(const void* value, void* out) -> void* {
  return Emplace(out, Read<Real>(value).Ceil());
}

auto lyra_rt_real_sin(const void* value, void* out) -> void* {
  return Emplace(out, Read<Real>(value).Sin());
}

auto lyra_rt_real_cos(const void* value, void* out) -> void* {
  return Emplace(out, Read<Real>(value).Cos());
}

auto lyra_rt_real_tan(const void* value, void* out) -> void* {
  return Emplace(out, Read<Real>(value).Tan());
}

auto lyra_rt_real_asin(const void* value, void* out) -> void* {
  return Emplace(out, Read<Real>(value).Asin());
}

auto lyra_rt_real_acos(const void* value, void* out) -> void* {
  return Emplace(out, Read<Real>(value).Acos());
}

auto lyra_rt_real_atan(const void* value, void* out) -> void* {
  return Emplace(out, Read<Real>(value).Atan());
}

auto lyra_rt_real_atan2(const void* y, const void* x, void* out) -> void* {
  return Emplace(out, Read<Real>(y).Atan2(Read<Real>(x)));
}

auto lyra_rt_real_hypot(const void* x, const void* y, void* out) -> void* {
  return Emplace(out, Read<Real>(x).Hypot(Read<Real>(y)));
}

auto lyra_rt_real_sinh(const void* value, void* out) -> void* {
  return Emplace(out, Read<Real>(value).Sinh());
}

auto lyra_rt_real_cosh(const void* value, void* out) -> void* {
  return Emplace(out, Read<Real>(value).Cosh());
}

auto lyra_rt_real_tanh(const void* value, void* out) -> void* {
  return Emplace(out, Read<Real>(value).Tanh());
}

auto lyra_rt_real_asinh(const void* value, void* out) -> void* {
  return Emplace(out, Read<Real>(value).Asinh());
}

auto lyra_rt_real_acosh(const void* value, void* out) -> void* {
  return Emplace(out, Read<Real>(value).Acosh());
}

auto lyra_rt_real_atanh(const void* value, void* out) -> void* {
  return Emplace(out, Read<Real>(value).Atanh());
}

auto lyra_rt_real_from_integral(
    const void* value, std::int64_t value_width, bool value_is_signed,
    bool value_is_four_state, void* out) -> void* {
  return Emplace(
      out,
      Real::ConvertFromIntegral(
          NumberAt(value, value_width, value_is_signed, value_is_four_state)
              .View()));
}

auto lyra_rt_real_convert_from_shortreal(const void* value, void* out)
    -> void* {
  return Emplace(out, Real{Read<ShortReal>(value)});
}

auto lyra_rt_real_convert_from_real(const void* value, void* out) -> void* {
  return Emplace(out, Read<Real>(value));
}

auto lyra_rt_real_value_cell_alloc() noexcept -> void* {
  return AllocateValueCell<Real>();
}

void lyra_rt_real_value_cell_store(void* cell, const void* value) noexcept {
  ValueCellAt<Real>(cell).Assign(Read<Real>(value));
}

auto lyra_rt_real_value_cell_load(void* cell) noexcept -> void* {
  return &ValueCellAt<Real>(cell).Storage();
}

auto lyra_rt_real_print_item_make(
    const void* value, const void* spec, void* out) -> void* {
  return Emplace(out, PrintItem(Read<Real>(value), Read<FormatSpec>(spec)));
}

auto lyra_rt_real_format_arg_make(const void* value, void* out) -> void* {
  return Emplace(out, MakeFormatArg(Read<Real>(value)));
}

auto lyra_rt_shortreal_case_equal(const void* lhs, const void* rhs, void* out)
    -> void* {
  return Emplace(out, Read<ShortReal>(lhs).CaseEqual(Read<ShortReal>(rhs)));
}

auto lyra_rt_shortreal_pow(const void* base, const void* exponent, void* out)
    -> void* {
  return Emplace(out, Read<ShortReal>(base).Pow(Read<ShortReal>(exponent)));
}

auto lyra_rt_shortreal_to_integral(
    const void* value, std::int64_t out_width, bool out_is_four_state,
    void* out) -> void* {
  lyra::value::WrittenWords bits(
      lyra::runtime::BitsShape(out_width, out_is_four_state));
  Read<ShortReal>(value).RoundInto(bits.MutableView());
  bits.StoreTo(out);
  return out;
}

auto lyra_rt_shortreal_to_bits(const void* value) -> std::int64_t {
  return Read<ShortReal>(value).ToBits();
}

auto lyra_rt_shortreal_from_bits(std::int64_t bits, void* out) -> void* {
  return Emplace(out, ShortReal::FromBits(bits));
}

auto lyra_rt_shortreal_from_integral(
    const void* value, std::int64_t value_width, bool value_is_signed,
    bool value_is_four_state, void* out) -> void* {
  return Emplace(
      out,
      ShortReal::ConvertFromIntegral(
          NumberAt(value, value_width, value_is_signed, value_is_four_state)
              .View()));
}

auto lyra_rt_shortreal_convert_from_real(const void* value, void* out)
    -> void* {
  return Emplace(out, ShortReal{Read<Real>(value)});
}

auto lyra_rt_shortreal_value_cell_alloc() noexcept -> void* {
  return AllocateValueCell<ShortReal>();
}

void lyra_rt_shortreal_value_cell_store(
    void* cell, const void* value) noexcept {
  ValueCellAt<ShortReal>(cell).Assign(Read<ShortReal>(value));
}

auto lyra_rt_shortreal_value_cell_load(void* cell) noexcept -> void* {
  return &ValueCellAt<ShortReal>(cell).Storage();
}

auto lyra_rt_shortreal_print_item_make(
    const void* value, const void* spec, void* out) -> void* {
  return Emplace(
      out, PrintItem(Read<ShortReal>(value), Read<FormatSpec>(spec)));
}

auto lyra_rt_shortreal_format_arg_make(const void* value, void* out) -> void* {
  return Emplace(out, MakeFormatArg(Read<ShortReal>(value)));
}

auto lyra_rt_unmanagedref_case_equal(
    const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(
      out, Read<UnmanagedRef>(lhs).CaseEqual(Read<UnmanagedRef>(rhs)));
}

auto lyra_rt_unmanagedref_value_cell_alloc() noexcept -> void* {
  return AllocateValueCell<UnmanagedRef>();
}

void lyra_rt_unmanagedref_value_cell_store(
    void* cell, const void* value) noexcept {
  ValueCellAt<UnmanagedRef>(cell).Assign(Read<UnmanagedRef>(value));
}

auto lyra_rt_unmanagedref_value_cell_load(void* cell) noexcept -> void* {
  return &ValueCellAt<UnmanagedRef>(cell).Storage();
}

// A variable holding a reference that owns nothing, which a process may wait
// on: LRM 9.4.2 makes a write to one an event whenever the address it holds is
// not the address it held.
auto lyra_rt_unmanagedref_cell_get(void* cell) -> const void* {
  return &VariableAt<UnmanagedRef>(cell).Get();
}

void lyra_rt_unmanagedref_cell_initialize(
    void* cell, const void* prototype) noexcept {
  VariableAt<UnmanagedRef>(cell).Initialize(Read<UnmanagedRef>(prototype));
}

void lyra_rt_unmanagedref_cell_set(void* cell, const void* value) {
  VariableAt<UnmanagedRef>(cell).Set(Read<UnmanagedRef>(value));
}

void lyra_rt_unmanagedref_cell_arm_sampling(void* cell) {
  VariableAt<UnmanagedRef>(cell).ArmSampling();
}

auto lyra_rt_unmanagedref_cell_sampled_get(void* cell, const void* location)
    -> const void* {
  return &VariableAt<UnmanagedRef>(cell).SampledGet(SourceLocationAt(location));
}

// A handle referring to nothing (LRM 8.4), a value of the domain like any
// other.
auto lyra_rt_managedref_default(void* out) -> void* {
  return Emplace(out, ObjectRef{});
}

// Comparing two handles asks which object each names (LRM 11.4.5). The clause
// makes the answer always a known 1'b0 or 1'b1, so the entry answers with that
// value.
auto lyra_rt_managedref_eq(const void* lhs, const void* rhs) -> std::uint8_t {
  return Compared(Read<ObjectRef>(lhs) == Read<ObjectRef>(rhs));
}

auto lyra_rt_managedref_ne(const void* lhs, const void* rhs) -> std::uint8_t {
  return Compared(Read<ObjectRef>(lhs) != Read<ObjectRef>(rhs));
}

// LRM 11.4.5: `===` on a handle carries the same meaning as `==`.
auto lyra_rt_managedref_case_equal(const void* lhs, const void* rhs, void* out)
    -> void* {
  return Emplace(out, Read<ObjectRef>(lhs).CaseEqual(Read<ObjectRef>(rhs)));
}

auto lyra_rt_managedref_to_bool(const void* operand) -> bool {
  return static_cast<bool>(Read<ObjectRef>(operand));
}

auto lyra_rt_managedref_value_cell_alloc() noexcept -> void* {
  return AllocateValueCell<ObjectRef>();
}

// Storing copies the handle's share of ownership with it, which is what keeps
// the object alive once the handle it was stored from has ended; a reader that
// keeps what it loaded copies it, and so owns a share of its own.
void lyra_rt_managedref_value_cell_store(
    void* cell, const void* value) noexcept {
  ValueCellAt<ObjectRef>(cell).Assign(Read<ObjectRef>(value));
}

auto lyra_rt_managedref_value_cell_load(void* cell) noexcept -> void* {
  return &ValueCellAt<ObjectRef>(cell).Storage();
}

// A variable of class type, which a process may wait on: LRM 9.4.2 makes a
// write to one an event whenever the object it names is not the object it
// named. A store keeps the handle's share of ownership and a load hands one
// back, so the object outlives every body that touches the variable.
auto lyra_rt_managedref_cell_get(void* cell) -> const void* {
  return &VariableAt<ObjectRef>(cell).Get();
}

void lyra_rt_managedref_cell_initialize(
    void* cell, const void* prototype) noexcept {
  VariableAt<ObjectRef>(cell).Initialize(Read<ObjectRef>(prototype));
}

void lyra_rt_managedref_cell_set(void* cell, const void* value) {
  VariableAt<ObjectRef>(cell).Set(Read<ObjectRef>(value));
}

// Arming keeps a share of whatever the variable names at the moment it is
// armed, and every later slot the variable moves away from replaces it, so the
// object a sampled read answers with is alive for as long as that read can
// happen (LRM 8.4, 16.5.1).
void lyra_rt_managedref_cell_arm_sampling(void* cell) {
  VariableAt<ObjectRef>(cell).ArmSampling();
}

auto lyra_rt_managedref_cell_sampled_get(void* cell, const void* location)
    -> const void* {
  return &VariableAt<ObjectRef>(cell).SampledGet(SourceLocationAt(location));
}

auto lyra_rt_managedref_cell_begin_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) -> std::int64_t {
  return CellBeginTakeover<ObjectRef>(
      cell, level, first, count, SourceLocationAt(location));
}

auto lyra_rt_managedref_cell_drive_takeover(
    void* cell, std::int64_t level, std::int64_t generation, const void* value,
    const void* location) -> bool {
  return CellDriveTakeover<ObjectRef>(
      cell, level, generation, value, SourceLocationAt(location));
}

void lyra_rt_managedref_cell_end_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) {
  CellEndTakeover<ObjectRef>(
      cell, level, first, count, SourceLocationAt(location));
}

auto lyra_rt_managedref_ref_begin_takeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) -> std::int64_t {
  return RefBeginTakeover<ObjectRef>(
      reference, level, first, count, SourceLocationAt(location));
}

auto lyra_rt_managedref_ref_drive_takeover(
    void* reference, std::int64_t level, std::int64_t generation,
    const void* value, const void* location) -> bool {
  return RefDriveTakeover<ObjectRef>(
      reference, level, generation, value, SourceLocationAt(location));
}

void lyra_rt_managedref_ref_end_takeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) {
  RefEndTakeover<ObjectRef>(
      reference, level, first, count, SourceLocationAt(location));
}

void lyra_rt_laidout_cell_initialize(
    void* cell, const void* prototype) noexcept {
  VariableAt<LaidOutValue>(cell).Initialize(prototype);
}

void lyra_rt_laidout_cell_set(void* cell, const void* value) {
  VariableAt<LaidOutValue>(cell).SetBytes(value);
}

void lyra_rt_laidout_cell_arm_sampling(void* cell) {
  VariableAt<LaidOutValue>(cell).ArmSampling();
}

auto lyra_rt_laidout_cell_begin_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location, const void* value_take_positions) -> std::int64_t {
  return VariableAt<LaidOutValue>(cell).BeginTakeover(
      level, first, count, SourceLocationAt(location),
      lyra::runtime::TakePositionsAt(value_take_positions));
}

auto lyra_rt_laidout_cell_drive_takeover(
    void* cell, std::int64_t level, std::int64_t generation, const void* value,
    const void* location) -> bool {
  return VariableAt<LaidOutValue>(cell).DriveTakeoverBytes(
      level, generation, value, SourceLocationAt(location));
}

void lyra_rt_laidout_cell_end_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) {
  VariableAt<LaidOutValue>(cell).EndTakeover(
      level, first, count, SourceLocationAt(location));
}

auto lyra_rt_laidout_ref_begin_takeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location, const void* value_take_positions) -> std::int64_t {
  return lyra::runtime::LaidOutRefBeginTakeover(
      reference, level, first, count, SourceLocationAt(location),
      value_take_positions);
}

auto lyra_rt_laidout_ref_drive_takeover(
    void* reference, std::int64_t level, std::int64_t generation,
    const void* value, const void* location) -> bool {
  return lyra::runtime::LaidOutRefDriveTakeover(
      reference, level, generation, value, SourceLocationAt(location));
}

void lyra_rt_laidout_ref_end_takeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) {
  lyra::runtime::EndTakeoverThrough<
      lyra::runtime::VariableOf<LaidOutValue>::Type>(
      lyra::runtime::ErasedAt(reference), level, first, count,
      SourceLocationAt(location));
}

auto lyra_rt_laidout_cell_sampled_get(void* cell, const void* location) -> const
    void* {
  return VariableAt<LaidOutValue>(cell).SampledBytes(
      SourceLocationAt(location));
}

auto lyra_rt_laidout_value_cell_alloc(const void* held_type) noexcept -> void* {
  return AllocateValueCell<LaidOutValue>(
      lyra::runtime::ElementTypeAt(held_type));
}

auto lyra_rt_dynamic_array_construct(const void* element, void* out) noexcept
    -> void* {
  return std::construct_at(
      static_cast<lyra::value::DynamicArrayCore*>(out),
      lyra::value::DynamicArrayConstruct(
          static_cast<const lyra::value::ValueType*>(element)));
}

auto lyra_rt_dynamic_array_generation(
    const void* array, std::uint64_t count) noexcept -> void* {
  return lyra::value::DynamicArrayGeneration(
      *static_cast<const lyra::value::DynamicArrayCore*>(array), count);
}

void lyra_rt_dynamic_array_adopt(
    void* array, void* data, std::uint64_t count) noexcept {
  lyra::value::DynamicArrayAdopt(
      *static_cast<lyra::value::DynamicArrayCore*>(array), data, count);
}

auto lyra_rt_dynamic_array_let_go(void* array) noexcept -> bool {
  return lyra::value::DynamicArrayLetGo(
      *static_cast<lyra::value::DynamicArrayCore*>(array));
}

void lyra_rt_dynamic_array_free(void* array) noexcept {
  lyra::value::DynamicArrayFree(
      *static_cast<lyra::value::DynamicArrayCore*>(array));
}

auto lyra_rt_queue_construct(const void* element, void* out) noexcept -> void* {
  return std::construct_at(
      static_cast<lyra::value::QueueCore*>(out),
      lyra::value::QueueConstruct(
          static_cast<const lyra::value::ValueType*>(element)));
}

void lyra_rt_queue_reserve(void* queue, std::uint64_t count) noexcept {
  lyra::value::QueueReserve(
      *static_cast<lyra::value::QueueCore*>(queue), count);
}

void lyra_rt_queue_commit(
    void* queue, std::uint64_t position, void* storage) noexcept {
  lyra::value::QueueCommit(
      *static_cast<lyra::value::QueueCore*>(queue), position, storage);
}

void lyra_rt_queue_free_ring(void* queue) noexcept {
  lyra::value::QueueFreeRing(*static_cast<lyra::value::QueueCore*>(queue));
}

void lyra_rt_queue_adopt(void* queue, void* from) noexcept {
  lyra::value::QueueAdopt(
      *static_cast<lyra::value::QueueCore*>(queue),
      *static_cast<lyra::value::QueueCore*>(from));
}

void lyra_rt_queue_reverse(void* queue) noexcept {
  lyra::value::QueueReverse(*static_cast<lyra::value::QueueCore*>(queue));
}

void lyra_rt_queue_grew_past_its_bound(const void* location) noexcept {
  lyra::value::ReportQueueGrewPastItsBound(SourceLocationAt(location));
}

void lyra_rt_queue_written_at_an_invalid_index(const void* location) noexcept {
  lyra::value::ReportQueueWrittenAtAnInvalidIndex(SourceLocationAt(location));
}

void lyra_rt_assoc_read_at_an_invalid_index(const void* location) noexcept {
  lyra::value::ReportAssociativeArrayReadAtAnInvalidIndex(
      SourceLocationAt(location));
}

void lyra_rt_assoc_read_at_a_missing_entry(const void* location) noexcept {
  lyra::value::ReportAssociativeArrayReadAtAMissingEntry(
      SourceLocationAt(location));
}

void lyra_rt_assoc_written_at_an_invalid_index(const void* location) noexcept {
  lyra::value::ReportAssociativeArrayWrittenAtAnInvalidIndex(
      SourceLocationAt(location));
}

auto lyra_rt_recycler_allocate(void* recycler) noexcept -> void* {
  return lyra::value::RecyclerAllocate(
      static_cast<lyra::value::Recycler*>(recycler));
}

auto lyra_rt_default_placement_new(void* storage) noexcept -> void* {
  return storage;
}

auto lyra_rt_queue_unlink(void* queue, std::uint64_t position) noexcept
    -> void* {
  return lyra::value::QueueUnlink(
      *static_cast<lyra::value::QueueCore*>(queue), position);
}

// The positions a sort answers are 64-bit two-state integers, whose bytes are
// the word each holds.
void lyra_rt_queue_apply_order(void* queue, const void* order) noexcept {
  lyra::value::QueueApplyOrder(
      *static_cast<lyra::value::QueueCore*>(queue),
      *static_cast<const lyra::value::DynamicArrayCore*>(order));
}

// LRM 10.9.1: an enumerated element list is the unit laid down once, and a
// replicated or uniform one the unit laid down until the array is full. The
// array's type fixes how many elements that is, so a unit that does not fill
// it a whole number of times is a call no lowering states.
auto lyra_rt_laidout_from_elements(
    void* unit, std::uint64_t unit_count, const void* answer_type, void* out)
    -> void* {
  const lyra::value::FixedArrayType& type = ArrayTypeAt(answer_type);
  const std::span<const void* const> handles =
      SpanAt<const void* const>(unit, unit_count);
  if (handles.empty() || type.Count() % handles.size() != 0) {
    throw lyra::InternalError(
        std::format(
            "a fixed-size unpacked array of {} elements is built from a unit "
            "of {}, which does not fill it -- please report this as a bug",
            type.Count(), handles.size()));
  }
  type.BuildEach(out, [&](std::size_t i, void* built) {
    type.ElementType().Type().Copy(handles[i % handles.size()], built);
  });
  return out;
}

auto lyra_rt_byte_array_from_string(
    const void* text, const void* answer_type, void* out) -> void* {
  return Emplace(
      out, ArrayFromString(ArrayTypeAt(answer_type), Read<String>(text)));
}

auto lyra_rt_byte_array_from_bits(
    const void* bits, std::int64_t bits_width, bool bits_are_four_state,
    const void* answer_type, void* out) -> void* {
  const lyra::value::LoadedWords words =
      BitsAt(bits, bits_width, bits_are_four_state);
  return Emplace(
      out, ArrayFromIntegral(ArrayTypeAt(answer_type), words.View()));
}

auto lyra_rt_string_count_bits(
    const void* value, const void* control_bits, const void* location,
    void* out) -> void* {
  return Emplace(
      out, Read<String>(value).CountBits(
               Read<lyra::value::LogicVector<4>>(control_bits),
               SourceLocationAt(location)));
}

auto lyra_rt_string_bitstream_width(
    const void* value, const void* location, void* out) -> void* {
  return Emplace(
      out, Read<String>(value).BitstreamWidth(SourceLocationAt(location)));
}

auto lyra_rt_from_bitstream(
    const void* bits, std::int64_t bits_width, bool bits_are_four_state,
    const void* location, const void* answer_type,
    const void* answer_operations, void* out) -> void* {
  const lyra::base::SourceLocationScope bound(SourceLocationAt(location));
  const lyra::value::LoadedWords stream =
      BitsAt(bits, bits_width, bits_are_four_state);
  const lyra::value::ValueOperations& operations =
      lyra::runtime::OperationsAt(answer_operations);
  lyra::runtime::ElementTypeAt(answer_type).BuildDefault(out);
  operations.functions->read_from_stream(
      operations, stream.View(), 0, static_cast<std::uint64_t>(bits_width),
      out);
  return out;
}

auto lyra_rt_stream_write(
    LyraStreamOf stream_of, const void* value, std::int64_t bits_width,
    bool bits_are_four_state, const void* stream, std::uint64_t filled)
    -> std::uint64_t {
  lyra::runtime::StorageForBits made(
      lyra::runtime::BitsShape(bits_width, bits_are_four_state));
  stream_of(value, nullptr, made.Bytes());
  const lyra::value::LoadedWords written =
      BitsAt(made.Bytes(), bits_width, bits_are_four_state);
  const auto& into = *static_cast<const lyra::value::IntegralView*>(stream);
  const std::uint64_t width = written.View().width;
  lyra::value::Insert(
      into, written.View(),
      static_cast<std::int64_t>(into.width - filled - width));
  return filled + width;
}

void lyra_rt_positions_write(
    LyraPositionsOf positions_of, const void* value, std::int64_t bits_width,
    bool bits_are_four_state, const void* out) {
  lyra::runtime::StorageForBits made(
      lyra::runtime::BitsShape(bits_width, bits_are_four_state));
  positions_of(value, made.Bytes());
  lyra::value::Extract(
      *static_cast<const lyra::value::IntegralView*>(out),
      BitsAt(made.Bytes(), bits_width, bits_are_four_state).View(), 0);
}

auto lyra_rt_stream_build(
    LyraBuiltFromStream built_from, const void* stream, std::uint64_t taken,
    std::int64_t bits_width, bool bits_are_four_state, void* out)
    -> std::uint64_t {
  const lyra::value::IntegralShape shape =
      lyra::runtime::BitsShape(bits_width, bits_are_four_state);
  lyra::value::WrittenWords read(shape);
  const auto& from =
      *static_cast<const lyra::value::ConstIntegralView*>(stream);
  const auto width = static_cast<std::uint64_t>(bits_width);
  lyra::value::Extract(
      read.MutableView(), from,
      static_cast<std::int64_t>(from.width - taken - width));
  lyra::runtime::StorageForBits held(shape);
  read.StoreTo(held.Bytes());
  built_from(held.Bytes(), nullptr, out);
  return taken + width;
}

auto lyra_rt_run_write(
    LyraRunOf run_of, const void* value, const void* stream,
    std::uint64_t filled) -> std::uint64_t {
  const auto run = lyra::value::TakeBuilt<BitVec>(
      [&](void* storage) { run_of(value, nullptr, storage); });
  return lyra::value::WriteRunToStream(
      run, *static_cast<const lyra::value::IntegralView*>(stream), filled);
}

auto lyra_rt_run_build(
    LyraBuiltFromRun built_from, const void* stream, std::uint64_t taken,
    std::uint64_t until, void* out) -> std::uint64_t {
  const auto& from =
      *static_cast<const lyra::value::ConstIntegralView*>(stream);
  const BitVec run = lyra::value::BitVec::Of(from, taken, until);
  built_from(&run, nullptr, out);
  return until;
}

void lyra_rt_type_lacks_operation() {
  lyra::value::LacksOperation();
}

auto lyra_rt_wildcard_index_make(
    const void* value, const void* value_type, void* out) -> void* {
  return std::construct_at(
      static_cast<lyra::value::AnyValue*>(out),
      lyra::runtime::OwnedCopy(value, value_type));
}

auto lyra_rt_wildcard_index_copy(const void* value, void* out) -> void* {
  return std::construct_at(
      static_cast<lyra::value::AnyValue*>(out),
      *static_cast<const lyra::value::AnyValue*>(value));
}

auto lyra_rt_wildcard_index_relocate(void* value, void* out) -> void* {
  return Relocate<lyra::value::AnyValue>(value, out);
}

void lyra_rt_wildcard_index_destroy(void* object) {
  std::destroy_at(static_cast<lyra::value::AnyValue*>(object));
}

void lyra_rt_wildcard_index_assign(void* storage, const void* value) {
  *static_cast<lyra::value::AnyValue*>(storage) =
      *static_cast<const lyra::value::AnyValue*>(value);
}

auto lyra_rt_any_make(
    const void* value, const void* value_type, const char* identity, void* out)
    -> void* {
  return std::construct_at(
      static_cast<lyra::value::Any*>(out),
      lyra::value::Any::CopyOf(
          lyra::runtime::TypeAt(value_type), value, identity));
}

auto lyra_rt_see_as_any(
    const void* incoming, const char* identity, void* carry,
    const void* value_type, void* out) -> void* {
  return std::construct_at(
      static_cast<lyra::runtime::AnyReference*>(out),
      lyra::runtime::ErasedAt(incoming).storage,
      lyra::runtime::TypeAt(value_type), identity,
      lyra::runtime::Borrow<lyra::support::EffectSignature>(
          *static_cast<lyra::runtime::ClosureValue*>(carry)));
}

auto lyra_rt_receive_any(const void* reference, const void* value) -> bool {
  return lyra::runtime::ReceiveAny(
      *static_cast<const lyra::runtime::AnyReference*>(reference),
      *static_cast<const lyra::value::Any*>(value));
}

auto lyra_rt_any_default(void* out) -> void* {
  return std::construct_at(static_cast<lyra::value::Any*>(out));
}
auto lyra_rt_any_reference_default(void* out) -> void* {
  return std::construct_at(static_cast<AnyReference*>(out));
}
auto lyra_rt_weak_reference_default(void* out) -> void* {
  return std::construct_at(static_cast<WeakObjectRef*>(out));
}

auto lyra_rt_any_copy(const void* value, void* out) -> void* {
  return std::construct_at(
      static_cast<lyra::value::Any*>(out),
      *static_cast<const lyra::value::Any*>(value));
}

auto lyra_rt_any_relocate(void* value, void* out) -> void* {
  return Relocate<lyra::value::Any>(value, out);
}

void lyra_rt_any_destroy(void* object) {
  std::destroy_at(static_cast<lyra::value::Any*>(object));
}

void lyra_rt_any_assign(void* storage, const void* value) {
  *static_cast<lyra::value::Any*>(storage) =
      *static_cast<const lyra::value::Any*>(value);
}

// What holds a value of any type in a variable, and a reference to one.
namespace {
using AnyValueOfAnyType = lyra::value::Any;
}  // namespace

auto lyra_rt_any_cell_refer(void* cell, void* out) -> void* {
  return lyra::runtime::ReferToCell<AnyValueOfAnyType>(cell, out);
}
auto lyra_rt_any_cell_get(void* cell) -> const void* {
  return &lyra::runtime::VariableAt<AnyValueOfAnyType>(cell).Get();
}
void lyra_rt_any_cell_initialize(void* cell, const void* prototype) noexcept {
  lyra::runtime::VariableAt<AnyValueOfAnyType>(cell).Initialize(
      lyra::runtime::Read<AnyValueOfAnyType>(prototype));
}
void lyra_rt_any_cell_set(void* cell, const void* value) {
  lyra::runtime::VariableAt<AnyValueOfAnyType>(cell).Set(
      lyra::runtime::Read<AnyValueOfAnyType>(value));
}
void lyra_rt_any_cell_arm_sampling(void* cell) {
  lyra::runtime::VariableAt<AnyValueOfAnyType>(cell).ArmSampling();
}
auto lyra_rt_any_cell_sampled_get(void* cell, const void* location) -> const
    void* {
  return &lyra::runtime::VariableAt<AnyValueOfAnyType>(cell).SampledGet(
      SourceLocationAt(location));
}
auto lyra_rt_any_cell_open_for_write(void* cell, void* out) -> void* {
  return lyra::runtime::OpenCellWrite<AnyValueOfAnyType>(cell, out);
}
void lyra_rt_any_cell_construct(void* storage) {
  lyra::runtime::BuildAt<lyra::runtime::Var<AnyValueOfAnyType>>(storage);
}
void lyra_rt_any_cell_destroy(void* storage) {
  std::destroy_at(&lyra::runtime::VariableAt<AnyValueOfAnyType>(storage));
}
auto lyra_rt_any_ref_get(void* reference) -> const void* {
  return lyra::runtime::RefGet<AnyValueOfAnyType>(reference);
}
void lyra_rt_any_ref_set(void* reference, const void* value) {
  lyra::runtime::RefSet<AnyValueOfAnyType>(reference, value);
}
void lyra_rt_any_ref_arm_sampling(void* reference) {
  lyra::runtime::RefArmSampling<AnyValueOfAnyType>(reference);
}
auto lyra_rt_any_ref_sampled_get(void* reference, const void* location) -> const
    void* {
  return lyra::runtime::RefSampledGet<AnyValueOfAnyType>(
      reference, SourceLocationAt(location));
}
auto lyra_rt_any_ref_open_for_write(void* reference, void* out) -> void* {
  return lyra::runtime::OpenRefWrite<AnyValueOfAnyType>(reference, out);
}
auto lyra_rt_any_value_cell_alloc() noexcept -> void* {
  return lyra::runtime::AllocateValueCell<AnyValueOfAnyType>();
}
void lyra_rt_any_value_cell_store(void* cell, const void* value) noexcept {
  lyra::runtime::ValueCellAt<AnyValueOfAnyType>(cell).Assign(
      lyra::runtime::Read<AnyValueOfAnyType>(value));
}
auto lyra_rt_any_value_cell_load(void* cell) noexcept -> void* {
  return &lyra::runtime::ValueCellAt<AnyValueOfAnyType>(cell).Storage();
}
void lyra_rt_any_value_cell_construct(void* storage) {
  lyra::runtime::BuildAt<lyra::runtime::ValueCell<AnyValueOfAnyType>>(storage);
}
void lyra_rt_any_value_cell_destroy(void* storage) {
  std::destroy_at(&lyra::runtime::ValueCellAt<AnyValueOfAnyType>(storage));
}
auto lyra_rt_any_shared_cell_make(void* out) -> void* {
  return lyra::runtime::MakeSharedCell<AnyValueOfAnyType>(out);
}
auto lyra_rt_any_land(const void* designation) noexcept -> void* {
  return lyra::runtime::LandDesignation<AnyValueOfAnyType>(designation);
}
void lyra_rt_any_sampled_history_construct(void* storage) {
  lyra::runtime::BuildAt<lyra::runtime::SampledHistory<AnyValueOfAnyType>>(
      storage);
}

auto lyra_rt_string_bit_identical(const void* lhs, const void* rhs) -> bool {
  return lyra::runtime::BitIdentical<String>(lhs, rhs);
}
auto lyra_rt_real_bit_identical(const void* lhs, const void* rhs) -> bool {
  return lyra::runtime::BitIdentical<Real>(lhs, rhs);
}
auto lyra_rt_shortreal_bit_identical(const void* lhs, const void* rhs) -> bool {
  return lyra::runtime::BitIdentical<ShortReal>(lhs, rhs);
}
auto lyra_rt_unmanagedref_bit_identical(const void* lhs, const void* rhs)
    -> bool {
  return lyra::runtime::BitIdentical<UnmanagedRef>(lhs, rhs);
}
auto lyra_rt_managedref_bit_identical(const void* lhs, const void* rhs)
    -> bool {
  return lyra::runtime::BitIdentical<ObjectRef>(lhs, rhs);
}

auto lyra_rt_string_has_unknown(const void* value) -> bool {
  return lyra::runtime::HasUnknown<String>(value);
}
auto lyra_rt_real_has_unknown(const void* value) -> bool {
  return lyra::runtime::HasUnknown<Real>(value);
}
auto lyra_rt_shortreal_has_unknown(const void* value) -> bool {
  return lyra::runtime::HasUnknown<ShortReal>(value);
}
auto lyra_rt_unmanagedref_has_unknown(const void* value) -> bool {
  return lyra::runtime::HasUnknown<UnmanagedRef>(value);
}
auto lyra_rt_managedref_has_unknown(const void* value) -> bool {
  return lyra::runtime::HasUnknown<ObjectRef>(value);
}

// A class handle carries no bit stream of its own, which its operations
// answer once, so these ask them rather than decide again.
auto lyra_rt_managedref_bitstream_width(
    const void* value, const void* location, void* out) -> void* {
  const lyra::base::SourceLocationScope bound(SourceLocationAt(location));
  const lyra::value::ValueOperations& operations =
      lyra::value::lyra_rt_managedref_value_operations;
  operations.functions->bitstream_width(operations, value, out);
  return out;
}

auto lyra_rt_managedref_count_bits(
    const void* value, const void* control_bits, const void* location,
    void* out) -> void* {
  const lyra::base::SourceLocationScope bound(SourceLocationAt(location));
  const lyra::value::ValueOperations& operations =
      lyra::value::lyra_rt_managedref_value_operations;
  operations.functions->count_bits(
      operations, value, Read<lyra::value::LogicVector<4>>(control_bits), out);
  return out;
}

auto lyra_rt_string_to_bitvec(
    const void* value, const void* location, void* out) -> void* {
  return Emplace(out, Read<String>(value).ToBitVec(SourceLocationAt(location)));
}
auto lyra_rt_string_from_bitvec(
    const void* run, const void* location, void* out) -> void* {
  return Emplace(
      out, String::FromBitVec(Read<BitVec>(run), SourceLocationAt(location)));
}

auto lyra_rt_bitvec_default(void* out) -> void* {
  return Emplace(out, BitVec{});
}
auto lyra_rt_bitvec_of(
    const void* bits, std::int64_t bits_width, bool bits_are_four_state,
    void* out) -> void* {
  return Emplace(
      out, BitVec::Of(BitsAt(bits, bits_width, bits_are_four_state).View()));
}
void lyra_rt_bitvec_extend(
    void* run, const void* bits, std::int64_t bits_width,
    bool bits_are_four_state) {
  static_cast<BitVec*>(run)->Extend(
      BitsAt(bits, bits_width, bits_are_four_state).View());
}
void lyra_rt_bitvec_append(void* run, const void* part) {
  static_cast<BitVec*>(run)->Append(Read<BitVec>(part));
}
void lyra_rt_bitvec_insert_bits(void* run, std::int64_t at, const void* part) {
  static_cast<BitVec*>(run)->InsertBits(at, Read<BitVec>(part));
}
auto lyra_rt_bitvec_reverse_blocks(
    const void* run, std::int64_t block, void* out) -> void* {
  return Emplace(out, Read<BitVec>(run).ReverseBlocks(block));
}
auto lyra_rt_bitvec_len(const void* run) noexcept -> std::int64_t {
  return Read<BitVec>(run).Len();
}
auto lyra_rt_bitvec_extract_bits(
    const void* run, std::int64_t at, std::int64_t count, void* out) -> void* {
  return Emplace(out, Read<BitVec>(run).ExtractBits(at, count));
}
auto lyra_rt_bitvec_load(
    const void* run, std::int64_t at, std::int64_t out_width,
    bool out_is_four_state, void* out) -> void* {
  lyra::value::WrittenWords bits(
      lyra::runtime::BitsShape(out_width, out_is_four_state));
  Read<BitVec>(run).LoadInto(
      static_cast<std::uint64_t>(at), bits.MutableView());
  bits.StoreTo(out);
  return out;
}
auto lyra_rt_bitvec_padded_to(
    const void* run, std::int64_t length, void* out) noexcept -> void* {
  return Emplace(out, Read<BitVec>(run).PaddedTo(length));
}

auto lyra_rt_laidout_size(const void* array, const void* array_type, void* out)
    -> void* {
  return Emplace(
      out, lyra::value::Int::FromInt(
               static_cast<std::int64_t>(ArrayAt(array, array_type).Count())));
}

void lyra_rt_laidout_net_initialize_tri_state(
    void* net, std::int64_t fill, std::int64_t strength) {
  NetOf<LaidOutValue>(net).InitializeTriState(fill, strength);
}

void lyra_rt_laidout_net_initialize_wired_and(
    void* net, std::int64_t fill, std::int64_t strength) {
  NetOf<LaidOutValue>(net).InitializeWiredAnd(fill, strength);
}

void lyra_rt_laidout_net_initialize_wired_or(
    void* net, std::int64_t fill, std::int64_t strength) {
  NetOf<LaidOutValue>(net).InitializeWiredOr(fill, strength);
}

void lyra_rt_laidout_net_initialize_retaining(
    void* net, std::int64_t fill, std::int64_t strength) {
  NetOf<LaidOutValue>(net).InitializeRetaining(fill, strength);
}

void lyra_rt_laidout_unresolved_net_initialize(
    void* net, const void* prototype) {
  UnresolvedNet<LaidOutValue>& unresolved = UnresolvedNetOf<LaidOutValue>(net);
  unresolved.InitializeUnresolved(unresolved.ToOwned(prototype));
}

auto lyra_rt_laidout_resolved_net_begin_takeover(
    void* net, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) -> std::int64_t {
  return NetOf<LaidOutValue>(net).BeginTakeover(
      level, first, count, SourceLocationAt(location));
}

auto lyra_rt_laidout_resolved_net_drive_takeover(
    void* net, std::int64_t level, std::int64_t generation, const void* value,
    const void* location) -> bool {
  return NetOf<LaidOutValue>(net).DriveTakeoverBytes(
      level, generation, value, SourceLocationAt(location));
}

void lyra_rt_laidout_resolved_net_end_takeover(
    void* net, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) {
  NetOf<LaidOutValue>(net).EndTakeover(
      level, first, count, SourceLocationAt(location));
}

void lyra_rt_laidout_resolved_net_arm_sampling(void* net) {
  NetOf<LaidOutValue>(net).ArmSampling();
}

auto lyra_rt_laidout_resolved_net_sampled_get(void* net, const void* location)
    -> const void* {
  return HandleTo(
      NetOf<LaidOutValue>(net).SampledGet(SourceLocationAt(location)));
}

auto lyra_rt_laidout_unresolved_net_begin_takeover(
    void* net, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location, const void* value_take_positions) -> std::int64_t {
  return UnresolvedNetOf<LaidOutValue>(net).BeginTakeover(
      level, first, count, SourceLocationAt(location),
      lyra::runtime::TakePositionsAt(value_take_positions));
}

auto lyra_rt_laidout_unresolved_net_drive_takeover(
    void* net, std::int64_t level, std::int64_t generation, const void* value,
    const void* location) -> bool {
  return UnresolvedNetOf<LaidOutValue>(net).DriveTakeoverBytes(
      level, generation, value, SourceLocationAt(location));
}

void lyra_rt_laidout_unresolved_net_end_takeover(
    void* net, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) {
  UnresolvedNetOf<LaidOutValue>(net).EndTakeover(
      level, first, count, SourceLocationAt(location));
}

void lyra_rt_laidout_unresolved_net_arm_sampling(void* net) {
  UnresolvedNetOf<LaidOutValue>(net).ArmSampling();
}

auto lyra_rt_laidout_unresolved_net_sampled_get(void* net, const void* location)
    -> const void* {
  return HandleTo(
      UnresolvedNetOf<LaidOutValue>(net).SampledGet(
          SourceLocationAt(location)));
}

// Some positions of one net and of another stated to be one physical net. The
// other net is of whatever type its own unit gave it, which this side never
// learns, and what the two share is positions, which carry no type.
void lyra_rt_net_join(
    void* net, void* other, std::int64_t here, std::int64_t there,
    std::int64_t count, std::int64_t here_reached_first,
    std::int64_t here_reached_count, std::int64_t there_reached_first,
    std::int64_t there_reached_count, const void* location) {
  static_cast<lyra::runtime::ConnectableNet*>(net)->Join(
      static_cast<lyra::runtime::ConnectableNet*>(other), here, there, count,
      here_reached_first, here_reached_count, there_reached_first,
      there_reached_count, SourceLocationAt(location));
}

auto lyra_rt_attach_driver(
    void* net, std::int64_t strength, std::int64_t first, std::int64_t count,
    std::int64_t reached_first, std::int64_t reached_count,
    const void* location) -> void* {
  return &static_cast<lyra::runtime::ConnectableNet*>(net)->AttachDriverSlot(
      strength, first, count, reached_first, reached_count,
      SourceLocationAt(location));
}

auto lyra_rt_laidout_attach_whole_driver(void* net, const void* location)
    -> void* {
  return &UnresolvedNetOf<LaidOutValue>(net).AttachWholeDriver(
      SourceLocationAt(location));
}

auto lyra_rt_laidout_driver_get(void* driver) -> const void* {
  return DriverOf<LaidOutValue>(driver).Bytes();
}

void lyra_rt_laidout_driver_set(void* driver, const void* value) {
  DriverOf<LaidOutValue>(driver).SetBytes(value);
}

void lyra_rt_string_unresolved_net_initialize(
    void* net, const void* prototype) {
  UnresolvedNetOf<String>(net).InitializeUnresolved(Read<String>(prototype));
}
auto lyra_rt_string_unresolved_net_begin_takeover(
    void* net, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) -> std::int64_t {
  return UnresolvedNetOf<String>(net).BeginTakeover(
      level, first, count, SourceLocationAt(location));
}
auto lyra_rt_string_unresolved_net_drive_takeover(
    void* net, std::int64_t level, std::int64_t generation, const void* value,
    const void* location) -> bool {
  return UnresolvedNetOf<String>(net).DriveTakeover(
      level, generation, Read<String>(value), SourceLocationAt(location));
}
void lyra_rt_string_unresolved_net_end_takeover(
    void* net, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) {
  UnresolvedNetOf<String>(net).EndTakeover(
      level, first, count, SourceLocationAt(location));
}
void lyra_rt_string_unresolved_net_arm_sampling(void* net) {
  UnresolvedNetOf<String>(net).ArmSampling();
}
auto lyra_rt_string_unresolved_net_sampled_get(void* net, const void* location)
    -> const void* {
  return &UnresolvedNetOf<String>(net).SampledGet(SourceLocationAt(location));
}
auto lyra_rt_string_attach_whole_driver(void* net, const void* location)
    -> void* {
  return &UnresolvedNetOf<String>(net).AttachWholeDriver(
      SourceLocationAt(location));
}
auto lyra_rt_string_driver_get(void* driver) -> const void* {
  return &DriverOf<String>(driver).Get();
}
void lyra_rt_string_driver_set(void* driver, const void* value) {
  DriverOf<String>(driver).Set(Read<String>(value));
}
void lyra_rt_real_unresolved_net_initialize(void* net, const void* prototype) {
  UnresolvedNetOf<Real>(net).InitializeUnresolved(Read<Real>(prototype));
}
auto lyra_rt_real_unresolved_net_begin_takeover(
    void* net, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) -> std::int64_t {
  return UnresolvedNetOf<Real>(net).BeginTakeover(
      level, first, count, SourceLocationAt(location));
}
auto lyra_rt_real_unresolved_net_drive_takeover(
    void* net, std::int64_t level, std::int64_t generation, const void* value,
    const void* location) -> bool {
  return UnresolvedNetOf<Real>(net).DriveTakeover(
      level, generation, Read<Real>(value), SourceLocationAt(location));
}
void lyra_rt_real_unresolved_net_end_takeover(
    void* net, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) {
  UnresolvedNetOf<Real>(net).EndTakeover(
      level, first, count, SourceLocationAt(location));
}
void lyra_rt_real_unresolved_net_arm_sampling(void* net) {
  UnresolvedNetOf<Real>(net).ArmSampling();
}
auto lyra_rt_real_unresolved_net_sampled_get(void* net, const void* location)
    -> const void* {
  return &UnresolvedNetOf<Real>(net).SampledGet(SourceLocationAt(location));
}
auto lyra_rt_real_attach_whole_driver(void* net, const void* location)
    -> void* {
  return &UnresolvedNetOf<Real>(net).AttachWholeDriver(
      SourceLocationAt(location));
}
auto lyra_rt_real_driver_get(void* driver) -> const void* {
  return &DriverOf<Real>(driver).Get();
}
void lyra_rt_real_driver_set(void* driver, const void* value) {
  DriverOf<Real>(driver).Set(Read<Real>(value));
}
void lyra_rt_shortreal_unresolved_net_initialize(
    void* net, const void* prototype) {
  UnresolvedNetOf<ShortReal>(net).InitializeUnresolved(
      Read<ShortReal>(prototype));
}
auto lyra_rt_shortreal_unresolved_net_begin_takeover(
    void* net, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) -> std::int64_t {
  return UnresolvedNetOf<ShortReal>(net).BeginTakeover(
      level, first, count, SourceLocationAt(location));
}
auto lyra_rt_shortreal_unresolved_net_drive_takeover(
    void* net, std::int64_t level, std::int64_t generation, const void* value,
    const void* location) -> bool {
  return UnresolvedNetOf<ShortReal>(net).DriveTakeover(
      level, generation, Read<ShortReal>(value), SourceLocationAt(location));
}
void lyra_rt_shortreal_unresolved_net_end_takeover(
    void* net, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) {
  UnresolvedNetOf<ShortReal>(net).EndTakeover(
      level, first, count, SourceLocationAt(location));
}
void lyra_rt_shortreal_unresolved_net_arm_sampling(void* net) {
  UnresolvedNetOf<ShortReal>(net).ArmSampling();
}
auto lyra_rt_shortreal_unresolved_net_sampled_get(
    void* net, const void* location) -> const void* {
  return &UnresolvedNetOf<ShortReal>(net).SampledGet(
      SourceLocationAt(location));
}
auto lyra_rt_shortreal_attach_whole_driver(void* net, const void* location)
    -> void* {
  return &UnresolvedNetOf<ShortReal>(net).AttachWholeDriver(
      SourceLocationAt(location));
}
auto lyra_rt_shortreal_driver_get(void* driver) -> const void* {
  return &DriverOf<ShortReal>(driver).Get();
}
void lyra_rt_shortreal_driver_set(void* driver, const void* value) {
  DriverOf<ShortReal>(driver).Set(Read<ShortReal>(value));
}
void lyra_rt_managedref_unresolved_net_initialize(
    void* net, const void* prototype) {
  UnresolvedNetOf<ObjectRef>(net).InitializeUnresolved(
      Read<ObjectRef>(prototype));
}
auto lyra_rt_managedref_unresolved_net_begin_takeover(
    void* net, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) -> std::int64_t {
  return UnresolvedNetOf<ObjectRef>(net).BeginTakeover(
      level, first, count, SourceLocationAt(location));
}
auto lyra_rt_managedref_unresolved_net_drive_takeover(
    void* net, std::int64_t level, std::int64_t generation, const void* value,
    const void* location) -> bool {
  return UnresolvedNetOf<ObjectRef>(net).DriveTakeover(
      level, generation, Read<ObjectRef>(value), SourceLocationAt(location));
}
void lyra_rt_managedref_unresolved_net_end_takeover(
    void* net, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) {
  UnresolvedNetOf<ObjectRef>(net).EndTakeover(
      level, first, count, SourceLocationAt(location));
}
void lyra_rt_managedref_unresolved_net_arm_sampling(void* net) {
  UnresolvedNetOf<ObjectRef>(net).ArmSampling();
}
auto lyra_rt_managedref_unresolved_net_sampled_get(
    void* net, const void* location) -> const void* {
  return &UnresolvedNetOf<ObjectRef>(net).SampledGet(
      SourceLocationAt(location));
}
auto lyra_rt_managedref_attach_whole_driver(void* net, const void* location)
    -> void* {
  return &UnresolvedNetOf<ObjectRef>(net).AttachWholeDriver(
      SourceLocationAt(location));
}
auto lyra_rt_managedref_driver_get(void* driver) -> const void* {
  return &DriverOf<ObjectRef>(driver).Get();
}
void lyra_rt_managedref_driver_set(void* driver, const void* value) {
  DriverOf<ObjectRef>(driver).Set(Read<ObjectRef>(value));
}

auto lyra_rt_assoc_construct(
    std::int64_t kind, const void* index, const void* element,
    void* out) noexcept -> void* {
  return std::construct_at(
      static_cast<lyra::value::AssociativeArrayCore*>(out),
      lyra::value::AssociativeArrayConstruct(
          kind, &lyra::runtime::TypeAt(index),
          &lyra::runtime::TypeAt(element)));
}
auto lyra_rt_assoc_find(const void* array, const void* index) noexcept
    -> void* {
  return lyra::value::AssociativeArrayFind(AssociativeCoreAt(array), index);
}
auto lyra_rt_assoc_prepare(const void* array) noexcept -> void* {
  return lyra::value::AssociativeArrayPrepare(AssociativeCoreAt(array));
}
auto lyra_rt_assoc_index_of(const void* array, void* element) noexcept
    -> void* {
  return lyra::value::AssociativeArrayIndexOf(
      AssociativeCoreAt(array), element);
}
void lyra_rt_assoc_commit(void* array, void* element) noexcept {
  lyra::value::AssociativeArrayCommit(AssociativeCoreAt(array), element);
}
void lyra_rt_assoc_remove(void* array, void* element) noexcept {
  lyra::value::AssociativeArrayRemove(AssociativeCoreAt(array), element);
}
void lyra_rt_assoc_clear(void* array) noexcept {
  lyra::value::AssociativeArrayClear(AssociativeCoreAt(array));
}
auto lyra_rt_assoc_core_first(const void* array) noexcept -> void* {
  return lyra::value::AssociativeArrayFirst(AssociativeCoreAt(array));
}
auto lyra_rt_assoc_core_last(const void* array) noexcept -> void* {
  return lyra::value::AssociativeArrayLast(AssociativeCoreAt(array));
}
auto lyra_rt_assoc_core_next(const void* array, const void* element) noexcept
    -> void* {
  return lyra::value::AssociativeArrayNext(AssociativeCoreAt(array), element);
}
auto lyra_rt_assoc_core_previous(
    const void* array, const void* element) noexcept -> void* {
  return lyra::value::AssociativeArrayPrevious(
      AssociativeCoreAt(array), element);
}
auto lyra_rt_assoc_after(const void* array, const void* index) noexcept
    -> void* {
  return lyra::value::AssociativeArrayAfter(AssociativeCoreAt(array), index);
}
auto lyra_rt_assoc_before(const void* array, const void* index) noexcept
    -> void* {
  return lyra::value::AssociativeArrayBefore(AssociativeCoreAt(array), index);
}
auto lyra_rt_assoc_prepare_default(const void* array) noexcept -> void* {
  return lyra::value::AssociativeArrayPrepareDefault(AssociativeCoreAt(array));
}
void lyra_rt_assoc_set_default(void* array, void* element) noexcept {
  lyra::value::AssociativeArraySetDefault(AssociativeCoreAt(array), element);
}
auto lyra_rt_assoc_core_stated_default(const void* array) noexcept -> const
    void* {
  return lyra::value::AssociativeArrayStatedDefault(AssociativeCoreAt(array));
}
void lyra_rt_assoc_end(void* array) noexcept {
  lyra::value::AssociativeArrayEnd(AssociativeCoreAt(array));
}
void lyra_rt_assoc_adopt(void* array, void* from) noexcept {
  lyra::value::AssociativeArrayAdopt(
      AssociativeCoreAt(array), AssociativeCoreAt(from));
}
auto lyra_rt_wildcard_index_has_unknown(const void* index) noexcept -> bool {
  return lyra::value::WildcardIndexHasUnknown(
      *static_cast<const lyra::value::AnyValue*>(index));
}

auto lyra_rt_read_mem(
    void* runtime, const void* name, void* dims, std::uint64_t dims_count,
    std::int64_t base, std::int64_t start, void* out) -> void* {
  return lyra::runtime::MemoryRequest(
      runtime, lyra::runtime::AnyNumber{}, name, dims, dims_count, base, start,
      std::nullopt, out);
}
auto lyra_rt_read_mem_within(
    void* runtime, const void* name, void* dims, std::uint64_t dims_count,
    std::int64_t base, std::int64_t start, std::int64_t finish, void* out)
    -> void* {
  return lyra::runtime::MemoryRequest(
      runtime, lyra::runtime::AnyNumber{}, name, dims, dims_count, base, start,
      finish, out);
}
auto lyra_rt_read_enumerated_mem(
    void* runtime, const void* enumeration, const void* name, void* dims,
    std::uint64_t dims_count, std::int64_t base, std::int64_t start, void* out)
    -> void* {
  return lyra::runtime::MemoryRequest(
      runtime, lyra::runtime::MemberValues{Read<Enumeration>(enumeration)},
      name, dims, dims_count, base, start, std::nullopt, out);
}
auto lyra_rt_read_enumerated_mem_within(
    void* runtime, const void* enumeration, const void* name, void* dims,
    std::uint64_t dims_count, std::int64_t base, std::int64_t start,
    std::int64_t finish, void* out) -> void* {
  return lyra::runtime::MemoryRequest(
      runtime, lyra::runtime::MemberValues{Read<Enumeration>(enumeration)},
      name, dims, dims_count, base, start, finish, out);
}
auto lyra_rt_write_mem(
    void* runtime, const void* name, void* dims, std::uint64_t dims_count,
    std::int64_t base, std::int64_t start, void* out) -> void* {
  return lyra::runtime::MemoryRequest(
      runtime, lyra::runtime::AnyNumber{}, name, dims, dims_count, base, start,
      std::nullopt, out);
}
auto lyra_rt_write_mem_within(
    void* runtime, const void* name, void* dims, std::uint64_t dims_count,
    std::int64_t base, std::int64_t start, std::int64_t finish, void* out)
    -> void* {
  return lyra::runtime::MemoryRequest(
      runtime, lyra::runtime::AnyNumber{}, name, dims, dims_count, base, start,
      finish, out);
}

void lyra_rt_memory_words_read_grid(void* words, const void* location) {
  lyra::runtime::WordsAt(words).ReadGrid(SourceLocationAt(location));
}
void lyra_rt_memory_words_read_dense(
    void* words, std::int64_t count, const void* location) {
  lyra::runtime::WordsAt(words).ReadDense(count, SourceLocationAt(location));
}
void lyra_rt_memory_words_read_keyed(void* words, const void* location) {
  lyra::runtime::WordsAt(words).ReadKeyed(SourceLocationAt(location));
}
void lyra_rt_memory_words_plan_grid(void* words) {
  lyra::runtime::WordsAt(words).PlanGrid();
}
void lyra_rt_memory_words_plan_dense(void* words, std::int64_t count) {
  lyra::runtime::WordsAt(words).PlanDense(count);
}
auto lyra_rt_memory_words_count(const void* words) noexcept -> std::int64_t {
  return lyra::runtime::WordsAt(words).Count();
}
auto lyra_rt_memory_words_ordinal(
    const void* words, std::int64_t k, std::int64_t dimension) noexcept
    -> std::int64_t {
  return lyra::runtime::WordsAt(words).Ordinal(k, dimension);
}
auto lyra_rt_memory_words_key(const void* words, std::int64_t k) noexcept
    -> std::int64_t {
  return lyra::runtime::WordsAt(words).Key(k);
}
auto lyra_rt_memory_words_bytes_read(const void* words) noexcept
    -> std::int64_t {
  return lyra::runtime::WordsAt(words).BytesRead();
}
auto lyra_rt_memory_words_word(
    const void* words, std::int64_t k, const void* answer_type, void* out)
    -> void* {
  lyra::value::WrittenWords word(
      lyra::runtime::IntegralTypeAt(answer_type).Shape());
  lyra::runtime::WordsAt(words).Word(k, word.MutableView());
  word.StoreTo(out);
  return out;
}
void lyra_rt_memory_words_put(
    void* words, std::int64_t k, const void* word, const void* word_type) {
  const lyra::value::LoadedWords held =
      lyra::runtime::IntegralTypeAt(word_type).Load(word);
  lyra::runtime::WordsAt(words).Put(k, held.View());
}
void lyra_rt_memory_words_put_keyed(
    void* words, std::int64_t key, const void* word, const void* word_type) {
  const lyra::value::LoadedWords held =
      lyra::runtime::IntegralTypeAt(word_type).Load(word);
  lyra::runtime::WordsAt(words).PutKeyed(key, held.View());
}
void lyra_rt_memory_words_commit(void* words, const void* location) {
  lyra::runtime::WordsAt(words).Commit(SourceLocationAt(location));
}

auto lyra_rt_format_runtime(
    const void* format, void* args, std::uint64_t args_count,
    const void* scope_path, const void* library_binding,
    const void* time_format, std::int64_t timeunit_power, const void* location,
    void* out) -> void* {
  const std::span<const void* const> handles =
      SpanAt<const void* const>(args, args_count);
  std::vector<FormatArg> arguments(handles.size());
  std::ranges::transform(handles, arguments.begin(), [](const void* handle) {
    return *static_cast<const FormatArg*>(handle);
  });
  return Emplace(
      out, lyra::value::FormatRuntime(
               Read<String>(format), arguments, Read<String>(scope_path),
               Read<String>(library_binding), Read<TimeFormat>(time_format),
               timeunit_power, SourceLocationAt(location)));
}

auto lyra_rt_integral_format_arg_make(
    const void* value, std::int64_t value_width, bool value_is_signed,
    bool value_is_four_state, void* out) -> void* {
  return Emplace(
      out, lyra::runtime::IntegralFormatArg(
               value, lyra::runtime::NumberShape(
                          value_width, value_is_signed, value_is_four_state)));
}

auto lyra_rt_string_format_arg_make(const void* value, void* out) -> void* {
  return Emplace(out, MakeFormatArg(Read<String>(value)));
}

auto lyra_rt_patterned_format_arg_make(
    const void* value, std::int64_t value_width, bool value_is_signed,
    bool value_is_four_state, const void* pattern, void* out) -> void* {
  FormatArg arg = lyra::runtime::IntegralFormatArg(
      value, lyra::runtime::NumberShape(
                 value_width, value_is_signed, value_is_four_state));
  arg.pattern = &Read<String>(pattern);
  return Emplace(out, arg);
}

auto lyra_rt_rendered_format_arg_make(const void* pattern, void* out) -> void* {
  return Emplace(out, FormatArg::Rendered(Read<String>(pattern)));
}

auto lyra_rt_unmanagedref_format_arg_make(const void* value, void* out)
    -> void* {
  return Emplace(out, MakeFormatArg(Read<UnmanagedRef>(value)));
}

auto lyra_rt_managedref_format_arg_make(const void* value, void* out) -> void* {
  return Emplace(out, MakeFormatArg(Read<ObjectRef>(value)));
}

auto lyra_rt_dpi_bit_buffer_make(
    const void* sv, std::int64_t sv_width, bool sv_is_four_state, void* out)
    -> void* {
  const lyra::value::LoadedWords words = BitsAt(sv, sv_width, sv_is_four_state);
  return Emplace(out, DpiBitBuffer(words.View()));
}

auto lyra_rt_dpi_logic_buffer_make(
    const void* sv, std::int64_t sv_width, bool sv_is_four_state, void* out)
    -> void* {
  const lyra::value::LoadedWords words = BitsAt(sv, sv_width, sv_is_four_state);
  return Emplace(out, DpiLogicBuffer(words.View()));
}

auto lyra_rt_dpi_bit_buffer_data(void* buffer) -> void* {
  return static_cast<DpiBitBuffer*>(buffer)->Data();
}

auto lyra_rt_dpi_logic_buffer_data(void* buffer) -> void* {
  return static_cast<DpiLogicBuffer*>(buffer)->Data();
}

void lyra_rt_write_canonical_bit_vec(
    void* dst, const void* sv, std::int64_t sv_width, bool sv_is_four_state) {
  const lyra::value::LoadedWords words = BitsAt(sv, sv_width, sv_is_four_state);
  lyra::value::WriteCanonicalBitVec(
      static_cast<svBitVecVal*>(dst), words.View());
}

void lyra_rt_write_canonical_logic_vec(
    void* dst, const void* sv, std::int64_t sv_width, bool sv_is_four_state) {
  const lyra::value::LoadedWords words = BitsAt(sv, sv_width, sv_is_four_state);
  lyra::value::WriteCanonicalLogicVec(
      static_cast<svLogicVecVal*>(dst), words.View());
}

auto lyra_rt_to_sv_logic(
    const void* sv, std::int64_t sv_width, bool sv_is_four_state)
    -> std::uint8_t {
  const lyra::value::LoadedWords words = BitsAt(sv, sv_width, sv_is_four_state);
  return lyra::value::ToSvLogic(words.View());
}

// The image takes the actual erased, because it is element-type-independent
// (Annex H.7.3) and nothing here could read that representation off anything
// else. What the image needs of the actual's type is the type of one element,
// which the actual's own type reaches under its unpacked layers.
auto lyra_rt_dpi_open_array_make(
    void* bounds, std::uint64_t bounds_count, std::int64_t element_width,
    bool four_state, bool addressable_elements, void* out) -> void* {
  return std::construct_at(
      static_cast<DpiOpenArray*>(out),
      SpanAt<const std::int64_t>(bounds, bounds_count),
      static_cast<std::uint64_t>(element_width), four_state,
      addressable_elements);
}

auto lyra_rt_dpi_open_array_handle(void* image) -> void* {
  return static_cast<DpiOpenArray*>(image)->Handle();
}

auto lyra_rt_dpi_open_array_element_count(const void* image) noexcept
    -> std::int64_t {
  return static_cast<const DpiOpenArray*>(image)->ElementCount();
}
auto lyra_rt_dpi_open_array_ordinal(
    const void* image, std::int64_t k, std::int64_t dimension) noexcept
    -> std::int64_t {
  return static_cast<const DpiOpenArray*>(image)->Ordinal(k, dimension);
}
void lyra_rt_dpi_open_array_put(
    void* image, std::int64_t k, const void* element,
    const void* element_type) {
  const lyra::value::LoadedWords held =
      lyra::runtime::IntegralTypeAt(element_type).Load(element);
  static_cast<DpiOpenArray*>(image)->Put(k, held.View());
}
auto lyra_rt_dpi_open_array_word(
    const void* image, std::int64_t k, const void* answer_type, void* out)
    -> void* {
  lyra::value::WrittenWords word(
      lyra::runtime::IntegralTypeAt(answer_type).Shape());
  static_cast<const DpiOpenArray*>(image)->Word(k, word.MutableView());
  word.StoreTo(out);
  return out;
}

// Opening a write into the storage a wrapper stands for (LRM 11.5.1), in the
// storage the writing body gave. The body reaches the wrapper's contents
// through what this builds, writes the parts it writes where they lie, and
// ends it once the write is over, which is when the wrapper learns what the
// write did.
auto lyra_rt_string_cell_open_for_write(void* cell, void* out) -> void* {
  return OpenCellWrite<String>(cell, out);
}
auto lyra_rt_bitvec_cell_open_for_write(void* cell, void* out) -> void* {
  return OpenCellWrite<BitVec>(cell, out);
}
auto lyra_rt_real_cell_open_for_write(void* cell, void* out) -> void* {
  return OpenCellWrite<Real>(cell, out);
}
auto lyra_rt_shortreal_cell_open_for_write(void* cell, void* out) -> void* {
  return OpenCellWrite<ShortReal>(cell, out);
}
auto lyra_rt_unmanagedref_cell_open_for_write(void* cell, void* out) -> void* {
  return OpenCellWrite<UnmanagedRef>(cell, out);
}
auto lyra_rt_managedref_cell_open_for_write(void* cell, void* out) -> void* {
  return OpenCellWrite<ObjectRef>(cell, out);
}
auto lyra_rt_laidout_cell_open_for_write(void* cell, void* out) -> void* {
  return OpenTailCellWrite<LaidOutValue>(cell, out);
}

auto lyra_rt_string_ref_open_for_write(void* reference, void* out) -> void* {
  return OpenRefWrite<String>(reference, out);
}
auto lyra_rt_real_ref_open_for_write(void* reference, void* out) -> void* {
  return OpenRefWrite<Real>(reference, out);
}
auto lyra_rt_shortreal_ref_open_for_write(void* reference, void* out) -> void* {
  return OpenRefWrite<ShortReal>(reference, out);
}
auto lyra_rt_unmanagedref_ref_open_for_write(void* reference, void* out)
    -> void* {
  return OpenRefWrite<UnmanagedRef>(reference, out);
}
auto lyra_rt_managedref_ref_open_for_write(void* reference, void* out)
    -> void* {
  return OpenRefWrite<ObjectRef>(reference, out);
}
auto lyra_rt_laidout_ref_open_for_write(
    void* reference, const void* value_type, void* out) -> void* {
  return OpenTailRefWrite<LaidOutValue>(
      reference, lyra::runtime::TypeAt(value_type), out);
}

auto lyra_rt_laidout_driver_open_for_write(void* driver, void* out) -> void* {
  return OpenTailDriverWrite<LaidOutValue>(driver, out);
}

auto lyra_rt_string_driver_open_for_write(void* driver, void* out) -> void* {
  return OpenDriverWrite<String>(driver, out);
}
auto lyra_rt_real_driver_open_for_write(void* driver, void* out) -> void* {
  return OpenDriverWrite<Real>(driver, out);
}
auto lyra_rt_shortreal_driver_open_for_write(void* driver, void* out) -> void* {
  return OpenDriverWrite<ShortReal>(driver, out);
}
auto lyra_rt_managedref_driver_open_for_write(void* driver, void* out)
    -> void* {
  return OpenDriverWrite<ObjectRef>(driver, out);
}
void lyra_rt_bit8_resolved_net_drive_from_variable(void* net, void* cell) {
  DriveFromVariable<Bit8>(net, cell);
}
void lyra_rt_bit16_resolved_net_drive_from_variable(void* net, void* cell) {
  DriveFromVariable<Bit16>(net, cell);
}
void lyra_rt_bit32_resolved_net_drive_from_variable(void* net, void* cell) {
  DriveFromVariable<Bit32>(net, cell);
}
void lyra_rt_bit64_resolved_net_drive_from_variable(void* net, void* cell) {
  DriveFromVariable<Bit64>(net, cell);
}
void lyra_rt_bit_wide_resolved_net_drive_from_variable(void* net, void* cell) {
  DriveFromVariable<BitWide>(net, cell);
}
void lyra_rt_logic8_resolved_net_drive_from_variable(void* net, void* cell) {
  DriveFromVariable<Logic8>(net, cell);
}
void lyra_rt_logic16_resolved_net_drive_from_variable(void* net, void* cell) {
  DriveFromVariable<Logic16>(net, cell);
}
void lyra_rt_logic32_resolved_net_drive_from_variable(void* net, void* cell) {
  DriveFromVariable<Logic32>(net, cell);
}
void lyra_rt_logic64_resolved_net_drive_from_variable(void* net, void* cell) {
  DriveFromVariable<Logic64>(net, cell);
}
void lyra_rt_logic_wide_resolved_net_drive_from_variable(
    void* net, void* cell) {
  DriveFromVariable<LogicWide>(net, cell);
}
void lyra_rt_string_unresolved_net_drive_from_variable(void* net, void* cell) {
  DriveFromVariable<String>(net, cell);
}
void lyra_rt_real_unresolved_net_drive_from_variable(void* net, void* cell) {
  DriveFromVariable<Real>(net, cell);
}
void lyra_rt_shortreal_unresolved_net_drive_from_variable(
    void* net, void* cell) {
  DriveFromVariable<ShortReal>(net, cell);
}
void lyra_rt_managedref_unresolved_net_drive_from_variable(
    void* net, void* cell) {
  DriveFromVariable<ObjectRef>(net, cell);
}
void lyra_rt_laidout_resolved_net_drive_from_variable(void* net, void* cell) {
  DriveFromVariable<LaidOutValue>(net, cell);
}
void lyra_rt_laidout_unresolved_net_drive_from_variable(void* net, void* cell) {
  DriveFromVariable<LaidOutValue>(net, cell);
}

void lyra_rt_bit8_resolved_net_drive_from_net(void* net, void* source) {
  DriveFromNet<ResolvedNet<Bit8>>(net, source);
}
void lyra_rt_bit16_resolved_net_drive_from_net(void* net, void* source) {
  DriveFromNet<ResolvedNet<Bit16>>(net, source);
}
void lyra_rt_bit32_resolved_net_drive_from_net(void* net, void* source) {
  DriveFromNet<ResolvedNet<Bit32>>(net, source);
}
void lyra_rt_bit64_resolved_net_drive_from_net(void* net, void* source) {
  DriveFromNet<ResolvedNet<Bit64>>(net, source);
}
void lyra_rt_bit_wide_resolved_net_drive_from_net(void* net, void* source) {
  DriveFromNet<ResolvedNet<BitWide>>(net, source);
}
void lyra_rt_logic8_resolved_net_drive_from_net(void* net, void* source) {
  DriveFromNet<ResolvedNet<Logic8>>(net, source);
}
void lyra_rt_logic16_resolved_net_drive_from_net(void* net, void* source) {
  DriveFromNet<ResolvedNet<Logic16>>(net, source);
}
void lyra_rt_logic32_resolved_net_drive_from_net(void* net, void* source) {
  DriveFromNet<ResolvedNet<Logic32>>(net, source);
}
void lyra_rt_logic64_resolved_net_drive_from_net(void* net, void* source) {
  DriveFromNet<ResolvedNet<Logic64>>(net, source);
}
void lyra_rt_logic_wide_resolved_net_drive_from_net(void* net, void* source) {
  DriveFromNet<ResolvedNet<LogicWide>>(net, source);
}
void lyra_rt_string_unresolved_net_drive_from_net(void* net, void* source) {
  DriveFromNet<UnresolvedNet<String>>(net, source);
}
void lyra_rt_real_unresolved_net_drive_from_net(void* net, void* source) {
  DriveFromNet<UnresolvedNet<Real>>(net, source);
}
void lyra_rt_shortreal_unresolved_net_drive_from_net(void* net, void* source) {
  DriveFromNet<UnresolvedNet<ShortReal>>(net, source);
}
void lyra_rt_managedref_unresolved_net_drive_from_net(void* net, void* source) {
  DriveFromNet<UnresolvedNet<ObjectRef>>(net, source);
}
void lyra_rt_laidout_resolved_net_drive_from_net(void* net, void* source) {
  DriveFromNet<ResolvedNet<LaidOutValue>>(net, source);
}
void lyra_rt_laidout_unresolved_net_drive_from_net(void* net, void* source) {
  DriveFromNet<UnresolvedNet<LaidOutValue>>(net, source);
}

void lyra_rt_drive_from_reference(void* net, void* reference) {
  DriveFromReference(net, reference);
}
auto lyra_rt_show_source(void* net) -> bool {
  return NetShowsSource(net);
}

auto lyra_rt_bit8_cell_reports_to(void* cell) -> void* {
  return CellReportsTo<Bit8>(cell);
}
auto lyra_rt_bit16_cell_reports_to(void* cell) -> void* {
  return CellReportsTo<Bit16>(cell);
}
auto lyra_rt_bit32_cell_reports_to(void* cell) -> void* {
  return CellReportsTo<Bit32>(cell);
}
auto lyra_rt_bit64_cell_reports_to(void* cell) -> void* {
  return CellReportsTo<Bit64>(cell);
}
auto lyra_rt_bit_wide_cell_reports_to(void* cell) -> void* {
  return CellReportsTo<BitWide>(cell);
}
auto lyra_rt_logic8_cell_reports_to(void* cell) -> void* {
  return CellReportsTo<Logic8>(cell);
}
auto lyra_rt_logic16_cell_reports_to(void* cell) -> void* {
  return CellReportsTo<Logic16>(cell);
}
auto lyra_rt_logic32_cell_reports_to(void* cell) -> void* {
  return CellReportsTo<Logic32>(cell);
}
auto lyra_rt_logic64_cell_reports_to(void* cell) -> void* {
  return CellReportsTo<Logic64>(cell);
}
auto lyra_rt_logic_wide_cell_reports_to(void* cell) -> void* {
  return CellReportsTo<LogicWide>(cell);
}
auto lyra_rt_string_cell_reports_to(void* cell) -> void* {
  return CellReportsTo<String>(cell);
}
auto lyra_rt_real_cell_reports_to(void* cell) -> void* {
  return CellReportsTo<Real>(cell);
}
auto lyra_rt_shortreal_cell_reports_to(void* cell) -> void* {
  return CellReportsTo<ShortReal>(cell);
}
auto lyra_rt_unmanagedref_cell_reports_to(void* cell) -> void* {
  return CellReportsTo<UnmanagedRef>(cell);
}
auto lyra_rt_managedref_cell_reports_to(void* cell) -> void* {
  return CellReportsTo<ObjectRef>(cell);
}
auto lyra_rt_laidout_cell_reports_to(void* cell) -> void* {
  return CellReportsTo<LaidOutValue>(cell);
}
auto lyra_rt_any_cell_reports_to(void* cell) -> void* {
  return CellReportsTo<lyra::value::Any>(cell);
}
auto lyra_rt_bit8_ref_reports_to(const void* reference) -> void* {
  return RefReportsTo(reference);
}
auto lyra_rt_bit16_ref_reports_to(const void* reference) -> void* {
  return RefReportsTo(reference);
}
auto lyra_rt_bit32_ref_reports_to(const void* reference) -> void* {
  return RefReportsTo(reference);
}
auto lyra_rt_bit64_ref_reports_to(const void* reference) -> void* {
  return RefReportsTo(reference);
}
auto lyra_rt_bit_wide_ref_reports_to(const void* reference) -> void* {
  return RefReportsTo(reference);
}
auto lyra_rt_logic8_ref_reports_to(const void* reference) -> void* {
  return RefReportsTo(reference);
}
auto lyra_rt_logic16_ref_reports_to(const void* reference) -> void* {
  return RefReportsTo(reference);
}
auto lyra_rt_logic32_ref_reports_to(const void* reference) -> void* {
  return RefReportsTo(reference);
}
auto lyra_rt_logic64_ref_reports_to(const void* reference) -> void* {
  return RefReportsTo(reference);
}
auto lyra_rt_logic_wide_ref_reports_to(const void* reference) -> void* {
  return RefReportsTo(reference);
}
auto lyra_rt_string_ref_reports_to(const void* reference) -> void* {
  return RefReportsTo(reference);
}
auto lyra_rt_real_ref_reports_to(const void* reference) -> void* {
  return RefReportsTo(reference);
}
auto lyra_rt_shortreal_ref_reports_to(const void* reference) -> void* {
  return RefReportsTo(reference);
}
auto lyra_rt_unmanagedref_ref_reports_to(const void* reference) -> void* {
  return RefReportsTo(reference);
}
auto lyra_rt_managedref_ref_reports_to(const void* reference) -> void* {
  return RefReportsTo(reference);
}
auto lyra_rt_laidout_ref_reports_to(const void* reference) -> void* {
  return RefReportsTo(reference);
}
auto lyra_rt_any_ref_reports_to(const void* reference) -> void* {
  return RefReportsTo(reference);
}
auto lyra_rt_bit8_resolved_net_reports_to(void* net) -> void* {
  return NetReportsTo<ResolvedNet<Bit8>>(net);
}
auto lyra_rt_bit16_resolved_net_reports_to(void* net) -> void* {
  return NetReportsTo<ResolvedNet<Bit16>>(net);
}
auto lyra_rt_bit32_resolved_net_reports_to(void* net) -> void* {
  return NetReportsTo<ResolvedNet<Bit32>>(net);
}
auto lyra_rt_bit64_resolved_net_reports_to(void* net) -> void* {
  return NetReportsTo<ResolvedNet<Bit64>>(net);
}
auto lyra_rt_bit_wide_resolved_net_reports_to(void* net) -> void* {
  return NetReportsTo<ResolvedNet<BitWide>>(net);
}
auto lyra_rt_logic8_resolved_net_reports_to(void* net) -> void* {
  return NetReportsTo<ResolvedNet<Logic8>>(net);
}
auto lyra_rt_logic16_resolved_net_reports_to(void* net) -> void* {
  return NetReportsTo<ResolvedNet<Logic16>>(net);
}
auto lyra_rt_logic32_resolved_net_reports_to(void* net) -> void* {
  return NetReportsTo<ResolvedNet<Logic32>>(net);
}
auto lyra_rt_logic64_resolved_net_reports_to(void* net) -> void* {
  return NetReportsTo<ResolvedNet<Logic64>>(net);
}
auto lyra_rt_logic_wide_resolved_net_reports_to(void* net) -> void* {
  return NetReportsTo<ResolvedNet<LogicWide>>(net);
}
auto lyra_rt_string_unresolved_net_reports_to(void* net) -> void* {
  return NetReportsTo<UnresolvedNet<String>>(net);
}
auto lyra_rt_real_unresolved_net_reports_to(void* net) -> void* {
  return NetReportsTo<UnresolvedNet<Real>>(net);
}
auto lyra_rt_shortreal_unresolved_net_reports_to(void* net) -> void* {
  return NetReportsTo<UnresolvedNet<ShortReal>>(net);
}
auto lyra_rt_managedref_unresolved_net_reports_to(void* net) -> void* {
  return NetReportsTo<UnresolvedNet<ObjectRef>>(net);
}
auto lyra_rt_laidout_resolved_net_reports_to(void* net) -> void* {
  return NetReportsTo<ResolvedNet<LaidOutValue>>(net);
}
auto lyra_rt_laidout_unresolved_net_reports_to(void* net) -> void* {
  return NetReportsTo<UnresolvedNet<LaidOutValue>>(net);
}

auto lyra_rt_designate_whole(void* write, void* out) -> void* {
  return std::construct_at(
      static_cast<ErasedDesignation*>(out),
      static_cast<OpenWrite*>(write)->Whole());
}

auto lyra_rt_laidout_designate_element(
    const void* designation, const void* position, const void* array_type,
    void* out) -> void* {
  return DesignateFixedArrayElement(
      designation, ArrayTypeAt(array_type), position, out);
}

auto lyra_rt_laidout_dynamic_array_designate_element(
    const void* designation, const void* position, const void* array_type,
    void* out) -> void* {
  return DesignateFoundElement(
      designation, DynamicArrayTypeAt(array_type), position, out);
}
auto lyra_rt_laidout_queue_designate_element(
    const void* designation, const void* position, const void* location,
    const void* array_type, void* out) -> void* {
  const lyra::base::SourceLocationScope bound(SourceLocationAt(location));
  return DesignateFoundElement(
      designation, QueueTypeAt(array_type), position, out);
}

auto lyra_rt_laidout_assoc_designate_element(
    const void* designation, const void* index, const void* location,
    const void* array_type, void* out) -> void* {
  const lyra::base::SourceLocationScope bound(SourceLocationAt(location));
  return DesignateAssociativeArrayElement(
      designation, lyra::runtime::AssociativeArrayTypeAt(array_type), index,
      out);
}

auto lyra_rt_laidout_designate_component(
    const void* designation, std::int64_t index, const void* tuple_type,
    void* out) -> void* {
  const ErasedDesignation& within = DesignationAt(designation);
  if (within.write->Tells()) {
    within.write->Stepped(
        lyra::runtime::PartPath::Written(
            lyra::support::PartStep::kComponent, index));
  }
  return std::construct_at(
      static_cast<ErasedDesignation*>(out),
      ErasedDesignation{
          .write = within.write,
          .part =
              lyra::runtime::TupleTypeAt(tuple_type)
                  .ComponentAt(within.part, static_cast<std::size_t>(index))});
}

auto lyra_rt_laidout_designate_member(
    const void* designation, const void* union_type, void* out) -> void* {
  const ErasedDesignation& within = DesignationAt(designation);
  if (within.write->Tells()) {
    within.write->BeforeModifyLaidOut(
        lyra::runtime::TypeAt(union_type), within.part);
    within.write->Stepped(
        lyra::runtime::PartPath::Written(
            lyra::support::PartStep::kUnionMember, std::nullopt));
  }
  return std::construct_at(static_cast<ErasedDesignation*>(out), within);
}

auto lyra_rt_string_land(const void* designation) noexcept -> void* {
  return LandDesignation<String>(designation);
}
auto lyra_rt_bitvec_land(const void* designation) noexcept -> void* {
  return LandDesignation<BitVec>(designation);
}
auto lyra_rt_real_land(const void* designation) noexcept -> void* {
  return LandDesignation<Real>(designation);
}
auto lyra_rt_shortreal_land(const void* designation) noexcept -> void* {
  return LandDesignation<ShortReal>(designation);
}
auto lyra_rt_unmanagedref_land(const void* designation) noexcept -> void* {
  return LandDesignation<UnmanagedRef>(designation);
}
auto lyra_rt_laidout_designated(
    const void* designation, const void* first, std::int64_t count,
    const void* value_type) noexcept -> void* {
  const ErasedDesignation& designated = DesignationAt(designation);
  const lyra::value::ValueType& type = lyra::runtime::TypeAt(value_type);
  void* const part = designated.part;
  if (lyra::value::ChangesSizeAtRunTime(type.Kind())) {
    designated.write->BeforeModifyLaidOut(type, part);
    return part;
  }
  if (!designated.write->Announces()) {
    return part;
  }
  // A run is written into an array, and one whose size cannot change is of a
  // fixed size.
  const lyra::value::FixedArrayType& array = *type.AsFixedArray();
  for (const std::int64_t ordinal : lyra::runtime::PositionsASliceWrites(
           PositionNamedAt(first), count,
           static_cast<std::int64_t>(array.Count()))) {
    designated.write->BeforeModifyLaidOut(
        array.ElementType().Type(),
        array.GetUnchecked(part, static_cast<std::size_t>(ordinal)),
        lyra::runtime::PartPath::Element(ordinal));
  }
  return part;
}

void lyra_rt_laidout_moved(
    const void* designation, const void* begin, const void* end) noexcept {
  DesignationAt(designation)
      .write->Moved(*PositionNamedAt(begin), *PositionNamedAt(end));
}

auto lyra_rt_laidout_land(
    const void* designation, const void* value_type) noexcept -> void* {
  return LandLaidOutDesignation(designation, lyra::runtime::TypeAt(value_type));
}

auto lyra_rt_managedref_land(const void* designation) noexcept -> void* {
  return LandDesignation<ObjectRef>(designation);
}

// A value written into storage that already holds one of its domain -- an
// element, a member, the contents a write opened -- which takes it where it
// lies rather than being replaced by a new object, so whatever names that
// storage goes on naming it (LRM 7.6).
void lyra_rt_string_assign(void* storage, const void* value) {
  *static_cast<String*>(storage) = Read<String>(value);
}
void lyra_rt_bitvec_assign(void* storage, const void* value) {
  *static_cast<BitVec*>(storage) = Read<BitVec>(value);
}
void lyra_rt_managedref_assign(void* storage, const void* value) {
  *static_cast<ObjectRef*>(storage) = Read<ObjectRef>(value);
}
// Binding a reference-typed place -- a `ref` port's own name (LRM 23.3.3.2) --
// replaces the reference it holds, never what it names.
void lyra_rt_reference_assign(void* storage, const void* value) {
  *static_cast<ErasedReference*>(storage) = Read<ErasedReference>(value);
}
void lyra_rt_any_reference_assign(void* storage, const void* value) {
  *static_cast<AnyReference*>(storage) = Read<AnyReference>(value);
}
void lyra_rt_weak_reference_assign(void* storage, const void* value) {
  *static_cast<WeakObjectRef*>(storage) = Read<WeakObjectRef>(value);
}
void lyra_rt_wait_assign(void* storage, void* value) {
  *static_cast<Wait*>(storage) = std::move(*static_cast<Wait*>(value));
}

auto lyra_rt_element_default(const void* element_type) noexcept -> const void* {
  return lyra::runtime::ElementTypeAt(element_type).Default();
}
auto lyra_rt_discarded_write(const void* element_type) noexcept -> void* {
  return lyra::runtime::ElementTypeAt(element_type).DiscardedWrite();
}

// Ending an object the generated body held in its own storage, where ending one
// has something to do. An object whose type has a trivial destructor is ended
// by its storage going away, so it has no entry here.
void lyra_rt_string_destroy(void* object) {
  std::destroy_at(static_cast<String*>(object));
}
void lyra_rt_bitvec_destroy(void* object) {
  std::destroy_at(static_cast<BitVec*>(object));
}
void lyra_rt_managedref_destroy(void* object) {
  std::destroy_at(static_cast<ObjectRef*>(object));
}
void lyra_rt_hierarchy_segment_destroy(void* object) {
  std::destroy_at(static_cast<HierarchySegment*>(object));
}
void lyra_rt_trigger_destroy(void* object) {
  std::destroy_at(static_cast<Trigger*>(object));
}
void lyra_rt_observation_destroy(void* object) {
  std::destroy_at(static_cast<Observation*>(object));
}
void lyra_rt_read_report_destroy(void* object) {
  std::destroy_at(static_cast<ReadReport*>(object));
}
void lyra_rt_wait_destroy(void* object) {
  std::destroy_at(static_cast<Wait*>(object));
}
void lyra_rt_dpi_bit_buffer_destroy(void* object) {
  std::destroy_at(static_cast<DpiBitBuffer*>(object));
}
void lyra_rt_dpi_logic_buffer_destroy(void* object) {
  std::destroy_at(static_cast<DpiLogicBuffer*>(object));
}
void lyra_rt_dpi_open_array_destroy(void* object) {
  std::destroy_at(static_cast<DpiOpenArray*>(object));
}
void lyra_rt_memory_words_destroy(void* object) {
  std::destroy_at(static_cast<lyra::runtime::MemoryWords*>(object));
}
void lyra_rt_channel_cancellation_destroy(void* object) {
  std::destroy_at(static_cast<ChannelCancellation*>(object));
}
void lyra_rt_shared_pointer_destroy(void* object) {
  std::destroy_at(static_cast<SharedPointer*>(object));
}
void lyra_rt_weak_reference_destroy(void* object) {
  std::destroy_at(static_cast<WeakObjectRef*>(object));
}
void lyra_rt_open_write_destroy(void* object) {
  std::destroy_at(static_cast<OpenWrite*>(object));
}
void lyra_rt_object_write_destroy(void* object) {
  std::destroy_at(static_cast<ErasedObjectWrite*>(object));
}
void lyra_rt_leased_reference_destroy(void* object) {
  std::destroy_at(static_cast<LeasedReference*>(object));
}

// A second value equal to one the body already holds, built in further storage
// the body gave -- where a value it only reads has to become one it owns.
auto lyra_rt_string_copy(const void* value, void* out) -> void* {
  return Emplace(out, Read<String>(value));
}
auto lyra_rt_bitvec_copy(const void* value, void* out) -> void* {
  return Emplace(out, Read<BitVec>(value));
}
auto lyra_rt_managedref_copy(const void* value, void* out) -> void* {
  return Emplace(out, Read<ObjectRef>(value));
}
auto lyra_rt_shared_pointer_copy(const void* value, void* out) -> void* {
  return Emplace(out, Read<SharedPointer>(value));
}
auto lyra_rt_leased_reference_copy(const void* value, void* out) -> void* {
  return Emplace(out, Read<LeasedReference>(value));
}
auto lyra_rt_print_item_copy(const void* value, void* out) -> void* {
  return Emplace(out, Read<PrintItem>(value));
}
auto lyra_rt_format_spec_copy(const void* value, void* out) -> void* {
  return Emplace(out, Read<FormatSpec>(value));
}
auto lyra_rt_format_arg_copy(const void* value, void* out) -> void* {
  return Emplace(out, Read<FormatArg>(value));
}
auto lyra_rt_hierarchy_segment_copy(const void* value, void* out) -> void* {
  return Emplace(out, Read<HierarchySegment>(value));
}
auto lyra_rt_trigger_copy(const void* value, void* out) -> void* {
  return Emplace(out, Read<Trigger>(value));
}
auto lyra_rt_observation_copy(const void* value, void* out) -> void* {
  return Emplace(out, Read<Observation>(value));
}
auto lyra_rt_dpi_bit_buffer_copy(const void* value, void* out) -> void* {
  return Emplace(out, Read<DpiBitBuffer>(value));
}
auto lyra_rt_dpi_logic_buffer_copy(const void* value, void* out) -> void* {
  return Emplace(out, Read<DpiLogicBuffer>(value));
}
auto lyra_rt_dpi_open_array_copy(const void* value, void* out) -> void* {
  return Emplace(out, Read<DpiOpenArray>(value));
}
auto lyra_rt_channel_cancellation_copy(const void* value, void* out) -> void* {
  return Emplace(out, Read<ChannelCancellation>(value));
}
auto lyra_rt_no_thread_context_copy(const void* value, void* out) -> void* {
  return Emplace(out, Read<lyra::runtime::NoThreadContext>(value));
}
auto lyra_rt_reference_copy(const void* value, void* out) -> void* {
  return Emplace(out, Read<ErasedReference>(value));
}
auto lyra_rt_any_reference_copy(const void* value, void* out) -> void* {
  return Emplace(out, Read<AnyReference>(value));
}
auto lyra_rt_weak_reference_copy(const void* value, void* out) -> void* {
  return Emplace(out, Read<WeakObjectRef>(value));
}

// A value relocated into storage that takes it over; the storage it left is
// owed nothing afterwards.
auto lyra_rt_string_relocate(void* value, void* out) -> void* {
  return Relocate<String>(value, out);
}
auto lyra_rt_bitvec_relocate(void* value, void* out) -> void* {
  return Relocate<BitVec>(value, out);
}
auto lyra_rt_managedref_relocate(void* value, void* out) -> void* {
  return Relocate<ObjectRef>(value, out);
}
auto lyra_rt_shared_pointer_relocate(void* value, void* out) -> void* {
  return Relocate<SharedPointer>(value, out);
}
auto lyra_rt_print_item_relocate(void* value, void* out) -> void* {
  return Relocate<PrintItem>(value, out);
}
auto lyra_rt_format_spec_relocate(void* value, void* out) -> void* {
  return Relocate<FormatSpec>(value, out);
}
auto lyra_rt_format_arg_relocate(void* value, void* out) -> void* {
  return Relocate<FormatArg>(value, out);
}
auto lyra_rt_hierarchy_segment_relocate(void* value, void* out) -> void* {
  return Relocate<HierarchySegment>(value, out);
}
auto lyra_rt_trigger_relocate(void* value, void* out) -> void* {
  return Relocate<Trigger>(value, out);
}
auto lyra_rt_observation_relocate(void* value, void* out) -> void* {
  return Relocate<Observation>(value, out);
}
auto lyra_rt_read_report_relocate(void* value, void* out) -> void* {
  return Relocate<ReadReport>(value, out);
}
auto lyra_rt_wait_relocate(void* value, void* out) -> void* {
  return Relocate<Wait>(value, out);
}
auto lyra_rt_dpi_bit_buffer_relocate(void* value, void* out) -> void* {
  return Relocate<DpiBitBuffer>(value, out);
}
auto lyra_rt_dpi_logic_buffer_relocate(void* value, void* out) -> void* {
  return Relocate<DpiLogicBuffer>(value, out);
}
auto lyra_rt_dpi_open_array_relocate(void* value, void* out) -> void* {
  return Relocate<DpiOpenArray>(value, out);
}
auto lyra_rt_channel_cancellation_relocate(void* value, void* out) -> void* {
  return Relocate<ChannelCancellation>(value, out);
}
auto lyra_rt_no_thread_context_relocate(void* value, void* out) -> void* {
  return Relocate<lyra::runtime::NoThreadContext>(value, out);
}
auto lyra_rt_reference_relocate(void* value, void* out) -> void* {
  return Relocate<ErasedReference>(value, out);
}
auto lyra_rt_any_reference_relocate(void* value, void* out) -> void* {
  return Relocate<AnyReference>(value, out);
}
auto lyra_rt_weak_reference_relocate(void* value, void* out) -> void* {
  return Relocate<WeakObjectRef>(value, out);
}

void lyra_rt_borrowed_handle_construct(void* storage) {
  BuildAt<void*>(storage);
}
void lyra_rt_reference_construct(void* storage) {
  BuildAt<ErasedReference>(storage);
}
void lyra_rt_any_reference_construct(void* storage) {
  BuildAt<AnyReference>(storage);
}
void lyra_rt_weak_reference_construct(void* storage) {
  BuildAt<WeakObjectRef>(storage);
}
void lyra_rt_string_cell_construct(void* storage) {
  BuildAt<Var<String>>(storage);
}
void lyra_rt_bitvec_cell_construct(void* storage) {
  BuildAt<Var<BitVec>>(storage);
}
void lyra_rt_real_cell_construct(void* storage) {
  BuildAt<Var<Real>>(storage);
}
void lyra_rt_shortreal_cell_construct(void* storage) {
  BuildAt<Var<ShortReal>>(storage);
}
void lyra_rt_unmanagedref_cell_construct(void* storage) {
  BuildAt<Var<UnmanagedRef>>(storage);
}
void lyra_rt_laidout_cell_construct(const void* held_type, void* storage) {
  BuildAt<VariableOf<LaidOutValue>::Type>(
      storage, lyra::runtime::ElementTypeAt(held_type));
}

void lyra_rt_managedref_cell_construct(void* storage) {
  BuildAt<Var<ObjectRef>>(storage);
}
void lyra_rt_string_value_cell_construct(void* storage) {
  BuildAt<ValueCell<String>>(storage);
}
void lyra_rt_bitvec_value_cell_construct(void* storage) {
  BuildAt<ValueCell<BitVec>>(storage);
}
void lyra_rt_real_value_cell_construct(void* storage) {
  BuildAt<ValueCell<Real>>(storage);
}
void lyra_rt_shortreal_value_cell_construct(void* storage) {
  BuildAt<ValueCell<ShortReal>>(storage);
}
void lyra_rt_unmanagedref_value_cell_construct(void* storage) {
  BuildAt<ValueCell<UnmanagedRef>>(storage);
}
void lyra_rt_laidout_value_cell_construct(
    const void* held_type, void* storage) {
  BuildAt<ValueCellFor<LaidOutValue>::Type>(
      storage, lyra::runtime::ElementTypeAt(held_type));
}

void lyra_rt_managedref_value_cell_construct(void* storage) {
  BuildAt<ValueCell<ObjectRef>>(storage);
}
void lyra_rt_laidout_resolved_net_construct(
    const void* held_type, const void* held_net_witnesses, void* storage) {
  BuildAt<ResolvedNet<LaidOutValue>>(
      storage, lyra::runtime::ElementTypeAt(held_type),
      *static_cast<const lyra::value::NetWitnessTable*>(held_net_witnesses));
}
void lyra_rt_laidout_unresolved_net_construct(
    const void* held_type, void* storage) {
  BuildAt<UnresolvedNet<LaidOutValue>>(
      storage, lyra::runtime::ElementTypeAt(held_type));
}

void lyra_rt_string_unresolved_net_construct(void* storage) {
  BuildAt<UnresolvedNet<String>>(storage);
}
void lyra_rt_real_unresolved_net_construct(void* storage) {
  BuildAt<UnresolvedNet<Real>>(storage);
}
void lyra_rt_shortreal_unresolved_net_construct(void* storage) {
  BuildAt<UnresolvedNet<ShortReal>>(storage);
}
void lyra_rt_managedref_unresolved_net_construct(void* storage) {
  BuildAt<UnresolvedNet<ObjectRef>>(storage);
}
void lyra_rt_string_sampled_history_construct(void* storage) {
  BuildAt<SampledHistory<String>>(storage);
}
void lyra_rt_real_sampled_history_construct(void* storage) {
  BuildAt<SampledHistory<Real>>(storage);
}
void lyra_rt_shortreal_sampled_history_construct(void* storage) {
  BuildAt<SampledHistory<ShortReal>>(storage);
}
void lyra_rt_laidout_sampled_history_construct(void* storage) {
  BuildAt<SampledHistory<LaidOutValue>>(storage);
}

void lyra_rt_managedref_sampled_history_construct(void* storage) {
  BuildAt<SampledHistory<ObjectRef>>(storage);
}
void lyra_rt_cancellation_target_construct(void* storage) {
  BuildAt<CancellationTarget>(storage);
}
void lyra_rt_evaluation_attempts_construct(void* storage) {
  BuildAt<EvaluationAttempts>(storage);
}
void lyra_rt_assertion_control_construct(void* storage) {
  BuildAt<AssertionControl>(storage);
}
void lyra_rt_timer_construct(void* storage) {
  BuildAt<Timer>(storage);
}
void lyra_rt_loop_blocks_construct(void* storage) {
  BuildAt<LoopBlocks>(storage);
}
void lyra_rt_channel_cancellation_construct(void* storage) {
  BuildAt<ChannelCancellation>(storage);
}
void lyra_rt_no_thread_context_construct(void* storage) {
  BuildAt<lyra::runtime::NoThreadContext>(storage);
}
void lyra_rt_shared_pointer_construct(void* storage) {
  BuildAt<SharedPointer>(storage);
}

void lyra_rt_string_cell_destroy(void* storage) {
  std::destroy_at(&VariableAt<String>(storage));
}
void lyra_rt_bitvec_cell_destroy(void* storage) {
  std::destroy_at(&VariableAt<BitVec>(storage));
}
void lyra_rt_real_cell_destroy(void* storage) {
  std::destroy_at(&VariableAt<Real>(storage));
}
void lyra_rt_shortreal_cell_destroy(void* storage) {
  std::destroy_at(&VariableAt<ShortReal>(storage));
}
void lyra_rt_unmanagedref_cell_destroy(void* storage) {
  std::destroy_at(&VariableAt<UnmanagedRef>(storage));
}
void lyra_rt_laidout_cell_destroy(void* storage) {
  std::destroy_at(&VariableAt<LaidOutValue>(storage));
}

void lyra_rt_managedref_cell_destroy(void* storage) {
  std::destroy_at(&VariableAt<ObjectRef>(storage));
}
void lyra_rt_string_value_cell_destroy(void* storage) {
  std::destroy_at(&ValueCellAt<String>(storage));
}
void lyra_rt_bitvec_value_cell_destroy(void* storage) {
  std::destroy_at(&ValueCellAt<BitVec>(storage));
}
void lyra_rt_laidout_value_cell_destroy(void* storage) {
  std::destroy_at(static_cast<ValueCellFor<LaidOutValue>::Type*>(storage));
}

void lyra_rt_managedref_value_cell_destroy(void* storage) {
  std::destroy_at(&ValueCellAt<ObjectRef>(storage));
}
void lyra_rt_laidout_resolved_net_destroy(void* storage) {
  std::destroy_at(static_cast<ResolvedNet<LaidOutValue>*>(storage));
}
void lyra_rt_laidout_unresolved_net_destroy(void* storage) {
  std::destroy_at(static_cast<UnresolvedNet<LaidOutValue>*>(storage));
}

void lyra_rt_string_unresolved_net_destroy(void* storage) {
  std::destroy_at(&UnresolvedNetOf<String>(storage));
}
void lyra_rt_real_unresolved_net_destroy(void* storage) {
  std::destroy_at(&UnresolvedNetOf<Real>(storage));
}
void lyra_rt_shortreal_unresolved_net_destroy(void* storage) {
  std::destroy_at(&UnresolvedNetOf<ShortReal>(storage));
}
void lyra_rt_managedref_unresolved_net_destroy(void* storage) {
  std::destroy_at(&UnresolvedNetOf<ObjectRef>(storage));
}
void lyra_rt_string_sampled_history_destroy(void* storage) {
  std::destroy_at(static_cast<SampledHistory<String>*>(storage));
}
void lyra_rt_real_sampled_history_destroy(void* storage) {
  std::destroy_at(static_cast<SampledHistory<Real>*>(storage));
}
void lyra_rt_shortreal_sampled_history_destroy(void* storage) {
  std::destroy_at(static_cast<SampledHistory<ShortReal>*>(storage));
}
void lyra_rt_laidout_sampled_history_destroy(void* storage) {
  std::destroy_at(static_cast<SampledHistory<LaidOutValue>*>(storage));
}

void lyra_rt_managedref_sampled_history_destroy(void* storage) {
  std::destroy_at(static_cast<SampledHistory<ObjectRef>*>(storage));
}
void lyra_rt_cancellation_target_destroy(void* storage) {
  std::destroy_at(static_cast<CancellationTarget*>(storage));
}
void lyra_rt_evaluation_attempts_destroy(void* storage) {
  std::destroy_at(static_cast<EvaluationAttempts*>(storage));
}
void lyra_rt_assertion_control_destroy(void* storage) {
  std::destroy_at(static_cast<AssertionControl*>(storage));
}
void lyra_rt_timer_destroy(void* storage) {
  std::destroy_at(static_cast<Timer*>(storage));
}
void lyra_rt_loop_blocks_destroy(void* storage) {
  std::destroy_at(static_cast<LoopBlocks*>(storage));
}

// The holders over each layout an integral value no wider than a word has. A
// value crosses as its own bytes, which a holder keeps as they are. What an
// entry is told of the value's type is a count where the bytes cannot say it:
// the positions a net resolves, the declared width a write into some bits is
// bounded by.
void lyra_rt_bit8_cell_construct(void* storage) {
  BuildAt<Var<Bit8>>(storage);
}
void lyra_rt_bit8_cell_destroy(void* storage) {
  std::destroy_at(&VariableAt<Bit8>(storage));
}
void lyra_rt_bit8_cell_initialize(void* cell, const void* prototype) noexcept {
  VariableAt<Bit8>(cell).Initialize(Read<Bit8>(prototype));
}
void lyra_rt_bit8_cell_set(void* cell, const void* value) {
  VariableAt<Bit8>(cell).Set(Read<Bit8>(value));
}
void lyra_rt_bit8_cell_arm_sampling(void* cell) {
  VariableAt<Bit8>(cell).ArmSampling();
}
auto lyra_rt_bit8_cell_sampled_get(void* cell, const void* location) -> const
    void* {
  return HandleTo(
      VariableAt<Bit8>(cell).SampledGet(SourceLocationAt(location)));
}
auto lyra_rt_bit8_cell_begin_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) -> std::int64_t {
  return CellBeginTakeover<Bit8>(
      cell, level, first, count, SourceLocationAt(location));
}
auto lyra_rt_bit8_cell_drive_takeover(
    void* cell, std::int64_t level, std::int64_t generation, const void* value,
    const void* location) -> bool {
  return CellDriveTakeover<Bit8>(
      cell, level, generation, value, SourceLocationAt(location));
}
void lyra_rt_bit8_cell_end_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) {
  CellEndTakeover<Bit8>(cell, level, first, count, SourceLocationAt(location));
}
auto lyra_rt_bit8_ref_begin_takeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) -> std::int64_t {
  return RefBeginTakeover<Bit8>(
      reference, level, first, count, SourceLocationAt(location));
}
auto lyra_rt_bit8_ref_drive_takeover(
    void* reference, std::int64_t level, std::int64_t generation,
    const void* value, const void* location) -> bool {
  return RefDriveTakeover<Bit8>(
      reference, level, generation, value, SourceLocationAt(location));
}
void lyra_rt_bit8_ref_end_takeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) {
  RefEndTakeover<Bit8>(
      reference, level, first, count, SourceLocationAt(location));
}
auto lyra_rt_bit8_cell_refer(void* cell, void* out) -> void* {
  return ReferToCell<Bit8>(cell, out);
}
auto lyra_rt_bit8_cell_open_for_write(void* cell, void* out) -> void* {
  return OpenCellWrite<Bit8>(cell, out);
}
auto lyra_rt_bit8_shared_cell_make(void* out) -> void* {
  return MakeSharedCell<Bit8>(out);
}
auto lyra_rt_bit8_ref_get(void* reference) -> const void* {
  return RefGet<Bit8>(reference);
}
void lyra_rt_bit8_ref_set(void* reference, const void* value) {
  RefSet<Bit8>(reference, value);
}
void lyra_rt_bit8_ref_arm_sampling(void* reference) {
  RefArmSampling<Bit8>(reference);
}
auto lyra_rt_bit8_ref_sampled_get(void* reference, const void* location)
    -> const void* {
  return RefSampledGet<Bit8>(reference, SourceLocationAt(location));
}
auto lyra_rt_bit8_ref_open_for_write(void* reference, void* out) -> void* {
  return OpenRefWrite<Bit8>(reference, out);
}
auto lyra_rt_bit8_value_cell_alloc() noexcept -> void* {
  return AllocateValueCell<Bit8>();
}
void lyra_rt_bit8_value_cell_construct(void* storage) {
  BuildAt<ValueCell<Bit8>>(storage);
}
void lyra_rt_bit8_sampled_history_construct(void* storage) {
  BuildAt<SampledHistory<Bit8>>(storage);
}
void lyra_rt_bit8_sampled_history_destroy(void* storage) {
  std::destroy_at(&HistoryAt<Bit8>(storage));
}
void lyra_rt_bit8_sampled_history_install(
    void* history, const void* default_value, std::int64_t depth) {
  HistoryAt<Bit8>(history).Install(Read<Bit8>(default_value), depth);
}
void lyra_rt_bit8_sampled_history_push(void* history, const void* value) {
  HistoryAt<Bit8>(history).Push(Read<Bit8>(value));
}
auto lyra_rt_bit8_sampled_history_at(
    const void* history, std::int64_t ticks_back) -> const void* {
  return HandleTo(
      static_cast<const SampledHistory<Bit8>*>(history)->At(ticks_back));
}
auto lyra_rt_bit8_land(const void* designation) noexcept -> void* {
  return LandDesignation<Bit8>(designation);
}
void lyra_rt_bit8_insert_designated_bits(
    const void* designation, std::int64_t width, void* outer_parts,
    std::uint64_t outer_parts_count, const void* start, const void* bits,
    std::int64_t bits_width, bool bits_are_four_state) {
  InsertDesignatedBits<Bit8>(
      designation, width, outer_parts, outer_parts_count, start, bits,
      bits_width, bits_are_four_state);
}
void lyra_rt_bit8_resolved_net_construct(std::int64_t width, void* storage) {
  BuildAt<ResolvedNet<Bit8>>(storage, static_cast<std::uint64_t>(width));
}
void lyra_rt_bit8_resolved_net_destroy(void* storage) {
  std::destroy_at(&NetOf<Bit8>(storage));
}
void lyra_rt_bit8_net_initialize_tri_state(
    void* net, std::int64_t fill, std::int64_t strength) {
  NetOf<Bit8>(net).InitializeTriState(fill, strength);
}
void lyra_rt_bit8_net_initialize_wired_and(
    void* net, std::int64_t fill, std::int64_t strength) {
  NetOf<Bit8>(net).InitializeWiredAnd(fill, strength);
}
void lyra_rt_bit8_net_initialize_wired_or(
    void* net, std::int64_t fill, std::int64_t strength) {
  NetOf<Bit8>(net).InitializeWiredOr(fill, strength);
}
void lyra_rt_bit8_net_initialize_retaining(
    void* net, std::int64_t fill, std::int64_t strength) {
  NetOf<Bit8>(net).InitializeRetaining(fill, strength);
}
auto lyra_rt_bit8_resolved_net_begin_takeover(
    void* net, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) -> std::int64_t {
  return NetOf<Bit8>(net).BeginTakeover(
      level, first, count, SourceLocationAt(location));
}
auto lyra_rt_bit8_resolved_net_drive_takeover(
    void* net, std::int64_t level, std::int64_t generation, const void* value,
    const void* location) -> bool {
  return NetOf<Bit8>(net).DriveTakeover(
      level, generation, Read<Bit8>(value), SourceLocationAt(location));
}
void lyra_rt_bit8_resolved_net_end_takeover(
    void* net, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) {
  NetOf<Bit8>(net).EndTakeover(level, first, count, SourceLocationAt(location));
}
void lyra_rt_bit8_resolved_net_arm_sampling(void* net) {
  NetOf<Bit8>(net).ArmSampling();
}
auto lyra_rt_bit8_resolved_net_sampled_get(void* net, const void* location)
    -> const void* {
  return HandleTo(NetOf<Bit8>(net).SampledGet(SourceLocationAt(location)));
}
void lyra_rt_bit8_driver_set(void* driver, const void* value) {
  DriverOf<Bit8>(driver).Set(Read<Bit8>(value));
}

void lyra_rt_bit16_cell_construct(void* storage) {
  BuildAt<Var<Bit16>>(storage);
}
void lyra_rt_bit16_cell_destroy(void* storage) {
  std::destroy_at(&VariableAt<Bit16>(storage));
}
void lyra_rt_bit16_cell_initialize(void* cell, const void* prototype) noexcept {
  VariableAt<Bit16>(cell).Initialize(Read<Bit16>(prototype));
}
void lyra_rt_bit16_cell_set(void* cell, const void* value) {
  VariableAt<Bit16>(cell).Set(Read<Bit16>(value));
}
void lyra_rt_bit16_cell_arm_sampling(void* cell) {
  VariableAt<Bit16>(cell).ArmSampling();
}
auto lyra_rt_bit16_cell_sampled_get(void* cell, const void* location) -> const
    void* {
  return HandleTo(
      VariableAt<Bit16>(cell).SampledGet(SourceLocationAt(location)));
}
auto lyra_rt_bit16_cell_begin_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) -> std::int64_t {
  return CellBeginTakeover<Bit16>(
      cell, level, first, count, SourceLocationAt(location));
}
auto lyra_rt_bit16_cell_drive_takeover(
    void* cell, std::int64_t level, std::int64_t generation, const void* value,
    const void* location) -> bool {
  return CellDriveTakeover<Bit16>(
      cell, level, generation, value, SourceLocationAt(location));
}
void lyra_rt_bit16_cell_end_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) {
  CellEndTakeover<Bit16>(cell, level, first, count, SourceLocationAt(location));
}
auto lyra_rt_bit16_ref_begin_takeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) -> std::int64_t {
  return RefBeginTakeover<Bit16>(
      reference, level, first, count, SourceLocationAt(location));
}
auto lyra_rt_bit16_ref_drive_takeover(
    void* reference, std::int64_t level, std::int64_t generation,
    const void* value, const void* location) -> bool {
  return RefDriveTakeover<Bit16>(
      reference, level, generation, value, SourceLocationAt(location));
}
void lyra_rt_bit16_ref_end_takeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) {
  RefEndTakeover<Bit16>(
      reference, level, first, count, SourceLocationAt(location));
}
auto lyra_rt_bit16_cell_refer(void* cell, void* out) -> void* {
  return ReferToCell<Bit16>(cell, out);
}
auto lyra_rt_bit16_cell_open_for_write(void* cell, void* out) -> void* {
  return OpenCellWrite<Bit16>(cell, out);
}
auto lyra_rt_bit16_shared_cell_make(void* out) -> void* {
  return MakeSharedCell<Bit16>(out);
}
auto lyra_rt_bit16_ref_get(void* reference) -> const void* {
  return RefGet<Bit16>(reference);
}
void lyra_rt_bit16_ref_set(void* reference, const void* value) {
  RefSet<Bit16>(reference, value);
}
void lyra_rt_bit16_ref_arm_sampling(void* reference) {
  RefArmSampling<Bit16>(reference);
}
auto lyra_rt_bit16_ref_sampled_get(void* reference, const void* location)
    -> const void* {
  return RefSampledGet<Bit16>(reference, SourceLocationAt(location));
}
auto lyra_rt_bit16_ref_open_for_write(void* reference, void* out) -> void* {
  return OpenRefWrite<Bit16>(reference, out);
}
auto lyra_rt_bit16_value_cell_alloc() noexcept -> void* {
  return AllocateValueCell<Bit16>();
}
void lyra_rt_bit16_value_cell_construct(void* storage) {
  BuildAt<ValueCell<Bit16>>(storage);
}
void lyra_rt_bit16_sampled_history_construct(void* storage) {
  BuildAt<SampledHistory<Bit16>>(storage);
}
void lyra_rt_bit16_sampled_history_destroy(void* storage) {
  std::destroy_at(&HistoryAt<Bit16>(storage));
}
void lyra_rt_bit16_sampled_history_install(
    void* history, const void* default_value, std::int64_t depth) {
  HistoryAt<Bit16>(history).Install(Read<Bit16>(default_value), depth);
}
void lyra_rt_bit16_sampled_history_push(void* history, const void* value) {
  HistoryAt<Bit16>(history).Push(Read<Bit16>(value));
}
auto lyra_rt_bit16_sampled_history_at(
    const void* history, std::int64_t ticks_back) -> const void* {
  return HandleTo(
      static_cast<const SampledHistory<Bit16>*>(history)->At(ticks_back));
}
auto lyra_rt_bit16_land(const void* designation) noexcept -> void* {
  return LandDesignation<Bit16>(designation);
}
void lyra_rt_bit16_insert_designated_bits(
    const void* designation, std::int64_t width, void* outer_parts,
    std::uint64_t outer_parts_count, const void* start, const void* bits,
    std::int64_t bits_width, bool bits_are_four_state) {
  InsertDesignatedBits<Bit16>(
      designation, width, outer_parts, outer_parts_count, start, bits,
      bits_width, bits_are_four_state);
}
void lyra_rt_bit16_resolved_net_construct(std::int64_t width, void* storage) {
  BuildAt<ResolvedNet<Bit16>>(storage, static_cast<std::uint64_t>(width));
}
void lyra_rt_bit16_resolved_net_destroy(void* storage) {
  std::destroy_at(&NetOf<Bit16>(storage));
}
void lyra_rt_bit16_net_initialize_tri_state(
    void* net, std::int64_t fill, std::int64_t strength) {
  NetOf<Bit16>(net).InitializeTriState(fill, strength);
}
void lyra_rt_bit16_net_initialize_wired_and(
    void* net, std::int64_t fill, std::int64_t strength) {
  NetOf<Bit16>(net).InitializeWiredAnd(fill, strength);
}
void lyra_rt_bit16_net_initialize_wired_or(
    void* net, std::int64_t fill, std::int64_t strength) {
  NetOf<Bit16>(net).InitializeWiredOr(fill, strength);
}
void lyra_rt_bit16_net_initialize_retaining(
    void* net, std::int64_t fill, std::int64_t strength) {
  NetOf<Bit16>(net).InitializeRetaining(fill, strength);
}
auto lyra_rt_bit16_resolved_net_begin_takeover(
    void* net, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) -> std::int64_t {
  return NetOf<Bit16>(net).BeginTakeover(
      level, first, count, SourceLocationAt(location));
}
auto lyra_rt_bit16_resolved_net_drive_takeover(
    void* net, std::int64_t level, std::int64_t generation, const void* value,
    const void* location) -> bool {
  return NetOf<Bit16>(net).DriveTakeover(
      level, generation, Read<Bit16>(value), SourceLocationAt(location));
}
void lyra_rt_bit16_resolved_net_end_takeover(
    void* net, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) {
  NetOf<Bit16>(net).EndTakeover(
      level, first, count, SourceLocationAt(location));
}
void lyra_rt_bit16_resolved_net_arm_sampling(void* net) {
  NetOf<Bit16>(net).ArmSampling();
}
auto lyra_rt_bit16_resolved_net_sampled_get(void* net, const void* location)
    -> const void* {
  return HandleTo(NetOf<Bit16>(net).SampledGet(SourceLocationAt(location)));
}
void lyra_rt_bit16_driver_set(void* driver, const void* value) {
  DriverOf<Bit16>(driver).Set(Read<Bit16>(value));
}

void lyra_rt_bit32_cell_construct(void* storage) {
  BuildAt<Var<Bit32>>(storage);
}
void lyra_rt_bit32_cell_destroy(void* storage) {
  std::destroy_at(&VariableAt<Bit32>(storage));
}
void lyra_rt_bit32_cell_initialize(void* cell, const void* prototype) noexcept {
  VariableAt<Bit32>(cell).Initialize(Read<Bit32>(prototype));
}
void lyra_rt_bit32_cell_set(void* cell, const void* value) {
  VariableAt<Bit32>(cell).Set(Read<Bit32>(value));
}
void lyra_rt_bit32_cell_arm_sampling(void* cell) {
  VariableAt<Bit32>(cell).ArmSampling();
}
auto lyra_rt_bit32_cell_sampled_get(void* cell, const void* location) -> const
    void* {
  return HandleTo(
      VariableAt<Bit32>(cell).SampledGet(SourceLocationAt(location)));
}
auto lyra_rt_bit32_cell_begin_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) -> std::int64_t {
  return CellBeginTakeover<Bit32>(
      cell, level, first, count, SourceLocationAt(location));
}
auto lyra_rt_bit32_cell_drive_takeover(
    void* cell, std::int64_t level, std::int64_t generation, const void* value,
    const void* location) -> bool {
  return CellDriveTakeover<Bit32>(
      cell, level, generation, value, SourceLocationAt(location));
}
void lyra_rt_bit32_cell_end_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) {
  CellEndTakeover<Bit32>(cell, level, first, count, SourceLocationAt(location));
}
auto lyra_rt_bit32_ref_begin_takeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) -> std::int64_t {
  return RefBeginTakeover<Bit32>(
      reference, level, first, count, SourceLocationAt(location));
}
auto lyra_rt_bit32_ref_drive_takeover(
    void* reference, std::int64_t level, std::int64_t generation,
    const void* value, const void* location) -> bool {
  return RefDriveTakeover<Bit32>(
      reference, level, generation, value, SourceLocationAt(location));
}
void lyra_rt_bit32_ref_end_takeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) {
  RefEndTakeover<Bit32>(
      reference, level, first, count, SourceLocationAt(location));
}
auto lyra_rt_bit32_cell_refer(void* cell, void* out) -> void* {
  return ReferToCell<Bit32>(cell, out);
}
auto lyra_rt_bit32_cell_open_for_write(void* cell, void* out) -> void* {
  return OpenCellWrite<Bit32>(cell, out);
}
auto lyra_rt_bit32_shared_cell_make(void* out) -> void* {
  return MakeSharedCell<Bit32>(out);
}
auto lyra_rt_bit32_ref_get(void* reference) -> const void* {
  return RefGet<Bit32>(reference);
}
void lyra_rt_bit32_ref_set(void* reference, const void* value) {
  RefSet<Bit32>(reference, value);
}
void lyra_rt_bit32_ref_arm_sampling(void* reference) {
  RefArmSampling<Bit32>(reference);
}
auto lyra_rt_bit32_ref_sampled_get(void* reference, const void* location)
    -> const void* {
  return RefSampledGet<Bit32>(reference, SourceLocationAt(location));
}
auto lyra_rt_bit32_ref_open_for_write(void* reference, void* out) -> void* {
  return OpenRefWrite<Bit32>(reference, out);
}
auto lyra_rt_bit32_value_cell_alloc() noexcept -> void* {
  return AllocateValueCell<Bit32>();
}
void lyra_rt_bit32_value_cell_construct(void* storage) {
  BuildAt<ValueCell<Bit32>>(storage);
}
void lyra_rt_bit32_sampled_history_construct(void* storage) {
  BuildAt<SampledHistory<Bit32>>(storage);
}
void lyra_rt_bit32_sampled_history_destroy(void* storage) {
  std::destroy_at(&HistoryAt<Bit32>(storage));
}
void lyra_rt_bit32_sampled_history_install(
    void* history, const void* default_value, std::int64_t depth) {
  HistoryAt<Bit32>(history).Install(Read<Bit32>(default_value), depth);
}
void lyra_rt_bit32_sampled_history_push(void* history, const void* value) {
  HistoryAt<Bit32>(history).Push(Read<Bit32>(value));
}
auto lyra_rt_bit32_sampled_history_at(
    const void* history, std::int64_t ticks_back) -> const void* {
  return HandleTo(
      static_cast<const SampledHistory<Bit32>*>(history)->At(ticks_back));
}
auto lyra_rt_bit32_land(const void* designation) noexcept -> void* {
  return LandDesignation<Bit32>(designation);
}
void lyra_rt_bit32_insert_designated_bits(
    const void* designation, std::int64_t width, void* outer_parts,
    std::uint64_t outer_parts_count, const void* start, const void* bits,
    std::int64_t bits_width, bool bits_are_four_state) {
  InsertDesignatedBits<Bit32>(
      designation, width, outer_parts, outer_parts_count, start, bits,
      bits_width, bits_are_four_state);
}
void lyra_rt_bit32_resolved_net_construct(std::int64_t width, void* storage) {
  BuildAt<ResolvedNet<Bit32>>(storage, static_cast<std::uint64_t>(width));
}
void lyra_rt_bit32_resolved_net_destroy(void* storage) {
  std::destroy_at(&NetOf<Bit32>(storage));
}
void lyra_rt_bit32_net_initialize_tri_state(
    void* net, std::int64_t fill, std::int64_t strength) {
  NetOf<Bit32>(net).InitializeTriState(fill, strength);
}
void lyra_rt_bit32_net_initialize_wired_and(
    void* net, std::int64_t fill, std::int64_t strength) {
  NetOf<Bit32>(net).InitializeWiredAnd(fill, strength);
}
void lyra_rt_bit32_net_initialize_wired_or(
    void* net, std::int64_t fill, std::int64_t strength) {
  NetOf<Bit32>(net).InitializeWiredOr(fill, strength);
}
void lyra_rt_bit32_net_initialize_retaining(
    void* net, std::int64_t fill, std::int64_t strength) {
  NetOf<Bit32>(net).InitializeRetaining(fill, strength);
}
auto lyra_rt_bit32_resolved_net_begin_takeover(
    void* net, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) -> std::int64_t {
  return NetOf<Bit32>(net).BeginTakeover(
      level, first, count, SourceLocationAt(location));
}
auto lyra_rt_bit32_resolved_net_drive_takeover(
    void* net, std::int64_t level, std::int64_t generation, const void* value,
    const void* location) -> bool {
  return NetOf<Bit32>(net).DriveTakeover(
      level, generation, Read<Bit32>(value), SourceLocationAt(location));
}
void lyra_rt_bit32_resolved_net_end_takeover(
    void* net, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) {
  NetOf<Bit32>(net).EndTakeover(
      level, first, count, SourceLocationAt(location));
}
void lyra_rt_bit32_resolved_net_arm_sampling(void* net) {
  NetOf<Bit32>(net).ArmSampling();
}
auto lyra_rt_bit32_resolved_net_sampled_get(void* net, const void* location)
    -> const void* {
  return HandleTo(NetOf<Bit32>(net).SampledGet(SourceLocationAt(location)));
}
void lyra_rt_bit32_driver_set(void* driver, const void* value) {
  DriverOf<Bit32>(driver).Set(Read<Bit32>(value));
}

void lyra_rt_bit64_cell_construct(void* storage) {
  BuildAt<Var<Bit64>>(storage);
}
void lyra_rt_bit64_cell_destroy(void* storage) {
  std::destroy_at(&VariableAt<Bit64>(storage));
}
void lyra_rt_bit64_cell_initialize(void* cell, const void* prototype) noexcept {
  VariableAt<Bit64>(cell).Initialize(Read<Bit64>(prototype));
}
void lyra_rt_bit64_cell_set(void* cell, const void* value) {
  VariableAt<Bit64>(cell).Set(Read<Bit64>(value));
}
void lyra_rt_bit64_cell_arm_sampling(void* cell) {
  VariableAt<Bit64>(cell).ArmSampling();
}
auto lyra_rt_bit64_cell_sampled_get(void* cell, const void* location) -> const
    void* {
  return HandleTo(
      VariableAt<Bit64>(cell).SampledGet(SourceLocationAt(location)));
}
auto lyra_rt_bit64_cell_begin_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) -> std::int64_t {
  return CellBeginTakeover<Bit64>(
      cell, level, first, count, SourceLocationAt(location));
}
auto lyra_rt_bit64_cell_drive_takeover(
    void* cell, std::int64_t level, std::int64_t generation, const void* value,
    const void* location) -> bool {
  return CellDriveTakeover<Bit64>(
      cell, level, generation, value, SourceLocationAt(location));
}
void lyra_rt_bit64_cell_end_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) {
  CellEndTakeover<Bit64>(cell, level, first, count, SourceLocationAt(location));
}
auto lyra_rt_bit64_ref_begin_takeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) -> std::int64_t {
  return RefBeginTakeover<Bit64>(
      reference, level, first, count, SourceLocationAt(location));
}
auto lyra_rt_bit64_ref_drive_takeover(
    void* reference, std::int64_t level, std::int64_t generation,
    const void* value, const void* location) -> bool {
  return RefDriveTakeover<Bit64>(
      reference, level, generation, value, SourceLocationAt(location));
}
void lyra_rt_bit64_ref_end_takeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) {
  RefEndTakeover<Bit64>(
      reference, level, first, count, SourceLocationAt(location));
}
auto lyra_rt_bit64_cell_refer(void* cell, void* out) -> void* {
  return ReferToCell<Bit64>(cell, out);
}
auto lyra_rt_bit64_cell_open_for_write(void* cell, void* out) -> void* {
  return OpenCellWrite<Bit64>(cell, out);
}
auto lyra_rt_bit64_shared_cell_make(void* out) -> void* {
  return MakeSharedCell<Bit64>(out);
}
auto lyra_rt_bit64_ref_get(void* reference) -> const void* {
  return RefGet<Bit64>(reference);
}
void lyra_rt_bit64_ref_set(void* reference, const void* value) {
  RefSet<Bit64>(reference, value);
}
void lyra_rt_bit64_ref_arm_sampling(void* reference) {
  RefArmSampling<Bit64>(reference);
}
auto lyra_rt_bit64_ref_sampled_get(void* reference, const void* location)
    -> const void* {
  return RefSampledGet<Bit64>(reference, SourceLocationAt(location));
}
auto lyra_rt_bit64_ref_open_for_write(void* reference, void* out) -> void* {
  return OpenRefWrite<Bit64>(reference, out);
}
auto lyra_rt_bit64_value_cell_alloc() noexcept -> void* {
  return AllocateValueCell<Bit64>();
}
void lyra_rt_bit64_value_cell_construct(void* storage) {
  BuildAt<ValueCell<Bit64>>(storage);
}
void lyra_rt_bit64_sampled_history_construct(void* storage) {
  BuildAt<SampledHistory<Bit64>>(storage);
}
void lyra_rt_bit64_sampled_history_destroy(void* storage) {
  std::destroy_at(&HistoryAt<Bit64>(storage));
}
void lyra_rt_bit64_sampled_history_install(
    void* history, const void* default_value, std::int64_t depth) {
  HistoryAt<Bit64>(history).Install(Read<Bit64>(default_value), depth);
}
void lyra_rt_bit64_sampled_history_push(void* history, const void* value) {
  HistoryAt<Bit64>(history).Push(Read<Bit64>(value));
}
auto lyra_rt_bit64_sampled_history_at(
    const void* history, std::int64_t ticks_back) -> const void* {
  return HandleTo(
      static_cast<const SampledHistory<Bit64>*>(history)->At(ticks_back));
}
auto lyra_rt_bit64_land(const void* designation) noexcept -> void* {
  return LandDesignation<Bit64>(designation);
}
void lyra_rt_bit64_insert_designated_bits(
    const void* designation, std::int64_t width, void* outer_parts,
    std::uint64_t outer_parts_count, const void* start, const void* bits,
    std::int64_t bits_width, bool bits_are_four_state) {
  InsertDesignatedBits<Bit64>(
      designation, width, outer_parts, outer_parts_count, start, bits,
      bits_width, bits_are_four_state);
}
void lyra_rt_bit64_resolved_net_construct(std::int64_t width, void* storage) {
  BuildAt<ResolvedNet<Bit64>>(storage, static_cast<std::uint64_t>(width));
}
void lyra_rt_bit64_resolved_net_destroy(void* storage) {
  std::destroy_at(&NetOf<Bit64>(storage));
}
void lyra_rt_bit64_net_initialize_tri_state(
    void* net, std::int64_t fill, std::int64_t strength) {
  NetOf<Bit64>(net).InitializeTriState(fill, strength);
}
void lyra_rt_bit64_net_initialize_wired_and(
    void* net, std::int64_t fill, std::int64_t strength) {
  NetOf<Bit64>(net).InitializeWiredAnd(fill, strength);
}
void lyra_rt_bit64_net_initialize_wired_or(
    void* net, std::int64_t fill, std::int64_t strength) {
  NetOf<Bit64>(net).InitializeWiredOr(fill, strength);
}
void lyra_rt_bit64_net_initialize_retaining(
    void* net, std::int64_t fill, std::int64_t strength) {
  NetOf<Bit64>(net).InitializeRetaining(fill, strength);
}
auto lyra_rt_bit64_resolved_net_begin_takeover(
    void* net, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) -> std::int64_t {
  return NetOf<Bit64>(net).BeginTakeover(
      level, first, count, SourceLocationAt(location));
}
auto lyra_rt_bit64_resolved_net_drive_takeover(
    void* net, std::int64_t level, std::int64_t generation, const void* value,
    const void* location) -> bool {
  return NetOf<Bit64>(net).DriveTakeover(
      level, generation, Read<Bit64>(value), SourceLocationAt(location));
}
void lyra_rt_bit64_resolved_net_end_takeover(
    void* net, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) {
  NetOf<Bit64>(net).EndTakeover(
      level, first, count, SourceLocationAt(location));
}
void lyra_rt_bit64_resolved_net_arm_sampling(void* net) {
  NetOf<Bit64>(net).ArmSampling();
}
auto lyra_rt_bit64_resolved_net_sampled_get(void* net, const void* location)
    -> const void* {
  return HandleTo(NetOf<Bit64>(net).SampledGet(SourceLocationAt(location)));
}
void lyra_rt_bit64_driver_set(void* driver, const void* value) {
  DriverOf<Bit64>(driver).Set(Read<Bit64>(value));
}

void lyra_rt_logic8_cell_construct(void* storage) {
  BuildAt<Var<Logic8>>(storage);
}
void lyra_rt_logic8_cell_destroy(void* storage) {
  std::destroy_at(&VariableAt<Logic8>(storage));
}
void lyra_rt_logic8_cell_initialize(
    void* cell, const void* prototype) noexcept {
  VariableAt<Logic8>(cell).Initialize(Read<Logic8>(prototype));
}
void lyra_rt_logic8_cell_set(void* cell, const void* value) {
  VariableAt<Logic8>(cell).Set(Read<Logic8>(value));
}
void lyra_rt_logic8_cell_arm_sampling(void* cell) {
  VariableAt<Logic8>(cell).ArmSampling();
}
auto lyra_rt_logic8_cell_sampled_get(void* cell, const void* location) -> const
    void* {
  return HandleTo(
      VariableAt<Logic8>(cell).SampledGet(SourceLocationAt(location)));
}
auto lyra_rt_logic8_cell_begin_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) -> std::int64_t {
  return CellBeginTakeover<Logic8>(
      cell, level, first, count, SourceLocationAt(location));
}
auto lyra_rt_logic8_cell_drive_takeover(
    void* cell, std::int64_t level, std::int64_t generation, const void* value,
    const void* location) -> bool {
  return CellDriveTakeover<Logic8>(
      cell, level, generation, value, SourceLocationAt(location));
}
void lyra_rt_logic8_cell_end_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) {
  CellEndTakeover<Logic8>(
      cell, level, first, count, SourceLocationAt(location));
}
auto lyra_rt_logic8_ref_begin_takeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) -> std::int64_t {
  return RefBeginTakeover<Logic8>(
      reference, level, first, count, SourceLocationAt(location));
}
auto lyra_rt_logic8_ref_drive_takeover(
    void* reference, std::int64_t level, std::int64_t generation,
    const void* value, const void* location) -> bool {
  return RefDriveTakeover<Logic8>(
      reference, level, generation, value, SourceLocationAt(location));
}
void lyra_rt_logic8_ref_end_takeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) {
  RefEndTakeover<Logic8>(
      reference, level, first, count, SourceLocationAt(location));
}
auto lyra_rt_logic8_cell_refer(void* cell, void* out) -> void* {
  return ReferToCell<Logic8>(cell, out);
}
auto lyra_rt_logic8_cell_open_for_write(void* cell, void* out) -> void* {
  return OpenCellWrite<Logic8>(cell, out);
}
auto lyra_rt_logic8_shared_cell_make(void* out) -> void* {
  return MakeSharedCell<Logic8>(out);
}
auto lyra_rt_logic8_ref_get(void* reference) -> const void* {
  return RefGet<Logic8>(reference);
}
void lyra_rt_logic8_ref_set(void* reference, const void* value) {
  RefSet<Logic8>(reference, value);
}
void lyra_rt_logic8_ref_arm_sampling(void* reference) {
  RefArmSampling<Logic8>(reference);
}
auto lyra_rt_logic8_ref_sampled_get(void* reference, const void* location)
    -> const void* {
  return RefSampledGet<Logic8>(reference, SourceLocationAt(location));
}
auto lyra_rt_logic8_ref_open_for_write(void* reference, void* out) -> void* {
  return OpenRefWrite<Logic8>(reference, out);
}
auto lyra_rt_logic8_value_cell_alloc() noexcept -> void* {
  return AllocateValueCell<Logic8>();
}
void lyra_rt_logic8_value_cell_construct(void* storage) {
  BuildAt<ValueCell<Logic8>>(storage);
}
void lyra_rt_logic8_sampled_history_construct(void* storage) {
  BuildAt<SampledHistory<Logic8>>(storage);
}
void lyra_rt_logic8_sampled_history_destroy(void* storage) {
  std::destroy_at(&HistoryAt<Logic8>(storage));
}
void lyra_rt_logic8_sampled_history_install(
    void* history, const void* default_value, std::int64_t depth) {
  HistoryAt<Logic8>(history).Install(Read<Logic8>(default_value), depth);
}
void lyra_rt_logic8_sampled_history_push(void* history, const void* value) {
  HistoryAt<Logic8>(history).Push(Read<Logic8>(value));
}
auto lyra_rt_logic8_sampled_history_at(
    const void* history, std::int64_t ticks_back) -> const void* {
  return HandleTo(
      static_cast<const SampledHistory<Logic8>*>(history)->At(ticks_back));
}
auto lyra_rt_logic8_land(const void* designation) noexcept -> void* {
  return LandDesignation<Logic8>(designation);
}
void lyra_rt_logic8_insert_designated_bits(
    const void* designation, std::int64_t width, void* outer_parts,
    std::uint64_t outer_parts_count, const void* start, const void* bits,
    std::int64_t bits_width, bool bits_are_four_state) {
  InsertDesignatedBits<Logic8>(
      designation, width, outer_parts, outer_parts_count, start, bits,
      bits_width, bits_are_four_state);
}
void lyra_rt_logic8_resolved_net_construct(std::int64_t width, void* storage) {
  BuildAt<ResolvedNet<Logic8>>(storage, static_cast<std::uint64_t>(width));
}
void lyra_rt_logic8_resolved_net_destroy(void* storage) {
  std::destroy_at(&NetOf<Logic8>(storage));
}
void lyra_rt_logic8_net_initialize_tri_state(
    void* net, std::int64_t fill, std::int64_t strength) {
  NetOf<Logic8>(net).InitializeTriState(fill, strength);
}
void lyra_rt_logic8_net_initialize_wired_and(
    void* net, std::int64_t fill, std::int64_t strength) {
  NetOf<Logic8>(net).InitializeWiredAnd(fill, strength);
}
void lyra_rt_logic8_net_initialize_wired_or(
    void* net, std::int64_t fill, std::int64_t strength) {
  NetOf<Logic8>(net).InitializeWiredOr(fill, strength);
}
void lyra_rt_logic8_net_initialize_retaining(
    void* net, std::int64_t fill, std::int64_t strength) {
  NetOf<Logic8>(net).InitializeRetaining(fill, strength);
}
auto lyra_rt_logic8_resolved_net_begin_takeover(
    void* net, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) -> std::int64_t {
  return NetOf<Logic8>(net).BeginTakeover(
      level, first, count, SourceLocationAt(location));
}
auto lyra_rt_logic8_resolved_net_drive_takeover(
    void* net, std::int64_t level, std::int64_t generation, const void* value,
    const void* location) -> bool {
  return NetOf<Logic8>(net).DriveTakeover(
      level, generation, Read<Logic8>(value), SourceLocationAt(location));
}
void lyra_rt_logic8_resolved_net_end_takeover(
    void* net, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) {
  NetOf<Logic8>(net).EndTakeover(
      level, first, count, SourceLocationAt(location));
}
void lyra_rt_logic8_resolved_net_arm_sampling(void* net) {
  NetOf<Logic8>(net).ArmSampling();
}
auto lyra_rt_logic8_resolved_net_sampled_get(void* net, const void* location)
    -> const void* {
  return HandleTo(NetOf<Logic8>(net).SampledGet(SourceLocationAt(location)));
}
void lyra_rt_logic8_driver_set(void* driver, const void* value) {
  DriverOf<Logic8>(driver).Set(Read<Logic8>(value));
}

void lyra_rt_logic16_cell_construct(void* storage) {
  BuildAt<Var<Logic16>>(storage);
}
void lyra_rt_logic16_cell_destroy(void* storage) {
  std::destroy_at(&VariableAt<Logic16>(storage));
}
void lyra_rt_logic16_cell_initialize(
    void* cell, const void* prototype) noexcept {
  VariableAt<Logic16>(cell).Initialize(Read<Logic16>(prototype));
}
void lyra_rt_logic16_cell_set(void* cell, const void* value) {
  VariableAt<Logic16>(cell).Set(Read<Logic16>(value));
}
void lyra_rt_logic16_cell_arm_sampling(void* cell) {
  VariableAt<Logic16>(cell).ArmSampling();
}
auto lyra_rt_logic16_cell_sampled_get(void* cell, const void* location) -> const
    void* {
  return HandleTo(
      VariableAt<Logic16>(cell).SampledGet(SourceLocationAt(location)));
}
auto lyra_rt_logic16_cell_begin_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) -> std::int64_t {
  return CellBeginTakeover<Logic16>(
      cell, level, first, count, SourceLocationAt(location));
}
auto lyra_rt_logic16_cell_drive_takeover(
    void* cell, std::int64_t level, std::int64_t generation, const void* value,
    const void* location) -> bool {
  return CellDriveTakeover<Logic16>(
      cell, level, generation, value, SourceLocationAt(location));
}
void lyra_rt_logic16_cell_end_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) {
  CellEndTakeover<Logic16>(
      cell, level, first, count, SourceLocationAt(location));
}
auto lyra_rt_logic16_ref_begin_takeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) -> std::int64_t {
  return RefBeginTakeover<Logic16>(
      reference, level, first, count, SourceLocationAt(location));
}
auto lyra_rt_logic16_ref_drive_takeover(
    void* reference, std::int64_t level, std::int64_t generation,
    const void* value, const void* location) -> bool {
  return RefDriveTakeover<Logic16>(
      reference, level, generation, value, SourceLocationAt(location));
}
void lyra_rt_logic16_ref_end_takeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) {
  RefEndTakeover<Logic16>(
      reference, level, first, count, SourceLocationAt(location));
}
auto lyra_rt_logic16_cell_refer(void* cell, void* out) -> void* {
  return ReferToCell<Logic16>(cell, out);
}
auto lyra_rt_logic16_cell_open_for_write(void* cell, void* out) -> void* {
  return OpenCellWrite<Logic16>(cell, out);
}
auto lyra_rt_logic16_shared_cell_make(void* out) -> void* {
  return MakeSharedCell<Logic16>(out);
}
auto lyra_rt_logic16_ref_get(void* reference) -> const void* {
  return RefGet<Logic16>(reference);
}
void lyra_rt_logic16_ref_set(void* reference, const void* value) {
  RefSet<Logic16>(reference, value);
}
void lyra_rt_logic16_ref_arm_sampling(void* reference) {
  RefArmSampling<Logic16>(reference);
}
auto lyra_rt_logic16_ref_sampled_get(void* reference, const void* location)
    -> const void* {
  return RefSampledGet<Logic16>(reference, SourceLocationAt(location));
}
auto lyra_rt_logic16_ref_open_for_write(void* reference, void* out) -> void* {
  return OpenRefWrite<Logic16>(reference, out);
}
auto lyra_rt_logic16_value_cell_alloc() noexcept -> void* {
  return AllocateValueCell<Logic16>();
}
void lyra_rt_logic16_value_cell_construct(void* storage) {
  BuildAt<ValueCell<Logic16>>(storage);
}
void lyra_rt_logic16_sampled_history_construct(void* storage) {
  BuildAt<SampledHistory<Logic16>>(storage);
}
void lyra_rt_logic16_sampled_history_destroy(void* storage) {
  std::destroy_at(&HistoryAt<Logic16>(storage));
}
void lyra_rt_logic16_sampled_history_install(
    void* history, const void* default_value, std::int64_t depth) {
  HistoryAt<Logic16>(history).Install(Read<Logic16>(default_value), depth);
}
void lyra_rt_logic16_sampled_history_push(void* history, const void* value) {
  HistoryAt<Logic16>(history).Push(Read<Logic16>(value));
}
auto lyra_rt_logic16_sampled_history_at(
    const void* history, std::int64_t ticks_back) -> const void* {
  return HandleTo(
      static_cast<const SampledHistory<Logic16>*>(history)->At(ticks_back));
}
auto lyra_rt_logic16_land(const void* designation) noexcept -> void* {
  return LandDesignation<Logic16>(designation);
}
void lyra_rt_logic16_insert_designated_bits(
    const void* designation, std::int64_t width, void* outer_parts,
    std::uint64_t outer_parts_count, const void* start, const void* bits,
    std::int64_t bits_width, bool bits_are_four_state) {
  InsertDesignatedBits<Logic16>(
      designation, width, outer_parts, outer_parts_count, start, bits,
      bits_width, bits_are_four_state);
}
void lyra_rt_logic16_resolved_net_construct(std::int64_t width, void* storage) {
  BuildAt<ResolvedNet<Logic16>>(storage, static_cast<std::uint64_t>(width));
}
void lyra_rt_logic16_resolved_net_destroy(void* storage) {
  std::destroy_at(&NetOf<Logic16>(storage));
}
void lyra_rt_logic16_net_initialize_tri_state(
    void* net, std::int64_t fill, std::int64_t strength) {
  NetOf<Logic16>(net).InitializeTriState(fill, strength);
}
void lyra_rt_logic16_net_initialize_wired_and(
    void* net, std::int64_t fill, std::int64_t strength) {
  NetOf<Logic16>(net).InitializeWiredAnd(fill, strength);
}
void lyra_rt_logic16_net_initialize_wired_or(
    void* net, std::int64_t fill, std::int64_t strength) {
  NetOf<Logic16>(net).InitializeWiredOr(fill, strength);
}
void lyra_rt_logic16_net_initialize_retaining(
    void* net, std::int64_t fill, std::int64_t strength) {
  NetOf<Logic16>(net).InitializeRetaining(fill, strength);
}
auto lyra_rt_logic16_resolved_net_begin_takeover(
    void* net, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) -> std::int64_t {
  return NetOf<Logic16>(net).BeginTakeover(
      level, first, count, SourceLocationAt(location));
}
auto lyra_rt_logic16_resolved_net_drive_takeover(
    void* net, std::int64_t level, std::int64_t generation, const void* value,
    const void* location) -> bool {
  return NetOf<Logic16>(net).DriveTakeover(
      level, generation, Read<Logic16>(value), SourceLocationAt(location));
}
void lyra_rt_logic16_resolved_net_end_takeover(
    void* net, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) {
  NetOf<Logic16>(net).EndTakeover(
      level, first, count, SourceLocationAt(location));
}
void lyra_rt_logic16_resolved_net_arm_sampling(void* net) {
  NetOf<Logic16>(net).ArmSampling();
}
auto lyra_rt_logic16_resolved_net_sampled_get(void* net, const void* location)
    -> const void* {
  return HandleTo(NetOf<Logic16>(net).SampledGet(SourceLocationAt(location)));
}
void lyra_rt_logic16_driver_set(void* driver, const void* value) {
  DriverOf<Logic16>(driver).Set(Read<Logic16>(value));
}

void lyra_rt_logic32_cell_construct(void* storage) {
  BuildAt<Var<Logic32>>(storage);
}
void lyra_rt_logic32_cell_destroy(void* storage) {
  std::destroy_at(&VariableAt<Logic32>(storage));
}
void lyra_rt_logic32_cell_initialize(
    void* cell, const void* prototype) noexcept {
  VariableAt<Logic32>(cell).Initialize(Read<Logic32>(prototype));
}
void lyra_rt_logic32_cell_set(void* cell, const void* value) {
  VariableAt<Logic32>(cell).Set(Read<Logic32>(value));
}
void lyra_rt_logic32_cell_arm_sampling(void* cell) {
  VariableAt<Logic32>(cell).ArmSampling();
}
auto lyra_rt_logic32_cell_sampled_get(void* cell, const void* location) -> const
    void* {
  return HandleTo(
      VariableAt<Logic32>(cell).SampledGet(SourceLocationAt(location)));
}
auto lyra_rt_logic32_cell_begin_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) -> std::int64_t {
  return CellBeginTakeover<Logic32>(
      cell, level, first, count, SourceLocationAt(location));
}
auto lyra_rt_logic32_cell_drive_takeover(
    void* cell, std::int64_t level, std::int64_t generation, const void* value,
    const void* location) -> bool {
  return CellDriveTakeover<Logic32>(
      cell, level, generation, value, SourceLocationAt(location));
}
void lyra_rt_logic32_cell_end_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) {
  CellEndTakeover<Logic32>(
      cell, level, first, count, SourceLocationAt(location));
}
auto lyra_rt_logic32_ref_begin_takeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) -> std::int64_t {
  return RefBeginTakeover<Logic32>(
      reference, level, first, count, SourceLocationAt(location));
}
auto lyra_rt_logic32_ref_drive_takeover(
    void* reference, std::int64_t level, std::int64_t generation,
    const void* value, const void* location) -> bool {
  return RefDriveTakeover<Logic32>(
      reference, level, generation, value, SourceLocationAt(location));
}
void lyra_rt_logic32_ref_end_takeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) {
  RefEndTakeover<Logic32>(
      reference, level, first, count, SourceLocationAt(location));
}
auto lyra_rt_logic32_cell_refer(void* cell, void* out) -> void* {
  return ReferToCell<Logic32>(cell, out);
}
auto lyra_rt_logic32_cell_open_for_write(void* cell, void* out) -> void* {
  return OpenCellWrite<Logic32>(cell, out);
}
auto lyra_rt_logic32_shared_cell_make(void* out) -> void* {
  return MakeSharedCell<Logic32>(out);
}
auto lyra_rt_logic32_ref_get(void* reference) -> const void* {
  return RefGet<Logic32>(reference);
}
void lyra_rt_logic32_ref_set(void* reference, const void* value) {
  RefSet<Logic32>(reference, value);
}
void lyra_rt_logic32_ref_arm_sampling(void* reference) {
  RefArmSampling<Logic32>(reference);
}
auto lyra_rt_logic32_ref_sampled_get(void* reference, const void* location)
    -> const void* {
  return RefSampledGet<Logic32>(reference, SourceLocationAt(location));
}
auto lyra_rt_logic32_ref_open_for_write(void* reference, void* out) -> void* {
  return OpenRefWrite<Logic32>(reference, out);
}
auto lyra_rt_logic32_value_cell_alloc() noexcept -> void* {
  return AllocateValueCell<Logic32>();
}
void lyra_rt_logic32_value_cell_construct(void* storage) {
  BuildAt<ValueCell<Logic32>>(storage);
}
void lyra_rt_logic32_sampled_history_construct(void* storage) {
  BuildAt<SampledHistory<Logic32>>(storage);
}
void lyra_rt_logic32_sampled_history_destroy(void* storage) {
  std::destroy_at(&HistoryAt<Logic32>(storage));
}
void lyra_rt_logic32_sampled_history_install(
    void* history, const void* default_value, std::int64_t depth) {
  HistoryAt<Logic32>(history).Install(Read<Logic32>(default_value), depth);
}
void lyra_rt_logic32_sampled_history_push(void* history, const void* value) {
  HistoryAt<Logic32>(history).Push(Read<Logic32>(value));
}
auto lyra_rt_logic32_sampled_history_at(
    const void* history, std::int64_t ticks_back) -> const void* {
  return HandleTo(
      static_cast<const SampledHistory<Logic32>*>(history)->At(ticks_back));
}
auto lyra_rt_logic32_land(const void* designation) noexcept -> void* {
  return LandDesignation<Logic32>(designation);
}
void lyra_rt_logic32_insert_designated_bits(
    const void* designation, std::int64_t width, void* outer_parts,
    std::uint64_t outer_parts_count, const void* start, const void* bits,
    std::int64_t bits_width, bool bits_are_four_state) {
  InsertDesignatedBits<Logic32>(
      designation, width, outer_parts, outer_parts_count, start, bits,
      bits_width, bits_are_four_state);
}
void lyra_rt_logic32_resolved_net_construct(std::int64_t width, void* storage) {
  BuildAt<ResolvedNet<Logic32>>(storage, static_cast<std::uint64_t>(width));
}
void lyra_rt_logic32_resolved_net_destroy(void* storage) {
  std::destroy_at(&NetOf<Logic32>(storage));
}
void lyra_rt_logic32_net_initialize_tri_state(
    void* net, std::int64_t fill, std::int64_t strength) {
  NetOf<Logic32>(net).InitializeTriState(fill, strength);
}
void lyra_rt_logic32_net_initialize_wired_and(
    void* net, std::int64_t fill, std::int64_t strength) {
  NetOf<Logic32>(net).InitializeWiredAnd(fill, strength);
}
void lyra_rt_logic32_net_initialize_wired_or(
    void* net, std::int64_t fill, std::int64_t strength) {
  NetOf<Logic32>(net).InitializeWiredOr(fill, strength);
}
void lyra_rt_logic32_net_initialize_retaining(
    void* net, std::int64_t fill, std::int64_t strength) {
  NetOf<Logic32>(net).InitializeRetaining(fill, strength);
}
auto lyra_rt_logic32_resolved_net_begin_takeover(
    void* net, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) -> std::int64_t {
  return NetOf<Logic32>(net).BeginTakeover(
      level, first, count, SourceLocationAt(location));
}
auto lyra_rt_logic32_resolved_net_drive_takeover(
    void* net, std::int64_t level, std::int64_t generation, const void* value,
    const void* location) -> bool {
  return NetOf<Logic32>(net).DriveTakeover(
      level, generation, Read<Logic32>(value), SourceLocationAt(location));
}
void lyra_rt_logic32_resolved_net_end_takeover(
    void* net, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) {
  NetOf<Logic32>(net).EndTakeover(
      level, first, count, SourceLocationAt(location));
}
void lyra_rt_logic32_resolved_net_arm_sampling(void* net) {
  NetOf<Logic32>(net).ArmSampling();
}
auto lyra_rt_logic32_resolved_net_sampled_get(void* net, const void* location)
    -> const void* {
  return HandleTo(NetOf<Logic32>(net).SampledGet(SourceLocationAt(location)));
}
void lyra_rt_logic32_driver_set(void* driver, const void* value) {
  DriverOf<Logic32>(driver).Set(Read<Logic32>(value));
}

void lyra_rt_logic64_cell_construct(void* storage) {
  BuildAt<Var<Logic64>>(storage);
}
void lyra_rt_logic64_cell_destroy(void* storage) {
  std::destroy_at(&VariableAt<Logic64>(storage));
}
void lyra_rt_logic64_cell_initialize(
    void* cell, const void* prototype) noexcept {
  VariableAt<Logic64>(cell).Initialize(Read<Logic64>(prototype));
}
void lyra_rt_logic64_cell_set(void* cell, const void* value) {
  VariableAt<Logic64>(cell).Set(Read<Logic64>(value));
}
void lyra_rt_logic64_cell_arm_sampling(void* cell) {
  VariableAt<Logic64>(cell).ArmSampling();
}
auto lyra_rt_logic64_cell_sampled_get(void* cell, const void* location) -> const
    void* {
  return HandleTo(
      VariableAt<Logic64>(cell).SampledGet(SourceLocationAt(location)));
}
auto lyra_rt_logic64_cell_begin_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) -> std::int64_t {
  return CellBeginTakeover<Logic64>(
      cell, level, first, count, SourceLocationAt(location));
}
auto lyra_rt_logic64_cell_drive_takeover(
    void* cell, std::int64_t level, std::int64_t generation, const void* value,
    const void* location) -> bool {
  return CellDriveTakeover<Logic64>(
      cell, level, generation, value, SourceLocationAt(location));
}
void lyra_rt_logic64_cell_end_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) {
  CellEndTakeover<Logic64>(
      cell, level, first, count, SourceLocationAt(location));
}
auto lyra_rt_logic64_ref_begin_takeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) -> std::int64_t {
  return RefBeginTakeover<Logic64>(
      reference, level, first, count, SourceLocationAt(location));
}
auto lyra_rt_logic64_ref_drive_takeover(
    void* reference, std::int64_t level, std::int64_t generation,
    const void* value, const void* location) -> bool {
  return RefDriveTakeover<Logic64>(
      reference, level, generation, value, SourceLocationAt(location));
}
void lyra_rt_logic64_ref_end_takeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) {
  RefEndTakeover<Logic64>(
      reference, level, first, count, SourceLocationAt(location));
}
auto lyra_rt_logic64_cell_refer(void* cell, void* out) -> void* {
  return ReferToCell<Logic64>(cell, out);
}
auto lyra_rt_logic64_cell_open_for_write(void* cell, void* out) -> void* {
  return OpenCellWrite<Logic64>(cell, out);
}
auto lyra_rt_logic64_shared_cell_make(void* out) -> void* {
  return MakeSharedCell<Logic64>(out);
}
auto lyra_rt_logic64_ref_get(void* reference) -> const void* {
  return RefGet<Logic64>(reference);
}
void lyra_rt_logic64_ref_set(void* reference, const void* value) {
  RefSet<Logic64>(reference, value);
}
void lyra_rt_logic64_ref_arm_sampling(void* reference) {
  RefArmSampling<Logic64>(reference);
}
auto lyra_rt_logic64_ref_sampled_get(void* reference, const void* location)
    -> const void* {
  return RefSampledGet<Logic64>(reference, SourceLocationAt(location));
}
auto lyra_rt_logic64_ref_open_for_write(void* reference, void* out) -> void* {
  return OpenRefWrite<Logic64>(reference, out);
}
auto lyra_rt_logic64_value_cell_alloc() noexcept -> void* {
  return AllocateValueCell<Logic64>();
}
void lyra_rt_logic64_value_cell_construct(void* storage) {
  BuildAt<ValueCell<Logic64>>(storage);
}
void lyra_rt_logic64_sampled_history_construct(void* storage) {
  BuildAt<SampledHistory<Logic64>>(storage);
}
void lyra_rt_logic64_sampled_history_destroy(void* storage) {
  std::destroy_at(&HistoryAt<Logic64>(storage));
}
void lyra_rt_logic64_sampled_history_install(
    void* history, const void* default_value, std::int64_t depth) {
  HistoryAt<Logic64>(history).Install(Read<Logic64>(default_value), depth);
}
void lyra_rt_logic64_sampled_history_push(void* history, const void* value) {
  HistoryAt<Logic64>(history).Push(Read<Logic64>(value));
}
auto lyra_rt_logic64_sampled_history_at(
    const void* history, std::int64_t ticks_back) -> const void* {
  return HandleTo(
      static_cast<const SampledHistory<Logic64>*>(history)->At(ticks_back));
}
auto lyra_rt_logic64_land(const void* designation) noexcept -> void* {
  return LandDesignation<Logic64>(designation);
}
void lyra_rt_logic64_insert_designated_bits(
    const void* designation, std::int64_t width, void* outer_parts,
    std::uint64_t outer_parts_count, const void* start, const void* bits,
    std::int64_t bits_width, bool bits_are_four_state) {
  InsertDesignatedBits<Logic64>(
      designation, width, outer_parts, outer_parts_count, start, bits,
      bits_width, bits_are_four_state);
}
void lyra_rt_logic64_resolved_net_construct(std::int64_t width, void* storage) {
  BuildAt<ResolvedNet<Logic64>>(storage, static_cast<std::uint64_t>(width));
}
void lyra_rt_logic64_resolved_net_destroy(void* storage) {
  std::destroy_at(&NetOf<Logic64>(storage));
}
void lyra_rt_logic64_net_initialize_tri_state(
    void* net, std::int64_t fill, std::int64_t strength) {
  NetOf<Logic64>(net).InitializeTriState(fill, strength);
}
void lyra_rt_logic64_net_initialize_wired_and(
    void* net, std::int64_t fill, std::int64_t strength) {
  NetOf<Logic64>(net).InitializeWiredAnd(fill, strength);
}
void lyra_rt_logic64_net_initialize_wired_or(
    void* net, std::int64_t fill, std::int64_t strength) {
  NetOf<Logic64>(net).InitializeWiredOr(fill, strength);
}
void lyra_rt_logic64_net_initialize_retaining(
    void* net, std::int64_t fill, std::int64_t strength) {
  NetOf<Logic64>(net).InitializeRetaining(fill, strength);
}
auto lyra_rt_logic64_resolved_net_begin_takeover(
    void* net, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) -> std::int64_t {
  return NetOf<Logic64>(net).BeginTakeover(
      level, first, count, SourceLocationAt(location));
}
auto lyra_rt_logic64_resolved_net_drive_takeover(
    void* net, std::int64_t level, std::int64_t generation, const void* value,
    const void* location) -> bool {
  return NetOf<Logic64>(net).DriveTakeover(
      level, generation, Read<Logic64>(value), SourceLocationAt(location));
}
void lyra_rt_logic64_resolved_net_end_takeover(
    void* net, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) {
  NetOf<Logic64>(net).EndTakeover(
      level, first, count, SourceLocationAt(location));
}
void lyra_rt_logic64_resolved_net_arm_sampling(void* net) {
  NetOf<Logic64>(net).ArmSampling();
}
auto lyra_rt_logic64_resolved_net_sampled_get(void* net, const void* location)
    -> const void* {
  return HandleTo(NetOf<Logic64>(net).SampledGet(SourceLocationAt(location)));
}
void lyra_rt_logic64_driver_set(void* driver, const void* value) {
  DriverOf<Logic64>(driver).Set(Read<Logic64>(value));
}

void lyra_rt_bit_wide_cell_construct(std::int64_t width, void* storage) {
  BuildAt<lyra::runtime::VariableOf<BitWide>::Type>(
      storage, static_cast<std::uint64_t>(width));
}
void lyra_rt_bit_wide_cell_destroy(void* storage) {
  std::destroy_at(&lyra::runtime::VariableAt<BitWide>(storage));
}
void lyra_rt_bit_wide_cell_initialize(
    void* cell, const void* prototype) noexcept {
  lyra::runtime::VariableAt<BitWide>(cell).Initialize(prototype);
}
void lyra_rt_bit_wide_cell_set(void* cell, const void* value) {
  lyra::runtime::VariableAt<BitWide>(cell).SetBytes(value);
}
void lyra_rt_bit_wide_cell_arm_sampling(void* cell) {
  lyra::runtime::VariableAt<BitWide>(cell).ArmSampling();
}
auto lyra_rt_bit_wide_cell_sampled_get(void* cell, const void* location)
    -> const void* {
  return lyra::runtime::VariableAt<BitWide>(cell).SampledBytes(
      SourceLocationAt(location));
}
auto lyra_rt_bit_wide_cell_begin_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) -> std::int64_t {
  return lyra::runtime::VariableAt<BitWide>(cell).BeginTakeover(
      level, first, count, SourceLocationAt(location));
}
auto lyra_rt_bit_wide_cell_drive_takeover(
    void* cell, std::int64_t level, std::int64_t generation, const void* value,
    const void* location) -> bool {
  return lyra::runtime::VariableAt<BitWide>(cell).DriveTakeoverBytes(
      level, generation, value, SourceLocationAt(location));
}
void lyra_rt_bit_wide_cell_end_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) {
  lyra::runtime::VariableAt<BitWide>(cell).EndTakeover(
      level, first, count, SourceLocationAt(location));
}
auto lyra_rt_bit_wide_ref_begin_takeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) -> std::int64_t {
  return lyra::runtime::WideRefBeginTakeover<BitWide>(
      reference, level, first, count, SourceLocationAt(location));
}
auto lyra_rt_bit_wide_ref_drive_takeover(
    void* reference, std::int64_t level, std::int64_t generation,
    const void* value, const void* location) -> bool {
  return lyra::runtime::WideRefDriveTakeover<BitWide>(
      reference, level, generation, value, SourceLocationAt(location));
}
void lyra_rt_bit_wide_ref_end_takeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) {
  lyra::runtime::EndTakeoverThrough<lyra::runtime::VariableOf<BitWide>::Type>(
      lyra::runtime::ErasedAt(reference), level, first, count,
      SourceLocationAt(location));
}
auto lyra_rt_bit_wide_cell_refer(void* cell, void* out) -> void* {
  return lyra::runtime::ReferToCell<BitWide>(cell, out);
}
auto lyra_rt_bit_wide_cell_open_for_write(void* cell, void* out) -> void* {
  return OpenTailCellWrite<BitWide>(cell, out);
}
auto lyra_rt_bit_wide_shared_cell_make(std::int64_t width, void* out) -> void* {
  return MakeSharedCell<BitWide>(out, static_cast<std::uint64_t>(width));
}
auto lyra_rt_bit_wide_ref_get(void* reference) -> const void* {
  return ErasedAt(reference).storage;
}
void lyra_rt_bit_wide_ref_set(
    void* reference, std::int64_t width, const void* value) {
  lyra::runtime::WideRefSet<BitWide>(reference, width, value);
}
void lyra_rt_bit_wide_ref_arm_sampling(void* reference) {
  ArmWholeVariable<lyra::runtime::VariableOf<BitWide>::Type>(reference);
}
auto lyra_rt_bit_wide_ref_sampled_get(void* reference, const void* location)
    -> const void* {
  return TailRefSampledGet<BitWide>(reference, SourceLocationAt(location));
}
auto lyra_rt_bit_wide_ref_open_for_write(
    void* reference, std::int64_t width, void* out) -> void* {
  return OpenTailRefWrite<BitWide>(
      reference, static_cast<std::uint64_t>(width), out);
}
auto lyra_rt_bit_wide_value_cell_alloc(std::int64_t width) noexcept -> void* {
  return AllocateValueCell<BitWide>(static_cast<std::uint64_t>(width));
}
void lyra_rt_bit_wide_value_cell_construct(std::int64_t width, void* storage) {
  BuildAt<ValueCellFor<BitWide>::Type>(
      storage, static_cast<std::uint64_t>(width));
}
void lyra_rt_bit_wide_value_cell_destroy(void* storage) {
  std::destroy_at(&ValueCellAt<BitWide>(storage));
}
void lyra_rt_bit_wide_sampled_history_construct(void* storage) {
  BuildAt<SampledHistory<BitWide>>(storage);
}
void lyra_rt_bit_wide_sampled_history_destroy(void* storage) {
  std::destroy_at(&HistoryAt<BitWide>(storage));
}
void lyra_rt_bit_wide_sampled_history_install(
    void* history, const void* default_value, std::int64_t width,
    std::int64_t depth) {
  HistoryAt<BitWide>(history).Install(
      lyra::runtime::WideOf<BitWide>(width, default_value), depth);
}
void lyra_rt_bit_wide_sampled_history_push(void* history, const void* value) {
  HistoryAt<BitWide>(history).PushBytes(value);
}
auto lyra_rt_bit_wide_sampled_history_at(
    const void* history, std::int64_t ticks_back) -> const void* {
  return HandleTo(
      static_cast<const SampledHistory<BitWide>*>(history)->At(ticks_back));
}
auto lyra_rt_bit_wide_land(const void* designation, std::int64_t width) noexcept
    -> void* {
  return lyra::runtime::LandWideDesignation<BitWide>(designation, width);
}
void lyra_rt_bit_wide_insert_designated_bits(
    const void* designation, std::int64_t width, void* outer_parts,
    std::uint64_t outer_parts_count, const void* start, const void* bits,
    std::int64_t bits_width, bool bits_are_four_state) {
  lyra::runtime::InsertDesignatedWideBits<BitWide>(
      designation, width, outer_parts, outer_parts_count, start, bits,
      bits_width, bits_are_four_state);
}
void lyra_rt_bit_wide_resolved_net_construct(
    std::int64_t width, void* storage) {
  BuildAt<ResolvedNet<BitWide>>(storage, static_cast<std::uint64_t>(width));
}
void lyra_rt_bit_wide_resolved_net_destroy(void* storage) {
  std::destroy_at(&NetOf<BitWide>(storage));
}
void lyra_rt_bit_wide_net_initialize_tri_state(
    void* net, std::int64_t fill, std::int64_t strength) {
  NetOf<BitWide>(net).InitializeTriState(fill, strength);
}
void lyra_rt_bit_wide_net_initialize_wired_and(
    void* net, std::int64_t fill, std::int64_t strength) {
  NetOf<BitWide>(net).InitializeWiredAnd(fill, strength);
}
void lyra_rt_bit_wide_net_initialize_wired_or(
    void* net, std::int64_t fill, std::int64_t strength) {
  NetOf<BitWide>(net).InitializeWiredOr(fill, strength);
}
void lyra_rt_bit_wide_net_initialize_retaining(
    void* net, std::int64_t fill, std::int64_t strength) {
  NetOf<BitWide>(net).InitializeRetaining(fill, strength);
}
auto lyra_rt_bit_wide_resolved_net_begin_takeover(
    void* net, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) -> std::int64_t {
  return NetOf<BitWide>(net).BeginTakeover(
      level, first, count, SourceLocationAt(location));
}
auto lyra_rt_bit_wide_resolved_net_drive_takeover(
    void* net, std::int64_t level, std::int64_t generation, const void* value,
    const void* location) -> bool {
  return NetOf<BitWide>(net).DriveTakeoverBytes(
      level, generation, value, SourceLocationAt(location));
}
void lyra_rt_bit_wide_resolved_net_end_takeover(
    void* net, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) {
  NetOf<BitWide>(net).EndTakeover(
      level, first, count, SourceLocationAt(location));
}
void lyra_rt_bit_wide_resolved_net_arm_sampling(void* net) {
  NetOf<BitWide>(net).ArmSampling();
}
auto lyra_rt_bit_wide_resolved_net_sampled_get(void* net, const void* location)
    -> const void* {
  return HandleTo(NetOf<BitWide>(net).SampledGet(SourceLocationAt(location)));
}
void lyra_rt_bit_wide_driver_set(void* driver, const void* value) {
  DriverOf<BitWide>(driver).SetBytes(value);
}

void lyra_rt_logic_wide_cell_construct(std::int64_t width, void* storage) {
  BuildAt<lyra::runtime::VariableOf<LogicWide>::Type>(
      storage, static_cast<std::uint64_t>(width));
}
void lyra_rt_logic_wide_cell_destroy(void* storage) {
  std::destroy_at(&lyra::runtime::VariableAt<LogicWide>(storage));
}
void lyra_rt_logic_wide_cell_initialize(
    void* cell, const void* prototype) noexcept {
  lyra::runtime::VariableAt<LogicWide>(cell).Initialize(prototype);
}
void lyra_rt_logic_wide_cell_set(void* cell, const void* value) {
  lyra::runtime::VariableAt<LogicWide>(cell).SetBytes(value);
}
void lyra_rt_logic_wide_cell_arm_sampling(void* cell) {
  lyra::runtime::VariableAt<LogicWide>(cell).ArmSampling();
}
auto lyra_rt_logic_wide_cell_sampled_get(void* cell, const void* location)
    -> const void* {
  return lyra::runtime::VariableAt<LogicWide>(cell).SampledBytes(
      SourceLocationAt(location));
}
auto lyra_rt_logic_wide_cell_begin_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) -> std::int64_t {
  return lyra::runtime::VariableAt<LogicWide>(cell).BeginTakeover(
      level, first, count, SourceLocationAt(location));
}
auto lyra_rt_logic_wide_cell_drive_takeover(
    void* cell, std::int64_t level, std::int64_t generation, const void* value,
    const void* location) -> bool {
  return lyra::runtime::VariableAt<LogicWide>(cell).DriveTakeoverBytes(
      level, generation, value, SourceLocationAt(location));
}
void lyra_rt_logic_wide_cell_end_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) {
  lyra::runtime::VariableAt<LogicWide>(cell).EndTakeover(
      level, first, count, SourceLocationAt(location));
}
auto lyra_rt_logic_wide_ref_begin_takeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) -> std::int64_t {
  return lyra::runtime::WideRefBeginTakeover<LogicWide>(
      reference, level, first, count, SourceLocationAt(location));
}
auto lyra_rt_logic_wide_ref_drive_takeover(
    void* reference, std::int64_t level, std::int64_t generation,
    const void* value, const void* location) -> bool {
  return lyra::runtime::WideRefDriveTakeover<LogicWide>(
      reference, level, generation, value, SourceLocationAt(location));
}
void lyra_rt_logic_wide_ref_end_takeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) {
  lyra::runtime::EndTakeoverThrough<lyra::runtime::VariableOf<LogicWide>::Type>(
      lyra::runtime::ErasedAt(reference), level, first, count,
      SourceLocationAt(location));
}
auto lyra_rt_logic_wide_cell_refer(void* cell, void* out) -> void* {
  return lyra::runtime::ReferToCell<LogicWide>(cell, out);
}
auto lyra_rt_logic_wide_cell_open_for_write(void* cell, void* out) -> void* {
  return OpenTailCellWrite<LogicWide>(cell, out);
}
auto lyra_rt_logic_wide_shared_cell_make(std::int64_t width, void* out)
    -> void* {
  return MakeSharedCell<LogicWide>(out, static_cast<std::uint64_t>(width));
}
auto lyra_rt_logic_wide_ref_get(void* reference) -> const void* {
  return ErasedAt(reference).storage;
}
void lyra_rt_logic_wide_ref_set(
    void* reference, std::int64_t width, const void* value) {
  lyra::runtime::WideRefSet<LogicWide>(reference, width, value);
}
void lyra_rt_logic_wide_ref_arm_sampling(void* reference) {
  ArmWholeVariable<lyra::runtime::VariableOf<LogicWide>::Type>(reference);
}
auto lyra_rt_logic_wide_ref_sampled_get(void* reference, const void* location)
    -> const void* {
  return TailRefSampledGet<LogicWide>(reference, SourceLocationAt(location));
}
auto lyra_rt_logic_wide_ref_open_for_write(
    void* reference, std::int64_t width, void* out) -> void* {
  return OpenTailRefWrite<LogicWide>(
      reference, static_cast<std::uint64_t>(width), out);
}
auto lyra_rt_logic_wide_value_cell_alloc(std::int64_t width) noexcept -> void* {
  return AllocateValueCell<LogicWide>(static_cast<std::uint64_t>(width));
}
void lyra_rt_logic_wide_value_cell_construct(
    std::int64_t width, void* storage) {
  BuildAt<ValueCellFor<LogicWide>::Type>(
      storage, static_cast<std::uint64_t>(width));
}
void lyra_rt_logic_wide_value_cell_destroy(void* storage) {
  std::destroy_at(&ValueCellAt<LogicWide>(storage));
}
void lyra_rt_logic_wide_sampled_history_construct(void* storage) {
  BuildAt<SampledHistory<LogicWide>>(storage);
}
void lyra_rt_logic_wide_sampled_history_destroy(void* storage) {
  std::destroy_at(&HistoryAt<LogicWide>(storage));
}
void lyra_rt_logic_wide_sampled_history_install(
    void* history, const void* default_value, std::int64_t width,
    std::int64_t depth) {
  HistoryAt<LogicWide>(history).Install(
      lyra::runtime::WideOf<LogicWide>(width, default_value), depth);
}
void lyra_rt_logic_wide_sampled_history_push(void* history, const void* value) {
  HistoryAt<LogicWide>(history).PushBytes(value);
}
auto lyra_rt_logic_wide_sampled_history_at(
    const void* history, std::int64_t ticks_back) -> const void* {
  return HandleTo(
      static_cast<const SampledHistory<LogicWide>*>(history)->At(ticks_back));
}
auto lyra_rt_logic_wide_land(
    const void* designation, std::int64_t width) noexcept -> void* {
  return lyra::runtime::LandWideDesignation<LogicWide>(designation, width);
}
void lyra_rt_logic_wide_insert_designated_bits(
    const void* designation, std::int64_t width, void* outer_parts,
    std::uint64_t outer_parts_count, const void* start, const void* bits,
    std::int64_t bits_width, bool bits_are_four_state) {
  lyra::runtime::InsertDesignatedWideBits<LogicWide>(
      designation, width, outer_parts, outer_parts_count, start, bits,
      bits_width, bits_are_four_state);
}
void lyra_rt_logic_wide_resolved_net_construct(
    std::int64_t width, void* storage) {
  BuildAt<ResolvedNet<LogicWide>>(storage, static_cast<std::uint64_t>(width));
}
void lyra_rt_logic_wide_resolved_net_destroy(void* storage) {
  std::destroy_at(&NetOf<LogicWide>(storage));
}
void lyra_rt_logic_wide_net_initialize_tri_state(
    void* net, std::int64_t fill, std::int64_t strength) {
  NetOf<LogicWide>(net).InitializeTriState(fill, strength);
}
void lyra_rt_logic_wide_net_initialize_wired_and(
    void* net, std::int64_t fill, std::int64_t strength) {
  NetOf<LogicWide>(net).InitializeWiredAnd(fill, strength);
}
void lyra_rt_logic_wide_net_initialize_wired_or(
    void* net, std::int64_t fill, std::int64_t strength) {
  NetOf<LogicWide>(net).InitializeWiredOr(fill, strength);
}
void lyra_rt_logic_wide_net_initialize_retaining(
    void* net, std::int64_t fill, std::int64_t strength) {
  NetOf<LogicWide>(net).InitializeRetaining(fill, strength);
}
auto lyra_rt_logic_wide_resolved_net_begin_takeover(
    void* net, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) -> std::int64_t {
  return NetOf<LogicWide>(net).BeginTakeover(
      level, first, count, SourceLocationAt(location));
}
auto lyra_rt_logic_wide_resolved_net_drive_takeover(
    void* net, std::int64_t level, std::int64_t generation, const void* value,
    const void* location) -> bool {
  return NetOf<LogicWide>(net).DriveTakeoverBytes(
      level, generation, value, SourceLocationAt(location));
}
void lyra_rt_logic_wide_resolved_net_end_takeover(
    void* net, std::int64_t level, std::int64_t first, std::int64_t count,
    const void* location) {
  NetOf<LogicWide>(net).EndTakeover(
      level, first, count, SourceLocationAt(location));
}
void lyra_rt_logic_wide_resolved_net_arm_sampling(void* net) {
  NetOf<LogicWide>(net).ArmSampling();
}
auto lyra_rt_logic_wide_resolved_net_sampled_get(
    void* net, const void* location) -> const void* {
  return HandleTo(NetOf<LogicWide>(net).SampledGet(SourceLocationAt(location)));
}
void lyra_rt_logic_wide_driver_set(void* driver, const void* value) {
  DriverOf<LogicWide>(driver).SetBytes(value);
}
}
