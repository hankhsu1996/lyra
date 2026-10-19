#include "lyra/runtime/runtime_abi.hpp"

#include <algorithm>
#include <array>
#include <coroutine>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <cxxabi.h>
#include <deque>
#include <exception>
#include <format>
#include <memory>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/function_ref.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/runtime/ambient_run_context.hpp"
#include "lyra/runtime/cancellation.hpp"
#include "lyra/runtime/closure.hpp"
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
#include "lyra/runtime/named_event.hpp"
#include "lyra/runtime/nba_region.hpp"
#include "lyra/runtime/net.hpp"
#include "lyra/runtime/object_change.hpp"
#include "lyra/runtime/object_ref.hpp"
#include "lyra/runtime/open_write.hpp"
#include "lyra/runtime/plusargs.hpp"
#include "lyra/runtime/process_control.hpp"
#include "lyra/runtime/property_wait.hpp"
#include "lyra/runtime/random.hpp"
#include "lyra/runtime/reaction.hpp"
#include "lyra/runtime/read_report.hpp"
#include "lyra/runtime/runtime.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/sampled_history.hpp"
#include "lyra/runtime/scope.hpp"
#include "lyra/runtime/shared_pointer.hpp"
#include "lyra/runtime/sim_time.hpp"
#include "lyra/runtime/simulation_entry.hpp"
#include "lyra/runtime/timer.hpp"
#include "lyra/runtime/value_cell.hpp"
#include "lyra/runtime/value_change_wait.hpp"
#include "lyra/runtime/value_families.hpp"
#include "lyra/runtime/value_handle.hpp"
#include "lyra/runtime/var.hpp"
#include "lyra/support/callable_bounds.hpp"
#include "lyra/value/any_value.hpp"
#include "lyra/value/assert.hpp"
#include "lyra/value/chandle.hpp"
#include "lyra/value/dpi_canonical.hpp"
#include "lyra/value/dpi_open_array.hpp"
#include "lyra/value/element_type.hpp"
#include "lyra/value/empty.hpp"
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
#include "lyra/value/managed_ref.hpp"
#include "lyra/value/queue_type.hpp"
#include "lyra/value/real.hpp"
#include "lyra/value/runtime_array_ordering.hpp"
#include "lyra/value/runtime_associative_array.hpp"
#include "lyra/value/runtime_dynamic_array.hpp"
#include "lyra/value/runtime_memory.hpp"
#include "lyra/value/runtime_queue.hpp"
#include "lyra/value/scan.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/tuple_type.hpp"
#include "lyra/value/unpacked_array.hpp"
#include "lyra/value/unpacked_range.hpp"
#include "lyra/value/wide.hpp"

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
auto TakeBranches(LyraSpan branches) -> std::vector<Coroutine<void>> {
  const std::span<Coroutine<void>* const> handles(
      static_cast<Coroutine<void>* const*>(branches.data), branches.count);
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

// A variable of `T`, and a procedural local of one, behind the addresses the
// ABI carries them as.
template <class T>
auto VariableAt(void* cell) -> typename VariableOf<T>::Type& {
  return *static_cast<typename VariableOf<T>::Type*>(cell);
}

template <class T>
auto LocalAt(void* cell) -> typename LocalOf<T>::Type& {
  return *static_cast<typename LocalOf<T>::Type*>(cell);
}

// A counted hold on a new variable of `T`, told the width or the type of a
// value it keeps after itself.
template <class T, class... Told>
auto MakeSharedCell(void* out, const Told&... told) -> void* {
  if constexpr (DynamicallySized<T>) {
    return std::construct_at(
        static_cast<SharedPointer*>(out),
        SharedPointer::MakeTailCell<typename StorageFor<T>::Type>(told...));
  } else {
    return std::construct_at(
        static_cast<SharedPointer*>(out), SharedPointer::MakeCell<T>());
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
auto RefSampledGet(void* reference) -> const void* {
  return HandleTo(LentAt<T>(reference).SampledGet());
}

// A procedural continuous assignment on a variable, or on part of it (LRM
// 10.6), reached through its cell or through storage a caller lent.
template <value::LyraValue T>
auto CellBeginTakeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count)
    -> std::int64_t {
  return VariableAt<T>(cell).BeginTakeover(level, first, count);
}

template <value::LyraValue T>
auto CellDriveTakeover(
    void* cell, std::int64_t level, std::int64_t generation, const void* value)
    -> bool {
  return VariableAt<T>(cell).DriveTakeover(level, generation, Read<T>(value));
}

template <value::LyraValue T>
void CellEndTakeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count) {
  VariableAt<T>(cell).EndTakeover(level, first, count);
}

template <value::LyraValue T>
auto RefBeginTakeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count)
    -> std::int64_t {
  return LentAt<T>(reference).BeginTakeover(level, first, count);
}

template <value::LyraValue T>
auto RefDriveTakeover(
    void* reference, std::int64_t level, std::int64_t generation,
    const void* value) -> bool {
  return LentAt<T>(reference).DriveTakeover(level, generation, Read<T>(value));
}

template <value::LyraValue T>
void RefEndTakeover(
    void* reference, std::int64_t level, std::int64_t first,
    std::int64_t count) {
  LentAt<T>(reference).EndTakeover(level, first, count);
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
auto TailRefSampledGet(void* reference) -> const void* {
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
                .SampledBytes();
          },
          [](GcObject*) -> const void* {
            throw SimulationError(
                "a sampled value of a class property lent by reference is not "
                "yet supported");
          },
          [&](HeldByNet) -> const void* { return lent.storage; }},
      lent.holder);
}

// The integral type an entry is told where it builds values of one and keeps
// them with it -- a key of an associative memory, an element of a byte array.
auto IntegralTypeAt(const void* type) -> const value::IntegralValueType& {
  return *static_cast<const value::IntegralValueType*>(type);
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

// The type of a queue an entry building one is told, which states its
// elements and its bound.
auto QueueTypeAt(const void* type) -> const value::QueueType& {
  return *static_cast<const value::QueueType*>(type);
}

// The element type an entry is told: by one building a dynamic array or an
// associative array, which is all such an array learns of its elements, and
// by one asking what the type answers for an index naming no element.
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
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count)
    -> std::int64_t {
  using Words = typename VariableOf<T>::Type;
  return BeginTakeoverThrough<Words>(
      ErasedAt(reference), level, first, count,
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
    const void* value) -> bool {
  using Words = typename VariableOf<T>::Type;
  return DriveTakeoverThrough<Words>(
      ErasedAt(reference), level, [&](Words& variable) {
        return variable.DriveTakeoverBytes(level, generation, value);
      });
}

// The same through a reference to a value laid out by its type, whose member
// is given a cell laid out by the type of the cell it showed.
auto LaidOutRefBeginTakeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count)
    -> std::int64_t {
  using LaidOut = VariableOf<value::LaidOutValue>::Type;
  return BeginTakeoverThrough<LaidOut>(
      ErasedAt(reference), level, first, count,
      [](const LaidOut& showing) {
        std::shared_ptr<LaidOut> forced =
            MakeTailCell<LaidOutStorage>(showing.HeldStorage().Type());
        forced->Initialize(showing.Bytes());
        return forced;
      },
      [](LaidOut& forced) { return WholeReference(forced); });
}

auto LaidOutRefDriveTakeover(
    void* reference, std::int64_t level, std::int64_t generation,
    const void* value) -> bool {
  using LaidOut = VariableOf<value::LaidOutValue>::Type;
  return DriveTakeoverThrough<LaidOut>(
      ErasedAt(reference), level, [&](LaidOut& variable) {
        return variable.DriveTakeoverBytes(level, generation, value);
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
      [&] { return ReplaceWhole(held, assign); });
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
  void PublishTransition(const PartPath& reached, const Change& change) const {
    lent_.Report(reached, change);
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
  void PublishTransition(const PartPath& reached, const Change& change) const {
    holder_->PublishTransition(reached, change);
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

// The same over a value laid out by its type -- a structure, a union, a tuple,
// a fixed-size unpacked array -- which a reference names as its bytes where it
// lies: no object of the runtime's stands for one there, since a component, a
// member or an element of one is bytes of what holds it.
// Nothing in those bytes says what they are, so an access that has to know is
// told the type.
void LaidOutRefSet(
    void* reference, const value::ValueType& type, const void* value) {
  const ErasedReference& lent = ErasedAt(reference);
  const auto assign = [&] { type.Assign(lent.storage, value); };
  SetThrough(
      lent,
      [&] {
        ApplyOverriddenWriteThrough<value::LaidOutValue>(lent, value, assign);
      },
      assign, [&] { return type.BitIdentical(lent.storage, value); },
      [&] {
        assign();
        return Change::Whole();
      });
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
auto DriverOf(void* driver) -> Driver<T>& {
  return *static_cast<Driver<T>*>(driver);
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

// A driver told the object it carries the whole of, and asked at its turn
// whether its net shows that object.
template <NetValue T>
void DriveFromVariable(void* driver, void* cell) {
  DriverOf<T>(driver).DriveFrom(&VariableAt<T>(cell));
}

template <NetValue T>
void DriveFromNet(void* driver, void* net) {
  DriverOf<T>(driver).DriveFrom(&NetOf<T>(net));
}

template <NetValue T>
void DriveFromReference(void* driver, void* reference) {
  DriverOf<T>(driver).DriveFrom(static_cast<ErasedReference*>(reference));
}

template <NetValue T>
auto NetReportsTo(void* net) -> void* {
  return ReportsTo(NetOf<T>(net)).Word();
}

template <NetValue T>
auto ShowSource(void* driver) -> bool {
  return DriverOf<T>(driver).ShowSource();
}

// The process a `process` handle names (LRM 9.7), recovered from the erased
// share the managed-reference domain carries. Taking a typed owner is what
// keeps the node alive for the length of the call: the entry is the one place
// that knows which object the share is of, which is what erasing it costs and
// all it costs.
auto ProcessOf(const void* handle) -> value::ObjectRef {
  return RefToObject(
      std::static_pointer_cast<RuntimeProcess>(
          Read<value::ObjectRef>(handle).Handle().Share()));
}

// The type an erased value crosses beside, which is what states its
// representation where nothing on this side could have read it off anything
// else.
auto TypeAt(const void* type) -> const value::ValueType& {
  return *static_cast<const value::ValueType*>(type);
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

// An index that crossed with its type, read where it lies.
auto IndexAt(const void* index, const void* type) -> value::IndexView {
  return {.bytes = index, .type = &TypeAt(type)};
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

// The steps and the landing a write in progress takes, each over the value a
// designation names; they do what a designation's own methods do where the
// value's type is known.
template <typename Container, typename Index>
auto DesignateElement(const void* designation, const Index& index, void* out)
    -> void* {
  const ErasedDesignation& within = DesignationAt(designation);
  value::Formation formed{};
  void* element =
      static_cast<Container*>(within.part)->ElementMut(index, formed);
  within.write->Formed(formed);
  if (within.write->Undecided()) {
    within.write->Stepped(StepTo(index));
  }
  return std::construct_at(
      static_cast<ErasedDesignation*>(out),
      ErasedDesignation{.write = within.write, .part = element});
}

template <typename Container, typename Replacement>
void AssignDesignatedSlice(
    const void* designation, std::optional<std::int64_t> start,
    std::int64_t count, const Replacement& replacement) {
  const ErasedDesignation& within = DesignationAt(designation);
  // A start that names no position assigns nothing, so one that moved an
  // element names one.
  if (static_cast<Container*>(within.part)
          ->AssignSlice(start, count, replacement)) {
    within.write->Moved(*start, count);
  }
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

auto DesignateArrayElement(
    const void* designation, const value::FixedArrayType& type,
    std::optional<std::int64_t> position, void* out) -> void* {
  const ErasedDesignation& within = DesignationAt(designation);
  value::Formation formed{};
  void* element = type.ElementMut(within.part, position, formed);
  within.write->Formed(formed);
  if (within.write->Undecided()) {
    within.write->Stepped(StepTo(position));
  }
  return std::construct_at(
      static_cast<ErasedDesignation*>(out),
      ErasedDesignation{.write = within.write, .part = element});
}

void AssignDesignatedArraySlice(
    const void* designation, const value::FixedArrayType& type,
    std::optional<std::int64_t> start, std::int64_t count,
    std::span<const void* const> replacement) {
  const ErasedDesignation& within = DesignationAt(designation);
  // A start that names no position assigns nothing, so one that moved an
  // element names one.
  if (type.AssignSlice(
          within.part, start, static_cast<std::size_t>(count), replacement)) {
    within.write->Moved(*start, count);
  }
}

// A reference to storage nothing is told about -- a variable of a function, a
// closure's own copy of a variable -- which a write through it only writes.
auto ReferToStorage(void* storage, void* out) -> void* {
  return BuildReference(out, ErasedReference{.holder = {}, .storage = storage});
}

// A reference to a property of an object, held by the object so a write
// through it tells the object. Reaching a property through a handle naming no
// object is the design's own failure (LRM 8.4).
auto ReferToProperty(void* object, void* storage, void* out) -> void* {
  if (object == nullptr) {
    value::RaiseNullObjectHandleAccess();
  }
  return BuildReference(
      out, ErasedReference{
               .holder = static_cast<GcObject*>(object), .storage = storage});
}

// The steps a reference takes into a part of what it names, each over the
// value the reference names; they do what a reference's own methods do where
// the value's type is known.
template <typename Container, typename Index>
auto ReferElement(const void* reference, const Index& index, void* out)
    -> void* {
  const ErasedReference& from = *static_cast<const ErasedReference*>(reference);
  from.AdmitStep();
  value::Formation formed{};
  void* element =
      static_cast<Container*>(from.storage)->ElementMut(index, formed);
  return BuildReference(
      out, from.Part(element, formed, RetainedStepTo<Container>(index)));
}

// The same step into a fixed-size unpacked array, which the reference names as
// its elements where they lie, told the array's type.
auto ReferArrayElement(
    const void* reference, const value::FixedArrayType& type,
    std::optional<std::int64_t> position, void* out) -> void* {
  const ErasedReference& from = ErasedAt(reference);
  from.AdmitStep();
  value::Formation formed{};
  void* element = type.ElementMut(from.storage, position, formed);
  return BuildReference(out, from.Part(element, formed, StepTo(position)));
}

// Builds a copy of one element of `type`, in the storage the call handed over.
auto ElementInto(void* out, const value::ValueType& type, const void* element)
    -> void* {
  type.Copy(element, out);
  return out;
}

// Stores each of a literal's entries under the index it names (LRM 7.9.11). An
// entry is the product of the two, of `entry_type`, which states each one's
// type, so nothing here converts either.
void StoreLiteralEntries(
    value::RuntimeAssociativeArray& array, LyraSpan entries,
    const value::TupleType& entry_type) {
  const std::span<const void* const> handles(
      static_cast<const void* const*>(entries.data), entries.count);
  const value::TupleComponent& index = entry_type.Components()[0];
  for (const void* entry : handles) {
    const void* const stated = entry_type.ComponentAt(entry, 0);
    // A wildcard index is held with the type it was written in (LRM 7.8.1),
    // which is the type it is stored and compared by.
    if (index.type == &value::lyra_rt_wildcard_index_value_type) {
      const auto& held = *static_cast<const value::AnyValue*>(stated);
      array.Store(
          value::IndexView{.bytes = held.Bytes(), .type = &held.Type()},
          entry_type.ComponentAt(entry, 1));
      continue;
    }
    array.Store(
        value::IndexView{.bytes = stated, .type = index.type},
        entry_type.ComponentAt(entry, 1));
  }
}

// The element count `new[N]` asks for (LRM 7.5.1), which the design computes,
// so a negative one is its own failure.
auto NewCount(std::int64_t count) -> std::size_t {
  if (count < 0) {
    throw SimulationError(
        "the size given to a dynamic array's new[] is negative");
  }
  return static_cast<std::size_t>(count);
}

// A literal's element handles repeated `count` times (LRM 10.9.1). An
// enumerated element list is this with a count of one, which is why a uniform
// array, a replicated pattern and a plain list all reach one entry. Each handle
// is an element where the literal laid it out, which the container copies.
auto ReplicateHandles(LyraSpan unit, std::int64_t count)
    -> std::vector<const void*> {
  const std::span<const void* const> handles(
      static_cast<const void* const*>(unit.data), unit.count);
  std::vector<const void*> collected;
  collected.reserve(unit.count * static_cast<std::size_t>(count));
  for (std::int64_t i = 0; i < count; ++i) {
    collected.insert(collected.end(), handles.begin(), handles.end());
  }
  return collected;
}

// An array's elements in its own order, each where it lies (LRM 7.6), for a
// container built from or extended by them, which copies each. The array is of
// `type`, one of the kinds whose parts are ordered by position.
auto ElementHandles(const void* array, const value::ValueType& type)
    -> std::vector<const void*> {
  const std::size_t count = type.PartCount(array);
  std::vector<const void*> handles;
  handles.reserve(count);
  for (std::size_t position = 0; position < count; ++position) {
    handles.push_back(type.PartAt(array, position));
  }
  return handles;
}

auto ElementHandles(const void* array, const void* type)
    -> std::vector<const void*> {
  return ElementHandles(array, TypeAt(type));
}

template <typename Container>
auto ElementHandles(const Container& array) -> std::vector<const void*> {
  return ElementHandles(&array, value::LibraryTypeOf<Container>());
}

// A sequence of machine integers, which crosses as the integers themselves.
auto MachineIntsOf(LyraSpan values) -> std::span<const std::int64_t> {
  return {static_cast<const std::int64_t*>(values.data), values.count};
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

// Where a domain carries no bit stream, which of the two ways that is answered
// -- a program that should never have reached here, or an operation not yet
// carried out -- is decided once, by the domain's type, so these ask it rather
// than decide again.
template <typename T>
auto StreamWidthOf(const void* value, void* out) -> void* {
  value::LibraryTypeOf<T>().BitstreamWidth(value, out);
  return out;
}

template <typename T>
auto StreamCountBitsOf(
    const void* value, const value::LoadedWords& control, void* out) -> void* {
  value::LibraryTypeOf<T>().CountBits(value, control.View(), out);
  return out;
}

// The stream of bits a value makes (LRM 6.24.3), laid out in `out` as the
// unsigned integral value of `width` bits the caller states it is.
template <typename Write>
auto StreamInto(void* out, std::int64_t width, bool is_four_state, Write write)
    -> void* {
  value::WrittenWords stream(BitsShape(width, is_four_state));
  write(stream.MutableView());
  stream.StoreTo(out);
  return out;
}

template <typename T>
auto StreamOf(
    const void* value, std::int64_t width, bool is_four_state, void* out)
    -> void* {
  return StreamInto(
      out, width, is_four_state, [&](const value::IntegralView& stream) {
        value::LibraryTypeOf<T>().WriteToStream(value, stream, 0);
      });
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
    return value::FormatIntegralOperand(spec, words.View(), ctx);
  };
  return arg;
}

// The order an LRM 7.12.2 sort is handed, as the value layer takes it. The
// closure is borrowed rather than taken: the sort asks it only before
// returning, while the frame that made it is still alive.
auto BorrowedOrder(void* order)
    -> base::FunctionRef<lyra::support::OrderSignature> {
  return Borrow<lyra::support::OrderSignature>(
      *static_cast<ClosureValue*>(order));
}

// A value of one of the library's own kinds, held with its type.
template <typename T>
auto Held(T value) -> value::AnyValue {
  return value::AnyValue::Built(value::LibraryTypeOf<T>(), [&](void* out) {
    std::construct_at(static_cast<T*>(out), std::move(value));
  });
}

// A value laid out by its type that the runtime holds, which already holds it
// with the type it was handed over with, so it is given up as it stands.
auto Held(value::LaidOutValue laid_out) -> value::AnyValue {
  return std::move(laid_out).IntoAnyValue();
}

// The components of a completion holding the one value `value`.
template <typename T>
auto Single(T value) -> std::vector<value::AnyValue> {
  std::vector<value::AnyValue> components;
  components.push_back(Held(std::move(value)));
  return components;
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
    component.Type().Move(component.Bytes(), type.ComponentAt(out, i));
  }
  return out;
}

// The matched-conversion count, how far the parse advanced, and one value per
// conversion (LRM 21.3.4.3). Each value starts as the prototype the call
// supplied and is parsed in place, so a conversion that never ran carries its
// prototype back and the caller's own destination stays as it was. A scan
// destination is an integral or a string and lowering rejects anything else,
// so a value of any other type reaching here is a compiler bug. An integral
// one is parsed in its planes, which are laid back out in the value once the
// scan is over.
auto EmplaceScan(
    void* out, const value::TupleType& type, const value::String& input,
    const value::String& format, value::detail::NullByte null_byte,
    const void* prototypes, const value::TupleType& prototypes_type) -> void* {
  const std::span<const value::TupleComponent> stated =
      prototypes_type.Components();
  std::vector<value::AnyValue> components;
  components.reserve(stated.size() + 2);
  components.emplace_back();
  components.emplace_back();
  for (std::size_t i = 0; i < stated.size(); ++i) {
    components.push_back(
        value::AnyValue::CopyOf(
            *stated[i].type, prototypes_type.ComponentAt(prototypes, i)));
  }
  std::deque<value::WrittenWords> parsed;
  std::vector<value::ScanTarget> targets;
  targets.reserve(stated.size());
  for (std::size_t i = 2; i < components.size(); ++i) {
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

  const value::ScanCount count =
      value::detail::ScanImpl(input, format, null_byte, targets);
  auto words = parsed.cbegin();
  for (std::size_t i = 2; i < components.size(); ++i) {
    if (components[i].Type().AsIntegral() != nullptr) {
      words->StoreTo(components[i].Bytes());
      ++words;
    }
  }
  components[0] = Held(value::Integer::FromInt(count.items));
  components[1] = Held(value::Int::FromInt(count.consumed));
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
  leading.Type().Move(leading.Bytes(), type.ComponentAt(out, 0));
  second.StoreTo(type.ComponentAt(out, 1));
  return out;
}

// The SV int a traversal answers with and the index it visited (LRM 7.9.4 --
// 7.9.7), the visited index being the probe itself where the array holds no
// such neighbour.
auto EmplaceVisited(
    void* out, const value::TupleType& type, const value::AnyValue* index,
    const void* probe, const void* probe_type) -> void* {
  const bool found = index != nullptr;
  std::vector<value::AnyValue> components;
  components.push_back(Held(value::Int::FromBool(found)));
  components.push_back(found ? *index : OwnedCopy(probe, probe_type));
  return EmplaceCompletion(out, type, std::move(components));
}

// The least or greatest index an array holds (LRM 20.7), built in `out`, or
// the default of `answer_type` where it holds none.
auto IndexInto(void* out, const value::AnyValue* index, const void* answer_type)
    -> void* {
  if (index == nullptr) {
    TypeAt(answer_type).BuildDefault(out);
    return out;
  }
  return ElementInto(out, index->Type(), index->Bytes());
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

// The memory tasks (LRM 21.4, 21.5) over the memories this backend holds,
// whose words are integral values where they lie: a token is parsed at the
// type of the word it replaces, and a word is written out through its planes.
// Each memory is read into a copy, which the load answers with: a fixed-size
// one arrives as that copy, the entry having made it of the caller's bytes.
auto StoreMemoryWord(
    value::MemoryWord word, std::string_view token, unsigned base) -> bool {
  value::ScopedWordsWrite parsed(word.bytes, word.type->Shape());
  return value::FromDigits(parsed.MutableView(), MemoryRadix(base), token);
}

auto RenderedWord(value::ConstMemoryWord word, unsigned base) -> std::string {
  const value::LoadedWords words = word.type->Load(word.bytes);
  return RenderedMemoryWord(words.View(), base);
}

auto ReadUnpackedMemory(
    RuntimeEffects& runtime, value::LaidOutValue loaded,
    const value::String& filename, std::span<const value::UnpackedRange> dims,
    std::int64_t base, std::int64_t start, std::optional<std::int64_t> finish)
    -> value::LaidOutValue {
  const auto radix = static_cast<unsigned>(base);
  ReadMemGridCore(
      runtime, filename, radix, dims[0].Low(), dims[0].High(),
      detail::InnerLeafCount(dims.subspan(1)), start, finish,
      [&](std::int64_t top, std::size_t ordinal, std::string_view token) {
        return StoreMemoryWord(
            value::MemoryLeaf(loaded, dims, top, ordinal), token, radix);
      });
  return loaded;
}

void WriteUnpackedMemory(
    RuntimeEffects& runtime, const value::FixedArrayView& src,
    const value::String& filename, std::span<const value::UnpackedRange> dims,
    std::int64_t base, std::int64_t start, std::optional<std::int64_t> finish) {
  const auto radix = static_cast<unsigned>(base);
  WriteMemGridCore(
      runtime, filename, radix, dims[0].Low(), dims[0].High(),
      detail::InnerLeafCount(dims.subspan(1)), start, finish,
      [&](std::int64_t top, std::size_t ordinal) {
        return RenderedWord(value::MemoryLeaf(src, dims, top, ordinal), radix);
      });
}

// A dynamic array or a queue, addressed `[0, size-1]` with one word per
// address (LRM 21.4.1).
template <typename Container>
auto ReadFlatMemory(
    RuntimeEffects& runtime, const Container& dest,
    const value::String& filename, std::int64_t base, std::int64_t start,
    std::optional<std::int64_t> finish) -> Container {
  Container loaded = dest;
  const auto radix = static_cast<unsigned>(base);
  ReadMemGridCore(
      runtime, filename, radix, 0,
      static_cast<std::int64_t>(loaded.Count()) - 1, 1, start, finish,
      [&](std::int64_t address, std::size_t, std::string_view token) {
        return StoreMemoryWord(
            value::MemoryWordOf(
                loaded.ElementType().Type(),
                loaded.ElementAt(static_cast<std::size_t>(address))),
            token, radix);
      });
  return loaded;
}

template <typename Container>
void WriteFlatMemory(
    RuntimeEffects& runtime, const Container& src,
    const value::String& filename, std::int64_t base, std::int64_t start,
    std::optional<std::int64_t> finish) {
  const auto radix = static_cast<unsigned>(base);
  WriteMemGridCore(
      runtime, filename, radix, 0, static_cast<std::int64_t>(src.Count()) - 1,
      1, start, finish, [&](std::int64_t address, std::size_t) {
        return RenderedWord(
            value::MemoryWordOf(
                src.ElementType().Type(),
                src.ElementAt(static_cast<std::size_t>(address))),
            radix);
      });
}

// An associative memory, addressed by key (LRM 21.4.1). The keys a load builds
// are of the index type the array was declared with, which arrives beside the
// call, so each compares equal to the key an ordinary access builds.
auto ReadKeyedMemory(
    RuntimeEffects& runtime, const value::RuntimeAssociativeArray& dest,
    const value::String& filename, std::int64_t base, std::int64_t start,
    std::optional<std::int64_t> finish, const void* key_type)
    -> value::RuntimeAssociativeArray {
  value::RuntimeAssociativeArray loaded = dest;
  const auto radix = static_cast<unsigned>(base);
  const value::IntegralValueType& index_type = IntegralTypeAt(key_type);
  ReadMemKeyedCore(
      runtime, filename, radix, start, finish,
      [&](std::int64_t key, std::string_view token) {
        value::WrittenWords words(index_type.Shape());
        value::FromInt(words.MutableView(), key);
        const value::AnyValue index = value::AnyValue::Built(
            index_type.Type(), [&](void* out) { words.StoreTo(out); });
        value::Formation formed{};
        void* word = loaded.ElementMut(
            value::IndexView{
                .bytes = index.Bytes(), .type = &index_type.Type()},
            formed);
        return StoreMemoryWord(
            value::MemoryWordOf(loaded.ElementType().Type(), word), token,
            radix);
      });
  return loaded;
}

void WriteKeyedMemory(
    RuntimeEffects& runtime, const value::RuntimeAssociativeArray& src,
    const value::String& filename, std::int64_t base, std::int64_t start,
    std::optional<std::int64_t> finish) {
  const auto radix = static_cast<unsigned>(base);
  std::vector<RenderedMemoryEntry> entries;
  for (const auto& [index, word] : src.Entries()) {
    const value::ConstMemoryWord key =
        value::MemoryWordOf(index->Type(), index->Bytes());
    entries.push_back(
        RenderedMemoryEntry{
            .key = IntOf(key.type->Load(key.bytes)),
            .key_text = RenderedWord(key, 16U),
            .word_text = RenderedWord(
                value::MemoryWordOf(src.ElementType().Type(), word), radix)});
  }
  WriteMemKeyedCore(runtime, filename, radix, start, finish, entries);
}

// A DPI-C open array's image (Annex H.12) of an actual this backend hands over
// erased, and the write-back into one. Each level is reached through its type's
// ordered parts in the image's C layout order, one level per unpacked
// dimension, and the leaves there are integral values where they lie, read and
// written through their planes.
auto ImageLeafType(const value::ValueType& type)
    -> const value::IntegralValueType& {
  const value::IntegralValueType* leaf = type.AsIntegral();
  if (leaf == nullptr) {
    throw InternalError(
        "an open array's element is an integral value (LRM 35.5.6.1) -- "
        "please report this as a bug");
  }
  return *leaf;
}

// The type of one element of the actual `level`, a value of `type`, under its
// `dimensions` unpacked layers. A fixed-size layer holds at least one element
// (LRM 7.4.2), which is what the layer below is read through.
auto ImageElementType(
    const void* level, const value::ValueType& type, std::size_t dimensions)
    -> const value::IntegralValueType& {
  if (dimensions == 0) {
    return ImageLeafType(type);
  }
  if (type.PartCount(level) == 0) {
    throw InternalError(
        "an open array's actual holds an element under each of its unpacked "
        "dimensions (LRM 7.4.2) -- please report this as a bug");
  }
  return ImageElementType(
      type.PartAt(level, 0), type.PartType(level), dimensions - 1);
}

void FillImage(
    value::DpiOpenArray& image, const void* level, const value::ValueType& type,
    std::size_t dimension, std::size_t dimensions, std::size_t& position) {
  if (dimension == dimensions) {
    const value::LoadedWords words = ImageLeafType(type).Load(level);
    image.WriteElement(position, words.View());
    ++position;
    return;
  }
  const value::ValueType& part = type.PartType(level);
  for (std::size_t p = 0; p < type.PartCount(level); ++p) {
    FillImage(
        image, type.PartAt(level, image.OrdinalAt(dimension, p)), part,
        dimension + 1, dimensions, position);
  }
}

// A value of `type` laid out in `out` that an image is read back into: a
// fixed-size unpacked array under each of `dimensions` layers, every leaf
// built at its type's default.
void BuildImageShape(
    const value::ValueType& type, void* out, std::size_t dimensions) {
  if (dimensions == 0) {
    type.BuildDefault(out);
    return;
  }
  const value::FixedArrayType* array = type.AsFixedArray();
  if (array == nullptr) {
    throw InternalError(
        "an open array's write-back is a fixed-size unpacked array under each "
        "of its unpacked dimensions -- please report this as a bug");
  }
  array->BuildEach(out, [&](std::size_t, void* element) {
    BuildImageShape(array->ElementType().Type(), element, dimensions - 1);
  });
}

void ReadImageBack(
    const value::DpiOpenArray& image, void* level, const value::ValueType& type,
    std::size_t dimension, std::size_t dimensions, std::size_t& position) {
  if (dimension == dimensions) {
    {
      value::ScopedWordsWrite words(level, ImageLeafType(type).Shape());
      image.ReadElement(position, words.MutableView());
    }
    ++position;
    return;
  }
  const value::ValueType& part = type.PartType(level);
  for (std::size_t p = 0; p < type.PartCount(level); ++p) {
    ReadImageBack(
        image, type.PartAt(level, image.OrdinalAt(dimension, p)), part,
        dimension + 1, dimensions, position);
  }
}

// The steps of a part's path cross as a run of machine integers.
auto StepsAt(LyraSpan steps) -> std::span<const std::int64_t> {
  return {static_cast<const std::int64_t*>(steps.data), steps.count};
}

// An event control's leaves cross as a span of pointers to values this call
// does not own, and the wait reads them there.
auto TriggerHandles(LyraSpan triggers) -> std::span<const Trigger* const> {
  return {static_cast<const Trigger* const*>(triggers.data), triggers.count};
}

// A collecting wait's reports cross the same way, one per event expression,
// and so do the observations deciding it.
auto ReportHandles(LyraSpan reports) -> std::span<ReadReport* const> {
  return {static_cast<ReadReport* const*>(reports.data), reports.count};
}

auto ObservationHandles(LyraSpan observations)
    -> std::span<const Observation* const> {
  return {
      static_cast<const Observation* const*>(observations.data),
      observations.count};
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

// A cell the running activation owns, for a value a body holds across a
// suspension, told the width or the type of a value it keeps after itself.
template <class T, class... Told>
auto AllocateValueCell(const Told&... told) noexcept -> void* {
  using Local = typename LocalOf<T>::Type;
  ActivationValueStore& values =
      GeneratedCallScope::Current().ActivationValues();
  if constexpr (DynamicallySized<T>) {
    return values.NewWithTail<Local>(
        StorageFor<T>::Type::ValueLayoutFor(told...), told...);
  } else {
    return values.New<Local>();
  }
}

// A value `width` bits wide of the bytes at `value`, which is what installs a
// holder of one.
template <value::HeldAsWords T>
auto WideOf(std::int64_t width, const void* value) -> T {
  return T(static_cast<std::uint64_t>(width), value);
}

// Some positions of one net and of another stated to be one physical net. The
// other net is of whatever integral type its own unit gave it, which this side
// never learns; every such net is reached as a connection reaches one.
template <BitAddressed T>
void JoinNets(
    void* net, void* other, std::int64_t here, std::int64_t there,
    std::int64_t width) {
  NetOf<T>(net).Join(static_cast<ConnectableNet*>(other), here, there, width);
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
    std::int64_t width, LyraSpan outer_parts, const void* start,
    std::int64_t count) -> std::optional<ResolvedBitRange> {
  const auto whole = static_cast<std::uint64_t>(width);
  std::int64_t at = 0;
  std::optional<value::BitPositions> window =
      value::BitPositions{.lsb = 0, .width = whole};
  for (const LyraBitsPart& part : std::span<const LyraBitsPart>(
           static_cast<const LyraBitsPart*>(outer_parts.data),
           outer_parts.count)) {
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
    const void* designation, std::int64_t width, LyraSpan outer_parts,
    const void* start, const void* bits, std::int64_t count,
    bool bits_are_four_state) {
  const std::optional<ResolvedBitRange> range =
      ResolveBitRange(width, outer_parts, start, count);
  if (!range.has_value()) {
    return;
  }
  const ErasedDesignation& within = DesignationAt(designation);
  T& whole = *static_cast<T*>(within.part);
  const T before = whole;
  InsertBitsInto(
      whole.MutableView().Truncated(static_cast<std::uint64_t>(width)), bits,
      count, bits_are_four_state, *range);
  if (whole.IsBitIdentical(before) || !within.write->Undecided()) {
    return;
  }
  KeptPart<T> kept(before, range->clamped);
  if (const std::optional<Change> change = kept.ChangeTo(whole)) {
    within.write->Landed(*change);
  }
}

// The same over a designated value wider than a word, which is `width` bits of
// the bytes the designation names. What the write keeps of it from before is
// the bits it reaches, and only where anything reads what it did.
template <value::HeldAsWords T>
void InsertDesignatedWideBits(
    const void* designation, std::int64_t width, LyraSpan outer_parts,
    const void* start, const void* bits, std::int64_t count,
    bool bits_are_four_state) {
  const std::optional<ResolvedBitRange> range =
      ResolveBitRange(width, outer_parts, start, count);
  if (!range.has_value()) {
    return;
  }
  const ErasedDesignation& within = DesignationAt(designation);
  const value::WideAt<T::kDomain> whole{
      .bytes = within.part, .width = static_cast<std::uint64_t>(width)};
  if (!within.write->Undecided()) {
    InsertBitsInto(
        whole.MutableView(), bits, count, bits_are_four_state, *range);
    return;
  }
  KeptPart<value::WideAt<T::kDomain>> kept(whole, range->clamped);
  InsertBitsInto(whole.MutableView(), bits, count, bits_are_four_state, *range);
  if (const std::optional<Change> change = kept.ChangeTo(whole)) {
    within.write->Landed(*change);
  }
}

// Bits written into an integral value `width` bits wide lying at `bytes`,
// which nothing is told of (LRM 11.5.1).
void InsertBitsAt(
    void* bytes, std::int64_t width, bool is_four_state, LyraSpan outer_parts,
    const void* start, const void* bits, std::int64_t count,
    bool bits_are_four_state) {
  const std::optional<ResolvedBitRange> range =
      ResolveBitRange(width, outer_parts, start, count);
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

}  // namespace

}  // namespace lyra::runtime

using lyra::runtime::Activation;
using lyra::runtime::AdoptClosure;
using lyra::runtime::AdoptObject;
using lyra::runtime::AllocateValueCell;
using lyra::runtime::ArmWholeVariable;
using lyra::runtime::ArrayAt;
using lyra::runtime::ArrayTypeAt;
using lyra::runtime::AssignDesignatedArraySlice;
using lyra::runtime::AssignDesignatedSlice;
using lyra::runtime::AttemptsOf;
using lyra::runtime::BindPropertyEvaluation;
using lyra::runtime::Bit16;
using lyra::runtime::Bit32;
using lyra::runtime::Bit64;
using lyra::runtime::Bit8;
using lyra::runtime::BitsAt;
using lyra::runtime::BitWide;
using lyra::runtime::BuildAt;
using lyra::runtime::BuildReference;
using lyra::runtime::CancellationTarget;
using lyra::runtime::CellBeginTakeover;
using lyra::runtime::CellDriveTakeover;
using lyra::runtime::CellEndTakeover;
using lyra::runtime::ChannelCancellation;
using lyra::runtime::ClosureValue;
using lyra::runtime::Compared;
using lyra::runtime::ConstructionTable;
using lyra::runtime::ControlEffect;
using lyra::runtime::Coroutine;
using lyra::runtime::current_runtime;
using lyra::runtime::CurrentExportScope;
using lyra::runtime::CurrentForeignProcess;
using lyra::runtime::Delay;
using lyra::runtime::DelayReal;
using lyra::runtime::DelayTicks;
using lyra::runtime::DelayTicksReal;
using lyra::runtime::DesignateArrayElement;
using lyra::runtime::DesignateElement;
using lyra::runtime::DesignationAt;
using lyra::runtime::DiagnosticDispatcher;
using lyra::runtime::DisablePropertyEvaluation;
using lyra::runtime::DriveFromNet;
using lyra::runtime::DriveFromReference;
using lyra::runtime::DriveFromVariable;
using lyra::runtime::DriveOnForeignStack;
using lyra::runtime::DriverOf;
using lyra::runtime::Emplace;
using lyra::runtime::EndClosure;
using lyra::runtime::EnterCancellationTarget;
using lyra::runtime::EnterForeignTask;
using lyra::runtime::ErasedAt;
using lyra::runtime::ErasedDesignation;
using lyra::runtime::ErasedObjectWrite;
using lyra::runtime::ErasedReference;
using lyra::runtime::EvaluationAttempts;
using lyra::runtime::EventSourceOf;
using lyra::runtime::FileTable;
using lyra::runtime::FindExportEntry;
using lyra::runtime::ForkWaitAll;
using lyra::runtime::ForkWaitFirst;
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
using lyra::runtime::JoinNets;
using lyra::runtime::LaidOutRefSet;
using lyra::runtime::LandDesignation;
using lyra::runtime::LandLaidOutDesignation;
using lyra::runtime::LeaveCancellationTarget;
using lyra::runtime::LocalAt;
using lyra::runtime::LocalOf;
using lyra::runtime::Logic16;
using lyra::runtime::Logic32;
using lyra::runtime::Logic64;
using lyra::runtime::Logic8;
using lyra::runtime::LogicWide;
using lyra::runtime::LoopBlocks;
using lyra::runtime::MakeForeignExecution;
using lyra::runtime::MakeSharedCell;
using lyra::runtime::NamedEvent;
using lyra::runtime::NetOf;
using lyra::runtime::NetReportsTo;
using lyra::runtime::NumberAt;
using lyra::runtime::ObjectDefinition;
using lyra::runtime::Observable;
using lyra::runtime::Observation;
using lyra::runtime::ObservationHandles;
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
using lyra::runtime::ProcessOf;
using lyra::runtime::ProcessResume;
using lyra::runtime::ProcessSelf;
using lyra::runtime::ProcessStatus;
using lyra::runtime::ProcessSuspend;
using lyra::runtime::ProgramLifetime;
using lyra::runtime::QueueTypeAt;
using lyra::runtime::RaiseDeclinedDeparture;
using lyra::runtime::Read;
using lyra::runtime::ReadReport;
using lyra::runtime::RealTimeInUnit;
using lyra::runtime::ReceiveDeparture;
using lyra::runtime::RefArmSampling;
using lyra::runtime::RefBeginTakeover;
using lyra::runtime::RefDriveTakeover;
using lyra::runtime::RefEndTakeover;
using lyra::runtime::ReferArrayElement;
using lyra::runtime::ReferElement;
using lyra::runtime::ReferToCell;
using lyra::runtime::ReferToProperty;
using lyra::runtime::ReferToStorage;
using lyra::runtime::RefGet;
using lyra::runtime::RefSampledGet;
using lyra::runtime::RefSet;
using lyra::runtime::RefuseReport;
using lyra::runtime::ReportHandles;
using lyra::runtime::ResolvedNet;
using lyra::runtime::ResumeInNbaRegion;
using lyra::runtime::RunDesignRoot;
using lyra::runtime::RunHostCommand;
using lyra::runtime::RunNullHostCommand;
using lyra::runtime::RuntimeEffects;
using lyra::runtime::RuntimeProcess;
using lyra::runtime::SampledHistory;
using lyra::runtime::ScalarPropagationDelay;
using lyra::runtime::Scope;
using lyra::runtime::SharedPointer;
using lyra::runtime::ShowSource;
using lyra::runtime::SimTimeInUnit;
using lyra::runtime::SpawnAll;
using lyra::runtime::STimeInUnit;
using lyra::runtime::TailRefSampledGet;
using lyra::runtime::TakeBranches;
using lyra::runtime::TestPlusargs;
using lyra::runtime::Timer;
using lyra::runtime::Trigger;
using lyra::runtime::TriggerHandles;
using lyra::runtime::ValueCell;
using lyra::runtime::Var;
using lyra::runtime::VariableAt;
using lyra::runtime::VariableOf;
using lyra::runtime::VectorPropagationDelay;
using lyra::runtime::ViewOf;
using lyra::runtime::Wait;
using lyra::runtime::WaitFork;
using lyra::runtime::WaitOn;
using lyra::runtime::WaitOnImplicitList;
using lyra::runtime::WaitOnPropertyEvaluation;
using lyra::runtime::WaitRecollecting;
using lyra::runtime::WaitUntil;
using lyra::runtime::WatchedPlace;
using lyra::value::AssociativeIndexOrder;
using lyra::value::Chandle;
using lyra::value::DpiBitBuffer;
using lyra::value::DpiLogicBuffer;
using lyra::value::DpiOpenArray;
using lyra::value::Empty;
using lyra::value::Enumeration;
using lyra::value::Format;
using lyra::value::FormatArg;
using lyra::value::FormatSpec;
using lyra::value::LaidOutValue;
using lyra::value::MakeFormatArg;
using lyra::value::ObjectRef;
using lyra::value::PrintItem;
using lyra::value::PrintLiteralItem;
using lyra::value::PrintValueItem;
using lyra::value::Real;
using lyra::value::RuntimeAssociativeArray;
using lyra::value::RuntimeDynamicArray;
using lyra::value::RuntimeQueue;
using lyra::value::ShortReal;
using lyra::value::String;
using lyra::value::TimeFormat;
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

auto lyra_rt_file_read_memory(
    void* files, const void* dest, const void* dest_type, std::int64_t fd,
    LyraSpan bounds, std::int64_t start, std::int64_t count,
    const void* answer_type, void* out) -> void* {
  LaidOutValue memory = OwnedLaidOutCopy(dest, dest_type);
  const std::vector<UnpackedRange> dims =
      lyra::value::UnpackedRangesOf(lyra::runtime::MachineIntsOf(bounds));
  const UnpackedRange range = dims.front();
  const lyra::value::IntegralValueType& word_type =
      *lyra::value::MemoryLeaf(memory, dims, range.Low(), 0).type;
  const std::int32_t read = lyra::runtime::ReadMemoryWords(
      *static_cast<FileTable*>(files), fd, word_type.Shape().width, range,
      start, count, [&](std::int64_t sv, std::span<const char> bytes) {
        lyra::value::WrittenWords word(word_type.Shape());
        lyra::runtime::ReadBigEndian(word.MutableView(), bytes);
        word.StoreTo(lyra::value::MemoryLeaf(memory, dims, sv, 0).bytes);
      });
  std::vector<lyra::value::AnyValue> components;
  components.push_back(lyra::runtime::Held(lyra::value::Int::FromInt(read)));
  components.push_back(lyra::runtime::Held(std::move(memory)));
  return lyra::runtime::EmplaceCompletion(
      out, lyra::runtime::TupleTypeAt(answer_type), std::move(components));
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

auto lyra_rt_peek_buffered(void* files, std::int64_t fd, void* out) -> void* {
  return Emplace(out, static_cast<FileTable*>(files)->PeekBuffered(fd));
}

void lyra_rt_advance_fd(void* files, std::int64_t fd, std::int64_t count) {
  static_cast<FileTable*>(files)->AdvanceFd(fd, count);
}

auto lyra_rt_cancellation_for(void* files, std::int64_t descriptor, void* out)
    -> void* {
  return Emplace(
      out, static_cast<FileTable*>(files)->CancellationFor(descriptor));
}

auto lyra_rt_is_cancelled(const void* cancellation, void* out) -> void* {
  return Emplace(out, Read<ChannelCancellation>(cancellation).IsCancelled());
}

auto lyra_rt_string_make(void* cstr, void* out) -> void* {
  return Emplace(out, String(static_cast<const char*>(cstr)));
}

auto lyra_rt_make_print_literal_item(void* string_value, void* out) -> void* {
  return Emplace(
      out, PrintItem(PrintLiteralItem(*static_cast<String*>(string_value))));
}

auto lyra_rt_format(LyraSpan items, const void* time_format, void* out)
    -> void* {
  std::span<PrintItem*> handles(
      static_cast<PrintItem**>(items.data), items.count);
  std::vector<PrintItem> collected;
  collected.reserve(items.count);
  for (PrintItem* handle : handles) {
    collected.push_back(*handle);
  }
  return Emplace(
      out, Format(collected, *static_cast<const TimeFormat*>(time_format)));
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

void lyra_rt_record_coverage(void* runtime, const void* site, bool succeeded) {
  static_cast<RuntimeEffects*>(runtime)->RecordCoverage(
      Read<String>(site), succeeded);
}

auto lyra_rt_enter_coroutine(void* frame, void* out) noexcept -> void* {
  return lyra::runtime::StartGeneratedProcess(
      out, lyra::runtime::GeneratedCoroutine(frame));
}

auto lyra_rt_enter_reaction_running_first(
    void* receiver, bool (*setup)(void*, void*), bool (*pass)(void*, void*),
    void* out) noexcept -> void* {
  return std::construct_at(
      static_cast<Coroutine<void>*>(out),
      lyra::runtime::EnterReactionRunningFirst(receiver, setup, pass));
}

auto lyra_rt_enter_reaction_stopping_first(
    void* receiver, bool (*setup)(void*, void*), bool (*pass)(void*, void*),
    void* out) noexcept -> void* {
  return std::construct_at(
      static_cast<Coroutine<void>*>(out),
      lyra::runtime::EnterReactionStoppingFirst(receiver, setup, pass));
}

auto lyra_rt_await_coroutine(void* runtime, void* activation) -> bool {
  auto& svc = *static_cast<RuntimeEffects*>(runtime);
  lyra::runtime::RuntimeProcess& process = svc.CurrentProcess();
  Activation* const caller = process.CurrentLeaf();
  Activation* const called = process.PushActivation(
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
  lyra::runtime::RuntimeProcess& process =
      static_cast<RuntimeEffects*>(runtime)->CurrentProcess();
  // A run-time error that left the called body was stored rather than allowed
  // to travel, because a coroutine's promise stores whatever escapes the body
  // it drives. The call is one statement of this thread, so it continues from
  // here -- taken before the activation is released, which destroys the promise
  // holding it.
  std::exception_ptr raised = process.TakeInnermostRaisedError();
  process.PopActivation();
  if (raised) {
    std::rethrow_exception(raised);
  }
}

void lyra_rt_spawn_all(void* runtime, LyraSpan branches) {
  auto& svc = *static_cast<RuntimeEffects*>(runtime);
  std::vector<Coroutine<void>> taken = TakeBranches(branches);
  SpawnAll(svc, std::span<Coroutine<void>>{taken});
}

auto lyra_rt_fork_wait_all(void* runtime, LyraSpan branches, void* out)
    -> void* {
  auto& svc = *static_cast<RuntimeEffects*>(runtime);
  std::vector<Coroutine<void>> taken = TakeBranches(branches);
  return Emplace(out, ForkWaitAll(svc, std::span<Coroutine<void>>{taken}));
}

auto lyra_rt_fork_wait_first(void* runtime, LyraSpan branches, void* out)
    -> void* {
  auto& svc = *static_cast<RuntimeEffects*>(runtime);
  std::vector<Coroutine<void>> taken = TakeBranches(branches);
  return Emplace(out, ForkWaitFirst(svc, std::span<Coroutine<void>>{taken}));
}

auto lyra_rt_wait_fork(void* runtime, void* out) -> void* {
  return Emplace(out, WaitFork(*static_cast<RuntimeEffects*>(runtime)));
}

void lyra_rt_disable_fork(void* runtime) {
  lyra::runtime::DisableFork(*static_cast<RuntimeEffects*>(runtime));
}

auto lyra_rt_process_self(void* runtime, void* out) -> void* {
  return Emplace(out, ProcessSelf(*static_cast<RuntimeEffects*>(runtime)));
}

auto lyra_rt_process_status(const void* self, void* out) -> void* {
  return Emplace(out, ProcessStatus(ProcessOf(self)));
}

void lyra_rt_process_kill(const void* self, void* runtime) {
  ProcessKill(ProcessOf(self), *static_cast<RuntimeEffects*>(runtime));
}

auto lyra_rt_process_await(const void* self, void* runtime, void* out)
    -> void* {
  return Emplace(
      out,
      ProcessAwait(ProcessOf(self), *static_cast<RuntimeEffects*>(runtime)));
}

void lyra_rt_process_suspend(const void* self, void* runtime) {
  ProcessSuspend(ProcessOf(self), *static_cast<RuntimeEffects*>(runtime));
}

void lyra_rt_process_resume(const void* self, void* runtime) {
  ProcessResume(ProcessOf(self), *static_cast<RuntimeEffects*>(runtime));
}

void lyra_rt_closure_box_end(void* closure) {
  EndClosure{}(static_cast<ClosureValue*>(closure));
}

auto lyra_rt_object_adopt(void* object, void* out) -> void* {
  return Emplace(out, AdoptObject(object));
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

auto lyra_rt_chandle_shared_cell_make(void* out) -> void* {
  return MakeSharedCell<Chandle>(out);
}

auto lyra_rt_managedref_shared_cell_make(void* out) -> void* {
  return MakeSharedCell<ObjectRef>(out);
}

auto lyra_rt_laidout_shared_cell_make(const void* held_type, void* out)
    -> void* {
  return MakeSharedCell<LaidOutValue>(out, lyra::runtime::TypeAt(held_type));
}

auto lyra_rt_dynarray_shared_cell_make(void* out) -> void* {
  return MakeSharedCell<RuntimeDynamicArray>(out);
}

auto lyra_rt_queue_shared_cell_make(void* out) -> void* {
  return MakeSharedCell<RuntimeQueue>(out);
}

auto lyra_rt_assocarray_shared_cell_make(void* out) -> void* {
  return MakeSharedCell<RuntimeAssociativeArray>(out);
}

auto lyra_rt_shared_pointer_deref(void* handle) -> void* {
  return Read<SharedPointer>(handle).Pointee();
}

void lyra_rt_submit_nba(void* runtime, void* closure) {
  static_cast<RuntimeEffects*>(runtime)->SubmitNba(
      AdoptClosure<lyra::support::EffectSignature>(closure));
}

void lyra_rt_submit_nba_after(
    void* runtime, const void* duration, std::int64_t duration_width,
    bool duration_is_signed, bool duration_is_four_state,
    std::int64_t unit_power, std::int64_t precision_power, void* closure) {
  const lyra::value::LoadedWords amount = NumberAt(
      duration, duration_width, duration_is_signed, duration_is_four_state);
  static_cast<RuntimeEffects*>(runtime)->SubmitNbaAfter(
      amount.View(), unit_power, precision_power,
      AdoptClosure<lyra::support::EffectSignature>(closure));
}

void lyra_rt_submit_nba_after_real(
    void* runtime, const void* duration, std::int64_t unit_power,
    std::int64_t precision_power, void* closure) {
  static_cast<RuntimeEffects*>(runtime)->SubmitNbaAfterReal(
      Read<Real>(duration), unit_power, precision_power,
      AdoptClosure<lyra::support::EffectSignature>(closure));
}

void lyra_rt_run_detached(void* runtime, void* carrier) {
  static_cast<RuntimeEffects*>(runtime)->RunDetached(
      std::move(*static_cast<Coroutine<void>*>(carrier)));
}

void lyra_rt_submit_postponed(void* runtime, void* closure) {
  static_cast<RuntimeEffects*>(runtime)->SubmitPostponed(
      AdoptClosure<lyra::support::EffectSignature>(closure));
}

void lyra_rt_submit_observed(void* runtime, void* closure) {
  static_cast<RuntimeEffects*>(runtime)->SubmitObserved(
      AdoptClosure<lyra::support::EffectSignature>(closure));
}

void lyra_rt_submit_violation_report(void* runtime, void* closure) {
  static_cast<RuntimeEffects*>(runtime)->SubmitViolationReport(
      AdoptClosure<lyra::support::EffectSignature>(closure));
}

void lyra_rt_submit_deferred_observed(void* runtime, void* body) {
  static_cast<RuntimeEffects*>(runtime)->SubmitDeferredObserved(
      std::move(*static_cast<Coroutine<void>*>(body)));
}

void lyra_rt_submit_deferred_final(void* runtime, void* body) {
  static_cast<RuntimeEffects*>(runtime)->SubmitDeferredFinal(
      std::move(*static_cast<Coroutine<void>*>(body)));
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

// What crosses is the place's one word. For a place reached by itself that is
// the cell's own address -- a variable, a net, a named event -- and a `void*`
// carries no type to adjust by, so it is read as the address of what waits on
// that cell: every such cell names `Observable` as its first base, which is
// what makes the two addresses one under the platform ABI. For a place reached
// through a reference bound into a member it is the word that reference
// answered, which says so itself.
auto lyra_rt_make_trigger(
    void* observable, const void* observation, LyraSpan steps,
    std::int64_t lsb_bit_offset, std::int64_t bit_width, void* out) -> void* {
  return Emplace(
      out,
      Trigger(
          WatchedPlace::FromWord(observable), Read<Observation>(observation),
          lyra::runtime::StepsAt(steps), lsb_bit_offset, bit_width));
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

auto lyra_rt_observation_qualified(void* condition, void* out) -> void* {
  return Emplace(
      out, Observation::Qualified(
               AdoptClosure<lyra::support::ConditionSignature>(condition)));
}

auto lyra_rt_observation_fires(const void* observation) -> std::int64_t {
  return static_cast<const Observation*>(observation)->Fires() ? 1 : 0;
}

auto lyra_rt_wait_recollecting(
    LyraSpan reports, LyraSpan observations, void* out) -> void* {
  return Emplace(
      out, WaitRecollecting(
               ReportHandles(reports), ObservationHandles(observations)));
}

auto lyra_rt_wait_until(LyraSpan reports, void* out) -> void* {
  return Emplace(out, WaitUntil(ReportHandles(reports)));
}

auto lyra_rt_wait_on(LyraSpan triggers, void* out) -> void* {
  return Emplace(out, WaitOn(TriggerHandles(triggers)));
}

auto lyra_rt_wait_on_implicit_list(const void* report, void* out) -> void* {
  return Emplace(
      out, WaitOnImplicitList(static_cast<const ReadReport*>(report)));
}

auto lyra_rt_wait_on_property_evaluation(
    void* runtime, LyraSpan clock, LyraSpan disable_reads, std::uint64_t words,
    void* out) -> void* {
  return Emplace(
      out, WaitOnPropertyEvaluation(
               *static_cast<RuntimeEffects*>(runtime), TriggerHandles(clock),
               TriggerHandles(disable_reads), words));
}

auto lyra_rt_attempts_of(void* wait) -> void* {
  return AttemptsOf(static_cast<Wait*>(wait));
}

void lyra_rt_bind_property_evaluation(void* wait, void* step, void* test) {
  BindPropertyEvaluation(
      static_cast<Wait*>(wait),
      AdoptClosure<lyra::support::EffectSignature>(step),
      AdoptClosure<lyra::support::EffectSignature>(test));
}

void lyra_rt_disable_property_evaluation(void* wait) {
  DisablePropertyEvaluation(static_cast<Wait*>(wait));
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
    void* report, void* place, LyraSpan steps, std::int64_t lsb_bit_offset,
    std::int64_t bit_width) noexcept {
  static_cast<ReadReport*>(report)->Add(
      WatchedPlace::FromWord(place), lyra::runtime::StepsAt(steps),
      lsb_bit_offset, bit_width);
}

void lyra_rt_read_report_add_object(void* report, void* object) {
  static_cast<ReadReport*>(report)->AddObject(static_cast<GcObject*>(object));
}

void lyra_rt_read_report_add_write(
    void* report, void* place, LyraSpan steps, std::int64_t lsb_bit_offset,
    std::int64_t bit_width) noexcept {
  static_cast<ReadReport*>(report)->AddWrite(
      WatchedPlace::FromWord(place), lyra::runtime::StepsAt(steps),
      lsb_bit_offset, bit_width);
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

void lyra_rt_refuse_report(const void* why) {
  RefuseReport(static_cast<const char*>(why));
}

auto lyra_rt_resume_in_nba_region(void* out) -> void* {
  return Emplace(out, ResumeInNbaRegion());
}

void lyra_rt_trigger(void* event, void* runtime) {
  static_cast<NamedEvent*>(event)->Trigger(
      *static_cast<RuntimeEffects*>(runtime));
}

auto lyra_rt_triggered(const void* event, void* runtime, void* out) -> void* {
  return Emplace(
      out, static_cast<const NamedEvent*>(event)->Triggered(
               *static_cast<RuntimeEffects*>(runtime)));
}

auto lyra_rt_receive_departure(void* exception) -> void* {
  // The landing is handed what the unwinder carries, not the effect itself;
  // receiving is what turns one into the other, and it is the point after which
  // this departure is this landing's to finish or to decline.
  abi::__cxa_begin_catch(exception);
  return ReceiveDeparture().target;
}

void lyra_rt_finish_departure() {
  abi::__cxa_end_catch();
}

void lyra_rt_decline_departure(void* target) {
  // Carries on the departure the landing holds, which is not always what the
  // unwinder brought: an error it received is carried on as the departure it
  // became, so no later landing receives the error a second time.
  abi::__cxa_end_catch();
  RaiseDeclinedDeparture(
      ControlEffect{.target = static_cast<CancellationTarget*>(target)});
}

void lyra_rt_settle_departure(void* target) {
  // Settles the departure the landing holds rather than what the unwinder
  // brought, for the reason declining carries that one on.
  abi::__cxa_end_catch();
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

auto lyra_rt_effect_names_target(void* effect, void* target, void* out) noexcept
    -> void* {
  // A control effect crosses as the target it names, since that is all one
  // carries, so naming a target is comparing the two.
  return Emplace(out, lyra::value::Bit::FromBool(effect == target));
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
    void* runtime, const void* origin, std::int64_t finish_number) {
  Finish(
      *static_cast<RuntimeEffects*>(runtime), Read<String>(origin),
      finish_number);
}

void lyra_rt_stop(
    void* runtime, const void* origin, std::int64_t finish_number) {
  Stop(
      *static_cast<RuntimeEffects*>(runtime), Read<String>(origin),
      finish_number);
}

void lyra_rt_exit(void* runtime, const void* origin) {
  Exit(*static_cast<RuntimeEffects*>(runtime), Read<String>(origin));
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
    std::int64_t seed, std::int64_t mean, const void* answer_type, void* out)
    -> void* {
  return lyra::runtime::EmplaceBoth(
      out, lyra::runtime::TupleTypeAt(answer_type),
      lyra::runtime::DistExponential(seed, mean));
}

auto lyra_rt_dist_poisson(
    std::int64_t seed, std::int64_t mean, const void* answer_type, void* out)
    -> void* {
  return lyra::runtime::EmplaceBoth(
      out, lyra::runtime::TupleTypeAt(answer_type),
      lyra::runtime::DistPoisson(seed, mean));
}

auto lyra_rt_dist_chi_square(
    std::int64_t seed, std::int64_t degrees_of_freedom, const void* answer_type,
    void* out) -> void* {
  return lyra::runtime::EmplaceBoth(
      out, lyra::runtime::TupleTypeAt(answer_type),
      lyra::runtime::DistChiSquare(seed, degrees_of_freedom));
}

auto lyra_rt_dist_t(
    std::int64_t seed, std::int64_t degrees_of_freedom, const void* answer_type,
    void* out) -> void* {
  return lyra::runtime::EmplaceBoth(
      out, lyra::runtime::TupleTypeAt(answer_type),
      lyra::runtime::DistT(seed, degrees_of_freedom));
}

auto lyra_rt_dist_erlang(
    std::int64_t seed, std::int64_t stages, std::int64_t mean,
    const void* answer_type, void* out) -> void* {
  return lyra::runtime::EmplaceBoth(
      out, lyra::runtime::TupleTypeAt(answer_type),
      lyra::runtime::DistErlang(seed, stages, mean));
}

void lyra_rt_register_initial(
    void* self, void* unit_instance, void* coroutine, const char* written_at) {
  RegisterInitialProcess(
      static_cast<Scope*>(self), static_cast<Scope*>(unit_instance),
      std::move(*static_cast<Coroutine<void>*>(coroutine)), written_at);
}

void lyra_rt_register_always(
    void* self, void* unit_instance, void* coroutine, const char* written_at) {
  RegisterAlwaysProcess(
      static_cast<Scope*>(self), static_cast<Scope*>(unit_instance),
      std::move(*static_cast<Coroutine<void>*>(coroutine)), written_at);
}

void lyra_rt_register_continuous_driver(
    void* self, void* unit_instance, void* runs_as, void* coroutine,
    const char* written_at) {
  RegisterContinuousDriver(
      static_cast<Scope*>(self), static_cast<Scope*>(unit_instance),
      static_cast<Scope*>(runs_as),
      std::move(*static_cast<Coroutine<void>*>(coroutine)), written_at);
}

void lyra_rt_register_triggered(
    void* self, void* unit_instance, void* coroutine, const char* written_at) {
  RegisterTriggeredProcess(
      static_cast<Scope*>(self), static_cast<Scope*>(unit_instance),
      std::move(*static_cast<Coroutine<void>*>(coroutine)), written_at);
}

void lyra_rt_register_final(
    void* self, void* unit_instance, void* coroutine, const char* written_at) {
  RegisterFinalProcess(
      static_cast<Scope*>(self), static_cast<Scope*>(unit_instance),
      std::move(*static_cast<Coroutine<void>*>(coroutine)), written_at);
}

void lyra_rt_run_assertion_action(
    void* self, void* unit_instance, void* coroutine, const char* written_at) {
  RunAssertionAction(
      static_cast<Scope*>(self), static_cast<Scope*>(unit_instance),
      std::move(*static_cast<Coroutine<void>*>(coroutine)), written_at);
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

void lyra_rt_enter_dpi_scope(void* runtime, void* decl_scope) {
  EnterDpiScope(
      *static_cast<RuntimeEffects*>(runtime), static_cast<Scope*>(decl_scope));
}

void lyra_rt_leave_dpi_scope(void* runtime) noexcept {
  LeaveDpiScope(*static_cast<RuntimeEffects*>(runtime));
}

auto lyra_rt_disable_is_active(void* runtime) -> std::int32_t {
  return DisableIsActive(*static_cast<RuntimeEffects*>(runtime));
}

void lyra_rt_check_import_task_acknowledged(
    void* runtime, std::int32_t returned) {
  CheckImportTaskAcknowledged(*static_cast<RuntimeEffects*>(runtime), returned);
}

void lyra_rt_check_import_function_acknowledged(void* runtime) {
  CheckImportFunctionAcknowledged(*static_cast<RuntimeEffects*>(runtime));
}

void lyra_rt_check_export_reachable(void* runtime) {
  CheckExportReachable(*static_cast<RuntimeEffects*>(runtime));
}

auto lyra_rt_claim_namespace_initialize(void* runtime, const char* name)
    -> std::int64_t {
  return ClaimNamespaceInitialization(
      *static_cast<RuntimeEffects*>(runtime), name);
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
  RuntimeProcess& process = CurrentForeignProcess();
  Activation* const called = process.PushActivation(
      std::move(*static_cast<Coroutine<void>*>(activation)));
  DriveOnForeignStack(called);
  // A run-time error that left the body was stored rather than allowed to
  // travel through the frames that drove it, so it travels on from here, where
  // the entry's own region receives it before it can reach the foreign frame
  // above. Taken before the activation is released, which destroys what holds
  // it.
  std::exception_ptr raised = process.TakeInnermostRaisedError();
  process.PopActivation();
  if (raised) {
    std::rethrow_exception(raised);
  }
}

auto lyra_rt_make_segment(void* label, LyraSpan indices, void* out) -> void* {
  return std::construct_at(
      static_cast<HierarchySegment*>(out),
      std::string(static_cast<const char*>(label)),
      lyra::runtime::MachineIntsOf(indices));
}

auto lyra_rt_hierarchical_path(void* self, void* out) -> void* {
  return Emplace(out, String(static_cast<Scope*>(self)->HierarchicalPath()));
}

auto lyra_rt_parent(void* self) -> void* {
  return static_cast<Scope*>(self)->Parent();
}

auto lyra_rt_add_owned_child(void* parent, void* child) -> void* {
  return static_cast<Scope*>(parent)->AddOwnedChild(
      std::unique_ptr<Scope>(static_cast<Scope*>(child)));
}

void lyra_rt_insert_bits(
    void* whole, std::int64_t whole_width, bool whole_is_four_state,
    LyraSpan outer_parts, const void* start, const void* bits,
    std::int64_t bits_width, bool bits_are_four_state) {
  lyra::runtime::InsertBitsAt(
      whole, whole_width, whole_is_four_state, outer_parts, start, bits,
      bits_width, bits_are_four_state);
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

auto lyra_rt_sequence_make(LyraSpan handles) -> void* {
  const std::span<void* const> raw(
      static_cast<void* const*>(handles.data), handles.count);
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

auto lyra_rt_handle_view(const void* handle) -> void* {
  return Read<ObjectRef>(handle).View<void>();
}

auto lyra_rt_handle_with_view(const void* handle, void* view, void* out)
    -> void* {
  return Emplace(
      out, view == nullptr ? ObjectRef{}
                           : ObjectRef(Read<ObjectRef>(handle).Handle(), view));
}

auto lyra_rt_view_of(const void* handle) -> void* {
  return ViewOf(Read<ObjectRef>(handle));
}

auto lyra_rt_self_handle(void* self, void* out) -> void* {
  return Emplace(out, lyra::runtime::SelfHandle(static_cast<GcObject*>(self)));
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
    std::int32_t argc, char** argv, void* (*make)(void*, const void*),
    const void* name) -> std::int32_t {
  return RunDesignRoot(
      argc, argv, std::string_view{static_cast<const char*>(name)},
      [make](Scope* parent, HierarchySegment segment) {
        return std::unique_ptr<Scope>(
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

auto lyra_rt_refer_storage(void* storage, void* out) -> void* {
  return ReferToStorage(storage, out);
}

auto lyra_rt_refer_property(void* object, void* storage, void* out) -> void* {
  return ReferToProperty(object, storage, out);
}

auto lyra_rt_reference_reports_to(const void* reference) -> void* {
  return WatchedPlace::Through(ErasedAt(reference)).Word();
}

void lyra_rt_bind_member(void* member, const void* bound) {
  lyra::runtime::current_runtime().Bound().Bind(
      *static_cast<ErasedReference*>(member), ErasedAt(bound));
}

void lyra_rt_bind_alias(void* alias, const void* bound) {
  lyra::runtime::current_runtime().Bound().Alias(
      *static_cast<ErasedReference*>(alias), ErasedAt(bound));
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
auto lyra_rt_chandle_cell_refer(void* cell, void* out) -> void* {
  return ReferToCell<Chandle>(cell, out);
}
auto lyra_rt_managedref_cell_refer(void* cell, void* out) -> void* {
  return ReferToCell<ObjectRef>(cell, out);
}
auto lyra_rt_laidout_cell_refer(void* cell, void* out) -> void* {
  return ReferToCell<LaidOutValue>(cell, out);
}

auto lyra_rt_dynarray_cell_refer(void* cell, void* out) -> void* {
  return ReferToCell<RuntimeDynamicArray>(cell, out);
}
auto lyra_rt_queue_cell_refer(void* cell, void* out) -> void* {
  return ReferToCell<RuntimeQueue>(cell, out);
}
auto lyra_rt_assocarray_cell_refer(void* cell, void* out) -> void* {
  return ReferToCell<RuntimeAssociativeArray>(cell, out);
}

auto lyra_rt_dynarray_refer_element(
    const void* reference, const void* position, void* out) -> void* {
  return ReferElement<RuntimeDynamicArray>(
      reference, PositionNamedAt(position), out);
}
auto lyra_rt_laidout_refer_element(
    const void* reference, const void* position, const void* array_type,
    void* out) -> void* {
  return ReferArrayElement(
      reference, ArrayTypeAt(array_type), PositionNamedAt(position), out);
}
auto lyra_rt_queue_refer_element(
    const void* reference, const void* position, void* out) -> void* {
  return ReferElement<RuntimeQueue>(reference, PositionNamedAt(position), out);
}
auto lyra_rt_assocarray_refer_element(
    const void* reference, const void* index, const void* index_type, void* out)
    -> void* {
  return ReferElement<RuntimeAssociativeArray>(
      reference, lyra::runtime::IndexAt(index, index_type), out);
}
auto lyra_rt_laidout_refer_component(
    const void* reference, std::int64_t index, const void* tuple_type,
    void* out) -> void* {
  const ErasedReference& from = ErasedAt(reference);
  return BuildReference(
      out,
      from.Part(
          lyra::runtime::TupleTypeAt(tuple_type)
              .ComponentAt(from.storage, static_cast<std::size_t>(index)),
          lyra::value::Formation::kExisting,
          lyra::runtime::PartPath::Child(static_cast<std::uint64_t>(index))));
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

auto lyra_rt_string_ref_sampled_get(void* reference) -> const void* {
  return RefSampledGet<String>(reference);
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

auto lyra_rt_real_ref_sampled_get(void* reference) -> const void* {
  return RefSampledGet<Real>(reference);
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

auto lyra_rt_shortreal_ref_sampled_get(void* reference) -> const void* {
  return RefSampledGet<ShortReal>(reference);
}

auto lyra_rt_chandle_ref_get(void* reference) -> const void* {
  return RefGet<Chandle>(reference);
}

void lyra_rt_chandle_ref_set(void* reference, const void* value) {
  RefSet<Chandle>(reference, value);
}

void lyra_rt_chandle_ref_arm_sampling(void* reference) {
  RefArmSampling<Chandle>(reference);
}

auto lyra_rt_chandle_ref_sampled_get(void* reference) -> const void* {
  return RefSampledGet<Chandle>(reference);
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

auto lyra_rt_managedref_ref_sampled_get(void* reference) -> const void* {
  return RefSampledGet<ObjectRef>(reference);
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

auto lyra_rt_laidout_ref_sampled_get(void* reference) -> const void* {
  return TailRefSampledGet<LaidOutValue>(reference);
}

auto lyra_rt_dynarray_ref_get(void* reference) -> const void* {
  return RefGet<RuntimeDynamicArray>(reference);
}

void lyra_rt_dynarray_ref_set(void* reference, const void* value) {
  RefSet<RuntimeDynamicArray>(reference, value);
}

void lyra_rt_dynarray_ref_arm_sampling(void* reference) {
  RefArmSampling<RuntimeDynamicArray>(reference);
}

auto lyra_rt_dynarray_ref_sampled_get(void* reference) -> const void* {
  return RefSampledGet<RuntimeDynamicArray>(reference);
}

auto lyra_rt_queue_ref_get(void* reference) -> const void* {
  return RefGet<RuntimeQueue>(reference);
}

void lyra_rt_queue_ref_set(void* reference, const void* value) {
  RefSet<RuntimeQueue>(reference, value);
}

void lyra_rt_queue_ref_arm_sampling(void* reference) {
  RefArmSampling<RuntimeQueue>(reference);
}

auto lyra_rt_queue_ref_sampled_get(void* reference) -> const void* {
  return RefSampledGet<RuntimeQueue>(reference);
}

auto lyra_rt_assocarray_ref_get(void* reference) -> const void* {
  return RefGet<RuntimeAssociativeArray>(reference);
}

void lyra_rt_assocarray_ref_set(void* reference, const void* value) {
  RefSet<RuntimeAssociativeArray>(reference, value);
}

void lyra_rt_assocarray_ref_arm_sampling(void* reference) {
  RefArmSampling<RuntimeAssociativeArray>(reference);
}

auto lyra_rt_assocarray_ref_sampled_get(void* reference) -> const void* {
  return RefSampledGet<RuntimeAssociativeArray>(reference);
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

auto lyra_rt_string_cell_sampled_get(void* cell) -> const void* {
  return &VariableAt<String>(cell).SampledGet();
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

auto lyra_rt_real_cell_sampled_get(void* cell) -> const void* {
  return &VariableAt<Real>(cell).SampledGet();
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

auto lyra_rt_shortreal_cell_sampled_get(void* cell) -> const void* {
  return &VariableAt<ShortReal>(cell).SampledGet();
}

auto lyra_rt_string_cell_begin_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count)
    -> std::int64_t {
  return CellBeginTakeover<String>(cell, level, first, count);
}

auto lyra_rt_string_cell_drive_takeover(
    void* cell, std::int64_t level, std::int64_t generation, const void* value)
    -> bool {
  return CellDriveTakeover<String>(cell, level, generation, value);
}

void lyra_rt_string_cell_end_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count) {
  CellEndTakeover<String>(cell, level, first, count);
}

auto lyra_rt_string_ref_begin_takeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count)
    -> std::int64_t {
  return RefBeginTakeover<String>(reference, level, first, count);
}

auto lyra_rt_string_ref_drive_takeover(
    void* reference, std::int64_t level, std::int64_t generation,
    const void* value) -> bool {
  return RefDriveTakeover<String>(reference, level, generation, value);
}

void lyra_rt_string_ref_end_takeover(
    void* reference, std::int64_t level, std::int64_t first,
    std::int64_t count) {
  RefEndTakeover<String>(reference, level, first, count);
}

auto lyra_rt_real_cell_begin_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count)
    -> std::int64_t {
  return CellBeginTakeover<Real>(cell, level, first, count);
}

auto lyra_rt_real_cell_drive_takeover(
    void* cell, std::int64_t level, std::int64_t generation, const void* value)
    -> bool {
  return CellDriveTakeover<Real>(cell, level, generation, value);
}

void lyra_rt_real_cell_end_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count) {
  CellEndTakeover<Real>(cell, level, first, count);
}

auto lyra_rt_real_ref_begin_takeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count)
    -> std::int64_t {
  return RefBeginTakeover<Real>(reference, level, first, count);
}

auto lyra_rt_real_ref_drive_takeover(
    void* reference, std::int64_t level, std::int64_t generation,
    const void* value) -> bool {
  return RefDriveTakeover<Real>(reference, level, generation, value);
}

void lyra_rt_real_ref_end_takeover(
    void* reference, std::int64_t level, std::int64_t first,
    std::int64_t count) {
  RefEndTakeover<Real>(reference, level, first, count);
}

auto lyra_rt_shortreal_cell_begin_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count)
    -> std::int64_t {
  return CellBeginTakeover<ShortReal>(cell, level, first, count);
}

auto lyra_rt_shortreal_cell_drive_takeover(
    void* cell, std::int64_t level, std::int64_t generation, const void* value)
    -> bool {
  return CellDriveTakeover<ShortReal>(cell, level, generation, value);
}

void lyra_rt_shortreal_cell_end_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count) {
  CellEndTakeover<ShortReal>(cell, level, first, count);
}

auto lyra_rt_shortreal_ref_begin_takeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count)
    -> std::int64_t {
  return RefBeginTakeover<ShortReal>(reference, level, first, count);
}

auto lyra_rt_shortreal_ref_drive_takeover(
    void* reference, std::int64_t level, std::int64_t generation,
    const void* value) -> bool {
  return RefDriveTakeover<ShortReal>(reference, level, generation, value);
}

void lyra_rt_shortreal_ref_end_takeover(
    void* reference, std::int64_t level, std::int64_t first,
    std::int64_t count) {
  RefEndTakeover<ShortReal>(reference, level, first, count);
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
    const void* history, std::int64_t ticks_back, void* out) -> void* {
  return Emplace(
      out, static_cast<const SampledHistory<String>*>(history)->At(ticks_back));
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
    const void* history, std::int64_t ticks_back, void* out) -> void* {
  return Emplace(
      out, static_cast<const SampledHistory<Real>*>(history)->At(ticks_back));
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
    const void* history, std::int64_t ticks_back, void* out) -> void* {
  return Emplace(
      out,
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
    const void* history, std::int64_t ticks_back, void* out) -> void* {
  return Emplace(
      out, static_cast<const SampledHistory<LaidOutValue>*>(history)->At(
               ticks_back));
}

void lyra_rt_dynarray_sampled_history_install(
    void* history, const void* default_value, std::int64_t depth) {
  static_cast<SampledHistory<RuntimeDynamicArray>*>(history)->Install(
      Read<RuntimeDynamicArray>(default_value), depth);
}

void lyra_rt_dynarray_sampled_history_push(void* history, const void* value) {
  static_cast<SampledHistory<RuntimeDynamicArray>*>(history)->Push(
      Read<RuntimeDynamicArray>(value));
}

auto lyra_rt_dynarray_sampled_history_at(
    const void* history, std::int64_t ticks_back, void* out) -> void* {
  return Emplace(
      out, static_cast<const SampledHistory<RuntimeDynamicArray>*>(history)->At(
               ticks_back));
}

void lyra_rt_queue_sampled_history_install(
    void* history, const void* default_value, std::int64_t depth) {
  static_cast<SampledHistory<RuntimeQueue>*>(history)->Install(
      Read<RuntimeQueue>(default_value), depth);
}

void lyra_rt_queue_sampled_history_push(void* history, const void* value) {
  static_cast<SampledHistory<RuntimeQueue>*>(history)->Push(
      Read<RuntimeQueue>(value));
}

auto lyra_rt_queue_sampled_history_at(
    const void* history, std::int64_t ticks_back, void* out) -> void* {
  return Emplace(
      out, static_cast<const SampledHistory<RuntimeQueue>*>(history)->At(
               ticks_back));
}

void lyra_rt_assocarray_sampled_history_install(
    void* history, const void* default_value, std::int64_t depth) {
  static_cast<SampledHistory<RuntimeAssociativeArray>*>(history)->Install(
      Read<RuntimeAssociativeArray>(default_value), depth);
}

void lyra_rt_assocarray_sampled_history_push(void* history, const void* value) {
  static_cast<SampledHistory<RuntimeAssociativeArray>*>(history)->Push(
      Read<RuntimeAssociativeArray>(value));
}

auto lyra_rt_assocarray_sampled_history_at(
    const void* history, std::int64_t ticks_back, void* out) -> void* {
  return Emplace(
      out,
      static_cast<const SampledHistory<RuntimeAssociativeArray>*>(history)->At(
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
    const void* history, std::int64_t ticks_back, void* out) -> void* {
  return Emplace(
      out,
      static_cast<const SampledHistory<ObjectRef>*>(history)->At(ticks_back));
}

void lyra_rt_evaluation_attempts_install(
    void* attempts, void* effects, std::uint64_t words, bool pending_holds,
    void* pass_action, void* fail_action) {
  static_cast<EvaluationAttempts*>(attempts)->Install(
      *static_cast<RuntimeEffects*>(effects), words, pending_holds,
      AdoptClosure<lyra::support::EffectSignature>(pass_action),
      AdoptClosure<lyra::support::EffectSignature>(fail_action));
}

void lyra_rt_evaluation_attempts_seed_word(
    void* attempts, std::uint64_t word, std::uint64_t bits) {
  static_cast<EvaluationAttempts*>(attempts)->SeedWord(word, bits);
}

void lyra_rt_evaluation_attempts_open_attempt(void* attempts) {
  static_cast<EvaluationAttempts*>(attempts)->OpenAttempt();
}

void lyra_rt_evaluation_attempts_begin_tick(void* attempts) {
  static_cast<EvaluationAttempts*>(attempts)->BeginTick();
}

void lyra_rt_evaluation_attempts_disable_tick(void* attempts) {
  static_cast<EvaluationAttempts*>(attempts)->DisableTick();
}

auto lyra_rt_evaluation_attempts_live_word(
    const void* attempts, std::uint64_t word) -> std::uint64_t {
  return static_cast<const EvaluationAttempts*>(attempts)->LiveWord(word);
}

auto lyra_rt_evaluation_attempts_next_unstepped(void* attempts)
    -> std::int64_t {
  return static_cast<EvaluationAttempts*>(attempts)->NextUnstepped();
}

auto lyra_rt_evaluation_attempts_bits_at(
    const void* attempts, std::int64_t index, std::uint64_t word)
    -> std::uint64_t {
  return static_cast<const EvaluationAttempts*>(attempts)->BitsAt(index, word);
}

void lyra_rt_evaluation_attempts_set_word(
    void* attempts, std::int64_t index, std::uint64_t word,
    std::uint64_t bits) {
  static_cast<EvaluationAttempts*>(attempts)->SetWord(index, word, bits);
}

void lyra_rt_evaluation_attempts_step(
    void* attempts, std::int64_t index, std::uint64_t outcome) {
  static_cast<EvaluationAttempts*>(attempts)->Step(index, outcome);
}

void lyra_rt_evaluation_attempts_seed(
    void* attempts, std::int64_t index, bool this_tick) {
  static_cast<EvaluationAttempts*>(attempts)->Seed(index, this_tick);
}

void lyra_rt_evaluation_attempts_settle(void* attempts, void* effects) {
  static_cast<EvaluationAttempts*>(attempts)->Settle(
      *static_cast<RuntimeEffects*>(effects));
}

auto lyra_rt_evaluation_attempts_answer(const void* attempts) -> std::uint64_t {
  return static_cast<const EvaluationAttempts*>(attempts)->Answer();
}

// A procedural local whose value crosses a suspension. The cell is allocated in
// the running execution's own value store, so the handle the generated frame
// carries across a suspension points at storage that outlives every stretch of
// that body. A store overwrites the cell in place -- the first store installs
// the declared representation -- and a load answers with the value where the
// cell holds it. A procedural local is not observable, so no runtime handle
// threads through and no subscriber wakes.
auto lyra_rt_string_value_cell_alloc() noexcept -> void* {
  return AllocateValueCell<String>();
}

void lyra_rt_string_value_cell_store(void* cell, const void* value) noexcept {
  LocalAt<String>(cell).Assign(Read<String>(value));
}

auto lyra_rt_string_value_cell_load(void* cell) noexcept -> void* {
  return &LocalAt<String>(cell).Storage();
}

void lyra_rt_assert(bool holds, const char* message) {
  lyra::value::Assert(holds, message);
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

auto lyra_rt_string_concat(const void* lhs, const void* rhs, void* out)
    -> void* {
  return Emplace(out, Read<String>(lhs).Concat(Read<String>(rhs)));
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
    const void* prototypes_type, const void* answer_type, void* out) -> void* {
  return lyra::runtime::EmplaceScan(
      out, lyra::runtime::TupleTypeAt(answer_type), Read<String>(input),
      Read<String>(format), lyra::value::detail::NullByte::kWhiteSpace,
      prototypes, lyra::runtime::TupleTypeAt(prototypes_type));
}

auto lyra_rt_string_scan_file(
    const void* input, const void* format, const void* prototypes,
    const void* prototypes_type, const void* answer_type, void* out) -> void* {
  return lyra::runtime::EmplaceScan(
      out, lyra::runtime::TupleTypeAt(answer_type), Read<String>(input),
      Read<String>(format), lyra::value::detail::NullByte::kOrdinary,
      prototypes, lyra::runtime::TupleTypeAt(prototypes_type));
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

auto lyra_rt_make_format_spec(
    std::int64_t kind, std::int64_t field_width, std::int64_t precision,
    std::int64_t zero_pad, std::int64_t left_align, std::int64_t timeunit_power,
    void* out) -> void* {
  return Emplace(
      out,
      FormatSpec(
          kind, field_width, precision, zero_pad, left_align, timeunit_power));
}

auto lyra_rt_integral_make_print_value_item(
    const void* value, std::int64_t value_width, bool value_is_signed,
    bool value_is_four_state, const void* spec, void* out) -> void* {
  return Emplace(
      out,
      PrintItem(PrintValueItem(
          lyra::runtime::IntegralFormatArg(
              value, lyra::runtime::NumberShape(
                         value_width, value_is_signed, value_is_four_state)),
          Read<FormatSpec>(spec))));
}

auto lyra_rt_string_make_print_value_item(
    const void* value, const void* spec, void* out) -> void* {
  return Emplace(
      out,
      PrintItem(PrintValueItem(Read<String>(value), Read<FormatSpec>(spec))));
}

auto lyra_rt_chandle_make_print_value_item(
    const void* value, const void* spec, void* out) -> void* {
  return Emplace(
      out,
      PrintItem(PrintValueItem(Read<Chandle>(value), Read<FormatSpec>(spec))));
}

auto lyra_rt_managedref_make_print_value_item(
    const void* value, const void* spec, void* out) -> void* {
  return Emplace(
      out, PrintItem(
               PrintValueItem(Read<ObjectRef>(value), Read<FormatSpec>(spec))));
}

auto lyra_rt_real_add(const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(out, Read<Real>(lhs) + Read<Real>(rhs));
}

auto lyra_rt_real_sub(const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(out, Read<Real>(lhs) - Read<Real>(rhs));
}

auto lyra_rt_real_mul(const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(out, Read<Real>(lhs) * Read<Real>(rhs));
}

auto lyra_rt_real_div(const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(out, Read<Real>(lhs) / Read<Real>(rhs));
}

auto lyra_rt_real_neg(const void* operand, void* out) -> void* {
  return Emplace(out, -Read<Real>(operand));
}

auto lyra_rt_real_eq(const void* lhs, const void* rhs) -> std::uint8_t {
  return Compared(Read<Real>(lhs) == Read<Real>(rhs));
}

auto lyra_rt_real_ne(const void* lhs, const void* rhs) -> std::uint8_t {
  return Compared(Read<Real>(lhs) != Read<Real>(rhs));
}

auto lyra_rt_real_case_equal(const void* lhs, const void* rhs, void* out)
    -> void* {
  return Emplace(out, Read<Real>(lhs).CaseEqual(Read<Real>(rhs)));
}

auto lyra_rt_real_lt(const void* lhs, const void* rhs) -> std::uint8_t {
  return Compared(Read<Real>(lhs) < Read<Real>(rhs));
}

auto lyra_rt_real_le(const void* lhs, const void* rhs) -> std::uint8_t {
  return Compared(Read<Real>(lhs) <= Read<Real>(rhs));
}

auto lyra_rt_real_gt(const void* lhs, const void* rhs) -> std::uint8_t {
  return Compared(Read<Real>(lhs) > Read<Real>(rhs));
}

auto lyra_rt_real_ge(const void* lhs, const void* rhs) -> std::uint8_t {
  return Compared(Read<Real>(lhs) >= Read<Real>(rhs));
}

auto lyra_rt_real_to_bool(const void* operand) -> bool {
  return static_cast<bool>(Read<Real>(operand));
}

auto lyra_rt_real_pow(const void* base, const void* exponent, void* out)
    -> void* {
  return Emplace(out, Read<Real>(base).Pow(Read<Real>(exponent)));
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

auto lyra_rt_real_const(double value, void* out) -> void* {
  return Emplace(out, Real{value});
}

auto lyra_rt_real_from_int(std::int64_t value, void* out) -> void* {
  return Emplace(out, Real::FromInt(value));
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
  LocalAt<Real>(cell).Assign(Read<Real>(value));
}

auto lyra_rt_real_value_cell_load(void* cell) noexcept -> void* {
  return &LocalAt<Real>(cell).Storage();
}

auto lyra_rt_real_make_print_value_item(
    const void* value, const void* spec, void* out) -> void* {
  return Emplace(
      out,
      PrintItem(PrintValueItem(Read<Real>(value), Read<FormatSpec>(spec))));
}

auto lyra_rt_real_make_format_arg(const void* value, void* out) -> void* {
  return Emplace(out, MakeFormatArg(Read<Real>(value)));
}

auto lyra_rt_shortreal_add(const void* lhs, const void* rhs, void* out)
    -> void* {
  return Emplace(out, Read<ShortReal>(lhs) + Read<ShortReal>(rhs));
}

auto lyra_rt_shortreal_sub(const void* lhs, const void* rhs, void* out)
    -> void* {
  return Emplace(out, Read<ShortReal>(lhs) - Read<ShortReal>(rhs));
}

auto lyra_rt_shortreal_mul(const void* lhs, const void* rhs, void* out)
    -> void* {
  return Emplace(out, Read<ShortReal>(lhs) * Read<ShortReal>(rhs));
}

auto lyra_rt_shortreal_div(const void* lhs, const void* rhs, void* out)
    -> void* {
  return Emplace(out, Read<ShortReal>(lhs) / Read<ShortReal>(rhs));
}

auto lyra_rt_shortreal_neg(const void* operand, void* out) -> void* {
  return Emplace(out, -Read<ShortReal>(operand));
}

auto lyra_rt_shortreal_eq(const void* lhs, const void* rhs) -> std::uint8_t {
  return Compared(Read<ShortReal>(lhs) == Read<ShortReal>(rhs));
}

auto lyra_rt_shortreal_ne(const void* lhs, const void* rhs) -> std::uint8_t {
  return Compared(Read<ShortReal>(lhs) != Read<ShortReal>(rhs));
}

auto lyra_rt_shortreal_case_equal(const void* lhs, const void* rhs, void* out)
    -> void* {
  return Emplace(out, Read<ShortReal>(lhs).CaseEqual(Read<ShortReal>(rhs)));
}

auto lyra_rt_shortreal_lt(const void* lhs, const void* rhs) -> std::uint8_t {
  return Compared(Read<ShortReal>(lhs) < Read<ShortReal>(rhs));
}

auto lyra_rt_shortreal_le(const void* lhs, const void* rhs) -> std::uint8_t {
  return Compared(Read<ShortReal>(lhs) <= Read<ShortReal>(rhs));
}

auto lyra_rt_shortreal_gt(const void* lhs, const void* rhs) -> std::uint8_t {
  return Compared(Read<ShortReal>(lhs) > Read<ShortReal>(rhs));
}

auto lyra_rt_shortreal_ge(const void* lhs, const void* rhs) -> std::uint8_t {
  return Compared(Read<ShortReal>(lhs) >= Read<ShortReal>(rhs));
}

auto lyra_rt_shortreal_to_bool(const void* operand) -> bool {
  return static_cast<bool>(Read<ShortReal>(operand));
}

auto lyra_rt_shortreal_pow(const void* base, const void* exponent, void* out)
    -> void* {
  return Emplace(out, Read<ShortReal>(base).Pow(Read<ShortReal>(exponent)));
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

auto lyra_rt_shortreal_from_bits(std::int64_t bits, void* out) -> void* {
  return Emplace(out, ShortReal::FromBits(bits));
}

auto lyra_rt_shortreal_const(float value, void* out) -> void* {
  return Emplace(out, ShortReal{value});
}

auto lyra_rt_shortreal_from_int(std::int64_t value, void* out) -> void* {
  return Emplace(out, ShortReal::FromInt(value));
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
  LocalAt<ShortReal>(cell).Assign(Read<ShortReal>(value));
}

auto lyra_rt_shortreal_value_cell_load(void* cell) noexcept -> void* {
  return &LocalAt<ShortReal>(cell).Storage();
}

auto lyra_rt_shortreal_make_print_value_item(
    const void* value, const void* spec, void* out) -> void* {
  return Emplace(
      out, PrintItem(
               PrintValueItem(Read<ShortReal>(value), Read<FormatSpec>(spec))));
}

auto lyra_rt_shortreal_make_format_arg(const void* value, void* out) -> void* {
  return Emplace(out, MakeFormatArg(Read<ShortReal>(value)));
}

// The two directions of the boundary between a chandle and the host pointer it
// carries (LRM 6.14). They are entries because which bits a domain's value is
// stays the runtime's answer: a generated body asks for a chandle and is handed
// whatever one is.
auto lyra_rt_chandle_default(void* out) -> void* {
  return Emplace(out, Chandle{});
}

auto lyra_rt_chandle_make(void* pointer, void* out) -> void* {
  return Emplace(out, Chandle{pointer});
}

auto lyra_rt_chandle_ptr(const void* operand) -> void* {
  return Read<Chandle>(operand).Ptr();
}

auto lyra_rt_chandle_eq(const void* lhs, const void* rhs) -> std::uint8_t {
  return Compared(Read<Chandle>(lhs) == Read<Chandle>(rhs));
}

auto lyra_rt_chandle_ne(const void* lhs, const void* rhs) -> std::uint8_t {
  return Compared(Read<Chandle>(lhs) != Read<Chandle>(rhs));
}

auto lyra_rt_chandle_case_equal(const void* lhs, const void* rhs, void* out)
    -> void* {
  return Emplace(out, Read<Chandle>(lhs).CaseEqual(Read<Chandle>(rhs)));
}

auto lyra_rt_chandle_to_bool(const void* operand) -> bool {
  return static_cast<bool>(Read<Chandle>(operand));
}

auto lyra_rt_chandle_value_cell_alloc() noexcept -> void* {
  return AllocateValueCell<Chandle>();
}

void lyra_rt_chandle_value_cell_store(void* cell, const void* value) noexcept {
  LocalAt<Chandle>(cell).Assign(Read<Chandle>(value));
}

auto lyra_rt_chandle_value_cell_load(void* cell) noexcept -> void* {
  return &LocalAt<Chandle>(cell).Storage();
}

// A chandle variable, which a process may wait on: LRM 9.4.2 makes a write to
// one an event whenever the pointer it holds is not the pointer it held.
auto lyra_rt_chandle_cell_get(void* cell) -> const void* {
  return &VariableAt<Chandle>(cell).Get();
}

void lyra_rt_chandle_cell_initialize(
    void* cell, const void* prototype) noexcept {
  VariableAt<Chandle>(cell).Initialize(Read<Chandle>(prototype));
}

void lyra_rt_chandle_cell_set(void* cell, const void* value) {
  VariableAt<Chandle>(cell).Set(Read<Chandle>(value));
}

void lyra_rt_chandle_cell_arm_sampling(void* cell) {
  VariableAt<Chandle>(cell).ArmSampling();
}

auto lyra_rt_chandle_cell_sampled_get(void* cell) -> const void* {
  return &VariableAt<Chandle>(cell).SampledGet();
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
  LocalAt<ObjectRef>(cell).Assign(Read<ObjectRef>(value));
}

auto lyra_rt_managedref_value_cell_load(void* cell) noexcept -> void* {
  return &LocalAt<ObjectRef>(cell).Storage();
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

auto lyra_rt_managedref_cell_sampled_get(void* cell) -> const void* {
  return &VariableAt<ObjectRef>(cell).SampledGet();
}

auto lyra_rt_managedref_cell_begin_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count)
    -> std::int64_t {
  return CellBeginTakeover<ObjectRef>(cell, level, first, count);
}

auto lyra_rt_managedref_cell_drive_takeover(
    void* cell, std::int64_t level, std::int64_t generation, const void* value)
    -> bool {
  return CellDriveTakeover<ObjectRef>(cell, level, generation, value);
}

void lyra_rt_managedref_cell_end_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count) {
  CellEndTakeover<ObjectRef>(cell, level, first, count);
}

auto lyra_rt_managedref_ref_begin_takeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count)
    -> std::int64_t {
  return RefBeginTakeover<ObjectRef>(reference, level, first, count);
}

auto lyra_rt_managedref_ref_drive_takeover(
    void* reference, std::int64_t level, std::int64_t generation,
    const void* value) -> bool {
  return RefDriveTakeover<ObjectRef>(reference, level, generation, value);
}

void lyra_rt_managedref_ref_end_takeover(
    void* reference, std::int64_t level, std::int64_t first,
    std::int64_t count) {
  RefEndTakeover<ObjectRef>(reference, level, first, count);
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
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count)
    -> std::int64_t {
  return VariableAt<LaidOutValue>(cell).BeginTakeover(level, first, count);
}

auto lyra_rt_laidout_cell_drive_takeover(
    void* cell, std::int64_t level, std::int64_t generation, const void* value)
    -> bool {
  return VariableAt<LaidOutValue>(cell).DriveTakeoverBytes(
      level, generation, value);
}

void lyra_rt_laidout_cell_end_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count) {
  VariableAt<LaidOutValue>(cell).EndTakeover(level, first, count);
}

auto lyra_rt_laidout_ref_begin_takeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count)
    -> std::int64_t {
  return lyra::runtime::LaidOutRefBeginTakeover(reference, level, first, count);
}

auto lyra_rt_laidout_ref_drive_takeover(
    void* reference, std::int64_t level, std::int64_t generation,
    const void* value) -> bool {
  return lyra::runtime::LaidOutRefDriveTakeover(
      reference, level, generation, value);
}

void lyra_rt_laidout_ref_end_takeover(
    void* reference, std::int64_t level, std::int64_t first,
    std::int64_t count) {
  lyra::runtime::EndTakeoverThrough<
      lyra::runtime::VariableOf<LaidOutValue>::Type>(
      lyra::runtime::ErasedAt(reference), level, first, count);
}

auto lyra_rt_laidout_cell_sampled_get(void* cell) -> const void* {
  return VariableAt<LaidOutValue>(cell).SampledBytes();
}

auto lyra_rt_laidout_value_cell_alloc(const void* held_type) noexcept -> void* {
  return AllocateValueCell<LaidOutValue>(lyra::runtime::TypeAt(held_type));
}

// A tagged union's `void` member (LRM 7.3.2) carries a value with no bits.
// `default` builds the one value it has.
auto lyra_rt_empty_default(void* out) -> void* {
  return Emplace(out, lyra::value::Empty{});
}

auto lyra_rt_make_dynamic_array_new(
    std::int64_t size, const void* answer_element_type, void* out) -> void* {
  return Emplace(
      out, RuntimeDynamicArray(
               lyra::runtime::ElementTypeAt(answer_element_type),
               lyra::runtime::NewCount(size), nullptr));
}

auto lyra_rt_make_dynamic_array_new_copy(
    std::int64_t size, const void* src, const void* answer_element_type,
    void* out) -> void* {
  return Emplace(
      out, RuntimeDynamicArray(
               lyra::runtime::ElementTypeAt(answer_element_type),
               lyra::runtime::NewCount(size), &Read<RuntimeDynamicArray>(src)));
}

auto lyra_rt_dynarray_from_literal(
    LyraSpan unit, std::int64_t count, const void* answer_element_type,
    void* out) -> void* {
  return Emplace(
      out, RuntimeDynamicArray::FromElements(
               lyra::runtime::ElementTypeAt(answer_element_type),
               lyra::runtime::ReplicateHandles(unit, count)));
}

auto lyra_rt_laidout_dynamic_array_from_array(
    const void* source, const void* source_type,
    const void* answer_element_type, void* out) -> void* {
  return Emplace(
      out, RuntimeDynamicArray::FromElements(
               lyra::runtime::ElementTypeAt(answer_element_type),
               lyra::runtime::ElementHandles(source, source_type)));
}

auto lyra_rt_queue_dynamic_array_from_array(
    const void* source, const void* answer_element_type, void* out) -> void* {
  return Emplace(
      out, RuntimeDynamicArray::FromElements(
               lyra::runtime::ElementTypeAt(answer_element_type),
               lyra::runtime::ElementHandles(Read<RuntimeQueue>(source))));
}

// Reads the element at `position` where it lies. A position naming no element
// reads the element default (LRM 7.4.5).
auto lyra_rt_dynarray_element(const void* array, const void* position) -> const
    void* {
  return Read<RuntimeDynamicArray>(array).Element(PositionNamedAt(position));
}

auto lyra_rt_dynarray_element_mut(void* array, const void* position) -> void* {
  lyra::value::Formation formed{};
  return static_cast<RuntimeDynamicArray*>(array)->ElementMut(
      PositionNamedAt(position), formed);
}

auto lyra_rt_dynarray_concat_element(
    const void* array, const void* item, void* out) -> void* {
  const std::array<const void*, 1> items{item};
  return Emplace(out, Read<RuntimeDynamicArray>(array).Concat(items));
}

auto lyra_rt_dynarray_concat_spread(
    const void* array, const void* part, const void* part_type, void* out)
    -> void* {
  return Emplace(
      out, Read<RuntimeDynamicArray>(array).Concat(
               lyra::runtime::ElementHandles(part, part_type)));
}

// LRM 7.5.3 `delete`, which empties the array where it lies.
void lyra_rt_dynarray_delete(void* array) {
  static_cast<RuntimeDynamicArray*>(array)->Delete();
}

auto lyra_rt_dynarray_element_slice(
    const void* array, const void* start, std::int64_t count,
    const void* answer_type, void* out) -> void* {
  const auto& source = Read<RuntimeDynamicArray>(array);
  return Emplace(
      out, ArrayFromElements(
               ArrayTypeAt(answer_type),
               source.SliceElements(PositionNamedAt(start), count)));
}

void lyra_rt_dynarray_assign_element_slice(
    void* array, const void* start, std::int64_t count, const void* replacement,
    const void* replacement_type) {
  static_cast<RuntimeDynamicArray*>(array)->AssignSlice(
      PositionNamedAt(start), count,
      lyra::runtime::ElementHandles(replacement, replacement_type));
}

auto lyra_rt_dynarray_size(const void* array, void* out) -> void* {
  return Emplace(out, Read<RuntimeDynamicArray>(array).Size());
}

auto lyra_rt_dynarray_eq(const void* lhs, const void* rhs) -> std::uint8_t {
  return Compared(
      Read<RuntimeDynamicArray>(lhs) == Read<RuntimeDynamicArray>(rhs));
}

auto lyra_rt_dynarray_ne(const void* lhs, const void* rhs) -> std::uint8_t {
  return Compared(
      Read<RuntimeDynamicArray>(lhs) != Read<RuntimeDynamicArray>(rhs));
}

auto lyra_rt_dynarray_case_equal(const void* lhs, const void* rhs, void* out)
    -> void* {
  return Emplace(
      out,
      Read<RuntimeDynamicArray>(lhs).CaseEqual(Read<RuntimeDynamicArray>(rhs)));
}

auto lyra_rt_dynarray_merge_conditional(
    const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(
      out, Read<RuntimeDynamicArray>(lhs).MergeConditional(
               Read<RuntimeDynamicArray>(rhs)));
}

auto lyra_rt_dynarray_cell_get(void* cell) -> const void* {
  return &VariableAt<RuntimeDynamicArray>(cell).Get();
}

void lyra_rt_dynarray_cell_initialize(
    void* cell, const void* prototype) noexcept {
  VariableAt<RuntimeDynamicArray>(cell).Initialize(
      Read<RuntimeDynamicArray>(prototype));
}

void lyra_rt_dynarray_cell_set(void* cell, const void* value) {
  VariableAt<RuntimeDynamicArray>(cell).Set(Read<RuntimeDynamicArray>(value));
}

void lyra_rt_dynarray_cell_arm_sampling(void* cell) {
  VariableAt<RuntimeDynamicArray>(cell).ArmSampling();
}

auto lyra_rt_dynarray_cell_sampled_get(void* cell) -> const void* {
  return &VariableAt<RuntimeDynamicArray>(cell).SampledGet();
}

auto lyra_rt_dynarray_value_cell_alloc() noexcept -> void* {
  return AllocateValueCell<RuntimeDynamicArray>();
}

void lyra_rt_dynarray_value_cell_store(void* cell, const void* value) noexcept {
  LocalAt<RuntimeDynamicArray>(cell).Assign(Read<RuntimeDynamicArray>(value));
}

auto lyra_rt_dynarray_value_cell_load(void* cell) noexcept -> void* {
  return &LocalAt<RuntimeDynamicArray>(cell).Storage();
}

// LRM 10.9.1: an enumerated element list is the unit laid down once, and a
// replicated or uniform one the unit laid down until the array is full. The
// array's type fixes how many elements that is, so a unit that does not fill
// it a whole number of times is a call no lowering states.
auto lyra_rt_laidout_from_elements(
    LyraSpan unit, const void* answer_type, void* out) -> void* {
  const lyra::value::FixedArrayType& type = ArrayTypeAt(answer_type);
  const std::span<const void* const> handles(
      static_cast<const void* const*>(unit.data), unit.count);
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

// LRM 10.10: adopt an unpacked concatenation's parts, accumulated into a
// dynamic array, into a fixed-size target. A count the front end could not
// verify -- because a spread part is sized at run time -- is checked here, a
// mismatch being the design's own failure.
auto lyra_rt_laidout_conform_size(
    const void* parts, const void* answer_type, void* out) -> void* {
  const auto& source = Read<RuntimeDynamicArray>(parts);
  const lyra::value::FixedArrayType& target = ArrayTypeAt(answer_type);
  if (source.Count() != target.Count()) {
    throw lyra::SimulationError(
        "an unpacked array concatenation yields {} elements, while the "
        "fixed-size target has {}",
        source.Count(), target.Count());
  }
  return Emplace(
      out, ArrayFromElements(target, lyra::runtime::ElementHandles(source)));
}

auto lyra_rt_dynarray_unpacked_array_from_array(
    const void* source, const void* answer_type, void* out) -> void* {
  return Emplace(
      out,
      ArrayFromElements(
          ArrayTypeAt(answer_type),
          lyra::runtime::ElementHandles(Read<RuntimeDynamicArray>(source))));
}

auto lyra_rt_queue_unpacked_array_from_array(
    const void* source, const void* answer_type, void* out) -> void* {
  return Emplace(
      out, ArrayFromElements(
               ArrayTypeAt(answer_type),
               lyra::runtime::ElementHandles(Read<RuntimeQueue>(source))));
}

auto lyra_rt_laidout_merge_conditional(
    const void* lhs, const void* rhs, const void* lhs_type, void* out)
    -> void* {
  const lyra::value::FixedArrayView left = ArrayAt(lhs, lhs_type);
  const lyra::value::FixedArrayView right = ArrayAt(rhs, lhs_type);
  const lyra::value::ElementType& element = left.ElementType();
  ArrayTypeAt(lhs_type).BuildEach(out, [&](std::size_t i, void* built) {
    const bool agree = lyra::value::Holds(
        element.Type().Equal(left.ElementAt(i), right.ElementAt(i)));
    element.Type().Copy(agree ? left.ElementAt(i) : element.Default(), built);
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

auto lyra_rt_dynarray_count_bits(
    const void* value, const void* control_bits, std::int64_t control_width,
    bool control_is_four_state, void* out) -> void* {
  const lyra::value::LoadedWords control =
      BitsAt(control_bits, control_width, control_is_four_state);
  return Emplace(
      out, Read<RuntimeDynamicArray>(value).CountBits(control.View()));
}

auto lyra_rt_string_count_bits(
    const void* value, const void* control_bits, std::int64_t control_width,
    bool control_is_four_state, void* out) -> void* {
  const lyra::value::LoadedWords control =
      BitsAt(control_bits, control_width, control_is_four_state);
  return Emplace(out, Read<String>(value).CountBits(control.View()));
}

auto lyra_rt_string_bitstream_width(const void* value, void* out) -> void* {
  return Emplace(out, Read<String>(value).BitstreamWidth());
}

auto lyra_rt_dynarray_bitstream_width(const void* value, void* out) -> void* {
  return Emplace(out, Read<RuntimeDynamicArray>(value).BitstreamWidth());
}

auto lyra_rt_from_bitstream(
    const void* bits, std::int64_t bits_width, bool bits_are_four_state,
    const void* answer_type, void* out) -> void* {
  const lyra::value::LoadedWords stream =
      BitsAt(bits, bits_width, bits_are_four_state);
  lyra::runtime::TypeAt(answer_type).ReadFromStream(stream.View(), 0, out);
  return out;
}

auto lyra_rt_stream_write(
    LyraStreamOf stream_of, const void* value, std::int64_t bits_width,
    bool bits_are_four_state, const void* stream, std::uint64_t filled)
    -> std::uint64_t {
  lyra::runtime::StorageForBits made(
      lyra::runtime::BitsShape(bits_width, bits_are_four_state));
  stream_of(value, made.Bytes());
  const lyra::value::LoadedWords written =
      BitsAt(made.Bytes(), bits_width, bits_are_four_state);
  const auto& into = *static_cast<const lyra::value::IntegralView*>(stream);
  const std::uint64_t width = written.View().width;
  lyra::value::Insert(
      into, written.View(),
      static_cast<std::int64_t>(into.width - filled - width));
  return filled + width;
}

auto lyra_rt_stream_build(
    LyraBuiltFromStream built_from, const void* stream, std::uint64_t taken,
    std::int64_t bits_width, bool bits_are_four_state, void* out)
    -> std::uint64_t {
  lyra::runtime::StorageForBits read(
      lyra::runtime::BitsShape(bits_width, bits_are_four_state));
  const std::uint64_t after = lyra_rt_stream_read(
      stream, taken, bits_width, bits_are_four_state, read.Bytes());
  built_from(read.Bytes(), out);
  return after;
}

auto lyra_rt_stream_read(
    const void* stream, std::uint64_t taken, std::int64_t out_width,
    bool out_is_four_state, void* out) -> std::uint64_t {
  lyra::value::WrittenWords read(
      lyra::runtime::BitsShape(out_width, out_is_four_state));
  const auto& from =
      *static_cast<const lyra::value::ConstIntegralView*>(stream);
  const auto width = static_cast<std::uint64_t>(out_width);
  lyra::value::Extract(
      read.MutableView(), from,
      static_cast<std::int64_t>(from.width - taken - width));
  read.StoreTo(out);
  return taken + width;
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

auto lyra_rt_wildcard_index_move(void* value, void* out) -> void* {
  return std::construct_at(
      static_cast<lyra::value::AnyValue*>(out),
      std::move(*static_cast<lyra::value::AnyValue*>(value)));
}

void lyra_rt_wildcard_index_destroy(void* object) {
  std::destroy_at(static_cast<lyra::value::AnyValue*>(object));
}

void lyra_rt_wildcard_index_assign(void* storage, const void* value) {
  *static_cast<lyra::value::AnyValue*>(storage) =
      *static_cast<const lyra::value::AnyValue*>(value);
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
auto lyra_rt_chandle_bit_identical(const void* lhs, const void* rhs) -> bool {
  return lyra::runtime::BitIdentical<Chandle>(lhs, rhs);
}
auto lyra_rt_dynarray_bit_identical(const void* lhs, const void* rhs) -> bool {
  return lyra::runtime::BitIdentical<RuntimeDynamicArray>(lhs, rhs);
}
auto lyra_rt_queue_bit_identical(const void* lhs, const void* rhs) -> bool {
  return lyra::runtime::BitIdentical<RuntimeQueue>(lhs, rhs);
}
auto lyra_rt_assocarray_bit_identical(const void* lhs, const void* rhs)
    -> bool {
  return lyra::runtime::BitIdentical<RuntimeAssociativeArray>(lhs, rhs);
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
auto lyra_rt_chandle_has_unknown(const void* value) -> bool {
  return lyra::runtime::HasUnknown<Chandle>(value);
}
auto lyra_rt_dynarray_has_unknown(const void* value) -> bool {
  return lyra::runtime::HasUnknown<RuntimeDynamicArray>(value);
}
auto lyra_rt_queue_has_unknown(const void* value) -> bool {
  return lyra::runtime::HasUnknown<RuntimeQueue>(value);
}
auto lyra_rt_assocarray_has_unknown(const void* value) -> bool {
  return lyra::runtime::HasUnknown<RuntimeAssociativeArray>(value);
}
auto lyra_rt_managedref_has_unknown(const void* value) -> bool {
  return lyra::runtime::HasUnknown<ObjectRef>(value);
}

auto lyra_rt_managedref_bitstream_width(const void* value, void* out) -> void* {
  return lyra::runtime::StreamWidthOf<ObjectRef>(value, out);
}

auto lyra_rt_managedref_count_bits(
    const void* value, const void* control_bits, std::int64_t control_width,
    bool control_is_four_state, void* out) -> void* {
  return lyra::runtime::StreamCountBitsOf<ObjectRef>(
      value, BitsAt(control_bits, control_width, control_is_four_state), out);
}

auto lyra_rt_string_to_bitstream(
    const void* value, std::int64_t out_width, bool out_is_four_state,
    void* out) -> void* {
  return lyra::runtime::StreamOf<String>(
      value, out_width, out_is_four_state, out);
}
auto lyra_rt_dynarray_to_bitstream(
    const void* value, std::int64_t out_width, bool out_is_four_state,
    void* out) -> void* {
  return lyra::runtime::StreamOf<RuntimeDynamicArray>(
      value, out_width, out_is_four_state, out);
}
auto lyra_rt_queue_to_bitstream(
    const void* value, std::int64_t out_width, bool out_is_four_state,
    void* out) -> void* {
  return lyra::runtime::StreamOf<RuntimeQueue>(
      value, out_width, out_is_four_state, out);
}
auto lyra_rt_assocarray_to_bitstream(
    const void* value, std::int64_t out_width, bool out_is_four_state,
    void* out) -> void* {
  return lyra::runtime::StreamOf<RuntimeAssociativeArray>(
      value, out_width, out_is_four_state, out);
}
auto lyra_rt_managedref_to_bitstream(
    const void* value, std::int64_t out_width, bool out_is_four_state,
    void* out) -> void* {
  return lyra::runtime::StreamOf<ObjectRef>(
      value, out_width, out_is_four_state, out);
}

auto lyra_rt_laidout_size(const void* array, const void* array_type, void* out)
    -> void* {
  return Emplace(
      out, lyra::value::Int::FromInt(
               static_cast<std::int64_t>(ArrayAt(array, array_type).Count())));
}

auto lyra_rt_laidout_element_slice(
    const void* array, const void* start, std::int64_t count,
    const void* array_type, const void* answer_type, void* out) -> void* {
  const std::vector<const void*> items =
      ArrayTypeAt(array_type)
          .SliceElements(
              array, PositionNamedAt(start), lyra::value::SliceCount(count));
  return Emplace(out, ArrayFromElements(ArrayTypeAt(answer_type), items));
}

void lyra_rt_laidout_assign_element_slice(
    void* array, const void* start, std::int64_t count, const void* replacement,
    const void* replacement_type, const void* array_type) {
  ArrayTypeAt(array_type)
      .AssignSlice(
          array, PositionNamedAt(start), static_cast<std::size_t>(count),
          lyra::runtime::ElementHandles(replacement, replacement_type));
}

void lyra_rt_laidout_aggregate_net_initialize_tri_state(
    void* net, const void* prototype, std::int64_t fill,
    std::int64_t strength) {
  ResolvedNet<LaidOutValue>& resolved = NetOf<LaidOutValue>(net);
  resolved.InitializeTriState(resolved.ToOwned(prototype), fill, strength);
}

void lyra_rt_laidout_aggregate_net_initialize_wired_and(
    void* net, const void* prototype, std::int64_t fill,
    std::int64_t strength) {
  ResolvedNet<LaidOutValue>& resolved = NetOf<LaidOutValue>(net);
  resolved.InitializeWiredAnd(resolved.ToOwned(prototype), fill, strength);
}

void lyra_rt_laidout_aggregate_net_initialize_wired_or(
    void* net, const void* prototype, std::int64_t fill,
    std::int64_t strength) {
  ResolvedNet<LaidOutValue>& resolved = NetOf<LaidOutValue>(net);
  resolved.InitializeWiredOr(resolved.ToOwned(prototype), fill, strength);
}

void lyra_rt_laidout_aggregate_net_initialize_retaining(
    void* net, const void* prototype, std::int64_t fill,
    std::int64_t strength) {
  ResolvedNet<LaidOutValue>& resolved = NetOf<LaidOutValue>(net);
  resolved.InitializeRetaining(resolved.ToOwned(prototype), fill, strength);
}

auto lyra_rt_laidout_net_begin_takeover(
    void* net, std::int64_t level, std::int64_t first, std::int64_t count)
    -> std::int64_t {
  return NetOf<LaidOutValue>(net).BeginTakeover(level, first, count);
}

auto lyra_rt_laidout_net_drive_takeover(
    void* net, std::int64_t level, std::int64_t generation, const void* value)
    -> bool {
  return NetOf<LaidOutValue>(net).DriveTakeoverBytes(level, generation, value);
}

void lyra_rt_laidout_net_end_takeover(
    void* net, std::int64_t level, std::int64_t first, std::int64_t count) {
  NetOf<LaidOutValue>(net).EndTakeover(level, first, count);
}

void lyra_rt_laidout_net_arm_sampling(void* net) {
  NetOf<LaidOutValue>(net).ArmSampling();
}

auto lyra_rt_laidout_net_sampled_get(void* net) -> const void* {
  return HandleTo(NetOf<LaidOutValue>(net).SampledGet());
}

auto lyra_rt_laidout_attach_driver(void* net, std::int64_t strength) -> void* {
  return &NetOf<LaidOutValue>(net).AttachDriver(strength);
}

auto lyra_rt_laidout_driver_get(void* driver) -> const void* {
  return DriverOf<LaidOutValue>(driver).Bytes();
}

void lyra_rt_laidout_driver_set(void* driver, const void* value) {
  DriverOf<LaidOutValue>(driver).SetBytes(value);
}

auto lyra_rt_queue_from_literal(
    LyraSpan unit, std::int64_t count, const void* answer_type, void* out)
    -> void* {
  return Emplace(
      out, RuntimeQueue::FromElements(
               QueueTypeAt(answer_type),
               lyra::runtime::ReplicateHandles(unit, count)));
}

auto lyra_rt_queue_conform_bound(
    const void* queue, const void* answer_type, void* out) -> void* {
  return Emplace(
      out, Read<RuntimeQueue>(queue).ConformBound(QueueTypeAt(answer_type)));
}

auto lyra_rt_laidout_queue_from_array(
    const void* source, const void* source_type, const void* answer_type,
    void* out) -> void* {
  return Emplace(
      out, RuntimeQueue::FromElements(
               QueueTypeAt(answer_type),
               lyra::runtime::ElementHandles(source, source_type)));
}

auto lyra_rt_dynarray_queue_from_array(
    const void* source, const void* answer_type, void* out) -> void* {
  return Emplace(
      out,
      RuntimeQueue::FromElements(
          QueueTypeAt(answer_type),
          lyra::runtime::ElementHandles(Read<RuntimeDynamicArray>(source))));
}

auto lyra_rt_queue_element(const void* queue, const void* position) -> const
    void* {
  return Read<RuntimeQueue>(queue).Element(PositionNamedAt(position));
}

auto lyra_rt_queue_element_mut(void* queue, const void* position) -> void* {
  lyra::value::Formation formed{};
  return static_cast<RuntimeQueue*>(queue)->ElementMut(
      PositionNamedAt(position), formed);
}

auto lyra_rt_queue_slice(
    const void* queue, const void* lo, const void* hi, const void* answer_type,
    void* out) -> void* {
  return Emplace(
      out,
      Read<RuntimeQueue>(queue).Slice(
          PositionNamedAt(lo), PositionNamedAt(hi), QueueTypeAt(answer_type)));
}

auto lyra_rt_queue_size(const void* queue, void* out) -> void* {
  return Emplace(out, Read<RuntimeQueue>(queue).Size());
}

void lyra_rt_queue_push_back(void* queue, const void* item) {
  static_cast<RuntimeQueue*>(queue)->PushBack(item);
}

void lyra_rt_queue_push_front(void* queue, const void* item) {
  static_cast<RuntimeQueue*>(queue)->PushFront(item);
}

auto lyra_rt_queue_concat_element(
    const void* queue, const void* item, void* out) -> void* {
  const std::array<const void*, 1> items{item};
  return Emplace(out, Read<RuntimeQueue>(queue).Concat(items));
}

auto lyra_rt_queue_concat_spread(
    const void* queue, const void* part, const void* part_type, void* out)
    -> void* {
  return Emplace(
      out, Read<RuntimeQueue>(queue).Concat(
               lyra::runtime::ElementHandles(part, part_type)));
}

void lyra_rt_queue_insert(void* queue, const void* position, const void* item) {
  static_cast<RuntimeQueue*>(queue)->Insert(PositionNamedAt(position), item);
}

auto lyra_rt_queue_pop_front(void* queue, void* out) -> void* {
  static_cast<RuntimeQueue*>(queue)->PopFront(out);
  return out;
}

auto lyra_rt_queue_pop_back(void* queue, void* out) -> void* {
  static_cast<RuntimeQueue*>(queue)->PopBack(out);
  return out;
}

void lyra_rt_queue_delete(void* queue) {
  static_cast<RuntimeQueue*>(queue)->Delete();
}

void lyra_rt_queue_delete_index(void* queue, const void* position) {
  static_cast<RuntimeQueue*>(queue)->DeleteIndex(PositionNamedAt(position));
}

auto lyra_rt_queue_eq(const void* lhs, const void* rhs) -> std::uint8_t {
  return Compared(Read<RuntimeQueue>(lhs) == Read<RuntimeQueue>(rhs));
}

auto lyra_rt_queue_ne(const void* lhs, const void* rhs) -> std::uint8_t {
  return Compared(Read<RuntimeQueue>(lhs) != Read<RuntimeQueue>(rhs));
}

auto lyra_rt_queue_case_equal(const void* lhs, const void* rhs, void* out)
    -> void* {
  return Emplace(
      out, Read<RuntimeQueue>(lhs).CaseEqual(Read<RuntimeQueue>(rhs)));
}

auto lyra_rt_queue_merge_conditional(
    const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(
      out, Read<RuntimeQueue>(lhs).MergeConditional(Read<RuntimeQueue>(rhs)));
}

auto lyra_rt_queue_bitstream_width(const void* queue, void* out) -> void* {
  return Emplace(out, Read<RuntimeQueue>(queue).BitstreamWidth());
}

auto lyra_rt_queue_count_bits(
    const void* queue, const void* control_bits, std::int64_t control_width,
    bool control_is_four_state, void* out) -> void* {
  const lyra::value::LoadedWords control =
      BitsAt(control_bits, control_width, control_is_four_state);
  return Emplace(out, Read<RuntimeQueue>(queue).CountBits(control.View()));
}

auto lyra_rt_queue_cell_get(void* cell) -> const void* {
  return &VariableAt<RuntimeQueue>(cell).Get();
}

void lyra_rt_queue_cell_initialize(void* cell, const void* prototype) noexcept {
  VariableAt<RuntimeQueue>(cell).Initialize(Read<RuntimeQueue>(prototype));
}

void lyra_rt_queue_cell_set(void* cell, const void* value) {
  VariableAt<RuntimeQueue>(cell).Set(Read<RuntimeQueue>(value));
}

void lyra_rt_queue_cell_arm_sampling(void* cell) {
  VariableAt<RuntimeQueue>(cell).ArmSampling();
}

auto lyra_rt_queue_cell_sampled_get(void* cell) -> const void* {
  return &VariableAt<RuntimeQueue>(cell).SampledGet();
}

auto lyra_rt_queue_value_cell_alloc() noexcept -> void* {
  return AllocateValueCell<RuntimeQueue>();
}

void lyra_rt_queue_value_cell_store(void* cell, const void* value) noexcept {
  LocalAt<RuntimeQueue>(cell).Assign(Read<RuntimeQueue>(value));
}

auto lyra_rt_queue_value_cell_load(void* cell) noexcept -> void* {
  return &LocalAt<RuntimeQueue>(cell).Storage();
}

// LRM 7.9.11 `'{index: value, ...}`: each entry crosses as the product of the
// index and the element it stores, a tuple whose type states each component's
// type, which is what a keyed container needs of both: it knows the type of
// neither in advance.
auto lyra_rt_assocarray_from_entries(
    LyraSpan entries, const void* entry_type, const void* answer_element_type,
    void* out) -> void* {
  RuntimeAssociativeArray array(
      AssociativeIndexOrder::kIndexValueDomain,
      lyra::runtime::ElementTypeAt(answer_element_type));
  lyra::runtime::StoreLiteralEntries(
      array, entries, lyra::runtime::TupleTypeAt(entry_type));
  return Emplace(out, std::move(array));
}

auto lyra_rt_assocarray_from_entries_wildcard(
    LyraSpan entries, const void* entry_type, const void* answer_element_type,
    void* out) -> void* {
  RuntimeAssociativeArray array(
      AssociativeIndexOrder::kWildcardNumeric,
      lyra::runtime::ElementTypeAt(answer_element_type));
  lyra::runtime::StoreLiteralEntries(
      array, entries, lyra::runtime::TupleTypeAt(entry_type));
  return Emplace(out, std::move(array));
}

auto lyra_rt_assocarray_assoc_with_default(
    const void* array, const void* stated_default, void* out) -> void* {
  return Emplace(
      out, Read<RuntimeAssociativeArray>(array).WithDefault(stated_default));
}

auto lyra_rt_assocarray_element(
    const void* array, const void* index, const void* index_type) -> const
    void* {
  return Read<RuntimeAssociativeArray>(array).Element(
      lyra::runtime::IndexAt(index, index_type));
}

auto lyra_rt_assocarray_element_mut(
    void* array, const void* index, const void* index_type) -> void* {
  lyra::value::Formation formed{};
  return static_cast<RuntimeAssociativeArray*>(array)->ElementMut(
      lyra::runtime::IndexAt(index, index_type), formed);
}

auto lyra_rt_assocarray_exists(
    const void* array, const void* index, const void* index_type, void* out)
    -> void* {
  return Emplace(
      out, Read<RuntimeAssociativeArray>(array).Exists(
               lyra::runtime::IndexAt(index, index_type)));
}

auto lyra_rt_assocarray_size(const void* array, void* out) -> void* {
  return Emplace(out, Read<RuntimeAssociativeArray>(array).Size());
}

void lyra_rt_assocarray_delete(void* array) {
  static_cast<RuntimeAssociativeArray*>(array)->Delete();
}

void lyra_rt_assocarray_delete_index(
    void* array, const void* index, const void* index_type) {
  static_cast<RuntimeAssociativeArray*>(array)->DeleteIndex(
      lyra::runtime::IndexAt(index, index_type));
}

auto lyra_rt_assocarray_eq(const void* lhs, const void* rhs) -> std::uint8_t {
  return Compared(
      Read<RuntimeAssociativeArray>(lhs) == Read<RuntimeAssociativeArray>(rhs));
}

auto lyra_rt_assocarray_ne(const void* lhs, const void* rhs) -> std::uint8_t {
  return Compared(
      Read<RuntimeAssociativeArray>(lhs) != Read<RuntimeAssociativeArray>(rhs));
}

auto lyra_rt_assocarray_case_equal(const void* lhs, const void* rhs, void* out)
    -> void* {
  return Emplace(
      out, Read<RuntimeAssociativeArray>(lhs).CaseEqual(
               Read<RuntimeAssociativeArray>(rhs)));
}

auto lyra_rt_assocarray_bitstream_width(const void* array, void* out) -> void* {
  return Emplace(out, Read<RuntimeAssociativeArray>(array).BitstreamWidth());
}

auto lyra_rt_assocarray_assoc_min_index(
    const void* array, const void* answer_type, void* out) -> void* {
  return lyra::runtime::IndexInto(
      out, Read<RuntimeAssociativeArray>(array).FirstIndex(), answer_type);
}

auto lyra_rt_assocarray_assoc_max_index(
    const void* array, const void* answer_type, void* out) -> void* {
  return lyra::runtime::IndexInto(
      out, Read<RuntimeAssociativeArray>(array).LastIndex(), answer_type);
}

auto lyra_rt_assocarray_assoc_first(
    const void* array, const void* probe, const void* probe_type,
    const void* answer_type, void* out) -> void* {
  return lyra::runtime::EmplaceVisited(
      out, lyra::runtime::TupleTypeAt(answer_type),
      Read<RuntimeAssociativeArray>(array).FirstIndex(), probe, probe_type);
}

auto lyra_rt_assocarray_assoc_last(
    const void* array, const void* probe, const void* probe_type,
    const void* answer_type, void* out) -> void* {
  return lyra::runtime::EmplaceVisited(
      out, lyra::runtime::TupleTypeAt(answer_type),
      Read<RuntimeAssociativeArray>(array).LastIndex(), probe, probe_type);
}

auto lyra_rt_assocarray_assoc_next(
    const void* array, const void* probe, const void* probe_type,
    const void* answer_type, void* out) -> void* {
  return lyra::runtime::EmplaceVisited(
      out, lyra::runtime::TupleTypeAt(answer_type),
      Read<RuntimeAssociativeArray>(array).NextIndex(
          lyra::runtime::IndexAt(probe, probe_type)),
      probe, probe_type);
}

auto lyra_rt_assocarray_assoc_prev(
    const void* array, const void* probe, const void* probe_type,
    const void* answer_type, void* out) -> void* {
  return lyra::runtime::EmplaceVisited(
      out, lyra::runtime::TupleTypeAt(answer_type),
      Read<RuntimeAssociativeArray>(array).PrevIndex(
          lyra::runtime::IndexAt(probe, probe_type)),
      probe, probe_type);
}

auto lyra_rt_assocarray_count_bits(
    const void* array, const void* control_bits, std::int64_t control_width,
    bool control_is_four_state, void* out) -> void* {
  const lyra::value::LoadedWords control =
      BitsAt(control_bits, control_width, control_is_four_state);
  return Emplace(
      out, Read<RuntimeAssociativeArray>(array).CountBits(control.View()));
}

auto lyra_rt_assocarray_cell_get(void* cell) -> const void* {
  return &VariableAt<RuntimeAssociativeArray>(cell).Get();
}

void lyra_rt_assocarray_cell_initialize(
    void* cell, const void* prototype) noexcept {
  VariableAt<RuntimeAssociativeArray>(cell).Initialize(
      Read<RuntimeAssociativeArray>(prototype));
}

void lyra_rt_assocarray_cell_set(void* cell, const void* value) {
  VariableAt<RuntimeAssociativeArray>(cell).Set(
      Read<RuntimeAssociativeArray>(value));
}

void lyra_rt_assocarray_cell_arm_sampling(void* cell) {
  VariableAt<RuntimeAssociativeArray>(cell).ArmSampling();
}

auto lyra_rt_assocarray_cell_sampled_get(void* cell) -> const void* {
  return &VariableAt<RuntimeAssociativeArray>(cell).SampledGet();
}

auto lyra_rt_assocarray_value_cell_alloc() noexcept -> void* {
  return AllocateValueCell<RuntimeAssociativeArray>();
}

void lyra_rt_assocarray_value_cell_store(
    void* cell, const void* value) noexcept {
  LocalAt<RuntimeAssociativeArray>(cell).Assign(
      Read<RuntimeAssociativeArray>(value));
}

auto lyra_rt_assocarray_value_cell_load(void* cell) noexcept -> void* {
  return &LocalAt<RuntimeAssociativeArray>(cell).Storage();
}

// LRM 7.12.2 ordering methods, which reorder the array where it lies.
void lyra_rt_laidout_sort(
    void* receiver, void* order, const void* receiver_type) {
  lyra::value::RuntimeArraySort(
      ArrayTypeAt(receiver_type), receiver,
      lyra::runtime::BorrowedOrder(order));
}

void lyra_rt_dynarray_sort(void* receiver, void* order) {
  auto& target = *static_cast<RuntimeDynamicArray*>(receiver);
  lyra::value::RuntimeArraySort(target, lyra::runtime::BorrowedOrder(order));
}

void lyra_rt_queue_sort(void* receiver, void* order) {
  auto& target = *static_cast<RuntimeQueue*>(receiver);
  lyra::value::RuntimeArraySort(target, lyra::runtime::BorrowedOrder(order));
}

void lyra_rt_laidout_reverse(void* receiver, const void* receiver_type) {
  lyra::value::RuntimeArrayReverse(ArrayTypeAt(receiver_type), receiver);
}

void lyra_rt_dynarray_reverse(void* receiver) {
  auto& target = *static_cast<RuntimeDynamicArray*>(receiver);
  lyra::value::RuntimeArrayReverse(target);
}

void lyra_rt_queue_reverse(void* receiver) {
  auto& target = *static_cast<RuntimeQueue*>(receiver);
  lyra::value::RuntimeArrayReverse(target);
}

auto lyra_rt_laidout_read_mem(
    void* runtime, const void* memory, const void* name, LyraSpan dims,
    std::int64_t base, std::int64_t start, const void* memory_type,
    const void* answer_type, void* out) -> void* {
  return lyra::runtime::EmplaceCompletion(
      out, lyra::runtime::TupleTypeAt(answer_type),
      lyra::runtime::Single(
          lyra::runtime::ReadUnpackedMemory(
              *static_cast<RuntimeEffects*>(runtime),
              OwnedLaidOutCopy(memory, memory_type), Read<String>(name),
              lyra::value::UnpackedRangesOf(lyra::runtime::MachineIntsOf(dims)),
              base, start, std::nullopt)));
}

auto lyra_rt_laidout_read_mem_within(
    void* runtime, const void* memory, const void* name, LyraSpan dims,
    std::int64_t base, std::int64_t start, std::int64_t finish,
    const void* memory_type, const void* answer_type, void* out) -> void* {
  return lyra::runtime::EmplaceCompletion(
      out, lyra::runtime::TupleTypeAt(answer_type),
      lyra::runtime::Single(
          lyra::runtime::ReadUnpackedMemory(
              *static_cast<RuntimeEffects*>(runtime),
              OwnedLaidOutCopy(memory, memory_type), Read<String>(name),
              lyra::value::UnpackedRangesOf(lyra::runtime::MachineIntsOf(dims)),
              base, start, finish)));
}

void lyra_rt_laidout_write_mem(
    void* runtime, const void* memory, const void* name, LyraSpan dims,
    std::int64_t base, std::int64_t start, const void* memory_type) {
  lyra::runtime::WriteUnpackedMemory(
      *static_cast<RuntimeEffects*>(runtime), ArrayAt(memory, memory_type),
      Read<String>(name),
      lyra::value::UnpackedRangesOf(lyra::runtime::MachineIntsOf(dims)), base,
      start, std::nullopt);
}

void lyra_rt_laidout_write_mem_within(
    void* runtime, const void* memory, const void* name, LyraSpan dims,
    std::int64_t base, std::int64_t start, std::int64_t finish,
    const void* memory_type) {
  lyra::runtime::WriteUnpackedMemory(
      *static_cast<RuntimeEffects*>(runtime), ArrayAt(memory, memory_type),
      Read<String>(name),
      lyra::value::UnpackedRangesOf(lyra::runtime::MachineIntsOf(dims)), base,
      start, finish);
}

auto lyra_rt_dynarray_read_mem(
    void* runtime, const void* memory, const void* name, std::int64_t base,
    std::int64_t start, const void* answer_type, void* out) -> void* {
  return lyra::runtime::EmplaceCompletion(
      out, lyra::runtime::TupleTypeAt(answer_type),
      lyra::runtime::Single(
          lyra::runtime::ReadFlatMemory(
              *static_cast<RuntimeEffects*>(runtime),
              Read<RuntimeDynamicArray>(memory), Read<String>(name), base,
              start, std::nullopt)));
}

auto lyra_rt_dynarray_read_mem_within(
    void* runtime, const void* memory, const void* name, std::int64_t base,
    std::int64_t start, std::int64_t finish, const void* answer_type, void* out)
    -> void* {
  return lyra::runtime::EmplaceCompletion(
      out, lyra::runtime::TupleTypeAt(answer_type),
      lyra::runtime::Single(
          lyra::runtime::ReadFlatMemory(
              *static_cast<RuntimeEffects*>(runtime),
              Read<RuntimeDynamicArray>(memory), Read<String>(name), base,
              start, finish)));
}

void lyra_rt_dynarray_write_mem(
    void* runtime, const void* memory, const void* name, std::int64_t base,
    std::int64_t start) {
  lyra::runtime::WriteFlatMemory(
      *static_cast<RuntimeEffects*>(runtime), Read<RuntimeDynamicArray>(memory),
      Read<String>(name), base, start, std::nullopt);
}

void lyra_rt_dynarray_write_mem_within(
    void* runtime, const void* memory, const void* name, std::int64_t base,
    std::int64_t start, std::int64_t finish) {
  lyra::runtime::WriteFlatMemory(
      *static_cast<RuntimeEffects*>(runtime), Read<RuntimeDynamicArray>(memory),
      Read<String>(name), base, start, finish);
}

auto lyra_rt_queue_read_mem(
    void* runtime, const void* memory, const void* name, std::int64_t base,
    std::int64_t start, const void* answer_type, void* out) -> void* {
  return lyra::runtime::EmplaceCompletion(
      out, lyra::runtime::TupleTypeAt(answer_type),
      lyra::runtime::Single(
          lyra::runtime::ReadFlatMemory(
              *static_cast<RuntimeEffects*>(runtime),
              Read<RuntimeQueue>(memory), Read<String>(name), base, start,
              std::nullopt)));
}

auto lyra_rt_queue_read_mem_within(
    void* runtime, const void* memory, const void* name, std::int64_t base,
    std::int64_t start, std::int64_t finish, const void* answer_type, void* out)
    -> void* {
  return lyra::runtime::EmplaceCompletion(
      out, lyra::runtime::TupleTypeAt(answer_type),
      lyra::runtime::Single(
          lyra::runtime::ReadFlatMemory(
              *static_cast<RuntimeEffects*>(runtime),
              Read<RuntimeQueue>(memory), Read<String>(name), base, start,
              finish)));
}

void lyra_rt_queue_write_mem(
    void* runtime, const void* memory, const void* name, std::int64_t base,
    std::int64_t start) {
  lyra::runtime::WriteFlatMemory(
      *static_cast<RuntimeEffects*>(runtime), Read<RuntimeQueue>(memory),
      Read<String>(name), base, start, std::nullopt);
}

void lyra_rt_queue_write_mem_within(
    void* runtime, const void* memory, const void* name, std::int64_t base,
    std::int64_t start, std::int64_t finish) {
  lyra::runtime::WriteFlatMemory(
      *static_cast<RuntimeEffects*>(runtime), Read<RuntimeQueue>(memory),
      Read<String>(name), base, start, finish);
}

auto lyra_rt_assocarray_read_mem(
    void* runtime, const void* memory, const void* key_type, const void* name,
    std::int64_t base, std::int64_t start, const void* answer_type, void* out)
    -> void* {
  return lyra::runtime::EmplaceCompletion(
      out, lyra::runtime::TupleTypeAt(answer_type),
      lyra::runtime::Single(
          lyra::runtime::ReadKeyedMemory(
              *static_cast<RuntimeEffects*>(runtime),
              Read<RuntimeAssociativeArray>(memory), Read<String>(name), base,
              start, std::nullopt, key_type)));
}

auto lyra_rt_assocarray_read_mem_within(
    void* runtime, const void* memory, const void* key_type, const void* name,
    std::int64_t base, std::int64_t start, std::int64_t finish,
    const void* answer_type, void* out) -> void* {
  return lyra::runtime::EmplaceCompletion(
      out, lyra::runtime::TupleTypeAt(answer_type),
      lyra::runtime::Single(
          lyra::runtime::ReadKeyedMemory(
              *static_cast<RuntimeEffects*>(runtime),
              Read<RuntimeAssociativeArray>(memory), Read<String>(name), base,
              start, finish, key_type)));
}

void lyra_rt_assocarray_write_mem(
    void* runtime, const void* memory, const void* name, std::int64_t base,
    std::int64_t start) {
  lyra::runtime::WriteKeyedMemory(
      *static_cast<RuntimeEffects*>(runtime),
      Read<RuntimeAssociativeArray>(memory), Read<String>(name), base, start,
      std::nullopt);
}

void lyra_rt_assocarray_write_mem_within(
    void* runtime, const void* memory, const void* name, std::int64_t base,
    std::int64_t start, std::int64_t finish) {
  lyra::runtime::WriteKeyedMemory(
      *static_cast<RuntimeEffects*>(runtime),
      Read<RuntimeAssociativeArray>(memory), Read<String>(name), base, start,
      finish);
}

auto lyra_rt_format_runtime(
    const void* format, LyraSpan args, const void* scope_path,
    const void* library_binding, const void* time_format,
    std::int64_t timeunit_power, void* out) -> void* {
  const std::span<const void* const> handles{
      static_cast<const void* const*>(args.data), args.count};
  std::vector<FormatArg> arguments(handles.size());
  std::ranges::transform(handles, arguments.begin(), [](const void* handle) {
    return *static_cast<const FormatArg*>(handle);
  });
  return Emplace(
      out, lyra::value::FormatRuntime(
               Read<String>(format), arguments, Read<String>(scope_path),
               Read<String>(library_binding), Read<TimeFormat>(time_format),
               timeunit_power));
}

auto lyra_rt_integral_make_format_arg(
    const void* value, std::int64_t value_width, bool value_is_signed,
    bool value_is_four_state, void* out) -> void* {
  return Emplace(
      out, lyra::runtime::IntegralFormatArg(
               value, lyra::runtime::NumberShape(
                          value_width, value_is_signed, value_is_four_state)));
}

auto lyra_rt_string_make_format_arg(const void* value, void* out) -> void* {
  return Emplace(out, MakeFormatArg(Read<String>(value)));
}

auto lyra_rt_make_patterned_format_arg(
    const void* value, std::int64_t value_width, bool value_is_signed,
    bool value_is_four_state, const void* pattern, void* out) -> void* {
  FormatArg arg = lyra::runtime::IntegralFormatArg(
      value, lyra::runtime::NumberShape(
                 value_width, value_is_signed, value_is_four_state));
  arg.pattern = &Read<String>(pattern);
  return Emplace(out, arg);
}

auto lyra_rt_make_rendered_format_arg(const void* pattern, void* out) -> void* {
  return Emplace(out, FormatArg::Rendered(Read<String>(pattern)));
}

auto lyra_rt_chandle_make_format_arg(const void* value, void* out) -> void* {
  return Emplace(out, MakeFormatArg(Read<Chandle>(value)));
}

auto lyra_rt_managedref_make_format_arg(const void* value, void* out) -> void* {
  return Emplace(out, MakeFormatArg(Read<ObjectRef>(value)));
}

auto lyra_rt_make_dpi_bit_buffer(
    const void* sv, std::int64_t sv_width, bool sv_is_four_state, void* out)
    -> void* {
  const lyra::value::LoadedWords words = BitsAt(sv, sv_width, sv_is_four_state);
  return Emplace(out, DpiBitBuffer(words.View()));
}

auto lyra_rt_make_dpi_logic_buffer(
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
auto lyra_rt_make_dpi_open_array(
    const void* sv, const void* sv_type, LyraSpan bounds,
    bool addressable_elements, void* out) -> void* {
  const std::span<const std::int64_t> declared =
      lyra::runtime::MachineIntsOf(bounds);
  const std::vector<UnpackedRange> dims =
      lyra::value::UnpackedRangesOf(declared);
  const lyra::value::ValueType& type = lyra::runtime::TypeAt(sv_type);
  const lyra::value::IntegralShape element =
      lyra::runtime::ImageElementType(sv, type, dims.size()).Shape();
  auto* image = std::construct_at(
      static_cast<DpiOpenArray*>(out), declared, element.width, element.domain,
      addressable_elements);
  std::size_t position = 0;
  lyra::runtime::FillImage(*image, sv, type, 0, dims.size(), position);
  return image;
}

auto lyra_rt_dpi_open_array_handle(void* image) -> void* {
  return static_cast<DpiOpenArray*>(image)->Handle();
}

auto lyra_rt_dpi_open_array_value(
    const void* image, const void* answer_type, void* out) -> void* {
  const lyra::value::ValueType& type = lyra::runtime::TypeAt(answer_type);
  const auto& read = *static_cast<const DpiOpenArray*>(image);
  const auto dimensions = static_cast<std::size_t>(read.Dimensions());
  lyra::runtime::BuildImageShape(type, out, dimensions);
  std::size_t position = 0;
  lyra::runtime::ReadImageBack(read, out, type, 0, dimensions, position);
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
auto lyra_rt_real_cell_open_for_write(void* cell, void* out) -> void* {
  return OpenCellWrite<Real>(cell, out);
}
auto lyra_rt_shortreal_cell_open_for_write(void* cell, void* out) -> void* {
  return OpenCellWrite<ShortReal>(cell, out);
}
auto lyra_rt_chandle_cell_open_for_write(void* cell, void* out) -> void* {
  return OpenCellWrite<Chandle>(cell, out);
}
auto lyra_rt_managedref_cell_open_for_write(void* cell, void* out) -> void* {
  return OpenCellWrite<ObjectRef>(cell, out);
}
auto lyra_rt_laidout_cell_open_for_write(void* cell, void* out) -> void* {
  return OpenTailCellWrite<LaidOutValue>(cell, out);
}

auto lyra_rt_dynarray_cell_open_for_write(void* cell, void* out) -> void* {
  return OpenCellWrite<RuntimeDynamicArray>(cell, out);
}
auto lyra_rt_queue_cell_open_for_write(void* cell, void* out) -> void* {
  return OpenCellWrite<RuntimeQueue>(cell, out);
}
auto lyra_rt_assocarray_cell_open_for_write(void* cell, void* out) -> void* {
  return OpenCellWrite<RuntimeAssociativeArray>(cell, out);
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
auto lyra_rt_chandle_ref_open_for_write(void* reference, void* out) -> void* {
  return OpenRefWrite<Chandle>(reference, out);
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

auto lyra_rt_dynarray_ref_open_for_write(void* reference, void* out) -> void* {
  return OpenRefWrite<RuntimeDynamicArray>(reference, out);
}
auto lyra_rt_queue_ref_open_for_write(void* reference, void* out) -> void* {
  return OpenRefWrite<RuntimeQueue>(reference, out);
}
auto lyra_rt_assocarray_ref_open_for_write(void* reference, void* out)
    -> void* {
  return OpenRefWrite<RuntimeAssociativeArray>(reference, out);
}
auto lyra_rt_laidout_driver_open_for_write(void* driver, void* out) -> void* {
  return OpenTailDriverWrite<LaidOutValue>(driver, out);
}

void lyra_rt_logic8_driver_drive_from_variable(void* driver, void* cell) {
  DriveFromVariable<Logic8>(driver, cell);
}
void lyra_rt_logic16_driver_drive_from_variable(void* driver, void* cell) {
  DriveFromVariable<Logic16>(driver, cell);
}
void lyra_rt_logic32_driver_drive_from_variable(void* driver, void* cell) {
  DriveFromVariable<Logic32>(driver, cell);
}
void lyra_rt_logic64_driver_drive_from_variable(void* driver, void* cell) {
  DriveFromVariable<Logic64>(driver, cell);
}
void lyra_rt_logic_wide_driver_drive_from_variable(void* driver, void* cell) {
  DriveFromVariable<LogicWide>(driver, cell);
}
void lyra_rt_laidout_driver_drive_from_variable(void* driver, void* cell) {
  DriveFromVariable<LaidOutValue>(driver, cell);
}

void lyra_rt_logic8_driver_drive_from_net(void* driver, void* net) {
  DriveFromNet<Logic8>(driver, net);
}
void lyra_rt_logic16_driver_drive_from_net(void* driver, void* net) {
  DriveFromNet<Logic16>(driver, net);
}
void lyra_rt_logic32_driver_drive_from_net(void* driver, void* net) {
  DriveFromNet<Logic32>(driver, net);
}
void lyra_rt_logic64_driver_drive_from_net(void* driver, void* net) {
  DriveFromNet<Logic64>(driver, net);
}
void lyra_rt_logic_wide_driver_drive_from_net(void* driver, void* net) {
  DriveFromNet<LogicWide>(driver, net);
}
void lyra_rt_laidout_driver_drive_from_net(void* driver, void* net) {
  DriveFromNet<LaidOutValue>(driver, net);
}

void lyra_rt_logic8_driver_drive_from_reference(void* driver, void* reference) {
  DriveFromReference<Logic8>(driver, reference);
}
void lyra_rt_logic16_driver_drive_from_reference(
    void* driver, void* reference) {
  DriveFromReference<Logic16>(driver, reference);
}
void lyra_rt_logic32_driver_drive_from_reference(
    void* driver, void* reference) {
  DriveFromReference<Logic32>(driver, reference);
}
void lyra_rt_logic64_driver_drive_from_reference(
    void* driver, void* reference) {
  DriveFromReference<Logic64>(driver, reference);
}
void lyra_rt_logic_wide_driver_drive_from_reference(
    void* driver, void* reference) {
  DriveFromReference<LogicWide>(driver, reference);
}
void lyra_rt_laidout_driver_drive_from_reference(
    void* driver, void* reference) {
  DriveFromReference<LaidOutValue>(driver, reference);
}

auto lyra_rt_logic8_net_reports_to(void* net) -> void* {
  return NetReportsTo<Logic8>(net);
}
auto lyra_rt_logic16_net_reports_to(void* net) -> void* {
  return NetReportsTo<Logic16>(net);
}
auto lyra_rt_logic32_net_reports_to(void* net) -> void* {
  return NetReportsTo<Logic32>(net);
}
auto lyra_rt_logic64_net_reports_to(void* net) -> void* {
  return NetReportsTo<Logic64>(net);
}
auto lyra_rt_logic_wide_net_reports_to(void* net) -> void* {
  return NetReportsTo<LogicWide>(net);
}
auto lyra_rt_laidout_net_reports_to(void* net) -> void* {
  return NetReportsTo<LaidOutValue>(net);
}

auto lyra_rt_logic8_driver_show_source(void* driver) -> bool {
  return ShowSource<Logic8>(driver);
}
auto lyra_rt_logic16_driver_show_source(void* driver) -> bool {
  return ShowSource<Logic16>(driver);
}
auto lyra_rt_logic32_driver_show_source(void* driver) -> bool {
  return ShowSource<Logic32>(driver);
}
auto lyra_rt_logic64_driver_show_source(void* driver) -> bool {
  return ShowSource<Logic64>(driver);
}
auto lyra_rt_logic_wide_driver_show_source(void* driver) -> bool {
  return ShowSource<LogicWide>(driver);
}
auto lyra_rt_laidout_driver_show_source(void* driver) -> bool {
  return ShowSource<LaidOutValue>(driver);
}

auto lyra_rt_designate_whole(void* write, void* out) -> void* {
  return std::construct_at(
      static_cast<ErasedDesignation*>(out),
      static_cast<OpenWrite*>(write)->Whole());
}

auto lyra_rt_dynarray_designate_element(
    const void* designation, const void* position, void* out) -> void* {
  return DesignateElement<RuntimeDynamicArray>(
      designation, PositionNamedAt(position), out);
}

auto lyra_rt_laidout_designate_element(
    const void* designation, const void* position, const void* array_type,
    void* out) -> void* {
  return DesignateArrayElement(
      designation, ArrayTypeAt(array_type), PositionNamedAt(position), out);
}

auto lyra_rt_queue_designate_element(
    const void* designation, const void* position, void* out) -> void* {
  return DesignateElement<RuntimeQueue>(
      designation, PositionNamedAt(position), out);
}

auto lyra_rt_assocarray_designate_element(
    const void* designation, const void* index, const void* index_type,
    void* out) -> void* {
  return DesignateElement<RuntimeAssociativeArray>(
      designation, lyra::runtime::IndexAt(index, index_type), out);
}

auto lyra_rt_laidout_designate_component(
    const void* designation, std::int64_t index, const void* tuple_type,
    void* out) -> void* {
  const ErasedDesignation& within = DesignationAt(designation);
  if (within.write->Undecided()) {
    within.write->Stepped(
        lyra::runtime::PartPath::Child(static_cast<std::uint64_t>(index)));
  }
  return std::construct_at(
      static_cast<ErasedDesignation*>(out),
      ErasedDesignation{
          .write = within.write,
          .part =
              lyra::runtime::TupleTypeAt(tuple_type)
                  .ComponentAt(within.part, static_cast<std::size_t>(index))});
}

auto lyra_rt_laidout_designate_member(const void* designation, void* out)
    -> void* {
  const ErasedDesignation& within = DesignationAt(designation);
  if (within.write->Undecided()) {
    within.write->Stepped(lyra::runtime::PartPath::Inexact());
  }
  return std::construct_at(static_cast<ErasedDesignation*>(out), within);
}

void lyra_rt_dynarray_assign_slice(
    const void* designation, const void* start, std::int64_t count,
    const void* replacement, const void* replacement_type) {
  AssignDesignatedSlice<RuntimeDynamicArray>(
      designation, PositionNamedAt(start), count,
      lyra::runtime::ElementHandles(replacement, replacement_type));
}

void lyra_rt_laidout_assign_slice(
    const void* designation, const void* start, std::int64_t count,
    const void* replacement, const void* replacement_type,
    const void* array_type) {
  AssignDesignatedArraySlice(
      designation, ArrayTypeAt(array_type), PositionNamedAt(start), count,
      lyra::runtime::ElementHandles(replacement, replacement_type));
}

auto lyra_rt_string_land(const void* designation) noexcept -> void* {
  return LandDesignation<String>(designation);
}
auto lyra_rt_real_land(const void* designation) noexcept -> void* {
  return LandDesignation<Real>(designation);
}
auto lyra_rt_shortreal_land(const void* designation) noexcept -> void* {
  return LandDesignation<ShortReal>(designation);
}
auto lyra_rt_chandle_land(const void* designation) noexcept -> void* {
  return LandDesignation<Chandle>(designation);
}
auto lyra_rt_empty_land(const void* designation) noexcept -> void* {
  return LandDesignation<Empty>(designation);
}
auto lyra_rt_laidout_land(
    const void* designation, const void* value_type) noexcept -> void* {
  return LandLaidOutDesignation(designation, lyra::runtime::TypeAt(value_type));
}

auto lyra_rt_dynarray_land(const void* designation) noexcept -> void* {
  return LandDesignation<RuntimeDynamicArray>(designation);
}
auto lyra_rt_queue_land(const void* designation) noexcept -> void* {
  return LandDesignation<RuntimeQueue>(designation);
}
auto lyra_rt_assocarray_land(const void* designation) noexcept -> void* {
  return LandDesignation<RuntimeAssociativeArray>(designation);
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
void lyra_rt_real_assign(void* storage, const void* value) {
  *static_cast<Real*>(storage) = Read<Real>(value);
}
void lyra_rt_shortreal_assign(void* storage, const void* value) {
  *static_cast<ShortReal*>(storage) = Read<ShortReal>(value);
}
void lyra_rt_chandle_assign(void* storage, const void* value) {
  *static_cast<Chandle*>(storage) = Read<Chandle>(value);
}
void lyra_rt_empty_assign(void* storage, const void* value) {
  *static_cast<lyra::value::Empty*>(storage) = Read<lyra::value::Empty>(value);
}
void lyra_rt_dynarray_assign(void* storage, const void* value) {
  *static_cast<RuntimeDynamicArray*>(storage) =
      Read<RuntimeDynamicArray>(value);
}
void lyra_rt_queue_assign(void* storage, const void* value) {
  *static_cast<RuntimeQueue*>(storage) = Read<RuntimeQueue>(value);
}
void lyra_rt_assocarray_assign(void* storage, const void* value) {
  *static_cast<RuntimeAssociativeArray*>(storage) =
      Read<RuntimeAssociativeArray>(value);
}
void lyra_rt_managedref_assign(void* storage, const void* value) {
  *static_cast<ObjectRef*>(storage) = Read<ObjectRef>(value);
}
// Binding a reference-typed place -- a `ref` port's own name (LRM 23.3.3.2) --
// replaces the reference it holds, never what it names.
void lyra_rt_reference_assign(void* storage, const void* value) {
  *static_cast<ErasedReference*>(storage) = Read<ErasedReference>(value);
}
void lyra_rt_wait_assign(void* storage, void* value) {
  *static_cast<Wait*>(storage) = std::move(*static_cast<Wait*>(value));
}

// A fixed-size unpacked array is laid out by the body that holds it as its
// elements alone, so each whole-value operation on one is its type's, which
// crosses after the array.

auto lyra_rt_element_default(const void* element_type) -> const void* {
  return lyra::runtime::ElementTypeAt(element_type).Default();
}
auto lyra_rt_discarded_write(const void* element_type) -> void* {
  return lyra::runtime::ElementTypeAt(element_type).DiscardedWrite();
}

// Ending an object the generated body held in its own storage, where ending one
// has something to do. An object whose type has a trivial destructor is ended
// by its storage going away, so it has no entry here.
void lyra_rt_string_destroy(void* object) {
  std::destroy_at(static_cast<String*>(object));
}
void lyra_rt_dynarray_destroy(void* object) {
  std::destroy_at(static_cast<RuntimeDynamicArray*>(object));
}
void lyra_rt_queue_destroy(void* object) {
  std::destroy_at(static_cast<RuntimeQueue*>(object));
}
void lyra_rt_assocarray_destroy(void* object) {
  std::destroy_at(static_cast<RuntimeAssociativeArray*>(object));
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
void lyra_rt_channel_cancellation_destroy(void* object) {
  std::destroy_at(static_cast<ChannelCancellation*>(object));
}
void lyra_rt_shared_pointer_destroy(void* object) {
  std::destroy_at(static_cast<SharedPointer*>(object));
}
void lyra_rt_open_write_destroy(void* object) {
  std::destroy_at(static_cast<OpenWrite*>(object));
}
void lyra_rt_object_write_destroy(void* object) {
  std::destroy_at(static_cast<ErasedObjectWrite*>(object));
}

// A second value equal to one the body already holds, built in further storage
// the body gave -- where a value it only reads has to become one it owns.
auto lyra_rt_string_copy(const void* value, void* out) -> void* {
  return Emplace(out, Read<String>(value));
}
auto lyra_rt_real_copy(const void* value, void* out) -> void* {
  return Emplace(out, Read<Real>(value));
}
auto lyra_rt_shortreal_copy(const void* value, void* out) -> void* {
  return Emplace(out, Read<ShortReal>(value));
}
auto lyra_rt_chandle_copy(const void* value, void* out) -> void* {
  return Emplace(out, Read<Chandle>(value));
}
auto lyra_rt_empty_copy(const void* value, void* out) -> void* {
  return Emplace(out, Read<lyra::value::Empty>(value));
}
auto lyra_rt_dynarray_copy(const void* value, void* out) -> void* {
  return Emplace(out, Read<RuntimeDynamicArray>(value));
}
auto lyra_rt_queue_copy(const void* value, void* out) -> void* {
  return Emplace(out, Read<RuntimeQueue>(value));
}
auto lyra_rt_assocarray_copy(const void* value, void* out) -> void* {
  return Emplace(out, Read<RuntimeAssociativeArray>(value));
}
auto lyra_rt_managedref_copy(const void* value, void* out) -> void* {
  return Emplace(out, Read<ObjectRef>(value));
}
auto lyra_rt_shared_pointer_copy(const void* value, void* out) -> void* {
  return Emplace(out, Read<SharedPointer>(value));
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
auto lyra_rt_reference_copy(const void* value, void* out) -> void* {
  return Emplace(out, Read<ErasedReference>(value));
}

// A value moved into storage that takes it over. What is left behind is still
// an object, which the body then ends.
auto lyra_rt_string_move(void* value, void* out) -> void* {
  return Emplace(out, std::move(*static_cast<String*>(value)));
}
auto lyra_rt_real_move(void* value, void* out) -> void* {
  return Emplace(out, *static_cast<Real*>(value));
}
auto lyra_rt_shortreal_move(void* value, void* out) -> void* {
  return Emplace(out, *static_cast<ShortReal*>(value));
}
auto lyra_rt_chandle_move(void* value, void* out) -> void* {
  return Emplace(out, *static_cast<Chandle*>(value));
}
auto lyra_rt_empty_move(void* value, void* out) -> void* {
  return Emplace(out, *static_cast<lyra::value::Empty*>(value));
}
auto lyra_rt_dynarray_move(void* value, void* out) -> void* {
  return Emplace(out, std::move(*static_cast<RuntimeDynamicArray*>(value)));
}
auto lyra_rt_queue_move(void* value, void* out) -> void* {
  return Emplace(out, std::move(*static_cast<RuntimeQueue*>(value)));
}
auto lyra_rt_assocarray_move(void* value, void* out) -> void* {
  return Emplace(out, std::move(*static_cast<RuntimeAssociativeArray*>(value)));
}
auto lyra_rt_managedref_move(void* value, void* out) -> void* {
  return Emplace(out, std::move(*static_cast<ObjectRef*>(value)));
}
auto lyra_rt_shared_pointer_move(void* value, void* out) -> void* {
  return Emplace(out, std::move(*static_cast<SharedPointer*>(value)));
}
auto lyra_rt_print_item_move(void* value, void* out) -> void* {
  return Emplace(out, std::move(*static_cast<PrintItem*>(value)));
}
auto lyra_rt_format_spec_move(void* value, void* out) -> void* {
  return Emplace(out, std::move(*static_cast<FormatSpec*>(value)));
}
auto lyra_rt_format_arg_move(void* value, void* out) -> void* {
  return Emplace(out, std::move(*static_cast<FormatArg*>(value)));
}
auto lyra_rt_hierarchy_segment_move(void* value, void* out) -> void* {
  return Emplace(out, std::move(*static_cast<HierarchySegment*>(value)));
}
auto lyra_rt_trigger_move(void* value, void* out) -> void* {
  return Emplace(out, std::move(*static_cast<Trigger*>(value)));
}
auto lyra_rt_observation_move(void* value, void* out) -> void* {
  return Emplace(out, std::move(*static_cast<Observation*>(value)));
}
auto lyra_rt_read_report_move(void* value, void* out) -> void* {
  return Emplace(out, std::move(*static_cast<ReadReport*>(value)));
}
auto lyra_rt_wait_move(void* value, void* out) -> void* {
  return Emplace(out, std::move(*static_cast<Wait*>(value)));
}
auto lyra_rt_dpi_bit_buffer_move(void* value, void* out) -> void* {
  return Emplace(out, std::move(*static_cast<DpiBitBuffer*>(value)));
}
auto lyra_rt_dpi_logic_buffer_move(void* value, void* out) -> void* {
  return Emplace(out, std::move(*static_cast<DpiLogicBuffer*>(value)));
}
auto lyra_rt_dpi_open_array_move(void* value, void* out) -> void* {
  return Emplace(out, std::move(*static_cast<DpiOpenArray*>(value)));
}
auto lyra_rt_channel_cancellation_move(void* value, void* out) -> void* {
  return Emplace(out, std::move(*static_cast<ChannelCancellation*>(value)));
}
auto lyra_rt_reference_move(void* value, void* out) -> void* {
  return Emplace(out, Read<ErasedReference>(value));
}

void lyra_rt_borrowed_handle_construct(void* storage) {
  BuildAt<void*>(storage);
}
void lyra_rt_reference_construct(void* storage) {
  BuildAt<ErasedReference>(storage);
}
void lyra_rt_string_cell_construct(void* storage) {
  BuildAt<Var<String>>(storage);
}
void lyra_rt_real_cell_construct(void* storage) {
  BuildAt<Var<Real>>(storage);
}
void lyra_rt_shortreal_cell_construct(void* storage) {
  BuildAt<Var<ShortReal>>(storage);
}
void lyra_rt_chandle_cell_construct(void* storage) {
  BuildAt<Var<Chandle>>(storage);
}
void lyra_rt_laidout_cell_construct(const void* held_type, void* storage) {
  BuildAt<VariableOf<LaidOutValue>::Type>(
      storage, lyra::runtime::TypeAt(held_type));
}

void lyra_rt_dynarray_cell_construct(void* storage) {
  BuildAt<Var<RuntimeDynamicArray>>(storage);
}
void lyra_rt_queue_cell_construct(void* storage) {
  BuildAt<Var<RuntimeQueue>>(storage);
}
void lyra_rt_assocarray_cell_construct(void* storage) {
  BuildAt<Var<RuntimeAssociativeArray>>(storage);
}
void lyra_rt_managedref_cell_construct(void* storage) {
  BuildAt<Var<ObjectRef>>(storage);
}
void lyra_rt_string_value_cell_construct(void* storage) {
  BuildAt<ValueCell<String>>(storage);
}
void lyra_rt_real_value_cell_construct(void* storage) {
  BuildAt<ValueCell<Real>>(storage);
}
void lyra_rt_shortreal_value_cell_construct(void* storage) {
  BuildAt<ValueCell<ShortReal>>(storage);
}
void lyra_rt_chandle_value_cell_construct(void* storage) {
  BuildAt<ValueCell<Chandle>>(storage);
}
void lyra_rt_laidout_value_cell_construct(
    const void* held_type, void* storage) {
  BuildAt<LocalOf<LaidOutValue>::Type>(
      storage, lyra::runtime::TypeAt(held_type));
}

void lyra_rt_dynarray_value_cell_construct(void* storage) {
  BuildAt<ValueCell<RuntimeDynamicArray>>(storage);
}
void lyra_rt_queue_value_cell_construct(void* storage) {
  BuildAt<ValueCell<RuntimeQueue>>(storage);
}
void lyra_rt_assocarray_value_cell_construct(void* storage) {
  BuildAt<ValueCell<RuntimeAssociativeArray>>(storage);
}
void lyra_rt_managedref_value_cell_construct(void* storage) {
  BuildAt<ValueCell<ObjectRef>>(storage);
}
void lyra_rt_laidout_net_construct(const void* held_type, void* storage) {
  BuildAt<ResolvedNet<LaidOutValue>>(storage, lyra::runtime::TypeAt(held_type));
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

void lyra_rt_dynarray_sampled_history_construct(void* storage) {
  BuildAt<SampledHistory<RuntimeDynamicArray>>(storage);
}
void lyra_rt_queue_sampled_history_construct(void* storage) {
  BuildAt<SampledHistory<RuntimeQueue>>(storage);
}
void lyra_rt_assocarray_sampled_history_construct(void* storage) {
  BuildAt<SampledHistory<RuntimeAssociativeArray>>(storage);
}
void lyra_rt_managedref_sampled_history_construct(void* storage) {
  BuildAt<SampledHistory<ObjectRef>>(storage);
}
void lyra_rt_named_event_construct(void* storage) {
  BuildAt<NamedEvent>(storage);
}
void lyra_rt_cancellation_target_construct(void* storage) {
  BuildAt<CancellationTarget>(storage);
}
void lyra_rt_evaluation_attempts_construct(void* storage) {
  BuildAt<EvaluationAttempts>(storage);
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
void lyra_rt_shared_pointer_construct(void* storage) {
  BuildAt<SharedPointer>(storage);
}

void lyra_rt_string_cell_destroy(void* storage) {
  std::destroy_at(&VariableAt<String>(storage));
}
void lyra_rt_real_cell_destroy(void* storage) {
  std::destroy_at(&VariableAt<Real>(storage));
}
void lyra_rt_shortreal_cell_destroy(void* storage) {
  std::destroy_at(&VariableAt<ShortReal>(storage));
}
void lyra_rt_chandle_cell_destroy(void* storage) {
  std::destroy_at(&VariableAt<Chandle>(storage));
}
void lyra_rt_laidout_cell_destroy(void* storage) {
  std::destroy_at(&VariableAt<LaidOutValue>(storage));
}

void lyra_rt_dynarray_cell_destroy(void* storage) {
  std::destroy_at(&VariableAt<RuntimeDynamicArray>(storage));
}
void lyra_rt_queue_cell_destroy(void* storage) {
  std::destroy_at(&VariableAt<RuntimeQueue>(storage));
}
void lyra_rt_assocarray_cell_destroy(void* storage) {
  std::destroy_at(&VariableAt<RuntimeAssociativeArray>(storage));
}
void lyra_rt_managedref_cell_destroy(void* storage) {
  std::destroy_at(&VariableAt<ObjectRef>(storage));
}
void lyra_rt_string_value_cell_destroy(void* storage) {
  std::destroy_at(&LocalAt<String>(storage));
}
void lyra_rt_laidout_value_cell_destroy(void* storage) {
  std::destroy_at(static_cast<LocalOf<LaidOutValue>::Type*>(storage));
}

void lyra_rt_dynarray_value_cell_destroy(void* storage) {
  std::destroy_at(&LocalAt<RuntimeDynamicArray>(storage));
}
void lyra_rt_queue_value_cell_destroy(void* storage) {
  std::destroy_at(&LocalAt<RuntimeQueue>(storage));
}
void lyra_rt_assocarray_value_cell_destroy(void* storage) {
  std::destroy_at(&LocalAt<RuntimeAssociativeArray>(storage));
}
void lyra_rt_managedref_value_cell_destroy(void* storage) {
  std::destroy_at(&LocalAt<ObjectRef>(storage));
}
void lyra_rt_laidout_net_destroy(void* storage) {
  std::destroy_at(static_cast<ResolvedNet<LaidOutValue>*>(storage));
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

void lyra_rt_dynarray_sampled_history_destroy(void* storage) {
  std::destroy_at(static_cast<SampledHistory<RuntimeDynamicArray>*>(storage));
}
void lyra_rt_queue_sampled_history_destroy(void* storage) {
  std::destroy_at(static_cast<SampledHistory<RuntimeQueue>*>(storage));
}
void lyra_rt_assocarray_sampled_history_destroy(void* storage) {
  std::destroy_at(
      static_cast<SampledHistory<RuntimeAssociativeArray>*>(storage));
}
void lyra_rt_managedref_sampled_history_destroy(void* storage) {
  std::destroy_at(static_cast<SampledHistory<ObjectRef>*>(storage));
}
void lyra_rt_named_event_destroy(void* storage) {
  std::destroy_at(static_cast<NamedEvent*>(storage));
}
void lyra_rt_cancellation_target_destroy(void* storage) {
  std::destroy_at(static_cast<CancellationTarget*>(storage));
}
void lyra_rt_evaluation_attempts_destroy(void* storage) {
  std::destroy_at(static_cast<EvaluationAttempts*>(storage));
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
auto lyra_rt_bit8_cell_sampled_get(void* cell) -> const void* {
  return HandleTo(VariableAt<Bit8>(cell).SampledGet());
}
auto lyra_rt_bit8_cell_begin_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count)
    -> std::int64_t {
  return CellBeginTakeover<Bit8>(cell, level, first, count);
}
auto lyra_rt_bit8_cell_drive_takeover(
    void* cell, std::int64_t level, std::int64_t generation, const void* value)
    -> bool {
  return CellDriveTakeover<Bit8>(cell, level, generation, value);
}
void lyra_rt_bit8_cell_end_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count) {
  CellEndTakeover<Bit8>(cell, level, first, count);
}
auto lyra_rt_bit8_ref_begin_takeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count)
    -> std::int64_t {
  return RefBeginTakeover<Bit8>(reference, level, first, count);
}
auto lyra_rt_bit8_ref_drive_takeover(
    void* reference, std::int64_t level, std::int64_t generation,
    const void* value) -> bool {
  return RefDriveTakeover<Bit8>(reference, level, generation, value);
}
void lyra_rt_bit8_ref_end_takeover(
    void* reference, std::int64_t level, std::int64_t first,
    std::int64_t count) {
  RefEndTakeover<Bit8>(reference, level, first, count);
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
auto lyra_rt_bit8_ref_sampled_get(void* reference) -> const void* {
  return RefSampledGet<Bit8>(reference);
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
    const void* history, std::int64_t ticks_back, void* out) -> void* {
  return Emplace(
      out, static_cast<const SampledHistory<Bit8>*>(history)->At(ticks_back));
}
auto lyra_rt_bit8_land(const void* designation) noexcept -> void* {
  return LandDesignation<Bit8>(designation);
}
void lyra_rt_bit8_insert_designated_bits(
    const void* designation, std::int64_t width, LyraSpan outer_parts,
    const void* start, const void* bits, std::int64_t bits_width,
    bool bits_are_four_state) {
  InsertDesignatedBits<Bit8>(
      designation, width, outer_parts, start, bits, bits_width,
      bits_are_four_state);
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
auto lyra_rt_bit16_cell_sampled_get(void* cell) -> const void* {
  return HandleTo(VariableAt<Bit16>(cell).SampledGet());
}
auto lyra_rt_bit16_cell_begin_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count)
    -> std::int64_t {
  return CellBeginTakeover<Bit16>(cell, level, first, count);
}
auto lyra_rt_bit16_cell_drive_takeover(
    void* cell, std::int64_t level, std::int64_t generation, const void* value)
    -> bool {
  return CellDriveTakeover<Bit16>(cell, level, generation, value);
}
void lyra_rt_bit16_cell_end_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count) {
  CellEndTakeover<Bit16>(cell, level, first, count);
}
auto lyra_rt_bit16_ref_begin_takeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count)
    -> std::int64_t {
  return RefBeginTakeover<Bit16>(reference, level, first, count);
}
auto lyra_rt_bit16_ref_drive_takeover(
    void* reference, std::int64_t level, std::int64_t generation,
    const void* value) -> bool {
  return RefDriveTakeover<Bit16>(reference, level, generation, value);
}
void lyra_rt_bit16_ref_end_takeover(
    void* reference, std::int64_t level, std::int64_t first,
    std::int64_t count) {
  RefEndTakeover<Bit16>(reference, level, first, count);
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
auto lyra_rt_bit16_ref_sampled_get(void* reference) -> const void* {
  return RefSampledGet<Bit16>(reference);
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
    const void* history, std::int64_t ticks_back, void* out) -> void* {
  return Emplace(
      out, static_cast<const SampledHistory<Bit16>*>(history)->At(ticks_back));
}
auto lyra_rt_bit16_land(const void* designation) noexcept -> void* {
  return LandDesignation<Bit16>(designation);
}
void lyra_rt_bit16_insert_designated_bits(
    const void* designation, std::int64_t width, LyraSpan outer_parts,
    const void* start, const void* bits, std::int64_t bits_width,
    bool bits_are_four_state) {
  InsertDesignatedBits<Bit16>(
      designation, width, outer_parts, start, bits, bits_width,
      bits_are_four_state);
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
auto lyra_rt_bit32_cell_sampled_get(void* cell) -> const void* {
  return HandleTo(VariableAt<Bit32>(cell).SampledGet());
}
auto lyra_rt_bit32_cell_begin_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count)
    -> std::int64_t {
  return CellBeginTakeover<Bit32>(cell, level, first, count);
}
auto lyra_rt_bit32_cell_drive_takeover(
    void* cell, std::int64_t level, std::int64_t generation, const void* value)
    -> bool {
  return CellDriveTakeover<Bit32>(cell, level, generation, value);
}
void lyra_rt_bit32_cell_end_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count) {
  CellEndTakeover<Bit32>(cell, level, first, count);
}
auto lyra_rt_bit32_ref_begin_takeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count)
    -> std::int64_t {
  return RefBeginTakeover<Bit32>(reference, level, first, count);
}
auto lyra_rt_bit32_ref_drive_takeover(
    void* reference, std::int64_t level, std::int64_t generation,
    const void* value) -> bool {
  return RefDriveTakeover<Bit32>(reference, level, generation, value);
}
void lyra_rt_bit32_ref_end_takeover(
    void* reference, std::int64_t level, std::int64_t first,
    std::int64_t count) {
  RefEndTakeover<Bit32>(reference, level, first, count);
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
auto lyra_rt_bit32_ref_sampled_get(void* reference) -> const void* {
  return RefSampledGet<Bit32>(reference);
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
    const void* history, std::int64_t ticks_back, void* out) -> void* {
  return Emplace(
      out, static_cast<const SampledHistory<Bit32>*>(history)->At(ticks_back));
}
auto lyra_rt_bit32_land(const void* designation) noexcept -> void* {
  return LandDesignation<Bit32>(designation);
}
void lyra_rt_bit32_insert_designated_bits(
    const void* designation, std::int64_t width, LyraSpan outer_parts,
    const void* start, const void* bits, std::int64_t bits_width,
    bool bits_are_four_state) {
  InsertDesignatedBits<Bit32>(
      designation, width, outer_parts, start, bits, bits_width,
      bits_are_four_state);
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
auto lyra_rt_bit64_cell_sampled_get(void* cell) -> const void* {
  return HandleTo(VariableAt<Bit64>(cell).SampledGet());
}
auto lyra_rt_bit64_cell_begin_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count)
    -> std::int64_t {
  return CellBeginTakeover<Bit64>(cell, level, first, count);
}
auto lyra_rt_bit64_cell_drive_takeover(
    void* cell, std::int64_t level, std::int64_t generation, const void* value)
    -> bool {
  return CellDriveTakeover<Bit64>(cell, level, generation, value);
}
void lyra_rt_bit64_cell_end_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count) {
  CellEndTakeover<Bit64>(cell, level, first, count);
}
auto lyra_rt_bit64_ref_begin_takeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count)
    -> std::int64_t {
  return RefBeginTakeover<Bit64>(reference, level, first, count);
}
auto lyra_rt_bit64_ref_drive_takeover(
    void* reference, std::int64_t level, std::int64_t generation,
    const void* value) -> bool {
  return RefDriveTakeover<Bit64>(reference, level, generation, value);
}
void lyra_rt_bit64_ref_end_takeover(
    void* reference, std::int64_t level, std::int64_t first,
    std::int64_t count) {
  RefEndTakeover<Bit64>(reference, level, first, count);
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
auto lyra_rt_bit64_ref_sampled_get(void* reference) -> const void* {
  return RefSampledGet<Bit64>(reference);
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
    const void* history, std::int64_t ticks_back, void* out) -> void* {
  return Emplace(
      out, static_cast<const SampledHistory<Bit64>*>(history)->At(ticks_back));
}
auto lyra_rt_bit64_land(const void* designation) noexcept -> void* {
  return LandDesignation<Bit64>(designation);
}
void lyra_rt_bit64_insert_designated_bits(
    const void* designation, std::int64_t width, LyraSpan outer_parts,
    const void* start, const void* bits, std::int64_t bits_width,
    bool bits_are_four_state) {
  InsertDesignatedBits<Bit64>(
      designation, width, outer_parts, start, bits, bits_width,
      bits_are_four_state);
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
auto lyra_rt_logic8_cell_sampled_get(void* cell) -> const void* {
  return HandleTo(VariableAt<Logic8>(cell).SampledGet());
}
auto lyra_rt_logic8_cell_begin_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count)
    -> std::int64_t {
  return CellBeginTakeover<Logic8>(cell, level, first, count);
}
auto lyra_rt_logic8_cell_drive_takeover(
    void* cell, std::int64_t level, std::int64_t generation, const void* value)
    -> bool {
  return CellDriveTakeover<Logic8>(cell, level, generation, value);
}
void lyra_rt_logic8_cell_end_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count) {
  CellEndTakeover<Logic8>(cell, level, first, count);
}
auto lyra_rt_logic8_ref_begin_takeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count)
    -> std::int64_t {
  return RefBeginTakeover<Logic8>(reference, level, first, count);
}
auto lyra_rt_logic8_ref_drive_takeover(
    void* reference, std::int64_t level, std::int64_t generation,
    const void* value) -> bool {
  return RefDriveTakeover<Logic8>(reference, level, generation, value);
}
void lyra_rt_logic8_ref_end_takeover(
    void* reference, std::int64_t level, std::int64_t first,
    std::int64_t count) {
  RefEndTakeover<Logic8>(reference, level, first, count);
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
auto lyra_rt_logic8_ref_sampled_get(void* reference) -> const void* {
  return RefSampledGet<Logic8>(reference);
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
    const void* history, std::int64_t ticks_back, void* out) -> void* {
  return Emplace(
      out, static_cast<const SampledHistory<Logic8>*>(history)->At(ticks_back));
}
auto lyra_rt_logic8_land(const void* designation) noexcept -> void* {
  return LandDesignation<Logic8>(designation);
}
void lyra_rt_logic8_insert_designated_bits(
    const void* designation, std::int64_t width, LyraSpan outer_parts,
    const void* start, const void* bits, std::int64_t bits_width,
    bool bits_are_four_state) {
  InsertDesignatedBits<Logic8>(
      designation, width, outer_parts, start, bits, bits_width,
      bits_are_four_state);
}
void lyra_rt_logic8_net_construct(std::int64_t width, void* storage) {
  BuildAt<ResolvedNet<Logic8>>(storage, static_cast<std::uint64_t>(width));
}
void lyra_rt_logic8_net_destroy(void* storage) {
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
auto lyra_rt_logic8_net_begin_takeover(
    void* net, std::int64_t level, std::int64_t first, std::int64_t count)
    -> std::int64_t {
  return NetOf<Logic8>(net).BeginTakeover(level, first, count);
}
auto lyra_rt_logic8_net_drive_takeover(
    void* net, std::int64_t level, std::int64_t generation, const void* value)
    -> bool {
  return NetOf<Logic8>(net).DriveTakeover(
      level, generation, Read<Logic8>(value));
}
void lyra_rt_logic8_net_end_takeover(
    void* net, std::int64_t level, std::int64_t first, std::int64_t count) {
  NetOf<Logic8>(net).EndTakeover(level, first, count);
}
void lyra_rt_logic8_net_arm_sampling(void* net) {
  NetOf<Logic8>(net).ArmSampling();
}
auto lyra_rt_logic8_net_sampled_get(void* net) -> const void* {
  return HandleTo(NetOf<Logic8>(net).SampledGet());
}
auto lyra_rt_logic8_attach_driver(void* net, std::int64_t strength) -> void* {
  return &NetOf<Logic8>(net).AttachDriver(strength);
}
void lyra_rt_logic8_net_join(
    void* net, void* other, std::int64_t here, std::int64_t there,
    std::int64_t count) {
  JoinNets<Logic8>(net, other, here, there, count);
}
auto lyra_rt_logic8_driver_get(void* driver) -> const void* {
  return &DriverOf<Logic8>(driver).Get();
}
void lyra_rt_logic8_driver_set(void* driver, const void* value) {
  DriverOf<Logic8>(driver).Set(Read<Logic8>(value));
}
auto lyra_rt_logic8_driver_open_for_write(void* driver, void* out) -> void* {
  return OpenDriverWrite<Logic8>(driver, out);
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
auto lyra_rt_logic16_cell_sampled_get(void* cell) -> const void* {
  return HandleTo(VariableAt<Logic16>(cell).SampledGet());
}
auto lyra_rt_logic16_cell_begin_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count)
    -> std::int64_t {
  return CellBeginTakeover<Logic16>(cell, level, first, count);
}
auto lyra_rt_logic16_cell_drive_takeover(
    void* cell, std::int64_t level, std::int64_t generation, const void* value)
    -> bool {
  return CellDriveTakeover<Logic16>(cell, level, generation, value);
}
void lyra_rt_logic16_cell_end_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count) {
  CellEndTakeover<Logic16>(cell, level, first, count);
}
auto lyra_rt_logic16_ref_begin_takeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count)
    -> std::int64_t {
  return RefBeginTakeover<Logic16>(reference, level, first, count);
}
auto lyra_rt_logic16_ref_drive_takeover(
    void* reference, std::int64_t level, std::int64_t generation,
    const void* value) -> bool {
  return RefDriveTakeover<Logic16>(reference, level, generation, value);
}
void lyra_rt_logic16_ref_end_takeover(
    void* reference, std::int64_t level, std::int64_t first,
    std::int64_t count) {
  RefEndTakeover<Logic16>(reference, level, first, count);
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
auto lyra_rt_logic16_ref_sampled_get(void* reference) -> const void* {
  return RefSampledGet<Logic16>(reference);
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
    const void* history, std::int64_t ticks_back, void* out) -> void* {
  return Emplace(
      out,
      static_cast<const SampledHistory<Logic16>*>(history)->At(ticks_back));
}
auto lyra_rt_logic16_land(const void* designation) noexcept -> void* {
  return LandDesignation<Logic16>(designation);
}
void lyra_rt_logic16_insert_designated_bits(
    const void* designation, std::int64_t width, LyraSpan outer_parts,
    const void* start, const void* bits, std::int64_t bits_width,
    bool bits_are_four_state) {
  InsertDesignatedBits<Logic16>(
      designation, width, outer_parts, start, bits, bits_width,
      bits_are_four_state);
}
void lyra_rt_logic16_net_construct(std::int64_t width, void* storage) {
  BuildAt<ResolvedNet<Logic16>>(storage, static_cast<std::uint64_t>(width));
}
void lyra_rt_logic16_net_destroy(void* storage) {
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
auto lyra_rt_logic16_net_begin_takeover(
    void* net, std::int64_t level, std::int64_t first, std::int64_t count)
    -> std::int64_t {
  return NetOf<Logic16>(net).BeginTakeover(level, first, count);
}
auto lyra_rt_logic16_net_drive_takeover(
    void* net, std::int64_t level, std::int64_t generation, const void* value)
    -> bool {
  return NetOf<Logic16>(net).DriveTakeover(
      level, generation, Read<Logic16>(value));
}
void lyra_rt_logic16_net_end_takeover(
    void* net, std::int64_t level, std::int64_t first, std::int64_t count) {
  NetOf<Logic16>(net).EndTakeover(level, first, count);
}
void lyra_rt_logic16_net_arm_sampling(void* net) {
  NetOf<Logic16>(net).ArmSampling();
}
auto lyra_rt_logic16_net_sampled_get(void* net) -> const void* {
  return HandleTo(NetOf<Logic16>(net).SampledGet());
}
auto lyra_rt_logic16_attach_driver(void* net, std::int64_t strength) -> void* {
  return &NetOf<Logic16>(net).AttachDriver(strength);
}
void lyra_rt_logic16_net_join(
    void* net, void* other, std::int64_t here, std::int64_t there,
    std::int64_t count) {
  JoinNets<Logic16>(net, other, here, there, count);
}
auto lyra_rt_logic16_driver_get(void* driver) -> const void* {
  return &DriverOf<Logic16>(driver).Get();
}
void lyra_rt_logic16_driver_set(void* driver, const void* value) {
  DriverOf<Logic16>(driver).Set(Read<Logic16>(value));
}
auto lyra_rt_logic16_driver_open_for_write(void* driver, void* out) -> void* {
  return OpenDriverWrite<Logic16>(driver, out);
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
auto lyra_rt_logic32_cell_sampled_get(void* cell) -> const void* {
  return HandleTo(VariableAt<Logic32>(cell).SampledGet());
}
auto lyra_rt_logic32_cell_begin_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count)
    -> std::int64_t {
  return CellBeginTakeover<Logic32>(cell, level, first, count);
}
auto lyra_rt_logic32_cell_drive_takeover(
    void* cell, std::int64_t level, std::int64_t generation, const void* value)
    -> bool {
  return CellDriveTakeover<Logic32>(cell, level, generation, value);
}
void lyra_rt_logic32_cell_end_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count) {
  CellEndTakeover<Logic32>(cell, level, first, count);
}
auto lyra_rt_logic32_ref_begin_takeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count)
    -> std::int64_t {
  return RefBeginTakeover<Logic32>(reference, level, first, count);
}
auto lyra_rt_logic32_ref_drive_takeover(
    void* reference, std::int64_t level, std::int64_t generation,
    const void* value) -> bool {
  return RefDriveTakeover<Logic32>(reference, level, generation, value);
}
void lyra_rt_logic32_ref_end_takeover(
    void* reference, std::int64_t level, std::int64_t first,
    std::int64_t count) {
  RefEndTakeover<Logic32>(reference, level, first, count);
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
auto lyra_rt_logic32_ref_sampled_get(void* reference) -> const void* {
  return RefSampledGet<Logic32>(reference);
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
    const void* history, std::int64_t ticks_back, void* out) -> void* {
  return Emplace(
      out,
      static_cast<const SampledHistory<Logic32>*>(history)->At(ticks_back));
}
auto lyra_rt_logic32_land(const void* designation) noexcept -> void* {
  return LandDesignation<Logic32>(designation);
}
void lyra_rt_logic32_insert_designated_bits(
    const void* designation, std::int64_t width, LyraSpan outer_parts,
    const void* start, const void* bits, std::int64_t bits_width,
    bool bits_are_four_state) {
  InsertDesignatedBits<Logic32>(
      designation, width, outer_parts, start, bits, bits_width,
      bits_are_four_state);
}
void lyra_rt_logic32_net_construct(std::int64_t width, void* storage) {
  BuildAt<ResolvedNet<Logic32>>(storage, static_cast<std::uint64_t>(width));
}
void lyra_rt_logic32_net_destroy(void* storage) {
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
auto lyra_rt_logic32_net_begin_takeover(
    void* net, std::int64_t level, std::int64_t first, std::int64_t count)
    -> std::int64_t {
  return NetOf<Logic32>(net).BeginTakeover(level, first, count);
}
auto lyra_rt_logic32_net_drive_takeover(
    void* net, std::int64_t level, std::int64_t generation, const void* value)
    -> bool {
  return NetOf<Logic32>(net).DriveTakeover(
      level, generation, Read<Logic32>(value));
}
void lyra_rt_logic32_net_end_takeover(
    void* net, std::int64_t level, std::int64_t first, std::int64_t count) {
  NetOf<Logic32>(net).EndTakeover(level, first, count);
}
void lyra_rt_logic32_net_arm_sampling(void* net) {
  NetOf<Logic32>(net).ArmSampling();
}
auto lyra_rt_logic32_net_sampled_get(void* net) -> const void* {
  return HandleTo(NetOf<Logic32>(net).SampledGet());
}
auto lyra_rt_logic32_attach_driver(void* net, std::int64_t strength) -> void* {
  return &NetOf<Logic32>(net).AttachDriver(strength);
}
void lyra_rt_logic32_net_join(
    void* net, void* other, std::int64_t here, std::int64_t there,
    std::int64_t count) {
  JoinNets<Logic32>(net, other, here, there, count);
}
auto lyra_rt_logic32_driver_get(void* driver) -> const void* {
  return &DriverOf<Logic32>(driver).Get();
}
void lyra_rt_logic32_driver_set(void* driver, const void* value) {
  DriverOf<Logic32>(driver).Set(Read<Logic32>(value));
}
auto lyra_rt_logic32_driver_open_for_write(void* driver, void* out) -> void* {
  return OpenDriverWrite<Logic32>(driver, out);
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
auto lyra_rt_logic64_cell_sampled_get(void* cell) -> const void* {
  return HandleTo(VariableAt<Logic64>(cell).SampledGet());
}
auto lyra_rt_logic64_cell_begin_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count)
    -> std::int64_t {
  return CellBeginTakeover<Logic64>(cell, level, first, count);
}
auto lyra_rt_logic64_cell_drive_takeover(
    void* cell, std::int64_t level, std::int64_t generation, const void* value)
    -> bool {
  return CellDriveTakeover<Logic64>(cell, level, generation, value);
}
void lyra_rt_logic64_cell_end_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count) {
  CellEndTakeover<Logic64>(cell, level, first, count);
}
auto lyra_rt_logic64_ref_begin_takeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count)
    -> std::int64_t {
  return RefBeginTakeover<Logic64>(reference, level, first, count);
}
auto lyra_rt_logic64_ref_drive_takeover(
    void* reference, std::int64_t level, std::int64_t generation,
    const void* value) -> bool {
  return RefDriveTakeover<Logic64>(reference, level, generation, value);
}
void lyra_rt_logic64_ref_end_takeover(
    void* reference, std::int64_t level, std::int64_t first,
    std::int64_t count) {
  RefEndTakeover<Logic64>(reference, level, first, count);
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
auto lyra_rt_logic64_ref_sampled_get(void* reference) -> const void* {
  return RefSampledGet<Logic64>(reference);
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
    const void* history, std::int64_t ticks_back, void* out) -> void* {
  return Emplace(
      out,
      static_cast<const SampledHistory<Logic64>*>(history)->At(ticks_back));
}
auto lyra_rt_logic64_land(const void* designation) noexcept -> void* {
  return LandDesignation<Logic64>(designation);
}
void lyra_rt_logic64_insert_designated_bits(
    const void* designation, std::int64_t width, LyraSpan outer_parts,
    const void* start, const void* bits, std::int64_t bits_width,
    bool bits_are_four_state) {
  InsertDesignatedBits<Logic64>(
      designation, width, outer_parts, start, bits, bits_width,
      bits_are_four_state);
}
void lyra_rt_logic64_net_construct(std::int64_t width, void* storage) {
  BuildAt<ResolvedNet<Logic64>>(storage, static_cast<std::uint64_t>(width));
}
void lyra_rt_logic64_net_destroy(void* storage) {
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
auto lyra_rt_logic64_net_begin_takeover(
    void* net, std::int64_t level, std::int64_t first, std::int64_t count)
    -> std::int64_t {
  return NetOf<Logic64>(net).BeginTakeover(level, first, count);
}
auto lyra_rt_logic64_net_drive_takeover(
    void* net, std::int64_t level, std::int64_t generation, const void* value)
    -> bool {
  return NetOf<Logic64>(net).DriveTakeover(
      level, generation, Read<Logic64>(value));
}
void lyra_rt_logic64_net_end_takeover(
    void* net, std::int64_t level, std::int64_t first, std::int64_t count) {
  NetOf<Logic64>(net).EndTakeover(level, first, count);
}
void lyra_rt_logic64_net_arm_sampling(void* net) {
  NetOf<Logic64>(net).ArmSampling();
}
auto lyra_rt_logic64_net_sampled_get(void* net) -> const void* {
  return HandleTo(NetOf<Logic64>(net).SampledGet());
}
auto lyra_rt_logic64_attach_driver(void* net, std::int64_t strength) -> void* {
  return &NetOf<Logic64>(net).AttachDriver(strength);
}
void lyra_rt_logic64_net_join(
    void* net, void* other, std::int64_t here, std::int64_t there,
    std::int64_t count) {
  JoinNets<Logic64>(net, other, here, there, count);
}
auto lyra_rt_logic64_driver_get(void* driver) -> const void* {
  return &DriverOf<Logic64>(driver).Get();
}
void lyra_rt_logic64_driver_set(void* driver, const void* value) {
  DriverOf<Logic64>(driver).Set(Read<Logic64>(value));
}
auto lyra_rt_logic64_driver_open_for_write(void* driver, void* out) -> void* {
  return OpenDriverWrite<Logic64>(driver, out);
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
auto lyra_rt_bit_wide_cell_sampled_get(void* cell) -> const void* {
  return lyra::runtime::VariableAt<BitWide>(cell).SampledBytes();
}
auto lyra_rt_bit_wide_cell_begin_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count)
    -> std::int64_t {
  return lyra::runtime::VariableAt<BitWide>(cell).BeginTakeover(
      level, first, count);
}
auto lyra_rt_bit_wide_cell_drive_takeover(
    void* cell, std::int64_t level, std::int64_t generation, const void* value)
    -> bool {
  return lyra::runtime::VariableAt<BitWide>(cell).DriveTakeoverBytes(
      level, generation, value);
}
void lyra_rt_bit_wide_cell_end_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count) {
  lyra::runtime::VariableAt<BitWide>(cell).EndTakeover(level, first, count);
}
auto lyra_rt_bit_wide_ref_begin_takeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count)
    -> std::int64_t {
  return lyra::runtime::WideRefBeginTakeover<BitWide>(
      reference, level, first, count);
}
auto lyra_rt_bit_wide_ref_drive_takeover(
    void* reference, std::int64_t level, std::int64_t generation,
    const void* value) -> bool {
  return lyra::runtime::WideRefDriveTakeover<BitWide>(
      reference, level, generation, value);
}
void lyra_rt_bit_wide_ref_end_takeover(
    void* reference, std::int64_t level, std::int64_t first,
    std::int64_t count) {
  lyra::runtime::EndTakeoverThrough<lyra::runtime::VariableOf<BitWide>::Type>(
      lyra::runtime::ErasedAt(reference), level, first, count);
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
auto lyra_rt_bit_wide_ref_sampled_get(void* reference) -> const void* {
  return TailRefSampledGet<BitWide>(reference);
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
  BuildAt<LocalOf<BitWide>::Type>(storage, static_cast<std::uint64_t>(width));
}
void lyra_rt_bit_wide_value_cell_destroy(void* storage) {
  std::destroy_at(&LocalAt<BitWide>(storage));
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
    const void* history, std::int64_t ticks_back, void* out) -> void* {
  return Emplace(
      out,
      static_cast<const SampledHistory<BitWide>*>(history)->At(ticks_back));
}
auto lyra_rt_bit_wide_land(const void* designation, std::int64_t width) noexcept
    -> void* {
  return lyra::runtime::LandWideDesignation<BitWide>(designation, width);
}
void lyra_rt_bit_wide_insert_designated_bits(
    const void* designation, std::int64_t width, LyraSpan outer_parts,
    const void* start, const void* bits, std::int64_t bits_width,
    bool bits_are_four_state) {
  lyra::runtime::InsertDesignatedWideBits<BitWide>(
      designation, width, outer_parts, start, bits, bits_width,
      bits_are_four_state);
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
auto lyra_rt_logic_wide_cell_sampled_get(void* cell) -> const void* {
  return lyra::runtime::VariableAt<LogicWide>(cell).SampledBytes();
}
auto lyra_rt_logic_wide_cell_begin_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count)
    -> std::int64_t {
  return lyra::runtime::VariableAt<LogicWide>(cell).BeginTakeover(
      level, first, count);
}
auto lyra_rt_logic_wide_cell_drive_takeover(
    void* cell, std::int64_t level, std::int64_t generation, const void* value)
    -> bool {
  return lyra::runtime::VariableAt<LogicWide>(cell).DriveTakeoverBytes(
      level, generation, value);
}
void lyra_rt_logic_wide_cell_end_takeover(
    void* cell, std::int64_t level, std::int64_t first, std::int64_t count) {
  lyra::runtime::VariableAt<LogicWide>(cell).EndTakeover(level, first, count);
}
auto lyra_rt_logic_wide_ref_begin_takeover(
    void* reference, std::int64_t level, std::int64_t first, std::int64_t count)
    -> std::int64_t {
  return lyra::runtime::WideRefBeginTakeover<LogicWide>(
      reference, level, first, count);
}
auto lyra_rt_logic_wide_ref_drive_takeover(
    void* reference, std::int64_t level, std::int64_t generation,
    const void* value) -> bool {
  return lyra::runtime::WideRefDriveTakeover<LogicWide>(
      reference, level, generation, value);
}
void lyra_rt_logic_wide_ref_end_takeover(
    void* reference, std::int64_t level, std::int64_t first,
    std::int64_t count) {
  lyra::runtime::EndTakeoverThrough<lyra::runtime::VariableOf<LogicWide>::Type>(
      lyra::runtime::ErasedAt(reference), level, first, count);
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
auto lyra_rt_logic_wide_ref_sampled_get(void* reference) -> const void* {
  return TailRefSampledGet<LogicWide>(reference);
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
  BuildAt<LocalOf<LogicWide>::Type>(storage, static_cast<std::uint64_t>(width));
}
void lyra_rt_logic_wide_value_cell_destroy(void* storage) {
  std::destroy_at(&LocalAt<LogicWide>(storage));
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
    const void* history, std::int64_t ticks_back, void* out) -> void* {
  return Emplace(
      out,
      static_cast<const SampledHistory<LogicWide>*>(history)->At(ticks_back));
}
auto lyra_rt_logic_wide_land(
    const void* designation, std::int64_t width) noexcept -> void* {
  return lyra::runtime::LandWideDesignation<LogicWide>(designation, width);
}
void lyra_rt_logic_wide_insert_designated_bits(
    const void* designation, std::int64_t width, LyraSpan outer_parts,
    const void* start, const void* bits, std::int64_t bits_width,
    bool bits_are_four_state) {
  lyra::runtime::InsertDesignatedWideBits<LogicWide>(
      designation, width, outer_parts, start, bits, bits_width,
      bits_are_four_state);
}
void lyra_rt_logic_wide_net_construct(std::int64_t width, void* storage) {
  BuildAt<ResolvedNet<LogicWide>>(storage, static_cast<std::uint64_t>(width));
}
void lyra_rt_logic_wide_net_destroy(void* storage) {
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
auto lyra_rt_logic_wide_net_begin_takeover(
    void* net, std::int64_t level, std::int64_t first, std::int64_t count)
    -> std::int64_t {
  return NetOf<LogicWide>(net).BeginTakeover(level, first, count);
}
auto lyra_rt_logic_wide_net_drive_takeover(
    void* net, std::int64_t level, std::int64_t generation, const void* value)
    -> bool {
  return NetOf<LogicWide>(net).DriveTakeoverBytes(level, generation, value);
}
void lyra_rt_logic_wide_net_end_takeover(
    void* net, std::int64_t level, std::int64_t first, std::int64_t count) {
  NetOf<LogicWide>(net).EndTakeover(level, first, count);
}
void lyra_rt_logic_wide_net_arm_sampling(void* net) {
  NetOf<LogicWide>(net).ArmSampling();
}
auto lyra_rt_logic_wide_net_sampled_get(void* net) -> const void* {
  return HandleTo(NetOf<LogicWide>(net).SampledGet());
}
auto lyra_rt_logic_wide_attach_driver(void* net, std::int64_t strength)
    -> void* {
  return &NetOf<LogicWide>(net).AttachDriver(strength);
}
void lyra_rt_logic_wide_net_join(
    void* net, void* other, std::int64_t here, std::int64_t there,
    std::int64_t count) {
  JoinNets<LogicWide>(net, other, here, there, count);
}
auto lyra_rt_logic_wide_driver_get(void* driver) -> const void* {
  return DriverOf<LogicWide>(driver).Bytes();
}
void lyra_rt_logic_wide_driver_set(void* driver, const void* value) {
  DriverOf<LogicWide>(driver).SetBytes(value);
}
auto lyra_rt_logic_wide_driver_open_for_write(void* driver, void* out)
    -> void* {
  return OpenTailDriverWrite<LogicWide>(driver, out);
}
}
