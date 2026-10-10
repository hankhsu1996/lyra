#include "lyra/runtime/runtime_abi.hpp"

#include <algorithm>
#include <array>
#include <coroutine>
#include <cstddef>
#include <cstdint>
#include <cxxabi.h>
#include <deque>
#include <exception>
#include <functional>
#include <memory>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/runtime/activation_value_cell.hpp"
#include "lyra/runtime/ambient_run_context.hpp"
#include "lyra/runtime/cancellation.hpp"
#include "lyra/runtime/closure.hpp"
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
#include "lyra/runtime/named_event.hpp"
#include "lyra/runtime/nba_region.hpp"
#include "lyra/runtime/net.hpp"
#include "lyra/runtime/object_change.hpp"
#include "lyra/runtime/object_ref.hpp"
#include "lyra/runtime/open_write.hpp"
#include "lyra/runtime/plusargs.hpp"
#include "lyra/runtime/process_control.hpp"
#include "lyra/runtime/random.hpp"
#include "lyra/runtime/read_report.hpp"
#include "lyra/runtime/runtime.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/sampled_history.hpp"
#include "lyra/runtime/scope.hpp"
#include "lyra/runtime/shared_pointer.hpp"
#include "lyra/runtime/sim_time.hpp"
#include "lyra/runtime/simulation_entry.hpp"
#include "lyra/runtime/value_change_wait.hpp"
#include "lyra/runtime/value_families.hpp"
#include "lyra/runtime/value_handle.hpp"
#include "lyra/runtime/var.hpp"
#include "lyra/support/event_edge.hpp"
#include "lyra/value/any_value.hpp"
#include "lyra/value/chandle.hpp"
#include "lyra/value/dpi_canonical.hpp"
#include "lyra/value/dpi_open_array.hpp"
#include "lyra/value/empty.hpp"
#include "lyra/value/enumeration.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/formation.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_value_type.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/library_value_types.hpp"
#include "lyra/value/managed_ref.hpp"
#include "lyra/value/real.hpp"
#include "lyra/value/require.hpp"
#include "lyra/value/runtime_array_manipulation.hpp"
#include "lyra/value/runtime_associative_array.hpp"
#include "lyra/value/runtime_dynamic_array.hpp"
#include "lyra/value/runtime_memory.hpp"
#include "lyra/value/runtime_queue.hpp"
#include "lyra/value/runtime_tagged_union.hpp"
#include "lyra/value/runtime_tuple.hpp"
#include "lyra/value/runtime_union.hpp"
#include "lyra/value/runtime_unpacked_array.hpp"
#include "lyra/value/scan.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/unpacked_range.hpp"

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

// A body run by a site that does not stay to supply its environment per
// invocation -- a process, a spawned branch, an enabled task: its frame, built
// and not yet begun, and whatever that frame reads that nothing else keeps
// alive. A body reaching its members through a receiver reads storage that
// outlives every activation reading it, so it keeps nothing here; a branch
// reads captures copied where the `fork` ran, which outlive nothing on their
// own, so the closure holding them is kept here and ends after the frame does.
class GeneratedBody {
 public:
  GeneratedBody(void* frame, OwnedClosure captures)
      : captures_(std::move(captures)), frame_(frame) {
  }

  [[nodiscard]] auto Done() const -> bool {
    return frame_.Done();
  }
  void Resume() const {
    frame_.Resume();
  }

 private:
  // Declared ahead of the frame so it is destroyed after it: the frame reads
  // the captures until it ends.
  OwnedClosure captures_;
  GeneratedCoroutine frame_;
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
// hold itself -- that body's own coroutine, what it reads that nothing else
// keeps alive, and this execution's value store -- so all are released
// together, on every path this leaves. The ramp laid the body's frame out and
// stopped before its first statement, so no generated code has run yet and
// nothing has reached for the store.
auto RunGeneratedProcess(GeneratedBody generated) -> Coroutine<void> {
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
auto StartGeneratedProcess(void* out, GeneratedBody body) -> void* {
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

// Storage a declaration holds, built empty where its owner was laid out; what
// it holds arrives afterwards through its own access.
template <typename T>
void BuildAt(void* storage) {
  std::construct_at(static_cast<T*>(storage));
}

template <value::LyraValue T>
auto LentAt(const void* reference) -> Ref<T> {
  return Ref<T>{ErasedAt(reference)};
}

auto BuildReference(void* out, const ErasedReference& reference) -> void* {
  return std::construct_at(static_cast<ErasedReference*>(out), reference);
}

template <value::LyraValue T>
auto MakeSharedCell(void* out) -> void* {
  return std::construct_at(
      static_cast<SharedPointer*>(out), SharedPointer::MakeCell<T>());
}

// A reference to the whole of a subscribable variable, which a write through it
// is a write of.
template <value::LyraValue T>
auto ReferToCell(void* cell, void* out) -> void* {
  return BuildReference(out, Ref<T>{*static_cast<Var<T>*>(cell)}.Erased());
}

// Reading and writing storage a caller lent, which answers through the variable
// it belongs to where it belongs to one.
template <value::LyraValue T>
auto RefGet(void* reference) -> const void* {
  return &LentAt<T>(reference).Get();
}

// Opening a write into what a wrapper stands for, in the storage the caller
// gave. A cell and a watchable referent report the write when it ends;
// storage nothing can watch takes it with nothing to report; a driver
// re-resolves its net if its contribution moved.
template <value::LyraValue T>
auto OpenCellWrite(void* cell, void* out) -> void* {
  return std::construct_at(
      static_cast<OpenWrite*>(out), Ref<T>{*static_cast<Var<T>*>(cell)});
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
auto RefSampledLoad(void* reference, void* out) -> void* {
  return Emplace(out, LentAt<T>(reference).SampledGet());
}

// The same over a tuple, which a reference names as the bytes its type laid
// out where it lies: no object of the runtime's stands for a tuple there, since
// one component of a tuple is bytes of the tuple holding it. So these do what
// a reference's own methods do, through the table those bytes open with.
auto ReferToTupleCell(void* cell, void* out) -> void* {
  auto& variable = *static_cast<Var<value::RuntimeTuple>*>(cell);
  return BuildReference(
      out, ErasedReference{
               .holder = static_cast<VariableCell*>(&variable),
               .storage = HandleTo(variable.Storage()),
               .whole = true});
}

void TupleRefSet(void* reference, const void* value) {
  const ErasedReference& lent = ErasedAt(reference);
  if (!lent.Admits()) {
    return;
  }
  if (!lent.Watched()) {
    value::RuntimeTuple::AssignAt(lent.storage, value);
    return;
  }
  if (value::RuntimeTuple::BitIdentical(lent.storage, value)) {
    return;
  }
  value::RuntimeTuple::AssignAt(lent.storage, value);
  lent.Report(Change::Whole());
}

void TupleRefArmSampling(void* reference) {
  const ErasedReference& lent = ErasedAt(reference);
  if (lent.whole) {
    WholeVariable<value::RuntimeTuple>(lent).ArmSampling();
  }
}

auto TupleRefSampledLoad(void* reference, void* out) -> void* {
  const ErasedReference& lent = ErasedAt(reference);
  return std::visit(
      Overloaded{
          [&](std::monostate) -> void* {
            return Emplace(out, Read<value::RuntimeTuple>(lent.storage));
          },
          [&](VariableCell*) -> void* {
            if (!lent.whole) {
              throw SimulationError(
                  "a sampled value of part of a variable lent by reference "
                  "is not yet supported");
            }
            return Emplace(
                out, WholeVariable<value::RuntimeTuple>(lent).SampledGet());
          },
          [](GcObject*) -> void* {
            throw SimulationError(
                "a sampled value of a class property lent by reference is not "
                "yet supported");
          }},
      lent.holder);
}

// A tuple lent by reference as a write through the reference reaches it. A copy
// is a tuple of its own, which is what a write the variable turns away lands in
// (LRM 10.6).
class LentTuple {
 public:
  explicit LentTuple(void* bytes) : bytes_(bytes) {
  }
  LentTuple(const LentTuple& other)
      : kept_(value::RuntimeTuple::CopyOf(other.bytes_)),
        bytes_(kept_->Bytes()) {
  }
  auto operator=(const LentTuple&) -> LentTuple& = delete;
  LentTuple(LentTuple&&) = delete;
  auto operator=(LentTuple&&) -> LentTuple& = delete;
  ~LentTuple() = default;

  [[nodiscard]] auto Bytes() const -> void* {
    return bytes_;
  }

 private:
  std::optional<value::RuntimeTuple> kept_;
  void* bytes_;
};

// The sink a write through a reference to a tuple writes into: the tuple where
// the reference lends it, and what holds that storage to tell.
class TupleReference {
 public:
  using ValueType = LentTuple;

  explicit TupleReference(const ErasedReference& lent)
      : lent_(lent), place_(lent.storage) {
  }
  // A copy is the same reference, so it lends the same place.
  TupleReference(const TupleReference& other)
      : lent_(other.lent_), place_(other.lent_.storage) {
  }
  TupleReference(TupleReference&& other) noexcept
      : lent_(other.lent_), place_(other.lent_.storage) {
  }
  auto operator=(const TupleReference&) -> TupleReference& = delete;
  auto operator=(TupleReference&&) -> TupleReference& = delete;
  ~TupleReference() = default;

  [[nodiscard]] auto AdmitsWrite() const -> bool {
    return lent_.Admits();
  }
  [[nodiscard]] auto MutationStorage() -> LentTuple& {
    return place_;
  }
  [[nodiscard]] auto Watched() const -> bool {
    return lent_.Watched();
  }
  void PublishTransition(const Change& change) const {
    lent_.Report(change);
  }

 private:
  ErasedReference lent_;
  LentTuple place_;
};

static_assert(MutationSink<TupleReference>);

auto OpenTupleRefWrite(void* reference, void* out) -> void* {
  return std::construct_at(
      static_cast<OpenWrite*>(out), TupleReference{ErasedAt(reference)});
}

// The integral type an entry is told where it builds values of one and keeps
// them with it -- a key of an associative memory, an element of a byte array.
auto IntegralTypeAt(const void* type) -> const value::IntegralValueType& {
  return *static_cast<const value::IntegralValueType*>(type);
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

// The planes of such an operand, and of one the entry reads the bits of, whose
// signedness it is told nothing of.
auto NumberAt(
    const void* value, std::int64_t width, bool is_signed, bool is_four_state)
    -> value::LoadedWords {
  return value::LoadedWords::Load(
      value, NumberShape(width, is_signed, is_four_state));
}

auto BitsAt(const void* value, std::int64_t width, bool is_four_state)
    -> value::LoadedWords {
  return value::LoadedWords::Load(
      value, NumberShape(width, false, is_four_state));
}

// The machine number such planes hold.
auto IntOf(const value::LoadedWords& number) -> std::int64_t {
  return value::ToInt64(
      number.Read(), number.Shape().width, number.Shape().signedness);
}

// The position a position value names, none where it holds x or z (LRM
// 7.4.5).
auto PositionNamedAt(const void* position) -> std::optional<std::int64_t> {
  return value::ReadPosition(Read<value::Position>(position));
}

// The same over an integral value wider than a word, which a reference names
// as its bytes where they lie for the reason it names a tuple so. Nothing in
// those bytes says how many of them the value is, so an access that has to
// know is told how wide the value is.
template <value::HeldAsWords T>
auto ReferToWideCell(void* cell, void* out) -> void* {
  auto& variable = *static_cast<Var<T>*>(cell);
  return BuildReference(
      out, ErasedReference{
               .holder = static_cast<VariableCell*>(&variable),
               .storage = HandleTo(variable.Storage()),
               .whole = true});
}

// The value of `T` a reference names, where it lies, `width` bits wide.
template <value::HeldAsWords T>
auto WideLentAt(const ErasedReference& lent, std::int64_t width)
    -> value::WideAt<T::kDomain> {
  return {.bytes = lent.storage, .width = static_cast<std::uint64_t>(width)};
}

template <value::HeldAsWords T>
void WideRefSet(void* reference, std::int64_t width, const void* value) {
  const ErasedReference& lent = ErasedAt(reference);
  if (!lent.Admits()) {
    return;
  }
  const value::WideAt<T::kDomain> held = WideLentAt<T>(lent, width);
  const auto assign = [&] { std::memmove(held.bytes, value, held.ByteSize()); };
  if (!lent.Watched()) {
    assign();
    return;
  }
  if (std::memcmp(held.bytes, value, held.ByteSize()) == 0) {
    return;
  }
  lent.Report(ReplaceWhole(held, assign));
}

template <value::HeldAsWords T>
void WideRefArmSampling(void* reference) {
  const ErasedReference& lent = ErasedAt(reference);
  if (lent.whole) {
    WholeVariable<T>(lent).ArmSampling();
  }
}

template <value::HeldAsWords T>
auto WideRefSampledLoad(void* reference, std::int64_t width, void* out)
    -> void* {
  const ErasedReference& lent = ErasedAt(reference);
  return std::visit(
      Overloaded{
          [&](std::monostate) -> void* {
            const value::WideAt<T::kDomain> held = WideLentAt<T>(lent, width);
            std::memcpy(out, held.bytes, held.ByteSize());
            return out;
          },
          [&](VariableCell*) -> void* {
            if (!lent.whole) {
              throw SimulationError(
                  "a sampled value of part of a variable lent by reference "
                  "is not yet supported");
            }
            return Emplace(out, WholeVariable<T>(lent).SampledGet());
          },
          [](GcObject*) -> void* {
            throw SimulationError(
                "a sampled value of a class property lent by reference is not "
                "yet supported");
          }},
      lent.holder);
}

// An integral value wider than a word lent by reference as a write through the
// reference reaches it. A copy is a value of its own, which is what a write
// the variable turns away lands in (LRM 10.6).
template <value::HeldAsWords T>
class LentWide {
 public:
  LentWide(void* bytes, std::uint64_t width) : width_(width), bytes_(bytes) {
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

  [[nodiscard]] auto Width() const -> std::uint64_t {
    return width_;
  }
  [[nodiscard]] auto Bytes() const -> void* {
    return bytes_;
  }
  [[nodiscard]] auto Read() const -> value::ConstPlanes {
    return value::WidePlanesAt(
        static_cast<const void*>(bytes_), width_, T::kDomain);
  }

 private:
  std::uint64_t width_;
  // Holds a value only in a copy.
  T kept_;
  void* bytes_;
};

// The sink a write through a reference to such a value writes into: the value
// where the reference lends it, and what holds that storage to tell.
template <value::HeldAsWords T>
class WideReference {
 public:
  using ValueType = LentWide<T>;

  WideReference(const ErasedReference& lent, std::uint64_t width)
      : lent_(lent), place_(lent.storage, width) {
  }
  // A copy is the same reference, so it lends the same place.
  WideReference(const WideReference& other)
      : lent_(other.lent_), place_(other.lent_.storage, other.place_.Width()) {
  }
  WideReference(WideReference&& other) noexcept
      : lent_(other.lent_), place_(other.lent_.storage, other.place_.Width()) {
  }
  auto operator=(const WideReference&) -> WideReference& = delete;
  auto operator=(WideReference&&) -> WideReference& = delete;
  ~WideReference() = default;

  [[nodiscard]] auto AdmitsWrite() const -> bool {
    return lent_.Admits();
  }
  [[nodiscard]] auto MutationStorage() -> LentWide<T>& {
    return place_;
  }
  [[nodiscard]] auto Watched() const -> bool {
    return lent_.Watched();
  }
  void PublishTransition(const Change& change) const {
    lent_.Report(change);
  }

 private:
  ErasedReference lent_;
  LentWide<T> place_;
};

static_assert(MutationSink<WideReference<value::WideLogicVector>>);

template <value::HeldAsWords T>
auto OpenWideRefWrite(void* reference, std::int64_t width, void* out) -> void* {
  return std::construct_at(
      static_cast<OpenWrite*>(out),
      WideReference<T>{ErasedAt(reference), static_cast<std::uint64_t>(width)});
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
      static_cast<Container*>(within.part)->ElementRef(index, formed);
  within.write->Formed(formed);
  return std::construct_at(
      static_cast<ErasedDesignation*>(out),
      ErasedDesignation{.write = within.write, .part = element});
}

template <typename Container, typename Replacement>
void AssignDesignatedSlice(
    const void* designation, std::optional<std::int64_t> start,
    std::int64_t count, const Replacement& replacement) {
  const ErasedDesignation& within = DesignationAt(designation);
  if (static_cast<Container*>(within.part)
          ->AssignSlice(start, count, replacement)) {
    within.write->Moved();
  }
}

template <typename Part>
auto LandDesignation(const void* designation) noexcept -> void* {
  const ErasedDesignation& landed = DesignationAt(designation);
  landed.write->Land(*static_cast<Part*>(landed.part));
  return landed.part;
}

// A tuple designated where it lies is its bytes, which no object stands for.
auto LandTupleDesignation(const void* designation) noexcept -> void* {
  const ErasedDesignation& landed = DesignationAt(designation);
  landed.write->LandTuple(landed.part);
  return landed.part;
}

// So is an integral value wider than a word, whose bytes do not say how many
// of them it is, so the landing is told how wide the value is.
template <value::HeldAsWords T>
auto LandWideDesignation(const void* designation, std::int64_t width) noexcept
    -> void* {
  const ErasedDesignation& landed = DesignationAt(designation);
  landed.write->LandWide(
      value::WideAt<T::kDomain>{
          .bytes = landed.part, .width = static_cast<std::uint64_t>(width)});
  return landed.part;
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
      static_cast<Container*>(from.storage)->ElementRef(index, formed);
  return BuildReference(out, from.Part(element, formed));
}

// Builds a copy of one element of `type`, in the storage the call handed over.
auto ElementInto(void* out, const value::ValueType& type, const void* element)
    -> void* {
  type.Copy(element, out);
  return out;
}

// Stores each of a literal's entries under the index it names (LRM 7.9.11). An
// entry is the product of the two, whose type states each one's type, so
// nothing here converts either.
void SeedAssociativeEntries(
    value::RuntimeAssociativeArray& array, LyraSpan entries) {
  const std::span<const void* const> handles(
      static_cast<const void* const*>(entries.data), entries.count);
  for (const void* entry : handles) {
    const value::TupleComponent& index =
        value::RuntimeTuple::TypeAt(entry).Components()[0];
    const void* const stated = value::RuntimeTuple::ComponentAt(entry, 0);
    // A wildcard index is held with the type it was written in (LRM 7.8.1),
    // which is the type it is stored and compared by.
    if (index.type == &lyra_rt_wildcard_index_value_type) {
      const auto& held = *static_cast<const value::AnyValue*>(stated);
      array.Store(
          value::IndexView{.bytes = held.Bytes(), .type = &held.Type()},
          value::RuntimeTuple::ComponentAt(entry, 1));
      continue;
    }
    array.Store(
        value::IndexView{.bytes = stated, .type = index.type},
        value::RuntimeTuple::ComponentAt(entry, 1));
  }
}

// The element count `new[N]` asks for (LRM 7.5.1), which the design computes,
// so a negative one is its own failure.
auto NewCount(std::int64_t count) -> std::size_t {
  if (count < 0) {
    throw SimulationError(
        "dynamic array new[N]: size operand is negative (LRM 7.5.1)");
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
auto PartsOf(const value::ValueType& type) -> const value::PartsByPosition& {
  const value::PartsByPosition* parts = type.Parts();
  if (parts == nullptr) {
    throw InternalError(
        "a value is walked by position whose type orders no parts so -- "
        "please report this as a bug");
  }
  return *parts;
}

auto ElementHandles(const void* array, const value::ValueType& type)
    -> std::vector<const void*> {
  const value::PartsByPosition& parts = PartsOf(type);
  const std::size_t count = parts.Count(array);
  std::vector<const void*> handles;
  handles.reserve(count);
  for (std::size_t position = 0; position < count; ++position) {
    handles.push_back(parts.At(array, position));
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

// What a body holding a closure hands over when something longer-lived takes
// it: the closure's one owner. The closure itself stays where it was made.
auto TakeOwner(void* closure) -> OwnedClosure {
  return std::move(*static_cast<OwnedClosure*>(closure));
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
  value::LibraryTypeOf<T>().CountBits(
      value, control.Read(), control.Shape().width, out);
  return out;
}

// The stream of bits a value makes (LRM 6.24.3), laid out in `out` as the
// unsigned integral value of `width` bits the caller states it is.
template <typename Write>
auto StreamInto(void* out, std::int64_t width, bool is_four_state, Write write)
    -> void* {
  value::LoadedWords stream(NumberShape(width, false, is_four_state));
  write(stream.Write(), stream.Shape().width);
  stream.StoreTo(out);
  return out;
}

template <typename T>
auto StreamOf(
    const void* value, std::int64_t width, bool is_four_state, void* out)
    -> void* {
  return StreamInto(
      out, width, is_four_state,
      [&](value::Planes stream, std::uint64_t stream_width) {
        value::LibraryTypeOf<T>().WriteToStream(value, stream, stream_width, 0);
      });
}

// Two contributions folded under the truth table `fold` names, which each
// entry states outright.
template <value::NetResolvable T>
auto Resolved(
    value::NetResolution fold, const void* lhs, const void* rhs, void* out)
    -> void* {
  return Emplace(out, value::Resolve(fold, Read<T>(lhs), Read<T>(rhs)));
}

template <value::NetResolvable T>
auto Dominating(const void* stronger, const void* weaker, void* out) -> void* {
  return Emplace(out, Read<T>(stronger).Dominating(Read<T>(weaker)));
}

// The scalar every bit of a filled value holds (LRM 6.7.1), which crosses as
// the integral value a structure's own operation states it as.
auto FillAt(const void* fill, std::int64_t width, bool is_four_state)
    -> value::Logic {
  return value::Logic::Filled(
      value::LeastSignificantBit(BitsAt(fill, width, is_four_state).Read()));
}

template <value::NetResolvable T>
auto FilledLike(
    const void* prototype, const void* fill, std::int64_t fill_width,
    bool fill_is_four_state, void* out) -> void* {
  return Emplace(
      out,
      T::FilledLike(
          Read<T>(prototype), FillAt(fill, fill_width, fill_is_four_state)));
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
    const value::LoadedWords planes =
        value::LoadedWords::Load(operand.ptr, operand.read_as);
    return value::FormatIntegralOperand(spec, planes.View(), ctx);
  };
  return arg;
}

// A submitted closure runs after the body that built it has returned, so the
// region takes it rather than borrowing it.
auto TakeClosure(void* closure) -> OwnedCall {
  return [held = TakeOwner(closure)] { held->Invoke(); };
}

// A closure kept and handed to a region again each time it is due -- a
// concurrent assertion's action, submitted once per attempt that settles --
// so every submission shares it rather than one of them owning it.
auto ShareClosure(void* closure) -> std::function<void()> {
  return [held = std::shared_ptr<ClosureValue>(TakeOwner(closure))] {
    held->Invoke();
  };
}

// Whether an `iff` qualifier holds (LRM 12.4), which is the whole of what an
// event control reads of it.
struct QualifierTruth {
  bool holds;

  [[nodiscard]] auto IsTruthy() const -> bool {
    return holds;
  }
};

// A closure an observation keeps and runs each time it is asked -- what the
// watched expression is worth now, or whether an `iff` qualifier holds. The
// observation outlives the body that built the closure, so it takes it.
auto TakeCondition(void* closure) {
  return [held = TakeOwner(closure)] {
    return QualifierTruth{.holds = held->RunTruth()};
  };
}

// What an edge event control watches of its expression: the least significant
// bit, which is the whole of what an edge is a transition of (LRM 9.4.2).
struct WatchedBit {
  value::FourStateBit bit;

  [[nodiscard]] auto Lsb() const -> value::FourStateBit {
    return bit;
  }
  [[nodiscard]] auto IsBitIdentical(const WatchedBit& other) const -> bool {
    return bit == other.bit;
  }
};

// An observation of what `closure` answers, which `observe` builds from what
// evaluates it. An edge is a transition of one bit, which is what a body
// watched for one answers (LRM 9.4.2); any other event is a change anywhere in
// a value of whatever type the body answers.
template <typename Observe>
auto ObservingClosure(void* closure, std::int64_t edge, Observe observe)
    -> Observation {
  OwnedClosure held = TakeOwner(closure);
  switch (EventEdgeOf(edge)) {
    case support::EventEdge::kAnyChange:
      return observe(
          [held = std::move(held)] { return held->RunValue(); }, edge);
    case support::EventEdge::kPosedge:
    case support::EventEdge::kNegedge:
    case support::EventEdge::kBothEdges:
      return observe(
          [held = std::move(held)] {
            return WatchedBit{.bit = held->RunWatchedBit()};
          },
          edge);
  }
  throw InternalError("runtime abi: unknown event edge");
}

// The body an LRM 7.12 method runs, as the value layer takes it. The closure is
// borrowed rather than taken: the method runs it to completion before
// returning, so the frame that built it is still alive for the whole walk.
auto ArrayBody(void* body) -> value::ArrayMethodBody {
  return [closure = Read<OwnedClosure>(body).get()](
             const void* item, const void* index) -> value::AnyValue {
    return closure->RunPerElement(item, index);
  };
}

// A value the library holds in storage of its own, moved into the storage the
// call handed over. What is left behind is ended with its holder.
auto AnswerInto(void* out, value::AnyValue answer) -> void* {
  answer.Type().Move(answer.Bytes(), out);
  return out;
}

// A value of one of the library's own kinds, held with its type.
template <typename T>
auto Held(T value) -> value::AnyValue {
  if constexpr (value::IntegralValue<T>) {
    return value::AnyValue::CopyOf(value::IntegralValueType::Of<T>(), &value);
  } else {
    return value::AnyValue::Built(value::LibraryTypeOf<T>(), [&](void* out) {
      std::construct_at(static_cast<T*>(out), std::move(value));
    });
  }
}

// The components of a completion holding the one value `value`.
template <typename T>
auto Single(const T& value) -> std::vector<value::AnyValue> {
  std::vector<value::AnyValue> components;
  components.push_back(Held(value));
  return components;
}

// A call that answers with more than one value completes with the product of
// them, laid out in the storage the caller gave, which already states the
// product's type. Stated once here, so each entry below says only what its own
// components are.
auto EmplaceCompletion(void* out, std::vector<value::AnyValue> components)
    -> void* {
  const std::span<const value::TupleComponent> stated =
      value::RuntimeTuple::TypeAt(out).Components();
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
    component.Type().Move(
        component.Bytes(), value::RuntimeTuple::ComponentAt(out, i));
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
    void* out, const value::String& input, const value::String& format,
    value::detail::NullByte null_byte, const void* prototypes) -> void* {
  const std::span<const value::TupleComponent> stated =
      value::RuntimeTuple::TypeAt(prototypes).Components();
  std::vector<value::AnyValue> components;
  components.reserve(stated.size() + 2);
  components.emplace_back();
  components.emplace_back();
  for (std::size_t i = 0; i < stated.size(); ++i) {
    components.push_back(
        value::AnyValue::CopyOf(
            *stated[i].type, value::RuntimeTuple::ComponentAt(prototypes, i)));
  }
  std::deque<value::LoadedWords> parsed;
  std::vector<value::ScanTarget> targets;
  targets.reserve(stated.size());
  for (std::size_t i = 2; i < components.size(); ++i) {
    value::AnyValue& component = components[i];
    if (const value::IntegralValueType* integral =
            component.Type().AsIntegral()) {
      parsed.push_back(integral->Load(component.Bytes()));
      targets.emplace_back(parsed.back().MutableView());
    } else if (&component.Type() == &lyra_rt_string_value_type) {
      targets.emplace_back(static_cast<value::String*>(component.Bytes()));
    } else {
      throw InternalError(
          "a scan parses into an integral or a string (LRM 21.3.4.3)");
    }
  }

  const value::ScanCount count =
      value::detail::ScanImpl(input, format, null_byte, targets);
  auto planes = parsed.cbegin();
  for (std::size_t i = 2; i < components.size(); ++i) {
    if (components[i].Type().AsIntegral() != nullptr) {
      planes->StoreTo(components[i].Bytes());
      ++planes;
    }
  }
  components[0] = Held(value::Integer::FromInt(count.items));
  components[1] = Held(value::Int::FromInt(count.consumed));
  return EmplaceCompletion(out, std::move(components));
}

// A completion of a value the library types and an integral value of a type
// the caller stated, whose planes are laid out where the product's type puts
// its second component.
template <typename First>
auto EmplaceWithWords(
    void* out, const First& first, const value::LoadedWords& second) -> void* {
  constexpr std::size_t kComponents = 2;
  const std::span<const value::TupleComponent> stated =
      value::RuntimeTuple::TypeAt(out).Components();
  value::AnyValue leading = Held(first);
  if (stated.size() != kComponents ||
      !value::SameType(*stated[0].type, leading.Type())) {
    throw InternalError(
        "a completion is built from components its type does not state -- "
        "please report this as a bug");
  }
  leading.Type().Move(
      leading.Bytes(), value::RuntimeTuple::ComponentAt(out, 0));
  second.StoreTo(value::RuntimeTuple::ComponentAt(out, 1));
  return out;
}

// The SV int a traversal answers with and the index it visited (LRM 7.9.4 --
// 7.9.7), the visited index being the probe itself where the array holds no
// such neighbour.
auto EmplaceVisited(
    void* out, const value::AnyValue* index, const void* probe,
    const void* probe_type) -> void* {
  const bool found = index != nullptr;
  std::vector<value::AnyValue> components;
  components.push_back(Held(value::Int::FromBool(found)));
  components.push_back(found ? *index : OwnedCopy(probe, probe_type));
  return EmplaceCompletion(out, std::move(components));
}

// The least or greatest index an array holds (LRM 20.7), or `unallocated`
// where it holds none, built in `out`.
auto IndexInto(
    void* out, const value::AnyValue* index, const void* unallocated,
    const void* unallocated_type) -> void* {
  if (index == nullptr) {
    return ElementInto(out, TypeAt(unallocated_type), unallocated);
  }
  return ElementInto(out, index->Type(), index->Bytes());
}

// A completion the runtime already assembled as a pair, laid out component by
// component. Which two values they are is the entry's own business -- the
// value drawn and the seed it advanced (LRM 20.14.2), a byte count and the text
// or memory those bytes filled (LRM 21.3.4.2, 21.3.4.4, 21.3.7).
template <typename First, typename Second>
auto EmplaceBoth(void* out, const value::Tuple<First, Second>& completion)
    -> void* {
  std::vector<value::AnyValue> components;
  components.push_back(Held(completion.template Component<0>()));
  components.push_back(Held(completion.template Component<1>()));
  return EmplaceCompletion(out, std::move(components));
}

// The member an enumeration step lands on (LRM 6.19.5.3, 6.19.5.4), laid out
// in `out` at the type of the value stepped from, or that type's default where
// the value is no member.
auto EmplaceEnumerationMember(
    void* out, value::IntegralShape held,
    std::optional<value::ConstPlanes> member) -> void* {
  value::LoadedWords landed(held);
  const value::Planes planes = landed.Write();
  if (member.has_value()) {
    std::ranges::copy(member->value, planes.value.begin());
    std::ranges::copy(member->unknown, planes.unknown.begin());
  } else {
    value::FillDefault(planes, held.width);
  }
  landed.StoreTo(out);
  return out;
}

// The memory tasks (LRM 21.4, 21.5) over the memories this backend holds,
// whose words are integral values where they lie: a token is parsed at the
// type of the word it replaces, and a word is written out through its planes.
// Each memory is read into a copy, which the load answers with.
auto StoreMemoryWord(
    value::MemoryWord word, std::string_view token, unsigned base) -> bool {
  value::LoadedWords parsed = word.type->Load(word.bytes);
  if (!value::FromDigits(
          parsed.Write(), parsed.Shape().width, MemoryRadix(base), token)) {
    return false;
  }
  parsed.StoreTo(word.bytes);
  return true;
}

auto RenderedWord(value::ConstMemoryWord word, unsigned base) -> std::string {
  const value::LoadedWords planes = word.type->Load(word.bytes);
  return RenderedMemoryWord(planes.View(), base);
}

auto ReadUnpackedMemory(
    RuntimeEffects& runtime, const value::RuntimeUnpackedArray& dest,
    const value::String& filename, std::span<const value::UnpackedRange> dims,
    std::int64_t base, std::int64_t start, std::optional<std::int64_t> finish)
    -> value::RuntimeUnpackedArray {
  value::RuntimeUnpackedArray loaded = dest;
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
    RuntimeEffects& runtime, const value::RuntimeUnpackedArray& src,
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
                loaded.ElementType(),
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
                src.ElementType(),
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
        value::LoadedWords planes(index_type.Shape());
        value::FromInt(planes.Write(), index_type.Shape().width, key);
        const value::AnyValue index = value::AnyValue::Built(
            index_type, [&](void* out) { planes.StoreTo(out); });
        value::Formation formed{};
        void* word = loaded.ElementRef(
            value::IndexView{.bytes = index.Bytes(), .type = &index_type},
            formed);
        return StoreMemoryWord(
            value::MemoryWordOf(loaded.ElementType(), word), token, radix);
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
                value::MemoryWordOf(src.ElementType(), word), radix)});
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
  const value::PartsByPosition& parts = PartsOf(type);
  if (parts.Count(level) == 0) {
    throw InternalError(
        "an open array's actual holds an element under each of its unpacked "
        "dimensions (LRM 7.4.2) -- please report this as a bug");
  }
  return ImageElementType(
      parts.At(level, 0), parts.Type(level), dimensions - 1);
}

void FillImage(
    value::DpiOpenArray& image, const void* level, const value::ValueType& type,
    std::size_t dimension, std::size_t dimensions, std::size_t& position) {
  if (dimension == dimensions) {
    const value::LoadedWords planes = ImageLeafType(type).Load(level);
    image.WriteElement(position, planes.Read());
    ++position;
    return;
  }
  const value::PartsByPosition& parts = PartsOf(type);
  const value::ValueType& part = parts.Type(level);
  for (std::size_t p = 0; p < parts.Count(level); ++p) {
    FillImage(
        image, parts.At(level, image.OrdinalAt(dimension, p)), part,
        dimension + 1, dimensions, position);
  }
}

void ReadImageBack(
    const value::DpiOpenArray& image, void* level, const value::ValueType& type,
    std::size_t dimension, std::size_t dimensions, std::size_t& position) {
  if (dimension == dimensions) {
    value::LoadedWords planes = ImageLeafType(type).Load(level);
    image.ReadElement(position, planes.Write());
    planes.StoreTo(level);
    ++position;
    return;
  }
  const value::PartsByPosition& parts = PartsOf(type);
  const value::ValueType& part = parts.Type(level);
  for (std::size_t p = 0; p < parts.Count(level); ++p) {
    ReadImageBack(
        image, parts.RefAt(level, image.OrdinalAt(dimension, p)), part,
        dimension + 1, dimensions, position);
  }
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

// A variable's cell and a history of one layout, behind the addresses the ABI
// carries them as.
template <BitAddressed T>
auto CellAt(void* cell) -> Var<T>& {
  return *static_cast<Var<T>*>(cell);
}

template <BitAddressed T>
auto HistoryAt(void* history) -> SampledHistory<T>& {
  return *static_cast<SampledHistory<T>*>(history);
}

template <BitAddressed T>
auto ValueCellAt(void* cell) -> ActivationValueCell<T>& {
  return *static_cast<ActivationValueCell<T>*>(cell);
}

// A cell the running activation owns, for a value a body holds across a
// suspension.
template <BitAddressed T>
auto AllocateValueCell() noexcept -> void* {
  return GeneratedCallScope::Current()
      .ActivationValues()
      .New<ActivationValueCell<T>>();
}

// A value `width` bits wide of the bytes at `value`, which is what installs a
// holder of one.
template <value::HeldAsWords T>
auto WideOf(std::int64_t width, const void* value) -> T {
  return T(static_cast<std::uint64_t>(width), value);
}

// A procedural local wider than a word, built in `storage` holding the default
// of a type `width` bits wide (LRM Table 6-7). A local has no declaration to
// install it, so it is built at its width, and every store into it is the
// bytes of a value that wide.
template <value::HeldAsWords T>
void BuildWideValueCell(void* storage, std::int64_t width) {
  std::construct_at(static_cast<ActivationValueCell<T>*>(storage))
      ->Install(
          T::Filled(
              static_cast<std::uint64_t>(width),
              value::FourStateBit::kUnknown));
}

// The same for one the activation owns, which lives until the activation ends.
template <value::HeldAsWords T>
auto AllocateWideValueCell(std::int64_t width) -> void* {
  auto* cell = GeneratedCallScope::Current()
                   .ActivationValues()
                   .New<ActivationValueCell<T>>();
  cell->Install(
      T::Filled(
          static_cast<std::uint64_t>(width), value::FourStateBit::kUnknown));
  return cell;
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

// Bits of a designated value of one layout written where they lie (LRM 11.5.1):
// `count` of them at the position `start` names, in a value whose declared
// type has `width` positions. `written` is that value as it stands once the
// bits are in it, which the caller builds, placing bits in a value being
// arithmetic on the value's own type. What is done here is what only the write
// can do: keep the bits it reaches from before it, where anything reads what
// it did, and tell the write which of them moved. A start naming no position
// reaches none, and `written` is then the value as it was.
template <value::IntegralValue T>
void AssignDesignatedBits(
    const void* designation, std::int64_t width, const void* start,
    std::int64_t count, const void* written) {
  const ErasedDesignation& within = DesignationAt(designation);
  T& whole = *static_cast<T*>(within.part);
  const std::optional<std::int64_t> at = PositionNamedAt(start);
  const std::optional<value::BitPositions> reached =
      at.has_value() && within.write->Undecided()
          ? value::Reached(
                static_cast<std::uint64_t>(width),
                static_cast<std::uint64_t>(count), *at)
          : std::nullopt;
  if (!reached.has_value()) {
    whole = Read<T>(written);
    return;
  }
  KeptPart<T> kept(whole, *reached);
  whole = Read<T>(written);
  if (const std::optional<Change> change = kept.ChangeTo(whole)) {
    within.write->Landed(*change);
  }
}

// The same over a designated value wider than a word, which is `width` bits of
// the bytes the designation names.
template <value::HeldAsWords T>
void AssignDesignatedWideBits(
    const void* designation, std::int64_t width, const void* start,
    std::int64_t count, const void* written) {
  const ErasedDesignation& within = DesignationAt(designation);
  const value::WideAt<T::kDomain> whole{
      .bytes = within.part, .width = static_cast<std::uint64_t>(width)};
  const std::optional<std::int64_t> at = PositionNamedAt(start);
  const std::optional<value::BitPositions> reached =
      at.has_value() && within.write->Undecided()
          ? value::Reached(whole.width, static_cast<std::uint64_t>(count), *at)
          : std::nullopt;
  if (!reached.has_value()) {
    std::memcpy(whole.bytes, written, whole.ByteSize());
    return;
  }
  KeptPart<value::WideAt<T::kDomain>> kept(whole, *reached);
  std::memcpy(whole.bytes, written, whole.ByteSize());
  if (const std::optional<Change> change = kept.ChangeTo(whole)) {
    within.write->Landed(*change);
  }
}

// What a comparison answered, as it crosses: the number its scalar is.
template <value::ComparisonAnswer R>
auto Compared(const R& answer) -> std::uint8_t {
  return std::to_underlying(value::AnswerScalar(answer));
}

}  // namespace

}  // namespace lyra::runtime

using lyra::runtime::Activation;
using lyra::runtime::ActivationValueCell;
using lyra::runtime::AdoptObject;
using lyra::runtime::AllocateValueCell;
using lyra::runtime::AssignDesignatedBits;
using lyra::runtime::AssignDesignatedSlice;
using lyra::runtime::Bit16;
using lyra::runtime::Bit32;
using lyra::runtime::Bit64;
using lyra::runtime::Bit8;
using lyra::runtime::BitsAt;
using lyra::runtime::BitWide;
using lyra::runtime::BuildAt;
using lyra::runtime::BuildReference;
using lyra::runtime::CancellationTarget;
using lyra::runtime::CellAt;
using lyra::runtime::ChannelCancellation;
using lyra::runtime::ClosureDefinition;
using lyra::runtime::ClosureValue;
using lyra::runtime::Compared;
using lyra::runtime::ControlEffect;
using lyra::runtime::Coroutine;
using lyra::runtime::current_runtime;
using lyra::runtime::CurrentExportScope;
using lyra::runtime::CurrentForeignProcess;
using lyra::runtime::Delay;
using lyra::runtime::DelayReal;
using lyra::runtime::DesignateElement;
using lyra::runtime::DesignationAt;
using lyra::runtime::DiagnosticDispatcher;
using lyra::runtime::DriveOnForeignStack;
using lyra::runtime::DriverOf;
using lyra::runtime::Emplace;
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
using lyra::runtime::IntegralTypeAt;
using lyra::runtime::IntOf;
using lyra::runtime::JoinNets;
using lyra::runtime::LandDesignation;
using lyra::runtime::LandTupleDesignation;
using lyra::runtime::LeaveCancellationTarget;
using lyra::runtime::Logic16;
using lyra::runtime::Logic32;
using lyra::runtime::Logic64;
using lyra::runtime::Logic8;
using lyra::runtime::LogicWide;
using lyra::runtime::MakeForeignExecution;
using lyra::runtime::MakeSharedCell;
using lyra::runtime::NamedEvent;
using lyra::runtime::NetOf;
using lyra::runtime::NumberAt;
using lyra::runtime::ObjectDefinition;
using lyra::runtime::Observable;
using lyra::runtime::Observation;
using lyra::runtime::ObservationHandles;
using lyra::runtime::ObservingClosure;
using lyra::runtime::OpenCellWrite;
using lyra::runtime::OpenDriverWrite;
using lyra::runtime::OpenRefWrite;
using lyra::runtime::OpenTupleRefWrite;
using lyra::runtime::OpenWrite;
using lyra::runtime::OwnedClosure;
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
using lyra::runtime::RaiseDeclinedDeparture;
using lyra::runtime::Read;
using lyra::runtime::ReadReport;
using lyra::runtime::RealTimeInUnit;
using lyra::runtime::ReceiveDeparture;
using lyra::runtime::RefArmSampling;
using lyra::runtime::ReferElement;
using lyra::runtime::ReferToCell;
using lyra::runtime::ReferToProperty;
using lyra::runtime::ReferToStorage;
using lyra::runtime::ReferToTupleCell;
using lyra::runtime::RefGet;
using lyra::runtime::RefSampledLoad;
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
using lyra::runtime::Scope;
using lyra::runtime::ShareClosure;
using lyra::runtime::SharedPointer;
using lyra::runtime::SimTimeInUnit;
using lyra::runtime::SpawnAll;
using lyra::runtime::STimeInUnit;
using lyra::runtime::TakeBranches;
using lyra::runtime::TakeClosure;
using lyra::runtime::TakeCondition;
using lyra::runtime::TakeOwner;
using lyra::runtime::TestPlusargs;
using lyra::runtime::Trigger;
using lyra::runtime::TriggerHandles;
using lyra::runtime::TupleRefArmSampling;
using lyra::runtime::TupleRefSampledLoad;
using lyra::runtime::TupleRefSet;
using lyra::runtime::Var;
using lyra::runtime::ViewOf;
using lyra::runtime::Wait;
using lyra::runtime::WaitFork;
using lyra::runtime::WaitOn;
using lyra::runtime::WaitOnImplicitList;
using lyra::runtime::WaitRecollecting;
using lyra::runtime::WaitUntil;
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
using lyra::value::MakeFormatArg;
using lyra::value::NetResolution;
using lyra::value::ObjectRef;
using lyra::value::PrintItem;
using lyra::value::PrintLiteralItem;
using lyra::value::PrintValueItem;
using lyra::value::Real;
using lyra::value::RuntimeAssociativeArray;
using lyra::value::RuntimeDynamicArray;
using lyra::value::RuntimeQueue;
using lyra::value::RuntimeTaggedUnion;
using lyra::value::RuntimeTuple;
using lyra::value::RuntimeUnion;
using lyra::value::RuntimeUnpackedArray;
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

auto lyra_rt_file_gets(void* files, std::int64_t fd, void* out) -> void* {
  return lyra::runtime::EmplaceBoth(
      out, static_cast<FileTable*>(files)->Gets(fd));
}

auto lyra_rt_file_error(void* files, std::int64_t fd, void* out) -> void* {
  return lyra::runtime::EmplaceBoth(
      out, static_cast<FileTable*>(files)->Error(fd));
}

auto lyra_rt_file_read(
    void* files, const void* dest, std::int64_t dest_width, bool dest_is_signed,
    bool dest_is_four_state, std::int64_t fd, void* out) -> void* {
  lyra::value::LoadedWords read_into =
      NumberAt(dest, dest_width, dest_is_signed, dest_is_four_state);
  const lyra::value::Int read =
      static_cast<FileTable*>(files)->ReadInto(read_into.MutableView(), fd);
  return lyra::runtime::EmplaceWithWords(out, read, read_into);
}

auto lyra_rt_file_read_memory(
    void* files, const void* dest, std::int64_t fd, LyraSpan bounds,
    std::int64_t start, std::int64_t count, void* out) -> void* {
  lyra::value::RuntimeUnpackedArray memory =
      Read<lyra::value::RuntimeUnpackedArray>(dest);
  const std::vector<UnpackedRange> dims =
      lyra::value::UnpackedRangesOf(lyra::runtime::MachineIntsOf(bounds));
  const UnpackedRange range = dims.front();
  const lyra::value::IntegralValueType& word_type =
      *lyra::value::MemoryWordOf(memory.ElementType(), memory.ElementDefault())
           .type;
  const std::int32_t read = lyra::runtime::ReadMemoryWords(
      *static_cast<FileTable*>(files), fd, word_type.Shape().width, range,
      start, count, [&](std::int64_t sv, std::span<const char> bytes) {
        lyra::value::LoadedWords word(word_type.Shape());
        lyra::runtime::ReadBigEndian(word.MutableView(), bytes);
        word.StoreTo(lyra::value::MemoryLeaf(memory, dims, sv, 0).bytes);
      });
  std::vector<lyra::value::AnyValue> components;
  components.push_back(lyra::runtime::Held(lyra::value::Int::FromInt(read)));
  components.push_back(lyra::runtime::Held(std::move(memory)));
  return lyra::runtime::EmplaceCompletion(out, std::move(components));
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

auto lyra_rt_enter_coroutine_borrowed_environment(
    void* frame, void* out) noexcept -> void* {
  return lyra::runtime::StartGeneratedProcess(
      out, lyra::runtime::GeneratedBody(frame, nullptr));
}

// The site holds only the closure's owner, which generated code cannot see
// through, so the frame is built here, and the owner is kept for as long as
// that frame can run.
auto lyra_rt_enter_coroutine_owned_environment(
    void* closure, void* out) noexcept -> void* {
  OwnedClosure captures = TakeOwner(closure);
  void* frame = captures->Start();
  return lyra::runtime::StartGeneratedProcess(
      out, lyra::runtime::GeneratedBody(frame, std::move(captures)));
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

auto lyra_rt_closure_make(const void* definition, void* out) -> void* {
  OwnedClosure* owner = std::construct_at(
      static_cast<OwnedClosure*>(out),
      ClosureValue::Make(static_cast<const ClosureDefinition*>(definition)));
  return owner->get();
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

auto lyra_rt_tuple_shared_cell_make(void* out) -> void* {
  return MakeSharedCell<RuntimeTuple>(out);
}

auto lyra_rt_union_shared_cell_make(void* out) -> void* {
  return MakeSharedCell<RuntimeUnion>(out);
}

auto lyra_rt_tagged_union_shared_cell_make(void* out) -> void* {
  return MakeSharedCell<RuntimeTaggedUnion>(out);
}

auto lyra_rt_dynarray_shared_cell_make(void* out) -> void* {
  return MakeSharedCell<RuntimeDynamicArray>(out);
}

auto lyra_rt_unpackedarray_shared_cell_make(void* out) -> void* {
  return MakeSharedCell<RuntimeUnpackedArray>(out);
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
  static_cast<RuntimeEffects*>(runtime)->SubmitNba(TakeClosure(closure));
}

void lyra_rt_submit_nba_after(
    void* runtime, const void* duration, std::int64_t duration_width,
    bool duration_is_signed, bool duration_is_four_state,
    std::int64_t unit_power, std::int64_t precision_power, void* closure) {
  const lyra::value::LoadedWords amount = NumberAt(
      duration, duration_width, duration_is_signed, duration_is_four_state);
  static_cast<RuntimeEffects*>(runtime)->SubmitNbaAfter(
      amount.View(), unit_power, precision_power, TakeClosure(closure));
}

void lyra_rt_submit_nba_after_real(
    void* runtime, const void* duration, std::int64_t unit_power,
    std::int64_t precision_power, void* closure) {
  static_cast<RuntimeEffects*>(runtime)->SubmitNbaAfterReal(
      Read<Real>(duration), unit_power, precision_power, TakeClosure(closure));
}

void lyra_rt_run_detached(void* runtime, void* carrier) {
  static_cast<RuntimeEffects*>(runtime)->RunDetached(
      std::move(*static_cast<Coroutine<void>*>(carrier)));
}

void lyra_rt_submit_postponed(void* runtime, void* closure) {
  static_cast<RuntimeEffects*>(runtime)->SubmitPostponed(TakeClosure(closure));
}

void lyra_rt_submit_observed(void* runtime, void* closure) {
  static_cast<RuntimeEffects*>(runtime)->SubmitObserved(TakeClosure(closure));
}

void lyra_rt_submit_violation_report(void* runtime, void* closure) {
  static_cast<RuntimeEffects*>(runtime)->SubmitViolationReport(
      TakeClosure(closure));
}

void lyra_rt_submit_deferred_observed(void* runtime, void* closure) {
  static_cast<RuntimeEffects*>(runtime)->SubmitDeferredObserved(
      TakeClosure(closure));
}

void lyra_rt_submit_deferred_final(void* runtime, void* closure) {
  static_cast<RuntimeEffects*>(runtime)->SubmitDeferredFinal(
      TakeClosure(closure));
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

// What crosses is the cell's own address -- a variable, a net, a named event --
// and a `void*` carries no type to adjust by, so it is read here as the address
// of what waits on that cell. Every such cell names `Observable` as its first
// base, which is what makes the two addresses one under the platform ABI.
auto lyra_rt_make_trigger(
    void* observable, const void* observation, std::int64_t lsb_bit_offset,
    std::int64_t bit_width, void* out) -> void* {
  return Emplace(
      out, Trigger(
               static_cast<Observable*>(observable),
               Read<Observation>(observation), lsb_bit_offset, bit_width));
}

auto lyra_rt_observation_on_reaching(void* out) -> void* {
  return Emplace(out, Observation::OnReaching());
}

auto lyra_rt_observation_of_value(
    void* expression, std::int64_t edge, void* out) -> void* {
  return Emplace(
      out, ObservingClosure(
               expression, edge, [](auto evaluate, std::int64_t stated) {
                 return Observation::OfValue(std::move(evaluate), stated);
               }));
}

auto lyra_rt_observation_of_value_qualified(
    void* expression, std::int64_t edge, void* condition, void* out) -> void* {
  return Emplace(
      out, ObservingClosure(
               expression, edge, [&](auto evaluate, std::int64_t stated) {
                 return Observation::OfValueQualified(
                     std::move(evaluate), stated, TakeCondition(condition));
               }));
}

auto lyra_rt_observation_qualified(void* condition, void* out) -> void* {
  return Emplace(out, Observation::Qualified(TakeCondition(condition)));
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
    void* report, void* place, std::int64_t lsb_bit_offset,
    std::int64_t bit_width) {
  static_cast<ReadReport*>(report)->Add(
      static_cast<Observable*>(place), lsb_bit_offset, bit_width);
}

void lyra_rt_read_report_add_through_handle(
    void* report, void* place, std::int64_t lsb_bit_offset,
    std::int64_t bit_width) {
  static_cast<ReadReport*>(report)->AddThroughHandle(
      static_cast<Observable*>(place), lsb_bit_offset, bit_width);
}

void lyra_rt_read_report_enter_call_on_handle(void* report) {
  static_cast<ReadReport*>(report)->EnterCallOnHandle();
}

void lyra_rt_read_report_leave_call_on_handle(void* report) {
  static_cast<ReadReport*>(report)->LeaveCallOnHandle();
}

void lyra_rt_read_report_add_every_object(void* report) {
  static_cast<ReadReport*>(report)->AddEveryObject();
}

void lyra_rt_read_report_add_write(
    void* report, void* place, std::int64_t lsb_bit_offset,
    std::int64_t bit_width) {
  static_cast<ReadReport*>(report)->AddWrite(
      static_cast<Observable*>(place), lsb_bit_offset, bit_width);
}

void lyra_rt_read_report_settle_as_implicit_list(void* report) {
  static_cast<ReadReport*>(report)->SettleAsImplicitList();
}

auto lyra_rt_read_report_enter(void* report) -> std::int64_t {
  return static_cast<ReadReport*>(report)->Enter();
}

void lyra_rt_read_report_leave(void* report) {
  static_cast<ReadReport*>(report)->Leave();
}

auto lyra_rt_read_report_runs_the_body(const void* report) -> std::int64_t {
  return static_cast<const ReadReport*>(report)->RunsTheBody();
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

void lyra_rt_finish(void* runtime, const void* origin, std::int64_t level) {
  Finish(*static_cast<RuntimeEffects*>(runtime), Read<String>(origin), level);
}

void lyra_rt_stop(void* runtime, const void* origin, std::int64_t level) {
  Stop(*static_cast<RuntimeEffects*>(runtime), Read<String>(origin), level);
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
    bool destination_is_four_state, void* out) -> void* {
  lyra::value::LoadedWords written = NumberAt(
      destination, destination_width, destination_is_signed,
      destination_is_four_state);
  const lyra::value::Int matched = lyra::runtime::ValuePlusargsInto(
      *static_cast<RuntimeEffects*>(runtime), Read<String>(user_string),
      written.MutableView());
  return lyra::runtime::EmplaceWithWords(out, matched, written);
}

auto lyra_rt_string_value_plusargs(
    void* runtime, const void* user_string, const void* destination, void* out)
    -> void* {
  return lyra::runtime::EmplaceBoth(
      out, ValuePlusargs(
               *static_cast<RuntimeEffects*>(runtime),
               Read<String>(user_string), Read<String>(destination)));
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
    std::int64_t seed, std::int64_t start, std::int64_t end, void* out)
    -> void* {
  return lyra::runtime::EmplaceBoth(
      out, lyra::runtime::DistUniform(seed, start, end));
}

auto lyra_rt_dist_normal(
    std::int64_t seed, std::int64_t mean, std::int64_t standard_deviation,
    void* out) -> void* {
  return lyra::runtime::EmplaceBoth(
      out, lyra::runtime::DistNormal(seed, mean, standard_deviation));
}

auto lyra_rt_dist_exponential(std::int64_t seed, std::int64_t mean, void* out)
    -> void* {
  return lyra::runtime::EmplaceBoth(
      out, lyra::runtime::DistExponential(seed, mean));
}

auto lyra_rt_dist_poisson(std::int64_t seed, std::int64_t mean, void* out)
    -> void* {
  return lyra::runtime::EmplaceBoth(
      out, lyra::runtime::DistPoisson(seed, mean));
}

auto lyra_rt_dist_chi_square(
    std::int64_t seed, std::int64_t degrees_of_freedom, void* out) -> void* {
  return lyra::runtime::EmplaceBoth(
      out, lyra::runtime::DistChiSquare(seed, degrees_of_freedom));
}

auto lyra_rt_dist_t(
    std::int64_t seed, std::int64_t degrees_of_freedom, void* out) -> void* {
  return lyra::runtime::EmplaceBoth(
      out, lyra::runtime::DistT(seed, degrees_of_freedom));
}

auto lyra_rt_dist_erlang(
    std::int64_t seed, std::int64_t stages, std::int64_t mean, void* out)
    -> void* {
  return lyra::runtime::EmplaceBoth(
      out, lyra::runtime::DistErlang(seed, stages, mean));
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

void lyra_rt_enter_scope_static_init(void* runtime, void* unit_instance) {
  EnterScopeStaticInit(
      *static_cast<RuntimeEffects*>(runtime),
      static_cast<Scope*>(unit_instance));
}

void lyra_rt_enter_namespace_static_init(void* runtime) {
  EnterNamespaceStaticInit(*static_cast<RuntimeEffects*>(runtime));
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
  auto& effects = *static_cast<RuntimeEffects*>(runtime);
  return !EnterForeignTask(
      effects, effects.CurrentProcess().CurrentLeaf(),
      MakeForeignExecution(TakeClosure(closure)));
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

auto lyra_rt_enclosing_scope(void* self, const void* definition) -> void* {
  return static_cast<Scope*>(self)->EnclosingScope(
      static_cast<const ObjectDefinition*>(definition));
}

auto lyra_rt_is_of_class(void* self, const void* definition) -> bool {
  return static_cast<Scope*>(self)->IsOfClass(
      static_cast<const ObjectDefinition*>(definition));
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
  return Read<Enumeration>(enumeration).PositionOf(asked.Read()).has_value()
             ? 1
             : 0;
}

auto lyra_rt_enumeration_name(
    const void* enumeration, const void* value, std::int64_t value_width,
    bool value_is_four_state, void* out) -> void* {
  const lyra::value::LoadedWords asked =
      BitsAt(value, value_width, value_is_four_state);
  return Emplace(out, Read<Enumeration>(enumeration).NameOf(asked.Read()));
}

auto lyra_rt_enumeration_next(
    const void* enumeration, const void* value, std::int64_t value_width,
    bool value_is_four_state, std::int64_t count, void* out) -> void* {
  const lyra::value::LoadedWords from =
      BitsAt(value, value_width, value_is_four_state);
  return lyra::runtime::EmplaceEnumerationMember(
      out, from.Shape(),
      Read<Enumeration>(enumeration).MemberAfter(from.Read(), count));
}

auto lyra_rt_enumeration_prev(
    const void* enumeration, const void* value, std::int64_t value_width,
    bool value_is_four_state, std::int64_t count, void* out) -> void* {
  const lyra::value::LoadedWords from =
      BitsAt(value, value_width, value_is_four_state);
  return lyra::runtime::EmplaceEnumerationMember(
      out, from.Shape(),
      Read<Enumeration>(enumeration).MemberBefore(from.Read(), count));
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

auto lyra_rt_refer_storage(void* storage, void* out) -> void* {
  return ReferToStorage(storage, out);
}

auto lyra_rt_refer_property(void* object, void* storage, void* out) -> void* {
  return ReferToProperty(object, storage, out);
}

auto lyra_rt_reference_reports_to(const void* reference) -> void* {
  return static_cast<const ErasedReference*>(reference)->ReportsTo();
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
auto lyra_rt_tuple_cell_refer(void* cell, void* out) -> void* {
  return ReferToTupleCell(cell, out);
}
auto lyra_rt_union_cell_refer(void* cell, void* out) -> void* {
  return ReferToCell<RuntimeUnion>(cell, out);
}
auto lyra_rt_tagged_union_cell_refer(void* cell, void* out) -> void* {
  return ReferToCell<RuntimeTaggedUnion>(cell, out);
}
auto lyra_rt_dynarray_cell_refer(void* cell, void* out) -> void* {
  return ReferToCell<RuntimeDynamicArray>(cell, out);
}
auto lyra_rt_unpackedarray_cell_refer(void* cell, void* out) -> void* {
  return ReferToCell<RuntimeUnpackedArray>(cell, out);
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
auto lyra_rt_unpackedarray_refer_element(
    const void* reference, const void* position, void* out) -> void* {
  return ReferElement<RuntimeUnpackedArray>(
      reference, PositionNamedAt(position), out);
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
auto lyra_rt_tuple_refer_component(
    const void* reference, std::int64_t index, void* out) -> void* {
  const ErasedReference& from = ErasedAt(reference);
  return BuildReference(
      out, from.Part(
               RuntimeTuple::ComponentAt(
                   from.storage, static_cast<std::size_t>(index)),
               lyra::value::Formation::kExisting));
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

auto lyra_rt_string_ref_sampled_load(void* reference, void* out) -> void* {
  return RefSampledLoad<String>(reference, out);
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

auto lyra_rt_real_ref_sampled_load(void* reference, void* out) -> void* {
  return RefSampledLoad<Real>(reference, out);
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

auto lyra_rt_shortreal_ref_sampled_load(void* reference, void* out) -> void* {
  return RefSampledLoad<ShortReal>(reference, out);
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

auto lyra_rt_chandle_ref_sampled_load(void* reference, void* out) -> void* {
  return RefSampledLoad<Chandle>(reference, out);
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

auto lyra_rt_managedref_ref_sampled_load(void* reference, void* out) -> void* {
  return RefSampledLoad<ObjectRef>(reference, out);
}

auto lyra_rt_tuple_ref_get(void* reference) -> const void* {
  return ErasedAt(reference).storage;
}

void lyra_rt_tuple_ref_set(void* reference, const void* value) {
  TupleRefSet(reference, value);
}

void lyra_rt_tuple_ref_arm_sampling(void* reference) {
  TupleRefArmSampling(reference);
}

auto lyra_rt_tuple_ref_sampled_load(void* reference, void* out) -> void* {
  return TupleRefSampledLoad(reference, out);
}

auto lyra_rt_union_ref_get(void* reference) -> const void* {
  return RefGet<RuntimeUnion>(reference);
}

void lyra_rt_union_ref_set(void* reference, const void* value) {
  RefSet<RuntimeUnion>(reference, value);
}

void lyra_rt_union_ref_arm_sampling(void* reference) {
  RefArmSampling<RuntimeUnion>(reference);
}

auto lyra_rt_union_ref_sampled_load(void* reference, void* out) -> void* {
  return RefSampledLoad<RuntimeUnion>(reference, out);
}

auto lyra_rt_tagged_union_ref_get(void* reference) -> const void* {
  return RefGet<RuntimeTaggedUnion>(reference);
}

void lyra_rt_tagged_union_ref_set(void* reference, const void* value) {
  RefSet<RuntimeTaggedUnion>(reference, value);
}

void lyra_rt_tagged_union_ref_arm_sampling(void* reference) {
  RefArmSampling<RuntimeTaggedUnion>(reference);
}

auto lyra_rt_tagged_union_ref_sampled_load(void* reference, void* out)
    -> void* {
  return RefSampledLoad<RuntimeTaggedUnion>(reference, out);
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

auto lyra_rt_dynarray_ref_sampled_load(void* reference, void* out) -> void* {
  return RefSampledLoad<RuntimeDynamicArray>(reference, out);
}

auto lyra_rt_unpackedarray_ref_get(void* reference) -> const void* {
  return RefGet<RuntimeUnpackedArray>(reference);
}

void lyra_rt_unpackedarray_ref_set(void* reference, const void* value) {
  RefSet<RuntimeUnpackedArray>(reference, value);
}

void lyra_rt_unpackedarray_ref_arm_sampling(void* reference) {
  RefArmSampling<RuntimeUnpackedArray>(reference);
}

auto lyra_rt_unpackedarray_ref_sampled_load(void* reference, void* out)
    -> void* {
  return RefSampledLoad<RuntimeUnpackedArray>(reference, out);
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

auto lyra_rt_queue_ref_sampled_load(void* reference, void* out) -> void* {
  return RefSampledLoad<RuntimeQueue>(reference, out);
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

auto lyra_rt_assocarray_ref_sampled_load(void* reference, void* out) -> void* {
  return RefSampledLoad<RuntimeAssociativeArray>(reference, out);
}

auto lyra_rt_string_cell_get(void* cell) -> const void* {
  return &static_cast<Var<String>*>(cell)->Get();
}

void lyra_rt_string_cell_initialize(
    void* cell, const void* prototype) noexcept {
  static_cast<Var<String>*>(cell)->Initialize(Read<String>(prototype));
}

void lyra_rt_string_cell_set(void* cell, const void* value) {
  static_cast<Var<String>*>(cell)->Set(Read<String>(value));
}

void lyra_rt_string_cell_arm_sampling(void* cell) {
  static_cast<Var<String>*>(cell)->ArmSampling();
}

auto lyra_rt_string_cell_sampled_load(void* cell, void* out) -> void* {
  return Emplace(out, static_cast<Var<String>*>(cell)->SampledGet());
}

auto lyra_rt_real_cell_get(void* cell) -> const void* {
  return &static_cast<Var<Real>*>(cell)->Get();
}

void lyra_rt_real_cell_initialize(void* cell, const void* prototype) noexcept {
  static_cast<Var<Real>*>(cell)->Initialize(Read<Real>(prototype));
}

void lyra_rt_real_cell_set(void* cell, const void* value) {
  static_cast<Var<Real>*>(cell)->Set(Read<Real>(value));
}

void lyra_rt_real_cell_arm_sampling(void* cell) {
  static_cast<Var<Real>*>(cell)->ArmSampling();
}

auto lyra_rt_real_cell_sampled_load(void* cell, void* out) -> void* {
  return Emplace(out, static_cast<Var<Real>*>(cell)->SampledGet());
}

auto lyra_rt_shortreal_cell_get(void* cell) -> const void* {
  return &static_cast<Var<ShortReal>*>(cell)->Get();
}

void lyra_rt_shortreal_cell_initialize(
    void* cell, const void* prototype) noexcept {
  static_cast<Var<ShortReal>*>(cell)->Initialize(Read<ShortReal>(prototype));
}

void lyra_rt_shortreal_cell_set(void* cell, const void* value) {
  static_cast<Var<ShortReal>*>(cell)->Set(Read<ShortReal>(value));
}

void lyra_rt_shortreal_cell_arm_sampling(void* cell) {
  static_cast<Var<ShortReal>*>(cell)->ArmSampling();
}

auto lyra_rt_shortreal_cell_sampled_load(void* cell, void* out) -> void* {
  return Emplace(out, static_cast<Var<ShortReal>*>(cell)->SampledGet());
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

void lyra_rt_tuple_sampled_history_install(
    void* history, const void* default_value, std::int64_t depth) {
  static_cast<SampledHistory<RuntimeTuple>*>(history)->Install(
      Read<RuntimeTuple>(default_value), depth);
}

void lyra_rt_tuple_sampled_history_push(void* history, const void* value) {
  static_cast<SampledHistory<RuntimeTuple>*>(history)->Push(
      Read<RuntimeTuple>(value));
}

auto lyra_rt_tuple_sampled_history_at(
    const void* history, std::int64_t ticks_back, void* out) -> void* {
  return Emplace(
      out, static_cast<const SampledHistory<RuntimeTuple>*>(history)->At(
               ticks_back));
}

void lyra_rt_union_sampled_history_install(
    void* history, const void* default_value, std::int64_t depth) {
  static_cast<SampledHistory<RuntimeUnion>*>(history)->Install(
      Read<RuntimeUnion>(default_value), depth);
}

void lyra_rt_union_sampled_history_push(void* history, const void* value) {
  static_cast<SampledHistory<RuntimeUnion>*>(history)->Push(
      Read<RuntimeUnion>(value));
}

auto lyra_rt_union_sampled_history_at(
    const void* history, std::int64_t ticks_back, void* out) -> void* {
  return Emplace(
      out, static_cast<const SampledHistory<RuntimeUnion>*>(history)->At(
               ticks_back));
}

void lyra_rt_tagged_union_sampled_history_install(
    void* history, const void* default_value, std::int64_t depth) {
  static_cast<SampledHistory<RuntimeTaggedUnion>*>(history)->Install(
      Read<RuntimeTaggedUnion>(default_value), depth);
}

void lyra_rt_tagged_union_sampled_history_push(
    void* history, const void* value) {
  static_cast<SampledHistory<RuntimeTaggedUnion>*>(history)->Push(
      Read<RuntimeTaggedUnion>(value));
}

auto lyra_rt_tagged_union_sampled_history_at(
    const void* history, std::int64_t ticks_back, void* out) -> void* {
  return Emplace(
      out, static_cast<const SampledHistory<RuntimeTaggedUnion>*>(history)->At(
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

void lyra_rt_unpackedarray_sampled_history_install(
    void* history, const void* default_value, std::int64_t depth) {
  static_cast<SampledHistory<RuntimeUnpackedArray>*>(history)->Install(
      Read<RuntimeUnpackedArray>(default_value), depth);
}

void lyra_rt_unpackedarray_sampled_history_push(
    void* history, const void* value) {
  static_cast<SampledHistory<RuntimeUnpackedArray>*>(history)->Push(
      Read<RuntimeUnpackedArray>(value));
}

auto lyra_rt_unpackedarray_sampled_history_at(
    const void* history, std::int64_t ticks_back, void* out) -> void* {
  return Emplace(
      out,
      static_cast<const SampledHistory<RuntimeUnpackedArray>*>(history)->At(
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
      ShareClosure(pass_action), ShareClosure(fail_action));
}

void lyra_rt_evaluation_attempts_seed_word(
    void* attempts, std::uint64_t word, std::uint64_t bits) {
  static_cast<EvaluationAttempts*>(attempts)->SeedWord(word, bits);
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

// A procedural local whose value crosses a suspension. The cell is allocated in
// the running execution's own value store, so the handle the generated frame
// carries across a suspension points at storage that outlives every stretch of
// that body. A store overwrites the cell in place -- the first store installs
// the declared representation -- and a load answers with the value where the
// cell holds it. A procedural local is not observable, so no runtime handle
// threads through and no subscriber wakes.
auto lyra_rt_string_value_cell_alloc() noexcept -> void* {
  return GeneratedCallScope::Current()
      .ActivationValues()
      .New<ActivationValueCell<String>>();
}

void lyra_rt_string_value_cell_store(void* cell, const void* value) noexcept {
  static_cast<ActivationValueCell<String>*>(cell)->Store(Read<String>(value));
}

auto lyra_rt_string_value_cell_load(void* cell) noexcept -> void* {
  return &static_cast<ActivationValueCell<String>*>(cell)->Storage();
}

// The guarded value is handed back rather than copied: what crosses here is the
// handle the caller already holds, and a guard that let the access through has
// changed nothing about it.
auto lyra_rt_require(
    void* value, const void* condition, std::int64_t condition_width,
    bool condition_is_four_state, const char* message) -> void* {
  const lyra::value::LoadedWords asked =
      BitsAt(condition, condition_width, condition_is_four_state);
  lyra::value::RequireCondition(
      lyra::value::Truth(asked.Read()) ==
          lyra::value::Truthiness::kKnownNonzero,
      message);
  return value;
}

auto lyra_rt_string_from_bits(
    const void* bits, std::int64_t bits_width, bool bits_are_four_state,
    void* out) -> void* {
  const lyra::value::LoadedWords planes =
      BitsAt(bits, bits_width, bits_are_four_state);
  return Emplace(out, String::FromIntegral(planes.View()));
}

auto lyra_rt_string_from_byte_array(const void* bytes, void* out) -> void* {
  return Emplace(out, Read<RuntimeUnpackedArray>(bytes).ToByteString());
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

auto lyra_rt_string_element(const void* value, const void* position, void* out)
    -> void* {
  return Emplace(
      out, Read<String>(value).Element(Read<lyra::value::Position>(position)));
}

// The character write (LRM 6.16.2): a new string with the character at
// `position` replaced. A character is a view of its string rather than storage
// of its own, so a write to one rebuilds the string, which is then stored
// whole.
auto lyra_rt_string_with_element(
    const void* value, const void* position, const void* replacement,
    std::int64_t replacement_width, bool replacement_is_signed,
    bool replacement_is_four_state, void* out) -> void* {
  String written = Read<String>(value);
  written.PutCharacter(
      Read<lyra::value::Position>(position),
      IntOf(NumberAt(
          replacement, replacement_width, replacement_is_signed,
          replacement_is_four_state)));
  return Emplace(out, std::move(written));
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
    const void* input, const void* format, const void* prototypes, void* out)
    -> void* {
  return lyra::runtime::EmplaceScan(
      out, Read<String>(input), Read<String>(format),
      lyra::value::detail::NullByte::kWhiteSpace, prototypes);
}

auto lyra_rt_string_scan_file(
    const void* input, const void* format, const void* prototypes, void* out)
    -> void* {
  return lyra::runtime::EmplaceScan(
      out, Read<String>(input), Read<String>(format),
      lyra::value::detail::NullByte::kOrdinary, prototypes);
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
  return GeneratedCallScope::Current()
      .ActivationValues()
      .New<ActivationValueCell<Real>>();
}

void lyra_rt_real_value_cell_store(void* cell, const void* value) noexcept {
  static_cast<ActivationValueCell<Real>*>(cell)->Store(Read<Real>(value));
}

auto lyra_rt_real_value_cell_load(void* cell) noexcept -> void* {
  return &static_cast<ActivationValueCell<Real>*>(cell)->Storage();
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
  return GeneratedCallScope::Current()
      .ActivationValues()
      .New<ActivationValueCell<ShortReal>>();
}

void lyra_rt_shortreal_value_cell_store(
    void* cell, const void* value) noexcept {
  static_cast<ActivationValueCell<ShortReal>*>(cell)->Store(
      Read<ShortReal>(value));
}

auto lyra_rt_shortreal_value_cell_load(void* cell) noexcept -> void* {
  return &static_cast<ActivationValueCell<ShortReal>*>(cell)->Storage();
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
  return GeneratedCallScope::Current()
      .ActivationValues()
      .New<ActivationValueCell<Chandle>>();
}

void lyra_rt_chandle_value_cell_store(void* cell, const void* value) noexcept {
  static_cast<ActivationValueCell<Chandle>*>(cell)->Store(Read<Chandle>(value));
}

auto lyra_rt_chandle_value_cell_load(void* cell) noexcept -> void* {
  return &static_cast<ActivationValueCell<Chandle>*>(cell)->Storage();
}

// A chandle variable, which a process may wait on: LRM 9.4.2 makes a write to
// one an event whenever the pointer it holds is not the pointer it held.
auto lyra_rt_chandle_cell_get(void* cell) -> const void* {
  return &static_cast<Var<Chandle>*>(cell)->Get();
}

void lyra_rt_chandle_cell_initialize(
    void* cell, const void* prototype) noexcept {
  static_cast<Var<Chandle>*>(cell)->Initialize(Read<Chandle>(prototype));
}

void lyra_rt_chandle_cell_set(void* cell, const void* value) {
  static_cast<Var<Chandle>*>(cell)->Set(Read<Chandle>(value));
}

void lyra_rt_chandle_cell_arm_sampling(void* cell) {
  static_cast<Var<Chandle>*>(cell)->ArmSampling();
}

auto lyra_rt_chandle_cell_sampled_load(void* cell, void* out) -> void* {
  return Emplace(out, static_cast<Var<Chandle>*>(cell)->SampledGet());
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
  return GeneratedCallScope::Current()
      .ActivationValues()
      .New<ActivationValueCell<ObjectRef>>();
}

// Storing copies the handle's share of ownership with it, which is what keeps
// the object alive once the handle it was stored from has ended; a reader that
// keeps what it loaded copies it, and so owns a share of its own.
void lyra_rt_managedref_value_cell_store(
    void* cell, const void* value) noexcept {
  static_cast<ActivationValueCell<ObjectRef>*>(cell)->Store(
      Read<ObjectRef>(value));
}

auto lyra_rt_managedref_value_cell_load(void* cell) noexcept -> void* {
  return &static_cast<ActivationValueCell<ObjectRef>*>(cell)->Storage();
}

// A variable of class type, which a process may wait on: LRM 9.4.2 makes a
// write to one an event whenever the object it names is not the object it
// named. A store keeps the handle's share of ownership and a load hands one
// back, so the object outlives every body that touches the variable.
auto lyra_rt_managedref_cell_get(void* cell) -> const void* {
  return &static_cast<Var<ObjectRef>*>(cell)->Get();
}

void lyra_rt_managedref_cell_initialize(
    void* cell, const void* prototype) noexcept {
  static_cast<Var<ObjectRef>*>(cell)->Initialize(Read<ObjectRef>(prototype));
}

void lyra_rt_managedref_cell_set(void* cell, const void* value) {
  static_cast<Var<ObjectRef>*>(cell)->Set(Read<ObjectRef>(value));
}

// Arming keeps a share of whatever the variable names at the moment it is
// armed, and every later slot the variable moves away from replaces it, so the
// object a sampled read answers with is alive for as long as that read can
// happen (LRM 8.4, 16.5.1).
void lyra_rt_managedref_cell_arm_sampling(void* cell) {
  static_cast<Var<ObjectRef>*>(cell)->ArmSampling();
}

auto lyra_rt_managedref_cell_sampled_load(void* cell, void* out) -> void* {
  return Emplace(out, static_cast<Var<ObjectRef>*>(cell)->SampledGet());
}

auto lyra_rt_tuple_cell_get(void* cell) -> const void* {
  return HandleTo(static_cast<Var<RuntimeTuple>*>(cell)->Get());
}

void lyra_rt_tuple_cell_initialize(void* cell, const void* prototype) noexcept {
  static_cast<Var<RuntimeTuple>*>(cell)->Initialize(
      Read<RuntimeTuple>(prototype));
}

void lyra_rt_tuple_cell_set(void* cell, const void* value) {
  static_cast<Var<RuntimeTuple>*>(cell)->Set(Read<RuntimeTuple>(value));
}

void lyra_rt_tuple_cell_arm_sampling(void* cell) {
  static_cast<Var<RuntimeTuple>*>(cell)->ArmSampling();
}

auto lyra_rt_tuple_cell_sampled_load(void* cell, void* out) -> void* {
  return Emplace(out, static_cast<Var<RuntimeTuple>*>(cell)->SampledGet());
}

auto lyra_rt_tuple_value_cell_alloc() noexcept -> void* {
  return GeneratedCallScope::Current()
      .ActivationValues()
      .New<ActivationValueCell<RuntimeTuple>>();
}

void lyra_rt_tuple_value_cell_store(void* cell, const void* value) noexcept {
  static_cast<ActivationValueCell<RuntimeTuple>*>(cell)->Store(
      Read<RuntimeTuple>(value));
}

auto lyra_rt_tuple_value_cell_load(void* cell) noexcept -> void* {
  return HandleTo(
      static_cast<ActivationValueCell<RuntimeTuple>*>(cell)->Storage());
}

auto lyra_rt_union_make(
    std::int64_t index, const void* value, const void* value_type, void* out)
    -> void* {
  return Emplace(
      out, RuntimeUnion(
               static_cast<std::size_t>(index),
               lyra::runtime::OwnedCopy(value, value_type)));
}

auto lyra_rt_union_component(const void* value, std::int64_t index, void* out)
    -> void* {
  const lyra::value::AnyValue& member =
      Read<RuntimeUnion>(value).Component(static_cast<std::size_t>(index));
  return lyra::runtime::ElementInto(out, member.Type(), member.Bytes());
}

auto lyra_rt_union_with_component(
    const void* value, std::int64_t index, const void* member,
    const void* member_type, void* out) -> void* {
  RuntimeUnion result = Read<RuntimeUnion>(value);
  result.SetComponent(
      static_cast<std::size_t>(index),
      lyra::runtime::OwnedCopy(member, member_type));
  return Emplace(out, std::move(result));
}

auto lyra_rt_union_eq(const void* lhs, const void* rhs) -> std::uint8_t {
  return Compared(Read<RuntimeUnion>(lhs) == Read<RuntimeUnion>(rhs));
}

auto lyra_rt_union_ne(const void* lhs, const void* rhs) -> std::uint8_t {
  return Compared(Read<RuntimeUnion>(lhs) != Read<RuntimeUnion>(rhs));
}

auto lyra_rt_union_case_equal(const void* lhs, const void* rhs, void* out)
    -> void* {
  return Emplace(
      out, Read<RuntimeUnion>(lhs).CaseEqual(Read<RuntimeUnion>(rhs)));
}

auto lyra_rt_union_is_unknown(const void* value, void* out) -> void* {
  return Emplace(out, Read<RuntimeUnion>(value).IsUnknown());
}

auto lyra_rt_union_cell_get(void* cell) -> const void* {
  return &static_cast<Var<RuntimeUnion>*>(cell)->Get();
}

void lyra_rt_union_cell_initialize(void* cell, const void* prototype) noexcept {
  static_cast<Var<RuntimeUnion>*>(cell)->Initialize(
      Read<RuntimeUnion>(prototype));
}

void lyra_rt_union_cell_set(void* cell, const void* value) {
  static_cast<Var<RuntimeUnion>*>(cell)->Set(Read<RuntimeUnion>(value));
}

void lyra_rt_union_cell_arm_sampling(void* cell) {
  static_cast<Var<RuntimeUnion>*>(cell)->ArmSampling();
}

auto lyra_rt_union_cell_sampled_load(void* cell, void* out) -> void* {
  return Emplace(out, static_cast<Var<RuntimeUnion>*>(cell)->SampledGet());
}

auto lyra_rt_union_value_cell_alloc() noexcept -> void* {
  return GeneratedCallScope::Current()
      .ActivationValues()
      .New<ActivationValueCell<RuntimeUnion>>();
}

void lyra_rt_union_value_cell_store(void* cell, const void* value) noexcept {
  static_cast<ActivationValueCell<RuntimeUnion>*>(cell)->Store(
      Read<RuntimeUnion>(value));
}

auto lyra_rt_union_value_cell_load(void* cell) noexcept -> void* {
  return &static_cast<ActivationValueCell<RuntimeUnion>*>(cell)->Storage();
}

auto lyra_rt_tagged_union_make(
    std::int64_t tag, const void* payload, const void* payload_type, void* out)
    -> void* {
  return Emplace(
      out, RuntimeTaggedUnion(
               static_cast<std::size_t>(tag),
               lyra::runtime::OwnedCopy(payload, payload_type)));
}

auto lyra_rt_tagged_union_component(
    const void* value, std::int64_t index, void* out) -> void* {
  const lyra::value::AnyValue& member =
      Read<RuntimeTaggedUnion>(value).Component(
          static_cast<std::size_t>(index));
  return lyra::runtime::ElementInto(out, member.Type(), member.Bytes());
}

auto lyra_rt_tagged_union_with_component(
    const void* value, std::int64_t index, const void* member,
    const void* member_type, void* out) -> void* {
  RuntimeTaggedUnion result = Read<RuntimeTaggedUnion>(value);
  result.SetComponent(
      static_cast<std::size_t>(index),
      lyra::runtime::OwnedCopy(member, member_type));
  return Emplace(out, std::move(result));
}

// Whether the active tag is `index`, as the machine boolean the pattern-match
// guard tests (LRM 12.6). The runtime holds the comparison, so no tag constant
// crosses the boundary.
auto lyra_rt_tagged_union_tag_matches(const void* value, std::int64_t index)
    -> bool {
  return Read<RuntimeTaggedUnion>(value).Tag() ==
         static_cast<std::size_t>(index);
}

auto lyra_rt_tagged_union_eq(const void* lhs, const void* rhs) -> std::uint8_t {
  return Compared(
      Read<RuntimeTaggedUnion>(lhs) == Read<RuntimeTaggedUnion>(rhs));
}

auto lyra_rt_tagged_union_ne(const void* lhs, const void* rhs) -> std::uint8_t {
  return Compared(
      Read<RuntimeTaggedUnion>(lhs) != Read<RuntimeTaggedUnion>(rhs));
}

auto lyra_rt_tagged_union_case_equal(
    const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(
      out,
      Read<RuntimeTaggedUnion>(lhs).CaseEqual(Read<RuntimeTaggedUnion>(rhs)));
}

auto lyra_rt_tagged_union_is_unknown(const void* value, void* out) -> void* {
  return Emplace(out, Read<RuntimeTaggedUnion>(value).IsUnknown());
}

auto lyra_rt_tagged_union_cell_get(void* cell) -> const void* {
  return &static_cast<Var<RuntimeTaggedUnion>*>(cell)->Get();
}

void lyra_rt_tagged_union_cell_initialize(
    void* cell, const void* prototype) noexcept {
  static_cast<Var<RuntimeTaggedUnion>*>(cell)->Initialize(
      Read<RuntimeTaggedUnion>(prototype));
}

void lyra_rt_tagged_union_cell_set(void* cell, const void* value) {
  static_cast<Var<RuntimeTaggedUnion>*>(cell)->Set(
      Read<RuntimeTaggedUnion>(value));
}

void lyra_rt_tagged_union_cell_arm_sampling(void* cell) {
  static_cast<Var<RuntimeTaggedUnion>*>(cell)->ArmSampling();
}

auto lyra_rt_tagged_union_cell_sampled_load(void* cell, void* out) -> void* {
  return Emplace(
      out, static_cast<Var<RuntimeTaggedUnion>*>(cell)->SampledGet());
}

auto lyra_rt_tagged_union_value_cell_alloc() noexcept -> void* {
  return GeneratedCallScope::Current()
      .ActivationValues()
      .New<ActivationValueCell<RuntimeTaggedUnion>>();
}

void lyra_rt_tagged_union_value_cell_store(
    void* cell, const void* value) noexcept {
  static_cast<ActivationValueCell<RuntimeTaggedUnion>*>(cell)->Store(
      Read<RuntimeTaggedUnion>(value));
}

auto lyra_rt_tagged_union_value_cell_load(void* cell) noexcept -> void* {
  return &static_cast<ActivationValueCell<RuntimeTaggedUnion>*>(cell)
              ->Storage();
}

// A tagged union's `void` member (LRM 7.3.2) carries a value with no bits.
// `default` builds the one value it has.
auto lyra_rt_empty_default(void* out) -> void* {
  return Emplace(out, lyra::value::Empty{});
}

auto lyra_rt_make_dynamic_array_default(
    const void* prototype, const void* prototype_type, void* out) -> void* {
  return Emplace(
      out, RuntimeDynamicArray(
               lyra::runtime::TypeAt(prototype_type), prototype, 0, nullptr));
}

auto lyra_rt_make_dynamic_array_new(
    std::int64_t size, const void* prototype, const void* prototype_type,
    void* out) -> void* {
  return Emplace(
      out, RuntimeDynamicArray(
               lyra::runtime::TypeAt(prototype_type), prototype,
               lyra::runtime::NewCount(size), nullptr));
}

auto lyra_rt_make_dynamic_array_new_copy(
    std::int64_t size, const void* prototype, const void* prototype_type,
    const void* src, void* out) -> void* {
  return Emplace(
      out, RuntimeDynamicArray(
               lyra::runtime::TypeAt(prototype_type), prototype,
               lyra::runtime::NewCount(size), &Read<RuntimeDynamicArray>(src)));
}

auto lyra_rt_dynarray_from_literal(
    const void* prototype, const void* prototype_type, LyraSpan unit,
    std::int64_t count, void* out) -> void* {
  return Emplace(
      out, RuntimeDynamicArray::FromElements(
               lyra::runtime::TypeAt(prototype_type), prototype,
               lyra::runtime::ReplicateHandles(unit, count)));
}

auto lyra_rt_unpackedarray_dynamic_array_from_array(
    const void* source, const void* prototype, const void* prototype_type,
    void* out) -> void* {
  return Emplace(
      out,
      RuntimeDynamicArray::FromElements(
          lyra::runtime::TypeAt(prototype_type), prototype,
          lyra::runtime::ElementHandles(Read<RuntimeUnpackedArray>(source))));
}

auto lyra_rt_queue_dynamic_array_from_array(
    const void* source, const void* prototype, const void* prototype_type,
    void* out) -> void* {
  return Emplace(
      out, RuntimeDynamicArray::FromElements(
               lyra::runtime::TypeAt(prototype_type), prototype,
               lyra::runtime::ElementHandles(Read<RuntimeQueue>(source))));
}

// Reads the element at `position` where it lies. A position naming no element
// reads the element default (LRM 7.4.5).
auto lyra_rt_dynarray_element(const void* array, const void* position) -> const
    void* {
  return Read<RuntimeDynamicArray>(array).Element(PositionNamedAt(position));
}

auto lyra_rt_dynarray_element_ref(void* array, const void* position) -> void* {
  lyra::value::Formation formed{};
  return static_cast<RuntimeDynamicArray*>(array)->ElementRef(
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
    const void* array, const void* start, std::int64_t count, void* out)
    -> void* {
  const auto& source = Read<RuntimeDynamicArray>(array);
  return Emplace(
      out, RuntimeUnpackedArray(
               source.ElementType(), source.ElementDefault(),
               source.SliceElements(PositionNamedAt(start), count)));
}

void lyra_rt_dynarray_element_slice_ref(
    void* array, const void* start, std::int64_t count,
    const void* replacement) {
  static_cast<RuntimeDynamicArray*>(array)->AssignSlice(
      PositionNamedAt(start), count,
      lyra::runtime::ElementHandles(Read<RuntimeUnpackedArray>(replacement)));
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

auto lyra_rt_dynarray_cell_get(void* cell) -> const void* {
  return &static_cast<Var<RuntimeDynamicArray>*>(cell)->Get();
}

void lyra_rt_dynarray_cell_initialize(
    void* cell, const void* prototype) noexcept {
  static_cast<Var<RuntimeDynamicArray>*>(cell)->Initialize(
      Read<RuntimeDynamicArray>(prototype));
}

void lyra_rt_dynarray_cell_set(void* cell, const void* value) {
  static_cast<Var<RuntimeDynamicArray>*>(cell)->Set(
      Read<RuntimeDynamicArray>(value));
}

void lyra_rt_dynarray_cell_arm_sampling(void* cell) {
  static_cast<Var<RuntimeDynamicArray>*>(cell)->ArmSampling();
}

auto lyra_rt_dynarray_cell_sampled_load(void* cell, void* out) -> void* {
  return Emplace(
      out, static_cast<Var<RuntimeDynamicArray>*>(cell)->SampledGet());
}

auto lyra_rt_dynarray_value_cell_alloc() noexcept -> void* {
  return GeneratedCallScope::Current()
      .ActivationValues()
      .New<ActivationValueCell<RuntimeDynamicArray>>();
}

void lyra_rt_dynarray_value_cell_store(void* cell, const void* value) noexcept {
  static_cast<ActivationValueCell<RuntimeDynamicArray>*>(cell)->Store(
      Read<RuntimeDynamicArray>(value));
}

auto lyra_rt_dynarray_value_cell_load(void* cell) noexcept -> void* {
  return &static_cast<ActivationValueCell<RuntimeDynamicArray>*>(cell)
              ->Storage();
}

auto lyra_rt_unpackedarray_from_literal(
    const void* prototype, const void* prototype_type, LyraSpan unit,
    std::int64_t count, void* out) -> void* {
  return Emplace(
      out, RuntimeUnpackedArray(
               lyra::runtime::TypeAt(prototype_type), prototype,
               lyra::runtime::ReplicateHandles(unit, count)));
}

// LRM 10.10: adopt an unpacked concatenation's parts, accumulated into a
// dynamic array, into a fixed-size target. A count the front end could not
// verify -- because a spread part is sized at run time -- is checked here, a
// mismatch being the design's own failure.
auto lyra_rt_unpackedarray_conform_size(
    const void* parts, std::int64_t count, void* out) -> void* {
  const auto& source = Read<RuntimeDynamicArray>(parts);
  const std::int64_t size = source.Size().ToInt64();
  if (size != count) {
    throw lyra::SimulationError(
        std::format(
            "unpacked array concatenation yields {} elements but the "
            "fixed-size target has {} (LRM 10.10)",
            size, count));
  }
  return Emplace(
      out, RuntimeUnpackedArray(
               source.ElementType(), source.ElementDefault(),
               lyra::runtime::ElementHandles(source)));
}

auto lyra_rt_dynarray_unpacked_array_from_array(
    const void* source, const void* prototype, const void* prototype_type,
    std::int64_t declared, void* out) -> void* {
  return Emplace(
      out, RuntimeUnpackedArray::FromElements(
               lyra::runtime::TypeAt(prototype_type), prototype,
               lyra::runtime::ElementHandles(Read<RuntimeDynamicArray>(source)),
               declared));
}

auto lyra_rt_queue_unpacked_array_from_array(
    const void* source, const void* prototype, const void* prototype_type,
    std::int64_t declared, void* out) -> void* {
  return Emplace(
      out,
      RuntimeUnpackedArray::FromElements(
          lyra::runtime::TypeAt(prototype_type), prototype,
          lyra::runtime::ElementHandles(Read<RuntimeQueue>(source)), declared));
}

auto lyra_rt_unpackedarray_merge_conditional(
    const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(
      out, Read<RuntimeUnpackedArray>(lhs).MergeConditional(
               Read<RuntimeUnpackedArray>(rhs)));
}

// Reads the element a position names. A position that names no element reads
// the element default (LRM 7.4.5).
auto lyra_rt_unpackedarray_element(const void* array, const void* position)
    -> const void* {
  return Read<RuntimeUnpackedArray>(array).Element(PositionNamedAt(position));
}

// The element a position names, as storage a write lands in (LRM 7.4.5). A
// position that names none yields storage nothing reads.
auto lyra_rt_unpackedarray_element_ref(void* array, const void* position)
    -> void* {
  lyra::value::Formation formed{};
  return static_cast<RuntimeUnpackedArray*>(array)->ElementRef(
      PositionNamedAt(position), formed);
}

auto lyra_rt_byte_array_from_string(
    const void* text, std::int64_t count, const void* element_type, void* out)
    -> void* {
  return Emplace(
      out, RuntimeUnpackedArray::FromString(
               Read<String>(text), IntegralTypeAt(element_type), count));
}

auto lyra_rt_byte_array_from_bits(
    const void* bits, std::int64_t bits_width, bool bits_are_four_state,
    std::int64_t count, const void* element_type, void* out) -> void* {
  const lyra::value::LoadedWords planes =
      BitsAt(bits, bits_width, bits_are_four_state);
  return Emplace(
      out, RuntimeUnpackedArray::FromIntegral(
               planes.View(), IntegralTypeAt(element_type), count));
}

auto lyra_rt_unpackedarray_count_bits(
    const void* value, const void* control_bits, std::int64_t control_width,
    bool control_is_four_state, void* out) -> void* {
  const lyra::value::LoadedWords control =
      BitsAt(control_bits, control_width, control_is_four_state);
  return Emplace(
      out, Read<RuntimeUnpackedArray>(value).CountBits(control.View()));
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

auto lyra_rt_unpackedarray_bitstream_width(const void* value, void* out)
    -> void* {
  return Emplace(out, Read<RuntimeUnpackedArray>(value).BitstreamWidth());
}

auto lyra_rt_unpackedarray_to_bitstream(
    const void* value, std::int64_t out_width, bool out_is_four_state,
    void* out) -> void* {
  return lyra::runtime::StreamOf<RuntimeUnpackedArray>(
      value, out_width, out_is_four_state, out);
}

auto lyra_rt_from_bitstream(
    const void* bits, std::int64_t bits_width, bool bits_are_four_state,
    const void* prototype, const void* prototype_type, void* out) -> void* {
  const lyra::value::LoadedWords stream =
      BitsAt(bits, bits_width, bits_are_four_state);
  lyra::runtime::TypeAt(prototype_type)
      .ReadFromStream(stream.Read(), stream.Shape().width, 0, prototype, out);
  return out;
}

auto lyra_rt_stream_write(
    const void* bits, std::int64_t bits_width, bool bits_are_four_state,
    const void* stream, std::uint64_t stream_width, std::uint64_t filled)
    -> std::uint64_t {
  const lyra::value::LoadedWords written =
      BitsAt(bits, bits_width, bits_are_four_state);
  const std::uint64_t width = written.Shape().width;
  lyra::value::Insert(
      *static_cast<const lyra::value::Planes*>(stream), stream_width,
      written.Read(), width,
      static_cast<std::int64_t>(stream_width - filled - width));
  return filled + width;
}

auto lyra_rt_stream_read(
    const void* stream, std::uint64_t stream_width, std::uint64_t taken,
    std::int64_t out_width, bool out_is_four_state, void* out)
    -> std::uint64_t {
  lyra::value::LoadedWords read(
      lyra::runtime::NumberShape(out_width, false, out_is_four_state));
  const std::uint64_t width = read.Shape().width;
  lyra::value::Extract(
      read.Write(), width,
      *static_cast<const lyra::value::ConstPlanes*>(stream), stream_width,
      static_cast<std::int64_t>(stream_width - taken - width));
  read.StoreTo(out);
  return taken + width;
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
auto lyra_rt_union_bit_identical(const void* lhs, const void* rhs) -> bool {
  return lyra::runtime::BitIdentical<RuntimeUnion>(lhs, rhs);
}
auto lyra_rt_tagged_union_bit_identical(const void* lhs, const void* rhs)
    -> bool {
  return lyra::runtime::BitIdentical<RuntimeTaggedUnion>(lhs, rhs);
}
auto lyra_rt_dynarray_bit_identical(const void* lhs, const void* rhs) -> bool {
  return lyra::runtime::BitIdentical<RuntimeDynamicArray>(lhs, rhs);
}
auto lyra_rt_unpackedarray_bit_identical(const void* lhs, const void* rhs)
    -> bool {
  return lyra::runtime::BitIdentical<RuntimeUnpackedArray>(lhs, rhs);
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
auto lyra_rt_union_has_unknown(const void* value) -> bool {
  return lyra::runtime::HasUnknown<RuntimeUnion>(value);
}
auto lyra_rt_tagged_union_has_unknown(const void* value) -> bool {
  return lyra::runtime::HasUnknown<RuntimeTaggedUnion>(value);
}
auto lyra_rt_dynarray_has_unknown(const void* value) -> bool {
  return lyra::runtime::HasUnknown<RuntimeDynamicArray>(value);
}
auto lyra_rt_unpackedarray_has_unknown(const void* value) -> bool {
  return lyra::runtime::HasUnknown<RuntimeUnpackedArray>(value);
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

auto lyra_rt_union_bitstream_width(const void* value, void* out) -> void* {
  return lyra::runtime::StreamWidthOf<RuntimeUnion>(value, out);
}
auto lyra_rt_tagged_union_bitstream_width(const void* value, void* out)
    -> void* {
  return lyra::runtime::StreamWidthOf<RuntimeTaggedUnion>(value, out);
}
auto lyra_rt_managedref_bitstream_width(const void* value, void* out) -> void* {
  return lyra::runtime::StreamWidthOf<ObjectRef>(value, out);
}

auto lyra_rt_union_count_bits(
    const void* value, const void* control_bits, std::int64_t control_width,
    bool control_is_four_state, void* out) -> void* {
  return lyra::runtime::StreamCountBitsOf<RuntimeUnion>(
      value, BitsAt(control_bits, control_width, control_is_four_state), out);
}
auto lyra_rt_tagged_union_count_bits(
    const void* value, const void* control_bits, std::int64_t control_width,
    bool control_is_four_state, void* out) -> void* {
  return lyra::runtime::StreamCountBitsOf<RuntimeTaggedUnion>(
      value, BitsAt(control_bits, control_width, control_is_four_state), out);
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
auto lyra_rt_union_to_bitstream(
    const void* value, std::int64_t out_width, bool out_is_four_state,
    void* out) -> void* {
  return lyra::runtime::StreamOf<RuntimeUnion>(
      value, out_width, out_is_four_state, out);
}
auto lyra_rt_tagged_union_to_bitstream(
    const void* value, std::int64_t out_width, bool out_is_four_state,
    void* out) -> void* {
  return lyra::runtime::StreamOf<RuntimeTaggedUnion>(
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

auto lyra_rt_union_resolve_tri_state(
    const void* lhs, const void* rhs, void* out) -> void* {
  return lyra::runtime::Resolved<RuntimeUnion>(
      NetResolution::kTriState, lhs, rhs, out);
}
auto lyra_rt_unpackedarray_resolve_tri_state(
    const void* lhs, const void* rhs, void* out) -> void* {
  return lyra::runtime::Resolved<RuntimeUnpackedArray>(
      NetResolution::kTriState, lhs, rhs, out);
}
auto lyra_rt_union_resolve_wired_and(
    const void* lhs, const void* rhs, void* out) -> void* {
  return lyra::runtime::Resolved<RuntimeUnion>(
      NetResolution::kWiredAnd, lhs, rhs, out);
}
auto lyra_rt_unpackedarray_resolve_wired_and(
    const void* lhs, const void* rhs, void* out) -> void* {
  return lyra::runtime::Resolved<RuntimeUnpackedArray>(
      NetResolution::kWiredAnd, lhs, rhs, out);
}
auto lyra_rt_union_resolve_wired_or(const void* lhs, const void* rhs, void* out)
    -> void* {
  return lyra::runtime::Resolved<RuntimeUnion>(
      NetResolution::kWiredOr, lhs, rhs, out);
}
auto lyra_rt_unpackedarray_resolve_wired_or(
    const void* lhs, const void* rhs, void* out) -> void* {
  return lyra::runtime::Resolved<RuntimeUnpackedArray>(
      NetResolution::kWiredOr, lhs, rhs, out);
}
auto lyra_rt_union_dominating(
    const void* stronger, const void* weaker, void* out) -> void* {
  return lyra::runtime::Dominating<RuntimeUnion>(stronger, weaker, out);
}
auto lyra_rt_unpackedarray_dominating(
    const void* stronger, const void* weaker, void* out) -> void* {
  return lyra::runtime::Dominating<RuntimeUnpackedArray>(stronger, weaker, out);
}
auto lyra_rt_union_filled_like(
    const void* prototype, const void* fill, std::int64_t fill_width,
    bool fill_is_four_state, void* out) -> void* {
  return lyra::runtime::FilledLike<RuntimeUnion>(
      prototype, fill, fill_width, fill_is_four_state, out);
}
auto lyra_rt_unpackedarray_filled_like(
    const void* prototype, const void* fill, std::int64_t fill_width,
    bool fill_is_four_state, void* out) -> void* {
  return lyra::runtime::FilledLike<RuntimeUnpackedArray>(
      prototype, fill, fill_width, fill_is_four_state, out);
}

auto lyra_rt_unpackedarray_size(const void* array, void* out) -> void* {
  return Emplace(out, Read<RuntimeUnpackedArray>(array).Size());
}

auto lyra_rt_unpackedarray_element_slice(
    const void* array, const void* start, std::int64_t count, void* out)
    -> void* {
  return Emplace(
      out,
      Read<RuntimeUnpackedArray>(array).Slice(PositionNamedAt(start), count));
}

void lyra_rt_unpackedarray_element_slice_ref(
    void* array, const void* start, std::int64_t count,
    const void* replacement) {
  static_cast<RuntimeUnpackedArray*>(array)->AssignSlice(
      PositionNamedAt(start), count,
      lyra::runtime::ElementHandles(Read<RuntimeUnpackedArray>(replacement)));
}

auto lyra_rt_unpackedarray_eq(const void* lhs, const void* rhs)
    -> std::uint8_t {
  return Compared(
      Read<RuntimeUnpackedArray>(lhs) == Read<RuntimeUnpackedArray>(rhs));
}

auto lyra_rt_unpackedarray_ne(const void* lhs, const void* rhs)
    -> std::uint8_t {
  return Compared(
      Read<RuntimeUnpackedArray>(lhs) != Read<RuntimeUnpackedArray>(rhs));
}

auto lyra_rt_unpackedarray_case_equal(
    const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(
      out, Read<RuntimeUnpackedArray>(lhs).CaseEqual(
               Read<RuntimeUnpackedArray>(rhs)));
}

auto lyra_rt_unpackedarray_is_unknown(const void* value, void* out) -> void* {
  return Emplace(out, Read<RuntimeUnpackedArray>(value).IsUnknown());
}

auto lyra_rt_unpackedarray_cell_get(void* cell) -> const void* {
  return &static_cast<Var<RuntimeUnpackedArray>*>(cell)->Get();
}

void lyra_rt_unpackedarray_cell_initialize(
    void* cell, const void* prototype) noexcept {
  static_cast<Var<RuntimeUnpackedArray>*>(cell)->Initialize(
      Read<RuntimeUnpackedArray>(prototype));
}

void lyra_rt_unpackedarray_cell_set(void* cell, const void* value) {
  static_cast<Var<RuntimeUnpackedArray>*>(cell)->Set(
      Read<RuntimeUnpackedArray>(value));
}

void lyra_rt_unpackedarray_cell_arm_sampling(void* cell) {
  static_cast<Var<RuntimeUnpackedArray>*>(cell)->ArmSampling();
}

auto lyra_rt_unpackedarray_cell_sampled_load(void* cell, void* out) -> void* {
  return Emplace(
      out, static_cast<Var<RuntimeUnpackedArray>*>(cell)->SampledGet());
}

auto lyra_rt_tuple_net_get(void* net) -> const void* {
  return HandleTo(NetOf<RuntimeTuple>(net).Get());
}

void lyra_rt_tuple_aggregate_net_initialize_tri_state(
    void* net, const void* prototype, std::int64_t fill,
    std::int64_t strength) {
  NetOf<RuntimeTuple>(net).InitializeTriState(
      Read<RuntimeTuple>(prototype), fill, strength);
}

void lyra_rt_tuple_aggregate_net_initialize_wired_and(
    void* net, const void* prototype, std::int64_t fill,
    std::int64_t strength) {
  NetOf<RuntimeTuple>(net).InitializeWiredAnd(
      Read<RuntimeTuple>(prototype), fill, strength);
}

void lyra_rt_tuple_aggregate_net_initialize_wired_or(
    void* net, const void* prototype, std::int64_t fill,
    std::int64_t strength) {
  NetOf<RuntimeTuple>(net).InitializeWiredOr(
      Read<RuntimeTuple>(prototype), fill, strength);
}

void lyra_rt_tuple_aggregate_net_initialize_retaining(
    void* net, const void* prototype, std::int64_t fill,
    std::int64_t strength) {
  NetOf<RuntimeTuple>(net).InitializeRetaining(
      Read<RuntimeTuple>(prototype), fill, strength);
}

auto lyra_rt_tuple_net_begin_takeover(void* net, std::int64_t level)
    -> std::int64_t {
  return NetOf<RuntimeTuple>(net).BeginTakeover(level);
}

auto lyra_rt_tuple_net_drive_takeover(
    void* net, std::int64_t level, std::int64_t generation, const void* value)
    -> bool {
  return NetOf<RuntimeTuple>(net).DriveTakeover(
      level, generation, Read<RuntimeTuple>(value));
}

void lyra_rt_tuple_net_end_takeover(void* net, std::int64_t level) {
  NetOf<RuntimeTuple>(net).EndTakeover(level);
}

auto lyra_rt_tuple_attach_driver(void* net, std::int64_t strength) -> void* {
  return &NetOf<RuntimeTuple>(net).AttachDriver(strength);
}

auto lyra_rt_tuple_driver_get(void* driver) -> const void* {
  return HandleTo(DriverOf<RuntimeTuple>(driver).Get());
}

void lyra_rt_tuple_driver_set(void* driver, const void* value) {
  DriverOf<RuntimeTuple>(driver).Set(Read<RuntimeTuple>(value));
}

auto lyra_rt_union_net_get(void* net) -> const void* {
  return &NetOf<RuntimeUnion>(net).Get();
}

void lyra_rt_union_aggregate_net_initialize_tri_state(
    void* net, const void* prototype, std::int64_t fill,
    std::int64_t strength) {
  NetOf<RuntimeUnion>(net).InitializeTriState(
      Read<RuntimeUnion>(prototype), fill, strength);
}

void lyra_rt_union_aggregate_net_initialize_wired_and(
    void* net, const void* prototype, std::int64_t fill,
    std::int64_t strength) {
  NetOf<RuntimeUnion>(net).InitializeWiredAnd(
      Read<RuntimeUnion>(prototype), fill, strength);
}

void lyra_rt_union_aggregate_net_initialize_wired_or(
    void* net, const void* prototype, std::int64_t fill,
    std::int64_t strength) {
  NetOf<RuntimeUnion>(net).InitializeWiredOr(
      Read<RuntimeUnion>(prototype), fill, strength);
}

void lyra_rt_union_aggregate_net_initialize_retaining(
    void* net, const void* prototype, std::int64_t fill,
    std::int64_t strength) {
  NetOf<RuntimeUnion>(net).InitializeRetaining(
      Read<RuntimeUnion>(prototype), fill, strength);
}

auto lyra_rt_union_net_begin_takeover(void* net, std::int64_t level)
    -> std::int64_t {
  return NetOf<RuntimeUnion>(net).BeginTakeover(level);
}

auto lyra_rt_union_net_drive_takeover(
    void* net, std::int64_t level, std::int64_t generation, const void* value)
    -> bool {
  return NetOf<RuntimeUnion>(net).DriveTakeover(
      level, generation, Read<RuntimeUnion>(value));
}

void lyra_rt_union_net_end_takeover(void* net, std::int64_t level) {
  NetOf<RuntimeUnion>(net).EndTakeover(level);
}

auto lyra_rt_union_attach_driver(void* net, std::int64_t strength) -> void* {
  return &NetOf<RuntimeUnion>(net).AttachDriver(strength);
}

auto lyra_rt_union_driver_get(void* driver) -> const void* {
  return &DriverOf<RuntimeUnion>(driver).Get();
}

void lyra_rt_union_driver_set(void* driver, const void* value) {
  DriverOf<RuntimeUnion>(driver).Set(Read<RuntimeUnion>(value));
}

auto lyra_rt_unpackedarray_net_get(void* net) -> const void* {
  return &NetOf<RuntimeUnpackedArray>(net).Get();
}

void lyra_rt_unpackedarray_aggregate_net_initialize_tri_state(
    void* net, const void* prototype, std::int64_t fill,
    std::int64_t strength) {
  NetOf<RuntimeUnpackedArray>(net).InitializeTriState(
      Read<RuntimeUnpackedArray>(prototype), fill, strength);
}

void lyra_rt_unpackedarray_aggregate_net_initialize_wired_and(
    void* net, const void* prototype, std::int64_t fill,
    std::int64_t strength) {
  NetOf<RuntimeUnpackedArray>(net).InitializeWiredAnd(
      Read<RuntimeUnpackedArray>(prototype), fill, strength);
}

void lyra_rt_unpackedarray_aggregate_net_initialize_wired_or(
    void* net, const void* prototype, std::int64_t fill,
    std::int64_t strength) {
  NetOf<RuntimeUnpackedArray>(net).InitializeWiredOr(
      Read<RuntimeUnpackedArray>(prototype), fill, strength);
}

void lyra_rt_unpackedarray_aggregate_net_initialize_retaining(
    void* net, const void* prototype, std::int64_t fill,
    std::int64_t strength) {
  NetOf<RuntimeUnpackedArray>(net).InitializeRetaining(
      Read<RuntimeUnpackedArray>(prototype), fill, strength);
}

auto lyra_rt_unpackedarray_net_begin_takeover(void* net, std::int64_t level)
    -> std::int64_t {
  return NetOf<RuntimeUnpackedArray>(net).BeginTakeover(level);
}

auto lyra_rt_unpackedarray_net_drive_takeover(
    void* net, std::int64_t level, std::int64_t generation, const void* value)
    -> bool {
  return NetOf<RuntimeUnpackedArray>(net).DriveTakeover(
      level, generation, Read<RuntimeUnpackedArray>(value));
}

void lyra_rt_unpackedarray_net_end_takeover(void* net, std::int64_t level) {
  NetOf<RuntimeUnpackedArray>(net).EndTakeover(level);
}

auto lyra_rt_unpackedarray_attach_driver(void* net, std::int64_t strength)
    -> void* {
  return &NetOf<RuntimeUnpackedArray>(net).AttachDriver(strength);
}

auto lyra_rt_unpackedarray_driver_get(void* driver) -> const void* {
  return &DriverOf<RuntimeUnpackedArray>(driver).Get();
}

void lyra_rt_unpackedarray_driver_set(void* driver, const void* value) {
  DriverOf<RuntimeUnpackedArray>(driver).Set(Read<RuntimeUnpackedArray>(value));
}

auto lyra_rt_unpackedarray_value_cell_alloc() noexcept -> void* {
  return GeneratedCallScope::Current()
      .ActivationValues()
      .New<ActivationValueCell<RuntimeUnpackedArray>>();
}

void lyra_rt_unpackedarray_value_cell_store(
    void* cell, const void* value) noexcept {
  static_cast<ActivationValueCell<RuntimeUnpackedArray>*>(cell)->Store(
      Read<RuntimeUnpackedArray>(value));
}

auto lyra_rt_unpackedarray_value_cell_load(void* cell) noexcept -> void* {
  return &static_cast<ActivationValueCell<RuntimeUnpackedArray>*>(cell)
              ->Storage();
}

auto lyra_rt_queue_from_literal(
    const void* prototype, const void* prototype_type, LyraSpan unit,
    std::int64_t count, void* out) -> void* {
  return Emplace(
      out, RuntimeQueue::FromElements(
               lyra::runtime::TypeAt(prototype_type), prototype,
               lyra::runtime::ReplicateHandles(unit, count)));
}

auto lyra_rt_queue_from_literal_bounded(
    const void* prototype, const void* prototype_type, LyraSpan unit,
    std::int64_t count, std::int64_t max_bound, void* out) -> void* {
  return Emplace(
      out, RuntimeQueue::FromElements(
               lyra::runtime::TypeAt(prototype_type), prototype, max_bound,
               lyra::runtime::ReplicateHandles(unit, count)));
}

auto lyra_rt_queue_conform_bound(
    const void* queue, std::int64_t max_bound, void* out) -> void* {
  return Emplace(out, Read<RuntimeQueue>(queue).ConformBound(max_bound));
}

auto lyra_rt_unpackedarray_queue_from_array(
    const void* source, const void* prototype, const void* prototype_type,
    std::int64_t max_bound, void* out) -> void* {
  return Emplace(
      out,
      RuntimeQueue::FromElements(
          lyra::runtime::TypeAt(prototype_type), prototype, max_bound,
          lyra::runtime::ElementHandles(Read<RuntimeUnpackedArray>(source))));
}

auto lyra_rt_dynarray_queue_from_array(
    const void* source, const void* prototype, const void* prototype_type,
    std::int64_t max_bound, void* out) -> void* {
  return Emplace(
      out,
      RuntimeQueue::FromElements(
          lyra::runtime::TypeAt(prototype_type), prototype, max_bound,
          lyra::runtime::ElementHandles(Read<RuntimeDynamicArray>(source))));
}

auto lyra_rt_queue_element(const void* queue, const void* position) -> const
    void* {
  return Read<RuntimeQueue>(queue).Element(PositionNamedAt(position));
}

auto lyra_rt_queue_element_ref(void* queue, const void* position) -> void* {
  lyra::value::Formation formed{};
  return static_cast<RuntimeQueue*>(queue)->ElementRef(
      PositionNamedAt(position), formed);
}

auto lyra_rt_queue_slice(
    const void* queue, const void* lo, const void* hi, void* out) -> void* {
  return Emplace(
      out, Read<RuntimeQueue>(queue).Slice(
               PositionNamedAt(lo), PositionNamedAt(hi)));
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
  return &static_cast<Var<RuntimeQueue>*>(cell)->Get();
}

void lyra_rt_queue_cell_initialize(void* cell, const void* prototype) noexcept {
  static_cast<Var<RuntimeQueue>*>(cell)->Initialize(
      Read<RuntimeQueue>(prototype));
}

void lyra_rt_queue_cell_set(void* cell, const void* value) {
  static_cast<Var<RuntimeQueue>*>(cell)->Set(Read<RuntimeQueue>(value));
}

void lyra_rt_queue_cell_arm_sampling(void* cell) {
  static_cast<Var<RuntimeQueue>*>(cell)->ArmSampling();
}

auto lyra_rt_queue_cell_sampled_load(void* cell, void* out) -> void* {
  return Emplace(out, static_cast<Var<RuntimeQueue>*>(cell)->SampledGet());
}

auto lyra_rt_queue_value_cell_alloc() noexcept -> void* {
  return GeneratedCallScope::Current()
      .ActivationValues()
      .New<ActivationValueCell<RuntimeQueue>>();
}

void lyra_rt_queue_value_cell_store(void* cell, const void* value) noexcept {
  static_cast<ActivationValueCell<RuntimeQueue>*>(cell)->Store(
      Read<RuntimeQueue>(value));
}

auto lyra_rt_queue_value_cell_load(void* cell) noexcept -> void* {
  return &static_cast<ActivationValueCell<RuntimeQueue>*>(cell)->Storage();
}

// LRM 7.9.11 `'{index: value, ...}`: each entry crosses as the product of the
// index and the element it stores, a tuple whose type states each component's
// type, which is what a keyed container needs of both: it knows the type of
// neither in advance.
auto lyra_rt_assocarray_from_entries_default(
    const void* prototype, const void* prototype_type, LyraSpan entries,
    const void* user_default, void* out) -> void* {
  RuntimeAssociativeArray array(
      AssociativeIndexOrder::kIndexValueDomain,
      lyra::runtime::TypeAt(prototype_type), prototype, user_default);
  lyra::runtime::SeedAssociativeEntries(array, entries);
  return Emplace(out, std::move(array));
}

auto lyra_rt_assocarray_from_entries_default_wildcard(
    const void* prototype, const void* prototype_type, LyraSpan entries,
    const void* user_default, void* out) -> void* {
  RuntimeAssociativeArray array(
      AssociativeIndexOrder::kWildcardNumeric,
      lyra::runtime::TypeAt(prototype_type), prototype, user_default);
  lyra::runtime::SeedAssociativeEntries(array, entries);
  return Emplace(out, std::move(array));
}

auto lyra_rt_assocarray_element(
    const void* array, const void* index, const void* index_type) -> const
    void* {
  return Read<RuntimeAssociativeArray>(array).Element(
      lyra::runtime::IndexAt(index, index_type));
}

auto lyra_rt_assocarray_element_ref(
    void* array, const void* index, const void* index_type) -> void* {
  lyra::value::Formation formed{};
  return static_cast<RuntimeAssociativeArray*>(array)->ElementRef(
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
    const void* array, const void* unallocated, const void* unallocated_type,
    void* out) -> void* {
  return lyra::runtime::IndexInto(
      out, Read<RuntimeAssociativeArray>(array).FirstIndex(), unallocated,
      unallocated_type);
}

auto lyra_rt_assocarray_assoc_max_index(
    const void* array, const void* unallocated, const void* unallocated_type,
    void* out) -> void* {
  return lyra::runtime::IndexInto(
      out, Read<RuntimeAssociativeArray>(array).LastIndex(), unallocated,
      unallocated_type);
}

auto lyra_rt_assocarray_assoc_first(
    const void* array, const void* probe, const void* probe_type, void* out)
    -> void* {
  return lyra::runtime::EmplaceVisited(
      out, Read<RuntimeAssociativeArray>(array).FirstIndex(), probe,
      probe_type);
}

auto lyra_rt_assocarray_assoc_last(
    const void* array, const void* probe, const void* probe_type, void* out)
    -> void* {
  return lyra::runtime::EmplaceVisited(
      out, Read<RuntimeAssociativeArray>(array).LastIndex(), probe, probe_type);
}

auto lyra_rt_assocarray_assoc_next(
    const void* array, const void* probe, const void* probe_type, void* out)
    -> void* {
  return lyra::runtime::EmplaceVisited(
      out,
      Read<RuntimeAssociativeArray>(array).NextIndex(
          lyra::runtime::IndexAt(probe, probe_type)),
      probe, probe_type);
}

auto lyra_rt_assocarray_assoc_prev(
    const void* array, const void* probe, const void* probe_type, void* out)
    -> void* {
  return lyra::runtime::EmplaceVisited(
      out,
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
  return &static_cast<Var<RuntimeAssociativeArray>*>(cell)->Get();
}

void lyra_rt_assocarray_cell_initialize(
    void* cell, const void* prototype) noexcept {
  static_cast<Var<RuntimeAssociativeArray>*>(cell)->Initialize(
      Read<RuntimeAssociativeArray>(prototype));
}

void lyra_rt_assocarray_cell_set(void* cell, const void* value) {
  static_cast<Var<RuntimeAssociativeArray>*>(cell)->Set(
      Read<RuntimeAssociativeArray>(value));
}

void lyra_rt_assocarray_cell_arm_sampling(void* cell) {
  static_cast<Var<RuntimeAssociativeArray>*>(cell)->ArmSampling();
}

auto lyra_rt_assocarray_cell_sampled_load(void* cell, void* out) -> void* {
  return Emplace(
      out, static_cast<Var<RuntimeAssociativeArray>*>(cell)->SampledGet());
}

auto lyra_rt_assocarray_value_cell_alloc() noexcept -> void* {
  return GeneratedCallScope::Current()
      .ActivationValues()
      .New<ActivationValueCell<RuntimeAssociativeArray>>();
}

void lyra_rt_assocarray_value_cell_store(
    void* cell, const void* value) noexcept {
  static_cast<ActivationValueCell<RuntimeAssociativeArray>*>(cell)->Store(
      Read<RuntimeAssociativeArray>(value));
}

auto lyra_rt_assocarray_value_cell_load(void* cell) noexcept -> void* {
  return &static_cast<ActivationValueCell<RuntimeAssociativeArray>*>(cell)
              ->Storage();
}

auto lyra_rt_unpackedarray_sum(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return lyra::runtime::AnswerInto(
      out,
      lyra::value::RuntimeArraySum(
          Read<RuntimeUnpackedArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_unpackedarray_product(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return lyra::runtime::AnswerInto(
      out,
      lyra::value::RuntimeArrayProduct(
          Read<RuntimeUnpackedArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_unpackedarray_and(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return lyra::runtime::AnswerInto(
      out,
      lyra::value::RuntimeArrayAnd(
          Read<RuntimeUnpackedArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_unpackedarray_or(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return lyra::runtime::AnswerInto(
      out,
      lyra::value::RuntimeArrayOr(
          Read<RuntimeUnpackedArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_unpackedarray_xor(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return lyra::runtime::AnswerInto(
      out,
      lyra::value::RuntimeArrayXor(
          Read<RuntimeUnpackedArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_unpackedarray_find(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return Emplace(
      out,
      lyra::value::RuntimeArrayFind(
          Read<RuntimeUnpackedArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_unpackedarray_find_index(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return Emplace(
      out,
      lyra::value::RuntimeArrayFindIndex(
          Read<RuntimeUnpackedArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_unpackedarray_find_first(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return Emplace(
      out,
      lyra::value::RuntimeArrayFindFirst(
          Read<RuntimeUnpackedArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_unpackedarray_find_first_index(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return Emplace(
      out,
      lyra::value::RuntimeArrayFindFirstIndex(
          Read<RuntimeUnpackedArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_unpackedarray_find_last(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return Emplace(
      out,
      lyra::value::RuntimeArrayFindLast(
          Read<RuntimeUnpackedArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_unpackedarray_find_last_index(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return Emplace(
      out,
      lyra::value::RuntimeArrayFindLastIndex(
          Read<RuntimeUnpackedArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_unpackedarray_min(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return Emplace(
      out,
      lyra::value::RuntimeArrayMin(
          Read<RuntimeUnpackedArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_unpackedarray_max(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return Emplace(
      out,
      lyra::value::RuntimeArrayMax(
          Read<RuntimeUnpackedArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_unpackedarray_unique(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return Emplace(
      out,
      lyra::value::RuntimeArrayUnique(
          Read<RuntimeUnpackedArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_unpackedarray_unique_index(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return Emplace(
      out,
      lyra::value::RuntimeArrayUniqueIndex(
          Read<RuntimeUnpackedArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_unpackedarray_map(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return Emplace(
      out,
      lyra::value::RuntimeArrayMap(
          Read<RuntimeUnpackedArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_dynarray_sum(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return lyra::runtime::AnswerInto(
      out,
      lyra::value::RuntimeArraySum(
          Read<RuntimeDynamicArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_dynarray_product(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return lyra::runtime::AnswerInto(
      out,
      lyra::value::RuntimeArrayProduct(
          Read<RuntimeDynamicArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_dynarray_and(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return lyra::runtime::AnswerInto(
      out,
      lyra::value::RuntimeArrayAnd(
          Read<RuntimeDynamicArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_dynarray_or(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return lyra::runtime::AnswerInto(
      out,
      lyra::value::RuntimeArrayOr(
          Read<RuntimeDynamicArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_dynarray_xor(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return lyra::runtime::AnswerInto(
      out,
      lyra::value::RuntimeArrayXor(
          Read<RuntimeDynamicArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_dynarray_find(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return Emplace(
      out,
      lyra::value::RuntimeArrayFind(
          Read<RuntimeDynamicArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_dynarray_find_index(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return Emplace(
      out,
      lyra::value::RuntimeArrayFindIndex(
          Read<RuntimeDynamicArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_dynarray_find_first(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return Emplace(
      out,
      lyra::value::RuntimeArrayFindFirst(
          Read<RuntimeDynamicArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_dynarray_find_first_index(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return Emplace(
      out,
      lyra::value::RuntimeArrayFindFirstIndex(
          Read<RuntimeDynamicArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_dynarray_find_last(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return Emplace(
      out,
      lyra::value::RuntimeArrayFindLast(
          Read<RuntimeDynamicArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_dynarray_find_last_index(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return Emplace(
      out,
      lyra::value::RuntimeArrayFindLastIndex(
          Read<RuntimeDynamicArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_dynarray_min(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return Emplace(
      out,
      lyra::value::RuntimeArrayMin(
          Read<RuntimeDynamicArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_dynarray_max(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return Emplace(
      out,
      lyra::value::RuntimeArrayMax(
          Read<RuntimeDynamicArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_dynarray_unique(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return Emplace(
      out,
      lyra::value::RuntimeArrayUnique(
          Read<RuntimeDynamicArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_dynarray_unique_index(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return Emplace(
      out,
      lyra::value::RuntimeArrayUniqueIndex(
          Read<RuntimeDynamicArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_dynarray_map(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return Emplace(
      out,
      lyra::value::RuntimeArrayMap(
          Read<RuntimeDynamicArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_queue_sum(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return lyra::runtime::AnswerInto(
      out, lyra::value::RuntimeArraySum(
               Read<RuntimeQueue>(receiver), lyra::runtime::ArrayBody(body),
               lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_queue_product(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return lyra::runtime::AnswerInto(
      out, lyra::value::RuntimeArrayProduct(
               Read<RuntimeQueue>(receiver), lyra::runtime::ArrayBody(body),
               lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_queue_and(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return lyra::runtime::AnswerInto(
      out, lyra::value::RuntimeArrayAnd(
               Read<RuntimeQueue>(receiver), lyra::runtime::ArrayBody(body),
               lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_queue_or(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return lyra::runtime::AnswerInto(
      out, lyra::value::RuntimeArrayOr(
               Read<RuntimeQueue>(receiver), lyra::runtime::ArrayBody(body),
               lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_queue_xor(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return lyra::runtime::AnswerInto(
      out, lyra::value::RuntimeArrayXor(
               Read<RuntimeQueue>(receiver), lyra::runtime::ArrayBody(body),
               lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_queue_find(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return Emplace(
      out, lyra::value::RuntimeArrayFind(
               Read<RuntimeQueue>(receiver), lyra::runtime::ArrayBody(body),
               lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_queue_find_index(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return Emplace(
      out, lyra::value::RuntimeArrayFindIndex(
               Read<RuntimeQueue>(receiver), lyra::runtime::ArrayBody(body),
               lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_queue_find_first(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return Emplace(
      out, lyra::value::RuntimeArrayFindFirst(
               Read<RuntimeQueue>(receiver), lyra::runtime::ArrayBody(body),
               lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_queue_find_first_index(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return Emplace(
      out, lyra::value::RuntimeArrayFindFirstIndex(
               Read<RuntimeQueue>(receiver), lyra::runtime::ArrayBody(body),
               lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_queue_find_last(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return Emplace(
      out, lyra::value::RuntimeArrayFindLast(
               Read<RuntimeQueue>(receiver), lyra::runtime::ArrayBody(body),
               lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_queue_find_last_index(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return Emplace(
      out, lyra::value::RuntimeArrayFindLastIndex(
               Read<RuntimeQueue>(receiver), lyra::runtime::ArrayBody(body),
               lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_queue_min(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return Emplace(
      out, lyra::value::RuntimeArrayMin(
               Read<RuntimeQueue>(receiver), lyra::runtime::ArrayBody(body),
               lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_queue_max(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return Emplace(
      out, lyra::value::RuntimeArrayMax(
               Read<RuntimeQueue>(receiver), lyra::runtime::ArrayBody(body),
               lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_queue_unique(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return Emplace(
      out, lyra::value::RuntimeArrayUnique(
               Read<RuntimeQueue>(receiver), lyra::runtime::ArrayBody(body),
               lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_queue_unique_index(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return Emplace(
      out, lyra::value::RuntimeArrayUniqueIndex(
               Read<RuntimeQueue>(receiver), lyra::runtime::ArrayBody(body),
               lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_queue_map(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return Emplace(
      out, lyra::value::RuntimeArrayMap(
               Read<RuntimeQueue>(receiver), lyra::runtime::ArrayBody(body),
               lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_assocarray_sum(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return lyra::runtime::AnswerInto(
      out, lyra::value::RuntimeArraySum(
               Read<RuntimeAssociativeArray>(receiver),
               lyra::runtime::ArrayBody(body),
               lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_assocarray_product(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return lyra::runtime::AnswerInto(
      out, lyra::value::RuntimeArrayProduct(
               Read<RuntimeAssociativeArray>(receiver),
               lyra::runtime::ArrayBody(body),
               lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_assocarray_and(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return lyra::runtime::AnswerInto(
      out, lyra::value::RuntimeArrayAnd(
               Read<RuntimeAssociativeArray>(receiver),
               lyra::runtime::ArrayBody(body),
               lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_assocarray_or(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return lyra::runtime::AnswerInto(
      out, lyra::value::RuntimeArrayOr(
               Read<RuntimeAssociativeArray>(receiver),
               lyra::runtime::ArrayBody(body),
               lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_assocarray_xor(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return lyra::runtime::AnswerInto(
      out, lyra::value::RuntimeArrayXor(
               Read<RuntimeAssociativeArray>(receiver),
               lyra::runtime::ArrayBody(body),
               lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_assocarray_find(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return Emplace(
      out, lyra::value::RuntimeArrayFind(
               Read<RuntimeAssociativeArray>(receiver),
               lyra::runtime::ArrayBody(body),
               lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_assocarray_find_index(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return Emplace(
      out, lyra::value::RuntimeArrayFindIndex(
               Read<RuntimeAssociativeArray>(receiver),
               lyra::runtime::ArrayBody(body),
               lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_assocarray_find_first(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return Emplace(
      out, lyra::value::RuntimeArrayFindFirst(
               Read<RuntimeAssociativeArray>(receiver),
               lyra::runtime::ArrayBody(body),
               lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_assocarray_find_first_index(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return Emplace(
      out, lyra::value::RuntimeArrayFindFirstIndex(
               Read<RuntimeAssociativeArray>(receiver),
               lyra::runtime::ArrayBody(body),
               lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_assocarray_find_last(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return Emplace(
      out, lyra::value::RuntimeArrayFindLast(
               Read<RuntimeAssociativeArray>(receiver),
               lyra::runtime::ArrayBody(body),
               lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_assocarray_find_last_index(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return Emplace(
      out, lyra::value::RuntimeArrayFindLastIndex(
               Read<RuntimeAssociativeArray>(receiver),
               lyra::runtime::ArrayBody(body),
               lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_assocarray_min(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return Emplace(
      out, lyra::value::RuntimeArrayMin(
               Read<RuntimeAssociativeArray>(receiver),
               lyra::runtime::ArrayBody(body),
               lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_assocarray_max(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return Emplace(
      out, lyra::value::RuntimeArrayMax(
               Read<RuntimeAssociativeArray>(receiver),
               lyra::runtime::ArrayBody(body),
               lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_assocarray_unique(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return Emplace(
      out, lyra::value::RuntimeArrayUnique(
               Read<RuntimeAssociativeArray>(receiver),
               lyra::runtime::ArrayBody(body),
               lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_assocarray_unique_index(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return Emplace(
      out, lyra::value::RuntimeArrayUniqueIndex(
               Read<RuntimeAssociativeArray>(receiver),
               lyra::runtime::ArrayBody(body),
               lyra::runtime::TypeAt(prototype_type), prototype));
}

auto lyra_rt_assocarray_map(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void* {
  return Emplace(
      out, lyra::value::RuntimeArrayMap(
               Read<RuntimeAssociativeArray>(receiver),
               lyra::runtime::ArrayBody(body),
               lyra::runtime::TypeAt(prototype_type), prototype));
}

// LRM 7.12.2 ordering methods, which reorder the array where it lies.
void lyra_rt_unpackedarray_sort(void* receiver, void* body) {
  auto& target = *static_cast<RuntimeUnpackedArray*>(receiver);
  lyra::value::RuntimeArraySort(target, lyra::runtime::ArrayBody(body));
}

void lyra_rt_unpackedarray_rsort(void* receiver, void* body) {
  auto& target = *static_cast<RuntimeUnpackedArray*>(receiver);
  lyra::value::RuntimeArrayRsort(target, lyra::runtime::ArrayBody(body));
}

void lyra_rt_dynarray_sort(void* receiver, void* body) {
  auto& target = *static_cast<RuntimeDynamicArray*>(receiver);
  lyra::value::RuntimeArraySort(target, lyra::runtime::ArrayBody(body));
}

void lyra_rt_dynarray_rsort(void* receiver, void* body) {
  auto& target = *static_cast<RuntimeDynamicArray*>(receiver);
  lyra::value::RuntimeArrayRsort(target, lyra::runtime::ArrayBody(body));
}

void lyra_rt_queue_sort(void* receiver, void* body) {
  auto& target = *static_cast<RuntimeQueue*>(receiver);
  lyra::value::RuntimeArraySort(target, lyra::runtime::ArrayBody(body));
}

void lyra_rt_queue_rsort(void* receiver, void* body) {
  auto& target = *static_cast<RuntimeQueue*>(receiver);
  lyra::value::RuntimeArrayRsort(target, lyra::runtime::ArrayBody(body));
}

void lyra_rt_unpackedarray_reverse(void* receiver) {
  auto& target = *static_cast<RuntimeUnpackedArray*>(receiver);
  lyra::value::RuntimeArrayReverse(target);
}

void lyra_rt_dynarray_reverse(void* receiver) {
  auto& target = *static_cast<RuntimeDynamicArray*>(receiver);
  lyra::value::RuntimeArrayReverse(target);
}

void lyra_rt_queue_reverse(void* receiver) {
  auto& target = *static_cast<RuntimeQueue*>(receiver);
  lyra::value::RuntimeArrayReverse(target);
}

auto lyra_rt_unpackedarray_read_mem(
    void* runtime, const void* memory, const void* name, LyraSpan dims,
    std::int64_t base, std::int64_t start, void* out) -> void* {
  return lyra::runtime::EmplaceCompletion(
      out,
      lyra::runtime::Single(
          lyra::runtime::ReadUnpackedMemory(
              *static_cast<RuntimeEffects*>(runtime),
              Read<RuntimeUnpackedArray>(memory), Read<String>(name),
              lyra::value::UnpackedRangesOf(lyra::runtime::MachineIntsOf(dims)),
              base, start, std::nullopt)));
}

auto lyra_rt_unpackedarray_read_mem_within(
    void* runtime, const void* memory, const void* name, LyraSpan dims,
    std::int64_t base, std::int64_t start, std::int64_t finish, void* out)
    -> void* {
  return lyra::runtime::EmplaceCompletion(
      out,
      lyra::runtime::Single(
          lyra::runtime::ReadUnpackedMemory(
              *static_cast<RuntimeEffects*>(runtime),
              Read<RuntimeUnpackedArray>(memory), Read<String>(name),
              lyra::value::UnpackedRangesOf(lyra::runtime::MachineIntsOf(dims)),
              base, start, finish)));
}

void lyra_rt_unpackedarray_write_mem(
    void* runtime, const void* memory, const void* name, LyraSpan dims,
    std::int64_t base, std::int64_t start) {
  lyra::runtime::WriteUnpackedMemory(
      *static_cast<RuntimeEffects*>(runtime),
      Read<RuntimeUnpackedArray>(memory), Read<String>(name),
      lyra::value::UnpackedRangesOf(lyra::runtime::MachineIntsOf(dims)), base,
      start, std::nullopt);
}

void lyra_rt_unpackedarray_write_mem_within(
    void* runtime, const void* memory, const void* name, LyraSpan dims,
    std::int64_t base, std::int64_t start, std::int64_t finish) {
  lyra::runtime::WriteUnpackedMemory(
      *static_cast<RuntimeEffects*>(runtime),
      Read<RuntimeUnpackedArray>(memory), Read<String>(name),
      lyra::value::UnpackedRangesOf(lyra::runtime::MachineIntsOf(dims)), base,
      start, finish);
}

auto lyra_rt_dynarray_read_mem(
    void* runtime, const void* memory, const void* name, std::int64_t base,
    std::int64_t start, void* out) -> void* {
  return lyra::runtime::EmplaceCompletion(
      out, lyra::runtime::Single(
               lyra::runtime::ReadFlatMemory(
                   *static_cast<RuntimeEffects*>(runtime),
                   Read<RuntimeDynamicArray>(memory), Read<String>(name), base,
                   start, std::nullopt)));
}

auto lyra_rt_dynarray_read_mem_within(
    void* runtime, const void* memory, const void* name, std::int64_t base,
    std::int64_t start, std::int64_t finish, void* out) -> void* {
  return lyra::runtime::EmplaceCompletion(
      out, lyra::runtime::Single(
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
    std::int64_t start, void* out) -> void* {
  return lyra::runtime::EmplaceCompletion(
      out, lyra::runtime::Single(
               lyra::runtime::ReadFlatMemory(
                   *static_cast<RuntimeEffects*>(runtime),
                   Read<RuntimeQueue>(memory), Read<String>(name), base, start,
                   std::nullopt)));
}

auto lyra_rt_queue_read_mem_within(
    void* runtime, const void* memory, const void* name, std::int64_t base,
    std::int64_t start, std::int64_t finish, void* out) -> void* {
  return lyra::runtime::EmplaceCompletion(
      out, lyra::runtime::Single(
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
    std::int64_t base, std::int64_t start, void* out) -> void* {
  return lyra::runtime::EmplaceCompletion(
      out, lyra::runtime::Single(
               lyra::runtime::ReadKeyedMemory(
                   *static_cast<RuntimeEffects*>(runtime),
                   Read<RuntimeAssociativeArray>(memory), Read<String>(name),
                   base, start, std::nullopt, key_type)));
}

auto lyra_rt_assocarray_read_mem_within(
    void* runtime, const void* memory, const void* key_type, const void* name,
    std::int64_t base, std::int64_t start, std::int64_t finish, void* out)
    -> void* {
  return lyra::runtime::EmplaceCompletion(
      out, lyra::runtime::Single(
               lyra::runtime::ReadKeyedMemory(
                   *static_cast<RuntimeEffects*>(runtime),
                   Read<RuntimeAssociativeArray>(memory), Read<String>(name),
                   base, start, finish, key_type)));
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
    const void* time_format, std::int64_t timeunit_power, void* out) -> void* {
  const std::span<const void* const> handles{
      static_cast<const void* const*>(args.data), args.count};
  std::vector<FormatArg> arguments(handles.size());
  std::ranges::transform(handles, arguments.begin(), [](const void* handle) {
    return *static_cast<const FormatArg*>(handle);
  });
  return Emplace(
      out, lyra::value::FormatRuntime(
               Read<String>(format), arguments, Read<String>(scope_path),
               Read<TimeFormat>(time_format), timeunit_power));
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
  const lyra::value::LoadedWords planes =
      BitsAt(sv, sv_width, sv_is_four_state);
  return Emplace(out, DpiBitBuffer(planes.Read(), planes.Shape().width));
}

auto lyra_rt_make_dpi_logic_buffer(
    const void* sv, std::int64_t sv_width, bool sv_is_four_state, void* out)
    -> void* {
  const lyra::value::LoadedWords planes =
      BitsAt(sv, sv_width, sv_is_four_state);
  return Emplace(out, DpiLogicBuffer(planes.Read(), planes.Shape().width));
}

auto lyra_rt_dpi_bit_buffer_data(void* buffer) -> void* {
  return static_cast<DpiBitBuffer*>(buffer)->Data();
}

auto lyra_rt_dpi_logic_buffer_data(void* buffer) -> void* {
  return static_cast<DpiLogicBuffer*>(buffer)->Data();
}

void lyra_rt_write_canonical_bit_vec(
    void* dst, const void* sv, std::int64_t sv_width, bool sv_is_four_state) {
  const lyra::value::LoadedWords planes =
      BitsAt(sv, sv_width, sv_is_four_state);
  lyra::value::WriteCanonicalBitVec(
      static_cast<svBitVecVal*>(dst), planes.Read(), planes.Shape().width);
}

void lyra_rt_write_canonical_logic_vec(
    void* dst, const void* sv, std::int64_t sv_width, bool sv_is_four_state) {
  const lyra::value::LoadedWords planes =
      BitsAt(sv, sv_width, sv_is_four_state);
  lyra::value::WriteCanonicalLogicVec(
      static_cast<svLogicVecVal*>(dst), planes.Read(), planes.Shape().width);
}

auto lyra_rt_to_sv_logic(
    const void* sv, std::int64_t sv_width, bool sv_is_four_state)
    -> std::uint8_t {
  const lyra::value::LoadedWords planes =
      BitsAt(sv, sv_width, sv_is_four_state);
  return lyra::value::ToSvLogic(planes.Read());
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
    const void* image, const void* prototype, const void* prototype_type,
    void* out) -> void* {
  const lyra::value::ValueType& type = lyra::runtime::TypeAt(prototype_type);
  lyra::runtime::ElementInto(out, type, prototype);
  const auto& read = *static_cast<const DpiOpenArray*>(image);
  std::size_t position = 0;
  lyra::runtime::ReadImageBack(
      read, out, type, 0, static_cast<std::size_t>(read.Dimensions()),
      position);
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
auto lyra_rt_tuple_cell_open_for_write(void* cell, void* out) -> void* {
  return OpenCellWrite<RuntimeTuple>(cell, out);
}
auto lyra_rt_union_cell_open_for_write(void* cell, void* out) -> void* {
  return OpenCellWrite<RuntimeUnion>(cell, out);
}
auto lyra_rt_tagged_union_cell_open_for_write(void* cell, void* out) -> void* {
  return OpenCellWrite<RuntimeTaggedUnion>(cell, out);
}
auto lyra_rt_dynarray_cell_open_for_write(void* cell, void* out) -> void* {
  return OpenCellWrite<RuntimeDynamicArray>(cell, out);
}
auto lyra_rt_unpackedarray_cell_open_for_write(void* cell, void* out) -> void* {
  return OpenCellWrite<RuntimeUnpackedArray>(cell, out);
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
auto lyra_rt_tuple_ref_open_for_write(void* reference, void* out) -> void* {
  return OpenTupleRefWrite(reference, out);
}
auto lyra_rt_union_ref_open_for_write(void* reference, void* out) -> void* {
  return OpenRefWrite<RuntimeUnion>(reference, out);
}
auto lyra_rt_tagged_union_ref_open_for_write(void* reference, void* out)
    -> void* {
  return OpenRefWrite<RuntimeTaggedUnion>(reference, out);
}
auto lyra_rt_dynarray_ref_open_for_write(void* reference, void* out) -> void* {
  return OpenRefWrite<RuntimeDynamicArray>(reference, out);
}
auto lyra_rt_unpackedarray_ref_open_for_write(void* reference, void* out)
    -> void* {
  return OpenRefWrite<RuntimeUnpackedArray>(reference, out);
}
auto lyra_rt_queue_ref_open_for_write(void* reference, void* out) -> void* {
  return OpenRefWrite<RuntimeQueue>(reference, out);
}
auto lyra_rt_assocarray_ref_open_for_write(void* reference, void* out)
    -> void* {
  return OpenRefWrite<RuntimeAssociativeArray>(reference, out);
}
auto lyra_rt_tuple_driver_open_for_write(void* driver, void* out) -> void* {
  return OpenDriverWrite<RuntimeTuple>(driver, out);
}
auto lyra_rt_union_driver_open_for_write(void* driver, void* out) -> void* {
  return OpenDriverWrite<RuntimeUnion>(driver, out);
}
auto lyra_rt_unpackedarray_driver_open_for_write(void* driver, void* out)
    -> void* {
  return OpenDriverWrite<RuntimeUnpackedArray>(driver, out);
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

auto lyra_rt_unpackedarray_designate_element(
    const void* designation, const void* position, void* out) -> void* {
  return DesignateElement<RuntimeUnpackedArray>(
      designation, PositionNamedAt(position), out);
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

auto lyra_rt_tuple_designate_component(
    const void* designation, std::int64_t index, void* out) -> void* {
  const ErasedDesignation& within = DesignationAt(designation);
  return std::construct_at(
      static_cast<ErasedDesignation*>(out),
      ErasedDesignation{
          .write = within.write,
          .part = RuntimeTuple::ComponentAt(
              within.part, static_cast<std::size_t>(index))});
}

void lyra_rt_dynarray_assign_slice(
    const void* designation, const void* start, std::int64_t count,
    const void* replacement) {
  AssignDesignatedSlice<RuntimeDynamicArray>(
      designation, PositionNamedAt(start), count,
      lyra::runtime::ElementHandles(Read<RuntimeUnpackedArray>(replacement)));
}

void lyra_rt_unpackedarray_assign_slice(
    const void* designation, const void* start, std::int64_t count,
    const void* replacement) {
  AssignDesignatedSlice<RuntimeUnpackedArray>(
      designation, PositionNamedAt(start), count,
      lyra::runtime::ElementHandles(Read<RuntimeUnpackedArray>(replacement)));
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
auto lyra_rt_tuple_land(const void* designation) noexcept -> void* {
  return LandTupleDesignation(designation);
}
auto lyra_rt_union_land(const void* designation) noexcept -> void* {
  return LandDesignation<RuntimeUnion>(designation);
}
auto lyra_rt_tagged_union_land(const void* designation) noexcept -> void* {
  return LandDesignation<RuntimeTaggedUnion>(designation);
}
auto lyra_rt_dynarray_land(const void* designation) noexcept -> void* {
  return LandDesignation<RuntimeDynamicArray>(designation);
}
auto lyra_rt_unpackedarray_land(const void* designation) noexcept -> void* {
  return LandDesignation<RuntimeUnpackedArray>(designation);
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
void lyra_rt_union_assign(void* storage, const void* value) {
  *static_cast<RuntimeUnion*>(storage) = Read<RuntimeUnion>(value);
}
void lyra_rt_tagged_union_assign(void* storage, const void* value) {
  *static_cast<RuntimeTaggedUnion*>(storage) = Read<RuntimeTaggedUnion>(value);
}
void lyra_rt_dynarray_assign(void* storage, const void* value) {
  *static_cast<RuntimeDynamicArray*>(storage) =
      Read<RuntimeDynamicArray>(value);
}
void lyra_rt_unpackedarray_assign(void* storage, const void* value) {
  *static_cast<RuntimeUnpackedArray*>(storage) =
      Read<RuntimeUnpackedArray>(value);
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

// Ending an object the generated body held in its own storage, where ending one
// has something to do. An object whose type has a trivial destructor is ended
// by its storage going away, so it has no entry here.
void lyra_rt_string_destroy(void* object) {
  std::destroy_at(static_cast<String*>(object));
}
void lyra_rt_union_destroy(void* object) {
  std::destroy_at(static_cast<RuntimeUnion*>(object));
}
void lyra_rt_tagged_union_destroy(void* object) {
  std::destroy_at(static_cast<RuntimeTaggedUnion*>(object));
}
void lyra_rt_dynarray_destroy(void* object) {
  std::destroy_at(static_cast<RuntimeDynamicArray*>(object));
}
void lyra_rt_unpackedarray_destroy(void* object) {
  std::destroy_at(static_cast<RuntimeUnpackedArray*>(object));
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
void lyra_rt_closure_destroy(void* object) {
  std::destroy_at(static_cast<OwnedClosure*>(object));
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
auto lyra_rt_union_copy(const void* value, void* out) -> void* {
  return Emplace(out, Read<RuntimeUnion>(value));
}
auto lyra_rt_tagged_union_copy(const void* value, void* out) -> void* {
  return Emplace(out, Read<RuntimeTaggedUnion>(value));
}
auto lyra_rt_dynarray_copy(const void* value, void* out) -> void* {
  return Emplace(out, Read<RuntimeDynamicArray>(value));
}
auto lyra_rt_unpackedarray_copy(const void* value, void* out) -> void* {
  return Emplace(out, Read<RuntimeUnpackedArray>(value));
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
auto lyra_rt_union_move(void* value, void* out) -> void* {
  return Emplace(out, std::move(*static_cast<RuntimeUnion*>(value)));
}
auto lyra_rt_tagged_union_move(void* value, void* out) -> void* {
  return Emplace(out, std::move(*static_cast<RuntimeTaggedUnion*>(value)));
}
auto lyra_rt_dynarray_move(void* value, void* out) -> void* {
  return Emplace(out, std::move(*static_cast<RuntimeDynamicArray*>(value)));
}
auto lyra_rt_unpackedarray_move(void* value, void* out) -> void* {
  return Emplace(out, std::move(*static_cast<RuntimeUnpackedArray*>(value)));
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
auto lyra_rt_closure_move(void* value, void* out) -> void* {
  return Emplace(out, TakeOwner(value));
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
void lyra_rt_tuple_cell_construct(void* storage) {
  BuildAt<Var<RuntimeTuple>>(storage);
}
void lyra_rt_union_cell_construct(void* storage) {
  BuildAt<Var<RuntimeUnion>>(storage);
}
void lyra_rt_tagged_union_cell_construct(void* storage) {
  BuildAt<Var<RuntimeTaggedUnion>>(storage);
}
void lyra_rt_dynarray_cell_construct(void* storage) {
  BuildAt<Var<RuntimeDynamicArray>>(storage);
}
void lyra_rt_unpackedarray_cell_construct(void* storage) {
  BuildAt<Var<RuntimeUnpackedArray>>(storage);
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
  BuildAt<ActivationValueCell<String>>(storage);
}
void lyra_rt_real_value_cell_construct(void* storage) {
  BuildAt<ActivationValueCell<Real>>(storage);
}
void lyra_rt_shortreal_value_cell_construct(void* storage) {
  BuildAt<ActivationValueCell<ShortReal>>(storage);
}
void lyra_rt_chandle_value_cell_construct(void* storage) {
  BuildAt<ActivationValueCell<Chandle>>(storage);
}
void lyra_rt_tuple_value_cell_construct(void* storage) {
  BuildAt<ActivationValueCell<RuntimeTuple>>(storage);
}
void lyra_rt_union_value_cell_construct(void* storage) {
  BuildAt<ActivationValueCell<RuntimeUnion>>(storage);
}
void lyra_rt_tagged_union_value_cell_construct(void* storage) {
  BuildAt<ActivationValueCell<RuntimeTaggedUnion>>(storage);
}
void lyra_rt_dynarray_value_cell_construct(void* storage) {
  BuildAt<ActivationValueCell<RuntimeDynamicArray>>(storage);
}
void lyra_rt_unpackedarray_value_cell_construct(void* storage) {
  BuildAt<ActivationValueCell<RuntimeUnpackedArray>>(storage);
}
void lyra_rt_queue_value_cell_construct(void* storage) {
  BuildAt<ActivationValueCell<RuntimeQueue>>(storage);
}
void lyra_rt_assocarray_value_cell_construct(void* storage) {
  BuildAt<ActivationValueCell<RuntimeAssociativeArray>>(storage);
}
void lyra_rt_managedref_value_cell_construct(void* storage) {
  BuildAt<ActivationValueCell<ObjectRef>>(storage);
}
void lyra_rt_tuple_net_construct(void* storage) {
  BuildAt<ResolvedNet<RuntimeTuple>>(storage);
}
void lyra_rt_union_net_construct(void* storage) {
  BuildAt<ResolvedNet<RuntimeUnion>>(storage);
}
void lyra_rt_unpackedarray_net_construct(void* storage) {
  BuildAt<ResolvedNet<RuntimeUnpackedArray>>(storage);
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
void lyra_rt_tuple_sampled_history_construct(void* storage) {
  BuildAt<SampledHistory<RuntimeTuple>>(storage);
}
void lyra_rt_union_sampled_history_construct(void* storage) {
  BuildAt<SampledHistory<RuntimeUnion>>(storage);
}
void lyra_rt_tagged_union_sampled_history_construct(void* storage) {
  BuildAt<SampledHistory<RuntimeTaggedUnion>>(storage);
}
void lyra_rt_dynarray_sampled_history_construct(void* storage) {
  BuildAt<SampledHistory<RuntimeDynamicArray>>(storage);
}
void lyra_rt_unpackedarray_sampled_history_construct(void* storage) {
  BuildAt<SampledHistory<RuntimeUnpackedArray>>(storage);
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
void lyra_rt_channel_cancellation_construct(void* storage) {
  BuildAt<ChannelCancellation>(storage);
}
void lyra_rt_shared_pointer_construct(void* storage) {
  BuildAt<SharedPointer>(storage);
}

void lyra_rt_string_cell_destroy(void* storage) {
  std::destroy_at(static_cast<Var<String>*>(storage));
}
void lyra_rt_real_cell_destroy(void* storage) {
  std::destroy_at(static_cast<Var<Real>*>(storage));
}
void lyra_rt_shortreal_cell_destroy(void* storage) {
  std::destroy_at(static_cast<Var<ShortReal>*>(storage));
}
void lyra_rt_chandle_cell_destroy(void* storage) {
  std::destroy_at(static_cast<Var<Chandle>*>(storage));
}
void lyra_rt_tuple_cell_destroy(void* storage) {
  std::destroy_at(static_cast<Var<RuntimeTuple>*>(storage));
}
void lyra_rt_union_cell_destroy(void* storage) {
  std::destroy_at(static_cast<Var<RuntimeUnion>*>(storage));
}
void lyra_rt_tagged_union_cell_destroy(void* storage) {
  std::destroy_at(static_cast<Var<RuntimeTaggedUnion>*>(storage));
}
void lyra_rt_dynarray_cell_destroy(void* storage) {
  std::destroy_at(static_cast<Var<RuntimeDynamicArray>*>(storage));
}
void lyra_rt_unpackedarray_cell_destroy(void* storage) {
  std::destroy_at(static_cast<Var<RuntimeUnpackedArray>*>(storage));
}
void lyra_rt_queue_cell_destroy(void* storage) {
  std::destroy_at(static_cast<Var<RuntimeQueue>*>(storage));
}
void lyra_rt_assocarray_cell_destroy(void* storage) {
  std::destroy_at(static_cast<Var<RuntimeAssociativeArray>*>(storage));
}
void lyra_rt_managedref_cell_destroy(void* storage) {
  std::destroy_at(static_cast<Var<ObjectRef>*>(storage));
}
void lyra_rt_string_value_cell_destroy(void* storage) {
  std::destroy_at(static_cast<ActivationValueCell<String>*>(storage));
}
void lyra_rt_tuple_value_cell_destroy(void* storage) {
  std::destroy_at(static_cast<ActivationValueCell<RuntimeTuple>*>(storage));
}
void lyra_rt_union_value_cell_destroy(void* storage) {
  std::destroy_at(static_cast<ActivationValueCell<RuntimeUnion>*>(storage));
}
void lyra_rt_tagged_union_value_cell_destroy(void* storage) {
  std::destroy_at(
      static_cast<ActivationValueCell<RuntimeTaggedUnion>*>(storage));
}
void lyra_rt_dynarray_value_cell_destroy(void* storage) {
  std::destroy_at(
      static_cast<ActivationValueCell<RuntimeDynamicArray>*>(storage));
}
void lyra_rt_unpackedarray_value_cell_destroy(void* storage) {
  std::destroy_at(
      static_cast<ActivationValueCell<RuntimeUnpackedArray>*>(storage));
}
void lyra_rt_queue_value_cell_destroy(void* storage) {
  std::destroy_at(static_cast<ActivationValueCell<RuntimeQueue>*>(storage));
}
void lyra_rt_assocarray_value_cell_destroy(void* storage) {
  std::destroy_at(
      static_cast<ActivationValueCell<RuntimeAssociativeArray>*>(storage));
}
void lyra_rt_managedref_value_cell_destroy(void* storage) {
  std::destroy_at(static_cast<ActivationValueCell<ObjectRef>*>(storage));
}
void lyra_rt_tuple_net_destroy(void* storage) {
  std::destroy_at(static_cast<ResolvedNet<RuntimeTuple>*>(storage));
}
void lyra_rt_union_net_destroy(void* storage) {
  std::destroy_at(static_cast<ResolvedNet<RuntimeUnion>*>(storage));
}
void lyra_rt_unpackedarray_net_destroy(void* storage) {
  std::destroy_at(static_cast<ResolvedNet<RuntimeUnpackedArray>*>(storage));
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
void lyra_rt_tuple_sampled_history_destroy(void* storage) {
  std::destroy_at(static_cast<SampledHistory<RuntimeTuple>*>(storage));
}
void lyra_rt_union_sampled_history_destroy(void* storage) {
  std::destroy_at(static_cast<SampledHistory<RuntimeUnion>*>(storage));
}
void lyra_rt_tagged_union_sampled_history_destroy(void* storage) {
  std::destroy_at(static_cast<SampledHistory<RuntimeTaggedUnion>*>(storage));
}
void lyra_rt_dynarray_sampled_history_destroy(void* storage) {
  std::destroy_at(static_cast<SampledHistory<RuntimeDynamicArray>*>(storage));
}
void lyra_rt_unpackedarray_sampled_history_destroy(void* storage) {
  std::destroy_at(static_cast<SampledHistory<RuntimeUnpackedArray>*>(storage));
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

// The holders over each layout an integral value no wider than a word has. A
// value crosses as its own bytes, which a holder keeps as they are. What an
// entry is told of the value's type is a count where the bytes cannot say it:
// the positions a net resolves, the declared width a write into some bits is
// bounded by.
void lyra_rt_bit8_cell_construct(void* storage) {
  BuildAt<Var<Bit8>>(storage);
}
void lyra_rt_bit8_cell_destroy(void* storage) {
  std::destroy_at(&CellAt<Bit8>(storage));
}
void lyra_rt_bit8_cell_initialize(void* cell, const void* prototype) noexcept {
  CellAt<Bit8>(cell).Initialize(Read<Bit8>(prototype));
}
void lyra_rt_bit8_cell_set(void* cell, const void* value) {
  CellAt<Bit8>(cell).Set(Read<Bit8>(value));
}
void lyra_rt_bit8_cell_arm_sampling(void* cell) {
  CellAt<Bit8>(cell).ArmSampling();
}
auto lyra_rt_bit8_cell_sampled_load(void* cell, void* out) -> void* {
  return Emplace(out, CellAt<Bit8>(cell).SampledGet());
}
auto lyra_rt_bit8_cell_begin_takeover(void* cell, std::int64_t level)
    -> std::int64_t {
  return CellAt<Bit8>(cell).BeginTakeover(level);
}
auto lyra_rt_bit8_cell_drive_takeover(
    void* cell, std::int64_t level, std::int64_t generation, const void* value)
    -> bool {
  return CellAt<Bit8>(cell).DriveTakeover(level, generation, Read<Bit8>(value));
}
void lyra_rt_bit8_cell_end_takeover(void* cell, std::int64_t level) {
  CellAt<Bit8>(cell).EndTakeover(level);
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
auto lyra_rt_bit8_ref_sampled_load(void* reference, void* out) -> void* {
  return RefSampledLoad<Bit8>(reference, out);
}
auto lyra_rt_bit8_ref_open_for_write(void* reference, void* out) -> void* {
  return OpenRefWrite<Bit8>(reference, out);
}
auto lyra_rt_bit8_value_cell_alloc() noexcept -> void* {
  return AllocateValueCell<Bit8>();
}
void lyra_rt_bit8_value_cell_construct(void* storage) {
  BuildAt<ActivationValueCell<Bit8>>(storage);
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
void lyra_rt_bit8_report_bits(
    const void* designation, std::int64_t width, const void* start,
    const void* written, std::int64_t bits_width) {
  AssignDesignatedBits<Bit8>(designation, width, start, bits_width, written);
}

void lyra_rt_bit16_cell_construct(void* storage) {
  BuildAt<Var<Bit16>>(storage);
}
void lyra_rt_bit16_cell_destroy(void* storage) {
  std::destroy_at(&CellAt<Bit16>(storage));
}
void lyra_rt_bit16_cell_initialize(void* cell, const void* prototype) noexcept {
  CellAt<Bit16>(cell).Initialize(Read<Bit16>(prototype));
}
void lyra_rt_bit16_cell_set(void* cell, const void* value) {
  CellAt<Bit16>(cell).Set(Read<Bit16>(value));
}
void lyra_rt_bit16_cell_arm_sampling(void* cell) {
  CellAt<Bit16>(cell).ArmSampling();
}
auto lyra_rt_bit16_cell_sampled_load(void* cell, void* out) -> void* {
  return Emplace(out, CellAt<Bit16>(cell).SampledGet());
}
auto lyra_rt_bit16_cell_begin_takeover(void* cell, std::int64_t level)
    -> std::int64_t {
  return CellAt<Bit16>(cell).BeginTakeover(level);
}
auto lyra_rt_bit16_cell_drive_takeover(
    void* cell, std::int64_t level, std::int64_t generation, const void* value)
    -> bool {
  return CellAt<Bit16>(cell).DriveTakeover(
      level, generation, Read<Bit16>(value));
}
void lyra_rt_bit16_cell_end_takeover(void* cell, std::int64_t level) {
  CellAt<Bit16>(cell).EndTakeover(level);
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
auto lyra_rt_bit16_ref_sampled_load(void* reference, void* out) -> void* {
  return RefSampledLoad<Bit16>(reference, out);
}
auto lyra_rt_bit16_ref_open_for_write(void* reference, void* out) -> void* {
  return OpenRefWrite<Bit16>(reference, out);
}
auto lyra_rt_bit16_value_cell_alloc() noexcept -> void* {
  return AllocateValueCell<Bit16>();
}
void lyra_rt_bit16_value_cell_construct(void* storage) {
  BuildAt<ActivationValueCell<Bit16>>(storage);
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
void lyra_rt_bit16_report_bits(
    const void* designation, std::int64_t width, const void* start,
    const void* written, std::int64_t bits_width) {
  AssignDesignatedBits<Bit16>(designation, width, start, bits_width, written);
}

void lyra_rt_bit32_cell_construct(void* storage) {
  BuildAt<Var<Bit32>>(storage);
}
void lyra_rt_bit32_cell_destroy(void* storage) {
  std::destroy_at(&CellAt<Bit32>(storage));
}
void lyra_rt_bit32_cell_initialize(void* cell, const void* prototype) noexcept {
  CellAt<Bit32>(cell).Initialize(Read<Bit32>(prototype));
}
void lyra_rt_bit32_cell_set(void* cell, const void* value) {
  CellAt<Bit32>(cell).Set(Read<Bit32>(value));
}
void lyra_rt_bit32_cell_arm_sampling(void* cell) {
  CellAt<Bit32>(cell).ArmSampling();
}
auto lyra_rt_bit32_cell_sampled_load(void* cell, void* out) -> void* {
  return Emplace(out, CellAt<Bit32>(cell).SampledGet());
}
auto lyra_rt_bit32_cell_begin_takeover(void* cell, std::int64_t level)
    -> std::int64_t {
  return CellAt<Bit32>(cell).BeginTakeover(level);
}
auto lyra_rt_bit32_cell_drive_takeover(
    void* cell, std::int64_t level, std::int64_t generation, const void* value)
    -> bool {
  return CellAt<Bit32>(cell).DriveTakeover(
      level, generation, Read<Bit32>(value));
}
void lyra_rt_bit32_cell_end_takeover(void* cell, std::int64_t level) {
  CellAt<Bit32>(cell).EndTakeover(level);
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
auto lyra_rt_bit32_ref_sampled_load(void* reference, void* out) -> void* {
  return RefSampledLoad<Bit32>(reference, out);
}
auto lyra_rt_bit32_ref_open_for_write(void* reference, void* out) -> void* {
  return OpenRefWrite<Bit32>(reference, out);
}
auto lyra_rt_bit32_value_cell_alloc() noexcept -> void* {
  return AllocateValueCell<Bit32>();
}
void lyra_rt_bit32_value_cell_construct(void* storage) {
  BuildAt<ActivationValueCell<Bit32>>(storage);
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
void lyra_rt_bit32_report_bits(
    const void* designation, std::int64_t width, const void* start,
    const void* written, std::int64_t bits_width) {
  AssignDesignatedBits<Bit32>(designation, width, start, bits_width, written);
}

void lyra_rt_bit64_cell_construct(void* storage) {
  BuildAt<Var<Bit64>>(storage);
}
void lyra_rt_bit64_cell_destroy(void* storage) {
  std::destroy_at(&CellAt<Bit64>(storage));
}
void lyra_rt_bit64_cell_initialize(void* cell, const void* prototype) noexcept {
  CellAt<Bit64>(cell).Initialize(Read<Bit64>(prototype));
}
void lyra_rt_bit64_cell_set(void* cell, const void* value) {
  CellAt<Bit64>(cell).Set(Read<Bit64>(value));
}
void lyra_rt_bit64_cell_arm_sampling(void* cell) {
  CellAt<Bit64>(cell).ArmSampling();
}
auto lyra_rt_bit64_cell_sampled_load(void* cell, void* out) -> void* {
  return Emplace(out, CellAt<Bit64>(cell).SampledGet());
}
auto lyra_rt_bit64_cell_begin_takeover(void* cell, std::int64_t level)
    -> std::int64_t {
  return CellAt<Bit64>(cell).BeginTakeover(level);
}
auto lyra_rt_bit64_cell_drive_takeover(
    void* cell, std::int64_t level, std::int64_t generation, const void* value)
    -> bool {
  return CellAt<Bit64>(cell).DriveTakeover(
      level, generation, Read<Bit64>(value));
}
void lyra_rt_bit64_cell_end_takeover(void* cell, std::int64_t level) {
  CellAt<Bit64>(cell).EndTakeover(level);
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
auto lyra_rt_bit64_ref_sampled_load(void* reference, void* out) -> void* {
  return RefSampledLoad<Bit64>(reference, out);
}
auto lyra_rt_bit64_ref_open_for_write(void* reference, void* out) -> void* {
  return OpenRefWrite<Bit64>(reference, out);
}
auto lyra_rt_bit64_value_cell_alloc() noexcept -> void* {
  return AllocateValueCell<Bit64>();
}
void lyra_rt_bit64_value_cell_construct(void* storage) {
  BuildAt<ActivationValueCell<Bit64>>(storage);
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
void lyra_rt_bit64_report_bits(
    const void* designation, std::int64_t width, const void* start,
    const void* written, std::int64_t bits_width) {
  AssignDesignatedBits<Bit64>(designation, width, start, bits_width, written);
}

void lyra_rt_logic8_cell_construct(void* storage) {
  BuildAt<Var<Logic8>>(storage);
}
void lyra_rt_logic8_cell_destroy(void* storage) {
  std::destroy_at(&CellAt<Logic8>(storage));
}
void lyra_rt_logic8_cell_initialize(
    void* cell, const void* prototype) noexcept {
  CellAt<Logic8>(cell).Initialize(Read<Logic8>(prototype));
}
void lyra_rt_logic8_cell_set(void* cell, const void* value) {
  CellAt<Logic8>(cell).Set(Read<Logic8>(value));
}
void lyra_rt_logic8_cell_arm_sampling(void* cell) {
  CellAt<Logic8>(cell).ArmSampling();
}
auto lyra_rt_logic8_cell_sampled_load(void* cell, void* out) -> void* {
  return Emplace(out, CellAt<Logic8>(cell).SampledGet());
}
auto lyra_rt_logic8_cell_begin_takeover(void* cell, std::int64_t level)
    -> std::int64_t {
  return CellAt<Logic8>(cell).BeginTakeover(level);
}
auto lyra_rt_logic8_cell_drive_takeover(
    void* cell, std::int64_t level, std::int64_t generation, const void* value)
    -> bool {
  return CellAt<Logic8>(cell).DriveTakeover(
      level, generation, Read<Logic8>(value));
}
void lyra_rt_logic8_cell_end_takeover(void* cell, std::int64_t level) {
  CellAt<Logic8>(cell).EndTakeover(level);
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
auto lyra_rt_logic8_ref_sampled_load(void* reference, void* out) -> void* {
  return RefSampledLoad<Logic8>(reference, out);
}
auto lyra_rt_logic8_ref_open_for_write(void* reference, void* out) -> void* {
  return OpenRefWrite<Logic8>(reference, out);
}
auto lyra_rt_logic8_value_cell_alloc() noexcept -> void* {
  return AllocateValueCell<Logic8>();
}
void lyra_rt_logic8_value_cell_construct(void* storage) {
  BuildAt<ActivationValueCell<Logic8>>(storage);
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
void lyra_rt_logic8_report_bits(
    const void* designation, std::int64_t width, const void* start,
    const void* written, std::int64_t bits_width) {
  AssignDesignatedBits<Logic8>(designation, width, start, bits_width, written);
}
void lyra_rt_logic8_net_construct(void* storage) {
  BuildAt<ResolvedNet<Logic8>>(storage);
}
void lyra_rt_logic8_net_destroy(void* storage) {
  std::destroy_at(&NetOf<Logic8>(storage));
}
void lyra_rt_logic8_net_initialize_tri_state(
    void* net, std::int64_t count, std::int64_t fill, std::int64_t strength) {
  NetOf<Logic8>(net).InitializeTriState(count, fill, strength);
}
void lyra_rt_logic8_net_initialize_wired_and(
    void* net, std::int64_t count, std::int64_t fill, std::int64_t strength) {
  NetOf<Logic8>(net).InitializeWiredAnd(count, fill, strength);
}
void lyra_rt_logic8_net_initialize_wired_or(
    void* net, std::int64_t count, std::int64_t fill, std::int64_t strength) {
  NetOf<Logic8>(net).InitializeWiredOr(count, fill, strength);
}
void lyra_rt_logic8_net_initialize_retaining(
    void* net, std::int64_t count, std::int64_t fill, std::int64_t strength) {
  NetOf<Logic8>(net).InitializeRetaining(count, fill, strength);
}
auto lyra_rt_logic8_net_begin_takeover(void* net, std::int64_t level)
    -> std::int64_t {
  return NetOf<Logic8>(net).BeginTakeover(level);
}
auto lyra_rt_logic8_net_drive_takeover(
    void* net, std::int64_t level, std::int64_t generation, const void* value)
    -> bool {
  return NetOf<Logic8>(net).DriveTakeover(
      level, generation, Read<Logic8>(value));
}
void lyra_rt_logic8_net_end_takeover(void* net, std::int64_t level) {
  NetOf<Logic8>(net).EndTakeover(level);
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
  std::destroy_at(&CellAt<Logic16>(storage));
}
void lyra_rt_logic16_cell_initialize(
    void* cell, const void* prototype) noexcept {
  CellAt<Logic16>(cell).Initialize(Read<Logic16>(prototype));
}
void lyra_rt_logic16_cell_set(void* cell, const void* value) {
  CellAt<Logic16>(cell).Set(Read<Logic16>(value));
}
void lyra_rt_logic16_cell_arm_sampling(void* cell) {
  CellAt<Logic16>(cell).ArmSampling();
}
auto lyra_rt_logic16_cell_sampled_load(void* cell, void* out) -> void* {
  return Emplace(out, CellAt<Logic16>(cell).SampledGet());
}
auto lyra_rt_logic16_cell_begin_takeover(void* cell, std::int64_t level)
    -> std::int64_t {
  return CellAt<Logic16>(cell).BeginTakeover(level);
}
auto lyra_rt_logic16_cell_drive_takeover(
    void* cell, std::int64_t level, std::int64_t generation, const void* value)
    -> bool {
  return CellAt<Logic16>(cell).DriveTakeover(
      level, generation, Read<Logic16>(value));
}
void lyra_rt_logic16_cell_end_takeover(void* cell, std::int64_t level) {
  CellAt<Logic16>(cell).EndTakeover(level);
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
auto lyra_rt_logic16_ref_sampled_load(void* reference, void* out) -> void* {
  return RefSampledLoad<Logic16>(reference, out);
}
auto lyra_rt_logic16_ref_open_for_write(void* reference, void* out) -> void* {
  return OpenRefWrite<Logic16>(reference, out);
}
auto lyra_rt_logic16_value_cell_alloc() noexcept -> void* {
  return AllocateValueCell<Logic16>();
}
void lyra_rt_logic16_value_cell_construct(void* storage) {
  BuildAt<ActivationValueCell<Logic16>>(storage);
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
void lyra_rt_logic16_report_bits(
    const void* designation, std::int64_t width, const void* start,
    const void* written, std::int64_t bits_width) {
  AssignDesignatedBits<Logic16>(designation, width, start, bits_width, written);
}
void lyra_rt_logic16_net_construct(void* storage) {
  BuildAt<ResolvedNet<Logic16>>(storage);
}
void lyra_rt_logic16_net_destroy(void* storage) {
  std::destroy_at(&NetOf<Logic16>(storage));
}
void lyra_rt_logic16_net_initialize_tri_state(
    void* net, std::int64_t count, std::int64_t fill, std::int64_t strength) {
  NetOf<Logic16>(net).InitializeTriState(count, fill, strength);
}
void lyra_rt_logic16_net_initialize_wired_and(
    void* net, std::int64_t count, std::int64_t fill, std::int64_t strength) {
  NetOf<Logic16>(net).InitializeWiredAnd(count, fill, strength);
}
void lyra_rt_logic16_net_initialize_wired_or(
    void* net, std::int64_t count, std::int64_t fill, std::int64_t strength) {
  NetOf<Logic16>(net).InitializeWiredOr(count, fill, strength);
}
void lyra_rt_logic16_net_initialize_retaining(
    void* net, std::int64_t count, std::int64_t fill, std::int64_t strength) {
  NetOf<Logic16>(net).InitializeRetaining(count, fill, strength);
}
auto lyra_rt_logic16_net_begin_takeover(void* net, std::int64_t level)
    -> std::int64_t {
  return NetOf<Logic16>(net).BeginTakeover(level);
}
auto lyra_rt_logic16_net_drive_takeover(
    void* net, std::int64_t level, std::int64_t generation, const void* value)
    -> bool {
  return NetOf<Logic16>(net).DriveTakeover(
      level, generation, Read<Logic16>(value));
}
void lyra_rt_logic16_net_end_takeover(void* net, std::int64_t level) {
  NetOf<Logic16>(net).EndTakeover(level);
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
  std::destroy_at(&CellAt<Logic32>(storage));
}
void lyra_rt_logic32_cell_initialize(
    void* cell, const void* prototype) noexcept {
  CellAt<Logic32>(cell).Initialize(Read<Logic32>(prototype));
}
void lyra_rt_logic32_cell_set(void* cell, const void* value) {
  CellAt<Logic32>(cell).Set(Read<Logic32>(value));
}
void lyra_rt_logic32_cell_arm_sampling(void* cell) {
  CellAt<Logic32>(cell).ArmSampling();
}
auto lyra_rt_logic32_cell_sampled_load(void* cell, void* out) -> void* {
  return Emplace(out, CellAt<Logic32>(cell).SampledGet());
}
auto lyra_rt_logic32_cell_begin_takeover(void* cell, std::int64_t level)
    -> std::int64_t {
  return CellAt<Logic32>(cell).BeginTakeover(level);
}
auto lyra_rt_logic32_cell_drive_takeover(
    void* cell, std::int64_t level, std::int64_t generation, const void* value)
    -> bool {
  return CellAt<Logic32>(cell).DriveTakeover(
      level, generation, Read<Logic32>(value));
}
void lyra_rt_logic32_cell_end_takeover(void* cell, std::int64_t level) {
  CellAt<Logic32>(cell).EndTakeover(level);
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
auto lyra_rt_logic32_ref_sampled_load(void* reference, void* out) -> void* {
  return RefSampledLoad<Logic32>(reference, out);
}
auto lyra_rt_logic32_ref_open_for_write(void* reference, void* out) -> void* {
  return OpenRefWrite<Logic32>(reference, out);
}
auto lyra_rt_logic32_value_cell_alloc() noexcept -> void* {
  return AllocateValueCell<Logic32>();
}
void lyra_rt_logic32_value_cell_construct(void* storage) {
  BuildAt<ActivationValueCell<Logic32>>(storage);
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
void lyra_rt_logic32_report_bits(
    const void* designation, std::int64_t width, const void* start,
    const void* written, std::int64_t bits_width) {
  AssignDesignatedBits<Logic32>(designation, width, start, bits_width, written);
}
void lyra_rt_logic32_net_construct(void* storage) {
  BuildAt<ResolvedNet<Logic32>>(storage);
}
void lyra_rt_logic32_net_destroy(void* storage) {
  std::destroy_at(&NetOf<Logic32>(storage));
}
void lyra_rt_logic32_net_initialize_tri_state(
    void* net, std::int64_t count, std::int64_t fill, std::int64_t strength) {
  NetOf<Logic32>(net).InitializeTriState(count, fill, strength);
}
void lyra_rt_logic32_net_initialize_wired_and(
    void* net, std::int64_t count, std::int64_t fill, std::int64_t strength) {
  NetOf<Logic32>(net).InitializeWiredAnd(count, fill, strength);
}
void lyra_rt_logic32_net_initialize_wired_or(
    void* net, std::int64_t count, std::int64_t fill, std::int64_t strength) {
  NetOf<Logic32>(net).InitializeWiredOr(count, fill, strength);
}
void lyra_rt_logic32_net_initialize_retaining(
    void* net, std::int64_t count, std::int64_t fill, std::int64_t strength) {
  NetOf<Logic32>(net).InitializeRetaining(count, fill, strength);
}
auto lyra_rt_logic32_net_begin_takeover(void* net, std::int64_t level)
    -> std::int64_t {
  return NetOf<Logic32>(net).BeginTakeover(level);
}
auto lyra_rt_logic32_net_drive_takeover(
    void* net, std::int64_t level, std::int64_t generation, const void* value)
    -> bool {
  return NetOf<Logic32>(net).DriveTakeover(
      level, generation, Read<Logic32>(value));
}
void lyra_rt_logic32_net_end_takeover(void* net, std::int64_t level) {
  NetOf<Logic32>(net).EndTakeover(level);
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
  std::destroy_at(&CellAt<Logic64>(storage));
}
void lyra_rt_logic64_cell_initialize(
    void* cell, const void* prototype) noexcept {
  CellAt<Logic64>(cell).Initialize(Read<Logic64>(prototype));
}
void lyra_rt_logic64_cell_set(void* cell, const void* value) {
  CellAt<Logic64>(cell).Set(Read<Logic64>(value));
}
void lyra_rt_logic64_cell_arm_sampling(void* cell) {
  CellAt<Logic64>(cell).ArmSampling();
}
auto lyra_rt_logic64_cell_sampled_load(void* cell, void* out) -> void* {
  return Emplace(out, CellAt<Logic64>(cell).SampledGet());
}
auto lyra_rt_logic64_cell_begin_takeover(void* cell, std::int64_t level)
    -> std::int64_t {
  return CellAt<Logic64>(cell).BeginTakeover(level);
}
auto lyra_rt_logic64_cell_drive_takeover(
    void* cell, std::int64_t level, std::int64_t generation, const void* value)
    -> bool {
  return CellAt<Logic64>(cell).DriveTakeover(
      level, generation, Read<Logic64>(value));
}
void lyra_rt_logic64_cell_end_takeover(void* cell, std::int64_t level) {
  CellAt<Logic64>(cell).EndTakeover(level);
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
auto lyra_rt_logic64_ref_sampled_load(void* reference, void* out) -> void* {
  return RefSampledLoad<Logic64>(reference, out);
}
auto lyra_rt_logic64_ref_open_for_write(void* reference, void* out) -> void* {
  return OpenRefWrite<Logic64>(reference, out);
}
auto lyra_rt_logic64_value_cell_alloc() noexcept -> void* {
  return AllocateValueCell<Logic64>();
}
void lyra_rt_logic64_value_cell_construct(void* storage) {
  BuildAt<ActivationValueCell<Logic64>>(storage);
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
void lyra_rt_logic64_report_bits(
    const void* designation, std::int64_t width, const void* start,
    const void* written, std::int64_t bits_width) {
  AssignDesignatedBits<Logic64>(designation, width, start, bits_width, written);
}
void lyra_rt_logic64_net_construct(void* storage) {
  BuildAt<ResolvedNet<Logic64>>(storage);
}
void lyra_rt_logic64_net_destroy(void* storage) {
  std::destroy_at(&NetOf<Logic64>(storage));
}
void lyra_rt_logic64_net_initialize_tri_state(
    void* net, std::int64_t count, std::int64_t fill, std::int64_t strength) {
  NetOf<Logic64>(net).InitializeTriState(count, fill, strength);
}
void lyra_rt_logic64_net_initialize_wired_and(
    void* net, std::int64_t count, std::int64_t fill, std::int64_t strength) {
  NetOf<Logic64>(net).InitializeWiredAnd(count, fill, strength);
}
void lyra_rt_logic64_net_initialize_wired_or(
    void* net, std::int64_t count, std::int64_t fill, std::int64_t strength) {
  NetOf<Logic64>(net).InitializeWiredOr(count, fill, strength);
}
void lyra_rt_logic64_net_initialize_retaining(
    void* net, std::int64_t count, std::int64_t fill, std::int64_t strength) {
  NetOf<Logic64>(net).InitializeRetaining(count, fill, strength);
}
auto lyra_rt_logic64_net_begin_takeover(void* net, std::int64_t level)
    -> std::int64_t {
  return NetOf<Logic64>(net).BeginTakeover(level);
}
auto lyra_rt_logic64_net_drive_takeover(
    void* net, std::int64_t level, std::int64_t generation, const void* value)
    -> bool {
  return NetOf<Logic64>(net).DriveTakeover(
      level, generation, Read<Logic64>(value));
}
void lyra_rt_logic64_net_end_takeover(void* net, std::int64_t level) {
  NetOf<Logic64>(net).EndTakeover(level);
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

void lyra_rt_bit_wide_cell_construct(void* storage) {
  BuildAt<Var<BitWide>>(storage);
}
void lyra_rt_bit_wide_cell_destroy(void* storage) {
  std::destroy_at(&CellAt<BitWide>(storage));
}
auto lyra_rt_bit_wide_cell_get(void* cell) -> const void* {
  return CellAt<BitWide>(cell).Get().Bytes();
}
void lyra_rt_bit_wide_cell_initialize(
    void* cell, const void* prototype, std::int64_t width) noexcept {
  CellAt<BitWide>(cell).Initialize(
      lyra::runtime::WideOf<BitWide>(width, prototype));
}
void lyra_rt_bit_wide_cell_set(void* cell, const void* value) {
  CellAt<BitWide>(cell).SetBytes(value);
}
void lyra_rt_bit_wide_cell_arm_sampling(void* cell) {
  CellAt<BitWide>(cell).ArmSampling();
}
auto lyra_rt_bit_wide_cell_sampled_load(void* cell, void* out) -> void* {
  return Emplace(out, CellAt<BitWide>(cell).SampledGet());
}
auto lyra_rt_bit_wide_cell_begin_takeover(void* cell, std::int64_t level)
    -> std::int64_t {
  return CellAt<BitWide>(cell).BeginTakeover(level);
}
auto lyra_rt_bit_wide_cell_drive_takeover(
    void* cell, std::int64_t level, std::int64_t generation, const void* value)
    -> bool {
  return CellAt<BitWide>(cell).DriveTakeoverBytes(level, generation, value);
}
void lyra_rt_bit_wide_cell_end_takeover(void* cell, std::int64_t level) {
  CellAt<BitWide>(cell).EndTakeover(level);
}
auto lyra_rt_bit_wide_cell_refer(void* cell, void* out) -> void* {
  return lyra::runtime::ReferToWideCell<BitWide>(cell, out);
}
auto lyra_rt_bit_wide_cell_open_for_write(void* cell, void* out) -> void* {
  return OpenCellWrite<BitWide>(cell, out);
}
auto lyra_rt_bit_wide_shared_cell_make(void* out) -> void* {
  return MakeSharedCell<BitWide>(out);
}
auto lyra_rt_bit_wide_ref_get(void* reference) -> const void* {
  return ErasedAt(reference).storage;
}
void lyra_rt_bit_wide_ref_set(
    void* reference, std::int64_t width, const void* value) {
  lyra::runtime::WideRefSet<BitWide>(reference, width, value);
}
void lyra_rt_bit_wide_ref_arm_sampling(void* reference) {
  lyra::runtime::WideRefArmSampling<BitWide>(reference);
}
auto lyra_rt_bit_wide_ref_sampled_load(
    void* reference, std::int64_t width, void* out) -> void* {
  return lyra::runtime::WideRefSampledLoad<BitWide>(reference, width, out);
}
auto lyra_rt_bit_wide_ref_open_for_write(
    void* reference, std::int64_t width, void* out) -> void* {
  return lyra::runtime::OpenWideRefWrite<BitWide>(reference, width, out);
}
auto lyra_rt_bit_wide_value_cell_alloc(std::int64_t width) noexcept -> void* {
  return lyra::runtime::AllocateWideValueCell<BitWide>(width);
}
void lyra_rt_bit_wide_value_cell_construct(void* storage, std::int64_t width) {
  lyra::runtime::BuildWideValueCell<BitWide>(storage, width);
}
void lyra_rt_bit_wide_value_cell_destroy(void* storage) {
  std::destroy_at(&lyra::runtime::ValueCellAt<BitWide>(storage));
}
void lyra_rt_bit_wide_value_cell_store(void* cell, const void* value) noexcept {
  lyra::runtime::ValueCellAt<BitWide>(cell).Storage().TakeBytes(value);
}
auto lyra_rt_bit_wide_value_cell_load(void* cell) noexcept -> void* {
  return lyra::runtime::ValueCellAt<BitWide>(cell).Storage().Bytes();
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
void lyra_rt_bit_wide_report_bits(
    const void* designation, std::int64_t width, const void* start,
    const void* written, std::int64_t bits_width) {
  lyra::runtime::AssignDesignatedWideBits<BitWide>(
      designation, width, start, bits_width, written);
}

void lyra_rt_logic_wide_cell_construct(void* storage) {
  BuildAt<Var<LogicWide>>(storage);
}
void lyra_rt_logic_wide_cell_destroy(void* storage) {
  std::destroy_at(&CellAt<LogicWide>(storage));
}
auto lyra_rt_logic_wide_cell_get(void* cell) -> const void* {
  return CellAt<LogicWide>(cell).Get().Bytes();
}
void lyra_rt_logic_wide_cell_initialize(
    void* cell, const void* prototype, std::int64_t width) noexcept {
  CellAt<LogicWide>(cell).Initialize(
      lyra::runtime::WideOf<LogicWide>(width, prototype));
}
void lyra_rt_logic_wide_cell_set(void* cell, const void* value) {
  CellAt<LogicWide>(cell).SetBytes(value);
}
void lyra_rt_logic_wide_cell_arm_sampling(void* cell) {
  CellAt<LogicWide>(cell).ArmSampling();
}
auto lyra_rt_logic_wide_cell_sampled_load(void* cell, void* out) -> void* {
  return Emplace(out, CellAt<LogicWide>(cell).SampledGet());
}
auto lyra_rt_logic_wide_cell_begin_takeover(void* cell, std::int64_t level)
    -> std::int64_t {
  return CellAt<LogicWide>(cell).BeginTakeover(level);
}
auto lyra_rt_logic_wide_cell_drive_takeover(
    void* cell, std::int64_t level, std::int64_t generation, const void* value)
    -> bool {
  return CellAt<LogicWide>(cell).DriveTakeoverBytes(level, generation, value);
}
void lyra_rt_logic_wide_cell_end_takeover(void* cell, std::int64_t level) {
  CellAt<LogicWide>(cell).EndTakeover(level);
}
auto lyra_rt_logic_wide_cell_refer(void* cell, void* out) -> void* {
  return lyra::runtime::ReferToWideCell<LogicWide>(cell, out);
}
auto lyra_rt_logic_wide_cell_open_for_write(void* cell, void* out) -> void* {
  return OpenCellWrite<LogicWide>(cell, out);
}
auto lyra_rt_logic_wide_shared_cell_make(void* out) -> void* {
  return MakeSharedCell<LogicWide>(out);
}
auto lyra_rt_logic_wide_ref_get(void* reference) -> const void* {
  return ErasedAt(reference).storage;
}
void lyra_rt_logic_wide_ref_set(
    void* reference, std::int64_t width, const void* value) {
  lyra::runtime::WideRefSet<LogicWide>(reference, width, value);
}
void lyra_rt_logic_wide_ref_arm_sampling(void* reference) {
  lyra::runtime::WideRefArmSampling<LogicWide>(reference);
}
auto lyra_rt_logic_wide_ref_sampled_load(
    void* reference, std::int64_t width, void* out) -> void* {
  return lyra::runtime::WideRefSampledLoad<LogicWide>(reference, width, out);
}
auto lyra_rt_logic_wide_ref_open_for_write(
    void* reference, std::int64_t width, void* out) -> void* {
  return lyra::runtime::OpenWideRefWrite<LogicWide>(reference, width, out);
}
auto lyra_rt_logic_wide_value_cell_alloc(std::int64_t width) noexcept -> void* {
  return lyra::runtime::AllocateWideValueCell<LogicWide>(width);
}
void lyra_rt_logic_wide_value_cell_construct(
    void* storage, std::int64_t width) {
  lyra::runtime::BuildWideValueCell<LogicWide>(storage, width);
}
void lyra_rt_logic_wide_value_cell_destroy(void* storage) {
  std::destroy_at(&lyra::runtime::ValueCellAt<LogicWide>(storage));
}
void lyra_rt_logic_wide_value_cell_store(
    void* cell, const void* value) noexcept {
  lyra::runtime::ValueCellAt<LogicWide>(cell).Storage().TakeBytes(value);
}
auto lyra_rt_logic_wide_value_cell_load(void* cell) noexcept -> void* {
  return lyra::runtime::ValueCellAt<LogicWide>(cell).Storage().Bytes();
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
void lyra_rt_logic_wide_report_bits(
    const void* designation, std::int64_t width, const void* start,
    const void* written, std::int64_t bits_width) {
  lyra::runtime::AssignDesignatedWideBits<LogicWide>(
      designation, width, start, bits_width, written);
}
void lyra_rt_logic_wide_net_construct(void* storage) {
  BuildAt<ResolvedNet<LogicWide>>(storage);
}
void lyra_rt_logic_wide_net_destroy(void* storage) {
  std::destroy_at(&NetOf<LogicWide>(storage));
}
auto lyra_rt_logic_wide_net_get(void* net) -> const void* {
  return NetOf<LogicWide>(net).Get().Bytes();
}
void lyra_rt_logic_wide_net_initialize_tri_state(
    void* net, std::int64_t count, std::int64_t fill, std::int64_t strength) {
  NetOf<LogicWide>(net).InitializeTriState(count, fill, strength);
}
void lyra_rt_logic_wide_net_initialize_wired_and(
    void* net, std::int64_t count, std::int64_t fill, std::int64_t strength) {
  NetOf<LogicWide>(net).InitializeWiredAnd(count, fill, strength);
}
void lyra_rt_logic_wide_net_initialize_wired_or(
    void* net, std::int64_t count, std::int64_t fill, std::int64_t strength) {
  NetOf<LogicWide>(net).InitializeWiredOr(count, fill, strength);
}
void lyra_rt_logic_wide_net_initialize_retaining(
    void* net, std::int64_t count, std::int64_t fill, std::int64_t strength) {
  NetOf<LogicWide>(net).InitializeRetaining(count, fill, strength);
}
auto lyra_rt_logic_wide_net_begin_takeover(void* net, std::int64_t level)
    -> std::int64_t {
  return NetOf<LogicWide>(net).BeginTakeover(level);
}
auto lyra_rt_logic_wide_net_drive_takeover(
    void* net, std::int64_t level, std::int64_t generation, const void* value)
    -> bool {
  return NetOf<LogicWide>(net).DriveTakeoverBytes(level, generation, value);
}
void lyra_rt_logic_wide_net_end_takeover(void* net, std::int64_t level) {
  NetOf<LogicWide>(net).EndTakeover(level);
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
  return DriverOf<LogicWide>(driver).Get().Bytes();
}
void lyra_rt_logic_wide_driver_set(void* driver, const void* value) {
  DriverOf<LogicWide>(driver).SetBytes(value);
}
auto lyra_rt_logic_wide_driver_open_for_write(void* driver, void* out)
    -> void* {
  return OpenDriverWrite<LogicWide>(driver, out);
}
}
