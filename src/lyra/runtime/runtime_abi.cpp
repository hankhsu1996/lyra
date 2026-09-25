#include "lyra/runtime/runtime_abi.hpp"

#include <algorithm>
#include <array>
#include <bit>
#include <coroutine>
#include <cstddef>
#include <cstdint>
#include <cxxabi.h>
#include <deque>
#include <exception>
#include <functional>
#include <memory>
#include <span>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
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
#include "lyra/runtime/evaluation_attempts.hpp"
#include "lyra/runtime/file_table.hpp"
#include "lyra/runtime/finish.hpp"
#include "lyra/runtime/fork.hpp"
#include "lyra/runtime/generated_call_scope.hpp"
#include "lyra/runtime/hierarchy_segment.hpp"
#include "lyra/runtime/host_command.hpp"
#include "lyra/runtime/managed_object.hpp"
#include "lyra/runtime/named_event.hpp"
#include "lyra/runtime/nba_region.hpp"
#include "lyra/runtime/object_ref.hpp"
#include "lyra/runtime/plusargs.hpp"
#include "lyra/runtime/process_control.hpp"
#include "lyra/runtime/program_declarations.hpp"
#include "lyra/runtime/promoted_scope.hpp"
#include "lyra/runtime/random.hpp"
#include "lyra/runtime/runtime.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/sampled_history.hpp"
#include "lyra/runtime/scope.hpp"
#include "lyra/runtime/scope_program.hpp"
#include "lyra/runtime/sim_time.hpp"
#include "lyra/runtime/simulation_entry.hpp"
#include "lyra/runtime/storage_block.hpp"
#include "lyra/runtime/var.hpp"
#include "lyra/support/runtime_object.hpp"
#include "lyra/value/chandle.hpp"
#include "lyra/value/dpi_canonical.hpp"
#include "lyra/value/dpi_open_array.hpp"
#include "lyra/value/empty.hpp"
#include "lyra/value/enumeration.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/managed_ref.hpp"
#include "lyra/value/packed_array.hpp"
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
#include "lyra/value/runtime_value.hpp"
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

// The environment a callable value binds, held for as long as the body it was
// bound for can read it. Both alternatives are that one concept: a process, a
// spawned branch and an enabled task are each a body run by a site that does
// not stay to supply the environment per invocation.
//
// Which alternative one takes is how long the environment outlives the body,
// never what construct it came from. A body reaching its members through a
// receiver borrows one that outlives every activation reading it, so the frame
// carries it and there is nothing to hold here; a branch reads captures copied
// where the `fork` ran, which outlive nothing on their own, so they are owned
// here and die with this frame.
class GeneratedEnvironment {
 public:
  static auto Borrowing(void* frame) -> GeneratedEnvironment {
    return GeneratedEnvironment{Borrowed{.frame = frame}};
  }
  static auto Owning(ClosureValue closure) -> GeneratedEnvironment {
    return GeneratedEnvironment{std::move(closure)};
  }

  // The body's own frame, built and not yet begun, handed over once. Where the
  // captures are held here, building the frame is what binds the body to their
  // address, so it cannot happen until this environment is where it will stay.
  auto TakeFrame() -> void* {
    return std::visit(
        Overloaded{
            [](Borrowed& b) { return b.frame; },
            [](ClosureValue& c) { return c.Start(); }},
        held_);
  }

 private:
  struct Borrowed {
    void* frame = nullptr;
  };
  using Held = std::variant<Borrowed, ClosureValue>;

  explicit GeneratedEnvironment(Held held) : held_(std::move(held)) {
  }

  Held held_;
};

// Reaches the running coroutine's own record without suspending, which is how a
// body names storage belonging to the execution rather than to itself.
struct RunningExecution {
  PromiseBase* promise = nullptr;
  // A coroutine awaiter hook is an instance customization point by contract, so
  // it stays a member even where the implementation reads no awaiter state.
  // NOLINTNEXTLINE(readability-convert-member-functions-to-static)
  [[nodiscard]] auto await_ready() const noexcept -> bool {
    return false;
  }
  template <class P>
  auto await_suspend(std::coroutine_handle<P> self) noexcept -> bool {
    promise = &self.promise();
    return false;
  }
  [[nodiscard]] auto await_resume() const noexcept -> PromiseBase* {
    return promise;
  }
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
// hold itself -- that body's own coroutine, the environment it was entered on,
// and this execution's value store -- so all are released together, on every
// path this leaves.
auto RunGeneratedProcess(GeneratedEnvironment environment) -> Coroutine<void> {
  PromiseBase& execution = *(co_await RunningExecution{});
  execution.activation_values = std::make_unique<ActivationValueStore>();
  ActivationValueStore& values = *execution.activation_values;

  // The ramp lays out the body's frame and stops before its first statement, so
  // no generated code has run yet and nothing has reached for the store.
  GeneratedCoroutine generated{environment.TakeFrame()};

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
auto StartGeneratedProcess(void* out, GeneratedEnvironment environment)
    -> void* {
  return std::construct_at(
      static_cast<Coroutine<void>*>(out),
      RunGeneratedProcess(std::move(environment)));
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

// A value crossing the boundary is an opaque handle to a runtime object in
// storage the generated body gave it. These name the two directions of that
// correspondence so the entry points below read as the operation they perform,
// not as a wall of casts: reading what a handle names, and building what an
// entry answers with in the storage the call handed it, answering with that
// storage. The body that gave the storage is the one that ends what is built
// there, at the end of the evaluation that asked for it.
template <typename T>
auto Read(const void* handle) -> const T& {
  return *static_cast<const T*>(handle);
}

template <typename T>
auto Emplace(void* out, T value) -> void* {
  return std::construct_at(static_cast<T*>(out), std::move(value));
}

// The storage a caller lent, and which of the two forms it is. A body holding
// a reference is lowered once for every caller and so cannot ask what it was
// handed (LRM 13.5.2), while the two forms answer a write differently: one is
// a subscribable variable, where a write wakes whoever waited on it, and one
// is storage nothing subscribes to, where it does not. So the form travels in
// the address, in the low bit the alignment of every referenceable storage
// leaves free.
class LentStorage {
 public:
  [[nodiscard]] static auto OverCell(void* cell) -> void* {
    return std::bit_cast<void*>(
        std::bit_cast<std::uintptr_t>(cell) | kSubscribable);
  }

  [[nodiscard]] static auto OverValue(void* storage) -> void* {
    return storage;
  }

  explicit LentStorage(void* reference)
      : bits_(std::bit_cast<std::uintptr_t>(reference)) {
  }

  [[nodiscard]] auto Subscribable() const -> bool {
    return (bits_ & kSubscribable) != 0;
  }

  template <value::LyraValue T>
  [[nodiscard]] auto Cell() const -> Var<T>* {
    return std::bit_cast<Var<T>*>(bits_ & ~kSubscribable);
  }

  template <value::LyraValue T>
  [[nodiscard]] auto Value() const -> ActivationValueCell<T>* {
    return std::bit_cast<ActivationValueCell<T>*>(bits_);
  }

 private:
  static constexpr std::uintptr_t kSubscribable = 1;

  std::uintptr_t bits_;
};

// Reading and writing storage a caller lent. Each asks the form the reference
// carries which storage answers, and the answer is that storage's own access:
// a subscribable variable's write raises its update event, and storage nothing
// subscribes to is written directly.
template <value::LyraValue T>
auto RefGet(void* reference, void* out) -> void* {
  const LentStorage lent{reference};
  return lent.Subscribable() ? Emplace(out, lent.Cell<T>()->Get())
                             : Emplace(out, lent.Value<T>()->Get());
}

template <value::LyraValue T>
void RefSet(void* reference, const void* value) {
  const LentStorage lent{reference};
  if (lent.Subscribable()) {
    lent.Cell<T>()->Set(Read<T>(value));
  } else {
    lent.Value<T>()->Store(Read<T>(value));
  }
}

// What a time slot moved away from is kept only where something retains it, so
// arming storage nothing subscribes to asks for nothing.
template <value::LyraValue T>
void RefArmSampling(void* reference) {
  const LentStorage lent{reference};
  if (lent.Subscribable()) {
    lent.Cell<T>()->ArmSampling();
  }
}

// LRM 16.5.1 gives a variable its value in the Preponed region, excepting an
// automatic variable, whose sampled value is the value it holds. Storage
// nothing subscribes to covers both -- a caller may lend an automatic variable
// or a class property -- and what the reference carries is which of the two
// forms the storage is, never which kind of variable, so neither answer can be
// given without risking the other's. Refused rather than guessed.
template <value::LyraValue T>
auto RefSampledLoad(void* reference, void* out) -> void* {
  const LentStorage lent{reference};
  if (!lent.Subscribable()) {
    throw lyra::SimulationError(
        "a sampled value of storage lent by reference is only available where "
        "that storage is an observable cell");
  }
  return Emplace(out, lent.Cell<T>()->SampledGet());
}

// The obligation the encoding above places on storage, claimed here rather
// than assumed: storage a reference can name is built at an alignment that
// leaves the low bit free. Every representation is reached through one of
// these two families, so checking them checks the family.
static_assert(alignof(Var<value::PackedArray>) > 1);
static_assert(alignof(ActivationValueCell<value::PackedArray>) > 1);

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

// The process a `process` handle names (LRM 9.7), recovered from the erased
// share the managed-reference domain carries. Taking a typed owner is what
// keeps the node alive for the length of the call: the entry is the one place
// that knows which object the share is of, which is what erasing it costs and
// all it costs.
auto ProcessOf(const void* handle) -> value::ObjectRef {
  return RefToObject(
      std::static_pointer_cast<RuntimeProcess>(
          Read<value::ManagedRef>(handle).Share()));
}

// Takes over the erased value a boxed handle carries. A value crosses this way
// when it is what states a representation, so nothing on this side could have
// read that representation off anything else. The handle names a box the
// caller built for this call and ends after it, so its contents move rather
// than copy.
auto ErasedValue(void* handle) -> value::RuntimeValue {
  return std::move(*static_cast<value::RuntimeValue*>(handle));
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

// Copies one element out across the opaque-handle boundary as a value of the
// element's own domain, in the storage the call handed over.
auto ElementInto(void* out, const value::RuntimeValue& element) -> void* {
  return std::visit(
      [&](const auto& value) -> void* { return Emplace(out, value); },
      element.value);
}

// Erases an incoming element handle into the domain the container's element
// default names, which is where a container reads the target domain from.
auto ElementFrom(const value::RuntimeValue& element_default, void* value)
    -> value::RuntimeValue {
  return std::visit(
      [&](const auto& prototype) -> value::RuntimeValue {
        using T = std::decay_t<decltype(prototype)>;
        return value::RuntimeValue{Read<T>(value)};
      },
      element_default.value);
}

// Stores each of a literal's entries under the index it names. An entry is the
// product of the two, and a product's components are already erased, so nothing
// here converts either one (LRM 7.9.11).
auto SeedAssociativeEntries(
    const value::RuntimeAssociativeArray& array, LyraSpan entries)
    -> value::RuntimeAssociativeArray {
  const std::span<value::RuntimeTuple* const> handles(
      static_cast<value::RuntimeTuple* const*>(entries.data), entries.count);
  std::vector<value::RuntimeAssociativeEntry> seeded;
  seeded.reserve(handles.size());
  for (const value::RuntimeTuple* entry : handles) {
    seeded.push_back(
        value::RuntimeAssociativeEntry{
            .index = entry->Component(0), .element = entry->Component(1)});
  }
  return array.WithEntries(std::move(seeded));
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

// A run of values of one kind, each crossing as the opaque handle every value
// crosses as. What the run points at is the whole of what the two sides must
// agree on, the signature saying only that a run crosses, so it is read in one
// place whatever kind of value the run holds.
template <typename T>
auto ValuesOf(LyraSpan values) -> std::vector<T> {
  const std::span<const void* const> raw(
      static_cast<const void* const*>(values.data), values.count);
  std::vector<T> resolved;
  resolved.reserve(raw.size());
  for (const void* value : raw) {
    resolved.push_back(Read<T>(value));
  }
  return resolved;
}

// A submitted closure runs after the body that built it has returned, so the
// region takes the value out of that body's frame rather than borrowing it.
// The region holds what it takes by a shared handle because a region queue
// holds copyable callables, while a closure value owns its captures and is
// therefore only movable.
auto TakeClosure(void* closure) -> std::function<void()> {
  return [held = std::make_shared<ClosureValue>(std::move(
              *static_cast<ClosureValue*>(closure)))] { held->Invoke(); };
}

// A closure an observation keeps and runs each time it is asked -- what the
// watched expression is worth now, or whether an `iff` qualifier holds. The
// observation outlives the body that built the closure, so it takes it, and
// holds it by a shared handle because an observation is copied to each leaf of
// the expression while a closure value owns its captures and is only movable.
auto TakeEvaluator(void* closure) {
  return [held = std::make_shared<ClosureValue>(
              std::move(*static_cast<ClosureValue*>(closure)))] {
    return held->RunValue();
  };
}

// The body an LRM 7.12 method runs, as the value layer takes it. The closure is
// borrowed rather than taken: the method runs it to completion before
// returning, so the frame that built it is still alive for the whole walk.
auto ArrayBody(void* body) -> value::ArrayMethodBody {
  return [closure = static_cast<ClosureValue*>(body)](
             const value::RuntimeValue& item,
             const value::RuntimeValue& index) -> value::RuntimeValue {
    return closure->RunPerElement(item, index);
  };
}

// A call that answers with more than one value completes with the product of
// them, which crosses the boundary type-erased like every other product. Stated
// once here, so each entry below says only what its own components are.
auto EmplaceCompletion(void* out, std::vector<value::RuntimeValue> components)
    -> void* {
  return Emplace(out, value::RuntimeTuple(std::move(components)));
}

// Where one conversion parses to. A scan destination is an integral or a
// string (LRM 21.3.4.3) and lowering rejects anything else, so a value of any
// other domain reaching here is a compiler bug.
auto ScanTargetOf(value::RuntimeValue& value) -> value::ScanTarget {
  if (auto* packed = std::get_if<value::PackedArray>(&value.value)) {
    return value::ScanTarget{packed};
  }
  if (auto* text = std::get_if<value::String>(&value.value)) {
    return value::ScanTarget{text};
  }
  throw InternalError(
      "a scan parses into an integral or a string (LRM 21.3.4.3)");
}

// The matched-conversion count, how far the parse advanced, and one value per
// conversion (LRM 21.3.4.3). Each value starts as the prototype the call
// supplied and is parsed in place, so a conversion that never ran carries its
// prototype back and the caller's own destination stays as it was.
auto EmplaceScan(
    void* out, const value::String& input, const value::String& format,
    value::detail::NullByte null_byte, const value::RuntimeTuple& prototypes)
    -> void* {
  const std::size_t arity = prototypes.RawSize();
  std::vector<value::RuntimeValue> parsed;
  parsed.reserve(arity);
  for (std::size_t i = 0; i < arity; ++i) {
    parsed.push_back(prototypes.Component(i));
  }
  std::vector<value::ScanTarget> targets;
  targets.reserve(arity);
  for (value::RuntimeValue& value : parsed) {
    targets.push_back(ScanTargetOf(value));
  }

  value::PackedArray consumed = value::PackedArray::Int(0);
  value::PackedArray matched =
      value::detail::ScanImpl(input, format, null_byte, consumed, targets);

  std::vector<value::RuntimeValue> components;
  components.reserve(arity + 2);
  components.push_back(value::RuntimeValue{std::move(matched)});
  components.push_back(value::RuntimeValue{std::move(consumed)});
  for (value::RuntimeValue& value : parsed) {
    components.push_back(std::move(value));
  }
  return EmplaceCompletion(out, std::move(components));
}

// The queue left once the element goes, and the element itself (LRM 7.10.2.4 /
// 7.10.2.5).
auto EmplacePopped(
    void* out, value::RuntimeQueue remaining,
    const value::RuntimeValue& element) -> void* {
  return EmplaceCompletion(
      out, std::vector<value::RuntimeValue>{
               value::RuntimeValue{std::move(remaining)}, element});
}

// The SV int a traversal answers with and the index it visited (LRM 7.9.4 --
// 7.9.7), the visited index being the probe itself where the array holds no
// such neighbour.
auto EmplaceVisited(
    void* out, std::optional<value::RuntimeValue> index, const void* probe)
    -> void* {
  const bool found = index.has_value();
  return EmplaceCompletion(
      out, std::vector<value::RuntimeValue>{
               value::RuntimeValue{value::PackedArray::Int(found ? 1 : 0)},
               found ? *std::move(index) : Read<value::RuntimeValue>(probe)});
}

// A completion the runtime already assembled as a pair, boxed into the erased
// representation component by component. Which two values they are is the
// entry's own business -- the value drawn and the seed it advanced (LRM
// 20.14.2), a byte count and the text or memory those bytes filled (LRM
// 21.3.4.2, 21.3.4.4, 21.3.7).
template <typename First, typename Second>
auto EmplaceBoth(void* out, const value::Tuple<First, Second>& completion)
    -> void* {
  return EmplaceCompletion(
      out, std::vector<value::RuntimeValue>{
               value::RuntimeValue{completion.template Get<0>()},
               value::RuntimeValue{completion.template Get<1>()}});
}

// An event control's leaves cross as a span of pointers to values this call
// does not own, so what the wait is built from is gathered here. What the wait
// itself is, is decided by the one function both backends call.
auto TriggersOf(LyraSpan triggers) -> std::vector<Trigger> {
  const std::span<Trigger* const> handles(
      static_cast<Trigger* const*>(triggers.data), triggers.count);
  std::vector<Trigger> collected;
  collected.reserve(triggers.count);
  for (const Trigger* handle : handles) {
    collected.push_back(*handle);
  }
  return collected;
}

}  // namespace

}  // namespace lyra::runtime

using lyra::runtime::AbiStringRef;
using lyra::runtime::ActivationValueCell;
using lyra::runtime::BehaviorAt;
using lyra::runtime::BehaviorCoordinate;
using lyra::runtime::CancellationTarget;
using lyra::runtime::ChannelCancellation;
using lyra::runtime::ClassValue;
using lyra::runtime::ClosureDefinition;
using lyra::runtime::ClosureValue;
using lyra::runtime::ControlEffect;
using lyra::runtime::Coroutine;
using lyra::runtime::CoroutineHandle;
using lyra::runtime::current_runtime;
using lyra::runtime::CurrentExportScope;
using lyra::runtime::CurrentForeignProcess;
using lyra::runtime::DeclareBase;
using lyra::runtime::DeclareBehaviorName;
using lyra::runtime::DeclareBodyName;
using lyra::runtime::DeclareClass;
using lyra::runtime::DeclareClassName;
using lyra::runtime::DeclareClosure;
using lyra::runtime::DeclareExportName;
using lyra::runtime::DeclareIntroduction;
using lyra::runtime::DeclareMembers;
using lyra::runtime::DeclarePropertyName;
using lyra::runtime::DeclareScopeClass;
using lyra::runtime::DeclareScopeProgram;
using lyra::runtime::DeclareSharedStorage;
using lyra::runtime::DeclareSubroutineName;
using lyra::runtime::DeclareTakeover;
using lyra::runtime::DeclareVariableSchema;
using lyra::runtime::Delay;
using lyra::runtime::DelayReal;
using lyra::runtime::DiagnosticDispatcher;
using lyra::runtime::DriveOnForeignStack;
using lyra::runtime::DriverOf;
using lyra::runtime::Emplace;
using lyra::runtime::EnterCancellationTarget;
using lyra::runtime::EnterForeignTask;
using lyra::runtime::EvaluationAttempts;
using lyra::runtime::FileTable;
using lyra::runtime::FindBehavior;
using lyra::runtime::FindExportEntry;
using lyra::runtime::FindProperty;
using lyra::runtime::ForkWaitAll;
using lyra::runtime::ForkWaitFirst;
using lyra::runtime::GcObject;
using lyra::runtime::GeneratedCallScope;
using lyra::runtime::HierarchySegment;
using lyra::runtime::LeaveCancellationTarget;
using lyra::runtime::LentStorage;
using lyra::runtime::MakeForeignExecution;
using lyra::runtime::MakeManagedObject;
using lyra::runtime::MemberStorageSchema;
using lyra::runtime::NamedEvent;
using lyra::runtime::NetOf;
using lyra::runtime::ObjectDefinition;
using lyra::runtime::ObjectIsOfClass;
using lyra::runtime::ObjectOf;
using lyra::runtime::Observable;
using lyra::runtime::Observation;
using lyra::runtime::ProcessAwait;
using lyra::runtime::ProcessKill;
using lyra::runtime::ProcessOf;
using lyra::runtime::ProcessResume;
using lyra::runtime::ProcessSelf;
using lyra::runtime::ProcessStatus;
using lyra::runtime::ProcessSuspend;
using lyra::runtime::ProgramLifetime;
using lyra::runtime::PromotedScopeRef;
using lyra::runtime::PropertyAt;
using lyra::runtime::PropertyCoordinate;
using lyra::runtime::RaiseDeclinedDeparture;
using lyra::runtime::Read;
using lyra::runtime::RealTimeInUnit;
using lyra::runtime::ReceiveDeparture;
using lyra::runtime::RefArmSampling;
using lyra::runtime::RefGet;
using lyra::runtime::RefSampledLoad;
using lyra::runtime::RefSet;
using lyra::runtime::Region;
using lyra::runtime::ResumeInNbaRegion;
using lyra::runtime::RunDeclaredProgram;
using lyra::runtime::RunHostCommand;
using lyra::runtime::RunNullHostCommand;
using lyra::runtime::RuntimeEffects;
using lyra::runtime::RuntimeProcess;
using lyra::runtime::SampledHistory;
using lyra::runtime::Scope;
using lyra::runtime::ScopeDefinition;
using lyra::runtime::SimTimeInUnit;
using lyra::runtime::SpawnAll;
using lyra::runtime::STimeInUnit;
using lyra::runtime::StorageBlock;
using lyra::runtime::SubscribeToLeaves;
using lyra::runtime::TakeBranches;
using lyra::runtime::TakeClosure;
using lyra::runtime::TakeEvaluator;
using lyra::runtime::TestPlusargs;
using lyra::runtime::Trigger;
using lyra::runtime::TriggersOf;
using lyra::runtime::ValuesOf;
using lyra::runtime::Var;
using lyra::runtime::WaitAny;
using lyra::runtime::WaitFork;
using lyra::runtime::WaitUntil;
using lyra::value::AssociativeIndexOrder;
using lyra::value::Chandle;
using lyra::value::DpiBitBuffer;
using lyra::value::DpiLogicBuffer;
using lyra::value::DpiOpenArray;
using lyra::value::Enumeration;
using lyra::value::Format;
using lyra::value::FormatArg;
using lyra::value::FormatSpec;
using lyra::value::MakeFormatArg;
using lyra::value::ManagedRef;
using lyra::value::ObjectRef;
using lyra::value::PackedArray;
using lyra::value::PackedRange;
using lyra::value::PackedType;
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
using lyra::value::RuntimeValue;
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
    void* runtime, const void* units_power, const void* precision,
    const void* suffix, const void* min_width) {
  static_cast<RuntimeEffects*>(runtime)->SetTimeFormat(
      Read<PackedArray>(units_power), Read<PackedArray>(precision),
      Read<String>(suffix), Read<PackedArray>(min_width));
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

void lyra_rt_file_close(void* files, const void* descriptor) {
  static_cast<FileTable*>(files)->Close(Read<PackedArray>(descriptor));
}

auto lyra_rt_file_getc(void* files, const void* fd, void* out) -> void* {
  return Emplace(
      out, static_cast<FileTable*>(files)->Getc(Read<PackedArray>(fd)));
}

auto lyra_rt_file_gets(void* files, const void* fd, void* out) -> void* {
  return lyra::runtime::EmplaceBoth(
      out, static_cast<FileTable*>(files)->Gets(Read<PackedArray>(fd)));
}

auto lyra_rt_file_error(void* files, const void* fd, void* out) -> void* {
  return lyra::runtime::EmplaceBoth(
      out, static_cast<FileTable*>(files)->Error(Read<PackedArray>(fd)));
}

auto lyra_rt_file_read(void* files, const void* dest, const void* fd, void* out)
    -> void* {
  return lyra::runtime::EmplaceBoth(
      out, static_cast<FileTable*>(files)->Read(
               Read<PackedArray>(dest), Read<PackedArray>(fd)));
}

auto lyra_rt_file_read_memory(
    void* files, const void* dest, const void* fd, const void* declared,
    const void* start, const void* count, void* out) -> void* {
  const auto memory = Read<lyra::value::RuntimeUnpackedArray>(dest);
  const auto& range = Read<UnpackedRange>(declared);
  const std::array dims{range};
  const std::int64_t lowest = range.Low();
  std::vector<PackedArray> words = lyra::value::MemoryWords(memory, dims);
  const std::int32_t read = lyra::runtime::ReadMemoryWords(
      *static_cast<FileTable*>(files), Read<PackedArray>(fd),
      std::get<PackedArray>(memory.ElementDefault().value), range,
      Read<PackedArray>(start).ToInt64(), Read<PackedArray>(count).ToInt64(),
      [&words, lowest](std::int64_t sv, PackedArray word) {
        words[static_cast<std::size_t>(sv - lowest)] = std::move(word);
      });
  return lyra::runtime::EmplaceCompletion(
      out, std::vector<lyra::value::RuntimeValue>{
               lyra::value::RuntimeValue{PackedArray::Int(read)},
               lyra::value::RuntimeValue{
                   lyra::value::MemoryWithWords(memory, dims, words)}});
}

auto lyra_rt_file_ungetc(void* files, const void* c, const void* fd, void* out)
    -> void* {
  return Emplace(
      out, static_cast<FileTable*>(files)->Ungetc(
               Read<PackedArray>(c), Read<PackedArray>(fd)));
}

auto lyra_rt_file_seek(
    void* files, const void* fd, const void* offset, const void* operation,
    void* out) -> void* {
  return Emplace(
      out, static_cast<FileTable*>(files)->Seek(
               Read<PackedArray>(fd), Read<PackedArray>(offset),
               Read<PackedArray>(operation)));
}

auto lyra_rt_file_rewind(void* files, const void* fd, void* out) -> void* {
  return Emplace(
      out, static_cast<FileTable*>(files)->Rewind(Read<PackedArray>(fd)));
}

auto lyra_rt_file_tell(void* files, const void* fd, void* out) -> void* {
  return Emplace(
      out, static_cast<FileTable*>(files)->Tell(Read<PackedArray>(fd)));
}

auto lyra_rt_file_eof(void* files, const void* fd, void* out) -> void* {
  return Emplace(
      out, static_cast<FileTable*>(files)->Eof(Read<PackedArray>(fd)));
}

void lyra_rt_file_flush(void* files, const void* descriptor) {
  static_cast<FileTable*>(files)->Flush(Read<PackedArray>(descriptor));
}

void lyra_rt_file_flush_all(void* files) {
  static_cast<FileTable*>(files)->FlushAll();
}

auto lyra_rt_peek_buffered(void* files, const void* fd, void* out) -> void* {
  return Emplace(
      out, static_cast<FileTable*>(files)->PeekBuffered(Read<PackedArray>(fd)));
}

void lyra_rt_advance_fd(void* files, const void* fd, const void* count) {
  static_cast<FileTable*>(files)->AdvanceFd(
      Read<PackedArray>(fd), Read<PackedArray>(count));
}

auto lyra_rt_cancellation_for(void* files, const void* descriptor, void* out)
    -> void* {
  return Emplace(
      out, static_cast<FileTable*>(files)->CancellationFor(
               Read<PackedArray>(descriptor)));
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

auto lyra_rt_packed_from_words(
    LyraSpan value_words, LyraSpan unknown_words, const void* type, void* out)
    -> void* {
  return Emplace(
      out, PackedArray::FromWords(
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

auto lyra_rt_make_unpacked_range(std::int64_t left, std::int64_t right) -> const
    void* {
  return ProgramLifetime(UnpackedRange{.left = left, .right = right});
}

auto lyra_rt_make_packed_type(LyraSpan dims, bool is_signed, bool is_four_state)
    -> const void* {
  const std::span<const void* const> entries{
      static_cast<const void* const*>(dims.data), dims.count};
  PackedType::Dims ranges(entries.size());
  std::ranges::transform(entries, ranges.begin(), [](const void* entry) {
    return *static_cast<const PackedRange*>(entry);
  });
  return ProgramLifetime(PackedType{ranges, is_signed, is_four_state});
}

auto lyra_rt_make_enumeration(const void* base, LyraSpan planes, LyraSpan names)
    -> const void* {
  return ProgramLifetime(
      Enumeration{
          Read<PackedType>(base),
          std::span<const std::uint64_t>{
              static_cast<const std::uint64_t*>(planes.data), planes.count},
          std::span<const char* const>{
              static_cast<const char* const*>(names.data), names.count}});
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

void lyra_rt_record_coverage(void* runtime, const void* site, bool succeeded) {
  static_cast<RuntimeEffects*>(runtime)->RecordCoverage(
      Read<String>(site), succeeded);
}

auto lyra_rt_enter_coroutine_borrowed_environment(
    void* frame, void* out) noexcept -> void* {
  return lyra::runtime::StartGeneratedProcess(
      out, lyra::runtime::GeneratedEnvironment::Borrowing(frame));
}

auto lyra_rt_enter_coroutine_owned_environment(
    void* closure, void* out) noexcept -> void* {
  return lyra::runtime::StartGeneratedProcess(
      out, lyra::runtime::GeneratedEnvironment::Owning(
               std::move(*static_cast<ClosureValue*>(closure))));
}

auto lyra_rt_await_coroutine(void* runtime, void* activation) -> bool {
  auto& svc = *static_cast<RuntimeEffects*>(runtime);
  lyra::runtime::RuntimeProcess& process = svc.CurrentProcess();
  const CoroutineHandle caller = process.CurrentLeaf();
  const CoroutineHandle called = process.PushActivation(
      std::move(*static_cast<Coroutine<void>*>(activation)));
  called->self.resume();
  // An activation that consumed no time is over before its caller could have
  // waited for it, and the caller is still on the stack below, so nothing
  // continues it and it must not park.
  if (called->self.done()) {
    return false;
  }
  called->continuation = caller->self;
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

auto lyra_rt_fork_wait_all(void* runtime, LyraSpan branches) -> bool {
  auto& svc = *static_cast<RuntimeEffects*>(runtime);
  std::vector<Coroutine<void>> taken = TakeBranches(branches);
  return ForkWaitAll(svc, std::span<Coroutine<void>>{taken});
}

auto lyra_rt_fork_wait_first(void* runtime, LyraSpan branches) -> bool {
  auto& svc = *static_cast<RuntimeEffects*>(runtime);
  std::vector<Coroutine<void>> taken = TakeBranches(branches);
  return ForkWaitFirst(svc, std::span<Coroutine<void>>{taken});
}

auto lyra_rt_wait_fork(void* runtime) -> bool {
  return WaitFork(*static_cast<RuntimeEffects*>(runtime));
}

void lyra_rt_disable_fork(void* runtime) {
  lyra::runtime::DisableFork(*static_cast<RuntimeEffects*>(runtime));
}

auto lyra_rt_process_self(void* runtime, void* out) -> void* {
  return Emplace(
      out, ProcessSelf(*static_cast<RuntimeEffects*>(runtime)).Handle());
}

auto lyra_rt_process_status(const void* self, void* out) -> void* {
  return Emplace(out, ProcessStatus(ProcessOf(self)));
}

void lyra_rt_process_kill(const void* self, void* runtime) {
  ProcessKill(ProcessOf(self), *static_cast<RuntimeEffects*>(runtime));
}

auto lyra_rt_process_await(const void* self, void* runtime) -> bool {
  return ProcessAwait(ProcessOf(self), *static_cast<RuntimeEffects*>(runtime));
}

void lyra_rt_process_suspend(const void* self, void* runtime) {
  ProcessSuspend(ProcessOf(self), *static_cast<RuntimeEffects*>(runtime));
}

void lyra_rt_process_resume(const void* self, void* runtime) {
  ProcessResume(ProcessOf(self), *static_cast<RuntimeEffects*>(runtime));
}

auto lyra_rt_closure_make(const void* definition, LyraSpan captures, void* out)
    -> void* {
  return std::construct_at(
      static_cast<ClosureValue*>(out),
      static_cast<const ClosureDefinition*>(definition),
      std::span<void* const>(
          static_cast<void* const*>(captures.data), captures.count));
}

auto lyra_rt_closure_capture(void* self, std::uint32_t index) -> void* {
  return static_cast<ClosureValue*>(self)->Capture(index);
}

auto lyra_rt_object_make(const void* definition, void* out) -> void* {
  ObjectRef object =
      MakeManagedObject(static_cast<const ObjectDefinition*>(definition));
  return Emplace(out, object.Handle());
}

auto lyra_rt_make_promoted_scope(const void* definition, void* out) -> void* {
  return std::construct_at(
      static_cast<PromotedScopeRef*>(out),
      static_cast<const ObjectDefinition*>(definition));
}

auto lyra_rt_promoted_scope_deref(void* handle) -> void* {
  return Read<PromotedScopeRef>(handle).Storage();
}

void lyra_rt_submit_nba(void* runtime, void* closure) {
  static_cast<RuntimeEffects*>(runtime)->SubmitNba(TakeClosure(closure));
}

void lyra_rt_submit_nba_after(
    void* runtime, const void* duration, const void* unit_power,
    const void* precision_power, void* closure) {
  static_cast<RuntimeEffects*>(runtime)->SubmitNbaAfter(
      Read<PackedArray>(duration), Read<PackedArray>(unit_power),
      Read<PackedArray>(precision_power), TakeClosure(closure));
}

void lyra_rt_submit_nba_after_real(
    void* runtime, const void* duration, const void* unit_power,
    const void* precision_power, void* closure) {
  static_cast<RuntimeEffects*>(runtime)->SubmitNbaAfterReal(
      Read<Real>(duration), Read<PackedArray>(unit_power),
      Read<PackedArray>(precision_power), TakeClosure(closure));
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
    void* runtime, const void* duration, const void* unit_power,
    const void* precision_power) -> bool {
  return Delay(
      *static_cast<RuntimeEffects*>(runtime), Read<PackedArray>(duration),
      Read<PackedArray>(unit_power), Read<PackedArray>(precision_power));
}

auto lyra_rt_delay_real(
    void* runtime, const void* duration, const void* unit_power,
    const void* precision_power) -> bool {
  return DelayReal(
      *static_cast<RuntimeEffects*>(runtime), Read<Real>(duration),
      Read<PackedArray>(unit_power), Read<PackedArray>(precision_power));
}

// What crosses is the cell's own address -- a variable, a net, a named event --
// and a `void*` carries no type to adjust by, so it is read here as the address
// of what waits on that cell. Every such cell names `Observable` as its first
// base, which is what makes the two addresses one under the platform ABI.
auto lyra_rt_make_trigger(
    void* observable, const void* observation, const void* lsb_bit_offset,
    const void* bit_width, void* out) -> void* {
  return Emplace(
      out,
      Trigger(
          static_cast<Observable*>(observable), Read<Observation>(observation),
          Read<PackedArray>(lsb_bit_offset), Read<PackedArray>(bit_width)));
}

auto lyra_rt_observation_on_reaching(void* out) -> void* {
  return Emplace(out, Observation::OnReaching());
}

auto lyra_rt_observation_of_value(void* expression, const void* edge, void* out)
    -> void* {
  return Emplace(
      out,
      Observation::OfValue(TakeEvaluator(expression), Read<PackedArray>(edge)));
}

auto lyra_rt_observation_of_value_qualified(
    void* expression, const void* edge, void* condition, void* out) -> void* {
  return Emplace(
      out, Observation::OfValueQualified(
               TakeEvaluator(expression), Read<PackedArray>(edge),
               TakeEvaluator(condition)));
}

auto lyra_rt_observation_qualified(void* condition, void* out) -> void* {
  return Emplace(out, Observation::Qualified(TakeEvaluator(condition)));
}

auto lyra_rt_wait_any(void* runtime, LyraSpan triggers) -> bool {
  return WaitAny(*static_cast<RuntimeEffects*>(runtime), TriggersOf(triggers));
}

auto lyra_rt_wait_until(void* runtime, LyraSpan triggers) -> bool {
  return WaitUntil(
      *static_cast<RuntimeEffects*>(runtime), TriggersOf(triggers));
}

auto lyra_rt_resume_in_nba_region(void* runtime) -> bool {
  return ResumeInNbaRegion(*static_cast<RuntimeEffects*>(runtime));
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

auto lyra_rt_retain_constant(const void* value) -> const void* {
  // A copy rather than a move: the body that built it goes on owning what it
  // built and ends it, and this is the run taking its own.
  return ProgramLifetime(Read<PackedArray>(value));
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
  return Emplace(out, PackedArray::Bit(effect == target));
}

void lyra_rt_take_departure_if_due(void* runtime) {
  // A foreign call made before any procedure starts runs with no process at all
  // (LRM 10.5, 26.2), and a point one returns through is not an execution that
  // could have been disabled or terminated, so nothing is owed there.
  TakeDepartureIfDue(*static_cast<RuntimeEffects*>(runtime));
}

auto lyra_rt_sim_time(void* runtime, const void* unit_power, void* out)
    -> void* {
  return Emplace(
      out, SimTimeInUnit(
               *static_cast<RuntimeEffects*>(runtime),
               Read<PackedArray>(unit_power)));
}

auto lyra_rt_stime(void* runtime, const void* unit_power, void* out) -> void* {
  return Emplace(
      out, STimeInUnit(
               *static_cast<RuntimeEffects*>(runtime),
               Read<PackedArray>(unit_power)));
}

auto lyra_rt_realtime(void* runtime, const void* unit_power, void* out)
    -> void* {
  return Emplace(
      out, RealTimeInUnit(
               *static_cast<RuntimeEffects*>(runtime),
               Read<PackedArray>(unit_power)));
}

void lyra_rt_finish(void* runtime, const void* origin, const void* level) {
  Finish(
      *static_cast<RuntimeEffects*>(runtime), Read<String>(origin),
      Read<PackedArray>(level));
}

void lyra_rt_stop(void* runtime, const void* origin, const void* level) {
  Stop(
      *static_cast<RuntimeEffects*>(runtime), Read<String>(origin),
      Read<PackedArray>(level));
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

auto lyra_rt_packed_value_plusargs(
    void* runtime, const void* user_string, const void* destination, void* out)
    -> void* {
  return lyra::runtime::EmplaceBoth(
      out, ValuePlusargs(
               *static_cast<RuntimeEffects*>(runtime),
               Read<String>(user_string), Read<PackedArray>(destination)));
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

auto lyra_rt_urandom_seeded(void* runtime, const void* seed, void* out)
    -> void* {
  return Emplace(
      out,
      lyra::runtime::UrandomSeeded(
          *static_cast<RuntimeEffects*>(runtime), Read<PackedArray>(seed)));
}

auto lyra_rt_urandom_range(
    void* runtime, const void* maxval, const void* minval, void* out) -> void* {
  return Emplace(
      out, lyra::runtime::UrandomRange(
               *static_cast<RuntimeEffects*>(runtime),
               Read<PackedArray>(maxval), Read<PackedArray>(minval)));
}

auto lyra_rt_random(void* runtime, void* out) -> void* {
  return Emplace(
      out, lyra::runtime::Random(*static_cast<RuntimeEffects*>(runtime)));
}

auto lyra_rt_dist_uniform(
    const void* seed, const void* start, const void* end, void* out) -> void* {
  return lyra::runtime::EmplaceBoth(
      out, lyra::runtime::DistUniform(
               Read<PackedArray>(seed), Read<PackedArray>(start),
               Read<PackedArray>(end)));
}

auto lyra_rt_dist_normal(
    const void* seed, const void* mean, const void* standard_deviation,
    void* out) -> void* {
  return lyra::runtime::EmplaceBoth(
      out, lyra::runtime::DistNormal(
               Read<PackedArray>(seed), Read<PackedArray>(mean),
               Read<PackedArray>(standard_deviation)));
}

auto lyra_rt_dist_exponential(const void* seed, const void* mean, void* out)
    -> void* {
  return lyra::runtime::EmplaceBoth(
      out, lyra::runtime::DistExponential(
               Read<PackedArray>(seed), Read<PackedArray>(mean)));
}

auto lyra_rt_dist_poisson(const void* seed, const void* mean, void* out)
    -> void* {
  return lyra::runtime::EmplaceBoth(
      out, lyra::runtime::DistPoisson(
               Read<PackedArray>(seed), Read<PackedArray>(mean)));
}

auto lyra_rt_dist_chi_square(
    const void* seed, const void* degrees_of_freedom, void* out) -> void* {
  return lyra::runtime::EmplaceBoth(
      out, lyra::runtime::DistChiSquare(
               Read<PackedArray>(seed), Read<PackedArray>(degrees_of_freedom)));
}

auto lyra_rt_dist_t(const void* seed, const void* degrees_of_freedom, void* out)
    -> void* {
  return lyra::runtime::EmplaceBoth(
      out, lyra::runtime::DistT(
               Read<PackedArray>(seed), Read<PackedArray>(degrees_of_freedom)));
}

auto lyra_rt_dist_erlang(
    const void* seed, const void* stages, const void* mean, void* out)
    -> void* {
  return lyra::runtime::EmplaceBoth(
      out, lyra::runtime::DistErlang(
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
    -> void (*)() {
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
  // A wait this thread registers parks its innermost activation, and the body
  // reached here is one: it runs in the thread that entered the foreign call
  // (LRM 9.5), so entering it is what makes a delay inside it park the right
  // frame rather than the one that called out.
  RuntimeProcess& process = CurrentForeignProcess();
  const CoroutineHandle called = process.PushActivation(
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
      ValuesOf<PackedArray>(indices));
}

auto lyra_rt_make_scope(
    const void* definition, void* parent, void* segment, LyraSpan arguments)
    -> void* {
  const auto* def = static_cast<const ScopeDefinition*>(definition);
  auto* identity = static_cast<HierarchySegment*>(segment);
  std::unique_ptr<Scope> instance(
      ClassValue::Make<Scope>(
          def, static_cast<Scope*>(parent), *identity, def));
  def->construct(
      instance.get(), static_cast<Scope*>(parent), identity,
      lyra::runtime::ScopeConstructArguments{
          .data = static_cast<void* const*>(arguments.data),
          .size = arguments.count});
  return instance.release();
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

auto lyra_rt_resolve_visible_child(
    void* self, const void* head_name, LyraSpan head_indices) -> void* {
  return static_cast<Scope*>(self)->ResolveVisibleChild(
      static_cast<const char*>(head_name), ValuesOf<PackedArray>(head_indices));
}

auto lyra_rt_find_child(void* self, const void* name, LyraSpan indices)
    -> void* {
  return static_cast<Scope*>(self)->FindChild(
      static_cast<const char*>(name), ValuesOf<PackedArray>(indices));
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

auto lyra_rt_object_deref(void* handle) -> void* {
  const auto& object = Read<ManagedRef>(handle);
  if (!static_cast<bool>(object)) {
    lyra::value::RaiseNullObjectHandleAccess();
  }
  return object.Share().get();
}

auto lyra_rt_method(
    void* value, const void* introduced_by, std::uint32_t ordinal)
    -> LyraMethodEntry {
  return static_cast<const ClassValue*>(value)->Method(
      static_cast<const ObjectDefinition*>(introduced_by), ordinal);
}

auto lyra_rt_class_find_property(const void* definition, const void* name)
    -> const void* {
  return FindProperty(
      static_cast<const ObjectDefinition*>(definition),
      static_cast<const char*>(name));
}

auto lyra_rt_class_find_behavior(const void* definition, const void* name)
    -> const void* {
  return FindBehavior(
      static_cast<const ObjectDefinition*>(definition),
      static_cast<const char*>(name));
}

auto lyra_rt_class_find_behavior_body(const void* definition, const void* name)
    -> LyraMethodEntry {
  return FindBehaviorBody(
      static_cast<const ObjectDefinition*>(definition),
      static_cast<const char*>(name));
}

auto lyra_rt_object_of(const void* handle) -> void* {
  return ObjectOf(Read<ManagedRef>(handle));
}

auto lyra_rt_object_is_of_class(const void* handle, const void* definition)
    -> std::int64_t {
  return ObjectIsOfClass(
      Read<ManagedRef>(handle),
      static_cast<const ObjectDefinition*>(definition));
}

auto lyra_rt_enumeration_has(const void* enumeration, const void* value)
    -> std::int64_t {
  return Read<Enumeration>(enumeration).Has(Read<PackedArray>(value)) ? 1 : 0;
}

auto lyra_rt_enumeration_name(
    const void* enumeration, const void* value, void* out) -> void* {
  return Emplace(
      out, Read<Enumeration>(enumeration).Name(Read<PackedArray>(value)));
}

auto lyra_rt_enumeration_next(
    const void* enumeration, const void* value, const void* count, void* out)
    -> void* {
  return Emplace(
      out, Read<Enumeration>(enumeration)
               .Next(Read<PackedArray>(value), Read<PackedArray>(count)));
}

auto lyra_rt_enumeration_prev(
    const void* enumeration, const void* value, const void* count, void* out)
    -> void* {
  return Emplace(
      out, Read<Enumeration>(enumeration)
               .Prev(Read<PackedArray>(value), Read<PackedArray>(count)));
}

auto lyra_rt_property_at(const void* handle, const void* coordinate) -> void* {
  return PropertyAt(
      Read<ManagedRef>(handle),
      static_cast<const PropertyCoordinate*>(coordinate));
}

auto lyra_rt_behavior_at(const void* handle, const void* coordinate)
    -> LyraMethodEntry {
  return BehaviorAt(
      Read<ManagedRef>(handle),
      static_cast<const BehaviorCoordinate*>(coordinate));
}

void lyra_rt_register_signal(void* self, const void* name, void* cell) {
  static_cast<Scope*>(self)->RegisterSignal(
      static_cast<const char*>(name), cell);
}

auto lyra_rt_find_signal(void* self, const void* name) -> void* {
  return static_cast<Scope*>(self)->FindSignal(static_cast<const char*>(name));
}

auto lyra_rt_find_class(void* self, const void* name) -> const void* {
  return static_cast<Scope*>(self)->FindClass(static_cast<const char*>(name));
}

auto lyra_rt_find_subroutine(void* self, const void* name) -> void (*)() {
  return static_cast<Scope*>(self)->FindSubroutine(
      static_cast<const char*>(name));
}

void lyra_rt_register_disable_target(void* self, void* target) {
  static_cast<Scope*>(self)->RegisterDisableTarget(
      static_cast<CancellationTarget*>(target));
}

auto lyra_rt_find_disable_target(void* self) -> void* {
  return static_cast<Scope*>(self)->FindDisableTarget();
}

auto lyra_rt_variables_open(const void* schema) noexcept -> void* {
  return std::make_unique<StorageBlock>(
             *static_cast<const MemberStorageSchema*>(schema))
      .release();
}

auto lyra_rt_variable_addr(void* variables, std::uint32_t index) noexcept
    -> void* {
  return static_cast<StorageBlock*>(variables)->Address(index);
}

void lyra_rt_variables_close(void* variables) noexcept {
  // Taking the storage back into an owner is what ends it, and with it every
  // variable in it.
  const std::unique_ptr<StorageBlock> ending(
      static_cast<StorageBlock*>(variables));
}

auto lyra_rt_variable_schema_declare(const void* described, std::uint64_t count)
    -> const void* {
  return DeclareVariableSchema(
      {static_cast<const lyra::support::DeclaredMemberStorage*>(described),
       count});
}

auto lyra_rt_shared_storage_declare(std::uint8_t kind, std::uint8_t domain)
    -> void* {
  return DeclareSharedStorage(
      lyra::support::DeclaredMemberStorage{
          .kind = static_cast<lyra::support::MemberStorageKind>(kind),
          .domain = static_cast<lyra::support::ValueDomain>(domain)});
}

auto lyra_rt_closure_declare_synchronous(
    const void* captures, std::uint64_t count, void (*body)(void* self))
    -> const void* {
  return DeclareClosure(
      {static_cast<const lyra::support::DeclaredMemberStorage*>(captures),
       count},
      lyra::runtime::SynchronousBody{.run = body});
}

auto lyra_rt_closure_declare_coroutine(
    const void* captures, std::uint64_t count, void* (*body)(void* self))
    -> const void* {
  return DeclareClosure(
      {static_cast<const lyra::support::DeclaredMemberStorage*>(captures),
       count},
      lyra::runtime::CoroutineBody{.start = body});
}

auto lyra_rt_closure_declare_per_element(
    const void* captures, std::uint64_t count,
    void* (*body)(void* self, const void* item, const void* index, void* out),
    std::uint8_t result_domain) -> const void* {
  return DeclareClosure(
      {static_cast<const lyra::support::DeclaredMemberStorage*>(captures),
       count},
      lyra::runtime::PerElementBody{
          .run = body,
          .result_domain =
              static_cast<lyra::support::ValueDomain>(result_domain)});
}

auto lyra_rt_closure_declare_value(
    const void* captures, std::uint64_t count,
    void* (*body)(void* self, void* out), std::uint8_t result_domain) -> const
    void* {
  return DeclareClosure(
      {static_cast<const lyra::support::DeclaredMemberStorage*>(captures),
       count},
      lyra::runtime::ValueBody{
          .run = body,
          .result_domain =
              static_cast<lyra::support::ValueDomain>(result_domain)});
}

auto lyra_rt_class_declare() -> void* {
  return DeclareClass();
}

auto lyra_rt_scope_class_declare(
    std::int8_t time_unit_power, std::int8_t time_precision_power) -> void* {
  return DeclareScopeClass(time_unit_power, time_precision_power);
}

void lyra_rt_class_declare_base(void* cls, const void* base) {
  DeclareBase(
      static_cast<ObjectDefinition*>(cls),
      static_cast<const ObjectDefinition* const*>(base));
}

void lyra_rt_class_declare_members(
    void* cls, const void* described, std::uint64_t count) {
  DeclareMembers(
      static_cast<ObjectDefinition*>(cls),
      {static_cast<const lyra::support::DeclaredMemberStorage*>(described),
       count});
}

void lyra_rt_class_declare_introduction(void* cls, LyraMethodEntry body) {
  DeclareIntroduction(static_cast<ObjectDefinition*>(cls), body);
}

void lyra_rt_class_declare_takeover(
    void* cls, const void* introduced_by, std::uint32_t ordinal,
    LyraMethodEntry body) {
  DeclareTakeover(
      static_cast<ObjectDefinition*>(cls),
      static_cast<const ObjectDefinition* const*>(introduced_by), ordinal,
      body);
}

void lyra_rt_class_declare_property_name(
    void* cls, const void* name, std::uint32_t length, std::uint32_t position) {
  DeclarePropertyName(
      static_cast<ObjectDefinition*>(cls),
      AbiStringRef{static_cast<const char*>(name), length}, position);
}

void lyra_rt_class_declare_behavior_name(
    void* cls, const void* name, std::uint32_t length, std::uint32_t position) {
  DeclareBehaviorName(
      static_cast<ObjectDefinition*>(cls),
      AbiStringRef{static_cast<const char*>(name), length}, position);
}

void lyra_rt_class_declare_body_name(
    void* cls, const void* name, std::uint32_t length, LyraMethodEntry body) {
  DeclareBodyName(
      static_cast<ObjectDefinition*>(cls),
      AbiStringRef{static_cast<const char*>(name), length}, body);
}

void lyra_rt_scope_declare_program(
    void* scope, LyraMethodEntry resolve_state,
    LyraMethodEntry initialize_state, LyraMethodEntry create_processes,
    LyraMethodEntry construct) {
  DeclareScopeProgram(
      static_cast<ScopeDefinition*>(scope),
      std::bit_cast<lyra::runtime::ScopeEntry>(resolve_state),
      std::bit_cast<lyra::runtime::ScopeEntry>(initialize_state),
      std::bit_cast<lyra::runtime::ScopeEntry>(create_processes),
      std::bit_cast<lyra::runtime::ScopeConstructEntry>(construct));
}

void lyra_rt_scope_declare_subroutine(
    void* scope, const void* name, std::uint32_t length,
    LyraMethodEntry entry) {
  DeclareSubroutineName(
      static_cast<ScopeDefinition*>(scope),
      AbiStringRef{static_cast<const char*>(name), length}, entry);
}

void lyra_rt_scope_declare_export(
    void* scope, const void* name, std::uint32_t length,
    LyraMethodEntry entry) {
  DeclareExportName(
      static_cast<ScopeDefinition*>(scope),
      AbiStringRef{static_cast<const char*>(name), length}, entry);
}

void lyra_rt_scope_declare_class(
    void* scope, const void* name, std::uint32_t length, const void* declared) {
  DeclareClassName(
      static_cast<ScopeDefinition*>(scope),
      AbiStringRef{static_cast<const char*>(name), length},
      static_cast<const ObjectDefinition* const*>(declared));
}

auto lyra_rt_run_program(
    std::int32_t argc, char** argv, const void* root, const void* name,
    std::uint32_t length) -> std::int32_t {
  return RunDeclaredProgram(
      argc, argv, std::string_view{static_cast<const char*>(name), length},
      **static_cast<const ScopeDefinition* const*>(root));
}

auto lyra_rt_packed_cell_get(void* cell, void* out) -> void* {
  return Emplace(out, static_cast<Var<PackedArray>*>(cell)->Get());
}

void lyra_rt_packed_cell_initialize(
    void* cell, const void* prototype) noexcept {
  static_cast<Var<PackedArray>*>(cell)->Initialize(
      Read<PackedArray>(prototype));
}

void lyra_rt_packed_cell_set(void* cell, const void* value) {
  static_cast<Var<PackedArray>*>(cell)->Set(Read<PackedArray>(value));
}

void lyra_rt_packed_cell_arm_sampling(void* cell) {
  static_cast<Var<PackedArray>*>(cell)->ArmSampling();
}

auto lyra_rt_packed_cell_sampled_load(void* cell, void* out) -> void* {
  return Emplace(out, static_cast<Var<PackedArray>*>(cell)->SampledGet());
}

auto lyra_rt_ref_to_cell(void* cell) -> void* {
  return LentStorage::OverCell(cell);
}

auto lyra_rt_ref_to_value(void* storage) -> void* {
  return LentStorage::OverValue(storage);
}

auto lyra_rt_packed_ref_get(void* reference, void* out) -> void* {
  return RefGet<PackedArray>(reference, out);
}

void lyra_rt_packed_ref_set(void* reference, const void* value) {
  RefSet<PackedArray>(reference, value);
}

void lyra_rt_packed_ref_arm_sampling(void* reference) {
  RefArmSampling<PackedArray>(reference);
}

auto lyra_rt_packed_ref_sampled_load(void* reference, void* out) -> void* {
  return RefSampledLoad<PackedArray>(reference, out);
}

auto lyra_rt_string_ref_get(void* reference, void* out) -> void* {
  return RefGet<String>(reference, out);
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

auto lyra_rt_real_ref_get(void* reference, void* out) -> void* {
  return RefGet<Real>(reference, out);
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

auto lyra_rt_shortreal_ref_get(void* reference, void* out) -> void* {
  return RefGet<ShortReal>(reference, out);
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

auto lyra_rt_managedref_ref_get(void* reference, void* out) -> void* {
  return RefGet<ManagedRef>(reference, out);
}

void lyra_rt_managedref_ref_set(void* reference, const void* value) {
  RefSet<ManagedRef>(reference, value);
}

void lyra_rt_managedref_ref_arm_sampling(void* reference) {
  RefArmSampling<ManagedRef>(reference);
}

auto lyra_rt_managedref_ref_sampled_load(void* reference, void* out) -> void* {
  return RefSampledLoad<ManagedRef>(reference, out);
}

auto lyra_rt_tuple_ref_get(void* reference, void* out) -> void* {
  return RefGet<RuntimeTuple>(reference, out);
}

void lyra_rt_tuple_ref_set(void* reference, const void* value) {
  RefSet<RuntimeTuple>(reference, value);
}

void lyra_rt_tuple_ref_arm_sampling(void* reference) {
  RefArmSampling<RuntimeTuple>(reference);
}

auto lyra_rt_tuple_ref_sampled_load(void* reference, void* out) -> void* {
  return RefSampledLoad<RuntimeTuple>(reference, out);
}

auto lyra_rt_union_ref_get(void* reference, void* out) -> void* {
  return RefGet<RuntimeUnion>(reference, out);
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

auto lyra_rt_tagged_union_ref_get(void* reference, void* out) -> void* {
  return RefGet<RuntimeTaggedUnion>(reference, out);
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

auto lyra_rt_dynarray_ref_get(void* reference, void* out) -> void* {
  return RefGet<RuntimeDynamicArray>(reference, out);
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

auto lyra_rt_unpackedarray_ref_get(void* reference, void* out) -> void* {
  return RefGet<RuntimeUnpackedArray>(reference, out);
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

auto lyra_rt_queue_ref_get(void* reference, void* out) -> void* {
  return RefGet<RuntimeQueue>(reference, out);
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

auto lyra_rt_assocarray_ref_get(void* reference, void* out) -> void* {
  return RefGet<RuntimeAssociativeArray>(reference, out);
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

auto lyra_rt_packed_cell_begin_takeover(
    void* cell, const void* level, void* out) -> void* {
  return Emplace(
      out, static_cast<Var<PackedArray>*>(cell)->BeginTakeover(
               Read<PackedArray>(level)));
}

auto lyra_rt_packed_cell_drive_takeover(
    void* cell, const void* level, const void* generation, const void* value)
    -> bool {
  return static_cast<Var<PackedArray>*>(cell)->DriveTakeover(
      Read<PackedArray>(level), Read<PackedArray>(generation),
      Read<PackedArray>(value));
}

void lyra_rt_packed_cell_end_takeover(void* cell, const void* level) {
  static_cast<Var<PackedArray>*>(cell)->EndTakeover(Read<PackedArray>(level));
}

auto lyra_rt_string_cell_get(void* cell, void* out) -> void* {
  return Emplace(out, static_cast<Var<String>*>(cell)->Get());
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

auto lyra_rt_real_cell_get(void* cell, void* out) -> void* {
  return Emplace(out, static_cast<Var<Real>*>(cell)->Get());
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

auto lyra_rt_shortreal_cell_get(void* cell, void* out) -> void* {
  return Emplace(out, static_cast<Var<ShortReal>*>(cell)->Get());
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

void lyra_rt_packed_sampled_history_install(
    void* history, const void* default_value, const void* depth) {
  static_cast<SampledHistory<PackedArray>*>(history)->Install(
      Read<PackedArray>(default_value), Read<PackedArray>(depth));
}

void lyra_rt_packed_sampled_history_push(void* history, const void* value) {
  static_cast<SampledHistory<PackedArray>*>(history)->Push(
      Read<PackedArray>(value));
}

auto lyra_rt_packed_sampled_history_at(
    const void* history, const void* ticks_back, void* out) -> void* {
  return Emplace(
      out, static_cast<const SampledHistory<PackedArray>*>(history)->At(
               Read<PackedArray>(ticks_back)));
}

void lyra_rt_string_sampled_history_install(
    void* history, const void* default_value, const void* depth) {
  static_cast<SampledHistory<String>*>(history)->Install(
      Read<String>(default_value), Read<PackedArray>(depth));
}

void lyra_rt_string_sampled_history_push(void* history, const void* value) {
  static_cast<SampledHistory<String>*>(history)->Push(Read<String>(value));
}

auto lyra_rt_string_sampled_history_at(
    const void* history, const void* ticks_back, void* out) -> void* {
  return Emplace(
      out, static_cast<const SampledHistory<String>*>(history)->At(
               Read<PackedArray>(ticks_back)));
}

void lyra_rt_real_sampled_history_install(
    void* history, const void* default_value, const void* depth) {
  static_cast<SampledHistory<Real>*>(history)->Install(
      Read<Real>(default_value), Read<PackedArray>(depth));
}

void lyra_rt_real_sampled_history_push(void* history, const void* value) {
  static_cast<SampledHistory<Real>*>(history)->Push(Read<Real>(value));
}

auto lyra_rt_real_sampled_history_at(
    const void* history, const void* ticks_back, void* out) -> void* {
  return Emplace(
      out, static_cast<const SampledHistory<Real>*>(history)->At(
               Read<PackedArray>(ticks_back)));
}

void lyra_rt_shortreal_sampled_history_install(
    void* history, const void* default_value, const void* depth) {
  static_cast<SampledHistory<ShortReal>*>(history)->Install(
      Read<ShortReal>(default_value), Read<PackedArray>(depth));
}

void lyra_rt_shortreal_sampled_history_push(void* history, const void* value) {
  static_cast<SampledHistory<ShortReal>*>(history)->Push(
      Read<ShortReal>(value));
}

auto lyra_rt_shortreal_sampled_history_at(
    const void* history, const void* ticks_back, void* out) -> void* {
  return Emplace(
      out, static_cast<const SampledHistory<ShortReal>*>(history)->At(
               Read<PackedArray>(ticks_back)));
}

void lyra_rt_tuple_sampled_history_install(
    void* history, const void* default_value, const void* depth) {
  static_cast<SampledHistory<RuntimeTuple>*>(history)->Install(
      Read<RuntimeTuple>(default_value), Read<PackedArray>(depth));
}

void lyra_rt_tuple_sampled_history_push(void* history, const void* value) {
  static_cast<SampledHistory<RuntimeTuple>*>(history)->Push(
      Read<RuntimeTuple>(value));
}

auto lyra_rt_tuple_sampled_history_at(
    const void* history, const void* ticks_back, void* out) -> void* {
  return Emplace(
      out, static_cast<const SampledHistory<RuntimeTuple>*>(history)->At(
               Read<PackedArray>(ticks_back)));
}

void lyra_rt_union_sampled_history_install(
    void* history, const void* default_value, const void* depth) {
  static_cast<SampledHistory<RuntimeUnion>*>(history)->Install(
      Read<RuntimeUnion>(default_value), Read<PackedArray>(depth));
}

void lyra_rt_union_sampled_history_push(void* history, const void* value) {
  static_cast<SampledHistory<RuntimeUnion>*>(history)->Push(
      Read<RuntimeUnion>(value));
}

auto lyra_rt_union_sampled_history_at(
    const void* history, const void* ticks_back, void* out) -> void* {
  return Emplace(
      out, static_cast<const SampledHistory<RuntimeUnion>*>(history)->At(
               Read<PackedArray>(ticks_back)));
}

void lyra_rt_tagged_union_sampled_history_install(
    void* history, const void* default_value, const void* depth) {
  static_cast<SampledHistory<RuntimeTaggedUnion>*>(history)->Install(
      Read<RuntimeTaggedUnion>(default_value), Read<PackedArray>(depth));
}

void lyra_rt_tagged_union_sampled_history_push(
    void* history, const void* value) {
  static_cast<SampledHistory<RuntimeTaggedUnion>*>(history)->Push(
      Read<RuntimeTaggedUnion>(value));
}

auto lyra_rt_tagged_union_sampled_history_at(
    const void* history, const void* ticks_back, void* out) -> void* {
  return Emplace(
      out, static_cast<const SampledHistory<RuntimeTaggedUnion>*>(history)->At(
               Read<PackedArray>(ticks_back)));
}

void lyra_rt_dynarray_sampled_history_install(
    void* history, const void* default_value, const void* depth) {
  static_cast<SampledHistory<RuntimeDynamicArray>*>(history)->Install(
      Read<RuntimeDynamicArray>(default_value), Read<PackedArray>(depth));
}

void lyra_rt_dynarray_sampled_history_push(void* history, const void* value) {
  static_cast<SampledHistory<RuntimeDynamicArray>*>(history)->Push(
      Read<RuntimeDynamicArray>(value));
}

auto lyra_rt_dynarray_sampled_history_at(
    const void* history, const void* ticks_back, void* out) -> void* {
  return Emplace(
      out, static_cast<const SampledHistory<RuntimeDynamicArray>*>(history)->At(
               Read<PackedArray>(ticks_back)));
}

void lyra_rt_unpackedarray_sampled_history_install(
    void* history, const void* default_value, const void* depth) {
  static_cast<SampledHistory<RuntimeUnpackedArray>*>(history)->Install(
      Read<RuntimeUnpackedArray>(default_value), Read<PackedArray>(depth));
}

void lyra_rt_unpackedarray_sampled_history_push(
    void* history, const void* value) {
  static_cast<SampledHistory<RuntimeUnpackedArray>*>(history)->Push(
      Read<RuntimeUnpackedArray>(value));
}

auto lyra_rt_unpackedarray_sampled_history_at(
    const void* history, const void* ticks_back, void* out) -> void* {
  return Emplace(
      out,
      static_cast<const SampledHistory<RuntimeUnpackedArray>*>(history)->At(
          Read<PackedArray>(ticks_back)));
}

void lyra_rt_queue_sampled_history_install(
    void* history, const void* default_value, const void* depth) {
  static_cast<SampledHistory<RuntimeQueue>*>(history)->Install(
      Read<RuntimeQueue>(default_value), Read<PackedArray>(depth));
}

void lyra_rt_queue_sampled_history_push(void* history, const void* value) {
  static_cast<SampledHistory<RuntimeQueue>*>(history)->Push(
      Read<RuntimeQueue>(value));
}

auto lyra_rt_queue_sampled_history_at(
    const void* history, const void* ticks_back, void* out) -> void* {
  return Emplace(
      out, static_cast<const SampledHistory<RuntimeQueue>*>(history)->At(
               Read<PackedArray>(ticks_back)));
}

void lyra_rt_assocarray_sampled_history_install(
    void* history, const void* default_value, const void* depth) {
  static_cast<SampledHistory<RuntimeAssociativeArray>*>(history)->Install(
      Read<RuntimeAssociativeArray>(default_value), Read<PackedArray>(depth));
}

void lyra_rt_assocarray_sampled_history_push(void* history, const void* value) {
  static_cast<SampledHistory<RuntimeAssociativeArray>*>(history)->Push(
      Read<RuntimeAssociativeArray>(value));
}

auto lyra_rt_assocarray_sampled_history_at(
    const void* history, const void* ticks_back, void* out) -> void* {
  return Emplace(
      out,
      static_cast<const SampledHistory<RuntimeAssociativeArray>*>(history)->At(
          Read<PackedArray>(ticks_back)));
}

// Each entry keeps a share of the object its handle names, so an object the
// program no longer names anywhere is still there for a past tick to answer
// with -- which LRM 8.4 requires, since it reclaims an object only once nothing
// references it and a kept sampled value is a reference.
void lyra_rt_managedref_sampled_history_install(
    void* history, const void* default_value, const void* depth) {
  static_cast<SampledHistory<ManagedRef>*>(history)->Install(
      Read<ManagedRef>(default_value), Read<PackedArray>(depth));
}

void lyra_rt_managedref_sampled_history_push(void* history, const void* value) {
  static_cast<SampledHistory<ManagedRef>*>(history)->Push(
      Read<ManagedRef>(value));
}

auto lyra_rt_managedref_sampled_history_at(
    const void* history, const void* ticks_back, void* out) -> void* {
  return Emplace(
      out, static_cast<const SampledHistory<ManagedRef>*>(history)->At(
               Read<PackedArray>(ticks_back)));
}

void lyra_rt_evaluation_attempts_install(
    void* attempts, void* effects, std::uint64_t words, bool pending_holds,
    void* pass_action, void* fail_action) {
  static_cast<EvaluationAttempts*>(attempts)->Install(
      *static_cast<RuntimeEffects*>(effects), words, pending_holds,
      TakeClosure(pass_action), TakeClosure(fail_action));
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
// the declared representation -- and a load copies the current value into the
// storage the reader gives, like any other value the boundary hands back. A
// procedural
// local is not observable, so no runtime handle threads through and no
// subscriber wakes.
auto lyra_rt_packed_value_cell_alloc() noexcept -> void* {
  return GeneratedCallScope::Current()
      .ActivationValues()
      .New<ActivationValueCell<PackedArray>>();
}

auto lyra_rt_string_value_cell_alloc() noexcept -> void* {
  return GeneratedCallScope::Current()
      .ActivationValues()
      .New<ActivationValueCell<String>>();
}

void lyra_rt_packed_value_cell_store(void* cell, const void* value) noexcept {
  static_cast<ActivationValueCell<PackedArray>*>(cell)->Store(
      Read<PackedArray>(value));
}

void lyra_rt_string_value_cell_store(void* cell, const void* value) noexcept {
  static_cast<ActivationValueCell<String>*>(cell)->Store(Read<String>(value));
}

auto lyra_rt_packed_value_cell_load(const void* cell, void* out) noexcept
    -> void* {
  return Emplace(
      out, static_cast<const ActivationValueCell<PackedArray>*>(cell)->Get());
}

auto lyra_rt_string_value_cell_load(const void* cell, void* out) noexcept
    -> void* {
  return Emplace(
      out, static_cast<const ActivationValueCell<String>*>(cell)->Get());
}

auto lyra_rt_packed_add(const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(out, Read<PackedArray>(lhs) + Read<PackedArray>(rhs));
}

auto lyra_rt_packed_sub(const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(out, Read<PackedArray>(lhs) - Read<PackedArray>(rhs));
}

auto lyra_rt_packed_mul(const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(out, Read<PackedArray>(lhs) * Read<PackedArray>(rhs));
}

auto lyra_rt_packed_div(const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(out, Read<PackedArray>(lhs) / Read<PackedArray>(rhs));
}

auto lyra_rt_packed_mod(const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(out, Read<PackedArray>(lhs) % Read<PackedArray>(rhs));
}

auto lyra_rt_packed_and(const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(out, Read<PackedArray>(lhs) & Read<PackedArray>(rhs));
}

auto lyra_rt_packed_or(const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(out, Read<PackedArray>(lhs) | Read<PackedArray>(rhs));
}

auto lyra_rt_packed_xor(const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(out, Read<PackedArray>(lhs) ^ Read<PackedArray>(rhs));
}

auto lyra_rt_packed_eq(const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(out, Read<PackedArray>(lhs) == Read<PackedArray>(rhs));
}

auto lyra_rt_packed_ne(const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(out, Read<PackedArray>(lhs) != Read<PackedArray>(rhs));
}

auto lyra_rt_packed_lt(const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(out, Read<PackedArray>(lhs) < Read<PackedArray>(rhs));
}

auto lyra_rt_packed_le(const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(out, Read<PackedArray>(lhs) <= Read<PackedArray>(rhs));
}

auto lyra_rt_packed_gt(const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(out, Read<PackedArray>(lhs) > Read<PackedArray>(rhs));
}

auto lyra_rt_packed_ge(const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(out, Read<PackedArray>(lhs) >= Read<PackedArray>(rhs));
}

auto lyra_rt_packed_logical_and(const void* lhs, const void* rhs, void* out)
    -> void* {
  return Emplace(out, Read<PackedArray>(lhs) && Read<PackedArray>(rhs));
}

auto lyra_rt_packed_logical_or(const void* lhs, const void* rhs, void* out)
    -> void* {
  return Emplace(out, Read<PackedArray>(lhs) || Read<PackedArray>(rhs));
}

auto lyra_rt_packed_neg(const void* operand, void* out) -> void* {
  return Emplace(out, -Read<PackedArray>(operand));
}

auto lyra_rt_packed_not(const void* operand, void* out) -> void* {
  return Emplace(out, ~Read<PackedArray>(operand));
}

auto lyra_rt_packed_logical_not(const void* operand, void* out) -> void* {
  return Emplace(out, !Read<PackedArray>(operand));
}

auto lyra_rt_packed_inc(const void* operand, void* out) -> void* {
  PackedArray value = Read<PackedArray>(operand);
  ++value;
  return Emplace(out, std::move(value));
}

auto lyra_rt_packed_dec(const void* operand, void* out) -> void* {
  PackedArray value = Read<PackedArray>(operand);
  --value;
  return Emplace(out, std::move(value));
}

auto lyra_rt_packed_to_bool(const void* operand) -> bool {
  return static_cast<bool>(Read<PackedArray>(operand));
}

auto lyra_rt_packed_convert_from_packed(
    const void* src, const void* type, void* out) -> void* {
  return Emplace(
      out,
      PackedArray::ConvertFrom(Read<PackedArray>(src), Read<PackedType>(type)));
}

auto lyra_rt_packed_from_int(std::int64_t value, const void* type, void* out)
    -> void* {
  return Emplace(out, PackedArray::FromInt(value, Read<PackedType>(type)));
}

auto lyra_rt_packed_from_bool(bool value, void* out) -> void* {
  return Emplace(out, PackedArray::FromBool(value));
}

auto lyra_rt_packed_to_int64(const void* value) -> std::int64_t {
  return Read<PackedArray>(value).ToInt64();
}

auto lyra_rt_packed_is_unknown(const void* value, void* out) -> void* {
  return Emplace(out, Read<PackedArray>(value).IsUnknown());
}

auto lyra_rt_packed_count_bits(
    const void* value, const void* control_bits, void* out) -> void* {
  return Emplace(
      out, Read<PackedArray>(value).CountBits(Read<PackedArray>(control_bits)));
}

auto lyra_rt_packed_clog2(const void* value, void* out) -> void* {
  return Emplace(out, Read<PackedArray>(value).Clog2());
}

auto lyra_rt_packed_pow(const void* base, const void* exponent, void* out)
    -> void* {
  return Emplace(out, Read<PackedArray>(base).Pow(Read<PackedArray>(exponent)));
}

// The guarded value is handed back rather than copied: what crosses here is the
// handle the caller already holds, and a guard that let the access through has
// changed nothing about it.
auto lyra_rt_require(void* value, const void* condition, const char* message)
    -> void* {
  lyra::value::RequireCondition(Read<PackedArray>(condition), message);
  return value;
}

auto lyra_rt_packed_concat(const void* lhs, const void* rhs, void* out)
    -> void* {
  return Emplace(out, Read<PackedArray>(lhs).Concat(Read<PackedArray>(rhs)));
}

auto lyra_rt_packed_replicate(
    const void* operand, std::int64_t count, void* out) -> void* {
  return Emplace(out, Read<PackedArray>(operand).Replicate(count));
}

auto lyra_rt_packed_shift_left(const void* value, const void* amount, void* out)
    -> void* {
  return Emplace(
      out, Read<PackedArray>(value).ShiftLeft(Read<PackedArray>(amount)));
}

auto lyra_rt_packed_logical_shift_right(
    const void* value, const void* amount, void* out) -> void* {
  return Emplace(
      out,
      Read<PackedArray>(value).LogicalShiftRight(Read<PackedArray>(amount)));
}

auto lyra_rt_packed_arithmetic_shift_right(
    const void* value, const void* amount, void* out) -> void* {
  return Emplace(
      out,
      Read<PackedArray>(value).ArithmeticShiftRight(Read<PackedArray>(amount)));
}

// The applying form of each shift (LRM 11.4.1). What a generated module applies
// it to is a copy read out of the receiver's storage, so what "applying" means
// here is a value with the shift already in it, handed back for the caller to
// store where the receiver came from.
auto lyra_rt_packed_shift_left_assign(
    const void* value, const void* amount, void* out) -> void* {
  PackedArray applied = Read<PackedArray>(value);
  applied.ShiftLeftAssign(Read<PackedArray>(amount));
  return Emplace(out, std::move(applied));
}

auto lyra_rt_packed_logical_shift_right_assign(
    const void* value, const void* amount, void* out) -> void* {
  PackedArray applied = Read<PackedArray>(value);
  applied.LogicalShiftRightAssign(Read<PackedArray>(amount));
  return Emplace(out, std::move(applied));
}

auto lyra_rt_packed_arithmetic_shift_right_assign(
    const void* value, const void* amount, void* out) -> void* {
  PackedArray applied = Read<PackedArray>(value);
  applied.ArithmeticShiftRightAssign(Read<PackedArray>(amount));
  return Emplace(out, std::move(applied));
}

auto lyra_rt_packed_bitwise_xnor(const void* lhs, const void* rhs, void* out)
    -> void* {
  return Emplace(
      out, Read<PackedArray>(lhs).BitwiseXnor(Read<PackedArray>(rhs)));
}

auto lyra_rt_packed_logical_implication(
    const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(
      out, Read<PackedArray>(lhs).LogicalImplication(Read<PackedArray>(rhs)));
}

auto lyra_rt_packed_logical_equivalence(
    const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(
      out, Read<PackedArray>(lhs).LogicalEquivalence(Read<PackedArray>(rhs)));
}

auto lyra_rt_packed_case_equal(const void* lhs, const void* rhs, void* out)
    -> void* {
  return Emplace(out, Read<PackedArray>(lhs).CaseEqual(Read<PackedArray>(rhs)));
}

auto lyra_rt_packed_wildcard_equals(const void* lhs, const void* rhs, void* out)
    -> void* {
  return Emplace(
      out, Read<PackedArray>(lhs).WildcardEquals(Read<PackedArray>(rhs)));
}

auto lyra_rt_packed_casez_equals(const void* lhs, const void* rhs, void* out)
    -> void* {
  return Emplace(
      out, Read<PackedArray>(lhs).CasezEquals(Read<PackedArray>(rhs)));
}

auto lyra_rt_packed_casex_equals(const void* lhs, const void* rhs, void* out)
    -> void* {
  return Emplace(
      out, Read<PackedArray>(lhs).CasexEquals(Read<PackedArray>(rhs)));
}

auto lyra_rt_packed_merge_conditional(
    const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(
      out, Read<PackedArray>(lhs).MergeConditional(Read<PackedArray>(rhs)));
}

auto lyra_rt_packed_reduction_and(const void* value, void* out) -> void* {
  return Emplace(out, Read<PackedArray>(value).ReductionAnd());
}

auto lyra_rt_packed_reduction_or(const void* value, void* out) -> void* {
  return Emplace(out, Read<PackedArray>(value).ReductionOr());
}

auto lyra_rt_packed_reduction_xor(const void* value, void* out) -> void* {
  return Emplace(out, Read<PackedArray>(value).ReductionXor());
}

auto lyra_rt_packed_reduction_nand(const void* value, void* out) -> void* {
  return Emplace(out, Read<PackedArray>(value).ReductionNand());
}

auto lyra_rt_packed_reduction_nor(const void* value, void* out) -> void* {
  return Emplace(out, Read<PackedArray>(value).ReductionNor());
}

auto lyra_rt_packed_reduction_xnor(const void* value, void* out) -> void* {
  return Emplace(out, Read<PackedArray>(value).ReductionXnor());
}

auto lyra_rt_packed_slice(
    const void* value, const void* position, std::int64_t width, void* out)
    -> void* {
  return Emplace(
      out, Read<PackedArray>(value).Slice(Read<PackedArray>(position), width));
}

auto lyra_rt_packed_with_slice(
    const void* value, const void* position, std::int64_t width,
    const void* replacement, void* out) -> void* {
  return Emplace(
      out,
      Read<PackedArray>(value).WithSlice(
          Read<PackedArray>(position), width, Read<PackedArray>(replacement)));
}

auto lyra_rt_packed_to_position(const void* index, void* out) -> void* {
  return Emplace(out, PackedArray::ToPosition(Read<PackedArray>(index)));
}

// Materializes a borrowed packed view (a container element or slice read) into
// an owning value. On the execution backend a container access already copies
// the element out, so this is an idempotent copy that keeps the ownership shape
// the source-level `to_owned` names.
auto lyra_rt_packed_to_owned(const void* value, void* out) -> void* {
  return Emplace(out, Read<PackedArray>(value).ToOwned());
}

auto lyra_rt_string_from_packed_array(const void* bits, void* out) -> void* {
  return Emplace(out, String::FromPackedArray(Read<PackedArray>(bits)));
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

auto lyra_rt_string_getc(const void* value, const void* index, void* out)
    -> void* {
  return Emplace(out, Read<String>(value).Getc(Read<PackedArray>(index)));
}

auto lyra_rt_string_element(const void* value, const void* index, void* out)
    -> void* {
  return Emplace(out, Read<String>(value).Element(Read<PackedArray>(index)));
}

// The functional character write (LRM 6.16.2): a new string with character
// `index` replaced. Synthesized at MIR-to-LIR for a string reached by an opaque
// handle, the string counterpart of `lyra_rt_dynarray_with_element`.
auto lyra_rt_string_with_element(
    const void* value, const void* index, const void* replacement, void* out)
    -> void* {
  return Emplace(
      out, Read<String>(value).WithElement(
               Read<PackedArray>(index), Read<PackedArray>(replacement)));
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
               Read<PackedArray>(first), Read<PackedArray>(last)));
}

auto lyra_rt_string_concat(const void* lhs, const void* rhs, void* out)
    -> void* {
  return Emplace(out, Read<String>(lhs).Concat(Read<String>(rhs)));
}

auto lyra_rt_string_replicate(
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

// The formatting family mutates its receiver in the source language, so each
// entry copies the receiver, applies the mutation to the copy, and returns it.
auto lyra_rt_string_putc(
    const void* value, const void* index, const void* character, void* out)
    -> void* {
  String result = Read<String>(value);
  result.Putc(Read<PackedArray>(index), Read<PackedArray>(character));
  return Emplace(out, std::move(result));
}

auto lyra_rt_string_itoa(const void* value, const void* number, void* out)
    -> void* {
  String result = Read<String>(value);
  result.Itoa(Read<PackedArray>(number));
  return Emplace(out, std::move(result));
}

auto lyra_rt_string_hextoa(const void* value, const void* number, void* out)
    -> void* {
  String result = Read<String>(value);
  result.Hextoa(Read<PackedArray>(number));
  return Emplace(out, std::move(result));
}

auto lyra_rt_string_octtoa(const void* value, const void* number, void* out)
    -> void* {
  String result = Read<String>(value);
  result.Octtoa(Read<PackedArray>(number));
  return Emplace(out, std::move(result));
}

auto lyra_rt_string_bintoa(const void* value, const void* number, void* out)
    -> void* {
  String result = Read<String>(value);
  result.Bintoa(Read<PackedArray>(number));
  return Emplace(out, std::move(result));
}

auto lyra_rt_string_realtoa(const void* value, const void* number, void* out)
    -> void* {
  String result = Read<String>(value);
  result.Realtoa(Read<Real>(number));
  return Emplace(out, std::move(result));
}

auto lyra_rt_string_scan_string(
    const void* input, const void* format, const void* prototypes, void* out)
    -> void* {
  return lyra::runtime::EmplaceScan(
      out, Read<String>(input), Read<String>(format),
      lyra::value::detail::NullByte::kWhiteSpace,
      Read<lyra::value::RuntimeTuple>(prototypes));
}

auto lyra_rt_string_scan_file(
    const void* input, const void* format, const void* prototypes, void* out)
    -> void* {
  return lyra::runtime::EmplaceScan(
      out, Read<String>(input), Read<String>(format),
      lyra::value::detail::NullByte::kOrdinary,
      Read<lyra::value::RuntimeTuple>(prototypes));
}

auto lyra_rt_string_add(const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(out, Read<String>(lhs) + Read<String>(rhs));
}

auto lyra_rt_string_eq(const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(out, Read<String>(lhs) == Read<String>(rhs));
}

auto lyra_rt_string_case_equal(const void* lhs, const void* rhs, void* out)
    -> void* {
  return Emplace(out, Read<String>(lhs) == Read<String>(rhs));
}

auto lyra_rt_string_ne(const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(out, Read<String>(lhs) != Read<String>(rhs));
}

auto lyra_rt_string_lt(const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(out, Read<String>(lhs) < Read<String>(rhs));
}

auto lyra_rt_string_le(const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(out, Read<String>(lhs) <= Read<String>(rhs));
}

auto lyra_rt_string_gt(const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(out, Read<String>(lhs) > Read<String>(rhs));
}

auto lyra_rt_string_ge(const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(out, Read<String>(lhs) >= Read<String>(rhs));
}

auto lyra_rt_make_format_spec(
    const void* kind, const void* width, const void* precision,
    const void* zero_pad, const void* left_align, const void* timeunit_power,
    void* out) -> void* {
  return Emplace(
      out,
      FormatSpec(
          Read<PackedArray>(kind), Read<PackedArray>(width),
          Read<PackedArray>(precision), Read<PackedArray>(zero_pad),
          Read<PackedArray>(left_align), Read<PackedArray>(timeunit_power)));
}

auto lyra_rt_packed_make_print_value_item(
    const void* value, const void* spec, void* out) -> void* {
  return Emplace(
      out, PrintItem(PrintValueItem(
               Read<PackedArray>(value), Read<FormatSpec>(spec))));
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
      out, PrintItem(PrintValueItem(
               Read<ManagedRef>(value), Read<FormatSpec>(spec))));
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

auto lyra_rt_real_inc(const void* operand, void* out) -> void* {
  Real value = Read<Real>(operand);
  ++value;
  return Emplace(out, std::move(value));
}

auto lyra_rt_real_dec(const void* operand, void* out) -> void* {
  Real value = Read<Real>(operand);
  --value;
  return Emplace(out, std::move(value));
}

auto lyra_rt_real_eq(const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(out, Read<Real>(lhs) == Read<Real>(rhs));
}

auto lyra_rt_real_ne(const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(out, Read<Real>(lhs) != Read<Real>(rhs));
}

auto lyra_rt_real_lt(const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(out, Read<Real>(lhs) < Read<Real>(rhs));
}

auto lyra_rt_real_le(const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(out, Read<Real>(lhs) <= Read<Real>(rhs));
}

auto lyra_rt_real_gt(const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(out, Read<Real>(lhs) > Read<Real>(rhs));
}

auto lyra_rt_real_ge(const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(out, Read<Real>(lhs) >= Read<Real>(rhs));
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

auto lyra_rt_real_value_cell_load(const void* cell, void* out) noexcept
    -> void* {
  return Emplace(
      out, static_cast<const ActivationValueCell<Real>*>(cell)->Get());
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

auto lyra_rt_shortreal_inc(const void* operand, void* out) -> void* {
  ShortReal value = Read<ShortReal>(operand);
  ++value;
  return Emplace(out, std::move(value));
}

auto lyra_rt_shortreal_dec(const void* operand, void* out) -> void* {
  ShortReal value = Read<ShortReal>(operand);
  --value;
  return Emplace(out, std::move(value));
}

auto lyra_rt_shortreal_eq(const void* lhs, const void* rhs, void* out)
    -> void* {
  return Emplace(out, Read<ShortReal>(lhs) == Read<ShortReal>(rhs));
}

auto lyra_rt_shortreal_ne(const void* lhs, const void* rhs, void* out)
    -> void* {
  return Emplace(out, Read<ShortReal>(lhs) != Read<ShortReal>(rhs));
}

auto lyra_rt_shortreal_lt(const void* lhs, const void* rhs, void* out)
    -> void* {
  return Emplace(out, Read<ShortReal>(lhs) < Read<ShortReal>(rhs));
}

auto lyra_rt_shortreal_le(const void* lhs, const void* rhs, void* out)
    -> void* {
  return Emplace(out, Read<ShortReal>(lhs) <= Read<ShortReal>(rhs));
}

auto lyra_rt_shortreal_gt(const void* lhs, const void* rhs, void* out)
    -> void* {
  return Emplace(out, Read<ShortReal>(lhs) > Read<ShortReal>(rhs));
}

auto lyra_rt_shortreal_ge(const void* lhs, const void* rhs, void* out)
    -> void* {
  return Emplace(out, Read<ShortReal>(lhs) >= Read<ShortReal>(rhs));
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

auto lyra_rt_shortreal_value_cell_load(const void* cell, void* out) noexcept
    -> void* {
  return Emplace(
      out, static_cast<const ActivationValueCell<ShortReal>*>(cell)->Get());
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

auto lyra_rt_chandle_eq(const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(out, Read<Chandle>(lhs) == Read<Chandle>(rhs));
}

auto lyra_rt_chandle_ne(const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(out, Read<Chandle>(lhs) != Read<Chandle>(rhs));
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

auto lyra_rt_chandle_value_cell_load(const void* cell, void* out) noexcept
    -> void* {
  return Emplace(
      out, static_cast<const ActivationValueCell<Chandle>*>(cell)->Get());
}

// A handle referring to nothing (LRM 8.4), a value of the domain like any
// other.
auto lyra_rt_managedref_default(void* out) -> void* {
  return Emplace(out, ManagedRef{});
}

// Comparing two handles asks which object each names (LRM 11.4.5). The clause
// makes the answer always a known 1'b0 or 1'b1, so the entry answers with that
// value.
auto lyra_rt_managedref_eq(const void* lhs, const void* rhs, void* out)
    -> void* {
  return Emplace(out, Read<ManagedRef>(lhs) == Read<ManagedRef>(rhs));
}

auto lyra_rt_managedref_ne(const void* lhs, const void* rhs, void* out)
    -> void* {
  return Emplace(out, Read<ManagedRef>(lhs) != Read<ManagedRef>(rhs));
}

// LRM 11.4.5: `===` on a handle carries the same meaning as `==`.
auto lyra_rt_managedref_case_equal(const void* lhs, const void* rhs, void* out)
    -> void* {
  return Emplace(out, Read<ManagedRef>(lhs).CaseEqual(Read<ManagedRef>(rhs)));
}

auto lyra_rt_managedref_to_bool(const void* operand) -> bool {
  return static_cast<bool>(Read<ManagedRef>(operand));
}

auto lyra_rt_managedref_value_cell_alloc() noexcept -> void* {
  return GeneratedCallScope::Current()
      .ActivationValues()
      .New<ActivationValueCell<ManagedRef>>();
}

// Storing copies the handle's share of ownership with it, which is what keeps
// the object alive once the handle it was stored from has ended; loading
// copies one back out, so the reader owns what it was handed for as long as it
// holds it.
void lyra_rt_managedref_value_cell_store(
    void* cell, const void* value) noexcept {
  static_cast<ActivationValueCell<ManagedRef>*>(cell)->Store(
      Read<ManagedRef>(value));
}

auto lyra_rt_managedref_value_cell_load(const void* cell, void* out) noexcept
    -> void* {
  return Emplace(
      out, static_cast<const ActivationValueCell<ManagedRef>*>(cell)->Get());
}

// A variable of class type, which a process may wait on: LRM 9.4.2 makes a
// write to one an event whenever the object it names is not the object it
// named. A store keeps the handle's share of ownership and a load hands one
// back, so the object outlives every body that touches the variable.
auto lyra_rt_managedref_cell_get(void* cell, void* out) -> void* {
  return Emplace(out, static_cast<Var<ManagedRef>*>(cell)->Get());
}

void lyra_rt_managedref_cell_initialize(
    void* cell, const void* prototype) noexcept {
  static_cast<Var<ManagedRef>*>(cell)->Initialize(Read<ManagedRef>(prototype));
}

void lyra_rt_managedref_cell_set(void* cell, const void* value) {
  static_cast<Var<ManagedRef>*>(cell)->Set(Read<ManagedRef>(value));
}

// Arming keeps a share of whatever the variable names at the moment it is
// armed, and every later slot the variable moves away from replaces it, so the
// object a sampled read answers with is alive for as long as that read can
// happen (LRM 8.4, 16.5.1).
void lyra_rt_managedref_cell_arm_sampling(void* cell) {
  static_cast<Var<ManagedRef>*>(cell)->ArmSampling();
}

auto lyra_rt_managedref_cell_sampled_load(void* cell, void* out) -> void* {
  return Emplace(out, static_cast<Var<ManagedRef>*>(cell)->SampledGet());
}

// Boxes a value-domain handle into a type-erased `RuntimeValue`. A value
// crosses this way exactly where it states a representation the entry receiving
// it has no other way to know: a product's components, each of its own domain,
// and a container construction's element prototype. The domain rides in the
// symbol name, so the generated side never inspects the value's runtime
// representation.
auto lyra_rt_packed_value_box(const void* value, void* out) -> void* {
  return Emplace(out, RuntimeValue{Read<PackedArray>(value)});
}

auto lyra_rt_string_value_box(const void* value, void* out) -> void* {
  return Emplace(out, RuntimeValue{Read<String>(value)});
}

auto lyra_rt_real_value_box(const void* value, void* out) -> void* {
  return Emplace(out, RuntimeValue{Read<Real>(value)});
}

auto lyra_rt_shortreal_value_box(const void* value, void* out) -> void* {
  return Emplace(out, RuntimeValue{Read<ShortReal>(value)});
}

auto lyra_rt_chandle_value_box(const void* value, void* out) -> void* {
  return Emplace(out, RuntimeValue{Read<Chandle>(value)});
}

auto lyra_rt_managedref_value_box(const void* value, void* out) -> void* {
  return Emplace(out, RuntimeValue{Read<ManagedRef>(value)});
}

auto lyra_rt_tuple_value_box(const void* value, void* out) -> void* {
  return Emplace(out, RuntimeValue{Read<RuntimeTuple>(value)});
}

auto lyra_rt_dynarray_value_box(const void* value, void* out) -> void* {
  return Emplace(out, RuntimeValue{Read<RuntimeDynamicArray>(value)});
}

auto lyra_rt_unpackedarray_value_box(const void* value, void* out) -> void* {
  return Emplace(out, RuntimeValue{Read<RuntimeUnpackedArray>(value)});
}

auto lyra_rt_tuple_make(LyraSpan components, void* out) -> void* {
  std::span<RuntimeValue*> handles(
      static_cast<RuntimeValue**>(components.data), components.count);
  std::vector<RuntimeValue> collected;
  collected.reserve(components.count);
  for (RuntimeValue* handle : handles) {
    collected.push_back(std::move(*handle));
  }
  return Emplace(out, RuntimeTuple(std::move(collected)));
}

auto lyra_rt_tuple_extract(const void* tuple, std::int64_t index, void* out)
    -> void* {
  return lyra::runtime::ElementInto(
      out,
      Read<RuntimeTuple>(tuple).Component(static_cast<std::size_t>(index)));
}

auto lyra_rt_tuple_update(
    const void* tuple, std::int64_t index, void* value, void* out) -> void* {
  RuntimeTuple result = Read<RuntimeTuple>(tuple);
  const auto slot = static_cast<std::size_t>(index);
  RuntimeValue replacement = std::visit(
      [&](const auto& current) -> RuntimeValue {
        using T = std::decay_t<decltype(current)>;
        return RuntimeValue{Read<T>(value)};
      },
      result.Component(slot).value);
  result.SetComponent(slot, std::move(replacement));
  return Emplace(out, std::move(result));
}

auto lyra_rt_tuple_eq(const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(out, Read<RuntimeTuple>(lhs) == Read<RuntimeTuple>(rhs));
}

auto lyra_rt_tuple_ne(const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(out, Read<RuntimeTuple>(lhs) != Read<RuntimeTuple>(rhs));
}

auto lyra_rt_tuple_case_equal(const void* lhs, const void* rhs, void* out)
    -> void* {
  return Emplace(
      out, Read<RuntimeTuple>(lhs).CaseEqual(Read<RuntimeTuple>(rhs)));
}

auto lyra_rt_tuple_is_unknown(const void* value, void* out) -> void* {
  return Emplace(out, Read<RuntimeTuple>(value).IsUnknown());
}

auto lyra_rt_tuple_cell_get(void* cell, void* out) -> void* {
  return Emplace(out, static_cast<Var<RuntimeTuple>*>(cell)->Get());
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

auto lyra_rt_tuple_value_cell_load(const void* cell, void* out) noexcept
    -> void* {
  return Emplace(
      out, static_cast<const ActivationValueCell<RuntimeTuple>*>(cell)->Get());
}

auto lyra_rt_union_make(std::int64_t index, void* value, void* out) -> void* {
  return Emplace(
      out,
      RuntimeUnion(
          static_cast<std::size_t>(index), lyra::runtime::ErasedValue(value)));
}

auto lyra_rt_union_extract(const void* value, std::int64_t index, void* out)
    -> void* {
  return lyra::runtime::ElementInto(
      out, Read<RuntimeUnion>(value).Member(static_cast<std::size_t>(index)));
}

auto lyra_rt_union_update(
    const void* value, std::int64_t index, void* member, void* out) -> void* {
  RuntimeUnion result = Read<RuntimeUnion>(value);
  result.SetActive(
      static_cast<std::size_t>(index), lyra::runtime::ErasedValue(member));
  return Emplace(out, std::move(result));
}

auto lyra_rt_union_value_box(const void* value, void* out) -> void* {
  return Emplace(out, RuntimeValue{Read<RuntimeUnion>(value)});
}

auto lyra_rt_union_eq(const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(out, Read<RuntimeUnion>(lhs) == Read<RuntimeUnion>(rhs));
}

auto lyra_rt_union_ne(const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(out, Read<RuntimeUnion>(lhs) != Read<RuntimeUnion>(rhs));
}

auto lyra_rt_union_case_equal(const void* lhs, const void* rhs, void* out)
    -> void* {
  return Emplace(
      out, Read<RuntimeUnion>(lhs).CaseEqual(Read<RuntimeUnion>(rhs)));
}

auto lyra_rt_union_is_unknown(const void* value, void* out) -> void* {
  return Emplace(out, Read<RuntimeUnion>(value).IsUnknown());
}

auto lyra_rt_union_cell_get(void* cell, void* out) -> void* {
  return Emplace(out, static_cast<Var<RuntimeUnion>*>(cell)->Get());
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

auto lyra_rt_union_value_cell_load(const void* cell, void* out) noexcept
    -> void* {
  return Emplace(
      out, static_cast<const ActivationValueCell<RuntimeUnion>*>(cell)->Get());
}

auto lyra_rt_tagged_union_make(std::int64_t tag, void* payload, void* out)
    -> void* {
  return Emplace(
      out,
      RuntimeTaggedUnion(
          static_cast<std::size_t>(tag), lyra::runtime::ErasedValue(payload)));
}

auto lyra_rt_tagged_union_extract(
    const void* value, std::int64_t index, void* out) -> void* {
  return lyra::runtime::ElementInto(
      out,
      Read<RuntimeTaggedUnion>(value).Member(static_cast<std::size_t>(index)));
}

auto lyra_rt_tagged_union_update(
    const void* value, std::int64_t index, void* member, void* out) -> void* {
  RuntimeTaggedUnion result = Read<RuntimeTaggedUnion>(value);
  result.SetMember(
      static_cast<std::size_t>(index), lyra::runtime::ErasedValue(member));
  return Emplace(out, std::move(result));
}

// Whether the active tag is `index`, as the machine boolean the pattern-match
// guard tests (LRM 12.6) -- the same shape a value's `to_bool` yields, which an
// enclosing `from_bool` lifts to the packed one-bit surface. The runtime holds
// the comparison, so no packed tag constant crosses the boundary.
auto lyra_rt_tagged_union_tag_matches(const void* value, std::int64_t index)
    -> bool {
  return Read<RuntimeTaggedUnion>(value).Tag() ==
         static_cast<std::size_t>(index);
}

auto lyra_rt_tagged_union_value_box(const void* value, void* out) -> void* {
  return Emplace(out, RuntimeValue{Read<RuntimeTaggedUnion>(value)});
}

auto lyra_rt_tagged_union_eq(const void* lhs, const void* rhs, void* out)
    -> void* {
  return Emplace(
      out, Read<RuntimeTaggedUnion>(lhs) == Read<RuntimeTaggedUnion>(rhs));
}

auto lyra_rt_tagged_union_ne(const void* lhs, const void* rhs, void* out)
    -> void* {
  return Emplace(
      out, Read<RuntimeTaggedUnion>(lhs) != Read<RuntimeTaggedUnion>(rhs));
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

auto lyra_rt_tagged_union_cell_get(void* cell, void* out) -> void* {
  return Emplace(out, static_cast<Var<RuntimeTaggedUnion>*>(cell)->Get());
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

auto lyra_rt_tagged_union_value_cell_load(const void* cell, void* out) noexcept
    -> void* {
  return Emplace(
      out,
      static_cast<const ActivationValueCell<RuntimeTaggedUnion>*>(cell)->Get());
}

// A tagged union's `void` member (LRM 7.3.2) carries a value with no bits.
// `default` builds the one value it has; `value_box` erases it for a build's
// payload the way every other domain does.
auto lyra_rt_empty_default(void* out) -> void* {
  return Emplace(out, lyra::value::Empty{});
}

auto lyra_rt_empty_value_box(const void* value, void* out) -> void* {
  return Emplace(out, RuntimeValue{Read<lyra::value::Empty>(value)});
}

auto lyra_rt_make_dynamic_array_default(void* prototype, void* out) -> void* {
  return Emplace(
      out, RuntimeDynamicArray(lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_make_dynamic_array_new(
    const void* size, void* prototype, void* out) -> void* {
  return Emplace(
      out, RuntimeDynamicArray(
               Read<PackedArray>(size), lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_make_dynamic_array_new_copy(
    const void* size, void* prototype, const void* src, void* out) -> void* {
  return Emplace(
      out, RuntimeDynamicArray(
               Read<PackedArray>(size), lyra::runtime::ErasedValue(prototype),
               Read<RuntimeDynamicArray>(src)));
}

auto lyra_rt_dynarray_from_literal(
    void* prototype, LyraSpan unit, std::int64_t count, void* out) -> void* {
  RuntimeValue element_default = lyra::runtime::ErasedValue(prototype);
  std::vector<RuntimeValue> elements =
      lyra::runtime::ReplicateLiteral(element_default, unit, count);
  return Emplace(
      out,
      RuntimeDynamicArray(std::move(element_default), std::move(elements)));
}

auto lyra_rt_dynarray_from_array_unpackedarray(
    const void* source, void* prototype, void* out) -> void* {
  return Emplace(
      out, RuntimeDynamicArray::FromArray(
               RuntimeValue{Read<RuntimeUnpackedArray>(source)},
               lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_dynarray_from_array_queue(
    const void* source, void* prototype, void* out) -> void* {
  return Emplace(
      out, RuntimeDynamicArray::FromArray(
               RuntimeValue{Read<RuntimeQueue>(source)},
               lyra::runtime::ErasedValue(prototype)));
}

// Reads element `index`, copying it out across the opaque-handle boundary as a
// value of the element's own domain. An out-of-range index reads the element
// default (LRM 7.4.5).
auto lyra_rt_dynarray_element(const void* array, const void* index, void* out)
    -> void* {
  return lyra::runtime::ElementInto(
      out, Read<RuntimeDynamicArray>(array).Element(Read<PackedArray>(index)));
}

auto lyra_rt_dynarray_concat_element(const void* array, void* item, void* out)
    -> void* {
  const auto& source = Read<RuntimeDynamicArray>(array);
  return Emplace(
      out, source.ConcatElement(
               lyra::runtime::ElementFrom(source.ElementDefault(), item)));
}

auto lyra_rt_dynarray_concat_spread(
    const void* array, const void* part, void* out) -> void* {
  return Emplace(
      out,
      Read<RuntimeDynamicArray>(array).ConcatSpread(Read<RuntimeValue>(part)));
}

// The functional element write (LRM 7.4.6): yields a new array with element
// `index` replaced. The incoming value is a handle of the element domain, boxed
// into the erased representation by the domain the element default names.
auto lyra_rt_dynarray_with_element(
    const void* array, const void* index, void* value, void* out) -> void* {
  const auto& source = Read<RuntimeDynamicArray>(array);
  return Emplace(
      out, source.WithElement(
               Read<PackedArray>(index),
               lyra::runtime::ElementFrom(source.ElementDefault(), value)));
}

auto lyra_rt_dynarray_delete(const void* array, void* out) -> void* {
  return Emplace(out, Read<RuntimeDynamicArray>(array).Delete());
}

auto lyra_rt_dynarray_slice(
    const void* array, const void* start, std::int64_t count, void* out)
    -> void* {
  return Emplace(
      out,
      Read<RuntimeDynamicArray>(array).Slice(Read<PackedArray>(start), count));
}

auto lyra_rt_dynarray_with_slice(
    const void* array, const void* start, std::int64_t count,
    const void* replacement, void* out) -> void* {
  return Emplace(
      out, Read<RuntimeDynamicArray>(array).WithSlice(
               Read<PackedArray>(start), count,
               Read<RuntimeUnpackedArray>(replacement)));
}

auto lyra_rt_dynarray_size(const void* array, void* out) -> void* {
  return Emplace(out, Read<RuntimeDynamicArray>(array).Size());
}

auto lyra_rt_dynarray_eq(const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(
      out, Read<RuntimeDynamicArray>(lhs) == Read<RuntimeDynamicArray>(rhs));
}

auto lyra_rt_dynarray_ne(const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(
      out, Read<RuntimeDynamicArray>(lhs) != Read<RuntimeDynamicArray>(rhs));
}

auto lyra_rt_dynarray_case_equal(const void* lhs, const void* rhs, void* out)
    -> void* {
  return Emplace(
      out,
      Read<RuntimeDynamicArray>(lhs).CaseEqual(Read<RuntimeDynamicArray>(rhs)));
}

auto lyra_rt_dynarray_cell_get(void* cell, void* out) -> void* {
  return Emplace(out, static_cast<Var<RuntimeDynamicArray>*>(cell)->Get());
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

auto lyra_rt_dynarray_value_cell_load(const void* cell, void* out) noexcept
    -> void* {
  return Emplace(
      out, static_cast<const ActivationValueCell<RuntimeDynamicArray>*>(cell)
               ->Get());
}

auto lyra_rt_unpackedarray_from_literal(
    void* prototype, LyraSpan unit, std::int64_t count, void* out) -> void* {
  RuntimeValue element_default = lyra::runtime::ErasedValue(prototype);
  std::vector<RuntimeValue> unit_elements =
      lyra::runtime::ReplicateLiteral(element_default, unit, 1);
  return Emplace(
      out, RuntimeUnpackedArray(
               std::move(element_default), std::move(unit_elements),
               static_cast<std::size_t>(count)));
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
  std::vector<RuntimeValue> elements;
  elements.reserve(static_cast<std::size_t>(size));
  for (std::int64_t i = 0; i < size; ++i) {
    elements.push_back(source.ElementAt(static_cast<std::size_t>(i)));
  }
  return Emplace(
      out, RuntimeUnpackedArray(
               source.ElementDefault(), std::move(elements),
               static_cast<std::size_t>(1)));
}

auto lyra_rt_unpackedarray_from_array_dynarray(
    const void* source, void* prototype, std::int64_t declared, void* out)
    -> void* {
  return Emplace(
      out, RuntimeUnpackedArray::FromArray(
               RuntimeValue{Read<RuntimeDynamicArray>(source)},
               lyra::runtime::ErasedValue(prototype), declared));
}

auto lyra_rt_unpackedarray_from_array_queue(
    const void* source, void* prototype, std::int64_t declared, void* out)
    -> void* {
  return Emplace(
      out, RuntimeUnpackedArray::FromArray(
               RuntimeValue{Read<RuntimeQueue>(source)},
               lyra::runtime::ErasedValue(prototype), declared));
}

auto lyra_rt_unpackedarray_merge_conditional(
    const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(
      out, Read<RuntimeUnpackedArray>(lhs).MergeConditional(
               Read<RuntimeUnpackedArray>(rhs)));
}

// Reads the element a position names. A position that names no element reads
// the element default (LRM 7.4.5).
auto lyra_rt_unpackedarray_element(
    const void* array, const void* position, void* out) -> void* {
  return lyra::runtime::ElementInto(
      out,
      Read<RuntimeUnpackedArray>(array).Element(Read<PackedArray>(position)));
}

// The functional element write (LRM 7.4.5): yields a new array with the named
// element replaced, and the original unchanged when the position names none.
auto lyra_rt_unpackedarray_with_element(
    const void* array, const void* position, void* value, void* out) -> void* {
  const auto& source = Read<RuntimeUnpackedArray>(array);
  return Emplace(
      out, source.WithElement(
               Read<PackedArray>(position),
               lyra::runtime::ElementFrom(source.ElementDefault(), value)));
}

auto lyra_rt_packed_from_string(const void* text, const void* type, void* out)
    -> void* {
  return Emplace(
      out, PackedArray::FromString(Read<String>(text), Read<PackedType>(type)));
}

auto lyra_rt_unpackedarray_from_string(
    const void* text, const void* element_type, const void* count, void* out)
    -> void* {
  return Emplace(
      out, RuntimeUnpackedArray::FromString(
               Read<String>(text), Read<PackedType>(element_type),
               Read<PackedArray>(count)));
}

auto lyra_rt_unpackedarray_from_packed_array(
    const void* bits, const void* element_type, const void* count, void* out)
    -> void* {
  return Emplace(
      out, RuntimeUnpackedArray::FromPackedArray(
               Read<PackedArray>(bits), Read<PackedType>(element_type),
               Read<PackedArray>(count)));
}

auto lyra_rt_unpackedarray_count_bits(
    const void* value, const void* control_bits, void* out) -> void* {
  return Emplace(
      out, Read<RuntimeUnpackedArray>(value).CountBits(
               Read<PackedArray>(control_bits)));
}

auto lyra_rt_tuple_count_bits(
    const void* value, const void* control_bits, void* out) -> void* {
  return Emplace(
      out,
      Read<RuntimeTuple>(value).CountBits(Read<PackedArray>(control_bits)));
}

auto lyra_rt_dynarray_count_bits(
    const void* value, const void* control_bits, void* out) -> void* {
  return Emplace(
      out, Read<RuntimeDynamicArray>(value).CountBits(
               Read<PackedArray>(control_bits)));
}

auto lyra_rt_string_count_bits(
    const void* value, const void* control_bits, void* out) -> void* {
  return Emplace(
      out, Read<String>(value).CountBits(Read<PackedArray>(control_bits)));
}

auto lyra_rt_string_bitstream_width(const void* value, void* out) -> void* {
  return Emplace(out, Read<String>(value).BitstreamWidth());
}

auto lyra_rt_tuple_bitstream_width(const void* value, void* out) -> void* {
  return Emplace(out, Read<RuntimeTuple>(value).BitstreamWidth());
}

auto lyra_rt_dynarray_bitstream_width(const void* value, void* out) -> void* {
  return Emplace(out, Read<RuntimeDynamicArray>(value).BitstreamWidth());
}

auto lyra_rt_unpackedarray_bitstream_width(const void* value, void* out)
    -> void* {
  return Emplace(out, Read<RuntimeUnpackedArray>(value).BitstreamWidth());
}

auto lyra_rt_packed_to_bitstream(const void* value, void* out) -> void* {
  return Emplace(out, Read<PackedArray>(value).ToBitstream());
}

auto lyra_rt_tuple_to_bitstream(const void* value, void* out) -> void* {
  return Emplace(out, Read<RuntimeTuple>(value).ToBitstream());
}

auto lyra_rt_unpackedarray_to_bitstream(const void* value, void* out) -> void* {
  return Emplace(out, Read<RuntimeUnpackedArray>(value).ToBitstream());
}

auto lyra_rt_packed_from_bitstream(const void* bits, void* prototype, void* out)
    -> void* {
  const lyra::value::RuntimeValue shape = lyra::runtime::ErasedValue(prototype);
  return Emplace(
      out, PackedArray::FromBitstream(
               Read<PackedArray>(bits), std::get<PackedArray>(shape.value)));
}

auto lyra_rt_tuple_from_bitstream(const void* bits, void* prototype, void* out)
    -> void* {
  const lyra::value::RuntimeValue shape = lyra::runtime::ErasedValue(prototype);
  return Emplace(
      out, RuntimeTuple::FromBitstream(
               Read<PackedArray>(bits), std::get<RuntimeTuple>(shape.value)));
}

auto lyra_rt_unpackedarray_from_bitstream(
    const void* bits, void* prototype, void* out) -> void* {
  const lyra::value::RuntimeValue shape = lyra::runtime::ErasedValue(prototype);
  return Emplace(
      out, RuntimeUnpackedArray::FromBitstream(
               Read<PackedArray>(bits),
               std::get<RuntimeUnpackedArray>(shape.value)));
}

auto lyra_rt_packed_reverse_blocks(
    const void* value, std::int64_t block, void* out) -> void* {
  return Emplace(out, Read<PackedArray>(value).ReverseBlocks(block));
}

auto lyra_rt_unpackedarray_size(const void* array, void* out) -> void* {
  return Emplace(out, Read<RuntimeUnpackedArray>(array).Size());
}

auto lyra_rt_unpackedarray_slice(
    const void* array, const void* start, std::int64_t count, void* out)
    -> void* {
  return Emplace(
      out,
      Read<RuntimeUnpackedArray>(array).Slice(Read<PackedArray>(start), count));
}

auto lyra_rt_unpackedarray_with_slice(
    const void* array, const void* start, std::int64_t count,
    const void* replacement, void* out) -> void* {
  return Emplace(
      out, Read<RuntimeUnpackedArray>(array).WithSlice(
               Read<PackedArray>(start), count,
               Read<RuntimeUnpackedArray>(replacement)));
}

auto lyra_rt_unpackedarray_eq(const void* lhs, const void* rhs, void* out)
    -> void* {
  return Emplace(
      out, Read<RuntimeUnpackedArray>(lhs) == Read<RuntimeUnpackedArray>(rhs));
}

auto lyra_rt_unpackedarray_ne(const void* lhs, const void* rhs, void* out)
    -> void* {
  return Emplace(
      out, Read<RuntimeUnpackedArray>(lhs) != Read<RuntimeUnpackedArray>(rhs));
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

auto lyra_rt_unpackedarray_cell_get(void* cell, void* out) -> void* {
  return Emplace(out, static_cast<Var<RuntimeUnpackedArray>*>(cell)->Get());
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

auto lyra_rt_packed_net_get(void* net, void* out) -> void* {
  return Emplace(out, NetOf<PackedArray>(net).Get());
}

void lyra_rt_packed_net_initialize_tri_state(
    void* net, const void* prototype, const void* fill, const void* strength) {
  NetOf<PackedArray>(net).InitializeTriState(
      Read<PackedArray>(prototype), Read<PackedArray>(fill),
      Read<PackedArray>(strength));
}

void lyra_rt_packed_net_initialize_wired_and(
    void* net, const void* prototype, const void* fill, const void* strength) {
  NetOf<PackedArray>(net).InitializeWiredAnd(
      Read<PackedArray>(prototype), Read<PackedArray>(fill),
      Read<PackedArray>(strength));
}

void lyra_rt_packed_net_initialize_wired_or(
    void* net, const void* prototype, const void* fill, const void* strength) {
  NetOf<PackedArray>(net).InitializeWiredOr(
      Read<PackedArray>(prototype), Read<PackedArray>(fill),
      Read<PackedArray>(strength));
}

void lyra_rt_packed_net_initialize_retaining(
    void* net, const void* prototype, const void* fill, const void* strength) {
  NetOf<PackedArray>(net).InitializeRetaining(
      Read<PackedArray>(prototype), Read<PackedArray>(fill),
      Read<PackedArray>(strength));
}

auto lyra_rt_packed_net_begin_takeover(void* net, const void* level, void* out)
    -> void* {
  return Emplace(
      out, NetOf<PackedArray>(net).BeginTakeover(Read<PackedArray>(level)));
}

auto lyra_rt_packed_net_drive_takeover(
    void* net, const void* level, const void* generation, const void* value)
    -> bool {
  return NetOf<PackedArray>(net).DriveTakeover(
      Read<PackedArray>(level), Read<PackedArray>(generation),
      Read<PackedArray>(value));
}

void lyra_rt_packed_net_end_takeover(void* net, const void* level) {
  NetOf<PackedArray>(net).EndTakeover(Read<PackedArray>(level));
}

auto lyra_rt_packed_attach_driver(void* net, const void* strength) -> void* {
  return &NetOf<PackedArray>(net).AttachDriver(Read<PackedArray>(strength));
}

void lyra_rt_packed_net_join(
    void* net, void* other, const void* here, const void* there,
    const void* width) {
  NetOf<PackedArray>(net).Join(
      &NetOf<PackedArray>(other), Read<PackedArray>(here),
      Read<PackedArray>(there), Read<PackedArray>(width));
}

auto lyra_rt_packed_driver_get(void* driver, void* out) -> void* {
  return Emplace(out, DriverOf<PackedArray>(driver).Get());
}

void lyra_rt_packed_driver_set(void* driver, const void* value) {
  DriverOf<PackedArray>(driver).Set(Read<PackedArray>(value));
}

auto lyra_rt_tuple_net_get(void* net, void* out) -> void* {
  return Emplace(out, NetOf<RuntimeTuple>(net).Get());
}

void lyra_rt_tuple_net_initialize_tri_state(
    void* net, const void* prototype, const void* fill, const void* strength) {
  NetOf<RuntimeTuple>(net).InitializeTriState(
      Read<RuntimeTuple>(prototype), Read<PackedArray>(fill),
      Read<PackedArray>(strength));
}

void lyra_rt_tuple_net_initialize_wired_and(
    void* net, const void* prototype, const void* fill, const void* strength) {
  NetOf<RuntimeTuple>(net).InitializeWiredAnd(
      Read<RuntimeTuple>(prototype), Read<PackedArray>(fill),
      Read<PackedArray>(strength));
}

void lyra_rt_tuple_net_initialize_wired_or(
    void* net, const void* prototype, const void* fill, const void* strength) {
  NetOf<RuntimeTuple>(net).InitializeWiredOr(
      Read<RuntimeTuple>(prototype), Read<PackedArray>(fill),
      Read<PackedArray>(strength));
}

void lyra_rt_tuple_net_initialize_retaining(
    void* net, const void* prototype, const void* fill, const void* strength) {
  NetOf<RuntimeTuple>(net).InitializeRetaining(
      Read<RuntimeTuple>(prototype), Read<PackedArray>(fill),
      Read<PackedArray>(strength));
}

auto lyra_rt_tuple_attach_driver(void* net, const void* strength) -> void* {
  return &NetOf<RuntimeTuple>(net).AttachDriver(Read<PackedArray>(strength));
}

void lyra_rt_tuple_net_join(
    void* net, void* other, const void* here, const void* there,
    const void* width) {
  NetOf<RuntimeTuple>(net).Join(
      &NetOf<RuntimeTuple>(other), Read<PackedArray>(here),
      Read<PackedArray>(there), Read<PackedArray>(width));
}

auto lyra_rt_tuple_driver_get(void* driver, void* out) -> void* {
  return Emplace(out, DriverOf<RuntimeTuple>(driver).Get());
}

void lyra_rt_tuple_driver_set(void* driver, const void* value) {
  DriverOf<RuntimeTuple>(driver).Set(Read<RuntimeTuple>(value));
}

auto lyra_rt_union_net_get(void* net, void* out) -> void* {
  return Emplace(out, NetOf<RuntimeUnion>(net).Get());
}

void lyra_rt_union_net_initialize_tri_state(
    void* net, const void* prototype, const void* fill, const void* strength) {
  NetOf<RuntimeUnion>(net).InitializeTriState(
      Read<RuntimeUnion>(prototype), Read<PackedArray>(fill),
      Read<PackedArray>(strength));
}

void lyra_rt_union_net_initialize_wired_and(
    void* net, const void* prototype, const void* fill, const void* strength) {
  NetOf<RuntimeUnion>(net).InitializeWiredAnd(
      Read<RuntimeUnion>(prototype), Read<PackedArray>(fill),
      Read<PackedArray>(strength));
}

void lyra_rt_union_net_initialize_wired_or(
    void* net, const void* prototype, const void* fill, const void* strength) {
  NetOf<RuntimeUnion>(net).InitializeWiredOr(
      Read<RuntimeUnion>(prototype), Read<PackedArray>(fill),
      Read<PackedArray>(strength));
}

void lyra_rt_union_net_initialize_retaining(
    void* net, const void* prototype, const void* fill, const void* strength) {
  NetOf<RuntimeUnion>(net).InitializeRetaining(
      Read<RuntimeUnion>(prototype), Read<PackedArray>(fill),
      Read<PackedArray>(strength));
}

auto lyra_rt_union_attach_driver(void* net, const void* strength) -> void* {
  return &NetOf<RuntimeUnion>(net).AttachDriver(Read<PackedArray>(strength));
}

void lyra_rt_union_net_join(
    void* net, void* other, const void* here, const void* there,
    const void* width) {
  NetOf<RuntimeUnion>(net).Join(
      &NetOf<RuntimeUnion>(other), Read<PackedArray>(here),
      Read<PackedArray>(there), Read<PackedArray>(width));
}

auto lyra_rt_union_driver_get(void* driver, void* out) -> void* {
  return Emplace(out, DriverOf<RuntimeUnion>(driver).Get());
}

void lyra_rt_union_driver_set(void* driver, const void* value) {
  DriverOf<RuntimeUnion>(driver).Set(Read<RuntimeUnion>(value));
}

auto lyra_rt_unpackedarray_net_get(void* net, void* out) -> void* {
  return Emplace(out, NetOf<RuntimeUnpackedArray>(net).Get());
}

void lyra_rt_unpackedarray_net_initialize_tri_state(
    void* net, const void* prototype, const void* fill, const void* strength) {
  NetOf<RuntimeUnpackedArray>(net).InitializeTriState(
      Read<RuntimeUnpackedArray>(prototype), Read<PackedArray>(fill),
      Read<PackedArray>(strength));
}

void lyra_rt_unpackedarray_net_initialize_wired_and(
    void* net, const void* prototype, const void* fill, const void* strength) {
  NetOf<RuntimeUnpackedArray>(net).InitializeWiredAnd(
      Read<RuntimeUnpackedArray>(prototype), Read<PackedArray>(fill),
      Read<PackedArray>(strength));
}

void lyra_rt_unpackedarray_net_initialize_wired_or(
    void* net, const void* prototype, const void* fill, const void* strength) {
  NetOf<RuntimeUnpackedArray>(net).InitializeWiredOr(
      Read<RuntimeUnpackedArray>(prototype), Read<PackedArray>(fill),
      Read<PackedArray>(strength));
}

void lyra_rt_unpackedarray_net_initialize_retaining(
    void* net, const void* prototype, const void* fill, const void* strength) {
  NetOf<RuntimeUnpackedArray>(net).InitializeRetaining(
      Read<RuntimeUnpackedArray>(prototype), Read<PackedArray>(fill),
      Read<PackedArray>(strength));
}

auto lyra_rt_unpackedarray_attach_driver(void* net, const void* strength)
    -> void* {
  return &NetOf<RuntimeUnpackedArray>(net).AttachDriver(
      Read<PackedArray>(strength));
}

void lyra_rt_unpackedarray_net_join(
    void* net, void* other, const void* here, const void* there,
    const void* width) {
  NetOf<RuntimeUnpackedArray>(net).Join(
      &NetOf<RuntimeUnpackedArray>(other), Read<PackedArray>(here),
      Read<PackedArray>(there), Read<PackedArray>(width));
}

auto lyra_rt_unpackedarray_driver_get(void* driver, void* out) -> void* {
  return Emplace(out, DriverOf<RuntimeUnpackedArray>(driver).Get());
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

auto lyra_rt_unpackedarray_value_cell_load(const void* cell, void* out) noexcept
    -> void* {
  return Emplace(
      out, static_cast<const ActivationValueCell<RuntimeUnpackedArray>*>(cell)
               ->Get());
}

auto lyra_rt_queue_from_literal(
    void* prototype, LyraSpan unit, std::int64_t count, void* out) -> void* {
  RuntimeValue element_default = lyra::runtime::ErasedValue(prototype);
  std::vector<RuntimeValue> elements =
      lyra::runtime::ReplicateLiteral(element_default, unit, count);
  return Emplace(
      out, RuntimeQueue(std::move(element_default), std::move(elements)));
}

auto lyra_rt_queue_from_literal_bounded(
    void* prototype, LyraSpan unit, std::int64_t count, const void* max_bound,
    void* out) -> void* {
  RuntimeValue element_default = lyra::runtime::ErasedValue(prototype);
  std::vector<RuntimeValue> elements =
      lyra::runtime::ReplicateLiteral(element_default, unit, count);
  return Emplace(
      out, RuntimeQueue(
               std::move(element_default), std::move(elements),
               Read<PackedArray>(max_bound)));
}

auto lyra_rt_queue_conform_bound(
    const void* queue, const void* max_bound, void* out) -> void* {
  return Emplace(
      out,
      Read<RuntimeQueue>(queue).ConformBound(Read<PackedArray>(max_bound)));
}

auto lyra_rt_queue_from_array_unpackedarray(
    const void* source, void* prototype, const void* max_bound, void* out)
    -> void* {
  return Emplace(
      out,
      RuntimeQueue::FromArray(
          RuntimeValue{Read<RuntimeUnpackedArray>(source)},
          lyra::runtime::ErasedValue(prototype), Read<PackedArray>(max_bound)));
}

auto lyra_rt_queue_from_array_dynarray(
    const void* source, void* prototype, const void* max_bound, void* out)
    -> void* {
  return Emplace(
      out,
      RuntimeQueue::FromArray(
          RuntimeValue{Read<RuntimeDynamicArray>(source)},
          lyra::runtime::ErasedValue(prototype), Read<PackedArray>(max_bound)));
}

auto lyra_rt_queue_element(const void* queue, const void* index, void* out)
    -> void* {
  return lyra::runtime::ElementInto(
      out, Read<RuntimeQueue>(queue).Element(Read<PackedArray>(index)));
}

auto lyra_rt_queue_with_element(
    const void* queue, const void* index, void* value, void* out) -> void* {
  const auto& source = Read<RuntimeQueue>(queue);
  return Emplace(
      out, source.WithElement(
               Read<PackedArray>(index),
               lyra::runtime::ElementFrom(source.ElementDefault(), value)));
}

auto lyra_rt_queue_slice(
    const void* queue, const void* lo, const void* hi, void* out) -> void* {
  return Emplace(
      out, Read<RuntimeQueue>(queue).Slice(
               Read<PackedArray>(lo), Read<PackedArray>(hi)));
}

auto lyra_rt_queue_size(const void* queue, void* out) -> void* {
  return Emplace(out, Read<RuntimeQueue>(queue).Size());
}

auto lyra_rt_queue_push_back(const void* queue, void* item, void* out)
    -> void* {
  const auto& source = Read<RuntimeQueue>(queue);
  return Emplace(
      out, source.PushBack(
               lyra::runtime::ElementFrom(source.ElementDefault(), item)));
}

auto lyra_rt_queue_push_front(const void* queue, void* item, void* out)
    -> void* {
  const auto& source = Read<RuntimeQueue>(queue);
  return Emplace(
      out, source.PushFront(
               lyra::runtime::ElementFrom(source.ElementDefault(), item)));
}

auto lyra_rt_queue_concat_element(const void* queue, void* item, void* out)
    -> void* {
  const auto& source = Read<RuntimeQueue>(queue);
  return Emplace(
      out, source.PushBack(
               lyra::runtime::ElementFrom(source.ElementDefault(), item)));
}

auto lyra_rt_queue_concat_spread(const void* queue, const void* part, void* out)
    -> void* {
  return Emplace(
      out, Read<RuntimeQueue>(queue).ConcatSpread(Read<RuntimeValue>(part)));
}

auto lyra_rt_queue_insert(
    const void* queue, const void* index, void* item, void* out) -> void* {
  const auto& source = Read<RuntimeQueue>(queue);
  return Emplace(
      out, source.Insert(
               Read<PackedArray>(index),
               lyra::runtime::ElementFrom(source.ElementDefault(), item)));
}

auto lyra_rt_queue_pop_front(const void* queue, void* out) -> void* {
  const auto& source = Read<RuntimeQueue>(queue);
  return lyra::runtime::EmplacePopped(out, source.PopFront(), source.Front());
}

auto lyra_rt_queue_pop_back(const void* queue, void* out) -> void* {
  const auto& source = Read<RuntimeQueue>(queue);
  return lyra::runtime::EmplacePopped(out, source.PopBack(), source.Back());
}

auto lyra_rt_queue_delete(const void* queue, void* out) -> void* {
  return Emplace(out, Read<RuntimeQueue>(queue).Delete());
}

auto lyra_rt_queue_delete_index(const void* queue, const void* index, void* out)
    -> void* {
  return Emplace(
      out, Read<RuntimeQueue>(queue).DeleteIndex(Read<PackedArray>(index)));
}

auto lyra_rt_queue_eq(const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(out, Read<RuntimeQueue>(lhs) == Read<RuntimeQueue>(rhs));
}

auto lyra_rt_queue_ne(const void* lhs, const void* rhs, void* out) -> void* {
  return Emplace(out, Read<RuntimeQueue>(lhs) != Read<RuntimeQueue>(rhs));
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
    const void* queue, const void* control_bits, void* out) -> void* {
  return Emplace(
      out,
      Read<RuntimeQueue>(queue).CountBits(Read<PackedArray>(control_bits)));
}

auto lyra_rt_queue_value_box(const void* value, void* out) -> void* {
  return Emplace(out, RuntimeValue{Read<RuntimeQueue>(value)});
}

auto lyra_rt_queue_cell_get(void* cell, void* out) -> void* {
  return Emplace(out, static_cast<Var<RuntimeQueue>*>(cell)->Get());
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

auto lyra_rt_queue_value_cell_load(const void* cell, void* out) noexcept
    -> void* {
  return Emplace(
      out, static_cast<const ActivationValueCell<RuntimeQueue>*>(cell)->Get());
}

// LRM 7.9.11 `'{index: value, ...}`: each entry crosses as the product of the
// index and the element it stores. A product already holds its components
// erased, which is the form a keyed container needs both of them in: it knows
// the representation of neither in advance.
auto lyra_rt_assocarray_from_entries_default(
    void* prototype, LyraSpan entries, void* user_default, void* out) -> void* {
  RuntimeValue element_default = lyra::runtime::ErasedValue(prototype);
  RuntimeValue miss = lyra::runtime::ElementFrom(element_default, user_default);
  return Emplace(
      out, lyra::runtime::SeedAssociativeEntries(
               RuntimeAssociativeArray(
                   AssociativeIndexOrder::kIndexValueDomain,
                   std::move(element_default), std::move(miss)),
               entries));
}

auto lyra_rt_assocarray_from_entries_default_wildcard(
    void* prototype, LyraSpan entries, void* user_default, void* out) -> void* {
  RuntimeValue element_default = lyra::runtime::ErasedValue(prototype);
  RuntimeValue miss = lyra::runtime::ElementFrom(element_default, user_default);
  return Emplace(
      out, lyra::runtime::SeedAssociativeEntries(
               RuntimeAssociativeArray(
                   AssociativeIndexOrder::kWildcardNumeric,
                   std::move(element_default), std::move(miss)),
               entries));
}

auto lyra_rt_assocarray_element(const void* array, const void* index, void* out)
    -> void* {
  return lyra::runtime::ElementInto(
      out,
      Read<RuntimeAssociativeArray>(array).Element(Read<RuntimeValue>(index)));
}

auto lyra_rt_assocarray_with_element(
    const void* array, const void* index, void* value, void* out) -> void* {
  const auto& source = Read<RuntimeAssociativeArray>(array);
  return Emplace(
      out, source.WithElement(
               Read<RuntimeValue>(index),
               lyra::runtime::ElementFrom(source.ElementDefault(), value)));
}

auto lyra_rt_assocarray_exists(const void* array, const void* index, void* out)
    -> void* {
  return Emplace(
      out,
      Read<RuntimeAssociativeArray>(array).Exists(Read<RuntimeValue>(index)));
}

auto lyra_rt_assocarray_size(const void* array, void* out) -> void* {
  return Emplace(out, Read<RuntimeAssociativeArray>(array).Size());
}

auto lyra_rt_assocarray_delete(const void* array, void* out) -> void* {
  return Emplace(out, Read<RuntimeAssociativeArray>(array).Delete());
}

auto lyra_rt_assocarray_delete_index(
    const void* array, const void* index, void* out) -> void* {
  return Emplace(
      out, Read<RuntimeAssociativeArray>(array).DeleteIndex(
               Read<RuntimeValue>(index)));
}

auto lyra_rt_assocarray_eq(const void* lhs, const void* rhs, void* out)
    -> void* {
  return Emplace(
      out,
      Read<RuntimeAssociativeArray>(lhs) == Read<RuntimeAssociativeArray>(rhs));
}

auto lyra_rt_assocarray_ne(const void* lhs, const void* rhs, void* out)
    -> void* {
  return Emplace(
      out,
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
    const void* array, void* unallocated, void* out) -> void* {
  return lyra::runtime::ElementInto(
      out, Read<RuntimeAssociativeArray>(array).MinIndex(
               lyra::runtime::ErasedValue(unallocated)));
}

auto lyra_rt_assocarray_assoc_max_index(
    const void* array, void* unallocated, void* out) -> void* {
  return lyra::runtime::ElementInto(
      out, Read<RuntimeAssociativeArray>(array).MaxIndex(
               lyra::runtime::ErasedValue(unallocated)));
}

auto lyra_rt_assocarray_assoc_first(const void* array, void* probe, void* out)
    -> void* {
  return lyra::runtime::EmplaceVisited(
      out, Read<RuntimeAssociativeArray>(array).FirstIndex(), probe);
}

auto lyra_rt_assocarray_assoc_last(const void* array, void* probe, void* out)
    -> void* {
  return lyra::runtime::EmplaceVisited(
      out, Read<RuntimeAssociativeArray>(array).LastIndex(), probe);
}

auto lyra_rt_assocarray_assoc_next(const void* array, void* probe, void* out)
    -> void* {
  return lyra::runtime::EmplaceVisited(
      out,
      Read<RuntimeAssociativeArray>(array).NextIndex(Read<RuntimeValue>(probe)),
      probe);
}

auto lyra_rt_assocarray_assoc_prev(const void* array, void* probe, void* out)
    -> void* {
  return lyra::runtime::EmplaceVisited(
      out,
      Read<RuntimeAssociativeArray>(array).PrevIndex(Read<RuntimeValue>(probe)),
      probe);
}

auto lyra_rt_assocarray_count_bits(
    const void* array, const void* control_bits, void* out) -> void* {
  return Emplace(
      out, Read<RuntimeAssociativeArray>(array).CountBits(
               Read<PackedArray>(control_bits)));
}

auto lyra_rt_assocarray_value_box(const void* value, void* out) -> void* {
  return Emplace(out, RuntimeValue{Read<RuntimeAssociativeArray>(value)});
}

auto lyra_rt_assocarray_cell_get(void* cell, void* out) -> void* {
  return Emplace(out, static_cast<Var<RuntimeAssociativeArray>*>(cell)->Get());
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

auto lyra_rt_assocarray_value_cell_load(const void* cell, void* out) noexcept
    -> void* {
  return Emplace(
      out,
      static_cast<const ActivationValueCell<RuntimeAssociativeArray>*>(cell)
          ->Get());
}

auto lyra_rt_unpackedarray_sum(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return lyra::runtime::ElementInto(
      out,
      lyra::value::RuntimeArraySum(
          Read<RuntimeUnpackedArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_unpackedarray_product(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return lyra::runtime::ElementInto(
      out,
      lyra::value::RuntimeArrayProduct(
          Read<RuntimeUnpackedArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_unpackedarray_and(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return lyra::runtime::ElementInto(
      out,
      lyra::value::RuntimeArrayAnd(
          Read<RuntimeUnpackedArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_unpackedarray_or(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return lyra::runtime::ElementInto(
      out,
      lyra::value::RuntimeArrayOr(
          Read<RuntimeUnpackedArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_unpackedarray_xor(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return lyra::runtime::ElementInto(
      out,
      lyra::value::RuntimeArrayXor(
          Read<RuntimeUnpackedArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_unpackedarray_find(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return Emplace(
      out,
      lyra::value::RuntimeArrayFind(
          Read<RuntimeUnpackedArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_unpackedarray_find_index(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return Emplace(
      out,
      lyra::value::RuntimeArrayFindIndex(
          Read<RuntimeUnpackedArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_unpackedarray_find_first(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return Emplace(
      out,
      lyra::value::RuntimeArrayFindFirst(
          Read<RuntimeUnpackedArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_unpackedarray_find_first_index(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return Emplace(
      out,
      lyra::value::RuntimeArrayFindFirstIndex(
          Read<RuntimeUnpackedArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_unpackedarray_find_last(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return Emplace(
      out,
      lyra::value::RuntimeArrayFindLast(
          Read<RuntimeUnpackedArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_unpackedarray_find_last_index(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return Emplace(
      out,
      lyra::value::RuntimeArrayFindLastIndex(
          Read<RuntimeUnpackedArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_unpackedarray_min(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return Emplace(
      out,
      lyra::value::RuntimeArrayMin(
          Read<RuntimeUnpackedArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_unpackedarray_max(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return Emplace(
      out,
      lyra::value::RuntimeArrayMax(
          Read<RuntimeUnpackedArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_unpackedarray_unique(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return Emplace(
      out,
      lyra::value::RuntimeArrayUnique(
          Read<RuntimeUnpackedArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_unpackedarray_unique_index(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return Emplace(
      out,
      lyra::value::RuntimeArrayUniqueIndex(
          Read<RuntimeUnpackedArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_unpackedarray_map(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return Emplace(
      out,
      lyra::value::RuntimeArrayMap(
          Read<RuntimeUnpackedArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_dynarray_sum(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return lyra::runtime::ElementInto(
      out,
      lyra::value::RuntimeArraySum(
          Read<RuntimeDynamicArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_dynarray_product(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return lyra::runtime::ElementInto(
      out,
      lyra::value::RuntimeArrayProduct(
          Read<RuntimeDynamicArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_dynarray_and(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return lyra::runtime::ElementInto(
      out,
      lyra::value::RuntimeArrayAnd(
          Read<RuntimeDynamicArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_dynarray_or(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return lyra::runtime::ElementInto(
      out,
      lyra::value::RuntimeArrayOr(
          Read<RuntimeDynamicArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_dynarray_xor(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return lyra::runtime::ElementInto(
      out,
      lyra::value::RuntimeArrayXor(
          Read<RuntimeDynamicArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_dynarray_find(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return Emplace(
      out,
      lyra::value::RuntimeArrayFind(
          Read<RuntimeDynamicArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_dynarray_find_index(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return Emplace(
      out,
      lyra::value::RuntimeArrayFindIndex(
          Read<RuntimeDynamicArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_dynarray_find_first(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return Emplace(
      out,
      lyra::value::RuntimeArrayFindFirst(
          Read<RuntimeDynamicArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_dynarray_find_first_index(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return Emplace(
      out,
      lyra::value::RuntimeArrayFindFirstIndex(
          Read<RuntimeDynamicArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_dynarray_find_last(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return Emplace(
      out,
      lyra::value::RuntimeArrayFindLast(
          Read<RuntimeDynamicArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_dynarray_find_last_index(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return Emplace(
      out,
      lyra::value::RuntimeArrayFindLastIndex(
          Read<RuntimeDynamicArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_dynarray_min(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return Emplace(
      out,
      lyra::value::RuntimeArrayMin(
          Read<RuntimeDynamicArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_dynarray_max(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return Emplace(
      out,
      lyra::value::RuntimeArrayMax(
          Read<RuntimeDynamicArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_dynarray_unique(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return Emplace(
      out,
      lyra::value::RuntimeArrayUnique(
          Read<RuntimeDynamicArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_dynarray_unique_index(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return Emplace(
      out,
      lyra::value::RuntimeArrayUniqueIndex(
          Read<RuntimeDynamicArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_dynarray_map(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return Emplace(
      out,
      lyra::value::RuntimeArrayMap(
          Read<RuntimeDynamicArray>(receiver), lyra::runtime::ArrayBody(body),
          lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_queue_sum(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return lyra::runtime::ElementInto(
      out, lyra::value::RuntimeArraySum(
               Read<RuntimeQueue>(receiver), lyra::runtime::ArrayBody(body),
               lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_queue_product(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return lyra::runtime::ElementInto(
      out, lyra::value::RuntimeArrayProduct(
               Read<RuntimeQueue>(receiver), lyra::runtime::ArrayBody(body),
               lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_queue_and(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return lyra::runtime::ElementInto(
      out, lyra::value::RuntimeArrayAnd(
               Read<RuntimeQueue>(receiver), lyra::runtime::ArrayBody(body),
               lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_queue_or(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return lyra::runtime::ElementInto(
      out, lyra::value::RuntimeArrayOr(
               Read<RuntimeQueue>(receiver), lyra::runtime::ArrayBody(body),
               lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_queue_xor(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return lyra::runtime::ElementInto(
      out, lyra::value::RuntimeArrayXor(
               Read<RuntimeQueue>(receiver), lyra::runtime::ArrayBody(body),
               lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_queue_find(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return Emplace(
      out, lyra::value::RuntimeArrayFind(
               Read<RuntimeQueue>(receiver), lyra::runtime::ArrayBody(body),
               lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_queue_find_index(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return Emplace(
      out, lyra::value::RuntimeArrayFindIndex(
               Read<RuntimeQueue>(receiver), lyra::runtime::ArrayBody(body),
               lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_queue_find_first(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return Emplace(
      out, lyra::value::RuntimeArrayFindFirst(
               Read<RuntimeQueue>(receiver), lyra::runtime::ArrayBody(body),
               lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_queue_find_first_index(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return Emplace(
      out, lyra::value::RuntimeArrayFindFirstIndex(
               Read<RuntimeQueue>(receiver), lyra::runtime::ArrayBody(body),
               lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_queue_find_last(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return Emplace(
      out, lyra::value::RuntimeArrayFindLast(
               Read<RuntimeQueue>(receiver), lyra::runtime::ArrayBody(body),
               lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_queue_find_last_index(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return Emplace(
      out, lyra::value::RuntimeArrayFindLastIndex(
               Read<RuntimeQueue>(receiver), lyra::runtime::ArrayBody(body),
               lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_queue_min(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return Emplace(
      out, lyra::value::RuntimeArrayMin(
               Read<RuntimeQueue>(receiver), lyra::runtime::ArrayBody(body),
               lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_queue_max(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return Emplace(
      out, lyra::value::RuntimeArrayMax(
               Read<RuntimeQueue>(receiver), lyra::runtime::ArrayBody(body),
               lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_queue_unique(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return Emplace(
      out, lyra::value::RuntimeArrayUnique(
               Read<RuntimeQueue>(receiver), lyra::runtime::ArrayBody(body),
               lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_queue_unique_index(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return Emplace(
      out, lyra::value::RuntimeArrayUniqueIndex(
               Read<RuntimeQueue>(receiver), lyra::runtime::ArrayBody(body),
               lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_queue_map(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return Emplace(
      out, lyra::value::RuntimeArrayMap(
               Read<RuntimeQueue>(receiver), lyra::runtime::ArrayBody(body),
               lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_assocarray_sum(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return lyra::runtime::ElementInto(
      out, lyra::value::RuntimeArraySum(
               Read<RuntimeAssociativeArray>(receiver),
               lyra::runtime::ArrayBody(body),
               lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_assocarray_product(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return lyra::runtime::ElementInto(
      out, lyra::value::RuntimeArrayProduct(
               Read<RuntimeAssociativeArray>(receiver),
               lyra::runtime::ArrayBody(body),
               lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_assocarray_and(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return lyra::runtime::ElementInto(
      out, lyra::value::RuntimeArrayAnd(
               Read<RuntimeAssociativeArray>(receiver),
               lyra::runtime::ArrayBody(body),
               lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_assocarray_or(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return lyra::runtime::ElementInto(
      out, lyra::value::RuntimeArrayOr(
               Read<RuntimeAssociativeArray>(receiver),
               lyra::runtime::ArrayBody(body),
               lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_assocarray_xor(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return lyra::runtime::ElementInto(
      out, lyra::value::RuntimeArrayXor(
               Read<RuntimeAssociativeArray>(receiver),
               lyra::runtime::ArrayBody(body),
               lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_assocarray_find(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return Emplace(
      out, lyra::value::RuntimeArrayFind(
               Read<RuntimeAssociativeArray>(receiver),
               lyra::runtime::ArrayBody(body),
               lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_assocarray_find_index(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return Emplace(
      out, lyra::value::RuntimeArrayFindIndex(
               Read<RuntimeAssociativeArray>(receiver),
               lyra::runtime::ArrayBody(body),
               lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_assocarray_find_first(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return Emplace(
      out, lyra::value::RuntimeArrayFindFirst(
               Read<RuntimeAssociativeArray>(receiver),
               lyra::runtime::ArrayBody(body),
               lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_assocarray_find_first_index(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return Emplace(
      out, lyra::value::RuntimeArrayFindFirstIndex(
               Read<RuntimeAssociativeArray>(receiver),
               lyra::runtime::ArrayBody(body),
               lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_assocarray_find_last(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return Emplace(
      out, lyra::value::RuntimeArrayFindLast(
               Read<RuntimeAssociativeArray>(receiver),
               lyra::runtime::ArrayBody(body),
               lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_assocarray_find_last_index(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return Emplace(
      out, lyra::value::RuntimeArrayFindLastIndex(
               Read<RuntimeAssociativeArray>(receiver),
               lyra::runtime::ArrayBody(body),
               lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_assocarray_min(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return Emplace(
      out, lyra::value::RuntimeArrayMin(
               Read<RuntimeAssociativeArray>(receiver),
               lyra::runtime::ArrayBody(body),
               lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_assocarray_max(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return Emplace(
      out, lyra::value::RuntimeArrayMax(
               Read<RuntimeAssociativeArray>(receiver),
               lyra::runtime::ArrayBody(body),
               lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_assocarray_unique(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return Emplace(
      out, lyra::value::RuntimeArrayUnique(
               Read<RuntimeAssociativeArray>(receiver),
               lyra::runtime::ArrayBody(body),
               lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_assocarray_unique_index(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return Emplace(
      out, lyra::value::RuntimeArrayUniqueIndex(
               Read<RuntimeAssociativeArray>(receiver),
               lyra::runtime::ArrayBody(body),
               lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_assocarray_map(
    const void* receiver, void* body, void* prototype, void* out) -> void* {
  return Emplace(
      out, lyra::value::RuntimeArrayMap(
               Read<RuntimeAssociativeArray>(receiver),
               lyra::runtime::ArrayBody(body),
               lyra::runtime::ErasedValue(prototype)));
}

auto lyra_rt_unpackedarray_sort(const void* receiver, void* body, void* out)
    -> void* {
  return Emplace(
      out, lyra::value::RuntimeArraySort(
               Read<RuntimeUnpackedArray>(receiver),
               lyra::runtime::ArrayBody(body)));
}

auto lyra_rt_unpackedarray_rsort(const void* receiver, void* body, void* out)
    -> void* {
  return Emplace(
      out, lyra::value::RuntimeArrayRsort(
               Read<RuntimeUnpackedArray>(receiver),
               lyra::runtime::ArrayBody(body)));
}

auto lyra_rt_dynarray_sort(const void* receiver, void* body, void* out)
    -> void* {
  return Emplace(
      out,
      lyra::value::RuntimeArraySort(
          Read<RuntimeDynamicArray>(receiver), lyra::runtime::ArrayBody(body)));
}

auto lyra_rt_dynarray_rsort(const void* receiver, void* body, void* out)
    -> void* {
  return Emplace(
      out,
      lyra::value::RuntimeArrayRsort(
          Read<RuntimeDynamicArray>(receiver), lyra::runtime::ArrayBody(body)));
}

auto lyra_rt_queue_sort(const void* receiver, void* body, void* out) -> void* {
  return Emplace(
      out, lyra::value::RuntimeArraySort(
               Read<RuntimeQueue>(receiver), lyra::runtime::ArrayBody(body)));
}

auto lyra_rt_queue_rsort(const void* receiver, void* body, void* out) -> void* {
  return Emplace(
      out, lyra::value::RuntimeArrayRsort(
               Read<RuntimeQueue>(receiver), lyra::runtime::ArrayBody(body)));
}

auto lyra_rt_unpackedarray_reverse(const void* receiver, void* out) -> void* {
  return Emplace(
      out,
      lyra::value::RuntimeArrayReverse(Read<RuntimeUnpackedArray>(receiver)));
}

auto lyra_rt_dynarray_reverse(const void* receiver, void* out) -> void* {
  return Emplace(
      out,
      lyra::value::RuntimeArrayReverse(Read<RuntimeDynamicArray>(receiver)));
}

auto lyra_rt_queue_reverse(const void* receiver, void* out) -> void* {
  return Emplace(
      out, lyra::value::RuntimeArrayReverse(Read<RuntimeQueue>(receiver)));
}

auto lyra_rt_unpackedarray_read_mem(
    void* runtime, const void* memory, const void* name, LyraSpan dims,
    const void* base, const void* start, void* out) -> void* {
  return lyra::runtime::EmplaceCompletion(
      out, std::vector<RuntimeValue>{RuntimeValue{lyra::runtime::ReadMem(
               *static_cast<RuntimeEffects*>(runtime),
               Read<RuntimeUnpackedArray>(memory), Read<String>(name),
               ValuesOf<UnpackedRange>(dims), Read<PackedArray>(base),
               Read<PackedArray>(start), std::nullopt)}});
}

auto lyra_rt_unpackedarray_read_mem_within(
    void* runtime, const void* memory, const void* name, LyraSpan dims,
    const void* base, const void* start, const void* finish, void* out)
    -> void* {
  return lyra::runtime::EmplaceCompletion(
      out,
      std::vector<RuntimeValue>{RuntimeValue{lyra::runtime::ReadMem(
          *static_cast<RuntimeEffects*>(runtime),
          Read<RuntimeUnpackedArray>(memory), Read<String>(name),
          ValuesOf<UnpackedRange>(dims), Read<PackedArray>(base),
          Read<PackedArray>(start), Read<PackedArray>(finish).ToInt64())}});
}

void lyra_rt_unpackedarray_write_mem(
    void* runtime, const void* memory, const void* name, LyraSpan dims,
    const void* base, const void* start) {
  lyra::runtime::WriteMem(
      *static_cast<RuntimeEffects*>(runtime),
      Read<RuntimeUnpackedArray>(memory), Read<String>(name),
      ValuesOf<UnpackedRange>(dims), Read<PackedArray>(base),
      Read<PackedArray>(start), std::nullopt);
}

void lyra_rt_unpackedarray_write_mem_within(
    void* runtime, const void* memory, const void* name, LyraSpan dims,
    const void* base, const void* start, const void* finish) {
  lyra::runtime::WriteMem(
      *static_cast<RuntimeEffects*>(runtime),
      Read<RuntimeUnpackedArray>(memory), Read<String>(name),
      ValuesOf<UnpackedRange>(dims), Read<PackedArray>(base),
      Read<PackedArray>(start), Read<PackedArray>(finish).ToInt64());
}

auto lyra_rt_dynarray_read_mem(
    void* runtime, const void* memory, const void* name, const void* base,
    const void* start, void* out) -> void* {
  return lyra::runtime::EmplaceCompletion(
      out,
      std::vector<RuntimeValue>{RuntimeValue{lyra::runtime::ReadMem(
          *static_cast<RuntimeEffects*>(runtime),
          Read<RuntimeDynamicArray>(memory), Read<String>(name),
          Read<PackedArray>(base), Read<PackedArray>(start), std::nullopt)}});
}

auto lyra_rt_dynarray_read_mem_within(
    void* runtime, const void* memory, const void* name, const void* base,
    const void* start, const void* finish, void* out) -> void* {
  return lyra::runtime::EmplaceCompletion(
      out, std::vector<RuntimeValue>{RuntimeValue{lyra::runtime::ReadMem(
               *static_cast<RuntimeEffects*>(runtime),
               Read<RuntimeDynamicArray>(memory), Read<String>(name),
               Read<PackedArray>(base), Read<PackedArray>(start),
               Read<PackedArray>(finish).ToInt64())}});
}

void lyra_rt_dynarray_write_mem(
    void* runtime, const void* memory, const void* name, const void* base,
    const void* start) {
  lyra::runtime::WriteMem(
      *static_cast<RuntimeEffects*>(runtime), Read<RuntimeDynamicArray>(memory),
      Read<String>(name), Read<PackedArray>(base), Read<PackedArray>(start),
      std::nullopt);
}

void lyra_rt_dynarray_write_mem_within(
    void* runtime, const void* memory, const void* name, const void* base,
    const void* start, const void* finish) {
  lyra::runtime::WriteMem(
      *static_cast<RuntimeEffects*>(runtime), Read<RuntimeDynamicArray>(memory),
      Read<String>(name), Read<PackedArray>(base), Read<PackedArray>(start),
      Read<PackedArray>(finish).ToInt64());
}

auto lyra_rt_queue_read_mem(
    void* runtime, const void* memory, const void* name, const void* base,
    const void* start, void* out) -> void* {
  return lyra::runtime::EmplaceCompletion(
      out,
      std::vector<RuntimeValue>{RuntimeValue{lyra::runtime::ReadMem(
          *static_cast<RuntimeEffects*>(runtime), Read<RuntimeQueue>(memory),
          Read<String>(name), Read<PackedArray>(base), Read<PackedArray>(start),
          std::nullopt)}});
}

auto lyra_rt_queue_read_mem_within(
    void* runtime, const void* memory, const void* name, const void* base,
    const void* start, const void* finish, void* out) -> void* {
  return lyra::runtime::EmplaceCompletion(
      out,
      std::vector<RuntimeValue>{RuntimeValue{lyra::runtime::ReadMem(
          *static_cast<RuntimeEffects*>(runtime), Read<RuntimeQueue>(memory),
          Read<String>(name), Read<PackedArray>(base), Read<PackedArray>(start),
          Read<PackedArray>(finish).ToInt64())}});
}

void lyra_rt_queue_write_mem(
    void* runtime, const void* memory, const void* name, const void* base,
    const void* start) {
  lyra::runtime::WriteMem(
      *static_cast<RuntimeEffects*>(runtime), Read<RuntimeQueue>(memory),
      Read<String>(name), Read<PackedArray>(base), Read<PackedArray>(start),
      std::nullopt);
}

void lyra_rt_queue_write_mem_within(
    void* runtime, const void* memory, const void* name, const void* base,
    const void* start, const void* finish) {
  lyra::runtime::WriteMem(
      *static_cast<RuntimeEffects*>(runtime), Read<RuntimeQueue>(memory),
      Read<String>(name), Read<PackedArray>(base), Read<PackedArray>(start),
      Read<PackedArray>(finish).ToInt64());
}

auto lyra_rt_assocarray_read_mem(
    void* runtime, const void* memory, const void* name,
    const void* key_prototype, const void* base, const void* start, void* out)
    -> void* {
  return lyra::runtime::EmplaceCompletion(
      out, std::vector<RuntimeValue>{RuntimeValue{lyra::runtime::ReadMem(
               *static_cast<RuntimeEffects*>(runtime),
               Read<RuntimeAssociativeArray>(memory), Read<String>(name),
               Read<PackedArray>(key_prototype), Read<PackedArray>(base),
               Read<PackedArray>(start), std::nullopt)}});
}

auto lyra_rt_assocarray_read_mem_within(
    void* runtime, const void* memory, const void* name,
    const void* key_prototype, const void* base, const void* start,
    const void* finish, void* out) -> void* {
  return lyra::runtime::EmplaceCompletion(
      out,
      std::vector<RuntimeValue>{RuntimeValue{lyra::runtime::ReadMem(
          *static_cast<RuntimeEffects*>(runtime),
          Read<RuntimeAssociativeArray>(memory), Read<String>(name),
          Read<PackedArray>(key_prototype), Read<PackedArray>(base),
          Read<PackedArray>(start), Read<PackedArray>(finish).ToInt64())}});
}

void lyra_rt_assocarray_write_mem(
    void* runtime, const void* memory, const void* name, const void* base,
    const void* start) {
  lyra::runtime::WriteMem(
      *static_cast<RuntimeEffects*>(runtime),
      Read<RuntimeAssociativeArray>(memory), Read<String>(name),
      Read<PackedArray>(base), Read<PackedArray>(start), std::nullopt);
}

void lyra_rt_assocarray_write_mem_within(
    void* runtime, const void* memory, const void* name, const void* base,
    const void* start, const void* finish) {
  lyra::runtime::WriteMem(
      *static_cast<RuntimeEffects*>(runtime),
      Read<RuntimeAssociativeArray>(memory), Read<String>(name),
      Read<PackedArray>(base), Read<PackedArray>(start),
      Read<PackedArray>(finish).ToInt64());
}

auto lyra_rt_format_runtime(
    const void* format, LyraSpan args, const void* scope_path,
    const void* time_format, const void* timeunit_power, void* out) -> void* {
  const std::span<const void* const> handles{
      static_cast<const void* const*>(args.data), args.count};
  std::vector<FormatArg> arguments(handles.size());
  std::ranges::transform(handles, arguments.begin(), [](const void* handle) {
    return *static_cast<const FormatArg*>(handle);
  });
  return Emplace(
      out,
      lyra::value::FormatRuntime(
          Read<String>(format), arguments, Read<String>(scope_path),
          Read<TimeFormat>(time_format), Read<PackedArray>(timeunit_power)));
}

auto lyra_rt_packed_make_format_arg(const void* value, void* out) -> void* {
  return Emplace(out, MakeFormatArg(Read<PackedArray>(value)));
}

auto lyra_rt_string_make_format_arg(const void* value, void* out) -> void* {
  return Emplace(out, MakeFormatArg(Read<String>(value)));
}

auto lyra_rt_packed_make_format_arg_with_pattern(
    const void* value, const void* pattern, void* out) -> void* {
  return Emplace(
      out, FormatArg(Read<PackedArray>(value), Read<String>(pattern)));
}

auto lyra_rt_make_rendered_format_arg(const void* pattern, void* out) -> void* {
  return Emplace(out, FormatArg::Rendered(Read<String>(pattern)));
}

auto lyra_rt_chandle_make_format_arg(const void* value, void* out) -> void* {
  return Emplace(out, MakeFormatArg(Read<Chandle>(value)));
}

auto lyra_rt_managedref_make_format_arg(const void* value, void* out) -> void* {
  return Emplace(out, MakeFormatArg(Read<ManagedRef>(value)));
}

auto lyra_rt_make_dpi_bit_buffer(const void* sv, void* out) -> void* {
  return Emplace(out, DpiBitBuffer(Read<PackedArray>(sv)));
}

auto lyra_rt_make_dpi_logic_buffer(const void* sv, void* out) -> void* {
  return Emplace(out, DpiLogicBuffer(Read<PackedArray>(sv)));
}

auto lyra_rt_dpi_bit_buffer_data(void* buffer) -> void* {
  return static_cast<DpiBitBuffer*>(buffer)->Data();
}

auto lyra_rt_dpi_logic_buffer_data(void* buffer) -> void* {
  return static_cast<DpiLogicBuffer*>(buffer)->Data();
}

auto lyra_rt_read_canonical_bit_vec(
    const void* src, const void* type, void* out) -> void* {
  return Emplace(
      out, lyra::value::ReadCanonicalBitVec(
               static_cast<const svBitVecVal*>(src), Read<PackedType>(type)));
}

auto lyra_rt_read_canonical_logic_vec(
    const void* src, const void* type, void* out) -> void* {
  return Emplace(
      out, lyra::value::ReadCanonicalLogicVec(
               static_cast<const svLogicVecVal*>(src), Read<PackedType>(type)));
}

void lyra_rt_write_canonical_bit_vec(void* dst, const void* sv) {
  lyra::value::WriteCanonicalBitVec(
      static_cast<svBitVecVal*>(dst), Read<PackedArray>(sv));
}

void lyra_rt_write_canonical_logic_vec(void* dst, const void* sv) {
  lyra::value::WriteCanonicalLogicVec(
      static_cast<svLogicVecVal*>(dst), Read<PackedArray>(sv));
}

auto lyra_rt_to_sv_logic(const void* sv) -> std::uint8_t {
  return lyra::value::ToSvLogic(Read<PackedArray>(sv));
}

auto lyra_rt_from_sv_logic(std::uint8_t encoded, const void* type, void* out)
    -> void* {
  return Emplace(
      out, lyra::value::FromSvLogic(encoded, Read<PackedType>(type)));
}

// The image takes the actual erased, because it is element-type-independent
// (Annex H.7.3) and nothing here could read that representation off anything
// else. What the image does need of the actual's declaration is the shape of
// one element, which arrives as its own operand rather than being read back
// off an element the actual may not hold.
auto lyra_rt_make_dpi_open_array(
    void* sv, LyraSpan bounds, const void* element_type,
    bool addressable_elements, void* out) -> void* {
  return Emplace(
      out, DpiOpenArray(
               lyra::runtime::ErasedValue(sv), ValuesOf<UnpackedRange>(bounds),
               Read<PackedType>(element_type), addressable_elements));
}

auto lyra_rt_dpi_open_array_handle(void* image) -> void* {
  return static_cast<DpiOpenArray*>(image)->Handle();
}

auto lyra_rt_dpi_open_array_value(const void* image, void* prototype, void* out)
    -> void* {
  return lyra::runtime::ElementInto(
      out, static_cast<const DpiOpenArray*>(image)->ToErasedValue(
               lyra::runtime::ErasedValue(prototype)));
}

// Ending an object the generated body held in its own storage, where ending one
// has something to do. An object whose type has a trivial destructor is ended
// by its storage going away, so it has no entry here.
void lyra_rt_packed_destroy(void* object) {
  std::destroy_at(static_cast<PackedArray*>(object));
}
void lyra_rt_string_destroy(void* object) {
  std::destroy_at(static_cast<String*>(object));
}
void lyra_rt_tuple_destroy(void* object) {
  std::destroy_at(static_cast<RuntimeTuple*>(object));
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
  std::destroy_at(static_cast<ManagedRef*>(object));
}
void lyra_rt_closure_destroy(void* object) {
  std::destroy_at(static_cast<ClosureValue*>(object));
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
void lyra_rt_erased_value_destroy(void* object) {
  std::destroy_at(static_cast<RuntimeValue*>(object));
}
void lyra_rt_promoted_scope_destroy(void* object) {
  std::destroy_at(static_cast<PromotedScopeRef*>(object));
}

// A second value equal to one the body already holds, built in further storage
// the body gave -- where a value it only reads has to become one it owns.
auto lyra_rt_packed_copy(const void* value, void* out) -> void* {
  return Emplace(out, Read<PackedArray>(value));
}
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
auto lyra_rt_tuple_copy(const void* value, void* out) -> void* {
  return Emplace(out, Read<RuntimeTuple>(value));
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
  return Emplace(out, Read<ManagedRef>(value));
}
auto lyra_rt_promoted_scope_copy(const void* value, void* out) -> void* {
  return Emplace(out, Read<PromotedScopeRef>(value));
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
auto lyra_rt_erased_value_copy(const void* value, void* out) -> void* {
  return Emplace(out, Read<RuntimeValue>(value));
}

// A value moved into storage that takes it over. What is left behind is still
// an object, which the body then ends.
auto lyra_rt_packed_move(void* value, void* out) -> void* {
  return Emplace(out, std::move(*static_cast<PackedArray*>(value)));
}
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
auto lyra_rt_tuple_move(void* value, void* out) -> void* {
  return Emplace(out, std::move(*static_cast<RuntimeTuple*>(value)));
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
  return Emplace(out, std::move(*static_cast<ManagedRef*>(value)));
}
auto lyra_rt_closure_move(void* value, void* out) -> void* {
  return Emplace(out, std::move(*static_cast<ClosureValue*>(value)));
}
auto lyra_rt_promoted_scope_move(void* value, void* out) -> void* {
  return Emplace(out, std::move(*static_cast<PromotedScopeRef*>(value)));
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
auto lyra_rt_erased_value_move(void* value, void* out) -> void* {
  return Emplace(out, std::move(*static_cast<RuntimeValue*>(value)));
}
}

namespace lyra::runtime {

namespace {

// The storage generated code gives an object is sized from what the two sides
// state about it, not from this side's types, so each type an entry builds is
// held to that statement here.
template <typename T>
constexpr auto LaidOutAs(support::RuntimeObject object) -> bool {
  const support::ObjectLayout layout = support::LayoutOf(object);
  return sizeof(T) == layout.size && alignof(T) == layout.align &&
         std::is_trivially_destructible_v<T> == layout.ends_with_nothing_to_do;
}

using support::LibraryObject;
using support::ValueDomain;
static_assert(LaidOutAs<value::PackedArray>(ValueDomain::kPacked));
static_assert(LaidOutAs<value::String>(ValueDomain::kString));
static_assert(LaidOutAs<value::Real>(ValueDomain::kReal));
static_assert(LaidOutAs<value::ShortReal>(ValueDomain::kShortReal));
static_assert(LaidOutAs<value::Chandle>(ValueDomain::kChandle));
static_assert(LaidOutAs<value::Empty>(ValueDomain::kEmpty));
static_assert(LaidOutAs<value::RuntimeTuple>(ValueDomain::kTuple));
static_assert(LaidOutAs<value::RuntimeUnion>(ValueDomain::kUnion));
static_assert(LaidOutAs<value::RuntimeTaggedUnion>(ValueDomain::kTaggedUnion));
static_assert(LaidOutAs<value::RuntimeDynamicArray>(ValueDomain::kDynArray));
static_assert(
    LaidOutAs<value::RuntimeUnpackedArray>(ValueDomain::kUnpackedArray));
static_assert(LaidOutAs<value::RuntimeQueue>(ValueDomain::kQueue));
static_assert(
    LaidOutAs<value::RuntimeAssociativeArray>(ValueDomain::kAssocArray));
static_assert(LaidOutAs<value::ManagedRef>(ValueDomain::kManagedRef));
static_assert(LaidOutAs<ClosureValue>(LibraryObject::kClosure));
static_assert(LaidOutAs<value::PrintItem>(LibraryObject::kPrintItem));
static_assert(LaidOutAs<value::FormatSpec>(LibraryObject::kFormatSpec));
static_assert(LaidOutAs<value::FormatArg>(LibraryObject::kFormatArg));
static_assert(LaidOutAs<HierarchySegment>(LibraryObject::kHierarchySegment));
static_assert(LaidOutAs<Trigger>(LibraryObject::kTrigger));
static_assert(LaidOutAs<Observation>(LibraryObject::kObservation));
static_assert(LaidOutAs<value::DpiBitBuffer>(LibraryObject::kDpiBitBuffer));
static_assert(LaidOutAs<value::DpiLogicBuffer>(LibraryObject::kDpiLogicBuffer));
static_assert(LaidOutAs<value::DpiOpenArray>(LibraryObject::kDpiOpenArray));
static_assert(
    LaidOutAs<ChannelCancellation>(LibraryObject::kChannelCancellation));
static_assert(LaidOutAs<value::RuntimeValue>(LibraryObject::kErasedValue));
static_assert(LaidOutAs<Coroutine<void>>(LibraryObject::kExecution));
static_assert(LaidOutAs<PromotedScopeRef>(LibraryObject::kPromotedScope));

}  // namespace

}  // namespace lyra::runtime
