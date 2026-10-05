#pragma once

#include <concepts>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <optional>
#include <span>
#include <type_traits>
#include <utility>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/base/time.hpp"
#include "lyra/runtime/object_change.hpp"
#include "lyra/runtime/object_ref.hpp"
#include "lyra/runtime/observable.hpp"
#include "lyra/runtime/registration.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/takeover.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/runtime/value_storage_core.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/formation.hpp"
#include "lyra/value/object_ref.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::runtime {

// What a write into part of an owner's storage writes through (LRM 11.5.1).
// The write reaches the owner's storage and lands its part there directly; the
// owner is told once, when the write is over, whether it changed. Four things
// that takes, named for the role rather than for either owner's vocabulary.
//
// `AdmitsWrite` is whether the write goes into the owner at all: one does not
// while a procedural continuous assignment holds a variable (LRM 10.6), and it
// then lands in a copy the write keeps and nobody reads. Asking is the first
// write of the time slot at the latest, so a sampled variable keeps its value
// from before it here (LRM 16.5.1).
//
// `MutationStorage` is the storage itself, so a write reaches the part it
// names and disturbs nothing else. Two things follow: a write cannot lose one
// performed through another while it was open, and what writing one element
// costs does not scale with the size of the whole value.
//
// `Watched` is whether anything reads the answer. A variable cell with nothing
// waiting on it has no one to tell (LRM 4.3), so the write keeps nothing from
// before it; a net driver always has its net, which resolves again only where
// the driver's own contribution moved (LRM 6.5).
//
// `PublishTransition` tells the owner the write changed it, with what can be
// said about which of its bits moved.
//
// Each sink claims the contract with a `static_assert(MutationSink<...>)`
// beside its own definition, the way a value type claims its `lyra::value`
// concepts; the bracket itself takes the sink unconstrained, because a sink
// names the handle in its own partial-write entry's return type and checking
// the constraint there would depend on the sink being complete.
template <class S>
concept MutationSink = requires(S sink, const ProjectionUnchanged& unchanged) {
  typename S::ValueType;
  { sink.AdmitsWrite() } -> std::same_as<bool>;
  { sink.MutationStorage() } -> std::same_as<typename S::ValueType&>;
  { sink.Watched() } -> std::same_as<bool>;
  sink.PublishTransition(unchanged);
};

template <class Sink>
class ScopedMutation;

template <value::LyraValue T>
class Ref;

// Reads one leaf's bits out of the values a change moved between, so a wait
// that reads only bits this change left alone is passed over. Defined in the
// library: a unit writing a packed cell reaches it, and no design shapes it.
[[nodiscard]] auto MakePackedProjectionTest(
    const value::PackedArray& old_val, const value::PackedArray& new_val)
    -> ProjectionUnchanged;

// What only a variable something samples or takes over has to do before a
// write lands in it: keep its value from before the time slot's first change
// (LRM 16.5.1), and turn the write away while a procedural continuous
// assignment holds it (LRM 10.6). Almost no variable is either, so this exists
// only once one is, behind the variable, and the variable asks it for nothing
// otherwise.
class RareWriteState {
 public:
  RareWriteState() = default;
  RareWriteState(const RareWriteState&) = delete;
  auto operator=(const RareWriteState&) -> RareWriteState& = delete;
  RareWriteState(RareWriteState&&) = delete;
  auto operator=(RareWriteState&&) -> RareWriteState& = delete;
  virtual ~RareWriteState();

  // Before a write lands: keeps the value the variable held when the slot
  // began, the first time in the slot, and answers whether the write lands.
  [[nodiscard]] virtual auto Admit() -> bool = 0;
  [[nodiscard]] virtual auto TakenOver() const -> bool = 0;
};

// What every variable is whatever it holds: the waits parked on it, and
// whatever a sampled or taken-over variable has to do before a write lands. A
// write that knows the variable only as this -- one through a reference to a
// part of it, which knows the part's type and not the variable's -- asks it
// everything it needs.
class VariableCell : public Observable {
 public:
  VariableCell(const VariableCell&) = delete;
  auto operator=(const VariableCell&) -> VariableCell& = delete;
  VariableCell(VariableCell&&) = delete;
  auto operator=(VariableCell&&) -> VariableCell& = delete;

  // Written here rather than in the library's own source because every write
  // asks them, and for a variable nothing samples or takes over each is a null
  // test.
  [[nodiscard]] auto AdmitsWrite() -> bool {
    return rare_ == nullptr || rare_->Admit();
  }
  // Whether a write into the variable has anyone to tell what it did: a wait
  // parked here (LRM 4.3). Under a procedural continuous assignment a write
  // lands where nothing reads it (LRM 10.6), so it has nothing to tell either.
  [[nodiscard]] auto Watched() const -> bool {
    return HasWaiter() && (rare_ == nullptr || !rare_->TakenOver());
  }

 protected:
  VariableCell();
  ~VariableCell();

  [[nodiscard]] auto Rare() const -> RareWriteState* {
    return rare_.get();
  }
  void InstallRare(std::unique_ptr<RareWriteState> rare) {
    rare_ = std::move(rare);
  }

 private:
  std::unique_ptr<RareWriteState> rare_;
};

// What a sampled or taken-over variable of type `T` keeps: the value it held
// when the current slot began, and the slot that value belongs to (LRM
// 16.5.1); and the procedural continuous assignments it has been put under
// (LRM 10.6). Each part is empty until its feature is used.
template <value::LyraValue T>
class CellRareState final : public RareWriteState {
 public:
  explicit CellRareState(const T& value) : value_(&value) {
  }
  CellRareState(const CellRareState&) = delete;
  auto operator=(const CellRareState&) -> CellRareState& = delete;
  CellRareState(CellRareState&&) = delete;
  auto operator=(CellRareState&&) -> CellRareState& = delete;
  ~CellRareState() override;

  auto Admit() -> bool override {
    KeepPreponed();
    return !TakenOver();
  }
  [[nodiscard]] auto TakenOver() const -> bool override {
    return takeovers_ != nullptr && takeovers_->Highest() != nullptr;
  }

  // Arms the variable to answer for its sampled value, taking what it holds
  // now, stamped with time zero.
  void ArmSampling() {
    if (retained_.has_value()) {
      return;
    }
    retained_ = *value_;
    retained_slot_ = SimTime{};
  }

  [[nodiscard]] auto Retained() const -> const std::optional<T>& {
    return retained_;
  }
  [[nodiscard]] auto RetainedSlot() const -> SimTime {
    return retained_slot_;
  }

  // Keeps what the variable held in the Preponed region of the current slot,
  // the first time a write reaches it in that slot (LRM 4.4.2.1, 16.5.1).
  // Nothing in the slot has changed it before its first write, so what it
  // holds then is that value, whether or not the write goes on to change it.
  void KeepPreponed() {
    if (!retained_.has_value()) {
      return;
    }
    const SimTime now = current_runtime().Now();
    if (retained_slot_ == now) {
      return;
    }
    retained_ = *value_;
    retained_slot_ = now;
  }

  [[nodiscard]] auto Takeovers() -> runtime::Takeovers<T>& {
    if (takeovers_ == nullptr) {
      takeovers_ = std::make_unique<runtime::Takeovers<T>>();
    }
    return *takeovers_;
  }
  [[nodiscard]] auto ExistingTakeovers() const -> runtime::Takeovers<T>* {
    return takeovers_.get();
  }

 private:
  const T* value_;
  std::optional<T> retained_;
  SimTime retained_slot_ = SimTime{};
  std::unique_ptr<runtime::Takeovers<T>> takeovers_;
};

template <value::LyraValue T>
CellRareState<T>::~CellRareState() = default;

template <value::LyraValue T>
class Var : public VariableCell, public ValueStorageCore<T> {
 public:
  Var();
  Var(const Var&) = delete;
  auto operator=(const Var&) -> Var& = delete;
  Var(Var&&) = delete;
  auto operator=(Var&&) -> Var& = delete;
  ~Var();

  // The write a declaration makes. The first one installs the cell's
  // representation and contents from `prototype`, a value of the declared
  // type; a later one -- a declaration reached again, which begins a fresh
  // variable in the one storage -- overwrites at that representation. What is
  // fixed is the representation, not the number of times a declaration runs,
  // so a prototype that does not match the installed one is the lowering
  // defect and is what refuses. A store before any of this is one too, and the
  // store path is what refuses it.
  void Initialize(T prototype) {
    if (!this->IsInstalled()) {
      this->Install(std::move(prototype));
      return;
    }
    this->Overwrite(prototype);
  }

  // Commits a whole-variable write and, on a real change (LRM 4.3 update
  // event), wakes whoever waits through the engine. The engine is the ambient
  // one: it has the standing of a stack pointer, so a store does not carry it.
  //
  // A procedural continuous assignment overrides the procedural writes a cell
  // takes on its own (LRM 10.6), so while one is in effect the value arriving
  // here is discarded outright rather than held anywhere: after the takeover
  // ends the cell keeps what the takeover last gave it, and never the write
  // that was overridden.
  void Set(const T& new_val);

  // Starts a procedural continuous assignment at `level`, superseding whatever
  // was driving that level, and answers with the generation its evaluation
  // carries (LRM 10.6).
  auto BeginTakeover(const value::PackedArray& level) -> value::PackedArray;

  // States what the takeover at `level` has evaluated to. It reaches the cell
  // only when no higher level covers it, and is recorded either way so that
  // ending the level above it needs nobody to recompute. Answers whether the
  // evaluation offering the value is still the one driving that level, which
  // is how an evaluation superseded by a later takeover, or ended by a
  // `deassign` or `release`, learns to stop.
  auto DriveTakeover(
      const value::PackedArray& level, const value::PackedArray& generation,
      const T& new_val) -> bool;

  // Ends the procedural continuous assignment at `level`, handing the cell to
  // the highest level still in effect. Where none is left the cell is left
  // exactly as it stands, which is what a released variable keeps (LRM
  // 10.6.2).
  void EndTakeover(const value::PackedArray& level);

  // Arms the cell to answer for its sampled value (LRM 16.5.1) and installs the
  // one every read answers with until the first change of some later slot.
  //
  // A static variable's default sampled value is the value its declaration
  // assigns (LRM 16.5.1), which is in the cell once time-zero initialization
  // has run (LRM 10.5), so arming takes what it finds there. Stamping it at
  // time zero is what makes the whole of time zero answer with it: a write then
  // finds the slot already current and leaves the retained value alone.
  //
  // More than one read may name one cell, and they all want the same answer, so
  // arming an armed cell is not an error and changes nothing.
  void ArmSampling() {
    RareState().ArmSampling();
  }

  // What the cell held in the Preponed region of the current time slot -- its
  // value before anything in that slot ran (LRM 4.4.2.1, 16.5.1). Once the slot
  // has changed the cell that value is the retained one; until then the cell
  // still holds it.
  [[nodiscard]] auto SampledGet() const -> const T& {
    const CellRareState<T>* rare = ExistingRareState();
    if (rare == nullptr || !rare->Retained().has_value()) {
      throw InternalError(
          "Var::SampledGet: a sampled value was read from a cell nothing armed "
          "to answer for one");
    }
    if (rare->RetainedSlot() == current_runtime().Now()) {
      return *rare->Retained();
    }
    return this->Get();
  }

  // Tells whoever waits here that a write changed the cell (LRM 4.3), passing
  // over a wait whose bits `unchanged` shows the write left alone.
  void PublishTransition(const ProjectionUnchanged& unchanged) {
    current_runtime().WakeWaitersOf(*this, unchanged);
  }

  // Opens a write into the cell for the full-expression that writes. The cell's
  // own storage is designated within it, the steps into parts are taken from
  // there, and the write lands where they are dereferenced, so a part is
  // written in the cell as it lies and the cell is told once whether it
  // changed. The write is non-copyable and non-movable, so keeping it past the
  // statement is rejected at compile time.
  auto Mutate() -> ScopedMutation<Ref<T>>;

 private:
  // The one path a whole value reaches the cell's storage by, whoever sent it.
  // What it adds over a partial write is the representation match, which only
  // a whole value can be checked for -- a partial write lands a part and never
  // restates the whole. The value arriving is compared with the one it would
  // replace before anything is written, so telling whoever waits keeps no copy
  // of the old value, except the old bits a packed value's waits are passed
  // over by.
  void Store(const T& new_val) {
    if constexpr (std::same_as<T, value::PackedArray>) {
      if (!this->IsInstalled()) {
        throw InternalError(
            "Var<PackedArray>: store into a cell that was never initialized");
      }
    }
    if (CellRareState<T>* rare = ExistingRareState()) {
      rare->KeepPreponed();
    }
    if (!this->HasWaiter()) {
      this->Overwrite(new_val);
      return;
    }
    if (this->Get().IsBitIdentical(new_val)) {
      return;
    }
    if constexpr (std::same_as<T, value::PackedArray>) {
      const T before = this->Get();
      this->Overwrite(new_val);
      PublishTransition(MakePackedProjectionTest(before, this->Get()));
    } else {
      this->Overwrite(new_val);
      PublishTransition(MakeWholeValueProjectionTest());
    }
  }

  // Whether a procedural continuous assignment shows through the cell (LRM
  // 10.6).
  [[nodiscard]] auto TakenOver() const -> bool {
    const CellRareState<T>* rare = ExistingRareState();
    return rare != nullptr && rare->TakenOver();
  }

  // What only a sampled or taken-over cell keeps, made the first time either
  // is asked for. This cell is the only thing that makes it, so it is always
  // of the cell's own type.
  [[nodiscard]] auto RareState() -> CellRareState<T>& {
    if (Rare() == nullptr) {
      InstallRare(std::make_unique<CellRareState<T>>(this->Get()));
    }
    return static_cast<CellRareState<T>&>(*Rare());
  }
  [[nodiscard]] auto ExistingRareState() const -> CellRareState<T>* {
    return static_cast<CellRareState<T>*>(Rare());
  }
};

// Which bits of a part landed on a change between the two values left alone:
// a packed value's by position, and nothing that can be shown of any other.
template <class Part>
auto LandedChange(const Part& before, const Part& after)
    -> ProjectionUnchanged {
  if constexpr (std::same_as<Part, value::PackedArray>) {
    return MakePackedProjectionTest(before, after);
  } else {
    return MakeWholeValueProjectionTest();
  }
}

// A packed part is what no design shapes, so it is compiled once, in the
// library.
extern template auto LandedChange<value::PackedArray>(
    const value::PackedArray& before, const value::PackedArray& after)
    -> ProjectionUnchanged;

// Writes `value` into storage a reference names, at the representation the
// storage already has; the first write into a packed value nothing has written
// yet is the one that gives it one, as a local's declaration does.
template <value::LyraValue T>
void StoreInto(T& storage, const T& value) {
  if constexpr (std::same_as<T, value::PackedArray>) {
    if (!storage.IsUninitialized() && !storage.SameRepresentation(value)) {
      throw InternalError(
          "StoreInto: a value's representation does not match the storage a "
          "reference names; a required conversion was not emitted");
    }
  }
  storage = value;
}

// What holds the storage a reference names, which is what a write through the
// reference is a write of (LRM 13.5.2): a variable something may wait on, the
// object a class property belongs to (LRM 9.4.2), or nothing anyone is told
// about -- a variable of a function, which nothing can wait on (LRM 13.4.4), a
// closure's own copy of a variable, which no other process reaches, and an
// element an index named none of.
using StorageHolder = std::variant<std::monostate, VariableCell*, GcObject*>;

// A reference in a form that names no type: where a value lies, and what holds
// that storage. What a reference does that does not depend on the type of the
// value it names is done here, once, for every reference whichever side holds
// it.
struct ErasedReference {
  StorageHolder holder;
  void* storage = nullptr;
  // Whether the storage is the whole of a variable, which is when the
  // reference stands for the variable itself. Only where the reference is
  // formed can say so: a part can lie at the address its whole does.
  bool whole = false;

  // Written here rather than in the library's own source because every write
  // through a reference asks them, and for storage whose variable nothing
  // samples or takes over each is a test or two.

  // Whether a write through the reference now lands where it names. Only a
  // variable can be put under a procedural continuous assignment (LRM 10.6).
  [[nodiscard]] auto Admits() const -> bool {
    VariableCell* const* variable = std::get_if<VariableCell*>(&holder);
    return variable == nullptr || (*variable)->AdmitsWrite();
  }

  [[nodiscard]] auto Watched() const -> bool {
    if (VariableCell* const* variable = std::get_if<VariableCell*>(&holder)) {
      return (*variable)->Watched();
    }
    GcObject* const* object = std::get_if<GcObject*>(&holder);
    return object != nullptr && (*object)->Watched();
  }

  // The rest are defined in the library's own source: none depends on the type
  // of the value the reference names, and none is on every write's path.

  // Tells whoever waits on what holds the storage that a write through the
  // reference changed it (LRM 4.3). A wait on an object reevaluates the
  // expression that reached it, which decides whether the write was an event,
  // so it is told nothing about which bits moved.
  void Report(const ProjectionUnchanged& unchanged) const;

  // A step can form what it reaches, which is a write into the variable, so
  // the variable admits it first.
  void AdmitStep() const;

  // What a wait on the storage registers on: what `Report` tells, which is the
  // variable or the object's event source. Storage that belongs to nothing
  // answers with none, since no write to it is ever told.
  [[nodiscard]] auto ReportsTo() const -> Observable*;

  // The reference to a part of what this one names, at `part`, which forming
  // did `formed` to. The part has the same holder, which is told at once where
  // forming it changed the storage -- an associative entry made by being bound
  // (LRM 7.8.7). An index naming no element names storage that belongs to
  // nothing (LRM 7.4.6).
  [[nodiscard]] auto Part(void* part, value::Formation formed) const
      -> ErasedReference;
};

// The variable a reference to the whole of it names, whose value is then of
// the reference's own type `T`.
template <value::LyraValue T>
auto WholeVariable(const ErasedReference& reference) -> Var<T>& {
  return static_cast<Var<T>&>(**std::get_if<VariableCell*>(&reference.holder));
}

// A reference, typed by the value it names. A reference to the whole of a
// variable and one to a part of it are the same thing, so a write through
// either is the variable's write, told to whoever waits on the variable at the
// moment it lands (LRM 4.3). Copyable, so a ref formal can be forwarded as a
// ref argument to a nested call.
template <value::LyraValue T>
class Ref {
 public:
  using ValueType = T;

  // A null view, default-constructed as a member and bound before first use:
  // a `ref` port's child-side member is declared with the child and filled by
  // the parent during elaboration (LRM 23.3.3.2), before simulation reads it.
  Ref() = default;
  explicit Ref(Var<T>& cell)
      : erased_{
            .holder = static_cast<VariableCell*>(&cell),
            .storage = &cell.Storage(),
            .whole = true} {
  }
  explicit Ref(T& storage) : erased_{.holder = {}, .storage = &storage} {
  }
  explicit Ref(const ErasedReference& erased) : erased_(erased) {
  }

  [[nodiscard]] auto Erased() const -> const ErasedReference& {
    return erased_;
  }

  [[nodiscard]] auto Get() const -> const T& {
    return Storage();
  }

  // Const: a `Ref` is a view, so `Set` writes the referenced storage, not the
  // handle's own pointers -- as `*p = v` is allowed through a `T* const p`.
  void Set(const T& new_val) const {
    if (!erased_.Admits()) {
      return;
    }
    if (!erased_.Watched()) {
      StoreInto(Storage(), new_val);
      return;
    }
    if (Storage().IsBitIdentical(new_val)) {
      return;
    }
    const T before = Storage();
    StoreInto(Storage(), new_val);
    erased_.Report(LandedChange(before, Storage()));
  }

  // Opens a write into the referenced storage, as an observable cell itself
  // does; only storage that belongs to a variable has anyone to tell what the
  // write did.
  [[nodiscard]] auto Mutate() const -> ScopedMutation<Ref<T>>;

  // A reference to an element or a component of what this one names (LRM
  // 13.5.2), which belongs to the same variable.
  template <typename Key>
  [[nodiscard]] auto ReferElement(const Key& key) const {
    erased_.AdmitStep();
    value::Formation formed{};
    auto& element = Storage().ElementRef(key, formed);
    return Ref<std::remove_reference_t<decltype(element)>>{
        erased_.Part(&element, formed)};
  }

  template <std::size_t I>
  [[nodiscard]] auto ReferComponent() const {
    auto& component = Storage().template ComponentRef<I>();
    return Ref<std::remove_reference_t<decltype(component)>>{
        erased_.Part(&component, value::Formation::kExisting)};
  }

  // The `MutationSink` surface. Storage that belongs to no variable has no
  // observation at all, so nothing reads what a write did to it; that is the
  // same answer a variable gives while nothing waits on it, reached by a
  // shorter route.
  [[nodiscard]] auto AdmitsWrite() const -> bool {
    return erased_.Admits();
  }
  [[nodiscard]] auto MutationStorage() const -> T& {
    return Storage();
  }
  [[nodiscard]] auto Watched() const -> bool {
    return erased_.Watched();
  }

  // A reference denotes the storage it binds (LRM 23.3.3.2), so the operations
  // on a cell answer through it. Storage nothing holds changes only where the
  // reference is, so its sampled value is its current one. A variable keeps
  // the value a time slot moved away from, and keeps it whole, so a reference
  // to a part of one has none to answer with; and nothing keeps a class
  // property's Preponed value.
  void ArmSampling() const {
    if (Whole()) {
      Cell().ArmSampling();
    }
  }
  [[nodiscard]] auto SampledGet() const -> const T& {
    return std::visit(
        Overloaded{
            [&](std::monostate) -> const T& { return Storage(); },
            [&](VariableCell*) -> const T& {
              if (!Whole()) {
                throw SimulationError(
                    "a sampled value of part of a variable lent by reference "
                    "is not yet supported");
              }
              return Cell().SampledGet();
            },
            [](GcObject*) -> const T& {
              throw SimulationError(
                  "a sampled value of a class property lent by reference is "
                  "not yet supported");
            }},
        erased_.holder);
  }

  void PublishTransition(const ProjectionUnchanged& unchanged) const {
    erased_.Report(unchanged);
  }

 private:
  [[nodiscard]] auto Storage() const -> T& {
    return *static_cast<T*>(erased_.storage);
  }

  [[nodiscard]] auto Whole() const -> bool {
    return erased_.whole;
  }

  [[nodiscard]] auto Cell() const -> Var<T>& {
    return WholeVariable<T>(erased_);
  }

  ErasedReference erased_;
};

// The class declaring a member, and the member's type, as a pointer to the
// member states them.
template <class MemberPointer>
struct MemberOf;

template <class T, class C>
struct MemberOf<T C::*> {
  using Class = C;
  using Value = T;
};

// A reference to the property `Property` of an object (LRM 8.4), held by the
// object so a write through it tells the object as it lands (LRM 9.4.2,
// 13.5.2), given whatever reaches the object: the running method's own object,
// or a handle. The property is a step taken on the object, so the object is
// reached once. Reaching a property through a handle naming no object is the
// design's own failure.
template <auto Property>
auto ReferProperty(typename MemberOf<decltype(Property)>::Class* object)
    -> Ref<typename MemberOf<decltype(Property)>::Value> {
  if (object == nullptr) {
    value::RaiseNullObjectHandleAccess();
  }
  return Ref<typename MemberOf<decltype(Property)>::Value>{ErasedReference{
      .holder = static_cast<GcObject*>(object),
      .storage = &(object->*Property)}};
}

template <auto Property>
auto ReferProperty(const value::ObjectRef& handle)
    -> Ref<typename MemberOf<decltype(Property)>::Value> {
  return ReferProperty<Property>(
      handle.View<typename MemberOf<decltype(Property)>::Class>());
}

// What a wait on the storage `reference` names registers on (LRM 13.5.2): what
// a write through the reference is told to, never the reference itself.
template <value::LyraValue T>
auto ReportsTo(const Ref<T>& reference) -> Observable* {
  return reference.Erased().ReportsTo();
}

// Makes `frame` runnable again when what happens at one of `triggers` is an
// event for the wait (LRM 9.4.2 / 9.4.2.2 / 9.4.3 / 15.5.2). Each subscription
// registers on the frame's own wait-registration set, so waking or destroying
// the frame revokes every leaf and the one that wakes it drops the siblings;
// the engine has no idea what kind of wait this is. Each leaf is copied into
// the target's waiter record, so `triggers` is only read for the duration of
// this call.
//
// An empty leaf set is legal and means "never wake up" -- an `always_comb`
// whose body reads nothing (`always_comb c = 7;`) runs once, then suspends
// forever.
void SubscribeToLeaves(
    CoroutineHandle frame, std::span<const Trigger> triggers);

// Which frame is waiting is the runtime's own to know, so nothing here is
// handed one: both backends reach this the same way and answer their caller
// with the same bool. The wait keeps its own copy of each leaf, so the leaves
// are only read here -- laid out in one array, or each where its builder left
// it and named by address.
auto WaitAny(RuntimeEffects& services, std::span<const Trigger> triggers)
    -> bool;
auto WaitAny(RuntimeEffects& services, std::span<const Trigger* const> triggers)
    -> bool;

// Defaulted here rather than where they are declared: a constructor or
// destructor defaulted on its first declaration is not user-provided, so a unit
// constructing a cell would define it itself with everything it reaches, and a
// family stated as already compiled would not withhold it.
template <value::LyraValue T>
Var<T>::Var() = default;

template <value::LyraValue T>
Var<T>::~Var() = default;

template <value::LyraValue T>
void Var<T>::Set(const T& new_val) {
  // A procedural write is discarded outright while any takeover shows through
  // this cell (LRM 10.6). A cell nobody has ever taken over holds no record at
  // all, so the ordinary write pays one null test.
  if (TakenOver()) {
    return;
  }
  Store(new_val);
}

template <value::LyraValue T>
auto Var<T>::BeginTakeover(const value::PackedArray& level)
    -> value::PackedArray {
  return TakeoverGenerationValue(
      RareState().Takeovers().Begin(TakeoverLevelOf(level)));
}

template <value::LyraValue T>
auto Var<T>::DriveTakeover(
    const value::PackedArray& level, const value::PackedArray& generation,
    const T& new_val) -> bool {
  const CellRareState<T>* rare = ExistingRareState();
  Takeovers<T>* takeovers =
      rare == nullptr ? nullptr : rare->ExistingTakeovers();
  if (takeovers == nullptr ||
      !takeovers->Drive(
          TakeoverLevelOf(level), TakeoverGenerationOf(generation), new_val)) {
    return false;
  }
  // A level that just recorded always leaves something showing, and it is this
  // value only where no higher level covers it. Storing whatever shows needs
  // no question asked: where a higher level covers this one, what shows has
  // not moved, and a store that changes nothing publishes nothing.
  Store(*takeovers->Highest());
  return true;
}

template <value::LyraValue T>
void Var<T>::EndTakeover(const value::PackedArray& level) {
  // Ending a level nothing occupies is what a `release` on an untaken variable
  // does, and the language gives it no effect (LRM 10.6.2).
  const CellRareState<T>* rare = ExistingRareState();
  Takeovers<T>* takeovers =
      rare == nullptr ? nullptr : rare->ExistingTakeovers();
  if (takeovers == nullptr) {
    return;
  }
  takeovers->End(TakeoverLevelOf(level));
  // Where a level is still in effect underneath, the cell takes what that
  // level already holds. Where none is, the cell keeps what it has, which is
  // the value the ended takeover last gave it.
  if (const T* showing = takeovers->Highest(); showing != nullptr) {
    Store(*showing);
  }
}

// One write in progress into what a sink stands for, and what it has learned
// so far about whether the write changed it (LRM 4.3). The write reaches the
// sink's storage and lands where its steps lead; what it did there is learned
// in three ways, whichever comes first settling the answer:
//
//   - a step that forms the part it reaches can change the owner by forming it
//     -- an element made by being written (LRM 7.8.7, 7.10.1) -- or reach no
//     part of it at all, where an invalid index makes the write ignored
//     (LRM 7.4.6), and either answers the question whatever is written;
//   - a slice write compares each element before writing it, so it says
//     whether it moved one;
//   - otherwise the part landed on is compared with its value from before the
//     write, kept only while something reads the answer.
//
// The answer reaches the sink once. It is the owner's own storage the write
// lands in, so a write is visible to everything reading the owner from the
// moment it lands (LRM 13.5.2), and a second write open over the same owner
// cannot be overwritten by this one ending.
template <class Sink>
class WriteBracket {
 public:
  using ValueType = typename Sink::ValueType;

  // A write the sink turns away (LRM 10.6) still has to reach a part to land
  // on, so it lands in a copy of the sink's storage that this write keeps and
  // nobody reads, and it has no one to tell.
  explicit WriteBracket(Sink sink) : sink_(sink) {
    if (sink_.AdmitsWrite()) {
      storage_ = &sink_.MutationStorage();
      watched_ = sink_.Watched();
    } else {
      storage_ = &discarded_.emplace(sink_.MutationStorage());
    }
  }

  [[nodiscard]] auto Storage() const -> ValueType& {
    return *storage_;
  }

  // Whether the part a write lands on is worth keeping from before the write:
  // something reads the answer, and no step has given it already.
  [[nodiscard]] auto Undecided() const -> bool {
    return watched_ && Open();
  }

  void Formed(value::Formation formed) {
    if (!Open()) {
      return;
    }
    switch (formed) {
      case value::Formation::kExisting:
        return;
      case value::Formation::kMade:
        outcome_ = Outcome::kChanged;
        return;
      case value::Formation::kNowhere:
        outcome_ = Outcome::kSettled;
        return;
    }
    throw InternalError("WriteBracket: unknown formation");
  }

  // A slice write moved at least one element.
  void Moved() {
    if (Open()) {
      outcome_ = Outcome::kChanged;
    }
  }

  // The part landed on is not what it was, and `unchanged` says which of its
  // bits moved. A packed value has no parts that are storage of their own, so
  // a write into one lands on the whole and the bits it moved are the whole's;
  // the waits on any other value are not bit-addressed.
  void Landed(const ProjectionUnchanged& unchanged) {
    if (!Open()) {
      return;
    }
    outcome_ = Outcome::kSettled;
    if constexpr (std::same_as<ValueType, value::PackedArray>) {
      sink_.PublishTransition(unchanged);
    } else {
      sink_.PublishTransition(MakeWholeValueProjectionTest());
    }
  }

  // The end of the write, with the full-expression that opened it.
  void End() {
    switch (outcome_) {
      case Outcome::kUndecided:
      case Outcome::kSettled:
        return;
      case Outcome::kChanged:
        if (watched_) {
          sink_.PublishTransition(MakeWholeValueProjectionTest());
        }
        return;
    }
    throw InternalError("WriteBracket: unknown outcome");
  }

 private:
  enum class Outcome : std::uint8_t { kUndecided, kChanged, kSettled };

  // Whether nothing has yet settled what the write did.
  [[nodiscard]] auto Open() const -> bool {
    switch (outcome_) {
      case Outcome::kUndecided:
        return true;
      case Outcome::kChanged:
      case Outcome::kSettled:
        return false;
    }
    throw InternalError("WriteBracket: unknown outcome");
  }

  Sink sink_;
  ValueType* storage_ = nullptr;
  bool watched_ = false;
  Outcome outcome_ = Outcome::kUndecided;
  std::optional<ValueType> discarded_;
};

template <class Sink, class Slice>
class DesignatedSlice;

// A place designated within a write: the whole of what the write was opened
// on, or a part of it reached by the steps below. It borrows the write. The
// steps reach the parts that are storage of their own (LRM 13.5.2) -- an
// element, reported to the write as it is formed, a component, and a slice of
// elements -- each answering with the part designated within the same write.
// Dereferencing it is where the write lands, which is when the value from
// before the write is kept.
template <class Sink, class Part>
class Designation {
 public:
  Designation(WriteBracket<Sink>& write, Part& part)
      : write_(&write), part_(&part) {
  }

  Designation(const Designation&) = delete;
  auto operator=(const Designation&) -> Designation& = delete;
  Designation(Designation&&) = delete;
  auto operator=(Designation&&) -> Designation& = delete;

  ~Designation() {
    if (before_.has_value() && !before_->IsBitIdentical(*part_)) {
      write_->Landed(LandedChange(*before_, *part_));
    }
  }

  template <typename Key>
  auto ElementRef(const Key& key) {
    value::Formation formed{};
    auto& element = part_->ElementRef(key, formed);
    write_->Formed(formed);
    return Designation<Sink, std::remove_reference_t<decltype(element)>>{
        *write_, element};
  }

  template <std::size_t I>
  auto ComponentRef() {
    auto& component = part_->template ComponentRef<I>();
    return Designation<Sink, std::remove_reference_t<decltype(component)>>{
        *write_, component};
  }

  template <typename... Bounds>
  auto SliceRef(const Bounds&... bounds) {
    auto slice = part_->SliceRef(bounds...);
    return DesignatedSlice<Sink, decltype(slice)>{*write_, std::move(slice)};
  }

  auto operator*() -> Part& {
    if (write_->Undecided()) {
      before_.emplace(*part_);
    }
    return *part_;
  }

  // A member reached through the designation is reached on what `*` lands on,
  // as `p->m` is `(*p).m`.
  auto operator->() -> Part* {
    return &**this;
  }

 private:
  WriteBracket<Sink>* write_;
  Part* part_;
  std::optional<Part> before_;
};

// A slice of elements designated within a write (LRM 7.6). It is several
// elements rather than one place, so what the write learns comes from the
// assignment into it, which compares each element before writing it.
template <class Sink, class Slice>
class DesignatedSlice {
 public:
  DesignatedSlice(WriteBracket<Sink>& write, Slice slice)
      : write_(&write), slice_(std::move(slice)) {
  }

  DesignatedSlice(const DesignatedSlice&) = delete;
  auto operator=(const DesignatedSlice&) -> DesignatedSlice& = delete;
  DesignatedSlice(DesignatedSlice&&) = delete;
  auto operator=(DesignatedSlice&&) -> DesignatedSlice& = delete;
  ~DesignatedSlice() = default;

  auto operator*() -> DesignatedSlice& {
    return *this;
  }

  template <typename Value>
  auto operator=(const Value& value) -> DesignatedSlice& {
    if (slice_.Assign(value)) {
      write_->Moved();
    }
    return *this;
  }

 private:
  WriteBracket<Sink>* write_;
  Slice slice_;
};

// A write opened in the full-expression that writes, and ended with it, which
// is when the sink is told once whether the write changed it. It names no
// place itself: the whole of what the sink stands for is designated within it,
// and the steps into parts start there. Non-copyable and non-movable:
// returning it by value from the entry that opens one relies on C++17
// mandatory copy elision.
template <class Sink>
class ScopedMutation {
 public:
  using ValueType = typename Sink::ValueType;

  explicit ScopedMutation(Sink sink) : write_(sink) {
  }

  ScopedMutation(const ScopedMutation&) = delete;
  auto operator=(const ScopedMutation&) -> ScopedMutation& = delete;
  ScopedMutation(ScopedMutation&&) = delete;
  auto operator=(ScopedMutation&&) -> ScopedMutation& = delete;

  ~ScopedMutation() {
    write_.End();
  }

  auto WholeRef() -> Designation<Sink, ValueType> {
    return Designation<Sink, ValueType>{write_, write_.Storage()};
  }

 private:
  WriteBracket<Sink> write_;
};

template <value::LyraValue T>
auto Var<T>::Mutate() -> ScopedMutation<Ref<T>> {
  return ScopedMutation<Ref<T>>{Ref<T>{*this}};
}

template <value::LyraValue T>
auto Ref<T>::Mutate() const -> ScopedMutation<Ref<T>> {
  return ScopedMutation<Ref<T>>{*this};
}

static_assert(MutationSink<Ref<value::PackedArray>>);

}  // namespace lyra::runtime
