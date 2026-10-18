#pragma once

#include <cstdint>
#include <functional>
#include <memory>
#include <unordered_map>
#include <vector>

#include "lyra/runtime/observable.hpp"
#include "lyra/runtime/owned_call.hpp"
#include "lyra/runtime/source_follower.hpp"
#include "lyra/runtime/var.hpp"

namespace lyra::runtime {

// The members of scopes that references were bound into (LRM 23.3.3), and what
// a force on one needs (LRM 10.6.2).
//
// A port that stands for what its connection drives is such a member. Binding
// it records nothing but what was bound from it in turn -- a port handed on to
// a child's port, or a `ref` port connected to it -- so a design that forces
// nothing keeps a list per port handed on and nothing else.
//
// A force gives a member storage of the force's own to name, together with
// every member bound from it and every wait enrolled through any of them, so
// that what the member drives sees the forced value and what drives the member
// does not. What drove the member goes on holding its own value, untouched,
// and the storage follows it wherever the force holds no position, as the
// connection it stands for would carry it (LRM 23.3.3); a release makes them
// all name it again. The storage lasts from the force to the release, whatever
// becomes of the process that made it.
class BoundMembers {
 public:
  BoundMembers();
  BoundMembers(const BoundMembers&) = delete;
  auto operator=(const BoundMembers&) -> BoundMembers& = delete;
  BoundMembers(BoundMembers&&) = delete;
  auto operator=(BoundMembers&&) -> BoundMembers& = delete;
  ~BoundMembers();

  // Binds `member` to what `bound` names, and with it whatever was bound from
  // `member` before `member` itself was. Where `bound` is a member not bound
  // yet, `member` takes what it names once it is.
  void Bind(ErasedReference& member, const ErasedReference& bound);

  // Binds `alias` to what `bound` names, as another name of it rather than a
  // member of its own (LRM 23.3.3.2): where `bound` names a member, the alias
  // goes on naming whatever that member names, and a takeover through it acts
  // on that member.
  void Alias(ErasedReference& alias, const ErasedReference& bound);

  // Whether a force is in effect on `member`.
  [[nodiscard]] auto Forced(const ErasedReference& member) const -> bool;

  // Where storage a force gives `member` to name is kept: none until the
  // first force on it, and from then on the same storage for every force, so
  // what is evaluating a force it gave up never mistakes a later one for its
  // own.
  [[nodiscard]] auto ForcedStorage(const ErasedReference& member)
      -> std::shared_ptr<void>&;

  // Puts `member` under a force: it and what follows it name `forced`, and
  // each change to what drives it runs what `follow` makes of that driver
  // until the release.
  void Force(
      ErasedReference& member, const ErasedReference& forced,
      const std::function<OwnedCall(const ErasedReference& driver)>& follow);

  // What drives `member`, which is what it names when nothing forces it.
  [[nodiscard]] auto DriverOf(const ErasedReference& member) const
      -> ErasedReference;

  // Ends the force on `member`: it and what follows it name what drives it
  // again.
  void Release(ErasedReference& member);

 private:
  struct Member {
    // The members and the `ref` ports bound from this one.
    std::vector<ErasedReference*> bound_from_it;
    // What the member named before a force made it name other storage; the
    // force is in effect exactly while this holds one.
    std::unique_ptr<ErasedReference> driver;
    // The storage a force gives the member to name, kept from the first
    // force on.
    std::shared_ptr<void> forced;
    // What keeps that storage showing the driver wherever the force holds no
    // position, while the force is in effect.
    std::unique_ptr<SourceFollower> follower;
  };

  // What a change of what `member` names reaches. A member bound from it names
  // the same storage and follows, and so on down; one that a force of its own
  // covers keeps naming that force's storage, and only what it will name once
  // released follows.
  struct Followers {
    std::vector<ErasedReference*> naming;
    std::vector<ErasedReference*> forced_below;
  };
  [[nodiscard]] auto Following(ErasedReference& member) const -> Followers;

  // Makes every alias among `followers` not yet named a name of `member`.
  static void NameAliases(const Followers& followers, ErasedReference& member);

  // Makes each follower name what `names` does, and enrols there every wait
  // enrolled on `from` through a member that now names it.
  static void Move(
      const Followers& followers, Observable* from,
      const ErasedReference& names);

  std::unordered_map<const ErasedReference*, Member> members_;
};

// What generated code calls to bind a member, on the engine it runs under.
template <value::LyraValue T>
void BindMember(VariableRef<T>& member, const VariableRef<T>& bound) {
  current_runtime().Bound().Bind(member.AsMember(), bound.Erased());
}

// What generated code calls to bind a `ref` port, on the engine it runs under.
template <value::LyraValue T>
void BindAlias(VariableRef<T>& alias, const VariableRef<T>& bound) {
  current_runtime().Bound().Alias(alias.AsMember(), bound.Erased());
}

// Whether a procedural continuous assignment through `reference` acts on a
// member rather than on the variable it names. A member is a port standing for
// what its connection drives, a value of its own (LRM 23.3.3.2), so a takeover
// through it, or through a `ref` port connected to it, re-points the member and
// leaves its source alone. Which of the two a reference is follows from how
// each instance is connected, so it is answered here, per reference.
[[nodiscard]] inline auto NamesAMember(const ErasedReference& reference)
    -> bool {
  return reference.member != nullptr;
}

// The member a procedural continuous assignment at `level` on `reference`
// acts on: the one the reference is, or is a copy of. Only a force reaches a
// name that owns no storage, so anything else arrives here only by a lowering
// defect.
[[nodiscard]] auto ForcedMember(
    const ErasedReference& reference, std::int64_t level) -> ErasedReference&;

// The three operations of a procedural continuous assignment (LRM 10.6)
// through a reference, on what it names, over the class `C` of the cells the
// variables it names are, at the positions `first` and `count` name of it: a
// variable is taken over where it lies, and a member is given a cell of its
// own to name, which is taken over as a variable is and goes on showing what
// drives the member wherever the takeovers hold no position, as the connection
// the member stands for carries it continuously (LRM 23.3.3). `fresh` makes
// that cell holding what a cell holds, the first time the member is forced,
// and `refer` forms the reference that names it, in whichever form the
// references of the running program take.

template <class C, class Fresh, class Refer>
auto BeginTakeoverThrough(
    const ErasedReference& reference, std::int64_t level, std::int64_t first,
    std::int64_t count, Fresh fresh, Refer refer) -> std::int64_t {
  if (!NamesAMember(reference)) {
    return WholeVariable<C>(reference).BeginTakeover(level, first, count);
  }
  ErasedReference& member = ForcedMember(reference, level);
  BoundMembers& bound = current_runtime().Bound();
  if (!bound.Forced(member)) {
    // The cell first holds what the member showed, so naming it changes
    // nothing a wait could see; the forced value then arrives as any
    // takeover's does.
    std::shared_ptr<void>& kept = bound.ForcedStorage(member);
    if (kept == nullptr) {
      std::shared_ptr<C> made = fresh(WholeVariable<C>(member));
      current_runtime().DriveContinuously(*made);
      kept = made;
    } else {
      static_cast<C*>(kept.get())->SetFrom(WholeVariable<C>(member));
    }
    C& cell = *static_cast<C*>(kept.get());
    bound.Force(member, refer(cell), [&cell](const ErasedReference& driver) {
      return OwnedCall(
          [&cell, &driver] { cell.SetFrom(WholeVariable<C>(driver)); });
    });
  }
  return WholeVariable<C>(member).BeginTakeover(level, first, count);
}

// `drive` states the evaluation's value to the variable under the takeover,
// answering whether that evaluation is still in effect.
template <class C, class Drive>
auto DriveTakeoverThrough(
    const ErasedReference& reference, std::int64_t level, Drive drive) -> bool {
  if (!NamesAMember(reference)) {
    return drive(WholeVariable<C>(reference));
  }
  const ErasedReference& member = ForcedMember(reference, level);
  if (!current_runtime().Bound().Forced(member)) {
    return false;
  }
  return drive(WholeVariable<C>(member));
}

// A member's cell is a variable its connection drives continuously, so a
// release reestablishes that and schedules its reevaluation (LRM 10.6.2), as
// it does for any such variable. Once nothing holds a position of the cell,
// the member names what drives it again, from the same reevaluation: the cell
// takes that value first, as any write, so whoever waits on the member is told
// exactly when the release changed what it shows, and a force begun before then
// keeps the cell.
template <class C>
void EndTakeoverThrough(
    const ErasedReference& reference, std::int64_t level, std::int64_t first,
    std::int64_t count) {
  if (!NamesAMember(reference)) {
    WholeVariable<C>(reference).EndTakeover(level, first, count);
    return;
  }
  ErasedReference& member = ForcedMember(reference, level);
  RuntimeEffects& runtime = current_runtime();
  if (!runtime.Bound().Forced(member)) {
    return;
  }
  C& cell = WholeVariable<C>(member);
  cell.EndTakeover(level, first, count);
  if (cell.TakenOver()) {
    return;
  }
  runtime.Submit(runtime.Now(), Region::kActive, [&member, &cell] {
    BoundMembers& bound = current_runtime().Bound();
    if (!bound.Forced(member) || cell.TakenOver()) {
      return;
    }
    cell.SetFrom(WholeVariable<C>(bound.DriverOf(member)));
    bound.Release(member);
  });
}

template <value::LyraValue T>
auto VariableRef<T>::BeginTakeover(
    std::int64_t level, std::int64_t first, std::int64_t count) const
    -> std::int64_t {
  return BeginTakeoverThrough<Var<T>>(
      erased_, level, first, count,
      [](const Var<T>& showing) {
        auto forced = std::make_shared<Var<T>>();
        forced->Initialize(showing.Get());
        return forced;
      },
      [](Var<T>& forced) { return VariableRef<T>{forced}.Erased(); });
}

template <value::LyraValue T>
auto VariableRef<T>::DriveTakeover(
    std::int64_t level, std::int64_t generation, const T& new_val) const
    -> bool {
  return DriveTakeoverThrough<Var<T>>(erased_, level, [&](Var<T>& variable) {
    return variable.DriveTakeover(level, generation, new_val);
  });
}

template <value::LyraValue T>
void VariableRef<T>::EndTakeover(
    std::int64_t level, std::int64_t first, std::int64_t count) const {
  EndTakeoverThrough<Var<T>>(erased_, level, first, count);
}

}  // namespace lyra::runtime
