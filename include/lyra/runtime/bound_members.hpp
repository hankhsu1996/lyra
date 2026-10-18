#pragma once

#include <cstdint>
#include <memory>
#include <unordered_map>
#include <vector>

#include "lyra/base/simulation_error.hpp"
#include "lyra/runtime/observable.hpp"
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
// does not. What drove the member goes on holding its own value, untouched, and
// a release makes them all name it again. The storage lasts from the force to
// the release, whatever becomes of the process that made it.
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

  // Puts `member` under a force: it and what follows it name `forced`, which
  // `storage` keeps alive until the release.
  void Force(
      ErasedReference& member, const ErasedReference& forced,
      std::shared_ptr<void> storage);

  // Starts an evaluation of the force on `member`, superseding the one before
  // it, and answers the generation it carries.
  auto NextGeneration(ErasedReference& member) -> std::int64_t;

  // Whether the evaluation that began at `generation` is still the one driving
  // the force on `member`.
  [[nodiscard]] auto StillForcing(
      const ErasedReference& member, std::int64_t generation) const -> bool;

  // What drives `member`, which is what it names when nothing forces it.
  [[nodiscard]] auto DriverOf(const ErasedReference& member) const
      -> ErasedReference;

  // Ends the force on `member`: it and what follows it name what drives it
  // again, and whatever was evaluating the force finds it superseded.
  void Release(ErasedReference& member);

 private:
  struct Member {
    // The members and the `ref` ports bound from this one.
    std::vector<ErasedReference*> bound_from_it;
    // What the member named before a force made it name other storage; the
    // force is in effect exactly while this holds one.
    std::unique_ptr<ErasedReference> driver;
    // The storage the force gave the member to name.
    std::shared_ptr<void> forced;
    std::int64_t generation = 0;
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

// A member given storage of the assignment's own stops showing what its
// connection drives, so only the whole of it can be taken over that way: the
// rest of it would have to go on following its source.
inline void RequireWholeMember(std::int64_t count) {
  if (count != 0) {
    throw SimulationError(
        "a force on part of a port that stands for what its connection drives "
        "is not yet supported (LRM 10.6.2)");
  }
}

// The three operations of a procedural continuous assignment (LRM 10.6)
// through a reference, on what it names, over the class `C` of the cells the
// variables it names are, at the positions `lsb` and `count` name of it: a
// variable is taken over where it lies, and a member is given storage of the
// assignment's own to name. `fresh` makes that storage holding what a cell
// holds, and `refer` forms the reference that names it, in whichever form the
// references of the running program take.

template <class C, class Fresh, class Refer>
auto BeginTakeoverThrough(
    const ErasedReference& reference, std::int64_t level, std::int64_t lsb,
    std::int64_t count, Fresh fresh, Refer refer) -> std::int64_t {
  if (!NamesAMember(reference)) {
    return WholeVariable<C>(reference).BeginTakeover(level, lsb, count);
  }
  RequireWholeMember(count);
  ErasedReference& member = ForcedMember(reference, level);
  BoundMembers& bound = current_runtime().Bound();
  if (!bound.Forced(member)) {
    // The storage first holds what the member showed, so naming it changes
    // nothing a wait could see; the forced value then arrives as any write.
    std::shared_ptr<C> forced = fresh(WholeVariable<C>(member));
    bound.Force(member, refer(*forced), forced);
  }
  return bound.NextGeneration(member);
}

// `drive` states the evaluation's value to a variable under its own takeover,
// answering whether that evaluation is still in effect; `write` writes it to
// the storage a member's force gave it, as any write.
template <class C, class Drive, class Write>
auto DriveTakeoverThrough(
    const ErasedReference& reference, std::int64_t level,
    std::int64_t generation, Drive drive, Write write) -> bool {
  if (!NamesAMember(reference)) {
    return drive(WholeVariable<C>(reference));
  }
  const ErasedReference& member = ForcedMember(reference, level);
  if (!current_runtime().Bound().StillForcing(member, generation)) {
    return false;
  }
  write(WholeVariable<C>(member));
  return true;
}

// A member shows what drives it from here on, so the force's storage takes
// that value first, as any write, and whoever waits on the member is told
// exactly when the release changed what it shows.
template <class C>
void EndTakeoverThrough(
    const ErasedReference& reference, std::int64_t level, std::int64_t lsb,
    std::int64_t count) {
  if (!NamesAMember(reference)) {
    WholeVariable<C>(reference).EndTakeover(level, lsb, count);
    return;
  }
  RequireWholeMember(count);
  ErasedReference& member = ForcedMember(reference, level);
  BoundMembers& bound = current_runtime().Bound();
  if (bound.Forced(member)) {
    WholeVariable<C>(member).SetFrom(WholeVariable<C>(bound.DriverOf(member)));
  }
  bound.Release(member);
}

template <value::LyraValue T>
auto VariableRef<T>::BeginTakeover(
    std::int64_t level, std::int64_t lsb, std::int64_t count) const
    -> std::int64_t {
  return BeginTakeoverThrough<Var<T>>(
      erased_, level, lsb, count,
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
  return DriveTakeoverThrough<Var<T>>(
      erased_, level, generation,
      [&](Var<T>& variable) {
        return variable.DriveTakeover(level, generation, new_val);
      },
      [&](Var<T>& forced) { forced.Set(new_val); });
}

template <value::LyraValue T>
void VariableRef<T>::EndTakeover(
    std::int64_t level, std::int64_t lsb, std::int64_t count) const {
  EndTakeoverThrough<Var<T>>(erased_, level, lsb, count);
}

}  // namespace lyra::runtime
