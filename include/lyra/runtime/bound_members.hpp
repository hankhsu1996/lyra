#pragma once

#include <cstdint>
#include <memory>
#include <unordered_map>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/observable.hpp"
#include "lyra/runtime/takeover.hpp"
#include "lyra/runtime/var.hpp"
#include "lyra/support/takeover_level.hpp"

namespace lyra::runtime {

// The members of scopes that references were bound into (LRM 23.3.3), and what
// a force on one needs (LRM 10.6.2).
//
// A port that stands for what its connection drives is such a member. Binding
// it records nothing but which members were bound from it in turn -- a port
// handed on to a child's port -- so a design that forces nothing keeps a list
// per port handed on and nothing else.
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
    // The members bound from this one.
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

  // Makes each follower name what `names` does, and enrols there every wait
  // enrolled on `from` through a member that now names it.
  static void Move(
      const Followers& followers, Observable* from,
      const ErasedReference& names);

  std::unordered_map<const ErasedReference*, Member> members_;
};

// What generated code calls to bind a member, on the engine it runs under.
template <value::LyraValue T>
void BindMember(Ref<T>& member, const Ref<T>& bound) {
  current_runtime().Bound().Bind(member.AsMember(), bound.Erased());
}

// The member a procedural continuous assignment at `level` on `reference`
// acts on: the one the reference is, or is a copy of. Only a force reaches a
// name that owns no storage, and only a member can be such a name, so anything
// else reached here by a lowering defect.
[[nodiscard]] auto ForcedMember(
    const ErasedReference& reference, std::int64_t level) -> ErasedReference&;

// The variable `member` names, as the one of `T` it is: a member is bound to
// the whole of a variable, its own connection's or the force's.
template <value::LyraValue T>
[[nodiscard]] auto VariableNamedBy(const ErasedReference& member) -> Var<T>& {
  if (!member.whole || !std::holds_alternative<VariableCell*>(member.holder)) {
    throw InternalError(
        "VariableNamedBy: a bound member names something other than the whole "
        "of a variable");
  }
  return WholeVariable<T>(member);
}

// The three operations of a procedural continuous assignment (LRM 10.6), on a
// name that owns no storage. `refer` forms the reference that names the
// force's storage, in whichever form the references of the running program
// take.

template <value::LyraValue T, class Refer>
auto BeginMemberTakeover(
    const ErasedReference& reference, std::int64_t level, Refer refer)
    -> std::int64_t {
  ErasedReference& member = ForcedMember(reference, level);
  BoundMembers& bound = current_runtime().Bound();
  if (!bound.Forced(member)) {
    // The storage first holds what the member showed, so naming it changes
    // nothing a wait could see; the forced value then arrives as any write.
    auto forced = std::make_shared<Var<T>>();
    forced->Initialize(VariableNamedBy<T>(member).Get());
    bound.Force(member, refer(*forced), forced);
  }
  return bound.NextGeneration(member);
}

template <value::LyraValue T, class Drive>
auto DriveMemberTakeover(
    const ErasedReference& reference, std::int64_t level,
    std::int64_t generation, Drive drive) -> bool {
  const ErasedReference& member = ForcedMember(reference, level);
  if (!current_runtime().Bound().StillForcing(member, generation)) {
    return false;
  }
  drive(VariableNamedBy<T>(member));
  return true;
}

// The member shows what drives it from here on, so the force's storage takes
// that value first, as any write, and whoever waits on the member is told
// exactly when the release changed what it shows.
template <value::LyraValue T>
void EndMemberTakeover(const ErasedReference& reference, std::int64_t level) {
  ErasedReference& member = ForcedMember(reference, level);
  BoundMembers& bound = current_runtime().Bound();
  if (bound.Forced(member)) {
    VariableNamedBy<T>(member).Set(
        VariableNamedBy<T>(bound.DriverOf(member)).Get());
  }
  bound.Release(member);
}

template <value::LyraValue T>
auto Ref<T>::BeginTakeover(std::int64_t level) const -> std::int64_t {
  return BeginMemberTakeover<T>(
      erased_, level, [](Var<T>& forced) { return Ref<T>{forced}.Erased(); });
}

template <value::LyraValue T>
auto Ref<T>::DriveTakeover(
    std::int64_t level, std::int64_t generation, const T& new_val) const
    -> bool {
  return DriveMemberTakeover<T>(
      erased_, level, generation, [&](Var<T>& forced) { forced.Set(new_val); });
}

template <value::LyraValue T>
void Ref<T>::EndTakeover(std::int64_t level) const {
  EndMemberTakeover<T>(erased_, level);
}

}  // namespace lyra::runtime
