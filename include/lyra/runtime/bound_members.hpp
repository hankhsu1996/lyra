#pragma once

#include <cstdint>
#include <memory>
#include <unordered_map>
#include <vector>

#include "lyra/runtime/observable.hpp"
#include "lyra/runtime/var.hpp"

namespace lyra::runtime {

// The members of scopes that references were bound into (LRM 23.3.3), and what
// a force on one needs (LRM 10.6.2).
//
// A port that stands for what its connection drives is such a member. Binding
// it records nothing but which members were bound from it in turn -- a port
// handed on to a child's port -- so a design that forces nothing keeps a list
// per port handed on and nothing else.
//
// A force makes a member name storage of the force's own, together with every
// member bound from it and every wait enrolled through any of them, so that
// what the member drives sees the forced value and what drives the member does
// not. A release makes them name what drives the member again. Whoever forces
// holds the storage and evaluates into it; this holds only which members and
// waits follow.
class BoundMembers {
 public:
  BoundMembers();
  BoundMembers(const BoundMembers&) = delete;
  auto operator=(const BoundMembers&) -> BoundMembers& = delete;
  BoundMembers(BoundMembers&&) = delete;
  auto operator=(BoundMembers&&) -> BoundMembers& = delete;
  ~BoundMembers();

  // Binds `member` to what `bound` names.
  void Bind(ErasedReference& member, const ErasedReference& bound);

  // Starts a force on `member`, superseding one in effect, and answers the
  // generation its evaluation carries.
  auto BeginForce(ErasedReference& member) -> std::int64_t;

  // Makes `member`, and what follows it, name `forced`, where the force that
  // began at `generation` is still the one that was last begun and nothing
  // has released the member since. A force is in effect from the statement
  // that makes it, and its evaluation runs later, so a release or another
  // force in between is what this finds.
  void Retarget(
      ErasedReference& member, const ErasedReference& forced,
      std::int64_t generation);

  // Whether the force that began at `generation` is still the one in effect.
  [[nodiscard]] auto StillForcing(
      const ErasedReference& member, std::int64_t generation) const -> bool;

  // What an evaluation of a force on `member` waits on to learn it has ended.
  [[nodiscard]] auto ForceEnded(ErasedReference& member) -> Observable&;

  // What drives `member`, which is what it names when nothing forces it.
  [[nodiscard]] auto DriverOf(const ErasedReference& member) const
      -> ErasedReference;

  // Ends the force on `member`: it and what follows it name what drives it
  // again, and the force's evaluation is woken to end.
  void Release(ErasedReference& member);

 private:
  struct Member {
    // The members bound from this one.
    std::vector<ErasedReference*> bound_from_it;
    // What the member named before a force made it name other storage; the
    // force is in effect exactly while this holds one.
    std::unique_ptr<ErasedReference> driver;
    std::int64_t generation = 0;
    // Reached when a force on the member ends or is superseded. Held apart so
    // the waits enrolled on it keep their place while the table grows.
    std::unique_ptr<Observable> ended;
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

// The member `reference` is, or is a copy of. Only a reference bound into a
// member is ever forced, so one that names none reached here by a lowering
// defect.
[[nodiscard]] auto BoundMember(const ErasedReference& reference)
    -> ErasedReference&;

// What generated code calls, each on the engine it runs under. A force and a
// release are stated through these and through ordinary reads and writes of
// the references involved, so none of them reads a value.

template <value::LyraValue T>
void BindMember(Ref<T>& member, const Ref<T>& bound) {
  current_runtime().Bound().Bind(member.AsMember(), bound.Erased());
}

template <value::LyraValue T>
auto BeginForce(const Ref<T>& member) -> std::int64_t {
  return current_runtime().Bound().BeginForce(BoundMember(member.Erased()));
}

template <value::LyraValue T>
void RetargetMember(
    const Ref<T>& member, const Ref<T>& forced, std::int64_t generation) {
  current_runtime().Bound().Retarget(
      BoundMember(member.Erased()), forced.Erased(), generation);
}

template <value::LyraValue T>
auto StillForcing(const Ref<T>& member, std::int64_t generation) -> bool {
  return current_runtime().Bound().StillForcing(
      BoundMember(member.Erased()), generation);
}

template <value::LyraValue T>
auto ForceEnded(const Ref<T>& member) -> WatchedPlace {
  return WatchedPlace{
      &current_runtime().Bound().ForceEnded(BoundMember(member.Erased()))};
}

template <value::LyraValue T>
auto DriverOfMember(const Ref<T>& member) -> Ref<T> {
  return Ref<T>{
      current_runtime().Bound().DriverOf(BoundMember(member.Erased()))};
}

template <value::LyraValue T>
void ReleaseMember(const Ref<T>& member) {
  current_runtime().Bound().Release(BoundMember(member.Erased()));
}

}  // namespace lyra::runtime
