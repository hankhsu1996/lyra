#pragma once

namespace lyra::runtime {

// A link in an intrusive list: the list holds no storage of its own, and an
// object is in one by being linked through this. `Node` derives from it and is
// what the list holds -- a wait's membership on a target, a process's
// membership on a disable target, an activation's place in a queue.
//
// Whoever the node serves owns it, and the list only links it. So the relation
// is recorded once and reached from both ends, and leaving the list is an
// unlink: neither side ever searches the other, and neither can hold a stale
// copy of what the other believes. Ending the owner destroys the node, which
// unlinks itself, so nothing is left able to reach what it named -- LRM 9.7
// process control releases a process while it is parked, which is what makes
// that direction reachable.
template <class Node>
struct IntrusiveListNode {
  Node* prev = nullptr;
  Node* next = nullptr;

  IntrusiveListNode() = default;
  IntrusiveListNode(const IntrusiveListNode&) = delete;
  auto operator=(const IntrusiveListNode&) -> IntrusiveListNode& = delete;
  // Non-movable: the list points at this address.
  IntrusiveListNode(IntrusiveListNode&&) = delete;
  auto operator=(IntrusiveListNode&&) -> IntrusiveListNode& = delete;
  ~IntrusiveListNode() {
    Unlink();
  }

  // Detaches from whichever list holds it. Pointer surgery only -- which is why
  // a node needs no pointer back to its list, and why a list is never consulted
  // to leave it.
  void Unlink() noexcept {
    if (next == nullptr) {
      return;
    }
    prev->next = next;
    next->prev = prev;
    prev = nullptr;
    next = nullptr;
  }
};

// The nodes one list currently holds, all of one kind. The ring closes through
// an embedded sentinel, so an unlink touches no list state -- that is what
// keeps it constant-time and list-agnostic -- and moving a whole list onto
// another is a constant-time relink rather than a walk.
template <class Node>
class IntrusiveList {
 public:
  IntrusiveList() {
    sentinel_.prev = &sentinel_;
    sentinel_.next = &sentinel_;
  }
  IntrusiveList(const IntrusiveList&) = delete;
  auto operator=(const IntrusiveList&) -> IntrusiveList& = delete;
  // Non-movable: the linked nodes point at this sentinel's address.
  IntrusiveList(IntrusiveList&&) = delete;
  auto operator=(IntrusiveList&&) -> IntrusiveList& = delete;
  // A node's owner can outlive the list, so detach what is still linked rather
  // than leave it pointing into freed storage.
  ~IntrusiveList() {
    Clear();
  }

  [[nodiscard]] auto Empty() const noexcept -> bool {
    return sentinel_.next == &sentinel_;
  }

  void PushBack(Node& node) noexcept {
    node.prev = sentinel_.prev;
    node.next = &sentinel_;
    sentinel_.prev->next = &node;
    sentinel_.prev = &node;
  }

  // Takes the oldest node off the list, or null when there is none.
  auto PopFront() noexcept -> Node* {
    Node* node = sentinel_.next;
    if (node == &sentinel_) {
      return nullptr;
    }
    sentinel_.next = node->next;
    node->next->prev = &sentinel_;
    node->prev = nullptr;
    node->next = nullptr;
    return node;
  }

  // Moves every node onto the back of `other`, in order, leaving this list
  // empty.
  void SpliceBackOnto(IntrusiveList& other) noexcept {
    if (Empty()) {
      return;
    }
    Node* first = sentinel_.next;
    Node* last = sentinel_.prev;
    first->prev = other.sentinel_.prev;
    last->next = &other.sentinel_;
    other.sentinel_.prev->next = first;
    other.sentinel_.prev = last;
    sentinel_.prev = &sentinel_;
    sentinel_.next = &sentinel_;
  }

  void Clear() noexcept {
    while (PopFront() != nullptr) {
    }
  }

  // Visits each node. `fn` may unlink the one it is handed -- the walk has
  // already stepped past it -- but must not unlink any other.
  template <class Fn>
  void ForEach(Fn fn) {
    Node* node = sentinel_.next;
    while (node != &sentinel_) {
      Node* next = node->next;
      fn(*node);
      node = next;
    }
  }

 private:
  Node sentinel_;
};

}  // namespace lyra::runtime
