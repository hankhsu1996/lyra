#pragma once

namespace lyra::mir {

enum class BuiltinMethod {
  // Dynamic array operations
  kNewArray,     // new[size] or new[size](init) - returns array
  kArraySize,    // arr.size() - returns int
  kArrayDelete,  // arr.delete() - void, effectful

  // Queue operations
  kQueueSize,       // q.size() - returns int
  kQueueDelete,     // q.delete() - clear all
  kQueueDeleteAt,   // q.delete(idx) - remove at index
  kQueuePushBack,   // q.push_back(val) - append
  kQueuePushFront,  // q.push_front(val) - prepend
  kQueuePopBack,    // q.pop_back() - returns element, mutates queue
  kQueuePopFront,   // q.pop_front() - returns element, mutates queue
  kQueueInsert,     // q.insert(idx, val) - insert at index

  // Enum operations (runtime only - first/last/num are constant-folded)
  kEnumNext,  // e.next([N]) - returns next enum value (wraps)
  kEnumPrev,  // e.prev([N]) - returns previous enum value (wraps)
  kEnumName,  // e.name() - returns string name of current value
};

}  // namespace lyra::mir
