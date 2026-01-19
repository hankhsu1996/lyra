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
};

}  // namespace lyra::mir
