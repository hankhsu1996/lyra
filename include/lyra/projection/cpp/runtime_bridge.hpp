#pragma once

namespace lyra::projection::cpp {

using SingleEntryFn = void (*)(void* self_void);

// Run a single initial process against the real Engine.
// Returns 0 on success, nonzero on engine error.
//
// This is a single-process prototype entry point. It is not a generic
// process runtime. The entry_fn is called exactly once and the process
// is marked finished immediately.
auto RunSingleInitial(void* object_ptr, SingleEntryFn entry_fn) -> int;

}  // namespace lyra::projection::cpp
