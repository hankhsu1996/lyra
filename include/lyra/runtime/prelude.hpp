#pragma once

// Umbrella header that an emitted project's precompiled header pre-parses.
// Forms the superset of standard-library and Lyra headers any emitted
// `main.cpp` or scope header could pull in. Listing a header here that the
// emit does not use is harmless (its parse cost is paid once when building
// the PCH and never again); missing one means the emit pays the parse cost
// at compile time and loses no correctness. Keep the list a superset rather
// than a tight union so additions in the backend require no synchronized
// edit here. Per-line `IWYU pragma: keep` marks the file's intent so
// include-cleaner tools do not flag the umbrella as having unused includes.

#include <array>       // IWYU pragma: keep
#include <cmath>       // IWYU pragma: keep
#include <cstdint>     // IWYU pragma: keep
#include <functional>  // IWYU pragma: keep
#include <memory>      // IWYU pragma: keep
#include <span>        // IWYU pragma: keep
#include <stdexcept>   // IWYU pragma: keep
#include <string>      // IWYU pragma: keep
#include <vector>      // IWYU pragma: keep

#include "lyra/runtime/coroutine.hpp"          // IWYU pragma: keep
#include "lyra/runtime/delay.hpp"              // IWYU pragma: keep
#include "lyra/runtime/diagnostic.hpp"         // IWYU pragma: keep
#include "lyra/runtime/engine.hpp"             // IWYU pragma: keep
#include "lyra/runtime/event.hpp"              // IWYU pragma: keep
#include "lyra/runtime/extern_up.hpp"          // IWYU pragma: keep
#include "lyra/runtime/file_table.hpp"         // IWYU pragma: keep
#include "lyra/runtime/finish.hpp"             // IWYU pragma: keep
#include "lyra/runtime/fork.hpp"               // IWYU pragma: keep
#include "lyra/runtime/hierarchy_segment.hpp"  // IWYU pragma: keep
#include "lyra/runtime/named_event.hpp"        // IWYU pragma: keep
#include "lyra/runtime/process_kind.hpp"       // IWYU pragma: keep
#include "lyra/runtime/runtime_process.hpp"    // IWYU pragma: keep
#include "lyra/runtime/runtime_services.hpp"   // IWYU pragma: keep
#include "lyra/runtime/runtime_traversal.hpp"  // IWYU pragma: keep
#include "lyra/runtime/scope.hpp"              // IWYU pragma: keep
#include "lyra/runtime/sim_time.hpp"           // IWYU pragma: keep
#include "lyra/runtime/simulation_entry.hpp"   // IWYU pragma: keep
#include "lyra/runtime/stream_dispatcher.hpp"  // IWYU pragma: keep
#include "lyra/runtime/trigger.hpp"            // IWYU pragma: keep
#include "lyra/runtime/var.hpp"                // IWYU pragma: keep
#include "lyra/value/array_case_equal.hpp"     // IWYU pragma: keep
#include "lyra/value/associative_array.hpp"    // IWYU pragma: keep
#include "lyra/value/dynamic_array.hpp"        // IWYU pragma: keep
#include "lyra/value/enum.hpp"                 // IWYU pragma: keep
#include "lyra/value/format.hpp"               // IWYU pragma: keep
#include "lyra/value/integral_format.hpp"      // IWYU pragma: keep
#include "lyra/value/packed.hpp"               // IWYU pragma: keep
#include "lyra/value/packed_array.hpp"         // IWYU pragma: keep
#include "lyra/value/packed_bitwise.hpp"       // IWYU pragma: keep
#include "lyra/value/packed_convert.hpp"       // IWYU pragma: keep
#include "lyra/value/packed_reduction.hpp"     // IWYU pragma: keep
#include "lyra/value/packed_type.hpp"          // IWYU pragma: keep
#include "lyra/value/queue.hpp"                // IWYU pragma: keep
#include "lyra/value/scan.hpp"                 // IWYU pragma: keep
#include "lyra/value/string.hpp"               // IWYU pragma: keep
#include "lyra/value/string_op.hpp"            // IWYU pragma: keep
#include "lyra/value/unpacked_array.hpp"       // IWYU pragma: keep
