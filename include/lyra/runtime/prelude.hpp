#pragma once

// Umbrella header naming everything an emitted project may use, and the one
// header emitted code includes. It is also what the precompiled header
// pre-parses, so the set the PCH covers and the set the emit pulls in are the
// same set by construction rather than by two lists agreeing -- a header
// reachable from the emit but absent from the PCH would be re-parsed once per
// compiled design, and nothing would report it. Listing a header the emit does
// not use is harmless: its parse cost is paid once, when the PCH is built.
// Per-line `IWYU pragma: keep` marks the file's intent so include-cleaner
// tools do not flag the umbrella as having unused includes.

#include <array>       // IWYU pragma: keep
#include <cmath>       // IWYU pragma: keep
#include <cstdint>     // IWYU pragma: keep
#include <functional>  // IWYU pragma: keep
#include <memory>      // IWYU pragma: keep
#include <span>        // IWYU pragma: keep
#include <stdexcept>   // IWYU pragma: keep
#include <string>      // IWYU pragma: keep
#include <vector>      // IWYU pragma: keep

#include "lyra/runtime/ambient_run_context.hpp"  // IWYU pragma: keep
#include "lyra/runtime/coroutine.hpp"            // IWYU pragma: keep
#include "lyra/runtime/delay.hpp"                // IWYU pragma: keep
#include "lyra/runtime/design.hpp"               // IWYU pragma: keep
#include "lyra/runtime/diagnostic.hpp"           // IWYU pragma: keep
#include "lyra/runtime/distribution.hpp"         // IWYU pragma: keep
#include "lyra/runtime/dpi_context.hpp"          // IWYU pragma: keep
#include "lyra/runtime/event.hpp"                // IWYU pragma: keep
#include "lyra/runtime/file_table.hpp"           // IWYU pragma: keep
#include "lyra/runtime/finish.hpp"               // IWYU pragma: keep
#include "lyra/runtime/foreign_execution.hpp"    // IWYU pragma: keep
#include "lyra/runtime/fork.hpp"                 // IWYU pragma: keep
#include "lyra/runtime/gc_ref.hpp"               // IWYU pragma: keep
#include "lyra/runtime/hierarchy_segment.hpp"    // IWYU pragma: keep
#include "lyra/runtime/host_command.hpp"         // IWYU pragma: keep
#include "lyra/runtime/named_event.hpp"          // IWYU pragma: keep
#include "lyra/runtime/net.hpp"                  // IWYU pragma: keep
#include "lyra/runtime/process_control.hpp"      // IWYU pragma: keep
#include "lyra/runtime/process_kind.hpp"         // IWYU pragma: keep
#include "lyra/runtime/random.hpp"               // IWYU pragma: keep
#include "lyra/runtime/runtime.hpp"              // IWYU pragma: keep
#include "lyra/runtime/runtime_effects.hpp"      // IWYU pragma: keep
#include "lyra/runtime/runtime_process.hpp"      // IWYU pragma: keep
#include "lyra/runtime/scope.hpp"                // IWYU pragma: keep
#include "lyra/runtime/scope_exit.hpp"           // IWYU pragma: keep
#include "lyra/runtime/sim_time.hpp"             // IWYU pragma: keep
#include "lyra/runtime/simulation_entry.hpp"     // IWYU pragma: keep
#include "lyra/runtime/stream_dispatcher.hpp"    // IWYU pragma: keep
#include "lyra/runtime/trigger.hpp"              // IWYU pragma: keep
#include "lyra/runtime/var.hpp"                  // IWYU pragma: keep
#include "lyra/value/array_case_equal.hpp"       // IWYU pragma: keep
#include "lyra/value/associative_array.hpp"      // IWYU pragma: keep
#include "lyra/value/chandle.hpp"                // IWYU pragma: keep
#include "lyra/value/conditional_select.hpp"     // IWYU pragma: keep
#include "lyra/value/dpi_canonical.hpp"          // IWYU pragma: keep
#include "lyra/value/dpi_open_array.hpp"         // IWYU pragma: keep
#include "lyra/value/dynamic_array.hpp"          // IWYU pragma: keep
#include "lyra/value/format.hpp"                 // IWYU pragma: keep
#include "lyra/value/integral_format.hpp"        // IWYU pragma: keep
#include "lyra/value/packed.hpp"                 // IWYU pragma: keep
#include "lyra/value/packed_array.hpp"           // IWYU pragma: keep
#include "lyra/value/packed_bitwise.hpp"         // IWYU pragma: keep
#include "lyra/value/packed_convert.hpp"         // IWYU pragma: keep
#include "lyra/value/packed_reduction.hpp"       // IWYU pragma: keep
#include "lyra/value/packed_type.hpp"            // IWYU pragma: keep
#include "lyra/value/queue.hpp"                  // IWYU pragma: keep
#include "lyra/value/real.hpp"                   // IWYU pragma: keep
#include "lyra/value/require.hpp"                // IWYU pragma: keep
#include "lyra/value/scan.hpp"                   // IWYU pragma: keep
#include "lyra/value/string.hpp"                 // IWYU pragma: keep
#include "lyra/value/string_op.hpp"              // IWYU pragma: keep
#include "lyra/value/tagged_union.hpp"           // IWYU pragma: keep
#include "lyra/value/tuple.hpp"                  // IWYU pragma: keep
#include "lyra/value/union.hpp"                  // IWYU pragma: keep
#include "lyra/value/unpacked_array.hpp"         // IWYU pragma: keep
