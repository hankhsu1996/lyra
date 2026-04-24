# Benchmark Result Schema (v3)

Schema for `bench.json` output produced by `run_benchmarks.py`.

## Top-level object

| Field            | Type   | Description                                                                                      |
| ---------------- | ------ | ------------------------------------------------------------------------------------------------ |
| `schema_version` | int    | Always `3`.                                                                                      |
| `git`            | string | Short git SHA of the tested commit.                                                              |
| `tier`           | string | Tier name used for the run (`pr`, `nightly`).                                                    |
| `profile`        | string | Scale profile selected by the tier (e.g., `small`, `large`). Applies to all results in this run. |
| `timestamp`      | string | ISO 8601 UTC timestamp of the run.                                                               |
| `results`        | array  | Array of result objects (one per fixture/backend).                                               |

## Result object

All fields are always present. Fields that do not apply to a given backend use their zero value
(`0`, `0.0`, `""`, `{}`) as a sentinel -- never omitted.

### Identity fields

| Field         | Type        | Description                                                          |
| ------------- | ----------- | -------------------------------------------------------------------- |
| `fixture`     | string      | Globally unique fixture name (matches `benchmark.name`).             |
| `category`    | string      | Top-level category (`simulation-engine`, `process-kernel`).          |
| `subcategory` | string/null | Optional subcategory (e.g., `memory`). Null if absent.               |
| `backend`     | string      | One of: `aot`, `aot-two-state`, `jit`, `verilator`. No other values. |

### Metadata fields

| Field       | Type     | Description                                                                                             |
| ----------- | -------- | ------------------------------------------------------------------------------------------------------- |
| `intent`    | string   | Complete sentence: what regression this fixture protects against.                                       |
| `focus`     | string   | Dimension being measured. Open string field; must be non-empty. Currently all fixtures use `"runtime"`. |
| `primary`   | string   | One-line description of the main workload characteristic.                                               |
| `secondary` | string[] | Secondary workload characteristics being exercised.                                                     |

Field semantics (pinned):

- `focus` -- the measurement dimension. An open string, not an enum. Currently always `"runtime"`
  for simulation time benchmarks. Future fixtures may introduce other values (e.g., `"compile"` for
  compile-only benchmarks). Consumers should handle unknown values gracefully.
- `primary` -- describes the dominant workload characteristic of this fixture. Answers "what is this
  fixture mainly stressing?"
- `secondary` -- lists additional workload characteristics that are exercised but are not the
  primary focus. Useful for understanding what else might regress.

### Measurement fields

All measurement fields are always present. When a metric does not apply to a backend, the zero value
is emitted (not omitted).

| Field        | Type  | Description                                        | Backend notes                                       |
| ------------ | ----- | -------------------------------------------------- | --------------------------------------------------- |
| `compile_s`  | float | Compile time in seconds.                           | All backends.                                       |
| `sim_s`      | float | Simulation time in seconds.                        | All backends.                                       |
| `wall_s`     | float | Wall-clock time (compile + sim) in seconds.        | All backends.                                       |
| `llvm_insts` | int   | Number of LLVM IR instructions in compiled output. | Lyra only. `0` for `verilator`.                     |
| `mir_stmts`  | int   | Number of MIR statements.                          | Lyra only. `0` for `verilator`.                     |
| `binary_kb`  | int   | Binary size in KB.                                 | `aot`, `aot-two-state`, `verilator`. `0` for `jit`. |
| `rss_max_mb` | float | Peak RSS in MB.                                    | All backends.                                       |

### Detail fields

| Field      | Type   | Description                                                                                                                                                   |
| ---------- | ------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `params`   | object | Resolved scale parameters (key-value pairs).                                                                                                                  |
| `phases`   | object | Per-phase timing breakdown (key: phase, value: seconds). Empty object for `verilator`.                                                                        |
| `counters` | object | Always an empty object (`{}`). Reserved for future per-fixture counters. Present in JSON output for forward compatibility; not declared in fixture manifests. |
| `error`    | string | Error message if the run failed; empty string if OK.                                                                                                          |

## Fixture identity

Fixture names are globally unique across all categories and subcategories. Identity is the `fixture`
field (the name), not the directory path. Two fixtures in different categories must not share a
name.

## Compatibility rules

- Consumers must check `schema_version` before parsing.
- Fields may be added in future versions; consumers should ignore unknown fields.
- Field removal or type changes require a schema version bump.
- The `version` and `family` keys from schema v2 are removed and must not appear.
