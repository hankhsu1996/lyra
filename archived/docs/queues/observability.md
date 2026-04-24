# Observability

> **Queue rules.** Checkboxes at top for progress. Unchecked = gap. Finished items: remove the section, keep one checked line. Every unchecked item must have a short tag/ID for tracking (e.g. O2). Each item should be independently addressable -- it can be investigated and landed as a standalone change. No code in items (no function names, variable names, snippets). Items describe what the gap is, where to look, and why it matters -- enough to re-investigate from scratch. Investigation logs and design details belong in conversation history, not here.

Debugging, tracing, and inspection tooling for simulation correctness and scheduling efficiency.

## North Star

When a design produces wrong output or converges slowly, the developer can quickly identify the root cause without manual $display bisection.

## Progress

- [x] O1: Activation trace (`--trace-activations`, ring buffer + live stderr)
- [x] Signal trace architecture (metadata registry, selection registry, trace manager, sink dispatch)
- [x] Text signal trace sink (`--trace-signals`, end-of-time-slot committed values)
- [x] Runtime stats tiers (`-vv` core counters, `-vvv` detailed per-element counters)
- [ ] O2: VCD waveform sink (`--trace-vcd`, GTKWave-compatible output)
- [ ] Control surface (`$dumpfile` / `$dumpvars` SV task support, CLI scoping)

## O2: VCD waveform sink

Standard RTL debugging workflow. Enables side-by-side comparison with Verilator traces in GTKWave.

A VCD trace sink implementing the existing sink interface. Header emission from trace metadata, initial dump for selected signals, value changes from the existing stream. VCD-specific state (identifier allocation, file format rules) stays in the sink.

First milestone: `$dumpfile` / `$dumpvars` producing valid VCD readable in GTKWave.

## Control surface

User-facing options for trace control: `--trace-vcd=FILE`, `$dumpfile` / `$dumpvars` SystemVerilog task support, scope-based selection. Not blocked on full SV dump task semantics -- the runtime core already supports the needed primitives.
