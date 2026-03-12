# Execution Modes

How Lyra compiles and runs SystemVerilog designs. Both modes share a single
codegen pipeline and diverge only at the final step.

## Modes

**AOT (artifact mode)** -- default, production path.

Compile to a native executable with the runtime linked in, then run it.

```
MIR -> LLVM IR -> object file -> link -> <dir>/<name>
```

**JIT (hosted mode)** -- fast iteration, interactive tooling, future tiered compilation.

Compile to LLVM IR in-process, JIT-link, and call the entry point directly.
No artifact on disk.

```
MIR -> LLVM IR -> ORC JIT -> in-memory execution
```

## Shared pipeline

Both AOT and JIT consume compiled specialization artifacts. The codegen shape
contract (init patterns, process functions, runtime calls) is identical across
modes. LLVM compilation is per-specialization; realization produces the design-level
tables. The only mode difference is `MainAbi`:

- `kArgvForwarding` (AOT): `main(argc, argv)` forwards CLI args as plusargs,
  resolves fs_base_dir from executable directory
- `kEmbeddedPlusargs` (JIT): `main()` with plusargs baked into IR as global
  string constants

Fixing codegen shape (memset/memcpy init, process sharing, IR size) benefits
both modes equally. There is no AOT-only or JIT-only IR path.

## CLI semantics

`lyra run` always means "execute". The backend is an implementation detail.

```
lyra run [files...]              # Execute (AOT, default)
lyra run --backend=jit [files]   # Execute (JIT)
lyra compile [files...]          # Produce artifact only (no execution)
```

AOT `run` compiles to a temp directory, executes the binary, and cleans up.
`compile` produces a persistent executable at the specified output directory.

### Plusargs

For AOT standalone executables, plusargs are passed directly as CLI arguments:

```
lyra compile -o out/ design.sv
out/design +TEST=foo +SEED=42
```

For `lyra run`, plusargs go after `--`:

```
lyra run design.sv -- +TEST=foo +SEED=42
```

## AOT output layout

`lyra compile -o <dir>` produces a single self-contained executable:

```
<dir>/<name>                    # Native executable (runtime linked in)
```

The Lyra runtime archive is linked into the binary by normal archive linking,
so no shared library or rpath is needed. The binary still dynamically links
libc, libstdc++, libm, and libpthread.

fs_base_dir resolution for file I/O (`$readmemh`, etc.):

1. `LYRA_FS_BASE_DIR` env var (internal, set by `lyra run` for temp dirs)
2. Directory of the executable (dirname of `argv[0]`)
3. Current working directory (fallback)

## JIT future directions

Both AOT and JIT are production modes. AOT is the default; JIT enables
workflows that AOT cannot:

- **Tiered compilation**: interpret or use bytecode by default, JIT-compile
  hot processes when triggered
- **REPL / interactive debug**: modify design, re-run to a specific time point
  without full recompilation
- **Profile-guided specialization**: specialize codegen for observed parameter
  values, widths, or paths in a particular run
