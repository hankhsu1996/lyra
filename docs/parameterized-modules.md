# Parameterized Modules

How Lyra handles SystemVerilog parameterized modules through specialization-based compilation.

## slang Elaboration

slang creates a distinct `InstanceBodySymbol` for each module instance, even with identical parameters:

```
inner #(.VALUE(100)) inst1();  ->  inst1.body -> VariableSymbol("data") @ 0x1000
inner #(.VALUE(100)) inst2();  ->  inst2.body -> VariableSymbol("data") @ 0x2000
```

Each instance is a complete, independent scope with unique symbol pointers.

## Specialization Model

Lyra does not preserve slang's per-instance model in compiled artifacts. Instead, instances with the same structural parameters share a single compiled specialization:

```
ModuleSpecId = (ModuleDefId, StructuralFingerprint)
```

Two instances of the same module with different `BANK_ID` values but identical layout, generate structure, and process topology map to the **same** specialization. The `BANK_ID` value becomes a per-instance constant read from an `InstanceConstBlock`.

See [compilation-model.md](compilation-model.md) for the full parameter classification (structural vs value-only).

## Parameter Resolution

All parameters are resolved at elaboration time by slang:

- `logic [WIDTH-1:0]` where WIDTH=8 becomes a concrete type with `bit_width=8`
- Parameter values are embedded in the elaborated AST
- No runtime parameter handling needed

After elaboration, the compiler classifies each parameter:

- **Structural** (affects layout/generate) -> part of `StructuralFingerprint`
- **Value-only** (affects only computation) -> stored in `InstanceConstBlock`

## Hierarchical References

Hierarchical access (e.g., `inst2.data`) is resolved at assembly/link time, not at specialization compile time. The compiled specialization code addresses state via `this_base + offset`. Assembly maps instance paths to concrete memory locations.

This means hierarchical references do not prevent specialization sharing. Two instances of the same specialization have different `this_base` pointers but execute identical code.

## Related

- [compilation-model.md](compilation-model.md) -- specialization identity and parameter classification
- [module-hierarchy.md](module-hierarchy.md) -- hierarchy support and port connections
