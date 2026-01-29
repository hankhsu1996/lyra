# Parameterized Modules

How Lyra handles SystemVerilog parameterized modules via slang's elaboration model.

## slang's Per-Instance Elaboration

slang creates a distinct `InstanceBodySymbol` for **each module instance**, even with identical parameters:

```
inner #(.VALUE(100)) inst1();  ->  inst1.body -> VariableSymbol("data") @ 0x1000
inner #(.VALUE(100)) inst2();  ->  inst2.body -> VariableSymbol("data") @ 0x2000
```

Each instance is a complete, independent scope with unique symbol pointers.

## Why Per-Instance IR Matters

MIR preserves slang's per-instance model. This is essential for:

- **Hierarchical references**: `inst2.data` must resolve using `inst2`'s specific symbols
- **Interpreter correctness**: Instance contexts use symbol pointers for O(1) lookup
- **Module linking**: `SubmoduleInstance` links to child modules by instance symbol pointer

If we deduplicated at IR construction time (keeping only `inst1`'s module), hierarchical access to `inst2` would fail because the symbol pointers wouldn't match.

## Parameter Resolution

All parameters are resolved at elaboration time by slang:

- `logic [WIDTH-1:0]` where WIDTH=8 becomes a concrete type with `bit_width=8`
- Parameter values are embedded in the elaborated AST
- No runtime parameter handling needed

## Related

- `docs/string-types.md` - How string literals are typed and tracked
