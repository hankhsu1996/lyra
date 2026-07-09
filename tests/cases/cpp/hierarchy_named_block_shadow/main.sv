// LRM 23.9: a named-block head resolves by the layout-visible downward route
// relative to the reader's scope, not by a unit-global label search. A block
// label reused in different structural scopes must resolve to the block in the
// reader's own scope for a bare name, and to the named scope's block for a
// hierarchical path. Generate block `g` and the module both declare a `blk`;
// a process inside `g` reads its own `g.blk` by bare name (must be 222) and the
// module's `blk` by absolute path (must be 111). Neither may bind the other's
// block. The module-level `blk` is declared after `g`, so its head identity
// must already exist when the reference inside `g` is lowered.

module Test;
  int local_read;
  int outer_read;

  if (1) begin : g
    initial begin : blk
      static int x = 222;
    end
    initial begin
      #1;
      local_read = blk.x;
      outer_read = Test.blk.x;
      $display("local=%0d outer=%0d", local_read, outer_read);
    end
  end

  initial begin : blk
    static int x = 111;
  end
endmodule
