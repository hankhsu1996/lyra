// A block label is resolved against the scopes that enclose the reference, not
// by searching the design for that label: an identifier written without a
// hierarchical path is looked for locally and then outward through the
// containing scopes (LRM 23.9), while a path that begins at a top-level module
// names one scope and no other (LRM 23.6). The same label may therefore be
// declared in a generate block and again in the module body, and a reference
// inside the generate block reaches its own block by the bare label and the
// module's block by the rooted path -- neither binding to the other's block,
// and the module's block resolving even though it stands later in the source.
module Top;
  int local_read;
  int rooted_read;

  if (1) begin : g
    initial begin : blk
      static int x = 222;
    end

    initial begin
      #1;
      local_read = blk.x;
      rooted_read = Top.blk.x;
    end
  end

  initial begin : blk
    static int x = 111;
  end

  final begin
    if (local_read !== 222)
      $fatal(1, "blk.x read inside g was %0d, expected 222", local_read);
    if (rooted_read !== 111)
      $fatal(1, "Top.blk.x was %0d, expected 111", rooted_read);
    $display("All checks passed");
  end
endmodule
