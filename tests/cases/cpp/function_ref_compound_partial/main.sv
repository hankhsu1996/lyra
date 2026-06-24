module Top;
  int p;
  int q;

  // Compound assignment through a ref formal: routes through the cell's
  // partial-write proxy (LRM 11.4 op-assign over a ref, LRM 13.5.2).
  function automatic void add_into(ref int x);
    x += 7;
  endfunction

  // Partial (bit-select) write through a ref formal: the selector chain roots
  // in the ref and lands in the mutation snapshot.
  function automatic void set_low_nibble(ref int x);
    x[3:0] = 4'hf;
  endfunction

  initial begin
    p = 10;
    add_into(p);
    q = 0;
    set_low_nibble(q);
  end
endmodule
