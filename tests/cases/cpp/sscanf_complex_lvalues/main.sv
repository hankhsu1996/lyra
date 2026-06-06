module Top;
  // LRM 21.3.4.3: scan output args may be any writable lvalue. Today the
  // lowering restricts them to bare variable references; this case proves
  // bit-slice, packed struct field, unpacked element, and dynamic element
  // all work as output args.

  int unp[3];
  int dyn[];
  bit [15:0] v;
  struct packed {
    int a;
    int b;
  } s;

  int count_unp, count_dyn, count_slice, count_field;
  int unp_if;
  int count_tern, dyn_tern;

  initial begin
    dyn = new[2];

    // Statement position: complex lvalues.
    count_unp = $sscanf("10 20", "%d %d", unp[0], unp[1]);
    count_dyn = $sscanf("30 40", "%d %d", dyn[0], dyn[1]);
    count_slice = $sscanf("ab", "%h", v[7:0]);
    count_field = $sscanf("99 77", "%d %d", s.a, s.b);

    // Expression position: if-condition with complex lvalue output.
    if ($sscanf("5", "%d", unp[2])) unp_if = unp[2];

    // Ternary: only the taken arm's scan runs (C++ ternary is short-circuit).
    count_tern = (unp_if == 5) ? $sscanf("7", "%d", dyn[0]) : 0;
    dyn_tern = dyn[0];
  end
endmodule
