module Top;
  // LRM 7.8.1 wildcard index: any integral expression indexes the array, and
  // the key is the numerical value independent of the index expression's width.
  int w [*];

  int v5;
  int n_after_collapse;
  int v300;
  int n_two;
  int ex5;
  int ex7;
  int v_unsigned;
  int n_after_delete;

  initial begin
    // Two indices of the same value but different widths name the same entry
    // (LRM 7.8.1), so the second write overwrites the first.
    w[8'd5] = 100;
    w[16'd5] = 200;
    v5 = w[5];
    n_after_collapse = w.num();

    // A distinct value is a distinct entry.
    w[300] = 7;
    v300 = w[300];
    n_two = w.num();

    ex5 = w.exists(5);
    ex7 = w.exists(7);

    // The index is treated as unsigned (LRM 7.8.1): -1 and its unsigned
    // bit-pattern name the same entry.
    w[-1] = 42;
    v_unsigned = w[32'hFFFFFFFF];

    w.delete(5);
    n_after_delete = w.num();
  end
endmodule
