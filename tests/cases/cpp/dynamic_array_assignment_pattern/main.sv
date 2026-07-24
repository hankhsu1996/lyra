module Top;
  // 1D positional declaration initializer.
  int a [] = '{10, 20, 30};
  // 1D replicated declaration initializer.
  int r [] = '{4{7}};
  // 2D jagged: each inner pattern sets its row's runtime size.
  int j [][] = '{'{1, 2}, '{3, 4, 5}};
  // 2D replicated nested inside the outer replication.
  int m [][] = '{3{'{8, 9}}};

  // Procedural assignment with resize: pre-sized destination is resized to the
  // pattern's element count, overwriting the prior contents (LRM 7.6).
  int p [];

  // Index-keyed form: the keys name element positions, and the frontend admits
  // the form only when they cover a dense 0..N-1 set, so the pattern is the
  // positional one placed by key. Source order is therefore not element order --
  // `k` lists key 1 first and must still land 55 at index 0.
  int i [];
  int k [];

  initial begin
    p = new[2];
    p[0] = 99;
    p[1] = 99;
    p = '{1, 2, 3, 4};

    i = '{0: 10, 1: 20, 2: 30};
    k = '{1: 77, 0: 55};
  end
endmodule
