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

  initial begin
    p = new[2];
    p[0] = 99;
    p[1] = 99;
    p = '{1, 2, 3, 4};
  end
endmodule
