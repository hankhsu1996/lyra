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
  int a0, a1, a2;
  int r0, r3;
  int j00, j01, j10, j11, j12;
  int m00, m01, m10, m11, m20, m21;
  int p0, p1, p2, p3;

  initial begin
    p = new[2];
    p[0] = 99;
    p[1] = 99;
    p = '{1, 2, 3, 4};

    a0 = a[0]; a1 = a[1]; a2 = a[2];
    r0 = r[0]; r3 = r[3];
    j00 = j[0][0]; j01 = j[0][1];
    j10 = j[1][0]; j11 = j[1][1]; j12 = j[1][2];
    m00 = m[0][0]; m01 = m[0][1];
    m10 = m[1][0]; m11 = m[1][1];
    m20 = m[2][0]; m21 = m[2][1];
    p0 = p[0]; p1 = p[1]; p2 = p[2]; p3 = p[3];
  end
endmodule
