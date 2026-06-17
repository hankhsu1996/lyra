module Top;
  // Jagged dynamic-of-dynamic: each row has its own length, so the inner bound
  // is sampled per outer iteration. A single flat counter cannot iterate this;
  // the nested-loop lowering (LRM 12.7.3) takes each dimension's size at its
  // own level.
  int jag [][];
  int sum_jag;

  // A dynamic outer dimension with a fixed inner dimension.
  int rect [][2];
  int sum_rect;

  // A fixed outer dimension with a dynamic inner dimension: the inner bound is
  // `fixouter[i].size()`, still sampled per outer iteration.
  int fixouter [3][];
  int sum_fixouter;

  // break must leave every nested dimension (LRM 12.8), even when the bounds
  // are runtime.
  int sum_break;

  initial begin
    jag = new[3];
    jag[0] = '{1, 2};
    jag[1] = '{3, 4, 5, 6};
    jag[2] = '{7};
    sum_jag = 0;
    foreach (jag[i, j]) sum_jag = sum_jag + jag[i][j];

    rect = new[2];
    rect[0] = '{10, 20};
    rect[1] = '{30, 40};
    sum_rect = 0;
    foreach (rect[i, j]) sum_rect = sum_rect + rect[i][j];

    fixouter[0] = '{1, 2};
    fixouter[1] = '{10, 20, 30};
    fixouter[2] = '{100};
    sum_fixouter = 0;
    foreach (fixouter[i, j]) sum_fixouter = sum_fixouter + fixouter[i][j];

    sum_break = 0;
    foreach (jag[i, j]) begin
      if (jag[i][j] == 4) break;
      sum_break = sum_break + jag[i][j];
    end
  end
endmodule
