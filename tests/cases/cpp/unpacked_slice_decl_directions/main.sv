module Top;
  // Three non-canonical declared ranges. Each const slice exercises a
  // distinct branch of the unpacked declared-range translation:
  // ascending non-zero base, descending, and negative base.
  int asc_nz   [3:5]  = '{30, 40, 50};
  int desc     [5:3]  = '{50, 40, 30};
  int neg_base [-1:1] = '{11, 22, 33};

  int sl_anz [3], sl_desc [3], sl_neg [3];

  initial begin
    sl_anz = asc_nz[3:5];
    sl_desc = desc[5:3];
    sl_neg = neg_base[-1:1];
  end
endmodule
