module Top;
  // Three non-canonical declared ranges. Each const slice exercises a
  // distinct branch of the unpacked declared-range translation:
  // ascending non-zero base, descending, and negative base.
  int asc_nz   [3:5]  = '{30, 40, 50};
  int desc     [5:3]  = '{50, 40, 30};
  int neg_base [-1:1] = '{11, 22, 33};

  int sl_anz [3], sl_desc [3], sl_neg [3];
  int anz_e0, anz_e1, anz_e2;
  int desc_e0, desc_e1, desc_e2;
  int neg_e0, neg_e1, neg_e2;

  initial begin
    sl_anz = asc_nz[3:5];
    anz_e0 = sl_anz[0]; anz_e1 = sl_anz[1]; anz_e2 = sl_anz[2];

    sl_desc = desc[5:3];
    desc_e0 = sl_desc[0]; desc_e1 = sl_desc[1]; desc_e2 = sl_desc[2];

    sl_neg = neg_base[-1:1];
    neg_e0 = sl_neg[0]; neg_e1 = sl_neg[1]; neg_e2 = sl_neg[2];
  end
endmodule
