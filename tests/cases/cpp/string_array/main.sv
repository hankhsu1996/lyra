module Top;
  // LRM 7.4 / Table 6-7: `string` is a value type, so it is a legal element of
  // an unpacked array (dynamic, queue, fixed). The OOB shield default is the
  // empty string `""`.
  string sa[] = '{"alpha", "beta", "gamma"};
  string sa2[] = '{"alpha", "beta", "gamma"};
  string sq[$] = '{"one", "two"};
  string sf[2] = '{"x", "y"};

  string written[];
  string oob;
  bit eq;

  initial begin
    written = sa;
    written[1] = "BETA";
    sq.push_back("three");
    // LRM 7.4.5: read on an invalid index returns the element default ("").
    oob = sa[99];
    eq = (sa == sa2);
  end
endmodule
