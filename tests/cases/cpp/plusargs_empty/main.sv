// LRM 21.6 command-line plusargs, no CLI tokens available: every query
// returns 0 and $value$plusargs leaves its output untouched. Covers both
// system functions and the format-specifier surface the test can reach
// without CLI plumbing (which arrives in a later cut).
module Top;
  int    test_hit;
  int    value_hit_s;
  int    value_hit_d;
  int    value_hit_b;
  string s_out;
  int    d_out;
  logic [7:0] b_out;

  initial begin
    // Sentinels prove no write occurs on 0 return (LRM 21.6: "the
    // variable provided is not modified").
    s_out = "sentinel";
    d_out = 42;
    b_out = 8'hA5;

    test_hit    = $test$plusargs("nothing_matches");
    value_hit_s = $value$plusargs("nothing_matches=%s", s_out);
    value_hit_d = $value$plusargs("nothing_matches=%d", d_out);
    value_hit_b = $value$plusargs("nothing_matches=%b", b_out);
  end
endmodule
