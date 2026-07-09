// LRM 21.6 command-line plusargs, with CLI tokens supplied. Cover the
// prefix-only $test$plusargs form, the value form across every integral
// conversion (%d, %h, %o, %b) and %s, and a $test$plusargs miss to prove
// no false-positive against unrelated tokens.
module Top;
  int    test_hit_bool;
  int    test_hit_prefix;
  int    test_hit_miss;
  int    hit_d;
  int    hit_h;
  int    hit_o;
  int    hit_b;
  int    hit_s;
  int    d_out;
  int    h_out;
  int    o_out;
  logic [7:0] b_out;
  string s_out;

  initial begin
    test_hit_bool   = $test$plusargs("BOOL_ONLY");
    test_hit_prefix = $test$plusargs("INT=");
    test_hit_miss   = $test$plusargs("NOT_HERE");

    hit_d = $value$plusargs("INT=%d", d_out);
    hit_h = $value$plusargs("HEX=%h", h_out);
    hit_o = $value$plusargs("OCT=%o", o_out);
    hit_b = $value$plusargs("BIN=%b", b_out);
    hit_s = $value$plusargs("STR=%s", s_out);
  end
endmodule
