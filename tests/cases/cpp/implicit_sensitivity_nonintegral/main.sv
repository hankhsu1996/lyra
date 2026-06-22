// always_comb / `@*` / `wait` / continuous-assign derive sensitivity from the
// read set, which covers any value type -- each reacts to a non-integral write.
module Top;
  string cs;
  int clen;
  always_comb clen = cs.len();

  real rs;
  int rflag;
  always @* rflag = (rs > 2.0) ? 1 : 0;

  string ws;
  int wgot;
  initial begin
    ws = "no";
    wgot = 0;
    wait (ws == "go");
    wgot = 1;
  end

  string ds;
  string ds2;
  assign ds2 = ds;

  int ua[2];
  int usum;
  always_comb usum = ua[0] + ua[1];

  initial begin
    cs = "ab";
    rs = 0.0;
    ds = "lo";
    ua[0] = 1;
    ua[1] = 2;
    #5;
    cs = "abcdef";
    rs = 5.0;
    ws = "go";
    ds = "high";
    ua[0] = 10;
  end
endmodule
