// LRM 9.4.2: `@(expr)` with no edge is any-change on a singular value cell --
// legal for string / real / enum, not only packed bit vectors.
typedef enum logic [1:0] { RED, GRN, BLU } col_t;

module Top;
  string s;
  real r;
  col_t c;
  int sw, rw, cw;

  initial begin
    s = "x";
    sw = 0;
    @(s);
    sw = 1;
  end

  initial begin
    r = 0.0;
    rw = 0;
    @(r);
    rw = 1;
  end

  initial begin
    c = RED;
    cw = 0;
    @(c);
    cw = 1;
  end

  initial begin
    #5;
    s = "y";
    r = 1.0;
    c = BLU;
  end
endmodule
