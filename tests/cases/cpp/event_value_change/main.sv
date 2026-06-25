// LRM 9.4.2: `@(expr)` with no edge qualifier wakes on any value change of a
// singular cell. Coverage: a real change wakes (changed); same-value writes do
// not wake while genuine changes do (same_value_count reaches 3 across the
// five writes to c2); any-change applies to a multi-bit vector (mb); and it is
// legal for non-integral singular types -- string, real, enum (sw / rw / ew).
typedef enum logic [1:0] {RED, GRN, BLU} col_t;

module Top;
  logic c1 = 0;
  logic c2 = 0;
  logic [7:0] data = 8'h00;
  string s = "x";
  real r = 0.0;
  col_t e = RED;

  int changed = 0;
  int same_value_count = 0;
  int mb = 0;
  int sw = 0;
  int rw = 0;
  int ew = 0;

  initial begin
    @(c1);
    changed = 1;
  end
  initial begin
    repeat (3) begin
      @(c2);
      same_value_count = same_value_count + 1;
    end
  end
  initial begin
    @(data);
    mb = 1;
  end
  initial begin
    @(s);
    sw = 1;
  end
  initial begin
    @(r);
    rw = 1;
  end
  initial begin
    @(e);
    ew = 1;
  end

  initial begin
    #5 c1 = 1;
    #5 data = 8'h42;
    #5;
    s = "y";
    r = 1.0;
    e = BLU;
  end
  initial begin
    #5 c2 = 0;
    #5 c2 = 1;
    #5 c2 = 1;
    #5 c2 = 0;
    #5 c2 = 1;
  end
endmodule
