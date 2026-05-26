module Top;
  logic eq_, neq_;
  logic lt_, le_, gt_, ge_;
  logic eq_xz, neq_xz;
  logic lt_xz, le_xz, gt_xz, ge_xz;
  initial begin
    logic signed [7:0] a;
    logic signed [7:0] b;
    a = 5; b = 5;
    eq_ = (a == b);
    a = 5; b = 7;
    neq_ = (a != b);
    a = -3; b = 4;
    lt_ = (a < b);
    a = 4; b = 4;
    le_ = (a <= b);
    a = 7; b = 2;
    gt_ = (a > b);
    a = -1; b = -1;
    ge_ = (a >= b);
    a = 8'b000000xz; b = 8'b00000010;
    eq_xz = (a == b);
    neq_xz = (a != b);
    lt_xz = (a < b);
    le_xz = (a <= b);
    gt_xz = (a > b);
    ge_xz = (a >= b);
  end
endmodule
