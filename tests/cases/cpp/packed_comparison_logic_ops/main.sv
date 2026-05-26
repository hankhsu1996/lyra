module Top;
  logic eq_, neq_;
  logic lt_, le_, gt_, ge_;
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
  end
endmodule
