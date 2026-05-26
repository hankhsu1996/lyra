module Top;
  logic signed [7:0] add_;
  logic signed [7:0] sub_;
  logic signed [7:0] sub_neg;
  logic signed [7:0] mul_;
  logic signed [7:0] mul_neg;
  logic signed [7:0] div_pos;
  logic signed [7:0] div_neg;
  logic signed [7:0] mod_;
  initial begin
    logic signed [7:0] a;
    logic signed [7:0] b;
    a = 30; b = 12;
    add_ = a + b;
    sub_ = a - b;
    sub_neg = b - a;
    a = 5; b = 7;
    mul_ = a * b;
    a = b - 12;
    mul_neg = a * b;
    a = 100; b = 4;
    div_pos = a / b;
    a = b - 104;
    div_neg = a / b;
    a = 17; b = 5;
    mod_ = a % b;
  end
endmodule
