module Top;
  logic signed [7:0] a;
  logic [2:0] s;
  logic [7:0] y;
  initial begin
    y = a << s;
    y = a <<< s;
    y = a >> s;
    y = a >>> s;
  end
endmodule
