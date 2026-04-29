module Top;
  logic signed [3:0] s;
  logic signed [7:0] w;

  initial begin
    s = 4'b1x01;
    w = s;
    $display("%b", w);
  end
endmodule
