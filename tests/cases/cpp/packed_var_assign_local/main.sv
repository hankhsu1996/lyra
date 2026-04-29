module Top;
  logic [7:0] out;

  initial begin
    logic [7:0] tmp;
    tmp = 8'b10100101;
    out = tmp;
    $display("%b", out);
  end
endmodule
