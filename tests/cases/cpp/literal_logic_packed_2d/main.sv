module Top;
  logic [3:0][7:0] p;
  initial begin
    p = 32'hdeadbeef;
    $display("%b", p);
  end
endmodule
