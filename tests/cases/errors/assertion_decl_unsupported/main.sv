module Top;
  logic clk = 1'b0;
  logic a = 1'b0;

  checker Watch(logic x);
    assert property (@(posedge clk) x);
  endchecker

  Watch watch_inst(a);
endmodule
