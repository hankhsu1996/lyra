module Child;
  initial $display("child");
endmodule

module Top;
  for (genvar i = 0; i < 2; i = i + 1) begin : row
    Child c();
  end
  if (1) begin : g
    Child d();
  end
endmodule
