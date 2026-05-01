module Top;
  logic [7:0] v;

  initial begin
    v = 8'bzzzzzzzz;
    $display("%h", v);
  end
endmodule
