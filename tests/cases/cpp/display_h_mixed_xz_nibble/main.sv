module Top;
  logic [3:0] v;
  initial begin
    v = 4'b1xz0;
    $display("%h", v);
  end
endmodule
