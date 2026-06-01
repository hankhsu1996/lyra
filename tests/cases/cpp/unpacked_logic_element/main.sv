module Top;
  logic [7:0] a [3] = '{8'haa, 8'hbb, 8'hcc};
  initial begin
    $display("%0h", a[0]);
    $display("%0h", a[1]);
    $display("%0h", a[2]);
  end
endmodule
