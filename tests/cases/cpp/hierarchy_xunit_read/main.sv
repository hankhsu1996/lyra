module Child;
  int x;
  wire [7:0] w;
  assign w = 8'd5;
  initial x = 42;
endmodule

module Top;
  Child c();
  int r;
  logic [7:0] rw;
  initial begin
    #1;
    r = c.x;
    rw = c.w;
    $display("r=%0d rw=%0d", r, rw);
  end
endmodule
