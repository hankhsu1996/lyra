module Top;
  logic [7:0] a;
  logic [7:0] b;
  logic [7:0] c;
  logic [7:0] d;
  initial begin
    a = '0;
    b = '1;
    c = 'x;
    d = 'z;
    $display("%b", a);
    $display("%b", b);
    $display("%b", c);
    $display("%b", d);
  end
endmodule
