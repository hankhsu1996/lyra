module Top;
  bit [3:0] a;
  bit [3:0] b;
  bit [3:0] c;
  bit [3:0] d;
  bit [15:0] result;

  initial begin
    a = 4'h1;
    b = 4'h2;
    c = 4'h3;
    d = 4'h4;
    result = {{a, b}, {c, d}};
  end
endmodule
