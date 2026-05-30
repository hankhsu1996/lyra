module Top;
  byte a;
  byte b;
  bit [3:0] na;
  bit [3:0] nb;
  bit [39:0] inner_replicate;
  bit [15:0] nested_concat;
  bit [15:0] zero_replicate;

  initial begin
    a = 8'hAA;
    b = 8'hBB;
    na = 4'hA;
    nb = 4'hB;
    inner_replicate = {a, {3{b}}, a};
    nested_concat = {2{na, nb}};
    zero_replicate = {8'hAB, {0{8'hFF}}, 8'hCD};
  end
endmodule
