module Comb(input int a, output int b);
  always_comb b = a + 1;
endmodule

module Counter(input bit clk, output int count);
  always_ff @(posedge clk) count <= count + 1;
endmodule

module Top;
  int src, combed;
  bit clk;
  int cnt;
  Comb u(.a(src), .b(combed));
  Counter ctr(.clk(clk), .count(cnt));
  initial begin
    src = 5;
    clk = 0;
    #1;
    $display("%0d", combed);
    clk = 1;
    #1;
    clk = 0;
    #1;
    clk = 1;
    #1;
    $display("%0d", cnt);
  end
endmodule
