// One packed value, filled, added to and written in part. N is its width in
// bits, which nothing the program does repeats: each statement is stated once
// whatever the width.
module Top #(parameter int N = 4);
  logic [N-1:0] a;
  logic [N-1:0] b;

  initial begin
    a = '0;
    b = '1;
    a = a + b;
    a[N-1 -: 2] = b[1:0];
  end
endmodule
