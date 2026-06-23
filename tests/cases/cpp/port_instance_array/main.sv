// A port connection on an array of instances drives each element's own cell
// (LRM 23.3.3.5). slang distributes the connection per element: when the
// connection's size matches a single port it replicates to every element; when
// it is an array matching the instance-array dimensions it maps element to
// element. Either way each element's port is the same implied continuous
// assignment a scalar instance gets, in either direction, including a
// multidimensional array.
module Adder(input int a, output int b);
  always_comb b = a + 1;
endmodule

module Test;
  int in1 [3];
  int o1 [3];
  int shared;
  int o2 [2];
  int in2 [2][2];
  int o3 [2][2];

  Adder u [3] (.a(in1), .b(o1));
  Adder r [2] (.a(shared), .b(o2));
  Adder g [2][2] (.a(in2), .b(o3));

  initial begin
    for (int i = 0; i < 3; i++) in1[i] = i * 10;
    shared = 7;
    for (int i = 0; i < 2; i++)
      for (int j = 0; j < 2; j++) in2[i][j] = (i * 2 + j) * 100;
  end

  final begin
    $display("o1=%0d,%0d,%0d", o1[0], o1[1], o1[2]);
    $display("o2=%0d,%0d", o2[0], o2[1]);
    $display("o3=%0d,%0d,%0d,%0d", o3[0][0], o3[0][1], o3[1][0], o3[1][1]);
  end
endmodule
