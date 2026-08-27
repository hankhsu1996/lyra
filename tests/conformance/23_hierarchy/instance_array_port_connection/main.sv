// A port connection written once on an array of instances is distributed over
// the elements (LRM 23.3.3.5). When the connection's size and type match a
// single instance's port, the same connection is made to every element; when
// it is an unpacked array whose slowest varying dimensions match the instance
// array's dimensions, its elements are matched to the elements of the instance
// array left index to left index. Either way what each element receives is the
// continuous assignment a scalar instance would get, in both directions and
// for an array of more than one dimension.
module Adder(input int a, output int b);
  always_comb b = a + 1;
endmodule

module Top;
  int per_element_in [3];
  int per_element_out [3];
  Adder u [3] (.a(per_element_in), .b(per_element_out));

  int shared_in;
  int replicated_out [2];
  Adder r [2] (.a(shared_in), .b(replicated_out));

  int grid_in [2][2];
  int grid_out [2][2];
  Adder g [2][2] (.a(grid_in), .b(grid_out));

  initial begin
    for (int i = 0; i < 3; i++) per_element_in[i] = i * 10;
    shared_in = 7;
    for (int i = 0; i < 2; i++)
      for (int j = 0; j < 2; j++) grid_in[i][j] = (i * 2 + j) * 100;
  end

  final begin
    if (per_element_out[0] !== 1)
      $fatal(1, "per_element_out[0] was %0d, expected 1", per_element_out[0]);
    if (per_element_out[1] !== 11)
      $fatal(1, "per_element_out[1] was %0d, expected 11", per_element_out[1]);
    if (per_element_out[2] !== 21)
      $fatal(1, "per_element_out[2] was %0d, expected 21", per_element_out[2]);

    if (replicated_out[0] !== 8)
      $fatal(1, "replicated_out[0] was %0d, expected 8", replicated_out[0]);
    if (replicated_out[1] !== 8)
      $fatal(1, "replicated_out[1] was %0d, expected 8", replicated_out[1]);

    if (grid_out[0][0] !== 1)
      $fatal(1, "grid_out[0][0] was %0d, expected 1", grid_out[0][0]);
    if (grid_out[0][1] !== 101)
      $fatal(1, "grid_out[0][1] was %0d, expected 101", grid_out[0][1]);
    if (grid_out[1][0] !== 201)
      $fatal(1, "grid_out[1][0] was %0d, expected 201", grid_out[1][0]);
    if (grid_out[1][1] !== 301)
      $fatal(1, "grid_out[1][1] was %0d, expected 301", grid_out[1][1]);
    $display("All checks passed");
  end
endmodule
