// An index key in an array assignment pattern names the element its value
// lands at, every element is covered by a key or a default, and no index may
// be given twice (LRM 10.9.1). A dynamic array resized by an assignment takes
// as many elements as its source has (LRM 7.6), and for an index-keyed
// pattern the standard does not say how many that is. Lyra sizes the array to
// the elements the keys name, and places each value at its key whatever order
// the keys are written in. This case holds Lyra to that.
module Top;
  int keyed [];
  int keyed_out_of_order [];

  initial begin
    keyed = '{0: 10, 1: 20, 2: 30};
    keyed_out_of_order = '{1: 66, 0: 55};
  end

  final begin
    if (keyed.size() !== 3 || keyed[0] !== 10 || keyed[2] !== 30)
      $fatal(1, "a keyed pattern left %p, expected '{10, 20, 30}", keyed);
    if (keyed_out_of_order.size() !== 2 || keyed_out_of_order[0] !== 55 ||
        keyed_out_of_order[1] !== 66)
      $fatal(1, "keys out of order left %p, expected '{55, 66}",
             keyed_out_of_order);
    $display("All checks passed");
  end
endmodule
