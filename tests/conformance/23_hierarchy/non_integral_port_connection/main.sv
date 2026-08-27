// Values of all data types can be passed through ports, because a port
// connection only requires the two sides to be assignment compatible
// (LRM 23.3.3). An input port takes any expression of a compatible data type
// and an output port drives any variable of one (LRM 23.3.3.2), so a string
// and an unpacked array cross a port in either direction on the same terms an
// integral value does, and the continuous assignment on each side re-evaluates
// when its source changes.
module Child(
    input string label, output string echo, input int din [2],
    output int dsum);
  always_comb echo = label;
  always_comb dsum = din[0] + din[1];
endmodule

module Top;
  string sent;
  string returned;
  int arr [2];
  int sum;

  Child u(.label(sent), .echo(returned), .din(arr), .dsum(sum));

  initial begin
    sent = "hi";
    arr[0] = 3;
    arr[1] = 4;
    #5;
    sent = "hello";
    arr[0] = 30;
  end

  final begin
    if (returned != "hello")
      $fatal(1, "returned was '%s', expected 'hello'", returned);
    if (sum !== 34) $fatal(1, "sum was %0d, expected 34", sum);
    $display("All checks passed");
  end
endmodule
