// A non-integral port drives the child cell like an integral one (LRM 23.3.3):
// the connection is a continuous assignment between the two objects' own
// storage, in either direction. Covers a string and an unpacked array, each
// connected as both an input and an output, with an integral output as the
// control. The output side re-triggers when the child re-drives it.
module Child(
    input string label, output string echo, input int din [2],
    output int dsum);
  always_comb echo = label;
  always_comb dsum = din[0] + din[1];
endmodule

module Top;
  string s;
  string back;
  int arr [2];
  int sum;
  Child u(.label(s), .echo(back), .din(arr), .dsum(sum));

  initial begin
    s = "hi";
    arr[0] = 3;
    arr[1] = 4;
    #5;
    s = "hello";
    arr[0] = 30;
  end

  final $display("back=%s sum=%0d", back, sum);
endmodule
