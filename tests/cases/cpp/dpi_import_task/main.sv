module Top;
  import "DPI-C" task set_pair(input int a, output int b, output int c);
  import "DPI-C" task accumulate(input int delta, inout int total);
  import "DPI-C" task remember(input int x);
  import "DPI-C" task recall(output int y);
  int b;
  int c;
  int total;
  int got;
  initial begin
    set_pair(7, b, c);
    $display("b=%0d c=%0d", b, c);
    total = 100;
    accumulate(5, total);
    accumulate(5, total);
    $display("total=%0d", total);
    remember(42);
    recall(got);
    $display("got=%0d", got);
  end
endmodule
