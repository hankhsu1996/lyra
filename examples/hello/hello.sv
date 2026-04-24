// Simple example: Hello World with $display
module Top;
  int x = 42;
  initial begin
    $display("Hello, World!");
    $display("x = ", x);
    $finish;
  end
endmodule
