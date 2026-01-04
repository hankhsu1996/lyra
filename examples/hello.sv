// Simple example: Hello World with $display
module Top;
  int x;
  initial begin
    x = 42;  // TODO: initializer `int x = 42` not yet supported
    $display("Hello, World!");
    $display("x = ", x);
    $finish;
  end
endmodule
