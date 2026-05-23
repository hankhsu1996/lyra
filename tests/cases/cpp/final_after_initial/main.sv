module Top;
  int x;

  initial begin
    x = 1;
    $display("initial x=%0d", x);
    x = 2;
  end

  final begin
    $display("final x=%0d", x);
  end
endmodule
