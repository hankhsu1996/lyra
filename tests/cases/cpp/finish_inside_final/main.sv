module Top;
  final begin
    $display("before-finish");
    $finish;
    $display("after-finish");
  end
endmodule
