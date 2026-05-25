module Top;
  initial begin
    $display("on stdout");
    $warning("on stderr");
    $display("still on stdout");
  end
endmodule
