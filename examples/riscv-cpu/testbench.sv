module testbench
  import riscv_package::*;
();

  logic clk = 0;
  logic rst_n = 0;

  always #5 clk = ~clk;

  cpu processor (
      .clk   (clk),
      .rst_n (rst_n)
  );

  initial begin
    #20 rst_n = 1;

    repeat (100) @(posedge clk);

    $display("Test: Sum of 1..10");
    $display("x1 (sum) = %0d", processor.regfile.registers[1]);
    $display("x2 (i)   = %0d", processor.regfile.registers[2]);
    $display("x3 (res) = %0d", processor.regfile.registers[3]);
    $display("x4 (lim) = %0d", processor.regfile.registers[4]);

    if (processor.regfile.registers[3] == 55)
      $display("PASS: Result is 55");
    else
      $display("FAIL: Expected 55, got %0d", processor.regfile.registers[3]);

    $finish;
  end

endmodule
