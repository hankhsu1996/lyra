// LRM 18.16 makes randcase weights non-negative integral values and says
// nothing of one holding an x or z bit. Lyra takes such a weight as zero, as
// LRM 12.7.2 takes an unknown repeat count and LRM 9.4.1 an unknown delay: the
// item is never taken, and where every weight is unknown no item is.
module Top;
  logic [3:0] unknown = 4'b1x01;
  logic [3:0] floating = 4'b1z00;
  int taken[3];
  int none_taken = 0;

  initial begin
    repeat (1000) begin
      randcase
        unknown: taken[0]++;
        1: taken[1]++;
        floating: taken[2]++;
      endcase
    end
    repeat (100) begin
      randcase
        unknown: none_taken++;
        floating: none_taken++;
      endcase
    end
  end

  final begin
    if (taken[0] != 0 || taken[2] != 0 || taken[1] != 1000)
      $fatal(1, "unknown weights were taken %0d and %0d times, the known one %0d",
             taken[0], taken[2], taken[1]);
    if (none_taken != 0)
      $fatal(1, "an item of unknown weights only was taken %0d times", none_taken);
    $display("All checks passed");
  end
endmodule
