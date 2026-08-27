// A general purpose always procedure repeats for the whole of the simulation,
// and an event control suspends it until the event it names occurs, so its body
// runs once per occurrence rather than once in total (LRM 9.2.2.1, 9.4.2). An
// edge event is detected in one direction only, so a procedure controlled by a
// posedge does not resume when the variable falls back to zero, and one
// controlled by a negedge does not resume when it rises (LRM 9.4.2). A
// waveform that ends high holds one more rise than fall, so the two procedures
// end up having run a different number of times.
module Top;
  bit clk;
  int posedges;
  int negedges;

  always @(posedge clk) posedges = posedges + 1;
  always @(negedge clk) negedges = negedges + 1;

  initial begin
    repeat (5) begin
      #5 clk = 1;
      #5 clk = 0;
    end
    #5 clk = 1;
    #1;
  end

  final begin
    if (posedges !== 6) $fatal(1, "posedges was %0d, expected 6", posedges);
    if (negedges !== 5) $fatal(1, "negedges was %0d, expected 5", negedges);
    $display("All checks passed");
  end
endmodule
