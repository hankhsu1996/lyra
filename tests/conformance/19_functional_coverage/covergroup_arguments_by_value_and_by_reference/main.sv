// A covergroup's input argument is the value it was given when it was made,
// and a `ref` argument names a variable whose value each sample reads and
// whose change the clocking event watches (LRM 19.3).
module Top;
  bit clk;
  bit [1:0] v;
  int lo = 1;

  covergroup cg (ref bit c, ref bit [1:0] x, input int low) @(posedge c);
    cp: coverpoint x { bins b[] = {[low:3]}; }
  endgroup

  cg g = new(clk, v, lo);
  real cov;

  initial begin
    lo = 0;
    v = 1; #1 clk = 1; #1 clk = 0;
    v = 3; #1 clk = 1; #1 clk = 0;
    cov = g.get_inst_coverage();
  end

  final begin
    // Bins 1 to 3, lo having been 1 when the instance was made; 1 and 3 hit.
    if (cov < 66.66 || cov > 66.67) $fatal(1, "the coverage was %0f, expected 66.67", cov);
    $display("All checks passed");
  end
endmodule
