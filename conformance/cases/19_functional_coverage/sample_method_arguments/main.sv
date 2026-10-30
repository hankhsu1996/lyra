// A covergroup may declare the arguments of its `sample` method, which its
// coverpoints name; a call of `sample` passes the values (LRM 19.8.1).
module Top;
  covergroup cg with function sample(int s, bit [3:0] t);
    coverpoint s { bins one = {1}; bins two = {2}; }
    coverpoint t { bins low = {[0:7]}; bins high = {[8:15]}; }
  endgroup

  real cov;

  initial begin
    static cg g = new;
    g.sample(1, 12);
    cov = g.get_inst_coverage();
  end

  final begin
    if (cov != 50.0) $fatal(1, "the coverage was %0f, expected 50", cov);
    $display("All checks passed");
  end
endmodule
