// @rejected:
//
// A cross bin is a set of the cross's tuples, each a bin of every coverpoint
// the cross crosses, and `binsof` yields the bins of a coverpoint or of one of
// its bins (LRM 19.6.1). A coverpoint the cross does not cross has no bin in
// any of its tuples, so which tuples its bins select is not settled, and Lyra
// refuses the cross bin.
module Top;
  int a;
  int b;
  int c;

  covergroup cg;
    pa: coverpoint a { bins zero = {0}; }
    pb: coverpoint b { bins zero = {0}; }
    pc: coverpoint c { bins zero = {0}; }
    ab: cross pa, pb { bins s = binsof(pc); }
  endgroup

  initial begin
    static cg g = new;
    g.sample();
    $display("the program the standard does not settle ran");
  end
endmodule
