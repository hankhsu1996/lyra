// @rejected:
//
// The operations a virtual interface takes are listed, and among comparisons
// they are equality and inequality with another virtual interface or null
// only, so case inequality is none of them and a conforming tool rejects this
// program (LRM 25.9).
interface I;
endinterface

module Top;
  I i ();
  virtual I v;

  initial begin
    v = i;
    if (v !== null) $display("the program the standard rejects ran");
  end
endmodule
