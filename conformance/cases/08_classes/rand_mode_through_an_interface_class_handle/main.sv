// @rejected:
//
// An interface class has randomize() and the two hooks (LRM 8.26.9), but "use
// of rand_mode and constraint_mode shall not be legal as a consequence of the
// name resolution rules and the fact that interface classes are not permitted
// to contain data members", so a conforming tool rejects this program.
interface class I;
endclass

class C implements I;
  rand bit [3:0] v;
endclass

module Top;
  C c = new;
  I h = c;

  initial h.rand_mode(0);
endmodule
