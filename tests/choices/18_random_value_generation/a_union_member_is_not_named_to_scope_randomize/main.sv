// @rejected:
//
// The scope randomize function is handed a list of variable identifiers (LRM
// 18.12), and the front end Lyra pins accepts a member or an element of one as
// an extension. A union is one storage its members are alternative readings of
// (LRM 7.3), so what lies in a member has no storage of its own to randomize,
// as LRM 18.4 says an unpacked union shall not be declared random at all, and
// the front end refuses it.
module Top;
  typedef union { int a; int b; } u_t;
  u_t v;

  initial begin
    void'(std::randomize(v.a) with { v.a == 3; });
    $display("the program the standard rejects ran");
  end
endmodule
