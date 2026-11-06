// @rejected:
//
// The scope randomize function assigns random values to the variables it is
// handed (LRM 18.12), where they lie, as a write through a reference would. A
// member of an unpacked union is not among what may be passed by reference
// (LRM 13.5.2), and Lyra holds what scope randomize names to that list.
module Top;
  typedef union { int a; int b; } u_t;
  u_t v;

  initial begin
    void'(std::randomize(v.a) with { v.a == 3; });
    $display("the program the standard rejects ran");
  end
endmodule
