// rand_mode() names a member of one element of an array of structures (LRM
// 18.8): switching it off leaves every other member of that element and the
// same member of every other element on, and the call's value form answers
// each one's own state.
module Top;
  typedef struct {
    rand bit [7:0] x;
    rand bit [7:0] y;
  } pair_t;

  class Pairs;
    rand pair_t pairs[2];
  endclass

  initial begin
    Pairs p;
    p = new;
    p.pairs[1].y.rand_mode(0);
    if (p.pairs[1].y.rand_mode() !== 0)
      $fatal(1, "the member switched off answered %0d", p.pairs[1].y.rand_mode());
    if (p.pairs[1].x.rand_mode() !== 1)
      $fatal(1, "another member of the element was switched off too");
    if (p.pairs[0].y.rand_mode() !== 1)
      $fatal(1, "the member of another element was switched off too");
    $display("All checks passed");
  end
endmodule
