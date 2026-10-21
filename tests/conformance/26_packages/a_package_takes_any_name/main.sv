// A package is named by any identifier the definitions name space does not
// already hold (LRM 3.13, 26.2). These take names other languages give a
// namespace of their own -- the library a program runs on, an operating
// system's interface, a future standard library -- and each is reached by its
// name, so the name is still this package's alone.
package lyra;
  class runtime;
    int id = 1;
  endclass
  class value;
    int id = 2;
  endclass
  int count = 3;
endpackage

package posix;
  function automatic int twice(int x);
    return 2 * x;
  endfunction
endpackage

package std2;
  typedef struct {
    int a;
  } pair_t;
  pair_t made = '{a: 4};
endpackage

module Top;
  lyra::runtime r;
  lyra::value v;
  int r_id = -1;
  int v_id = -1;
  int doubled = -1;
  int made_a = -1;
  int counted = -1;

  initial begin
    r = new;
    v = new;
    r_id = r.id;
    v_id = v.id;
    counted = lyra::count;
    doubled = posix::twice(5);
    made_a = std2::made.a;
  end

  final begin
    if (r_id !== 1) $fatal(1, "lyra::runtime held %0d, expected 1", r_id);
    if (v_id !== 2) $fatal(1, "lyra::value held %0d, expected 2", v_id);
    if (counted !== 3) $fatal(1, "lyra::count was %0d, expected 3", counted);
    if (doubled !== 10) $fatal(1, "posix::twice(5) was %0d, expected 10", doubled);
    if (made_a !== 4) $fatal(1, "std2::made.a was %0d, expected 4", made_a);
    $display("All checks passed");
  end
endmodule
