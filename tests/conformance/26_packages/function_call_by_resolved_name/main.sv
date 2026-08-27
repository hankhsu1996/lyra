// A function declared in a package is called from another scope by the package
// scope resolution operator, with no import (LRM 26.2, 26.3). Inside the
// function the package's own parameters and types resolve unqualified -- in its
// return type and its formals as well as its body -- and the calling scope
// names none of them, since what crosses the boundary is a value of the type
// those declarations resolve to.
package pkg;
  localparam int Base = 100;
  typedef enum int {Low = 10, High = 20} level_t;
  typedef int count_t;

  function automatic count_t add_base(count_t x);
    return x + Base;
  endfunction

  function automatic count_t pick(count_t hi);
    level_t lv;
    lv = (hi > 0) ? High : Low;
    return lv + hi;
  endfunction
endpackage

module Top;
  int base_sum;
  int picked;

  initial begin
    base_sum = pkg::add_base(23);
    picked = pkg::pick(5);
  end

  final begin
    if (base_sum !== 123) $fatal(1, "base_sum was %0d, expected 123", base_sum);
    if (picked !== 25) $fatal(1, "picked was %0d, expected 25", picked);
    $display("All checks passed");
  end
endmodule
