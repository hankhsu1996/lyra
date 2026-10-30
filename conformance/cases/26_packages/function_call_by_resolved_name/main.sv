// A function declared in a package is called from another scope by the package
// scope resolution operator, with no import (LRM 26.2, 26.3). Inside the
// function the package's own parameters and types resolve unqualified -- in its
// return type and its formals as well as its body -- and the calling scope
// names none of them, since what crosses the boundary is a value of the type
// those declarations resolve to.
//
// A call the package writes to one of its own is the same name resolved from
// the inside, and the declaration order does not bound it: a function reaches a
// sibling the source declared after it, and one reaches itself (LRM 26.3, 13.4
// on recursion). Both are what a scope resolved as a whole means, rather than
// one read top to bottom.
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

  // Both callees are declared below this one.
  function automatic count_t combine(count_t x);
    return scale(x) + offset();
  endfunction

  function automatic count_t scale(count_t x);
    return x * 2;
  endfunction

  function automatic count_t offset();
    return 7;
  endfunction

  function automatic count_t countdown(count_t n);
    if (n <= 0) return 0;
    return countdown(n - 1) + 1;
  endfunction
endpackage

module Top;
  int base_sum;
  int picked;
  int combined;
  int counted;

  initial begin
    base_sum = pkg::add_base(23);
    picked = pkg::pick(5);
    combined = pkg::combine(4);
    counted = pkg::countdown(6);
  end

  final begin
    if (base_sum !== 123) $fatal(1, "base_sum was %0d, expected 123", base_sum);
    if (picked !== 25) $fatal(1, "picked was %0d, expected 25", picked);
    if (combined !== 15) $fatal(1, "combined was %0d, expected 15", combined);
    if (counted !== 6) $fatal(1, "counted was %0d, expected 6", counted);
    $display("All checks passed");
  end
endmodule
