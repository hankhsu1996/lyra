// A package's own enum and typedef, named in a package function's signature and
// body (LRM 26). The package is a namespace: its type declarations belong to
// that namespace, and a receiver-less package function resolves them there --
// no enclosing class is involved. The caller passes and receives plain ints;
// the enum and typedef stay inside the package.
package pkg;
  typedef enum int {Low = 10, High = 20} level_t;
  typedef int count_t;

  function automatic count_t pick(count_t hi);
    level_t lv;
    lv = (hi != 0) ? High : Low;
    return lv + hi;
  endfunction
endpackage

module Top;
  int r;
  initial begin
    r = pkg::pick(5);
  end
endmodule
