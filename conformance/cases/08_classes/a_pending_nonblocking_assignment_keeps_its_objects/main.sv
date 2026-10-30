// An object is strongly reachable while a nonblocking assignment to one of its
// non-static members is pending (LRM 8.29), so it is not cleared from a weak
// reference before the assignment lands, even where nothing else refers to
// it. An object a nonblocking assignment assigns is the value the assignment
// evaluated where the statement ran (LRM 10.4.2), so it is still that object
// once the assignment lands, whatever happened to the variable it was read
// from. Every check reads before the pending assignment's region runs, or
// reads what it stored.
class C;
  int v;
endclass

module Top;
  typedef std::weak_reference #(C) c_ref;

  C target;

  function automatic C make(output c_ref w);
    automatic C c = new;
    w = new(c);
    return c;
  endfunction

  initial begin
    automatic c_ref w;
    automatic C obj;

    make(w).v <= 5;
    if (w.get() === null)
      $fatal(1, "an object a pending nonblocking assignment writes into was cleared");
    #0;
    if (w.get() === null)
      $fatal(1, "an object a pending nonblocking assignment writes into was cleared");

    target = null;
    obj = make(w);
    target <= obj;
    obj = null;
    if (w.get() === null)
      $fatal(1, "an object a pending nonblocking assignment assigns was cleared");
    #1;
    if (target === null) $fatal(1, "the nonblocking assignment did not land its object");
    if (target !== w.get()) $fatal(1, "the nonblocking assignment landed another object");

    target = null;
    target <= #1 make(w);
    #0;
    if (w.get() === null)
      $fatal(1, "an object a pending delayed nonblocking assignment assigns was cleared");
    #2;
    if (target === null) $fatal(1, "the delayed nonblocking assignment did not land its object");
    if (target !== w.get()) $fatal(1, "the delayed nonblocking assignment landed another object");

    $display("All checks passed");
  end
endmodule
