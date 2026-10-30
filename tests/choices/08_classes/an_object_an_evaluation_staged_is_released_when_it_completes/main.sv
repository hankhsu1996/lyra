// An object is no longer in use once nothing refers to it from an active scope
// and no pending nonblocking assignment writes into it (LRM 8.29), so an object
// a statement made and named only while it was evaluated -- read, written,
// called on, compared, handed to a function, read and then written -- is no
// longer in use once the evaluation completes, and one a nonblocking assignment
// writes into or assigns is no longer in use once the assignment has landed and
// nothing else refers to it. When a weak reference to such an object is then
// cleared is left to the tool (LRM 8.30.1). Lyra clears it as the last thing
// using the object lets go of it, so each check below asks right after that
// point. Every statement stands directly in the procedure's own block, which
// runs to the end of the case, so nothing here is released by a block ending.
class C;
  int v;
  function int get();
    return v;
  endfunction
endclass

module Top;
  typedef std::weak_reference #(C) c_ref;

  C target;

  function automatic C make(output c_ref w);
    automatic C c = new;
    w = new(c);
    return c;
  endfunction

  function automatic int read_of(C c);
    return c.v;
  endfunction

  initial begin
    automatic c_ref w;
    automatic C obj;
    automatic int x;

    x = make(w).v;
    if (w.get() !== null) $fatal(1, "an object read kept after the statement");
    make(w).v = 1;
    if (w.get() !== null) $fatal(1, "an object written kept after the statement");
    make(w).v[0] = 1'b1;
    if (w.get() !== null) $fatal(1, "an object's bit written kept after the statement");
    x = make(w).get();
    if (w.get() !== null) $fatal(1, "an object called on kept after the statement");
    case (make(w))
      null: x = 0;
      default: x = 1;
    endcase
    if (w.get() !== null) $fatal(1, "a case subject kept after the statement");
    x = (make(w) != null) ? 1 : 0;
    if (w.get() !== null) $fatal(1, "an object compared kept after the statement");
    x = read_of(make(w));
    if (w.get() !== null) $fatal(1, "an object handed to a function kept after the statement");
    void'(make(w));
    if (w.get() !== null) $fatal(1, "an object discarded kept after the statement");
    make(w).v += 1;
    if (w.get() !== null) $fatal(1, "an object read and written kept after the statement");
    make(w).v++;
    if (w.get() !== null) $fatal(1, "an object incremented kept after the statement");

    make(w).v <= 1;
    #1;
    if (w.get() !== null)
      $fatal(1, "an object a nonblocking assignment wrote into kept after it landed");

    target = null;
    obj = make(w);
    target <= obj;
    obj = null;
    #1;
    target = null;
    if (w.get() !== null)
      $fatal(1, "an object a nonblocking assignment assigned kept once its target let go");

    target <= #1 make(w);
    #2;
    target = null;
    if (w.get() !== null)
      $fatal(1, "an object a delayed nonblocking assignment assigned kept once its target let go");

    $display("All checks passed");
  end
endmodule
