// Creating an object runs the base class constructor, then initializes each
// property, then runs the rest of the class's own constructor (LRM 8.7). A
// process killed while it is constructing an object (LRM 9.7) leaves at
// whichever of those steps it had reached: the `new` never completes, so the
// handle it was to be assigned to keeps its value, and the run goes on. The
// object lives exactly as long as something refers to it (LRM 8.29): one
// whose construction is left is reclaimed whole when nothing else refers to it
// -- a string property already given a long value, in the class itself and in
// its base, included -- which a program that checks its memory reports if it
// is not (LRM 8.7 allows no memory leaks). One whose constructor had already
// handed `this` on (LRM 8.11) is still referred to, so it goes on existing,
// with whatever its construction had given it.
//
// A constructor may also return early (LRM 13.4.1), which completes the object
// rather than abandoning it.
module Top;
  typedef class Escapes;

  string kept_long = "a string longer than a string keeps in place, by far";
  Escapes escaped;

  function automatic void stop_this_process();
    process p;
    p = process::self();
    p.kill();
  endfunction

  // Left in its own constructor body, after a property has its value.
  class Own;
    string text;
    function new();
      text = kept_long;
      stop_this_process();
      text = "not reached";
    endfunction
  endclass

  // Left while initializing its properties, after the first has its value.
  class Initializing;
    string first = kept_long;
    bit second = stop_and_answer();
    static function bit stop_and_answer();
      stop_this_process();
      return 1;
    endfunction
  endclass

  class Base;
    string base_text;
    function new(bit stop);
      base_text = kept_long;
      if (stop) stop_this_process();
    endfunction
  endclass

  // Left after its base class constructor completed and its own property
  // has its value.
  class LeftAfterItsBase extends Base;
    string own_text = kept_long;
    function new();
      super.new(0);
      stop_this_process();
    endfunction
  endclass

  // Left inside its base class constructor, before its own properties exist.
  class LeftInItsBase extends Base;
    string own_text = kept_long;
    function new();
      super.new(1);
    endfunction
  endclass

  // Left after handing `this` on, which keeps it.
  class Escapes;
    string text;
    function new();
      text = kept_long;
      escaped = this;
      stop_this_process();
      text = "not reached";
    endfunction
  endclass

  // Returns before the end of its constructor, which completes it.
  class ReturnsEarly extends Base;
    string own_text = kept_long;
    int after;
    function new(int n);
      super.new(0);
      after = 1;
      if (n > 0) return;
      after = 2;
    endfunction
  endclass

  Own own;
  Initializing initializing;
  LeftAfterItsBase after_base;
  LeftInItsBase in_base;
  ReturnsEarly early;
  Escapes escaping;
  int reached;

  initial begin
    own = new;
    reached = reached + 1;
  end

  initial begin
    initializing = new;
    reached = reached + 1;
  end

  initial begin
    after_base = new;
    reached = reached + 1;
  end

  initial begin
    in_base = new;
    reached = reached + 1;
  end

  initial begin
    escaping = new;
    reached = reached + 1;
  end

  initial early = new(1);

  final begin
    if (reached !== 0)
      $fatal(1, "%0d processes went on past an abandoned new", reached);
    if (own !== null) $fatal(1, "own was assigned an object");
    if (initializing !== null) $fatal(1, "initializing was assigned an object");
    if (after_base !== null) $fatal(1, "after_base was assigned an object");
    if (in_base !== null) $fatal(1, "in_base was assigned an object");
    if (escaping !== null) $fatal(1, "escaping was assigned an object");
    if (escaped === null) $fatal(1, "the object that handed this on is gone");
    if (escaped.text != kept_long)
      $fatal(1, "the object that handed this on holds '%s'", escaped.text);
    if (early === null) $fatal(1, "early was not assigned its object");
    if (early.after !== 1) $fatal(1, "early.after was %0d, expected 1", early.after);
    if (early.own_text != kept_long || early.base_text != kept_long)
      $fatal(1, "the object that returned early lost its properties");
    $display("All checks passed");
  end
endmodule
