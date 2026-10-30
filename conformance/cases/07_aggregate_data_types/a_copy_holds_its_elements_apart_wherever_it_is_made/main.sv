// A queue, a string and a structure holding them are values, so wherever one
// is copied the copy holds its own elements and a change to either side leaves
// the other as it was: an argument passed by value, which the callee works on
// as its own (LRM 13.5.1); a structure built from variables (LRM 10.9.2); an
// output copied out on return (LRM 13.5.1); a value returned; the value a
// nonblocking assignment evaluates where the statement runs and stores later
// (LRM 10.4.2); an assignment; and a structure copied whole (LRM 7.2).
module Top;
  typedef int q_t[$];
  typedef struct {
    q_t q;
    string s;
  } pair_t;

  q_t kept;
  q_t landed;
  pair_t held;

  function automatic int grows_its_own(q_t formal);
    formal.push_back(9);
    return formal.size();
  endfunction

  function automatic void hands_out(output q_t out);
    out = kept;
  endfunction

  function automatic q_t hands_back();
    return kept;
  endfunction

  initial begin
    automatic q_t source = '{1, 2};
    automatic pair_t built;
    automatic q_t taken;
    automatic string word = "ab";
    automatic string copied;
    automatic int grown;

    grown = grows_its_own(source);
    if (grown !== 3) $fatal(1, "the callee's formal held %0d elements, expected 3", grown);
    if (source.size() !== 2)
      $fatal(1, "an argument passed by value changed with the callee's formal");

    built = '{q: source, s: word};
    source.push_back(3);
    word = "zz";
    if (built.q.size() !== 2 || built.s != "ab")
      $fatal(1, "a structure built from variables changed with them");

    kept = '{5};
    hands_out(taken);
    kept.push_back(6);
    if (taken.size() !== 1) $fatal(1, "an output copied out changed with its source");

    taken = hands_back();
    kept.push_back(7);
    taken.push_back(8);
    if (taken.size() !== 3 || kept.size() !== 3)
      $fatal(1, "a returned value and its source changed together");

    landed = '{0};
    landed <= source;
    source.push_back(4);
    #1;
    if (landed.size() !== 3)
      $fatal(1, "a nonblocking assignment stored %0d elements, expected 3", landed.size());

    copied = word;
    word = "yy";
    if (copied != "zz") $fatal(1, "an assigned string changed with its source");

    held = built;
    built.q.push_back(7);
    if (held.q.size() !== 2) $fatal(1, "a structure copied whole changed with its source");

    $display("All checks passed");
  end
endmodule
