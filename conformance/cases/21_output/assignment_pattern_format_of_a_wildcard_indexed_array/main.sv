// @includes: assignment_pattern.sv
//
// The assignment pattern conversion prints an associative array as its
// entries, each under the index it is stored at (LRM 21.2.1.6). An array
// declared with a wildcard index type holds entries the same way, and the
// standard orders them numerically (LRM 7.8.1), so the text the clause
// describes exists for one -- but every way of walking an associative array is
// withheld from that index type: `first`, `last`, `next` and `prev` each
// exclude it (LRM 7.9.4 through 7.9.7) and so does `foreach` (LRM 7.8.1).
//
// A type reached through another is the same question, because a structure
// prints each of its members under the name it declares for it, so a member
// with no text leaves the structure with none. The spelling of the pattern is
// the tool's, so each text is held to being one of the legal spellings of the
// value written here in full.
module Top;
  import assignment_pattern::Spells;

  typedef struct {
    int holds [*];
    int weight;
  } Wrapper;

  int     bare [*];
  Wrapper wrapped;

  string bare_text;
  string wrapped_text;

  initial begin
    bare[8'd5] = 100;
    bare[300] = 7;

    wrapped.weight = 2;
    wrapped.holds[1] = 10;

    bare_text = $sformatf("%p", bare);
    wrapped_text = $sformatf("%p", wrapped);
  end

  final begin
    if (!Spells(bare_text, "'{5:100, 300:7}"))
      $fatal(1, "bare_text was %s, expected '{5:100, 300:7}", bare_text);
    if (!Spells(wrapped_text, "'{holds:'{1:10}, weight:2}"))
      $fatal(1, "wrapped_text was %s, expected '{holds:'{1:10}, weight:2}",
             wrapped_text);
    $display("All checks passed");
  end
endmodule
