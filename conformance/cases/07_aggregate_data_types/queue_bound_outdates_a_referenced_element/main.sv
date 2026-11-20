// @reports: warning count=2
//
// Inserting into a queue outdates no reference to its elements, except that an
// insert or a push_front on a bounded queue that leaves it longer than its
// bound discards the highest-numbered element, with a warning, and a reference
// to that element becomes outdated (LRM 7.10.3, 7.10.5). An element passed by
// reference goes on existing until the subroutine completes, and what is
// written through the outdated reference is not seen in the queue (LRM
// 13.5.2), while a reference to an element the queue kept goes on naming it
// at the position it moved to.
module Top;
  int bounded [$:2];
  string texts [$:1];
  int seen_dropped;
  int seen_kept;
  string seen_text;

  task automatic hold(ref int element, output int seen);
    #2;
    seen = element;
    element = 99;
  endtask

  task automatic hold_text(ref string text);
    #2;
    seen_text = text;
    text = "written through an outdated reference";
  endtask

  initial begin
    bounded = '{10, 11, 12};
    seen_dropped = 0;
    seen_kept = 0;
    fork
      hold(bounded[2], seen_dropped);
      hold(bounded[0], seen_kept);
    join_none
    #1 bounded.push_front(9);
    #2;
    if (seen_dropped !== 12)
      $fatal(1, "the element the bound dropped read %0d", seen_dropped);
    if (seen_kept !== 10)
      $fatal(1, "an element the queue kept read %0d", seen_kept);
    if (bounded.size() !== 3)
      $fatal(1, "a write through a dropped element left the size %0d", bounded.size());
    if (bounded[0] !== 9 || bounded[1] !== 99 || bounded[2] !== 11)
      $fatal(1, "the queue held %0d %0d %0d, expected 9 99 11", bounded[0], bounded[1],
             bounded[2]);

    texts = '{"the first string, long enough to own its storage",
              "the second string, long enough to own its storage"};
    seen_text = "";
    fork hold_text(texts[1]); join_none
    #1 texts.insert(0, "an inserted string");
    #2;
    if (seen_text != "the second string, long enough to own its storage")
      $fatal(1, "the string the bound dropped read '%s'", seen_text);
    if (texts.size() !== 2 || texts[0] != "an inserted string" ||
        texts[1] != "the first string, long enough to own its storage")
      $fatal(1, "the queue held '%s' and '%s' after the insertion", texts[0], texts[1]);

    $display("All checks passed");
  end
endmodule
