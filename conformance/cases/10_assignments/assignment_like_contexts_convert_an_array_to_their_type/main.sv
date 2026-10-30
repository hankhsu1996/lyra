// @reports: warning count=13
//
// Every assignment-like context assigns its value to a target of the context's
// own type (LRM 10.8): an input argument to its formal, an output argument's
// formal back to its actual, a returned value to the function's type, either
// arm of a conditional to the conditional's, an assignment pattern's item to
// its element or to its member, positional or keyed, a static cast's operand to
// the cast's type, and a nonblocking assignment's value to its target. So do an
// item of an unpacked concatenation of a type equivalent to the element type,
// which is one element (LRM 10.10), a value a queue method inserts, which is
// the method's input (LRM 7.10.2), a dynamic array built from another (LRM
// 7.5.1), and a message a mailbox hands back (LRM 15.4.5). A queue's bound is
// no part of type equivalence (LRM 6.22.2), so each of these takes an array of
// queues of another bound, and each element written is a write to a bounded
// queue, which keeps only what its bound admits and warns when it discards (LRM
// 7.10.5).
module Top;
  typedef int wide_q [$:3];
  typedef int narrow_q [$:1];
  typedef narrow_q narrow_pair [2];
  typedef wide_q wide_pair [2];
  typedef struct { narrow_q m; int n; } holder_t;

  wide_pair source = '{'{1, 2, 3}, '{4}};
  wide_q long_queue = '{5, 6, 7};
  narrow_pair target;

  int passed_size;
  narrow_pair written_back;
  narrow_pair returned;
  narrow_pair chosen;
  narrow_pair patterned;
  narrow_pair cast_target;
  narrow_pair scheduled;
  holder_t by_position;
  holder_t by_name;
  narrow_q joined [$];
  narrow_q pushed [$];
  wide_q grown_from [];
  narrow_q built [];
  narrow_pair received;
  mailbox carrier = new;

  function automatic int size_of_first(narrow_pair taken);
    return taken[0].size();
  endfunction

  task automatic fill(output wide_pair given);
    given = source;
  endtask

  function automatic narrow_pair narrowed(wide_pair from);
    return from;
  endfunction

  initial begin
    passed_size = size_of_first(source);
    fill(written_back);
    returned = narrowed(source);
    chosen = (passed_size > 0) ? source : source;
    patterned = '{long_queue, long_queue[0:0]};
    cast_target = narrow_pair'(source);
    scheduled <= source;
  end

  // A second procedure, so each reports from a place of its own.
  initial begin
    by_position = '{long_queue, 1};
    by_name = '{m: long_queue, n: 2};
    joined = {long_queue};
    pushed.push_back(long_queue);
    grown_from = new[1];
    grown_from[0] = '{8, 9, 10};
    built = new[1](grown_from);
    carrier.put(source);
    carrier.get(received);
  end

  final begin
    if (passed_size !== 2)
      $fatal(1, "an input formal held %0d elements, expected 2", passed_size);
    if (written_back[0].size() !== 2 || written_back[0][1] !== 2)
      $fatal(1, "an output argument wrote back %p", written_back);
    if (returned[0].size() !== 2 || returned[1][0] !== 4)
      $fatal(1, "a returned value became %p", returned);
    if (chosen[0].size() !== 2)
      $fatal(1, "a conditional's arm became %p", chosen);
    if (patterned[0].size() !== 2 || patterned[0][1] !== 6 ||
        patterned[1].size() !== 1)
      $fatal(1, "an assignment pattern's items became %p", patterned);
    if (cast_target[0].size() !== 2)
      $fatal(1, "a static cast became %p", cast_target);
    if (scheduled[0].size() !== 2 || scheduled[0][1] !== 2)
      $fatal(1, "a nonblocking assignment wrote %p", scheduled);
    if (by_position.m.size() !== 2 || by_position.m[1] !== 6 ||
        by_name.m.size() !== 2)
      $fatal(1, "a structure's member from a pattern became %p %p",
             by_position.m, by_name.m);
    if (joined.size() !== 1 || joined[0].size() !== 2 || joined[0][1] !== 6)
      $fatal(1, "a concatenation's item became %p", joined);
    if (pushed.size() !== 1 || pushed[0].size() !== 2 || pushed[0][1] !== 6)
      $fatal(1, "a pushed queue became %p", pushed);
    if (built.size() !== 1 || built[0].size() !== 2 || built[0][1] !== 9)
      $fatal(1, "a dynamic array built from another became %p", built);
    if (received[0].size() !== 2 || received[0][1] !== 2)
      $fatal(1, "a mailbox handed back %p", received);
    $display("All checks passed");
  end
endmodule
