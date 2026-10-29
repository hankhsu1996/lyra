// The message a mailbox carries is named "any singular expression" (LRM
// 15.4.3 to 15.4.8), which an unpacked array or an unpacked structure is not,
// and the standard says nothing of putting one. The front end accepts it, and
// Lyra carries it as it does any value: the message keeps its type, and it is
// taken only into a variable of an equivalent type (LRM 6.22.2), a fixed-size
// array of as many equivalent elements however its range is written, a queue
// of an equivalent element whatever bound either was declared with (LRM
// 7.10.5), and a structure only into itself. This case holds Lyra to that.
module Top;
  typedef struct { int a; string b; } pair_t;

  mailbox box;
  int fixed_in [1:2] = '{3, 4};
  int fixed_out [2] = '{0, 0};
  int queue_in [$:4] = '{5, 6};
  int queue_out [$] = '{0};
  int unbounded_in [$] = '{8};
  int bounded_out [$:2] = '{0, 0};
  pair_t pair_in = '{7, "seven"};
  pair_t pair_out = '{0, ""};

  initial begin
    box = new;
    box.put(fixed_in);
    box.put(queue_in);
    box.put(unbounded_in);
    box.put(pair_in);
    box.get(fixed_out);
    box.get(queue_out);
    box.get(bounded_out);
    box.get(pair_out);
  end

  final begin
    if (fixed_out[0] !== 3 || fixed_out[1] !== 4)
      $fatal(1, "a fixed-size array got %p", fixed_out);
    if (queue_out.size() !== 2 || queue_out[0] !== 5 || queue_out[1] !== 6)
      $fatal(1, "a queue got %p", queue_out);
    if (bounded_out.size() !== 1 || bounded_out[0] !== 8)
      $fatal(1, "a bounded queue got %p", bounded_out);
    if (pair_out.a !== 7 || pair_out.b != "seven")
      $fatal(1, "a structure got %p", pair_out);
    $display("All checks passed");
  end
endmodule
