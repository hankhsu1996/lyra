// A real and a shortreal are singular values (LRM 6.12), and whatever holds a
// value holds one of them as it holds any other: a property of a class object
// (LRM 8.4), an element of a fixed-size, a dynamic and an associative array
// and of a queue (LRM 7.4, 7.5, 7.8, 7.10), and the actual a formal passed by
// reference names, which a write to the formal changes (LRM 13.5.2). Each
// gives back the value last written to it, an operation reads it where it is
// held, and copying a whole array or queue copies the values it holds and
// leaves the copy apart from the original (LRM 7.6, 7.10).
module Top;
  class Holder;
    real wide;
    shortreal narrow;
  endclass

  Holder holder;
  real fixed[3];
  real fixed_copy[3];
  real dynamic[];
  real by_key[string];
  real queue[$];
  real queue_copy[$];
  shortreal short_fixed[2];
  shortreal short_queue[$];
  real by_reference = -1.0;
  shortreal short_by_reference = -1.0;

  task automatic scale(ref real target, input real factor);
    target = target * factor;
  endtask

  task automatic scale_short(ref shortreal target, input shortreal factor);
    target = target * factor;
  endtask

  initial begin
    holder = new;
    holder.wide = 1.25;
    holder.wide = holder.wide * 4.0;
    holder.narrow = 0.5;
    holder.narrow = holder.narrow + 0.25;

    fixed[0] = 1.0;
    fixed[1] = 2.5;
    fixed[2] = fixed[1] * 2.0;
    fixed_copy = '{-1.0, -1.0, -1.0};
    fixed_copy = fixed;
    fixed[1] = 7.5;

    dynamic = new[2];
    dynamic[0] = -1.5;
    dynamic[1] = dynamic[0] - 1.0;

    by_key["a"] = 3.25;
    by_key["b"] = by_key["a"] / 2.0;

    queue.push_back(0.5);
    queue.push_back(queue[0] * 3.0);
    queue.push_front(-0.25);
    queue_copy = queue;
    queue[0] = 9.0;

    short_fixed[0] = 1.5;
    short_fixed[1] = short_fixed[0] * 2.0;
    short_queue.push_back(0.125);
    short_queue.push_back(short_queue[0] - 1.0);

    by_reference = 2.0;
    scale(by_reference, 1.5);
    scale(fixed[0], 3.0);
    short_by_reference = 2.0;
    scale_short(short_by_reference, 0.25);
  end

  final begin
    if (holder.wide != 5.0)
      $fatal(1, "a real property held %f, expected 5.0", holder.wide);
    if (holder.narrow != 0.75)
      $fatal(1, "a shortreal property held %f, expected 0.75", holder.narrow);

    if (fixed[0] != 3.0)
      $fatal(1, "an element scaled through a reference held %f, expected 3.0",
             fixed[0]);
    if (fixed[1] != 7.5)
      $fatal(1, "fixed[1] held %f, expected 7.5", fixed[1]);
    if (fixed[2] != 5.0)
      $fatal(1, "fixed[2] held %f, expected 5.0", fixed[2]);
    if (fixed_copy[0] != 1.0 || fixed_copy[1] != 2.5 || fixed_copy[2] != 5.0)
      $fatal(1, "a copied array held %f %f %f, expected 1.0 2.5 5.0",
             fixed_copy[0], fixed_copy[1], fixed_copy[2]);

    if (dynamic.size() !== 2)
      $fatal(1, "the dynamic array held %0d elements, expected 2",
             dynamic.size());
    if (dynamic[0] != -1.5 || dynamic[1] != -2.5)
      $fatal(1, "the dynamic array held %f %f, expected -1.5 -2.5",
             dynamic[0], dynamic[1]);

    if (by_key["a"] != 3.25 || by_key["b"] != 1.625)
      $fatal(1, "the associative array held %f %f, expected 3.25 1.625",
             by_key["a"], by_key["b"]);

    if (queue.size() !== 3)
      $fatal(1, "the queue held %0d elements, expected 3", queue.size());
    if (queue[0] != 9.0 || queue[1] != 0.5 || queue[2] != 1.5)
      $fatal(1, "the queue held %f %f %f, expected 9.0 0.5 1.5", queue[0],
             queue[1], queue[2]);
    if (queue_copy.size() !== 3)
      $fatal(1, "the copied queue held %0d elements, expected 3",
             queue_copy.size());
    if (queue_copy[0] != -0.25 || queue_copy[1] != 0.5 || queue_copy[2] != 1.5)
      $fatal(1, "the copied queue held %f %f %f, expected -0.25 0.5 1.5",
             queue_copy[0], queue_copy[1], queue_copy[2]);

    if (short_fixed[0] != 1.5 || short_fixed[1] != 3.0)
      $fatal(1, "the shortreal array held %f %f, expected 1.5 3.0",
             short_fixed[0], short_fixed[1]);
    if (short_queue.size() !== 2)
      $fatal(1, "the shortreal queue held %0d elements, expected 2",
             short_queue.size());
    if (short_queue[0] != 0.125 || short_queue[1] != -0.875)
      $fatal(1, "the shortreal queue held %f %f, expected 0.125 -0.875",
             short_queue[0], short_queue[1]);

    if (by_reference != 3.0)
      $fatal(1, "a real scaled through a reference held %f, expected 3.0",
             by_reference);
    if (short_by_reference != 0.5)
      $fatal(1, "a shortreal scaled through a reference held %f, expected 0.5",
             short_by_reference);
    $display("All checks passed");
  end
endmodule
