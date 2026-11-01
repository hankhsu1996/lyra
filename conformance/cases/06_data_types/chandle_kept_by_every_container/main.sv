// A queue, a dynamic array and an associative array hold `chandle` elements as
// they hold elements of any other type (LRM 7.5, 7.8, 7.10): a new element is
// null, the default every chandle starts at (LRM 6.14), each element keeps the
// pointer written to it and no other, and copying the whole container copies
// each pointer, so the copy and the source name the same storage and a later
// write to one leaves the other as it was.
module Top;
  import "DPI-C" function chandle allocate_cell(input int seed);
  import "DPI-C" function int read_cell(input chandle handle);
  import "DPI-C" function void release_cell(input chandle handle);

  chandle one;
  chandle two;

  chandle queue_of [$];
  chandle queue_copy [$];
  chandle dynamic_of [];
  chandle dynamic_copy [];
  chandle keyed_of [string];
  chandle keyed_copy [string];

  bit grown_is_null;
  bit new_is_null;
  bit popped_is_two;

  initial begin
    one = allocate_cell(1);
    two = allocate_cell(2);
    grown_is_null = 1'b0;
    new_is_null = 1'b0;
    popped_is_two = 1'b0;

    queue_of.push_back(one);
    queue_of.push_back(two);
    queue_of.push_front(null);
    queue_copy = queue_of;
    queue_copy[0] = two;
    popped_is_two = (queue_copy.pop_back() === two);

    dynamic_of = new[2];
    new_is_null = (dynamic_of[0] === null) && (dynamic_of[1] === null);
    dynamic_of[1] = one;
    dynamic_of = new[3] (dynamic_of);
    grown_is_null = (dynamic_of[2] === null);
    dynamic_of[2] = two;
    dynamic_copy = dynamic_of;
    dynamic_copy[1] = null;

    keyed_of["first"] = one;
    keyed_of["second"] = two;
    keyed_copy = keyed_of;
    keyed_copy["first"] = two;
    keyed_copy.delete("second");
  end

  final begin
    if (queue_of.size() !== 3) $fatal(1, "queue_of held %0d, expected 3", queue_of.size());
    if (queue_of[0] !== null) $fatal(1, "queue_of[0] was not null");
    if (queue_of[1] !== one) $fatal(1, "queue_of[1] was not the first pointer");
    if (queue_of[2] !== two) $fatal(1, "queue_of[2] was not the second pointer");
    if (read_cell(queue_of[2]) !== 2)
      $fatal(1, "queue_of[2] reached %0d, expected 2", read_cell(queue_of[2]));
    if (queue_copy.size() !== 2) $fatal(1, "queue_copy held %0d, expected 2", queue_copy.size());
    if (queue_copy[0] !== two) $fatal(1, "queue_copy[0] was not the second pointer");
    if (queue_copy[1] !== one) $fatal(1, "queue_copy[1] was not the first pointer");
    if (popped_is_two !== 1'b1) $fatal(1, "pop_back did not return the second pointer");

    if (new_is_null !== 1'b1) $fatal(1, "new[] made an element that was not null");
    if (grown_is_null !== 1'b1) $fatal(1, "an element new[] added was not null");
    if (dynamic_of[0] !== null) $fatal(1, "dynamic_of[0] was not null");
    if (dynamic_of[1] !== one) $fatal(1, "dynamic_of[1] was not the first pointer");
    if (dynamic_of[2] !== two) $fatal(1, "dynamic_of[2] was not the second pointer");
    if (dynamic_copy[1] !== null) $fatal(1, "dynamic_copy[1] was not null");
    if (dynamic_copy[2] !== two) $fatal(1, "dynamic_copy[2] was not the second pointer");

    if (keyed_of.num() !== 2) $fatal(1, "keyed_of held %0d, expected 2", keyed_of.num());
    if (keyed_of["first"] !== one) $fatal(1, "keyed_of[first] was not the first pointer");
    if (keyed_of["second"] !== two) $fatal(1, "keyed_of[second] was not the second pointer");
    if (keyed_copy.num() !== 1) $fatal(1, "keyed_copy held %0d, expected 1", keyed_copy.num());
    if (keyed_copy["first"] !== two) $fatal(1, "keyed_copy[first] was not the second pointer");
    if (read_cell(keyed_copy["first"]) !== 2)
      $fatal(1, "keyed_copy[first] reached %0d, expected 2", read_cell(keyed_copy["first"]));

    release_cell(one);
    release_cell(two);
    $display("All checks passed");
  end
endmodule
