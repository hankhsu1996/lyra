// A variable is written the value its right-hand side evaluates to (LRM 10.4),
// so it may be assigned from itself, or from something reached only through the
// value it holds. A class object stays while a handle refers to it (LRM 8.29),
// and what the variable names before the assignment may lose its last handle
// in the same statement.
class Node;
  int value;
  Node next;
endclass

typedef struct {
  string name;
  int items[$];
} record_t;

function automatic string self_string(string s);
  s = s;
  return s;
endfunction

function automatic int self_queue();
  int q[$] = '{1, 2, 3};
  q = q;
  return q[0] + q[1] + q[2];
endfunction

function automatic int self_record();
  record_t r;
  r.name = "a string long enough to own storage of its own";
  r.items.push_back(4);
  r = r;
  return r.name.len() + r.items[0];
endfunction

function automatic int walks_to_next();
  Node n = new;
  n.value = 1;
  n.next = new;
  n.next.value = 2;
  n = n.next;
  return n.value;
endfunction

function automatic logic [255:0] self_wide(logic [255:0] v);
  v = v;
  return v;
endfunction

module Top;
  string long = "a string long enough to own storage of its own";

  final begin
    if (self_string(long) != long) $fatal(1, "self_string lost its value");
    if (self_queue() !== 6) $fatal(1, "self_queue was %0d", self_queue());
    if (self_record() !== 50) $fatal(1, "self_record was %0d", self_record());
    if (walks_to_next() !== 2)
      $fatal(1, "walks_to_next was %0d", walks_to_next());
    if (self_wide(256'h5) !== 256'h5) $fatal(1, "self_wide lost its value");
    $display("All checks passed");
  end
endmodule
