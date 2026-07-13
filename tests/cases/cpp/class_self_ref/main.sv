// SystemVerilog class with a handle field of its own type (LRM 8): the emit
// must forward-declare the class before its field of a handle to it is
// materialized, and the runtime handle representation must accept the
// forward-declared pointee.
module Top;
  class Node;
    int value;
    Node next;
  endclass

  int head_val;
  int tail_val;
  bit chain_matches_tail;
  bit tail_next_is_null;

  initial begin
    Node head;
    Node tail;
    head = new;
    tail = new;
    head.value = 1;
    tail.value = 2;
    head.next = tail;
    head_val = head.value;
    tail_val = head.next.value;
    chain_matches_tail = (head.next == tail);
    tail_next_is_null = (tail.next == null);
  end
endmodule
