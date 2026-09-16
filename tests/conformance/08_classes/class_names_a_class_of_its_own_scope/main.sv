// A class variable may be declared before the class itself is declared, which
// is what a forward typedef provides for, so two classes in one scope may each
// hold a handle to the other (LRM 8.27). The same visibility lets a class hold
// a handle to itself, directly or through a container, since a class name is
// visible throughout the scope that declares it. The scope may be a package or
// a design element; both declare the classes, so the property below holds in
// each.
package chain_pkg;
  typedef class Tail;

  class Head;
    Tail partner;
    int mark = 1;
  endclass

  class Tail;
    Head partner;
    int mark = 2;
  endclass

  class Node;
    Node next;
    int v = 0;
  endclass

  class Bag;
    Node items[$];

    function void take(Node n);
      items.push_back(n);
    endfunction

    function int sum();
      int total = 0;
      for (int i = 0; i < items.size(); i++) total = total + items[i].v;
      return total;
    endfunction
  endclass
endpackage

module Top;
  int through_head = -1;
  int through_tail = -1;
  int down_the_chain = -1;
  int bagged = -1;
  int module_chain = -1;

  // The same self-reference, declared by a design element rather than a
  // package, so the case states the rule for both kinds of declaring scope.
  class Link;
    Link next;
    int v = 0;
  endclass

  chain_pkg::Head h = new();
  chain_pkg::Tail t = new();
  chain_pkg::Node first = new();
  chain_pkg::Node second = new();
  chain_pkg::Bag bag = new();
  Link a = new();
  Link b = new();

  initial begin
    h.partner = t;
    t.partner = h;
    through_head = h.partner.mark;
    through_tail = t.partner.mark;

    first.v = 4;
    second.v = 6;
    first.next = second;
    down_the_chain = first.next.v;

    bag.take(first);
    bag.take(second);
    bagged = bag.sum();

    a.v = 8;
    b.v = 9;
    a.next = b;
    module_chain = a.next.v;
  end

  final begin
    if (through_head !== 2)
      $fatal(1, "through_head was %0d, expected 2", through_head);
    if (through_tail !== 1)
      $fatal(1, "through_tail was %0d, expected 1", through_tail);
    if (down_the_chain !== 6)
      $fatal(1, "down_the_chain was %0d, expected 6", down_the_chain);
    if (bagged !== 10) $fatal(1, "bagged was %0d, expected 10", bagged);
    if (module_chain !== 9)
      $fatal(1, "module_chain was %0d, expected 9", module_chain);
    $display("All checks passed");
  end
endmodule
