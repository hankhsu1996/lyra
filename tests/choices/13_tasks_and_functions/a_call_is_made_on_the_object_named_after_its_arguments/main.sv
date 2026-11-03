// The order a call's arguments are evaluated in the standard leaves open (LRM
// 13.5), and so where among them the expression naming the object a method is
// called on is evaluated. Lyra evaluates the arguments the source wrote first,
// left to right, and then the object, so an argument that points the handle
// the source named at another object decides which object the call is made
// on. A call that leaves arguments to their defaults forms the object there
// too, before the first default, which is evaluated on it (LRM 13.5.3), and
// every default and the method are made on that object. This case holds Lyra
// to that, for a call writing every argument and for one leaving one to its
// default.
module Top;
  class Item;
    int p;
    function int f(int a, int x = p);
      return a * 1000 + x * 10 + p;
    endfunction
  endclass

  class Holder;
    Item item;
    function int swap();
      item = new;
      item.p = 7;
      return 9;
    endfunction
    function int every_argument_written();
      return item.f(swap(), 4);
    endfunction
    function int one_left_to_its_default();
      return item.f(swap());
    endfunction
  endclass

  Holder h;
  int written;
  int defaulted;

  initial begin
    h = new;
    h.item = new;
    h.item.p = 3;
    written = h.every_argument_written();
    h.item.p = 3;
    defaulted = h.one_left_to_its_default();
  end

  final begin
    if (written !== 9047)
      $fatal(1, "every argument written gave %0d, expected 9047", written);
    if (defaulted !== 9077)
      $fatal(1, "one left to its default gave %0d, expected 9077", defaulted);
    $display("All checks passed");
  end
endmodule
