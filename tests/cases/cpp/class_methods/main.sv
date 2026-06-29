module Top;
  class Counter;
    int value;
    function void set(int v);
      value = v;
    endfunction
    function void add(int delta);
      value = value + delta;
    endfunction
    function int get();
      return value;
    endfunction
  endclass

  Counter c;
  int after_set;
  int after_add;
  initial begin
    c = new;
    c.set(10);
    after_set = c.get();
    c.add(5);
    after_add = c.get();
  end
endmodule
