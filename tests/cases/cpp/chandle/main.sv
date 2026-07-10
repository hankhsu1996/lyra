module Top;
  import "DPI-C" function chandle make_obj(input int seed);
  import "DPI-C" function int read_obj(input chandle h);
  import "DPI-C" function void alloc_obj(output chandle h, input int seed);
  import "DPI-C" function void bump_obj(inout chandle h);
  import "DPI-C" function void free_obj(input chandle h);

  typedef struct {
    chandle h;
    int x;
  } Boxed;

  chandle h;
  chandle g;
  chandle m[string];
  int cnt[chandle];
  Boxed b;

  function automatic chandle identity(chandle x);
    return x;
  endfunction

  initial begin
    if (h == null) $display("default null");
    if (!h) $display("bang null");

    h = make_obj(7);
    if (h != null) $display("non null");
    if (h) $display("truthy");
    $display("read=%0d", read_obj(h));

    g = identity(h);
    if (g === h) $display("case eq");
    if (g !== null) $display("case ne null");

    m["a"] = h;
    if (m["a"] === h) $display("assoc holds");

    cnt[h] = 3;
    $display("keyed=%0d", cnt[h]);

    b.h = h;
    b.x = 9;
    if (b.h === h) $display("struct holds x=%0d", b.x);

    free_obj(h);
    h = null;
    if (h == null) $display("assigned null");

    alloc_obj(g, 5);
    $display("out read=%0d", read_obj(g));
    bump_obj(g);
    $display("inout read=%0d", read_obj(g));
    free_obj(g);
  end
endmodule
