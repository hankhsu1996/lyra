// A fork in a loop hands each spawned process its own copy of the
// per-iteration value through the fork's block_item_declaration, initialized
// as execution enters the fork, while a branch naming a variable of the
// enclosing block reads that variable itself, as it stands when the branch
// runs (LRM 9.3.2). Both hold whatever the variable's type: a `real`, a
// `shortreal`, a `chandle` and a virtual interface here.
interface Port;
  int tag;
endinterface

module Top;
  import "DPI-C" function chandle allocate_cell(input int seed);
  import "DPI-C" function int read_cell(input chandle handle);
  import "DPI-C" function void release_cell(input chandle handle);

  Port ports [3] ();

  real         real_seen [3];
  shortreal    short_seen [3];
  int          cell_seen [3];
  int          tag_seen [3];
  real         shared_real_seen;
  int          shared_cell_seen;
  int          shared_tag_seen;

  initial begin
    automatic chandle cells [3];
    automatic virtual Port views [3];
    automatic real shared_real = 0.5;
    automatic chandle shared_cell = null;
    automatic virtual Port shared_view = null;

    for (int i = 0; i < 3; i++) begin
      cells[i] = allocate_cell(10 + i);
      real_seen[i] = -1.0;
      short_seen[i] = -1.0;
      cell_seen[i] = -1;
      tag_seen[i] = -1;
    end
    ports[0].tag = 100;
    ports[1].tag = 101;
    ports[2].tag = 102;
    views[0] = ports[0];
    views[1] = ports[1];
    views[2] = ports[2];
    shared_real_seen = -1.0;
    shared_cell_seen = -1;
    shared_tag_seen = -1;

    for (int j = 0; j < 3; j++)
      fork
        automatic int at = j;
        automatic real r = 1.5 * j;
        automatic shortreal s = 0.25 * j;
        automatic chandle c = cells[j];
        automatic virtual Port v = views[j];
        begin
          #1;
          real_seen[at] = r;
          short_seen[at] = s;
          cell_seen[at] = read_cell(c);
          tag_seen[at] = v.tag;
        end
      join_none

    fork
      begin
        #1;
        shared_real_seen = shared_real;
        shared_cell_seen = read_cell(shared_cell);
        shared_tag_seen = shared_view.tag;
      end
    join_none
    // The branch has not run yet, so it reads these.
    shared_real = 2.5;
    shared_cell = cells[1];
    shared_view = views[2];

    #2;
    for (int i = 0; i < 3; i++) release_cell(cells[i]);
  end

  final begin
    for (int i = 0; i < 3; i++) begin
      if (real_seen[i] !== 1.5 * i)
        $fatal(1, "real_seen[%0d] was %f, expected %f", i, real_seen[i], 1.5 * i);
      if (short_seen[i] !== shortreal'(0.25 * i))
        $fatal(1, "short_seen[%0d] was %f, expected %f", i, short_seen[i], 0.25 * i);
      if (cell_seen[i] !== 10 + i)
        $fatal(1, "cell_seen[%0d] was %0d, expected %0d", i, cell_seen[i], 10 + i);
      if (tag_seen[i] !== 100 + i)
        $fatal(1, "tag_seen[%0d] was %0d, expected %0d", i, tag_seen[i], 100 + i);
    end
    if (shared_real_seen !== 2.5)
      $fatal(1, "shared_real_seen was %f, expected 2.5", shared_real_seen);
    if (shared_cell_seen !== 11)
      $fatal(1, "shared_cell_seen was %0d, expected 11", shared_cell_seen);
    if (shared_tag_seen !== 102)
      $fatal(1, "shared_tag_seen was %0d, expected 102", shared_tag_seen);
    $display("All checks passed");
  end
endmodule
