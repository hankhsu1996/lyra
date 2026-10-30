// A type's coverage is over every instance made of it, its bins merged by name
// under `merge_instances` (LRM 19.11.3); `$get_coverage` weighs the types
// (LRM 19.9); `$load_coverage_db` adds what a database holds to the types'
// counts, and `$set_coverage_db_name` names where this run's is written (LRM
// 19.9).
module Top;
  bit [1:0] v;

  covergroup cg;
    type_option.merge_instances = 1;
    a: coverpoint v { bins b[] = {[0:3]}; }
  endgroup

  real one, both, loaded, run_cov;

  initial begin
    static cg first = new;
    static cg second;
    int fd;
    v = 0; first.sample();
    one = cg::get_coverage();
    second = new;
    v = 1; second.sample();
    both = cg::get_coverage();
    // A database another run wrote, holding b[2] hit.
    fd = $fopen("loaded_coverage.xml", "w");
    $fdisplay(fd, "<UCIS ucisVersion=\"1.0\" writtenBy=\"test\" writtenTime=\"1970-01-01T00:00:00\">");
    $fdisplay(fd, "  <instanceCoverages name=\"Top.cg\" key=\"0\">");
    $fdisplay(fd, "        <coverpoint name=\"a\" key=\"0\">");
    $fdisplay(fd, "          <coverpointBin name=\"b[2]\" type=\"bins\" key=\"0\">");
    $fdisplay(fd, "              <contents coverageCount=\"4\"/>");
    $fdisplay(fd, "          </coverpointBin>");
    $fdisplay(fd, "        </coverpoint>");
    $fdisplay(fd, "  </instanceCoverages>");
    $fdisplay(fd, "</UCIS>");
    $fclose(fd);
    $load_coverage_db("loaded_coverage.xml");
    loaded = cg::get_coverage();
    run_cov = $get_coverage();
    $set_coverage_db_name("this_run_coverage.xml");
  end

  final begin
    if (one != 25.0) $fatal(1, "one instance's type coverage was %0f, expected 25", one);
    if (both != 50.0) $fatal(1, "two instances' merged coverage was %0f, expected 50", both);
    if (loaded != 75.0) $fatal(1, "the coverage with the database was %0f, expected 75", loaded);
    if (run_cov != loaded) $fatal(1, "$get_coverage was %0f, expected %0f", run_cov, loaded);
    $display("All checks passed");
  end
endmodule
