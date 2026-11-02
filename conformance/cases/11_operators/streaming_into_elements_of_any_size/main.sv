// A stream unpacked into a queue of structures holding a dynamic member makes
// each element from the bits it takes, and a structure's size changes nothing
// about that, up to one of many megabytes (LRM 11.4.14.3).
module Top;
  typedef struct {
    int body[4];
    string tail;
  } Small;

  typedef struct {
    int body[4000000];
    string tail;
  } Large;

  Small small_list[$];
  Large large_list[$];
  byte source[];

  initial begin
    source = new[24];
    source[3] = 8'h5A;
    small_list = {>>{source}};
    if (small_list.size() !== 1 || small_list[0].body[0] !== 32'h5A)
      $fatal(1, "the small structure held %0d elements, expected 1 starting 5A",
             small_list.size());

    source = new[16000008];
    source[3] = 8'h5A;
    large_list = {>>{source}};
    if (large_list.size() !== 1 || large_list[0].body[0] !== 32'h5A)
      $fatal(1, "the large structure held %0d elements, expected 1 starting 5A",
             large_list.size());
    $display("All checks passed");
  end
endmodule
