// A size-change callback is called after a dynamic array, an associative
// array, a queue or a string is resized, handed the new size (LRM 38.36.1,
// 37.17 detail 14), however it was resized -- by a method, by a write just past
// a queue's last element (LRM 7.10.1), by the whole value written, by a
// randomization choosing its size (LRM 18.5.7.1), as a queue held in an entry
// of an associative array, as a member or an element of what was written
// whole -- and not by a write that leaves the size as it was. Where one
// operation changes both a queue's size and the value of an element a callback
// watches, the size change is called back first (LRM 37.17 detail 14).
`timescale 1ns / 1ns

module Top;
  import "DPI-C" context function int watch_all();
  import "DPI-C" context function int calls_of(string name);
  import "DPI-C" context function int last_of(string name);
  import "DPI-C" context function string order();
  import "DPI-C" context function int differences_so_far();

  int q[$] = '{1};
  int da[];
  int aa[int];
  string s = "";
  int rd[];
  int qa[int][$] = '{1: '{1}};
  typedef struct {
    string s;
    int n;
  } named_t;
  named_t st = '{s: "", n: 0};
  int aq[2][$];

  initial begin
    if (watch_all() !== 0) $fatal(1, "placing the callbacks failed");
    #1;
    q.push_front(9);
    q[1] = 7;
    q.pop_back();
    q[$+1] = 3;
    q = '{4, 5};
    q = '{4};
    da = new[3];
    da[1] = 4;
    aa[7] = 1;
    aa[7] = 2;
    aa.delete(7);
    s = "ab";
    s = "cd";
    s = "";
    if (!std::randomize(rd) with { rd.size() == 4; })
      $fatal(1, "rd could not be randomized");
    qa[1].push_back(2);
    qa[1].push_back(3);
    qa[2] = '{5, 6, 7, 8};
    qa[1][0] = 6;
    st = '{s: "abc", n: 1};
    st = '{s: "abc", n: 2};
    aq = '{'{1}, '{1, 2, 3}};
    aq = '{'{9}, '{1, 2, 3}};
    #1;
    if (calls_of("q") !== 4 || last_of("q") !== 1)
      $fatal(1, "a size change of q was called back %0d times, last %0d",
             calls_of("q"), last_of("q"));
    if (order().substr(0, 1) != "SV")
      $fatal(1, "the queue's callbacks were called in the order %s", order());
    if (calls_of("da") !== 1 || last_of("da") !== 3)
      $fatal(1, "a size change of da was called back %0d times, last %0d",
             calls_of("da"), last_of("da"));
    if (calls_of("aa") !== 2 || last_of("aa") !== 0)
      $fatal(1, "a size change of aa was called back %0d times, last %0d",
             calls_of("aa"), last_of("aa"));
    if (calls_of("s") !== 2 || last_of("s") !== 0)
      $fatal(1, "a size change of s was called back %0d times, last %0d",
             calls_of("s"), last_of("s"));
    if (calls_of("rd") !== 1 || last_of("rd") !== 4)
      $fatal(1, "a size change of rd was called back %0d times, last %0d",
             calls_of("rd"), last_of("rd"));
    if (calls_of("qa[1]") !== 2 || last_of("qa[1]") !== 3)
      $fatal(1, "a size change of qa[1] was called back %0d times, last %0d",
             calls_of("qa[1]"), last_of("qa[1]"));
    if (calls_of("st.s") !== 1 || last_of("st.s") !== 3)
      $fatal(1, "a size change of st.s was called back %0d times, last %0d",
             calls_of("st.s"), last_of("st.s"));
    if (calls_of("aq[1]") !== 1 || last_of("aq[1]") !== 3)
      $fatal(1, "a size change of aq[1] was called back %0d times, last %0d",
             calls_of("aq[1]"), last_of("aq[1]"));
    if (differences_so_far() !== 0)
      $fatal(1, "the calls differ in %0d answers", differences_so_far());
    $display("All checks passed");
  end
endmodule
