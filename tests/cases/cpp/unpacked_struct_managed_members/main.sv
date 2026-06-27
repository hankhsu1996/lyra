module Top;
  typedef struct {
    string first;
    string last;
  } name_t;

  typedef struct {
    name_t nm;
    int    id;
  } rec_t;

  rec_t a;
  rec_t b;
  string a_first;
  string a_last;
  string b_first;
  int    b_id;
  int    eq_ab;

  name_t arr[2];
  string arr0_last;
  string arr1_first;

  initial begin
    a = '{nm: '{first: "ann", last: "lee"}, id: 3};
    b = a;
    b.nm.first = "bob";
    a_first = a.nm.first;
    a_last = a.nm.last;
    b_first = b.nm.first;
    b_id = b.id;
    eq_ab = (a == b);

    arr[0] = '{first: "cat", last: "dog"};
    arr[1] = arr[0];
    arr[0].last = "fox";
    arr0_last = arr[1].last;
    arr1_first = arr[1].first;
  end
endmodule
