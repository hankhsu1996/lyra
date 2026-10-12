package row_pkg;
  typedef struct {
    int tag;
    string name;
  } labelled_t;

  typedef int row_t[4];
  typedef labelled_t labelled_row_t[2];

  function automatic int sum_of(row_t row);
    int total = 0;
    foreach (row[i]) total += row[i];
    return total;
  endfunction
endpackage
