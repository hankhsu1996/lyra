// The compilation-unit-scope declaration the case is written against. A
// reference reaches only the part of that scope defined before it, which is why
// it is declared apart from the design element that reads it.
int shared = 42;
