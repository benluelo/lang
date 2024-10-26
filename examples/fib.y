{
    fib = n: int => int case n
        @ 0 = 0
        @ 1 = 1
        @ n = add { fib { sub n 1 } } { fib { sub n 2 } };
    callWithN = n: int f: int -> int => int f n;
    callWith3 = callWithN 3;
    callWith3 fib
}
