{
    fib = i: int => int case i
        @ 0 = 0
        @ 1 = 1
        @ i = add { fib { sub i 1 } } { fib { sub i 2 } };

    callWithN = n: int f: int -> int => int f n;
    callWith3 = callWithN 30;
    out = callWith3;
    out fib
}
