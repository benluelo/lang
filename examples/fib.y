fib = n uint => uint
	case n
		@ 0 = 0
		@ 1 = 1
		@ n = add { sub n 1 } { sub n 2 };

entry = n: uint => uint {
	fib n
};
