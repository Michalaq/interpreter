{ // test obrazujacy mozliwosci przekazywania przez referencje zmiennych, w tym funkcji
    var test3 :: (((int*) => int)*) -> (int*) => int;
    var f :: (int*) => int;
    var x :: int;
    x = 2;
    test3 = fn (var f :: ((int*) => int)*; var y :: int*) => int {
        y = 10;
        f = fn (var b :: int*) => int { y = 3 * y; return b + x; };
        return 5;
    };
    f = fn (var x :: int*) => int { x = x + 2; return x * 2; };
    print 123;
    print x;
    print f(x);
    print x;
    print test3(f, x);
    print x;
    print f(x);
    print x;
};
