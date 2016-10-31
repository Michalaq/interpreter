var test1 :: => (int => int);

// przyklad ciekawych domkniec
test1 = fn() => (int => int) {
    var x :: int;
    var y :: int;
    var a :: bool;
    var f :: (int => int) => (int => int);
    a = True;
    x = 5;
    f = fn(var g :: int => int) => (int => int)
        {
            var y :: int; y = g(x); 
            return fn(var z :: int) => int 
                    { return y + g(z); };
        };
    y = 5;
    if a == True or False then 
    { var y :: int; y = 2; }
    else 
        y = 0
    end;
    {
        var x :: int;
        x = 129;
        while x > 0 do {
            var g :: int => int;
            var h :: ((int => int) => (int => int)) -> (int => int) -> (int*) => int;
            g = fn(var y :: int) => int { return 2 * y; };
            x = x / 2;
            h = fn(var f :: ((int => int) => (int => int));
                   var g :: (int => int);
                   var x :: int*) => int { var tmp :: (int => int); tmp = f(g); x = x / 2; return tmp(x); };
            print -h(f, g, x);
            print x;
            y++;
        };
    };
    y--;
    return fn(var x :: int) => int { return x; };
};

{
    var x :: int => int;
    x = test1();
};

