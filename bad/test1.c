var test1 :: => (int => int);

// dzielenie przez zero
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
                    { return y / g(z); }; // tu bedzie dzielenie przez 0
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
            x = x / 8;
            h = fn(var f :: ((int => int) => (int => int));
                   var g :: (int => int);
                   var x :: int*) => int { var tmp :: (int => int); tmp = f(g); x = x / 8; return tmp(x); };
            print -h(f, g, x);
            print x;
            y++;
        };
    };
    y--;
    return fn(var x :: int) => int { return x; };
};

{
    var f :: int => int;
    f = test1();
};
