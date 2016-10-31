var test2 :: => int;

// lekko zmodyfikowany program z deklaracji jezyka
test2 = fn() => int {
    var f :: int -> int => bool;
    var g :: int -> int => bool;
    var i :: int;
    var j :: int;
    
    f = fn (var x :: int; var y :: int ) => bool { return x > y; };
    g = fn (var x :: int; var y :: int ) => bool { return x * 2 > y; };
    
    i = 0;
    i += 5;
    j = 9;
    
    {
        if f(i, j) then {
            print 1;
        } else {
            if g(i, j) then {
                print 2;
            } else {
                print 5;
            } end;
        } end;
        {
            var i :: int;
            i = j;
            if not f(i, j) then {
                print 3;
            } else {
                print 4;
            } end;
        };
    };
    return 1;
};

{
    var x :: int;
    x = test2();
};
