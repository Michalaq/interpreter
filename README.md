# Compiling  

```shell
make grammar & make
```

# Usage

```shell
./interpreter [filename]
```

# Contents  

* MyInterpreter.hs - module implementing my interpreter  
* TypeCheck.hs - module running static type check  

# Notes

* Built-in types: int, bool and functional.  
* Each function must be assigned to a variable.  
* Semantics of return: set the returned value (no jumps).  
* Every function is anonymous - think of lambdas.  
* Value of each function call must be assigned. If you return a function and want to use it then, you need to assign it first.  
* Both definitions and declarations of functions must have full type signature.  
