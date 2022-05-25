let rec fib n =
  if n < 2 then n else fib (n - 1) + fib (n - 2);;
  
let square x = x *. x;;

let pi = 4.0 *. atan 1.0;;

let rec leibniz = function n ->
  if n = 0 then 1. /. 3.
  else let n4 = 4. *. float_of_int(n) in leibniz(n-1) +. 1. /. ((n4 +. 1.)*.(n4 +. 3.));;

leibniz 1;;

let h = fun x y -> if x = y then 1 else 0;;