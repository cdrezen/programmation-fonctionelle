let rec puiss = fun (x, n) ->
  if n <= 1 then x
  else x *. puiss(x, n - 1);;

  let puissT = fun n x ->
    let rec puissterm = fun k xx ->
    if k=n then xx
    else puissterm (k+1) (x *. xx)
    in
    puissterm 0 1.;;