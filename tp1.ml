let numjour = function (an) -> 
  let (a, b, c) = (an mod 19, an mod 4, an mod 7)
    in let d = (19 * a + 24) mod 30 
      in 22 + d + (2*b + 4*c + 6*d + 5) mod 7;;

let traduit = fun (nj) -> string_of_int(nj - 31) ^ " avril"

let paques = function an ->
  "Dimanche " ^ traduit(numjour(an)) ^ " " ^ string_of_int(an);;



let ordi = fun (poid) ->
  if poid < 21 then 0.55
  else if poid < 51 then 0.83
  else 1.22