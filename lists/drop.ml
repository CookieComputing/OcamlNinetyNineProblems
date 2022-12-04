let drop l n = 
  let rec aux l' x = match l' with
    | [] -> []
    | h::t -> if x = 1 then aux t n else h :: (aux t (x-1)) in
  aux l n