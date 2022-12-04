let rotate l n = 
  let rec drop l i = match l with
    | [] -> l
    | (_ :: t) as l' -> if i = 0 then l' else drop t (i-1) in
  let rec take l n = match l with
    | [] -> []
    | h :: t -> if n = 0 then [] else h :: take t (n-1) in
  drop l n @ take l n