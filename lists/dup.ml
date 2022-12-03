let rec duplicate x = match x with
| [] -> []
| h :: t -> h :: h :: duplicate t
