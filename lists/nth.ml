let rec nth l n = match (l, n) with
| ([], x) -> raise (Failure "nth")
| (h :: _, 0) -> h
| (_ :: y, x) -> nth y (x-1)