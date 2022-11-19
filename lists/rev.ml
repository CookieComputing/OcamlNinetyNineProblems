let rec rev l = match l with
| [] -> []
| x :: y -> (rev y) @ [x]