let rec length l = match l with
| [] -> 0
| _ :: l' -> 1 + length l'