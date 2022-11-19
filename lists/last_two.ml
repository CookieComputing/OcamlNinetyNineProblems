let rec last_two l = match l with
| x :: y :: [] -> Option.Some((x, y))
| x :: [] -> Option.None
| _ :: l -> last_two l
| [] -> Option.None