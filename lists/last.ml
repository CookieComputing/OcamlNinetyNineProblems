let rec last l = match l with
| [] -> Option.None
| x :: [] -> Option.Some(x)
| _ :: x -> last x