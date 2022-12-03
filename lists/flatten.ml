type 'a node =
| One of 'a
| Many of 'a node list

let rec flatten l = match l with
| [] -> []
| One x :: l' -> x :: flatten l'
| Many x :: l' -> flatten x @ flatten l'

