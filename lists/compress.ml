let rec compress' l prev = match l with
| [] -> []
| h :: t when prev = h -> compress' t prev
| h :: t -> h :: compress' t h

let compress l = match l with
| [] -> []
| h :: t -> h :: compress' t h
