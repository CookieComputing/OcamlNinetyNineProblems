let rec list_eq x y = match (x, y) with
| ([], []) -> true
| (h1 :: x', h2:: y') -> h1 = h2 && list_eq x' y'
| _ -> false

let is_palindrome x = list_eq x (List.rev x)