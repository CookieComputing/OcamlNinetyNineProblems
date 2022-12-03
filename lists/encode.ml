let encode l = 
  let rec aux acc cur x = function
    | [] -> (x, cur) :: acc
    | h :: t when h = cur -> aux acc cur (x+1) t
    | h :: t -> aux ((x, cur) :: acc) h 1 t in
  match l with
  | [] -> []
  | h :: t -> List.rev (aux [] h 1 t)