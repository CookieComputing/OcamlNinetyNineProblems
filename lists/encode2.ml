type 'a rle =
  | One of 'a
  | Many of int * 'a

  let encode l = 
    let compute x c = if x = 1 then One c
      else Many (x, c) in
    let rec aux acc cur x = function
      | [] -> compute x cur :: acc
      | h :: t when h = cur -> aux acc cur (x+1) t
      | h :: t -> aux (compute x cur :: acc) h 1 t in
    match l with
    | [] -> []
    | h :: t -> List.rev (aux [] h 1 t)

let decode l =
  let rec decompress c = match c with 
      | One a -> [a]
      | Many (1, a) -> decompress (One a)
      | Many (x, a) -> a :: decompress (Many (x-1, a)) in
  List.fold_left (fun x y -> x @ y) [] (List.map decompress l) 