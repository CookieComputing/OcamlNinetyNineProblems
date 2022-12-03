let rec replicate l x = 
  let rec dup c x = if x == 0 then [] else c :: dup c (x-1) in
  List.fold_left (fun acc a -> acc @ (dup a x)) [] l